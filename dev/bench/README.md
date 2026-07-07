# Speed benches

Fixed-input benchmarks for **behavior-preserving** speedups, each built to drive a
`/goal` loop (see "Using with `/goal`"). Six live here, one per heavy compute
tier:

- **`speed_bench.R`** — the parcel MCMC sampler, Tier 4 (fixed seed; profile in
  `HOTSPOTS.md`; reference in `baseline.txt`).
- **`compliance_bench.R`** — the compliance-engine batch path
  `evaluate_compliance(precomputed = TRUE)` (RNG-free; profile in
  `COMPLIANCE_HOTSPOTS.md`; reference in `compliance_baseline.txt`). See
  "Compliance batch bench" below.
- **`discovery_bench.R`** — the LCC discovery tier (Tier 3A) that builds
  `discovered_lcc_library` from the parcel graph (fixed seed; profile in
  `DISCOVERY_HOTSPOTS.md`; reference in `discovery_baseline.txt`). See "LCC
  discovery bench" below.
- **`secondary_bench.R`** — the secondary discovery tier (Tier 3B) that builds
  `discovered_secondary_library`; the sibling of the LCC tier, still on the slow
  `bfs_grow_block` path (fixed seed; profile in `SECONDARY_HOTSPOTS.md`;
  reference in `secondary_baseline.txt`). See "Secondary discovery bench" below.
- **`graph_bench.R`** — the parcel graph construction tier (Tier 2) that builds
  the adjacency graph from district geometry (RNG-free; profile in
  `GRAPH_HOTSPOTS.md`; reference in `graph_baseline.txt`). See "Parcel graph
  construction bench" below.
- **`density_bench.R`** — the density-deduction precompute, the heaviest single
  spatial step in the compliance workflow (RNG-free, **TOLERANCE gate** not hash;
  profile in `DENSITY_HOTSPOTS.md`; reference in `density_baseline.{txt,rds}`).
  See "Density precompute bench" below.

Five print the same fields and obey the same rule: a correct optimization
**lowers ELAPSED while keeping OUTPUT_HASH equal to the baseline**. The sixth
(`density_bench.R`) swaps the hash for a per-parcel **tolerance** check — its
spatial output drifts by sub-square-foot amounts under a legitimate speedup — but
follows the same lower-ELAPSED-while-gate-holds rule.

# MCMC speed bench

A fixed-seed, bit-identical benchmark for **behavior-preserving** speedups of the
parcel MCMC sampler.

## What it does

`speed_bench.R` loads a pre-built **warm targets store** (`ext/_targets_Topsfield`,
the smallest fully-built store that still exercises every kernel — 2,316-vertex
graph, 903 LCCs, 183 secondaries), builds a single-chain config, and runs one
chain **twice with a fixed seed** (`set.seed(42)`, 2,000 steps). It prints:

```
ELAPSED: <s>          min wall-clock of the chain run (the thing to lower)
OUTPUT_HASH: <hash>   rlang::hash of parcel_samples (must stay constant)
DETERMINISTIC: TRUE   both runs hashed equal (guards against new nondeterminism)
HASH_MATCH: TRUE      OUTPUT_HASH == baseline hash (the correctness gate)
SPEEDUP: <x>          baseline_elapsed / ELAPSED
```

A correct optimization **lowers ELAPSED while keeping the hash equal**. Any change
to the hash means behavior changed — that's a bug, not a speedup.

2,000 steps is chosen to clear the online-enrichment burn-in (1,000), so the bench
exercises online enrichment too. The chain runner uses `enable_online_enrichment
= TRUE`, matching the production pipeline.

## Usage

```bash
Rscript dev/bench/speed_bench.R            # measure, compare to baseline.txt
Rscript dev/bench/speed_bench.R --capture  # (re)write baseline.txt from current code
```

`baseline.txt` holds the reference hash + time and the git commit they came from.
Re-capture only from a known-good commit (e.g. after merging an accepted speedup,
to set the next milestone).

## The fence: which optimizations this gate allows

Only **Class A** changes — those that preserve the exact random-draw sequence, so
output stays bit-identical under a fixed seed:

- vectorizing deterministic inner computations (MH-ratio terms, capacity/area sums)
- caching invariants (compatible-LCC set, neighbor counts)
- swapping data structures (lists → integer-indexed), reducing allocations/copies
- porting pure functions to Rcpp

**Not** allowed here (they change the draw sequence, so the hash legitimately
changes and this gate can't validate them): altering the proposal/acceptance math,
reordering proposal evaluation, changing the number/order of RNG draws, within-chain
parallelism. Those are real wins but need statistical equivalence checks across many
seeds — do them interactively with the convergence diagnostics, not in this loop.

## Frozen knobs

While a speed goal is active, do **not** edit any of these — they define the
measurement, and changing them is how a loop "cheats" by doing less work:

- this harness (`dev/bench/`) or `baseline.txt`
- the seed, step count, or store in `speed_bench.R`
- MCMC tuning constants in `inst/targets/temp_targets_*.R` / `R/config.R`
  (steps, burn-in, chains, priors, library sizes)

The optimization target is the sampler implementation in `R/mcmc_*.R`.

To make turns faster (shorter feedback loop), lower `STEPS`/`RUNS` in
`speed_bench.R` **before** setting the goal, then re-capture the baseline.

## Using with `/goal`

Set a goal whose condition embeds the baseline numbers and forbids the frozen
knobs. The condition is the authoritative source for the target time and the
baseline hash; the evaluator reads the bench output against it. Example lives in
the project notes / paste it from the session that set this up.

# Compliance batch bench

A fixed-input, bit-identical benchmark for **behavior-preserving** speedups of the
compliance-engine batch-evaluation path. This is the workflow the package
documents for evaluating thousands of zoning-parameter combinations on one
municipality: pre-compute spatial attributes once, then call
`evaluate_compliance(..., precomputed = TRUE)` per parameter set.

## What it does

`compliance_bench.R` loads one municipality once (Cambridge, 12,867 parcels),
pre-computes the cheap station-area overlap once, then times **60 evaluations**
over a **fixed, deterministic grid** of zoning-parameter sets and runs the whole
batch **twice**. It prints the same `ELAPSED / OUTPUT_HASH / DETERMINISTIC /
HASH_MATCH / SPEEDUP` block as the MCMC bench.

Unlike the MCMC sampler, this path draws **no random numbers** — so the gate is
even simpler: there is no draw-sequence to preserve, only the requirement that the
computed numbers don't change. `DETERMINISTIC: TRUE` (both runs hash equal) still
guards against an optimization that accidentally introduces nondeterminism (e.g.
parallelism, hash-map iteration order).

Density deductions are deliberately **not** pre-computed: the gross-density
denominator is a single `sum()` either way, so skipping the ~12-minute density
`st_intersection` keeps the timed loop pure arithmetic (the optimization target)
and the setup fast. The denominator falls back to total area, which does not change
what the bench measures.

## Usage

```bash
Rscript dev/bench/compliance_bench.R            # measure, compare to compliance_baseline.txt
Rscript dev/bench/compliance_bench.R --capture  # (re)write compliance_baseline.txt from current code
```

## The fence: which optimizations this gate allows

Only changes that keep the computed values bit-identical. The target is the
per-iteration compute path:

- `R/compliance_pipeline.R` (`evaluate_compliance`, `calculate_district_capacity`)
- `R/unit_capacity_calculations.R` (the 18 `calculate_*` functions)
- `precomputed = TRUE` branches of `R/gis_operations.R`, if needed

Do **not** edit the one-time setup functions — `load_municipality()`,
`precompute_spatial_attributes()`, `load_transit_stations()`,
`create_zoning_parameters()` — they are the bench's fixed input, not the thing
under test. Do not edit `dev/bench/`, `compliance_baseline.txt`, `N_ITERS`, the
municipality, or the parameter grid while a goal is active.

See `COMPLIANCE_HOTSPOTS.md` for the profile: ~85% of the time is one line —
subsetting the full sf (with geometry) when only numeric columns are needed.

# LCC discovery bench

A fixed-seed, bit-identical benchmark for **behavior-preserving** speedups of the
**LCC discovery tier** — the pipeline stage (Tier 3A in
`inst/targets/_targets.R`) that builds `discovered_lcc_library` from the parcel
graph, **before** the MCMC sampler runs. It is the other large compute tier in the
pipeline and, unlike the sampler, had no speed gate.

## What it does

`discovery_bench.R` loads the same warm store as the MCMC bench
(`ext/_targets_Topsfield`) for the two fixed inputs the discovery consumes —
`parcel_graph_result` and `constraints` — then runs the full discovery chain
**twice with a fixed seed** (`set.seed(42)`): tree enumeration
(`discover_lccs_from_trees`), BFS boundary supplement (`run_bfs_lcc_supplement`),
five capacity bands (`discover_lccs_single_band`), then the deterministic
`combine_*` and `build_lcc_library_from_tree_discovery`. It prints the same
`ELAPSED / OUTPUT_HASH / DETERMINISTIC / HASH_MATCH / SPEEDUP` block as the other
benches; `OUTPUT_HASH` is the `rlang::hash` of the discovered library.

This tier **draws random numbers**, so the gate is the MCMC-sampler kind, not the
compliance kind: under a fixed seed the run is reproducible, and a correct
optimization must preserve the exact draw sequence. The pipeline runs the five
bands as parallel crew workers; the bench runs them **serially in one process
under one seed** — it defines its own baseline from current code, so it need not
match the pipeline's stored library, only exercise the real code and be internally
reproducible. `DETERMINISTIC: TRUE` (both runs hash equal) guards that.

## Usage

```bash
Rscript dev/bench/discovery_bench.R            # measure, compare to discovery_baseline.txt
Rscript dev/bench/discovery_bench.R --capture  # (re)write discovery_baseline.txt from current code
```

## The fence: which optimizations this gate allows

Only **Class A** changes that preserve the random-draw sequence, so output stays
bit-identical under the fixed seed. The target is the discovery implementation:

- `R/mcmc_bfs_utils.R` (`bfs_grow_block` — where ~80% of the time is)
- `R/mcmc_parcel_library.R` (`run_bfs_lcc_supplement`, `discover_lccs_single_band`,
  `combine_*`, `build_lcc_library_from_tree_discovery`)
- `R/mcmc_spanning_tree.R` (`discover_lccs_from_trees`) — but **not** the
  `sample_spanning_tree` draw itself

Do **not** edit `dev/bench/`, `discovery_baseline.txt`, the seed, the store, or the
discovery size constants in `inst/targets/temp_targets_*.R` while a goal is active.

See `DISCOVERY_HOTSPOTS.md` for the profile: one line — a per-BFS-step
`igraph::neighbors()$name` lookup in `bfs_grow_block` — is 56.6% of the run; a
precomputed name-keyed adjacency list (reused from the pattern already in
`build_lcc_library_from_tree_discovery`) is the dominant lever, worth a ~2.5×
ceiling on its own.

# Secondary discovery bench

A fixed-seed, bit-identical benchmark for **behavior-preserving** speedups of the
**secondary discovery tier** — Tier 3B in `inst/targets/_targets.R`, which builds
`discovered_secondary_library` from the parcel graph in parallel to the LCC tier.
It is the sibling of the LCC discovery bench and uses the same machinery (Wilson
tree enumeration + BFS growth via `bfs_grow_block`).

## What it does

`secondary_bench.R` loads the same warm store (`ext/_targets_Topsfield`) for the
one fixed input the discovery consumes — `parcel_graph_result` — then runs the
full secondary discovery chain **twice with a fixed seed** (`set.seed(42)`):
`discover_secondaries_from_trees` → `run_bfs_secondary_supplement` →
`combine_discovered_blocks` → `build_secondary_library_from_discovery`. It prints
the same `ELAPSED / OUTPUT_HASH / DETERMINISTIC / HASH_MATCH / SPEEDUP` block;
`OUTPUT_HASH` is the `rlang::hash` of the discovered secondary library.

This tier **draws random numbers**, so the gate is the LCC-discovery / MCMC kind:
preserve the exact draw sequence, keep the hash bit-identical.

## Usage

```bash
Rscript dev/bench/secondary_bench.R            # measure, compare to secondary_baseline.txt
Rscript dev/bench/secondary_bench.R --capture  # (re)write secondary_baseline.txt from current code
```

## The fence

Only **Class A** changes that preserve the random-draw sequence. The target is the
discovery implementation in `R/mcmc_bfs_utils.R` (`bfs_grow_block` + the `ctx` fast
path), `R/mcmc_parcel_library.R` (`run_bfs_secondary_supplement`, `combine_*`,
`build_secondary_library_from_discovery`), and `R/mcmc_spanning_tree.R`
(`discover_secondaries_from_trees`) — but **not** the `sample_spanning_tree` draw.
Do **not** edit `dev/bench/`, `secondary_baseline.txt`, the seed, the store, or the
secondary size constants in `inst/targets/temp_targets_*.R`.

See `SECONDARY_HOTSPOTS.md`: `run_bfs_secondary_supplement` calls `bfs_grow_block`
on the **slow character path** (no `bfs_build_context`), so a per-BFS-step
`igraph::neighbors()$name` lookup is 47% of the run. The fix — wiring in the
integer `ctx` fast path already built and tested for the LCC tier — is the
dominant lever; a realistic target is ~3–4×.

# Parcel graph construction bench

A fixed-input, bit-identical benchmark for **behavior-preserving** speedups of the
**parcel graph construction tier** — Tier 2 in `inst/targets/_targets.R`, which
turns the loaded district geometry into the adjacency graph every later tier
consumes.

## What it does

`graph_bench.R` loads the warm store for the one fixed input the tier consumes —
`district_data` (its `$district_geometry` and `$district_right_of_way`) — then
rebuilds the graph **twice** (`build_adjacency_graph` →
`build_identity_parcel_graph`, the `MACRO_SCALE == 0` branch) and prints the same
`ELAPSED / OUTPUT_HASH / DETERMINISTIC / HASH_MATCH / SPEEDUP` block. The graph is
hashed via its canonical vertex + edge data frames (`as_data_frame(pg, "both")`)
so the gate is over graph content, not igraph-internal representation.

This tier is **RNG-free**, so the gate is the compliance kind: preserve the
computed graph content, keep the hash bit-identical.

## Usage

```bash
Rscript dev/bench/graph_bench.R            # measure, compare to graph_baseline.txt
Rscript dev/bench/graph_bench.R --capture  # (re)write graph_baseline.txt from current code
```

## The fence

Only changes that keep the graph bit-identical. The target is
`R/mcmc_graph_building.R` (`build_adjacency_graph` and its nearest-point / ROW
helpers) and `R/mcmc_parcel_construction.R` (`build_identity_parcel_graph`). Do
**not** edit `dev/bench/`, `graph_baseline.txt`, the store, or the Tier-2 constants
(`MAX_DIST_FEET`, `MIN_COVERAGE_RATIO`, `MACRO_SCALE`).

See `GRAPH_HOTSPOTS.md`: two helpers call vectorizable `sf` functions one element
at a time in an R loop — `build_nearest_point_lines` (`st_nearest_points` +
`st_length` per pair, 53%) and `validate_row_crossing_lines` (`st_length` per
feature, 26%). Batching each into a single vectorized `sf` call is the dominant
lever (~79% of the build), well above a 3× ceiling.

# Density precompute bench

A fixed-input benchmark for **behavior-preserving** speedups of the
density-deduction precompute — the single most expensive spatial step in the
compliance workflow. Unlike the other five, this one uses a **tolerance gate**,
not a hash: the legitimate speedup changes floating-point geometry math by
sub-square-foot amounts.

## What it does

`density_bench.R` loads one municipality (Maynard, 3,467 parcels) and the
statewide deduction layer once (untimed), then times
`precompute_spatial_attributes(..., density_deductions = )` **twice** and compares
the per-parcel `density_deduction_area` vector against the captured baseline (in
`density_baseline.rds`) **element-wise within 1 sqft**. It prints:

```
ELAPSED       min wall-clock of one density precompute (s)
N_AFFECTED    parcels with non-zero deduction (must match baseline exactly)
TOTAL_SQFT    sum of per-parcel deduction area
MAX_ABS_DIFF  max per-parcel |area - baseline|  (the tolerance signal)
WITHIN_TOL    MAX_ABS_DIFF <= 1 sqft AND N_AFFECTED matches  (the gate)
SPEEDUP       baseline_elapsed / ELAPSED
```

`DETERMINISTIC` (the two runs agree exactly) still guards against nondeterminism.

## Usage

```bash
Rscript dev/bench/density_bench.R            # measure, compare to density_baseline.{txt,rds}
Rscript dev/bench/density_bench.R --capture  # (re)write density_baseline.{txt,rds} from current code
```

## The fence

Edit only the density compute path: the density branch of
`precompute_spatial_attributes()` and (if a helper is shared)
`calculate_density_denominator()` in `R/gis_operations.R`. Keep `WITHIN_TOL: TRUE`
and `DETERMINISTIC: TRUE`. Do **not** edit `dev/bench/`,
`density_baseline.{txt,rds}`, `TOL_SQFT`, the municipality, or the deduction layer.
(The compliance bench treats `precompute_spatial_attributes` as fixed input; here
it is the thing under test — the two scopes, station vs. density overlap, are
disjoint.)

See `DENSITY_HOTSPOTS.md`: 95% of the time is one line —
`st_intersection(parcels, st_union(deductions))` intersects every parcel against
one unioned statewide multipolygon, defeating the spatial index. A bounding-box
prefilter (`st_intersects` / `st_filter`) before the heavy intersection is the
dominant lever; the per-parcel areas come out identical up to the 1-sqft
tolerance.
