# LCC discovery bench — profile / hotspots

Profile of the LCC discovery hot path: one full discovery run (Topsfield, 2,316
vertices, the Tier-3A chain `discover_lccs_from_trees` → `run_bfs_lcc_supplement`
→ 5× `discover_lccs_single_band` → `combine_*` → `build_lcc_library_from_tree_
discovery`), `Rprof` line-profiling, 38.5 s sampled, seed 42 (commit `ae9441a`).

Unlike the compliance bench, this path **draws random numbers** (Wilson tree
sampling, BFS seed/step selection). The gate is the same kind as the MCMC sampler
bench: under a fixed seed the run is reproducible, so a Class A speedup must keep
`OUTPUT_HASH` **exactly** `8bc9cf594d62f602d58d9adbe25c3687`. The fence is
**preserve the random-draw sequence** — change *how* the work is done, never
*which* random draws happen or *in what order*. Confirm with
`Rscript dev/bench/discovery_bench.R` after each change.

## Hotspots by self-time

Almost everything is in **`bfs_grow_block()`** (`R/mcmc_bfs_utils.R`), called
thousands of times (BFS supplement + 5 capacity bands × up to 1,000 attempts),
re-doing igraph neighbor lookups and character-vector set ops on **every BFS
step**.

| % self | where | what | Class A fix |
|---:|---|---|---|
| **56.6%** | `mcmc_bfs_utils.R#83` | `new_nbrs <- igraph::neighbors(graph, next_node)$name` — an igraph C-call **plus** `$name` attribute materialization, **once per BFS step**. This single line is the whole ballgame. | Precompute the name-keyed adjacency list **once per graph** (e.g. `igraph::as_adj_list(graph, mode = "all")` mapped through `V(graph)$name`, or a `setNames(lapply(...neighbors...$name), names)` — the exact pattern already used at `mcmc_parcel_library.R#1798`) and pass it into `bfs_grow_block`; replace the per-step call with a list lookup. **Must reproduce `neighbors(graph, v)$name` in the same order** — `new_nbrs` flows into `unique(c(frontier, new_nbrs))`, so a reordering changes which element `sample.int` picks and moves the hash. |
| 11.3% | `mcmc_bfs_utils.R#85` | `new_nbrs <- intersect(new_nbrs, eligible_pool)` — `intersect()` (a `match` + `unique`) on character vectors, per step | Represent `eligible_pool` as a fast membership set (precomputed logical keyed by node, or integer ids) and filter `new_nbrs[new_nbrs %in% eligible]` **after** the existing `unique`, preserving `intersect`'s order semantics. |
| 4.7% | `mcmc_bfs_utils.R#58` | `frontier <- igraph::neighbors(graph, seed)$name` — same igraph cost, once per seed | Falls out of the same precomputed-adjacency lookup as #83. |
| 4.5% | `mcmc_spanning_tree.R#516` | `igraph::sample_spanning_tree()` (Wilson's algorithm) in tree discovery | igraph-internal and RNG-bearing — **leave it**; reordering or reimplementing changes the draw sequence. Treat as floor. |
| 3.8% | `mcmc_parcel_library.R#1799` | `neighbor_cache <- setNames(lapply(all_parcels, \(m) igraph::neighbors(parcel_graph, m)$name), all_parcels)` — building the library's neighbor cache (deterministic, once per build) | Already a one-time cache; the same structure is what `bfs_grow_block` should reuse. Could be built **once** and shared between discovery and library assembly instead of rebuilt. |
| ~6% | `mcmc_bfs_utils.R#84, #86, #60, #66, #35, #43, #65` | `setdiff(new_nbrs, current_block)`, `unique(c(frontier, new_nbrs))`, `intersect(frontier, eligible_pool)`, `metric_lookup[next_node]` (named-vector lookup), `V(graph)$name` / `intersect(seed_pool, eligible_pool)` recomputed every call | Integer-index the block/frontier/eligible sets and `metric_lookup` (name → position once), so the inner loop is integer set ops instead of repeated character `match`. Hoist the per-call invariants (`all_nodes`, `valid_seeds`) out of the hot path. Each is small alone; together ~6%. |
| ~1.2% | `mcmc_parcel_library.R#1767–1776` | four separate `vapply(blocks, …)` passes over igraph vertex attributes (`area_in_station`, `capacity_in_station`, `centroid_x`, `centroid_y`) in `build_lcc_library_from_tree_discovery` | Extract all four attributes in **one** pass over blocks (or vectorize the vertex-attribute reads), deterministic. Minor — revisit only after the BFS lines land. |

## Read of the headroom

The igraph neighbor materialization (lines 83 + 58 ≈ **61%**) is the prize, and it
is a textbook Class A win: the *set* of neighbors of a fixed graph never changes,
so computing it once and looking it up is behavior-identical — **provided the
cached order matches `neighbors()$name`**, which the hash gate verifies directly.
Cutting that 61% toward zero sets a ceiling near **2.5×** on its own
(`1 / (1 - 0.61)`); folding in the character set-op vectorization (lines 85/84/86/60
≈ 18%) and integer-indexing the lookups pushes a realistic target to **~3–4×**.

Beyond that the floor is the Wilson spanning-tree sampler (igraph-internal, RNG —
untouchable here) and the irreducible BFS arithmetic repeated across thousands of
samples. The deterministic library-assembly tail (`build_lcc_library_*`,
`combine_*`) is only a few percent — real but not where the time is.

## The fence

Edit only the discovery compute path:
- `R/mcmc_bfs_utils.R` (`bfs_grow_block` and helpers)
- `R/mcmc_parcel_library.R` (`run_bfs_lcc_supplement`, `discover_lccs_single_band`,
  `combine_*`, `build_lcc_library_from_tree_discovery`)
- `R/mcmc_spanning_tree.R` (`discover_lccs_from_trees` and its helpers) — but **not**
  the `sample_spanning_tree` draw itself

Do **not** edit the bench harness (`dev/bench/`), `discovery_baseline.txt`, the
seed, the store, or the discovery size constants in
`inst/targets/temp_targets_config.R` / `temp_targets_parcel_config.R`
(`TREE_LCC_N_TREES`, `BFS_LCC_*`, `LCC_CAPACITY_BANDS_RELATIVE`,
`LCC_BAND_*`, `LCC_LIBRARY_MAX_SIZE`, `BFS_RESERVATION_LCC`) — those are the
bench's fixed input, not the thing under test.

This path **is** RNG-bearing, so the rule is the MCMC-sampler rule, not the
compliance rule: preserve the exact draw sequence. Keep `HASH_MATCH: TRUE` and
`DETERMINISTIC: TRUE` on every run; if the hash moves, a draw changed (or a cached
order diverged from `neighbors()`) — revert.
