# Parcel graph construction bench — profile / hotspots

Profile of the Tier-2 graph-construction hot path: one full build on Topsfield
(2,316 parcels, 29 right-of-way features) — `build_adjacency_graph` →
`build_identity_parcel_graph` — `Rprof` line-profiling, ~95 s sampled (2 reps).
This tier is **RNG-free**, so the profile is seed-independent.

The gate is the compliance-bench kind: a correct speedup LOWERS the time while
keeping `OUTPUT_HASH` **exactly** `ecdc71d95c4fc48b812e6b86a75de505` (the
`rlang::hash` of the graph's canonical vertex + edge data frames). No draw
sequence to preserve — just the computed graph content. `DETERMINISTIC: TRUE`
guards against an optimization that introduces nondeterminism (e.g. parallelism).

## Hotspots by self-time

Almost all the time is in two helpers in `R/mcmc_graph_building.R`, both of which
call **vectorizable `sf` functions one element at a time inside an R loop**.

| % self | where | what | Class A fix |
|---:|---|---|---|
| **32.9%** | `mcmc_graph_building.R#37` | `len <- as.numeric(sf::st_length(sf::st_sfc(line, crs = crs)))` inside `build_nearest_point_lines`'s per-pair `lapply` — one `st_sfc` construction + one `st_length` **per candidate pair**. | `st_length` is vectorized over an sfc. Build all nearest-point lines first, then call `st_length` **once** on the whole sfc. Same lengths, no per-pair sfc allocation. |
| 26.1% | `mcmc_graph_building.R#90` | `sum(as.numeric(sf::st_length(row_intersections[i])))` inside `validate_row_crossing_lines`'s `vapply` — per-feature `st_length` over the ROW-intersection geometries. | `st_length(row_intersections)` returns per-feature length for the whole sfc in one call; aggregate MULTILINESTRING/GEOMETRYCOLLECTION pieces with the vectorized result instead of a per-i `sum`. |
| 20.2% | `mcmc_graph_building.R#36` | `line <- sf::st_geometry(sf::st_nearest_points(from_geoms[i], to_geoms[i]))[[1]]` — `st_nearest_points` called **one pair at a time** in the same `lapply`. | `st_nearest_points(from_sfc, to_sfc, pairwise = TRUE)` computes all nearest-point lines in a single vectorized call. Combined with the #37 fix this collapses the whole `lapply` into ~2 vectorized calls. |
| 5.9% | `mcmc_graph_building.R#137` | edge-table assembly from candidate pairs | Minor; revisit after the two big loops. |
| 5.2% | `mcmc_graph_building.R#256` | `sf::st_distance(from_geoms, to_geoms, by_element = TRUE)` — already vectorized boundary-to-boundary distance for edge filtering | Already batched; largely irreducible GEOS cost. |
| 4.6% | `mcmc_graph_building.R#87` | `sf::st_intersection(crossing_lines, row_union)` — ROW-crossing intersection (already batched over crossing lines) | Already vectorized; irreducible. |
| 2.5% | `mcmc_graph_building.R#81` | `sf::st_intersects(lines_sfc, row_union, sparse = FALSE)` — ROW crossing pre-filter | Already vectorized; irreducible. |

## Read of the headroom

The prize is `build_nearest_point_lines` (lines 36 + 37 ≈ **53%**): it is a
per-pair `lapply` that calls `st_nearest_points` and `st_length` (each wrapped in
its own `st_sfc`) once per candidate edge. Both functions are vectorized over
geometry pairs / over an sfc, so the entire loop becomes roughly:
`lines <- st_nearest_points(from_sfc, to_sfc, pairwise = TRUE); lengths <-
st_length(lines)`. That removes thousands of single-element `sf` round-trips and
sfc allocations while producing the **same** lines and lengths — a textbook Class
A batching win. The `validate_row_crossing_lines` per-feature `st_length`
(line 90 ≈ **26%**) is the same shape: one vectorized `st_length` over the whole
intersection sfc.

Together those two loops are ~**79%** of the build, so eliminating the per-element
overhead sets a ceiling well above **3×**. Floats must stay bit-identical for the
hash gate — vectorized `st_length` / `st_nearest_points` call the same GEOS
routines as the per-element form, so the lengths and coordinates should match
exactly; verify with the bench after each change and, if a vectorized call ever
reorders or merges geometries, hash a canonicalized extract rather than relax the
gate.

The remaining spatial ops (`st_distance`, `st_intersection`, `st_intersects` ≈
12%) are already vectorized and are largely irreducible GEOS cost — the floor.

## The fence

Edit only the construction compute path:
- `R/mcmc_graph_building.R` (`build_adjacency_graph`, `build_nearest_point_lines`,
  `validate_row_crossing_lines`, and the ROW helpers)
- `R/mcmc_parcel_construction.R` (`build_identity_parcel_graph`,
  `build_parcel_graph_target`)

Do **not** edit the bench harness (`dev/bench/`), `graph_baseline.txt`, the store,
or the Tier-2 constants in `inst/targets/temp_targets_*.R` (`MAX_DIST_FEET`,
`MIN_COVERAGE_RATIO`, `MACRO_SCALE`) — those are the bench's fixed input, not the
thing under test.

This path is RNG-free, so the rule is the compliance-bench rule: preserve the
computed graph content. Keep `HASH_MATCH: TRUE` and `DETERMINISTIC: TRUE` on every
run; if the hash moves, the graph changed — that is a bug, not a speedup.
