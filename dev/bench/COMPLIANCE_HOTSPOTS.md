# Compliance batch bench — profile / hotspots

Profile of the compliance batch hot path: 120 `evaluate_compliance(precomputed =
TRUE)` calls over Cambridge (12,867 parcels), single district, `Rprof`
line-profiling, 37.6 s sampled (commit `34a8206`). Every item below is **Class A**
(deterministic, RNG-free) — this path draws no random numbers at all, so fixing
these must keep `OUTPUT_HASH` **exactly** `f1863cb592a4a710c694b5e3ad62d5ba`.
Confirm with `Rscript dev/bench/compliance_bench.R` after each change.

## Hotspots by self-time

| % self | where | what | Class A fix |
|---:|---|---|---|
| **84.6%** | `compliance_pipeline.R#1264-1267` | `parcels_with_capacity[<logical>, ]` — **subsetting the full sf object (with geometry) by a logical mask** inside the per-district metrics `lapply`. The `[.sf` method reprocesses the geometry list-column on every call; the metrics it feeds (`total_units`, `total_acres`, `developable_acres`, station sums, `density_deduction_area`, `nrow`) use **only non-geometry columns**. | Drop geometry **once** before the metrics loop (`md <- sf::st_drop_geometry(parcels_with_capacity)`) and subset/aggregate that plain data.frame (or data.table). The returned `result$parcel_detail` keeps geometry — only the internal metrics computation needs the light copy. This is the dominant lever. |
| 7.5% | `unit_capacity_calculations.R#168-175` | `purrr::pmap_dbl()` row-by-row min across 7 unit-method columns with per-row NA→Inf handling, in `calculate_final_unit_capacity()`. ~13K rows × calls. | Vectorize: `pmin(building_capacity, density_limits, …, na.rm = TRUE)` over the columns, then restore `NA` for the all-NA rows (so the all-NA→`NA_real_` branch is preserved). Same values, no per-row R closure. |
| ~2.8% | `unit_capacity_calculations.R#169-174` | the per-row closure body (`c(...)`, `is.na`, `min`) — same `pmap` call as above | falls out of vectorizing #168 |
| <1% each | `compliance_pipeline.R#1332`, `#598`, `#1351`, `#1368`, assorted | `do.call(rbind, lapply(..., as.data.frame))` for `by_district`, requirement lookups, list assembly | minor here (single district → 1-row rbind); `data.table::rbindlist` is tidier but not where the time is. Revisit only after the two above land. |

## Read of the headroom

One line is 85% of the cost: subsetting a 12,867-row **sf** (geometry and all) when
only numeric columns are needed. Dropping geometry before the metrics loop turns an
`[.sf` geometry rebuild into a cheap data.frame subset and, on its own, sets a
ceiling near **6-7×** (`1 / (1 - 0.85)`). Vectorizing the `pmap_dbl` min (≈10% once
the sf cost is gone) takes it further. After both land, time shifts to the
irreducible arithmetic + per-call list assembly — the Class A floor for running the
same N evaluations.

Rough ceiling: the sf fix alone makes ~5× plausible; sf + the `pmin` vectorization
makes ~6-8× plausible. Beyond that is per-call overhead (district assignment,
requirement lookup, result-list construction) repeated N times — diminishing returns.

## The fence

Edit only the per-iteration compute path:
- `R/compliance_pipeline.R` (`evaluate_compliance`, `calculate_district_capacity`)
- `R/unit_capacity_calculations.R` (the `calculate_*` functions)
- `precomputed = TRUE` branches of `R/gis_operations.R`, if needed

Do **not** edit the one-time setup — `load_municipality()`,
`precompute_spatial_attributes()`, `load_transit_stations()`,
`create_zoning_parameters()` — those are the bench's fixed input, not the thing
under test. Do not edit `dev/bench/` or `compliance_baseline.txt`.

The output is RNG-free, so unlike the MCMC bench there is no draw-sequence to
preserve — the only rule is that the numbers don't change. Keep `HASH_MATCH: TRUE`
and `DETERMINISTIC: TRUE` on every run; if the hash moves, the values changed —
revert.
