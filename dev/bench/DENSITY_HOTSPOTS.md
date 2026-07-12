# Density precompute bench — profile / hotspots

Profile of the density-deduction precompute hot path: one
`precompute_spatial_attributes(parcels, density_deductions = )` over Maynard
(3,467 parcels) against the statewide `Density_Denominator_Deductions` layer
(2,834 features), component timing (seed-independent — this path is RNG-free).

## The gate is a TOLERANCE gate, not a hash

Unlike the other three benches, this one does **not** demand a bit-identical
hash. The step produces geometry, and the obvious speedup (prefilter parcels /
deductions before the heavy intersection) changes the floating-point
aggregation order by sub-square-foot amounts. So the gate is a **per-parcel
tolerance** check: `max |this_area - baseline_area| <= 1.0 sqft` (the same 1-sqft
tolerance the package's regression tests use), plus an exact match on the count of
affected parcels. `DETERMINISTIC` (the two runs agree exactly) still has to hold —
a tolerance gate forgives floating-point drift from a *different* algorithm, not
nondeterminism within the *same* one.

## Hotspots by component (Maynard, one run ≈ 99 s)

| seconds | %    | step | what |
|--------:|-----:|------|------|
| **94.1** | **95%** | `gis_operations.R` `st_intersection(parcels, deduction_union)` | Intersect **every** parcel against ONE unioned statewide multipolygon. Unioning the deductions into a single geometry collapses the layer's STRtree, so each parcel is tested against the whole blob instead of just the few deduction features near it. This single line is the whole ballgame. |
| 3.2 | 3% | `st_union(density_deductions)` | Build the single deduction multipolygon. |
| 1.5 | 2% | `st_make_valid(density_deductions)` | Repair statewide geometry. |
| ~0.1 | <1% | per-parcel area sum loop | `for` loop assigning `intersection_areas` back to parcels by `rownames`. |

The same shape holds at Cambridge scale (12,867 parcels): the intersection grows
roughly with parcel count and is the ~12-minute step the compliance batch bench
deliberately skips.

## Read of the headroom

The prize is the 95% line. The standard fix is to **keep the spatial index
working**: instead of `st_intersection(parcels, st_union(deductions))`, first
cheaply narrow the candidate set — e.g. `hits <- st_intersects(parcels,
deductions)` (sparse, STRtree-backed) or `st_filter` by bounding box — then
intersect only the parcels that actually touch a deduction, and (optionally)
against the un-unioned deduction features so GEOS prunes by envelope. On Maynard
only a few hundred parcels touch any deduction, so the intersection workload drops
by ~an order of magnitude. The per-parcel deduction areas come out identical up to
floating point, which the 1-sqft tolerance gate is built to accept.

Beyond the prefilter, the union + make_valid (5%) are one-time and minor; the
write-back loop is trivial and can be vectorized (`tapply`/`data.table`) but is
not where the time is.

## The fence

Edit only the density compute path:
- the density branch of `precompute_spatial_attributes()` in `R/gis_operations.R`
- `calculate_density_denominator()` in `R/gis_operations.R`, if a shared helper is
  factored out (it has the same `st_union` → `st_intersection` structure at the
  district level)

This path is RNG-free, so the rule is the compliance-bench rule (preserve the
computed values), **relaxed to a tolerance** because the geometry math is
order-sensitive: keep `WITHIN_TOL: TRUE` (max per-parcel diff ≤ 1 sqft) and
`N_AFFECTED_MATCH: TRUE` and `DETERMINISTIC: TRUE` on every run. If the affected
count changes, or a parcel's area moves by more than a square foot, behavior
changed — that is a bug, not a speedup.

Do **not** edit the bench harness (`dev/bench/`), `density_baseline.{txt,rds}`,
`TOL_SQFT`, the municipality, or the deduction layer while a speed goal is active —
they are the bench's fixed input, not the thing under test.
