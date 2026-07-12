# Secondary discovery bench — profile / hotspots

Profile of the secondary-discovery hot path: one full Tier-3B chain on Topsfield
(2,316 vertices) — `discover_secondaries_from_trees` →
`run_bfs_secondary_supplement` → `combine_discovered_blocks` →
`build_secondary_library_from_discovery` — `Rprof` line-profiling, ~36 s sampled,
seed 42.

This is the **sibling of the LCC discovery tier** (`DISCOVERY_HOTSPOTS.md`). It
uses the same machinery — Wilson spanning-tree enumeration and BFS growth via
`bfs_grow_block` — but the LCC tier was already converted to the integer fast
path (`bfs_build_context` / `bfs_grow_block_ctx`) while the secondary tier was
not. So the profile here looks like the LCC tier looked *before* its speedup: the
time is in the slow CHARACTER body of `bfs_grow_block`.

This path **draws random numbers** (Wilson tree sampling; per-attempt
`runif(1, min_area, max_area)`; BFS seed/step selection). The gate is the
MCMC-sampler / LCC-discovery kind: under a fixed seed the run is reproducible, so
a Class A speedup must keep `OUTPUT_HASH` **exactly**
`5f7de7d9fc08346bf96b9e1f801d5ff4`. The fence is **preserve the random-draw
sequence** — change *how* the work is done, never *which* draws happen or *in what
order*. Confirm with `Rscript dev/bench/secondary_bench.R` after each change.

## Hotspots by self-time

`run_bfs_secondary_supplement` (`R/mcmc_parcel_library.R:1493`) calls
`bfs_grow_block` with **no `ctx`**, so it runs the original character-vector body
(`R/mcmc_bfs_utils.R`), re-doing igraph neighbor lookups and character set ops on
**every BFS step** across 3 size bands × up to 500 attempts.

| % self | where | what | Class A fix |
|---:|---|---|---|
| **47.4%** | `mcmc_bfs_utils.R#289` | `new_nbrs <- igraph::neighbors(graph, next_node)$name` — igraph C-call **plus** `$name` materialization, **once per BFS step**. The whole ballgame, exactly as in the LCC tier. | The fix already exists. Build `bfs_ctx <- bfs_build_context(parcel_graph, area_lookup, eligible_pool = NULL)` once in `run_bfs_secondary_supplement`, then call `bfs_grow_block(ctx = bfs_ctx, ...)` so it dispatches to `bfs_grow_block_ctx()` (integer adjacency lookup). Already proven hash-identical by `tests/testthat/test-mcmc-bfs-utils.R`. |
| 10.4% | `mcmc_spanning_tree.R#931` | tree-cut enumeration inside `discover_secondaries_from_trees` (deterministic per draw, but heavy) | Secondary — revisit after the BFS line. Some is irreducible cut bookkeeping. |
| 8.6% | `mcmc_bfs_utils.R#291` | `new_nbrs <- intersect(new_nbrs, eligible_pool)` — character `intersect` per step | Falls out of the same `ctx` conversion: the context carries an integer/logical eligibility mask, so this becomes an integer filter. |
| 8.0% | `mcmc_bfs_utils.R#264` | `frontier <- igraph::neighbors(graph, seed)$name` — same igraph cost, once per seed | Same precomputed-adjacency lookup as #289. |
| 4.3% | `mcmc_spanning_tree.R#47` | `sample_spanning_tree` / Wilson helper (RNG-bearing) | **Leave it** — igraph-internal and draw-sequence-defining. Treat as floor. |
| ~9% | `mcmc_bfs_utils.R#241, #249, #266, #290, #271, #272` | per-call invariants `V(graph)$name` (#241) and `intersect(seed_pool, eligible_pool)` (#249) recomputed every call; `intersect(frontier, eligible_pool)` (#266); `setdiff(new_nbrs, current_block)` (#290); `sample.int` + `metric_lookup[next_node]` (#271/#272) | All subsumed by the `ctx` conversion: invariants are hoisted into `bfs_build_context`, and the block/frontier/eligible sets become integer-indexed so the inner loop is integer set ops instead of character `match`. |

## Read of the headroom

The igraph neighbor materialization (lines 289 + 264 ≈ **55%**) is the prize, and
it is the same textbook Class A win already banked for the LCC tier: compute the
name-keyed adjacency **once per graph** and look it up. Because
`bfs_build_context` / `bfs_grow_block_ctx` and their equivalence tests already
exist, this is largely a **wiring change** in `run_bfs_secondary_supplement` (and
the analogous bare `bfs_grow_block` call at `mcmc_parcel_library.R:2756`), not new
algorithm work. Cutting the 55% toward zero sets a ceiling near **2.2×** on its
own (`1 / (1 - 0.55)`); folding in the character set-op lines (#291/#266/#290 ≈
12%) and integer-indexing pushes a realistic target to **~3–4×**, mirroring the
LCC result (~6.8× there, where BFS was a larger share).

Beyond that the floor is the Wilson spanning-tree sampler
(`mcmc_spanning_tree.R`, igraph-internal RNG — untouchable here) and the tree-cut
enumeration, together ~15%.

## The fence

Edit only the secondary-discovery compute path:
- `R/mcmc_bfs_utils.R` (`bfs_grow_block` and the `ctx` fast path — already built)
- `R/mcmc_parcel_library.R` (`run_bfs_secondary_supplement`,
  `combine_discovered_blocks`, `build_secondary_library_from_discovery`)
- `R/mcmc_spanning_tree.R` (`discover_secondaries_from_trees` and helpers) — but
  **not** the `sample_spanning_tree` draw itself

Do **not** edit the bench harness (`dev/bench/`), `secondary_baseline.txt`, the
seed, the store, or the secondary-discovery size constants in
`inst/targets/temp_targets_*.R` (`SEC_SIZE_BANDS`, `TREE_SEC_N_TREES`,
`BFS_SEC_QUOTA_PER_BAND`, `LIBRARY_DENSITY_THRESHOLD`, `SEC_LIBRARY_MAX_SIZE`,
`BFS_RESERVATION_SEC`) — those are the bench's fixed input, not the thing under
test.

This path **is** RNG-bearing, so the rule is the LCC-discovery rule: preserve the
exact draw sequence. Keep `HASH_MATCH: TRUE` and `DETERMINISTIC: TRUE` on every
run; if the hash moves, a draw changed (or a cached order diverged from
`neighbors()`) — revert.
