# MCMC speed bench — profile / hotspots

Profile of one Topsfield chain, 6,000 steps, `Rprof` interval 0.01s, 42.9s sampled
(commit `b5d76d2`). All items below are **Class A** (deterministic, RNG-preserving)
— fixing them must keep `OUTPUT_HASH` constant. Confirm with `Rscript
dev/bench/speed_bench.R` after each change.

## Call tree (by total time)

```
run_parcel_mcmc                              99.7%
└─ replace_lcc_move   (runner.R#800)         50.4%   <- dominant kernel (p_replace_lcc = 0.455)
   └─ reset_to_lcc    (state.R#891)          42.9%
      └─ lcc_neighbor_counts vapply (state.R#912-915)  ~39% SELF
```

## Hotspots by self-time

| % self | where | what | Class A fix |
|---:|---|---|---|
| ~39% | `state.R#912-915` | per-parcel neighbor-in-LCC count via `vapply` over ~2,300 parcels with **named-list `[[pname]]` lookups** and **named-vector subsetting** | precompute an **integer-indexed** neighbor list (drop name→hash lookups); strip names off `lcc_logical` before `sum`; or vectorize the whole count (e.g. `tabulate` over neighbor edges / sparse matrix-vector) |
| 18% | data.table `forderv` (`[.data.table` 22% total) | repeated ordering in the reset / replace-LCC path | precompute/cache the order, or replace with a base radix sort on integer keys; avoid re-sorting invariant data each call |
| 9.6% | `proc.time` | per-step kernel timing: `proc.time()[3]` twice per step (runner.R#571, #893) | time in coarser batches, or gate behind a debug flag. Feeds `stats`, **not** `parcel_samples`, so hash is unaffected |
| 8.8% | `<GC>` | allocation pressure from named-vector subsetting and per-call rebuilds | falls out of the fixes above (fewer allocations/copies) |
| 7.5% | `induced_subgraph_impl` | igraph subgraph rebuilt inside the hot path | cache, or compute the needed quantity from precomputed adjacency without rebuilding a subgraph |

## Read of the headroom

The dominant cost is `reset_to_lcc` rebuilding neighbor counts on every replace-LCC
proposal (~45% of steps) using slow R idioms over deterministic data — implementation
overhead, not algorithm. The `vapply` alone (~39% self) plus the per-step `proc.time`
(~9.6%) are the two fattest, safest targets. Fixing the `vapply` and the timing should
reach ≈1.6×; adding the data.table ordering and the consequent GC reduction makes
≈1.8–2× plausible. The 25% milestone (1.33×) is conservative.

## Going further: structural Class A wins (needed for >2×)

The line-level fixes above are constant-factor. Capturing **everything** the current
profile sees bottoms out around 3.5–4s (~4×). To go beyond that, attack the
*recomputation itself*, not its constant — these are still bit-identical (same values,
zero RNG), so `OUTPUT_HASH` must stay constant; verify with the bench after each.

1. **Precompute & cache per-library-LCC neighbor structures (biggest lever).**
   `replace_lcc_move` selects an LCC from the **fixed** LCC library. The per-parcel
   neighbor-in-LCC counts `reset_to_lcc` rebuilds are a pure function of (library LCC,
   graph) — independent of chain state. Compute them **once per library entry** (at
   library build/hydrate) and have `reset_to_lcc` look them up for library LCCs instead
   of recomputing. Near-eliminates the ~39%-self loop on the path that is 50% of runtime.
   Check the callers of `reset_to_lcc`: the lookup applies to library LCCs (the replace
   path); non-library LCCs still need the fast vectorized rebuild from item 1 above.

2. **Cache the data.table ordering (18%) and the igraph subgraph (7.5%)** keyed by LCC
   id in the same library cache — both are invariant per LCC across proposals.

3. **Re-profile after 1–2 land.** The hotspot will shift to the core step logic
   (MH-ratio, proposal draws, state updates). That is closer to the Class A floor — the
   irreducible cost of executing the same N steps — so expect diminishing returns there.

Rough ceiling: items 1–2 make ~3–4× plausible; 5×+ (≤3.3s) requires those *plus* wins
in the core step logic and is a genuine stretch within the Class A fence.
