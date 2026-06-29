# MCMC speed bench

A fixed-seed, bit-identical benchmark for **behavior-preserving** speedups of the
parcel MCMC sampler. Built to drive a `/goal` loop (see "Using with `/goal`").

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
