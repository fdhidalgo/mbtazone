# MCMC Diagnostics Summary (LLM)


# MCMC Diagnostics Summary

## Overview

- Total Chains: 6
- Total Steps: 30000 (5000 per chain)

## Current Tuning

- Capacity prior λ: 0.005
- Secondary-count prior λ: 0.5
- Secondary minimum area: 5 acres
- Replace-LCC capacity tolerance: 800
- Joint core p_keep:
- Joint LCC excess rate:
- Joint add weight (nothing):
- Swap capacity tolerance: 150

## Graph Statistics

- Parcels: 5129
- Parcel units: 5129 (compression: 1x)
- Parcel edges: 1.7026^{4}
- Avg degree: 6.6

## Library Coverage

### Secondary Library

| Size Band | Count |
|:----------|------:|
| band_1    |   190 |
| band_2    |   153 |
| band_3    |   157 |

### LCC Library

- Candidates: 5000
- Capacity range: 512 - 2554

### LCC Capacity: Library vs Sampled

*Compares library capacity distribution to sampled distribution. A
discrepancy reflects the posterior distribution shape, not proposal bias
(proposals are uniform).*

| Metric                  | Library | Sampled |
|:------------------------|:--------|:--------|
| Mean capacity           | 1362    | 1037    |
| Median capacity         | 1272    | 1039    |
| High-capacity (≥1022) % | 65.0%   | 57.2%   |


    **Capacity ratio (sampled/library): 0.76**

    ℹ️ **Posterior favors lower-capacity LCCs**: This is expected behavior.
    The min_lcc_fraction constraint (≥50%) limits how many secondaries can be added
    with high-capacity LCCs. Lower-capacity LCCs allow more secondary configurations,
    so the posterior has more mass there. This is not a proposal bias.

### LCC Library Capacity Bands

*Distribution of LCC capacities in the library relative to
min_capacity.*

| Capacity Band            | N LCCs | % of Library |
|:-------------------------|-------:|:-------------|
| \< min_capacity (1022)   |   1750 | 35.0%        |
| \[min_cap, 1.25×min_cap) |    760 | 15.2%        |
| \[1.25×, 1.5×min_cap)    |    740 | 14.8%        |
| \[1.5×, 2×min_cap)       |    982 | 19.6%        |
| \>= 2×min_capacity       |    768 | 15.4%        |

### Library Accessibility by Secondary Count (k)

*The min_lcc_fraction constraint requires LCC capacity \>= total
secondary capacity. As k increases, fewer LCCs are accessible.*

|   k | Est. Sec. Cap. | Min LCC Required | N Accessible | % Library |
|----:|---------------:|-----------------:|-------------:|:----------|
|   0 |          0.000 |            0.000 |         5000 | 100.0%    |
|   1 |        183.382 |          183.382 |         5000 | 100.0%    |
|   2 |        366.764 |          366.764 |         5000 | 100.0%    |
|   3 |        550.146 |          550.146 |         4916 | 98.3%     |
|   4 |        733.528 |          733.528 |         4449 | 89.0%     |
|   5 |        916.910 |          916.910 |         3773 | 75.5%     |
|   6 |       1100.292 |         1100.292 |         3007 | 60.1%     |

### Posterior-Weighted Library Accessibility

*Average library accessibility weighted by the observed k distribution.*

- Mean secondary block capacity (library): 183
- Posterior-weighted library accessibility: 99.7%

### Sampled LCC Capacity by k

*Observed relationship between secondary count and LCC capacity in
sampled states.*

|   k | N Samples | Mean LCC Cap | Min LCC Cap | Max LCC Cap |
|----:|----------:|-------------:|------------:|------------:|
|   0 |     12981 |         1168 |        1022 |        2351 |
|   1 |     10051 |          994 |         700 |        2370 |
|   2 |      4851 |          868 |         542 |        2422 |
|   3 |      1669 |          827 |         536 |        2190 |
|   4 |       391 |          822 |         529 |        2278 |
|   5 |        46 |          911 |         654 |        2190 |
|   6 |        11 |         1193 |         862 |        2190 |

- Correlation(k, LCC capacity): -0.525

### Secondary Capacity per Block

*The chain selects secondary blocks to satisfy the min_lcc_fraction
constraint.*

|   k | N Samples | Mean Total Sec Cap | Mean Cap/Block |
|----:|----------:|-------------------:|---------------:|
|   1 |     10051 |                178 |            178 |
|   2 |      4851 |                354 |            177 |
|   3 |      1669 |                505 |            168 |
|   4 |       391 |                616 |            154 |
|   5 |        46 |                729 |            146 |
|   6 |        11 |               1167 |            194 |

- Library mean secondary capacity: 183
- Sampled mean secondary capacity per block: 176

### Total Capacity Relative to Minimum

*Shows how far sampled plans sit above the statutory minimum capacity.*

| Metric                    | Value |
|:--------------------------|:------|
| Minimum required capacity | 1022  |
| Mean total capacity       | 1191  |
| Median total capacity     | 1124  |
| Mean excess capacity      | 169   |
| Median excess capacity    | 102   |
| 90th percentile excess    | 410   |
| Max excess capacity       | 3167  |
| States within +100 units  | 49.3% |
| States within +250 units  | 78.2% |
| States \>= 1.25× minimum  | 21.4% |
| States \>= 1.5× minimum   | 6.5%  |

### Excess Capacity by Secondary Count (k)

*Links secondary count to how far sampled plans sit above the minimum
requirement.*

|   k | N Samples | Mean Total Cap | Mean Excess | Median Excess | Pct \<= +100 |
|----:|----------:|---------------:|------------:|--------------:|:-------------|
|   0 |     12981 |           1168 |         146 |            71 | 61.1%        |
|   1 |     10051 |           1171 |         149 |           107 | 47.4%        |
|   2 |      4851 |           1221 |         199 |           151 | 34.3%        |
|   3 |      1669 |           1331 |         309 |           247 | 21.9%        |
|   4 |       391 |           1438 |         416 |           350 | 15.9%        |
|   5 |        46 |           1640 |         618 |           571 | 0.0%         |
|   6 |        11 |           2360 |        1338 |           695 | 0.0%         |

### Purge / Standalone-LCC Diagnostic

*Assesses whether low-secondary states are absent because the LCC rarely
meets the minimum requirement on its own.*

    **Full purge diagnostic available:**

| Feasible After Dropping Secondaries | Constraint Failed | Count | Pct   |
|:------------------------------------|:------------------|------:|:------|
| TRUE                                | NA                |   180 | 60.0% |
| FALSE                               | min_capacity      |   120 | 40.0% |

## Mixing Diagnostics (Multi-Chain)

### Effective Sample Size (ESS)

*ESS via coda::effectiveSize() across all chains.*

| Metric        |    ESS |
|:--------------|-------:|
| Capacity      | 1884.0 |
| N Components  |  557.0 |
| N Secondaries |  557.0 |
| LCC Capacity  |  781.0 |
| Centroid X    | 1206.6 |
| Centroid Y    | 2002.1 |

### Autocorrelation (Capacity)

- Lag 1: 0.877
- Lag 10: 0.291
- Lag 50: 0.008

### Secondary Dynamics

- Secondary births: 1197
- Secondary deaths: 1207

### Secondary Count Distribution

**Per-Chain Summary:**

| Chain      | Mean |   SD | Min | Max | k=0 % |
|:-----------|-----:|-----:|----:|----:|:------|
| chain_1    | 1.20 | 1.10 |   0 |   5 | 32.3% |
| chain_2    | 0.78 | 0.88 |   0 |   6 | 45.5% |
| chain_3    | 0.83 | 0.95 |   0 |   4 | 47.2% |
| chain_4    | 0.76 | 0.98 |   0 |   6 | 51.3% |
| k0_chain_1 | 0.81 | 0.94 |   0 |   5 | 45.8% |
| k0_chain_2 | 0.95 | 0.93 |   0 |   5 | 37.5% |

**Overall Distribution (All Chains):**

- Mean k: 0.89
- SD k: 0.98
- Range: 0 - 6
- Time at k=0: 43.27%

| k   | Count | Pct   |
|:----|------:|:------|
| 0   | 12981 | 43.3% |
| 1   | 10051 | 33.5% |
| 2   |  4851 | 16.2% |
| 3   |  1669 | 5.6%  |
| 4   |   391 | 1.3%  |
| 5   |    46 | 0.2%  |
| 6   |    11 | 0.0%  |

### LCC Capacity Distribution

- Mean: 1037
- SD: 226
- Range: 529 - 2422

### k=0 Chain Initialization Diagnostic

| Metric            | k\>0 Chains (mean) | k=0 Chains (mean) |
|:------------------|:-------------------|:------------------|
| Time at k=0       | 49.4%              | 47.9%             |
| Mean k            | 0.77               | 0.74              |
| Mean capacity     | 1181               | 1185              |
| Mean LCC capacity | 1044               | 1057              |

**k=0 chains stayed at low k** (mean k = 0.74 after burn-in).

This suggests the posterior is multimodal in k: the k=0 / low-k region
is a valid posterior mode that k\>0 chains cannot reach due to
birth/death pool asymmetry. The adopted plan (single contiguous district
with 0 secondaries) is supported by the posterior.

## Move Acceptance (Aggregated)

| Type               | Attempted | Feasible% | MH Accept% | Overall% |
|:-------------------|----------:|----------:|-----------:|---------:|
| lcc_local_add      |      1530 |      90.5 |       94.1 |     85.2 |
| lcc_local_remove   |      1456 |      75.3 |       74.2 |     55.9 |
| lifted_birth_death |     10470 |      83.4 |       27.6 |     23.0 |
| secondary_swap     |      3032 |      92.7 |       47.3 |     43.8 |
| replace_lcc        |     13512 |      71.2 |       42.6 |     30.3 |

### Birth/Death Kernel Statistics

**Kernel Mode: Lifted Birth/Death**

*Lifted MCMC maintains a momentum variable (birth/death direction). On
acceptance, the direction persists; on rejection, it flips. This creates
correlated sequences that can traverse k (secondary count) faster than
reversible proposals.*

- Total attempted: 10,470
- Feasible: 8,727 (83.4%)
- Accepted: 2,406 (27.6% of feasible)

**Direction breakdown (accepted moves):**

- Births: 1,197
- Deaths: 1,209
- Birth/Death ratio: 0.99

**Universe sizes (\|U\| = \|addable\| + \|removable\|):**

    - Mean |U|: 455.3
    - Range: 2 - 500

### Birth/Death Candidate Pools by k

*Forward proposal pool sizes for the active birth/death kernel. These
are the diagnostics that matter for add/remove imbalance; `|U|` alone
can hide a large `n_add / n_rem` skew.*

| k | Attempts | Mean n_add | Mean n_rem | Mean n_add/n_rem | Pct No Addable | Pct No Removable | Accept % |
|---:|---:|---:|---:|---:|:---|:---|:---|
| 0 | 4578 | 483.1 | 0 | 483.08 | 0.0% | 100.0% | 11.7% |
| 1 | 3463 | 465.8 | 1 | 465.78 | 0.0% | 0.0% | 27.5% |
| 2 | 1703 | 425.5 | 2 | 212.75 | 1.8% | 0.0% | 35.5% |
| 3 | 572 | 312.1 | 3 | 104.04 | 8.9% | 0.0% | 42.1% |
| 4 | 132 | 194.4 | 4 | 48.59 | 25.0% | 0.0% | 45.5% |
| 5 | 18 | 215.3 | 5 | 43.07 | 27.8% | 0.0% | 61.1% |
| 6 | 4 | 64.8 | 6 | 10.79 | 75.0% | 0.0% | 50.0% |

## Kernel Profiling

*Per-kernel timing breakdown for MCMC execution.*

    **Per-Chain Timing (seconds):**



    |Chain | LCC Local| Birth/Death| Swap| Replace LCC| Overhead|  Total|
    |:-----|---------:|-----------:|----:|-----------:|--------:|------:|
    |1     |      2.12|        7.92| 3.57|      296.91|    11.30| 321.81|
    |2     |      2.01|        7.13| 3.26|      298.06|    10.76| 321.22|
    |3     |      2.17|        7.05| 3.40|      305.27|    10.34| 328.23|
    |4     |      2.13|        7.48| 2.84|      300.96|    11.71| 325.13|
    |5     |      2.05|        7.74| 3.11|      298.95|    11.49| 323.34|
    |6     |      1.85|        7.72| 3.40|      306.62|    10.88| 330.47|

    **Aggregate Time Distribution:**

    - LCC Local: 0.6% (12.3s)
    - Birth/Death: 2.3% (45.0s)
    - Swap: 1.0% (19.6s)
    - Replace LCC: 92.6% (1806.8s)
    - Overhead: 3.4% (66.5s)
    - **Total: 1950.2s** across 6 chains

## Convergence (Multi-Chain)

### R-hat

| metric       |  rhat | rhat_upper |    n_eff |
|:-------------|------:|-----------:|---------:|
| capacity     | 1.005 |      1.009 | 1883.976 |
| n_components | 1.026 |      1.058 |  556.951 |
| lcc_capacity | 1.020 |      1.049 |  781.010 |
| centroid_x   | 1.010 |      1.022 | 1206.623 |
| centroid_y   | 1.012 |      1.018 | 2002.077 |

### Chain Separation (Jaccard Overlap)

- Min pairwise overlap: 0.625
- Separated: FALSE
- Evidence: Mixing OK: Min pairwise Jaccard overlap = 0.625 \>= 0.10
  threshold

**Overlap Matrix:**

|            | chain_1 | chain_2 | chain_3 | chain_4 | k0_chain_1 | k0_chain_2 |
|:-----------|--------:|--------:|--------:|--------:|-----------:|-----------:|
| chain_1    |   1.000 |   0.710 |   0.679 |   0.625 |      0.631 |      0.711 |
| chain_2    |   0.710 |   1.000 |   0.825 |   0.700 |      0.704 |      0.793 |
| chain_3    |   0.679 |   0.825 |   1.000 |   0.812 |      0.740 |      0.708 |
| chain_4    |   0.625 |   0.700 |   0.812 |   1.000 |      0.802 |      0.633 |
| k0_chain_1 |   0.631 |   0.704 |   0.740 |   0.802 |      1.000 |      0.668 |
| k0_chain_2 |   0.711 |   0.793 |   0.708 |   0.633 |      0.668 |      1.000 |

### Chain-Specific Statistics

**LCC Capacity by Chain:**

| Chain      | Region  | Mean |  SD | Min |  Max |
|:-----------|:--------|-----:|----:|----:|-----:|
| chain_1    | chain_1 |  973 | 238 | 529 | 2343 |
| chain_2    | chain_2 | 1049 | 209 | 581 | 2370 |
| chain_3    | chain_3 | 1037 | 220 | 536 | 2422 |
| chain_4    | chain_4 | 1078 | 235 | 542 | 2351 |
| k0_chain_1 | chain_5 | 1060 | 227 | 536 | 2190 |
| k0_chain_2 | chain_6 | 1024 | 213 | 548 | 2098 |

**N Components by Chain:**

| Chain      | Mean |   SD | Min | Max |
|:-----------|-----:|-----:|----:|----:|
| chain_1    | 2.20 | 1.10 |   1 |   6 |
| chain_2    | 1.78 | 0.88 |   1 |   7 |
| chain_3    | 1.83 | 0.95 |   1 |   5 |
| chain_4    | 1.76 | 0.98 |   1 |   7 |
| k0_chain_1 | 1.81 | 0.94 |   1 |   6 |
| k0_chain_2 | 1.95 | 0.93 |   1 |   6 |

**Capacity by Chain:**

| Chain      | Mean |  SD |  Min |  Max |
|:-----------|-----:|----:|-----:|-----:|
| chain_1    | 1176 | 192 | 1022 | 2613 |
| chain_2    | 1194 | 225 | 1022 | 4189 |
| chain_3    | 1182 | 195 | 1023 | 2836 |
| chain_4    | 1212 | 219 | 1022 | 3060 |
| k0_chain_1 | 1198 | 194 | 1022 | 2190 |
| k0_chain_2 | 1187 | 180 | 1023 | 2419 |

**LCC Transition Rates by Chain:**

| Chain      | Region  | Transitions | Rate  |
|:-----------|:--------|------------:|:------|
| chain_1    | chain_1 |         454 | 9.1%  |
| chain_2    | chain_2 |         459 | 9.2%  |
| chain_3    | chain_3 |         516 | 10.3% |
| chain_4    | chain_4 |         525 | 10.5% |
| k0_chain_1 | chain_5 |         510 | 10.2% |
| k0_chain_2 | chain_6 |         494 | 9.9%  |

### Geographic Coverage by Region

| Region | Parcels | Capacity | Mean Visit% | Mean Cap% | Min Visit% | Max Visit% |
|:-------|--------:|---------:|------------:|----------:|-----------:|-----------:|
| all    |    5129 |    59604 |          14 |      22.3 |       12.6 |       16.4 |

## State Space Exploration

### Unique LCCs Visited

- Mean unique LCC capacity values per chain: 857.8
- Mean LCC transitions per chain: 857
- LCC transition rate: 17.14%

### Parcel-Unit Stickiness

| Category          | Count | % of Total |
|-------------------|-------|------------|
| Always in (\>95%) | 0     | 0%         |
| Rarely in (\<5%)  | 4888  | 95.3%      |
| Variable          | 241   | 4.7%       |

| Inclusion Range | Count |
|:----------------|------:|
| (0.25,0.5\]     |    27 |
| (0.05,0.25\]    |   214 |
| \[0,0.05\]      |  4888 |

### Parcel Feasibility Analysis

**Classification Summary:**

| Classification    | Count | % of Total |
|:------------------|------:|-----------:|
| lcc_and_secondary |  1919 |       37.4 |
| unreachable       |  1446 |       28.2 |
| lcc_only          |  1417 |       27.6 |
| secondary_only    |   347 |        6.8 |

- **lcc_and_secondary**: Parcels in both LCC and secondary library
  blocks (most flexible)
- **lcc_only**: Parcels only in LCC blocks (can only be part of main
  component)
- **secondary_only**: Parcels only in secondary blocks (can only be
  detached secondaries)
- **unreachable**: Parcels not in any library block (cannot be part of
  any sampled configuration)

**Unreachable Parcel Breakdown:**

| Reason                   | Count | % of Unreachable |
|:-------------------------|------:|-----------------:|
| low_density              |  1336 |             92.4 |
| small_area               |   109 |              7.5 |
| not_in_discovered_blocks |     1 |              0.1 |

- **low_density**: Parcel density below threshold (15 units/acre)
- **small_area**: Area below 5-acre secondary threshold and not part of
  any LCC
- **not_in_discovered_blocks**: Parcel geometrically excluded from all
  discovered blocks

**Capacity by Classification:**

| Classification    | Total Capacity | % of Capacity |
|:------------------|---------------:|--------------:|
| lcc_and_secondary |          28109 |          47.2 |
| lcc_only          |          25079 |          42.1 |
| secondary_only    |           3699 |           6.2 |
| unreachable       |           2717 |           4.6 |

**Feasibility vs Stickiness:**

    Rarely visited (<5%) parcels: 4,888

      - Unreachable: 1,446 (29.6%)

      - Feasible but rarely visited: 3,442 (70.4%)

**Stickiness by Feasibility Class:**

| Classification    | Parcels | Never Visited | % Never | Mean Incl% | Max Incl% |
|:------------------|--------:|--------------:|--------:|-----------:|----------:|
| lcc_and_secondary |    1919 |             5 |     0.3 |       1.92 |      33.3 |
| lcc_only          |    1417 |          1052 |    74.2 |       0.78 |      37.3 |
| secondary_only    |     347 |             0 |     0.0 |       0.36 |       1.7 |
| unreachable       |    1446 |          1427 |    98.7 |       0.00 |       0.9 |

- Feasible but rarely visited: 67.1% of all parcels

### Unique States Visited

- Unique parcel states (across all chains): 2,518
- Total thinned samples: 3,000
- Uniqueness ratio: 83.9%

## Kernel-Specific Diagnostics

### Replace-LCC Rejection Breakdown

| Reason                            | Count | % of Attempted |
|:----------------------------------|------:|---------------:|
| attempted                         | 13512 |          100.0 |
| proposed                          | 13512 |          100.0 |
| feasible                          |  9618 |           71.2 |
| mh_rejected                       |  5521 |           40.9 |
| accepted                          |  4097 |           30.3 |
| infeasible                        |  3894 |           28.8 |
| proposal_failed                   |     0 |            0.0 |
| current_lcc_not_in_library        |     0 |            0.0 |
| no_candidates                     |     0 |            0.0 |
| secondaries_incompatible          |     0 |            0.0 |
| no_similar_capacity_lcc           |     0 |            0.0 |
| no_compatible_lcc                 |     0 |            0.0 |
| no_valid_lcc                      |     0 |            0.0 |
| invalid_forward_weights           |     0 |            0.0 |
| selected_lcc_invalid              |     0 |            0.0 |
| no_reverse_candidates             |     0 |            0.0 |
| old_lcc_not_in_reverse_set        |     0 |            0.0 |
| invalid_reverse_weights           |     0 |            0.0 |
| old_lcc_filtered_from_reverse_set |     0 |            0.0 |

| Constraint   | Failures | % of Failures |
|:-------------|---------:|--------------:|
| min_capacity |     3879 |          99.6 |
| min_density  |       15 |           0.4 |

**Replace-LCC MH Acceptance Analysis:**

    - MH proposals evaluated: 9,618
    - Mean accept prob: 0.427
    - Median accept prob: 0.271
    - Accept prob < 0.1: 3258 (33.9%)
    - Accept prob > 0.9: 2383 (24.8%)


    **Log proposal ratio (log q(x'→x) / q(x→x')):**

    - Mean: -1.38
    - SD: 1.65
    - Range: -5.12 to 4.36


    **Replace-LCC proposal diagnostics:**

| Metric                      |   Mean | Median | P90 |  Max |
|:----------------------------|-------:|-------:|----:|-----:|
| Retained k (secondaries)    |   1.07 |      1 |   2 |    6 |
| Forward candidate LCC count | 512.28 |    512 | 672 | 1039 |

**Replace-LCC Proposal Geometry:**

    - Forward candidate LCCs: mean=512.3, sd=122.1
    - Reverse candidate LCCs: mean=538.3, sd=128.3
    - Asymmetry ratio (fwd/rev): mean=0.97

### Birth/Death Direction Analysis

    **Direction of Accepted Moves:**


    ✓ Balanced: 0.99 births per death

### Legacy Multi-Move Birth/Death (r-value)

*Note: These diagnostics are for legacy multi-move birth/death kernels.
Current architecture uses symmetric_birth_death which proposes single
blocks.*

    Legacy multi-birth kernel not active (using symmetric_birth_death instead).

    Legacy multi-death kernel not active (using symmetric_birth_death instead).

### Swap Kernel Diagnostics

    **Capacity Change for Accepted Swaps:**

    - Total accepted swaps: 1,328
    - Mean capacity delta: 0.5
    - SD capacity delta: 61.9
    - Range: -150 to 150


    **Proposal Asymmetry (similar block counts):**

    - Forward similar blocks: mean=401.9, sd=64.8
    - Reverse similar blocks: mean=400.0, sd=68.9
    - Ratio (fwd/rev): mean=1.04

    **Swap Rejection Breakdown:**

| Reason          | Count | % of Attempted |
|:----------------|------:|---------------:|
| attempted       |  3032 |          100.0 |
| proposed        |  1749 |           57.7 |
| feasible        |  1527 |           50.4 |
| accepted        |  1328 |           43.8 |
| proposal_failed |  1283 |           42.3 |
| no_secondaries  |  1283 |           42.3 |
| infeasible      |   222 |            7.3 |
| mh_rejected     |   199 |            6.6 |


    **Swap Constraint Failures:**

| Constraint           | Failures |
|:---------------------|---------:|
| min_capacity         |      192 |
| station_area_pct     |       13 |
| min_lcc_fraction     |        8 |
| min_density          |        6 |
| station_capacity_pct |        3 |

## Capacity Prior Diagnostics

The capacity prior penalizes capacity above min_capacity with a linear
penalty: `penalty = λ × (capacity - min_capacity)` where λ = 0.005.

- Penalty source: runner diagnostics
- Steps above min_capacity: 29,900 / 30,000 (99.7%)

<!-- -->

    **Penalty Statistics (when above min_capacity):**

    - Mean penalty: 0.8498
    - Max penalty: 15.8350
    - Mean excess capacity: 170

## Library Utilization

### LCC Library

- Initial LCC library size: 5000
- Final library sizes: 5321, 5308, 5305, 5296, 5296, 5285
- Online enrichment adds: 1811

### Secondary Library

- Secondary library size: 500
- Lifted Birth/Death acceptance: 23.0%
- Lifted Birth/Death feasibility: 83.4%

## Library Coverage Statistics

*How well does the LCC library cover the parcel space?*

| Metric                     | Value |
|:---------------------------|:------|
| Total LCCs in library      | 5000  |
| Total parcels              | 5129  |
| Parcels covered (≥1 LCC)   | 3336  |
| Parcels uncovered (0 LCCs) | 1793  |
| Coverage rate              | 65.0% |
| Min coverage (excl. zero)  | 1     |
| Median coverage            | 7     |
| Mean coverage              | 55    |
| Max coverage               | 517   |
| Std dev coverage           | 95.7  |

### Coverage Distribution

| LCCs containing parcel | N Parcels | Percent |
|:-----------------------|----------:|:--------|
| 11-50                  |       968 | 18.9%   |
| 51-100                 |       470 | 9.2%    |
| 0                      |      1793 | 35.0%   |
| 6-10                   |       340 | 6.6%    |
| \>100                  |       949 | 18.5%   |
| 2-5                    |       407 | 7.9%    |
| 1                      |       202 | 3.9%    |

## Diagnostic Health Summary

| Check                           | Value             |
|:--------------------------------|:------------------|
| Convergence (R-hat)             | 1.026             |
| ESS                             | 557               |
| Chain Mixing                    | 0.625 min overlap |
| LCC Exploration                 | 17.14%            |
| Lifted Birth/Death Acceptance   | 23.0% acceptance  |
| Joint Core Refresh Feasibility  | 71.2%             |
| Parcel Variability (stickiness) | 95.3% sticky      |
