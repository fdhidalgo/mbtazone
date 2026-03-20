# MCMC Diagnostics Summary (LLM)


# MCMC Diagnostics Summary

## Overview

- Total Chains: 4
- Total Steps: 20000 (5000 per chain)

## Current Tuning

- Capacity prior λ: 0.005
- Secondary-count prior λ: 2
- Secondary minimum area: 5 acres
- Replace-LCC capacity tolerance: 800
- Joint core keep range: \[0.15, 0.85\]
- Joint LCC excess rate: 0.003
- Joint add-prob range: \[0.01, 0.30\]
- Swap capacity tolerance: 150

## Graph Statistics

- Parcels: 8052
- Parcel units: 8052 (compression: 1x)
- Parcel edges: 2.7293^{4}
- Avg degree: 6.8

## Library Coverage

### Secondary Library

| Size Band | Count |
|:----------|------:|
| band_3    |   161 |
| band_2    |   150 |
| band_1    |   189 |

### LCC Library

- Candidates: 3250
- Capacity range: 1025 - 3067

### LCC Capacity: Library vs Sampled

*Compares library capacity distribution to sampled distribution. A
discrepancy reflects the posterior distribution shape, not proposal bias
(proposals are uniform).*

| Metric                  | Library | Sampled |
|:------------------------|:--------|:--------|
| Mean capacity           | 1999    | 2151    |
| Median capacity         | 1905    | 2166    |
| High-capacity (≥2045) % | 30.8%   | 59.1%   |


    **Capacity ratio (sampled/library): 1.08**

    ✓ Sampled distribution matches library distribution.

### LCC Library Capacity Bands

*Distribution of LCC capacities in the library relative to
min_capacity.*

| Capacity Band            | N LCCs | % of Library |
|:-------------------------|-------:|:-------------|
| \< min_capacity (2045)   |   2250 | 69.2%        |
| \[min_cap, 1.25×min_cap) |    208 | 6.4%         |
| \[1.25×, 1.5×min_cap)    |    792 | 24.4%        |
| \[1.5×, 2×min_cap)       |      0 | 0.0%         |
| \>= 2×min_capacity       |      0 | 0.0%         |

### Library Accessibility by Secondary Count (k)

*The min_lcc_fraction constraint requires LCC capacity \>= total
secondary capacity. As k increases, fewer LCCs are accessible.*

|   k | Est. Sec. Cap. | Min LCC Required | N Accessible | % Library |
|----:|---------------:|-----------------:|-------------:|:----------|
|   0 |          0.000 |            0.000 |         3250 | 100.0%    |
|   1 |        312.064 |          312.064 |         3250 | 100.0%    |
|   2 |        624.128 |          624.128 |         3250 | 100.0%    |
|   3 |        936.192 |          936.192 |         3250 | 100.0%    |
|   4 |       1248.256 |         1248.256 |         3115 | 95.8%     |
|   5 |       1560.320 |         1560.320 |         2218 | 68.2%     |
|   6 |       1872.384 |         1872.384 |         1723 | 53.0%     |
|   7 |       2184.448 |         2184.448 |          955 | 29.4%     |
|   8 |       2496.512 |         2496.512 |          825 | 25.4%     |
|   9 |       2808.576 |         2808.576 |          552 | 17.0%     |
|  10 |       3120.640 |         3120.640 |            0 | 0.0%      |
|  11 |       3432.704 |         3432.704 |            0 | 0.0%      |
|  12 |       3744.768 |         3744.768 |            0 | 0.0%      |
|  13 |       4056.832 |         4056.832 |            0 | 0.0%      |
|  14 |       4368.896 |         4368.896 |            0 | 0.0%      |
|  15 |       4680.960 |         4680.960 |            0 | 0.0%      |

### Posterior-Weighted Library Accessibility

*Average library accessibility weighted by the observed k distribution.*

- Mean secondary block capacity (library): 312
- Posterior-weighted library accessibility: 20.7%

### Sampled LCC Capacity by k

*Observed relationship between secondary count and LCC capacity in
sampled states.*

|   k | N Samples | Mean LCC Cap | Min LCC Cap | Max LCC Cap |
|----:|----------:|-------------:|------------:|------------:|
|   0 |         2 |         2387 |        2387 |        2387 |
|   1 |        13 |         2756 |        2387 |        2987 |
|   2 |        22 |         2475 |        2040 |        2987 |
|   3 |        29 |         2386 |        2040 |        2987 |
|   4 |       254 |         2091 |        1528 |        3042 |
|   5 |       630 |         1967 |        1332 |        2987 |
|   6 |      1624 |         1994 |        1323 |        3009 |
|   7 |      2895 |         1996 |        1323 |        3041 |
|   8 |      3888 |         2093 |        1533 |        3041 |
|   9 |      4092 |         2151 |        1542 |        3041 |
|  10 |      3434 |         2238 |        1542 |        3041 |
|  11 |      1996 |         2336 |        1777 |        3041 |
|  12 |       780 |         2429 |        1790 |        3041 |
|  13 |       214 |         2556 |        2119 |        3015 |
|  14 |       108 |         2656 |        2198 |        3015 |
|  15 |        19 |         2679 |        2470 |        2946 |

- Correlation(k, LCC capacity): 0.355

### Secondary Capacity per Block

*The chain selects secondary blocks to satisfy the min_lcc_fraction
constraint.*

|   k | N Samples | Mean Total Sec Cap | Mean Cap/Block |
|----:|----------:|-------------------:|---------------:|
|   1 |        13 |                720 |            720 |
|   2 |        22 |                828 |            414 |
|   3 |        29 |                963 |            321 |
|   4 |       254 |                970 |            242 |
|   5 |       630 |               1129 |            226 |
|   6 |      1624 |               1314 |            219 |
|   7 |      2895 |               1475 |            211 |
|   8 |      3888 |               1622 |            203 |
|   9 |      4092 |               1792 |            199 |
|  10 |      3434 |               1961 |            196 |
|  11 |      1996 |               2097 |            191 |
|  12 |       780 |               2242 |            187 |
|  13 |       214 |               2364 |            182 |
|  14 |       108 |               2466 |            176 |
|  15 |        19 |               2634 |            176 |

- Library mean secondary capacity: 312
- Sampled mean secondary capacity per block: 203

### Total Capacity Relative to Minimum

*Shows how far sampled plans sit above the statutory minimum capacity.*

| Metric                    | Value |
|:--------------------------|:------|
| Minimum required capacity | 2045  |
| Mean total capacity       | 3878  |
| Median total capacity     | 3857  |
| Mean excess capacity      | 1833  |
| Median excess capacity    | 1812  |
| 90th percentile excess    | 2638  |
| Max excess capacity       | 3989  |
| States within +100 units  | 0.0%  |
| States within +250 units  | 0.1%  |
| States \>= 1.25× minimum  | 98.9% |
| States \>= 1.5× minimum   | 89.3% |

### Excess Capacity by Secondary Count (k)

*Links secondary count to how far sampled plans sit above the minimum
requirement.*

|   k | N Samples | Mean Total Cap | Mean Excess | Median Excess | Pct \<= +100 |
|----:|----------:|---------------:|------------:|--------------:|:-------------|
|   0 |         2 |           2387 |         342 |           342 | 0.0%         |
|   1 |        13 |           3476 |        1431 |          1744 | 0.0%         |
|   2 |        22 |           3302 |        1258 |          1192 | 0.0%         |
|   3 |        29 |           3349 |        1304 |          1069 | 0.0%         |
|   4 |       254 |           3061 |        1016 |           951 | 0.0%         |
|   5 |       630 |           3096 |        1051 |           956 | 0.0%         |
|   6 |      1624 |           3307 |        1262 |          1254 | 0.0%         |
|   7 |      2895 |           3471 |        1426 |          1411 | 0.0%         |
|   8 |      3888 |           3715 |        1670 |          1650 | 0.0%         |
|   9 |      4092 |           3943 |        1898 |          1894 | 0.0%         |
|  10 |      3434 |           4198 |        2153 |          2148 | 0.0%         |
|  11 |      1996 |           4433 |        2388 |          2340 | 0.0%         |
|  12 |       780 |           4671 |        2626 |          2568 | 0.0%         |
|  13 |       214 |           4920 |        2875 |          2810 | 0.0%         |
|  14 |       108 |           5122 |        3077 |          3314 | 0.0%         |
|  15 |        19 |           5313 |        3268 |          2865 | 0.0%         |

### Purge / Standalone-LCC Diagnostic

*Assesses whether low-secondary states are absent because the LCC rarely
meets the minimum requirement on its own.*

    **Full purge diagnostic available:**

| Feasible After Dropping Secondaries | Constraint Failed    | Count | Pct   |
|:------------------------------------|:---------------------|------:|:------|
| FALSE                               | min_capacity         |    81 | 40.5% |
| FALSE                               | station_capacity_pct |    62 | 31.0% |
| FALSE                               | station_area_pct     |    35 | 17.5% |
| TRUE                                | NA                   |    22 | 11.0% |

## Mixing Diagnostics (Multi-Chain)

### Effective Sample Size (ESS)

*ESS via coda::effectiveSize() across all chains.*

| Metric        |   ESS |
|:--------------|------:|
| Capacity      | 183.3 |
| N Components  | 419.0 |
| N Secondaries | 419.0 |
| LCC Capacity  |  13.4 |
| Centroid X    | 100.8 |
| Centroid Y    | 133.8 |

### Autocorrelation (Capacity)

- Lag 1: 0.97
- Lag 10: 0.687
- Lag 50: 0.555

### Secondary Dynamics

- Secondary births: 2140
- Secondary deaths: 2111

### Secondary Count Distribution

**Per-Chain Summary:**

| Chain   | Mean |   SD | Min | Max | k=0 % |
|:--------|-----:|-----:|----:|----:|:------|
| chain_1 | 7.82 | 1.53 |   4 |  12 | 0.0%  |
| chain_2 | 8.87 | 1.86 |   2 |  15 | 0.0%  |
| chain_3 | 8.69 | 2.01 |   0 |  14 | 0.0%  |
| chain_4 | 9.11 | 1.91 |   1 |  15 | 0.0%  |

**Overall Distribution (All Chains):**

- Mean k: 8.62
- SD k: 1.9
- Range: 0 - 15
- Time at k=0: 0.01%

| k   | Count | Pct   |
|:----|------:|:------|
| 0   |     2 | 0.0%  |
| 1   |    13 | 0.1%  |
| 2   |    22 | 0.1%  |
| 3   |    29 | 0.1%  |
| 4   |   254 | 1.3%  |
| 5   |   630 | 3.1%  |
| 6   |  1624 | 8.1%  |
| 7   |  2895 | 14.5% |
| 8   |  3888 | 19.4% |
| 9   |  4092 | 20.5% |
| 10  |  3434 | 17.2% |
| 11  |  1996 | 10.0% |
| 12  |   780 | 3.9%  |
| 13  |   214 | 1.1%  |
| 14  |   108 | 0.5%  |
| 15  |    19 | 0.1%  |

### LCC Capacity Distribution

- Mean: 2151
- SD: 344
- Range: 1323 - 3042

## Move Acceptance (Aggregated)

| Type               | Attempted | Feasible% | MH Accept% | Overall% |
|:-------------------|----------:|----------:|-----------:|---------:|
| lcc_local_add      |       983 |      99.7 |       89.5 |     89.2 |
| lcc_local_remove   |       983 |      87.1 |       95.4 |     83.1 |
| lifted_birth_death |      6966 |      94.3 |       64.6 |     60.9 |
| secondary_swap     |      1905 |      84.9 |       88.6 |     75.2 |
| replace_lcc        |      9163 |     100.0 |        0.3 |      0.3 |

### Birth/Death Kernel Statistics

**Kernel Mode: Lifted Birth/Death**

*Lifted MCMC maintains a momentum variable (birth/death direction). On
acceptance, the direction persists; on rejection, it flips. This creates
correlated sequences that can traverse k (secondary count) faster than
reversible proposals.*

- Total attempted: 6,966
- Feasible: 6,567 (94.3%)
- Accepted: 4,240 (64.6% of feasible)

**Direction breakdown (accepted moves):**

- Births: 2,136
- Deaths: 2,104
- Birth/Death ratio: 1.02

**Universe sizes (\|U\| = \|addable\| + \|removable\|):**

    - Mean |U|: 186.1
    - Range: 3 - 449

### Birth/Death Candidate Pools by k

*Forward proposal pool sizes for the active birth/death kernel. These
are the diagnostics that matter for add/remove imbalance; `|U|` alone
can hide a large `n_add / n_rem` skew.*

| k | Attempts | Mean n_add | Mean n_rem | Mean n_add/n_rem | Pct No Addable | Pct No Removable | Accept % |
|---:|---:|---:|---:|---:|:---|:---|:---|
| 0 | 2 | 438.0 | 0 | 438.00 | 0.0% | 100.0% | 50.0% |
| 1 | 4 | 430.5 | 1 | 430.50 | 0.0% | 0.0% | 75.0% |
| 2 | 8 | 420.6 | 2 | 210.31 | 0.0% | 0.0% | 75.0% |
| 3 | 15 | 319.4 | 3 | 106.47 | 20.0% | 0.0% | 73.3% |
| 4 | 87 | 360.0 | 4 | 90.00 | 1.1% | 0.0% | 52.9% |
| 5 | 229 | 326.1 | 5 | 65.22 | 0.9% | 0.0% | 62.0% |
| 6 | 562 | 266.9 | 6 | 44.49 | 4.4% | 0.0% | 60.1% |
| 7 | 1006 | 218.9 | 7 | 31.27 | 13.8% | 0.0% | 57.4% |
| 8 | 1334 | 209.2 | 8 | 26.16 | 9.5% | 0.0% | 61.6% |
| 9 | 1467 | 158.7 | 9 | 17.64 | 12.5% | 0.0% | 63.4% |
| 10 | 1203 | 119.3 | 10 | 11.93 | 24.1% | 0.0% | 60.6% |
| 11 | 682 | 99.3 | 11 | 9.03 | 27.3% | 0.0% | 59.1% |
| 12 | 256 | 72.3 | 12 | 6.02 | 41.8% | 0.0% | 62.5% |
| 13 | 79 | 77.2 | 13 | 5.94 | 34.2% | 0.0% | 67.1% |
| 14 | 26 | 57.4 | 14 | 4.10 | 26.9% | 0.0% | 61.5% |
| 15 | 6 | 10.3 | 15 | 0.69 | 66.7% | 0.0% | 50.0% |

## Kernel Profiling

*Per-kernel timing breakdown for MCMC execution.*

    **Per-Chain Timing (seconds):**



    |Chain | LCC Local| Birth/Death| Swap| Replace LCC| Overhead|   Total|
    |:-----|---------:|-----------:|----:|-----------:|--------:|-------:|
    |1     |      3.51|       16.09| 8.25|     1309.12|    25.02| 1361.99|
    |2     |      3.23|       16.07| 7.55|     1225.26|    24.92| 1277.03|
    |3     |      4.07|       16.90| 8.67|     1626.58|    27.61| 1683.83|
    |4     |      3.76|       16.89| 9.03|     1366.48|    26.17| 1422.32|

    **Aggregate Time Distribution:**

    - LCC Local: 0.3% (14.6s)
    - Birth/Death: 1.1% (65.9s)
    - Swap: 0.6% (33.5s)
    - Replace LCC: 96.2% (5527.4s)
    - Overhead: 1.8% (103.7s)
    - **Total: 5745.2s** across 4 chains

## Convergence (Multi-Chain)

### R-hat

| metric       |  rhat | rhat_upper |   n_eff |
|:-------------|------:|-----------:|--------:|
| capacity     | 1.815 |      2.956 | 183.287 |
| n_components | 1.116 |      1.322 | 418.979 |
| lcc_capacity | 2.249 |      4.078 |  13.398 |
| centroid_x   | 1.648 |      3.299 | 100.768 |
| centroid_y   | 1.396 |      2.973 | 133.781 |

### Chain Separation (Jaccard Overlap)

- Min pairwise overlap: 0.546
- Separated: FALSE
- Evidence: Mixing OK: Min pairwise Jaccard overlap = 0.546 \>= 0.10
  threshold

**Overlap Matrix:**

|         | chain_1 | chain_2 | chain_3 | chain_4 |
|:--------|--------:|--------:|--------:|--------:|
| chain_1 |   1.000 |   0.546 |   0.617 |   0.580 |
| chain_2 |   0.546 |   1.000 |   0.566 |   0.602 |
| chain_3 |   0.617 |   0.566 |   1.000 |   0.593 |
| chain_4 |   0.580 |   0.602 |   0.593 |   1.000 |

### Chain-Specific Statistics

**LCC Capacity by Chain:**

| Chain   | Region  | Mean |  SD |  Min |  Max |
|:--------|:--------|-----:|----:|-----:|-----:|
| chain_1 | chain_1 | 1756 | 171 | 1323 | 3020 |
| chain_2 | chain_2 | 2174 | 196 | 1697 | 2470 |
| chain_3 | chain_3 | 2397 | 264 | 1951 | 3025 |
| chain_4 | chain_4 | 2276 | 323 | 1781 | 3042 |

**N Components by Chain:**

| Chain   |  Mean |   SD | Min | Max |
|:--------|------:|-----:|----:|----:|
| chain_1 |  8.82 | 1.53 |   5 |  13 |
| chain_2 |  9.87 | 1.86 |   3 |  16 |
| chain_3 |  9.69 | 2.01 |   1 |  15 |
| chain_4 | 10.11 | 1.91 |   2 |  16 |

**Capacity by Chain:**

| Chain   | Mean |  SD |  Min |  Max |
|:--------|-----:|----:|-----:|-----:|
| chain_1 | 3213 | 363 | 2263 | 5968 |
| chain_2 | 4006 | 454 | 2615 | 4910 |
| chain_3 | 4193 | 574 | 2387 | 5847 |
| chain_4 | 4101 | 579 | 2698 | 6034 |

**LCC Transition Rates by Chain:**

| Chain   | Region  | Transitions | Rate |
|:--------|:--------|------------:|:-----|
| chain_1 | chain_1 |           5 | 0.1% |
| chain_2 | chain_2 |          15 | 0.3% |
| chain_3 | chain_3 |          18 | 0.4% |
| chain_4 | chain_4 |          18 | 0.4% |

### Geographic Coverage by Region

| Region | Parcels | Capacity | Mean Visit% | Mean Cap% | Min Visit% | Max Visit% |
|:-------|--------:|---------:|------------:|----------:|-----------:|-----------:|
| all    |    8052 |   141707 |          66 |      25.1 |       62.1 |       69.3 |

## State Space Exploration

### Unique LCCs Visited

- Mean unique LCC capacity values per chain: 202.8
- Mean LCC transitions per chain: 202
- LCC transition rate: 4.04%

### Parcel-Unit Stickiness

| Category          | Count | % of Total |
|-------------------|-------|------------|
| Always in (\>95%) | 0     | 0%         |
| Rarely in (\<5%)  | 3786  | 47%        |
| Variable          | 4266  | 53%        |

| Inclusion Range | Count |
|:----------------|------:|
| (0.25,0.5\]     |   336 |
| (0.05,0.25\]    |  3930 |
| \[0,0.05\]      |  3786 |

### Parcel Feasibility Analysis

**Classification Summary:**

| Classification    | Count | % of Total |
|:------------------|------:|-----------:|
| lcc_and_secondary |  5349 |       66.4 |
| secondary_only    |  1638 |       20.3 |
| unreachable       |   581 |        7.2 |
| lcc_only          |   484 |        6.0 |

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
| small_area               |   395 |             68.0 |
| low_density              |   127 |             21.9 |
| not_in_discovered_blocks |    59 |             10.2 |

- **low_density**: Parcel density below threshold (15 units/acre)
- **small_area**: Area below 5-acre secondary threshold and not part of
  any LCC
- **not_in_discovered_blocks**: Parcel geometrically excluded from all
  discovered blocks

**Capacity by Classification:**

| Classification    | Total Capacity | % of Capacity |
|:------------------|---------------:|--------------:|
| unreachable       |          55470 |          39.1 |
| lcc_and_secondary |          39507 |          27.9 |
| lcc_only          |          23877 |          16.8 |
| secondary_only    |          22853 |          16.1 |

**Feasibility vs Stickiness:**

    Rarely visited (<5%) parcels: 3,786

      - Unreachable: 581 (15.3%)

      - Feasible but rarely visited: 3,205 (84.7%)

**Stickiness by Feasibility Class:**

| Classification    | Parcels | Never Visited | % Never | Mean Incl% | Max Incl% |
|:------------------|--------:|--------------:|--------:|-----------:|----------:|
| lcc_and_secondary |    5349 |            74 |     1.4 |      10.92 |      44.6 |
| lcc_only          |     484 |           255 |    52.7 |       3.09 |      26.0 |
| secondary_only    |    1638 |           285 |    17.4 |       2.27 |      14.1 |
| unreachable       |     581 |           579 |    99.7 |       0.01 |       3.2 |

- Feasible but rarely visited: 39.8% of all parcels

### Unique States Visited

- Unique parcel states (across all chains): 1,974
- Total thinned samples: 2,000
- Uniqueness ratio: 98.7%

## Kernel-Specific Diagnostics

### Joint Core Refresh Rejection Breakdown

| Reason                     | Count | % of Attempted |
|:---------------------------|------:|---------------:|
| attempted                  |  9163 |          100.0 |
| proposed                   |  9163 |          100.0 |
| feasible                   |  9163 |          100.0 |
| mh_rejected                |  9131 |           99.7 |
| zero_reverse_support       |  3278 |           35.8 |
| accepted                   |    32 |            0.3 |
| proposal_failed            |     0 |            0.0 |
| infeasible                 |     0 |            0.0 |
| current_lcc_not_in_library |     0 |            0.0 |
| no_candidates              |     0 |            0.0 |
| secondaries_incompatible   |     0 |            0.0 |
| no_similar_capacity_lcc    |     0 |            0.0 |
| selected_lcc_invalid       |     0 |            0.0 |
| no_reverse_candidates      |     0 |            0.0 |
| no_lcc_for_core            |     0 |            0.0 |
| no_change                  |     0 |            0.0 |

    No replace-LCC constraint failures recorded (all infeasible moves may have unknown constraints).

**Joint Core Refresh MH Acceptance Analysis:**

    - MH proposals evaluated: 9,163
    - Mean accept prob: 0.004
    - Median accept prob: 0.000
    - Accept prob < 0.1: 9114 (99.5%)
    - Accept prob > 0.9: 26 (0.3%)

| Quantity          |   Mean | Median |    Min |   Max |
|:------------------|-------:|-------:|-------:|------:|
| Forward log q     | -18.27 | -17.73 | -50.71 | -5.68 |
| Reverse log q     |   -Inf | -61.36 |   -Inf | -5.50 |
| Reverse - Forward |   -Inf | -43.15 |   -Inf | 11.02 |


    **Joint proposal shape diagnostics:**

| Metric              |   Mean | Median |  P90 |  Max |
|:--------------------|-------:|-------:|-----:|-----:|
| Retained core size  |   5.00 |      5 |    7 |   13 |
| Removed secondaries |   3.64 |      4 |    6 |   10 |
| Added secondaries   |   2.10 |      2 |    4 |   10 |
| Candidate LCC count | 706.43 |    657 | 1047 | 2747 |


    - Proposals changing the LCC: 99.9%

**Joint Core Refresh Proposal Geometry:**

    - Forward candidate LCCs: mean=706.4, sd=268.5
    - Reverse candidate LCCs: mean=706.4, sd=268.5
    - Asymmetry ratio (fwd/rev): mean=1.00

### Birth/Death Direction Analysis

    **Direction of Accepted Moves:**


    ✓ Balanced: 1.02 births per death

### Legacy Multi-Move Birth/Death (r-value)

*Note: These diagnostics are for legacy multi-move birth/death kernels.
Current architecture uses symmetric_birth_death which proposes single
blocks.*

    Legacy multi-birth kernel not active (using symmetric_birth_death instead).

    Legacy multi-death kernel not active (using symmetric_birth_death instead).

### Swap Kernel Diagnostics

    **Capacity Change for Accepted Swaps:**

    - Total accepted swaps: 1,432
    - Mean capacity delta: 0.1
    - SD capacity delta: 68.7
    - Range: -150 to 149


    **Proposal Asymmetry (similar block counts):**

    - Forward similar blocks: mean=186.3, sd=37.6
    - Reverse similar blocks: mean=188.1, sd=38.5
    - Ratio (fwd/rev): mean=1.01

    **Swap Rejection Breakdown:**

| Reason      | Count | % of Attempted |
|:------------|------:|---------------:|
| attempted   |  1905 |          100.0 |
| proposed    |  1905 |          100.0 |
| feasible    |  1617 |           84.9 |
| accepted    |  1432 |           75.2 |
| infeasible  |   288 |           15.1 |
| mh_rejected |   185 |            9.7 |


    **Swap Constraint Failures:**

| Constraint           | Failures |
|:---------------------|---------:|
| min_lcc_fraction     |      164 |
| station_capacity_pct |      123 |
| station_area_pct     |        1 |

## Capacity Prior Diagnostics

The capacity prior penalizes capacity above min_capacity with a linear
penalty: `penalty = λ × (capacity - min_capacity)` where λ = 0.005.

- Penalty source: runner diagnostics
- Steps above min_capacity: 20,000 / 20,000 (100%)

<!-- -->

    **Penalty Statistics (when above min_capacity):**

    - Mean penalty: 9.1649
    - Max penalty: 19.9450
    - Mean excess capacity: 1833

## Library Utilization

### LCC Library

- Initial LCC library size: 3250
- Final library sizes: 3610, 3565, 3629, 3616
- Online enrichment adds: 1420

### Secondary Library

- Secondary library size: 500
- Lifted Birth/Death acceptance: 60.9%
- Lifted Birth/Death feasibility: 94.3%

## Library Coverage Statistics

*How well does the LCC library cover the parcel space?*

| Metric                     | Value |
|:---------------------------|:------|
| Total LCCs in library      | 3250  |
| Total parcels              | 8052  |
| Parcels covered (≥1 LCC)   | 5833  |
| Parcels uncovered (0 LCCs) | 2219  |
| Coverage rate              | 72.4% |
| Min coverage (excl. zero)  | 1     |
| Median coverage            | 94    |
| Mean coverage              | 112.5 |
| Max coverage               | 446   |
| Std dev coverage           | 107.8 |

### Coverage Distribution

| LCCs containing parcel | N Parcels | Percent |
|:-----------------------|----------:|:--------|
| 0                      |      2219 | 27.6%   |
| 51-100                 |       872 | 10.8%   |
| \>100                  |      3932 | 48.8%   |
| 11-50                  |       745 | 9.3%    |
| 6-10                   |        86 | 1.1%    |
| 2-5                    |        90 | 1.1%    |
| 1                      |       108 | 1.3%    |

## Diagnostic Health Summary

| Check                           | Value             |
|:--------------------------------|:------------------|
| Convergence (R-hat)             | 2.249             |
| ESS                             | 13                |
| Chain Mixing                    | 0.546 min overlap |
| LCC Exploration                 | 4.04%             |
| Lifted Birth/Death Acceptance   | 60.9% acceptance  |
| Joint Core Refresh Feasibility  | 100.0%            |
| Parcel Variability (stickiness) | 47.0% sticky      |
