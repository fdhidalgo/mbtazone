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
| Mean capacity           | 1999    | 1701    |
| Median capacity         | 1905    | 1652    |
| High-capacity (≥2045) % | 30.8%   | 4.3%    |


    **Capacity ratio (sampled/library): 0.85**

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
|  16 |       4993.024 |         4993.024 |            0 | 0.0%      |

### Posterior-Weighted Library Accessibility

*Average library accessibility weighted by the observed k distribution.*

- Mean secondary block capacity (library): 312
- Posterior-weighted library accessibility: 29.7%

### Sampled LCC Capacity by k

*Observed relationship between secondary count and LCC capacity in
sampled states.*

|   k | N Samples | Mean LCC Cap | Min LCC Cap | Max LCC Cap |
|----:|----------:|-------------:|------------:|------------:|
|   0 |         6 |         2723 |        2387 |        3050 |
|   1 |        15 |         2340 |        1964 |        2958 |
|   2 |        19 |         2522 |        1824 |        3065 |
|   3 |        94 |         1988 |        1391 |        3065 |
|   4 |       351 |         1680 |        1138 |        3065 |
|   5 |      1023 |         1595 |        1138 |        2683 |
|   6 |      2828 |         1576 |        1138 |        2753 |
|   7 |      4751 |         1617 |        1142 |        3018 |
|   8 |      5169 |         1682 |        1142 |        2996 |
|   9 |      3350 |         1785 |        1301 |        3056 |
|  10 |      1588 |         1883 |        1391 |        3067 |
|  11 |       638 |         2027 |        1513 |        3164 |
|  12 |        89 |         2259 |        1762 |        3052 |
|  13 |        38 |         2475 |        1835 |        3052 |
|  14 |        16 |         2952 |        2771 |        3061 |
|  15 |        13 |         2939 |        2771 |        3044 |
|  16 |        12 |         3011 |        2974 |        3052 |

- Correlation(k, LCC capacity): 0.352

### Secondary Capacity per Block

*The chain selects secondary blocks to satisfy the min_lcc_fraction
constraint.*

|   k | N Samples | Mean Total Sec Cap | Mean Cap/Block |
|----:|----------:|-------------------:|---------------:|
|   1 |        15 |                584 |            584 |
|   2 |        19 |               1218 |            609 |
|   3 |        94 |               1285 |            428 |
|   4 |       351 |               1086 |            272 |
|   5 |      1023 |               1112 |            222 |
|   6 |      2828 |               1207 |            201 |
|   7 |      4751 |               1332 |            190 |
|   8 |      5169 |               1469 |            184 |
|   9 |      3350 |               1590 |            177 |
|  10 |      1588 |               1724 |            172 |
|  11 |       638 |               1891 |            172 |
|  12 |        89 |               2076 |            173 |
|  13 |        38 |               2331 |            179 |
|  14 |        16 |               2646 |            189 |
|  15 |        13 |               2746 |            183 |
|  16 |        12 |               2941 |            184 |

- Library mean secondary capacity: 312
- Sampled mean secondary capacity per block: 191

### Total Capacity Relative to Minimum

*Shows how far sampled plans sit above the statutory minimum capacity.*

| Metric                    | Value |
|:--------------------------|:------|
| Minimum required capacity | 2045  |
| Mean total capacity       | 3134  |
| Median total capacity     | 3015  |
| Mean excess capacity      | 1089  |
| Median excess capacity    | 970   |
| 90th percentile excess    | 1732  |
| Max excess capacity       | 4012  |
| States within +100 units  | 0.2%  |
| States within +250 units  | 2.2%  |
| States \>= 1.25× minimum  | 90.7% |
| States \>= 1.5× minimum   | 46.5% |

### Excess Capacity by Secondary Count (k)

*Links secondary count to how far sampled plans sit above the minimum
requirement.*

|   k | N Samples | Mean Total Cap | Mean Excess | Median Excess | Pct \<= +100 |
|----:|----------:|---------------:|------------:|--------------:|:-------------|
|   0 |         6 |           2723 |         678 |           733 | 0.0%         |
|   1 |        15 |           2925 |         880 |           770 | 0.0%         |
|   2 |        19 |           3740 |        1695 |          1915 | 0.0%         |
|   3 |        94 |           3272 |        1227 |           680 | 5.3%         |
|   4 |       351 |           2767 |         722 |           498 | 4.6%         |
|   5 |      1023 |           2707 |         662 |           594 | 1.1%         |
|   6 |      2828 |           2783 |         738 |           694 | 0.3%         |
|   7 |      4751 |           2949 |         904 |           840 | 0.0%         |
|   8 |      5169 |           3151 |        1106 |          1017 | 0.0%         |
|   9 |      3350 |           3375 |        1330 |          1365 | 0.0%         |
|  10 |      1588 |           3607 |        1562 |          1608 | 0.0%         |
|  11 |       638 |           3918 |        1873 |          1799 | 0.0%         |
|  12 |        89 |           4334 |        2289 |          1858 | 0.0%         |
|  13 |        38 |           4806 |        2761 |          3132 | 0.0%         |
|  14 |        16 |           5598 |        3553 |          3486 | 0.0%         |
|  15 |        13 |           5685 |        3640 |          3680 | 0.0%         |
|  16 |        12 |           5952 |        3907 |          3919 | 0.0%         |

### Purge / Standalone-LCC Diagnostic

*Assesses whether low-secondary states are absent because the LCC rarely
meets the minimum requirement on its own.*

    Full purge diagnostic results are not present in these chain outputs.

    Capacity-only proxy below: `LCC >= min_capacity` means the main component alone clears the legal capacity threshold.

| LCC \>= min_capacity | N Steps | Pct    | Mean k | Mean Total Cap | Mean Excess |
|:---------------------|--------:|:-------|-------:|---------------:|------------:|
| Yes                  |     851 | 4.25%  |   8.75 |           4419 |        2374 |
| No                   |   19149 | 95.75% |   7.64 |           3077 |        1032 |

## Mixing Diagnostics (Multi-Chain)

### Effective Sample Size (ESS)

*ESS via coda::effectiveSize() across all chains.*

| Metric        |    ESS |
|:--------------|-------:|
| Capacity      |  344.3 |
| N Components  |  505.9 |
| N Secondaries |  505.9 |
| LCC Capacity  |  339.6 |
| Centroid X    |  939.5 |
| Centroid Y    | 1287.5 |

### Autocorrelation (Capacity)

- Lag 1: 0.964
- Lag 10: 0.638
- Lag 50: 0.134

### Secondary Dynamics

- Secondary births: 1898
- Secondary deaths: 1884

### Secondary Count Distribution

**Per-Chain Summary:**

| Chain   | Mean |   SD | Min | Max | k=0 % |
|:--------|-----:|-----:|----:|----:|:------|
| chain_1 | 7.74 | 1.64 |   0 |  12 | 0.0%  |
| chain_2 | 7.79 | 1.67 |   2 |  16 | 0.0%  |
| chain_3 | 7.77 | 1.53 |   0 |  13 | 0.1%  |
| chain_4 | 7.43 | 1.63 |   1 |  13 | 0.0%  |

**Overall Distribution (All Chains):**

- Mean k: 7.68
- SD k: 1.63
- Range: 0 - 16
- Time at k=0: 0.03%

| k   | Count | Pct   |
|:----|------:|:------|
| 0   |     6 | 0.0%  |
| 1   |    15 | 0.1%  |
| 2   |    19 | 0.1%  |
| 3   |    94 | 0.5%  |
| 4   |   351 | 1.8%  |
| 5   |  1023 | 5.1%  |
| 6   |  2828 | 14.1% |
| 7   |  4751 | 23.8% |
| 8   |  5169 | 25.8% |
| 9   |  3350 | 16.8% |
| 10  |  1588 | 7.9%  |
| 11  |   638 | 3.2%  |
| 12  |    89 | 0.4%  |
| 13  |    38 | 0.2%  |
| 14  |    16 | 0.1%  |
| 15  |    13 | 0.1%  |
| 16  |    12 | 0.1%  |

### LCC Capacity Distribution

- Mean: 1701
- SD: 289
- Range: 1138 - 3164

## Move Acceptance (Aggregated)

| Type               | Attempted | Feasible% | MH Accept% | Overall% |
|:-------------------|----------:|----------:|-----------:|---------:|
| lcc_local_add      |      1024 |      99.4 |       86.5 |     86.0 |
| lcc_local_remove   |      1040 |      90.8 |       96.8 |     87.9 |
| lifted_birth_death |      7039 |      89.5 |       60.0 |     53.7 |
| secondary_swap     |      1944 |      74.2 |       92.2 |     68.5 |
| replace_lcc        |      8953 |     100.0 |       47.7 |     47.7 |

### Birth/Death Kernel Statistics

**Kernel Mode: Lifted Birth/Death**

*Lifted MCMC maintains a momentum variable (birth/death direction). On
acceptance, the direction persists; on rejection, it flips. This creates
correlated sequences that can traverse k (secondary count) faster than
reversible proposals.*

- Total attempted: 7,039
- Feasible: 6,300 (89.5%)
- Accepted: 3,782 (60% of feasible)

**Direction breakdown (accepted moves):**

- Births: 1,898
- Deaths: 1,884
- Birth/Death ratio: 1.01

**Universe sizes (\|U\| = \|addable\| + \|removable\|):**

    Universe sizes were not stored in these chain outputs.

### Birth/Death Candidate Pools by k

*Forward proposal pool sizes for the active birth/death kernel. These
are the diagnostics that matter for add/remove imbalance; `|U|` alone
can hide a large `n_add / n_rem` skew.*

    Per-attempt birth/death pool diagnostics are not available in these chain outputs.

## Kernel Profiling

*Per-kernel timing breakdown for MCMC execution.*

    **Per-Chain Timing (seconds):**



    |Chain | LCC Local| Birth/Death| Swap| Replace LCC| Overhead|  Total|
    |:-----|---------:|-----------:|----:|-----------:|--------:|------:|
    |1     |      3.13|       13.16| 6.96|      842.48|    18.24| 883.96|
    |2     |      3.38|       13.45| 7.10|      770.85|    22.45| 817.24|
    |3     |      3.04|       13.39| 7.16|      770.86|    22.00| 816.44|
    |4     |      3.22|       13.83| 6.95|      758.96|    22.26| 805.22|

    **Aggregate Time Distribution:**

    - LCC Local: 0.4% (12.8s)
    - Birth/Death: 1.6% (53.8s)
    - Swap: 0.8% (28.2s)
    - Replace LCC: 94.6% (3143.1s)
    - Overhead: 2.6% (85.0s)
    - **Total: 3322.9s** across 4 chains

## Convergence (Multi-Chain)

### R-hat

| metric       |  rhat | rhat_upper |    n_eff |
|:-------------|------:|-----------:|---------:|
| capacity     | 1.008 |      1.016 |  344.315 |
| n_components | 1.005 |      1.014 |  505.867 |
| lcc_capacity | 1.012 |      1.028 |  339.608 |
| centroid_x   | 1.006 |      1.014 |  939.512 |
| centroid_y   | 1.008 |      1.015 | 1287.482 |

### Chain Separation (Jaccard Overlap)

- Min pairwise overlap: 0.729
- Separated: FALSE
- Evidence: Mixing OK: Min pairwise Jaccard overlap = 0.729 \>= 0.10
  threshold

**Overlap Matrix:**

|         | chain_1 | chain_2 | chain_3 | chain_4 |
|:--------|--------:|--------:|--------:|--------:|
| chain_1 |   1.000 |   0.790 |   0.803 |   0.753 |
| chain_2 |   0.790 |   1.000 |   0.755 |   0.729 |
| chain_3 |   0.803 |   0.755 |   1.000 |   0.754 |
| chain_4 |   0.753 |   0.729 |   0.754 |   1.000 |

### Chain-Specific Statistics

**LCC Capacity by Chain:**

| Chain   | Region  | Mean |  SD |  Min |  Max |
|:--------|:--------|-----:|----:|-----:|-----:|
| chain_1 | chain_1 | 1712 | 288 | 1142 | 3056 |
| chain_2 | chain_2 | 1724 | 329 | 1138 | 3164 |
| chain_3 | chain_3 | 1674 | 243 | 1193 | 3050 |
| chain_4 | chain_4 | 1694 | 288 | 1138 | 3065 |

**N Components by Chain:**

| Chain   | Mean |   SD | Min | Max |
|:--------|-----:|-----:|----:|----:|
| chain_1 | 8.74 | 1.64 |   1 |  13 |
| chain_2 | 8.79 | 1.67 |   3 |  17 |
| chain_3 | 8.77 | 1.53 |   1 |  14 |
| chain_4 | 8.43 | 1.63 |   2 |  14 |

**Capacity by Chain:**

| Chain   | Mean |  SD |  Min |  Max |
|:--------|-----:|----:|-----:|-----:|
| chain_1 | 3146 | 512 | 2143 | 5974 |
| chain_2 | 3169 | 617 | 2059 | 6045 |
| chain_3 | 3105 | 443 | 2070 | 4768 |
| chain_4 | 3116 | 515 | 2107 | 6057 |

**LCC Transition Rates by Chain:**

| Chain   | Region  | Transitions | Rate  |
|:--------|:--------|------------:|:------|
| chain_1 | chain_1 |         696 | 13.9% |
| chain_2 | chain_2 |         678 | 13.6% |
| chain_3 | chain_3 |         671 | 13.4% |
| chain_4 | chain_4 |         713 | 14.3% |

### Geographic Coverage by Region

| Region | Parcels | Capacity | Mean Visit% | Mean Cap% | Min Visit% | Max Visit% |
|:-------|--------:|---------:|------------:|----------:|-----------:|-----------:|
| all    |    8052 |   141707 |        70.1 |      32.9 |       69.6 |       70.9 |

## State Space Exploration

### Unique LCCs Visited

- Mean unique LCC capacity values per chain: 1251
- Mean LCC transitions per chain: 1250
- LCC transition rate: 25%

### Parcel-Unit Stickiness

| Category          | Count | % of Total |
|-------------------|-------|------------|
| Always in (\>95%) | 0     | 0%         |
| Rarely in (\<5%)  | 4011  | 49.8%      |
| Variable          | 4041  | 50.2%      |

| Inclusion Range | Count |
|:----------------|------:|
| (0.25,0.5\]     |   100 |
| (0.05,0.25\]    |  3941 |
| \[0,0.05\]      |  4011 |

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

    Rarely visited (<5%) parcels: 4,009

      - Unreachable: 581 (14.5%)

      - Feasible but rarely visited: 3,428 (85.5%)

**Stickiness by Feasibility Class:**

| Classification    | Parcels | Never Visited | % Never | Mean Incl% | Max Incl% |
|:------------------|--------:|--------------:|--------:|-----------:|----------:|
| lcc_and_secondary |    5349 |             2 |     0.0 |       9.53 |      36.0 |
| lcc_only          |     484 |            39 |     8.1 |       2.17 |      16.8 |
| secondary_only    |    1638 |           302 |    18.4 |       1.64 |      13.8 |
| unreachable       |     581 |           580 |    99.8 |       0.00 |       0.0 |

- Feasible but rarely visited: 42.6% of all parcels

### Unique States Visited

- Unique parcel states (across all chains): 1,995
- Total thinned samples: 2,000
- Uniqueness ratio: 99.8%

## Kernel-Specific Diagnostics

### Replace-LCC Rejection Breakdown

| Reason                     | Count | % of Attempted |
|:---------------------------|------:|---------------:|
| attempted                  |  8953 |          100.0 |
| proposed                   |  8953 |          100.0 |
| feasible                   |  8951 |          100.0 |
| mh_rejected                |  4682 |           52.3 |
| accepted                   |  4269 |           47.7 |
| infeasible                 |     2 |            0.0 |
| proposal_failed            |     0 |            0.0 |
| current_lcc_not_in_library |     0 |            0.0 |
| no_candidates              |     0 |            0.0 |
| secondaries_incompatible   |     0 |            0.0 |
| no_similar_capacity_lcc    |     0 |            0.0 |
| selected_lcc_invalid       |     0 |            0.0 |
| no_reverse_candidates      |     0 |            0.0 |

| Constraint  | Failures | % of Failures |
|:------------|---------:|--------------:|
| min_density |        2 |           100 |

**Replace-LCC MH Acceptance Analysis:**

    - MH proposals evaluated: 8,951
    - Mean accept prob: 0.481
    - Median accept prob: 0.378
    - Accept prob < 0.1: 2342 (26.2%)
    - Accept prob > 0.9: 2492 (27.8%)


    **Log proposal ratio (log q(x'→x) / q(x→x')):**

    - Mean: -1.13
    - SD: 1.52
    - Range: -4.78 to 4.33


    **Secondary blocks retained during LCC replacement:**

| k Retained | Count |
|:-----------|------:|
| 0          |     2 |
| 1          |     6 |
| 2          |     7 |
| 3          |    50 |
| 4          |   171 |
| 5          |   437 |
| 6          |  1264 |
| 7          |  2140 |
| 8          |  2303 |
| 9          |  1529 |
| 10         |   668 |
| 11         |   303 |
| 12         |    36 |
| 13         |    16 |
| 14         |     7 |
| 15         |     7 |
| 16         |     5 |

**Replace-LCC Similar LCC Counts:**

    - Forward similar LCCs: mean=436.8, sd=197.8
    - Reverse similar LCCs: mean=471.7, sd=208.9
    - Asymmetry ratio (fwd/rev): mean=0.93

### Birth/Death Direction Analysis

    **Direction of Accepted Moves:**


    ✓ Balanced: 1.01 births per death

### Legacy Multi-Move Birth/Death (r-value)

*Note: These diagnostics are for legacy multi-move birth/death kernels.
Current architecture uses symmetric_birth_death which proposes single
blocks.*

    Legacy multi-birth kernel not active (using symmetric_birth_death instead).

    Legacy multi-death kernel not active (using symmetric_birth_death instead).

### Swap Kernel Diagnostics

    **Capacity Change for Accepted Swaps:**

    - Total accepted swaps: 1,331
    - Mean capacity delta: 0.4
    - SD capacity delta: 65.6
    - Range: -150 to 150


    **Proposal Asymmetry (similar block counts):**

    - Forward similar blocks: mean=196.7, sd=38.0
    - Reverse similar blocks: mean=199.8, sd=38.5
    - Ratio (fwd/rev): mean=1.00

    **Swap Rejection Breakdown:**

| Reason      | Count | % of Attempted |
|:------------|------:|---------------:|
| attempted   |  1944 |          100.0 |
| proposed    |  1944 |          100.0 |
| feasible    |  1443 |           74.2 |
| accepted    |  1331 |           68.5 |
| infeasible  |   501 |           25.8 |
| mh_rejected |   112 |            5.8 |


    **Swap Constraint Failures:**

| Constraint           | Failures |
|:---------------------|---------:|
| min_lcc_fraction     |      266 |
| station_capacity_pct |      234 |
| station_area_pct     |        1 |

## Capacity Prior Diagnostics

The capacity prior penalizes capacity above min_capacity with a linear
penalty: `penalty = λ × (capacity - min_capacity)` where λ = 0.005.

- Penalty source: recomputed from capacity trajectory
- Steps above min_capacity: 20,000 / 20,000 (100%)

<!-- -->

    **Penalty Statistics (when above min_capacity):**

    - Mean penalty: 5.4451
    - Max penalty: 20.0600
    - Mean excess capacity: 1089

## Library Utilization

### LCC Library

- Initial LCC library size: 3250
- Final library sizes: 3643, 3619, 3619, 3619
- Online enrichment adds: 1500

### Secondary Library

- Secondary library size: 500
- Lifted Birth/Death acceptance: 53.7%
- Lifted Birth/Death feasibility: 89.5%

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
| Convergence (R-hat)             | 1.012             |
| ESS                             | 340               |
| Chain Mixing                    | 0.729 min overlap |
| LCC Exploration                 | 25.00%            |
| Lifted Birth/Death Acceptance   | 53.7% acceptance  |
| Replace-LCC Feasibility         | 100.0%            |
| Parcel Variability (stickiness) | 49.8% sticky      |
