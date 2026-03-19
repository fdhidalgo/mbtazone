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
| Mean capacity           | 1999    | 1329    |
| Median capacity         | 1905    | 1298    |
| High-capacity (≥2045) % | 30.8%   | 0.3%    |


    **Capacity ratio (sampled/library): 0.66**

    ℹ️ **Posterior favors lower-capacity LCCs**: This is expected behavior.
    The min_lcc_fraction constraint (≥50%) limits how many secondaries can be added
    with high-capacity LCCs. Lower-capacity LCCs allow more secondary configurations,
    so the posterior has more mass there. This is not a proposal bias.

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

### Posterior-Weighted Library Accessibility

*Average library accessibility weighted by the observed k distribution.*

- Mean secondary block capacity (library): 312
- Posterior-weighted library accessibility: 38.8%

### Sampled LCC Capacity by k

*Observed relationship between secondary count and LCC capacity in
sampled states.*

|   k | N Samples | Mean LCC Cap | Min LCC Cap | Max LCC Cap |
|----:|----------:|-------------:|------------:|------------:|
|   2 |       149 |         1594 |        1176 |        2387 |
|   3 |       537 |         1551 |        1044 |        3065 |
|   4 |       760 |         1390 |        1123 |        3065 |
|   5 |      1397 |         1374 |        1113 |        1683 |
|   6 |      4011 |         1298 |        1113 |        1798 |
|   7 |      5231 |         1282 |        1088 |        1813 |
|   8 |      5270 |         1327 |        1105 |        1831 |
|   9 |      2317 |         1360 |        1161 |        1717 |
|  10 |       325 |         1418 |        1265 |        1633 |
|  11 |         3 |         1475 |        1469 |        1488 |

- Correlation(k, LCC capacity): -0.13

### Secondary Capacity per Block

*The chain selects secondary blocks to satisfy the min_lcc_fraction
constraint.*

|   k | N Samples | Mean Total Sec Cap | Mean Cap/Block |
|----:|----------:|-------------------:|---------------:|
|   2 |       149 |               1093 |            546 |
|   3 |       537 |               1103 |            368 |
|   4 |       760 |               1096 |            274 |
|   5 |      1397 |               1036 |            207 |
|   6 |      4011 |               1053 |            176 |
|   7 |      5231 |               1097 |            157 |
|   8 |      5270 |               1157 |            145 |
|   9 |      2317 |               1265 |            141 |
|  10 |       325 |               1376 |            138 |
|  11 |         3 |               1462 |            133 |

- Library mean secondary capacity: 312
- Sampled mean secondary capacity per block: 172

### Total Capacity Relative to Minimum

*Shows how far sampled plans sit above the statutory minimum capacity.*

| Metric                    | Value |
|:--------------------------|:------|
| Minimum required capacity | 2045  |
| Mean total capacity       | 2452  |
| Median total capacity     | 2413  |
| Mean excess capacity      | 407   |
| Median excess capacity    | 368   |
| 90th percentile excess    | 734   |
| Max excess capacity       | 4012  |
| States within +100 units  | 6.5%  |
| States within +250 units  | 28.7% |
| States \>= 1.25× minimum  | 28.3% |
| States \>= 1.5× minimum   | 0.8%  |

### Excess Capacity by Secondary Count (k)

*Links secondary count to how far sampled plans sit above the minimum
requirement.*

|   k | N Samples | Mean Total Cap | Mean Excess | Median Excess | Pct \<= +100 |
|----:|----------:|---------------:|------------:|--------------:|:-------------|
|   2 |       149 |           2687 |         642 |           498 | 0.7%         |
|   3 |       537 |           2653 |         608 |           288 | 15.6%        |
|   4 |       760 |           2487 |         442 |           518 | 23.7%        |
|   5 |      1397 |           2410 |         365 |           293 | 28.1%        |
|   6 |      4011 |           2352 |         307 |           226 | 13.2%        |
|   7 |      5231 |           2379 |         334 |           301 | 2.0%         |
|   8 |      5270 |           2485 |         440 |           419 | 0.0%         |
|   9 |      2317 |           2625 |         580 |           552 | 0.0%         |
|  10 |       325 |           2795 |         750 |           780 | 0.0%         |
|  11 |         3 |           2938 |         893 |           877 | 0.0%         |

### Purge / Standalone-LCC Diagnostic

*Assesses whether low-secondary states are absent because the LCC rarely
meets the minimum requirement on its own.*

    Full purge diagnostic results are not present in these chain outputs.

    Capacity-only proxy below: `LCC >= min_capacity` means the main component alone clears the legal capacity threshold.

| LCC \>= min_capacity | N Steps | Pct    | Mean k | Mean Total Cap | Mean Excess |
|:---------------------|--------:|:-------|-------:|---------------:|------------:|
| Yes                  |      65 | 0.33%  |   3.08 |           4871 |        2826 |
| No                   |   19935 | 99.67% |   6.96 |           2445 |         400 |

## Mixing Diagnostics (Multi-Chain)

### Effective Sample Size (ESS)

*ESS via coda::effectiveSize() across all chains.*

| Metric        |   ESS |
|:--------------|------:|
| Capacity      | 205.8 |
| N Components  | 177.8 |
| N Secondaries | 177.8 |
| LCC Capacity  | 137.4 |
| Centroid X    | 314.1 |
| Centroid Y    | 323.9 |

### Autocorrelation (Capacity)

- Lag 1: 0.953
- Lag 10: 0.741
- Lag 50: 0.349

### Secondary Dynamics

- Secondary births: 1232
- Secondary deaths: 1219

### Secondary Count Distribution

**Per-Chain Summary:**

| Chain   | Mean |   SD | Min | Max | k=0 % |
|:--------|-----:|-----:|----:|----:|:------|
| chain_1 | 7.10 | 1.36 |   3 |  11 | 0.0%  |
| chain_2 | 7.27 | 1.51 |   2 |  11 | 0.0%  |
| chain_3 | 6.32 | 1.13 |   2 |   9 | 0.0%  |
| chain_4 | 7.09 | 1.81 |   2 |  10 | 0.0%  |

**Overall Distribution (All Chains):**

- Mean k: 6.95
- SD k: 1.52
- Range: 2 - 11
- Time at k=0: 0.00%

| k   | Count | Pct   |
|:----|------:|:------|
| 2   |   149 | 0.7%  |
| 3   |   537 | 2.7%  |
| 4   |   760 | 3.8%  |
| 5   |  1397 | 7.0%  |
| 6   |  4011 | 20.1% |
| 7   |  5231 | 26.2% |
| 8   |  5270 | 26.4% |
| 9   |  2317 | 11.6% |
| 10  |   325 | 1.6%  |
| 11  |     3 | 0.0%  |

### LCC Capacity Distribution

- Mean: 1329
- SD: 150
- Range: 1044 - 3065

## Move Acceptance (Aggregated)

| Type                  | Attempted | Feasible% | MH Accept% | Overall% |
|:----------------------|----------:|----------:|-----------:|---------:|
| lcc_local_add         |      1212 |      99.8 |       80.1 |     79.9 |
| lcc_local_remove      |      1176 |      87.2 |       97.3 |     84.9 |
| symmetric_birth_death |      6267 |      81.1 |       48.2 |     39.1 |
| secondary_swap        |      2389 |      55.5 |       69.4 |     38.5 |
| replace_lcc           |      8956 |      99.9 |       17.8 |     17.8 |

### Birth/Death Kernel Statistics

**Kernel Mode: Symmetric Birth/Death**

*The symmetric_birth_death kernel proposes either adding or removing a
secondary block with equal probability, then applies MH correction.*

- Total attempted: 6,267
- Feasible: 5,080 (81.1%)
- Accepted: 2,451 (48.2% of feasible)

**Direction breakdown (accepted moves):**

- Births: 1,232
- Deaths: 1,219
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
    |1     |      3.59|       11.53| 7.52|      699.88|    17.09| 739.60|
    |2     |      3.53|       12.62| 7.74|      670.03|    17.99| 711.90|
    |3     |      3.28|       11.53| 7.80|      681.62|    17.44| 721.67|
    |4     |      3.39|       12.33| 7.22|      690.82|    17.83| 731.58|

    **Aggregate Time Distribution:**

    - LCC Local: 0.5% (13.8s)
    - Birth/Death: 1.7% (48.0s)
    - Swap: 1.0% (30.3s)
    - Replace LCC: 94.4% (2742.3s)
    - Overhead: 2.4% (70.3s)
    - **Total: 2904.7s** across 4 chains

## Convergence (Multi-Chain)

### R-hat

| metric       |  rhat | rhat_upper |   n_eff |
|:-------------|------:|-----------:|--------:|
| capacity     | 1.287 |      1.714 | 205.809 |
| n_components | 1.200 |      1.524 | 177.818 |
| lcc_capacity | 1.348 |      1.843 | 137.401 |
| centroid_x   | 1.097 |      1.262 | 314.114 |
| centroid_y   | 1.044 |      1.125 | 323.852 |

### Chain Separation (Jaccard Overlap)

- Min pairwise overlap: 0.593
- Separated: FALSE
- Evidence: Mixing OK: Min pairwise Jaccard overlap = 0.593 \>= 0.10
  threshold

**Overlap Matrix:**

|         | chain_1 | chain_2 | chain_3 | chain_4 |
|:--------|--------:|--------:|--------:|--------:|
| chain_1 |   1.000 |   0.655 |   0.593 |   0.654 |
| chain_2 |   0.655 |   1.000 |   0.593 |   0.659 |
| chain_3 |   0.593 |   0.593 |   1.000 |   0.621 |
| chain_4 |   0.654 |   0.659 |   0.621 |   1.000 |

### Chain-Specific Statistics

**LCC Capacity by Chain:**

| Chain   | Region  | Mean |  SD |  Min |  Max |
|:--------|:--------|-----:|----:|-----:|-----:|
| chain_1 | chain_1 | 1309 | 132 | 1105 | 3026 |
| chain_2 | chain_2 | 1399 | 113 | 1138 | 2809 |
| chain_3 | chain_3 | 1269 | 146 | 1088 | 2387 |
| chain_4 | chain_4 | 1338 | 174 | 1044 | 3065 |

**N Components by Chain:**

| Chain   | Mean |   SD | Min | Max |
|:--------|-----:|-----:|----:|----:|
| chain_1 | 8.10 | 1.36 |   4 |  12 |
| chain_2 | 8.27 | 1.51 |   3 |  12 |
| chain_3 | 7.32 | 1.13 |   3 |  10 |
| chain_4 | 8.09 | 1.81 |   3 |  11 |

**Capacity by Chain:**

| Chain   | Mean |  SD |  Min |  Max |
|:--------|-----:|----:|-----:|-----:|
| chain_1 | 2390 | 235 | 2046 | 5974 |
| chain_2 | 2553 | 222 | 2045 | 5617 |
| chain_3 | 2372 | 261 | 2045 | 4768 |
| chain_4 | 2495 | 313 | 2047 | 6057 |

**LCC Transition Rates by Chain:**

| Chain   | Region  | Transitions | Rate |
|:--------|:--------|------------:|:-----|
| chain_1 | chain_1 |         168 | 3.4% |
| chain_2 | chain_2 |         205 | 4.1% |
| chain_3 | chain_3 |         119 | 2.4% |
| chain_4 | chain_4 |         174 | 3.5% |

### Geographic Coverage by Region

| Region | Parcels | Capacity | Mean Visit% | Mean Cap% | Min Visit% | Max Visit% |
|:-------|--------:|---------:|------------:|----------:|-----------:|-----------:|
| all    |    8052 |   141707 |        55.8 |      20.2 |       53.5 |       59.2 |

## State Space Exploration

### Unique LCCs Visited

- Mean unique LCC capacity values per chain: 571.8
- Mean LCC transitions per chain: 571
- LCC transition rate: 11.41%

### Parcel-Unit Stickiness

| Category          | Count | % of Total |
|-------------------|-------|------------|
| Always in (\>95%) | 0     | 0%         |
| Rarely in (\<5%)  | 4972  | 61.7%      |
| Variable          | 3080  | 38.3%      |

| Inclusion Range | Count |
|:----------------|------:|
| (0.5,0.75\]     |     1 |
| (0.25,0.5\]     |   318 |
| (0.05,0.25\]    |  2761 |
| \[0,0.05\]      |  4972 |

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

    Rarely visited (<5%) parcels: 4,971

      - Unreachable: 581 (11.7%)

      - Feasible but rarely visited: 4,390 (88.3%)

**Stickiness by Feasibility Class:**

| Classification    | Parcels | Never Visited | % Never | Mean Incl% | Max Incl% |
|:------------------|--------:|--------------:|--------:|-----------:|----------:|
| lcc_and_secondary |    5349 |           196 |     3.7 |       8.70 |      50.7 |
| lcc_only          |     484 |           137 |    28.3 |       1.63 |      28.5 |
| secondary_only    |    1638 |           661 |    40.4 |       0.61 |       8.3 |
| unreachable       |     581 |           581 |   100.0 |       0.00 |       0.0 |

- Feasible but rarely visited: 54.5% of all parcels

### Unique States Visited

- Unique parcel states (across all chains): 1,937
- Total thinned samples: 2,000
- Uniqueness ratio: 96.8%

## Kernel-Specific Diagnostics

### Replace-LCC Rejection Breakdown

| Reason                     | Count | % of Attempted |
|:---------------------------|------:|---------------:|
| attempted                  |  8956 |          100.0 |
| proposed                   |  8956 |          100.0 |
| feasible                   |  8948 |           99.9 |
| mh_rejected                |  7354 |           82.1 |
| accepted                   |  1594 |           17.8 |
| infeasible                 |     8 |            0.1 |
| proposal_failed            |     0 |            0.0 |
| current_lcc_not_in_library |     0 |            0.0 |
| no_candidates              |     0 |            0.0 |
| secondaries_incompatible   |     0 |            0.0 |
| no_similar_capacity_lcc    |     0 |            0.0 |
| selected_lcc_invalid       |     0 |            0.0 |
| no_reverse_candidates      |     0 |            0.0 |

| Constraint   | Failures | % of Failures |
|:-------------|---------:|--------------:|
| min_capacity |        8 |           100 |

**Replace-LCC MH Acceptance Analysis:**

    - MH proposals evaluated: 8,948
    - Mean accept prob: 0.176
    - Median accept prob: 0.005
    - Accept prob < 0.1: 6487 (72.5%)
    - Accept prob > 0.9: 906 (10.1%)


    **Log proposal ratio (log q(x'→x) / q(x→x')):**

    - Mean: -5.37
    - SD: 3.99
    - Range: -12.43 to 12.42


    **Secondary blocks retained during LCC replacement:**

| k Retained | Count |
|:-----------|------:|
| 2          |    66 |
| 3          |   236 |
| 4          |   359 |
| 5          |   592 |
| 6          |  1780 |
| 7          |  2316 |
| 8          |  2402 |
| 9          |  1044 |
| 10         |   152 |
| 11         |     1 |

**Replace-LCC Similar LCC Counts:**

    - Forward similar LCCs: mean=490.9, sd=212.7
    - Reverse similar LCCs: mean=551.2, sd=238.6
    - Asymmetry ratio (fwd/rev): mean=0.90

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

    - Total accepted swaps: 919
    - Mean capacity delta: 2.4
    - SD capacity delta: 51.6
    - Range: -149 to 150


    **Proposal Asymmetry (similar block counts):**

    - Forward similar blocks: mean=195.1, sd=36.6
    - Reverse similar blocks: mean=207.8, sd=40.2
    - Ratio (fwd/rev): mean=0.95

    **Swap Rejection Breakdown:**

| Reason      | Count | % of Attempted |
|:------------|------:|---------------:|
| attempted   |  2389 |          100.0 |
| proposed    |  2389 |          100.0 |
| feasible    |  1325 |           55.5 |
| infeasible  |  1064 |           44.5 |
| accepted    |   919 |           38.5 |
| mh_rejected |   406 |           17.0 |


    **Swap Constraint Failures:**

| Constraint           | Failures |
|:---------------------|---------:|
| station_capacity_pct |      661 |
| min_lcc_fraction     |      362 |
| min_capacity         |       34 |
| station_area_pct     |        7 |

## Capacity Prior Diagnostics

The capacity prior penalizes capacity above min_capacity with a linear
penalty: `penalty = λ × (capacity - min_capacity)` where λ = 0.005.

- Penalty source: recomputed from capacity trajectory
- Steps above min_capacity: 19,986 / 20,000 (99.9%)

<!-- -->

    **Penalty Statistics (when above min_capacity):**

    - Mean penalty: 2.0388
    - Max penalty: 20.0600
    - Mean excess capacity: 408

## Library Utilization

### LCC Library

- Initial LCC library size: 3250
- Final library sizes: 3672, 3654, 3655, 3633
- Online enrichment adds: 1614

### Secondary Library

- Secondary library size: 500
- Symmetric Birth/Death acceptance: 39.1%
- Symmetric Birth/Death feasibility: 81.1%

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

| Check                            | Value             |
|:---------------------------------|:------------------|
| Convergence (R-hat)              | 1.348             |
| ESS                              | 137               |
| Chain Mixing                     | 0.593 min overlap |
| LCC Exploration                  | 11.42%            |
| Symmetric Birth/Death Acceptance | 39.1% acceptance  |
| Replace-LCC Feasibility          | 99.9%             |
| Parcel Variability (stickiness)  | 61.7% sticky      |
