# MCMC Diagnostics Summary (LLM)


# MCMC Diagnostics Summary

## Overview

- Total Chains: 4
- Total Steps: 20000 (5000 per chain)

## Current Tuning

- Capacity prior λ: 0.002
- Secondary-count prior λ: 0.25
- Secondary minimum area: 5 acres
- Discovery capacity multiplier: 2.5×
- MCMC steps per chain: 5000
- Burn-in: 1000
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

- Candidates: 5000
- Capacity range: 1043 - 5112

### LCC Capacity: Library vs Sampled

*Compares library capacity distribution to sampled distribution. A
discrepancy reflects the posterior distribution shape, not proposal bias
(proposals are uniform).*

| Metric                  | Library | Sampled |
|:------------------------|:--------|:--------|
| Mean capacity           | 2994    | 2422    |
| Median capacity         | 2833    | 2397    |
| High-capacity (≥2045) % | 65.0%   | 70.0%   |


    **Capacity ratio (sampled/library): 0.81**

    ✓ Sampled distribution matches library distribution.

### LCC Library Capacity Bands

*Distribution of LCC capacities in the library relative to
min_capacity.*

| Capacity Band            | N LCCs | % of Library |
|:-------------------------|-------:|:-------------|
| \< min_capacity (2045)   |   1750 | 35.0%        |
| \[min_cap, 1.25×min_cap) |    349 | 7.0%         |
| \[1.25×, 1.5×min_cap)    |   1151 | 23.0%        |
| \[1.5×, 2×min_cap)       |    352 | 7.0%         |
| \>= 2×min_capacity       |   1398 | 28.0%        |

### Library Accessibility by Secondary Count (k)

*The min_lcc_fraction constraint requires LCC capacity \>= total
secondary capacity. As k increases, fewer LCCs are accessible.*

|   k | Est. Sec. Cap. | Min LCC Required | N Accessible | % Library |
|----:|---------------:|-----------------:|-------------:|:----------|
|   0 |          0.000 |            0.000 |         5000 | 100.0%    |
|   1 |        312.064 |          312.064 |         5000 | 100.0%    |
|   2 |        624.128 |          624.128 |         5000 | 100.0%    |
|   3 |        936.192 |          936.192 |         5000 | 100.0%    |
|   4 |       1248.256 |         1248.256 |         4919 | 98.4%     |
|   5 |       1560.320 |         1560.320 |         4231 | 84.6%     |
|   6 |       1872.384 |         1872.384 |         3851 | 77.0%     |
|   7 |       2184.448 |         2184.448 |         3179 | 63.6%     |
|   8 |       2496.512 |         2496.512 |         2950 | 59.0%     |

### Posterior-Weighted Library Accessibility

*Average library accessibility weighted by the observed k distribution.*

- Mean secondary block capacity (library): 312
- Posterior-weighted library accessibility: 99.6%

### Sampled LCC Capacity by k

*Observed relationship between secondary count and LCC capacity in
sampled states.*

|   k | N Samples | Mean LCC Cap | Min LCC Cap | Max LCC Cap |
|----:|----------:|-------------:|------------:|------------:|
|   0 |      6229 |         2655 |        2045 |        5131 |
|   1 |      5190 |         2336 |        1292 |        4822 |
|   2 |      2653 |         2262 |        1383 |        3988 |
|   3 |      1186 |         2111 |        1311 |        3659 |
|   4 |       431 |         2123 |        1408 |        3212 |
|   5 |       212 |         2098 |        1406 |        3056 |
|   6 |        82 |         2250 |        1502 |        3037 |
|   7 |        12 |         2124 |        1889 |        3037 |
|   8 |         5 |         3033 |        3026 |        3037 |

- Correlation(k, LCC capacity): -0.352

### Secondary Capacity per Block

*The chain selects secondary blocks to satisfy the min_lcc_fraction
constraint.*

|   k | N Samples | Mean Total Sec Cap | Mean Cap/Block |
|----:|----------:|-------------------:|---------------:|
|   1 |      5190 |                282 |            282 |
|   2 |      2653 |                536 |            268 |
|   3 |      1186 |                837 |            279 |
|   4 |       431 |               1034 |            259 |
|   5 |       212 |               1421 |            284 |
|   6 |        82 |               1627 |            271 |
|   7 |        12 |               1892 |            270 |
|   8 |         5 |               2972 |            372 |

- Library mean secondary capacity: 312
- Sampled mean secondary capacity per block: 277

### Total Capacity Relative to Minimum

*Shows how far sampled plans sit above the statutory minimum capacity.*

| Metric                    | Value |
|:--------------------------|:------|
| Minimum required capacity | 2045  |
| Mean total capacity       | 2722  |
| Median total capacity     | 2667  |
| Mean excess capacity      | 677   |
| Median excess capacity    | 622   |
| 90th percentile excess    | 1308  |
| Max excess capacity       | 3972  |
| States within +100 units  | 12.0% |
| States within +250 units  | 24.0% |
| States \>= 1.25× minimum  | 58.3% |
| States \>= 1.5× minimum   | 20.5% |

### Excess Capacity by Secondary Count (k)

*Links secondary count to how far sampled plans sit above the minimum
requirement.*

|   k | N Samples | Mean Total Cap | Mean Excess | Median Excess | Pct \<= +100 |
|----:|----------:|---------------:|------------:|--------------:|:-------------|
|   0 |      6229 |           2655 |         610 |           634 | 16.5%        |
|   1 |      5190 |           2618 |         573 |           430 | 14.0%        |
|   2 |      2653 |           2798 |         753 |           598 | 3.7%         |
|   3 |      1186 |           2949 |         904 |           762 | 4.6%         |
|   4 |       431 |           3157 |        1112 |          1041 | 2.8%         |
|   5 |       212 |           3518 |        1473 |          1424 | 0.0%         |
|   6 |        82 |           3877 |        1832 |          1693 | 0.0%         |
|   7 |        12 |           4016 |        1972 |          1589 | 0.0%         |
|   8 |         5 |           6005 |        3960 |          3961 | 0.0%         |

### Purge / Standalone-LCC Diagnostic

*Assesses whether low-secondary states are absent because the LCC rarely
meets the minimum requirement on its own.*

    **Full purge diagnostic available:**

| Feasible After Dropping Secondaries | Constraint Failed    | Count | Pct   |
|:------------------------------------|:---------------------|------:|:------|
| TRUE                                | NA                   |   129 | 64.5% |
| FALSE                               | min_capacity         |    65 | 32.5% |
| FALSE                               | station_capacity_pct |     5 | 2.5%  |
| FALSE                               | station_area_pct     |     1 | 0.5%  |

## Mixing Diagnostics (Multi-Chain)

### Effective Sample Size (ESS)

*ESS via coda::effectiveSize() across all chains.*

| Metric        |    ESS |
|:--------------|-------:|
| Capacity      |  527.7 |
| N Components  |  497.2 |
| N Secondaries |  497.2 |
| LCC Capacity  |  283.8 |
| Centroid X    | 1418.4 |
| Centroid Y    | 1354.2 |

### Autocorrelation (Capacity)

- Lag 1: 0.949
- Lag 10: 0.573
- Lag 50: 0.081

### Secondary Dynamics

- Secondary births: 1127
- Secondary deaths: 1131

### Secondary Count Distribution

**Per-Chain Summary:**

| Chain   | Mean |   SD | Min | Max | k=0 % |
|:--------|-----:|-----:|----:|----:|:------|
| chain_1 | 0.93 | 1.20 |   0 |   7 | 48.1% |
| chain_2 | 1.16 | 1.24 |   0 |   8 | 35.4% |
| chain_3 | 0.89 | 1.06 |   0 |   6 | 45.1% |
| chain_4 | 1.38 | 1.37 |   0 |   7 | 29.8% |

**Overall Distribution (All Chains):**

- Mean k: 1.09
- SD k: 1.21
- Range: 0 - 8
- Time at k=0: 38.93%

| k   | Count | Pct   |
|:----|------:|:------|
| 0   |  6229 | 38.9% |
| 1   |  5190 | 32.4% |
| 2   |  2653 | 16.6% |
| 3   |  1186 | 7.4%  |
| 4   |   431 | 2.7%  |
| 5   |   212 | 1.3%  |
| 6   |    82 | 0.5%  |
| 7   |    12 | 0.1%  |
| 8   |     5 | 0.0%  |

### LCC Capacity Distribution

- Mean: 2422
- SD: 490
- Range: 1292 - 5131

## Move Acceptance (Aggregated)

| Type               | Attempted | Feasible% | MH Accept% | Overall% |
|:-------------------|----------:|----------:|-----------:|---------:|
| lcc_local_add      |      3477 |      99.8 |       89.5 |     89.3 |
| lcc_local_remove   |      3514 |      93.7 |       96.5 |     90.4 |
| lifted_birth_death |      5924 |      89.5 |       42.6 |     38.1 |
| secondary_swap     |      2027 |      92.0 |       51.0 |     46.9 |
| replace_lcc        |      5058 |      95.8 |       65.4 |     62.7 |

### Birth/Death Kernel Statistics

**Kernel Mode: Lifted Birth/Death**

*Lifted MCMC maintains a momentum variable (birth/death direction). On
acceptance, the direction persists; on rejection, it flips. This creates
correlated sequences that can traverse k (secondary count) faster than
reversible proposals.*

- Total attempted: 5,924
- Feasible: 5,301 (89.5%)
- Accepted: 2,259 (42.6% of feasible)

**Direction breakdown (accepted moves):**

- Births: 1,127
- Deaths: 1,132
- Birth/Death ratio: 1

**Universe sizes (\|U\| = \|addable\| + \|removable\|):**

    - Mean |U|: 421.5
    - Range: 2 - 500

### Birth/Death Candidate Pools by k

*Forward proposal pool sizes for the active birth/death kernel. These
are the diagnostics that matter for add/remove imbalance; `|U|` alone
can hide a large `n_add / n_rem` skew.*

| k | Attempts | Mean n_add | Mean n_rem | Mean n_add/n_rem | Pct No Addable | Pct No Removable | Accept % |
|---:|---:|---:|---:|---:|:---|:---|:---|
| 0 | 2420 | 441.8 | 0 | 441.80 | 0.0% | 100.0% | 20.6% |
| 1 | 1862 | 427.0 | 1 | 427.02 | 0.0% | 0.0% | 45.9% |
| 2 | 937 | 405.3 | 2 | 202.63 | 0.6% | 0.0% | 55.1% |
| 3 | 406 | 375.0 | 3 | 125.02 | 0.0% | 0.0% | 54.9% |
| 4 | 164 | 361.4 | 4 | 90.34 | 0.0% | 0.0% | 57.3% |
| 5 | 90 | 281.6 | 5 | 56.33 | 5.6% | 0.0% | 54.4% |
| 6 | 36 | 259.6 | 6 | 43.27 | 11.1% | 0.0% | 55.6% |
| 7 | 7 | 62.7 | 7 | 8.96 | 42.9% | 0.0% | 57.1% |
| 8 | 2 | 0.0 | 8 | 0.00 | 100.0% | 0.0% | 50.0% |

## Kernel Profiling

*Per-kernel timing breakdown for MCMC execution.*

    **Per-Chain Timing (seconds):**



    |Chain | LCC Local| Birth/Death| Swap| Replace LCC| Overhead|  Total|
    |:-----|---------:|-----------:|----:|-----------:|--------:|------:|
    |1     |      9.25|        8.13| 4.38|      405.98|    17.81| 445.55|
    |2     |      8.85|        9.14| 4.45|      437.82|    18.06| 478.32|
    |3     |      8.65|        8.13| 3.97|      428.64|    17.86| 467.25|
    |4     |      8.79|        9.05| 4.73|      422.64|    17.86| 463.08|

    **Aggregate Time Distribution:**

    - LCC Local: 1.9% (35.5s)
    - Birth/Death: 1.9% (34.5s)
    - Swap: 0.9% (17.5s)
    - Replace LCC: 91.4% (1695.1s)
    - Overhead: 3.9% (71.6s)
    - **Total: 1854.2s** across 4 chains

## Convergence (Multi-Chain)

### R-hat

| metric       |  rhat | rhat_upper |    n_eff |
|:-------------|------:|-----------:|---------:|
| capacity     | 1.010 |      1.024 |  527.735 |
| n_components | 1.029 |      1.080 |  497.187 |
| lcc_capacity | 1.042 |      1.113 |  283.823 |
| centroid_x   | 1.010 |      1.024 | 1418.416 |
| centroid_y   | 1.012 |      1.035 | 1354.189 |

### Chain Separation (Jaccard Overlap)

- Min pairwise overlap: 0.814
- Separated: FALSE
- Evidence: Mixing OK: Min pairwise Jaccard overlap = 0.814 \>= 0.10
  threshold

**Overlap Matrix:**

|         | chain_1 | chain_2 | chain_3 | chain_4 |
|:--------|--------:|--------:|--------:|--------:|
| chain_1 |   1.000 |   0.855 |   0.868 |   0.814 |
| chain_2 |   0.855 |   1.000 |   0.864 |   0.833 |
| chain_3 |   0.868 |   0.864 |   1.000 |   0.836 |
| chain_4 |   0.814 |   0.833 |   0.836 |   1.000 |

### Chain-Specific Statistics

**LCC Capacity by Chain:**

| Chain   | Region  | Mean |  SD |  Min |  Max |
|:--------|:--------|-----:|----:|-----:|-----:|
| chain_1 | chain_1 | 2541 | 474 | 1351 | 5080 |
| chain_2 | chain_2 | 2480 | 522 | 1292 | 4465 |
| chain_3 | chain_3 | 2480 | 455 | 1311 | 4427 |
| chain_4 | chain_4 | 2270 | 533 | 1269 | 5131 |

**N Components by Chain:**

| Chain   | Mean |   SD | Min | Max |
|:--------|-----:|-----:|----:|----:|
| chain_1 | 1.93 | 1.20 |   1 |   8 |
| chain_2 | 2.16 | 1.24 |   1 |   9 |
| chain_3 | 1.89 | 1.06 |   1 |   7 |
| chain_4 | 2.38 | 1.37 |   1 |   8 |

**Capacity by Chain:**

| Chain   | Mean |  SD |  Min |   Max |
|:--------|-----:|----:|-----:|------:|
| chain_1 | 2808 | 673 | 2047 | 10105 |
| chain_2 | 2825 | 677 | 2046 |  8902 |
| chain_3 | 2706 | 446 | 2045 |  4775 |
| chain_4 | 2661 | 503 | 2045 |  5131 |

**LCC Transition Rates by Chain:**

| Chain   | Region  | Transitions | Rate  |
|:--------|:--------|------------:|:------|
| chain_1 | chain_1 |         652 | 13.0% |
| chain_2 | chain_2 |         690 | 13.8% |
| chain_3 | chain_3 |         714 | 14.3% |
| chain_4 | chain_4 |         680 | 13.6% |

### Geographic Coverage by Region

| Region | Parcels | Capacity | Mean Visit% | Mean Cap% | Min Visit% | Max Visit% |
|:-------|--------:|---------:|------------:|----------:|-----------:|-----------:|
| all    |    8052 |   141707 |        55.8 |      25.2 |       54.8 |         57 |

## State Space Exploration

### Unique LCCs Visited

- Mean unique LCC capacity values per chain: 1446
- Mean LCC transitions per chain: 1445
- LCC transition rate: 28.9%

### Parcel-Unit Stickiness

| Category          | Count | % of Total |
|-------------------|-------|------------|
| Always in (\>95%) | 0     | 0%         |
| Rarely in (\<5%)  | 5298  | 65.8%      |
| Variable          | 2754  | 34.2%      |

| Inclusion Range | Count |
|:----------------|------:|
| (0.25,0.5\]     |   130 |
| (0.05,0.25\]    |  2624 |
| \[0,0.05\]      |  5298 |

### Parcel Feasibility Analysis

**Classification Summary:**

| Classification    | Count | % of Total |
|:------------------|------:|-----------:|
| lcc_and_secondary |  5644 |       70.1 |
| secondary_only    |  1343 |       16.7 |
| lcc_only          |   621 |        7.7 |
| unreachable       |   444 |        5.5 |

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
| small_area               |   327 |             73.6 |
| low_density              |    79 |             17.8 |
| not_in_discovered_blocks |    38 |              8.6 |

- **low_density**: Parcel density below threshold (15 units/acre)
- **small_area**: Area below 5-acre secondary threshold and not part of
  any LCC
- **not_in_discovered_blocks**: Parcel geometrically excluded from all
  discovered blocks

**Capacity by Classification:**

| Classification    | Total Capacity | % of Capacity |
|:------------------|---------------:|--------------:|
| lcc_and_secondary |          43822 |          30.9 |
| lcc_only          |          40253 |          28.4 |
| unreachable       |          39094 |          27.6 |
| secondary_only    |          18538 |          13.1 |

**Feasibility vs Stickiness:**

    Rarely visited (<5%) parcels: 5,298

      - Unreachable: 444 (8.4%)

      - Feasible but rarely visited: 4,854 (91.6%)

**Stickiness by Feasibility Class:**

| Classification    | Parcels | Never Visited | % Never | Mean Incl% | Max Incl% |
|:------------------|--------:|--------------:|--------:|-----------:|----------:|
| lcc_and_secondary |    5644 |             5 |     0.1 |       6.69 |      30.8 |
| lcc_only          |     621 |           140 |    22.5 |       2.36 |      29.0 |
| secondary_only    |    1343 |             9 |     0.7 |       0.47 |       2.5 |
| unreachable       |     444 |           444 |   100.0 |       0.00 |       0.0 |

- Feasible but rarely visited: 60.3% of all parcels

### Unique States Visited

- Unique parcel states (across all chains): 1,974
- Total thinned samples: 2,000
- Uniqueness ratio: 98.7%

## Kernel-Specific Diagnostics

### Replace-LCC Rejection Breakdown

| Reason                            | Count | % of Attempted |
|:----------------------------------|------:|---------------:|
| attempted                         |  5058 |          100.0 |
| proposed                          |  5058 |          100.0 |
| feasible                          |  4846 |           95.8 |
| accepted                          |  3169 |           62.7 |
| mh_rejected                       |  1677 |           33.2 |
| infeasible                        |   212 |            4.2 |
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
| min_capacity |      212 |           100 |

**Replace-LCC MH Acceptance Analysis:**

    - MH proposals evaluated: 846
    - Mean accept prob: 0.647
    - Median accept prob: 0.718
    - Accept prob < 0.1: 1 (0.1%)
    - Accept prob > 0.9: 322 (38.1%)


    **Log proposal ratio (log q(x'→x) / q(x→x')):**

    - Mean: -0.44
    - SD: 0.98
    - Range: -2.39 to 2.48


    **Replace-LCC proposal diagnostics:**

| Metric                      |   Mean | Median |  P90 |  Max |
|:----------------------------|-------:|-------:|-----:|-----:|
| Retained k (secondaries)    |   1.20 |      1 |    3 |    7 |
| Forward candidate LCC count | 983.04 |   1002 | 1332 | 1852 |

**Replace-LCC Proposal Geometry:**

    - Forward candidate LCCs: mean=983.0, sd=278.2
    - Reverse candidate LCCs: mean=1074.4, sd=261.1
    - Asymmetry ratio (fwd/rev): mean=0.95

### Birth/Death Direction Analysis

    **Direction of Accepted Moves:**


    ✓ Balanced: 1.00 births per death

### Legacy Multi-Move Birth/Death (r-value)

*Note: These diagnostics are for legacy multi-move birth/death kernels.
Current architecture uses symmetric_birth_death which proposes single
blocks.*

    Legacy multi-birth kernel not active (using symmetric_birth_death instead).

    Legacy multi-death kernel not active (using symmetric_birth_death instead).

### Swap Kernel Diagnostics

    **Capacity Change for Accepted Swaps:**

    - Total accepted swaps: 951
    - Mean capacity delta: -3.6
    - SD capacity delta: 74.5
    - Range: -150 to 150


    **Proposal Asymmetry (similar block counts):**

    - Forward similar blocks: mean=229.6, sd=63.1
    - Reverse similar blocks: mean=231.2, sd=63.9
    - Ratio (fwd/rev): mean=1.02

    **Swap Rejection Breakdown:**

| Reason          | Count | % of Attempted |
|:----------------|------:|---------------:|
| attempted       |  2027 |          100.0 |
| proposed        |  1210 |           59.7 |
| feasible        |  1048 |           51.7 |
| accepted        |   951 |           46.9 |
| proposal_failed |   817 |           40.3 |
| no_secondaries  |   817 |           40.3 |
| infeasible      |   162 |            8.0 |
| mh_rejected     |    97 |            4.8 |


    **Swap Constraint Failures:**

| Constraint           | Failures |
|:---------------------|---------:|
| station_capacity_pct |      119 |
| min_capacity         |       31 |
| min_lcc_fraction     |        6 |
| station_area_pct     |        6 |

## Capacity Prior Diagnostics

The capacity prior penalizes capacity above min_capacity with a linear
penalty: `penalty = λ × (capacity - min_capacity)` where λ = 0.002.

- Penalty source: runner diagnostics
- Steps above min_capacity: 19,980 / 20,000 (99.9%)

<!-- -->

    **Penalty Statistics (when above min_capacity):**

    - Mean penalty: 1.4114
    - Max penalty: 16.1200
    - Mean excess capacity: 706

## Library Utilization

### LCC Library

- Initial LCC library size: 5000
- Final library sizes: 5718, 5762, 5731, 5734
- Online enrichment adds: 2945

### Secondary Library

- Secondary library size: 500
- Lifted Birth/Death acceptance: 38.1%
- Lifted Birth/Death feasibility: 89.5%

## Library Coverage Statistics

*How well does the LCC library cover the parcel space?*

| Metric                     | Value |
|:---------------------------|:------|
| Total LCCs in library      | 5000  |
| Total parcels              | 8052  |
| Parcels covered (≥1 LCC)   | 6265  |
| Parcels uncovered (0 LCCs) | 1787  |
| Coverage rate              | 77.8% |
| Min coverage (excl. zero)  | 1     |
| Median coverage            | 235   |
| Mean coverage              | 237.5 |
| Max coverage               | 772   |
| Std dev coverage           | 200.6 |

### Coverage Distribution

| LCCs containing parcel | N Parcels | Percent |
|:-----------------------|----------:|:--------|
| 0                      |      1787 | 22.2%   |
| \>100                  |      5369 | 66.7%   |
| 2-5                    |       179 | 2.2%    |
| 1                      |        80 | 1.0%    |
| 6-10                   |        93 | 1.2%    |
| 11-50                  |       250 | 3.1%    |
| 51-100                 |       294 | 3.7%    |

## Diagnostic Health Summary

| Check                           | Value             |
|:--------------------------------|:------------------|
| Convergence (R-hat)             | 1.042             |
| ESS                             | 284               |
| Chain Mixing                    | 0.814 min overlap |
| LCC Exploration                 | 28.90%            |
| Lifted Birth/Death Acceptance   | 38.1% acceptance  |
| Replace-LCC Feasibility         | 95.8%             |
| Parcel Variability (stickiness) | 65.8% sticky      |
