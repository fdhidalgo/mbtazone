# MCMC Diagnostics Summary (LLM)


# MCMC Diagnostics Summary

## Overview

- Total Chains: 4
- Total Steps: 20000 (5000 per chain)

## Current Tuning

- Capacity prior λ: 0.005
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
| Mean capacity           | 2994    | 2039    |
| Median capacity         | 2833    | 2053    |
| High-capacity (≥2045) % | 65.0%   | 55.4%   |


    **Capacity ratio (sampled/library): 0.68**

    ℹ️ **Posterior favors lower-capacity LCCs**: This is expected behavior.
    The min_lcc_fraction constraint (≥50%) limits how many secondaries can be added
    with high-capacity LCCs. Lower-capacity LCCs allow more secondary configurations,
    so the posterior has more mass there. This is not a proposal bias.

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

### Posterior-Weighted Library Accessibility

*Average library accessibility weighted by the observed k distribution.*

- Mean secondary block capacity (library): 312
- Posterior-weighted library accessibility: 99.9%

### Sampled LCC Capacity by k

*Observed relationship between secondary count and LCC capacity in
sampled states.*

|   k | N Samples | Mean LCC Cap | Min LCC Cap | Max LCC Cap |
|----:|----------:|-------------:|------------:|------------:|
|   0 |      8967 |         2226 |        2045 |        4295 |
|   1 |      5841 |         2009 |        1423 |        4295 |
|   2 |      3073 |         1782 |        1296 |        4386 |
|   3 |      1609 |         1660 |        1188 |        4465 |
|   4 |       414 |         1668 |        1200 |        4682 |
|   5 |        67 |         1983 |        1392 |        5024 |
|   6 |        18 |         3351 |        1481 |        5064 |
|   7 |        11 |         5047 |        5025 |        5080 |

- Correlation(k, LCC capacity): -0.48

### Secondary Capacity per Block

*The chain selects secondary blocks to satisfy the min_lcc_fraction
constraint.*

|   k | N Samples | Mean Total Sec Cap | Mean Cap/Block |
|----:|----------:|-------------------:|---------------:|
|   1 |      5841 |                255 |            255 |
|   2 |      3073 |                550 |            275 |
|   3 |      1609 |                750 |            250 |
|   4 |       414 |               1009 |            252 |
|   5 |        67 |               1509 |            302 |
|   6 |        18 |               2987 |            498 |
|   7 |        11 |               5025 |            718 |

- Library mean secondary capacity: 312
- Sampled mean secondary capacity per block: 261

### Total Capacity Relative to Minimum

*Shows how far sampled plans sit above the statutory minimum capacity.*

| Metric                    | Value |
|:--------------------------|:------|
| Minimum required capacity | 2045  |
| Mean total capacity       | 2290  |
| Median total capacity     | 2161  |
| Mean excess capacity      | 245   |
| Median excess capacity    | 116   |
| 90th percentile excess    | 607   |
| Max excess capacity       | 8060  |
| States within +100 units  | 45.9% |
| States within +250 units  | 68.7% |
| States \>= 1.25× minimum  | 14.0% |
| States \>= 1.5× minimum   | 1.8%  |

### Excess Capacity by Secondary Count (k)

*Links secondary count to how far sampled plans sit above the minimum
requirement.*

|   k | N Samples | Mean Total Cap | Mean Excess | Median Excess | Pct \<= +100 |
|----:|----------:|---------------:|------------:|--------------:|:-------------|
|   0 |      8967 |           2226 |         181 |            61 | 69.1%        |
|   1 |      5841 |           2265 |         220 |           159 | 32.4%        |
|   2 |      3073 |           2332 |         287 |           227 | 23.7%        |
|   3 |      1609 |           2410 |         365 |           259 | 21.3%        |
|   4 |       414 |           2677 |         632 |           479 | 5.8%         |
|   5 |        67 |           3492 |        1447 |           771 | 0.0%         |
|   6 |        18 |           6338 |        4293 |          6846 | 0.0%         |
|   7 |        11 |          10072 |        8027 |          8005 | 0.0%         |

### Purge / Standalone-LCC Diagnostic

*Assesses whether low-secondary states are absent because the LCC rarely
meets the minimum requirement on its own.*

    **Full purge diagnostic available:**

| Feasible After Dropping Secondaries | Constraint Failed    | Count | Pct   |
|:------------------------------------|:---------------------|------:|:------|
| TRUE                                | NA                   |   111 | 55.5% |
| FALSE                               | min_capacity         |    87 | 43.5% |
| FALSE                               | station_capacity_pct |     2 | 1.0%  |

## Mixing Diagnostics (Multi-Chain)

### Effective Sample Size (ESS)

*ESS via coda::effectiveSize() across all chains.*

| Metric        |   ESS |
|:--------------|------:|
| Capacity      | 434.1 |
| N Components  | 224.7 |
| N Secondaries | 224.7 |
| LCC Capacity  | 152.8 |
| Centroid X    | 393.7 |
| Centroid Y    | 665.2 |

### Autocorrelation (Capacity)

- Lag 1: 0.956
- Lag 10: 0.644
- Lag 50: 0.24

### Secondary Dynamics

- Secondary births: 538
- Secondary deaths: 554

### Secondary Count Distribution

**Per-Chain Summary:**

| Chain   | Mean |   SD | Min | Max | k=0 % |
|:--------|-----:|-----:|----:|----:|:------|
| chain_1 | 0.76 | 1.00 |   0 |   7 | 50.9% |
| chain_2 | 0.81 | 1.07 |   0 |   6 | 53.2% |
| chain_3 | 1.25 | 1.18 |   0 |   6 | 32.3% |
| chain_4 | 0.98 | 1.06 |   0 |   4 | 43.0% |

**Overall Distribution (All Chains):**

- Mean k: 0.95
- SD k: 1.1
- Range: 0 - 7
- Time at k=0: 44.84%

| k   | Count | Pct   |
|:----|------:|:------|
| 0   |  8967 | 44.8% |
| 1   |  5841 | 29.2% |
| 2   |  3073 | 15.4% |
| 3   |  1609 | 8.0%  |
| 4   |   414 | 2.1%  |
| 5   |    67 | 0.3%  |
| 6   |    18 | 0.1%  |
| 7   |    11 | 0.1%  |

### LCC Capacity Distribution

- Mean: 2039
- SD: 366
- Range: 1188 - 5080

## Move Acceptance (Aggregated)

| Type               | Attempted | Feasible% | MH Accept% | Overall% |
|:-------------------|----------:|----------:|-----------:|---------:|
| lcc_local_add      |      3562 |     100.0 |       90.2 |     90.2 |
| lcc_local_remove   |      3493 |      89.2 |       97.0 |     86.6 |
| lifted_birth_death |      5940 |      81.5 |       22.6 |     18.4 |
| secondary_swap     |      2049 |      82.6 |       38.4 |     31.7 |
| replace_lcc        |      4956 |      88.0 |       38.9 |     34.3 |

### Birth/Death Kernel Statistics

**Kernel Mode: Lifted Birth/Death**

*Lifted MCMC maintains a momentum variable (birth/death direction). On
acceptance, the direction persists; on rejection, it flips. This creates
correlated sequences that can traverse k (secondary count) faster than
reversible proposals.*

- Total attempted: 5,940
- Feasible: 4,844 (81.5%)
- Accepted: 1,093 (22.6% of feasible)

**Direction breakdown (accepted moves):**

- Births: 538
- Deaths: 555
- Birth/Death ratio: 0.97

**Universe sizes (\|U\| = \|addable\| + \|removable\|):**

    - Mean |U|: 426.9
    - Range: 2 - 490

### Birth/Death Candidate Pools by k

*Forward proposal pool sizes for the active birth/death kernel. These
are the diagnostics that matter for add/remove imbalance; `|U|` alone
can hide a large `n_add / n_rem` skew.*

| k | Attempts | Mean n_add | Mean n_rem | Mean n_add/n_rem | Pct No Addable | Pct No Removable | Accept % |
|---:|---:|---:|---:|---:|:---|:---|:---|
| 0 | 2654 | 444.2 | 0 | 444.23 | 0.0% | 100.0% | 9.3% |
| 1 | 1792 | 428.0 | 1 | 427.96 | 0.0% | 0.0% | 22.8% |
| 2 | 893 | 412.3 | 2 | 206.15 | 0.2% | 0.0% | 28.0% |
| 3 | 456 | 379.5 | 3 | 126.51 | 0.0% | 0.0% | 28.5% |
| 4 | 118 | 314.0 | 4 | 78.50 | 0.8% | 0.0% | 38.1% |
| 5 | 20 | 244.8 | 5 | 48.95 | 0.0% | 0.0% | 40.0% |
| 6 | 4 | 239.8 | 6 | 39.96 | 25.0% | 0.0% | 75.0% |
| 7 | 3 | 0.0 | 7 | 0.00 | 100.0% | 0.0% | 33.3% |

## Kernel Profiling

*Per-kernel timing breakdown for MCMC execution.*

    **Per-Chain Timing (seconds):**



    |Chain | LCC Local| Birth/Death| Swap| Replace LCC| Overhead|  Total|
    |:-----|---------:|-----------:|----:|-----------:|--------:|------:|
    |1     |      8.44|        8.10| 3.63|      382.22|    16.86| 419.25|
    |2     |      8.57|        8.11| 3.20|      404.51|    16.14| 440.52|
    |3     |      8.33|        8.42| 4.52|      415.37|    16.15| 452.80|
    |4     |      8.73|        8.26| 4.08|      386.10|    15.89| 423.05|

    **Aggregate Time Distribution:**

    - LCC Local: 2.0% (34.1s)
    - Birth/Death: 1.9% (32.9s)
    - Swap: 0.9% (15.4s)
    - Replace LCC: 91.5% (1588.2s)
    - Overhead: 3.7% (65.0s)
    - **Total: 1735.6s** across 4 chains

## Convergence (Multi-Chain)

### R-hat

| metric       |  rhat | rhat_upper |   n_eff |
|:-------------|------:|-----------:|--------:|
| capacity     | 1.008 |      1.019 | 434.128 |
| n_components | 1.060 |      1.150 | 224.690 |
| lcc_capacity | 1.031 |      1.064 | 152.780 |
| centroid_x   | 1.088 |      1.237 | 393.743 |
| centroid_y   | 1.036 |      1.080 | 665.230 |

### Chain Separation (Jaccard Overlap)

- Min pairwise overlap: 0.609
- Separated: FALSE
- Evidence: Mixing OK: Min pairwise Jaccard overlap = 0.609 \>= 0.10
  threshold

**Overlap Matrix:**

|         | chain_1 | chain_2 | chain_3 | chain_4 |
|:--------|--------:|--------:|--------:|--------:|
| chain_1 |   1.000 |   0.698 |   0.679 |   0.727 |
| chain_2 |   0.698 |   1.000 |   0.609 |   0.682 |
| chain_3 |   0.679 |   0.609 |   1.000 |   0.659 |
| chain_4 |   0.727 |   0.682 |   0.659 |   1.000 |

### Chain-Specific Statistics

**LCC Capacity by Chain:**

| Chain   | Region  | Mean |  SD |  Min |  Max |
|:--------|:--------|-----:|----:|-----:|-----:|
| chain_1 | chain_1 | 2133 | 373 | 1478 | 5080 |
| chain_2 | chain_2 | 2088 | 357 | 1423 | 4465 |
| chain_3 | chain_3 | 1942 | 350 | 1368 | 3113 |
| chain_4 | chain_4 | 1993 | 352 | 1188 | 3260 |

**N Components by Chain:**

| Chain   | Mean |   SD | Min | Max |
|:--------|-----:|-----:|----:|----:|
| chain_1 | 1.76 | 1.00 |   1 |   8 |
| chain_2 | 1.81 | 1.07 |   1 |   7 |
| chain_3 | 2.25 | 1.18 |   1 |   7 |
| chain_4 | 1.98 | 1.06 |   1 |   5 |

**Capacity by Chain:**

| Chain   | Mean |  SD |  Min |   Max |
|:--------|-----:|----:|-----:|------:|
| chain_1 | 2335 | 573 | 2046 | 10105 |
| chain_2 | 2278 | 491 | 2045 |  8902 |
| chain_3 | 2268 | 238 | 2045 |  3208 |
| chain_4 | 2279 | 264 | 2045 |  3701 |

**LCC Transition Rates by Chain:**

| Chain   | Region  | Transitions | Rate |
|:--------|:--------|------------:|:-----|
| chain_1 | chain_1 |         264 | 5.3% |
| chain_2 | chain_2 |         252 | 5.0% |
| chain_3 | chain_3 |         293 | 5.9% |
| chain_4 | chain_4 |         272 | 5.4% |

### Geographic Coverage by Region

| Region | Parcels | Capacity | Mean Visit% | Mean Cap% | Min Visit% | Max Visit% |
|:-------|--------:|---------:|------------:|----------:|-----------:|-----------:|
| all    |    8052 |   141707 |        44.8 |      18.1 |       41.7 |       46.3 |

## State Space Exploration

### Unique LCCs Visited

- Mean unique LCC capacity values per chain: 1000.2
- Mean LCC transitions per chain: 999
- LCC transition rate: 19.98%

### Parcel-Unit Stickiness

| Category          | Count | % of Total |
|-------------------|-------|------------|
| Always in (\>95%) | 0     | 0%         |
| Rarely in (\<5%)  | 6450  | 80.1%      |
| Variable          | 1602  | 19.9%      |

| Inclusion Range | Count |
|:----------------|------:|
| (0.5,0.75\]     |     4 |
| (0.25,0.5\]     |   325 |
| (0.05,0.25\]    |  1273 |
| \[0,0.05\]      |  6450 |

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

    Rarely visited (<5%) parcels: 6,449

      - Unreachable: 444 (6.9%)

      - Feasible but rarely visited: 6,005 (93.1%)

**Stickiness by Feasibility Class:**

| Classification    | Parcels | Never Visited | % Never | Mean Incl% | Max Incl% |
|:------------------|--------:|--------------:|--------:|-----------:|----------:|
| lcc_and_secondary |    5644 |           249 |     4.4 |       5.95 |      51.0 |
| lcc_only          |     621 |           184 |    29.6 |       1.52 |      45.7 |
| secondary_only    |    1343 |           382 |    28.4 |       0.19 |       1.1 |
| unreachable       |     444 |           444 |   100.0 |       0.00 |       0.0 |

- Feasible but rarely visited: 74.6% of all parcels

### Unique States Visited

- Unique parcel states (across all chains): 1,950
- Total thinned samples: 2,000
- Uniqueness ratio: 97.5%

## Kernel-Specific Diagnostics

### Replace-LCC Rejection Breakdown

| Reason                            | Count | % of Attempted |
|:----------------------------------|------:|---------------:|
| attempted                         |  4956 |          100.0 |
| proposed                          |  4956 |          100.0 |
| feasible                          |  4363 |           88.0 |
| mh_rejected                       |  2664 |           53.8 |
| accepted                          |  1699 |           34.3 |
| infeasible                        |   593 |           12.0 |
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
| min_capacity |      593 |           100 |

**Replace-LCC MH Acceptance Analysis:**

    - MH proposals evaluated: 4,363
    - Mean accept prob: 0.392
    - Median accept prob: 0.176
    - Accept prob < 0.1: 1888 (43.3%)
    - Accept prob > 0.9: 1036 (23.7%)


    **Log proposal ratio (log q(x'→x) / q(x→x')):**

    - Mean: -1.72
    - SD: 1.85
    - Range: -5.18 to 4.79


    **Replace-LCC proposal diagnostics:**

| Metric                      |   Mean | Median | P90 |  Max |
|:----------------------------|-------:|-------:|----:|-----:|
| Retained k (secondaries)    |   1.05 |      1 |   3 |    7 |
| Forward candidate LCC count | 629.91 |    607 | 903 | 1702 |

**Replace-LCC Proposal Geometry:**

    - Forward candidate LCCs: mean=629.9, sd=211.8
    - Reverse candidate LCCs: mean=783.3, sd=244.5
    - Asymmetry ratio (fwd/rev): mean=0.84

### Birth/Death Direction Analysis

    **Direction of Accepted Moves:**


    ✓ Balanced: 0.97 births per death

### Legacy Multi-Move Birth/Death (r-value)

*Note: These diagnostics are for legacy multi-move birth/death kernels.
Current architecture uses symmetric_birth_death which proposes single
blocks.*

    Legacy multi-birth kernel not active (using symmetric_birth_death instead).

    Legacy multi-death kernel not active (using symmetric_birth_death instead).

### Swap Kernel Diagnostics

    **Capacity Change for Accepted Swaps:**

    - Total accepted swaps: 650
    - Mean capacity delta: -1.5
    - SD capacity delta: 70.3
    - Range: -150 to 149


    **Proposal Asymmetry (similar block counts):**

    - Forward similar blocks: mean=240.6, sd=49.7
    - Reverse similar blocks: mean=243.1, sd=53.0
    - Ratio (fwd/rev): mean=1.02

    **Swap Rejection Breakdown:**

| Reason          | Count | % of Attempted |
|:----------------|------:|---------------:|
| attempted       |  2049 |          100.0 |
| proposed        |  1116 |           54.5 |
| proposal_failed |   933 |           45.5 |
| no_secondaries  |   933 |           45.5 |
| feasible        |   759 |           37.0 |
| accepted        |   650 |           31.7 |
| infeasible      |   357 |           17.4 |
| mh_rejected     |   109 |            5.3 |


    **Swap Constraint Failures:**

| Constraint           | Failures |
|:---------------------|---------:|
| station_capacity_pct |      268 |
| min_capacity         |       82 |
| station_area_pct     |        6 |
| min_lcc_fraction     |        1 |

## Capacity Prior Diagnostics

The capacity prior penalizes capacity above min_capacity with a linear
penalty: `penalty = λ × (capacity - min_capacity)` where λ = 0.005.

- Penalty source: runner diagnostics
- Steps above min_capacity: 19,858 / 20,000 (99.3%)

<!-- -->

    **Penalty Statistics (when above min_capacity):**

    - Mean penalty: 1.2343
    - Max penalty: 40.3000
    - Mean excess capacity: 247

## Library Utilization

### LCC Library

- Initial LCC library size: 5000
- Final library sizes: 5711, 5752, 5729, 5700
- Online enrichment adds: 2892

### Secondary Library

- Secondary library size: 500
- Lifted Birth/Death acceptance: 18.4%
- Lifted Birth/Death feasibility: 81.5%

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
| Convergence (R-hat)             | 1.088             |
| ESS                             | 153               |
| Chain Mixing                    | 0.609 min overlap |
| LCC Exploration                 | 19.98%            |
| Lifted Birth/Death Acceptance   | 18.4% acceptance  |
| Replace-LCC Feasibility         | 88.0%             |
| Parcel Variability (stickiness) | 80.1% sticky      |
