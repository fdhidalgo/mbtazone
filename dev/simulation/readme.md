grid_validation.R — What it does

The script answers two questions about the mbtazone MCMC parcel optimizer:

1. Sampler correctness: Given the block libraries, does the MCMC sample plans in proportion to
the target distribution?
2. Library completeness: Does the discovery process find all the blocks that actually exist on
the graph?

It does this on a toy 4x4 grid where both questions are answerable exactly — something impossible
on real municipalities with thousands of parcels.

Section 1: Graph

Builds a 4×4 grid graph (16 nodes, 24 edges). Each node is a "parcel" with capacity and area
attributes. With random = FALSE, all parcels are identical (capacity=1, area=6). With random =
TRUE (current setting), capacity is drawn from 1–5 and area from 3–10, making the weight
landscape more interesting since plans now have heterogeneous total capacities.

Section 2: Constraints

Sets the zoning feasibility constraints matching the MCMC: min_capacity=4, min_area=10,
min_density=0.1, min_lcc_fraction=0.5. No station constraints. These are deliberately loose so
the toy graph has many feasible plans.

Section 3: Library Building

Runs the same two-stage discovery pipeline (spanning tree enumeration + BFS supplement) that the
real MCMC uses, producing an LCC library and secondary library. The density threshold is set to 0
because the toy parcels have much lower density than real parcels.

Section 4: State Enumeration

This has two parts:

4a — Library completeness (the new section): Brute-force enumerates all 2^16 − 1 = 65,535 subsets
of the grid, checks connectivity, and classifies each connected subgraph as a potential LCC
(capacity ≥ 2) or secondary (area ≥ 5 acres). Then compares against the libraries to report:
- Block-level recall: What fraction of all true LCCs/secondaries did discovery find?
- Plan-level coverage: How many feasible plans can be assembled from the true blocks vs from the
library blocks?

4b — Library-restricted enumeration: Iterates over all (LCC, {secondary subset}) tuples using
only library blocks. For each LCC, finds compatible secondaries (not overlapping, not adjacent to
the LCC or each other), checks feasibility, and records the state. Computes the unnormalized
target weight:

log w = −λ_cap × excess − λ_k × k − log C(n_pool, k)

Multiple states can produce the same plan (same union of parcels, different decomposition into
LCC + secondaries). The plan-level probability sums weights over all its decompositions.

Section 5: MCMC Execution

Runs 4 chains × 20,000 steps using run_parcel_mcmc() with the LCC-local kernel disabled.
Disabling LCC-local is critical: that kernel modifies the LCC at the parcel level, creating
configurations not in the library and therefore not in our enumeration. With it off, the MCMC
only proposes library entries, and the enumerated state space is exact.

Section 6: Comparison

Extracts plan keys and capacities from the stored MCMC samples, pools across chains, and
computes:

- Total variation distance — the total amount of probability mass "in the wrong place"
- Chi-squared test — goodness-of-fit for plans with expected count ≥ 5 (anti-conservative due to
MCMC autocorrelation)
- Acceptance rates and Gelman-Rubin R-hat — standard MCMC diagnostics
- KS test on plan capacity — tests whether the marginal capacity distribution matches
- Effective sample size — computed from the autocorrelation of the capacity trace, quantifies the
cost of MCMC correlation
- Coverage fraction — what % of enumerated plans the MCMC actually visited

Section 7: Visualization

Five plots:

1. Capacity density overlay — the canonical redistricting validation figure. Overlays expected vs
observed distribution of total plan capacity. Convergence on 1D observables is orders of
magnitude faster than on the full distribution.
2. TV convergence curve — TV distance as a function of sample count, with the distribution-aware
iid noise floor. If the black line tracks the blue dashed line's 1/√N shape (just slightly above
it due to autocorrelation), the sampler is correct. A plateau would indicate bias.
3. Standardized residual histogram — (observed − expected) / √expected for each plan, overlaid
with N(0,1). The shape (symmetry, no fat tails) is diagnostic; the SD will exceed 1 due to
autocorrelation.
4. Observed/expected ratio by plan rank — each plan's O/E ratio with a 95% CI band. Points should
scatter around 1.0 with no trend.
5. Log-log scatter with confidence bands — the "money plot." Each point is a plan on log scale,
with binomial 95% CI. Reports what percentage of plans fall within the band.