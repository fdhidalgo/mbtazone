# config.R - Configuration constants and constraints for parcel MCMC
#
# Defines constants, constraints, and constraint names for the parcel
# MCMC zoning analysis pipeline.

# ============================================================================
# CONSTANTS
# ============================================================================

# Secondary component area threshold (acres)
SECONDARY_AREA_THRESHOLD <- 5

# Right-of-way (ROW) parameters for adjacency graph
ROW_PROXIMITY_THRESHOLD <- 180 # feet
ROW_MIN_CROSSING_LENGTH <- 20 # feet

# Convergence diagnostics thresholds
RHAT_CONVERGENCE_THRESHOLD <- 1.1 # R-hat < this indicates convergence

# ============================================================================
# CAPACITY PRIOR CONFIGURATION
# ============================================================================
#
# Exponential prior on capacity above min_capacity:
#   π(capacity) ∝ exp(-λ × (capacity - min_capacity))
#
# IMPORTANT: min_capacity remains a HARD constraint (legal mandate under MBTA).
# The prior applies ABOVE min_capacity to favor configurations closer to minimum.
#
# How to think about λ (lambda):
#   - Each unit of excess capacity reduces prior density by factor exp(-λ)
#   - "Half-life" = ln(2)/λ: capacity where prior density drops to 50%
#   - At λ=0.005: half-life ≈ 139 units
#
# Effect on MH acceptance when proposing +Δ capacity:
#   acceptance *= exp(-λ × Δ)
#
# Calibration table (effect of adding 200 capacity, typical secondary):
#   λ       | Prior ratio | Half-life | Interpretation
#   --------|-------------|-----------|----------------
#   0.001   | 0.82        | 693 units | Very gentle
#   0.002   | 0.67        | 347 units | Gentle
#   0.005   | 0.37        | 139 units | Moderate
#   0.010   | 0.14        |  69 units | Strong
#
CAPACITY_PRIOR_LAMBDA <- 0.005
# Discovery-only capacity bound (multiplier of min_capacity)
# Filters LCCs during tree enumeration to speed up discovery.
# LCCs with capacity > min_capacity * DISCOVERY_CAPACITY_MULTIPLIER are skipped.
# This is NOT a constraint - just efficiency optimization since high-capacity
# LCCs are heavily penalized by the capacity prior anyway.
# Note: Raised from 1.5 to 2.5 after Norwood diagnostic showed 1.5x cap
# excluded 10,627 standalone-feasible LCCs. The sweet spot for station
# constraint satisfaction is capacity 2000-3000 (62-72% feasible), and
# above 3000 still yields ~29% feasible — all invisible at the old cap.
DISCOVERY_CAPACITY_MULTIPLIER <- 2.5

# ============================================================================
# K PRIOR CONFIGURATION (Prior on Number of Secondaries)
# ============================================================================
#
# Geometric prior on number of secondary blocks:
#   π(k) ∝ exp(-λ × k)
#
# This favors fewer, larger secondary components over many small ones.
#
# How to think about λ (lambda):
#   - Each additional block multiplies prior density by exp(-λ)
#   - At λ=2.0: each block multiplies by 0.14 (very steep)
#   - At λ=0.5: each block multiplies by 0.61 (gentle)
#
# Effect on MH acceptance:
#   Birth (k → k+1): acceptance *= exp(-λ)
#   Death (k → k-1): acceptance *= exp(+λ)
#
# Calibration table:
#   λ    | Per-block | P(k=0) | P(k≥1) | E[k] | Interpretation
#   -----|-----------|--------|--------|------|----------------
#   0.25 | 0.78      |  22%   |  78%   | 3.5  | Very gentle
#   0.5  | 0.61      |  39%   |  61%   | 1.5  | Gentle
#   1.0  | 0.37      |  63%   |  37%   | 0.6  | Moderate
#   2.0  | 0.14      |  86%   |  14%   | 0.2  | Strong
#   3.0  | 0.05      |  95%   |   5%   | 0.05 | Very strong
#
# A correction term in the MH ratio accounts for the fact that there are
# far more ways to choose k=5 blocks than k=1 block from the pool.
# Without it, the sampler would drift toward mid-range k values simply
# because there are more possible configurations there, regardless of
# the prior. With the correction, the prior above works as intended.
#
# NOTE: With the reference measure correction, this prior directly controls
# the marginal on k (i.e., the distribution of k in MCMC output, averaging
# over all other variables). λ=0.5 gives E[k] ≈ 1.5 (gentle preference for low k).
# Increase to 1.0-2.0 for stronger preference toward k=0.
#
K_PRIOR_LAMBDA <- 0.25

# MCMC step count (single source of truth for all MCMC runs)
MCMC_STEPS_MACRO <- 5000L

# Burn-in: number of initial samples to discard when computing diagnostics
# Chains start from different regions and need time to reach stationary distribution
# Set to 0 to disable burn-in
MCMC_BURN_IN <- 1000L
