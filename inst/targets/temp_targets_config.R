# config.R - Configuration constants and constraints for parcel MCMC
#
# Defines constants, constraints, and constraint names for the parcel
# MCMC zoning analysis pipeline.

# ============================================================================
# CONSTANTS
# ============================================================================

# Secondary component area threshold (acres)
SECONDARY_AREA_THRESHOLD <- 5

# System resource allocation
DEFAULT_CREW_WORKERS <- max(
  1L,
  10L
)

# Right-of-way (ROW) parameters for adjacency graph
ROW_PROXIMITY_THRESHOLD <- 180 # feet
ROW_MIN_CROSSING_LENGTH <- 20 # feet

# Convergence diagnostics thresholds
RHAT_CONVERGENCE_THRESHOLD <- 1.1 # R-hat < this indicates convergence
CHAIN_OVERLAP_THRESHOLD <- 0.1 # Jaccard < this suggests separation
MIN_REGION_CAPACITY_FRACTION <- 0.5 # Fraction of min_capacity needed in region

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
DISCOVERY_CAPACITY_MULTIPLIER <- 3

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
#   Death (k → k-1): acceptance *= min(1, exp(+λ))
#
# Calibration table:
#   λ    | Per-block | P(k=1)/P(k=0) | P(k=2)/P(k=0) | Interpretation
#   -----|-----------|---------------|---------------|----------------
#   0.5  | 0.61      | 61%           | 37%           | Gentle
#   1.0  | 0.37      | 37%           | 14%           | Moderate
#   2.0  | 0.14      | 14%           |  2%           | Strong
#   3.0  | 0.05      |  5%           | 0.2%          | Very strong
#
# NOTE: λ=2.0 strongly favors k=0 (LCC only). Reduce to 0.5-1.0 if you want
# the posterior to include more secondary blocks.
#
K_PRIOR_LAMBDA <- 2.0

# MCMC step count (single source of truth for all MCMC runs)
MCMC_STEPS_MACRO <- 5000L

# Burn-in: number of initial samples to discard when computing diagnostics
# Chains start from different regions and need time to reach stationary distribution
# Set to 0 to disable burn-in
MCMC_BURN_IN <- 1000L

# Constraint names for rejection tracking
CONSTRAINT_NAMES <- c(
  "empty_plan",
  "capacity_low",
  "secondary_area",
  "total_area",
  "density",
  "lcc_fraction",
  "mh_reject",
  "station_area_pct",
  "station_capacity_pct",
  "unknown"
)
