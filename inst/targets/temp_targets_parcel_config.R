# ============================================================================
# DEBUG MODE
# ============================================================================
#
# When TRUE, validates state invariants after every accepted move.
# Catches subtle bugs from state mutations (overlapping blocks, capacity drift).
# Cost: ~100-500μs per accepted move (~1% overhead for 5000-step runs).
# Set to FALSE for production runs after debugging is complete.
DEBUG_INVARIANT_CHECKS <- TRUE

# ============================================================================
# MACRO CONSTRUCTION PARAMETERS
# ============================================================================

# Parcel scale factor
# 0 = raw parcels (no aggregation, each parcel is its own unit)
# 1 = aggregation mode (0.25-1.0 acre units)
# 2 = double size (0.5-2.0 acres), etc.
MACRO_SCALE <- 0

# Base area targets (scaled by MACRO_SCALE)
MACRO_BASE_AREA_MIN <- 0.25
MACRO_BASE_AREA_MAX <- 1.0

# Parcels >= this threshold are preserved as singleton units
# This matches the secondary component minimum area requirement
# Fixed regardless of MACRO_SCALE
MACRO_ATOMIC_THRESHOLD <- 5.0

# Derived thresholds (for backwards compatibility with existing code)
MACRO_TARGET_AREA_MIN <- MACRO_BASE_AREA_MIN * MACRO_SCALE
MACRO_TARGET_AREA_MAX <- MACRO_BASE_AREA_MAX * MACRO_SCALE

# ============================================================================
# LIBRARY CONSTRUCTION PARAMETERS
# ============================================================================
#
# Two libraries are built:
# - Secondary library (L_sec): Connected blocks with area >= 5 acres
# - LCC library (L_LCC): Connected blocks that can serve as the main component
#   of a feasible plan. NOT required to be standalone-feasible; secondaries
#   may supply capacity to reach min_capacity. Minimum viable LCC capacity is
#   min_capacity / 2 (for θ = 0.5), since secondaries can contribute up to half.

# Size bands for secondary library (in acres)
# Each band gets quota_per_band blocks via BFS sampling
SEC_SIZE_BANDS <- list(
  c(5, 8), # Small secondaries

  c(8, 12), # Medium secondaries
  c(12, 20) # Large secondaries
)

# Number of blocks to sample per size band
SEC_QUOTA_PER_BAND <- 50

# Minimum density (units/acre) for library blocks
LIBRARY_DENSITY_THRESHOLD <- 15

# Number of INITIAL LCC candidates to generate (seed library only)
# The main MCMC uses the discovered_lcc_library (~10,000 blocks)
# built by the tree enumeration and BFS discovery phases
LCC_N_CANDIDATES <- 100

# Minimum blocks required per size band (for coverage validation)
LIBRARY_MIN_BLOCKS_PER_BAND <- 10
LCC_MIN_CANDIDATES <- 20

# Note: MCMC_STEPS_MACRO is defined in R/config.R (single source of truth)
# Use MCMC_STEPS_MACRO for all step counts

# ============================================================================
# DISCOVERY CONFIGURATION (Parallel Tree + BFS Architecture)
# ============================================================================
#
# Both LCC and secondary libraries use the same two-stage discovery:
# 1. Tree Enumeration (Wilson's algorithm) - uniform coverage of tree-cuts
# 2. BFS Supplement - exploration of non-tree-cut configurations
#
# LCCs and secondaries sample trees independently (allows different counts).

# --- LCC Discovery ---
# Tree enumeration: Sample spanning trees, enumerate all valid LCC cuts
TREE_LCC_N_TREES <- 500L

# BFS supplement: Replaces MCMC discovery (faster, more direct)
# Seeds from tree LCCs at capacity quantiles, explores boundary perturbations
BFS_LCC_N_SAMPLES <- 100L
BFS_LCC_N_SEEDS <- 10L

# Library size cap
LCC_LIBRARY_MAX_SIZE <- 5000L

# --- Secondary Discovery ---
# Tree enumeration: Sample trees, enumerate cuts within area bands
TREE_SEC_N_TREES <- 200L

# BFS supplement: Fills gaps not covered by tree enumeration
BFS_SEC_QUOTA_PER_BAND <- 25L

# Library size cap
SEC_LIBRARY_MAX_SIZE <- 500L

# BFS reservation in final library (ensures BFS discoveries aren't crowded out)
BFS_RESERVATION_LCC <- 500L
BFS_RESERVATION_SEC <- 100L

# ============================================================================
# CAPACITY-STRATIFIED LCC DISCOVERY
# ============================================================================
#
# Tree discovery is biased toward high-capacity LCCs (78% have capacity > 5000).
# The capacity prior (lambda=0.005) makes the posterior favor capacity 1200-2000.
# Capacity-stratified BFS explicitly targets low-capacity bands to balance the library.
#
# Unlike the existing BFS supplement (which seeds from tree LCCs), this approach
# seeds from random parcels and grows toward explicit capacity targets.

# Capacity bands for stratified BFS discovery (relative to min_capacity)
# Band boundaries are multipliers of min_capacity (2045 for Norwood):
#   Band 1: [0.5, 0.75] * 2045 = [1023, 1534] - Minimum viable LCCs
#   Band 2: [0.75, 1.0] * 2045 = [1534, 2045] - Near-minimum (posterior mode)
#   Band 3: [1.0, 1.5] * 2045 = [2045, 3068] - Small excess
# High-capacity LCCs (>3068) are adequately covered by tree discovery.
LCC_CAPACITY_BANDS_RELATIVE <- list(
  c(0.5, 0.75),   # Need substantial secondaries to hit min_capacity

  c(0.75, 1.0),   # Primary posterior mode under capacity prior
  c(1.0, 1.5)     # Small excess, still favored over very high capacity
)

# Number of LCC samples per capacity band
LCC_BAND_SAMPLES_PER_BAND <- 500L

# Maximum BFS attempts per band before giving up
# Higher than samples_per_band because many attempts fail validation
LCC_BAND_MAX_ATTEMPTS <- 2000L

# ============================================================================
# KERNEL PROBABILITIES
# ============================================================================
#
# Move type selection probabilities for parcel MCMC.
# These should sum to 1.0.
#
# 4-kernel mix (with capacity prior):
#   p_lcc_local:             0.12   - LCC boundary moves
#   p_symmetric_birth_death: 0.305  - Unified birth/death for k-mixing
#   p_swap:                  0.12   - Capacity-balanced swap (geographic mixing)
#   p_replace_lcc:           0.455  - Library-based LCC relocation (mode mixing)
#
# Note: symmetric_birth_death uses a unified proposal that picks direction
#       uniformly, avoiding the old p_death/p_birth asymmetry correction.
# Note: Tree-based LCC discovery now populates the library (see spanning_tree.R),
#       so MCMC only uses library-based replace_lcc for global LCC moves.

# ============================================================================
# REPLACE-LCC RETENTION PARAMETERS
# ============================================================================
#
# Replace-LCC uses retention + capacity-matched sampling:
# 1. Sample new LCC from capacity-similar set (within ±tolerance of current)
# 2. Keep ALL current secondaries (no resampling)
# 3. Check all secondaries are compatible with new LCC
# 4. MH ratio corrects for asymmetric proposal via set size ratio
#
# This approach solves the k-mismatch problem: chain has k≈5 from birth/death,
# but resampling proposals generate k≈1-2, causing MH rejection.
# By retaining secondaries, k stays constant and MH acceptance improves.

# Capacity tolerance for Replace-LCC similar-capacity sampling
# LCCs within ±CAP_TOLERANCE of current LCC capacity are candidates
# Larger values = more candidates but higher capacity mismatch
# Smaller values = better capacity match but fewer candidates
# Note: Wider tolerance (800) allows replace_lcc to propose larger capacity jumps,
# enabling chains to explore different capacity regions.
# Tradeoff: more candidates to filter, but enables n_secondaries mixing.
REPLACE_LCC_CAP_TOLERANCE <- 800

# Capacity tolerance for secondary swap similar-capacity sampling
# Blocks within ±SWAP_CAP_TOLERANCE of removed block are candidates for swap
# Larger values = more geographic diversity, lower acceptance rate
# Smaller values = more capacity-neutral swaps, higher acceptance rate
# Note: Increased from 50 to 150 to reduce acceptance rate (~90% was too local)
# and encourage larger geographic moves.
SWAP_CAP_TOLERANCE <- 150

# ============================================================================
# MULTI-BIRTH / MULTI-DEATH MOVES
# ============================================================================
#
# Multi-move kernels can add/remove 1-3 secondary blocks per step.
# This enables "tunneling" between states with different numbers of
# secondary components (k), addressing the observation that k=0 states
# are feasible but rarely visited with single-block moves.

# Probability distribution for number of blocks to add/remove (r)
# P(r=1) = 0.90, P(r=2) = 0.08, P(r=3) = 0.02
# Strongly favor single-block moves; r>1 has near-zero MH acceptance
# due to combinatorial reverse proposal probability.
MULTI_MOVE_PROBS <- c(0.90, 0.08, 0.02)

# Maximum blocks to add/remove in a single move (derived from probs length)
MULTI_MOVE_MAX_R <- length(MULTI_MOVE_PROBS)


# ============================================================================
# ONLINE LCC LIBRARY ENRICHMENT
# ============================================================================
#
# Online enrichment adds visited LCCs to the library during main MCMC,
# enabling Replace-LCC moves to propose jumps FROM recently-visited states.

# Add current LCC to library every N steps
# Lower values = better coverage but more overhead
ONLINE_ENRICHMENT_INTERVAL <- 10L

# Maximum online entries before FIFO eviction (prevents unbounded growth)
ONLINE_MAX_ENTRIES <- 5000L

# Enable online enrichment for main runs
ENABLE_ONLINE_ENRICHMENT <- TRUE

# Freeze library after this many steps for strict stationarity.
# During burn-in: enrichment explores new LCCs, improving library coverage.
# After burn-in: frozen library → stationary kernel → valid MCMC theory.
# NULL means enrich continuously (time-inhomogeneous, theoretically unsound).
# Note: MCMC_BURN_IN is defined in R/config.R
ENRICHMENT_BURN_IN <- MCMC_BURN_IN

# ============================================================================
# SAMPLE STORAGE PARAMETERS
# ============================================================================
#
# To reduce memory usage, we:
# 1. Store LCC signatures (compact strings) at every step for discovery deduplication
# 2. Store thinned minimal states for visualization/metrics
# 3. Eliminate parcel-level samples (reconstruct on-demand from parcel_assignments)

# Target maximum number of stored states (uses adaptive thinning)
# thin_interval = max(1L, n_steps %/% SAMPLE_MAX_STORED)
# This ensures ~500 states regardless of run length
SAMPLE_MAX_STORED <- 500L

# Store LCC signatures for discovery deduplication
# TRUE = store signature at every step (needed for discovery)
# FALSE = skip signatures (saves memory if not needed)
STORE_LCC_SIGNATURES <- TRUE


# ============================================================================
# PARCEL MULTI-CHAIN IRREDUCIBILITY ANALYSIS
# ============================================================================

# Default number of chains for parcel multi-chain analysis
DEFAULT_N_CHAINS <- 4L

# Note: Use MCMC_STEPS_MACRO from R/config.R for step counts

# Minimum Jaccard overlap for chains to be considered "mixing"
# Below this threshold indicates potential irreducibility
CHAIN_OVERLAP_THRESHOLD <- 0.1
