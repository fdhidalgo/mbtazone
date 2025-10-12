#' Particle State Tracking for SMC Algorithm
#'
#' Functions to manage particle state during Sequential Monte Carlo (SMC)
#' simulation of MBTA overlay zoning districts. Implements 3-tier checking
#' architecture for efficient propagation: Tier 1 (running sums), Tier 2
#' (infeasibility tests), and Tier 3 (completion checks).
#'
#' @name smc_particle
NULL

#' Create New Particle State
#'
#' Initialize a particle from a seed parcel for SMC simulation. Sets up all
#' running sums (Tier 1) and infeasibility tracking (Tier 2) based on the
#' seed parcel's connected component.
#'
#' @param seed_parcel_id Integer index of seed parcel (row in map$data)
#' @param map An mbta_map object containing parcel data and requirements
#' @param seed_prob Probability of selecting this seed parcel (for biased
#'   seed selection strategies). Default: 1.0 (uniform random selection)
#'
#' @return A ParticleState S3 object (list) with components:
#'   \item{parcel_ids}{Integer vector of parcel indices in zone}
#'   \item{n_parcels}{Number of parcels in zone}
#'   \item{component_id}{Connected component ID}
#'   \item{total_capacity}{Total unit capacity in zone}
#'   \item{total_area}{Total area in zone (acres)}
#'   \item{station_capacity}{Capacity within station areas}
#'   \item{station_area}{Area within station areas (acres)}
#'   \item{remaining_capacity}{Capacity left in component}
#'   \item{remaining_area}{Area left in component (acres)}
#'   \item{remaining_station_capacity}{Station capacity left in component}
#'   \item{remaining_station_area}{Station area left in component (acres)}
#'   \item{log_proposal_path}{Cumulative log probability of proposal path Q(τ)}
#'   \item{valuation_score}{V(x) for resampling guidance}
#'   \item{active}{Boolean indicating if particle is alive}
#'   \item{n_steps}{Number of propagation steps taken}
#'
#' @details
#' ## Initialization Logic
#'
#' The particle is initialized with the seed parcel and all running sums are
#' set based on:
#' 1. Seed parcel attributes (capacity, area, station overlap)
#' 2. Connected component totals (for infeasibility tracking)
#' 3. Seed selection probability (for importance sampling)
#'
#' ## Running Sums (Tier 1)
#'
#' Two sets of running sums are maintained:
#' - **Totals**: Cumulative sums of parcels in zone (increment each step)
#' - **Remaining**: What's left in connected component (decrement each step)
#'
#' This enables O(1) infeasibility checks without recomputing reachable sets.
#'
#' ## Component-Based Tracking
#'
#' Particles can only add parcels from their initial connected component
#' (rook adjacency). The `remaining_*` fields track the maximum capacity/area
#' achievable by adding all remaining parcels in the component.
#'
#' @examples
#' \dontrun{
#' # Create mbta_map
#' parcels <- load_municipality("inst/extdata/parcels/57_CHELSEA_basic.zip")
#' chelsea_map <- mbta_map(parcels, community_type = "rapid_transit")
#'
#' # Initialize particle from random seed in station area
#' station_parcels <- which(parcels$in_station_area == 1)
#' seed_id <- sample(station_parcels, 1)
#' particle <- new_particle_state(seed_id, chelsea_map)
#'
#' # Examine initial state
#' print(particle$total_capacity)
#' print(particle$remaining_capacity)
#' }
#'
#' @seealso \code{\link{add_parcel_to_particle}}, \code{\link{check_infeasibility}}
#'
#' @export
new_particle_state <- function(seed_parcel_id, map, seed_prob = 1.0) {

  # ========== Input Validation ==========

  if (!inherits(map, "mbta_map")) {
    cli::cli_abort("{.arg map} must be an {.cls mbta_map} object")
  }

  n_parcels <- nrow(map$data)

  if (!is.numeric(seed_parcel_id) || seed_parcel_id < 1 || seed_parcel_id > n_parcels) {
    cli::cli_abort(c(
      "{.arg seed_parcel_id} must be between 1 and {n_parcels}",
      "x" = "Received: {seed_parcel_id}"
    ))
  }

  seed_parcel_id <- as.integer(seed_parcel_id)

  if (!is.numeric(seed_prob) || seed_prob <= 0 || seed_prob > 1) {
    cli::cli_abort("{.arg seed_prob} must be between 0 and 1")
  }

  # ========== Extract Seed Parcel Attributes ==========

  seed_parcel <- map$data[seed_parcel_id, ]

  # Get capacity (with NA handling)
  capacity_col <- map$capacity_col
  seed_capacity <- seed_parcel[[capacity_col]]
  if (is.na(seed_capacity)) {
    seed_capacity <- 0
  }

  # Get area in acres (SQFT column standard in parcel data)
  if (!"SQFT" %in% names(seed_parcel)) {
    cli::cli_abort("Parcel data must contain {.field SQFT} column")
  }
  seed_area_sqft <- seed_parcel$SQFT
  if (is.na(seed_area_sqft)) {
    seed_area_sqft <- 0
  }
  seed_area_acres <- seed_area_sqft / 43560

  # Get station area overlap
  # Check for precomputed station attributes
  if (map$precomputed && "station_area_sf" %in% names(seed_parcel)) {
    # Precomputed: use cached station area
    seed_station_area_sqft <- seed_parcel$station_area_sf
    if (is.na(seed_station_area_sqft)) seed_station_area_sqft <- 0
    seed_station_area_acres <- seed_station_area_sqft / 43560

    # Station capacity proportional to station area fraction
    station_fraction <- if (seed_area_sqft > 0) {
      seed_station_area_sqft / seed_area_sqft
    } else {
      0
    }
    seed_station_capacity <- seed_capacity * station_fraction

  } else if ("in_station_area" %in% names(seed_parcel)) {
    # Binary indicator: parcel fully in/out of station area
    in_station <- seed_parcel$in_station_area
    if (is.na(in_station)) in_station <- 0

    seed_station_capacity <- seed_capacity * in_station
    seed_station_area_acres <- seed_area_acres * in_station

  } else {
    # No station area information (e.g., adjacent communities)
    seed_station_capacity <- 0
    seed_station_area_acres <- 0
  }

  # ========== Identify Connected Component ==========

  component_id <- map$component_info$component_id[seed_parcel_id]

  # Get total capacity/area in this component
  comp_total_capacity <- map$component_info$total_capacity[component_id]
  comp_total_area <- map$component_info$total_area[component_id]

  # Calculate component station totals (sum over all parcels in component)
  comp_parcel_ids <- which(map$component_info$component_id == component_id)

  if (map$precomputed && "station_area_sf" %in% names(map$data)) {
    # Precomputed station areas
    comp_station_area_sqft <- sum(map$data$station_area_sf[comp_parcel_ids], na.rm = TRUE)
    comp_station_area_acres <- comp_station_area_sqft / 43560

    # Station capacity: capacity × (station_area / total_area) for each parcel
    comp_station_capacity <- sum(
      map$data[[capacity_col]][comp_parcel_ids] *
      (map$data$station_area_sf[comp_parcel_ids] / map$data$SQFT[comp_parcel_ids]),
      na.rm = TRUE
    )

  } else if ("in_station_area" %in% names(map$data)) {
    # Binary indicator
    station_mask <- map$data$in_station_area[comp_parcel_ids]
    station_mask[is.na(station_mask)] <- 0

    comp_station_capacity <- sum(
      map$data[[capacity_col]][comp_parcel_ids] * station_mask,
      na.rm = TRUE
    )
    comp_station_area_acres <- sum(
      (map$data$SQFT[comp_parcel_ids] / 43560) * station_mask,
      na.rm = TRUE
    )

  } else {
    # No station area information
    comp_station_capacity <- 0
    comp_station_area_acres <- 0
  }

  # ========== Initialize Particle State ==========

  particle <- list(
    # Zone composition
    parcel_ids = seed_parcel_id,
    n_parcels = 1L,
    component_id = component_id,

    # Tier 1: Running sums (what's in the zone)
    total_capacity = seed_capacity,
    total_area = seed_area_acres,
    station_capacity = seed_station_capacity,
    station_area = seed_station_area_acres,

    # Tier 2: Remaining in component (what's reachable)
    remaining_capacity = comp_total_capacity - seed_capacity,
    remaining_area = comp_total_area - seed_area_acres,
    remaining_station_capacity = comp_station_capacity - seed_station_capacity,
    remaining_station_area = comp_station_area_acres - seed_station_area_acres,

    # RSIS tracking (for importance sampling)
    log_proposal_path = log(seed_prob),
    valuation_score = NA_real_,  # Will be calculated after initialization
    active = TRUE,

    # Status
    n_steps = 0L
  )

  # Set S3 class
  class(particle) <- c("ParticleState", "list")

  # Calculate initial valuation
  particle$valuation_score <- calculate_valuation(particle, map)

  return(particle)
}

#' Add Parcel to Particle (Tier 1 Update)
#'
#' Add a parcel to the particle's zone and update all running sums. This
#' performs the Tier 1 update operation: O(1) arithmetic to increment totals
#' and decrement remaining capacity/area in the connected component.
#'
#' @param particle A ParticleState object
#' @param parcel_id Integer index of parcel to add (row in map$data)
#' @param map An mbta_map object
#' @param proposal_prob Probability of proposing this parcel addition
#'   (normalized proposal weight for importance sampling)
#'
#' @return Updated ParticleState object with new parcel added and running
#'   sums updated
#'
#' @details
#' ## Tier 1 Operations (O(1) each)
#'
#' This function performs the following updates:
#' 1. Add parcel to zone: `parcel_ids <- c(parcel_ids, parcel_id)`
#' 2. Increment totals: `total_capacity += capacity_parcel`
#' 3. Decrement remaining: `remaining_capacity -= capacity_parcel`
#' 4. Update proposal path: `log_proposal_path += log(proposal_prob)`
#' 5. Increment step counter: `n_steps += 1`
#'
#' All operations are simple arithmetic, ensuring O(1) cost per propagation step.
#'
#' ## Station Area Handling
#'
#' If the map has precomputed station attributes (`station_area_sf` column),
#' uses area-weighted station capacity. Otherwise uses binary indicator
#' (`in_station_area` column).
#'
#' ## NA Handling
#'
#' Missing capacity or area values are treated as 0 for aggregation. This is
#' conservative for infeasibility checking (won't falsely kill particles).
#'
#' @examples
#' \dontrun{
#' # Initialize particle
#' particle <- new_particle_state(seed_id, chelsea_map)
#'
#' # Get boundary parcels
#' boundary <- get_boundary_parcels(particle, chelsea_map)
#'
#' # Add a boundary parcel
#' next_parcel <- sample(boundary, 1)
#' proposal_prob <- 1 / length(boundary)  # Uniform proposal
#' particle <- add_parcel_to_particle(particle, next_parcel, chelsea_map, proposal_prob)
#'
#' # Check updated state
#' print(particle$n_parcels)  # Should be 2
#' print(particle$total_capacity)  # Incremented
#' }
#'
#' @seealso \code{\link{new_particle_state}}, \code{\link{check_infeasibility}}
#'
#' @export
add_parcel_to_particle <- function(particle, parcel_id, map, proposal_prob) {

  # ========== Input Validation ==========

  if (!inherits(particle, "ParticleState")) {
    cli::cli_abort("{.arg particle} must be a {.cls ParticleState} object")
  }

  if (!inherits(map, "mbta_map")) {
    cli::cli_abort("{.arg map} must be an {.cls mbta_map} object")
  }

  n_parcels <- nrow(map$data)
  if (!is.numeric(parcel_id) || parcel_id < 1 || parcel_id > n_parcels) {
    cli::cli_abort("{.arg parcel_id} must be between 1 and {n_parcels}")
  }
  parcel_id <- as.integer(parcel_id)

  if (parcel_id %in% particle$parcel_ids) {
    cli::cli_abort("Parcel {parcel_id} already in zone")
  }

  if (!is.numeric(proposal_prob) || proposal_prob <= 0 || proposal_prob > 1) {
    cli::cli_abort("{.arg proposal_prob} must be between 0 and 1")
  }

  # ========== Extract Parcel Attributes ==========

  parcel <- map$data[parcel_id, ]

  # Capacity
  capacity_col <- map$capacity_col
  parcel_capacity <- parcel[[capacity_col]]
  if (is.na(parcel_capacity)) parcel_capacity <- 0

  # Area (acres)
  parcel_area_sqft <- parcel$SQFT
  if (is.na(parcel_area_sqft)) parcel_area_sqft <- 0
  parcel_area_acres <- parcel_area_sqft / 43560

  # Station area overlap
  if (map$precomputed && "station_area_sf" %in% names(parcel)) {
    # Precomputed station area
    parcel_station_area_sqft <- parcel$station_area_sf
    if (is.na(parcel_station_area_sqft)) parcel_station_area_sqft <- 0
    parcel_station_area_acres <- parcel_station_area_sqft / 43560

    # Station capacity proportional to station area fraction
    station_fraction <- if (parcel_area_sqft > 0) {
      parcel_station_area_sqft / parcel_area_sqft
    } else {
      0
    }
    parcel_station_capacity <- parcel_capacity * station_fraction

  } else if ("in_station_area" %in% names(parcel)) {
    # Binary indicator
    in_station <- parcel$in_station_area
    if (is.na(in_station)) in_station <- 0

    parcel_station_capacity <- parcel_capacity * in_station
    parcel_station_area_acres <- parcel_area_acres * in_station

  } else {
    # No station area information
    parcel_station_capacity <- 0
    parcel_station_area_acres <- 0
  }

  # ========== Tier 1: Update Running Sums (O(1)) ==========

  # Add parcel to zone
  particle$parcel_ids <- c(particle$parcel_ids, parcel_id)
  particle$n_parcels <- particle$n_parcels + 1L

  # Increment totals (what's in the zone)
  particle$total_capacity <- particle$total_capacity + parcel_capacity
  particle$total_area <- particle$total_area + parcel_area_acres
  particle$station_capacity <- particle$station_capacity + parcel_station_capacity
  particle$station_area <- particle$station_area + parcel_station_area_acres

  # Decrement remaining (what's left in component)
  particle$remaining_capacity <- particle$remaining_capacity - parcel_capacity
  particle$remaining_area <- particle$remaining_area - parcel_area_acres
  particle$remaining_station_capacity <- particle$remaining_station_capacity - parcel_station_capacity
  particle$remaining_station_area <- particle$remaining_station_area - parcel_station_area_acres

  # Update RSIS tracking
  particle$log_proposal_path <- particle$log_proposal_path + log(proposal_prob)
  particle$n_steps <- particle$n_steps + 1L

  return(particle)
}

#' Check Particle Infeasibility (Tier 2)
#'
#' Determine if a particle can possibly reach the minimum requirements, even
#' if it adds all remaining parcels in its connected component. This is the
#' Tier 2 check: O(1) arithmetic to kill impossible particles early.
#'
#' @param particle A ParticleState object
#' @param map An mbta_map object containing minimum requirements
#' @param check_density Logical indicating whether to check gross density
#'   infeasibility (default: TRUE). This is an optimization that can be
#'   disabled if too aggressive.
#'
#' @return A list with components:
#'   \item{infeasible}{Logical: TRUE if particle cannot possibly succeed}
#'   \item{reason}{Character: reason for infeasibility ("capacity", "area",
#'     "station_coverage", "gross_density"), or NA if feasible}
#'
#' @details
#' ## Infeasibility Tests (All O(1))
#'
#' A particle is infeasible if any of the following hold:
#'
#' **1. Capacity Infeasibility:**
#' ```
#' max_capacity = total_capacity + remaining_capacity
#' if (max_capacity < target_capacity) → infeasible
#' ```
#'
#' **2. Area Infeasibility:**
#' ```
#' max_area = total_area + remaining_area
#' if (max_area < target_area) → infeasible
#' ```
#'
#' **3. Station Coverage Infeasibility** (if station requirement > 0):
#' ```
#' max_station_capacity = station_capacity + remaining_station_capacity
#' max_station_pct = max_station_capacity / max_capacity
#' if (max_station_pct < required_pct) → infeasible
#' ```
#'
#' **4. Gross Density Infeasibility** (optional optimization):
#' ```
#' min_density = max_capacity / max_area
#' if (min_density < 15.0 units/acre) → infeasible
#' ```
#'
#' ## Conservative Approach
#'
#' These checks assume the particle adds **all** remaining parcels in its
#' connected component. This is conservative: won't falsely kill particles
#' that could succeed, but allows early termination of truly impossible ones.
#'
#' ## Performance Impact
#'
#' Per PRD Section 4.3.6, Tier 2 checks prevent wasted Tier 3 compliance
#' calls. In typical runs:
#' - ~20% of particles killed by Tier 2
#' - Saves ~10% of total runtime
#' - Each check: ~0.001 seconds (pure arithmetic)
#'
#' @examples
#' \dontrun{
#' # Propagate particle
#' for (i in 1:50) {
#'   # Add parcel
#'   particle <- add_parcel_to_particle(particle, next_id, map, prob)
#'
#'   # Check infeasibility
#'   feasibility <- check_infeasibility(particle, map)
#'   if (feasibility$infeasible) {
#'     cat("Particle died:", feasibility$reason, "\n")
#'     break
#'   }
#' }
#' }
#'
#' @seealso \code{\link{add_parcel_to_particle}}, \code{\link{check_completion}}
#'
#' @export
check_infeasibility <- function(particle, map, check_density = TRUE) {

  # ========== Input Validation ==========

  if (!inherits(particle, "ParticleState")) {
    cli::cli_abort("{.arg particle} must be a {.cls ParticleState} object")
  }

  if (!inherits(map, "mbta_map")) {
    cli::cli_abort("{.arg map} must be an {.cls mbta_map} object")
  }

  # ========== Calculate Reachable Maxima ==========

  # Maximum capacity/area if we add ALL remaining parcels
  max_capacity <- particle$total_capacity + particle$remaining_capacity
  max_area <- particle$total_area + particle$remaining_area

  # ========== Test 1: Capacity Infeasibility ==========

  if (max_capacity < map$target_capacity) {
    return(list(
      infeasible = TRUE,
      reason = "capacity"
    ))
  }

  # ========== Test 2: Area Infeasibility ==========

  # Note: target_area can be NA for adjacent_small_town communities
  if (!is.na(map$target_area) && max_area < map$target_area) {
    return(list(
      infeasible = TRUE,
      reason = "area"
    ))
  }

  # ========== Test 3: Station Coverage Infeasibility ==========

  # Only check if station coverage is required (e.g., rapid_transit, commuter_rail)
  if (map$station_area_unit_pct > 0) {

    # Maximum station capacity achievable
    max_station_capacity <- particle$station_capacity + particle$remaining_station_capacity
    max_station_area <- particle$station_area + particle$remaining_station_area

    # Best possible station coverage (using both capacity and area metrics)
    # Per PRD Section 3.3: BOTH metrics must meet threshold

    # Unit capacity coverage
    max_unit_pct <- if (max_capacity > 0) {
      max_station_capacity / max_capacity
    } else {
      0
    }

    # Land area coverage
    max_area_pct <- if (max_area > 0) {
      max_station_area / max_area
    } else {
      0
    }

    # Check if either metric fails (both must pass)
    if (max_unit_pct < map$station_area_unit_pct || max_area_pct < map$station_area_land_pct) {
      return(list(
        infeasible = TRUE,
        reason = "station_coverage"
      ))
    }
  }

  # ========== Test 4: Gross Density Infeasibility (Optional) ==========

  # Per PRD Section 3.3: gross density must be >= 15.0 units/acre
  # This is an optimization to kill low-density particles early
  if (check_density && max_area > 0) {
    min_density <- max_capacity / max_area

    if (min_density < 15.0) {
      return(list(
        infeasible = TRUE,
        reason = "gross_density"
      ))
    }
  }

  # ========== All Tests Passed ==========

  return(list(
    infeasible = FALSE,
    reason = NA_character_
  ))
}

#' Check Particle Completion (Tier 3 Readiness)
#'
#' Determine if a particle has met the minimum capacity and area requirements
#' and is ready for full compliance evaluation (Tier 3). This does NOT check
#' other hard constraints (contiguity, density, station coverage) - those are
#' evaluated by `evaluate_compliance()` during Tier 3.
#'
#' @param particle A ParticleState object
#' @param map An mbta_map object containing minimum requirements
#'
#' @return A list with components:
#'   \item{complete}{Logical: TRUE if minimums are met}
#'   \item{capacity_met}{Logical: capacity >= target_capacity}
#'   \item{area_met}{Logical: area >= target_area (or target_area is NA)}
#'
#' @details
#' ## Completion Criteria
#'
#' A particle is "complete" and ready for Tier 3 when:
#' 1. `total_capacity >= target_capacity`
#' 2. `total_area >= target_area` (or `target_area` is NA)
#'
#' ## Not Checked Here
#'
#' The following constraints are NOT checked in Tier 3 readiness:
#' - Contiguity (50% in single component)
#' - Gross density (>= 15 units/acre)
#' - Station area coverage (50-90% in station buffers)
#'
#' These are evaluated by `evaluate_compliance()` after the particle completes.
#'
#' ## Stochastic Continuation
#'
#' Per PRD Section 4.3.2, when a particle completes:
#' - With probability `(1 - p_continue)`: Stop and run Tier 3 compliance
#' - With probability `p_continue`: Continue growing
#'
#' This prevents "first-hit bias" where all plans cluster at minimums.
#'
#' @examples
#' \dontrun{
#' # Propagation loop
#' while (TRUE) {
#'   # Add parcel
#'   particle <- add_parcel_to_particle(particle, next_id, map, prob)
#'
#'   # Check if minimums met
#'   completion <- check_completion(particle, map)
#'   if (completion$complete) {
#'     # Stochastic continuation
#'     if (runif(1) < 0.8) {
#'       # Stop and evaluate
#'       compliance <- evaluate_compliance(map$data[particle$parcel_ids, ], ...)
#'       break
#'     } else {
#'       # Continue growing (update log_proposal_path with continuation prob)
#'       particle$log_proposal_path <- particle$log_proposal_path + log(0.8)
#'     }
#'   }
#' }
#' }
#'
#' @seealso \code{\link{check_infeasibility}}, \code{\link{evaluate_compliance}}
#'
#' @export
check_completion <- function(particle, map) {

  # ========== Input Validation ==========

  if (!inherits(particle, "ParticleState")) {
    cli::cli_abort("{.arg particle} must be a {.cls ParticleState} object")
  }

  if (!inherits(map, "mbta_map")) {
    cli::cli_abort("{.arg map} must be an {.cls mbta_map} object")
  }

  # ========== Check Minimums ==========

  # Capacity requirement
  capacity_met <- particle$total_capacity >= map$target_capacity

  # Area requirement (handle NA for small towns)
  if (is.na(map$target_area)) {
    area_met <- TRUE  # No area requirement
  } else {
    area_met <- particle$total_area >= map$target_area
  }

  # Both must be met for completion
  complete <- capacity_met && area_met

  return(list(
    complete = complete,
    capacity_met = capacity_met,
    area_met = area_met
  ))
}

#' Calculate Valuation Function V(x)
#'
#' Calculate the valuation score for a particle, used to guide intermediate
#' resampling. The valuation function estimates the potential of a partial
#' plan to reach a compliant final state.
#'
#' @param particle A ParticleState object
#' @param map An mbta_map object
#' @param alpha_V Exponent for capacity ratio in valuation (default: 1.0)
#' @param beta_V Exponent for station coverage ratio in valuation (default: 1.0)
#'
#' @return Numeric valuation score V(x) >= 0
#'
#' @details
#' ## Valuation Function Formula
#'
#' Per PRD Section 4.3.4:
#' ```
#' V(x) = (Reachable_Capacity / Target_Capacity)^α_V ×
#'        (Reachable_Station_Coverage / Target_Coverage)^β_V
#' ```
#'
#' Where:
#' - `Reachable_Capacity = total_capacity + remaining_capacity`
#' - `Reachable_Station_Coverage = max_station_area / max_area`
#'
#' ## Behavior
#'
#' - Higher V(x) = more potential to reach compliance
#' - Particles with low V(x) are downweighted during resampling
#' - Particles expanding into low-capacity or low-station areas get lower V(x)
#'
#' ## Station Coverage Component
#'
#' Only included if `map$station_area_land_pct > 0` (e.g., rapid transit,
#' commuter rail). Adjacent communities have no station requirement, so V(x)
#' only considers capacity.
#'
#' ## Tuning Parameters
#'
#' - `alpha_V = 1.0`: Linear weighting of capacity potential
#' - `beta_V = 1.0`: Linear weighting of station coverage potential
#' - Higher values increase influence of that factor
#'
#' @examples
#' \dontrun{
#' # Calculate valuation after adding parcel
#' particle <- add_parcel_to_particle(particle, next_id, map, prob)
#' valuation <- calculate_valuation(particle, map)
#' print(valuation)
#' }
#'
#' @keywords internal
calculate_valuation <- function(particle, map, alpha_V = 1.0, beta_V = 1.0) {

  # Calculate reachable capacity
  reachable_capacity <- particle$total_capacity + particle$remaining_capacity

  if (reachable_capacity <= 0) {
    # Particle in component with no capacity
    return(0)
  }

  # Capacity ratio component
  capacity_ratio <- reachable_capacity / map$target_capacity

  # Station coverage component (only if required)
  if (map$station_area_land_pct > 0) {

    # Maximum achievable station coverage
    max_station_area <- particle$station_area + particle$remaining_station_area
    max_area <- particle$total_area + particle$remaining_area

    if (max_area <= 0) {
      # No area in component (shouldn't happen, but handle safely)
      return(0)
    }

    max_coverage <- max_station_area / max_area
    coverage_ratio <- max_coverage / map$station_area_land_pct

    # Combined valuation
    valuation <- (capacity_ratio ^ alpha_V) * (coverage_ratio ^ beta_V)

  } else {
    # No station requirement (adjacent communities)
    valuation <- capacity_ratio ^ alpha_V
  }

  # Ensure non-negative
  return(max(0, valuation))
}

#' Get Boundary Parcels
#'
#' Find parcels adjacent to the current zone that could be added next. Returns
#' the "boundary" of the zone: parcels that share an edge with at least one
#' parcel in the zone but are not yet included.
#'
#' @param particle A ParticleState object
#' @param map An mbta_map object containing adjacency graph
#'
#' @return Integer vector of parcel indices on the boundary (may be empty if
#'   zone is fully enclosed)
#'
#' @details
#' ## Boundary Definition
#'
#' A parcel is on the boundary if:
#' 1. It is adjacent to at least one parcel in the zone (rook adjacency)
#' 2. It is not already in the zone
#'
#' ## Rook Adjacency
#'
#' Uses the adjacency graph from `map$adj`, which is built with rook adjacency
#' (shared edge required, not just corner touch). See `build_adjacency()` for
#' details.
#'
#' ## Empty Boundary
#'
#' If the boundary is empty, the particle has no more adjacent parcels to add
#' and should be marked as dead (rare - only if zone is fully enclosed or
#' isolated).
#'
#' @examples
#' \dontrun{
#' # Get boundary
#' boundary <- get_boundary_parcels(particle, map)
#'
#' if (length(boundary) == 0) {
#'   # No more adjacent parcels
#'   particle$active <- FALSE
#' } else {
#'   # Propose next parcel from boundary
#'   next_parcel <- sample(boundary, 1)
#' }
#' }
#'
#' @seealso \code{\link{build_adjacency}}
#'
#' @export
get_boundary_parcels <- function(particle, map) {

  # ========== Input Validation ==========

  if (!inherits(particle, "ParticleState")) {
    cli::cli_abort("{.arg particle} must be a {.cls ParticleState} object")
  }

  if (!inherits(map, "mbta_map")) {
    cli::cli_abort("{.arg map} must be an {.cls mbta_map} object")
  }

  # ========== Find Boundary ==========

  # Get all neighbors of parcels in zone
  all_neighbors <- unique(unlist(map$adj[particle$parcel_ids]))

  # Remove parcels already in zone
  boundary <- setdiff(all_neighbors, particle$parcel_ids)

  return(as.integer(boundary))
}

#' Copy Particle State
#'
#' Create a deep copy of a particle for resampling. All state must be copied
#' to ensure offspring particles don't share references with parents.
#'
#' @param particle A ParticleState object
#'
#' @return A new ParticleState object with all state copied
#'
#' @details
#' ## Critical for Resampling
#'
#' During resampling (PRD Section 4.3.3), particles are duplicated to
#' concentrate computational effort on high-valuation particles. All state
#' must be copied to avoid shared references.
#'
#' ## What Gets Copied
#'
#' - `parcel_ids` vector (via `c()` to force copy)
#' - All numeric state (scalars are copied by value in R)
#' - **CRITICAL**: `log_proposal_path` must be copied for correct importance weights
#'
#' ## Resampling Probability
#'
#' After copying, the offspring's `log_proposal_path` must be updated with
#' the parent selection probability:
#' ```
#' offspring$log_proposal_path <- parent$log_proposal_path + log(V_norm_parent)
#' ```
#'
#' This accounts for the resampling decision in the proposal path Q(τ).
#'
#' @examples
#' \dontrun{
#' # Resampling step
#' offspring <- copy_particle_state(parent)
#' offspring$log_proposal_path <- parent$log_proposal_path + log(parent_prob)
#' }
#'
#' @keywords internal
copy_particle_state <- function(particle) {

  # ========== Input Validation ==========

  if (!inherits(particle, "ParticleState")) {
    cli::cli_abort("{.arg particle} must be a {.cls ParticleState} object")
  }

  # ========== Deep Copy All State ==========

  # Force copy of vector (R copy-on-write semantics)
  copied_parcel_ids <- c(particle$parcel_ids)

  # Create new particle (scalars copied by value)
  copy <- list(
    # Zone composition
    parcel_ids = copied_parcel_ids,
    n_parcels = particle$n_parcels,
    component_id = particle$component_id,

    # Tier 1: Running sums
    total_capacity = particle$total_capacity,
    total_area = particle$total_area,
    station_capacity = particle$station_capacity,
    station_area = particle$station_area,

    # Tier 2: Remaining
    remaining_capacity = particle$remaining_capacity,
    remaining_area = particle$remaining_area,
    remaining_station_capacity = particle$remaining_station_capacity,
    remaining_station_area = particle$remaining_station_area,

    # RSIS tracking (CRITICAL: must copy log_proposal_path)
    log_proposal_path = particle$log_proposal_path,
    valuation_score = particle$valuation_score,
    active = particle$active,

    # Status
    n_steps = particle$n_steps
  )

  # Set S3 class
  class(copy) <- c("ParticleState", "list")

  return(copy)
}

#' Print Method for ParticleState
#'
#' Display formatted summary of particle state
#'
#' @param x A ParticleState object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.ParticleState <- function(x, ...) {

  cli::cli_h1("SMC Particle State")

  # Status
  status_icon <- if (x$active) cli::col_green("\u2713") else cli::col_red("\u2717")
  cli::cli_text("Status: {status_icon} {if (x$active) 'Active' else 'Dead'}")
  cli::cli_text("")

  # Zone composition
  cli::cli_h2("Zone Composition")
  cli::cli_text("Parcels: {.val {x$n_parcels}} (component {.val {x$component_id}})")
  cli::cli_text("Steps: {.val {x$n_steps}}")
  cli::cli_text("")

  # Tier 1: Current totals
  cli::cli_h2("Current Totals (Tier 1)")
  cli::cli_text("Capacity: {.val {round(x$total_capacity, 1)}} units")
  cli::cli_text("Area: {.val {round(x$total_area, 2)}} acres")
  if (x$station_capacity > 0 || x$station_area > 0) {
    cli::cli_text("Station capacity: {.val {round(x$station_capacity, 1)}} units")
    cli::cli_text("Station area: {.val {round(x$station_area, 2)}} acres")
  }
  cli::cli_text("")

  # Tier 2: Remaining potential
  cli::cli_h2("Remaining Potential (Tier 2)")
  cli::cli_text("Remaining capacity: {.val {round(x$remaining_capacity, 1)}} units")
  cli::cli_text("Remaining area: {.val {round(x$remaining_area, 2)}} acres")

  max_capacity <- x$total_capacity + x$remaining_capacity
  max_area <- x$total_area + x$remaining_area
  cli::cli_text("Max achievable capacity: {.val {round(max_capacity, 1)}} units")
  cli::cli_text("Max achievable area: {.val {round(max_area, 2)}} acres")
  cli::cli_text("")

  # RSIS tracking
  cli::cli_h2("RSIS Tracking")
  cli::cli_text("Log proposal path: {.val {round(x$log_proposal_path, 3)}}")
  cli::cli_text("Valuation score: {.val {round(x$valuation_score, 3)}}")

  invisible(x)
}
