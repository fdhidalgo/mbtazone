# soft_constraints.R - Capacity prior penalty functions for parcel MCMC
#
# Implements a LINEAR capacity prior that penalizes total capacity above min_capacity.
# This models empirical observation that municipalities prefer configurations
# closer to the minimum required capacity.
#
# Hard constraints (min_capacity, area, density, LCC fraction, connectivity)
# remain unchanged and are checked elsewhere.
#
# The penalty enters the MH acceptance ratio as:
#   log_accept += penalty(current_state) - penalty(proposed_state)
#   penalty = CAPACITY_PRIOR_LAMBDA * (capacity - min_capacity)
#
# This preserves detailed balance while favoring lower-capacity configurations.

# ============================================================================
# PENALTY FUNCTIONS
# ============================================================================

#' Compute capacity prior penalty for a state
#'
#' Returns penalty value (>= 0) for capacity above min_capacity.
#' Zero penalty when capacity equals min_capacity (minimum legal requirement).
#'
#' NOTE: min_capacity is enforced as a HARD constraint (legal mandate)
#' and is checked in check_hard_constraints_only(). This penalty function
#' applies a soft prior ABOVE min_capacity to favor staying near the minimum.
#'
#' Uses linear form:
#'   penalty = lambda * (cap - min_cap)  if cap > min_cap
#'           = 0                         otherwise
#'
#' @param capacity Current total capacity
#' @param min_cap Minimum capacity constraint
#' @param lambda Penalty strength (default: CAPACITY_PRIOR_LAMBDA from config)
#' @return Numeric penalty value (>= 0, where 0 means at minimum)
compute_capacity_penalty <- function(capacity, min_cap,
                                     lambda = NULL) {
  if (is.null(lambda)) {
    lambda <- CAPACITY_PRIOR_LAMBDA
  }

  # Penalize capacity above min_capacity (prior favoring near-minimum)
  # min_capacity itself is a hard constraint checked elsewhere

  if (capacity > min_cap) {
    delta <- capacity - min_cap
    return(lambda * delta)
  }
  0.0
}

#' Compute penalty difference for MH acceptance
#'
#' Returns penalty(current) - penalty(proposed) for log MH ratio.
#' Positive value favors accepting the proposal (proposed has lower penalty).
#'
#' @param current_cap Current state capacity
#' @param proposed_cap Proposed state capacity
#' @param constraints Constraint list with min_capacity
#' @param lambda Capacity prior strength (default: CAPACITY_PRIOR_LAMBDA from config)
#' @return Log penalty difference to add to MH ratio
compute_penalty_difference <- function(current_cap, proposed_cap, constraints,
                                       lambda = NULL) {
  if (is.null(lambda)) {
    lambda <- CAPACITY_PRIOR_LAMBDA
  }
  pen_current <- compute_capacity_penalty(
    current_cap, constraints$min_capacity, lambda
  )
  pen_proposed <- compute_capacity_penalty(
    proposed_cap, constraints$min_capacity, lambda
  )

  # Return difference: positive means proposal is better (lower penalty)
  pen_current - pen_proposed
}

# ============================================================================
# HARD CONSTRAINT CHECKING
# ============================================================================

#' Check hard constraints only
#'
#' Checks constraints that remain hard (capacity above min is handled by prior):
#' - min_capacity (legal mandate)
#' - min_area
#' - min_density
#' - min_lcc_fraction
#' - lcc_connectivity
#'
#' Capacity bounds are NOT checked (handled via penalty).
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints
#' @return List with feasible (logical) and constraint_failed (character or NULL)
check_hard_constraints_only <- function(state, library, parcel_graph, constraints) {
  # Guard against invalid states (zero/NA values)
  if (is.null(state$total_area) || !is.finite(state$total_area) || state$total_area <= 0) {
    return(list(feasible = FALSE, constraint_failed = "invalid_area"))
  }
  if (is.null(state$total_capacity) || !is.finite(state$total_capacity) || state$total_capacity <= 0) {
    return(list(feasible = FALSE, constraint_failed = "invalid_capacity"))
  }

  # Min capacity is a HARD constraint (legal mandate)
  # Capacity above min is handled by a soft prior (penalized but not rejected)
  if (state$total_capacity < constraints$min_capacity) {
    return(list(feasible = FALSE, constraint_failed = "min_capacity"))
  }

  # Area
  if (state$total_area < constraints$min_area) {
    return(list(feasible = FALSE, constraint_failed = "min_area"))
  }

  # Density (safe division - total_area guaranteed > 0 above)
  density <- state$total_capacity / state$total_area
  if (!is.finite(density) || density < constraints$min_density) {
    return(list(feasible = FALSE, constraint_failed = "min_density"))
  }

  # LCC fraction (safe division - total_capacity guaranteed > 0 above)
  lcc_capacity <- get_lcc_capacity(state, library, parcel_graph)
  if (!is.finite(lcc_capacity) || lcc_capacity < 0) {
    return(list(feasible = FALSE, constraint_failed = "invalid_lcc_capacity"))
  }
  lcc_fraction <- lcc_capacity / state$total_capacity
  if (!is.finite(lcc_fraction) || lcc_fraction < constraints$min_lcc_fraction) {
    return(list(feasible = FALSE, constraint_failed = "min_lcc_fraction"))
  }

  # LCC connectivity
  lcc_parcels <- state$lcc_parcels
  if (length(lcc_parcels) > 1) {
    lcc_subgraph <- igraph::induced_subgraph(parcel_graph, lcc_parcels)
    if (!igraph::is_connected(lcc_subgraph)) {
      return(list(feasible = FALSE, constraint_failed = "lcc_connectivity"))
    }
  }
  
  ## Check min area and capacity near transit if specified in constraints
  if (!is.null(constraints$station_area_pct) && !is.na(constraints$station_area_pct)) {
    area_in_station_required <- (constraints$station_area_pct / 100) * constraints$min_area
    if (state$total_area_in_station < area_in_station_required) {
      return(list(feasible = FALSE, constraint_failed = "station_area_pct"))
    }
  }
  if (!is.null(constraints$station_capacity_pct) && !is.na(constraints$station_capacity_pct)) {
    capacity_in_station_required <- (constraints$station_capacity_pct / 100) * constraints$min_capacity
    if (state$total_capacity_in_station < capacity_in_station_required) {
      return(list(feasible = FALSE, constraint_failed = "station_capacity_pct"))
    }
  }

  list(feasible = TRUE, constraint_failed = NULL)
}
