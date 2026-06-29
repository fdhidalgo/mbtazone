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
#' Test connectivity of a parcel set via BFS over precomputed adjacency
#'
#' Equivalent to \code{igraph::is_connected(induced_subgraph(graph, parcels))}
#' but without constructing a subgraph object. Walks the precomputed
#' library-indexed adjacency list restricted to the in-set membership mask.
#'
#' @param set_idx Integer library indices of the parcels in the set.
#' @param in_set Logical vector (length n_parcels) marking set membership.
#' @param neighbor_idx_list List (library order) of integer neighbor indices.
#' @return TRUE if the set induces a single connected component.
#' @keywords internal
is_set_connected <- function(set_idx, in_set, neighbor_idx_list) {
  n <- length(set_idx)
  if (n <= 1L) {
    return(TRUE)
  }
  visited <- logical(length(in_set))
  frontier <- set_idx[1L]
  visited[frontier] <- TRUE
  count <- 1L
  while (length(frontier) > 0L) {
    nb <- unlist(neighbor_idx_list[frontier], use.names = FALSE)
    if (length(nb) == 0L) break
    nb <- nb[in_set[nb] & !visited[nb]]
    if (length(nb) == 0L) break
    nb <- unique(nb)
    visited[nb] <- TRUE
    count <- count + length(nb)
    frontier <- nb
  }
  count == n
}

#' Test whether an LCC parcel set is connected (BFS fast path with igraph fallback)
#'
#' Shared connectivity check for the replace-LCC feasibility test and the
#' lcc_local removal move. Uses \code{is_set_connected()} over the precomputed
#' library-indexed adjacency when both a membership mask and \code{neighbor_idx}
#' are available, otherwise falls back to \code{igraph} on the named parcel set.
#' Both paths return the same boolean. Connectivity is start-vertex independent,
#' so the membership mask need not match any particular index ordering.
#'
#' @param set_logical Logical membership mask (library-indexed), or NULL to force
#'   the igraph fallback.
#' @param set_parcels Character parcel ids of the set (igraph fallback + size).
#' @param neighbor_idx Library-indexed adjacency list, or NULL for the fallback.
#' @param parcel_graph igraph object (fallback source).
#' @return TRUE if the set is a single connected component.
#' @keywords internal
lcc_is_connected <- function(set_logical, set_parcels, neighbor_idx, parcel_graph) {
  if (length(set_parcels) <= 1L) {
    return(TRUE)
  }
  if (!is.null(neighbor_idx) && !is.null(set_logical)) {
    return(is_set_connected(which(set_logical), set_logical, neighbor_idx))
  }
  igraph::is_connected(igraph::induced_subgraph(parcel_graph, set_parcels))
}

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

  # LCC connectivity. Reuse the maintained library-indexed membership mask
  # (state$lcc_logical) for the BFS fast path; lcc_is_connected() falls back to
  # igraph when the precomputed adjacency or mask is unavailable.
  if (!lcc_is_connected(state$lcc_logical, state$lcc_parcels,
                        library$neighbor_idx, parcel_graph)) {
    return(list(feasible = FALSE, constraint_failed = "lcc_connectivity"))
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
