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
#   penalty = capacity_prior_lambda * (capacity - min_capacity)
#
# This preserves detailed balance while favoring lower-capacity configurations.

#' GIS-based density denominator for a set of MCMC units
#'
#' Unions the parcel geometries for the given unit IDs, applies a morphological
#' close (buffer out then back by `constraints$row_fill_m`) to fill road
#' right-of-way gaps between adjacent parcels, then subtracts the pre-dissolved
#' local density deductions to get the district area in acres.
#'
#' When `constraints$row_sfc` is set (by passing `right_of_way_sf` to
#' [define_constraints()]), the fill is ROW-constrained: only the portion of
#' the morphological close that overlaps the right-of-way polygon is added to
#' the parcel union.
#'
#' Without `row_sfc`, falls back to an unconstrained morphological close that
#' fills all gaps narrower than `2 * row_fill_m` (e.g. 18.29 m fills gaps up
#' to 36.58 m ≈ 120 ft). Set `row_fill_m = 0` in [define_constraints()] to
#' disable filling entirely.
#'
#' Callers apply this ONE BLOCK AT A TIME: an MCMC state's denominator is the
#' closed area of its LCC block plus the closed area of each secondary block,
#' each closed independently. Road fill between two different blocks is excluded
#' by definition, which makes block areas exactly additive.
#'
#' @param unit_ids Character vector of unit IDs (from parcel graph vertices)
#' @param constraints Constraints list from [define_constraints()]
#' @return Numeric scalar: density denominator in acres (0 when the union lies
#'   entirely inside the deductions)
#' @keywords internal
compute_gis_density_denom <- function(unit_ids, constraints) {
  loc_ids     <- unique(unlist(constraints$unit_to_loc_ids[unit_ids], use.names = FALSE))
  geom_subset <- constraints$geom_sfc[intersect(loc_ids, names(constraints$geom_sfc))]
  if (length(geom_subset) == 0) {
    cli::cli_abort("No geometries found for unit_ids: {paste(unit_ids, collapse = ', ')}")
  }
  union_geom <- sf::st_union(geom_subset)

  row_fill_m <- constraints$row_fill_m
  if (is.null(row_fill_m) || !is.finite(row_fill_m)) {
    cli::cli_abort(
      "constraints$row_fill_m must be a finite number; it is set by define_constraints()."
    )
  }
  if (row_fill_m > 0) {
    closed_geom <- union_geom |>
      sf::st_buffer(row_fill_m,  endCapStyle = "SQUARE") |>
      sf::st_buffer(-row_fill_m, endCapStyle = "SQUARE")

    if (!is.null(constraints$row_sfc) && length(constraints$row_sfc) > 0) {
      # result = union ∪ (closed ∩ ROW) — only right-of-way fill is kept.
      row_in_closed <- suppressWarnings(
        sf::st_intersection(
          sf::st_sf(geometry = closed_geom),
          sf::st_sf(geometry = constraints$row_sfc)
        )
      )
      if (nrow(row_in_closed) > 0) {
        union_geom <- sf::st_union(c(union_geom, sf::st_geometry(row_in_closed)))
      }
    } else {
      union_geom <- closed_geom
    }
  }

  if (length(constraints$ded_sfc) > 0) {
    remainder <- sf::st_difference(
      sf::st_sf(geometry = union_geom),
      sf::st_sf(geometry = constraints$ded_sfc)
    )
    # st_difference returns zero rows when the union lies entirely inside the
    # deductions; sum() over the empty area vector gives the correct 0 acres.
    return(sum(as.numeric(sf::st_area(remainder))) / 4047)
  }
  sum(as.numeric(sf::st_area(union_geom))) / 4047
}

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
#' @param lambda Penalty strength (`target_spec$priors$capacity_prior_lambda`)
#' @return Numeric penalty value (>= 0, where 0 means at minimum)
compute_capacity_penalty <- function(capacity, min_cap, lambda) {
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
#' @param lambda Capacity prior strength (`target_spec$priors$capacity_prior_lambda`)
#' @return Log penalty difference to add to MH ratio
compute_penalty_difference <- function(current_cap, proposed_cap, constraints, lambda) {
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

  # Optional HARD upper cap on capacity. The capacity prior does the shaping
  # below it; this is only a backstop that rules out runaway plans far above
  # anything a municipality adopted. It is opt-in: callers that do not set
  # constraints$max_capacity (NULL / non-finite) get the historical behavior of
  # no upper bound, so this check is a no-op for them.
  if (!is.null(constraints$max_capacity) &&
      is.finite(constraints$max_capacity) &&
      state$total_capacity > constraints$max_capacity) {
    return(list(feasible = FALSE, constraint_failed = "max_capacity"))
  }

  # Area and density use the GIS denominator, defined per block: the closed area
  # of the LCC block plus the closed area of each secondary block, each closed
  # independently. It is therefore a pure function of the state, and every kernel
  # maintains it exactly (state$total_gis_area = state$lcc_gis_area + sum of the
  # library gis_area of the current secondaries). Recomputing a whole-state union
  # here would give a different, kernel-path-dependent number and break detailed
  # balance, so a missing cache is a caller bug, not something to work around.
  gis_denom <- state$total_gis_area
  if (is.null(gis_denom) || !is.finite(gis_denom)) {
    cli::cli_abort(c(
      "state$total_gis_area is missing or non-finite.",
      i = "Every kernel must set total_gis_area (and lcc_gis_area) on the state it proposes."
    ))
  }

  if (gis_denom <= 0) {
    return(list(feasible = FALSE, constraint_failed = "invalid_area"))
  }
  if (gis_denom < constraints$min_area) {
    return(list(feasible = FALSE, constraint_failed = "min_area"))
  }

  density <- state$total_capacity / gis_denom
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

#' Precompute GIS density-denominator area for every block in a library
#'
#' Called once before the MCMC loop for both the LCC and secondary libraries.
#' Kernels then update \code{state$total_gis_area} via O(1) arithmetic instead of
#' calling \code{st_union()} at every proposal.
#'
#' The arithmetic is exact because the denominator is defined per block: each
#' block is closed independently and cross-block road fill is excluded, so a
#' state's denominator is the sum of its blocks' areas.
#'
#' @param library LCC or secondary library list.
#' @param constraints Constraints list from \code{\link{define_constraints}}.
#' @return The library with \code{metadata$gis_area} populated.
#' @keywords internal
enrich_library_with_gis_areas <- function(library, constraints) {
  n <- library$n_blocks
  if (n == 0L) {
    library$metadata[, gis_area := numeric(0)]
    return(library)
  }
  gis_areas <- vapply(seq_len(n), function(i) {
    block_unit_ids <- library$parcel_names[library$blocks[[i]]]
    compute_gis_density_denom(block_unit_ids, constraints)
  }, numeric(1))
  library$metadata[, gis_area := gis_areas]
  library
}
