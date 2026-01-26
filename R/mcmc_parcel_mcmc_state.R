# parcel_mcmc_state.R - State management for parcel MCMC
#
# Defines the parcel MCMC state structure and state manipulation functions.
# Key invariant: secondary_blocks (block IDs) is the SOURCE OF TRUTH.
# Component structure is derived from it, not computed geometrically.
#
# Uses integer indices with R's native %in% for efficient set operations.

# ============================================================================
# STATE STRUCTURE
# ============================================================================
#
# A parcel MCMC state is a list with:
#   X               - character vector of all parcel IDs in the overlay
#   X_indices       - sorted integer indices into parcel_names
#   secondary_blocks - integer vector of block IDs from L_sec (SOURCE OF TRUTH)
#   lcc_parcels     - character vector of parcel IDs in LCC (derived from X and secondary_blocks)
#   total_capacity  - integer total capacity
#   total_area      - numeric total area
#   total_capacity_in_station - integer total capacity in transit station areas
#   total_area_in_station     - numeric total area in transit station areas
#   secondary_union_indices   - sorted integer indices for union of secondary parcels
#   secondary_neighbor_counts - integer vector of neighbor counts for secondaries
#   secondary_neighbor_indices - sorted integer indices for union of secondary neighbors

# ============================================================================
# DERIVED QUANTITIES
# ============================================================================

#' Get all parcels belonging to secondary components
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @return Character vector of parcel IDs in secondaries
get_secondary_parcels <- function(state, library) {
  if (length(state$secondary_blocks) == 0) {
    return(character(0))
  }
  library_blocks_parcels(library, state$secondary_blocks)
}

#' Get parcels in the LCC
#'
#' LCC = X minus all secondary parcels
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @return Character vector of parcel IDs in LCC
get_lcc_parcels <- function(state, library) {
  sec_parcels <- get_secondary_parcels(state, library)
  setdiff(state$X, sec_parcels)
}

#' Get capacity of LCC
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @return Integer capacity
get_lcc_capacity <- function(state, library, parcel_graph) {
  lcc <- get_lcc_parcels(state, library)
  if (length(lcc) == 0) return(0L)
  sum(igraph::V(parcel_graph)[lcc]$capacity)
}

# ============================================================================
# SECONDARY UNION INDICES
# ============================================================================

#' Compute secondary union indices and neighbor counts
#'
#' @param secondary_block_ids Integer vector of secondary block IDs
#' @param library Secondary library
#' @return List with secondary_union_indices, secondary_neighbor_counts, secondary_neighbor_indices
#' @keywords internal
compute_secondary_union_fields <- function(secondary_block_ids, library) {
  n_parcels <- length(library$parcel_names)
  secondary_neighbor_counts <- integer(n_parcels)

  if (length(secondary_block_ids) == 0) {
    return(list(
      secondary_union_indices = integer(0),
      secondary_neighbor_counts = secondary_neighbor_counts,
      secondary_neighbor_indices = integer(0)
    ))
  }

  # Compute union of all secondary block indices
  secondary_union_indices <- sort(unique(unlist(
    library$blocks[secondary_block_ids]
  )))

  # Compute neighbor counts
  neighbor_indices <- library$neighbor_indices
  for (block_id in secondary_block_ids) {
    if (!is.null(neighbor_indices)) {
      nbr_idx <- neighbor_indices[[block_id]]
      if (length(nbr_idx) > 0) {
        secondary_neighbor_counts[nbr_idx] <- secondary_neighbor_counts[nbr_idx] + 1L
      }
    }
  }

  # Derive neighbor indices from counts
  secondary_neighbor_indices <- which(secondary_neighbor_counts > 0)

  list(
    secondary_union_indices = secondary_union_indices,
    secondary_neighbor_counts = secondary_neighbor_counts,
    secondary_neighbor_indices = secondary_neighbor_indices
  )
}

#' Add secondary union fields to state
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @return State with union fields populated
#' @keywords internal
add_secondary_union_fields <- function(state, library) {
  fields <- compute_secondary_union_fields(state$secondary_blocks, library)
  state$secondary_union_indices <- fields$secondary_union_indices
  state$secondary_neighbor_counts <- fields$secondary_neighbor_counts
  state$secondary_neighbor_indices <- fields$secondary_neighbor_indices
  state
}

# ============================================================================
# STATE INITIALIZATION
# ============================================================================

#' Initialize parcel MCMC state
#'
#' Creates a state from a set of parcel IDs. Secondaries must be specified
#' as block IDs (not derived geometrically).
#'
#' @param parcel_ids Character vector of all parcel IDs in the overlay
#' @param secondary_block_ids Integer vector of secondary block IDs (from L_sec)
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @return Parcel MCMC state
initialize_parcel_state <- function(parcel_ids,
                                    secondary_block_ids,
                                    library,
                                    parcel_graph) {
  # Ensure parcel_ids are character names (if indices provided)
  if (is.numeric(parcel_ids)) {
    if (length(parcel_ids) > 0) {
      parcel_ids <- library$parcel_names[parcel_ids]
    } else {
      parcel_ids <- character(0)
    }
  }

  # Validate secondary blocks are in the overlay
  if (length(secondary_block_ids) > 0) {
    sec_parcels <- library_blocks_parcels(library, secondary_block_ids)
    if (!all(sec_parcels %in% parcel_ids)) {
      stop("Secondary block parcels not all in overlay")
    }
  }

  # Build indices - ensure uniqueness (parcel_ids may have duplicates from overlapping blocks)
  all_parcel_names <- library$parcel_names
  parcel_ids <- unique(parcel_ids)  # Deduplicate before computing indices
  X_indices <- sort(unique(parcel_ids_to_indices(parcel_ids, all_parcel_names)))
  n_parcels <- length(all_parcel_names)
  X_logical <- logical(n_parcels)
  X_logical[X_indices] <- TRUE

  # DEBUG: Verify uniqueness invariant
  if (length(X_indices) != sum(X_logical)) {
    stop(sprintf(
      "BUG in initialize_parcel_state: length(X_indices)=%d != sum(X_logical)=%d. n_parcels=%d, max(X_indices)=%d",
      length(X_indices), sum(X_logical), n_parcels,
      if (length(X_indices) > 0) max(X_indices) else 0
    ))
  }

  # Compute aggregates from deduplicated parcel_ids
  total_capacity <- sum(igraph::V(parcel_graph)[parcel_ids]$capacity)
  total_area <- sum(igraph::V(parcel_graph)[parcel_ids]$area)
  total_capacity_in_station <- sum(igraph::V(parcel_graph)[parcel_ids]$capacity_in_station)
  total_area_in_station <- sum(igraph::V(parcel_graph)[parcel_ids]$area_in_station)

  # Derive LCC
  sec_parcels <- if (length(secondary_block_ids) > 0) {
    library_blocks_parcels(library, secondary_block_ids)
  } else {
    character(0)
  }
  lcc_parcels <- setdiff(parcel_ids, sec_parcels)

  secondary_fields <- compute_secondary_union_fields(secondary_block_ids, library)

  list(
    X = parcel_ids,
    X_indices = X_indices,
    X_logical = X_logical,
    secondary_blocks = secondary_block_ids,
    lcc_parcels = lcc_parcels,
    total_capacity = total_capacity,
    total_area = total_area,
    total_capacity_in_station = total_capacity_in_station,
    total_area_in_station = total_area_in_station,
    secondary_union_indices = secondary_fields$secondary_union_indices,
    secondary_neighbor_counts = secondary_fields$secondary_neighbor_counts,
    secondary_neighbor_indices = secondary_fields$secondary_neighbor_indices
  )
}

#' Create initial state from LCC-only (no secondaries)
#'
#' Convenience function for creating a single-component state.
#'
#' @param lcc_parcels Character vector of parcel IDs for the LCC
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @return Parcel MCMC state
create_lcc_only_state <- function(lcc_parcels, library, parcel_graph) {
  initialize_parcel_state(
    parcel_ids = lcc_parcels,
    secondary_block_ids = integer(0),
    library = library,
    parcel_graph = parcel_graph
  )
}

# ============================================================================
# ADDABLE/REMOVABLE BLOCKS
# ============================================================================

#' Get addable secondary blocks for birth move
#'
#' A block is addable if:
#' 1. It is disjoint from current state (no overlap)
#' 2. It is non-adjacent to current state (would create new component)
#' 3. (Optional) Adding it would not violate capacity or LCC-fraction constraints
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @param constraints Optional MBTA constraints for pre-screening
#' @param lcc_capacity Optional LCC capacity (required if constraints provided)
#' @return Integer vector of addable block indices
get_addable_blocks <- function(state, library, constraints = NULL, lcc_capacity = NULL) {
  n_blocks <- library$n_blocks
  if (n_blocks == 0) return(integer(0))

  X_logical <- state$X_logical
  if (is.null(X_logical)) {
    n_parcels <- length(library$parcel_names)
    X_logical <- logical(n_parcels)
    X_logical[state$X_indices] <- TRUE
  }

  # Pre-compute block capacities for constraint checking
  if (!is.null(constraints)) {
    block_capacities <- library$metadata$capacity
  }
  
  is_addable <- logical(n_blocks)
  blocks <- library$blocks
  neighbor_indices <- library$neighbor_indices

  for (i in seq_len(n_blocks)) {
    # Check disjoint
    if (any(X_logical[blocks[[i]]])) next

    # Check non-adjacent
    if (any(X_logical[neighbor_indices[[i]]])) next

    # Pre-screen constraints if provided (no max_capacity - capacity prior handles)
    if (!is.null(constraints)) {
      proposed_capacity <- state$total_capacity + block_capacities[i]

      # Check LCC fraction (LCC doesn't change when adding secondary)
      if (!is.null(lcc_capacity) && proposed_capacity > 0) {
        proposed_lcc_fraction <- lcc_capacity / proposed_capacity
        if (proposed_lcc_fraction < constraints$min_lcc_fraction) next
      }
    }

    is_addable[i] <- TRUE
  }

  which(is_addable)
}

#' Get addable blocks without constraint pre-screening
#'
#' For MH ratio computation in multi-birth/death moves, the reverse
#' probability must use the same proposal distribution as forward.
#' This helper returns ALL geometrically valid blocks, ignoring
#' capacity and LCC-fraction constraints. Feasibility is checked
#' separately after the full proposal is made.
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @return Integer vector of addable block indices
get_addable_blocks_unconstrained <- function(state, library) {
  n_blocks <- library$n_blocks
  if (n_blocks == 0) return(integer(0))

  # Optimized index-based check (avoiding S3 dispatch of bit package)
  # X_logical is a logical vector of length n_parcels
  X_logical <- state$X_logical
  
  # If X_logical is missing (legacy state), reconstruct from X_indices
  if (is.null(X_logical)) {
    n_parcels <- length(library$parcel_names)
    X_logical <- logical(n_parcels)
    X_logical[state$X_indices] <- TRUE
  }
  
  # We use a simple loop over indices which is extremely fast in R for this purpose
  # Pre-allocate logical vector to store results
  is_addable <- logical(n_blocks)
  
  blocks <- library$blocks
  neighbor_indices <- library$neighbor_indices
  
  for (i in seq_len(n_blocks)) {
    # Check disjoint (no overlap with current X)
    # blocks[[i]] are integer indices
    if (any(X_logical[blocks[[i]]])) next
    
    # Check non-adjacent (block's neighbors don't touch current X)
    if (any(X_logical[neighbor_indices[[i]]])) next
    
    is_addable[i] <- TRUE
  }
  
  which(is_addable)
}

#' Get removable secondary blocks for death move
#'
#' Returns the block IDs currently in state$secondary_blocks.
#'
#' @param state Parcel MCMC state
#' @return Integer vector of removable block IDs
get_removable_blocks <- function(state) {
  state$secondary_blocks
}

# ============================================================================
# LOCKED ZONE FOR LCC-LOCAL MOVES
# ============================================================================

#' Compute locked zone for LCC-local moves
#'
#' LCC-local moves must not modify secondary blocks. This function computes:
#' - locked_parcels: parcels in secondaries (cannot toggle)
#' - forbidden_adds: neighbors of secondaries (adding would merge)
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param neighbor_cache Optional named list of precomputed neighbors
#' @return List with locked_parcels and forbidden_adds
compute_locked_zone <- function(state, library, parcel_graph, neighbor_cache = NULL) {
  sec_parcels <- get_secondary_parcels(state, library)

  if (length(sec_parcels) == 0) {
    return(list(
      locked_parcels = character(0),
      forbidden_adds = character(0)
    ))
  }

  # Neighbors of secondary parcels
  sec_neighbors <- get_parcel_set_neighbors(sec_parcels, parcel_graph, neighbor_cache)

  list(
    locked_parcels = sec_parcels,
    forbidden_adds = sec_neighbors
  )
}

#' Build neighbor indices for all parcels
#'
#' Pre-computes integer index representations of each parcel's neighbors for fast
#' boundary checking during MCMC.
#'
#' @param neighbor_cache Named list of neighbor character vectors
#' @param parcel_names Character vector of all parcel names
#' @return Named list of integer index vectors
build_neighbor_indices <- function(neighbor_cache, parcel_names) {
  indices <- lapply(neighbor_cache, function(nbrs) parcel_ids_to_indices(nbrs, parcel_names))
  names(indices) <- names(neighbor_cache)
  indices
}

#' Get LCC boundary for local moves (excluding locked zone)
#'
#' Returns parcels that are valid candidates for LCC toggle moves:
#' - Inner boundary: LCC parcels with neighbors outside X
#' - Outer boundary: Non-X parcels adjacent to LCC (but not to secondaries)
#'
#' Uses integer index operations for fast boundary computation when neighbor_indices
#' is provided.
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param neighbor_cache Optional named list of precomputed neighbors
#' @param neighbor_indices Optional named list of precomputed neighbor indices
#' @return List with B_in (removable) and B_out (addable)
get_lcc_boundary <- function(state, library, parcel_graph,
                              neighbor_cache = NULL, neighbor_indices = NULL,
                              degrees = NULL) {
  parcel_names <- library$parcel_names
  n_parcels <- length(parcel_names)

  # Fast path: use incremental counts if available
  if (!is.null(state$lcc_neighbor_counts) && !is.null(degrees)) {
    lcc_logical <- state$lcc_logical
    X_logical <- state$X_logical
    lcc_counts <- state$lcc_neighbor_counts
    X_counts <- state$X_neighbor_counts

    if (sum(lcc_logical) == 0) {
      return(list(B_in = character(0), B_out = character(0)))
    }

    # Use cached secondary neighbor indices for forbidden adds
    # (parcels adjacent to any secondary - adding them would merge with secondary)
    forbidden_logical <- logical(n_parcels)
    if (length(state$secondary_neighbor_indices) > 0) {
      forbidden_logical[state$secondary_neighbor_indices] <- TRUE
    }

    # B_in: In LCC AND has neighbor outside X (X_counts < degree)
    is_B_in <- lcc_logical & (X_counts < degrees)
    B_in <- parcel_names[is_B_in]

    # B_out: Not in X AND not forbidden AND has neighbor in LCC (lcc_counts > 0)
    is_B_out <- (!X_logical) & (!forbidden_logical) & (lcc_counts > 0)
    B_out <- parcel_names[is_B_out]

    return(list(B_in = B_in, B_out = B_out))
  }

  # Fallback: loop-based implementation using integer indices
  lcc_parcels <- state$lcc_parcels

  if (length(lcc_parcels) == 0) {
    return(list(B_in = character(0), B_out = character(0)))
  }

  # Build index sets for fast operations
  X_indices <- state$X_indices
  lcc_indices <- parcel_ids_to_indices(lcc_parcels, parcel_names)
  outside_X_indices <- setdiff(seq_len(n_parcels), X_indices)

  # Use cached secondary neighbor indices if available, otherwise compute
  if (length(state$secondary_neighbor_indices) > 0) {
    forbidden_indices <- state$secondary_neighbor_indices
  } else {
    locked <- compute_locked_zone(state, library, parcel_graph, neighbor_cache)
    forbidden_indices <- parcel_ids_to_indices(locked$forbidden_adds, parcel_names)
  }

  # Inner boundary: LCC parcels with at least one neighbor outside X
  has_outside_neighbor <- vapply(lcc_indices, function(idx) {
    m <- parcel_names[idx]
    nbr_idx <- if (!is.null(neighbor_indices)) {
      neighbor_indices[[m]]
    } else if (!is.null(neighbor_cache)) {
      parcel_ids_to_indices(neighbor_cache[[m]], parcel_names)
    } else {
      parcel_ids_to_indices(igraph::neighbors(parcel_graph, m)$name, parcel_names)
    }
    any(nbr_idx %in% outside_X_indices)
  }, logical(1))
  B_in <- parcel_names[lcc_indices[has_outside_neighbor]]

  # Outer boundary: parcels outside X, adjacent to LCC, not forbidden
  candidate_indices <- setdiff(outside_X_indices, forbidden_indices)

  has_lcc_neighbor <- vapply(candidate_indices, function(idx) {
    m <- parcel_names[idx]
    nbr_idx <- if (!is.null(neighbor_indices)) {
      neighbor_indices[[m]]
    } else if (!is.null(neighbor_cache)) {
      parcel_ids_to_indices(neighbor_cache[[m]], parcel_names)
    } else {
      parcel_ids_to_indices(igraph::neighbors(parcel_graph, m)$name, parcel_names)
    }
    any(nbr_idx %in% lcc_indices)
  }, logical(1))
  B_out <- parcel_names[candidate_indices[has_lcc_neighbor]]

  list(B_in = B_in, B_out = B_out)
}

# ============================================================================
# STATE UPDATE HELPERS
# ============================================================================

#' Add a secondary block to state (for birth move)
#'
#' Adds a secondary block that must be disjoint from the current overlay.
#' Throws an error if the block overlaps X, as this indicates an upstream bug
#' (e.g., stale X_logical, wrong indices, or corrupted library).
#'
#' @param state Current state
#' @param block_id Block ID to add
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param neighbor_idx Optional integer neighbor indices for incremental updates
#' @return Updated state
add_secondary_block <- function(state, block_id_to_add, library, parcel_graph,
                                 neighbor_idx = NULL) {
  block_parcels <- library_block_parcels(library, block_id_to_add)
  parcel_names <- library$parcel_names

  # INVARIANT: Block must be disjoint from current overlay

  # If this fails, there's an upstream bug in get_addable_blocks_unconstrained()

  # or the state's X_logical/X_indices are stale
  overlap <- intersect(block_parcels, state$X)
  if (length(overlap) > 0) {
    # Debug: understand the mismatch
    block_indices <- library$blocks[[block_id_to_add]]
    X_logical_check <- if (!is.null(state$X_logical)) state$X_logical[block_indices] else rep(NA, length(block_indices))
    X_indices_check <- block_indices %in% state$X_indices

    # Check if X_logical matches X
    X_from_logical <- which(state$X_logical)
    X_from_indices <- state$X_indices
    logical_vs_indices_match <- setequal(X_from_logical, X_from_indices)

    # Check block_parcels vs block_indices correspondence
    expected_parcels <- library$parcel_names[block_indices]
    parcels_match <- setequal(block_parcels, expected_parcels)

    stop(sprintf(
      paste0(
        "Invariant violation in add_secondary_block: block %s overlaps X by %s parcels.\n",
        "  overlap: %s\n",
        "  block_indices: %s\n",
        "  X_logical[block_indices]: %s (should all be FALSE for addable block)\n",
        "  block_indices %%in%% X_indices: %s\n",
        "  X_logical matches X_indices: %s\n",
        "  block_parcels matches parcel_names[block_indices]: %s\n",
        "  length(X): %d, length(X_indices): %d, sum(X_logical): %d"
      ),
      block_id_to_add, length(overlap),
      paste(head(overlap, 5), collapse = ", "),
      paste(head(block_indices, 10), collapse = ", "),
      paste(head(X_logical_check, 10), collapse = ", "),
      paste(head(X_indices_check, 10), collapse = ", "),
      logical_vs_indices_match,
      parcels_match,
      length(state$X), length(state$X_indices), sum(state$X_logical)
    ))
  }

  new_X <- c(state$X, block_parcels)
  new_secondary_blocks <- c(state$secondary_blocks, block_id_to_add)

  # All block parcels are added (no overlap)
  added_capacity <- sum(igraph::V(parcel_graph)[block_parcels]$capacity)
  added_area <- sum(igraph::V(parcel_graph)[block_parcels]$area)
  added_capacity_in_station <- sum(igraph::V(parcel_graph)[block_parcels]$capacity_in_station)
  added_area_in_station <- sum(igraph::V(parcel_graph)[block_parcels]$area_in_station)

  # Update X_indices
  block_indices <- library$blocks[[block_id_to_add]]
  new_X_indices <- sort(union(state$X_indices, block_indices))

  new_state <- list(
    X = new_X,
    X_indices = new_X_indices,
    secondary_blocks = new_secondary_blocks,
    lcc_parcels = state$lcc_parcels,
    total_capacity = state$total_capacity + added_capacity,
    total_area = state$total_area + added_area,
    total_capacity_in_station = state$total_capacity_in_station + added_capacity_in_station,
    total_area_in_station = state$total_area_in_station + added_area_in_station
  )

  # Update secondary union indices if available (union is idempotent, handles overlaps)
  if (!is.null(state$secondary_union_indices)) {
    new_state$secondary_union_indices <- sort(union(
      state$secondary_union_indices,
      block_indices
    ))
  }
  if (!is.null(state$secondary_neighbor_counts)) {
    new_counts <- state$secondary_neighbor_counts
    neighbor_indices <- library$neighbor_indices
    nbr_idx <- integer(0)
    if (!is.null(neighbor_indices)) {
      nbr_idx <- neighbor_indices[[block_id_to_add]]
      if (length(nbr_idx) > 0) {
        new_counts[nbr_idx] <- new_counts[nbr_idx] + 1L
      }
    }
    new_state$secondary_neighbor_counts <- new_counts
    # Derive neighbor indices from counts
    new_state$secondary_neighbor_indices <- which(new_counts > 0)
  }

  # Preserve and update incremental tracking fields if available
  if (!is.null(state$lcc_logical)) {
    new_state$lcc_logical <- state$lcc_logical
    new_state$lcc_neighbor_counts <- state$lcc_neighbor_counts

    # Update X fields for all added parcels (no overlap, so all block parcels are new)
    new_X_logical <- state$X_logical
    new_X_counts <- state$X_neighbor_counts

    # Mark all block parcels as in X
    block_indices <- library$blocks[[block_id_to_add]]
    new_X_logical[block_indices] <- TRUE

    # Update X neighbor counts for all added parcels
    if (!is.null(neighbor_idx) && length(block_indices) > 0) {
      for (idx in block_indices) {
        m <- parcel_names[idx]
        nbr_indices <- neighbor_idx[[m]]
        new_X_counts[nbr_indices] <- new_X_counts[nbr_indices] + 1L
      }
    }

    new_state$X_logical <- new_X_logical
    new_state$X_neighbor_counts <- new_X_counts
  }

  new_state
}

#' Remove a secondary block from state (for death move)
#'
#' Removes a secondary block from the state. The block must be in the current
#' secondary_blocks. Since secondaries are guaranteed disjoint (enforced by
#' add_secondary_block), all block parcels are removed from X.
#'
#' @param state Current state
#' @param block_id Block ID to remove
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param neighbor_idx Optional integer neighbor indices for incremental updates
#' @return Updated state
remove_secondary_block <- function(state, block_id_to_remove, library, parcel_graph,
                                    neighbor_idx = NULL) {
  # INVARIANT: Block must be in current secondary_blocks
  if (!block_id_to_remove %in% state$secondary_blocks) {
    stop(sprintf(
      "Invariant violation in remove_secondary_block: block %s not in secondary_blocks [%s]. k=%d. This indicates an upstream bug in removable block selection.",
      block_id_to_remove,
      paste(head(state$secondary_blocks, 10), collapse = ","),
      length(state$secondary_blocks)
    ))
  }

  block_parcels <- library_block_parcels(library, block_id_to_remove)
  parcel_names <- library$parcel_names
  n_parcels <- length(parcel_names)

  new_secondary_blocks <- setdiff(state$secondary_blocks, block_id_to_remove)

  # Since secondaries are guaranteed disjoint (enforced by add_secondary_block),
  # all block parcels should be removed. They're not in LCC (secondaries are
  # non-adjacent to LCC) and not in other secondaries (mutual disjointness).
  new_X <- setdiff(state$X, block_parcels)

  # Compute capacity/area delta from removed parcels
  removed_capacity <- sum(igraph::V(parcel_graph)[block_parcels]$capacity)
  removed_area <- sum(igraph::V(parcel_graph)[block_parcels]$area)
  removed_capacity_in_station <- sum(igraph::V(parcel_graph)[block_parcels]$capacity_in_station)
  removed_area_in_station <- sum(igraph::V(parcel_graph)[block_parcels]$area_in_station)

  # Update X_indices
  block_indices <- library$blocks[[block_id_to_remove]]
  new_X_indices <- setdiff(state$X_indices, block_indices)

  new_state <- list(
    X = new_X,
    X_indices = new_X_indices,
    secondary_blocks = new_secondary_blocks,
    lcc_parcels = state$lcc_parcels,
    total_capacity = state$total_capacity - removed_capacity,
    total_area = state$total_area - removed_area,
    total_capacity_in_station = state$total_capacity_in_station - removed_capacity_in_station,
    total_area_in_station = state$total_area_in_station - removed_area_in_station
  )

  # Recompute secondary union indices from remaining blocks (handles overlaps)
  # NOTE: When new_secondary_blocks is empty, unlist() returns NULL, not integer(0).
  # We must handle this case explicitly to preserve the field.
  if (!is.null(state$secondary_union_indices)) {
    if (length(new_secondary_blocks) == 0) {
      new_state$secondary_union_indices <- integer(0)
    } else {
      new_state$secondary_union_indices <- sort(unique(unlist(
        library$blocks[new_secondary_blocks]
      )))
      # DEBUG: Verify union is non-empty when blocks exist
      if (length(new_state$secondary_union_indices) == 0) {
        block_lens <- vapply(library$blocks[new_secondary_blocks], length, integer(1))
        stop(sprintf(
          "BUG in remove_secondary_block: Empty secondary_union_indices with %d secondary blocks.\n  block IDs: %s\n  block lengths: %s\n  library$n_blocks: %d",
          length(new_secondary_blocks),
          paste(new_secondary_blocks, collapse = ","),
          paste(block_lens, collapse = ","),
          library$n_blocks
        ))
      }
    }
  }

  # Recompute secondary neighbor counts from remaining blocks
  if (!is.null(state$secondary_neighbor_counts)) {
    new_counts <- integer(n_parcels)
    neighbor_indices <- library$neighbor_indices
    if (!is.null(neighbor_indices)) {
      for (bid in new_secondary_blocks) {
        nbr_idx <- neighbor_indices[[bid]]
        if (length(nbr_idx) > 0) {
          new_counts[nbr_idx] <- new_counts[nbr_idx] + 1L
        }
      }
    }
    new_state$secondary_neighbor_counts <- new_counts
    # Derive neighbor indices from counts
    new_state$secondary_neighbor_indices <- which(new_counts > 0)
  }

  # Recompute X_logical from remaining secondaries
  if (!is.null(state$lcc_logical)) {
    new_state$lcc_logical <- state$lcc_logical
    new_state$lcc_neighbor_counts <- state$lcc_neighbor_counts

    # X_logical = lcc_logical | secondary_union (use indices)
    new_X_logical <- state$lcc_logical
    if (length(new_state$secondary_union_indices) > 0) {
      new_X_logical[new_state$secondary_union_indices] <- TRUE
    }
    new_state$X_logical <- new_X_logical

    # Incremental update of X_neighbor_counts: decrement for all removed block parcels
    if (!is.null(neighbor_idx) && length(block_parcels) > 0) {
      new_X_counts <- state$X_neighbor_counts
      removed_indices <- match(block_parcels, parcel_names)
      removed_indices <- removed_indices[!is.na(removed_indices)]
      for (idx in removed_indices) {
        m <- parcel_names[idx]
        nbr_indices <- neighbor_idx[[m]]
        new_X_counts[nbr_indices] <- new_X_counts[nbr_indices] - 1L
      }
      new_state$X_neighbor_counts <- new_X_counts
    } else {
      new_state$X_neighbor_counts <- state$X_neighbor_counts
    }
  }

  new_state
}

#' Update LCC (add or remove a parcel)
#'
#' Adds or removes a single parcel from the LCC. Throws an error if invariants
#' are violated (adding a parcel already in X, or removing one not in X).
#'
#' @param state Current state
#' @param unit_id Parcel ID to toggle
#' @param action "add" or "remove"
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param neighbor_idx Optional integer neighbor indices for incremental updates
#' @return Updated state
update_lcc <- function(state, unit_id, action, library, parcel_graph,
                        neighbor_idx = NULL) {
  parcel_capacity <- igraph::V(parcel_graph)[unit_id]$capacity
  parcel_area <- igraph::V(parcel_graph)[unit_id]$area
  parcel_names <- library$parcel_names

  # INVARIANT checks
  if (action == "add") {
    if (unit_id %in% state$X) {
      stop(sprintf(
        "Invariant violation in update_lcc: parcel '%s' already in X (cannot add). ",
        "This indicates an upstream bug in boundary computation.",
        unit_id
      ))
    }
    new_X <- c(state$X, unit_id)
    new_lcc <- c(state$lcc_parcels, unit_id)
    new_capacity <- state$total_capacity + parcel_capacity
    new_area <- state$total_area + parcel_area
    new_capacity_in_station <- state$total_capacity_in_station + igraph::V(parcel_graph)[unit_id]$capacity_in_station
    new_area_in_station <- state$total_area_in_station + igraph::V(parcel_graph)[unit_id]$area_in_station
  } else {
    if (!unit_id %in% state$X) {
      stop(sprintf(
        "Invariant violation in update_lcc: parcel '%s' not in X (cannot remove). ",
        "This indicates an upstream bug in boundary computation.",
        unit_id
      ))
    }
    new_X <- setdiff(state$X, unit_id)
    new_lcc <- setdiff(state$lcc_parcels, unit_id)
    new_capacity <- state$total_capacity - parcel_capacity
    new_area <- state$total_area - parcel_area
    new_capacity_in_station <- state$total_capacity_in_station - igraph::V(parcel_graph)[unit_id]$capacity_in_station
    new_area_in_station <- state$total_area_in_station - igraph::V(parcel_graph)[unit_id]$area_in_station
  }

  # Update X_indices
  unit_idx <- parcel_ids_to_indices(unit_id, parcel_names)
  if (action == "add") {
    new_X_indices <- sort(union(state$X_indices, unit_idx))
  } else {
    new_X_indices <- setdiff(state$X_indices, unit_idx)
  }

  # Build base state
  new_state <- list(
    X = new_X,
    X_indices = new_X_indices,
    secondary_blocks = state$secondary_blocks,
    lcc_parcels = new_lcc,
    total_capacity = new_capacity,
    total_area = new_area,
    total_capacity_in_station = new_capacity_in_station,
    total_area_in_station = new_area_in_station
  )

  # Preserve secondary union fields (unchanged by LCC moves)
  if (!is.null(state$secondary_union_indices)) {
    new_state$secondary_union_indices <- state$secondary_union_indices
  }
  if (!is.null(state$secondary_neighbor_counts)) {
    new_state$secondary_neighbor_counts <- state$secondary_neighbor_counts
  }
  if (!is.null(state$secondary_neighbor_indices)) {
    new_state$secondary_neighbor_indices <- state$secondary_neighbor_indices
  }

  # Incremental update of logical vectors and counts if available
  if (!is.null(state$lcc_logical) && !is.null(neighbor_idx)) {
    idx <- which(parcel_names == unit_id)
    nbr_indices <- neighbor_idx[[unit_id]]

    # Copy and update logical vectors
    new_X_logical <- state$X_logical
    new_lcc_logical <- state$lcc_logical

    # Copy and update neighbor counts
    new_lcc_counts <- state$lcc_neighbor_counts
    new_X_counts <- state$X_neighbor_counts

    if (action == "add") {
      new_X_logical[idx] <- TRUE
      new_lcc_logical[idx] <- TRUE
      # Neighbors now have +1 neighbor in LCC and +1 in X
      new_lcc_counts[nbr_indices] <- new_lcc_counts[nbr_indices] + 1L
      new_X_counts[nbr_indices] <- new_X_counts[nbr_indices] + 1L
    } else {
      new_X_logical[idx] <- FALSE
      new_lcc_logical[idx] <- FALSE
      # Neighbors now have -1 neighbor in LCC and -1 in X
      new_lcc_counts[nbr_indices] <- new_lcc_counts[nbr_indices] - 1L
      new_X_counts[nbr_indices] <- new_X_counts[nbr_indices] - 1L
    }

    new_state$X_logical <- new_X_logical
    new_state$lcc_logical <- new_lcc_logical
    new_state$lcc_neighbor_counts <- new_lcc_counts
    new_state$X_neighbor_counts <- new_X_counts
  }

  new_state
}

#' Reset state to a new LCC (for Replace-LCC move)
#'
#' Discards all secondaries and replaces entire overlay with new LCC.
#' Optionally initializes incremental tracking fields for fast boundary computation.
#'
#' @param lcc_parcels Character vector of new LCC parcel IDs
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param neighbor_idx List of integer neighbor indices (for incremental tracking)
#' @param parcel_names Character vector of all parcel names (for incremental tracking)
#' @return New state
reset_to_lcc <- function(lcc_parcels, library, parcel_graph,
                          neighbor_idx = NULL, parcel_names = NULL) {
  new_state <- create_lcc_only_state(lcc_parcels, library, parcel_graph)

  # Initialize tracking fields if neighbor_idx provided
  # IMPORTANT: Use library$parcel_names for consistency with X_indices
  # (X_indices are computed relative to library$parcel_names)
  if (!is.null(neighbor_idx) && !is.null(parcel_names)) {
    lib_parcel_names <- library$parcel_names
    n_parcels <- length(lib_parcel_names)

    # lcc_logical: TRUE if vertex is in LCC (indexed by library$parcel_names)
    lcc_logical <- logical(n_parcels)
    lcc_logical[new_state$X_indices] <- TRUE  # Use X_indices for consistency
    names(lcc_logical) <- lib_parcel_names

    # X_logical: same as lcc_logical (no secondaries yet)
    X_logical <- lcc_logical

    # lcc_neighbor_counts: for each vertex, count neighbors in LCC
    # Note: neighbor_idx is keyed by parcel names, not indices
    lcc_neighbor_counts <- vapply(seq_along(lib_parcel_names), function(i) {
      pname <- lib_parcel_names[i]
      if (!is.null(neighbor_idx[[pname]])) {
        sum(lcc_logical[neighbor_idx[[pname]]])
      } else {
        0L
      }
    }, integer(1))
    names(lcc_neighbor_counts) <- lib_parcel_names

    # X_neighbor_counts: same as lcc_neighbor_counts (no secondaries yet)
    X_neighbor_counts <- lcc_neighbor_counts

    new_state$lcc_logical <- lcc_logical
    new_state$X_logical <- X_logical
    new_state$lcc_neighbor_counts <- lcc_neighbor_counts
    new_state$X_neighbor_counts <- X_neighbor_counts
  }

  new_state
}

# ============================================================================
# MEMORY-EFFICIENT STORAGE HELPERS
# ============================================================================

#' Create minimal state for storage (strips runtime-only fields)
#'
#' Keeps only fields needed for downstream analysis:
#' - X (parcel IDs)
#' - secondary_blocks (block IDs)
#' - lcc_parcels (LCC parcel IDs)
#' - total_capacity, total_area
#'
#' Removes expensive runtime fields: X_indices, X_logical, lcc_logical,
#' lcc_neighbor_counts, X_neighbor_counts
#'
#' @param state Full parcel MCMC state
#' @return Minimal state (list) suitable for storage
minimize_state_for_storage <- function(state) {
  list(
    X = state$X,
    secondary_blocks = state$secondary_blocks,
    lcc_parcels = state$lcc_parcels,
    total_capacity = state$total_capacity,
    total_area = state$total_area,
    total_capacity_in_station = state$total_capacity_in_station,
    total_area_in_station = state$total_area_in_station
  )
}

#' Create LCC signature for deduplication
#'
#' Creates a canonical string representation of the LCC for fast
#' comparison and deduplication. Used in discovery phase to track
#' unique LCC configurations without storing full states.
#'
#' Uses digest hash to match the key format used in discovery and library.
#' Returns "__EMPTY__" for empty LCCs.
#'
#' @param state Parcel MCMC state (or minimal state with lcc_parcels)
#' @return Character string (xxhash64 digest of sorted parcel IDs)
create_lcc_signature <- function(state) {
  lcc <- state$lcc_parcels
  if (length(lcc) == 0) return("__EMPTY__")
  # unique() guards against buggy states with duplicate parcels
  digest::digest(sort(unique(lcc)), algo = "xxhash64")
}

# ============================================================================
# DEBUG INVARIANT VALIDATION
# ============================================================================

#' Validate state invariants (debug mode)
#'
#' Checks critical invariants that should hold after every accepted move.
#' Stops with an informative error if any invariant is violated.
#' Called only when DEBUG_INVARIANT_CHECKS is TRUE.
#'
#' Invariants checked:
#' 1. X == union(lcc_parcels, secondary_parcels) exactly
#' 2. lcc_parcels and secondary_parcels are disjoint
#' 3. Secondary blocks are pairwise disjoint
#' 4. Secondary blocks are pairwise non-adjacent
#' 5. X_indices matches X exactly
#' 6. total_capacity equals sum over X
#' 7. total_area equals sum over X
#'
#' @param state Current parcel MCMC state
#' @param parcel_graph igraph object with capacity/area attributes
#' @param secondary_library Secondary library for block lookups
#' @param step Optional step number for error messages
#' @param move_type Optional move type for error messages
#' @return TRUE invisibly if all invariants hold, stops otherwise
#' @keywords internal
validate_state_invariants <- function(
    state,
    parcel_graph,
    secondary_library,
    step = NA,
    move_type = NA
) {

  # Helper for error messages (use %s for safety - step might be numeric)
  ctx <- if (is.na(step)) "" else sprintf(" at step %s (%s)", as.character(step), move_type)

  # --- Invariant 1: X == union(lcc_parcels, secondary_parcels) ---
  sec_parcels <- if (length(state$secondary_blocks) == 0) {
    character(0)
  } else {
    library_blocks_parcels(secondary_library, state$secondary_blocks)
  }
  expected_X <- union(state$lcc_parcels, sec_parcels)

  if (!setequal(state$X, expected_X)) {
    in_X_not_expected <- setdiff(state$X, expected_X)
    in_expected_not_X <- setdiff(expected_X, state$X)
    stop(sprintf(
      "Invariant violation%s: X != union(lcc_parcels, secondary_parcels)\n  In X but not expected: %s\n  Expected but not in X: %s",
      ctx,
      if (length(in_X_not_expected) == 0) "(none)" else paste(head(in_X_not_expected, 5), collapse = ", "),
      if (length(in_expected_not_X) == 0) "(none)" else paste(head(in_expected_not_X, 5), collapse = ", ")
    ))
  }

  # --- Invariant 2: lcc_parcels ∩ secondary_parcels == ∅ ---
  overlap <- intersect(state$lcc_parcels, sec_parcels)
  if (length(overlap) > 0) {
    stop(sprintf(
      "Invariant violation%s: lcc_parcels and secondary_parcels overlap\n  Overlap: %s",
      ctx,
      paste(head(overlap, 5), collapse = ", ")
    ))
  }

  # --- Invariant 3 & 4: Secondaries pairwise disjoint and non-adjacent ---
  n_sec <- length(state$secondary_blocks)
  if (n_sec >= 2) {
    for (i in seq_len(n_sec - 1)) {
      for (j in (i + 1):n_sec) {
        block_i <- state$secondary_blocks[i]
        block_j <- state$secondary_blocks[j]

        # Check disjoint (using integer indices)
        indices_i <- secondary_library$blocks[[block_i]]
        indices_j <- secondary_library$blocks[[block_j]]

        if (!is.null(indices_i) && !is.null(indices_j) &&
            !(length(indices_i) == 1 && is.na(indices_i)) &&
            !(length(indices_j) == 1 && is.na(indices_j))) {
          if (any(indices_i %in% indices_j)) {
            stop(sprintf(
              "Invariant violation%s: Secondary blocks %s and %s overlap",
              ctx, block_i, block_j
            ))
          }

          # Check non-adjacent
          nbr_indices_i <- secondary_library$neighbor_indices[[block_i]]
          if (!is.null(nbr_indices_i) && !(length(nbr_indices_i) == 1 && is.na(nbr_indices_i))) {
            if (any(nbr_indices_i %in% indices_j)) {
              stop(sprintf(
                "Invariant violation%s: Secondary blocks %s and %s are adjacent",
                ctx, block_i, block_j
              ))
            }
          }
        }
      }
    }
  }

  # --- Invariant 5: X_indices matches X ---
  if (!is.null(state$X_indices)) {
    parcel_names <- secondary_library$parcel_names
    expected_indices <- sort(match(state$X, parcel_names))
    expected_indices <- expected_indices[!is.na(expected_indices)]

    if (!setequal(state$X_indices, expected_indices)) {
      stop(sprintf(
        "Invariant violation%s: X_indices doesn't match X\n  X_indices has %s indices, X has %s parcels",
        ctx, length(state$X_indices), length(state$X)
      ))
    }
  }

  # --- Invariant 6: total_capacity equals sum over X ---
  actual_capacity <- sum(igraph::V(parcel_graph)[state$X]$capacity)
  if (abs(actual_capacity - state$total_capacity) > 1) {
    stop(sprintf(
      "Invariant violation%s: Capacity mismatch\n  Stored: %.0f, Actual: %.0f, Diff: %.0f",
      ctx, state$total_capacity, actual_capacity, actual_capacity - state$total_capacity
    ))
  }

  # --- Invariant 7: total_area equals sum over X ---
  actual_area <- sum(igraph::V(parcel_graph)[state$X]$area)
  if (abs(actual_area - state$total_area) > 0.01) {
    stop(sprintf(
      "Invariant violation%s: Area mismatch\n  Stored: %.4f, Actual: %.4f, Diff: %.4f",
      ctx, state$total_area, actual_area, actual_area - state$total_area
    ))
  }

  invisible(TRUE)
}
