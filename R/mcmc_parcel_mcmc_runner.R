# parcel_mcmc_runner.R - Main MCMC sampler for parcel MCMC
#
# Contains the main MCMC loop and targets wrappers.

# ============================================================================
# INITIAL STATE GENERATION
# ============================================================================

#' Generate initial parcel state
#'
#' Creates a feasible initial state by selecting a random LCC from L_LCC.
#' Pre-filters the library to only feasible LCCs for efficiency.
#' Starts with no secondaries (single component).
#'
#' @param parcel_graph_result Result from build_parcel_graph_target()
#' @param constraints MBTA constraints
#' @param libraries Result from build_parcel_block_libraries_target()
#' @param seed Random seed
#' @return Initial parcel MCMC state
generate_initial_parcel_state <- function(
  parcel_graph_result,
  constraints,
  libraries,
  seed = 123
) {
  set.seed(seed)

  parcel_graph <- parcel_graph_result$parcel_graph
  lcc_library <- libraries$lcc_library
  secondary_library <- libraries$secondary_library

  if (lcc_library$n_blocks == 0) {
    stop("LCC library is empty - cannot generate initial state")
  }

  # Pre-filter LCC library to only feasible candidates
  # This is much more efficient than random sampling when few LCCs are feasible
  feasible_ids <- find_feasible_lcc_ids(
    lcc_library,
    parcel_graph,
    secondary_library,
    constraints
  )

  if (length(feasible_ids) == 0) {
    # Provide diagnostic information
    diagnose_lcc_feasibility(lcc_library, parcel_graph, constraints)
    stop("No feasible LCCs in library - check constraints vs graph structure")
  }

  cli::cli_alert_info(
    "Found {length(feasible_ids)}/{lcc_library$n_blocks} feasible LCCs in library"
  )

  # Pick a random feasible LCC
  # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
  candidate_id <- feasible_ids[sample.int(length(feasible_ids), 1)]
  lcc_parcels <- library_block_parcels(lcc_library, candidate_id)

  # Create state with no secondaries
  state <- create_lcc_only_state(lcc_parcels, secondary_library, parcel_graph)

  cli::cli_alert_success(
    "Initial state: {length(state$X)} parcels, capacity={state$total_capacity}, area={round(state$total_area, 1)}"
  )

  state
}

#' Find IDs of feasible LCCs in library
#'
#' Scans the LCC library and returns indices of LCCs that satisfy all constraints.
#'
#' @param lcc_library LCC library from build_lcc_library_from_discovered()
#' @param parcel_graph The parcel graph
#' @param secondary_library Secondary library (for state creation)
#' @param constraints MBTA constraints
#' @return Integer vector of feasible LCC indices
#' @keywords internal
find_feasible_lcc_ids <- function(
  lcc_library,
  parcel_graph,
  secondary_library,
  constraints
) {
  feasible_ids <- integer(0)

  for (i in seq_len(lcc_library$n_blocks)) {
    lcc_parcels <- library_block_parcels(lcc_library, i)
    state <- create_lcc_only_state(lcc_parcels, secondary_library, parcel_graph)
    feasibility <- check_parcel_feasibility(
      state,
      secondary_library,
      parcel_graph,
      constraints
    )

    if (feasibility$feasible) {
      feasible_ids <- c(feasible_ids, i)
    }
  }

  feasible_ids
}

#' Diagnose why no LCCs are feasible
#'
#' Provides detailed breakdown of constraint failures for debugging.
#'
#' @keywords internal
diagnose_lcc_feasibility <- function(lcc_library, parcel_graph, constraints) {
  cli::cli_h3("LCC Feasibility Diagnosis")

  density_fail <- 0
  capacity_low <- 0
  area_fail <- 0
  station_area_fail <- 0
  station_capacity_fail <- 0

  for (i in seq_len(lcc_library$n_blocks)) {
    lcc <- library_block_parcels(lcc_library, i)
    cap <- sum(igraph::V(parcel_graph)[lcc]$capacity)
    area <- sum(igraph::V(parcel_graph)[lcc]$area)
    density <- cap / area
    station_area <- sum(igraph::V(parcel_graph)[lcc]$area_in_station)
    station_capacity <- sum(igraph::V(parcel_graph)[lcc]$capacity_in_station)

    if (cap < constraints$min_capacity) {
      capacity_low <- capacity_low + 1
    }
    if (area < constraints$min_area) {
      area_fail <- area_fail + 1
    }
    if (density < constraints$min_density) density_fail <- density_fail + 1
    if (!is.null(constraints$station_area_pct) && !is.na(constraints$station_area_pct)) {
      required_station_area <- (constraints$station_area_pct / 100) * constraints$min_area
      if (station_area < required_station_area) {
        station_area_fail <- station_area_fail + 1
      }
    }
    if (!is.null(constraints$station_capacity_pct) && !is.na(constraints$station_capacity_pct)) {
      required_station_capacity <- (constraints$station_capacity_pct / 100) * constraints$min_capacity
      if (station_capacity < required_station_capacity) {
        station_capacity_fail <- station_capacity_fail + 1
      }
    }
  }

  cli::cli_alert_danger("Total LCCs: {lcc_library$n_blocks}")
  cli::cli_alert_danger(
    "Failed density (< {constraints$min_density}): {density_fail}"
  )
  cli::cli_alert_danger(
    "Failed capacity low (< {constraints$min_capacity}): {capacity_low}"
  )
  cli::cli_alert_danger("Failed area (< {constraints$min_area}): {area_fail}")
}

# ============================================================================
# STAT TRACKING HELPERS
# ============================================================================

#' Update move statistics from kernel result
#'
#' Common stat-tracking pattern extracted to reduce code duplication.
#' Updates n_attempted, n_proposed, n_feasible, n_accepted based on result.
#'
#' @param counts Environment of named integer counter vectors (n_attempted,
#'   n_proposed, n_feasible, n_accepted), each keyed by move_type. Mutating an
#'   environment field avoids data.table NSE overhead in the per-step hot loop;
#'   the counts are materialized back into the \code{stats} data.table after the
#'   loop. This is a pure diagnostic counter (does not affect RNG or samples).
#' @param mt Character string identifying the move type
#' @param result List returned by a move kernel
#' @return Invisibly returns counts (modified by reference)
#' @keywords internal
update_move_stats <- function(counts, mt, result) {
  # Mirror the original `stats[move_type == mt, ...]` semantics: an mt that is
  # not a known move_type (e.g. lcc_local proposal-failures report "lcc_local",
  # which has no stats row) is a silent no-op, not a new counter entry.
  if (is.na(match(mt, names(counts$n_attempted)))) {
    return(invisible(counts))
  }
  counts$n_attempted[mt] <- counts$n_attempted[mt] + 1L
  if (!isTRUE(result$proposal_failed)) {
    counts$n_proposed[mt] <- counts$n_proposed[mt] + 1L
  }
  if (!isTRUE(result$infeasible)) {
    counts$n_feasible[mt] <- counts$n_feasible[mt] + 1L
  }
  if (isTRUE(result$accepted)) {
    counts$n_accepted[mt] <- counts$n_accepted[mt] + 1L
  }
  invisible(counts)
}

# Enrich the online LCC library with the current LCC and keep the replace-LCC
# compatibility cache exact. Adding the current LCC guarantees the reverse
# replace-LCC move is always available (100% library coverage). Testing just the
# new entry (O(k)) and appending it keeps `compatible_lccs_cache` correct without
# waiting for the next secondary change to rebuild it. Returns the (possibly
# grown) library, the (possibly updated) state, and whether a new entry was added.
enrich_lcc_library_and_cache <- function(lcc_library, current_state, parcel_graph,
                                         secondary_library, max_online_entries,
                                         neighbor_cache, constraints = NULL) {
  enrich_result <- add_lcc_to_library(
    lcc_library,
    current_state$lcc_parcels,
    parcel_graph,
    max_online_entries,
    neighbor_cache = neighbor_cache,  # Use name-based cache, not neighbor_idx
    lcc_indices = if (!is.null(current_state$lcc_logical)) {
      which(current_state$lcc_logical)
    } else {
      NULL
    }
  )
  lcc_library <- enrich_result$lcc_library
  if (enrich_result$added) {
    nid <- enrich_result$new_block_id
    # Compute gis_area for the new block so replace-LCC can use O(1) arithmetic
    if (!is.null(constraints) && "gis_area" %in% names(lcc_library$metadata)) {
      block_unit_ids <- lcc_library$parcel_names[lcc_library$blocks[[nid]]]
      gis_area_val <- tryCatch(
        compute_gis_density_denom(block_unit_ids, constraints),
        error = function(e) NA_real_
      )
      data.table::set(lcc_library$metadata,
                      which(lcc_library$metadata$block_id == nid),
                      "gis_area", gis_area_val)
    }
    if (!is.null(current_state$compatible_lccs_cache)) {
      if (is_lcc_compatible_with_secondaries(
            lcc_library$blocks[[nid]],
            lcc_library$neighbor_indices[[nid]],
            current_state$secondary_blocks,
            secondary_library)) {
        current_state$compatible_lccs_cache <-
          c(current_state$compatible_lccs_cache, nid)
      }
    }
  }
  list(
    lcc_library = lcc_library,
    current_state = current_state,
    added = enrich_result$added
  )
}

# ============================================================================
# MAIN MCMC SAMPLER
# ============================================================================

#' Run parcel MCMC
#'
#' Main MCMC loop for parcel sampling.
#'
#' @param parcel_graph igraph object (parcel graph)
#' @param initial_state Initial parcel state
#' @param target_spec Target spec from `parcel_target_spec()` (hard
#'   constraints + capacity/k priors)
#' @param secondary_library Secondary library
#' @param lcc_library LCC library
#' @param sampler_spec Sampler spec from `parcel_sampler_spec()` (kernel mix,
#'   proposal tuning, debug flag)
#' @param parcel_assignments data.table for parcel conversion
#' @param neighbor_cache Optional named list of precomputed neighbors
#' @param verbose Print progress
#' @return List with parcel_samples (thinned), lcc_signatures, stats, lcc_library, diagnostics
run_parcel_mcmc <- function(
  parcel_graph,
  initial_state,
  target_spec,
  secondary_library,
  lcc_library,
  sampler_spec,
  parcel_assignments,
  neighbor_cache = NULL,
  verbose = TRUE
) {
  # Validate config
  config <- sampler_spec
  validate_kernel_config(config)

  constraints <- target_spec$constraints
  capacity_prior_lambda <- target_spec$priors$capacity_prior_lambda
  k_prior_lambda <- target_spec$priors$k_prior_lambda
  birth_tilt_lambda <- config$birth_tilt_lambda
  swap_cap_tolerance <- config$swap_cap_tolerance
  debug_invariant_checks <- config$debug_invariant_checks
  enable_online_enrichment <- config$enable_online_enrichment
  enrichment_interval <- config$enrichment_interval
  max_online_entries <- config$max_online_entries
  enrichment_burn_in <- config$enrichment_burn_in
  max_stored_samples <- config$max_stored_samples
  store_lcc_signatures <- config$store_lcc_signatures

  n_steps <- config$n_steps
  p_lcc_local <- config$p_lcc_local
  p_replace_lcc <- config$p_replace_lcc
  p_symmetric_birth_death <- config$p_symmetric_birth_death
  p_swap <- config$p_swap

  # Build neighbor indices for fast boundary computation
  neighbor_indices <- if (!is.null(neighbor_cache)) {
    build_neighbor_indices(neighbor_cache, secondary_library$parcel_names)
  } else {
    NULL
  }

  # Precompute degrees and integer neighbor indices for incremental updates
  # IMPORTANT: Use library$parcel_names as the canonical source of truth for indexing
  # This ensures consistency with X_indices and library$blocks which are library-indexed
  lib_parcel_names <- secondary_library$parcel_names
  n_parcels <- length(lib_parcel_names)

  # Graph parcel names (may differ in ordering from library after serialization)
  graph_parcel_names <- igraph::V(parcel_graph)$name

  # The fast boundary path compares library-ordered X_neighbor_counts against
  # graph-ordered degrees positionally, and kernels index the maintained counts
  # by library position. Both are silently wrong if the orders diverge.
  if (!identical(graph_parcel_names, lib_parcel_names)) {
    stop("run_parcel_mcmc: parcel_graph vertex order does not match secondary_library$parcel_names; positional count/degree comparisons would be incorrect")
  }

  degrees <- igraph::degree(parcel_graph)
  names(degrees) <- graph_parcel_names

  # Integer neighbor indices (based on library indexing)
  # Map from library index to neighbor library indices
  lib_name_to_idx <- setNames(seq_len(n_parcels), lib_parcel_names)
  neighbor_idx <- lapply(lib_parcel_names, function(m) {
    # Get character neighbors from cache, convert to library indices
    if (!is.null(neighbor_cache[[m]])) {
      unname(lib_name_to_idx[neighbor_cache[[m]]])
    } else {
      integer(0)
    }
  })
  names(neighbor_idx) <- lib_parcel_names

  # Flat integer edge arrays (library-indexed) for the vectorized neighbor-count
  # path in reset_to_lcc(); invariant for the run, so built once here.
  nbr_edges <- build_neighbor_edge_arrays(neighbor_idx, n_parcels)
  nbr_from <- nbr_edges$from
  nbr_to <- nbr_edges$to

  # Expose the library-indexed parcel adjacency on the library so the LCC
  # connectivity check (check_hard_constraints_only) can BFS over it instead of
  # rebuilding an igraph induced subgraph each call.
  secondary_library$neighbor_idx <- neighbor_idx

  # Flat block-closure arrays for the vectorized addable-block scan (runs twice
  # per birth/death/swap move); static because the secondary library never
  # changes during a run.
  if (is.null(secondary_library$closure_flat_block)) {
    closure_arrays <- build_block_closure_arrays(secondary_library)
    secondary_library$closure_flat_block <- closure_arrays$closure_flat_block
    secondary_library$closure_flat_parcel <- closure_arrays$closure_flat_parcel
  }

  # Per-id cache of secondary-free LCC-only states for the replace-LCC kernel.
  # Library block ids are monotonic and never reused, so caching is safe.
  lcc_state_cache <- new.env(parent = emptyenv(), hash = TRUE, size = 1024L)

  # Precompute per-parcel graph attributes as plain vectors indexed by library
  # parcel order (== X_indices / blocks indexing). This replaces the many
  # `igraph::V(parcel_graph)[names]$attr` hash-lookups scattered across the hot
  # path (get_lcc_capacity, initialize_parcel_state, add/remove_secondary_block,
  # validate_state_invariants, centroid tracking) with O(1) integer indexing.
  # Values are bit-identical (same per-parcel attribute, same sums).
  if (is.null(secondary_library$cap_vec)) {
    sl_names <- secondary_library$parcel_names
    secondary_library$cap_vec          <- igraph::V(parcel_graph)[sl_names]$capacity
    secondary_library$area_vec         <- igraph::V(parcel_graph)[sl_names]$area
    secondary_library$cap_station_vec  <- igraph::V(parcel_graph)[sl_names]$capacity_in_station
    secondary_library$area_station_vec <- igraph::V(parcel_graph)[sl_names]$area_in_station
    secondary_library$centroid_x_vec   <- igraph::V(parcel_graph)[sl_names]$centroid_x
    secondary_library$centroid_y_vec   <- igraph::V(parcel_graph)[sl_names]$centroid_y
  }

  # Precompute GIS density-denominator area for every library block (one
  # st_union per block, amortised over all MCMC proposals). Non-local kernels
  # (birth/death, swap, replace-LCC) update state$total_gis_area with O(1)
  # arithmetic; local moves still call compute_gis_density_denom() directly.
  if (!"gis_area" %in% names(secondary_library$metadata) ||
      anyNA(secondary_library$metadata$gis_area)) {
    secondary_library <- enrich_library_with_gis_areas(secondary_library, constraints)
  }
  if (!"gis_area" %in% names(lcc_library$metadata) ||
      anyNA(lcc_library$metadata$gis_area)) {
    lcc_library <- enrich_library_with_gis_areas(lcc_library, constraints)
  }

  # Augment initial_state with incremental boundary tracking fields
  # These enable O(d) boundary updates instead of O(N) full scans
  if (is.null(initial_state$secondary_union_indices) ||
      is.null(initial_state$secondary_neighbor_indices)) {
    initial_state <- add_secondary_union_fields(initial_state, secondary_library)
  }
  # Build X_logical from X_indices (ensures consistency with library indexing)
  # X_indices are already relative to library$parcel_names
  initial_state$X_logical <- logical(n_parcels)
  initial_state$X_logical[initial_state$X_indices] <- TRUE

  # DEBUG: Verify consistency after overwriting X_logical
  # Also verify X matches X_indices (the actual source of truth mismatch)
  expected_indices_from_X <- lib_name_to_idx[initial_state$X]
  expected_indices_from_X <- expected_indices_from_X[!is.na(expected_indices_from_X)]

  message(sprintf("[DEBUG] Runner check: |X|=%d, |X_indices|=%d, |unique(expected)|=%d, sum(X_logical)=%d",
                  length(initial_state$X),
                  length(initial_state$X_indices),
                  length(unique(expected_indices_from_X)),
                  sum(initial_state$X_logical)))

  if (!setequal(initial_state$X_indices, expected_indices_from_X)) {
    stop(sprintf(
      "BUG: X_indices doesn't match indices derived from X! |X|=%d, |X_indices|=%d, |expected|=%d",
      length(initial_state$X), length(initial_state$X_indices), length(expected_indices_from_X)
    ))
  }

  if (length(initial_state$X_indices) != sum(initial_state$X_logical)) {
    stop(sprintf(
      "BUG in runner X_logical overwrite: length(X_indices)=%d != sum(X_logical)=%d. n_parcels=%d, max(X_indices)=%d, length(lib_parcel_names)=%d",
      length(initial_state$X_indices), sum(initial_state$X_logical), n_parcels,
      if (length(initial_state$X_indices) > 0) max(initial_state$X_indices) else 0,
      length(lib_parcel_names)
    ))
  }

  # Build lcc_logical similarly using library indexing
  lcc_indices <- lib_name_to_idx[initial_state$lcc_parcels]
  lcc_indices <- lcc_indices[!is.na(lcc_indices)]
  initial_state$lcc_logical <- logical(n_parcels)
  initial_state$lcc_logical[lcc_indices] <- TRUE

  # Keep original parcel_names for kernel APIs that need graph-based naming
  parcel_names <- graph_parcel_names

  # Compute initial neighbor counts for boundary computation
  X_logical_vec <- initial_state$X_logical
  lcc_logical_vec <- initial_state$lcc_logical

  # lcc_neighbor_counts: # of neighbors in LCC (for B_out computation)
  initial_state$lcc_neighbor_counts <- vapply(
    seq_len(n_parcels),
    function(i) {
      sum(lcc_logical_vec[neighbor_idx[[i]]])
    },
    integer(1)
  )
  names(initial_state$lcc_neighbor_counts) <- parcel_names

  # X_neighbor_counts: # of neighbors in X (for B_in computation)
  initial_state$X_neighbor_counts <- vapply(
    seq_len(n_parcels),
    function(i) {
      sum(X_logical_vec[neighbor_idx[[i]]])
    },
    integer(1)
  )
  names(initial_state$X_neighbor_counts) <- parcel_names

  # Seed total_gis_area so the first non-local kernel can update it with O(1)
  # arithmetic. Local kernels recompute via compute_gis_density_denom anyway.
  if (is.null(initial_state$total_gis_area)) {
    initial_state$total_gis_area <- tryCatch(
      compute_gis_density_denom(initial_state$X, constraints),
      error = function(e) NULL
    )
  }

  # Initialize storage with adaptive thinning

  # thin_interval ensures ~max_stored_samples states regardless of n_steps
  # Use ceiling to avoid storing more than max_stored_samples for mid-sized runs
  thin_interval <- max(1L, as.integer(ceiling(n_steps / max_stored_samples)))
  n_thinned <- ceiling(n_steps / thin_interval)
  parcel_samples <- vector("list", n_thinned)
  thinned_steps <- integer(n_thinned)
  thin_idx <- 0L

  # LCC signatures for discovery deduplication (every step if enabled)
  lcc_signatures <- if (store_lcc_signatures) character(n_steps) else NULL

  # Move statistics
  move_types <- c(
    "lcc_local_add",
    "lcc_local_remove",
    "symmetric_birth_death",
    "lifted_birth_death",
    "secondary_swap",
    "replace_lcc"
  )
  stats <- data.table::data.table(
    move_type = move_types,
    n_attempted = 0L,
    n_proposed = 0L,
    n_feasible = 0L,
    n_accepted = 0L
  )

  # Diagnostics
  capacity_trajectory <- numeric(n_steps)
  n_components_trajectory <- integer(n_steps)
  n_secondaries_trajectory <- integer(n_steps)
  lcc_capacity_trajectory <- numeric(n_steps)

  # Capacity prior diagnostics
  penalty_trajectory <- numeric(n_steps)

  # Multi-chain diagnostics: centroid tracking and parcel inclusion
  all_parcel_names <- igraph::V(parcel_graph)$name
  centroid_x_trajectory <- numeric(n_steps)
  centroid_y_trajectory <- numeric(n_steps)
  # Accumulated by library index in the loop (a character-subscript update
  # rebuilds a hash over all parcel names every step); names are attached once
  # after the loop. Library order == graph order (asserted at setup).
  parcel_inclusion_count <- integer(length(all_parcel_names))

  # Purge diagnostic (tests if k=0 is feasible)
  purge_diagnostic_interval <- 100L
  purge_results <- vector("list", ceiling(n_steps / purge_diagnostic_interval))
  replace_lcc_reasons <- data.table::data.table(
    reason = c(
      "attempted",
      "proposed",
      "feasible",
      "accepted",
      "proposal_failed",
      "infeasible",
      "mh_rejected",
      "current_lcc_not_in_library",
      "no_candidates",
      "secondaries_incompatible",
      "no_similar_capacity_lcc",
      "no_compatible_lcc",
      "no_valid_lcc",
      "invalid_forward_weights",
      "selected_lcc_invalid",
      "no_reverse_candidates",
      "old_lcc_not_in_reverse_set",
      "invalid_reverse_weights",
      "old_lcc_filtered_from_reverse_set"
    ),
    count = 0L
  )
  replace_lcc_constraints <- data.table::data.table(
    constraint = c(
      "min_capacity",
      "min_area",
      "min_density",
      "min_lcc_fraction",
      "lcc_connectivity",
      "invalid_area",
      "invalid_capacity",
      "invalid_lcc_capacity",
      "station_area_pct",
      "station_capacity_pct",
      "unknown"
    ),
    count = 0L
  )
  # Diagnostic accumulators below are preallocated to n_steps (at most one
  # append per step) and truncated after the loop: repeated `c(x, new)` growth
  # reallocates the full vector on every append, which showed up as GC churn.
  replace_lcc_accept_prob <- numeric(n_steps); n_rl_accept_prob <- 0L
  replace_lcc_log_q_ratio <- numeric(n_steps); n_rl_log_q_ratio <- 0L
  replace_lcc_k_retained <- integer(n_steps); n_rl_k_retained <- 0L
  replace_lcc_n_similar_forward <- integer(n_steps); n_rl_sim_fwd <- 0L
  replace_lcc_n_similar_reverse <- integer(n_steps); n_rl_sim_rev <- 0L

  # Cross-region transition tracking (for replace_lcc moves)
  cross_region_transitions <- 0L
  same_region_transitions <- 0L

  # Online enrichment tracking
  online_adds <- 0L
  # Per-step enrichment trajectories (preallocated like the other diagnostics).
  # library_size records lcc_library$n_blocks at each step; adds records the
  # per-step count of LCCs added by enrichment (cumsum recovers the running
  # total). Both enrichment sites (interval + pre-Replace-LCC) update online_adds
  # before the record block runs, so online_adds_prev diffs capture both.
  library_size_trajectory <- integer(n_steps)
  enrichment_adds_trajectory <- integer(n_steps)
  online_adds_prev <- 0L

  # Birth move capacity tracking (for calibration)
  birth_accepted_caps <- numeric(n_steps); n_birth_caps <- 0L

  # Symmetric birth/death tracking
  symmetric_bd_births <- 0L
  symmetric_bd_deaths <- 0L
  symmetric_bd_universe_sizes <- numeric(n_steps); n_bd_universe <- 0L

  # Birth/death attempt log as preallocated typed columns, materialized into one
  # data.table after the loop. Building a 1-row data.table per attempt allocates
  # a full table (with data.table's 100-column over-allocation) every time.
  bd_step <- integer(n_steps)
  bd_kernel <- character(n_steps)
  bd_direction <- character(n_steps)
  bd_new_direction <- character(n_steps)
  bd_k_before <- integer(n_steps)
  bd_k_after <- integer(n_steps)
  bd_n_add <- integer(n_steps)
  bd_n_rem <- integer(n_steps)
  bd_n_universe <- integer(n_steps)
  bd_n_add_new <- integer(n_steps)
  bd_n_rem_new <- integer(n_steps)
  bd_n_universe_new <- integer(n_steps)
  bd_accepted <- logical(n_steps)
  bd_proposal_failed <- logical(n_steps)
  bd_infeasible <- logical(n_steps)
  bd_constraint_failed <- character(n_steps)
  bd_accept_prob <- numeric(n_steps)
  birth_death_attempt_idx <- 0L

  # Swap move tracking (capacity-balanced swap)
  swap_delta_caps <- numeric(n_steps); n_swap_deltas <- 0L # Capacity change for accepted swaps
  swap_n_similar_fwd <- integer(n_steps); n_swap_sim_fwd <- 0L # Forward similar-capacity set sizes
  swap_n_similar_rev <- integer(n_steps); n_swap_sim_rev <- 0L # Reverse similar-capacity set sizes

  # Swap rejection reason tracking
  swap_reasons <- data.table::data.table(
    reason = c(
      "attempted",
      "proposed",
      "feasible",
      "accepted",
      "proposal_failed",
      "infeasible",
      "mh_rejected",
      "no_secondaries",
      "no_addable",
      "no_similar_capacity",
      "no_reverse_similar"
    ),
    count = 0L
  )

  # Swap constraint failure tracking
  swap_constraints <- data.table::data.table(
    constraint = c(
      "min_capacity",
      "min_area",
      "min_density",
      "min_lcc_fraction",
      "lcc_connectivity",
      "secondary_overlap",
      "secondary_adjacent",
      "invalid_area",
      "invalid_capacity",
      "invalid_lcc_capacity",
      "station_area_pct",
      "station_capacity_pct",
      "unknown"
    ),
    count = 0L
  )

  # Fast per-step diagnostic counters.
  # The per-step hot loop updates these plain named-integer vectors / env fields
  # instead of doing data.table NSE filter+`:=` updates (which dominated runtime).
  # They are materialized back into the data.tables above after the loop. Pure
  # diagnostics: never read inside the loop, never affect RNG or parcel_samples.
  move_counts <- new.env(parent = emptyenv())
  move_counts$n_attempted <- setNames(integer(length(move_types)), move_types)
  move_counts$n_proposed  <- setNames(integer(length(move_types)), move_types)
  move_counts$n_feasible  <- setNames(integer(length(move_types)), move_types)
  move_counts$n_accepted  <- setNames(integer(length(move_types)), move_types)
  acc_rl_reasons     <- setNames(integer(nrow(replace_lcc_reasons)), replace_lcc_reasons$reason)
  acc_rl_constraints <- setNames(integer(nrow(replace_lcc_constraints)), replace_lcc_constraints$constraint)
  acc_swap_reasons     <- setNames(integer(nrow(swap_reasons)), swap_reasons$reason)
  acc_swap_constraints <- setNames(integer(nrow(swap_constraints)), swap_constraints$constraint)

  # Current state
  current_state <- initial_state

  # Lifted MCMC direction tracking (for non-reversible birth/death)
  use_lifted <- isTRUE(config$use_lifted)
  current_direction <- sample(c("birth", "death"), 1)
  direction_trajectory <- if (use_lifted) character(n_steps) else NULL
  direction_flips <- 0L
  lifted_bd_births <- 0L
  lifted_bd_deaths <- 0L

  # DEBUG: Check initial state consistency
  initial_actual_cap <- sum(igraph::V(parcel_graph)[current_state$X]$capacity)
  if (abs(initial_actual_cap - current_state$total_capacity) > 1) {
    warning(sprintf(
      "INITIAL STATE: Capacity mismatch! stored=%d actual=%d diff=%d",
      current_state$total_capacity, initial_actual_cap,
      initial_actual_cap - current_state$total_capacity
    ))
  }

  # DEBUG: Verify X_logical consistency right before main loop
  message(sprintf("[DEBUG] Pre-loop check: |X|=%d, |X_indices|=%d, sum(X_logical)=%d",
                  length(current_state$X), length(current_state$X_indices),
                  sum(current_state$X_logical)))
  if (length(current_state$X_indices) != sum(current_state$X_logical)) {
    stop(sprintf(
      "BUG: X_logical inconsistent BEFORE main loop! |X|=%d, |X_indices|=%d, sum(X_logical)=%d",
      length(current_state$X), length(current_state$X_indices), sum(current_state$X_logical)
    ))
  }

  # Progress milestones (log at 10%, 20%, ..., 100%)
  progress_interval <- max(1L, n_steps %/% 10L)
  start_time <- Sys.time()

  # Per-kernel timing accumulators (seconds)
  timing_lcc_local <- 0
  timing_birth_death <- 0
  timing_swap <- 0
  timing_replace_lcc <- 0

  # Kernel selection thresholds (constant across the loop)
  cumprob <- cumsum(c(
    p_lcc_local,
    p_symmetric_birth_death,
    p_swap,
    p_replace_lcc
  ))

  # Main loop
  for (step in seq_len(n_steps)) {
    # Select move type (4 kernels: lcc_local, symmetric_birth_death, swap, replace_lcc)
    u <- runif(1)
    if (u < cumprob[1]) {
      move_category <- "lcc_local"
    } else if (u < cumprob[2]) {
      # Use lifted or standard birth/death based on config
      move_category <- if (use_lifted) "lifted_birth_death" else "symmetric_birth_death"
    } else if (u < cumprob[3]) {
      move_category <- "swap"
    } else {
      move_category <- "replace_lcc"
    }


    # Execute move (with timing)
    kernel_start <- proc.time()[3]
    result <- switch(
      move_category,
      lcc_local = {
        r <- lcc_local_move(
          current_state,
          secondary_library,
          parcel_graph,
          constraints,
          capacity_prior_lambda,
          neighbor_cache,
          neighbor_indices,
          degrees,
          neighbor_idx
        )
        if (!is.null(r$move_type)) {
          update_move_stats(move_counts, r$move_type, r)
        }
        r
      },
      symmetric_birth_death = {
        r <- symmetric_birth_death_move(
          current_state,
          secondary_library,
          parcel_graph,
          constraints,
          capacity_prior_lambda = capacity_prior_lambda,
          k_prior_lambda = k_prior_lambda,
          birth_tilt_lambda = birth_tilt_lambda,
          neighbor_idx = neighbor_idx
        )
        update_move_stats(move_counts, "symmetric_birth_death", r)

        # Track capacity of accepted birth moves for calibration
        if (isTRUE(r$accepted) && isTRUE(r$direction == "birth") && !is.null(r$block_id)) {
          n_birth_caps <- n_birth_caps + 1L
          birth_accepted_caps[n_birth_caps] <-
            secondary_library$metadata$capacity[r$block_id]
        }

        # Track direction-specific counts for diagnostics
        if (isTRUE(r$accepted)) {
          if (isTRUE(r$direction == "birth")) {
            symmetric_bd_births <- symmetric_bd_births + 1L
          } else if (isTRUE(r$direction == "death")) {
            symmetric_bd_deaths <- symmetric_bd_deaths + 1L
          } else {
            # DEBUG: Catch malformed direction values
            warning(sprintf(
              "Step %d: symmetric_birth_death accepted but direction='%s' (class=%s, is.null=%s)",
              step, as.character(r$direction), class(r$direction)[1], is.null(r$direction)
            ))
          }
        }

        # Track birth/death attempt-level diagnostics (preallocated columns)
        birth_death_attempt_idx <- birth_death_attempt_idx + 1L
        bd_i <- birth_death_attempt_idx
        bd_step[bd_i] <- step
        bd_kernel[bd_i] <- "symmetric_birth_death"
        bd_direction[bd_i] <- if (is.null(r$direction)) NA_character_ else as.character(r$direction)
        bd_new_direction[bd_i] <- NA_character_
        bd_k_before[bd_i] <- if (is.null(r$k_before)) NA_integer_ else as.integer(r$k_before)
        bd_k_after[bd_i] <- if (is.null(r$k_after)) NA_integer_ else as.integer(r$k_after)
        bd_n_add[bd_i] <- if (is.null(r$n_add)) NA_integer_ else as.integer(r$n_add)
        bd_n_rem[bd_i] <- if (is.null(r$n_rem)) NA_integer_ else as.integer(r$n_rem)
        bd_n_universe[bd_i] <- if (is.null(r$n_universe)) NA_integer_ else as.integer(r$n_universe)
        bd_n_add_new[bd_i] <- if (is.null(r$n_add_new)) NA_integer_ else as.integer(r$n_add_new)
        bd_n_rem_new[bd_i] <- if (is.null(r$n_rem_new)) NA_integer_ else as.integer(r$n_rem_new)
        bd_n_universe_new[bd_i] <- if (is.null(r$n_universe_new)) NA_integer_ else as.integer(r$n_universe_new)
        bd_accepted[bd_i] <- isTRUE(r$accepted)
        bd_proposal_failed[bd_i] <- isTRUE(r$proposal_failed)
        bd_infeasible[bd_i] <- isTRUE(r$infeasible)
        bd_constraint_failed[bd_i] <- if (is.null(r$constraint_failed)) NA_character_ else as.character(r$constraint_failed)
        bd_accept_prob[bd_i] <- if (is.null(r$accept_prob)) NA_real_ else as.numeric(r$accept_prob)

        # Track universe sizes for analysis
        if (!is.null(r$n_universe) && is.finite(r$n_universe)) {
          n_bd_universe <- n_bd_universe + 1L
          symmetric_bd_universe_sizes[n_bd_universe] <- r$n_universe
        }

        r
      },
      lifted_birth_death = {
        r <- lifted_birth_death_move(
          current_state,
          current_direction,
          secondary_library,
          parcel_graph,
          constraints,
          capacity_prior_lambda = capacity_prior_lambda,
          k_prior_lambda = k_prior_lambda,
          birth_tilt_lambda = birth_tilt_lambda,
          neighbor_idx = neighbor_idx
        )
        update_move_stats(move_counts, "lifted_birth_death", r)

        # Track direction flip
        if (!is.null(r$new_direction) && r$new_direction != current_direction) {
          direction_flips <- direction_flips + 1L
        }

        # Update direction for next step
        current_direction <- r$new_direction

        # Record direction in trajectory
        if (!is.null(direction_trajectory)) {
          direction_trajectory[step] <- r$direction
        }

        # Track capacity of accepted birth moves for calibration
        if (isTRUE(r$accepted) && isTRUE(r$direction == "birth") && !is.null(r$block_id)) {
          n_birth_caps <- n_birth_caps + 1L
          birth_accepted_caps[n_birth_caps] <-
            secondary_library$metadata$capacity[r$block_id]
        }

        # Track direction-specific counts for diagnostics
        if (isTRUE(r$accepted)) {
          if (isTRUE(r$direction == "birth")) {
            lifted_bd_births <- lifted_bd_births + 1L
          } else if (isTRUE(r$direction == "death")) {
            lifted_bd_deaths <- lifted_bd_deaths + 1L
          }
        }

        # Track birth/death attempt-level diagnostics (preallocated columns)
        birth_death_attempt_idx <- birth_death_attempt_idx + 1L
        bd_i <- birth_death_attempt_idx
        bd_step[bd_i] <- step
        bd_kernel[bd_i] <- "lifted_birth_death"
        bd_direction[bd_i] <- if (is.null(r$direction)) NA_character_ else as.character(r$direction)
        bd_new_direction[bd_i] <- if (is.null(r$new_direction)) NA_character_ else as.character(r$new_direction)
        bd_k_before[bd_i] <- if (is.null(r$k_before)) NA_integer_ else as.integer(r$k_before)
        bd_k_after[bd_i] <- if (is.null(r$k_after)) NA_integer_ else as.integer(r$k_after)
        bd_n_add[bd_i] <- if (is.null(r$n_add)) NA_integer_ else as.integer(r$n_add)
        bd_n_rem[bd_i] <- if (is.null(r$n_rem)) NA_integer_ else as.integer(r$n_rem)
        bd_n_universe[bd_i] <- if (is.null(r$n_universe)) NA_integer_ else as.integer(r$n_universe)
        bd_n_add_new[bd_i] <- if (is.null(r$n_add_new)) NA_integer_ else as.integer(r$n_add_new)
        bd_n_rem_new[bd_i] <- if (is.null(r$n_rem_new)) NA_integer_ else as.integer(r$n_rem_new)
        bd_n_universe_new[bd_i] <- if (is.null(r$n_universe_new)) NA_integer_ else as.integer(r$n_universe_new)
        bd_accepted[bd_i] <- isTRUE(r$accepted)
        bd_proposal_failed[bd_i] <- isTRUE(r$proposal_failed)
        bd_infeasible[bd_i] <- isTRUE(r$infeasible)
        bd_constraint_failed[bd_i] <- if (is.null(r$constraint_failed)) NA_character_ else as.character(r$constraint_failed)
        bd_accept_prob[bd_i] <- if (is.null(r$accept_prob)) NA_real_ else as.numeric(r$accept_prob)

        r
      },
      swap = {
        r <- secondary_swap_move(
          current_state,
          secondary_library,
          parcel_graph,
          constraints,
          capacity_prior_lambda = capacity_prior_lambda,
          cap_tolerance = swap_cap_tolerance,
          neighbor_idx = neighbor_idx
        )
        update_move_stats(move_counts, "secondary_swap", r)

        # Track swap rejection reasons (fast accumulators; materialized post-loop)
        acc_swap_reasons[["attempted"]] <- acc_swap_reasons[["attempted"]] + 1L

        proposed <- !isTRUE(r$proposal_failed)
        feasible <- proposed && !isTRUE(r$infeasible)

        if (proposed) {
          acc_swap_reasons[["proposed"]] <- acc_swap_reasons[["proposed"]] + 1L
        }
        if (feasible) {
          acc_swap_reasons[["feasible"]] <- acc_swap_reasons[["feasible"]] + 1L
        }
        if (isTRUE(r$accepted)) {
          acc_swap_reasons[["accepted"]] <- acc_swap_reasons[["accepted"]] + 1L
        }

        # Track proposal failure reasons
        if (isTRUE(r$proposal_failed)) {
          acc_swap_reasons[["proposal_failed"]] <- acc_swap_reasons[["proposal_failed"]] + 1L
          if (!is.null(r$reason) && r$reason %in% names(acc_swap_reasons)) {
            acc_swap_reasons[[r$reason]] <- acc_swap_reasons[[r$reason]] + 1L
          }
        }

        # Track infeasibility and constraint failures
        if (isTRUE(r$infeasible)) {
          acc_swap_reasons[["infeasible"]] <- acc_swap_reasons[["infeasible"]] + 1L
          constraint_label <- if (
            !is.null(r$constraint_failed) &&
              r$constraint_failed %in% swap_constraints$constraint
          ) {
            r$constraint_failed
          } else {
            "unknown"
          }
          acc_swap_constraints[[constraint_label]] <- acc_swap_constraints[[constraint_label]] + 1L
        }

        # Track MH rejections
        if (feasible && !isTRUE(r$accepted)) {
          acc_swap_reasons[["mh_rejected"]] <- acc_swap_reasons[["mh_rejected"]] + 1L
        }

        # Track delta_cap for accepted swaps
        if (isTRUE(r$accepted) && !is.null(r$delta_cap)) {
          n_swap_deltas <- n_swap_deltas + 1L
          swap_delta_caps[n_swap_deltas] <- r$delta_cap
        }

        # Track similar-capacity set sizes for all proposals
        if (!is.null(r$n_similar_fwd)) {
          n_swap_sim_fwd <- n_swap_sim_fwd + 1L
          swap_n_similar_fwd[n_swap_sim_fwd] <- r$n_similar_fwd
        }
        if (!is.null(r$n_similar_rev)) {
          n_swap_sim_rev <- n_swap_sim_rev + 1L
          swap_n_similar_rev[n_swap_sim_rev] <- r$n_similar_rev
        }
        r
      },
      replace_lcc = {
        # Immediate enrichment: add current LCC to library before attempting Replace-LCC
        # This guarantees the reverse move is always possible (100% library coverage)
        if (enable_online_enrichment) {
          enrich_result <- enrich_lcc_library_and_cache(
            lcc_library, current_state, parcel_graph,
            secondary_library, max_online_entries, neighbor_cache, constraints
          )
          lcc_library <- enrich_result$lcc_library
          current_state <- enrich_result$current_state
          if (enrich_result$added) online_adds <- online_adds + 1L
        }

        r <- replace_lcc_move(
          current_state,
          lcc_library,
          secondary_library,
          parcel_graph,
          constraints,
          capacity_prior_lambda = capacity_prior_lambda,
          debug_invariant_checks = debug_invariant_checks,
          neighbor_idx = neighbor_idx,
          parcel_names = parcel_names,
          nbr_from = nbr_from,
          nbr_to = nbr_to,
          lcc_state_cache = lcc_state_cache
        )

        # Note: replace_lcc uses stricter feasibility semantics than other moves
        # (only counts as feasible if proposal succeeded AND passed feasibility)
        # Fast accumulators (materialized into data.tables after the loop)
        move_counts$n_attempted[["replace_lcc"]] <- move_counts$n_attempted[["replace_lcc"]] + 1L
        acc_rl_reasons[["attempted"]] <- acc_rl_reasons[["attempted"]] + 1L

        proposed <- !isTRUE(r$proposal_failed)
        feasible <- proposed && !isTRUE(r$infeasible)

        if (proposed) {
          move_counts$n_proposed[["replace_lcc"]] <- move_counts$n_proposed[["replace_lcc"]] + 1L
          acc_rl_reasons[["proposed"]] <- acc_rl_reasons[["proposed"]] + 1L
        }
        if (feasible) {
          move_counts$n_feasible[["replace_lcc"]] <- move_counts$n_feasible[["replace_lcc"]] + 1L
          acc_rl_reasons[["feasible"]] <- acc_rl_reasons[["feasible"]] + 1L
        }
        if (isTRUE(r$accepted)) {
          move_counts$n_accepted[["replace_lcc"]] <- move_counts$n_accepted[["replace_lcc"]] + 1L
          acc_rl_reasons[["accepted"]] <- acc_rl_reasons[["accepted"]] + 1L

          # Track cross-region transitions (only for accepted moves)
          if (!is.null(r$is_cross_region) && !is.na(r$is_cross_region)) {
            if (r$is_cross_region) {
              cross_region_transitions <- cross_region_transitions + 1L
            } else {
              same_region_transitions <- same_region_transitions + 1L
            }
          }
        }

        # Track failure reasons
        if (!proposed) {
          acc_rl_reasons[["proposal_failed"]] <- acc_rl_reasons[["proposal_failed"]] + 1L
        }
        if (isTRUE(r$infeasible)) {
          acc_rl_reasons[["infeasible"]] <- acc_rl_reasons[["infeasible"]] + 1L
          constraint_label <- if (
            !is.null(r$constraint_failed) &&
              r$constraint_failed %in% replace_lcc_constraints$constraint
          ) {
            r$constraint_failed
          } else {
            "unknown"
          }
          acc_rl_constraints[[constraint_label]] <- acc_rl_constraints[[constraint_label]] + 1L
        }
        if (!is.null(r$reason) && r$reason %in% names(acc_rl_reasons)) {
          acc_rl_reasons[[r$reason]] <- acc_rl_reasons[[r$reason]] + 1L
        }
        if (feasible && !isTRUE(r$accepted)) {
          acc_rl_reasons[["mh_rejected"]] <- acc_rl_reasons[["mh_rejected"]] + 1L
        }

        # Track MH diagnostics
        if (!is.null(r$accept_prob)) {
          n_rl_accept_prob <- n_rl_accept_prob + 1L
          replace_lcc_accept_prob[n_rl_accept_prob] <- r$accept_prob
        }
        if (!is.null(r$log_q_ratio)) {
          n_rl_log_q_ratio <- n_rl_log_q_ratio + 1L
          replace_lcc_log_q_ratio[n_rl_log_q_ratio] <- r$log_q_ratio
        }
        if (!is.null(r$k_retained)) {
          n_rl_k_retained <- n_rl_k_retained + 1L
          replace_lcc_k_retained[n_rl_k_retained] <- r$k_retained
        }
        if (!is.null(r$n_similar_forward)) {
          n_rl_sim_fwd <- n_rl_sim_fwd + 1L
          replace_lcc_n_similar_forward[n_rl_sim_fwd] <- r$n_similar_forward
        }
        if (!is.null(r$n_similar_reverse)) {
          n_rl_sim_rev <- n_rl_sim_rev + 1L
          replace_lcc_n_similar_reverse[n_rl_sim_rev] <- r$n_similar_reverse
        }
        r
      }
    )

    # Accumulate kernel timing
    kernel_elapsed <- proc.time()[3] - kernel_start
    if (move_category == "lcc_local") {
      timing_lcc_local <- timing_lcc_local + kernel_elapsed
    } else if (move_category %in% c("symmetric_birth_death", "lifted_birth_death")) {
      timing_birth_death <- timing_birth_death + kernel_elapsed
    } else if (move_category == "swap") {
      timing_swap <- timing_swap + kernel_elapsed
    } else if (move_category == "replace_lcc") {
      timing_replace_lcc <- timing_replace_lcc + kernel_elapsed
    }

    # Update current state
    current_state <- result$new_state

    # DEBUG: Check X_logical consistency after EVERY move. Gated behind
    # debug_invariant_checks and placed first so the O(N) sum(X_logical) is
    # short-circuited (not computed) in production runs.
    if (debug_invariant_checks &&
        length(current_state$X_indices) != sum(current_state$X_logical)) {
      # Compute expected X_logical from lcc and secondary union
      expected_from_components <- sum(current_state$lcc_logical) +
        length(setdiff(current_state$secondary_union_indices, which(current_state$lcc_logical)))
      stop(sprintf(
        paste0("BUG: X_logical corrupted after step %d (%s, accepted=%s)!\n",
               "  |X|=%d, |X_indices|=%d, sum(X_logical)=%d\n",
               "  sum(lcc_logical)=%d, |secondary_union_indices|=%d\n",
               "  expected_from_components=%d\n",
               "  |lcc_parcels|=%d, |secondary_blocks|=%d"),
        step, move_category, result$accepted,
        length(current_state$X), length(current_state$X_indices), sum(current_state$X_logical),
        sum(current_state$lcc_logical), length(current_state$secondary_union_indices),
        expected_from_components,
        length(current_state$lcc_parcels), length(current_state$secondary_blocks)
      ))
    }

    # DEBUG: Validate state invariants after accepted moves
    if (debug_invariant_checks && isTRUE(result$accepted)) {
      validate_state_invariants(
        current_state,
        parcel_graph,
        secondary_library,
        step = step,
        move_type = result$move_type
      )
    }

    # Store LCC signature (every step if enabled)
    if (!is.null(lcc_signatures)) {
      lcc_signatures[step] <- create_lcc_signature(current_state)
    }

    # Store thinned samples (every thin_interval steps, plus final step)
    if (step %% thin_interval == 0 || step == n_steps) {
      thin_idx <- thin_idx + 1L
      parcel_samples[[thin_idx]] <- minimize_state_for_storage(current_state)
      thinned_steps[thin_idx] <- step
    }

    # Online LCC library enrichment
    # Enrich if: enabled, on interval, and (no burn-in limit OR within burn-in)
    should_enrich <- enable_online_enrichment &&
      step %% enrichment_interval == 0 &&
      (is.null(enrichment_burn_in) || step <= enrichment_burn_in)
    if (should_enrich) {
      enrich_result <- enrich_lcc_library_and_cache(
        lcc_library, current_state, parcel_graph,
        secondary_library, max_online_entries, neighbor_cache, constraints
      )
      lcc_library <- enrich_result$lcc_library
      current_state <- enrich_result$current_state
      if (enrich_result$added) online_adds <- online_adds + 1L
    }

    # Record diagnostics
    library_size_trajectory[step] <- lcc_library$n_blocks
    enrichment_adds_trajectory[step] <- online_adds - online_adds_prev
    online_adds_prev <- online_adds
    capacity_trajectory[step] <- current_state$total_capacity
    n_secondaries_trajectory[step] <- length(current_state$secondary_blocks)
    n_components_trajectory[step] <- 1L + n_secondaries_trajectory[step] # LCC + secondaries
    lcc_capacity_trajectory[step] <- get_lcc_capacity(
      current_state,
      secondary_library,
      parcel_graph
    )

    # Capacity prior diagnostics
    penalty_trajectory[step] <- compute_capacity_penalty(
      current_state$total_capacity,
      constraints$min_capacity,
      capacity_prior_lambda
    )

    # Centroid tracking for multi-chain analysis. Use precomputed integer-indexed
    # attribute vectors (X_indices) instead of igraph hash lookups; the centroid
    # is an area-weighted mean and so order-independent (identical value).
    state_parcels <- current_state$X
    if (length(state_parcels) > 0) {
      xi <- current_state$X_indices
      areas <- secondary_library$area_vec[xi]
      cx <- secondary_library$centroid_x_vec[xi]
      cy <- secondary_library$centroid_y_vec[xi]
      total_area <- sum(areas)
      centroid_x_trajectory[step] <- sum(cx * areas) / total_area
      centroid_y_trajectory[step] <- sum(cy * areas) / total_area
      # Integer-indexed update: X_indices names the same parcels as X, and the
      # character subscript rebuilt a hash over all parcel names each step.
      parcel_inclusion_count[xi] <- parcel_inclusion_count[xi] + 1L
    }

    # Periodic purge diagnostic (test if k=0 is feasible from current state)
    if (step %% purge_diagnostic_interval == 0) {
      purge_idx <- step %/% purge_diagnostic_interval
      purge_result <- purge_diagnostic_move(
        current_state,
        secondary_library,
        parcel_graph,
        constraints
      )
      purge_results[[purge_idx]] <- c(list(step = step), purge_result)
    }

    # Log progress at milestones
    if (verbose && (step %% progress_interval == 0 || step == n_steps)) {
      pct <- round(100 * step / n_steps)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- step / elapsed
      eta_secs <- if (step < n_steps) (n_steps - step) / rate else 0
      eta_str <- if (eta_secs >= 3600) {
        sprintf("%.1fh", eta_secs / 3600)
      } else if (eta_secs >= 60) {
        sprintf("%.0fm", eta_secs / 60)
      } else {
        sprintf("%.0fs", eta_secs)
      }
      cli::cli_alert_info(
        "Parcel MCMC: {pct}% ({step}/{n_steps}) | cap={round(current_state$total_capacity)} | sec={length(current_state$secondary_blocks)} | ETA: {eta_str}"
      )
    }
  }

  # Materialize fast per-step diagnostic counters back into their data.tables.
  # (Counts accumulated in plain vectors/env during the loop to avoid data.table
  # NSE overhead; the final data.table contents are identical to per-step `:=`.)
  stats[, n_attempted := as.integer(move_counts$n_attempted[move_type])]
  stats[, n_proposed  := as.integer(move_counts$n_proposed[move_type])]
  stats[, n_feasible  := as.integer(move_counts$n_feasible[move_type])]
  stats[, n_accepted  := as.integer(move_counts$n_accepted[move_type])]
  replace_lcc_reasons[, count := as.integer(acc_rl_reasons[reason])]
  replace_lcc_constraints[, count := as.integer(acc_rl_constraints[constraint])]
  swap_reasons[, count := as.integer(acc_swap_reasons[reason])]
  swap_constraints[, count := as.integer(acc_swap_constraints[constraint])]

  # Compute timing overhead (total time minus kernel time)
  total_loop_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  total_kernel_time <- timing_lcc_local + timing_birth_death + timing_swap + timing_replace_lcc
  timing_overhead <- total_loop_time - total_kernel_time

  # Truncate the preallocated diagnostic accumulators to their fill counts and
  # attach names deferred from the loop (before the verbose summary reads them).
  birth_accepted_caps <- birth_accepted_caps[seq_len(n_birth_caps)]
  symmetric_bd_universe_sizes <- symmetric_bd_universe_sizes[seq_len(n_bd_universe)]
  swap_delta_caps <- swap_delta_caps[seq_len(n_swap_deltas)]
  swap_n_similar_fwd <- swap_n_similar_fwd[seq_len(n_swap_sim_fwd)]
  swap_n_similar_rev <- swap_n_similar_rev[seq_len(n_swap_sim_rev)]
  replace_lcc_accept_prob <- replace_lcc_accept_prob[seq_len(n_rl_accept_prob)]
  replace_lcc_log_q_ratio <- replace_lcc_log_q_ratio[seq_len(n_rl_log_q_ratio)]
  replace_lcc_k_retained <- replace_lcc_k_retained[seq_len(n_rl_k_retained)]
  replace_lcc_n_similar_forward <- replace_lcc_n_similar_forward[seq_len(n_rl_sim_fwd)]
  replace_lcc_n_similar_reverse <- replace_lcc_n_similar_reverse[seq_len(n_rl_sim_rev)]
  parcel_inclusion_count <- setNames(parcel_inclusion_count, all_parcel_names)

  # Summary statistics
  if (verbose) {
    cli::cli_h3("Move Statistics")
    for (i in seq_len(nrow(stats))) {
      mt <- stats$move_type[i]
      att <- stats$n_attempted[i]
      acc <- stats$n_accepted[i]
      rate <- if (att > 0) round(100 * acc / att, 1) else 0
      cli::cli_alert_info("{mt}: {acc}/{att} accepted ({rate}%)")
    }

    # Capacity prior summary
    cli::cli_h3("Capacity Prior")
    mean_pen <- round(mean(penalty_trajectory), 4)
    max_pen <- round(max(penalty_trajectory), 4)
    cli::cli_alert_info("Penalty: mean={mean_pen}, max={max_pen}")

    if (replace_lcc_reasons[reason == "attempted", count] > 0) {
      cli::cli_h3("Joint Core Refresh Diagnostics")
      for (i in seq_len(nrow(replace_lcc_reasons))) {
        r <- replace_lcc_reasons$reason[i]
        c <- replace_lcc_reasons$count[i]
        if (c > 0) {
          cli::cli_alert_info("{r}: {c}")
        }
      }
      if (sum(replace_lcc_constraints$count) > 0) {
        cli::cli_h3("Joint Core Refresh Constraint Failures")
        for (i in seq_len(nrow(replace_lcc_constraints))) {
          cn <- replace_lcc_constraints$constraint[i]
          c <- replace_lcc_constraints$count[i]
          if (c > 0) {
            cli::cli_alert_info("{cn}: {c}")
          }
        }
      }
    }
    # Online enrichment summary
    if (enable_online_enrichment && online_adds > 0) {
      cli::cli_h3("Online Enrichment")
      cli::cli_alert_info("LCCs added: {online_adds}")
      cli::cli_alert_info("Final library size: {lcc_library$n_blocks}")
    }

    # Swap diagnostics summary
    if (length(swap_delta_caps) > 0) {
      cli::cli_h3("Capacity-Balanced Swap Diagnostics")
      cli::cli_alert_info("Accepted swaps: {length(swap_delta_caps)}")
      cli::cli_alert_info(
        "Delta capacity: mean={round(mean(swap_delta_caps), 1)}, sd={round(sd(swap_delta_caps), 1)}"
      )
      cli::cli_alert_info(
        "Mean similar-capacity set size: fwd={round(mean(swap_n_similar_fwd), 1)}, rev={round(mean(swap_n_similar_rev), 1)}"
      )
    }
  }

  # Remove block_hashes environment before returning to avoid serialization issues
  if (!is.null(lcc_library$block_hashes)) {
    lcc_library$block_hashes <- NULL
  }

  # Materialize the birth/death attempt log from the preallocated columns
  # (identical rows/types to the former rbindlist of per-attempt tables).
  bd_seq <- seq_len(birth_death_attempt_idx)
  birth_death_attempts <- data.table::data.table(
    step = bd_step[bd_seq],
    kernel = bd_kernel[bd_seq],
    direction = bd_direction[bd_seq],
    new_direction = bd_new_direction[bd_seq],
    k_before = bd_k_before[bd_seq],
    k_after = bd_k_after[bd_seq],
    n_add = bd_n_add[bd_seq],
    n_rem = bd_n_rem[bd_seq],
    n_universe = bd_n_universe[bd_seq],
    n_add_new = bd_n_add_new[bd_seq],
    n_rem_new = bd_n_rem_new[bd_seq],
    n_universe_new = bd_n_universe_new[bd_seq],
    accepted = bd_accepted[bd_seq],
    proposal_failed = bd_proposal_failed[bd_seq],
    infeasible = bd_infeasible[bd_seq],
    constraint_failed = bd_constraint_failed[bd_seq],
    accept_prob = bd_accept_prob[bd_seq]
  )


  list(
    # Thinned minimal states (for visualization/metrics)
    parcel_samples = parcel_samples,
    thinned_steps = thinned_steps[seq_len(thin_idx)],  # Actual stored step indices
    thin_interval = thin_interval,
    # LCC signatures (for discovery deduplication)
    lcc_signatures = lcc_signatures,
    # Final state info (use parcel_graph_result$parcel_assignments for parcel reconstruction)
    final_secondary_blocks = current_state$secondary_blocks,
    final_state = minimize_state_for_storage(current_state),
    stats = stats,
    lcc_library = lcc_library,
    diagnostics = list(
      capacity_trajectory = capacity_trajectory,
      n_components_trajectory = n_components_trajectory,
      n_secondaries_trajectory = n_secondaries_trajectory,
      lcc_capacity_trajectory = lcc_capacity_trajectory,
      centroid_x_trajectory = centroid_x_trajectory,
      centroid_y_trajectory = centroid_y_trajectory,
      parcel_inclusion_count = parcel_inclusion_count,
      replace_lcc_reasons = replace_lcc_reasons,
      replace_lcc_constraints = replace_lcc_constraints,
      replace_lcc_accept_prob = replace_lcc_accept_prob,
      replace_lcc_log_q_ratio = replace_lcc_log_q_ratio,
      replace_lcc_k_retained = replace_lcc_k_retained,
      replace_lcc_n_similar_forward = replace_lcc_n_similar_forward,
      replace_lcc_n_similar_reverse = replace_lcc_n_similar_reverse,
      # Cross-region transition tracking
      cross_region_transitions = cross_region_transitions,
      same_region_transitions = same_region_transitions,
      n_steps = n_steps,
      online_adds = online_adds,
      final_library_size = lcc_library$n_blocks,
      # Per-step online enrichment trajectories
      library_size_trajectory = library_size_trajectory,
      enrichment_adds_trajectory = enrichment_adds_trajectory,
      purge_results = purge_results,
      birth_accepted_caps = birth_accepted_caps,
      birth_death_attempts = birth_death_attempts,
      # Symmetric birth/death diagnostics
      symmetric_bd_births = symmetric_bd_births,
      symmetric_bd_deaths = symmetric_bd_deaths,
      symmetric_bd_universe_sizes = symmetric_bd_universe_sizes,
      # Lifted birth/death diagnostics (non-reversible MCMC)
      lifted_bd_births = lifted_bd_births,
      lifted_bd_deaths = lifted_bd_deaths,
      direction_trajectory = direction_trajectory,
      direction_flips = direction_flips,
      use_lifted = use_lifted,
      # Swap move diagnostics (capacity-balanced swap)
      swap_delta_caps = swap_delta_caps,
      swap_n_similar_fwd = swap_n_similar_fwd,
      swap_n_similar_rev = swap_n_similar_rev,
      swap_reasons = swap_reasons,
      swap_constraints = swap_constraints,
      # Capacity prior diagnostics
      penalty_trajectory = penalty_trajectory,
      # Kernel timing (for profiling)
      timing = list(
        lcc_local = timing_lcc_local,
        birth_death = timing_birth_death,
        swap = timing_swap,
        replace_lcc = timing_replace_lcc,
        overhead = timing_overhead,
        total = total_loop_time
      )
    ),
    kernel_name = config$name,
    config = config
  )
}

# ============================================================================
# MCMC DISCOVERY SUPPLEMENT (Hybrid with Tree Discovery)
# ============================================================================

#' Run MCMC discovery chains seeded from tree-discovered LCCs
#'
#' Supplements tree discovery by exploring non-tree LCCs via lcc_local moves.
#' Tree enumeration only finds LCCs that are "tree cuts" (single edge crosses
#' the boundary). This function uses MCMC with 100% lcc_local to explore nearby
#' configurations that might have multiple boundary crossings.
#'
#' Each chain starts from a different tree-discovered LCC (selected for
#' diversity across capacity range) and explores locally.
#'
#' @param tree_discovered_lccs Output from discover_lccs_from_trees()
#' @param parcel_graph_result Result from build_parcel_graph_target()
#' @param target_spec Target spec from `parcel_target_spec()`
#' @param secondary_library Secondary block library (hydrated)
#' @param n_chains Number of MCMC chains (default 4)
#' @param n_steps Steps per chain (default 2000)
#' @param verbose Print progress
#' @return List with:
#'   - discovered_lccs: data.table with lcc_key, parcel_ids, capacity, area
#'   - n_chains: number of chains run
#'   - n_steps_per_chain: steps per chain
#'   - total_signatures: total LCC signatures collected
#'   - unique_lccs_found: number of unique LCCs discovered
#' @export
run_mcmc_discovery_supplement <- function(
    tree_discovered_lccs,
    parcel_graph_result,
    target_spec,
    secondary_library,
    n_chains = 4L,
    n_steps = 2000L,
    verbose = TRUE
) {
  parcel_graph <- parcel_graph_result$parcel_graph
  parcel_names <- igraph::V(parcel_graph)$name

  # Get tree-discovered LCCs data.table

  tree_lccs <- tree_discovered_lccs$discovered_lccs
  constraints <- target_spec$constraints

  if (nrow(tree_lccs) == 0) {
    cli::cli_alert_warning("No tree-discovered LCCs to seed from")
    return(list(
      discovered_lccs = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        source = character(0)
      ),
      n_chains = 0L,
      n_steps_per_chain = n_steps,
      total_signatures = 0L,
      unique_lccs_found = 0L
    ))
  }

  # Select diverse seed LCCs (spread across capacity range)
  tree_lccs <- tree_lccs[order(capacity)]
  n_available <- nrow(tree_lccs)
  n_seeds <- min(n_chains, n_available)

  # Pick at quantiles: 20%, 40%, 60%, 80% of capacity range
  quantile_positions <- seq(0.2, 0.8, length.out = n_seeds)
  seed_indices <- pmax(1L, pmin(n_available, round(quantile_positions * n_available)))
  seed_indices <- unique(seed_indices)  # Remove duplicates if small library

  if (verbose) {
    cli::cli_h2("MCMC Discovery Supplement")
    cli::cli_alert_info(
      "Seeding {length(seed_indices)} chains from {n_available} tree-discovered LCCs"
    )
    cli::cli_alert_info("Steps per chain: {n_steps}")
  }

  # Get discovery config (100% lcc_local). swap/birth-tilt tolerances are
  # inert here (p_swap = p_symmetric_birth_death = p_replace_lcc = 0), so 0
  # is an honest placeholder rather than a meaningful tuning choice.
  discovery_config <- parcel_sampler_spec_discovery(
    swap_cap_tolerance = 0,
    birth_tilt_lambda = 0,
    debug_invariant_checks = FALSE
  )
  discovery_config$n_steps <- n_steps

  # Hydrate secondary library if needed
  if (is.null(secondary_library$blocks)) {
    secondary_library <- hydrate_library(secondary_library)
  }

  # Create a minimal LCC library for the MCMC runner (not used for proposals)
  # The discovery config has p_replace_lcc = 0, so this is just for API compatibility
  dummy_lcc_library <- list(
    blocks = list(),
    neighbor_indices = list(),
    metadata = data.table::data.table(
      block_id = integer(0),
      capacity = integer(0),
      area = numeric(0)
    ),
    parcel_names = parcel_names,
    n_blocks = 0L
  )

  # Collect all LCC signatures across chains
  all_signatures <- list()
  all_lcc_data <- list()

  for (chain_idx in seq_along(seed_indices)) {
    seed_row <- tree_lccs[seed_indices[chain_idx]]
    seed_lcc_parcels <- seed_row$parcel_ids[[1]]
    seed_capacity <- seed_row$capacity

    if (verbose) {
      cli::cli_alert(
        "Chain {chain_idx}/{length(seed_indices)}: seed capacity = {seed_capacity}"
      )
    }

    # Create initial state from seed LCC (no secondaries for discovery)
    initial_state <- create_lcc_only_state(
      lcc_parcels = seed_lcc_parcels,
      library = secondary_library,
      parcel_graph = parcel_graph
    )

    # Offset seed for each chain
    chain_config <- discovery_config
    chain_config$seed <- discovery_config$seed + chain_idx * 1000L
    # This discovery supplement wants enrichment off and LCC signatures on —
    # the opposite of a production chain's defaults.
    chain_config$enable_online_enrichment <- FALSE
    chain_config$store_lcc_signatures <- TRUE

    # Run MCMC with lcc_local only
    result <- run_parcel_mcmc(
      parcel_graph = parcel_graph,
      initial_state = initial_state,
      target_spec = target_spec,
      secondary_library = secondary_library,
      lcc_library = dummy_lcc_library,
      sampler_spec = chain_config,
      parcel_assignments = parcel_graph_result$parcel_assignments,
      neighbor_cache = parcel_graph_result$neighbor_cache,
      verbose = FALSE
    )

    # Collect signatures
    if (!is.null(result$lcc_signatures)) {
      all_signatures[[chain_idx]] <- result$lcc_signatures
    }

    # Also extract capacity/area for each unique LCC from this chain
    chain_unique_sigs <- unique(result$lcc_signatures)
    # Filter out empty/invalid signatures to avoid NA values
    chain_unique_sigs <- chain_unique_sigs[
      !is.na(chain_unique_sigs) &
      chain_unique_sigs != "__EMPTY__" &
      nchar(chain_unique_sigs) > 0
    ]
    for (sig in chain_unique_sigs) {
      if (!sig %in% names(all_lcc_data)) {
        # Parse signature back to parcel IDs
        lcc_parcels <- strsplit(sig, ",")[[1]]
        lcc_indices <- match(lcc_parcels, parcel_names)
        # Guard against NA indices (parcels not found)
        lcc_indices <- lcc_indices[!is.na(lcc_indices)]
        if (length(lcc_indices) == 0) next
        lcc_capacity <- sum(igraph::V(parcel_graph)$capacity[lcc_indices])
        lcc_area <- sum(igraph::V(parcel_graph)$area[lcc_indices])

        all_lcc_data[[sig]] <- list(
          lcc_key = sig,
          parcel_ids = lcc_parcels,
          capacity = as.integer(lcc_capacity),
          area = lcc_area
        )
      }
    }

    if (verbose) {
      cli::cli_alert_success(
        "Chain {chain_idx}: {length(result$lcc_signatures)} steps, {length(chain_unique_sigs)} unique LCCs"
      )
    }
  }

  # Combine all signatures
  combined_signatures <- unlist(all_signatures)
  unique_signatures <- unique(combined_signatures)

  # Build output data.table
  if (length(all_lcc_data) > 0) {
    discovered_lccs <- data.table::rbindlist(lapply(all_lcc_data, function(d) {
      data.table::data.table(
        lcc_key = d$lcc_key,
        parcel_ids = list(d$parcel_ids),
        capacity = d$capacity,
        area = d$area,
        source = "mcmc"
      )
    }))
  } else {
    discovered_lccs <- data.table::data.table(
      lcc_key = character(0),
      parcel_ids = list(),
      capacity = integer(0),
      area = numeric(0),
      source = character(0)
    )
  }

  if (verbose) {
    cli::cli_h2("MCMC Discovery Complete")
    cli::cli_alert_success(
      "Found {nrow(discovered_lccs)} unique LCCs from {length(seed_indices)} chains"
    )
  }

  list(
    discovered_lccs = discovered_lccs,
    n_chains = length(seed_indices),
    n_steps_per_chain = n_steps,
    total_signatures = length(combined_signatures),
    unique_lccs_found = nrow(discovered_lccs)
  )
}
