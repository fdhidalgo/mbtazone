# parcel_mcmc_kernels.R - MCMC move implementations for parcel MCMC
#
# Implements four kernels:
# 1. LCC-local: Toggle/swap on LCC boundary only
# 2. Symmetric birth/death: Unified add/remove secondary block
# 3. Secondary swap: Capacity-balanced swap (geographic mixing)
# 4. Replace-LCC: Global LCC relocation (reset to single component)

# ============================================================================
# FEASIBILITY CHECKING
# ============================================================================

#' Check feasibility of a parcel state
#'
#' Verifies that the state satisfies MBTA constraints.
#'
#' @param state Parcel MCMC state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints
#' @return List with feasible (logical) and constraint_failed (character or NULL)
check_parcel_feasibility <- function(
  state,
  library,
  parcel_graph,
  constraints
) {
  # Minimum capacity (hard constraint - legal mandate)
  if (state$total_capacity < constraints$min_capacity) {
    return(list(feasible = FALSE, constraint_failed = "min_capacity"))
  }
  # Note: No max_capacity check - using capacity prior instead (soft constraint)

  # Area
  if (state$total_area < constraints$min_area) {
    return(list(feasible = FALSE, constraint_failed = "min_area"))
  }

  # Density
  density <- state$total_capacity / state$total_area
  if (density < constraints$min_density) {
    return(list(feasible = FALSE, constraint_failed = "min_density"))
  }

  # LCC fraction
  lcc_capacity <- get_lcc_capacity(state, library, parcel_graph)
  lcc_fraction <- lcc_capacity / state$total_capacity
  if (lcc_fraction < constraints$min_lcc_fraction) {
    return(list(feasible = FALSE, constraint_failed = "min_lcc_fraction"))
  }

  # Secondary area constraint (5-acre minimum)
  # This is automatically satisfied because we only add blocks from L_sec
  # which are pre-filtered for >= 5 acres

  # Check LCC connectivity
  lcc_parcels <- state$lcc_parcels
  if (length(lcc_parcels) > 1) {
    lcc_subgraph <- igraph::induced_subgraph(parcel_graph, lcc_parcels)
    if (!igraph::is_connected(lcc_subgraph)) {
      return(list(feasible = FALSE, constraint_failed = "lcc_connectivity"))
    }
  }
  
  # Check min area and capacity near transit if specified in constraints
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

#' Filter blocks by LCC fraction constraint
#'
#' Pre-filters compatible blocks to exclude those that would definitely violate
#' min_lcc_fraction if added. Used by toggle kernel to avoid wasted proposals.
#'
#' When adding a secondary block:
#' - LCC capacity stays constant
#' - Total capacity increases by block capacity
#' - LCC fraction decreases: new_frac = lcc_cap / (total_cap + block_cap)
#'
#' We keep blocks where: lcc_cap >= (total_cap + block_cap) * min_lcc_fraction
#' Equivalently: block_cap <= lcc_cap / min_lcc_fraction - total_cap
#'
#' @param block_ids Integer vector of compatible block IDs
#' @param library Secondary library (with $metadata$capacity)
#' @param total_capacity Current total capacity
#' @param lcc_capacity Current LCC capacity
#' @param min_lcc_fraction Minimum LCC fraction constraint (default 0.5)
#' @return List with:
#'   - filtered_ids: block IDs that won't violate constraint
#'   - n_filtered: number of blocks removed
filter_blocks_by_lcc_fraction <- function(
  block_ids,
  library,
  total_capacity,
  lcc_capacity,
  min_lcc_fraction = 0.5
) {
  if (length(block_ids) == 0) {
    return(list(filtered_ids = integer(0), n_filtered = 0L))
  }

  # Maximum capacity we can add while keeping lcc_fraction >= min_lcc_fraction
  # lcc_cap / (total + added) >= min_frac
  # lcc_cap >= min_frac * (total + added)
  # lcc_cap / min_frac >= total + added
  # added <= lcc_cap / min_frac - total
  max_addable_cap <- lcc_capacity / min_lcc_fraction - total_capacity

  # If max_addable_cap <= 0, no blocks can be added without violating
  if (max_addable_cap <= 0) {
    return(list(filtered_ids = integer(0), n_filtered = length(block_ids)))
  }

  # Get block capacities
  block_caps <- library$metadata$capacity[block_ids]

  # Keep blocks with capacity <= max_addable_cap
  # (conservative: assumes block is fully added with no overlap)
  keep_mask <- block_caps <= max_addable_cap
  filtered_ids <- block_ids[keep_mask]

  list(
    filtered_ids = filtered_ids,
    n_filtered = sum(!keep_mask)
  )
}

# ============================================================================
# MULTI-MOVE HELPERS
# ============================================================================

#' Compute weights for LCC selection (uniform)
#'
#' Returns uniform weights for LCC selection. Previously used capacity-aware
#' weighting (slack^lambda), but this was removed because it created a trap:
#' when secondary_cap is high, all high-capacity LCCs get near-zero weight,
#' preventing the chain from escaping (low LCC cap, high secondary count) states.
#'
#' MH acceptance handles capacity preference through the capacity prior, so
#' uniform proposal weights are sufficient and avoid the trap.
#'
#' @param lcc_ids Integer vector of LCC candidate IDs
#' @param lcc_library LCC library with metadata$capacity
#' @return List with weights, log_weights, log_sum_w
compute_lcc_capacity_weights <- function(lcc_ids, lcc_library) {
  if (length(lcc_ids) == 0) {
    return(list(
      weights = numeric(0),
      log_weights = numeric(0),
      log_sum_w = -Inf,
      valid = FALSE,
      lcc_ids = integer(0)
    ))
  }

  caps <- lcc_library$metadata$capacity[lcc_ids]

  # Guard against non-finite capacities
  finite_mask <- is.finite(caps)
  if (!any(finite_mask)) {
    return(list(
      weights = numeric(0),
      log_weights = numeric(0),
      log_sum_w = -Inf,
      valid = FALSE,
      lcc_ids = integer(0)
    ))
  }

  # Filter to finite capacities only
  if (!all(finite_mask)) {
    lcc_ids <- lcc_ids[finite_mask]
  }

  n <- length(lcc_ids)

  # Uniform weights - all LCCs equally likely
  # This ensures the chain can propose any similar-capacity LCC without bias
  log_weights <- rep(0, n)
  log_sum_w <- log(n)
  weights <- rep(1 / n, n)

  list(
    weights = weights,
    log_weights = log_weights,
    log_sum_w = log_sum_w,
    valid = TRUE,
    lcc_ids = lcc_ids
  )
}

# ============================================================================
# KERNEL 1: LCC-LOCAL REFINEMENT
# ============================================================================

#' LCC-local toggle move
#'
#' Proposes adding or removing a single parcel from the LCC boundary.
#' Respects locked zone (does not modify secondaries).
#'
#' @param state Current state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints
#' @param neighbor_cache Optional named list of precomputed neighbors
#' @param neighbor_indices Optional named list of precomputed neighbor indices
#' @param degrees Optional named integer vector of vertex degrees (for fast boundary)
#' @param neighbor_idx Optional named list of integer neighbor indices (for incremental updates)
#' @param p_add Probability of add vs remove when both possible
#' @return List with new_state, accepted, move_type, details
lcc_local_move <- function(
  state,
  library,
  parcel_graph,
  constraints,
  neighbor_cache = NULL,
  neighbor_indices = NULL,
  degrees = NULL,
  neighbor_idx = NULL,
  p_add = 0.5
) {
  # Get LCC boundary candidates
  boundary <- get_lcc_boundary(
    state,
    library,
    parcel_graph,
    neighbor_cache,
    neighbor_indices,
    degrees
  )
  B_in <- boundary$B_in
  B_out <- boundary$B_out

  n_in <- length(B_in)
  n_out <- length(B_out)

  # Check if any moves possible
  if (n_in == 0 && n_out == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "lcc_local"
    ))
  }

  # Decide add vs remove
  if (n_in == 0) {
    action <- "add"
  } else if (n_out == 0) {
    action <- "remove"
  } else {
    action <- if (runif(1) < p_add) "add" else "remove"
  }

  # Select parcel
  # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
  if (action == "add") {
    selected <- B_out[sample.int(length(B_out), 1)]
    new_state <- update_lcc(
      state,
      selected,
      "add",
      library,
      parcel_graph,
      neighbor_idx
    )
    # Compute reverse move size
    new_boundary <- get_lcc_boundary(
      new_state,
      library,
      parcel_graph,
      neighbor_cache,
      neighbor_indices,
      degrees
    )
    n_in_new <- length(new_boundary$B_in)
    n_out_new <- length(new_boundary$B_out)
  } else {
    selected <- B_in[sample.int(length(B_in), 1)]
    # Check LCC would remain connected
    remaining_lcc <- setdiff(state$lcc_parcels, selected)
    if (length(remaining_lcc) > 0) {
      sub <- igraph::induced_subgraph(parcel_graph, remaining_lcc)
      if (!igraph::is_connected(sub)) {
        return(list(
          new_state = state,
          accepted = FALSE,
          infeasible = TRUE,
          constraint_failed = "lcc_connectivity",
          move_type = "lcc_local_remove"
        ))
      }
    }
    new_state <- update_lcc(
      state,
      selected,
      "remove",
      library,
      parcel_graph,
      neighbor_idx
    )
    new_boundary <- get_lcc_boundary(
      new_state,
      library,
      parcel_graph,
      neighbor_cache,
      neighbor_indices,
      degrees
    )
    n_in_new <- length(new_boundary$B_in)
    n_out_new <- length(new_boundary$B_out)
  }

  # Check feasibility (hard constraints only - capacity handled by prior)
  feasibility <- check_hard_constraints_only(
    new_state,
    library,
    parcel_graph,
    constraints
  )
  if (!feasibility$feasible) {
    return(list(
      new_state = state,
      accepted = FALSE,
      infeasible = TRUE,
      constraint_failed = feasibility$constraint_failed,
      move_type = paste0("lcc_local_", action)
    ))
  }

  # Compute MH ratio (balanced boundary move)
  # For uniform target: α = min(1, q(X'→X) / q(X→X'))
  #
  # ADD move:
  #   Forward q(X→X') = 1/n_out (pick from outer boundary)
  #   Reverse q(X'→X) = 1/n_in_new (pick from inner boundary of new state)
  #   Ratio = n_out / n_in_new
  #
  # REMOVE move:
  #   Forward q(X→X') = 1/n_in (pick from inner boundary)
  #   Reverse q(X'→X) = 1/n_out_new (pick from outer boundary of new state)
  #   Ratio = n_in / n_out_new

  if (action == "add") {
    log_accept_ratio <- log(n_out) - log(n_in_new)
  } else {
    log_accept_ratio <- log(n_in) - log(n_out_new)
  }

  # Account for p_add asymmetry when direction is forced in either state
  #
  # Full proposal probability analysis:
  # - Current has both options: forward uses p_add (add) or 1-p_add (remove)
  # - Current forced add (n_in=0): forward uses 1
  # - Current forced remove (n_out=0): forward uses 1
  # - Same logic applies to reverse from new state
  #
  # Six valid transition cases:
  # 1. Both → Both: p_add cancels, no correction

  # 2. Both → Forced add: forward used p_add, reverse uses 1 → divide by p_add
  # 3. Both → Forced remove: forward used 1-p_add, reverse uses 1 → divide by 1-p_add
  # 4. Forced add → Both: forward used 1, reverse uses 1-p_add → multiply by 1-p_add
  # 5. Forced remove → Both: forward used 1, reverse uses p_add → multiply by p_add
  # 6. Forced → Forced (same dir): both use 1, no correction

  current_has_both <- (n_in > 0) && (n_out > 0)
  new_has_both <- (n_in_new > 0) && (n_out_new > 0)

  if (current_has_both && new_has_both) {
    # Case 1: Both states have choice - p_add cancels, no correction needed
  } else if (current_has_both && !new_has_both) {
    # Cases 2-3: Current has choice, new is forced
    if (action == "add" && n_out_new == 0) {
      # Case 2: Forward used p_add, reverse uses 1 → divide by p_add
      log_accept_ratio <- log_accept_ratio - log(p_add)
    } else if (action == "remove" && n_in_new == 0) {
      # Case 3: Forward used (1-p_add), reverse uses 1 → divide by (1-p_add)
      log_accept_ratio <- log_accept_ratio - log(1 - p_add)
    }
  } else if (!current_has_both && new_has_both) {
    # Cases 4-5: Current is forced, new has choice
    if (action == "add") {
      # Case 4: Forward used 1, reverse uses (1-p_add) → multiply by (1-p_add)
      log_accept_ratio <- log_accept_ratio + log(1 - p_add)
    } else {
      # Case 5: Forward used 1, reverse uses p_add → multiply by p_add
      log_accept_ratio <- log_accept_ratio + log(p_add)
    }
  }
  # Case 6: Both forced (same direction) - both use 1, no correction needed

  # Add penalty difference (capacity prior)
  penalty_diff <- compute_penalty_difference(
    state$total_capacity,
    new_state$total_capacity,
    constraints
  )
  log_accept_ratio <- log_accept_ratio + penalty_diff

  accept_prob <- min(1, exp(log_accept_ratio))
  accepted <- runif(1) < accept_prob

  list(
    new_state = if (accepted) new_state else state,
    accepted = accepted,
    proposal_failed = FALSE,
    infeasible = FALSE,
    move_type = paste0("lcc_local_", action),
    selected_parcel = selected,
    accept_prob = accept_prob
  )
}

# ============================================================================
# KERNEL 2: SYMMETRIC BIRTH/DEATH (UNIFIED)
# ============================================================================

#' Symmetric Birth/Death Move
#'
#' Proposes birth or death by sampling uniformly from universe U = addable U removable.
#'
#' Proposal q(y|x):
#' 1. Build U = {addable blocks} U {removable blocks}
#' 2. Sample block B uniformly from U
#' 3. If B is addable, propose birth; if removable, propose death
#'
#' MH Ratio:
#'   alpha = (pi(y)/pi(x)) * (q(x|y)/q(y|x))
#'   q(x|y)/q(y|x) = |U_old| / |U_new| = Z_old / Z_new
#'
#' @param state Current state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints
#' @param neighbor_idx Precomputed neighbor indices
#' @return List with new_state, accepted, direction, details
symmetric_birth_death_move <- function(
  state,
  library,
  parcel_graph,
  constraints,
  neighbor_idx = NULL
) {
  # Step 1: Identify Universe
  # Addable: blocks that can be added (geometrically valid)
  addable_all <- get_addable_blocks_unconstrained(state, library)

  # Filter to blocks with finite capacity
  block_caps <- library$metadata$capacity[addable_all]
  addable <- addable_all[is.finite(block_caps)]

  # LCC fraction filter
  if (length(addable) > 0) {
    lcc_capacity <- get_lcc_capacity(state, library, parcel_graph)
    lcc_filter <- filter_blocks_by_lcc_fraction(
      addable,
      library,
      state$total_capacity,
      lcc_capacity,
      constraints$min_lcc_fraction
    )
    addable <- lcc_filter$filtered_ids
  }

  removable <- get_removable_blocks(state)

  n_add <- length(addable)
  n_rem <- length(removable)

  # Total universe size
  Z_old <- n_add + n_rem

  if (Z_old == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "symmetric_birth_death",
      reason = "empty_universe"
    ))
  }

  # Step 2: Sample block uniformly from universe
  is_birth <- runif(1) < (n_add / Z_old)

  if (is_birth) {
    block_id <- addable[sample.int(n_add, 1)]
    direction <- "birth"
    proposed_state <- add_secondary_block(
      state,
      block_id,
      library,
      parcel_graph,
      neighbor_idx
    )
  } else {
    # Sample from removable (weighted uniform, so just sample uniform)
    sampled_idx <- sample.int(n_rem, 1)
    block_id <- removable[sampled_idx]
    direction <- "death"

    proposed_state <- remove_secondary_block(
      state,
      block_id,
      library,
      parcel_graph,
      neighbor_idx
    )
  }

  # Step 3: Check Feasibility (hard constraints only - capacity handled by prior)
  feasibility <- check_hard_constraints_only(
    proposed_state,
    library,
    parcel_graph,
    constraints
  )

  if (!feasibility$feasible) {
    return(list(
      new_state = state,
      accepted = FALSE,
      infeasible = TRUE,
      constraint_failed = feasibility$constraint_failed,
      move_type = "symmetric_birth_death",
      direction = direction
    ))
  }

  # Step 4: Compute Reverse Normalization Constant (Z_new)
  # We need to know n_add_new and n_rem_new for the proposed state

  # For n_rem_new:
  removable_new <- get_removable_blocks(proposed_state)
  n_rem_new <- length(removable_new)

  # For n_add_new:
  addable_rev_all <- get_addable_blocks_unconstrained(proposed_state, library)
  block_caps_rev <- library$metadata$capacity[addable_rev_all]
  addable_rev <- addable_rev_all[is.finite(block_caps_rev)]

  if (length(addable_rev) > 0) {
    lcc_capacity_rev <- get_lcc_capacity(proposed_state, library, parcel_graph)
    lcc_filter_rev <- filter_blocks_by_lcc_fraction(
      addable_rev,
      library,
      proposed_state$total_capacity,
      lcc_capacity_rev,
      constraints$min_lcc_fraction
    )
    addable_rev <- lcc_filter_rev$filtered_ids
  }
  n_add_new <- length(addable_rev)

  # Universe size in proposed state
  Z_new <- n_add_new + n_rem_new

  # Step 5: MH Ratio
  # q(y|x) = 1 / Z_old (uniform sampling)
  # q(x|y) = 1 / Z_new
  # ratio = Z_old / Z_new

  log_q_ratio <- log(Z_old) - log(Z_new)

  # Capacity prior penalty difference
  log_pi_ratio <- compute_penalty_difference(
    state$total_capacity,
    proposed_state$total_capacity,
    constraints
  )

  # K prior penalty difference (linear: penalizes birth, favors death)
  # Birth: k_proposed = k+1, so (k - (k+1)) = -1, log_k_ratio = -lambda
  # Death: k_proposed = k-1, so (k - (k-1)) = +1, log_k_ratio = +lambda
  k_current <- length(state$secondary_blocks)
  k_proposed <- length(proposed_state$secondary_blocks)
  log_k_ratio <- K_PRIOR_LAMBDA * (k_current - k_proposed)

  log_accept <- log_pi_ratio + log_k_ratio + log_q_ratio
  accept <- log(runif(1)) < log_accept

  if (accept) {
    return(list(
      new_state = proposed_state,
      accepted = TRUE,
      proposal_failed = FALSE,
      move_type = "symmetric_birth_death",
      direction = direction,
      n_universe = n_add + n_rem
    ))
  } else {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = FALSE,
      move_type = "symmetric_birth_death",
      direction = direction
    ))
  }
}

# ============================================================================
# KERNEL 2b: LIFTED BIRTH/DEATH (NON-REVERSIBLE MCMC)
# ============================================================================

#' Lifted Birth/Death Move with Momentum
#'
#' Non-reversible MCMC kernel that maintains direction across moves.
#' Unlike symmetric_birth_death which randomly chooses birth or death,
#' this kernel proposes moves in a persistent direction:
#' - On acceptance: keep direction (momentum continues)
#' - On rejection: flip direction (bounce back)
#'
#' This creates "runs" of same-direction moves that traverse k faster,
#' improving mixing in the secondary block count dimension.
#'
#' Satisfies global balance (not detailed balance) which is sufficient
#' for correct MCMC sampling. The marginal distribution over states
#' is the target distribution π(x).
#'
#' @param state Current parcel state
#' @param direction Current direction: "birth" or "death"
#' @param library Secondary library
#' @param parcel_graph Parcel graph
#' @param constraints Constraints list
#' @param neighbor_idx Neighbor index cache
#' @return List with new_state, accepted, new_direction, move_type, details
lifted_birth_death_move <- function(
    state,
    direction,
    library,
    parcel_graph,
    constraints,
    neighbor_idx = NULL
) {
  k <- length(state$secondary_blocks)

  # Get candidate pools (same logic as symmetric_birth_death)
  addable <- get_addable_blocks_unconstrained(state, library)
  addable <- addable[is.finite(library$metadata$capacity[addable])]

  if (length(addable) > 0) {
    lcc_capacity <- get_lcc_capacity(state, library, parcel_graph)
    lcc_filter <- filter_blocks_by_lcc_fraction(
      addable, library, state$total_capacity, lcc_capacity,
      constraints$min_lcc_fraction
    )
    addable <- lcc_filter$filtered_ids
  }

  removable <- get_removable_blocks(state)
  n_add <- length(addable)
  n_rem <- length(removable)

  # Handle boundary cases - forced direction flip
  if (direction == "birth" && n_add == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      new_direction = "death",
      move_type = "lifted_birth_death",
      direction = direction,
      forced_flip = TRUE,
      proposal_failed = TRUE
    ))
  }

  if (direction == "death" && n_rem == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      new_direction = "birth",
      move_type = "lifted_birth_death",
      direction = direction,
      forced_flip = TRUE,
      proposal_failed = TRUE
    ))
  }

  # Propose move in current direction only
  if (direction == "birth") {
    block_id <- addable[sample.int(n_add, 1)]
    proposed_state <- add_secondary_block(
      state, block_id, library, parcel_graph, neighbor_idx
    )

    # Compute reverse proposal size
    removable_new <- get_removable_blocks(proposed_state)
    n_rem_new <- length(removable_new)

    # Guard: if no removable blocks in proposed state, reverse is impossible
    # (Should not happen since we just added a block, but guard for robustness)
    if (n_rem_new == 0) {
      return(list(
        new_state = state,
        accepted = FALSE,
        new_direction = "death", # Flip to death
        move_type = "lifted_birth_death",
        direction = direction,
        no_reverse = TRUE,
        proposal_failed = FALSE
      ))
    }

    # Proposal ratio for lifted kernel
    # Forward: select from n_add addable blocks
    # Reverse: select from n_rem_new removable blocks
    log_q_ratio <- log(n_add) - log(n_rem_new)

  } else { # death
    block_id <- removable[sample.int(n_rem, 1)]
    proposed_state <- remove_secondary_block(
      state, block_id, library, parcel_graph, neighbor_idx
    )

    # Compute reverse proposal size
    addable_new <- get_addable_blocks_unconstrained(proposed_state, library)
    addable_new <- addable_new[is.finite(library$metadata$capacity[addable_new])]
    if (length(addable_new) > 0) {
      lcc_cap_new <- get_lcc_capacity(proposed_state, library, parcel_graph)
      lcc_filter_new <- filter_blocks_by_lcc_fraction(
        addable_new, library, proposed_state$total_capacity, lcc_cap_new,
        constraints$min_lcc_fraction
      )
      addable_new <- lcc_filter_new$filtered_ids
    }
    n_add_new <- length(addable_new)

    # Guard: if no addable blocks in proposed state, reverse is impossible
    # Reject and flip direction to maintain skew detailed balance
    if (n_add_new == 0) {
      return(list(
        new_state = state,
        accepted = FALSE,
        new_direction = "birth", # Flip to birth
        move_type = "lifted_birth_death",
        direction = direction,
        no_reverse = TRUE,
        proposal_failed = FALSE
      ))
    }

    # Proposal ratio for lifted kernel
    # Forward: select from n_rem removable blocks
    # Reverse: select from n_add_new addable blocks
    log_q_ratio <- log(n_rem) - log(n_add_new)
  }

  # Check feasibility (hard constraints only - capacity handled by prior)
  feasibility <- check_hard_constraints_only(
    proposed_state, library, parcel_graph, constraints
  )

  if (!feasibility$feasible) {
    # Infeasible -> reject -> flip direction
    return(list(
      new_state = state,
      accepted = FALSE,
      new_direction = if (direction == "birth") "death" else "birth",
      move_type = "lifted_birth_death",
      direction = direction,
      constraint_failed = feasibility$constraint_failed,
      infeasible = TRUE,
      proposal_failed = FALSE
    ))
  }

  # Compute MH acceptance
  # Capacity prior penalty difference
  log_cap_ratio <- compute_penalty_difference(
    state$total_capacity, proposed_state$total_capacity, constraints
  )

  # K prior penalty difference
  k_new <- length(proposed_state$secondary_blocks)
  log_k_ratio <- K_PRIOR_LAMBDA * (k - k_new)

  log_alpha <- log_k_ratio + log_cap_ratio + log_q_ratio
  accept <- log(runif(1)) < log_alpha

  if (accept) {
    # Accept -> keep direction (momentum)
    return(list(
      new_state = proposed_state,
      accepted = TRUE,
      new_direction = direction, # KEEP DIRECTION
      move_type = "lifted_birth_death",
      direction = direction,
      block_id = block_id,
      accept_prob = min(1, exp(log_alpha)),
      proposal_failed = FALSE,
      infeasible = FALSE
    ))
  } else {
    # Reject -> flip direction (bounce)
    return(list(
      new_state = state,
      accepted = FALSE,
      new_direction = if (direction == "birth") "death" else "birth", # FLIP
      move_type = "lifted_birth_death",
      direction = direction,
      accept_prob = min(1, exp(log_alpha)),
      proposal_failed = FALSE,
      infeasible = FALSE
    ))
  }
}

# ============================================================================
# KERNEL 3: SECONDARY CAPACITY-BALANCED SWAP
# ============================================================================

#' Secondary capacity-balanced swap move
#'
#' Proposes swapping an existing secondary with another block of similar capacity.
#' This enables geographic mixing while keeping total capacity roughly constant.
#'
#' Unlike the neighbor-based replace move, this samples from ALL addable blocks
#' with similar capacity, enabling longer-range geographic jumps.
#'
#' Algorithm:
#' 1. Select a secondary block B_old uniformly from current secondaries
#' 2. Compute state without B_old
#' 3. Get all addable blocks (disjoint + non-adjacent to remaining state)
#' 4. Filter to similar capacity: |cap - cap_old| <= tolerance
#' 5. Select B_new uniformly from similar-capacity set
#' 6. Compute reverse: similar-capacity addable blocks when removing B_new
#' 7. MH ratio = |S_rev| / |S_fwd|
#'
#' @param state Current state
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints
#' @param cap_tolerance Capacity tolerance for similar-capacity filtering (default 50)
#' @return List with new_state, accepted, move_type, details
secondary_swap_move <- function(
  state,
  library,
  parcel_graph,
  constraints,
  cap_tolerance = 50,
  neighbor_idx = NULL
) {
  # Step 1: Get removable secondaries
  removable <- get_removable_blocks(state)
  k <- length(removable)

  if (k == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "secondary_swap",
      reason = "no_secondaries"
    ))
  }

  # Step 2: Select one to remove uniformly
  # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
  old_id <- removable[sample.int(length(removable), 1)]
  old_cap <- library$metadata$capacity[old_id]

  # Step 3: Compute state without old block
  state_without_old <- remove_secondary_block(
    state,
    old_id,
    library,
    parcel_graph,
    neighbor_idx
  )

  # Step 4: Get addable blocks (unconstrained - geometry only)
  addable <- get_addable_blocks_unconstrained(state_without_old, library)

  if (length(addable) == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "secondary_swap",
      reason = "no_addable"
    ))
  }

  # Step 5: Filter to similar capacity (±tolerance)
  addable_caps <- library$metadata$capacity[addable]
  similar_mask <- abs(addable_caps - old_cap) <= cap_tolerance
  similar_ids <- addable[similar_mask]
  n_similar_fwd <- length(similar_ids)

  if (n_similar_fwd == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "secondary_swap",
      reason = "no_similar_capacity"
    ))
  }

  # Step 6: Select replacement uniformly from similar-capacity set
  # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
  new_id <- similar_ids[sample.int(length(similar_ids), 1)]
  new_cap <- library$metadata$capacity[new_id]

  # Step 7: Build proposed state
  proposed_state <- add_secondary_block(
    state_without_old,
    new_id,
    library,
    parcel_graph,
    neighbor_idx
  )

  # Step 8: Check feasibility (hard constraints only - capacity handled by prior)
  feasibility <- check_hard_constraints_only(
    proposed_state,
    library,
    parcel_graph,
    constraints
  )
  if (!feasibility$feasible) {
    return(list(
      new_state = state,
      accepted = FALSE,
      infeasible = TRUE,
      constraint_failed = feasibility$constraint_failed,
      move_type = "secondary_swap",
      old_id = old_id,
      new_id = new_id,
      delta_cap = new_cap - old_cap
    ))
  }

  # Step 9: Compute reverse proposal (similar-capacity blocks when removing new_id)
  state_without_new <- remove_secondary_block(
    proposed_state,
    new_id,
    library,
    parcel_graph,
    neighbor_idx
  )
  addable_rev <- get_addable_blocks_unconstrained(state_without_new, library)
  addable_rev_caps <- library$metadata$capacity[addable_rev]
  similar_rev_mask <- abs(addable_rev_caps - new_cap) <= cap_tolerance
  n_similar_rev <- sum(similar_rev_mask)

  if (n_similar_rev == 0) {
    # Reverse move impossible - shouldn't happen but guard against it
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "secondary_swap",
      reason = "no_reverse_similar"
    ))
  }

  # Step 10: MH ratio = |S_rev| / |S_fwd| + penalty_diff
  # q(X → X') = 1/k × 1/|S_fwd|
  # q(X' → X) = 1/k × 1/|S_rev|
  # Note: 1/k terms cancel since k is unchanged
  log_accept <- log(n_similar_rev) - log(n_similar_fwd)
  # Add penalty difference (capacity prior)
  penalty_diff <- compute_penalty_difference(
    state$total_capacity,
    proposed_state$total_capacity,
    constraints
  )
  log_accept <- log_accept + penalty_diff
  accept_prob <- min(1, exp(log_accept))

  accepted <- runif(1) < accept_prob

  list(
    new_state = if (accepted) proposed_state else state,
    accepted = accepted,
    proposal_failed = FALSE,
    infeasible = FALSE,
    move_type = "secondary_swap",
    old_id = old_id,
    new_id = new_id,
    old_cap = old_cap,
    new_cap = new_cap,
    delta_cap = new_cap - old_cap,
    n_similar_fwd = n_similar_fwd,
    n_similar_rev = n_similar_rev,
    accept_prob = accept_prob
  )
}

# ============================================================================
# KERNEL 4: REPLACE-LCC (GLOBAL RELOCATION) - HELPER FUNCTIONS
# ============================================================================

#' Check if an LCC is compatible with specific secondary blocks
#'
#' Returns TRUE if ALL given secondary blocks would be addable to this LCC.
#' Used for prefiltering LCC candidates in replace-LCC moves.
#' Optimized to check only the specific blocks rather than scanning the full library.
#'
#' @param lcc_indices Integer indices of LCC parcels
#' @param lcc_nbr_indices Integer indices of LCC neighbor parcels
#' @param secondary_ids Integer vector of secondary block IDs to check
#' @param secondary_library Secondary library
#' @return Logical: TRUE if all secondaries are compatible
is_lcc_compatible_with_secondaries <- function(
  lcc_indices,
  lcc_nbr_indices,
  secondary_ids,
  secondary_library
) {
  if (length(secondary_ids) == 0) {
    return(TRUE)
  }

  # Check ONLY the specific secondary blocks (O(k) instead of O(n_library))
  for (id in secondary_ids) {
    block_indices <- secondary_library$blocks[[id]]
    block_nbr_indices <- secondary_library$neighbor_indices[[id]]

    # Check disjoint with LCC
    if (any(block_indices %in% lcc_indices)) {
      return(FALSE)
    }
    # Check block neighbors don't overlap LCC
    if (any(block_nbr_indices %in% lcc_indices)) {
      return(FALSE)
    }
    # Check LCC neighbors don't overlap block
    if (any(lcc_nbr_indices %in% block_indices)) return(FALSE)
  }

  TRUE
}

#' Filter LCC IDs compatible with current secondaries
#'
#' Uses union indices when available for fast O(1) checks per candidate.
#'
#' @param lcc_ids Integer vector of LCC block IDs
#' @param lcc_library LCC library
#' @param secondary_library Secondary library
#' @param secondary_ids Integer vector of secondary block IDs
#' @param secondary_union_indices Integer indices of all secondary parcels (optional)
#' @param secondary_neighbor_indices Integer indices of all secondary neighbors (optional)
#' @return Filtered lcc_ids
#' @keywords internal
filter_compatible_lccs <- function(
  lcc_ids,
  lcc_library,
  secondary_library,
  secondary_ids,
  secondary_union_indices = NULL,
  secondary_neighbor_indices = NULL
) {
  if (length(lcc_ids) == 0) {
    return(integer(0))
  }
  if (length(secondary_ids) == 0) {
    return(lcc_ids)
  }

  use_union <- length(secondary_union_indices) > 0 ||
    length(secondary_neighbor_indices) > 0

  if (use_union) {
    compatible_mask <- vapply(
      lcc_ids,
      function(lcc_id) {
        lcc_indices <- lcc_library$blocks[[lcc_id]]
        lcc_nbr_indices <- lcc_library$neighbor_indices[[lcc_id]]
        if (is.null(lcc_indices) || is.null(lcc_nbr_indices)) {
          return(FALSE)
        }
        if (length(lcc_indices) == 1 && is.na(lcc_indices)) {
          return(FALSE)
        }
        if (length(lcc_nbr_indices) == 1 && is.na(lcc_nbr_indices)) {
          return(FALSE)
        }

        # Check disjoint with all secondaries
        if (any(lcc_indices %in% secondary_union_indices)) {
          return(FALSE)
        }
        # Check secondary neighbors don't overlap LCC
        if (any(secondary_neighbor_indices %in% lcc_indices)) {
          return(FALSE)
        }
        # Check LCC neighbors don't overlap secondaries
        if (any(lcc_nbr_indices %in% secondary_union_indices)) {
          return(FALSE)
        }
        TRUE
      },
      logical(1)
    )
  } else {
    compatible_mask <- vapply(
      lcc_ids,
      function(lcc_id) {
        lcc_indices <- lcc_library$blocks[[lcc_id]]
        lcc_nbr_indices <- lcc_library$neighbor_indices[[lcc_id]]
        if (is.null(lcc_indices) || is.null(lcc_nbr_indices)) {
          return(FALSE)
        }
        if (length(lcc_indices) == 1 && is.na(lcc_indices)) {
          return(FALSE)
        }
        if (length(lcc_nbr_indices) == 1 && is.na(lcc_nbr_indices)) {
          return(FALSE)
        }
        is_lcc_compatible_with_secondaries(
          lcc_indices,
          lcc_nbr_indices,
          secondary_ids,
          secondary_library
        )
      },
      logical(1)
    )
  }

  lcc_ids[compatible_mask]
}

#' Check if current LCC is representable in the LCC library
#'
#' For detailed balance, the reverse move must be able to propose the current state.
#' This checks if the current LCC exactly matches some block in L_LCC.
#' Uses hash-based lookup for O(1) performance when available.
#'
#' @param lcc_parcels Character vector of current LCC parcel IDs
#' @param lcc_library LCC candidate library
#' @return Integer: block ID if found, NA if not in library
find_lcc_in_library <- function(lcc_parcels, lcc_library) {
  if (lcc_library$n_blocks == 0) {
    return(NA_integer_)
  }

  lcc_indices <- parcel_ids_to_indices(lcc_parcels, lcc_library$parcel_names)
  if (length(lcc_indices) == 0) {
    return(NA_integer_)
  }
  key <- digest::digest(sort(lcc_indices), algo = "xxhash64")

  # OPTIMIZED: Extract source column once for vectorized access
  has_source <- !is.null(lcc_library$metadata$source)
  source_vec <- if (has_source) lcc_library$metadata$source else NULL

  # Fast path: hash lookup (O(1))
  if (
    !is.null(lcc_library$block_hashes) &&
      exists(key, envir = lcc_library$block_hashes, inherits = FALSE)
  ) {
    block_id <- get(key, envir = lcc_library$block_hashes, inherits = FALSE)
    # Verify not evicted (for online enrichment with FIFO eviction)
    if (
      has_source &&
        !is.na(source_vec[block_id]) &&
        source_vec[block_id] == "evicted"
    ) {
      return(NA_integer_)
    }
    return(block_id)
  }

  # Fallback: linear scan (for libraries without hash index)
  # OPTIMIZED: Use pre-extracted source_vec instead of row-by-row data.table access
  lcc_set <- sort(lcc_indices)
  for (i in seq_len(lcc_library$n_blocks)) {
    # Skip removed/evicted entries (NULL or NA sentinel)
    block_i <- lcc_library$blocks[[i]]
    if (is.null(block_i) || (length(block_i) == 1 && is.na(block_i))) {
      next
    }
    # Skip evicted entries - vectorized access
    if (has_source && !is.na(source_vec[i]) && source_vec[i] == "evicted") {
      next
    }
    lib_set <- sort(block_i)
    if (length(lcc_set) == length(lib_set) && all(lcc_set == lib_set)) {
      return(i)
    }
  }

  NA_integer_
}

# ============================================================================
# KERNEL 4: REPLACE-LCC (GLOBAL RELOCATION)
# ============================================================================

#' Replace-LCC move with retention + capacity-matched sampling
#'
#' Proposes jumping to a new LCC while keeping ALL current secondaries.
#' The new LCC is sampled from a capacity-similar set to improve acceptance.
#'
#' This approach solves the k-mismatch problem: birth/death creates states
#' with k≈5, but resampling proposals generate k≈1-2. By retaining secondaries,
#' k stays constant and MH acceptance improves dramatically.
#'
#' Proposal:
#' 1. Find LCCs with capacity within ±cap_tolerance of current
#' 2. Sample new LCC with capacity-aware weights (prefer smaller LCCs near max_cap)
#' 3. Check all current secondaries are compatible with new LCC
#' 4. Keep all secondaries (no resampling)
#'
#' MH ratio corrects for weighted proposal asymmetry:
#'   log_q_ratio = log(w_old) - log(w_new) + log(Z_forward) - log(Z_reverse)
#'
#' @param state Current state
#' @param lcc_library LCC candidate library
#' @param secondary_library Secondary library
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints
#' @param cap_tolerance Capacity tolerance for similar-LCC sampling (default 4000).
#'   Wide tolerance allows escaping from high-cap to low-cap modes.
#' @param neighbor_idx List of integer neighbor indices (for incremental tracking)
#' @param parcel_names Character vector of all parcel names (for incremental tracking)
#' @return List with new_state, accepted, move_type, details
replace_lcc_move <- function(
  state,
  lcc_library,
  secondary_library,
  parcel_graph,
  constraints,
  cap_tolerance = 4000,
  neighbor_idx = NULL,
  parcel_names = NULL
) {
  n_candidates <- lcc_library$n_blocks

  if (n_candidates == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "no_candidates"
    ))
  }

  # Step 1: Check if current LCC is in library (required for reversibility)
  current_lcc_parcels <- state$lcc_parcels
  current_secondary_ids <- state$secondary_blocks
  k_current <- length(current_secondary_ids)

  current_lcc_id <- find_lcc_in_library(current_lcc_parcels, lcc_library)

  if (is.na(current_lcc_id)) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "current_lcc_not_in_library"
    ))
  }

  # Step 2: Find capacity-similar LCCs (excluding evicted/NULL entries and self)
  current_lcc_cap <- lcc_library$metadata$capacity[current_lcc_id]
  all_caps <- lcc_library$metadata$capacity

  # Compute secondary capacity for min_lcc_fraction pre-filter
  secondary_cap <- state$total_capacity - current_lcc_cap

  # Get current LCC station metrics (for station pre-filter)
  current_lcc_station_area <- lcc_library$metadata$area_in_station[current_lcc_id]
  current_lcc_station_cap <- lcc_library$metadata$capacity_in_station[current_lcc_id]

  # Compute secondary station contributions (fixed for this move)
  # Use NA fallback for legacy libraries without station metrics
  secondary_area_in_station <- if (!is.null(current_lcc_station_area) && !is.na(current_lcc_station_area)) {
    state$total_area_in_station - current_lcc_station_area
  } else {
    NA_real_
  }
  secondary_cap_in_station <- if (!is.null(current_lcc_station_cap) && !is.na(current_lcc_station_cap)) {
    state$total_capacity_in_station - current_lcc_station_cap
  } else {
    NA_real_
  }

  # Build mask for active (non-evicted, non-NULL/NA) entries
  n_blocks <- lcc_library$n_blocks
  active_mask <- lcc_library$active_mask
  if (is.null(active_mask) || length(active_mask) != n_blocks) {
    active_mask <- compute_library_active_mask(lcc_library)
  }

  # OPTIMIZATION: Use cached compatible LCCs if available, else compute
  # The cache is invalidated when secondaries change (add/remove_secondary_block)
  # This gives ~1700x speedup for consecutive replace_lcc moves
  all_active_ids <- which(active_mask)
  if (!is.null(state$compatible_lccs_cache)) {
    # Use cached compatible set (very fast)
    all_compatible_lccs <- state$compatible_lccs_cache
  } else if (k_current > 0) {
    # Compute fresh - this is the expensive operation (~900ms)
    all_compatible_lccs <- filter_compatible_lccs(
      all_active_ids,
      lcc_library,
      secondary_library,
      current_secondary_ids,
      secondary_union_indices = state$secondary_union_indices,
      secondary_neighbor_indices = state$secondary_neighbor_indices
    )
  } else {
    all_compatible_lccs <- all_active_ids
  }

  # Find similar-capacity LCCs among active entries, excluding current
  similar_mask <- active_mask &
    (abs(all_caps - current_lcc_cap) <= cap_tolerance)
  similar_mask[current_lcc_id] <- FALSE # Exclude self to avoid no-op

  # Pre-filter by min_lcc_fraction constraint:
  # C_lcc / (C_lcc + C_sec) >= θ  =>  C_lcc >= (θ/(1-θ)) * C_sec
  # This removes most infeasible proposals before expensive compatibility checks
  theta <- constraints$min_lcc_fraction
  if (theta > 0 && theta < 1 && secondary_cap > 0) {
    min_lcc_cap_threshold <- (theta / (1 - theta)) * secondary_cap
    similar_mask <- similar_mask & (all_caps >= min_lcc_cap_threshold)
  }

  # Pre-filter by station_area_pct constraint (vectorized)
  # Only filter if station metrics are available in library and constraint is active
  lcc_areas_in_station <- lcc_library$metadata$area_in_station
  if (!is.null(constraints$station_area_pct) && !is.na(constraints$station_area_pct) &&
      !is.null(lcc_areas_in_station) && !is.na(secondary_area_in_station)) {
    required_station_area <- (constraints$station_area_pct / 100) * constraints$min_area
    station_area_ok <- is.na(lcc_areas_in_station) |
      (secondary_area_in_station + lcc_areas_in_station >= required_station_area)
    similar_mask <- similar_mask & station_area_ok
  }

  # Pre-filter by station_capacity_pct constraint (vectorized)
  lcc_caps_in_station <- lcc_library$metadata$capacity_in_station
  if (!is.null(constraints$station_capacity_pct) && !is.na(constraints$station_capacity_pct) &&
      !is.null(lcc_caps_in_station) && !is.na(secondary_cap_in_station)) {
    required_station_cap <- (constraints$station_capacity_pct / 100) * constraints$min_capacity
    station_cap_ok <- is.na(lcc_caps_in_station) |
      (secondary_cap_in_station + lcc_caps_in_station >= required_station_cap)
    similar_mask <- similar_mask & station_cap_ok
  }

  similar_ids <- which(similar_mask)

  if (length(similar_ids) == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "no_similar_capacity_lcc"
    ))
  }

  # Step 2b: Intersect with pre-computed compatible set (fast O(n) set intersection)
  # Replaces expensive per-element filter_compatible_lccs call
  similar_ids <- intersect(similar_ids, all_compatible_lccs)

  n_similar <- length(similar_ids)

  if (n_similar == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "no_compatible_lcc"
    ))
  }

  # Step 3: Compute weights and sample new LCC
  # (secondary_cap already computed above for min_lcc_fraction pre-filter)

  # Filter to LCCs with finite capacity
  lcc_caps <- all_caps[similar_ids]
  finite_mask <- is.finite(lcc_caps)
  similar_ids <- similar_ids[finite_mask]
  n_similar <- length(similar_ids)

  if (n_similar == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "no_valid_lcc"
    ))
  }

  # Uniform weights for LCC selection (capacity preference handled by prior)
  fwd_weights <- compute_lcc_capacity_weights(similar_ids, lcc_library)

  # Guard against invalid weights
  if (!fwd_weights$valid) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "invalid_forward_weights"
    ))
  }

  # Use potentially filtered lcc_ids from weight computation
  similar_ids <- fwd_weights$lcc_ids
  n_similar <- length(similar_ids)

  # Sample uniformly
  idx <- sample.int(n_similar, 1, prob = fwd_weights$weights)
  new_lcc_id <- similar_ids[idx]
  log_w_new <- fwd_weights$log_weights[idx]
  log_Z_forward <- fwd_weights$log_sum_w

  new_lcc_parcels <- library_block_parcels(lcc_library, new_lcc_id)
  new_lcc_cap <- all_caps[new_lcc_id]

  # Defensive check: verify selected LCC is valid

  if (is.null(new_lcc_parcels) || length(new_lcc_parcels) == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "selected_lcc_invalid"
    ))
  }

  # Step 4: Build new state (keep ALL secondaries)
  # Note: Compatibility already verified by prefiltering similar_ids above
  new_state <- reset_to_lcc(
    new_lcc_parcels,
    secondary_library,
    parcel_graph,
    neighbor_idx,
    parcel_names
  )
  for (bid in current_secondary_ids) {
    new_state <- add_secondary_block(
      new_state,
      bid,
      secondary_library,
      parcel_graph,
      neighbor_idx = neighbor_idx
    )
  }

  # Step 5: Check feasibility (hard constraints only - capacity handled by prior)
  feasibility <- check_hard_constraints_only(
    new_state,
    secondary_library,
    parcel_graph,
    constraints
  )
  if (!feasibility$feasible) {
    return(list(
      new_state = state,
      accepted = FALSE,
      infeasible = TRUE,
      move_type = "replace_lcc",
      constraint_failed = feasibility$constraint_failed
    ))
  }

  # Step 6: Compute MH ratio
  similar_reverse_mask <- active_mask &
    (abs(all_caps - new_lcc_cap) <= cap_tolerance)
  similar_reverse_mask[new_lcc_id] <- FALSE # Exclude self for reverse

  # Apply same min_lcc_fraction pre-filter as forward (same secondaries, same threshold)
  if (theta > 0 && theta < 1 && secondary_cap > 0) {
    similar_reverse_mask <- similar_reverse_mask & (all_caps >= min_lcc_cap_threshold)
  }

  # Apply same station pre-filters as forward (same secondaries, same thresholds)
  # This ensures symmetric proposal distributions for detailed balance
  if (!is.null(constraints$station_area_pct) && !is.na(constraints$station_area_pct) &&
      !is.null(lcc_areas_in_station) && !is.na(secondary_area_in_station)) {
    similar_reverse_mask <- similar_reverse_mask & station_area_ok
  }
  if (!is.null(constraints$station_capacity_pct) && !is.na(constraints$station_capacity_pct) &&
      !is.null(lcc_caps_in_station) && !is.na(secondary_cap_in_station)) {
    similar_reverse_mask <- similar_reverse_mask & station_cap_ok
  }

  reverse_similar_ids <- which(similar_reverse_mask)

  # Apply same compatibility filter using pre-computed set (fast O(n) intersection)
  # Reuses all_compatible_lccs computed at start of function
  reverse_similar_ids <- intersect(reverse_similar_ids, all_compatible_lccs)

  # Filter reverse set to LCCs with finite capacity
  lcc_caps_rev <- all_caps[reverse_similar_ids]
  finite_mask_rev <- is.finite(lcc_caps_rev)
  reverse_similar_ids <- reverse_similar_ids[finite_mask_rev]
  n_similar_reverse <- length(reverse_similar_ids)

  if (n_similar_reverse == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "no_reverse_candidates"
    ))
  }

  # Verify old LCC is in reverse set (required for reversibility)
  if (!current_lcc_id %in% reverse_similar_ids) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "old_lcc_not_in_reverse_set"
    ))
  }

  # Uniform weighted selection for reverse (capacity preference handled by prior)
  rev_weights <- compute_lcc_capacity_weights(reverse_similar_ids, lcc_library)

  # Guard against invalid reverse weights
  if (!rev_weights$valid) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "invalid_reverse_weights"
    ))
  }

  # Use potentially filtered lcc_ids and verify old LCC survived filtering
  reverse_similar_ids <- rev_weights$lcc_ids
  old_idx <- which(reverse_similar_ids == current_lcc_id)

  if (length(old_idx) == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "replace_lcc",
      reason = "old_lcc_filtered_from_reverse_set"
    ))
  }

  log_w_old <- rev_weights$log_weights[old_idx]
  log_Z_reverse <- rev_weights$log_sum_w
  n_similar_reverse <- length(reverse_similar_ids)

  # MH ratio with weighted proposal correction
  # q(old->new) = w(new) / Z_forward, q(new->old) = w(old) / Z_reverse
  log_q_ratio <- log_w_old - log_w_new + log_Z_forward - log_Z_reverse

  # Add penalty difference (capacity prior)
  penalty_diff <- compute_penalty_difference(
    state$total_capacity,
    new_state$total_capacity,
    constraints
  )
  log_q_ratio <- log_q_ratio + penalty_diff

  accept_prob <- min(1, exp(log_q_ratio))
  accepted <- runif(1) < accept_prob

  # Get spectral regions for cross-region tracking (if available in library)
  has_region <- !is.null(lcc_library$metadata$spectral_region)
  if (has_region) {
    old_lcc_region <- lcc_library$metadata$spectral_region[current_lcc_id]
    new_lcc_region <- lcc_library$metadata$spectral_region[new_lcc_id]
    is_cross_region <- !is.na(old_lcc_region) &&
      !is.na(new_lcc_region) &&
      old_lcc_region != new_lcc_region
  } else {
    old_lcc_region <- NA_character_
    new_lcc_region <- NA_character_
    is_cross_region <- NA
  }

  # Add cache to the returned state (whether accepted or rejected)
  # This allows subsequent replace_lcc moves to reuse the compatible set
  # The cache is invalidated when secondaries change (add/remove_secondary_block)
  if (accepted) {
    new_state$compatible_lccs_cache <- all_compatible_lccs
    returned_state <- new_state
  } else {
    # Create copy of state with cache added (don't modify original)
    returned_state <- state
    returned_state$compatible_lccs_cache <- all_compatible_lccs
  }

  list(
    new_state = returned_state,
    accepted = accepted,
    proposal_failed = FALSE,
    infeasible = FALSE,
    move_type = "replace_lcc",
    new_lcc_id = new_lcc_id,
    k_retained = k_current,
    old_lcc_cap = current_lcc_cap,
    new_lcc_cap = new_lcc_cap,
    n_similar_forward = n_similar,
    n_similar_reverse = n_similar_reverse,
    log_q_ratio = log_q_ratio,
    accept_prob = accept_prob,
    old_lcc_region = old_lcc_region,
    new_lcc_region = new_lcc_region,
    is_cross_region = is_cross_region
  )
}

# ============================================================================
# PURGE DIAGNOSTIC MOVE
# ============================================================================

#' Purge diagnostic move (does not transition, only tests feasibility)
#'
#' Tests whether dropping all secondaries (k -> 0) produces a feasible state.
#' This is a diagnostic tool to determine if k=0 states are genuinely
#' infeasible or just unreachable due to mixing problems.
#'
#' @param state Current parcel MCMC state
#' @param library Secondary library
#' @param parcel_graph Parcel graph
#' @param constraints Constraint list
#' @return Diagnostic result (never transitions)
purge_diagnostic_move <- function(state, library, parcel_graph, constraints) {
  k_current <- length(state$secondary_blocks)

  # Nothing to purge - already at k=0

  if (k_current == 0) {
    return(list(
      new_state = state,
      accepted = FALSE,
      proposal_failed = TRUE,
      move_type = "purge_diagnostic",
      k_before = 0L,
      feasible = TRUE,
      constraint_failed = NA_character_,
      lcc_capacity = NA_real_
    ))
  }

  # Propose k=0 state (LCC only)
  new_state <- reset_to_lcc(state$lcc_parcels, library, parcel_graph)

  # Get LCC capacity for diagnostics
  lcc_capacity <- sum(igraph::V(parcel_graph)[state$lcc_parcels]$capacity)

  # Check feasibility

  feasibility <- check_parcel_feasibility(
    new_state,
    library,
    parcel_graph,
    constraints
  )

  # Return diagnostic result (never actually transition)
  list(
    new_state = state,
    accepted = FALSE,
    proposal_failed = FALSE,
    move_type = "purge_diagnostic",
    k_before = k_current,
    feasible = feasibility$feasible,
    constraint_failed = if (is.null(feasibility$constraint_failed)) {
      NA_character_
    } else {
      feasibility$constraint_failed
    },
    lcc_capacity = lcc_capacity
  )
}
