# parcel_metrics.R - Mixing diagnostics for parcel MCMC
#
# Computes ESS, unique states, geographic coverage, and comparison metrics.

# ============================================================================
# EFFECTIVE SAMPLE SIZE
# ============================================================================

#' Compute effective sample size from trajectory
#'
#' @param x Numeric vector
#' @param lag_max Maximum lag for autocorrelation
#' @return ESS estimate
compute_ess_parcel <- function(x, lag_max = 50) {
  n <- length(x)
  if (n < 10) return(NA_real_)
  if (sd(x) < 1e-10) return(1)  # Constant trace

  acf_vals <- acf(x, lag.max = min(lag_max, n - 1), plot = FALSE)$acf
  n / (1 + 2 * sum(abs(acf_vals[2:min(lag_max + 1, length(acf_vals))])))
}

# ============================================================================
# UNIQUE STATE COUNTING
# ============================================================================

#' Count unique parcel states
#'
#' Two states are equal if they have the same X (set of parcels).
#'
#' @param parcel_samples List of parcel states
#' @return Integer count
count_unique_parcel_states <- function(parcel_samples) {
  state_keys <- vapply(parcel_samples, function(s) {
    digest::digest(sort(s$X), algo = "xxhash64")
  }, character(1))

  length(unique(state_keys))
}

#' Extract unique LCC configurations from parcel samples
#'
#' Uses state$lcc_parcels which is already stored in each state (see R/parcel_mcmc_state.R:15).
#' No need to recompute via get_lcc_parcels().
#'
#' @param parcel_samples List of parcel states from run_parcel_mcmc()
#' @param parcel_graph igraph with capacity/area attributes
#' @return data.table with columns: lcc_key, parcel_ids (list), capacity, area, count
extract_unique_lccs <- function(parcel_samples, parcel_graph) {
  # Extract LCC from each sample - already stored as state$lcc_parcels
  lcc_list <- lapply(parcel_samples, function(state) {
    sort(state$lcc_parcels)
  })

  # Create digest keys for deduplication (matches discovery format)
  lcc_keys <- vapply(lcc_list, function(lcc) {
    if (length(lcc) == 0) return("__EMPTY__")
    digest::digest(lcc, algo = "xxhash64")
  }, character(1))

  # Count occurrences and find first index for each unique key
  unique_keys <- unique(lcc_keys)
  first_idx <- match(unique_keys, lcc_keys)
  key_counts <- table(lcc_keys)

  # Build result table using stored parcel lists (not reconstructed from key)
  results <- lapply(seq_along(unique_keys), function(i) {
    key <- unique_keys[i]
    parcel_ids <- lcc_list[[first_idx[i]]]

    # Handle empty LCC case
    if (key == "__EMPTY__") {
      return(data.table::data.table(
        lcc_key = key,
        parcel_ids = list(character(0)),
        capacity = 0L,
        area = 0,
        count = as.integer(key_counts[[key]])
      ))
    }

    capacity <- sum(igraph::V(parcel_graph)[parcel_ids]$capacity)
    area <- sum(igraph::V(parcel_graph)[parcel_ids]$area)

    data.table::data.table(
      lcc_key = key,
      parcel_ids = list(parcel_ids),
      capacity = as.integer(capacity),
      area = area,
      count = as.integer(key_counts[[key]])
    )
  })

  data.table::rbindlist(results)
}

# ============================================================================
# COMPONENT DIAGNOSTICS
# ============================================================================

#' Extract secondary component count trajectory
#'
#' @param parcel_samples List of parcel states
#' @return Integer vector
trace_n_secondaries <- function(parcel_samples) {
  vapply(parcel_samples, function(s) {
    length(s$secondary_blocks)
  }, integer(1))
}

#' Count secondary birth/death events
#'
#' @param parcel_samples List of parcel states
#' @return List with births, deaths, net_change
count_secondary_turnover <- function(parcel_samples) {
  n_sec <- trace_n_secondaries(parcel_samples)
  diffs <- diff(n_sec)

  list(
    births = sum(diffs > 0),
    deaths = sum(diffs < 0),
    net_change = sum(diffs)
  )
}

# ============================================================================
# GEOGRAPHIC COVERAGE
# ============================================================================

#' Compute centroid trajectory
#'
#' @param parcel_samples List of parcel states
#' @param parcel_graph igraph with centroid_x, centroid_y attributes
#' @return data.table with step, centroid_x, centroid_y
trace_centroids <- function(parcel_samples, parcel_graph) {
  if (!"centroid_x" %in% igraph::vertex_attr_names(parcel_graph)) {
    return(NULL)
  }

  centroids <- lapply(seq_along(parcel_samples), function(i) {
    state <- parcel_samples[[i]]
    parcels <- state$X
    areas <- igraph::V(parcel_graph)[parcels]$area
    cx <- igraph::V(parcel_graph)[parcels]$centroid_x
    cy <- igraph::V(parcel_graph)[parcels]$centroid_y

    # Area-weighted centroid
    total_area <- sum(areas)
    data.table::data.table(
      step = i,
      centroid_x = sum(cx * areas) / total_area,
      centroid_y = sum(cy * areas) / total_area
    )
  })

  data.table::rbindlist(centroids)
}

#' Compute geographic spread (bounding box area) over time
#'
#' @param parcel_samples List of parcel states
#' @param parcel_graph igraph with centroid_x, centroid_y
#' @return Numeric vector of bounding box areas
trace_geographic_spread <- function(parcel_samples, parcel_graph) {
  if (!"centroid_x" %in% igraph::vertex_attr_names(parcel_graph)) {
    return(rep(NA_real_, length(parcel_samples)))
  }

  vapply(parcel_samples, function(state) {
    parcels <- state$X
    cx <- igraph::V(parcel_graph)[parcels]$centroid_x
    cy <- igraph::V(parcel_graph)[parcels]$centroid_y

    if (length(cx) < 2) return(0)

    (max(cx) - min(cx)) * (max(cy) - min(cy))
  }, numeric(1))
}

# ============================================================================
# PARCEL-UNIT STICKINESS
# ============================================================================

#' Compute parcel-unit stickiness scores
#'
#' Categorizes parcel-units as "always_in" (>95% inclusion), "rarely_in" (<5%),
#' or "variable" based on how often they appear in the MCMC samples.
#'
#' @param diagnostics The diagnostics list from run_parcel_mcmc()
#' @param high_threshold Fraction threshold for "always included" (default 0.95)
#' @param low_threshold Fraction threshold for "rarely included" (default 0.05)
#' @return data.table with parcel_id, inclusion_fraction, stickiness_category
compute_parcel_stickiness <- function(
    diagnostics,
    high_threshold = 0.95,
    low_threshold = 0.05
) {
  n_steps <- diagnostics$n_steps
  inclusion <- diagnostics$parcel_inclusion_count

  dt <- data.table::data.table(
    parcel_id = names(inclusion),
    inclusion_count = as.integer(inclusion)
  )

  dt[, inclusion_fraction := inclusion_count / n_steps]

  dt[, stickiness := data.table::fcase(
    inclusion_fraction >= high_threshold, "always_in",
    inclusion_fraction <= low_threshold, "rarely_in",
    default = "variable"
  )]

  data.table::setorder(dt, -inclusion_fraction)
  dt
}

#' Get summary of sticky parcel-units
#'
#' @param stickiness_dt data.table from compute_parcel_stickiness()
#' @return list with always_in and rarely_in parcel IDs and counts
get_sticky_parcels <- function(stickiness_dt) {
  list(
    always_in = stickiness_dt[stickiness == "always_in", parcel_id],
    rarely_in = stickiness_dt[stickiness == "rarely_in", parcel_id],
    n_always_in = sum(stickiness_dt$stickiness == "always_in"),
    n_rarely_in = sum(stickiness_dt$stickiness == "rarely_in"),
    n_variable = sum(stickiness_dt$stickiness == "variable")
  )
}

# ============================================================================
# MULTI-CHAIN METRICS (using coda for proper ESS)
# ============================================================================

#' Compute ESS using coda::effectiveSize for mcmc.list
#'
#' Creates mcmc.list objects from chain trajectories and uses
#' coda's multi-chain ESS calculation which properly handles
#' autocorrelation within chains before pooling.
#'
#' @param valid_chains List of valid chain results
#' @return Named list of ESS values per metric
compute_multichain_ess <- function(valid_chains) {
  # Trajectory names to process
  traj_map <- list(
    capacity = "capacity_trajectory",
    n_components = "n_components_trajectory",
    n_secondaries = "n_secondaries_trajectory",
    lcc_capacity = "lcc_capacity_trajectory",
    centroid_x = "centroid_x_trajectory",
    centroid_y = "centroid_y_trajectory"
  )

  ess_results <- list()

  for (metric in names(traj_map)) {
    traj_name <- traj_map[[metric]]

    # Extract trajectory from each chain (with burn-in)
    chain_trajs <- lapply(valid_chains, function(x) {
      traj <- x$diagnostics[[traj_name]]
      if (is.null(traj)) return(NULL)
      # Apply burn-in: discard first MCMC_BURN_IN samples
      if (MCMC_BURN_IN > 0) {
        if (length(traj) <= MCMC_BURN_IN) return(NULL)  # Not enough samples after burn-in
        traj <- traj[(MCMC_BURN_IN + 1):length(traj)]
      }
      if (length(traj) < 10) return(NULL)
      coda::mcmc(traj)
    })

    # Remove NULL chains
    chain_trajs <- Filter(Negate(is.null), chain_trajs)

    if (length(chain_trajs) >= 2) {
      mcmc_list <- coda::mcmc.list(chain_trajs)
      ess_results[[metric]] <- coda::effectiveSize(mcmc_list)
    } else if (length(chain_trajs) == 1) {
      # Single valid chain - use coda on that one
      ess_results[[metric]] <- coda::effectiveSize(chain_trajs[[1]])
    } else {
      ess_results[[metric]] <- NA_real_
    }
  }

  ess_results
}

#' Compute within-chain ACF, then average across chains
#'
#' @param valid_chains List of valid chain results
#' @param lags Vector of lags to compute (default c(1, 10, 50))
#' @return Named list with average ACF at each lag
compute_multichain_acf <- function(valid_chains, lags = c(1, 10, 50)) {
  max_lag <- max(lags)

  # Collect ACF values from each chain
  chain_acfs <- lapply(valid_chains, function(x) {
    cap_traj <- x$diagnostics$capacity_trajectory
    if (is.null(cap_traj) || length(cap_traj) <= max_lag + 1) return(NULL)

    acf_vals <- acf(cap_traj, lag.max = max_lag, plot = FALSE)$acf
    # Return ACF at requested lags (lag 0 is at index 1)
    setNames(as.numeric(acf_vals[lags + 1]), paste0("lag", lags))
  })

  # Remove NULLs
  chain_acfs <- Filter(Negate(is.null), chain_acfs)

  if (length(chain_acfs) == 0) {
    return(list(lag1 = NA_real_, lag10 = NA_real_, lag50 = NA_real_))
  }

  # Average across chains
  acf_matrix <- do.call(rbind, chain_acfs)
  avg_acf <- colMeans(acf_matrix, na.rm = TRUE)

  list(
    lag1 = avg_acf["lag1"],
    lag10 = avg_acf["lag10"],
    lag50 = avg_acf["lag50"]
  )
}

#' Aggregate move statistics across chains
#'
#' @param valid_chains List of valid chain results
#' @return Combined stats data.table
aggregate_move_stats <- function(valid_chains) {
  all_stats <- lapply(valid_chains, function(x) x$stats)

  # Combine by summing counts
  combined <- data.table::rbindlist(all_stats)
  combined[, .(
    n_attempted = sum(n_attempted),
    n_proposed = sum(n_proposed),
    n_feasible = sum(n_feasible),
    n_accepted = sum(n_accepted)
  ), by = move_type]
}

#' Aggregate parcel inclusion counts across chains
#'
#' Merges parcel_inclusion_count vectors from multiple chains.
#'
#' @param valid_chains List of valid chain results
#' @return Named integer vector of total inclusion counts
aggregate_parcel_inclusion <- function(valid_chains) {
  # Get all parcel names from first chain
  all_parcels <- names(valid_chains[[1]]$diagnostics$parcel_inclusion_count)

  # Sum across chains
  total_counts <- setNames(integer(length(all_parcels)), all_parcels)

  for (chain in valid_chains) {
    counts <- chain$diagnostics$parcel_inclusion_count
    total_counts[names(counts)] <- total_counts[names(counts)] + counts
  }

  total_counts
}

#' Aggregate secondary turnover across chains
#'
#' @param valid_chains List of valid chain results
#' @return List with total births, deaths, net_change
aggregate_secondary_turnover <- function(valid_chains) {
  total_births <- 0L
  total_deaths <- 0L

  for (chain in valid_chains) {
    n_sec <- chain$diagnostics$n_secondaries_trajectory
    if (is.null(n_sec) || length(n_sec) < 2) next
    diffs <- diff(n_sec)
    total_births <- total_births + sum(diffs > 0)
    total_deaths <- total_deaths + sum(diffs < 0)
  }

  list(
    births = total_births,
    deaths = total_deaths,
    net_change = total_births - total_deaths
  )
}

#' Aggregate kernel timing across chains
#'
#' Extracts per-kernel timing from each chain and aggregates into a summary table.
#'
#' @param valid_chains List of valid chain results
#' @return data.table with per-chain timing and aggregate totals
aggregate_chain_timing <- function(valid_chains) {
  timing_rows <- lapply(valid_chains, function(ch) {
    t <- ch$diagnostics$timing
    if (is.null(t)) return(NULL)
    data.table::data.table(
      chain_id = ch$chain_id,
      lcc_local = t$lcc_local,
      birth_death = t$birth_death,
      swap = t$swap,
      replace_lcc = t$replace_lcc,
      overhead = t$overhead,
      total = t$total
    )
  })

  dt <- data.table::rbindlist(Filter(Negate(is.null), timing_rows))

  if (nrow(dt) > 0) {
    totals <- dt[, .(
      lcc_local = sum(lcc_local),
      birth_death = sum(birth_death),
      swap = sum(swap),
      replace_lcc = sum(replace_lcc),
      overhead = sum(overhead),
      total = sum(total)
    )]
    totals[, chain_id := "TOTAL"]
    dt <- data.table::rbindlist(list(dt, totals), fill = TRUE)
  }

  dt
}

#' Compute multi-chain parcel mixing metrics
#'
#' Main function to compute mixing diagnostics from multiple chains.
#' Uses coda::effectiveSize() for proper multi-chain ESS that accounts
#' for autocorrelation within chains before pooling.
#'
#' @param chain_results Named list of chain results from run_parcel_chain_from_region()
#' @param parcel_graph igraph object
#' @return List of aggregated metrics
#' @export
compute_multichain_parcel_metrics <- function(chain_results, parcel_graph) {
  # Filter to valid chains
  valid_chains <- Filter(function(x) !isTRUE(x$initialization_failed), chain_results)

  if (length(valid_chains) < 2) {
    warning("Fewer than 2 valid chains - multi-chain metrics may be unreliable")
  }

  if (length(valid_chains) == 0) {
    warning("No valid chains - cannot compute metrics")
    return(NULL)
  }

  # ESS via coda::effectiveSize() - handles multi-chain properly
  ess_results <- compute_multichain_ess(valid_chains)

  # Within-chain ACF (compute per chain, then average)
  acf_results <- compute_multichain_acf(valid_chains)

  # Secondary dynamics (aggregate across chains)
  turnover <- aggregate_secondary_turnover(valid_chains)

  # Move statistics (aggregate across chains)
  combined_stats <- aggregate_move_stats(valid_chains)

  # Parcel inclusion (aggregate across chains)
  total_inclusion <- aggregate_parcel_inclusion(valid_chains)

  # Total steps across all chains
  total_steps <- sum(sapply(valid_chains, function(x) x$diagnostics$n_steps))
  steps_per_chain <- sapply(valid_chains, function(x) x$diagnostics$n_steps)

  # R-hat (from existing function in parcel_multichain.R)
  rhat_table <- compute_parcel_multichain_rhat(chain_results)

  # Per-chain summary
  chain_summary <- data.table::data.table(
    chain_id = sapply(valid_chains, function(x) x$chain_id),
    region_id = sapply(valid_chains, function(x) x$region_id),
    n_steps = steps_per_chain,
    initialization_failed = FALSE
  )

  list(
    # Multi-chain ESS (from coda)
    ess_capacity = ess_results$capacity,
    ess_n_components = ess_results$n_components,
    ess_n_secondaries = ess_results$n_secondaries,
    ess_lcc_capacity = ess_results$lcc_capacity,
    ess_centroid_x = ess_results$centroid_x,
    ess_centroid_y = ess_results$centroid_y,

    # Within-chain ACF (averaged)
    acf_cap_lag1 = acf_results$lag1,
    acf_cap_lag10 = acf_results$lag10,
    acf_cap_lag50 = acf_results$lag50,

    # Secondary dynamics (aggregated)
    secondary_births = turnover$births,
    secondary_deaths = turnover$deaths,

    # Sample counts
    n_chains = length(valid_chains),
    n_steps_per_chain = steps_per_chain,
    total_steps = total_steps,

    # Aggregated inclusion counts
    parcel_inclusion_count = total_inclusion,

    # Move statistics (aggregated)
    stats = combined_stats,

    # R-hat convergence (use upper CI for conservative check)
    rhat_table = rhat_table,
    all_converged = all(rhat_table$rhat_upper < RHAT_CONVERGENCE_THRESHOLD, na.rm = TRUE),
    max_rhat = max(rhat_table$rhat, na.rm = TRUE),
    max_rhat_upper = max(rhat_table$rhat_upper, na.rm = TRUE),

    # Per-chain summary
    chain_summary = chain_summary,

    # Kernel timing (for profiling)
    timing_summary = aggregate_chain_timing(valid_chains)
  )
}

#' Compute parcel-unit stickiness from multi-chain results
#'
#' Aggregates inclusion counts across all chains before computing stickiness.
#'
#' @param chain_results Named list of chain results
#' @param high_threshold Fraction for "always in" (default 0.95)
#' @param low_threshold Fraction for "rarely in" (default 0.05)
#' @return data.table with parcel_id, inclusion_fraction, stickiness
compute_multichain_stickiness <- function(
    chain_results,
    high_threshold = 0.95,
    low_threshold = 0.05
) {
  valid_chains <- Filter(function(x) !isTRUE(x$initialization_failed), chain_results)

  if (length(valid_chains) == 0) {
    return(data.table::data.table(
      parcel_id = character(0),
      inclusion_count = integer(0),
      inclusion_fraction = numeric(0),
      stickiness = character(0)
    ))
  }

  # Total steps across all chains
  total_steps <- sum(sapply(valid_chains, function(x) x$diagnostics$n_steps))

  # Aggregate inclusion counts
  inclusion <- aggregate_parcel_inclusion(valid_chains)

  dt <- data.table::data.table(
    parcel_id = names(inclusion),
    inclusion_count = as.integer(inclusion)
  )

  dt[, inclusion_fraction := inclusion_count / total_steps]

  dt[, stickiness := data.table::fcase(
    inclusion_fraction >= high_threshold, "always_in",
    inclusion_fraction <= low_threshold, "rarely_in",
    default = "variable"
  )]

  data.table::setorder(dt, -inclusion_fraction)
  dt
}
