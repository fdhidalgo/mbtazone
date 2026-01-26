# parcel_multichain.R - Multi-chain irreducibility analysis for parcel MCMC
#
# Implements multi-chain diagnostics for parcel MCMC approach.
# Uses shared discovered LCC library across all chains for testing mixing.
#
# Region partitioning uses station-feasibility analysis to identify viable
# station components that can satisfy the 90% station capacity constraint.
#
# Reuses:
#   - partition_parcels_by_station_feasibility() from R/parcel_library.R
#   - generate_initial_parcel_state_in_region() from R/parcel_library.R
#   - run_parcel_mcmc() from R/parcel_mcmc_runner.R

# ============================================================================
# GELMAN-RUBIN DIAGNOSTIC (via coda)
# ============================================================================

#' Compute Gelman-Rubin R-hat statistic using coda
#'
#' Wrapper around coda::gelman.diag() for computing the potential scale
#' reduction factor (R-hat) for assessing convergence across multiple chains.
#' R-hat < 1.1 indicates convergence.
#'
#' @param chain_values List of numeric vectors (one per chain)
#' @return List with rhat, rhat_upper (97.5% CI), n_eff
compute_gelman_rubin <- function(chain_values) {
  # Filter out NULL/empty/short chains

  chain_values <- Filter(function(x) !is.null(x) && length(x) >= 10, chain_values)

  if (length(chain_values) < 2) {
    return(list(rhat = NA_real_, rhat_upper = NA_real_, n_eff = NA_real_))
  }

  # Create mcmc.list for coda

  mcmc_chains <- lapply(chain_values, coda::mcmc)
  mcmc_list <- coda::mcmc.list(mcmc_chains)

  # Use coda's gelman.diag

  # autoburnin=FALSE: we handle burn-in elsewhere
  # multivariate=FALSE: we compute per-metric, not multivariate PSRF
  result <- tryCatch({
    coda::gelman.diag(mcmc_list, autoburnin = FALSE, multivariate = FALSE)
  }, error = function(e) {
    # Fall back to NA if gelman.diag fails (e.g., constant chains)
    return(NULL)
  })

  if (is.null(result)) {
    return(list(rhat = NA_real_, rhat_upper = NA_real_, n_eff = NA_real_))
  }

  list(
    rhat = result$psrf[1, "Point est."],
    rhat_upper = result$psrf[1, "Upper C.I."],
    n_eff = as.numeric(coda::effectiveSize(mcmc_list))
  )
}

# ============================================================================
# MULTI-CHAIN CONFIGURATION
# ============================================================================

#' Define parcel multi-chain configurations
#'
#' Creates configuration list for running multiple parcel MCMC chains from
#' different regions using the same discovered LCC library.
#'
#' Region IDs should come from partition_parcels_by_station_feasibility(), which
#' identifies viable station components. Regions are named "R1", "R2", etc.
#'
#' @param base_config Base kernel configuration (from define_parcel_kernel_configs())
#' @param n_chains Number of chains to run (default 4)
#' @param n_steps Number of MCMC steps per chain (default MCMC_STEPS_MACRO)
#' @param region_ids Character vector of region identifiers. Use
#'   names(partition_parcels_by_station_feasibility(...)$seed_pools) to get regions.
#'   Default c("R1", "R2", "R3", "R4") assumes 4 viable station components.
#' @return List of chain configurations with unique seeds and region assignments
#' @export
define_parcel_multichain_configs <- function(base_config,
                                             n_chains = DEFAULT_N_CHAINS,
                                             n_steps = MCMC_STEPS_MACRO,
                                             region_ids = c("R1", "R2", "R3", "R4")) {
  if (n_chains > length(region_ids)) {
    cli::cli_alert_warning(
      "Requested {n_chains} chains but only {length(region_ids)} regions available"
    )
    n_chains <- length(region_ids)
  }

  configs <- lapply(seq_len(n_chains), function(i) {
    config <- base_config
    config$chain_id <- i
    config$region_id <- region_ids[i]
    config$seed <- base_config$seed + i * 1000
    config$n_steps <- n_steps
    config$name <- sprintf("Parcel Chain %d (%s)", i, region_ids[i])
    config
  })

  names(configs) <- paste0("chain_", seq_len(n_chains))
  configs
}

# ============================================================================
# CHAIN EXECUTION
# ============================================================================

#' Run single parcel chain from region-seeded start
#'
#' Runs parcel MCMC from a region-seeded initial state using the shared
#' discovered LCC library.
#'
#' @param config Chain configuration from define_parcel_multichain_configs()
#' @param parcel_graph_result Result from build_parcel_graph_target()
#' @param constraints MBTA constraints
#' @param secondary_library Secondary block library
#' @param lcc_library Discovered LCC library (shared across all chains)
#' @param region_assignments Named vector from partition_parcels_by_station_feasibility()
#' @param seed_pools Optional list of parcel ID vectors per region (from station partitioning).
#'   If provided, seeds BFS from viable station components to guarantee constraint satisfaction.
#' @return List with parcel_samples, samples, stats, diagnostics, chain_id,
#'         region_id, initialization_failed, failure_reason
#' @export
run_parcel_chain_from_region <- function(config,
                                         parcel_graph_result,
                                         constraints,
                                         secondary_library,
                                         lcc_library,
                                         region_assignments,
                                         seed_pools = NULL,
                                         verbose = FALSE) {
  parcel_graph <- parcel_graph_result$parcel_graph

  # Hydrate libraries (convert safe integer indices to bit objects)
  secondary_library <- hydrate_library(secondary_library)
  lcc_library <- hydrate_library(lcc_library)

  # Get seed pool for this region if available
  seed_pool <- if (!is.null(seed_pools)) seed_pools[[config$region_id]] else NULL

  # Generate region-seeded initial state
  initial_state <- tryCatch({
    generate_initial_parcel_state_in_region(
      parcel_graph = parcel_graph,
      constraints = constraints,
      libraries = list(secondary_library = secondary_library,
                       lcc_library = lcc_library),
      region_assignments = region_assignments,
      target_region = config$region_id,
      seed_pool = seed_pool
    )
  }, error = function(e) NULL)

  if (is.null(initial_state)) {
    return(list(
      parcel_samples = NULL,
      samples = NULL,
      diagnostics = NULL,
      chain_id = config$chain_id,
      region_id = config$region_id,
      kernel_name = config$name,
      initialization_failed = TRUE,
      failure_reason = paste0("Failed to generate initial state in region ",
                              config$region_id)
    ))
  }

  # Run parcel MCMC with the shared lcc_library
  # Enable online enrichment so chains can escape discovered library
  result <- run_parcel_mcmc(
    parcel_graph = parcel_graph,
    initial_state = initial_state,
    constraints = constraints,
    secondary_library = secondary_library,
    lcc_library = lcc_library,
    config = config,
    parcel_assignments = parcel_graph_result$parcel_assignments,
    neighbor_cache = parcel_graph_result$neighbor_cache,
    enable_online_enrichment = TRUE,
    verbose = verbose
  )

  # Add chain metadata
  result$chain_id <- config$chain_id
  result$region_id <- config$region_id
  result$initial_state <- initial_state
  result$initialization_failed <- FALSE
  result$failure_reason <- NULL

  # Return diagnostics needed for multichain analysis:
  # - R-hat/ESS: capacity, n_components, n_secondaries, lcc_capacity, centroid trajectories
  # - Coverage/Separation: parcel_inclusion_count, n_steps
  # - Kernel diagnostics: symmetric_bd_*, replace_lcc_*, swap_*
  minimal_diagnostics <- list(
    capacity_trajectory = result$diagnostics$capacity_trajectory,
    n_components_trajectory = result$diagnostics$n_components_trajectory,
    n_secondaries_trajectory = result$diagnostics$n_secondaries_trajectory,
    lcc_capacity_trajectory = result$diagnostics$lcc_capacity_trajectory,
    centroid_x_trajectory = result$diagnostics$centroid_x_trajectory,
    centroid_y_trajectory = result$diagnostics$centroid_y_trajectory,
    parcel_inclusion_count = result$diagnostics$parcel_inclusion_count,
    n_steps = result$diagnostics$n_steps,
    # Symmetric birth/death diagnostics
    symmetric_bd_births = result$diagnostics$symmetric_bd_births,
    symmetric_bd_deaths = result$diagnostics$symmetric_bd_deaths,
    # Replace-LCC diagnostics
    replace_lcc_reasons = result$diagnostics$replace_lcc_reasons,
    replace_lcc_constraints = result$diagnostics$replace_lcc_constraints,
    replace_lcc_accept_prob = result$diagnostics$replace_lcc_accept_prob,
    replace_lcc_log_q_ratio = result$diagnostics$replace_lcc_log_q_ratio,
    replace_lcc_k_retained = result$diagnostics$replace_lcc_k_retained,
    replace_lcc_n_similar_forward = result$diagnostics$replace_lcc_n_similar_forward,
    replace_lcc_n_similar_reverse = result$diagnostics$replace_lcc_n_similar_reverse,
    # Swap kernel diagnostics
    swap_delta_caps = result$diagnostics$swap_delta_caps,
    swap_n_similar_fwd = result$diagnostics$swap_n_similar_fwd,
    swap_n_similar_rev = result$diagnostics$swap_n_similar_rev,
    swap_reasons = result$diagnostics$swap_reasons,
    swap_constraints = result$diagnostics$swap_constraints,
    # Cross-region transition tracking
    cross_region_transitions = result$diagnostics$cross_region_transitions,
    same_region_transitions = result$diagnostics$same_region_transitions,
    # Online enrichment tracking
    online_adds = result$diagnostics$online_adds,
    final_library_size = result$diagnostics$final_library_size,
    # Kernel timing (for profiling)
    timing = result$diagnostics$timing
  )

  list(
    diagnostics = minimal_diagnostics,
    stats = result$stats,
    chain_id = result$chain_id,
    region_id = result$region_id,
    kernel_name = result$kernel_name,
    initialization_failed = FALSE,
    failure_reason = NULL,
    # Include thinned samples for visualization (minimal state format)
    parcel_samples = result$parcel_samples
  )
}

# ============================================================================
# CONVERGENCE DIAGNOSTICS
# ============================================================================

#' Compute multi-chain R-hat for parcel metrics
#'
#' Computes Gelman-Rubin R-hat statistics for parcel-level metrics across
#' multiple chains: capacity, n_components, lcc_capacity, centroid_x, centroid_y.
#'
#' @param chain_results List of parcel chain results from run_parcel_chain_from_region()
#' @return data.table with columns: metric, rhat, rhat_upper, n_eff
#' @export
compute_parcel_multichain_rhat <- function(chain_results) {
  # Filter out failed chains
  valid_chains <- Filter(function(x) !isTRUE(x$initialization_failed), chain_results)

  if (length(valid_chains) < 2) {
    warning("Need at least 2 valid parcel chains for R-hat computation")
    return(data.table::data.table(
      metric = character(0), rhat = numeric(0),
      rhat_upper = numeric(0), n_eff = numeric(0)
    ))
  }

  # Metrics to track (must be in diagnostics)
  metrics <- c("capacity", "n_components", "lcc_capacity",
               "centroid_x", "centroid_y")

  # Map metric name to trajectory name in diagnostics
  traj_names <- c(
    capacity = "capacity_trajectory",
    n_components = "n_components_trajectory",
    lcc_capacity = "lcc_capacity_trajectory",
    centroid_x = "centroid_x_trajectory",
    centroid_y = "centroid_y_trajectory"
  )

  results <- lapply(metrics, function(metric) {
    traj_name <- traj_names[[metric]]
    chain_values <- lapply(valid_chains, function(r) {
      if (is.null(r$diagnostics)) return(NULL)
      traj <- r$diagnostics[[traj_name]]
      # Apply burn-in: discard first MCMC_BURN_IN samples
      if (MCMC_BURN_IN > 0) {
        if (length(traj) <= MCMC_BURN_IN) return(NULL)  # Not enough samples after burn-in
        traj <- traj[(MCMC_BURN_IN + 1):length(traj)]
      }
      traj
    })

    rhat_result <- compute_gelman_rubin(chain_values)

    data.table::data.table(
      metric = metric,
      rhat = rhat_result$rhat,
      rhat_upper = rhat_result$rhat_upper,
      n_eff = rhat_result$n_eff
    )
  })

  data.table::rbindlist(results)
}

# ============================================================================
# GEOGRAPHIC COVERAGE ANALYSIS
# ============================================================================

#' Compute parcel geographic coverage for a single chain
#'
#' Analyzes what fraction of each region's parcels were visited by the chain.
#' Uses parcel centroids and region partitioning.
#'
#' @param parcel_result Single parcel chain result
#' @param parcel_graph igraph object with centroid attributes
#' @param region_assignments Named vector from partition_parcels_by_station_feasibility()
#' @param inclusion_threshold Fraction of samples for "visited" (default 0.01)
#' @return data.table with region coverage statistics
compute_parcel_chain_coverage <- function(parcel_result,
                                          parcel_graph,
                                          region_assignments,
                                          inclusion_threshold = 0.01) {
  if (isTRUE(parcel_result$initialization_failed)) return(NULL)
  if (is.null(parcel_result$diagnostics)) return(NULL)

  # Get parcel inclusion fractions
  n_steps <- parcel_result$diagnostics$n_steps
  inclusion_counts <- parcel_result$diagnostics$parcel_inclusion_count

  if (is.null(inclusion_counts) || n_steps == 0) return(NULL)

  inclusion_frac <- inclusion_counts / n_steps

  # Parcels visited at least threshold fraction of time
  visited_parcels <- names(inclusion_frac)[inclusion_frac >= inclusion_threshold]

  # Compute coverage by region
  region_ids <- unique(region_assignments)
  region_ids <- region_ids[!is.na(region_ids)]

  coverage <- lapply(region_ids, function(rid) {
    region_parcels <- names(region_assignments)[region_assignments == rid]
    n_in_region <- length(region_parcels)
    visited_in_region <- intersect(visited_parcels, region_parcels)
    n_visited <- length(visited_in_region)

    # Capacity coverage
    total_capacity <- sum(igraph::V(parcel_graph)[region_parcels]$capacity)
    visited_capacity <- sum(igraph::V(parcel_graph)[visited_in_region]$capacity)

    data.table::data.table(
      region_id = rid,
      n_parcels = n_in_region,
      n_visited = n_visited,
      pct_visited = 100 * n_visited / max(n_in_region, 1),
      total_capacity = total_capacity,
      visited_capacity = visited_capacity,
      capacity_pct = 100 * visited_capacity / max(total_capacity, 1)
    )
  })

  data.table::rbindlist(coverage)
}

#' Summarize parcel geographic coverage across chains
#'
#' @param chain_results List of parcel chain results
#' @param parcel_graph igraph object
#' @param region_assignments Region partition
#' @return data.table with coverage summary by chain and region
#' @export
summarize_parcel_geographic_coverage <- function(chain_results,
                                                  parcel_graph,
                                                  region_assignments) {
  coverage_list <- lapply(names(chain_results), function(chain_name) {
    result <- chain_results[[chain_name]]
    coverage <- compute_parcel_chain_coverage(result, parcel_graph, region_assignments)
    if (!is.null(coverage)) {
      coverage$chain_id <- result$chain_id
      coverage$chain_name <- chain_name
    }
    coverage
  })

  data.table::rbindlist(Filter(Negate(is.null), coverage_list))
}

# ============================================================================
# CHAIN SEPARATION DETECTION
# ============================================================================

#' Detect parcel chain separation
#'
#' Checks if parcel chains remain in disjoint regions of the state space
#' by computing Jaccard overlap of visited parcel sets.
#'
#' @param chain_results List of parcel chain results
#' @param parcel_graph igraph object
#' @param visit_threshold Fraction for "frequently visited" (default 0.05)
#' @return List with separated, overlap_matrix, min_overlap, centroid_ranges,
#'         separation_evidence
#' @export
detect_parcel_chain_separation <- function(chain_results,
                                           parcel_graph,
                                           visit_threshold = 0.05) {
  valid_chains <- Filter(function(x) !isTRUE(x$initialization_failed), chain_results)

  if (length(valid_chains) < 2) {
    return(list(
      separated = NA,
      overlap_matrix = NULL,
      min_overlap = NA,
      centroid_ranges = NULL,
      separation_evidence = "Insufficient chains for separation analysis"
    ))
  }

  # Get frequently visited parcels for each chain
  visited_sets <- lapply(valid_chains, function(result) {
    if (is.null(result$diagnostics)) return(character(0))
    n_steps <- result$diagnostics$n_steps
    counts <- result$diagnostics$parcel_inclusion_count
    if (is.null(counts) || n_steps == 0) return(character(0))
    frac <- counts / n_steps
    names(frac)[frac >= visit_threshold]
  })

  n_chains <- length(visited_sets)
  chain_names <- names(valid_chains)
  if (is.null(chain_names)) chain_names <- paste0("chain_", seq_len(n_chains))

  # Compute pairwise Jaccard overlap
  overlap_matrix <- matrix(NA, n_chains, n_chains,
                           dimnames = list(chain_names, chain_names))

  for (i in seq_len(n_chains)) {
    for (j in seq_len(n_chains)) {
      set_i <- visited_sets[[i]]
      set_j <- visited_sets[[j]]
      intersection <- length(intersect(set_i, set_j))
      union_size <- length(union(set_i, set_j))
      overlap_matrix[i, j] <- if (union_size > 0) intersection / union_size else 0
    }
  }

  # Find minimum off-diagonal overlap
  min_overlap <- NA
  if (n_chains >= 2) {
    off_diag <- overlap_matrix[row(overlap_matrix) != col(overlap_matrix)]
    min_overlap <- min(off_diag, na.rm = TRUE)
  }

  # Compute centroid ranges per chain
  centroid_ranges <- matrix(NA, n_chains, 4,
                            dimnames = list(chain_names, c("x_min", "x_max", "y_min", "y_max")))

  for (i in seq_len(n_chains)) {
    result <- valid_chains[[i]]
    if (!is.null(result$diagnostics)) {
      cx <- result$diagnostics$centroid_x_trajectory
      cy <- result$diagnostics$centroid_y_trajectory
      if (!is.null(cx) && !is.null(cy) && length(cx) > 0) {
        centroid_ranges[i, ] <- c(min(cx), max(cx), min(cy), max(cy))
      }
    }
  }

  # Determine separation status
  separated <- !is.na(min_overlap) && min_overlap < CHAIN_OVERLAP_THRESHOLD

  separation_evidence <- if (is.na(min_overlap)) {
    "Unable to compute overlap"
  } else if (separated) {
    sprintf("SEPARATED: Min pairwise Jaccard overlap = %.3f < %.2f threshold",
            min_overlap, CHAIN_OVERLAP_THRESHOLD)
  } else {
    sprintf("Mixing OK: Min pairwise Jaccard overlap = %.3f >= %.2f threshold",
            min_overlap, CHAIN_OVERLAP_THRESHOLD)
  }

  list(
    separated = separated,
    overlap_matrix = overlap_matrix,
    min_overlap = min_overlap,
    centroid_ranges = centroid_ranges,
    separation_evidence = separation_evidence
  )
}

#' Compare parcel chain distributions using Kolmogorov-Smirnov test
#'
#' @param chain_results List of parcel chain results
#' @param burn_in_frac Fraction of samples to discard as burn-in (default 0.1)
#' @return data.table with pairwise KS test results on capacity
compare_parcel_chain_distributions <- function(chain_results, burn_in_frac = 0.1) {
  valid_chains <- Filter(function(x) !isTRUE(x$initialization_failed), chain_results)

  if (length(valid_chains) < 2) {
    return(data.table::data.table(
      chain_1 = character(0), chain_2 = character(0),
      ks_statistic = numeric(0), p_value = numeric(0), significant = logical(0)
    ))
  }

  # Extract post-burn-in capacity trajectories
  trajectories <- lapply(valid_chains, function(result) {
    if (is.null(result$diagnostics)) return(NULL)
    traj <- result$diagnostics$capacity_trajectory
    if (is.null(traj) || length(traj) == 0) return(NULL)
    burn_in <- floor(length(traj) * burn_in_frac)
    if (burn_in >= length(traj) - 10) return(NULL)
    traj[(burn_in + 1):length(traj)]
  })

  chain_names <- names(valid_chains)
  if (is.null(chain_names)) chain_names <- paste0("chain_", seq_along(valid_chains))
  names(trajectories) <- chain_names

  # Filter out NULL trajectories
  trajectories <- Filter(Negate(is.null), trajectories)

  if (length(trajectories) < 2) {
    return(data.table::data.table(
      chain_1 = character(0), chain_2 = character(0),
      ks_statistic = numeric(0), p_value = numeric(0), significant = logical(0)
    ))
  }

  # Pairwise KS tests
  chain_names <- names(trajectories)
  results <- list()

  for (i in seq_along(chain_names)) {
    for (j in seq_along(chain_names)) {
      if (j <= i) next
      ks_result <- stats::ks.test(trajectories[[i]], trajectories[[j]])
      results[[length(results) + 1]] <- data.table::data.table(
        chain_1 = chain_names[i],
        chain_2 = chain_names[j],
        ks_statistic = ks_result$statistic,
        p_value = ks_result$p.value,
        significant = ks_result$p.value < 0.05
      )
    }
  }

  data.table::rbindlist(results)
}

# ============================================================================
# VISUALIZATION
# ============================================================================

#' Plot parcel R-hat convergence summary
#'
#' Shows R-hat point estimates with error bars extending to upper 97.5% CI.
#' Convergence is assessed using the upper CI (more conservative).
#'
#' @param rhat_table Output from compute_parcel_multichain_rhat()
#' @return ggplot object
#' @export
plot_parcel_rhat_summary <- function(rhat_table) {
  if (nrow(rhat_table) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0.5, y = 0.5,
                               label = "No R-hat data available") +
             ggplot2::theme_void())
  }

  # Add convergence status (use upper CI for conservative check)
  rhat_table <- data.table::copy(rhat_table)
  rhat_table[, converged := rhat_upper < RHAT_CONVERGENCE_THRESHOLD]

  p <- ggplot2::ggplot(rhat_table, ggplot2::aes(x = metric, y = rhat, fill = converged)) +
    ggplot2::geom_col(width = 0.7) +
    # Error bars showing upper CI
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = rhat, ymax = rhat_upper),
      width = 0.2, linewidth = 0.6, color = "gray30"
    ) +
    ggplot2::geom_hline(yintercept = RHAT_CONVERGENCE_THRESHOLD,
                        linetype = "dashed", color = "red", linewidth = 0.8) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                               name = "Converged\n(Upper CI)",
                               labels = c("TRUE" = "Yes", "FALSE" = "No")) +
    ggplot2::labs(
      title = "Parcel Multi-Chain R-hat Convergence",
      subtitle = sprintf("Threshold = %.1f (error bars show 97.5%% CI)", RHAT_CONVERGENCE_THRESHOLD),
      x = "Metric",
      y = "R-hat"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  p
}

# ============================================================================
# REPORT GENERATION
# ============================================================================

#' Create comprehensive parcel irreducibility report
#'
#' @param chain_results List of parcel chain results
#' @param parcel_graph igraph object
#' @param region_assignments Region partition
#' @return List with all diagnostics and summary
#' @export
create_parcel_irreducibility_report <- function(chain_results,
                                                 parcel_graph,
                                                 region_assignments) {
  # Count chains
  n_attempted <- length(chain_results)
  n_valid <- sum(sapply(chain_results, function(x) !isTRUE(x$initialization_failed)))
  n_failed <- n_attempted - n_valid

  # Compute diagnostics
  rhat <- compute_parcel_multichain_rhat(chain_results)
  coverage <- summarize_parcel_geographic_coverage(chain_results, parcel_graph, region_assignments)
  separation <- detect_parcel_chain_separation(chain_results, parcel_graph)
  ks_tests <- compare_parcel_chain_distributions(chain_results)

  # Assess concerns
  concerns <- character(0)

  if (n_valid < 2) {
    concerns <- c(concerns, sprintf("Only %d valid chains (need >= 2)", n_valid))
  }

  if (nrow(rhat) > 0) {
    # Use upper CI for conservative convergence check
    high_rhat <- rhat[rhat_upper >= RHAT_CONVERGENCE_THRESHOLD, metric]
    if (length(high_rhat) > 0) {
      concerns <- c(concerns, sprintf("High R-hat (upper CI) for: %s", paste(high_rhat, collapse = ", ")))
    }
  }

  if (isTRUE(separation$separated)) {
    concerns <- c(concerns, "Chain separation detected (low Jaccard overlap)")
  }

  if (nrow(ks_tests) > 0 && any(ks_tests$significant)) {
    sig_pairs <- ks_tests[significant == TRUE, paste(chain_1, "vs", chain_2)]
    if (length(sig_pairs) > 0) {
      concerns <- c(concerns, sprintf("Significant KS tests: %s", paste(sig_pairs, collapse = "; ")))
    }
  }

  irreducibility_concern <- length(concerns) > 0

  summary_text <- if (irreducibility_concern) {
    sprintf("POTENTIAL IRREDUCIBILITY ISSUE: %d concern(s) flagged. %s",
            length(concerns), paste(concerns, collapse = "; "))
  } else {
    sprintf("Parcel multi-chain analysis passed (%d/%d chains valid, all diagnostics OK)",
            n_valid, n_attempted)
  }

  list(
    rhat = rhat,
    geographic_coverage = coverage,
    separation_analysis = separation,
    ks_tests = ks_tests,
    n_chains_attempted = n_attempted,
    n_chains_valid = n_valid,
    n_chains_failed = n_failed,
    irreducibility_concern = irreducibility_concern,
    concerns = concerns,
    summary = summary_text
  )
}
