# ============================================================================
# test_joint_refresh.R — Fast kernel test harness for joint_core_refresh_move
#
# Loads pipeline objects from a completed Norwood targets run, reconstructs
# full MCMC states from thinned samples, and benchmarks the joint core refresh
# kernel. Includes component log_q decomposition and grid sweep.
#
# Usage: source("dev/test_joint_refresh.R") from the package root.
# ============================================================================

# ============================================================================
# SECTION 1: Configuration
# ============================================================================

# Tuning overrides (applied after sourcing config files)
OVERRIDE_JOINT_CORE_P_KEEP <- NULL
OVERRIDE_JOINT_ADD_WEIGHT_NOTHING <- NULL
OVERRIDE_JOINT_LCC_EXCESS_RATE <- NULL

# Benchmark parameters
N_CALLS_PER_STATE <- 20L
N_STATES <- 20L
SEED <- 42L

# Grid sweep parameters (Section 6)
RUN_GRID_SWEEP <- TRUE
GRID_P_KEEP <- c(0.3, 0.5, 0.7)
GRID_ADD_WEIGHT_NOTHING <- c(1.0, 5.0, 10.0)

# Target store
TARGETS_STORE <- "ext/_targets_Norwood"

# ============================================================================
# SECTION 2: Load pipeline objects
# ============================================================================

cat("=== Loading pipeline objects ===\n")

library(targets)
library(igraph)
library(data.table)

# Source config files to set globals
source("inst/targets/temp_targets_config.R")
source("inst/targets/temp_targets_parcel_config.R")

# Load latest package code
devtools::load_all()

# Apply overrides
if (!is.null(OVERRIDE_JOINT_CORE_P_KEEP)) {
  JOINT_CORE_P_KEEP <<- OVERRIDE_JOINT_CORE_P_KEEP
}
if (!is.null(OVERRIDE_JOINT_ADD_WEIGHT_NOTHING)) {
  JOINT_ADD_WEIGHT_NOTHING <<- OVERRIDE_JOINT_ADD_WEIGHT_NOTHING
}
if (!is.null(OVERRIDE_JOINT_LCC_EXCESS_RATE)) {
  JOINT_LCC_EXCESS_RATE <<- OVERRIDE_JOINT_LCC_EXCESS_RATE
}

# Load from targets store
parcel_graph_result <- tar_read(parcel_graph_result, store = TARGETS_STORE)
constraints <- tar_read(constraints, store = TARGETS_STORE)
secondary_library <- tar_read(discovered_secondary_library, store = TARGETS_STORE)
lcc_library <- tar_read(discovered_lcc_library, store = TARGETS_STORE)
all_chain_results <- tar_read(all_parcel_chain_results, store = TARGETS_STORE)

parcel_graph <- parcel_graph_result$parcel_graph
neighbor_cache <- parcel_graph_result$neighbor_cache

cat(sprintf("  Parcel graph: %d vertices, %d edges\n",
            vcount(parcel_graph), ecount(parcel_graph)))
cat(sprintf("  LCC library: %d blocks\n", lcc_library$n_blocks))
cat(sprintf("  Secondary library: %d blocks\n", secondary_library$n_blocks))
cat(sprintf("  Constraints: min_capacity=%d, min_area=%.1f\n",
            constraints$min_capacity, constraints$min_area))
cat(sprintf("  Chains loaded: %d\n", length(all_chain_results)))

# ============================================================================
# SECTION 3: Reconstruct benchmark states
# ============================================================================

cat("\n=== Reconstructing benchmark states ===\n")

# Build neighbor_idx and parcel_names (same as runner)
lib_parcel_names <- secondary_library$parcel_names
n_parcels <- length(lib_parcel_names)
graph_parcel_names <- V(parcel_graph)$name
lib_name_to_idx <- setNames(seq_len(n_parcels), lib_parcel_names)

neighbor_idx <- lapply(lib_parcel_names, function(m) {
  if (!is.null(neighbor_cache[[m]])) {
    unname(lib_name_to_idx[neighbor_cache[[m]]])
  } else {
    integer(0)
  }
})
names(neighbor_idx) <- lib_parcel_names

parcel_names_for_kernel <- graph_parcel_names

# Extract parcel_samples from all chains and pool them
all_samples <- list()
all_k_values <- integer(0)

for (chain_name in names(all_chain_results)) {
  chain <- all_chain_results[[chain_name]]
  if (isTRUE(chain$initialization_failed)) next
  samples <- chain$parcel_samples
  if (is.null(samples) || length(samples) == 0) next
  for (s in samples) {
    if (!is.null(s) && !is.null(s$X)) {
      k <- length(s$secondary_blocks)
      all_samples <- c(all_samples, list(s))
      all_k_values <- c(all_k_values, k)
    }
  }
}

cat(sprintf("  Pooled %d thinned samples across chains\n", length(all_samples)))
cat(sprintf("  k distribution: %s\n",
            paste(names(table(all_k_values)), table(all_k_values), sep = ":", collapse = ", ")))

# Sample N_STATES states spanning the k distribution
set.seed(SEED)
if (length(all_samples) <= N_STATES) {
  selected_idx <- seq_along(all_samples)
} else {
  # Stratified sampling: try to cover all k values
  k_tab <- table(all_k_values)
  k_vals <- as.integer(names(k_tab))
  samples_per_k <- pmax(1L, round(N_STATES * as.numeric(k_tab) / length(all_samples)))
  # Adjust to hit target
  total_allocated <- sum(samples_per_k)
  if (total_allocated < N_STATES) {
    # Add extras from most common k values
    deficit <- N_STATES - total_allocated
    order_by_count <- order(-as.numeric(k_tab))
    for (i in order_by_count) {
      add <- min(deficit, as.numeric(k_tab[i]) - samples_per_k[i])
      if (add > 0) {
        samples_per_k[i] <- samples_per_k[i] + add
        deficit <- deficit - add
      }
      if (deficit == 0) break
    }
  }

  selected_idx <- integer(0)
  for (i in seq_along(k_vals)) {
    k <- k_vals[i]
    pool <- which(all_k_values == k)
    n_pick <- min(samples_per_k[i], length(pool))
    selected_idx <- c(selected_idx, sample(pool, n_pick))
  }
}

cat(sprintf("  Selected %d states for benchmarking\n", length(selected_idx)))

# Rebuild each selected state using initialize_parcel_state
reconstruct_state <- function(minimal_state) {
  state <- initialize_parcel_state(
    parcel_ids = minimal_state$X,
    secondary_block_ids = minimal_state$secondary_blocks,
    library = secondary_library,
    parcel_graph = parcel_graph
  )

  # Add incremental fields (lcc_logical, X_logical, neighbor counts)
  lcc_indices <- lib_name_to_idx[state$lcc_parcels]
  lcc_indices <- lcc_indices[!is.na(lcc_indices)]
  state$lcc_logical <- logical(n_parcels)
  state$lcc_logical[lcc_indices] <- TRUE

  state$X_logical <- logical(n_parcels)
  state$X_logical[state$X_indices] <- TRUE

  X_logical_vec <- state$X_logical
  lcc_logical_vec <- state$lcc_logical

  state$lcc_neighbor_counts <- vapply(
    seq_len(n_parcels),
    function(i) sum(lcc_logical_vec[neighbor_idx[[i]]]),
    integer(1)
  )
  names(state$lcc_neighbor_counts) <- lib_parcel_names

  state$X_neighbor_counts <- vapply(
    seq_len(n_parcels),
    function(i) sum(X_logical_vec[neighbor_idx[[i]]]),
    integer(1)
  )
  names(state$X_neighbor_counts) <- lib_parcel_names

  state
}

cat("  Reconstructing states...")
benchmark_states <- lapply(selected_idx, function(i) {
  reconstruct_state(all_samples[[i]])
})
benchmark_k <- all_k_values[selected_idx]
cat(sprintf(" done (%d states)\n", length(benchmark_states)))
cat(sprintf("  Benchmark k distribution: %s\n",
            paste(names(table(benchmark_k)), table(benchmark_k), sep = ":", collapse = ", ")))

# Enrich the LCC library with all benchmark states' LCCs
# (mirrors the immediate enrichment the runner does before each kernel call)
cat("  Enriching LCC library with benchmark LCCs...")
n_enriched <- 0L
for (state in benchmark_states) {
  enrich_result <- add_lcc_to_library(
    lcc_library,
    state$lcc_parcels,
    parcel_graph,
    max_online = 10000L,
    neighbor_cache = neighbor_cache
  )
  lcc_library <- enrich_result$lcc_library
  if (enrich_result$added) n_enriched <- n_enriched + 1L
}
cat(sprintf(" added %d new LCCs (library now %d blocks)\n",
            n_enriched, lcc_library$n_blocks))

# Also enrich with LCCs from ALL pooled samples (not just benchmark states)
# to approximate what a full chain run would have in its library
cat("  Enriching with all pooled sample LCCs...")
n_enriched_all <- 0L
for (s in all_samples) {
  if (!is.null(s$lcc_parcels) && length(s$lcc_parcels) > 0) {
    enrich_result <- add_lcc_to_library(
      lcc_library,
      s$lcc_parcels,
      parcel_graph,
      max_online = 10000L,
      neighbor_cache = neighbor_cache
    )
    lcc_library <- enrich_result$lcc_library
    if (enrich_result$added) n_enriched_all <- n_enriched_all + 1L
  }
}
cat(sprintf(" added %d new LCCs (library now %d blocks)\n",
            n_enriched_all, lcc_library$n_blocks))

# ============================================================================
# SECTION 4: Kernel benchmark loop
# ============================================================================

run_benchmark <- function(states, k_values, n_calls, label = "baseline",
                          save_raw = FALSE) {
  cat(sprintf("\n=== Running benchmark: %s (%d states x %d calls) ===\n",
              label, length(states), n_calls))

  results <- vector("list", length(states) * n_calls)
  raw_results <- if (save_raw) vector("list", length(states) * n_calls) else NULL
  result_idx <- 0L
  t_start <- proc.time()[3]

  for (si in seq_along(states)) {
    state <- states[[si]]
    k <- k_values[si]
    for (ci in seq_len(n_calls)) {
      result_idx <- result_idx + 1L
      r <- joint_core_refresh_move(
        state = state,
        lcc_library = lcc_library,
        secondary_library = secondary_library,
        parcel_graph = parcel_graph,
        constraints = constraints,
        neighbor_idx = neighbor_idx,
        parcel_names = parcel_names_for_kernel
      )
      results[[result_idx]] <- data.table(
        state_idx = si,
        call_idx = ci,
        k_start = k,
        accepted = isTRUE(r$accepted),
        reason = if (is.null(r$reason)) NA_character_ else r$reason,
        zero_reverse_subreason = if (is.null(r$zero_reverse_subreason)) NA_character_ else r$zero_reverse_subreason,
        accept_prob = if (is.null(r$accept_prob)) NA_real_ else as.numeric(r$accept_prob),
        log_q_forward = if (is.null(r$log_q_forward)) NA_real_ else as.numeric(r$log_q_forward),
        log_q_reverse = if (is.null(r$log_q_reverse)) NA_real_ else as.numeric(r$log_q_reverse),
        log_q_core_forward = if (is.null(r$log_q_core_forward)) NA_real_ else as.numeric(r$log_q_core_forward),
        log_q_core_reverse = if (is.null(r$log_q_core_reverse)) NA_real_ else as.numeric(r$log_q_core_reverse),
        log_q_lcc_forward = if (is.null(r$log_q_lcc_forward)) NA_real_ else as.numeric(r$log_q_lcc_forward),
        log_q_lcc_reverse = if (is.null(r$log_q_lcc_reverse)) NA_real_ else as.numeric(r$log_q_lcc_reverse),
        log_q_add_forward = if (is.null(r$log_q_add_forward)) NA_real_ else as.numeric(r$log_q_add_forward),
        log_q_add_reverse = if (is.null(r$log_q_add_reverse)) NA_real_ else as.numeric(r$log_q_add_reverse),
        core_size = if (is.null(r$core_size)) NA_integer_ else as.integer(r$core_size),
        n_added = if (is.null(r$n_added)) NA_integer_ else as.integer(r$n_added),
        n_removed = if (is.null(r$n_removed)) NA_integer_ else as.integer(r$n_removed),
        changed_lcc = if (is.null(r$changed_lcc)) NA else isTRUE(r$changed_lcc),
        candidate_lccs_forward = if (is.null(r$candidate_lccs_forward)) NA_integer_ else as.integer(r$candidate_lccs_forward),
        candidate_lccs_reverse = if (is.null(r$candidate_lccs_reverse)) NA_integer_ else as.integer(r$candidate_lccs_reverse),
        proposal_failed = isTRUE(r$proposal_failed),
        infeasible = isTRUE(r$infeasible)
      )
      if (save_raw) raw_results[[result_idx]] <- r
    }
    if (si %% 5 == 0) cat(sprintf("  State %d/%d done\n", si, length(states)))
  }

  elapsed <- proc.time()[3] - t_start
  dt <- rbindlist(results)
  dt[, label := label]
  dt[, elapsed := elapsed]
  list(dt = dt, raw = raw_results)
}

set.seed(SEED)
baseline_result <- run_benchmark(benchmark_states, benchmark_k, N_CALLS_PER_STATE,
                                  "baseline", save_raw = TRUE)
baseline_dt <- baseline_result$dt
baseline_raw <- baseline_result$raw

# ============================================================================
# SECTION 5: Scorecard
# ============================================================================

print_scorecard <- function(dt, label = "") {
  cat(sprintf("\n%s\n", paste(rep("=", 70), collapse = "")))
  cat(sprintf("SCORECARD: %s  (%d total calls)\n", label, nrow(dt)))
  cat(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  n_total <- nrow(dt)

  # Overall rates
  accept_rate <- mean(dt$accepted) * 100
  zero_rev_rate <- mean(dt$reason == "zero_reverse_support", na.rm = TRUE) * 100
  proposal_fail_rate <- mean(dt$proposal_failed) * 100
  infeasible_rate <- mean(dt$infeasible, na.rm = TRUE) * 100
  feasible_rate <- mean(!dt$proposal_failed & !dt$infeasible, na.rm = TRUE) * 100

  cat(sprintf("\n  Acceptance rate:        %.2f%%\n", accept_rate))
  cat(sprintf("  Proposal failed rate:   %.2f%%\n", proposal_fail_rate))
  cat(sprintf("  zero_reverse_support:   %.2f%%\n", zero_rev_rate))
  cat(sprintf("  Infeasible rate:        %.2f%%\n", infeasible_rate))
  cat(sprintf("  Feasible rate:          %.2f%%\n", feasible_rate))

  # Sub-reason split
  zero_rev <- dt[reason == "zero_reverse_support"]
  if (nrow(zero_rev) > 0) {
    cat(sprintf("\n  zero_reverse_support sub-reasons (n=%d):\n", nrow(zero_rev)))
    sr_tab <- zero_rev[, .N, by = zero_reverse_subreason]
    for (i in seq_len(nrow(sr_tab))) {
      cat(sprintf("    %-25s: %d (%.1f%%)\n",
                  sr_tab$zero_reverse_subreason[i],
                  sr_tab$N[i],
                  sr_tab$N[i] / nrow(zero_rev) * 100))
    }
  }

  # Reason breakdown
  cat("\n  Reason breakdown:\n")
  reason_tab <- dt[, .N, by = reason][order(-N)]
  for (i in seq_len(nrow(reason_tab))) {
    cat(sprintf("    %-25s: %d (%.1f%%)\n",
                if (is.na(reason_tab$reason[i])) "<accepted/mh_rejected>" else reason_tab$reason[i],
                reason_tab$N[i],
                reason_tab$N[i] / n_total * 100))
  }

  # Accept prob (when finite)
  finite_ap <- dt[is.finite(accept_prob) & !is.na(accept_prob)]
  if (nrow(finite_ap) > 0) {
    cat(sprintf("\n  Accept prob (finite, n=%d):\n", nrow(finite_ap)))
    cat(sprintf("    Mean:   %.4f\n", mean(finite_ap$accept_prob)))
    cat(sprintf("    Median: %.4f\n", median(finite_ap$accept_prob)))
    cat(sprintf("    Min:    %.4f\n", min(finite_ap$accept_prob)))
    cat(sprintf("    Max:    %.4f\n", max(finite_ap$accept_prob)))
  }

  # Forward/reverse log-q
  finite_fwd <- dt[is.finite(log_q_forward) & !is.na(log_q_forward)]
  if (nrow(finite_fwd) > 0) {
    cat(sprintf("\n  log_q_forward (n=%d): mean=%.2f, median=%.2f, min=%.2f, max=%.2f\n",
                nrow(finite_fwd),
                mean(finite_fwd$log_q_forward), median(finite_fwd$log_q_forward),
                min(finite_fwd$log_q_forward), max(finite_fwd$log_q_forward)))
  }
  finite_rev <- dt[is.finite(log_q_reverse) & !is.na(log_q_reverse)]
  if (nrow(finite_rev) > 0) {
    cat(sprintf("  log_q_reverse (n=%d): mean=%.2f, median=%.2f, min=%.2f, max=%.2f\n",
                nrow(finite_rev),
                mean(finite_rev$log_q_reverse), median(finite_rev$log_q_reverse),
                min(finite_rev$log_q_reverse), max(finite_rev$log_q_reverse)))
  }

  # Component log_q decomposition (for proposals that reached MH)
  finite_both <- dt[is.finite(log_q_forward) & is.finite(log_q_reverse)]
  if (nrow(finite_both) > 0 &&
      "log_q_add_forward" %in% names(dt) &&
      any(!is.na(dt$log_q_add_forward))) {
    fb <- finite_both[!is.na(log_q_add_forward)]
    if (nrow(fb) > 0) {
      cat(sprintf("\n  Component log_q gaps (reverse - forward, n=%d):\n", nrow(fb)))
      core_gap <- fb$log_q_core_reverse - fb$log_q_core_forward
      lcc_gap <- fb$log_q_lcc_reverse - fb$log_q_lcc_forward
      add_gap <- fb$log_q_add_reverse - fb$log_q_add_forward
      cat(sprintf("    core:  mean=%6.2f  median=%6.2f  sd=%5.2f\n",
                  mean(core_gap), median(core_gap), sd(core_gap)))
      cat(sprintf("    lcc:   mean=%6.2f  median=%6.2f  sd=%5.2f\n",
                  mean(lcc_gap), median(lcc_gap), sd(lcc_gap)))
      cat(sprintf("    add:   mean=%6.2f  median=%6.2f  sd=%5.2f\n",
                  mean(add_gap), median(add_gap), sd(add_gap)))
    }
  }

  # Core size, n_added, n_removed
  has_core <- dt[!is.na(core_size)]
  if (nrow(has_core) > 0) {
    cat(sprintf("\n  Core size: mean=%.1f, median=%d\n",
                mean(has_core$core_size), as.integer(median(has_core$core_size))))
    cat(sprintf("  n_added:   mean=%.1f, median=%d\n",
                mean(has_core$n_added, na.rm = TRUE),
                as.integer(median(has_core$n_added, na.rm = TRUE))))
    cat(sprintf("  n_removed: mean=%.1f, median=%d\n",
                mean(has_core$n_removed, na.rm = TRUE),
                as.integer(median(has_core$n_removed, na.rm = TRUE))))
  }

  # LCC change rate
  has_lcc <- dt[!is.na(changed_lcc)]
  if (nrow(has_lcc) > 0) {
    cat(sprintf("  LCC change rate: %.1f%%\n", mean(has_lcc$changed_lcc) * 100))
  }

  # Breakdown by n_removed (key diagnostic)
  has_nrem <- dt[!is.na(n_removed)]
  if (nrow(has_nrem) > 0) {
    cat("\n  By n_removed:\n")
    nrem_summary <- has_nrem[, .(
      n = .N,
      accept_pct = mean(accepted) * 100,
      zero_rev_pct = mean(reason == "zero_reverse_support", na.rm = TRUE) * 100,
      infeasible_pct = mean(infeasible, na.rm = TRUE) * 100,
      mean_ap = mean(accept_prob[is.finite(accept_prob)], na.rm = TRUE),
      mean_log_q_gap = mean(
        log_q_reverse[is.finite(log_q_reverse)] - log_q_forward[is.finite(log_q_reverse)],
        na.rm = TRUE
      )
    ), by = n_removed][order(n_removed)]
    for (i in seq_len(nrow(nrem_summary))) {
      cat(sprintf("    rem=%-2d: n=%-4d accept=%.1f%% zero_rev=%.1f%% infeas=%.1f%% mean_ap=%.4f log_q_gap=%.1f\n",
                  nrem_summary$n_removed[i], nrem_summary$n[i],
                  nrem_summary$accept_pct[i], nrem_summary$zero_rev_pct[i],
                  nrem_summary$infeasible_pct[i],
                  if (is.nan(nrem_summary$mean_ap[i])) 0 else nrem_summary$mean_ap[i],
                  if (is.nan(nrem_summary$mean_log_q_gap[i])) -Inf else nrem_summary$mean_log_q_gap[i]))
    }
  }

  # Breakdown by starting k
  cat("\n  By starting k:\n")
  k_summary <- dt[, .(
    n = .N,
    accept_pct = mean(accepted) * 100,
    zero_rev_pct = mean(reason == "zero_reverse_support", na.rm = TRUE) * 100,
    proposal_fail_pct = mean(proposal_failed) * 100,
    mean_n_removed = mean(n_removed, na.rm = TRUE)
  ), by = k_start][order(k_start)]
  for (i in seq_len(nrow(k_summary))) {
    cat(sprintf("    k=%-2d: n=%-4d accept=%.1f%% zero_rev=%.1f%% prop_fail=%.1f%% mean_n_rem=%.1f\n",
                k_summary$k_start[i], k_summary$n[i],
                k_summary$accept_pct[i], k_summary$zero_rev_pct[i],
                k_summary$proposal_fail_pct[i], k_summary$mean_n_removed[i]))
  }

  # Runtime
  elapsed <- dt$elapsed[1]
  cat(sprintf("\n  Runtime: %.1f seconds (%.1f ms/call)\n",
              elapsed, elapsed / n_total * 1000))
}

print_scorecard(baseline_dt, "baseline")

# ============================================================================
# SECTION 6: Grid sweep over JOINT_CORE_P_KEEP x JOINT_ADD_WEIGHT_NOTHING
# ============================================================================

if (RUN_GRID_SWEEP && (length(GRID_P_KEEP) > 1 || length(GRID_ADD_WEIGHT_NOTHING) > 1)) {
  cat(sprintf("\n%s\n", paste(rep("=", 70), collapse = "")))
  cat("GRID SWEEP\n")
  cat(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  grid_results <- list()

  for (pk in GRID_P_KEEP) {
    for (aw in GRID_ADD_WEIGHT_NOTHING) {
      JOINT_CORE_P_KEEP <<- pk
      JOINT_ADD_WEIGHT_NOTHING <<- aw
      label <- sprintf("p_keep=%.2f,add_w=%.1f", pk, aw)
      cat(sprintf("\n--- %s ---\n", label))

      set.seed(SEED)
      res <- run_benchmark(benchmark_states, benchmark_k, N_CALLS_PER_STATE, label)
      grid_results[[label]] <- res$dt
    }
  }

  # Print comparison table
  cat(sprintf("\n%s\n", paste(rep("=", 70), collapse = "")))
  cat("GRID SWEEP COMPARISON\n")
  cat(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  cat(sprintf("%-28s | %7s | %7s | %9s | %9s | %9s | %8s\n",
              "config", "accept", "zero_rv", "infeas", "mean_ap", "n_rem", "log_gap"))
  cat(paste(rep("-", 95), collapse = ""), "\n")

  for (label in names(grid_results)) {
    dt <- grid_results[[label]]
    n_total <- nrow(dt)
    accept_pct <- mean(dt$accepted) * 100
    zero_rev_pct <- mean(dt$reason == "zero_reverse_support", na.rm = TRUE) * 100
    infeas_pct <- mean(dt$infeasible, na.rm = TRUE) * 100

    finite_ap <- dt[is.finite(accept_prob) & !is.na(accept_prob)]
    mean_ap <- if (nrow(finite_ap) > 0) mean(finite_ap$accept_prob) else NA_real_

    has_core <- dt[!is.na(n_removed)]
    mean_n_rem <- if (nrow(has_core) > 0) mean(has_core$n_removed) else NA_real_

    finite_rev <- dt[is.finite(log_q_reverse) & is.finite(log_q_forward)]
    mean_gap <- if (nrow(finite_rev) > 0) {
      mean(finite_rev$log_q_reverse - finite_rev$log_q_forward)
    } else NA_real_

    cat(sprintf("%-28s | %6.2f%% | %6.2f%% | %8.1f%% | %8.4f | %8.2f | %7.1f\n",
                label, accept_pct, zero_rev_pct, infeas_pct,
                if (is.na(mean_ap)) 0 else mean_ap,
                if (is.na(mean_n_rem)) 0 else mean_n_rem,
                if (is.na(mean_gap)) -Inf else mean_gap))
  }

  # Print per-config scorecards
  for (label in names(grid_results)) {
    print_scorecard(grid_results[[label]], label = label)
  }

  # Restore original values
  JOINT_CORE_P_KEEP <<- if (!is.null(OVERRIDE_JOINT_CORE_P_KEEP)) {
    OVERRIDE_JOINT_CORE_P_KEEP
  } else {
    0.5
  }
  JOINT_ADD_WEIGHT_NOTHING <<- if (!is.null(OVERRIDE_JOINT_ADD_WEIGHT_NOTHING)) {
    OVERRIDE_JOINT_ADD_WEIGHT_NOTHING
  } else {
    5.0
  }
}

cat("\n=== Done ===\n")
