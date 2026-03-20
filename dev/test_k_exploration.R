# ============================================================================
# test_k_exploration.R — Quick single-chain run to check k-exploration
#
# Reuses pipeline objects from a completed Norwood targets run.
# Runs a single chain for N_STEPS steps and prints k-trace summary.
#
# Usage: source("dev/test_k_exploration.R") from the package root.
# ============================================================================

N_STEPS <- 2000L
SEED <- 99L
TARGETS_STORE <- "ext/_targets_Norwood"

cat("=== Loading pipeline objects ===\n")

library(targets)
library(igraph)
library(data.table)

source("inst/targets/temp_targets_config.R")
source("inst/targets/temp_targets_parcel_config.R")

devtools::load_all()

# Load from targets store
parcel_graph_result <- tar_read(parcel_graph_result, store = TARGETS_STORE)
constraints <- tar_read(constraints, store = TARGETS_STORE)
secondary_library <- tar_read(discovered_secondary_library, store = TARGETS_STORE)
lcc_library <- tar_read(discovered_lcc_library, store = TARGETS_STORE)

parcel_graph <- parcel_graph_result$parcel_graph
neighbor_cache <- parcel_graph_result$neighbor_cache

cat(sprintf("  Parcel graph: %d vertices, %d edges\n",
            vcount(parcel_graph), ecount(parcel_graph)))
cat(sprintf("  LCC library: %d blocks, Secondary library: %d blocks\n",
            lcc_library$n_blocks, secondary_library$n_blocks))
cat(sprintf("  Constraints: min_cap=%d, min_area=%.1f, min_lcc_frac=%.2f\n",
            constraints$min_capacity, constraints$min_area, constraints$min_lcc_fraction))

# Build initial state from a feasibility-checked LCC
cat("\n=== Building initial state ===\n")
feasibility <- tar_read(parcel_feasibility, store = TARGETS_STORE)

# Use the feasibility result to get a known-good initial state
# Or just grab one from existing chain results
all_chain_results <- tar_read(all_parcel_chain_results, store = TARGETS_STORE)
# Pick a state from an existing chain as starting point
for (chain_name in names(all_chain_results)) {
  chain <- all_chain_results[[chain_name]]
  if (!isTRUE(chain$initialization_failed) &&
      !is.null(chain$parcel_samples) &&
      length(chain$parcel_samples) > 0) {
    # Use the last sample from this chain
    init_sample <- chain$parcel_samples[[length(chain$parcel_samples)]]
    cat(sprintf("  Using final state from %s: k=%d, cap=%.0f\n",
                chain_name, length(init_sample$secondary_blocks),
                init_sample$total_capacity))
    break
  }
}

initial_state <- initialize_parcel_state(
  parcel_ids = init_sample$X,
  secondary_block_ids = init_sample$secondary_blocks,
  library = secondary_library,
  parcel_graph = parcel_graph
)

cat(sprintf("  Initial k=%d, total_capacity=%.0f\n",
            length(initial_state$secondary_blocks),
            initial_state$total_capacity))

# Configure single chain
config <- list(
  name = "k_exploration_test",
  seed = SEED,
  n_steps = N_STEPS,
  p_lcc_local = 0.12,
  p_symmetric_birth_death = 0.305,
  p_swap = 0.12,
  p_replace_lcc = 0.455,
  use_lifted = FALSE
)

parcel_assignments <- data.table(parcel_id = V(parcel_graph)$name)

cat(sprintf("\n=== Running chain: %d steps ===\n", N_STEPS))
t_start <- proc.time()[3]

set.seed(SEED)
result <- run_parcel_mcmc(
  parcel_graph = parcel_graph,
  initial_state = initial_state,
  constraints = constraints,
  secondary_library = secondary_library,
  lcc_library = lcc_library,
  config = config,
  parcel_assignments = parcel_assignments,
  neighbor_cache = neighbor_cache,
  enable_online_enrichment = ENABLE_ONLINE_ENRICHMENT,
  enrichment_interval = ONLINE_ENRICHMENT_INTERVAL,
  max_online_entries = ONLINE_MAX_ENTRIES,
  enrichment_burn_in = ENRICHMENT_BURN_IN,
  max_stored_samples = SAMPLE_MAX_STORED,
  store_lcc_signatures = STORE_LCC_SIGNATURES,
  verbose = TRUE
)

elapsed <- proc.time()[3] - t_start
cat(sprintf("\n=== Chain complete: %.1f seconds ===\n", elapsed))

# Extract k trace from samples
samples <- result$parcel_samples
k_trace <- vapply(samples, function(s) length(s$secondary_blocks), integer(1))
cap_trace <- vapply(samples, function(s) s$total_capacity, numeric(1))

cat(sprintf("\n=== k-trace summary ===\n"))
cat(sprintf("  Samples: %d\n", length(k_trace)))
cat(sprintf("  k range: [%d, %d]\n", min(k_trace), max(k_trace)))
cat(sprintf("  k mean:  %.1f\n", mean(k_trace)))
cat(sprintf("  k median: %d\n", as.integer(median(k_trace))))

cat(sprintf("\n  k distribution:\n"))
k_tab <- table(k_trace)
for (i in seq_along(k_tab)) {
  k_val <- as.integer(names(k_tab)[i])
  n <- as.integer(k_tab[i])
  pct <- n / length(k_trace) * 100
  bar <- paste(rep("#", max(1, round(pct))), collapse = "")
  cat(sprintf("    k=%-2d: %4d (%5.1f%%) %s\n", k_val, n, pct, bar))
}

cat(sprintf("\n  Capacity range: [%.0f, %.0f]\n", min(cap_trace), max(cap_trace)))
cat(sprintf("  Capacity mean:  %.0f\n", mean(cap_trace)))

# Print move stats
cat(sprintf("\n=== Move statistics ===\n"))
print(result$stats)

# k transitions
cat(sprintf("\n=== k transitions (first 100 samples) ===\n"))
n_show <- min(100, length(k_trace))
k_short <- k_trace[1:n_show]
# Show as a compact string
cat("  k: ", paste(k_short, collapse = " "), "\n")

# Check joint_refresh diagnostics
if (!is.null(result$diagnostics$joint_refresh_reasons)) {
  cat(sprintf("\n=== Joint refresh diagnostics ===\n"))
  jr_reasons <- result$diagnostics$joint_refresh_reasons
  reason_tab <- table(jr_reasons)
  cat("  Reasons:\n")
  for (i in seq_along(reason_tab)) {
    cat(sprintf("    %-25s: %d\n", names(reason_tab)[i], as.integer(reason_tab[i])))
  }
}

cat("\n=== Done ===\n")
