# Distributional comparison harness for the compatible-LCC cache lifecycle fix.
# Runs a longer fixed-seed chain and prints summary statistics that must be
# statistically equivalent (not bit-identical) before/after the fix:
# per-kernel acceptance rates, capacity posterior moments, k distribution.
# Usage: Rscript dev/mcmc_testing/b4_distributional_check.R <Town> <n_steps> <label>

args <- commandArgs(trailingOnly = TRUE)
town <- if (length(args) >= 1) args[[1]] else "Braintree"
n_steps <- if (length(args) >= 2) as.integer(args[[2]]) else 10000L
label <- if (length(args) >= 3) args[[3]] else "run"

suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(targets))

store <- file.path("ext", paste0("_targets_", town))
pg <- tar_read_raw("parcel_graph_result", store = store)
cons <- tar_read_raw("target_spec", store = store)
sec <- hydrate_library(tar_read_raw("discovered_secondary_library", store = store))
lcc <- hydrate_library(tar_read_raw("discovered_lcc_library", store = store))
cfg <- tar_read_raw("parcel_multichain_config", store = store)[[1]]
cfg$n_steps <- n_steps
ist <- tar_read_raw("parcel_initial_states", store = store)[[1]]
if (is.list(ist) && length(ist) == 1 && is.null(names(ist))) ist <- ist[[1]]

set.seed(20260702L)
t0 <- proc.time()[["elapsed"]]
res <- run_parcel_mcmc(pg$parcel_graph, ist, cons, sec, lcc, cfg,
  parcel_assignments = pg$parcel_assignments, neighbor_cache = pg$neighbor_cache,
  verbose = FALSE)
el <- proc.time()[["elapsed"]] - t0

st <- res$stats
burn <- seq_len(n_steps %/% 2)  # discard first half
cap <- res$diagnostics$capacity_trajectory[-burn]
k <- res$diagnostics$n_secondaries_trajectory[-burn]
cat(sprintf("[%s] %s %d steps in %.1fs (%.2f ms/step)\n", label, town, n_steps, el, 1000 * el / n_steps))
for (m in st$move_type) {
  cat(sprintf("  %s: att=%d acc=%d rate=%.3f\n", m,
    st$n_attempted[st$move_type == m], st$n_accepted[st$move_type == m],
    st$n_accepted[st$move_type == m] / max(1, st$n_attempted[st$move_type == m])))
}
cat(sprintf("  capacity: mean=%.1f sd=%.1f | k: mean=%.2f sd=%.2f\n",
  mean(cap), sd(cap), mean(k), sd(k)))
kt <- table(k)
cat("  k table:", paste(names(kt), as.integer(kt), sep = ":", collapse = " "), "\n")
