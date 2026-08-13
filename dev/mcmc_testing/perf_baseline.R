# Fixed-seed sampler run harness for performance work.
#
# Loads a town's pre-built inputs from its targets store (ext/_targets_<Town>),
# runs run_parcel_mcmc directly with a fixed seed, and either
#   (a) saves the result as a regression fixture (mode = "save"), or
#   (b) re-runs and compares against the saved fixture bit-for-bit
#       (mode = "check").
# Optimizations that only cache or delta-maintain quantities must leave every
# trajectory, sample, and stat identical; this harness is the regression net.
#
# Usage (from the package root):
#   Rscript dev/mcmc_testing/perf_baseline.R save Norwood 2000
#   Rscript dev/mcmc_testing/perf_baseline.R check Norwood 2000
#   Rscript dev/mcmc_testing/perf_baseline.R profile Norwood 2000
#
# Fixtures land in ext/perf_fixtures/ (gitignored with the rest of ext/).

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) args[[1]] else "save"
town <- if (length(args) >= 2) args[[2]] else "Norwood"
n_steps <- if (length(args) >= 3) as.integer(args[[3]]) else 2000L
FIXED_SEED <- 20260702L

stopifnot(mode %in% c("save", "check", "profile"))

suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(targets))

# run_parcel_mcmc reads its tuning (SAMPLE_MAX_STORED, DEBUG_INVARIANT_CHECKS,
# ONLINE_* ...) from the `sampler_spec`-derived `config` object read from the
# store below — nothing needs sourcing here.

store <- file.path("ext", paste0("_targets_", town))
stopifnot(dir.exists(store))

parcel_graph_result <- tar_read_raw("parcel_graph_result", store = store)
target_spec         <- tar_read_raw("target_spec", store = store)
secondary_library   <- hydrate_library(tar_read_raw("discovered_secondary_library", store = store))
lcc_library         <- hydrate_library(tar_read_raw("discovered_lcc_library", store = store))
configs             <- tar_read_raw("parcel_multichain_config", store = store)
initial_states      <- tar_read_raw("parcel_initial_states", store = store)

config <- configs[[1]]
config$n_steps <- n_steps
initial_state <- initial_states[[1]]
if (is.list(initial_state) && length(initial_state) == 1 && is.null(names(initial_state))) {
  initial_state <- initial_state[[1]]
}

run_once <- function() {
  set.seed(FIXED_SEED)
  run_parcel_mcmc(
    parcel_graph          = parcel_graph_result$parcel_graph,
    initial_state         = initial_state,
    target_spec           = target_spec,
    secondary_library     = secondary_library,
    lcc_library           = lcc_library,
    sampler_spec          = config,
    parcel_assignments    = parcel_graph_result$parcel_assignments,
    neighbor_cache        = parcel_graph_result$neighbor_cache,
    verbose               = FALSE
  )
}

# Timing fields vary run to run; everything else must be bit-identical.
# data.tables are normalized to data.frames because identical() compares the
# .internal.selfref external pointer, which never matches between a fresh
# object and a deserialized fixture.
normalize <- function(x) {
  if (data.table::is.data.table(x)) return(as.data.frame(x))
  if (is.list(x)) return(lapply(x, normalize))
  x
}
strip_volatile <- function(result) {
  result$diagnostics$timing <- NULL
  result$diagnostics$elapsed_time <- NULL
  if (!is.null(result$stats) && "elapsed_seconds" %in% names(result$stats)) {
    result$stats$elapsed_seconds <- NULL
  }
  normalize(result)
}

fixture_dir <- file.path("ext", "perf_fixtures")
dir.create(fixture_dir, showWarnings = FALSE, recursive = TRUE)
fixture_path <- file.path(fixture_dir, sprintf("%s_%dsteps.rds", town, n_steps))

if (mode == "profile") {
  prof_path <- file.path(fixture_dir, sprintf("%s_%dsteps.Rprof", town, n_steps))
  Rprof(prof_path, line.profiling = TRUE, interval = 0.005)
  t0 <- proc.time()[["elapsed"]]
  result <- run_once()
  elapsed <- proc.time()[["elapsed"]] - t0
  Rprof(NULL)
  cat(sprintf("%s: %d steps in %.1fs (%.1f ms/step)\n",
              town, n_steps, elapsed, 1000 * elapsed / n_steps))
  print(head(summaryRprof(prof_path)$by.self, 25))
} else if (mode == "save") {
  t0 <- proc.time()[["elapsed"]]
  result <- run_once()
  elapsed <- proc.time()[["elapsed"]] - t0
  saveRDS(strip_volatile(result), fixture_path)
  cat(sprintf("%s: %d steps in %.1fs (%.1f ms/step); fixture -> %s\n",
              town, n_steps, elapsed, 1000 * elapsed / n_steps, fixture_path))
} else {
  stopifnot(file.exists(fixture_path))
  baseline <- readRDS(fixture_path)
  t0 <- proc.time()[["elapsed"]]
  result <- strip_volatile(run_once())
  elapsed <- proc.time()[["elapsed"]] - t0
  cat(sprintf("%s: %d steps in %.1fs (%.1f ms/step)\n",
              town, n_steps, elapsed, 1000 * elapsed / n_steps))
  if (identical(baseline, result)) {
    cat("BIT-IDENTICAL: PASS\n")
  } else {
    # Localize the divergence for debugging before failing.
    for (nm in union(names(baseline), names(result))) {
      if (!identical(baseline[[nm]], result[[nm]])) {
        cat("  differs:", nm, "\n")
        if (is.list(baseline[[nm]])) {
          for (sub in union(names(baseline[[nm]]), names(result[[nm]]))) {
            if (!identical(baseline[[nm]][[sub]], result[[nm]][[sub]])) {
              cat("    differs:", nm, "$", sub, "\n")
            }
          }
        }
      }
    }
    stop("BIT-IDENTICAL: FAIL")
  }
}
