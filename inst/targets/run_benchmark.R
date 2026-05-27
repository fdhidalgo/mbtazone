# run_benchmark.R
#
# Runs a fixed set of benchmark districts and archives reports + config for
# comparison across MCMC iterations. Each run creates a labelled folder in
# ext/benchmarks/ containing LLM reports, HTML reports, a config snapshot,
# and a summary markdown.
#
# Usage (from package root or via MBTAZONE_PACKAGE_ROOT):
#
#   BENCHMARK_LABEL <- "remove_cap_tolerance"   # set before sourcing
#   source("inst/targets/run_benchmark.R")
#
#   Or without a label (folder name defaults to timestamp):
#   source("inst/targets/run_benchmark.R")

library(data.table)
library(targets)

# ============================================================================
# BENCHMARK DISTRICT LIST
# Edit this to control which districts are included. Aim for a mix of
# community types and known problem cases alongside well-behaved ones.
# ============================================================================

BENCHMARK_DISTRICTS <- list(
  list(name = "Norwood",    type = "commuter_rail"),  # primary dev district
  list(name = "Brookline",  type = "rapid_transit"),   # large, known convergence issues
  list(name = "Beverly",    type = "commuter_rail"), # Three distinct valid areas, geographic mixing test
  list(name = "Everett",    type = "rapid_transit"), # Reasonably well behaved rapid_transit
  list(name = "Ayer",    type = "commuter_rail"), # Area constraint bites, converges above minimum
  list(name = "Ashland",   type = "commuter_rail"), # Commuter rail with centroid_x drift
  list(name = "Malden",    type = "rapid_transit"), # Poor convergence rapid transit
  list(name = "Bedford",   type = "adjacent"), # Very well behaved adjacent district
  list(name = "Auburn",   type = "adjacent") # Slighyly less well-behaved adjacent district
)

# ============================================================================
# SETUP
# ============================================================================

pkg_root <- Sys.getenv(
  "MBTAZONE_PACKAGE_ROOT",
  unset = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
)
owd <- setwd(pkg_root)
on.exit({
  setwd(owd)
  Sys.unsetenv("DISTRICT_NAME")
  Sys.unsetenv("DISTRICT_TYPE")
}, add = TRUE)

# Build run label: optional user prefix + timestamp
run_label <- if (exists("BENCHMARK_LABEL") && nzchar(BENCHMARK_LABEL)) {
  paste0(BENCHMARK_LABEL, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
} else {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

out_dir <- file.path("ext", "benchmarks", run_label)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cat(sprintf("\nBenchmark run: %s\nOutput:        %s\n\n", run_label, out_dir))

# ============================================================================
# INSTALL PACKAGE
# ============================================================================

r_bin <- file.path(R.home("bin"), "R")
install_status <- system2(r_bin, c("CMD", "INSTALL", "--no-multiarch", shQuote(pkg_root)))
if (!identical(install_status, 0L)) {
  stop("Failed to install current mbtazone package before running benchmark.")
}

# ============================================================================
# HELPERS
# ============================================================================

copy_if_exists <- function(src, dst) {
  if (file.exists(src)) { file.copy(src, dst, overwrite = TRUE); TRUE } else FALSE
}

# Source config files into an isolated environment and return it.
# The two files must be sourced in order: temp_targets_config.R defines
# CAPACITY_PRIOR_LAMBDA which temp_targets_parcel_config.R references.
read_config <- function() {
  env <- new.env(parent = emptyenv())
  tryCatch({
    sys.source("inst/targets/temp_targets_config.R",        envir = env)
    sys.source("inst/targets/temp_targets_parcel_config.R", envir = env)
  }, error = function(e) warning("Config read error: ", conditionMessage(e)))
  env
}

cfg_val <- function(cfg, name, suffix = "") {
  v <- get0(name, envir = cfg, inherits = FALSE)
  if (is.null(v)) "N/A" else paste0(as.character(v), suffix)
}

# ============================================================================
# RUN DISTRICTS
# ============================================================================

n <- length(BENCHMARK_DISTRICTS)
results <- vector("list", n)

for (i in seq_len(n)) {
  d     <- BENCHMARK_DISTRICTS[[i]]
  name  <- d$name
  type  <- d$type
  store <- file.path("ext", paste0("_targets_", gsub(" ", "_", name)))

  cat(sprintf("[%d/%d] %s (%s)\n", i, n, name, type))
  Sys.setenv(DISTRICT_NAME = name, DISTRICT_TYPE = type)

  run_status <- "failed"
  run_error  <- NA_character_

  tryCatch({
    tar_make(
      script   = "inst/targets/_targets.R",
      store    = store,
      reporter = "timestamp"
    )
    run_status <- "success"
    cat(sprintf("  v SUCCESS\n"))
  }, error = function(e) {
    run_error  <<- conditionMessage(e)
    cat(sprintf("  x FAILED: %s\n", run_error))
  })

  # Copy reports — use district_name directly (pipeline uses it without substitution)
  llm_src  <- file.path("ext", "llm_reports", paste0(name, "_mcmc_diagnostics_llm.md"))
  html_src <- file.path("ext", "reports",     paste0(name, "_mcmc_diagnostics.html"))

  llm_ok  <- copy_if_exists(llm_src,  file.path(out_dir, paste0(name, "_llm.md")))
  html_ok <- copy_if_exists(html_src, file.path(out_dir, paste0(name, "_report.html")))

  if (!llm_ok)  cat(sprintf("  ! LLM report not found:  %s\n", llm_src))
  if (!html_ok) cat(sprintf("  ! HTML report not found: %s\n", html_src))

  results[[i]] <- list(
    name    = name,
    type    = type,
    status  = run_status,
    error   = run_error,
    llm_ok  = llm_ok,
    html_ok = html_ok
  )
}

# ============================================================================
# CONFIG SNAPSHOT
# ============================================================================

cfg <- read_config()

# Copy raw config files verbatim for exact reproducibility
file.copy("inst/targets/temp_targets_config.R",
          file.path(out_dir, "temp_targets_config.R"),        overwrite = TRUE)
file.copy("inst/targets/temp_targets_parcel_config.R",
          file.path(out_dir, "temp_targets_parcel_config.R"), overwrite = TRUE)

# ============================================================================
# SUMMARY MARKDOWN
# ============================================================================

git_hash   <- tryCatch(trimws(system("git rev-parse --short HEAD", intern = TRUE)),
                        error = function(e) "unknown")
git_branch <- tryCatch(trimws(system("git branch --show-current",  intern = TRUE)),
                        error = function(e) "unknown")

n_success <- sum(sapply(results, function(r) r$status == "success"))

md <- c(
  paste0("# Benchmark: ", run_label),
  "",
  "## Run Info",
  "",
  paste0("- **Timestamp**: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- **Branch**:    ", git_branch),
  paste0("- **Commit**:    ", git_hash),
  paste0("- **Districts**: ", n_success, " / ", n, " succeeded"),
  "",
  "## Config",
  "",
  "| Parameter | Value |",
  "|:---|---:|",
  paste0("| MCMC_STEPS_MACRO             | ", cfg_val(cfg, "MCMC_STEPS_MACRO"),             " |"),
  paste0("| MCMC_BURN_IN                 | ", cfg_val(cfg, "MCMC_BURN_IN"),                 " |"),
  paste0("| CAPACITY_PRIOR_LAMBDA        | ", cfg_val(cfg, "CAPACITY_PRIOR_LAMBDA"),        " |"),
  paste0("| K_PRIOR_LAMBDA               | ", cfg_val(cfg, "K_PRIOR_LAMBDA"),               " |"),
  paste0("| SWAP_CAP_TOLERANCE           | ", cfg_val(cfg, "SWAP_CAP_TOLERANCE"),           " |"),
  paste0("| REPLACE_LCC_CAP_TOLERANCE    | ", cfg_val(cfg, "REPLACE_LCC_CAP_TOLERANCE"),
         if (!is.null(get0("REPLACE_LCC_CAP_TOLERANCE", cfg))) " *(config only — removed from kernel)*" else "", " |"),
  paste0("| MAX_DIST_FEET                | ", cfg_val(cfg, "MAX_DIST_FEET"),                " |"),
  paste0("| MIN_COVERAGE_RATIO           | ", cfg_val(cfg, "MIN_COVERAGE_RATIO"),           " |"),
  paste0("| LCC_LIBRARY_MAX_SIZE         | ", cfg_val(cfg, "LCC_LIBRARY_MAX_SIZE"),         " |"),
  paste0("| SEC_LIBRARY_MAX_SIZE         | ", cfg_val(cfg, "SEC_LIBRARY_MAX_SIZE"),         " |"),
  paste0("| TREE_LCC_N_TREES             | ", cfg_val(cfg, "TREE_LCC_N_TREES"),             " |"),
  paste0("| BFS_LCC_N_SAMPLES            | ", cfg_val(cfg, "BFS_LCC_N_SAMPLES"),            " |"),
  paste0("| LCC_BAND_MAX_ATTEMPTS        | ", cfg_val(cfg, "LCC_BAND_MAX_ATTEMPTS"),        " |"),
  paste0("| ENABLE_ONLINE_ENRICHMENT     | ", cfg_val(cfg, "ENABLE_ONLINE_ENRICHMENT"),     " |"),
  paste0("| DEBUG_INVARIANT_CHECKS       | ", cfg_val(cfg, "DEBUG_INVARIANT_CHECKS"),       " |"),
  "",
  "## Kernel Mix",
  "",
  "| Kernel | Weight |",
  "|:---|---:|",
  paste0("| p_replace_lcc           | ", cfg_val(cfg, "p_replace_lcc"),           " |"),
  paste0("| p_symmetric_birth_death | ", cfg_val(cfg, "p_symmetric_birth_death"), " |"),
  paste0("| p_lcc_local             | ", cfg_val(cfg, "p_lcc_local"),             " |"),
  paste0("| p_swap                  | ", cfg_val(cfg, "p_swap"),                  " |"),
  "",
  "## District Results",
  "",
  "| District | Type | Status | LLM | HTML | Notes |",
  "|:---|:---|:---|:---:|:---:|:---|"
)

for (r in results) {
  note <- if (!is.na(r$error)) paste0("`", substr(r$error, 1, 80), "`") else ""
  md <- c(md, sprintf(
    "| %s | %s | %s | %s | %s | %s |",
    r$name, r$type, r$status,
    if (isTRUE(r$llm_ok))  "v" else "x",
    if (isTRUE(r$html_ok)) "v" else "x",
    note
  ))
}

writeLines(md, file.path(out_dir, "run_summary.md"))

# ============================================================================
# CONSOLE SUMMARY
# ============================================================================

cat(sprintf("\n=== BENCHMARK COMPLETE ===\n"))
cat(sprintf("Run:     %s\n", run_label))
cat(sprintf("Output:  %s\n", out_dir))
cat(sprintf("Success: %d / %d\n", n_success, n))
cat(sprintf("Summary: %s\n", file.path(out_dir, "run_summary.md")))
