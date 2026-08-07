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
  list(name = "Worcester",  type = "commuter_rail"), # Largest single-zone town; stress-tests unique-LCC cap / tree-discovery perf
  list(name = "Salem",      type = "commuter_rail"),
  list(name = "Everett",    type = "rapid_transit"),
  list(name = "Medford",    type = "rapid_transit"),
  list(name = "Maynard",    type = "adjacent"), # Known quantity from density-precompute bench
  list(name = "Shrewsbury", type = "adjacent"),
  list(name = "Groveland",  type = "adjacent_small_town"), # Density-aware fallback edge case (0 LCCs pre-fix)
  list(name = "Harvard",    type = "adjacent_small_town") # Known quantity from LCC discovery bench
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

# Read config files into an isolated environment.
# parse()+eval() is used instead of source()/sys.source() to avoid targets
# interactions. parent=baseenv() so base functions (list, c, etc.) are found.
read_config <- function() {
  env <- new.env(parent = baseenv())
  for (f in c("inst/targets/temp_targets_config.R",
              "inst/targets/temp_targets_parcel_config.R")) {
    tryCatch(
      eval(parse(file = f), envir = env),
      error = function(e) warning("Config read error in ", f, ": ", conditionMessage(e))
    )
  }
  env
}

cfg_val <- function(cfg, name) {
  v <- get0(name, envir = cfg, inherits = FALSE)
  if (is.null(v)) "N/A" else as.character(v)
}

# Format a duration in seconds as "Hh MMm SSs" (whole runs are hours long).
format_elapsed <- function(secs) {
  secs <- round(secs)
  sprintf("%dh %02dm %02ds", secs %/% 3600, (secs %% 3600) %/% 60, secs %% 60)
}

# ============================================================================
# RUN DISTRICTS
# ============================================================================

n <- length(BENCHMARK_DISTRICTS)
results <- vector("list", n)
benchmark_start <- Sys.time()

for (i in seq_len(n)) {
  d     <- BENCHMARK_DISTRICTS[[i]]
  name  <- d$name
  type  <- d$type
  store <- file.path("ext", paste0("_targets_", gsub(" ", "_", name)))

  cat(sprintf("[%d/%d] %s (%s)\n", i, n, name, type))
  Sys.setenv(DISTRICT_NAME = name, DISTRICT_TYPE = type)

  run_status  <- "failed"
  run_error   <- NA_character_
  district_start <- Sys.time()

  tryCatch({
    tar_make(
      script   = "inst/targets/_targets.R",
      store    = store,
      reporter = "timestamp"
    )
    run_status <- "success"
  }, error = function(e) {
    run_error  <<- conditionMessage(e)
  })

  elapsed_s <- as.numeric(difftime(Sys.time(), district_start, units = "secs"))
  if (run_status == "success") {
    cat(sprintf("  v SUCCESS (%s)\n", format_elapsed(elapsed_s)))
  } else {
    cat(sprintf("  x FAILED (%s): %s\n", format_elapsed(elapsed_s), run_error))
  }

  # Copy reports (pipeline writes using district_name verbatim)
  llm_src  <- file.path(
    "ext", "llm_reports", paste0(name, "_mcmc_diagnostics_llm.md")
  )
  html_src <- file.path(
    "ext", "reports", paste0(name, "_mcmc_diagnostics.html")
  )

  llm_ok  <- copy_if_exists(
    llm_src,  file.path(out_dir, paste0(name, "_llm.md"))
  )
  html_ok <- copy_if_exists(
    html_src, file.path(out_dir, paste0(name, "_report.html"))
  )

  if (!llm_ok)  cat(sprintf("  ! LLM report not found:  %s\n", llm_src))
  if (!html_ok) cat(sprintf("  ! HTML report not found: %s\n", html_src))

  results[[i]] <- list(
    name      = name,
    type      = type,
    status    = run_status,
    error     = run_error,
    llm_ok    = llm_ok,
    html_ok   = html_ok,
    elapsed_s = elapsed_s
  )
}

benchmark_elapsed_s <- as.numeric(difftime(Sys.time(), benchmark_start, units = "secs"))

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
  paste0("- **Elapsed**:   ", format_elapsed(benchmark_elapsed_s)),
  "",
  "## Config",
  "",
  "| Parameter | Value |",
  "|:---|---:|",
  paste0("| MCMC_STEPS_MACRO             | ", cfg_val(cfg, "MCMC_STEPS_MACRO"),             " |"),
  paste0("| MCMC_BURN_IN                 | ", cfg_val(cfg, "MCMC_BURN_IN"),                 " |"),
  paste0("| ROW_FILL_M                   | ", cfg_val(cfg, "ROW_FILL_M"),                   " |"),
  paste0("| CAPACITY_PRIOR_LAMBDA        | ", cfg_val(cfg, "CAPACITY_PRIOR_LAMBDA"),        " |"),
  paste0("| K_PRIOR_LAMBDA               | ", cfg_val(cfg, "K_PRIOR_LAMBDA"),               " |"),
  paste0("| SWAP_CAP_TOLERANCE           | ", cfg_val(cfg, "SWAP_CAP_TOLERANCE"),           " |"),
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
  "## District Results",
  "",
  "| District | Type | Status | Elapsed | LLM | HTML | Notes |",
  "|:---|:---|:---|---:|:---:|:---:|:---|"
)

for (r in results) {
  note <- if (!is.na(r$error)) paste0("`", substr(r$error, 1, 80), "`") else ""
  md <- c(md, sprintf(
    "| %s | %s | %s | %s | %s | %s | %s |",
    r$name, r$type, r$status, format_elapsed(r$elapsed_s),
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
cat(sprintf("Elapsed: %s\n", format_elapsed(benchmark_elapsed_s)))
cat(sprintf("Summary: %s\n", file.path(out_dir, "run_summary.md")))
