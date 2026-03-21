# run_grant_municipalities.R — Re-run MCMC pipeline for grant analysis municipalities
#
# Interactive usage (source in R console from package root):
#   source("dev/run_grant_municipalities.R")
#
#   # Run all 13 communities (invalidate chains only — reuses cached discovery)
#   run_all()
#
#   # Full rebuild from scratch
#   run_all(mode = "full")
#
#   # Run specific communities
#   run_communities(c("Peabody", "Norwood", "Beverly"))
#
#   # Run starting from a specific community
#   run_all(start_from = "Beverly")
#
#   # Run a single community
#   run_one("Peabody")
#   run_one("Peabody", mode = "full")
#
#   # Just prepare stores (invalidate chains) without running
#   prepare_stores()
#   prepare_stores(mode = "full")
#
#   # Install package (needed after code changes, before running)
#   install_package()
#
# Also works from command line:
#   Rscript dev/run_grant_municipalities.R                        # all, chains only
#   Rscript dev/run_grant_municipalities.R --full                 # all, full rebuild
#   Rscript dev/run_grant_municipalities.R --from Beverly         # start from Beverly
#   Rscript dev/run_grant_municipalities.R --only Peabody Norwood # specific communities

library(data.table)
library(targets)

# ---------------------------------------------------------------------------
# Configuration: 13 municipalities for grant analysis
# ---------------------------------------------------------------------------
GRANT_COMMUNITIES <- data.table(
  name = c("Lynnfield", "Northborough", "Ashland", "Burlington", "Duxbury",
           "Norwood", "Reading", "Peabody", "Marlborough", "Wellesley",
           "Beverly", "Brookline", "Malden"),
  type = c("adjacent", "adjacent", "commuter_rail", "adjacent", "adjacent",
           "commuter_rail", "commuter_rail", "adjacent", "adjacent", "commuter_rail",
           "commuter_rail", "rapid_transit", "rapid_transit")
)

# ---------------------------------------------------------------------------
# Helper: get store path for a community
# ---------------------------------------------------------------------------
store_path <- function(comm_name) {
  file.path("ext", paste0("_targets_", comm_name))
}

# ---------------------------------------------------------------------------
# Install package so crew workers see current code
# ---------------------------------------------------------------------------
install_package <- function() {
  cat("Installing package...\n")
  r_bin <- file.path(R.home("bin"), "R")
  status <- system2(
    r_bin,
    c("CMD", "INSTALL", "--no-multiarch", shQuote(normalizePath("."))),
    stdout = FALSE, stderr = FALSE
  )
  if (!identical(status, 0L)) stop("Failed to install mbtazone package.")
  cat("Package installed.\n")
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Prepare stores: clear locks and invalidate targets
# ---------------------------------------------------------------------------
#' @param communities data.table with name and type columns
#' @param mode "chains" (default) invalidates chain results only;
#'   "full" destroys entire store for full rebuild
prepare_stores <- function(communities = GRANT_COMMUNITIES, mode = "chains") {
  for (j in seq_len(nrow(communities))) {
    comm <- communities$name[j]
    store <- store_path(comm)

    # Remove stale lock
    lf <- file.path(store, "meta/process")
    if (file.exists(lf)) file.remove(lf)

    if (mode == "full") {
      tryCatch(tar_destroy(store = store), error = function(e) NULL)
      cat(comm, ": store destroyed (full rebuild)\n")
    } else {
      tryCatch(
        tar_delete(
          names = c("parcel_chain_results", "all_parcel_chain_results"),
          store = store
        ),
        error = function(e) NULL
      )
      cat(comm, ": chain results invalidated\n")
    }
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Run pipeline for a set of communities
# ---------------------------------------------------------------------------
run_pipeline <- function(communities, mode = "chains", install = TRUE) {
  if (install) install_package()
  prepare_stores(communities, mode = mode)

  results <- vector("list", nrow(communities))

  for (j in seq_len(nrow(communities))) {
    comm <- communities$name[j]
    comm_type <- communities$type[j]
    store <- store_path(comm)

    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat(sprintf(" [%d/%d] %s (%s)\n", j, nrow(communities), comm, comm_type))
    cat(" Time:", format(Sys.time()), "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")

    start_time <- Sys.time()

    tryCatch({
      Sys.setenv(DISTRICT_NAME = comm, DISTRICT_TYPE = comm_type)
      tar_make(store = store, script = "inst/targets/_targets.R")

      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      cat(sprintf("\n  SUCCESS: %s completed in %.1f minutes\n", comm, elapsed))
      results[[j]] <- data.table(
        name = comm, type = comm_type, status = "success",
        minutes = as.numeric(elapsed), error = NA_character_
      )
    }, error = function(e) {
      elapsed <- difftime(Sys.time(), start_time, units = "mins")
      cat(sprintf("\n  FAILED: %s after %.1f minutes\n", comm, elapsed))
      cat("  Error:", conditionMessage(e), "\n")
      results[[j]] <<- data.table(
        name = comm, type = comm_type, status = "failed",
        minutes = as.numeric(elapsed), error = conditionMessage(e)
      )
    })
  }

  Sys.unsetenv("DISTRICT_NAME")
  Sys.unsetenv("DISTRICT_TYPE")

  summary_dt <- rbindlist(results)
  cat("\n", paste(rep("=", 60), collapse = ""), "\n SUMMARY\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  for (k in seq_len(nrow(summary_dt))) {
    r <- summary_dt[k]
    cat(sprintf("  %-15s %-14s %s (%.1f min)\n",
                r$name, r$type, r$status, r$minutes))
  }
  cat(sprintf("\nSucceeded: %d / %d | Total: %.1f min\n",
              sum(summary_dt$status == "success"), nrow(summary_dt),
              sum(summary_dt$minutes)))

  invisible(summary_dt)
}

# ---------------------------------------------------------------------------
# Convenience wrappers
# ---------------------------------------------------------------------------

#' Run all 13 grant communities
run_all <- function(mode = "chains", start_from = NULL, install = TRUE) {
  communities <- GRANT_COMMUNITIES
  if (!is.null(start_from)) {
    idx <- match(start_from, communities$name)
    if (is.na(idx)) {
      stop(start_from, " not found. Available: ",
           paste(communities$name, collapse = ", "))
    }
    communities <- communities[idx:.N]
  }
  run_pipeline(communities, mode = mode, install = install)
}

#' Run specific communities by name
run_communities <- function(names, mode = "chains", install = TRUE) {
  communities <- GRANT_COMMUNITIES[name %in% names]
  if (nrow(communities) == 0) {
    stop("No matching communities. Available: ",
         paste(GRANT_COMMUNITIES$name, collapse = ", "))
  }
  # Preserve requested order
  communities <- communities[match(names, name, nomatch = 0)]
  run_pipeline(communities, mode = mode, install = install)
}

#' Run a single community
run_one <- function(comm_name, mode = "chains", install = TRUE) {
  run_communities(comm_name, mode = mode, install = install)
}

# ---------------------------------------------------------------------------
# Command-line mode: auto-run when called via Rscript
# ---------------------------------------------------------------------------
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  mode <- "chains"
  start_from <- NULL
  only_communities <- NULL

  i <- 1L
  while (i <= length(args)) {
    if (args[i] == "--full") {
      mode <- "full"
    } else if (args[i] == "--from" && i < length(args)) {
      i <- i + 1L
      start_from <- args[i]
    } else if (args[i] == "--only") {
      only_communities <- args[(i + 1L):length(args)]
      break
    }
    i <- i + 1L
  }

  if (!is.null(only_communities)) {
    run_communities(only_communities, mode = mode)
  } else {
    run_all(mode = mode, start_from = start_from)
  }
}
