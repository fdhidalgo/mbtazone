#!/usr/bin/env Rscript
# ============================================================================
# Density-deduction precompute speed bench — fixed-input, TOLERANCE gate.
#
# Purpose: give a `/goal` loop a signal for behavior-preserving speedups of the
# single most expensive spatial step in the compliance workflow — the per-parcel
# density-deduction overlap computed by `precompute_spatial_attributes(...,
# density_deductions = )`. On Maynard (3,467 parcels) ~95% of the time is one
# line: `st_intersection(parcels, deduction_union)` (R/gis_operations.R), which
# intersects every parcel against ONE unioned statewide multipolygon — defeating
# the spatial index. At Cambridge scale (12,867 parcels) this is the ~12-minute
# step the compliance batch bench deliberately skips. The classic fix — prefilter
# parcels/deductions by bounding-box (st_intersects / st_filter) before the heavy
# intersection, or keep the deductions un-unioned so the STRtree stays active —
# changes the floating-point aggregation order slightly, so a BIT-IDENTICAL hash
# would be too strict here.
#
# THE GATE IS LOOSER THAN THE OTHER BENCHES. This path is RNG-free but produces
# geometry, so the gate is a per-parcel TOLERANCE check, not a hash match:
#   WITHIN_TOL    max |this_area - baseline_area| over all parcels <= TOL_SQFT
# The reference per-parcel vector lives in density_baseline.rds (element-wise
# compare); density_baseline.txt holds human-readable summaries. TOL_SQFT = 1.0
# matches the 1-sq-ft tolerance the package's own regression tests use.
# DETERMINISTIC (the two runs agree EXACTLY) still guards against an optimization
# that introduces nondeterminism (e.g. parallel reduction order).
#
# It loads one municipality once and the statewide deduction layer once (untimed
# setup), then times the density precompute TWICE and reports:
#   ELAPSED       min wall-clock of one density precompute (s)
#   N_AFFECTED    parcels with non-zero deduction (must match baseline exactly)
#   TOTAL_SQFT    sum of per-parcel deduction area (reported; compared within tol)
#   MAX_ABS_DIFF  max per-parcel |area - baseline| (the tolerance signal)
#   WITHIN_TOL    MAX_ABS_DIFF <= TOL_SQFT (the correctness gate)
#   SPEEDUP       baseline_elapsed / ELAPSED
#
# Maynard (smallest municipality, ~99 s/run) keeps the loop tractable while
# exercising the exact hot path; the optimization helps at every municipality
# size.
#
# Usage:
#   Rscript dev/bench/density_bench.R              # measure + compare to baseline
#   Rscript dev/bench/density_bench.R --capture    # (re)write density_baseline.{txt,rds}
#
# SCOPE for an optimizer: the density branch of precompute_spatial_attributes()
# and, if shared, calculate_density_denominator() in R/gis_operations.R. Do NOT
# edit this file, density_baseline.{txt,rds}, TOL_SQFT, the municipality, or the
# deduction layer while a speed goal is active — they define the measurement.
# (Note: the COMPLIANCE bench treats precompute_spatial_attributes as fixed input;
# here it IS the thing under test. The two scopes are disjoint — station overlap
# vs. density overlap — and do not conflict.)
# ============================================================================

suppressMessages({ library(sf) })

# --- fixed measurement parameters (treat as frozen) -------------------------
PKG_ROOT   <- Sys.getenv("MBTAZONE_PACKAGE_ROOT", unset = "/home/dhidalgo/projects/mbtazone")
PARCEL_ZIP <- file.path(PKG_ROOT, "inst/extdata/parcels/174_MAYNARD_basic.zip")
COMMUNITY  <- "Maynard"
TOL_SQFT   <- 1.0      # per-parcel tolerance (matches package regression tests)
RUNS       <- 2L       # repeats; ELAPSED = min, runs must agree exactly
BASELINE   <- file.path(PKG_ROOT, "dev", "bench", "density_baseline.txt")
BASELINE_RDS <- file.path(PKG_ROOT, "dev", "bench", "density_baseline.rds")

setwd(PKG_ROOT)
suppressMessages(pkgload::load_all(PKG_ROOT, quiet = TRUE,
                                   helpers = FALSE, attach_testthat = FALSE))

# --- one-time setup (untimed): load parcels + statewide deduction layer ------
stopifnot("parcel zip missing" = file.exists(PARCEL_ZIP))
parcels    <- load_municipality(PARCEL_ZIP, community_name = COMMUNITY)
deductions <- load_density_deductions()

# --- one density precompute (the hot spatial path) --------------------------
# Returns the per-parcel density_deduction_area vector in parcel (input) order.
run_once <- function() {
  res <- suppressMessages(precompute_spatial_attributes(
    parcels           = parcels,
    station_areas     = NULL,
    density_deductions = deductions,
    verbose           = FALSE
  ))
  as.numeric(res$density_deduction_area)
}

elapsed <- numeric(RUNS); vecs <- vector("list", RUNS)
for (i in seq_len(RUNS)) {
  t <- system.time(v <- run_once())[["elapsed"]]
  elapsed[i] <- t; vecs[[i]] <- v
}

deterministic <- length(unique(vapply(vecs, rlang::hash, ""))) == 1L
this_elapsed  <- min(elapsed)
this_vec      <- vecs[[1]]
n_affected    <- sum(this_vec > 0)
total_sqft    <- sum(this_vec)

# --- capture mode: write baseline (txt + rds) and exit ----------------------
args <- commandArgs(trailingOnly = TRUE)
if ("--capture" %in% args) {
  git_hash <- tryCatch(trimws(system("git rev-parse --short HEAD", intern = TRUE)),
                       error = function(e) "unknown")
  saveRDS(this_vec, BASELINE_RDS)
  writeLines(c(
    sprintf("baseline_n_affected=%d", n_affected),
    sprintf("baseline_total_sqft=%.6f", total_sqft),
    sprintf("baseline_elapsed=%.3f", this_elapsed),
    sprintf("tol_sqft=%.6f", TOL_SQFT),
    sprintf("municipality=%s", COMMUNITY),
    sprintf("n_parcels=%d", length(this_vec)),
    sprintf("git_commit=%s", git_hash)
  ), BASELINE)
  cat(sprintf("\nBASELINE CAPTURED -> %s (+ .rds)\n", BASELINE))
  cat(sprintf("baseline_n_affected=%d\nbaseline_total_sqft=%.3f\nbaseline_elapsed=%.3f\ndeterministic=%s\n",
              n_affected, total_sqft, this_elapsed, deterministic))
  quit(save = "no")
}

# --- compare mode -----------------------------------------------------------
base <- if (file.exists(BASELINE)) {
  kv <- strsplit(readLines(BASELINE), "=")
  setNames(vapply(kv, `[`, "", 2), vapply(kv, `[`, "", 1))
} else NULL
base_vec <- if (file.exists(BASELINE_RDS)) readRDS(BASELINE_RDS) else NULL

cat(sprintf("\n=== DENSITY PRECOMPUTE BENCH (%s, runs=%d, tol=%.1f sqft) ===\n",
            COMMUNITY, RUNS, TOL_SQFT))
cat(sprintf("ELAPSED: %.2f\n", this_elapsed))
cat(sprintf("N_AFFECTED: %d\n", n_affected))
cat(sprintf("TOTAL_SQFT: %.3f\n", total_sqft))
cat(sprintf("DETERMINISTIC: %s\n", deterministic))
if (!is.null(base) && !is.null(base_vec)) {
  base_el <- as.numeric(base[["baseline_elapsed"]])
  if (length(base_vec) != length(this_vec)) {
    cat(sprintf("WITHIN_TOL: FALSE (length mismatch: %d vs baseline %d)\n",
                length(this_vec), length(base_vec)))
  } else {
    max_abs_diff <- max(abs(this_vec - base_vec))
    within_tol   <- max_abs_diff <= TOL_SQFT
    n_match      <- n_affected == as.integer(base[["baseline_n_affected"]])
    cat(sprintf("BASELINE_N_AFFECTED: %s\n", base[["baseline_n_affected"]]))
    cat(sprintf("BASELINE_ELAPSED: %.2f\n", base_el))
    cat(sprintf("MAX_ABS_DIFF: %.6g\n", max_abs_diff))
    cat(sprintf("N_AFFECTED_MATCH: %s\n", n_match))
    cat(sprintf("WITHIN_TOL: %s\n", within_tol && n_match))
    cat(sprintf("SPEEDUP: %.3fx\n", base_el / this_elapsed))
  }
  if (!deterministic)
    cat("NOTE: non-deterministic output across runs — this is a correctness bug.\n")
} else {
  cat("BASELINE: (none — run with --capture first)\n")
}
