# benchmark_density_check.R
#
# Benchmarks two approaches to the MCMC density check:
#
#   A. Current (fast, approximate):
#      density = capacity / sum(ACRES)
#
#   B. Correct (GIS-based, exact per state methodology):
#      density = capacity / (st_area(st_union(parcels)) - st_area(st_intersection(union, deductions)))
#
# For each community, simulates N_ITER MCMC-like candidate states (random
# parcel subsets of varying sizes) and times both approaches. Reports per-
# iteration timings and projected cost over a full MCMC run.
#
# Run from the package root. Requires env vars:
#   MBTAZONE_PIPELINE_DATA  — directory of .gpkg files
#   MBTAZONE_DENSITY_DEDUCTIONS — path to Density_Denominator_Deductions.shp

library(sf)
library(data.table)

# ---- Configuration ----------------------------------------------------------

COMMUNITIES <- c("Groveland", "Norwell", "Carlisle")  # communities to benchmark
N_ITER      <- 100    # candidate states to time per community
N_CHAINS    <- 4L     # chains run in parallel (matches pipeline default)
N_STEPS     <- 20000L # steps per chain (MCMC_STEPS_MACRO in temp_targets_config.R)
# Total MCMC density checks ≈ N_CHAINS * N_STEPS (once per step in kernels/soft constraints)
# plus additional calls in find_valid_cuts during LCC discovery initialization
N_MCMC      <- N_CHAINS * N_STEPS
SET_SIZES   <- c(5, 15, 30, 50)  # parcel subset sizes to test (simulates LCC/state sizes)
SEED        <- 42

# ---- Paths ------------------------------------------------------------------

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")

if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")
if (!file.exists(density_ded_path)) stop("Deductions shapefile not found: ", density_ded_path)

# ---- Load deductions layer once (shared across all communities) -------------

cat("Loading density denominator deductions shapefile...\n")
t0 <- proc.time()
deductions <- sf::st_read(density_ded_path, quiet = TRUE)
deductions <- sf::st_transform(deductions, 26986)
deductions <- sf::st_make_valid(deductions)
cat(sprintf("  Loaded in %.2f s  (%d features)\n\n", (proc.time() - t0)[3], nrow(deductions)))

# ---- Benchmark function -----------------------------------------------------

benchmark_community <- function(community) {
  gpkg <- file.path(pipeline_data_dir, paste0(gsub(" ", "_", community), ".gpkg"))
  if (!file.exists(gpkg)) {
    message("Skipping ", community, ": GeoPackage not found.")
    return(NULL)
  }

  parcels   <- sf::st_read(gpkg, layer = "parcels",   quiet = TRUE)
  districts <- sf::st_read(gpkg, layer = "districts", quiet = TRUE)

  # Use all parcels (not just in-district) to simulate MCMC proposing new members
  parcels <- sf::st_make_valid(parcels)

  # Clip deductions to community bounding box (speeds up intersection)
  bbox_poly  <- sf::st_as_sfc(sf::st_bbox(parcels))
  sf::st_crs(bbox_poly) <- sf::st_crs(parcels)
  ded_local  <- sf::st_intersection(deductions, bbox_poly)

  n_parcels  <- nrow(parcels)
  capacity   <- parcels$final_lot_multi_family_unit_capacity
  capacity[is.na(capacity)] <- 0

  set.seed(SEED)
  results <- rbindlist(lapply(SET_SIZES, function(k) {
    if (k > n_parcels) return(NULL)

    # --- Approach A: sum(ACRES) ----------------------------------------------
    times_a <- numeric(N_ITER)
    for (i in seq_len(N_ITER)) {
      idx <- sample(n_parcels, k)
      t0  <- proc.time()
      sub_area <- sum(parcels$ACRES[idx], na.rm = TRUE)
      sub_cap  <- sum(capacity[idx])
      density_a <- sub_cap / sub_area
      times_a[i] <- (proc.time() - t0)[3]
    }

    # --- Approach B: union geometry - deductions intersection ----------------
    times_b <- numeric(N_ITER)
    for (i in seq_len(N_ITER)) {
      idx <- sample(n_parcels, k)
      t0  <- proc.time()
      sub_geom   <- sf::st_union(parcels[idx, ])
      ded_isect  <- sf::st_intersection(ded_local, sub_geom)
      ded_area   <- if (nrow(ded_isect) > 0) sum(as.numeric(sf::st_area(ded_isect))) else 0
      sub_area_m2 <- as.numeric(sf::st_area(sub_geom))
      denom      <- sub_area_m2 / 4047 - ded_area / 4047
      sub_cap    <- sum(capacity[idx])
      density_b  <- sub_cap / denom
      times_b[i] <- (proc.time() - t0)[3]
    }

    data.table(
      community   = community,
      n_parcels_pool = n_parcels,
      subset_size = k,
      mean_ms_A   = round(mean(times_a) * 1000, 3),
      mean_ms_B   = round(mean(times_b) * 1000, 3),
      slowdown    = round(mean(times_b) / pmax(mean(times_a), 1e-9), 1),
      proj_hrs_B  = round(mean(times_b) * N_MCMC / 3600, 2)
    )
  }))

  results
}

# ---- Run benchmarks ---------------------------------------------------------

cat(sprintf("Benchmarking %d communities × %d subset sizes × %d iterations each\n\n",
            length(COMMUNITIES), length(SET_SIZES), N_ITER))

all_results <- rbindlist(lapply(COMMUNITIES, function(c) {
  cat("---", c, "---\n")
  res <- benchmark_community(c)
  print(res)
  cat("\n")
  res
}))

# ---- Summary ----------------------------------------------------------------

cat("=============================================================\n")
cat("SUMMARY\n")
cat("=============================================================\n")
cat(sprintf("Projected N_MCMC iterations: %s\n\n", format(N_MCMC, big.mark=",")))
print(all_results[, .(
  community, subset_size,
  mean_ms_A, mean_ms_B, slowdown,
  proj_hrs_B
)])

cat("\nOverall mean per-iteration time (approach B):",
    round(mean(all_results$mean_ms_B), 2), "ms\n")
cat("Overall mean slowdown vs approach A:          ",
    round(mean(all_results$slowdown), 1), "x\n")
cat("Projected extra time per million iterations:  ",
    round(mean(all_results$mean_ms_B) * 1e6 / 3600, 2), "hrs\n")
