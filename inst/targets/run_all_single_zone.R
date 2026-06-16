# run_all_single_zone.R
#
# Runs the targets pipeline for every municipality that has exactly one zoning
# district in its GeoPackage.  Multi-district municipalities are skipped until
# multi-zone support is implemented in the sampler.
#
# Run check_single_zone_districts.R first to generate
# inst/extdata/single_zone_communities.csv, or this script will derive the
# list itself at startup.
#
# Requires MBTAZONE_PIPELINE_DATA and MBTAZONE_RIGHT_OF_WAY.
# Run from package root, or set MBTAZONE_PACKAGE_ROOT.

library(data.table)
library(targets)

pkg_root <- Sys.getenv(
  "MBTAZONE_PACKAGE_ROOT",
  unset = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
)
owd <- setwd(pkg_root)
on.exit(setwd(owd), add = TRUE)

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = "")
if (!nzchar(pipeline_data_dir)) {
  stop(
    "MBTAZONE_PIPELINE_DATA is not set. ",
    "See inst/targets/.Renviron.example."
  )
}

r_bin <- file.path(R.home("bin"), "R")
install_status <- system2(
  r_bin,
  c("CMD", "INSTALL", "--no-multiarch", shQuote(pkg_root))
)
if (!identical(install_status, 0L)) {
  stop("Failed to install current mbtazone package before running targets.")
}

# Derive the single-zone list by counting districts in each gpkg.
# Uses sf::st_layers() so no geometry is loaded.
community_info <- fread("inst/extdata/community_info.csv")

cat("Scanning GeoPackages for district counts...\n")
gpkg_files <- list.files(pipeline_data_dir, pattern = "\\.gpkg$", full.names = TRUE)

district_counts <- rbindlist(lapply(gpkg_files, function(f) {
  gpkg_name <- tools::file_path_sans_ext(basename(f))
  community_name <- gsub("_", " ", gpkg_name)
  layers <- tryCatch(sf::st_layers(f), error = function(e) NULL)
  if (is.null(layers)) return(data.table(community_name = community_name, n_districts = NA_integer_))
  n <- layers$features[layers$name == "districts"]
  data.table(community_name = community_name, n_districts = if (length(n)) as.integer(n) else NA_integer_)
}))

# Filter: single district AND present in community_info
single_zone_names <- district_counts[n_districts == 1L, community_name]
districts <- community_info[community_name %in% single_zone_names]

skipped <- community_info[!community_name %in% single_zone_names]

cat(sprintf(
  "\nFound %d single-zone communities (skipping %d multi-zone or unmatched).\n\n",
  nrow(districts), nrow(skipped)
))
if (nrow(skipped) > 0) {
  cat("Skipped municipalities:\n")
  cat(paste0("  ", skipped$community_name, collapse = "\n"), "\n\n")
}

results <- vector("list", nrow(districts))

for (i in seq_len(nrow(districts))) {
  name  <- districts$community_name[i]
  type  <- districts$community_type[i]
  store <- paste0("ext/_targets_", gsub(" ", "_", name))

  cat("\n========================================\n")
  cat(sprintf("Running district %d/%d: %s (%s)\n", i, nrow(districts), name, type))
  cat("========================================\n")

  Sys.setenv(DISTRICT_NAME = name, DISTRICT_TYPE = type)

  tryCatch({
    tar_make(
      script   = "inst/targets/_targets.R",
      store    = store,
      reporter = "timestamp"
    )
    results[[i]] <- data.table(
      district_name = name,
      district_type = type,
      status        = "success",
      error         = NA_character_,
      timestamp     = Sys.time()
    )
    cat(sprintf("✓ SUCCESS: %s\n", name))

  }, error = function(e) {
    results[[i]] <<- data.table(
      district_name = name,
      district_type = type,
      status        = "failed",
      error         = conditionMessage(e),
      timestamp     = Sys.time()
    )
    cat(sprintf("✗ FAILED: %s\n  Error: %s\n", name, conditionMessage(e)))
  })
}

Sys.unsetenv("DISTRICT_NAME")
Sys.unsetenv("DISTRICT_TYPE")

summary_dt <- rbindlist(results)
dir.create("targets", recursive = TRUE, showWarnings = FALSE)
fwrite(summary_dt, "targets/single_zone_run_summary.csv")

cat("\n\n=== SUMMARY ===\n")
print(summary_dt[, .(district_name, district_type, status)])
cat(sprintf("\nSucceeded: %d / %d\n", sum(summary_dt$status == "success"), nrow(summary_dt)))
cat(sprintf("Failed:    %d / %d\n", sum(summary_dt$status == "failed"),  nrow(summary_dt)))
cat(sprintf("Skipped (multi-zone): %d\n", nrow(skipped)))
