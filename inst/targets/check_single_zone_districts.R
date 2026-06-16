# check_single_zone_districts.R
#
# Scans every municipality GeoPackage in MBTAZONE_PIPELINE_DATA and reports
# how many zoning districts each one contains.  Outputs a summary table and
# saves a CSV of single-zone communities suitable for use by
# run_all_single_zone.R.
#
# Usage (from package root):
#   Rscript inst/targets/check_single_zone_districts.R

library(data.table)

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = "")
if (!nzchar(pipeline_data_dir)) {
  stop(
    "MBTAZONE_PIPELINE_DATA is not set. ",
    "See inst/targets/.Renviron.example."
  )
}

community_info <- fread("inst/extdata/community_info.csv")

# Count districts for every gpkg found on disk using sf::st_layers(),
# which reads layer metadata only (no geometry loaded).
gpkg_files <- list.files(pipeline_data_dir, pattern = "\\.gpkg$", full.names = TRUE)
if (length(gpkg_files) == 0) {
  stop("No .gpkg files found in: ", pipeline_data_dir)
}

cat(sprintf("Found %d GeoPackage files in %s\n\n", length(gpkg_files), pipeline_data_dir))

counts <- rbindlist(lapply(gpkg_files, function(f) {
  gpkg_name <- tools::file_path_sans_ext(basename(f))
  community_name <- gsub("_", " ", gpkg_name)
  layers <- tryCatch(sf::st_layers(f), error = function(e) NULL)
  if (is.null(layers)) {
    return(data.table(
      gpkg_name      = gpkg_name,
      community_name = community_name,
      n_districts    = NA_integer_,
      error          = TRUE
    ))
  }
  n <- layers$features[layers$name == "districts"]
  if (length(n) == 0) n <- NA_integer_
  data.table(
    gpkg_name      = gpkg_name,
    community_name = community_name,
    n_districts    = as.integer(n),
    error          = FALSE
  )
}))

# Join with community_info to bring in community_type
counts <- merge(
  counts,
  community_info[, .(community_name, community_type)],
  by = "community_name",
  all.x = TRUE
)

# Report
cat("=== District count per municipality ===\n")
print(counts[order(n_districts), .(community_name, community_type, n_districts)])

cat("\n=== Summary ===\n")
cat(sprintf("  1 district  (single-zone): %d\n", sum(counts$n_districts == 1L, na.rm = TRUE)))
cat(sprintf("  2+ districts (multi-zone): %d\n", sum(counts$n_districts > 1L,  na.rm = TRUE)))
cat(sprintf("  NA / error:                %d\n", sum(is.na(counts$n_districts))))

# Save single-zone subset (must also be present in community_info)
single_zone <- counts[n_districts == 1L & !is.na(community_type)]
out_path <- "inst/extdata/single_zone_communities.csv"
fwrite(single_zone[, .(community_name, community_type)], out_path)
cat(sprintf("\nWrote %d single-zone communities to %s\n", nrow(single_zone), out_path))
