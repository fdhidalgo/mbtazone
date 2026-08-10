# diag_geom_source_error.R
#
# Checks whether the district-vs-Excel denominator error is concentrated in
# the Atlas gap-fill towns (~11) and supplement towns (~5), whose district
# polygons were NOT submitted by the municipality — they use MassGIS Atlas or
# hand-recovered geometry that may differ from what the community used in ArcGIS.
#
# Method:
#   1. Read district_geom_source from each single-zone GeoPackage's districts layer.
#   2. Compute district denominator = polygon area − GIS deductions within district.
#   3. Read Excel denominator from each community's CM workbook (Summary sheet).
#   4. Compare error (district_minus_excel) by district_geom_source group.
#
# Run from the mbtazone package root on the server:
#   Rscript dev/mcmc_testing/diag_geom_source_error.R
#
# Required env vars:
#   MBTAZONE_PIPELINE_DATA       — directory of per-community .gpkg files
#   MBTAZONE_DENSITY_DEDUCTIONS  — path to Density_Denominator_Deductions.shp
#   MBTAZONE_EXCEL_MODELS        — path to mbta_district_models/ directory

library(sf)
library(data.table)
library(readxl)

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")
excel_models_dir  <- Sys.getenv("MBTAZONE_EXCEL_MODELS")

if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")
if (!nzchar(excel_models_dir))  stop("MBTAZONE_EXCEL_MODELS not set.")

# ---- Load deductions once ---------------------------------------------------
cat("Loading GIS deductions...\n")
deductions_all <- sf::st_make_valid(
  sf::st_transform(sf::st_read(density_ded_path, quiet = TRUE), 26986)
)
cat(sprintf("  %d features.\n\n", nrow(deductions_all)))

# ---- Helpers ----------------------------------------------------------------
extract_excel_denom <- function(community_name, models_dir) {
  community_clean <- gsub("_", " ", community_name)
  # First try a subdirectory named after the community
  dirs <- list.dirs(models_dir, full.names = TRUE, recursive = FALSE)
  idx  <- which(tolower(basename(dirs)) == tolower(community_clean))
  if (length(idx) > 0) {
    xlsx_files <- list.files(dirs[idx[1]], pattern = "\\.xlsx$",
                             full.names = TRUE, ignore.case = TRUE)
  } else {
    all_xlsx <- list.files(models_dir, pattern = "\\.xlsx$",
                           full.names = TRUE, ignore.case = TRUE)
    pat <- paste0(
      "^",
      gsub("([.()^$*+?{}|\\[\\]])", "\\\\\\1", community_clean),
      "\\s*-\\s*CM"
    )
    xlsx_files <- all_xlsx[grepl(pat, basename(all_xlsx), ignore.case = TRUE)]
  }
  xlsx_files <- xlsx_files[!grepl("^~\\$", basename(xlsx_files))]
  for (f in xlsx_files) {
    tryCatch({
      if (!"Summary" %in% readxl::excel_sheets(f)) next
      s   <- readxl::read_excel(f, sheet = "Summary", col_names = FALSE,
                                .name_repair = "minimal")
      row <- grep("District Density Denominator", as.character(s[[1]]),
                  ignore.case = TRUE)[1]
      if (!is.na(row)) return(suppressWarnings(as.numeric(s[[2]][row])))
    }, error = function(e) NULL)
  }
  NA_real_
}

clip_to_bbox <- function(sf_obj, parcels_sf) {
  bbox_sfc <- sf::st_as_sfc(sf::st_bbox(parcels_sf))
  sf::st_crs(bbox_sfc) <- 26986
  suppressWarnings(sf::st_intersection(sf_obj, sf::st_sf(geometry = bbox_sfc)))
}

# ---- Identify single-zone communities ---------------------------------------
gpkg_files <- list.files(pipeline_data_dir, pattern = "\\.gpkg$", full.names = TRUE)

cat("Scanning GeoPackages for single-zone communities...\n")
single_zone_gpkgs <- Filter(function(f) {
  lyrs <- tryCatch(sf::st_layers(f), error = function(e) NULL)
  if (is.null(lyrs)) return(FALSE)
  n <- lyrs$features[lyrs$name == "districts"]
  length(n) == 1L && !is.na(n) && n == 1L
}, gpkg_files)
community_names <- tools::file_path_sans_ext(basename(single_zone_gpkgs))
cat(sprintf("  Found %d single-zone communities.\n\n", length(single_zone_gpkgs)))

# ---- Main loop --------------------------------------------------------------
cat("Computing district denominator vs Excel by geometry source...\n\n")

results <- rbindlist(lapply(seq_along(single_zone_gpkgs), function(i) {
  gpkg  <- single_zone_gpkgs[i]
  cname <- community_names[i]

  # Read layers
  parcels <- tryCatch(
    sf::st_make_valid(sf::st_read(gpkg, layer = "parcels",   quiet = TRUE)),
    error = function(e) NULL
  )
  districts <- tryCatch(
    sf::st_make_valid(sf::st_read(gpkg, layer = "districts", quiet = TRUE)),
    error = function(e) NULL
  )
  if (is.null(parcels) || is.null(districts)) {
    message(sprintf("  [SKIP] %s — failed to read layers", cname))
    return(NULL)
  }

  # Ensure EPSG:26986
  if (!is.na(sf::st_crs(parcels)$epsg) && sf::st_crs(parcels)$epsg != 26986) {
    parcels   <- sf::st_transform(parcels,   26986)
    districts <- sf::st_transform(districts, 26986)
  }

  # district_geom_source
  geom_src <- if ("district_geom_source" %in% names(districts)) {
    districts$district_geom_source[1]
  } else {
    NA_character_
  }

  # District polygon area
  dist_union <- sf::st_union(districts)
  dist_acres <- as.numeric(sf::st_area(dist_union)) / 4047

  # GIS deductions within district
  local_ded <- tryCatch(
    sf::st_make_valid(clip_to_bbox(deductions_all, parcels)),
    error = function(e) deductions_all[0, ]
  )
  ded_in_dist <- if (nrow(local_ded) > 0) {
    tryCatch(
      suppressWarnings(
        sf::st_intersection(sf::st_sf(geometry = dist_union), local_ded)
      ),
      error = function(e) local_ded[0, ]
    )
  } else {
    local_ded[0, ]
  }
  gis_ded_acres <- if (nrow(ded_in_dist) > 0)
    as.numeric(sf::st_area(sf::st_union(sf::st_geometry(ded_in_dist)))) / 4047
  else 0

  denom_district <- dist_acres - gis_ded_acres

  # Excel denominator
  denom_excel <- extract_excel_denom(cname, excel_models_dir)

  list(
    community         = cname,
    district_geom_src = geom_src,
    dist_acres        = dist_acres,
    gis_ded_acres     = gis_ded_acres,
    denom_district    = denom_district,
    denom_excel       = denom_excel,
    dist_minus_excel  = denom_district - denom_excel
  )
}), fill = TRUE)

cat("\n")

# ---- Report -----------------------------------------------------------------

# Per-community table
cat("=== Per-community results ===\n\n")
cat(sprintf("%-35s  %-13s  %8s  %8s  %8s  %+9s\n",
            "community", "geom_source", "dist_ac", "ded_ac", "denom_d", "d-excel"))
cat(strrep("-", 95), "\n")

setorder(results, district_geom_src, community)
for (k in seq_len(nrow(results))) {
  r <- results[k]
  sign <- if (!is.na(r$dist_minus_excel) && r$dist_minus_excel >= 0) "+" else ""
  cat(sprintf("%-35s  %-13s  %8.2f  %8.3f  %8.3f  %s%.3f\n",
              r$community, r$district_geom_src,
              r$dist_acres, r$gis_ded_acres, r$denom_district,
              sign,
              ifelse(is.na(r$dist_minus_excel), NA_real_, r$dist_minus_excel)))
}

# Summary by geometry source
cat("\n\n=== MAE by district geometry source ===\n\n")
cat(sprintf("%-14s  %3s  %3s  %7s  %7s  %7s  %7s\n",
            "geom_source", "n", "n_xl", "MAE_xl", "MeanE_xl",
            "pct<1ac", "pct<5ac"))
cat(strrep("-", 65), "\n")

groups <- unique(results$district_geom_src)
groups <- groups[!is.na(groups)]

for (g in sort(groups)) {
  sub <- results[district_geom_src == g]
  has_xl <- sub[!is.na(dist_minus_excel)]
  n    <- nrow(sub)
  n_xl <- nrow(has_xl)
  if (n_xl == 0) {
    cat(sprintf("%-14s  %3d  %3d  (no Excel data)\n", g, n, 0L))
    next
  }
  mae     <- mean(abs(has_xl$dist_minus_excel))
  mean_e  <- mean(has_xl$dist_minus_excel)
  pct1    <- 100 * mean(abs(has_xl$dist_minus_excel) < 1)
  pct5    <- 100 * mean(abs(has_xl$dist_minus_excel) < 5)
  cat(sprintf("%-14s  %3d  %3d  %7.2f  %+7.2f  %6.1f%%  %6.1f%%\n",
              g, n, n_xl, mae, mean_e, pct1, pct5))
}

# Overall
has_xl_all <- results[!is.na(dist_minus_excel)]
if (nrow(has_xl_all) > 0) {
  cat(strrep("-", 65), "\n")
  cat(sprintf("%-14s  %3d  %3d  %7.2f  %+7.2f  %6.1f%%  %6.1f%%\n",
              "OVERALL",
              nrow(results), nrow(has_xl_all),
              mean(abs(has_xl_all$dist_minus_excel)),
              mean(has_xl_all$dist_minus_excel),
              100 * mean(abs(has_xl_all$dist_minus_excel) < 1),
              100 * mean(abs(has_xl_all$dist_minus_excel) < 5)))
}

# Worst outliers
cat("\n\n=== Largest district-vs-Excel errors ===\n\n")
has_xl_all_sorted <- has_xl_all[order(-abs(dist_minus_excel))]
top20 <- head(has_xl_all_sorted, 20)
cat(sprintf("%-35s  %-13s  %+9s\n", "community", "geom_source", "d-excel_ac"))
cat(strrep("-", 62), "\n")
for (k in seq_len(nrow(top20))) {
  r <- top20[k]
  sign <- if (r$dist_minus_excel >= 0) "+" else ""
  cat(sprintf("%-35s  %-13s  %s%.3f\n",
              r$community, r$district_geom_src,
              sign, r$dist_minus_excel))
}

# Atlas/supplement vs districts_3a breakdown for just the outliers (|error| > 5 ac)
big_errors <- has_xl_all[abs(dist_minus_excel) > 5]
if (nrow(big_errors) > 0) {
  cat(sprintf("\n\nCommunities with |error| > 5 ac (%d total):\n", nrow(big_errors)))
  tbl <- table(big_errors$district_geom_src)
  for (nm in names(tbl)) {
    pct_of_src <- 100 * tbl[nm] / sum(results[district_geom_src == nm & !is.na(dist_minus_excel), .N])
    cat(sprintf("  %-14s  %d community/ies  (%.0f%% of that source's Excel-matched communities)\n",
                nm, tbl[nm], pct_of_src))
  }
}

cat("\n=== Done ===\n")
