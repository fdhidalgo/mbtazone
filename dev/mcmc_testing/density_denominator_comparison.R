# density_denominator_comparison.R
#
# Compares four approaches to computing the density denominator for each
# adopted single-zone district, validated against EOHLC Excel compliance
# models (ground truth).
#
# The four approaches:
#   1. parcel_sum   — sum of parcel ACRES fields (current MCMC approach; wrong)
#   2. gis          — district polygon area minus GIS intersection with the
#                     state Density Denominator Deductions shapefile (Hydrology,
#                     Wetlands, TitleV, SurfWatBC, Wellhead1 merged statewide)
#   3. per_parcel   — district polygon area minus sum of the per-parcel
#                     deduction fields stored in the GeoPackage parcels layer
#                     (Hydrology + Wetlands + TitleV + SurfWatBC + Wellhead1)
#   4. excel        — "District Density Denominator" row from the EOHLC Excel
#                     compliance model Summary sheet (ground truth; consistent
#                     with EOHLC determination letters)
#
# Key finding: GIS matches Excel within 0.10 ac for 28/48 communities.
# Large outliers (Salem +37 ac, Essex +26 ac, North_Reading +10 ac, Sherborn
# +9 ac) are coastal/estuarine communities where the district polygon contains
# tidal water not captured in the statewide deductions shapefile.
#
# Run from the mbtazone package root. Requires env vars:
#   MBTAZONE_PIPELINE_DATA       — directory of per-community .gpkg files
#   MBTAZONE_DENSITY_DEDUCTIONS  — path to Density_Denominator_Deductions.shp
#   MBTAZONE_EXCEL_MODELS        — path to mbta_district_models/ directory

library(sf)
library(data.table)

# ---- Inputs -----------------------------------------------------------------

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA env var not set.")

single_zone_csv <- "inst/extdata/single_zone_communities.csv"
if (!file.exists(single_zone_csv)) {
  stop(
    "Single-zone community list not found at ", single_zone_csv, ".\n",
    "Generate it first: Rscript inst/targets/check_single_zone_districts.R"
  )
}
single_zone <- data.table::fread(single_zone_csv)
cat("Loaded", nrow(single_zone), "single-zone communities\n\n")

all_gpkg_files <- list.files(pipeline_data_dir, pattern = "\\.gpkg$", full.names = TRUE)
gpkg_files <- all_gpkg_files[
  tools::file_path_sans_ext(basename(all_gpkg_files)) %in%
    gsub(" ", "_", single_zone$community_name)
]
cat("Matched", length(gpkg_files), "GeoPackage files\n\n")

# ---- Load statewide deductions shapefile ------------------------------------

density_ded_path <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")
deductions <- NULL
if (nzchar(density_ded_path) && file.exists(density_ded_path)) {
  cat("Loading density denominator deductions shapefile...\n")
  deductions <- sf::st_read(density_ded_path, quiet = TRUE)
  deductions <- sf::st_transform(deductions, 26986)
  deductions <- sf::st_make_valid(deductions)
  cat(sprintf("  Loaded: %d features\n\n", nrow(deductions)))
} else {
  cat("MBTAZONE_DENSITY_DEDUCTIONS not set — GIS columns will be NA.\n\n")
}

# ---- Helper: read density denominator from Excel Summary sheet --------------

excel_models_dir <- Sys.getenv("MBTAZONE_EXCEL_MODELS")
if (!nzchar(excel_models_dir)) {
  cat("MBTAZONE_EXCEL_MODELS not set — Excel columns will be NA.\n\n")
}

extract_excel_summary <- function(community_name, models_dir) {
  if (!nzchar(models_dir)) return(NULL)
  community_clean <- gsub("_", " ", community_name)

  # Layout A: subdirectory per community  (models_dir/Groveland/Groveland - CM.xlsx)
  dirs <- list.dirs(models_dir, full.names = TRUE, recursive = FALSE)
  match_idx <- which(tolower(basename(dirs)) == tolower(community_clean))
  if (length(match_idx) > 0) {
    xlsx_files <- list.files(dirs[match_idx[1]], pattern = "\\.xlsx$",
                             full.names = TRUE, ignore.case = TRUE)
  } else {
    # Layout B: flat directory  (models_dir/Groveland - CM.xlsx)
    all_xlsx <- list.files(models_dir, pattern = "\\.xlsx$",
                           full.names = TRUE, ignore.case = TRUE)
    xlsx_files <- all_xlsx[
      grepl(paste0("^", gsub("([.()^$*+?{}|\\[\\]])", "\\\\\\1", community_clean),
                   "\\s*-\\s*CM"), basename(all_xlsx), ignore.case = TRUE)
    ]
  }

  xlsx_files <- xlsx_files[!grepl("^~\\$", basename(xlsx_files))]
  if (length(xlsx_files) == 0) return(NULL)

  for (xlsx in xlsx_files) {
    tryCatch({
      if (!"Summary" %in% readxl::excel_sheets(xlsx)) next
      s      <- readxl::read_excel(xlsx, sheet = "Summary", col_names = FALSE,
                                   .name_repair = "minimal")
      labels <- as.character(s[[1]])
      denom_row   <- grep("District Density Denominator", labels, ignore.case = TRUE)[1]
      acreage_row <- grep("District Acreage",             labels, ignore.case = TRUE)[1]
      duac_row    <- grep("^DU/AC$",                      labels, ignore.case = TRUE)[1]
      if (is.na(denom_row)) next
      return(list(
        denom_excel   = suppressWarnings(as.numeric(s[[2]][denom_row])),
        acreage_excel = suppressWarnings(as.numeric(s[[2]][acreage_row])),
        density_excel = suppressWarnings(as.numeric(s[[2]][duac_row]))
      ))
    }, error = function(e) NULL)
  }
  NULL
}

# ---- Per-community computation ----------------------------------------------

results <- lapply(gpkg_files, function(gpkg) {
  community <- tools::file_path_sans_ext(basename(gpkg))
  tryCatch({
    parcels   <- sf::st_read(gpkg, layer = "parcels",   quiet = TRUE)
    districts <- sf::st_read(gpkg, layer = "districts", quiet = TRUE)
    in_d      <- parcels[parcels$in_district == TRUE, ]
    if (nrow(in_d) == 0) return(NULL)

    capacity         <- sum(in_d$final_lot_multi_family_unit_capacity, na.rm = TRUE)
    district_poly    <- sf::st_union(districts)
    polygon_acres    <- as.numeric(sf::st_area(district_poly)) / 4047
    parcel_sum_acres <- sum(in_d$ACRES, na.rm = TRUE)

    # Approach 2: GIS — clip deductions to district, dissolve the small local subset,
    # then subtract. Dissolving after clipping (not globally) avoids double-counting
    # where source layers overlap, without the cost of a statewide union.
    gis_deductions_acres <- NA_real_
    denom_gis            <- NA_real_
    if (!is.null(deductions)) {
      district_sf <- sf::st_sf(geometry = sf::st_make_valid(district_poly))
      clipped     <- sf::st_intersection(district_sf, deductions)
      if (nrow(clipped) > 0) {
        gis_deductions_acres <- as.numeric(sf::st_area(sf::st_union(clipped))) / 4047
      } else {
        gis_deductions_acres <- 0
      }
      denom_gis <- polygon_acres - gis_deductions_acres
    }

    # Approach 3: per-parcel deduction fields stored in the GeoPackage
    dd_cols <- intersect(c("Hydrology", "Wetlands", "TitleV", "SurfWatBC", "Wellhead1"),
                         names(in_d))
    if (length(dd_cols) > 0) {
      per_parcel_dd_acres <- sum(
        rowSums(sf::st_drop_geometry(in_d)[, dd_cols, drop = FALSE], na.rm = TRUE)
      ) / 43560
      denom_per_parcel    <- polygon_acres - per_parcel_dd_acres
    } else {
      per_parcel_dd_acres <- NA_real_
      denom_per_parcel    <- NA_real_
    }

    # Approach 4: Excel compliance model (ground truth)
    excel         <- extract_excel_summary(community, excel_models_dir)
    denom_excel   <- if (!is.null(excel)) excel$denom_excel   else NA_real_
    density_excel <- if (!is.null(excel)) excel$density_excel else NA_real_

    data.frame(
      community                        = community,
      district_polygon_acres           = round(polygon_acres,           3),
      parcel_sum_acres                 = round(parcel_sum_acres,        3),
      gis_deductions_acres             = round(gis_deductions_acres,    3),
      per_parcel_dd_acres              = round(per_parcel_dd_acres,     3),
      unit_capacity                    = capacity,
      # Density denominators (acres) — four approaches
      denom_parcel_sum                 = round(parcel_sum_acres,        3),
      denom_gis                        = round(denom_gis,               3),
      denom_per_parcel                 = round(denom_per_parcel,        3),
      denom_excel                      = round(denom_excel,             3),
      # Gross density (units/acre) — four approaches
      gross_density_parcel_sum         = round(capacity / parcel_sum_acres, 2),
      gross_density_gis                = round(capacity / denom_gis,        2),
      gross_density_per_parcel         = round(capacity / denom_per_parcel, 2),
      gross_density_excel              = round(density_excel,               2),
      # Validation: GIS vs Excel denominator difference (key diagnostic)
      gis_minus_excel_denom_acres      = round(denom_gis - denom_excel,     3),
      per_parcel_minus_excel_denom_acres = round(denom_per_parcel - denom_excel, 3),
      # Flag corrupted per-parcel fields (deductions exceed parcel area)
      per_parcel_fields_corrupted      = !is.na(denom_per_parcel) & denom_per_parcel < 0,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("  Skipping ", community, ": ", conditionMessage(e))
    NULL
  })
})

dt <- data.table::rbindlist(Filter(Negate(is.null), results))
data.table::setorder(dt, community)

# ---- Console output ---------------------------------------------------------

cat("==========================================================================\n")
cat("DENSITY DENOMINATOR COMPARISON —", nrow(dt), "single-zone communities\n")
cat("==========================================================================\n\n")

print(dt[, .(community,
             district_polygon_acres, parcel_sum_acres,
             denom_parcel_sum, denom_gis, denom_per_parcel, denom_excel,
             gross_density_parcel_sum, gross_density_gis,
             gross_density_per_parcel, gross_density_excel,
             gis_minus_excel_denom_acres)], nrow = 200)

cat("\n--- GIS vs Excel validation ---\n")
has_both <- dt[!is.na(denom_excel) & !is.na(denom_gis)]
if (nrow(has_both) > 0) {
  cat(sprintf("Communities with both GIS and Excel data: %d\n", nrow(has_both)))
  cat(sprintf("Mean  |GIS denom - Excel denom|: %.4f acres\n",
              mean(abs(has_both$gis_minus_excel_denom_acres), na.rm = TRUE)))
  cat(sprintf("Max   |GIS denom - Excel denom|: %.4f acres\n",
              max(abs(has_both$gis_minus_excel_denom_acres), na.rm = TRUE)))
  cat(sprintf("Within 0.05 ac of Excel: %d / %d\n",
              sum(abs(has_both$gis_minus_excel_denom_acres) < 0.05, na.rm = TRUE), nrow(has_both)))
  cat(sprintf("Within 0.10 ac of Excel: %d / %d\n",
              sum(abs(has_both$gis_minus_excel_denom_acres) < 0.10, na.rm = TRUE), nrow(has_both)))
  cat("\nLargest GIS-vs-Excel discrepancies:\n")
  print(has_both[order(-abs(gis_minus_excel_denom_acres))][1:min(5, .N),
                                                           .(community, denom_gis, denom_excel, gis_minus_excel_denom_acres,
                                                             gross_density_gis, gross_density_excel)])
}

cat("\n--- Communities where parcel-sum density < 15 but Excel density >= 15 ---\n")
fixed <- dt[!is.na(gross_density_excel) &
              gross_density_parcel_sum < 15 & gross_density_excel >= 15]
if (nrow(fixed) > 0) {
  print(fixed[, .(community, unit_capacity,
                  denom_parcel_sum, denom_gis, denom_per_parcel, denom_excel,
                  gross_density_parcel_sum, gross_density_gis,
                  gross_density_per_parcel, gross_density_excel)])
} else {
  cat("None found.\n")
}

cat("\n--- Communities where GIS density < 15 but Excel density >= 15 ---\n")
gis_miss <- dt[!is.na(gross_density_excel) & !is.na(gross_density_gis) &
                 gross_density_gis < 15 & gross_density_excel >= 15]
if (nrow(gis_miss) > 0) {
  print(gis_miss[, .(community, denom_gis, denom_per_parcel, denom_excel,
                     gross_density_gis, gross_density_per_parcel, gross_density_excel,
                     gis_minus_excel_denom_acres)])
} else {
  cat("None.\n")
}

# ---- CSV output -------------------------------------------------------------

out_csv <- "dev/mcmc_testing/density_denominator_comparison.csv"
data.table::fwrite(dt, out_csv)
cat(sprintf("\nWrote %d rows to %s\n", nrow(dt), out_csv))
