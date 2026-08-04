# density_denominator_comparison.R
#
# Single-pass computation of all density denominator methods for every
# single-zone community, plus summary comparisons and plots.
#
# Nomenclature (consistent throughout this file and all dependent scripts):
#
#   pipeline  — st_union(in-district parcel polygons) − GIS deductions
#               This is what compute_gis_density_denom() does in the live MCMC.
#               Requires only parcel data; works during MCMC search (no district
#               boundary polygon needed).
#
#   district  — st_union(adopted district polygons) − GIS deductions clipped to that polygon
#               Closest to the Excel model calculation. Only available for adopted zones.
#
#   per_parcel — district polygon area − sum of per-parcel deduction fields in .gpkg
#               (Hydrology + Wetlands + TitleV + SurfWatBC + Wellhead1, stored in sq ft)
#               Sanity check; can show corrupted values where field sums exceed parcel area.
#
#   parcel_sum — sum(in_d$ACRES): the pre-branch old approach. Retained for reference only.
#
#   excel      — "District Density Denominator" from the EOHLC Excel compliance model
#               Summary sheet. Ground truth for adopted boundaries.
#
#   atlas      — MA Zoning Atlas gross density (Summary__2 field in districts layer).
#               denom_atlas_implied = capacity / atlas_density: back-calculated from
#               Atlas density using pipeline capacity; mixes two sources.
#
# Output CSV columns follow the same prefix scheme:
#   denom_*          — denominator in acres
#   gross_density_*  — gross density in du/ac
#   ded_*_acres      — GIS deductions clipped to that method's base polygon
#
# Replaces: adopted_density_accuracy.R (merged here)
#
# Run from the mbtazone package root. Requires env vars:
#   MBTAZONE_PIPELINE_DATA       — directory of per-community .gpkg files
#   MBTAZONE_DENSITY_DEDUCTIONS  — path to Density_Denominator_Deductions.shp
#   MBTAZONE_EXCEL_MODELS        — path to mbta_district_models/ directory

library(sf)
library(ggplot2)
library(data.table)

# ---- Inputs -----------------------------------------------------------------

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")
excel_models_dir  <- Sys.getenv("MBTAZONE_EXCEL_MODELS")

if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA env var not set.")

single_zone_csv <- "inst/extdata/single_zone_communities.csv"
if (!file.exists(single_zone_csv))
  stop("Single-zone community list not found at ", single_zone_csv,
       ".\nGenerate it first: Rscript inst/targets/check_single_zone_districts.R")
single_zone <- data.table::fread(single_zone_csv)
cat(sprintf("Loaded %d single-zone communities\n\n", nrow(single_zone)))

all_gpkg <- list.files(pipeline_data_dir, pattern = "\\.gpkg$", full.names = TRUE)
gpkg_files <- all_gpkg[
  tools::file_path_sans_ext(basename(all_gpkg)) %in%
    gsub(" ", "_", single_zone$community_name)
]
cat(sprintf("Matched %d GeoPackage files\n\n", length(gpkg_files)))

# ---- Load statewide deductions ----------------------------------------------

deductions <- NULL
if (nzchar(density_ded_path) && file.exists(density_ded_path)) {
  cat("Loading density denominator deductions...\n")
  deductions <- sf::st_read(density_ded_path, quiet = TRUE)
  deductions <- sf::st_transform(deductions, 26986)
  deductions <- sf::st_make_valid(deductions)
  cat(sprintf("  %d deduction features\n\n", nrow(deductions)))
} else {
  cat("MBTAZONE_DENSITY_DEDUCTIONS not set — GIS deduction columns will be NA.\n\n")
}

if (!nzchar(excel_models_dir))
  cat("MBTAZONE_EXCEL_MODELS not set — Excel columns will be NA.\n\n")

# ---- Helper: read Excel density denominator ---------------------------------

extract_excel_summary <- function(community_name, models_dir) {
  if (!nzchar(models_dir)) return(NULL)
  community_clean <- gsub("_", " ", community_name)
  dirs      <- list.dirs(models_dir, full.names = TRUE, recursive = FALSE)
  match_idx <- which(tolower(basename(dirs)) == tolower(community_clean))
  if (length(match_idx) > 0) {
    xlsx_files <- list.files(dirs[match_idx[1]], pattern = "\\.xlsx$",
                             full.names = TRUE, ignore.case = TRUE)
  } else {
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

# ---- Per-community single-pass computation ----------------------------------

cat("Computing denominators for each community...\n")

results <- lapply(seq_along(gpkg_files), function(i) {
  gpkg      <- gpkg_files[[i]]
  community <- tools::file_path_sans_ext(basename(gpkg))
  cat(sprintf("  [%d/%d] %s\n", i, length(gpkg_files), community))

  tryCatch({
    parcels   <- sf::st_make_valid(sf::st_read(gpkg, layer = "parcels",   quiet = TRUE))
    districts <- sf::st_make_valid(sf::st_read(gpkg, layer = "districts", quiet = TRUE))
    if (is.na(sf::st_crs(parcels)$epsg) || sf::st_crs(parcels)$epsg != 26986) {
      parcels   <- sf::st_transform(parcels,   26986)
      districts <- sf::st_transform(districts, 26986)
    }
    in_d <- parcels[parcels$in_district == TRUE, ]
    if (nrow(in_d) == 0) return(NULL)

    capacity <- sum(in_d$final_lot_multi_family_unit_capacity, na.rm = TRUE)

    # --- Geometry: district boundary and parcel union -------------------------
    district_poly <- sf::st_union(districts)
    district_sf   <- sf::st_sf(geometry = district_poly)
    polygon_acres <- as.numeric(sf::st_area(district_poly)) / 4047

    parcel_union  <- sf::st_union(in_d)
    union_sf      <- sf::st_sf(geometry = parcel_union)
    union_acres   <- as.numeric(sf::st_area(parcel_union)) / 4047

    parcel_sum_acres <- sum(in_d$ACRES, na.rm = TRUE)

    # Gap: inside district boundary but no parcel polygon covers it.
    # district method counts this; pipeline method does not (undercount).
    gap_sf <- tryCatch(
      sf::st_make_valid(suppressWarnings(sf::st_difference(district_sf, union_sf))),
      error = function(e) NULL
    )
    gap_acres <- if (!is.null(gap_sf)) max(0, as.numeric(sf::st_area(gap_sf)) / 4047) else 0

    # Overhang: parcel polygon extends beyond district boundary.
    # pipeline counts this; district method does not (overcount).
    overhang_sf <- tryCatch(
      sf::st_make_valid(suppressWarnings(sf::st_difference(union_sf, district_sf))),
      error = function(e) NULL
    )
    overhang_acres <- if (!is.null(overhang_sf)) max(0, as.numeric(sf::st_area(overhang_sf)) / 4047) else 0

    # --- GIS deductions -------------------------------------------------------
    ded_district_acres <- NA_real_
    ded_pipeline_acres <- NA_real_
    denom_district     <- NA_real_
    denom_pipeline     <- NA_real_

    if (!is.null(deductions)) {
      bbox_sfc <- sf::st_as_sfc(sf::st_bbox(parcels))
      sf::st_crs(bbox_sfc) <- 26986
      local_ded <- sf::st_make_valid(
        sf::st_intersection(deductions, sf::st_sf(geometry = bbox_sfc))
      )

      if (nrow(local_ded) > 0) {
        clipped_district <- suppressWarnings(sf::st_intersection(district_sf, local_ded))
        ded_district_acres <- if (nrow(clipped_district) > 0)
          as.numeric(sf::st_area(sf::st_union(clipped_district))) / 4047 else 0

        clipped_pipeline <- suppressWarnings(sf::st_intersection(union_sf, local_ded))
        ded_pipeline_acres <- if (nrow(clipped_pipeline) > 0)
          as.numeric(sf::st_area(sf::st_union(clipped_pipeline))) / 4047 else 0
      } else {
        ded_district_acres <- 0
        ded_pipeline_acres <- 0
      }

      denom_district <- polygon_acres - ded_district_acres
      denom_pipeline <- union_acres   - ded_pipeline_acres
    }

    # --- Per-parcel deduction fields from .gpkg -------------------------------
    dd_cols <- intersect(c("Hydrology", "Wetlands", "TitleV", "SurfWatBC", "Wellhead1"),
                         names(in_d))
    per_parcel_dd_acres <- if (length(dd_cols) > 0) {
      sum(rowSums(sf::st_drop_geometry(in_d)[, dd_cols, drop = FALSE], na.rm = TRUE)) / 43560
    } else NA_real_
    denom_per_parcel <- if (!is.na(per_parcel_dd_acres))
      polygon_acres - per_parcel_dd_acres else NA_real_

    # --- Excel ground truth ---------------------------------------------------
    excel         <- extract_excel_summary(community, excel_models_dir)
    denom_excel   <- if (!is.null(excel)) excel$denom_excel   else NA_real_
    density_excel <- if (!is.null(excel)) excel$density_excel else NA_real_

    # --- MA Zoning Atlas (back-calculated implied denominator) ----------------
    atlas_density       <- suppressWarnings(as.numeric(districts$Summary__2[1]))
    denom_atlas_implied <- if (!is.na(atlas_density) && atlas_density > 0)
      capacity / atlas_density else NA_real_

    data.frame(
      community              = community,
      # Geometry components
      district_polygon_acres = round(polygon_acres,    3),
      union_acres            = round(union_acres,      3),
      parcel_sum_acres       = round(parcel_sum_acres, 3),
      gap_acres              = round(gap_acres,        3),
      overhang_acres         = round(overhang_acres,   3),
      ded_district_acres     = round(ded_district_acres,  3),
      ded_pipeline_acres     = round(ded_pipeline_acres,  3),
      per_parcel_dd_acres    = round(per_parcel_dd_acres, 3),
      capacity               = capacity,
      atlas_density          = round(atlas_density, 2),
      # Density denominators (acres)
      denom_pipeline      = round(denom_pipeline,     3),
      denom_district      = round(denom_district,     3),
      denom_per_parcel    = round(denom_per_parcel,   3),
      denom_parcel_sum    = round(parcel_sum_acres,   3),
      denom_excel         = round(denom_excel,        3),
      denom_atlas_implied = round(denom_atlas_implied, 3),
      # Gross densities (du/ac)
      gross_density_pipeline   = round(capacity / denom_pipeline,   2),
      gross_density_district   = round(capacity / denom_district,   2),
      gross_density_per_parcel = round(capacity / denom_per_parcel, 2),
      gross_density_parcel_sum = round(capacity / parcel_sum_acres, 2),
      gross_density_excel      = round(density_excel, 2),
      gross_density_atlas      = round(atlas_density, 2),
      # Diagnostic: difference vs Excel
      pipeline_minus_excel_acres = round(denom_pipeline - denom_excel, 3),
      district_minus_excel_acres = round(denom_district - denom_excel, 3),
      per_parcel_fields_corrupted = !is.na(denom_per_parcel) & denom_per_parcel < 0,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("  ERROR in ", community, ": ", conditionMessage(e))
    NULL
  })
})

dt <- data.table::rbindlist(Filter(Negate(is.null), results))
data.table::setorder(dt, community)

# ---- Console summary --------------------------------------------------------

cat("\n==========================================================================\n")
cat(sprintf("DENSITY DENOMINATOR ANALYSIS — %d single-zone communities\n", nrow(dt)))
cat("==========================================================================\n\n")

has_excel <- dt[!is.na(denom_excel)]

if (nrow(has_excel) > 0) {
  has_both <- has_excel[!is.na(denom_district)]

  cat("--- Pipeline (parcel union) vs Excel ---\n")
  cat(sprintf("Communities with Excel data:         %d\n", nrow(has_excel)))
  cat(sprintf("Mean |pipeline − excel|:              %.3f ac\n",
              mean(abs(has_excel$pipeline_minus_excel_acres), na.rm = TRUE)))
  cat(sprintf("Max  |pipeline − excel|:              %.3f ac (%s)\n",
              max(abs(has_excel$pipeline_minus_excel_acres), na.rm = TRUE),
              has_excel$community[which.max(abs(has_excel$pipeline_minus_excel_acres))]))
  cat(sprintf("Within 1 ac of Excel:                %d / %d\n\n",
              sum(abs(has_excel$pipeline_minus_excel_acres) < 1, na.rm = TRUE),
              nrow(has_excel)))

  cat("--- District (boundary polygon) vs Excel ---\n")
  cat(sprintf("Mean |district − excel|:              %.3f ac\n",
              mean(abs(has_both$district_minus_excel_acres), na.rm = TRUE)))
  cat(sprintf("Max  |district − excel|:              %.3f ac (%s)\n",
              max(abs(has_both$district_minus_excel_acres), na.rm = TRUE),
              has_both$community[which.max(abs(has_both$district_minus_excel_acres))]))
  cat(sprintf("Within 1 ac of Excel:                %d / %d\n\n",
              sum(abs(has_both$district_minus_excel_acres) < 1, na.rm = TRUE),
              nrow(has_both)))

  n_pipeline_wins <- sum(
    abs(has_both$pipeline_minus_excel_acres) < abs(has_both$district_minus_excel_acres),
    na.rm = TRUE
  )
  cat(sprintf("Pipeline closer to Excel than district: %d / %d\n",
              n_pipeline_wins, nrow(has_both)))
  cat(sprintf("District closer to Excel than pipeline: %d / %d\n\n",
              nrow(has_both) - n_pipeline_wins, nrow(has_both)))

  cat("All communities sorted by |pipeline − excel|:\n")
  print(has_excel[order(-abs(pipeline_minus_excel_acres)),
                  .(community, district_polygon_acres, union_acres,
                    gap_acres, overhang_acres,
                    denom_pipeline, denom_district, denom_excel,
                    pipeline_minus_excel_acres, district_minus_excel_acres)],
        nrow = 100)
}

# ---- Plots ------------------------------------------------------------------

LABEL_THRESHOLD <- 2.0
p1_data <- has_excel[!is.na(gross_density_excel) & !is.na(gross_density_pipeline)]
has_ggrepel <- requireNamespace("ggrepel", quietly = TRUE)

p1 <- ggplot(p1_data, aes(x = gross_density_excel, y = gross_density_pipeline)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed",
              linewidth = 0.8) +
  geom_point(aes(colour = pipeline_minus_excel_acres), size = 2.5, alpha = 0.85) +
  (if (has_ggrepel) {
    ggrepel::geom_text_repel(
      data = p1_data[abs(gross_density_pipeline - gross_density_excel) > LABEL_THRESHOLD],
      aes(label = community), size = 2.8, max.overlaps = 30, segment.colour = "grey60"
    )
  } else {
    geom_text(
      data = p1_data[abs(gross_density_pipeline - gross_density_excel) > LABEL_THRESHOLD],
      aes(label = community), size = 2.5, vjust = -0.6
    )
  }) +
  scale_colour_gradient2(
    name     = "Pipeline − Excel\ndenominator (ac)",
    low      = "#4575b4", mid = "white", high = "#d73027", midpoint = 0
  ) +
  labs(
    title    = "Pipeline vs Excel gross density for adopted boundaries",
    subtitle = paste0(
      "Pipeline = st_union(in-district parcel polygons) − GIS deductions.\n",
      "Red: pipeline denominator > Excel (parcel overhang); blue: < Excel (gap/water)."
    ),
    x = "Excel density (du/ac)  [ground truth]",
    y = "Pipeline density (du/ac)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right")

ggsave("dev/mcmc_testing/density_accuracy_scatter.png",
       p1, width = 8, height = 7, dpi = 150)
cat("Saved: dev/mcmc_testing/density_accuracy_scatter.png\n")

bar_data <- copy(has_excel)[!is.na(pipeline_minus_excel_acres)]
bar_data <- bar_data[order(pipeline_minus_excel_acres)]
bar_data[, community_f := factor(community, levels = community)]
bar_data[, overcount   := pipeline_minus_excel_acres > 0]

p2 <- ggplot(bar_data, aes(x = community_f, y = pipeline_minus_excel_acres,
                            fill = overcount)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = c(-1, 1), colour = "grey70", linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  scale_fill_manual(
    values = c(`FALSE` = "#4575b4", `TRUE` = "#d73027"),
    labels = c(`FALSE` = "Pipeline underestimates area (density overestimated — conservative)",
               `TRUE`  = "Pipeline overestimates area  (density underestimated — permissive)"),
    name = NULL
  ) +
  labs(
    title    = "Density denominator error: pipeline − Excel (acres)",
    subtitle = paste0(
      "Positive (red): parcel polygons extend beyond district boundary (overhang).\n",
      "Negative (blue): district boundary includes water/gaps not covered by parcels (gap)."
    ),
    x = NULL,
    y = "Pipeline − Excel denominator (acres)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    legend.position = "bottom",
    legend.text  = element_text(size = 8)
  )

ggsave("dev/mcmc_testing/density_accuracy_denom_error.png",
       p2, width = 13, height = 6, dpi = 150)
cat("Saved: dev/mcmc_testing/density_accuracy_denom_error.png\n")

# ---- CSV output -------------------------------------------------------------

out_csv <- "dev/mcmc_testing/density_denominator_all.csv"
data.table::fwrite(dt, out_csv)
cat(sprintf("\nSaved: %s (%d communities, %d columns)\n", out_csv, nrow(dt), ncol(dt)))
