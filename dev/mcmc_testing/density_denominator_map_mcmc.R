# density_denominator_map_mcmc.R
#
# Interactive map comparing the two density denominator methods visually:
#
#   pipeline  — morphological_close(st_union(in-district parcel polygons), ROW_FILL_M)
#               − GIS deductions  (what compute_gis_density_denom() now does)
#   district  — adopted district boundary polygon − GIS deductions
#               (what the Excel model uses)
#
# Map layers:
#   Blue    — In-district parcel polygons.
#   Yellow  — Road fill: area added by morphological close (pipeline − raw union).
#             Should look like road strips between parcels.
#   Orange  — Remaining gap: inside district boundary but still outside pipeline.
#             What the pipeline still misses after the close.
#   Purple  — Overhang: raw parcel union extends beyond district boundary.
#   Red     — GIS deductions within the pipeline polygon (subtracted by pipeline).
#   Green   — Pipeline polygon outline (closed union = new MCMC denominator base).
#   Black   — District boundary outline (the district denominator base).
#
# Set COMMUNITY before sourcing. Run from the mbtazone package root.
# Requires env vars: MBTAZONE_PIPELINE_DATA, MBTAZONE_DENSITY_DEDUCTIONS

library(sf)
library(mapgl)
library(readxl)

if (!exists("COMMUNITY")) COMMUNITY <- "Salem"  # override by setting before source()
ROW_FILL_M <- 25  # matches constraints$row_fill_m default in define_constraints()

# ---- Paths ------------------------------------------------------------------
pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")
excel_models_dir  <- Sys.getenv("MBTAZONE_EXCEL_MODELS")
if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")
if (!nzchar(excel_models_dir))  message("MBTAZONE_EXCEL_MODELS not set — Excel values will be NA.")

# ---- Extract Excel Summary values for a community ---------------------------
# Handles two layouts:
#   A: models_dir/Groveland/Groveland - CM.xlsx  (subdirectory per community)
#   B: models_dir/Groveland - CM.xlsx            (flat directory)
extract_excel_summary <- function(community_name, models_dir) {
  if (!nzchar(models_dir)) return(NULL)
  community_clean <- gsub("_", " ", community_name)

  dirs      <- list.dirs(models_dir, full.names = TRUE, recursive = FALSE)
  match_idx <- which(tolower(basename(dirs)) == tolower(community_clean))
  if (length(match_idx) > 0) {
    xlsx_files <- list.files(dirs[match_idx[1]], pattern = "\\.xlsx$",
                             full.names = TRUE, ignore.case = TRUE)
  } else {
    all_xlsx   <- list.files(models_dir, pattern = "\\.xlsx$",
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
      labels      <- as.character(s[[1]])
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

gpkg <- file.path(pipeline_data_dir, paste0(gsub(" ", "_", COMMUNITY), ".gpkg"))
if (!file.exists(gpkg)) stop("GeoPackage not found: ", gpkg)

# ---- Load parcels and district boundary ------------------------------------
cat("Loading", COMMUNITY, "...\n")
parcels   <- sf::st_make_valid(sf::st_read(gpkg, layer = "parcels",   quiet = TRUE))
districts <- sf::st_make_valid(sf::st_read(gpkg, layer = "districts", quiet = TRUE))

if (is.na(sf::st_crs(parcels)$epsg) || sf::st_crs(parcels)$epsg != 26986) {
  parcels   <- sf::st_transform(parcels,   26986)
  districts <- sf::st_transform(districts, 26986)
}
in_d <- parcels[parcels$in_district == TRUE, ]
cat(sprintf("  In-district parcels: %d\n", nrow(in_d)))

district_poly <- sf::st_union(districts)
district_sf   <- sf::st_sf(geometry = district_poly)
polygon_acres <- as.numeric(sf::st_area(district_poly)) / 4047

# Raw parcel union — used for gap/overhang diagnostics and filled-area layer.
parcel_union <- sf::st_union(in_d)
union_sf     <- sf::st_sf(geometry = parcel_union)
union_acres  <- as.numeric(sf::st_area(parcel_union)) / 4047

# Pipeline polygon: morphological close fills road gaps between parcels.
# Matches compute_gis_density_denom() — gaps narrower than 2*ROW_FILL_M are filled.
pipeline_geom  <- parcel_union |>
  sf::st_buffer(ROW_FILL_M) |>
  sf::st_buffer(-ROW_FILL_M)
pipeline_sf    <- sf::st_sf(geometry = pipeline_geom)
pipeline_acres <- as.numeric(sf::st_area(pipeline_geom)) / 4047
cat(sprintf("  Parcel union:    %.3f ac\n", union_acres))
cat(sprintf("  Pipeline (closed): %.3f ac  (+%.3f ac from close)\n",
            pipeline_acres, pipeline_acres - union_acres))

# ---- Gap, fill, and overhang ------------------------------------------------
# Filled: area added by morphological close (should be road strips).
filled_sf <- tryCatch({
  result <- suppressWarnings(sf::st_difference(pipeline_sf, union_sf))
  result <- sf::st_make_valid(result)
  if (as.numeric(sf::st_area(result)) < 100) NULL else result
}, error = function(e) { message("Fill diff: ", e$message); NULL })
filled_acres <- if (!is.null(filled_sf)) as.numeric(sf::st_area(filled_sf)) / 4047 else 0

# Remaining gap: inside district boundary but still outside the pipeline polygon.
gap_sf <- tryCatch({
  result <- suppressWarnings(sf::st_difference(district_sf, pipeline_sf))
  result <- sf::st_make_valid(result)
  if (as.numeric(sf::st_area(result)) < 100) NULL else result
}, error = function(e) { message("Gap diff: ", e$message); NULL })
gap_acres <- if (!is.null(gap_sf)) as.numeric(sf::st_area(gap_sf)) / 4047 else 0

# Overhang: raw parcel union extends beyond district boundary.
overhang_sf <- tryCatch({
  result <- suppressWarnings(sf::st_difference(union_sf, district_sf))
  result <- sf::st_make_valid(result)
  if (as.numeric(sf::st_area(result)) < 100) NULL else result
}, error = function(e) { message("Overhang diff: ", e$message); NULL })
overhang_acres <- if (!is.null(overhang_sf)) as.numeric(sf::st_area(overhang_sf)) / 4047 else 0

# ---- Deductions -------------------------------------------------------------
if (!exists("deductions") || !inherits(deductions, "sf")) {
  cat("Loading deductions shapefile...\n")
  deductions <- sf::st_read(density_ded_path, quiet = TRUE)
  deductions <- sf::st_transform(deductions, 26986)
  deductions <- sf::st_make_valid(deductions)
  cat(sprintf("  Loaded %d deduction features\n", nrow(deductions)))
} else {
  cat(sprintf("  Using cached deductions (%d features)\n", nrow(deductions)))
}

# Clip to bounding box first for speed
bbox_sfc <- sf::st_as_sfc(sf::st_bbox(parcels))
sf::st_crs(bbox_sfc) <- 26986
local_ded <- sf::st_make_valid(sf::st_intersection(deductions, sf::st_sf(geometry = bbox_sfc)))
cat(sprintf("  Local deduction features: %d\n", nrow(local_ded)))

# Within pipeline polygon — what MCMC subtracts (after morphological close)
ded_in_pipeline <- if (nrow(local_ded) > 0) {
  suppressWarnings(sf::st_intersection(pipeline_sf, local_ded))
} else {
  local_ded[0, ]
}
ded_pipeline_acres <- if (nrow(ded_in_pipeline) > 0) {
  as.numeric(sf::st_area(sf::st_union(ded_in_pipeline))) / 4047
} else 0

# Within district boundary — what district-GIS method subtracts
ded_in_district <- if (nrow(local_ded) > 0) {
  suppressWarnings(sf::st_intersection(district_sf, local_ded))
} else {
  local_ded[0, ]
}
ded_district_acres <- if (nrow(ded_in_district) > 0) {
  as.numeric(sf::st_area(sf::st_union(ded_in_district))) / 4047
} else 0

denom_pipeline <- pipeline_acres - ded_pipeline_acres
denom_district <- polygon_acres - ded_district_acres
capacity_total <- sum(in_d$final_lot_multi_family_unit_capacity, na.rm = TRUE)

# ---- Excel ground truth -----------------------------------------------------
excel       <- extract_excel_summary(COMMUNITY, excel_models_dir)
EXCEL_DENOM <- if (!is.null(excel)) excel$denom_excel   else NA_real_
EXCEL_DUAC  <- if (!is.null(excel)) excel$density_excel else NA_real_
if (!is.na(EXCEL_DENOM)) {
  cat(sprintf("Excel model found: denom = %.3f ac, density = %.2f du/ac\n",
              EXCEL_DENOM, EXCEL_DUAC))
} else {
  cat("No Excel model found for", COMMUNITY, "— set MBTAZONE_EXCEL_MODELS to enable comparison.\n")
}

# ---- Print comparison -------------------------------------------------------
cat(sprintf("\n%-40s %8.3f ac\n", "District boundary polygon:", polygon_acres))
cat(sprintf("%-40s %8.3f ac\n",   "Raw parcel union:", union_acres))
cat(sprintf("%-40s %8.3f ac\n",   "Pipeline (morphological close):", pipeline_acres))
cat(sprintf("  %-38s %8.3f ac  [filled by close — should be roads]\n",
            "Road fill (pipeline − union):", filled_acres))
cat(sprintf("  %-38s %8.3f ac  [pipeline still misses]\n",
            "Remaining gap (district − pipeline):", gap_acres))
cat(sprintf("  %-38s %8.3f ac  [pipeline overcounts]\n",
            "Overhang (union − district):", overhang_acres))
cat(sprintf("\n%-40s %8.3f ac\n",
            "GIS deductions in pipeline poly:", ded_pipeline_acres))
cat(sprintf("%-40s %8.3f ac\n",
            "GIS deductions in district poly:", ded_district_acres))
cat(sprintf("\n%-40s %8.3f ac  →  %.2f du/ac\n",
            "Pipeline denominator:", denom_pipeline,
            capacity_total / denom_pipeline))
cat(sprintf("%-40s %8.3f ac  →  %.2f du/ac\n",
            "District denominator:", denom_district,
            capacity_total / denom_district))
if (!is.na(EXCEL_DENOM)) {
  cat(sprintf("%-40s %8.3f ac  →  %.2f du/ac\n",
              "Excel denominator (ground truth):", EXCEL_DENOM,
              capacity_total / EXCEL_DENOM))
  cat(sprintf(
    "\nPipeline error vs Excel: %+.3f ac  (%+.1f%%),  density error %+.2f du/ac\n",
    denom_pipeline - EXCEL_DENOM,
    100 * (denom_pipeline - EXCEL_DENOM) / EXCEL_DENOM,
    capacity_total / denom_pipeline - capacity_total / EXCEL_DENOM))
}

# ---- Build map --------------------------------------------------------------
in_d_wgs     <- sf::st_transform(in_d, 4326)
parcels_wgs  <- sf::st_transform(parcels, 4326)
district_wgs <- sf::st_transform(district_sf, 4326)
union_wgs    <- sf::st_transform(union_sf, 4326)
pipeline_wgs <- sf::st_transform(pipeline_sf, 4326)
bounds       <- sf::st_bbox(district_wgs)

m <- maplibre(style = carto_style("positron"), bounds = bounds) |>
  # All parcels: light background
  add_fill_layer(
    id = "all_parcels", source = parcels_wgs,
    fill_color = "#cccccc", fill_opacity = 0.20
  ) |>
  add_line_layer(
    id = "all_parcels_line", source = parcels_wgs,
    line_color = "#888888", line_width = 0.2, line_opacity = 0.35
  ) |>
  # In-district parcels: blue
  add_fill_layer(
    id = "in_district", source = in_d_wgs,
    fill_color = "#6baed6", fill_opacity = 0.50,
    tooltip = concat(
      "<b>", get_column("LOC_ID"), "</b>",
      "<br>Cap: ", get_column("final_lot_multi_family_unit_capacity"),
      "<br>ACRES: ", get_column("ACRES")
    )
  ) |>
  add_line_layer(
    id = "in_district_line", source = in_d_wgs,
    line_color = "#2171b5", line_width = 0.4
  )

# Road fill (morphological close adds this — should look like road strips): yellow
if (!is.null(filled_sf) && filled_acres > 0) {
  m <- m |> add_fill_layer(
    id = "filled", source = sf::st_transform(filled_sf, 4326),
    fill_color = "#fee08b", fill_opacity = 0.70
  )
}

# Remaining gap (district − pipeline, what close still misses): orange
if (!is.null(gap_sf) && gap_acres > 0) {
  m <- m |> add_fill_layer(
    id = "gap", source = sf::st_transform(gap_sf, 4326),
    fill_color = "#fd8d3c", fill_opacity = 0.65
  )
}

# Overhang (raw union beyond district boundary): purple
if (!is.null(overhang_sf) && overhang_acres > 0) {
  m <- m |> add_fill_layer(
    id = "overhang", source = sf::st_transform(overhang_sf, 4326),
    fill_color = "#9e2a8f", fill_opacity = 0.65
  )
}

# GIS deductions within pipeline polygon: red
if (nrow(ded_in_pipeline) > 0) {
  m <- m |> add_fill_layer(
    id = "deductions", source = sf::st_transform(ded_in_pipeline, 4326),
    fill_color = "#d73027", fill_opacity = 0.70
  )
}

# Pipeline outline (closed union = MCMC denominator base): green
# District boundary: black
m <- m |>
  add_line_layer(
    id = "pipeline_outline", source = pipeline_wgs,
    line_color = "#1a9850", line_width = 2.5
  ) |>
  add_line_layer(
    id = "district_outline", source = district_wgs,
    line_color = "#000000", line_width = 2.5
  ) |>
  add_legend(
    legend_title = {
      excel_line <- if (!is.na(EXCEL_DENOM))
        sprintf("Excel:    %6.1f ac → %5.2f du/ac", EXCEL_DENOM,
                capacity_total / EXCEL_DENOM)
      else "Excel:    n/a"
      paste0(
        COMMUNITY, " — density denominator comparison\n",
        sprintf("Pipeline: %6.1f ac → %5.2f du/ac\n", denom_pipeline,
                capacity_total / denom_pipeline),
        sprintf("District: %6.1f ac → %5.2f du/ac\n", denom_district,
                capacity_total / denom_district),
        excel_line
      )
    },
    values = c(
      sprintf("District boundary (%.1f ac)", polygon_acres),
      sprintf("In-district parcels  n=%d  cap=%d units", nrow(in_d), capacity_total),
      sprintf("Road fill (close adds): %.1f ac", filled_acres),
      sprintf("Remaining gap (district − pipeline): %.1f ac", gap_acres),
      sprintf("Overhang (union − district): %.1f ac", overhang_acres),
      sprintf("GIS deductions in pipeline: %.1f ac", ded_pipeline_acres),
      sprintf("Pipeline outline (%.1f ac)", pipeline_acres)
    ),
    colors = c("#000000", "#6baed6", "#fee08b", "#fd8d3c", "#9e2a8f", "#d73027", "#1a9850"),
    type = "categorical"
  ) |>
  add_fullscreen_control(position = "top-left") |>
  add_navigation_control()

m
# To save for viewing on server:
# htmlwidgets::saveWidget(m, sprintf("dev/mcmc_testing/denom_map_mcmc_%s.html", COMMUNITY),
#                         selfcontained = TRUE)
