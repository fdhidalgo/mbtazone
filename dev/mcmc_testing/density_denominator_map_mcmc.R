# density_denominator_map_mcmc.R
#
# Interactive map comparing the two density denominator methods visually:
#
#   pipeline  — st_union(in-district parcel polygons) − GIS deductions
#               (what compute_gis_density_denom() does in the live MCMC)
#   district  — adopted district boundary polygon − GIS deductions
#               (what the Excel model uses)
#
# Map layers:
#   Orange  — Gap: inside district boundary but no parcel polygon.
#             district method counts this; pipeline does not (undercount).
#   Purple  — Overhang: parcel polygon extends beyond district boundary.
#             pipeline counts this; district method does not (overcount).
#   Red     — GIS deductions within the parcel union (subtracted by pipeline).
#   Green   — Parcel union outline (the pipeline denominator base polygon).
#   Black   — District boundary outline (the district denominator base).
#
# Set COMMUNITY before sourcing. Run from the mbtazone package root.
# Requires env vars: MBTAZONE_PIPELINE_DATA, MBTAZONE_DENSITY_DEDUCTIONS

library(sf)
library(mapgl)
library(readxl)

if (!exists("COMMUNITY")) COMMUNITY <- "Salem"  # override by setting before source()

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

# MCMC base: union of in-district parcel polygons (what compute_gis_density_denom uses)
parcel_union  <- sf::st_union(in_d)
union_sf      <- sf::st_sf(geometry = parcel_union)
union_acres   <- as.numeric(sf::st_area(parcel_union)) / 4047

# ---- Gap and overhang -------------------------------------------------------
# Gap: inside district boundary but not covered by any parcel polygon.
# District-GIS method counts this area; MCMC does not.
gap_sf <- tryCatch({
  result <- suppressWarnings(sf::st_difference(district_sf, union_sf))
  result <- sf::st_make_valid(result)
  if (as.numeric(sf::st_area(result)) < 100) NULL else result  # ignore < 100 sq ft noise
}, error = function(e) { message("Gap diff: ", e$message); NULL })
gap_acres <- if (!is.null(gap_sf)) as.numeric(sf::st_area(gap_sf)) / 4047 else 0

# Overhang: parcel polygon extends beyond district boundary.
# MCMC counts this area; district-GIS method does not.
overhang_sf <- tryCatch({
  result <- suppressWarnings(sf::st_difference(union_sf, district_sf))
  result <- sf::st_make_valid(result)
  if (as.numeric(sf::st_area(result)) < 100) NULL else result
}, error = function(e) { message("Overhang diff: ", e$message); NULL })
overhang_acres <- if (!is.null(overhang_sf)) as.numeric(sf::st_area(overhang_sf)) / 4047 else 0

# ---- Deductions -------------------------------------------------------------
cat("Loading deductions shapefile...\n")
deductions <- sf::st_read(density_ded_path, quiet = TRUE)
deductions <- sf::st_transform(deductions, 26986)
deductions <- sf::st_make_valid(deductions)

# Clip to bounding box first for speed
bbox_sfc <- sf::st_as_sfc(sf::st_bbox(parcels))
sf::st_crs(bbox_sfc) <- 26986
local_ded <- sf::st_make_valid(sf::st_intersection(deductions, sf::st_sf(geometry = bbox_sfc)))
cat(sprintf("  Local deduction features: %d\n", nrow(local_ded)))

# Within parcel union — what MCMC subtracts
ded_in_union <- if (nrow(local_ded) > 0) {
  suppressWarnings(sf::st_intersection(union_sf, local_ded))
} else {
  local_ded[0, ]
}
ded_union_acres <- if (nrow(ded_in_union) > 0) {
  as.numeric(sf::st_area(sf::st_union(ded_in_union))) / 4047
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

denom_pipeline <- union_acres   - ded_union_acres
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
cat(sprintf("%-40s %8.3f ac\n",   "Parcel union (pipeline base):", union_acres))
cat(sprintf("  %-38s %8.3f ac  [pipeline undercounts]\n",
            "Gap (district – parcel union):", gap_acres))
cat(sprintf("  %-38s %8.3f ac  [pipeline overcounts]\n",
            "Overhang (union – district):", overhang_acres))
cat(sprintf("\n%-40s %8.3f ac\n",
            "GIS deductions in parcel union:", ded_union_acres))
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

# Gap: orange
if (!is.null(gap_sf) && gap_acres > 0) {
  m <- m |> add_fill_layer(
    id = "gap", source = sf::st_transform(gap_sf, 4326),
    fill_color = "#fd8d3c", fill_opacity = 0.65
  )
}

# Overhang: purple
if (!is.null(overhang_sf) && overhang_acres > 0) {
  m <- m |> add_fill_layer(
    id = "overhang", source = sf::st_transform(overhang_sf, 4326),
    fill_color = "#9e2a8f", fill_opacity = 0.65
  )
}

# GIS deductions within parcel union: red
if (nrow(ded_in_union) > 0) {
  m <- m |> add_fill_layer(
    id = "deductions", source = sf::st_transform(ded_in_union, 4326),
    fill_color = "#d73027", fill_opacity = 0.70
  )
}

# Parcel union outline: green; district boundary: black
m <- m |>
  add_line_layer(
    id = "union_outline", source = union_wgs,
    line_color = "#1a9850", line_width = 2.5
  ) |>
  add_line_layer(
    id = "district_outline", source = district_wgs,
    line_color = "#000000", line_width = 2.5
  ) |>
  add_legend(
    legend_title = sprintf("%s — Pipeline vs District Denominator", COMMUNITY),
    values = c(
      sprintf("District boundary (%.2f ac)", polygon_acres),
      sprintf("In-district parcels (n=%d, cap=%d)", nrow(in_d), capacity_total),
      sprintf("Gap: district − parcels (%.3f ac) — pipeline undercounts", gap_acres),
      sprintf("Overhang: parcels − district (%.3f ac) — pipeline overcounts",
              overhang_acres),
      sprintf("GIS deductions in parcel union (%.3f ac subtracted)", ded_union_acres),
      sprintf("Parcel union — pipeline base (%.3f ac → denom %.3f ac)",
              union_acres, denom_pipeline)
    ),
    colors = c("#000000", "#6baed6", "#fd8d3c", "#9e2a8f", "#d73027", "#1a9850"),
    type = "categorical"
  ) |>
  add_fullscreen_control(position = "top-left") |>
  add_navigation_control()

m
# To save for viewing on server:
# htmlwidgets::saveWidget(m, sprintf("dev/mcmc_testing/denom_map_mcmc_%s.html", COMMUNITY),
#                         selfcontained = TRUE)
