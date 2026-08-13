# plot_density_denominator.R
#
# Interactive zoomable map of a district's GIS density denominator deductions.
# Use to diagnose large GIS-vs-Excel denominator discrepancies.
#
# Requires: MBTAZONE_PIPELINE_DATA, MBTAZONE_DENSITY_DEDUCTIONS env vars.
# Run from the mbtazone package root.

library(sf)
library(mapgl)
library(data.table)

COMMUNITY <- "Essex"   # <-- change to Salem, North_Reading, Sherborn, etc.

# ---- Paths ------------------------------------------------------------------

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")
if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")

gpkg <- file.path(pipeline_data_dir, paste0(gsub(" ", "_", COMMUNITY), ".gpkg"))
if (!file.exists(gpkg)) stop("GeoPackage not found: ", gpkg)

# ---- Load data --------------------------------------------------------------

cat("Loading", COMMUNITY, "...\n")
parcels   <- sf::st_make_valid(sf::st_read(gpkg, layer = "parcels",   quiet = TRUE))
districts <- sf::st_make_valid(sf::st_read(gpkg, layer = "districts", quiet = TRUE))

in_d          <- parcels[parcels$in_district == TRUE, ]
district_poly <- sf::st_union(districts)
polygon_acres <- as.numeric(sf::st_area(district_poly)) / 4047

cat("Loading deductions shapefile...\n")
deductions <- sf::st_read(density_ded_path, quiet = TRUE)
deductions <- sf::st_transform(deductions, sf::st_crs(districts))
deductions <- sf::st_make_valid(deductions)

district_sf     <- sf::st_sf(geometry = district_poly)
ded_in_district <- sf::st_intersection(district_sf, deductions)
ded_acres       <- as.numeric(sum(sf::st_area(ded_in_district))) / 4047
denom_gis       <- polygon_acres - ded_acres

cat(sprintf("\nDistrict polygon:  %.3f ac\n", polygon_acres))
cat(sprintf("GIS deductions:    %.3f ac  (%d features)\n", ded_acres, nrow(ded_in_district)))
cat(sprintf("GIS denominator:   %.3f ac\n\n", denom_gis))

# Add per-feature area for tooltips
ded_in_district$ded_acres_feat <- round(
  as.numeric(sf::st_area(ded_in_district)) / 4047, 4
)

# Identify type field if present
type_field <- intersect(
  c("type", "Type", "Category", "category", "SOURCE", "source", "layer", "Layer"),
  names(ded_in_district)
)
if (length(type_field) > 0) {
  ded_in_district$ded_type <- as.character(
    sf::st_drop_geometry(ded_in_district)[[type_field[1]]]
  )
  cat("Deduction types found:\n")
  dt <- as.data.table(sf::st_drop_geometry(ded_in_district))
  print(dt[, .(n = .N, acres = round(sum(ded_acres_feat), 3)), by = ded_type][order(-acres)])
} else {
  ded_in_district$ded_type <- "deduction"
}

# ---- Transform to WGS84 for maplibre ----------------------------------------

parcels_wgs  <- sf::st_transform(parcels,          4326)
in_d_wgs     <- sf::st_transform(in_d,             4326)
district_wgs <- sf::st_transform(district_sf,      4326)
ded_wgs      <- sf::st_transform(ded_in_district,  4326)

bounds <- sf::st_bbox(district_wgs)

# ---- Build map --------------------------------------------------------------

capacity_total <- sum(in_d$final_lot_multi_family_unit_capacity, na.rm = TRUE)

maplibre(style = carto_style("positron"), bounds = bounds) |>

  # All parcels — faint background
  add_fill_layer(
    id           = "all_parcels_fill",
    source       = parcels_wgs,
    fill_color   = "#dddddd",
    fill_opacity = 0.3
  ) |>
  add_line_layer(
    id           = "all_parcels_line",
    source       = parcels_wgs,
    line_color   = "#aaaaaa",
    line_width   = 0.3,
    line_opacity = 0.5
  ) |>

  # In-district parcels — blue
  add_fill_layer(
    id           = "in_district_fill",
    source       = in_d_wgs,
    fill_color   = "#6baed6",
    fill_opacity = 0.5,
    tooltip      = concat(
      "<b>Parcel</b>: ", get_column("LOC_ID"),
      "<br>ACRES: ",     get_column("ACRES")
    )
  ) |>
  add_line_layer(
    id         = "in_district_line",
    source     = in_d_wgs,
    line_color = "#2171b5",
    line_width = 0.5
  ) |>

  # GIS deductions — red, semi-transparent
  add_fill_layer(
    id           = "deductions_fill",
    source       = ded_wgs,
    fill_color   = "#d73027",
    fill_opacity = 0.55,
    tooltip      = concat(
      "<b>GIS deduction</b>",
      "<br>Type: ",  get_column("ded_type"),
      "<br>Area: ",  get_column("ded_acres_feat"), " ac"
    )
  ) |>
  add_line_layer(
    id         = "deductions_line",
    source     = ded_wgs,
    line_color = "#7f0000",
    line_width = 0.5
  ) |>

  # District boundary — thick black outline
  add_line_layer(
    id         = "district_outline",
    source     = district_wgs,
    line_color = "#000000",
    line_width = 2.5
  ) |>

  add_legend(
    legend_title = sprintf(
      "%s — Density Denominator", COMMUNITY
    ),
    values = c(
      sprintf("District boundary (%.2f ac)", polygon_acres),
      sprintf("In-district parcels (%d, cap=%d)", nrow(in_d), capacity_total),
      sprintf("GIS deductions (%.2f ac subtracted)", ded_acres),
      sprintf("GIS denominator = %.3f ac", denom_gis)
    ),
    colors = c("#000000", "#6baed6", "#d73027", "#ffffff")
  ) |>

  add_fullscreen_control(position = "top-left") |>
  add_navigation_control()
