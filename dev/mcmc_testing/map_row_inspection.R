# map_row_inspection.R
#
# Interactive map showing Right-of-Way shapefile coverage for a community,
# alongside the district boundary, in-district parcels, and the gap area
# (inside district but not covered by any parcel polygon).
#
# Uses load_district_data() — the same function as the targets pipeline and
# QMD reports — so geometry extent and ROW clipping match exactly.
#
# Map layers:
#   Light grey  — all parcels in the GeoPackage (full community context)
#   Blue        — in-district parcels
#   Black       — district boundary
#   Orange      — gap: inside district but no parcel polygon
#   Teal        — ROW polygons clipped to community bounding box
#
# Usage (run from the mbtazone package root):
#   COMMUNITY <- "Fall_River"
#   source("dev/mcmc_testing/map_row_inspection.R")
#
# Gap-dominated communities to inspect:
#   Fall_River (-205 ac), New_Bedford (-32 ac), Taunton (-17 ac),
#   Chelsea (-16 ac), Hanson (-6 ac), Groton (-6 ac), Medford (-3 ac)
#
# Required env vars:
#   MBTAZONE_PIPELINE_DATA     — directory of per-community .gpkg files
#   MBTAZONE_RIGHT_OF_WAY      — path to Excluded_Land_Right_of_Way.shp
#   MBTAZONE_DENSITY_DEDUCTIONS — path to Density_Denominator_Deductions.shp

library(sf)
library(mapgl)
library(mbtazone)

if (!exists("COMMUNITY")) COMMUNITY <- "Fall_River"

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
row_path          <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")

if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(row_path))          stop("MBTAZONE_RIGHT_OF_WAY not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")

gpkg <- file.path(pipeline_data_dir, paste0(gsub(" ", "_", COMMUNITY), ".gpkg"))
if (!file.exists(gpkg)) stop("GeoPackage not found: ", gpkg)

# Cache density deductions across source() calls (slow to load statewide)
if (!exists("deductions_global") || !inherits(deductions_global, "sf")) {
  cat("Loading density deductions (cached for subsequent source() calls)...\n")
  deductions_global <- sf::st_make_valid(
    sf::st_transform(sf::st_read(density_ded_path, quiet = TRUE), 26986)
  )
  cat(sprintf("  Loaded %d deduction features\n\n", nrow(deductions_global)))
} else {
  cat(sprintf("Using cached deductions (%d features)\n\n", nrow(deductions_global)))
}

# Look up district_type from community_info.csv
community_info <- read.csv(
  system.file("extdata/community_info.csv", package = "mbtazone"),
  stringsAsFactors = FALSE
)
cname_clean <- gsub("_", " ", COMMUNITY)
ci_row <- community_info[tolower(community_info$community_name) == tolower(cname_clean), ]
if (nrow(ci_row) == 0) stop("Community not found in community_info.csv: ", COMMUNITY)
district_type <- ci_row$community_type[1]

# Load using the same function as the targets pipeline and QMD reports
cat("Loading", COMMUNITY, "(", district_type, ")...\n")
district_data <- load_district_data(
  district_name      = COMMUNITY,
  district_type      = district_type,
  gpkg               = gpkg,
  right_of_way       = row_path,
  density_deductions = deductions_global
)

# All parcels in the GeoPackage — same as district_data$district_geometry in QMD
all_sf    <- district_data$district_geometry
# In-district subset (in_district flag from district_parcels; row order is preserved)
in_d_sf   <- all_sf[district_data$district_parcels$in_district == TRUE, ]
# ROW already clipped to community bbox by load_district_data()
local_row <- district_data$district_right_of_way
# District boundary
district_sf <- sf::st_sf(geometry = sf::st_make_valid(district_data$district_boundary))

cat(sprintf("  Total parcels in GeoPackage: %d\n", nrow(all_sf)))
cat(sprintf("  In-district parcels:         %d\n", nrow(in_d_sf)))
cat(sprintf("  District area:               %.1f ac\n",
            as.numeric(sf::st_area(district_sf)) / 4047))

# Gap: inside district but not covered by any in-district parcel polygon
parcel_union <- sf::st_sf(geometry = sf::st_union(in_d_sf))
gap_sf <- tryCatch(
  sf::st_make_valid(suppressWarnings(sf::st_difference(district_sf, parcel_union))),
  error = function(e) NULL
)
gap_acres <- if (!is.null(gap_sf)) as.numeric(sf::st_area(gap_sf)) / 4047 else 0
cat(sprintf("  Gap (district − parcels):    %.1f ac\n\n", gap_acres))

row_acres <- if (nrow(local_row) > 0)
  as.numeric(sf::st_area(sf::st_union(local_row))) / 4047 else 0

row_in_gap <- if (!is.null(gap_sf) && nrow(local_row) > 0) {
  tryCatch(
    suppressWarnings(sf::st_intersection(gap_sf, local_row)),
    error = function(e) local_row[0, ]
  )
} else local_row[0, ]
row_in_gap_acres <- if (nrow(row_in_gap) > 0)
  as.numeric(sf::st_area(sf::st_union(row_in_gap))) / 4047 else 0

cat(sprintf("ROW in community bbox: %d features (%.1f ac)\n", nrow(local_row), row_acres))
cat(sprintf("ROW within gap:        %.1f ac (%.0f%% of gap)\n\n",
            row_in_gap_acres,
            if (gap_acres > 0) 100 * row_in_gap_acres / gap_acres else 0))

# ---- Build map --------------------------------------------------------------
all_wgs   <- sf::st_transform(all_sf,       4326)
in_d_wgs  <- sf::st_transform(in_d_sf,      4326)
dist_wgs  <- sf::st_transform(district_sf,  4326)
bounds    <- sf::st_bbox(all_wgs)

m <- maplibre(style = carto_style("positron"), bounds = bounds) |>
  add_fill_layer(
    id = "all_parcels", source = all_wgs,
    fill_color = "#cccccc", fill_opacity = 0.20
  ) |>
  add_line_layer(
    id = "all_parcels_line", source = all_wgs,
    line_color = "#888888", line_width = 0.2, line_opacity = 0.35
  ) |>
  add_fill_layer(
    id = "in_district", source = in_d_wgs,
    fill_color = "#6baed6", fill_opacity = 0.45,
    tooltip = concat("<b>", get_column("LOC_ID"), "</b>")
  )

if (!is.null(gap_sf) && gap_acres > 0) {
  m <- m |> add_fill_layer(
    id = "gap", source = sf::st_transform(gap_sf, 4326),
    fill_color = "#fd8d3c", fill_opacity = 0.60
  )
}

if (nrow(local_row) > 0) {
  row_wgs <- sf::st_transform(local_row, 4326)
  m <- m |>
    add_fill_layer(
      id = "row_fill", source = row_wgs,
      fill_color = "#1d9fa6", fill_opacity = 0.55
    ) |>
    add_line_layer(
      id = "row_line", source = row_wgs,
      line_color = "#0d6e73", line_width = 0.5
    )
}

m <- m |>
  add_line_layer(
    id = "district_outline", source = dist_wgs,
    line_color = "#000000", line_width = 2.5
  ) |>
  add_legend(
    legend_title = sprintf(
      "%s — ROW inspection\nGap: %.1f ac  |  ROW in gap: %.1f ac (%.0f%%)",
      COMMUNITY, gap_acres, row_in_gap_acres,
      if (gap_acres > 0) 100 * row_in_gap_acres / gap_acres else 0
    ),
    values = c("All parcels (GeoPackage)",
               "In-district parcels",
               sprintf("Gap — no parcel coverage: %.1f ac", gap_acres),
               sprintf("ROW shapefile: %.1f ac in bbox", row_acres),
               "District boundary"),
    colors = c("#cccccc", "#6baed6", "#fd8d3c", "#1d9fa6", "#000000"),
    type = "categorical",
    circular_patches = FALSE
  )

m
