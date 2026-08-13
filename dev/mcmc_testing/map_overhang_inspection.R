# map_overhang_inspection.R
#
# Visualises overhang: in-district parcel polygons that extend beyond the
# district boundary, inflating the pipeline density denominator.
#
# Map layers:
#   Light grey  — all parcels (context)
#   Blue        — in-district parcels (full polygon extent)
#   Black       — district boundary
#   Red         — overhang: parcel area outside the district
#
# Also prints the top overhang-contributing parcels by area outside district.
#
# Usage:
#   COMMUNITY <- "Worcester"
#   source("dev/mcmc_testing/map_overhang_inspection.R")
#
# Overhang-dominated communities:
#   Worcester (+92 ac), Shrewsbury (+10 ac), Somerville (+22 ac)
#
# Required env vars: MBTAZONE_PIPELINE_DATA, MBTAZONE_RIGHT_OF_WAY,
#                    MBTAZONE_DENSITY_DEDUCTIONS

library(sf)
library(mapgl)
library(mbtazone)

if (!exists("COMMUNITY")) COMMUNITY <- "Worcester"

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
row_path          <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")

if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(row_path))          stop("MBTAZONE_RIGHT_OF_WAY not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")

gpkg <- file.path(pipeline_data_dir, paste0(gsub(" ", "_", COMMUNITY), ".gpkg"))
if (!file.exists(gpkg)) stop("GeoPackage not found: ", gpkg)

if (!exists("deductions_global") || !inherits(deductions_global, "sf")) {
  cat("Loading density deductions (cached)...\n")
  deductions_global <- sf::st_make_valid(
    sf::st_transform(sf::st_read(density_ded_path, quiet = TRUE), 26986)
  )
}

community_info <- read.csv(
  system.file("extdata/community_info.csv", package = "mbtazone"),
  stringsAsFactors = FALSE
)
ci_row        <- community_info[tolower(community_info$community_name) ==
                                  tolower(gsub("_", " ", COMMUNITY)), ]
district_type <- ci_row$community_type[1]

cat("Loading", COMMUNITY, "...\n")
district_data <- load_district_data(
  district_name      = COMMUNITY,
  district_type      = district_type,
  gpkg               = gpkg,
  right_of_way       = row_path,
  density_deductions = deductions_global
)

all_sf      <- district_data$district_geometry
in_d_sf     <- all_sf[district_data$district_parcels$in_district == TRUE, ]
district_sf <- sf::st_sf(geometry = sf::st_make_valid(district_data$district_boundary))

# Overhang: per-parcel area outside the district
dist_union   <- sf::st_union(district_sf)
parcel_union <- sf::st_union(in_d_sf)

overhang_sf <- tryCatch(
  sf::st_make_valid(suppressWarnings(sf::st_difference(parcel_union, dist_union))),
  error = function(e) NULL
)
overhang_acres <- if (!is.null(overhang_sf))
  as.numeric(sf::st_area(overhang_sf)) / 4047 else 0

cat(sprintf("  In-district parcels: %d\n", nrow(in_d_sf)))
cat(sprintf("  District area:       %.1f ac\n", as.numeric(sf::st_area(district_sf)) / 4047))
cat(sprintf("  Parcel union area:   %.1f ac\n", as.numeric(sf::st_area(parcel_union)) / 4047))
cat(sprintf("  Overhang:            %.1f ac\n\n", overhang_acres))

# Per-parcel overhang — which parcels contribute the most?
cat("Computing per-parcel overhang (this may take a moment)...\n")
outside_geom <- suppressWarnings(sf::st_difference(in_d_sf, district_sf))
outside_geom$outside_acres <- as.numeric(sf::st_area(outside_geom)) / 4047
top_overhang <- outside_geom[order(-outside_geom$outside_acres), ]
top_overhang <- top_overhang[top_overhang$outside_acres > 0.01, c("LOC_ID", "outside_acres")]
sf::st_geometry(top_overhang) <- NULL  # drop geometry for printing

cat(sprintf("Parcels with > 0.01 ac outside district: %d\n", nrow(top_overhang)))
cat("Top 10 by overhang area:\n")
print(head(top_overhang, 10))
cat("\n")

# Map
all_wgs      <- sf::st_transform(all_sf,      4326)
in_d_wgs     <- sf::st_transform(in_d_sf,     4326)
dist_wgs     <- sf::st_transform(district_sf, 4326)
bounds       <- sf::st_bbox(in_d_wgs)

m <- maplibre(style = carto_style("positron"), bounds = bounds) |>
  add_fill_layer(id = "all_parcels", source = all_wgs,
                 fill_color = "#cccccc", fill_opacity = 0.15) |>
  add_fill_layer(id = "in_district", source = in_d_wgs,
                 fill_color = "#6baed6", fill_opacity = 0.40,
                 tooltip = concat("<b>", get_column("LOC_ID"), "</b>")) |>
  add_line_layer(id = "district_outline", source = dist_wgs,
                 line_color = "#000000", line_width = 2.5)

if (!is.null(overhang_sf) && overhang_acres > 0) {
  m <- m |> add_fill_layer(
    id = "overhang", source = sf::st_transform(overhang_sf, 4326),
    fill_color = "#e31a1c", fill_opacity = 0.65
  )
}

m <- m |> add_legend(
  legend_title = sprintf("%s — overhang inspection\nOverhang: %.1f ac",
                         COMMUNITY, overhang_acres),
  values = c("In-district parcels (full extent)",
             sprintf("Overhang — outside district: %.1f ac", overhang_acres),
             "District boundary"),
  colors = c("#6baed6", "#e31a1c", "#000000"),
  type = "categorical", circular_patches = FALSE
)

m
