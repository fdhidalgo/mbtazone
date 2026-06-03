# mcmc_data_loading.R - Functions for loading district data for MCMC pipeline
#
# Loads per-municipality GeoPackages produced by the mbta-data-pipeline.
# Each .gpkg contains two layers:
#   - parcels:   parcel geometries with pre-computed compliance fields
#   - districts: district boundary geometries with zoning parameters


#' Find the GeoPackage path for a given district name
#'
#' @param district_name Community name matching the .gpkg filename (e.g., "Norwood")
#' @param district_type Community type: one of "rapid_transit", "commuter_rail",
#'   "adjacent", or "adjacent_small_town"
#' @param pipeline_data_dir Path to the mbta_pipeline_data directory containing
#'   per-municipality .gpkg files
#' @return Named list with `district_name`, `district_type`, and `gpkg` (path)
#' @export
get_district_paths <- function(
    district_name,
    district_type,
    pipeline_data_dir)
{
  gpkg_name <- paste0(gsub(" ", "_", district_name), ".gpkg")
  gpkg_path <- file.path(pipeline_data_dir, gpkg_name)

  if (!file.exists(gpkg_path)) {
    cli::cli_abort(c(
      "No GeoPackage found for {district_name}.",
      "x" = "Expected: {.file {gpkg_path}}"
    ))
  }

  cli::cli_alert_info("Using GeoPackage: {gpkg_path}")
  list(
    district_name = district_name,
    district_type = district_type,
    gpkg          = gpkg_path
  )
}

#' Load data for a single community from its GeoPackage
#'
#' Reads the `parcels` and `districts` layers from the pre-built GeoPackage
#' produced by the mbta-data-pipeline.
#'
#' @param district_name Community name (e.g., "Norwood")
#' @param district_type Community type: one of "rapid_transit", "commuter_rail",
#'   "adjacent", or "adjacent_small_town"
#' @param gpkg Path to the community's .gpkg file
#' @param right_of_way Path to the statewide right-of-way shapefile
#' @return Named list with:
#'   - district_parcels: data.table with LOC_ID, capacity, area_acres, etc.
#'   - district_geometry: sf object with LOC_ID, geometry, capacity, area, etc.
#'   - district_right_of_way: sf object with ROW polygons clipped to bbox
#'   - zoning_params: data.frame of per-district zoning parameters from gpkg
#'   - district_requirements: MBTA Communities Act requirements
#'   - transit_stations: sf object with 0.5-mile station buffers, clipped
#'   - district_boundary: sf geometry — union of all district polygons
#' @export
load_district_data <- function(
    district_name,
    district_type,
    gpkg,
    right_of_way = "data/Right_of_Way/Excluded_Land_Right_of_Way.shp"
) {
  # Step 1: Read both layers from GeoPackage
  parcels_sf <- sf::st_read(gpkg, layer = "parcels", quiet = TRUE)
  if (is.na(sf::st_crs(parcels_sf)$epsg) || sf::st_crs(parcels_sf)$epsg != 26986) {
    parcels_sf <- sf::st_transform(parcels_sf, 26986)
  }

  # Drop rows with no loc_id — incomplete records that carry no usable
  # geometry or attributes and would cause NA vertex names in igraph.
  n_na_loc <- sum(is.na(parcels_sf$loc_id))
  if (n_na_loc > 0) {
    cli::cli_warn("Dropping {n_na_loc} parcel{?s} with missing loc_id in {district_name}.")
    parcels_sf <- parcels_sf[!is.na(parcels_sf$loc_id), ]
  }

  districts_sf <- sf::st_read(gpkg, layer = "districts", quiet = TRUE)
  if (is.na(sf::st_crs(districts_sf)$epsg) || sf::st_crs(districts_sf)$epsg != 26986) {
    districts_sf <- sf::st_transform(districts_sf, 26986)
  }

  # Step 2: Load community requirements
  district_requirements <- get_community_requirements(
    community_name = district_name,
    community_type = district_type
  )

  # Step 3: Build capacity vector (NA -> 0)
  capacity <- parcels_sf$final_lot_multi_family_unit_capacity
  capacity[is.na(capacity)] <- 0

  in_station <- !is.na(parcels_sf$transit_station) & parcels_sf$transit_station

  # Step 4: MCMC-ready data.table (all parcels; in_district flag preserved)
  district_parcels <- data.table::data.table(
    LOC_ID           = parcels_sf$loc_id,
    capacity         = capacity,
    area_acres       = parcels_sf$parcel_acres,
    in_district      = parcels_sf$in_district,
    in_station_bounds = in_station,
    lot_area_sqft    = parcels_sf$parcel_sf,
    developable_area = parcels_sf$developable_sf_for_unit_calc,
    excluded_area    = parcels_sf$total_excluded_land
  )

  # Step 5: Geometry sf for adjacency graph (all parcels)
  district_geometry <- parcels_sf["loc_id"]
  names(district_geometry)[names(district_geometry) == "loc_id"] <- "LOC_ID"
  district_geometry$capacity           <- capacity
  district_geometry$area               <- parcels_sf$parcel_acres
  district_geometry$in_station_bounds  <- in_station
  district_geometry$area_in_station    <- ifelse(in_station, parcels_sf$parcel_acres, 0)
  district_geometry$capacity_in_station <- ifelse(in_station, capacity, 0)

  centroids <- sf::st_centroid(sf::st_geometry(district_geometry))
  coords <- sf::st_coordinates(centroids)
  district_geometry$centroid_x <- coords[, 1]
  district_geometry$centroid_y <- coords[, 2]

  # Step 6: Clip right-of-way and transit stations to district bounding box
  right_of_way_sf <- sf::st_read(right_of_way, quiet = TRUE)
  if (is.na(sf::st_crs(right_of_way_sf)$epsg) || sf::st_crs(right_of_way_sf)$epsg != 26986) {
    right_of_way_sf <- sf::st_transform(right_of_way_sf, 26986)
  }

  bbox_poly <- sf::st_as_sfc(sf::st_bbox(district_geometry))
  sf::st_crs(bbox_poly) <- sf::st_crs(district_geometry)

  district_right_of_way <- sf::st_make_valid(
    sf::st_intersection(right_of_way_sf, bbox_poly)
  )

  transit_stations     <- load_transit_stations()
  district_station_areas <- sf::st_intersection(transit_stations, bbox_poly)

  # District boundary: union of all district polygons in the gpkg
  district_boundary <- sf::st_union(districts_sf)

  list(
    district_parcels      = district_parcels,
    district_geometry     = district_geometry,
    district_right_of_way = district_right_of_way,
    zoning_params         = sf::st_drop_geometry(districts_sf),
    district_requirements = district_requirements,
    transit_stations      = district_station_areas,
    district_boundary     = district_boundary
  )
}
