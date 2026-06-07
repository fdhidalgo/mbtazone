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
#' For in-district parcels, capacity comes directly from the pipeline's
#' pre-computed `final_lot_multi_family_unit_capacity`. For out-of-district
#' parcels (where that field is NA), capacity is estimated by running
#' `calculate_district_capacity()` under the most permissive district's zoning
#' parameters (the district with the highest `final_unit_capacity_per_district`).
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

  # Drop rows with no LOC_ID — incomplete records that carry no usable
  # geometry or attributes and would cause NA vertex names in igraph.
  n_na_loc <- sum(is.na(parcels_sf$LOC_ID))
  if (n_na_loc > 0) {
    cli::cli_warn("Dropping {n_na_loc} parcel{?s} with missing LOC_ID in {district_name}.")
    parcels_sf <- parcels_sf[!is.na(parcels_sf$LOC_ID), ]
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

  # Step 3: Estimate capacity for out-of-district parcels using the most
  # permissive district's zoning parameters. In-district parcels keep their
  # pipeline-computed capacity; out-of-district parcels are NA in the gpkg.
  # "Most permissive" = highest DU/AC (density)
  district_area_acres <- as.numeric(sf::st_area(districts_sf)) / 43560
  du_ac <- districts_sf$final_unit_capacity_per_district / district_area_acres
  best_idx <- which.max(du_ac)
  best_zoning_params <- sf::st_drop_geometry(districts_sf[best_idx, ])

  # TRANSIT column is "Y"/"N"; calculate_district_capacity() needs in_station_area
  parcels_sf$in_station_area <- parcels_sf$TRANSIT == "Y"

  capacity <- parcels_sf$final_lot_multi_family_unit_capacity
  out_mask  <- !parcels_sf$in_district

  if (any(out_mask)) {
    capacity_result <- calculate_district_capacity(
      parcels       = parcels_sf,
      zoning_params = best_zoning_params,
      precomputed   = TRUE
    )
    capacity[out_mask] <- capacity_result$final_unit_capacity[out_mask]
  }
  capacity[is.na(capacity)] <- 0

  in_station <- parcels_sf$in_station_area

  # Step 4: MCMC-ready data.table (all parcels; in_district flag preserved)
  district_parcels <- data.table::data.table(
    LOC_ID            = parcels_sf$LOC_ID,
    capacity          = capacity,
    area_acres        = parcels_sf$ACRES,
    in_district       = parcels_sf$in_district,
    in_station_bounds = in_station,
    lot_area_sqft     = parcels_sf$SQFT,
    developable_area  = parcels_sf$SQFT - parcels_sf$Tot_Exclud,
    excluded_area     = parcels_sf$Tot_Exclud
  )

  # Step 5: Geometry sf for adjacency graph (all parcels)
  district_geometry <- parcels_sf["LOC_ID"]
  district_geometry$capacity            <- capacity
  district_geometry$area                <- parcels_sf$ACRES
  district_geometry$in_station_bounds   <- in_station
  district_geometry$area_in_station     <- ifelse(in_station, parcels_sf$ACRES, 0)
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

  transit_stations       <- load_transit_stations()
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
