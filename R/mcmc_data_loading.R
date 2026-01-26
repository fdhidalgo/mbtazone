# data_loading.R - Load Norwood municipal data for MCMC
#
# Wraps norwood_setup.R logic into a reusable function for targets pipeline.

#' Load Norwood Data for MCMC
#'
#' Loads parcel data, zoning parameters, requirements, and right-of-way polygons
#' for the Norwood municipality. This is the main data loading target.
#'
#' @param data_root Path to data directory (default: "data")
#' @param row_path Path to right-of-way shapefile (default: "data/Right_of_Way/...")
#' @return Named list with:
#'   - norwood_parcels: data.table with LOC_ID, capacity, area_acres, etc.
#'   - norwood_geometry: sf object with LOC_ID, geometry, capacity, area
#'   - norwood_right_of_way: sf object with ROW polygons
#'   - zoning_params: list of zoning parameters from Excel model
#'   - norwood_requirements: MBTA Communities Act requirements
#'   - transit_stations: sf object with 0.5-mile station buffers
#' @export
load_norwood_data <- function(
  data_root = "data",
  row_path = "data/Right_of_Way/Excluded_Land_Right_of_Way.shp"
) {

  # Define paths
  paths <- list(
    parcels = file.path(
      data_root,
      "land_record_shapefiles/basic/220_NORWOOD_basic.zip"
    ),
    district = file.path(
      data_root,
      "mbta_district_shapefiles/Norwood/Norwood_MBTAcommDistricts_20240723.shp"
    ),
    excel_model = file.path(
      data_root,
      "mbta_district_models/Norwood/Norwood - CM.xlsx"
    ),
    right_of_way = row_path
  )

  # Step 1: Load Norwood Parcels
  norwood_sf <- load_municipality(
    shapefile = paths$parcels,
    community_name = "Norwood",
    projection = 26986,
    validate = TRUE
  )

  # Step 2: Load MBTA District Boundary
  district_sf <- sf::st_read(paths$district, quiet = TRUE)
  if (
    is.na(sf::st_crs(district_sf)$epsg) ||
      sf::st_crs(district_sf)$epsg != 26986
  ) {
    district_sf <- sf::st_transform(district_sf, 26986)
  }

  # Step 3: Load Transit Station Areas
  transit_stations <- load_transit_stations()

  # Step 4: Extract Zoning Parameters
  zoning_params <- extract_zoning_parameters(
    excel_path = paths$excel_model,
    district = 1
  )

  # Step 5: Load Requirements
  norwood_requirements <- get_community_requirements(
    community_name = "Norwood",
    community_type = "commuter_rail"
  )

  # Step 6: Pre-compute Station Area Attributes
  norwood_precomputed <- precompute_spatial_attributes(
    parcels = norwood_sf,
    station_areas = transit_stations,
    density_deductions = NULL,
    verbose = FALSE
  )

  # Step 7: Assign Parcels to MBTA District
  district_assignments <- assign_parcels_to_districts(
    municipality = norwood_precomputed,
    districts = district_sf
  )
  norwood_precomputed$in_district <- !is.na(district_assignments$district_id)

  # Step 8: Calculate Unit Capacities
  norwood_with_capacity <- calculate_district_capacity(
    parcels = norwood_precomputed,
    zoning_params = zoning_params,
    station_areas = transit_stations,
    precomputed = TRUE
  )

  # Step 9: Create MCMC-Ready data.table
  norwood_parcels <- data.table::data.table(
    LOC_ID = norwood_with_capacity$LOC_ID,
    capacity = norwood_with_capacity$final_unit_capacity,
    area_acres = norwood_with_capacity$ACRES,
    in_district = norwood_with_capacity$in_district,
    in_station_bounds = norwood_with_capacity$in_station_area,
    station_area_pct = norwood_with_capacity$station_area_pct,
    lot_area_sqft = norwood_with_capacity$SQFT,
    developable_area = norwood_with_capacity$developable_area,
    excluded_area = norwood_with_capacity$Tot_Exclud
  )
  norwood_parcels[is.na(capacity), capacity := 0]

  # Keep geometry separately for adjacency graph
  norwood_geometry <- norwood_with_capacity[, c("LOC_ID", "geometry")]
  norwood_geometry$capacity <- norwood_with_capacity$final_unit_capacity
  norwood_geometry$area <- norwood_with_capacity$ACRES
  norwood_geometry$capacity[is.na(norwood_geometry$capacity)] <- 0
  norwood_geometry$in_station_bounds <- norwood_with_capacity$in_station_area
  norwood_geometry$area_in_station <- ifelse(norwood_geometry$in_station_bounds,norwood_geometry$area,0)
  norwood_geometry$capacity_in_station <- ifelse(norwood_geometry$in_station_bounds,norwood_geometry$capacity,0)

  # Add centroids for diagnostic tracking
  centroids <- sf::st_centroid(sf::st_geometry(norwood_geometry))
  coords <- sf::st_coordinates(centroids)
  norwood_geometry$centroid_x <- coords[, 1]
  norwood_geometry$centroid_y <- coords[, 2]

  # Step 10: Load Right-of-Way Data
  right_of_way_sf <- sf::st_read(paths$right_of_way, quiet = TRUE)
  if (
    is.na(sf::st_crs(right_of_way_sf)$epsg) ||
      sf::st_crs(right_of_way_sf)$epsg != 26986
  ) {
    right_of_way_sf <- sf::st_transform(right_of_way_sf, 26986)
  }

  # Subset to Norwood area
  norwood_bbox <- sf::st_bbox(norwood_geometry)
  norwood_bbox_poly <- sf::st_as_sfc(norwood_bbox)
  sf::st_crs(norwood_bbox_poly) <- sf::st_crs(norwood_geometry)

  norwood_right_of_way <- sf::st_intersection(
    right_of_way_sf,
    norwood_bbox_poly
  )
  norwood_right_of_way <- sf::st_make_valid(norwood_right_of_way)

  # Return all components
  list(
    norwood_parcels = norwood_parcels,
    norwood_geometry = norwood_geometry,
    norwood_right_of_way = norwood_right_of_way,
    zoning_params = zoning_params,
    norwood_requirements = norwood_requirements,
    transit_stations = transit_stations,
    district_boundary = district_sf
  )
}
