# mcmc_data_loading.R - Functions for loading district data for MCMC pipeline
# 
# Wrappers for loading district data for MCMC pipeline. Includes helper functions to find files based on district name, 
# and a main function to load all relevant data for a district into a structured format for MCMC processing.

#' Helper function to get paths given a district name
#'
#' @param district_name Name of district (e.g., "Norwood")
#' @param data_root Path to data directory (default: "data")
#' @return Named list with paths
#' @export
get_district_paths <- function(
    district_name,
    data_root = "data")
{
  # parcels data
  parcels_matches <- list.files(file.path(data_root, "land_record_shapefiles/basic/"),
                                pattern = paste0("^\\d{1,3}_", toupper(district_name), "_basic\\.zip$"),
                                full.names = TRUE)
  if (length(parcels_matches) == 0) {
    cli::cli_abort('No parcels data found for district {district_name}')
  }
  if (length(parcels_matches) > 1) {
    cli::cli_warn('Multiple parcels data files found for district {district_name}.')
  }
  cli::cli_alert_info('Using parcels data: {parcels_matches[1]}')
  parcels <- parcels_matches[1]

  # district data
  district_matches <- list.files(file.path(data_root, "mbta_district_shapefiles/", district_name),
                                 pattern = paste0("^", district_name, "_MBTAcommDistricts_.*\\.shp$"),
                                 full.names = TRUE)

  # If no .shp files found, try any .zip file
  if (length(district_matches) == 0) {
    district_matches <- list.files(file.path(data_root, "mbta_district_shapefiles/", district_name),
                                   pattern = "\\.zip$",
                                   full.names = TRUE)
  }

  if (length(district_matches) == 0) {
    cli::cli_abort('No district data found for district {district_name}')
  }
  if (length(district_matches) > 1) {
    cli::cli_warn('Multiple district data files found for district {district_name}.')
  }
  cli::cli_alert_info('Using district data: {district_matches[1]}')
  district <- district_matches[1]

  # district model
  excel_matches <- list.files(file.path(data_root, "mbta_district_models/", district_name),
                              pattern = paste0("^", district_name, " - CM.*\\.xlsx$"),
                              full.names = TRUE)

  if (length(excel_matches) == 0) {
    cli::cli_abort('No district model found for district {district_name}')
  }
  if (length(excel_matches) > 1) {
    cli::cli_warn('Multiple district models found for district {district_name}.')
  }
  cli::cli_alert_info('Using district model: {excel_matches[1]}')
  excel_model <- excel_matches[1]

  # Return paths
  list(
    district_name = district_name,
    parcels = parcels,
    district = district,
    excel_model = excel_model
  )
}

#' Load data for a single district
#' @param district_name Name of district (e.g., "Norwood")
#' @param district_type Community type, must be one of "rapid_transit", "commuter_rail", "adjacent" or "adjacent_small_town"
#' @param parcels Path to parcel shapefile or zip file
#' @param district Path to district shapefile
#' @param excel_model Path to district model
#' @param row_path Path to right-of-way shapefile (default: "data/Right_of_Way/...")
#' @return Named list with:
#'   - district_parcels: data.table with LOC_ID, capacity, area_acres, etc.
#'   - district_geometry: sf object with LOC_ID, geometry, capacity, area
#'   - district_right_of_way: sf object with ROW polygons
#'   - zoning_params: list of zoning parameters from Excel model
#'   - district_requirements: MBTA Communities Act requirements
#'   - transit_stations: sf object with 0.5-mile station buffers
#' @export
load_district_data <- function(
    district_name,
    district_type,
    parcels,
    district,
    excel_model,
    right_of_way = "data/Right_of_Way/Excluded_Land_Right_of_Way.shp"
) {
  paths <- list(
    parcels = parcels,
    district = district,
    excel_model = excel_model,
    right_of_way = right_of_way
  )

  # Step 1: Load District Parcels
  parcels_sf <- load_municipality(
    shapefile = paths$parcels,
    community_name = district_name,
    projection = 26986,
    validate = TRUE
  )

  # Step 2: Load MBTA District Boundary
  district_path = paths$district
  # Handle zip files
  if (tools::file_ext(district_path) == "zip") {
    # Create unique temp directory for this extraction
    temp_dir <- file.path(tempdir(), basename(tempfile()))
    dir.create(temp_dir, showWarnings = FALSE)
    utils::unzip(district_path, exdir = temp_dir)

    # Find the .shp file in extracted contents
    shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    if (length(shp_files) == 0) {
      cli::cli_abort("No .shp file found in zip archive: {.file {district_path}}")
    }
    if (length(shp_files) > 1) {
      cli::cli_warn("Multiple .shp files found, using: {.file {basename(shp_files[1])}}")
    }

    district_path <- shp_files[1]
  }

  district_sf <- sf::st_read(district_path, quiet = TRUE)
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
  district_requirements <- get_community_requirements(
    community_name = district_name,
    community_type = district_type
  )

  # Step 6: Pre-compute Station Area Attributes
  district_precomputed <- precompute_spatial_attributes(
    parcels = parcels_sf,
    station_areas = transit_stations,
    density_deductions = NULL,
    verbose = FALSE
  )

  # Step 7: Assign Parcels to MBTA District
  district_assignments <- assign_parcels_to_districts(
    municipality = district_precomputed,
    districts = district_sf
  )
  district_precomputed$in_district <- !is.na(district_assignments$district_id)

  # Step 8: Calculate Unit Capacities
  district_with_capacity <- calculate_district_capacity(
    parcels = district_precomputed,
    zoning_params = zoning_params,
    station_areas = transit_stations,
    precomputed = TRUE
  )

  # Step 9: Create MCMC-Ready data.table
  district_parcels <- data.table::data.table(
    LOC_ID = district_with_capacity$LOC_ID,
    capacity = district_with_capacity$final_unit_capacity,
    area_acres = district_with_capacity$ACRES,
    in_district = district_with_capacity$in_district,
    in_station_bounds = district_with_capacity$in_station_area,
    station_area_pct = district_with_capacity$station_area_pct,
    lot_area_sqft = district_with_capacity$SQFT,
    developable_area = district_with_capacity$developable_area,
    excluded_area = district_with_capacity$Tot_Exclud
  )
  district_parcels[is.na(capacity), capacity := 0]

  # Keep geometry separately for adjacency graph
  district_geometry <- district_with_capacity[, c("LOC_ID", "geometry")]
  district_geometry$capacity <- district_with_capacity$final_unit_capacity
  district_geometry$area <- district_with_capacity$ACRES
  district_geometry$capacity[is.na(district_geometry$capacity)] <- 0
  district_geometry$in_station_bounds <- district_with_capacity$in_station_area
  district_geometry$area_in_station <- ifelse(district_geometry$in_station_bounds,district_geometry$area,0)
  district_geometry$capacity_in_station <- ifelse(district_geometry$in_station_bounds,district_geometry$capacity,0)

  # Add centroids for diagnostic tracking
  centroids <- sf::st_centroid(sf::st_geometry(district_geometry))
  coords <- sf::st_coordinates(centroids)
  district_geometry$centroid_x <- coords[, 1]
  district_geometry$centroid_y <- coords[, 2]

  # Step 10: Load Right-of-Way Data
  right_of_way_sf <- sf::st_read(paths$right_of_way, quiet = TRUE)
  if (
    is.na(sf::st_crs(right_of_way_sf)$epsg) ||
    sf::st_crs(right_of_way_sf)$epsg != 26986
  ) {
    right_of_way_sf <- sf::st_transform(right_of_way_sf, 26986)
  }

  # Subset to District area
  district_bbox <- sf::st_bbox(district_geometry)
  district_bbox_poly <- sf::st_as_sfc(district_bbox)
  sf::st_crs(district_bbox_poly) <- sf::st_crs(district_geometry)

  district_right_of_way <- sf::st_intersection(
    right_of_way_sf,
    district_bbox_poly
  )
  district_right_of_way <- sf::st_make_valid(district_right_of_way)

  district_station_areas <- sf::st_intersection(
    transit_stations,
    district_bbox_poly
  )

  # Return all components
  list(
    district_parcels = district_parcels,
    district_geometry = district_geometry,
    district_right_of_way = district_right_of_way,
    zoning_params = zoning_params,
    district_requirements = district_requirements,
    transit_stations = district_station_areas,
    district_boundary = district_sf
  )
}
