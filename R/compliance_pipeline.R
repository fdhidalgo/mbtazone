#' Compliance Evaluation Pipeline
#'
#' Functions to orchestrate complete compliance evaluation for the Massachusetts
#' MBTA Communities Act. These functions integrate data loading, GIS operations,
#' unit capacity calculations, and compliance assessment into a complete workflow.
#'
#' @name compliance_pipeline
NULL

#' Assign Parcels to Districts
#'
#' Helper function to assign parcels to districts using flexible input methods.
#' Supports district specification via spatial polygons, column names, or
#' explicit parcel ID lists.
#'
#' @param municipality An sf object containing parcel data (from
#'   \code{\link{load_municipality}})
#' @param districts District specification in one of three formats:
#'   \itemize{
#'     \item \strong{sf polygon(s)}: Spatial intersection assigns parcels to districts
#'     \item \strong{character}: Column name in municipality containing district IDs
#'     \item \strong{list}: Named list mapping district IDs to LOC_ID vectors
#'   }
#'
#' @return A data.frame with columns:
#'   \itemize{
#'     \item \code{LOC_ID}: Parcel identifier
#'     \item \code{district_id}: Assigned district identifier
#'     \item \code{district_name}: District name (if available)
#'   }
#'
#' @details
#' This function handles three common district specification patterns:
#'
#' \strong{Method 1: Spatial Intersection}
#'
#' Provide an sf object with district boundary polygons. Parcels are assigned
#' to districts based on spatial intersection (centroid method to avoid
#' boundary issues).
#'
#' \strong{Method 2: Column Name}
#'
#' Provide the name of a column in the municipality data that contains district
#' assignments (e.g., "ZONING_DIST").
#'
#' \strong{Method 3: Explicit Mapping}
#'
#' Provide a named list where names are district IDs and values are vectors of
#' LOC_ID values.
#'
#' @examples
#' \dontrun{
#' parcels <- load_municipality("cambridge.zip")
#'
#' # Method 1: Spatial intersection
#' district_boundary <- sf::st_read("district.shp")
#' assignments <- assign_parcels_to_districts(parcels, district_boundary)
#'
#' # Method 2: Use existing column
#' assignments <- assign_parcels_to_districts(parcels, "ZONING_DIST")
#'
#' # Method 3: Explicit list
#' assignments <- assign_parcels_to_districts(
#'   parcels,
#'   list("District_1" = c("LOC_001", "LOC_002"),
#'        "District_2" = c("LOC_003", "LOC_004"))
#' )
#' }
#'
#' @keywords internal
assign_parcels_to_districts <- function(municipality, districts) {

  # Validate municipality input
  if (!inherits(municipality, "sf")) {
    cli::cli_abort("municipality must be an sf object from load_municipality()")
  }

  if (!"LOC_ID" %in% names(municipality)) {
    cli::cli_abort("municipality must have LOC_ID column")
  }

  # Method 1: Spatial intersection with sf polygon(s)
  if (inherits(districts, "sf")) {

    # Validate district geometry
    if (!all(sf::st_geometry_type(districts) %in% c("POLYGON", "MULTIPOLYGON"))) {
      cli::cli_abort("District sf object must contain POLYGON or MULTIPOLYGON geometries")
    }

    # Ensure matching CRS
    if (sf::st_crs(municipality) != sf::st_crs(districts)) {
      cli::cli_warn("CRS mismatch detected, transforming districts to match municipality")
      districts <- sf::st_transform(districts, sf::st_crs(municipality))
    }

    # Use centroid method to avoid boundary assignment issues
    parcel_centroids <- sf::st_centroid(municipality)

    # Find which district each parcel centroid falls within
    intersection <- sf::st_intersects(parcel_centroids, districts, sparse = TRUE)

    # Extract district assignments (row indices)
    district_assignment <- sapply(intersection, function(x) {
      if (length(x) == 0) return(NA_character_)
      if (length(x) > 1) {
        cli::cli_warn("Parcel in multiple districts, using first match")
      }
      as.character(x[1])
    })

    # Try to extract district IDs if available
    district_id_col <- NULL
    if ("district_id" %in% names(districts)) district_id_col <- "district_id"
    else if ("DISTRICT_ID" %in% names(districts)) district_id_col <- "DISTRICT_ID"
    else if ("id" %in% names(districts)) district_id_col <- "id"
    else if ("ID" %in% names(districts)) district_id_col <- "ID"

    if (!is.null(district_id_col)) {
      district_ids <- districts[[district_id_col]][as.integer(district_assignment)]
    } else {
      district_ids <- district_assignment  # fallback to row index
    }

    # Try to extract district names if available
    district_name_col <- NULL
    if ("name" %in% names(districts)) district_name_col <- "name"
    else if ("NAME" %in% names(districts)) district_name_col <- "NAME"
    else if ("district_name" %in% names(districts)) district_name_col <- "district_name"

    if (!is.null(district_name_col)) {
      district_names <- districts[[district_name_col]][as.integer(district_assignment)]
    } else {
      district_names <- paste0("District_", district_assignment)
    }

    result <- data.frame(
      LOC_ID = municipality$LOC_ID,
      district_id = district_ids,
      district_name = district_names,
      stringsAsFactors = FALSE
    )

    # Warn about unassigned parcels
    n_unassigned <- sum(is.na(result$district_id))
    if (n_unassigned > 0) {
      cli::cli_warn(
        "{n_unassigned} parcel{?s} not assigned to any district (outside boundaries)"
      )
    }

    return(result)
  }

  # Method 2: Column name in municipality data
  if (is.character(districts) && length(districts) == 1) {

    if (!districts %in% names(municipality)) {
      cli::cli_abort(
        "Column {.field {districts}} not found in municipality data"
      )
    }

    district_values <- municipality[[districts]]

    result <- data.frame(
      LOC_ID = municipality$LOC_ID,
      district_id = as.character(district_values),
      district_name = as.character(district_values),
      stringsAsFactors = FALSE
    )

    # Warn about missing assignments
    n_missing <- sum(is.na(result$district_id) | result$district_id == "")
    if (n_missing > 0) {
      cli::cli_warn(
        "{n_missing} parcel{?s} have missing or empty district assignments"
      )
    }

    return(result)
  }

  # Method 3: Named list mapping district IDs to LOC_ID vectors
  if (is.list(districts)) {

    if (is.null(names(districts)) || any(names(districts) == "")) {
      cli::cli_abort("District list must have named elements (district IDs)")
    }

    # Check all LOC_IDs are valid
    all_loc_ids <- unlist(districts, use.names = FALSE)
    invalid_ids <- setdiff(all_loc_ids, municipality$LOC_ID)

    if (length(invalid_ids) > 0) {
      cli::cli_abort(
        "Invalid LOC_ID values in district list: {.val {head(invalid_ids, 5)}}"
      )
    }

    # Check for duplicate assignments
    if (length(all_loc_ids) != length(unique(all_loc_ids))) {
      cli::cli_abort("Some parcels assigned to multiple districts")
    }

    # Create assignment data.frame
    assignments_list <- lapply(names(districts), function(district_name) {
      data.frame(
        LOC_ID = districts[[district_name]],
        district_id = district_name,
        district_name = district_name,
        stringsAsFactors = FALSE
      )
    })

    result <- do.call(rbind, assignments_list)

    # Warn about unassigned parcels
    n_unassigned <- nrow(municipality) - nrow(result)
    if (n_unassigned > 0) {
      cli::cli_warn(
        "{n_unassigned} parcel{?s} not assigned to any district"
      )
    }

    return(result)
  }

  # Invalid input
  cli::cli_abort(
    "districts must be an sf object, column name, or named list, not {.cls {class(districts)}}"
  )
}

#' Calculate District Capacity
#'
#' Apply all unit capacity calculations to parcels in a district. Chains together
#' all 18 calculation functions from the MBTA Communities compliance model to
#' determine final unit capacity for each parcel.
#'
#' @param parcels An sf object containing parcel data with required columns
#'   (from \code{\link{load_municipality}})
#' @param zoning_params Named list of zoning parameters for the district (from
#'   \code{\link{extract_zoning_parameters}} or manual specification). Required elements:
#'   \itemize{
#'     \item min_lot_size
#'     \item base_min_lot_size
#'     \item additional_lot_SF
#'     \item building_height
#'     \item FAR
#'     \item max_lot_coverage
#'     \item min_required_open_space
#'     \item parking_spaces_per_dwelling_unit
#'     \item lot_area_per_dwelling_unit
#'     \item max_dwelling_units_per_acre
#'     \item max_units_per_lot
#'     \item water_included
#'   }
#' @param station_areas Optional sf object with transit station buffers (from
#'   \code{\link{load_transit_stations}}). If provided, adds station area flag
#'   to parcels.
#' @param override_developable_sf Optional numeric vector of manual override
#'   values for developable area. Use NA for parcels without overrides.
#'
#' @return The input sf object with 18 additional columns representing each
#'   calculation step:
#'   \itemize{
#'     \item \code{developable_area} - Column N
#'     \item \code{net_developable_area} - Column Q
#'     \item \code{exclusion_ratio} - Column R
#'     \item \code{open_space_requirement} - Column S
#'     \item \code{required_open_space_area} - Column T
#'     \item \code{parking_area} - Column U
#'     \item \code{building_footprint} - Column V
#'     \item \code{building_floor_area} - Column W
#'     \item \code{units_building_capacity} - Column X
#'     \item \code{units_density_limits} - Column Y
#'     \item \code{units_lot_coverage} - Column Z
#'     \item \code{units_lot_area_req} - Column AA
#'     \item \code{units_far_limits} - Column AB
#'     \item \code{units_max_cap} - Column AC (adjusted units)
#'     \item \code{below_minimum_lot} - Column AD
#'     \item \code{units_graduated_lots} - Column AE
#'     \item \code{final_unit_capacity} - Column AF
#'     \item \code{units_per_acre} - Column AG
#'   }
#'
#' @details
#' This function implements the complete unit capacity calculation pipeline from
#' the Excel compliance model. It chains 18 calculation functions in sequence,
#' with each step building on previous results.
#'
#' The calculations follow Excel model columns N through AG:
#' 1. Calculate developable area after exclusions
#' 2. Apply manual overrides if provided
#' 3. Calculate exclusion ratios and open space requirements
#' 4. Determine parking and building footprint
#' 5. Calculate building floor area (envelope)
#' 6. Apply 7 different unit capacity constraint methods
#' 7. Take minimum of all methods as final capacity
#' 8. Calculate units per acre for density verification
#'
#' The function is vectorized and can process thousands of parcels efficiently
#' (target: <30 seconds for 20,000 parcels).
#'
#' @examples
#' \dontrun{
#' # Load parcel data
#' parcels <- load_municipality("cambridge.zip")
#'
#' # Get zoning parameters
#' params <- extract_zoning_parameters("cambridge_model.xlsx", district = 1)
#'
#' # Calculate capacity for all parcels
#' parcels_with_capacity <- calculate_district_capacity(parcels, params)
#'
#' # With transit station areas
#' stations <- load_transit_stations()
#' parcels_with_capacity <- calculate_district_capacity(
#'   parcels,
#'   params,
#'   station_areas = stations
#' )
#'
#' # View final capacities
#' summary(parcels_with_capacity$final_unit_capacity)
#' }
#'
#' @export
calculate_district_capacity <- function(parcels,
                                       zoning_params,
                                       station_areas = NULL,
                                       override_developable_sf = NULL) {

  # Input validation
  if (!inherits(parcels, "sf")) {
    cli::cli_abort("parcels must be an sf object")
  }

  required_cols <- c("LOC_ID", "SQFT", "ACRES", "Tot_Exclud")
  missing_cols <- setdiff(required_cols, names(parcels))
  if (length(missing_cols) > 0) {
    cli::cli_abort("parcels missing required columns: {.field {missing_cols}}")
  }

  required_params <- c(
    "min_lot_size", "base_min_lot_size", "additional_lot_SF",
    "building_height", "FAR", "max_lot_coverage",
    "min_required_open_space", "parking_spaces_per_dwelling_unit",
    "lot_area_per_dwelling_unit", "max_dwelling_units_per_acre",
    "max_units_per_lot", "water_included"
  )

  missing_params <- setdiff(required_params, names(zoning_params))
  if (length(missing_params) > 0) {
    cli::cli_abort("zoning_params missing required elements: {.field {missing_params}}")
  }

  # Set up override vector if not provided
  if (is.null(override_developable_sf)) {
    override_developable_sf <- rep(NA_real_, nrow(parcels))
  }

  # Validate override vector length
  if (length(override_developable_sf) != nrow(parcels)) {
    cli::cli_abort(
      "override_developable_sf length ({length(override_developable_sf)}) must match parcels ({nrow(parcels)})"
    )
  }

  # Add station area flag if provided
  if (!is.null(station_areas)) {
    parcel_centroids <- sf::st_centroid(parcels)
    in_station_area <- sf::st_intersects(parcel_centroids, station_areas, sparse = FALSE)
    parcels$in_station_area <- as.logical(rowSums(in_station_area) > 0)
  } else {
    parcels$in_station_area <- FALSE
  }

  # Extract parcel data for calculations
  lot_area <- parcels$SQFT
  excluded_area <- parcels$Tot_Exclud

  # ===== Step 1: Developable Area (Column N) =====
  parcels$developable_area <- calculate_developable_area(
    lot_area = lot_area,
    excluded_area = excluded_area,
    min_lot_size = zoning_params$min_lot_size
  )

  # ===== Step 2: Net Developable Area with Override (Column Q) =====
  parcels$net_developable_area <- calculate_net_developable_area(
    developable_area = parcels$developable_area,
    override_developable_sf = override_developable_sf
  )

  # ===== Step 3: Exclusion Ratio (Column R) =====
  parcels$exclusion_ratio <- calculate_exclusion_ratio(
    excluded_area = excluded_area,
    lot_area = lot_area
  )

  # ===== Step 4: Open Space Requirement (Column S) - District-level scalar =====
  open_space_req_scalar <- calculate_open_space_requirement(
    min_required_open_space = zoning_params$min_required_open_space
  )

  # ===== Step 5: Required Open Space Area (Column T) =====
  parcels$required_open_space_area <- calculate_required_open_space_area(
    lot_area = lot_area,
    exclusion_ratio = parcels$exclusion_ratio,
    open_space_requirement = open_space_req_scalar,
    net_developable_area = parcels$net_developable_area,
    override_developable_sf = override_developable_sf,
    water_included = zoning_params$water_included
  )

  # ===== Step 6: Parking Area (Column U) =====
  parcels$parking_area <- calculate_parking_area(
    lot_area = lot_area,
    required_open_space = parcels$required_open_space_area,
    net_developable_area = parcels$net_developable_area,
    parking_spaces_per_unit = zoning_params$parking_spaces_per_dwelling_unit
  )

  # ===== Step 7: Building Footprint (Column V) =====
  parcels$building_footprint <- calculate_building_footprint(
    lot_area = lot_area,
    required_open_space = parcels$required_open_space_area,
    parking_area = parcels$parking_area,
    net_developable_area = parcels$net_developable_area
  )

  # ===== Step 8: Building Floor Area (Column W) =====
  parcels$building_floor_area <- calculate_building_floor_area(
    building_footprint = parcels$building_footprint,
    building_height = zoning_params$building_height
  )

  # ===== Step 9: Units from Building Capacity (Column X) =====
  parcels$units_building_capacity <- calculate_units_from_building_capacity(
    building_floor_area = parcels$building_floor_area
  )

  # ===== Step 10: Units from Density Limits (Column Y) =====
  parcels$units_density_limits <- calculate_units_from_density_limits(
    lot_area = lot_area,
    max_dwelling_units_per_acre = zoning_params$max_dwelling_units_per_acre
  )

  # ===== Step 11: Units from Lot Coverage (Column Z) =====
  parcels$units_lot_coverage <- calculate_units_from_lot_coverage(
    lot_area = lot_area,
    max_lot_coverage = zoning_params$max_lot_coverage,
    building_height = zoning_params$building_height
  )

  # ===== Step 12: Units from Lot Area Requirement (Column AA) =====
  parcels$units_lot_area_req <- calculate_units_from_lot_area_requirement(
    lot_area = lot_area,
    lot_area_per_dwelling_unit = zoning_params$lot_area_per_dwelling_unit
  )

  # ===== Step 13: Units from FAR Limits (Column AB) =====
  parcels$units_far_limits <- calculate_units_from_far_limits(
    lot_area = lot_area,
    FAR = zoning_params$FAR
  )

  # ===== Step 14: Adjusted Units with Max Cap (Column AC) =====
  parcels$units_max_cap <- calculate_units_with_max_cap(
    units_building_capacity = parcels$units_building_capacity,
    max_units_per_lot = zoning_params$max_units_per_lot
  )

  # ===== Step 15: Below Minimum Lot Flag (Column AD) =====
  parcels$below_minimum_lot <- calculate_below_minimum_lot_flag(
    lot_area = lot_area,
    min_lot_size = zoning_params$min_lot_size
  )

  # ===== Step 16: Units from Graduated Lots (Column AE) =====
  parcels$units_graduated_lots <- calculate_units_from_graduated_lots(
    lot_area = lot_area,
    below_minimum_lot = parcels$below_minimum_lot,
    base_min_lot_size = zoning_params$base_min_lot_size,
    additional_lot_SF = zoning_params$additional_lot_SF
  )

  # ===== Step 17: Final Unit Capacity (Column AF) =====
  parcels$final_unit_capacity <- calculate_final_unit_capacity(
    units_building_capacity = parcels$units_building_capacity,
    units_density_limits = parcels$units_density_limits,
    units_lot_coverage = parcels$units_lot_coverage,
    units_lot_area_req = parcels$units_lot_area_req,
    units_far_limits = parcels$units_far_limits,
    units_max_cap = parcels$units_max_cap,
    units_graduated_lots = parcels$units_graduated_lots
  )

  # ===== Step 18: Units Per Acre (Column AG) =====
  parcels$units_per_acre <- calculate_units_per_acre(
    lot_area = lot_area,
    final_unit_capacity = parcels$final_unit_capacity
  )

  return(parcels)
}

#' Get Community Requirements
#'
#' Load community-specific compliance requirements for MBTA Communities Act
#' evaluation. Requirements vary by community type and individual municipality.
#'
#' @param community_name Optional character string with community name (e.g.,
#'   "Cambridge", "Chelsea"). If provided with community_type, attempts to load
#'   requirements from internal package data.
#' @param community_type Required character string specifying community category:
#'   \itemize{
#'     \item "rapid_transit" - Communities with rapid transit access
#'     \item "commuter_rail" - Communities with commuter rail access
#'     \item "adjacent" - Communities adjacent to MBTA service
#'     \item "adjacent_small_town" - Adjacent small towns
#'   }
#' @param custom_requirements Optional named list to override default requirements.
#'   Can include: min_units, min_acres, min_station_area_acres,
#'   station_area_unit_pct, station_area_land_pct, min_gross_density
#'
#' @return Named list with compliance requirements:
#'   \itemize{
#'     \item \code{min_units}: Minimum unit capacity required
#'     \item \code{min_acres}: Minimum land area required (acres)
#'     \item \code{min_station_area_acres}: Minimum developable acres in station areas
#'       (rapid_transit and commuter_rail only)
#'     \item \code{station_area_unit_pct}: Minimum percentage of units in station areas
#'       (rapid_transit and commuter_rail only)
#'     \item \code{station_area_land_pct}: Minimum percentage of land in station areas
#'       (rapid_transit and commuter_rail only)
#'     \item \code{min_gross_density}: Minimum units per acre (15 for all communities)
#'     \item \code{community_type}: Community type used
#'     \item \code{community_name}: Community name (if provided)
#'   }
#'
#' @details
#' Requirements are community-specific and loaded from internal package data when
#' both community_name and community_type are provided. If community data is not
#' available, generic requirements for the community type are returned.
#'
#' All communities must meet the minimum gross density requirement of 15 units per acre.
#'
#' Rapid transit and commuter rail communities have additional location requirements
#' for transit station areas.
#'
#' @examples
#' \dontrun{
#' # Get requirements with custom values
#' reqs <- get_community_requirements(
#'   community_type = "rapid_transit",
#'   custom_requirements = list(
#'     min_units = 6240,
#'     min_acres = 50
#'   )
#' )
#'
#' # Get requirements for known community
#' reqs <- get_community_requirements(
#'   community_name = "Cambridge",
#'   community_type = "rapid_transit"
#' )
#' }
#'
#' @export
get_community_requirements <- function(community_name = NULL,
                                      community_type = NULL,
                                      custom_requirements = NULL) {

  # Validate community type
  valid_types <- c("rapid_transit", "commuter_rail", "adjacent", "adjacent_small_town")

  if (is.null(community_type)) {
    cli::cli_abort("community_type must be specified")
  }

  if (!community_type %in% valid_types) {
    cli::cli_abort(
      "community_type must be one of: {.val {valid_types}}, not {.val {community_type}}"
    )
  }

  # Default requirements by community type (will be replaced by community_info.csv)
  # These are placeholders - actual values are community-specific
  default_reqs <- list(
    rapid_transit = list(
      min_units = NA_real_,  # Community-specific
      min_acres = 50,
      min_station_area_acres = 100,
      station_area_unit_pct = NA_real_,  # Community-specific
      station_area_land_pct = NA_real_,  # Community-specific
      min_gross_density = 15
    ),
    commuter_rail = list(
      min_units = NA_real_,
      min_acres = 50,
      min_station_area_acres = 100,
      station_area_unit_pct = NA_real_,
      station_area_land_pct = NA_real_,
      min_gross_density = 15
    ),
    adjacent = list(
      min_units = NA_real_,
      min_acres = 25,
      min_station_area_acres = NA_real_,  # Not applicable
      station_area_unit_pct = NA_real_,   # Not applicable
      station_area_land_pct = NA_real_,   # Not applicable
      min_gross_density = 15
    ),
    adjacent_small_town = list(
      min_units = NA_real_,
      min_acres = 25,
      min_station_area_acres = NA_real_,
      station_area_unit_pct = NA_real_,
      station_area_land_pct = NA_real_,
      min_gross_density = 15
    )
  )

  requirements <- default_reqs[[community_type]]

  # TODO: Load from community_info.csv if community_name provided
  # This will be implemented when package data is added
  if (!is.null(community_name)) {
    cli::cli_inform(
      "Community-specific data not yet implemented. Using default requirements for {community_type}. Provide custom_requirements to override."
    )
  }

  # Apply custom requirements
  if (!is.null(custom_requirements)) {
    for (param in names(custom_requirements)) {
      requirements[[param]] <- custom_requirements[[param]]
    }
  }

  # Add metadata
  requirements$community_type <- community_type
  requirements$community_name <- community_name

  return(requirements)
}

#' Check Compliance Requirements
#'
#' Evaluate district metrics against the 6 MBTA Communities Act compliance
#' requirements. Returns pass/fail status for each requirement with detailed
#' explanations for failures.
#'
#' @param district_metrics Named list with calculated district metrics:
#'   \itemize{
#'     \item total_units: Total unit capacity
#'     \item total_acres: Total land area in acres
#'     \item developable_station_acres: Developable acres in station areas
#'     \item gross_density: Units per acre (using density denominator)
#'     \item station_area_units: Unit capacity in station areas
#'     \item station_area_acres: Land area in station areas
#'   }
#' @param requirements Named list from \code{\link{get_community_requirements}}
#' @param community_type Character string specifying community category
#'
#' @return Named list with compliance results:
#'   \itemize{
#'     \item \code{compliant}: Overall TRUE/FALSE
#'     \item \code{requirements_met}: Named logical vector for each requirement
#'     \item \code{failure_reasons}: Character vector of reasons for non-compliance
#'     \item \code{summary}: Data frame with requirement details
#'   }
#'
#' @details
#' The function evaluates these 6 requirements:
#'
#' \strong{1. Minimum Unit Capacity} (all communities)
#' District must have at least the community-specific minimum unit capacity.
#'
#' \strong{2. Minimum Land Area} (all communities)
#' District must have at least the community-specific minimum land area.
#'
#' \strong{3. Developable Station Area} (rapid_transit and commuter_rail only)
#' Must have at least 100 acres of developable land within station areas.
#'
#' \strong{4. Gross Density} (all communities)
#' Must achieve at least 15 units per acre using gross density denominator.
#'
#' \strong{5. Unit Capacity Ratio in Station Areas} (rapid_transit and commuter_rail only)
#' Specified percentage of unit capacity must be within station areas.
#'
#' \strong{6. Land Area Ratio in Station Areas} (rapid_transit and commuter_rail only)
#' Specified percentage of land area must be within station areas.
#'
#' @examples
#' \dontrun{
#' # Calculate district metrics
#' metrics <- list(
#'   total_units = 6500,
#'   total_acres = 145,
#'   gross_density = 16.2,
#'   developable_station_acres = 110,
#'   station_area_units = 4200,
#'   station_area_acres = 95
#' )
#'
#' # Get requirements
#' reqs <- get_community_requirements(
#'   community_type = "rapid_transit",
#'   custom_requirements = list(min_units = 6240, min_acres = 50)
#' )
#'
#' # Check compliance
#' result <- check_compliance_requirements(metrics, reqs, "rapid_transit")
#' }
#'
#' @export
check_compliance_requirements <- function(district_metrics,
                                         requirements,
                                         community_type) {

  # Initialize results
  requirements_met <- list()
  failure_reasons <- character()

  # Requirement 1: Minimum Unit Capacity
  if (!is.na(requirements$min_units)) {
    requirements_met$min_unit_capacity <-
      district_metrics$total_units >= requirements$min_units

    if (!requirements_met$min_unit_capacity) {
      failure_reasons <- c(
        failure_reasons,
        sprintf(
          "Unit capacity (%d) below minimum (%d)",
          round(district_metrics$total_units),
          round(requirements$min_units)
        )
      )
    }
  } else {
    requirements_met$min_unit_capacity <- NA
  }

  # Requirement 2: Minimum Land Area
  if (!is.na(requirements$min_acres)) {
    requirements_met$min_land_area <-
      district_metrics$total_acres >= requirements$min_acres

    if (!requirements_met$min_land_area) {
      failure_reasons <- c(
        failure_reasons,
        sprintf(
          "Land area (%.1f acres) below minimum (%.1f acres)",
          district_metrics$total_acres,
          requirements$min_acres
        )
      )
    }
  } else {
    requirements_met$min_land_area <- NA
  }

  # Requirement 3: Developable Station Area (rapid transit / commuter rail only)
  if (community_type %in% c("rapid_transit", "commuter_rail")) {
    if (!is.na(requirements$min_station_area_acres)) {
      requirements_met$min_station_area <-
        district_metrics$developable_station_acres >= requirements$min_station_area_acres

      if (!requirements_met$min_station_area) {
        failure_reasons <- c(
          failure_reasons,
          sprintf(
            "Developable station area (%.1f acres) below minimum (%.1f acres)",
            district_metrics$developable_station_acres,
            requirements$min_station_area_acres
          )
        )
      }
    } else {
      requirements_met$min_station_area <- NA
    }
  } else {
    requirements_met$min_station_area <- NA  # Not applicable
  }

  # Requirement 4: Gross Density
  requirements_met$min_gross_density <-
    district_metrics$gross_density >= requirements$min_gross_density

  if (!requirements_met$min_gross_density) {
    failure_reasons <- c(
      failure_reasons,
      sprintf(
        "Gross density (%.2f units/acre) below minimum (%.2f units/acre)",
        district_metrics$gross_density,
        requirements$min_gross_density
      )
    )
  }

  # Requirement 5: Unit Capacity Ratio in Station Areas (rapid transit / commuter rail only)
  if (community_type %in% c("rapid_transit", "commuter_rail")) {
    if (!is.na(requirements$station_area_unit_pct)) {
      actual_unit_pct <- (district_metrics$station_area_units / district_metrics$total_units) * 100
      requirements_met$station_area_unit_ratio <-
        actual_unit_pct >= requirements$station_area_unit_pct

      if (!requirements_met$station_area_unit_ratio) {
        failure_reasons <- c(
          failure_reasons,
          sprintf(
            "Station area unit ratio (%.1f%%) below minimum (%.1f%%)",
            actual_unit_pct,
            requirements$station_area_unit_pct
          )
        )
      }
    } else {
      requirements_met$station_area_unit_ratio <- NA
    }
  } else {
    requirements_met$station_area_unit_ratio <- NA  # Not applicable
  }

  # Requirement 6: Land Area Ratio in Station Areas (rapid transit / commuter rail only)
  if (community_type %in% c("rapid_transit", "commuter_rail")) {
    if (!is.na(requirements$station_area_land_pct)) {
      actual_land_pct <- (district_metrics$station_area_acres / district_metrics$total_acres) * 100
      requirements_met$station_area_land_ratio <-
        actual_land_pct >= requirements$station_area_land_pct

      if (!requirements_met$station_area_land_ratio) {
        failure_reasons <- c(
          failure_reasons,
          sprintf(
            "Station area land ratio (%.1f%%) below minimum (%.1f%%)",
            actual_land_pct,
            requirements$station_area_land_pct
          )
        )
      }
    } else {
      requirements_met$station_area_land_ratio <- NA
    }
  } else {
    requirements_met$station_area_land_ratio <- NA  # Not applicable
  }

  # Overall compliance (all applicable requirements must be met)
  applicable_reqs <- unlist(requirements_met[!is.na(requirements_met)])
  compliant <- all(applicable_reqs)

  # Create summary data frame
  summary_df <- data.frame(
    requirement = names(requirements_met),
    status = sapply(requirements_met, function(x) {
      if (is.na(x)) "Not Applicable"
      else if (x) "PASS"
      else "FAIL"
    }),
    stringsAsFactors = FALSE
  )

  return(list(
    compliant = compliant,
    requirements_met = requirements_met,
    failure_reasons = if (length(failure_reasons) == 0) NULL else failure_reasons,
    summary = summary_df
  ))
}
#' Evaluate MBTA Communities Act Compliance
#'
#' Main API function to perform complete compliance evaluation for a municipality.
#' Orchestrates all package components: data loading, district assignment, unit
#' capacity calculations, and compliance assessment.
#'
#' @param municipality An sf object containing parcel data (from
#'   \code{\link{load_municipality}})
#' @param districts District specification (see \code{\link{assign_parcels_to_districts}}
#'   for supported formats): sf polygon(s), column name, or named list
#' @param zoning_params Named list (or list of lists for multiple districts) of
#'   zoning parameters from \code{\link{extract_zoning_parameters}}
#' @param community_type Required character string: "rapid_transit", "commuter_rail",
#'   "adjacent", or "adjacent_small_town"
#' @param transit_stations Optional sf object with transit station buffers (from
#'   \code{\link{load_transit_stations}}). Required for accurate station area
#'   calculations for rapid_transit and commuter_rail communities.
#' @param density_deductions Optional sf object with density deduction polygons
#'   (from \code{\link{load_density_deductions}}). Required for accurate gross
#'   density calculations.
#' @param community_name Optional community name for loading specific requirements
#' @param custom_requirements Optional list to override community requirements
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return Named list with complete compliance evaluation results:
#'   \describe{
#'     \item{summary}{List with overall metrics and compliance status}
#'     \item{by_district}{Data frame with metrics for each district}
#'     \item{parcel_detail}{sf object with all parcels and calculation columns}
#'     \item{compliance}{Detailed compliance check results}
#'     \item{metadata}{Evaluation metadata (date, package version, etc.)}
#'   }
#'
#' @details
#' This function performs the complete compliance evaluation workflow:
#'
#' \strong{Step 1: District Assignment}
#' Assigns parcels to districts using the flexible input method provided.
#'
#' \strong{Step 2: Unit Capacity Calculation}
#' Applies all 18 unit capacity calculations to each parcel using
#' \code{\link{calculate_district_capacity}}.
#'
#' \strong{Step 3: District Metrics}
#' Calculates district-level summary metrics:
#' - Total unit capacity
#' - Total land area
#' - Developable land area
#' - Gross density (with density denominator deductions)
#' - Station area metrics (if applicable)
#'
#' \strong{Step 4: Compliance Evaluation}
#' Evaluates all 6 compliance requirements using
#' \code{\link{check_compliance_requirements}}.
#'
#' @examples
#' \dontrun{
#' # Load data
#' parcels <- load_municipality("cambridge.zip")
#' stations <- load_transit_stations()
#' deductions <- load_density_deductions()
#'
#' # Get zoning parameters
#' params <- extract_zoning_parameters("cambridge_model.xlsx", district = 1)
#'
#' # Define district boundary
#' district_boundary <- sf::st_read("district.shp")
#'
#' # Evaluate compliance
#' result <- evaluate_compliance(
#'   municipality = parcels,
#'   districts = district_boundary,
#'   zoning_params = params,
#'   community_type = "rapid_transit",
#'   transit_stations = stations,
#'   density_deductions = deductions,
#'   custom_requirements = list(min_units = 6240, min_acres = 50)
#' )
#'
#' # View results
#' result$summary$compliant
#' result$compliance$summary
#' result$by_district
#' }
#'
#' @export
evaluate_compliance <- function(municipality,
                               districts,
                               zoning_params,
                               community_type,
                               transit_stations = NULL,
                               density_deductions = NULL,
                               community_name = NULL,
                               custom_requirements = NULL,
                               verbose = TRUE) {

  if (verbose) cli::cli_alert_info("Starting compliance evaluation...")

  # Step 1: Assign parcels to districts
  if (verbose) cli::cli_alert("Assigning parcels to districts...")
  district_assignments <- assign_parcels_to_districts(municipality, districts)

  # Get unique districts
  unique_districts <- unique(district_assignments$district_id[!is.na(district_assignments$district_id)])

  if (length(unique_districts) == 0) {
    cli::cli_abort("No parcels assigned to any district")
  }

  # Step 2: Calculate unit capacity for all parcels
  if (verbose) cli::cli_alert("Calculating unit capacity for {nrow(municipality)} parcel{?s}...")

  # Handle zoning params: single list or list of lists
  if (!is.list(zoning_params)) {
    cli::cli_abort("zoning_params must be a list")
  }

  # Detect if this is single district or multi-district mode
  # Single district: zoning_params has "min_lot_size" directly in names
  # Multi-district: zoning_params is a list of lists (nested structure)
  is_single_district <- "min_lot_size" %in% names(zoning_params)

  if (is_single_district) {
    # Single district mode - apply same params to all parcels
    parcels_with_capacity <- calculate_district_capacity(
      parcels = municipality,
      zoning_params = zoning_params,
      station_areas = transit_stations
    )
  } else {
    # Multi-district mode - check structure and apply district-specific params

    # Check if all elements are lists (indicating multi-district)
    # or if any are NOT lists (indicating malformed single-district params)
    elements_are_lists <- sapply(zoning_params, is.list)

    if (!any(elements_are_lists)) {
      # No lists at all - this is a malformed single-district param list
      # (missing min_lot_size, so it went down the wrong path)
      # Pass through to calculate_district_capacity which will give proper error
      parcels_with_capacity <- calculate_district_capacity(
        parcels = municipality,
        zoning_params = zoning_params,
        station_areas = transit_stations
      )
    } else if (!all(elements_are_lists)) {
      # Mixed structure - definitely malformed
      cli::cli_abort(
        "zoning_params must be either a single parameter list or a named list of parameter lists. Found mixed types."
      )
    } else {
      # All elements are lists - proceed with multi-district mode

      # Validate that zoning_params has names (district IDs)
    if (is.null(names(zoning_params)) || any(names(zoning_params) == "")) {
      cli::cli_abort(
        "Multi-district zoning_params must have named elements matching district IDs"
      )
    }

    # Check that all district names in zoning_params match actual districts
    param_districts <- names(zoning_params)
    missing_params <- setdiff(unique_districts, param_districts)
    extra_params <- setdiff(param_districts, unique_districts)

    if (length(missing_params) > 0) {
      cli::cli_abort(
        "zoning_params missing for district{?s}: {.field {missing_params}}"
      )
    }

    if (length(extra_params) > 0) {
      cli::cli_warn(
        "zoning_params provided for unknown district{?s}: {.field {extra_params}} (will be ignored)"
      )
    }

    # Process each district separately
    if (verbose) cli::cli_alert("Processing {length(unique_districts)} district{?s} with district-specific zoning parameters...")

    parcels_by_district <- lapply(unique_districts, function(dist_id) {
      # Get parcels for this district
      district_loc_ids <- district_assignments$LOC_ID[
        !is.na(district_assignments$district_id) &
          district_assignments$district_id == dist_id
      ]

      district_parcels <- municipality[municipality$LOC_ID %in% district_loc_ids, ]

      # Get zoning params for this district
      dist_params <- zoning_params[[dist_id]]

      # Calculate capacity for this district
      calculate_district_capacity(
        parcels = district_parcels,
        zoning_params = dist_params,
        station_areas = transit_stations
      )
    })

    # Combine results from all districts
    parcels_with_capacity <- do.call(rbind, parcels_by_district)

    # Ensure parcels are in same order as original municipality
    parcels_with_capacity <- parcels_with_capacity[
      match(municipality$LOC_ID, parcels_with_capacity$LOC_ID),
    ]
    }
  }

  # Merge district assignments
  parcels_with_capacity$district_id <- district_assignments$district_id[
    match(parcels_with_capacity$LOC_ID, district_assignments$LOC_ID)
  ]
  parcels_with_capacity$district_name <- district_assignments$district_name[
    match(parcels_with_capacity$LOC_ID, district_assignments$LOC_ID)
  ]

  # Step 3: Calculate district-level metrics
  if (verbose) cli::cli_alert("Calculating district-level metrics...")

  # Warn once if density_deductions not provided (before looping over districts)
  if (is.null(density_deductions) && verbose) {
    cli::cli_warn(
      "density_deductions not provided, using total area for gross density"
    )
  }

  district_metrics_list <- lapply(unique_districts, function(dist_id) {
    district_parcels <- parcels_with_capacity[
      !is.na(parcels_with_capacity$district_id) &
        parcels_with_capacity$district_id == dist_id,
    ]

    # Total metrics
    total_units <- sum(district_parcels$final_unit_capacity, na.rm = TRUE)
    total_acres <- sum(district_parcels$ACRES, na.rm = TRUE)
    developable_acres <- sum(district_parcels$developable_area / 43560, na.rm = TRUE)

    # Station area metrics
    station_area_units <- sum(
      district_parcels$final_unit_capacity[district_parcels$in_station_area],
      na.rm = TRUE
    )
    station_area_acres <- sum(
      district_parcels$ACRES[district_parcels$in_station_area],
      na.rm = TRUE
    )
    developable_station_acres <- sum(
      district_parcels$developable_area[district_parcels$in_station_area] / 43560,
      na.rm = TRUE
    )

    # Gross density denominator calculation
    if (!is.null(density_deductions)) {
      # Calculate area to deduct (intersection with deduction layer)
      district_geom <- sf::st_union(district_parcels)
      deductions_in_district <- sf::st_intersection(
        sf::st_make_valid(district_geom),
        sf::st_make_valid(density_deductions)
      )
      deduction_area_acres <- sum(sf::st_area(deductions_in_district)) / 43560
      gross_density_denominator <- total_acres - deduction_area_acres
    } else {
      # Use total area if deductions not provided
      gross_density_denominator <- total_acres
    }

    # Calculate gross density
    gross_density <- if (gross_density_denominator > 0) {
      total_units / gross_density_denominator
    } else {
      0
    }

    list(
      district_id = dist_id,
      district_name = district_assignments$district_name[
        match(dist_id, district_assignments$district_id)
      ][1],
      total_units = total_units,
      total_acres = total_acres,
      developable_acres = developable_acres,
      gross_density_denominator = gross_density_denominator,
      gross_density = gross_density,
      station_area_units = station_area_units,
      station_area_acres = station_area_acres,
      developable_station_acres = developable_station_acres,
      n_parcels = nrow(district_parcels)
    )
  })

  # Convert to data frame
  by_district <- do.call(rbind, lapply(district_metrics_list, as.data.frame))

  # Step 4: Calculate overall summary metrics
  summary_metrics <- list(
    total_units = sum(by_district$total_units, na.rm = TRUE),
    total_acres = sum(by_district$total_acres, na.rm = TRUE),
    developable_acres = sum(by_district$developable_acres, na.rm = TRUE),
    gross_density = sum(by_district$total_units, na.rm = TRUE) /
      sum(by_district$gross_density_denominator, na.rm = TRUE),
    station_area_units = sum(by_district$station_area_units, na.rm = TRUE),
    station_area_acres = sum(by_district$station_area_acres, na.rm = TRUE),
    developable_station_acres = sum(by_district$developable_station_acres, na.rm = TRUE),
    n_districts = length(unique_districts),
    n_parcels = nrow(municipality)
  )

  # Step 5: Get requirements and check compliance
  if (verbose) cli::cli_alert("Evaluating compliance requirements...")

  requirements <- get_community_requirements(
    community_name = community_name,
    community_type = community_type,
    custom_requirements = custom_requirements
  )

  compliance_result <- check_compliance_requirements(
    district_metrics = summary_metrics,
    requirements = requirements,
    community_type = community_type
  )

  # Add compliance status to summary
  summary_metrics$compliant <- compliance_result$compliant
  summary_metrics$requirements_met <- compliance_result$requirements_met

  # Step 6: Compile results
  result <- list(
    summary = summary_metrics,
    by_district = by_district,
    parcel_detail = parcels_with_capacity,
    compliance = compliance_result,
    metadata = list(
      community_name = community_name,
      community_type = community_type,
      evaluation_date = Sys.Date(),
      package_version = as.character(utils::packageVersion("mbtazone"))
    )
  )

  if (verbose) {
    if (compliance_result$compliant) {
      cli::cli_alert_success("Compliance evaluation complete: COMPLIANT")
    } else {
      cli::cli_alert_danger("Compliance evaluation complete: NOT COMPLIANT")
      if (!is.null(compliance_result$failure_reasons)) {
        cli::cli_ul(compliance_result$failure_reasons)
      }
    }
  }

  return(result)
}
