#' GIS Operations for MBTA Compliance
#'
#' Spatial analysis functions for district compliance calculations including
#' area measurements, contiguity validation, and transit station overlap analysis.
#'
#' @name gis_operations
NULL

#' Calculate District Area
#'
#' Calculate the total land area of a compliance district using NAD83 Massachusetts
#' State Plane projection. Handles both single-polygon and multi-polygon districts.
#'
#' @param district_sf An sf object representing the compliance district geometry.
#'   Must be in EPSG:26986 (NAD83 Massachusetts State Plane).
#' @param output_unit Character string specifying output units: "acres" (default)
#'   or "sqft" (square feet).
#'
#' @return Numeric value representing total district area in specified units.
#'   For multi-polygon geometries, returns the combined area of all features.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Validates input is an sf object with EPSG:26986 CRS
#'   \item Unions all features into single geometry (for multi-polygon districts)
#'   \item Calculates area using planar coordinates in US Survey Feet
#'   \item Converts to requested output units (1 acre = 43,560 sq ft)
#' }
#'
#' The function requires EPSG:26986 (NAD83 Massachusetts State Plane, Mainland zone)
#' for accurate area calculations. This projection uses US Survey Feet as the unit,
#' ensuring consistent area measurements across all Massachusetts municipalities.
#'
#' @section NA Handling:
#' Returns NA if:
#' \itemize{
#'   \item Input geometry is empty or invalid
#'   \item CRS cannot be determined
#' }
#'
#' @examples
#' \dontrun{
#' # Load a district shapefile
#' district <- sf::st_read("district.shp")
#' district <- sf::st_transform(district, 26986)
#'
#' # Calculate area in acres (default)
#' area_acres <- calculate_district_area(district)
#'
#' # Calculate area in square feet
#' area_sqft <- calculate_district_area(district, output_unit = "sqft")
#'
#' # Works with multi-polygon districts
#' multi_district <- sf::st_read("multi_district.shp")
#' multi_district <- sf::st_transform(multi_district, 26986)
#' total_area <- calculate_district_area(multi_district)
#' }
#'
#' @seealso \code{\link{validate_contiguity}}, \code{\link{calculate_density_denominator}}
#'
#' @export
calculate_district_area <- function(district_sf,
                                    output_unit = c("acres", "sqft")) {

  # Validate inputs
  if (!inherits(district_sf, "sf")) {
    cli::cli_abort("{.arg district_sf} must be an sf object")
  }

  output_unit <- match.arg(output_unit)

  # Validate CRS
  district_crs <- sf::st_crs(district_sf)
  if (is.na(district_crs)) {
    cli::cli_abort("{.arg district_sf} has no CRS defined")
  }

  if (district_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg district_sf} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{district_crs$epsg}",
        "i" = "Transform with: {.code sf::st_transform(district_sf, 26986)}"
      )
    )
  }

  # Check for empty geometries
  if (nrow(district_sf) == 0 || all(sf::st_is_empty(district_sf))) {
    cli::cli_warn("District has empty geometry, returning NA")
    return(NA_real_)
  }

  # Union all features into single geometry (handles multi-polygon districts)
  district_union <- sf::st_union(district_sf)

  # Calculate area in square feet (EPSG:26986 uses US Survey Feet)
  area_sqft <- as.numeric(sf::st_area(district_union))

  # Convert to requested units
  if (output_unit == "acres") {
    return(area_sqft / 43560)
  } else {
    return(area_sqft)
  }
}


#' Identify Excluded Land Area
#'
#' Aggregate excluded land area from parcels, with optional breakdown by
#' exclusion category. Excluded land includes water bodies, wetlands, protected
#' open space, and other non-developable areas.
#'
#' @param parcels An sf object containing parcel data with exclusion information.
#'   Must include column `Tot_Exclud` (total excluded area in square feet).
#' @param by_category Logical indicating whether to return breakdown by exclusion
#'   category (default: FALSE). If TRUE and category columns are available,
#'   returns named vector with exclusions by type.
#'
#' @return If `by_category = FALSE` (default), returns numeric value of total
#'   excluded area in square feet. If `by_category = TRUE`, returns named numeric
#'   vector with exclusions broken down by category.
#'
#' @details
#' This function sums the `Tot_Exclud` column across all parcels to determine
#' total excluded land area. The state-provided Basic land map shapefiles include
#' pre-calculated exclusion totals.
#'
#' When `by_category = TRUE`, the function attempts to break down exclusions
#' by type if the following columns are available:
#' \itemize{
#'   \item Water bodies and wetlands
#'   \item Protected open space (Chapter 61, 61A, 61B lands)
#'   \item Public institutional land
#'   \item Other non-public exclusions
#' }
#'
#' @section NA Handling:
#' \itemize{
#'   \item NA values in `Tot_Exclud` are treated as 0 (no exclusion)
#'   \item Returns 0 if all parcels have NA exclusions
#'   \item Warns if >10% of parcels have NA exclusion values
#' }
#'
#' @examples
#' \dontrun{
#' # Load municipality parcels
#' parcels <- load_municipality("cambridge.zip")
#'
#' # Get total excluded area
#' total_excluded <- identify_excluded_land(parcels)
#' cat("Total excluded:", total_excluded / 43560, "acres\n")
#'
#' # Get breakdown by category
#' exclusions_by_type <- identify_excluded_land(parcels, by_category = TRUE)
#' print(exclusions_by_type)
#' }
#'
#' @seealso \code{\link{calculate_density_denominator}}, \code{\link{load_municipality}}
#'
#' @export
identify_excluded_land <- function(parcels, by_category = FALSE) {

  # Validate inputs
  if (!inherits(parcels, "sf") && !inherits(parcels, "data.frame")) {
    cli::cli_abort("{.arg parcels} must be an sf object or data.frame")
  }

  if (!"Tot_Exclud" %in% names(parcels)) {
    cli::cli_abort(
      c(
        "{.arg parcels} must contain column {.field Tot_Exclud}",
        "i" = "Available columns: {.val {names(parcels)}}"
      )
    )
  }

  # Extract Tot_Exclud column
  excluded <- parcels$Tot_Exclud

  # Check for NA values
  na_count <- sum(is.na(excluded))
  if (na_count > 0) {
    na_pct <- na_count / length(excluded)
    if (na_pct > 0.1) {
      cli::cli_warn(
        "{.val {na_count}} parcels ({.val {round(na_pct * 100, 1)}%}) have NA exclusion values. Treating as 0."
      )
    }
  }

  # Replace NA with 0 (no exclusion)
  excluded[is.na(excluded)] <- 0

  if (!by_category) {
    # Return total excluded area
    return(sum(excluded))
  }

  # Attempt to break down by category
  # Category columns from Basic land map shapefiles
  category_cols <- c(
    "PublicInst" = "PublicInst",     # Public institutional land
    "NonPubExc" = "NonPubExc"        # Other non-public exclusions
  )

  # Find available category columns
  available_cats <- category_cols[category_cols %in% names(parcels)]

  if (length(available_cats) == 0) {
    cli::cli_warn(
      c(
        "No exclusion category columns found in parcels",
        "i" = "Returning total only. Expected columns: {.field {category_cols}}"
      )
    )
    return(c("Total" = sum(excluded)))
  }

  # Build category breakdown
  result <- setNames(
    numeric(length(available_cats) + 1),
    c(names(available_cats), "Total")
  )

  for (i in seq_along(available_cats)) {
    cat_col <- available_cats[i]
    cat_data <- parcels[[cat_col]]
    cat_data[is.na(cat_data)] <- 0
    result[i] <- sum(cat_data)
  }

  result["Total"] <- sum(excluded)

  return(result)
}


#' Calculate Station Area Intersection
#'
#' Calculate the area of each parcel that intersects with MBTA transit station
#' areas (0.5-mile buffers). Adds a new column `station_area_sf` to the parcels
#' containing intersection area in square feet.
#'
#' @param parcels An sf object containing parcel geometries. Must be in
#'   EPSG:26986 projection.
#' @param station_areas An sf object containing transit station buffer geometries
#'   (typically 0.5-mile radius buffers). Must be in EPSG:26986 projection.
#' @param output_unit Character string specifying output units for intersection
#'   area: "sqft" (default) or "acres".
#'
#' @return The input `parcels` sf object with an additional column:
#'   \itemize{
#'     \item `station_area_sf`: Numeric, area of parcel within station buffers
#'       (in specified units). Parcels with no station overlap get 0, not NA.
#'   }
#'
#' @details
#' This function performs spatial intersection between parcels and transit station
#' areas to determine compliance with location requirements. For Rapid Transit
#' and Commuter Rail communities, a minimum percentage of the district must be
#' within 0.5-mile station buffers.
#'
#' The function:
#' \enumerate{
#'   \item Validates both inputs are in EPSG:26986
#'   \item Uses `sf::st_intersection()` for spatial overlay
#'   \item Calculates intersection area for each parcel
#'   \item Handles multiple overlapping station areas (sums total overlap)
#'   \item Returns 0 for parcels outside all station areas (not NA)
#' }
#'
#' @section Performance:
#' For large parcel datasets (>10,000 parcels), this function uses spatial
#' indexing to optimize intersection calculations. Expected performance:
#' <30 seconds for 20,000 parcels.
#'
#' @section NA Handling:
#' \itemize{
#'   \item Parcels outside station areas get `station_area_sf = 0` (not NA)
#'   \item Invalid geometries are repaired with `sf::st_make_valid()`
#'   \item Empty geometries get `station_area_sf = 0`
#' }
#'
#' @examples
#' \dontrun{
#' # Load parcels and station areas
#' parcels <- load_municipality("chelsea.zip")
#' station_areas <- load_transit_stations()
#'
#' # Calculate station intersection
#' parcels <- calculate_station_intersection(
#'   parcels,
#'   station_areas,
#'   output_unit = "sqft"
#' )
#'
#' # Check how many parcels are in station areas
#' in_station <- sum(parcels$station_area_sf > 0)
#' cat("Parcels in station areas:", in_station, "\n")
#'
#' # Calculate total station area
#' total_station_area <- sum(parcels$station_area_sf) / 43560
#' cat("Total area in stations:", total_station_area, "acres\n")
#' }
#'
#' @seealso \code{\link{calculate_station_area_percentage}}, \code{\link{load_transit_stations}}
#'
#' @export
calculate_station_intersection <- function(parcels,
                                           station_areas,
                                           output_unit = c("sqft", "acres")) {

  # Validate inputs
  if (!inherits(parcels, "sf")) {
    cli::cli_abort("{.arg parcels} must be an sf object")
  }

  if (!inherits(station_areas, "sf")) {
    cli::cli_abort("{.arg station_areas} must be an sf object")
  }

  output_unit <- match.arg(output_unit)

  # Validate CRS for both inputs
  parcels_crs <- sf::st_crs(parcels)
  station_crs <- sf::st_crs(station_areas)

  if (is.na(parcels_crs) || is.na(station_crs)) {
    cli::cli_abort("Both {.arg parcels} and {.arg station_areas} must have defined CRS")
  }

  if (parcels_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg parcels} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{parcels_crs$epsg}"
      )
    )
  }

  if (station_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg station_areas} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{station_crs$epsg}"
      )
    )
  }

  # Check for empty geometries FIRST (before initializing column)
  if (nrow(parcels) == 0 || all(sf::st_is_empty(parcels))) {
    cli::cli_warn("All parcels have empty geometry, returning with station_area_sf = 0")
    parcels$station_area_sf <- numeric(0)  # Empty numeric vector for empty data
    return(parcels)
  }

  # Initialize station_area_sf column with 0 (not NA)
  parcels$station_area_sf <- 0

  if (nrow(station_areas) == 0 || all(sf::st_is_empty(station_areas))) {
    cli::cli_warn("Station areas are empty, returning parcels with station_area_sf = 0")
    return(parcels)
  }

  # Repair invalid geometries
  parcels <- sf::st_make_valid(parcels)
  station_areas <- sf::st_make_valid(station_areas)

  # Union all station areas into single geometry (handles overlapping buffers)
  station_union <- sf::st_union(station_areas)

  # Perform intersection
  # Use st_intersection to find overlapping areas
  suppressWarnings({
    intersection <- sf::st_intersection(parcels, station_union)
  })

  # If no intersections, return parcels with all 0s
  if (nrow(intersection) == 0) {
    return(parcels)
  }

  # Calculate intersection areas
  intersection_areas <- as.numeric(sf::st_area(intersection))

  # Match intersection results back to original parcels
  # st_intersection preserves row indices in the result
  parcel_indices <- as.integer(rownames(intersection))

  # Sum areas by parcel (in case a parcel was split into multiple intersection pieces)
  for (i in seq_along(parcel_indices)) {
    idx <- parcel_indices[i]
    parcels$station_area_sf[idx] <- parcels$station_area_sf[idx] + intersection_areas[i]
  }

  # Convert to requested units
  if (output_unit == "acres") {
    parcels$station_area_sf <- parcels$station_area_sf / 43560
  }

  return(parcels)
}


#' Calculate Station Area Percentage
#'
#' Calculate the percentage of a compliance district that falls within MBTA
#' transit station areas (0.5-mile buffers). Used to verify location requirements
#' for Rapid Transit and Commuter Rail communities.
#'
#' @param district_sf An sf object representing the compliance district geometry.
#'   Must be in EPSG:26986 projection.
#' @param station_areas An sf object containing transit station buffer geometries.
#'   Must be in EPSG:26986 projection.
#'
#' @return Numeric value between 0 and 100 representing the percentage of district
#'   area within station buffers. Returns 0 if district has no station overlap.
#'
#' @details
#' This function calculates what percentage of the total district area falls
#' within 0.5-mile buffers of MBTA transit stations. The MBTA Communities Act
#' requires:
#' \itemize{
#'   \item \strong{Rapid Transit:} ≥50% within station areas
#'   \item \strong{Commuter Rail:} No minimum (but encouraged)
#' }
#'
#' The calculation:
#' \enumerate{
#'   \item Unions all district features into single geometry
#'   \item Unions all station buffers into single geometry
#'   \item Intersects district with station areas
#'   \item Calculates percentage: (intersection area / total district area) * 100
#' }
#'
#' @section NA Handling:
#' Returns NA if:
#' \itemize{
#'   \item District geometry is empty or invalid
#'   \item District area is 0
#' }
#' Returns 0 (not NA) if district is outside all station areas.
#'
#' @examples
#' \dontrun{
#' # Load district and station areas
#' district <- sf::st_read("district.shp")
#' district <- sf::st_transform(district, 26986)
#' station_areas <- load_transit_stations()
#'
#' # Calculate percentage in station areas
#' pct <- calculate_station_area_percentage(district, station_areas)
#' cat("District within station areas:", pct, "%\n")
#'
#' # Check compliance (Rapid Transit requires ≥50%)
#' if (pct >= 50) {
#'   cat("Meets Rapid Transit location requirement\n")
#' }
#' }
#'
#' @seealso \code{\link{calculate_station_intersection}}, \code{\link{calculate_district_area}}
#'
#' @export
calculate_station_area_percentage <- function(district_sf, station_areas) {

  # Validate inputs
  if (!inherits(district_sf, "sf")) {
    cli::cli_abort("{.arg district_sf} must be an sf object")
  }

  if (!inherits(station_areas, "sf")) {
    cli::cli_abort("{.arg station_areas} must be an sf object")
  }

  # Validate CRS
  district_crs <- sf::st_crs(district_sf)
  station_crs <- sf::st_crs(station_areas)

  if (is.na(district_crs) || is.na(station_crs)) {
    cli::cli_abort("Both {.arg district_sf} and {.arg station_areas} must have defined CRS")
  }

  if (district_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg district_sf} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{district_crs$epsg}"
      )
    )
  }

  if (station_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg station_areas} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{station_crs$epsg}"
      )
    )
  }

  # Check for empty geometries
  if (nrow(district_sf) == 0 || all(sf::st_is_empty(district_sf))) {
    cli::cli_warn("District has empty geometry, returning NA")
    return(NA_real_)
  }

  if (nrow(station_areas) == 0 || all(sf::st_is_empty(station_areas))) {
    cli::cli_warn("Station areas are empty, returning 0%")
    return(0)
  }

  # Calculate total district area
  total_area <- calculate_district_area(district_sf, output_unit = "sqft")

  if (is.na(total_area) || total_area == 0) {
    cli::cli_warn("District area is 0 or NA, returning NA")
    return(NA_real_)
  }

  # Repair invalid geometries
  district_sf <- sf::st_make_valid(district_sf)
  station_areas <- sf::st_make_valid(station_areas)

  # Union all features
  district_union <- sf::st_union(district_sf)
  station_union <- sf::st_union(station_areas)

  # Calculate intersection
  suppressWarnings({
    intersection <- sf::st_intersection(district_union, station_union)
  })

  # Check if intersection is empty
  if (length(intersection) == 0 || all(sf::st_is_empty(intersection))) {
    return(0)
  }

  # Calculate intersection area
  intersection_area <- as.numeric(sf::st_area(intersection))

  # Calculate percentage
  percentage <- (intersection_area / total_area) * 100

  # Ensure percentage is between 0 and 100 (handle rounding errors)
  percentage <- max(0, min(100, percentage))

  return(percentage)
}


#' Calculate Density Denominator
#'
#' Calculate the gross density denominator for a compliance district by subtracting
#' density deductions (highways, railroads, water bodies, etc.) from total district
#' area. Used to calculate minimum gross density (units per acre).
#'
#' @param district_sf An sf object representing the compliance district geometry.
#'   Must be in EPSG:26986 projection.
#' @param deduction_layers An sf object containing density denominator deduction
#'   geometries (highways, railroads, water, etc.). Must be in EPSG:26986 projection.
#' @param output_unit Character string specifying output units: "acres" (default)
#'   or "sqft" (square feet).
#'
#' @return Numeric value representing gross density denominator area (total district
#'   area minus deductions) in specified units. Returns total district area if
#'   no deductions apply.
#'
#' @details
#' The gross density denominator is calculated as:
#' \deqn{Density Denominator = Total District Area - Deductions}
#'
#' Deductions typically include:
#' \itemize{
#'   \item Limited access highways
#'   \item Railroad rights-of-way
#'   \item Water bodies
#'   \item Certain protected lands
#' }
#'
#' The state provides a statewide Density Denominator Deductions layer
#' (available via `load_density_deductions()`) that includes all applicable
#' deduction types.
#'
#' The density denominator is used to calculate minimum gross density:
#' \deqn{Gross Density = Required Units / Density Denominator (acres)}
#'
#' @section NA Handling:
#' \itemize{
#'   \item Returns NA if district geometry is empty or invalid
#'   \item Returns total district area if deduction layers are empty (no deductions)
#'   \item Ensures result is non-negative (minimum 0)
#' }
#'
#' @examples
#' \dontrun{
#' # Load district and deduction layers
#' district <- sf::st_read("district.shp")
#' district <- sf::st_transform(district, 26986)
#' deductions <- load_density_deductions()
#'
#' # Calculate density denominator
#' dd_acres <- calculate_density_denominator(
#'   district,
#'   deductions,
#'   output_unit = "acres"
#' )
#'
#' # Calculate gross density
#' required_units <- 750
#' gross_density <- required_units / dd_acres
#' cat("Required gross density:", gross_density, "units/acre\n")
#' }
#'
#' @seealso \code{\link{calculate_district_area}}, \code{\link{load_density_deductions}}
#'
#' @export
calculate_density_denominator <- function(district_sf,
                                          deduction_layers,
                                          output_unit = c("acres", "sqft")) {

  # Validate inputs
  if (!inherits(district_sf, "sf")) {
    cli::cli_abort("{.arg district_sf} must be an sf object")
  }

  if (!inherits(deduction_layers, "sf")) {
    cli::cli_abort("{.arg deduction_layers} must be an sf object")
  }

  output_unit <- match.arg(output_unit)

  # Validate CRS
  district_crs <- sf::st_crs(district_sf)
  deduction_crs <- sf::st_crs(deduction_layers)

  if (is.na(district_crs) || is.na(deduction_crs)) {
    cli::cli_abort("Both {.arg district_sf} and {.arg deduction_layers} must have defined CRS")
  }

  if (district_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg district_sf} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{district_crs$epsg}"
      )
    )
  }

  if (deduction_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg deduction_layers} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{deduction_crs$epsg}"
      )
    )
  }

  # Calculate total district area
  total_area_sqft <- calculate_district_area(district_sf, output_unit = "sqft")

  if (is.na(total_area_sqft)) {
    cli::cli_warn("District area is NA, returning NA")
    return(NA_real_)
  }

  # Check for empty deduction layers
  if (nrow(deduction_layers) == 0 || all(sf::st_is_empty(deduction_layers))) {
    cli::cli_inform("No deductions apply, returning full district area")
    if (output_unit == "acres") {
      return(total_area_sqft / 43560)
    } else {
      return(total_area_sqft)
    }
  }

  # Repair invalid geometries
  district_sf <- sf::st_make_valid(district_sf)
  deduction_layers <- sf::st_make_valid(deduction_layers)

  # Union all features
  district_union <- sf::st_union(district_sf)
  deduction_union <- sf::st_union(deduction_layers)

  # Calculate intersection of district with deductions
  suppressWarnings({
    deduction_intersection <- sf::st_intersection(district_union, deduction_union)
  })

  # Calculate deduction area
  deduction_area_sqft <- 0
  if (length(deduction_intersection) > 0 && !all(sf::st_is_empty(deduction_intersection))) {
    deduction_area_sqft <- as.numeric(sf::st_area(deduction_intersection))
  }

  # Calculate density denominator (total - deductions)
  dd_sqft <- total_area_sqft - deduction_area_sqft

  # Ensure non-negative (in case of rounding errors)
  dd_sqft <- max(0, dd_sqft)

  # Convert to requested units
  if (output_unit == "acres") {
    return(dd_sqft / 43560)
  } else {
    return(dd_sqft)
  }
}


#' Pre-compute Spatial Attributes for Simulation Workflows
#'
#' Pre-compute all spatial relationships between parcels and GIS layers to enable
#' high-performance batch simulation. This function calculates expensive spatial
#' intersections once, allowing thousands of capacity calculations to run using
#' only fast arithmetic operations.
#'
#' @param parcels An sf object containing parcel data (from
#'   \code{\link{load_municipality}}). Must be in EPSG:26986 projection.
#' @param station_areas Optional sf object with transit station buffers (from
#'   \code{\link{load_transit_stations}}). If provided, calculates station area
#'   overlap for each parcel.
#' @param density_deductions Optional sf object with density deduction polygons
#'   (from \code{\link{load_density_deductions}}). If provided, calculates
#'   deduction area for each parcel.
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return The input parcels sf object with additional pre-computed columns:
#'   \describe{
#'     \item{station_area_sf}{Numeric, area of parcel within station buffers
#'       (square feet). 0 if no station overlap. Only added if station_areas provided.}
#'     \item{station_area_pct}{Numeric, percentage of parcel within station buffers
#'       (0 to 1). Only added if station_areas provided.}
#'     \item{in_station_area}{Logical, TRUE if parcel has any station area overlap.
#'       Only added if station_areas provided.}
#'     \item{density_deduction_area}{Numeric, area of parcel within density
#'       deduction zones (square feet). 0 if no deduction overlap. Only added if
#'       density_deductions provided.}
#'   }
#'
#'   The returned object has attribute \code{spatial_attributes_precomputed = TRUE}
#'   and \code{precomputation_date} for tracking.
#'
#' @details
#' \strong{Performance Benefits:}
#'
#' This function enables ~1000x performance improvement for batch simulation
#' scenarios. By separating one-time spatial computations from iterative arithmetic
#' calculations, you can evaluate thousands of zoning scenarios efficiently:
#'
#' \itemize{
#'   \item \strong{Without pre-computation:} 10,000 simulations × 30 sec = 83 hours
#'   \item \strong{With pre-computation:} 1× 30 sec + 10,000× 0.03 sec = 5 minutes
#' }
#'
#' \strong{Typical Workflow:}
#'
#' \preformatted{
#' # Setup phase (once per municipality)
#' parcels <- load_municipality("maynard.zip")
#' stations <- load_transit_stations()
#' deductions <- load_density_deductions()
#'
#' parcels_enriched <- precompute_spatial_attributes(
#'   parcels,
#'   station_areas = stations,
#'   density_deductions = deductions
#' )
#'
#' # Save for reuse
#' saveRDS(parcels_enriched, "maynard_enriched.rds")
#'
#' # Simulation phase (1000s of iterations)
#' parcels <- readRDS("maynard_enriched.rds")  # Fast load
#'
#' results <- lapply(1:10000, function(i) {
#'   zoning <- generate_random_zoning(...)
#'
#'   capacity <- calculate_district_capacity(
#'     parcels,
#'     zoning,
#'     precomputed = TRUE  # Skip spatial operations
#'   )
#'
#'   evaluate_compliance(capacity, ..., precomputed = TRUE)
#' })
#' }
#'
#' \strong{Computation Details:}
#'
#' \emph{Station Area Overlap:}
#' Uses \code{\link{calculate_station_intersection}} to compute spatial intersection
#' between each parcel and transit station buffers. Adds percentage and boolean
#' columns for easy filtering.
#'
#' \emph{Density Deduction Area:}
#' Calculates per-parcel intersection with the gross density denominator deduction
#' layer. This replaces the district-level intersection normally done in
#' \code{\link{evaluate_compliance}}.
#'
#' @section NA Handling:
#' \itemize{
#'   \item Parcels outside station areas get \code{station_area_sf = 0} (not NA)
#'   \item Parcels outside deduction zones get \code{density_deduction_area = 0} (not NA)
#'   \item Invalid geometries are repaired with \code{sf::st_make_valid()}
#' }
#'
#' @examples
#' \dontrun{
#' # Load municipality data
#' parcels <- load_municipality(
#'   "inst/extdata/parcels/174_MAYNARD_basic.zip",
#'   community_name = "Maynard"
#' )
#'
#' # Load GIS layers
#' stations <- load_transit_stations()
#' deductions <- load_density_deductions()
#'
#' # Pre-compute spatial attributes
#' parcels_enriched <- precompute_spatial_attributes(
#'   parcels,
#'   station_areas = stations,
#'   density_deductions = deductions
#' )
#'
#' # Check new columns
#' names(parcels_enriched)
#' # Includes: station_area_sf, station_area_pct, in_station_area,
#' #           density_deduction_area
#'
#' # Save for simulation
#' saveRDS(parcels_enriched, "maynard_enriched.rds")
#'
#' # Later: Load and use with precomputed = TRUE
#' parcels <- readRDS("maynard_enriched.rds")
#' zoning <- extract_zoning_parameters("model.xlsx", district = 1)
#'
#' capacity <- calculate_district_capacity(
#'   parcels,
#'   zoning,
#'   precomputed = TRUE
#' )
#' }
#'
#' @seealso
#' \code{\link{calculate_district_capacity}},
#' \code{\link{evaluate_compliance}},
#' \code{\link{calculate_station_intersection}},
#' \code{\link{calculate_density_denominator}}
#'
#' @export
precompute_spatial_attributes <- function(parcels,
                                          station_areas = NULL,
                                          density_deductions = NULL,
                                          verbose = TRUE) {

  # Validate inputs
  if (!inherits(parcels, "sf")) {
    cli::cli_abort("{.arg parcels} must be an sf object")
  }

  # Validate CRS
  parcels_crs <- sf::st_crs(parcels)
  if (is.na(parcels_crs)) {
    cli::cli_abort("{.arg parcels} has no CRS defined")
  }

  if (parcels_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg parcels} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{parcels_crs$epsg}",
        "i" = "Transform with: {.code sf::st_transform(parcels, 26986)}"
      )
    )
  }

  if (verbose) {
    cli::cli_alert_info("Pre-computing spatial attributes for {nrow(parcels)} parcel{?s}...")
  }

  # ===== Pre-compute Station Area Overlap =====
  if (!is.null(station_areas)) {
    if (verbose) {
      cli::cli_alert("Computing station area overlaps...")
    }

    # Validate station_areas CRS
    station_crs <- sf::st_crs(station_areas)
    if (is.na(station_crs)) {
      cli::cli_abort("{.arg station_areas} has no CRS defined")
    }

    if (station_crs$epsg != 26986) {
      cli::cli_abort(
        c(
          "{.arg station_areas} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
          "i" = "Current CRS: EPSG:{station_crs$epsg}"
        )
      )
    }

    # Use existing calculate_station_intersection function
    # This adds station_area_sf column (in square feet)
    parcels <- calculate_station_intersection(
      parcels = parcels,
      station_areas = station_areas,
      output_unit = "sqft"
    )

    # Add percentage column
    parcels$station_area_pct <- parcels$station_area_sf / parcels$SQFT

    # Add boolean flag
    parcels$in_station_area <- parcels$station_area_pct > 0

    if (verbose) {
      n_in_station <- sum(parcels$in_station_area, na.rm = TRUE)
      cli::cli_alert_success(
        "Station area overlap computed: {n_in_station} parcel{?s} in station areas ({round(n_in_station/nrow(parcels)*100, 1)}%)"
      )
    }
  }

  # ===== Pre-compute Density Deduction Areas =====
  if (!is.null(density_deductions)) {
    if (verbose) {
      cli::cli_alert("Computing density deduction overlaps...")
    }

    # Validate density_deductions CRS
    deduction_crs <- sf::st_crs(density_deductions)
    if (is.na(deduction_crs)) {
      cli::cli_abort("{.arg density_deductions} has no CRS defined")
    }

    if (deduction_crs$epsg != 26986) {
      cli::cli_abort(
        c(
          "{.arg density_deductions} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
          "i" = "Current CRS: EPSG:{deduction_crs$epsg}"
        )
      )
    }

    # Initialize deduction area column with 0
    parcels$density_deduction_area <- 0

    # Check for empty deduction layer
    if (nrow(density_deductions) == 0 || all(sf::st_is_empty(density_deductions))) {
      if (verbose) {
        cli::cli_alert_warning("Density deduction layer is empty, all parcels have 0 deduction")
      }
    } else {
      # Repair invalid geometries
      parcels <- sf::st_make_valid(parcels)
      density_deductions <- sf::st_make_valid(density_deductions)

      # Union all deduction features into single geometry
      deduction_union <- sf::st_union(density_deductions)

      # Perform intersection to find overlapping areas
      suppressWarnings({
        intersection <- sf::st_intersection(parcels, deduction_union)
      })

      # If there are intersections, calculate areas
      if (nrow(intersection) > 0) {
        intersection_areas <- as.numeric(sf::st_area(intersection))

        # Match intersection results back to original parcels
        parcel_indices <- as.integer(rownames(intersection))

        # Sum areas by parcel (in case parcel split into multiple pieces)
        for (i in seq_along(parcel_indices)) {
          idx <- parcel_indices[i]
          parcels$density_deduction_area[idx] <-
            parcels$density_deduction_area[idx] + intersection_areas[i]
        }

        if (verbose) {
          n_with_deduction <- sum(parcels$density_deduction_area > 0, na.rm = TRUE)
          total_deduction_acres <- sum(parcels$density_deduction_area / 43560, na.rm = TRUE)
          cli::cli_alert_success(
            "Density deductions computed: {n_with_deduction} parcel{?s} affected ({round(total_deduction_acres, 1)} acres total)"
          )
        }
      } else {
        if (verbose) {
          cli::cli_alert_info("No parcels intersect with density deduction layer")
        }
      }
    }
  }

  # ===== Add Metadata =====
  attr(parcels, "spatial_attributes_precomputed") <- TRUE
  attr(parcels, "precomputation_date") <- Sys.Date()

  if (!is.null(station_areas)) {
    attr(parcels, "precomputed_station_areas") <- TRUE
  }

  if (!is.null(density_deductions)) {
    attr(parcels, "precomputed_density_deductions") <- TRUE
  }

  if (verbose) {
    cli::cli_alert_success("Pre-computation complete")
  }

  return(parcels)
}


#' Validate District Contiguity
#'
#' Check if a compliance district meets contiguity requirements: at least 50%
#' of district area must be in a single contiguous portion, and all non-contiguous
#' portions must be at least 5 acres each.
#'
#' @param district_sf An sf object representing the compliance district geometry.
#'   Must be in EPSG:26986 projection.
#' @param min_contiguous_pct Minimum percentage of district area that must be
#'   contiguous (default: 0.5 for 50%).
#' @param min_portion_acres Minimum area in acres for each non-contiguous portion
#'   (default: 5 acres).
#'
#' @return A list with detailed contiguity analysis:
#'   \describe{
#'     \item{is_valid}{Logical, TRUE if district meets all contiguity requirements}
#'     \item{contiguous_pct}{Numeric, percentage (0-1) of district area in largest
#'       contiguous portion}
#'     \item{n_portions}{Integer, number of separate contiguous portions}
#'     \item{portion_areas}{Numeric vector, area of each portion in acres (sorted
#'       largest to smallest)}
#'     \item{violations}{Character vector, descriptions of any violations (empty
#'       if no violations)}
#'   }
#'
#' @details
#' The MBTA Communities Act contiguity requirements (760 CMR 59.04(3)(d)):
#' \enumerate{
#'   \item At least 50% of district area must be in a single contiguous portion
#'   \item Each non-contiguous portion must be at least 5 acres
#' }
#'
#' The function identifies contiguous portions using connected component analysis:
#' \enumerate{
#'   \item Union all district features into single geometry
#'   \item Cast to separate POLYGON geometries (splits into portions)
#'   \item Calculate area of each portion
#'   \item Check percentage in largest portion
#'   \item Check minimum size for all other portions
#' }
#'
#' \strong{Important:} Multi-polygon features can have internal holes/donuts, so
#' the number of features ≠ number of contiguous portions. For example, Somerville's
#' district has 7 features but 13 separate contiguous portions.
#'
#' @section NA Handling:
#' Returns list with is_valid = NA if:
#' \itemize{
#'   \item District geometry is empty or invalid
#'   \item District area is 0
#' }
#'
#' @examples
#' \dontrun{
#' # Load a district
#' district <- sf::st_read("district.shp")
#' district <- sf::st_transform(district, 26986)
#'
#' # Validate contiguity
#' result <- validate_contiguity(district)
#'
#' if (result$is_valid) {
#'   cat("District meets contiguity requirements\n")
#' } else {
#'   cat("Violations:\n")
#'   cat(paste("-", result$violations, collapse = "\n"))
#' }
#'
#' # Custom thresholds
#' result <- validate_contiguity(
#'   district,
#'   min_contiguous_pct = 0.75,  # 75% instead of 50%
#'   min_portion_acres = 10       # 10 acres instead of 5
#' )
#' }
#'
#' @seealso \code{\link{calculate_district_area}}
#'
#' @export
validate_contiguity <- function(district_sf,
                                min_contiguous_pct = 0.5,
                                min_portion_acres = 5) {

  # Validate inputs
  if (!inherits(district_sf, "sf")) {
    cli::cli_abort("{.arg district_sf} must be an sf object")
  }

  if (!is.numeric(min_contiguous_pct) || min_contiguous_pct < 0 || min_contiguous_pct > 1) {
    cli::cli_abort("{.arg min_contiguous_pct} must be between 0 and 1")
  }

  if (!is.numeric(min_portion_acres) || min_portion_acres < 0) {
    cli::cli_abort("{.arg min_portion_acres} must be non-negative")
  }

  # Validate CRS
  district_crs <- sf::st_crs(district_sf)
  if (is.na(district_crs)) {
    cli::cli_abort("{.arg district_sf} has no CRS defined")
  }

  if (district_crs$epsg != 26986) {
    cli::cli_abort(
      c(
        "{.arg district_sf} must use EPSG:26986 (NAD83 Massachusetts State Plane)",
        "i" = "Current CRS: EPSG:{district_crs$epsg}"
      )
    )
  }

  # Check for empty geometries
  if (nrow(district_sf) == 0 || all(sf::st_is_empty(district_sf))) {
    return(list(
      is_valid = NA,
      contiguous_pct = NA_real_,
      n_portions = 0L,
      portion_areas = numeric(0),
      violations = "District has empty geometry"
    ))
  }

  # Repair invalid geometries
  district_sf <- sf::st_make_valid(district_sf)

  # Union all features into single geometry
  district_union <- sf::st_union(district_sf)

  # Cast to separate POLYGON geometries to identify contiguous portions
  # This handles multi-polygon features with holes/donuts
  portions <- sf::st_cast(district_union, "POLYGON")

  # Calculate area of each portion
  portion_areas_sqft <- as.numeric(sf::st_area(portions))
  portion_areas_acres <- portion_areas_sqft / 43560

  # Sort portions by area (largest first)
  portion_areas_acres <- sort(portion_areas_acres, decreasing = TRUE)

  n_portions <- length(portion_areas_acres)
  total_area_acres <- sum(portion_areas_acres)

  # Calculate percentage in largest portion
  if (total_area_acres == 0) {
    return(list(
      is_valid = NA,
      contiguous_pct = NA_real_,
      n_portions = n_portions,
      portion_areas = portion_areas_acres,
      violations = "District has zero area"
    ))
  }

  largest_pct <- portion_areas_acres[1] / total_area_acres

  # Check for violations
  violations <- character(0)

  # Check 1: Largest portion must be >= min_contiguous_pct
  if (largest_pct < min_contiguous_pct) {
    violations <- c(
      violations,
      sprintf(
        "Largest contiguous portion is %.1f%% (%.2f acres), but must be ≥%.0f%%",
        largest_pct * 100,
        portion_areas_acres[1],
        min_contiguous_pct * 100
      )
    )
  }

  # Check 2: All non-contiguous portions must be >= min_portion_acres
  if (n_portions > 1) {
    small_portions <- portion_areas_acres[-1][portion_areas_acres[-1] < min_portion_acres]
    if (length(small_portions) > 0) {
      violations <- c(
        violations,
        sprintf(
          "%d non-contiguous portion(s) are smaller than %g acres (sizes: %s acres)",
          length(small_portions),
          min_portion_acres,
          paste(round(small_portions, 2), collapse = ", ")
        )
      )
    }
  }

  # Determine if valid
  is_valid <- length(violations) == 0

  return(list(
    is_valid = is_valid,
    contiguous_pct = largest_pct,
    n_portions = n_portions,
    portion_areas = portion_areas_acres,
    violations = violations
  ))
}