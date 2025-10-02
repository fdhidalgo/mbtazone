#' Unit Capacity Calculation Functions
#'
#' Core functions for calculating housing unit capacity under the
#' Massachusetts MBTA Communities Act compliance model.
#'
#' @name unit_capacity_calculations
NULL

#' Calculate Developable Area After Exclusions
#'
#' Determines the net developable area of a parcel after applying exclusions
#' and minimum lot size requirements. This is the foundational calculation
#' (Excel column N) that feeds into all subsequent unit capacity calculations.
#'
#' @param lot_area Numeric vector of total lot areas in square feet
#' @param excluded_area Numeric vector of excluded areas in square feet (water,
#'   wetlands, etc.)
#' @param min_lot_size Minimum allowable lot size in square feet for development.
#'   This is a district-specific zoning parameter that should be extracted using
#'   \code{\link{extract_zoning_parameters}} or specified manually via
#'   \code{\link{create_zoning_parameters}}.
#'
#' @return Numeric vector of developable areas in square feet. Returns 0 for
#'   lots below minimum size or where exclusions exceed total area. Returns NA
#'   for parcels with missing lot area or excluded area data.
#'
#' @details
#' The calculation follows this logic:
#' - If lot_area or excluded_area is NA, return NA (preserve uncertainty)
#' - If lot_area < min_lot_size, return 0 (undevelopable)
#' - Otherwise, return max(0, lot_area - excluded_area)
#'
#' This corresponds to Excel column N in the MBTA Communities compliance model.
#'
#' Note that min_lot_size varies by community and district. Use
#' \code{extract_zoning_parameters()} to extract the correct value from Excel
#' compliance models.
#'
#' @examples
#' # Extract zoning parameters first
#' \dontrun{
#' params <- extract_zoning_parameters("Chelsea_Model.xlsx", district = 1)
#' }
#'
#' # Or create manually
#' params <- create_zoning_parameters(min_lot_size = 5000)
#'
#' # Single parcel calculation
#' calculate_developable_area(
#'   lot_area = 10000,
#'   excluded_area = 1000,
#'   min_lot_size = params$min_lot_size
#' )
#' # Returns: 9000
#'
#' # Multiple parcels with missing data
#' calculate_developable_area(
#'   lot_area = c(10000, NA, 8000),
#'   excluded_area = c(1000, 500, 2000),
#'   min_lot_size = params$min_lot_size
#' )
#' # Returns: c(9000, NA, 6000)
#'
#' @seealso \code{\link{extract_zoning_parameters}}, \code{\link{create_zoning_parameters}}
#' @export
calculate_developable_area <- function(lot_area, excluded_area, min_lot_size) {

  # Validate critical assumptions
  stopifnot(
    length(min_lot_size) == 1,
    length(lot_area) == length(excluded_area)
  )

  # Calculate developable area using fcase for clarity
  developable_area <- data.table::fcase(
    is.na(lot_area) | is.na(excluded_area), NA_real_,
    lot_area < min_lot_size, 0,
    (lot_area - excluded_area) < 0, 0,
    default = lot_area - excluded_area
  )

  return(developable_area)
}

#' Calculate Final Unit Capacity 
#'
#' Determines the final allowable housing unit capacity by taking the minimum
#' of all applicable zoning constraint methods. This is the culminating 
#' calculation (Excel column AF) that determines compliance capacity.
#'
#' @param units_building_capacity Numeric vector of units from building capacity
#'   (floor area divided by 1000 sq ft per unit)
#' @param units_density_limits Numeric vector of units from density limits
#'   (lot area times max units per acre)
#' @param units_lot_coverage Numeric vector of units from lot coverage limits
#'   (coverage area times building height divided by 1000)
#' @param units_lot_area_req Numeric vector of units from lot area requirements
#'   (lot area divided by area per unit)
#' @param units_far_limits Numeric vector of units from FAR limits
#'   (lot area times FAR divided by 1000)
#' @param units_max_cap Numeric vector of adjusted units (capped by max units per lot)
#' @param units_graduated_lots Numeric vector of units from graduated lot sizing
#'
#' @return Numeric vector of final unit capacities. Values below 2.5 become 0,
#'   values 2.5-3.0 become 3, others are rounded to nearest integer. Returns NA
#'   if all constraint methods are NA.
#'
#' @details
#' This function implements the "minimum of methods" approach required by the
#' MBTA Communities Act. It evaluates 7 different zoning constraint methods
#' and takes the most restrictive (minimum) value for each parcel.
#' 
#' The 7 methods correspond to Excel columns X, Y, Z, AA, AB, AC, and AE.
#' NA values are treated as unlimited (Inf) during minimum calculation, unless
#' all methods are NA, in which case NA is returned.
#' 
#' Final rounding rules:
#' - Less than 2.5 units: return 0 (below threshold)
#' - Between 2.5 and 3.0 units: return 3 (minimum viable)
#' - Greater than 3.0 units: rounded to nearest integer
#'
#' @examples
#' # Single parcel with all methods applicable
#' calculate_final_unit_capacity(
#'   units_building_capacity = 5.2,
#'   units_density_limits = 4.8, 
#'   units_lot_coverage = 6.1,
#'   units_lot_area_req = 3.9,
#'   units_far_limits = 4.2,
#'   units_max_cap = 5.0,
#'   units_graduated_lots = 4.5
#' )
#' # Returns: 4 (minimum is 3.9, rounded to 4)
#' 
#' # Multiple parcels with some NA constraints
#' calculate_final_unit_capacity(
#'   units_building_capacity = c(5.2, 2.1, NA),
#'   units_density_limits = c(4.8, NA, NA),
#'   units_lot_coverage = c(6.1, 1.8, NA), 
#'   units_lot_area_req = c(3.9, 2.3, NA),
#'   units_far_limits = c(4.2, NA, NA),
#'   units_max_cap = c(5.0, 2.0, NA),
#'   units_graduated_lots = c(4.5, 2.2, NA)
#' )
#' # Returns: c(4, 0, NA)
#'
#' @export
calculate_final_unit_capacity <- function(units_building_capacity,
                                        units_density_limits,
                                        units_lot_coverage,
                                        units_lot_area_req,
                                        units_far_limits,
                                        units_max_cap,
                                        units_graduated_lots) {

  # Create data frame for minimum calculation (will fail naturally if lengths differ)
  unit_methods <- data.frame(
    building_capacity = units_building_capacity,
    density_limits = units_density_limits,
    lot_coverage = units_lot_coverage,
    lot_area_req = units_lot_area_req,
    far_limits = units_far_limits,
    max_cap = units_max_cap,
    graduated_lots = units_graduated_lots
  )

  # Calculate minimum across all methods using purrr for consistency
  min_values <- purrr::pmap_dbl(unit_methods, function(...) {
    values <- c(...)
    if (all(is.na(values))) {
      return(NA_real_)
    }
    values[is.na(values)] <- Inf
    min(values)
  })

  # Apply final rounding rules per MBTA Communities model
  final_capacity <- data.table::fcase(
    is.na(min_values), NA_real_,
    min_values < 2.5, 0,
    min_values >= 2.5 & min_values < 3, 3,
    default = round(min_values)
  )

  return(final_capacity)
}

#' Calculate Net Developable Area
#'
#' Determines the net developable area for unit capacity calculations, allowing
#' for manual override values when specified. This is Excel column Q.
#'
#' @param developable_area Numeric vector of calculated developable areas in square feet
#'   (typically from \code{\link{calculate_developable_area}})
#' @param override_developable_sf Numeric vector of manual override values. Use NA to
#'   indicate no override (most common case). When specified, this value replaces the
#'   calculated developable area.
#'
#' @return Numeric vector of net developable areas in square feet. Returns the override
#'   value when provided (non-NA), otherwise returns the calculated developable area.
#'
#' @details
#' The Excel compliance model allows users to manually override calculated developable
#' areas in column O. This function implements column Q logic:
#' - If override_developable_sf is NA (typical case), return developable_area
#' - If override_developable_sf is specified, return that value
#'
#' This corresponds to Excel column Q: =IF(ISNA(O), N, O)
#'
#' Override values are used for special cases where the automated calculation doesn't
#' accurately represent developable area (e.g., public land with development rights,
#' parcels with special zoning considerations).
#'
#' @examples
#' # No override - use calculated values
#' calculate_net_developable_area(
#'   developable_area = c(9000, 7000, 11500),
#'   override_developable_sf = c(NA, NA, NA)
#' )
#' # Returns: c(9000, 7000, 11500)
#'
#' # Mixed - some parcels with overrides
#' calculate_net_developable_area(
#'   developable_area = c(9000, 7000, 11500),
#'   override_developable_sf = c(NA, 8500, NA)
#' )
#' # Returns: c(9000, 8500, 11500) - middle parcel uses override
#'
#' @seealso \code{\link{calculate_developable_area}}
#' @export
calculate_net_developable_area <- function(developable_area,
                                           override_developable_sf) {

  # Validate inputs
  stopifnot(
    length(developable_area) == length(override_developable_sf)
  )

  # Use override when provided, otherwise use calculated value
  net_developable <- ifelse(
    is.na(override_developable_sf),
    developable_area,
    override_developable_sf
  )

  return(net_developable)
}

#' Calculate Exclusion Ratio
#'
#' Computes the ratio of excluded land to total lot area for each parcel.
#' This is Excel column R.
#'
#' @param excluded_area Numeric vector of excluded areas in square feet
#'   (water, wetlands, protected land, etc.)
#' @param lot_area Numeric vector of total lot areas in square feet
#'
#' @return Numeric vector of exclusion ratios (0 to 1). Returns 0 when
#'   lot_area is 0 or NA. Returns NA when both inputs are NA.
#'
#' @details
#' The exclusion ratio indicates what proportion of a parcel is unavailable
#' for development. It's used in open space calculations to account for
#' land already excluded from development.
#'
#' Formula: exclusion_ratio = excluded_area / lot_area
#'
#' This corresponds to Excel column R: =IFERROR(L/I, 0)
#'
#' Special handling:
#' - When lot_area = 0: returns 0 (avoid division by zero)
#' - When lot_area is NA: returns 0 (Excel behavior)
#' - When excluded_area is NA but lot_area is not: returns NA (preserve uncertainty)
#' - When both are NA: returns NA
#'
#' @examples
#' # Normal cases
#' calculate_exclusion_ratio(
#'   excluded_area = c(1000, 500, 2000),
#'   lot_area = c(10000, 8000, 12000)
#' )
#' # Returns: c(0.1, 0.0625, 0.1667)
#'
#' # Zero lot area (division by zero)
#' calculate_exclusion_ratio(
#'   excluded_area = c(1000, 500),
#'   lot_area = c(10000, 0)
#' )
#' # Returns: c(0.1, 0)
#'
#' # No exclusions
#' calculate_exclusion_ratio(
#'   excluded_area = c(0, 0, 0),
#'   lot_area = c(10000, 8000, 12000)
#' )
#' # Returns: c(0, 0, 0)
#'
#' @export
calculate_exclusion_ratio <- function(excluded_area, lot_area) {

  # Validate inputs
  stopifnot(
    length(excluded_area) == length(lot_area)
  )

  # Calculate ratio, handling division by zero and NA
  # Excel formula: =IFERROR(L/I, 0)
  exclusion_ratio <- data.table::fcase(
    is.na(lot_area), 0,
    lot_area == 0, 0,
    is.na(excluded_area), NA_real_,
    default = excluded_area / lot_area
  )

  return(exclusion_ratio)
}

#' Calculate Open Space Requirement
#'
#' Determines the minimum open space requirement as a proportion of lot area.
#' This is Excel column S.
#'
#' @param min_required_open_space Numeric value of minimum required open space
#'   from zoning parameters (as decimal, e.g., 0.15 for 15%). Single value applied
#'   to all parcels in the district.
#'
#' @return Numeric value of open space requirement (as decimal). Always returns
#'   max(0.2, min_required_open_space), with 0.2 (20%) as the minimum per MBTA
#'   Communities Act requirements. Returns 0.2 if input is NA or invalid.
#'
#' @details
#' The MBTA Communities Act requires a minimum 20% open space even if local
#' zoning allows less. This function enforces that floor while respecting
#' higher local requirements.
#'
#' Logic:
#' - If min_required_open_space is NA or >= 1 (invalid): return 0.2
#' - Otherwise: return max(0.2, min_required_open_space)
#'
#' This corresponds to Excel column S, which uses a conditional formula.
#'
#' @examples
#' # Zoning requires 15% - MBTA minimum of 20% applies
#' calculate_open_space_requirement(0.15)
#' # Returns: 0.2
#'
#' # Zoning requires 25% - use higher local requirement
#' calculate_open_space_requirement(0.25)
#' # Returns: 0.25
#'
#' # No requirement specified - use MBTA minimum
#' calculate_open_space_requirement(NA)
#' # Returns: 0.2
#'
#' @export
calculate_open_space_requirement <- function(min_required_open_space) {

  # Validate input
  stopifnot(
    length(min_required_open_space) == 1
  )

  # Apply MBTA 20% minimum requirement
  if (is.na(min_required_open_space) || min_required_open_space >= 1) {
    open_space_req <- 0.2
  } else {
    open_space_req <- max(0.2, min_required_open_space)
  }

  return(open_space_req)
}

#' Calculate Required Open Space Area
#'
#' Computes the total open space area required by zoning for each parcel.
#' This is Excel column T.
#'
#' @param lot_area Numeric vector of total lot areas in square feet
#' @param exclusion_ratio Numeric vector of exclusion ratios (from
#'   \code{\link{calculate_exclusion_ratio}})
#' @param open_space_requirement Numeric value of open space requirement (from
#'   \code{\link{calculate_open_space_requirement}})
#' @param net_developable_area Numeric vector of net developable areas (from
#'   \code{\link{calculate_net_developable_area}})
#' @param override_developable_sf Numeric vector indicating which parcels have
#'   override values (NA = no override, numeric = override applied)
#' @param water_included Character value indicating if water bodies count toward
#'   open space ("Y" or "N")
#'
#' @return Numeric vector of required open space areas in square feet.
#'
#' @details
#' This function implements complex conditional logic:
#'
#' **If override is specified (not NA):**
#' - If net_developable_area > 0: return net_developable_area * open_space_requirement
#' - Otherwise: return 0
#'
#' **If no override (NA):**
#' - If water_included = "Y": return max(exclusion_ratio, open_space_requirement) * lot_area
#' - If water_included = "N": return (exclusion_ratio + open_space_requirement) * lot_area
#'
#' The water_included flag determines whether water bodies can count toward the
#' open space requirement (max) or must be in addition to it (sum).
#'
#' @examples
#' # No water bodies, no override
#' calculate_required_open_space_area(
#'   lot_area = 10000,
#'   exclusion_ratio = 0.1,
#'   open_space_requirement = 0.2,
#'   net_developable_area = 9000,
#'   override_developable_sf = NA,
#'   water_included = "N"
#' )
#' # Returns: (0.1 + 0.2) * 10000 = 3000
#'
#' # Water included - use maximum
#' calculate_required_open_space_area(
#'   lot_area = 10000,
#'   exclusion_ratio = 0.25,
#'   open_space_requirement = 0.2,
#'   net_developable_area = 9000,
#'   override_developable_sf = NA,
#'   water_included = "Y"
#' )
#' # Returns: max(0.25, 0.2) * 10000 = 2500
#'
#' @export
calculate_required_open_space_area <- function(lot_area,
                                               exclusion_ratio,
                                               open_space_requirement,
                                               net_developable_area,
                                               override_developable_sf,
                                               water_included) {

  # Validate inputs
  stopifnot(
    length(lot_area) == length(exclusion_ratio),
    length(lot_area) == length(net_developable_area),
    length(lot_area) == length(override_developable_sf),
    length(open_space_requirement) == 1,
    length(water_included) == 1,
    water_included %in% c("Y", "N")
  )

  # Calculate required open space using Excel column T logic
  # Replicate water_included for vectorized comparison
  water_included_vec <- rep(water_included, length(lot_area))

  required_open_space <- data.table::fcase(
    # If override is specified and net developable > 0
    !is.na(override_developable_sf) & net_developable_area > 0,
    net_developable_area * open_space_requirement,

    # If override specified but net developable = 0
    !is.na(override_developable_sf),
    0,

    # No override, water included: use max of exclusion and open space
    water_included_vec == "Y",
    pmax(exclusion_ratio, open_space_requirement) * lot_area,

    # No override, water not included: sum exclusion and open space
    default = (exclusion_ratio + open_space_requirement) * lot_area
  )

  return(required_open_space)
}

#' Calculate Parking Area
#'
#' Computes the land area consumed by parking requirements based on zoning.
#' This is Excel column U.
#'
#' @param lot_area Numeric vector of total lot areas in square feet
#' @param required_open_space Numeric vector of required open space areas (from
#'   \code{\link{calculate_required_open_space_area}})
#' @param net_developable_area Numeric vector of net developable areas
#' @param parking_spaces_per_unit Numeric value of required parking spaces per
#'   dwelling unit from zoning parameters
#'
#' @return Numeric vector of parking areas in square feet. Returns 0 for parcels
#'   with no net developable area.
#'
#' @details
#' This function estimates surface parking area using a lookup table based on
#' parking spaces per unit:
#'
#' - 0 spaces/unit: 0% coverage factor (0 sq ft per space)
#' - 0.01-0.5 spaces/unit: 30% coverage factor (~600 sq ft per space)
#' - 0.51-1.0 spaces/unit: 45% coverage factor (400 sq ft per space)
#' - 1.01-1.25 spaces/unit: 55% coverage factor (~440 sq ft per space)
#' - 1.26-1.5 spaces/unit: 60% coverage factor (400 sq ft per space)
#' - >1.5 spaces/unit: 65% coverage factor (433 sq ft per space)
#'
#' Formula: parking_area = (lot_area - required_open_space) * parking_factor
#'
#' Only applies to parcels with positive net developable area.
#'
#' @examples
#' # 1 space per unit
#' calculate_parking_area(
#'   lot_area = 10000,
#'   required_open_space = 2000,
#'   net_developable_area = 8000,
#'   parking_spaces_per_unit = 1.0
#' )
#' # Returns: (10000 - 2000) * 0.45 = 3600
#'
#' # No parking required
#' calculate_parking_area(
#'   lot_area = 10000,
#'   required_open_space = 2000,
#'   net_developable_area = 8000,
#'   parking_spaces_per_unit = 0
#' )
#' # Returns: 0
#'
#' @export
calculate_parking_area <- function(lot_area,
                                   required_open_space,
                                   net_developable_area,
                                   parking_spaces_per_unit) {

  # Validate inputs
  stopifnot(
    length(lot_area) == length(required_open_space),
    length(lot_area) == length(net_developable_area),
    length(parking_spaces_per_unit) == 1
  )

  # Determine parking coverage factor based on spaces per unit
  parking_factor <- data.table::fcase(
    is.na(parking_spaces_per_unit), 0,
    parking_spaces_per_unit == 0, 0,
    parking_spaces_per_unit >= 0.01 & parking_spaces_per_unit <= 0.5, 0.3,
    parking_spaces_per_unit > 0.5 & parking_spaces_per_unit <= 1, 0.45,
    parking_spaces_per_unit > 1 & parking_spaces_per_unit <= 1.25, 0.55,
    parking_spaces_per_unit > 1.25 & parking_spaces_per_unit <= 1.5, 0.6,
    default = 0.65
  )

  # Calculate parking area only for developable parcels
  parking_area <- data.table::fcase(
    is.na(net_developable_area) | net_developable_area <= 0, 0,
    default = (lot_area - required_open_space) * parking_factor
  )

  return(parking_area)
}

#' Calculate Building Footprint
#'
#' Computes the land area available for building footprint after removing
#' open space and parking. This is Excel column V.
#'
#' @param lot_area Numeric vector of total lot areas in square feet
#' @param required_open_space Numeric vector of required open space areas
#' @param parking_area Numeric vector of parking areas
#' @param net_developable_area Numeric vector of net developable areas
#'
#' @return Numeric vector of building footprint areas in square feet. Returns 0
#'   for parcels with no net developable area.
#'
#' @details
#' Formula: building_footprint = lot_area - required_open_space - parking_area
#'
#' Only applies to parcels with positive net developable area.
#'
#' @examples
#' calculate_building_footprint(
#'   lot_area = 10000,
#'   required_open_space = 2000,
#'   parking_area = 3600,
#'   net_developable_area = 8000
#' )
#' # Returns: 10000 - 2000 - 3600 = 4400
#'
#' @export
calculate_building_footprint <- function(lot_area,
                                         required_open_space,
                                         parking_area,
                                         net_developable_area) {

  # Validate inputs
  stopifnot(
    length(lot_area) == length(required_open_space),
    length(lot_area) == length(parking_area),
    length(lot_area) == length(net_developable_area)
  )

  # Calculate footprint only for developable parcels
  building_footprint <- data.table::fcase(
    is.na(net_developable_area) | net_developable_area == 0, 0,
    default = lot_area - required_open_space - parking_area
  )

  return(building_footprint)
}

#' Calculate Building Floor Area
#'
#' Computes the total building floor area (building envelope) by multiplying
#' footprint by height. This is Excel column W.
#'
#' @param building_footprint Numeric vector of building footprint areas (from
#'   \code{\link{calculate_building_footprint}})
#' @param building_height Numeric value of maximum building height in stories
#'   from zoning parameters
#'
#' @return Numeric vector of building floor areas in square feet. Returns 0
#'   for parcels with zero or negative footprint.
#'
#' @details
#' Formula: building_floor_area = building_footprint * building_height
#'
#' Building height must be specified in stories (not feet). Divide feet by 10
#' to convert to stories per MBTA model conventions.
#'
#' Only applies to parcels with positive building footprint.
#'
#' @examples
#' # 7-story building
#' calculate_building_floor_area(
#'   building_footprint = 4400,
#'   building_height = 7
#' )
#' # Returns: 4400 * 7 = 30800
#'
#' # No footprint available
#' calculate_building_floor_area(
#'   building_footprint = 0,
#'   building_height = 7
#' )
#' # Returns: 0
#'
#' @export
calculate_building_floor_area <- function(building_footprint, building_height) {

  # Validate inputs
  stopifnot(
    length(building_height) == 1
  )

  # Calculate floor area only for positive footprints
  building_floor_area <- data.table::fcase(
    is.na(building_footprint) | building_footprint <= 0, 0,
    default = building_footprint * building_height
  )

  return(building_floor_area)
}