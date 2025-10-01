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
  
  # Input validation
  if (!is.numeric(lot_area) || !is.numeric(excluded_area) || !is.numeric(min_lot_size)) {
    stop("All inputs must be numeric")
  }
  
  if (length(min_lot_size) != 1) {
    stop("min_lot_size must be a single numeric value")
  }
  
  if (length(lot_area) != length(excluded_area)) {
    stop("lot_area and excluded_area must have the same length")
  }
  
  if (any(lot_area < 0, na.rm = TRUE) || any(excluded_area < 0, na.rm = TRUE)) {
    stop("Area values cannot be negative")
  }
  
  if (min_lot_size < 0) {
    stop("min_lot_size cannot be negative")
  }
  
  # Calculate developable area, preserving NA values
  developable_area <- ifelse(
    is.na(lot_area) | is.na(excluded_area),
    NA_real_,
    ifelse(
      lot_area < min_lot_size,
      0,
      ifelse((lot_area - excluded_area) < 0, 0, lot_area - excluded_area)
    )
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
#' - < 2.5 units → 0 (below threshold)  
#' - 2.5 to 3.0 units → 3 (minimum viable)
#' - > 3.0 units → rounded to nearest integer
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
  
  # Input validation
  all_inputs <- list(units_building_capacity, units_density_limits, units_lot_coverage,
                    units_lot_area_req, units_far_limits, units_max_cap, units_graduated_lots)
  
  # Check if inputs are numeric or single NA values using purrr
  is_valid <- purrr::map_lgl(all_inputs, ~ is.numeric(.x) || (length(.x) == 1 && is.na(.x)))
  if (!all(is_valid)) {
    stop("All unit capacity inputs must be numeric vectors")
  }
  
  lengths <- purrr::map_int(all_inputs, length)
  if (!all(lengths == lengths[1])) {
    stop("All input vectors must have the same length")
  }
  
  # Create data frame for minimum calculation
  unit_methods <- data.frame(
    building_capacity = units_building_capacity,
    density_limits = units_density_limits,
    lot_coverage = units_lot_coverage,
    lot_area_req = units_lot_area_req,
    far_limits = units_far_limits,
    max_cap = units_max_cap,
    graduated_lots = units_graduated_lots
  )
  
  # Calculate minimum across all methods
  min_values <- apply(unit_methods, 1, function(row) {
    # If all values are NA, return NA
    if (all(is.na(row))) {
      return(NA_real_)
    }
    # Otherwise, treat NA as Inf and find minimum
    row[is.na(row)] <- Inf
    min(row)
  })
  
  # Apply final rounding rules per MBTA Communities model
  final_capacity <- data.table::fcase(
    is.na(min_values), NA_real_,           # Preserve missing data
    min_values < 2.5, 0,                   # Below minimum threshold
    min_values >= 2.5 & min_values < 3, 3, # Minimum viable units
    default = round(min_values)             # Round to nearest integer
  )
  
  return(final_capacity)
}