#' Zoning Parameter Extraction and Management
#'
#' Functions to extract and manage district-specific zoning parameters from
#' Excel compliance models or manual specification.
#'
#' @name zoning_parameters
NULL

#' Extract Zoning Parameters from Excel Compliance Model
#'
#' Reads the "Checklist Parameters" sheet from an Excel compliance model
#' and extracts district-specific zoning parameters needed for unit capacity
#' calculations.
#'
#' @param excel_path Character path to Excel compliance model file
#' @param district Integer district number (1-5)
#'
#' @return Named list with the following zoning parameters:
#'   \describe{
#'     \item{min_lot_size}{Minimum allowable lot size in square feet}
#'     \item{base_min_lot_size}{Base minimum lot size for graduated sizing}
#'     \item{additional_lot_SF}{Additional square footage required per dwelling unit}
#'     \item{building_height}{Maximum building height in stories}
#'     \item{FAR}{Floor Area Ratio limit (as decimal)}
#'     \item{max_lot_coverage}{Maximum lot coverage (as decimal)}
#'     \item{min_required_open_space}{Minimum open space percentage (as decimal)}
#'     \item{parking_spaces_per_dwelling_unit}{Parking spaces required per unit}
#'     \item{lot_area_per_dwelling_unit}{Lot area required per dwelling unit}
#'     \item{max_dwelling_units_per_acre}{Maximum density limit}
#'     \item{max_units_per_lot}{Maximum units allowed per lot}
#'     \item{water_included}{Whether water bodies count toward open space ("Y" or "N")}
#'   }
#'
#' @details
#' The function reads from specific rows in the Excel "Checklist Parameters" sheet:
#' - Row 16: Maximum units per lot
#' - Row 22: Minimum lot size
#' - Row 24: Base minimum lot size (for graduated lots)
#' - Row 25: Additional lot square footage per unit
#' - Row 35: Building height (in stories)
#' - Row 43: Floor Area Ratio (FAR)
#' - Row 58: Maximum lot coverage
#' - Row 60: Minimum required open space
#' - Row 63: Water included in open space (Y/N)
#' - Row 86: Parking spaces per dwelling unit
#' - Row 101: Lot area per dwelling unit
#' - Row 102: Maximum dwelling units per acre
#'
#' District columns: 1 = column 5, 2 = column 8, 3 = column 11, 4 = column 14, 5 = column 17
#'
#' Excel "N/A" values are converted to NA_real_ or NA_character_ as appropriate.
#'
#' @examples
#' \dontrun{
#' # Extract parameters for Chelsea District 1
#' params <- extract_zoning_parameters(
#'   "Chelsea - Compliance Model 20231118.xlsx",
#'   district = 1
#' )
#'
#' # Use in calculations
#' developable <- calculate_developable_area(
#'   lot_area = parcel_data$area,
#'   excluded_area = parcel_data$excluded,
#'   min_lot_size = params$min_lot_size
#' )
#' }
#'
#' @export
extract_zoning_parameters <- function(excel_path, district = 1) {

  # Input validation
  if (!is.numeric(district) || district < 1 || district > 5) {
    stop("district must be an integer between 1 and 5")
  }

  # Determine column index for district (1=col 5, 2=col 8, 3=col 11, etc.)
  col_index <- 2 + district * 3

  # Read Checklist Parameters sheet without column names
  params_raw <- readxl::read_excel(
    excel_path,
    sheet = "Checklist Parameters",
    col_names = FALSE
  )

  # Helper function to extract and clean parameter values
  extract_param <- function(row_num, col_num, type = "numeric") {
    value <- params_raw[[col_num]][row_num]

    # Handle various "not applicable" representations
    if (is.na(value) ||
        (is.character(value) && toupper(value) %in% c("N/A", "NA", ""))) {
      return(if (type == "numeric") NA_real_ else NA_character_)
    }

    # Convert to appropriate type
    if (type == "numeric") {
      as.numeric(value)
    } else {
      as.character(value)
    }
  }

  # Extract all parameters from their respective rows
  params <- list(
    min_lot_size = extract_param(22, col_index, "numeric"),
    base_min_lot_size = extract_param(24, col_index, "numeric"),
    additional_lot_SF = extract_param(25, col_index, "numeric"),
    building_height = extract_param(35, col_index, "numeric"),
    FAR = extract_param(43, col_index, "numeric"),
    max_lot_coverage = extract_param(58, col_index, "numeric"),
    min_required_open_space = extract_param(60, col_index, "numeric"),
    parking_spaces_per_dwelling_unit = extract_param(86, col_index, "numeric"),
    lot_area_per_dwelling_unit = extract_param(101, col_index, "numeric"),
    max_dwelling_units_per_acre = extract_param(102, col_index, "numeric"),
    max_units_per_lot = extract_param(16, col_index, "numeric"),
    water_included = extract_param(63, col_index, "character")
  )

  # Validate that at least min_lot_size was extracted
  if (is.na(params$min_lot_size)) {
    warning(
      "Could not extract min_lot_size for district ", district,
      ". Check that the Excel file has the expected structure."
    )
  }

  # Add metadata
  attr(params, "source") <- "excel"
  attr(params, "excel_file") <- basename(excel_path)
  attr(params, "district") <- district
  attr(params, "extracted_date") <- Sys.Date()

  params
}

#' Create Zoning Parameters List Manually
#'
#' Helper function to create a properly structured zoning parameters list
#' for use in calculation functions. Useful when Excel models are not available
#' or for testing with specific parameter values.
#'
#' @param min_lot_size Minimum lot size in square feet (required)
#' @param base_min_lot_size Base minimum lot size for graduated sizing (default: NA)
#' @param additional_lot_SF Additional square footage per dwelling unit (default: NA)
#' @param building_height Maximum building height in stories (default: NA)
#' @param FAR Floor Area Ratio limit as decimal (default: NA)
#' @param max_lot_coverage Maximum lot coverage as decimal (default: NA)
#' @param min_required_open_space Minimum open space as decimal (default: 0.2)
#' @param parking_spaces_per_dwelling_unit Parking spaces per unit (default: NA)
#' @param lot_area_per_dwelling_unit Lot area per dwelling unit (default: NA)
#' @param max_dwelling_units_per_acre Maximum density limit (default: NA)
#' @param max_units_per_lot Maximum units per lot (default: NA)
#' @param water_included Whether water counts toward open space, "Y" or "N" (default: "N")
#'
#' @return Named list of zoning parameters with proper structure
#'
#' @details
#' Only `min_lot_size` is required. All other parameters default to NA, which
#' means they will be treated as unlimited/not applicable in calculations.
#' The default `min_required_open_space = 0.2` (20%) matches the MBTA
#' Communities Act model assumption.
#'
#' @examples
#' # Create parameters for a simple zoning district
#' params <- create_zoning_parameters(
#'   min_lot_size = 5000,
#'   building_height = 7,
#'   min_required_open_space = 0.15
#' )
#'
#' # Use in calculations
#' developable <- calculate_developable_area(
#'   lot_area = c(10000, 8000, 12000),
#'   excluded_area = c(1000, 500, 2000),
#'   min_lot_size = params$min_lot_size
#' )
#'
#' @export
create_zoning_parameters <- function(
    min_lot_size,
    base_min_lot_size = NA_real_,
    additional_lot_SF = NA_real_,
    building_height = NA_real_,
    FAR = NA_real_,
    max_lot_coverage = NA_real_,
    min_required_open_space = 0.2,
    parking_spaces_per_dwelling_unit = NA_real_,
    lot_area_per_dwelling_unit = NA_real_,
    max_dwelling_units_per_acre = NA_real_,
    max_units_per_lot = NA_real_,
    water_included = "N") {

  # Validate required parameter
  stopifnot(
    !missing(min_lot_size),
    !is.na(min_lot_size),
    is.numeric(min_lot_size),
    min_lot_size >= 0,
    water_included %in% c("Y", "N")
  )

  # Create parameter list
  params <- list(
    min_lot_size = min_lot_size,
    base_min_lot_size = base_min_lot_size,
    additional_lot_SF = additional_lot_SF,
    building_height = building_height,
    FAR = FAR,
    max_lot_coverage = max_lot_coverage,
    min_required_open_space = min_required_open_space,
    parking_spaces_per_dwelling_unit = parking_spaces_per_dwelling_unit,
    lot_area_per_dwelling_unit = lot_area_per_dwelling_unit,
    max_dwelling_units_per_acre = max_dwelling_units_per_acre,
    max_units_per_lot = max_units_per_lot,
    water_included = water_included
  )

  # Add metadata
  attr(params, "source") <- "manual"
  attr(params, "created_date") <- Sys.Date()

  params
}
