#' Excel Reference Data Extraction Utilities
#'
#' Functions to extract and process data from MBTA Communities Excel compliance
#' models for regression testing.

#' Get Path to Excel Reference Models Directory
#'
#' Resolves the path to Excel reference models, checking environment variable
#' first, then falling back to default symlink location.
#'
#' @return Character path to Excel models directory
#' @export
get_excel_reference_path <- function() {
  env_path <- Sys.getenv("MBTA_EXCEL_MODELS", unset = "")
  path <- if (env_path != "") env_path else testthat::test_path("excel_reference")

  if (!dir.exists(path)) {
    testthat::skip(sprintf(
      "Excel models not found at: %s\nSet MBTA_EXCEL_MODELS or create symlink",
      path
    ))
  }

  path
}

#' List Available Communities in Excel Reference
#'
#' @return Character vector of community names
#' @export
list_available_communities <- function() {
  excel_path <- get_excel_reference_path()

  dirs <- list.dirs(excel_path, full.names = FALSE, recursive = FALSE)

  # Filter out hidden directories and sort
  dirs <- dirs[!grepl("^\\.", dirs)]
  sort(dirs)
}

#' List Excel Files for a Community
#'
#' @param community Community name (e.g., "Chelsea", "Cambridge")
#' @return Character vector of Excel file paths
#' @export
list_community_excel_files <- function(community) {
  excel_path <- get_excel_reference_path()
  community_path <- file.path(excel_path, community)

  files <- list.files(
    community_path,
    pattern = "\\.xlsx?$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )

  stopifnot(
    "Community not found or no Excel files" = length(files) > 0
  )

  files
}

#' Get District Sheet Names from Excel File
#'
#' @param excel_path Path to Excel compliance model file
#' @return Character vector of district sheet names (e.g., "District 1", "District 2")
#' @export
get_district_sheets <- function(excel_path) {
  all_sheets <- readxl::excel_sheets(excel_path)

  # Filter for District sheets
  district_sheets <- all_sheets[grepl("^District \\d+$", all_sheets)]

  district_sheets
}

#' Read Parcel Data from District Sheet
#'
#' Reads the parcel-level calculation data from a District sheet in an Excel
#' compliance model. Skips the summary header (first 18 rows) and reads the
#' parcel data table.
#'
#' @param excel_path Path to Excel compliance model file
#' @param sheet Sheet name (e.g., "District 1")
#' @return data.frame with parcel data including all calculation columns
#' @export
read_district_data <- function(excel_path, sheet) {
  # Read data starting at row 19 (skip 18 header rows)
  data <- readxl::read_excel(
    excel_path,
    sheet = sheet,
    skip = 18
  )

  # Filter out completely empty rows
  data <- data[!apply(is.na(data) | data == "", 1, all), ]

  # Convert to data.frame for compatibility
  as.data.frame(data)
}

#' Extract Calculation Columns from District Data
#'
#' Extracts the key calculation columns from district data for comparison with
#' R package calculations.
#'
#' @param district_data data.frame from read_district_data()
#' @return list with named calculation vectors
#' @export
extract_calculation_columns <- function(district_data) {
  # Map Excel column names to R-friendly names
  col_map <- list(
    parcel_sf = "Parcel sf",
    excluded_public = "Excluded Public",
    excluded_nonpublic = "Excluded NonPublic",
    total_excluded_land = "Total Excluded Land",
    total_sensitive_land = "Total Sensitive Land",
    developable_parcel_sf = "Developable Parcel sf",
    override_developable_sf = "Override Developable sf",
    override_note = "Override Note",
    developable_sf_for_unit_calc = "Developable SF for Unit Calc",
    excluded_land_pct = "Excluded Land %",
    open_space_pct = "Open Space %",
    open_space_removed = "Open Space Removed",
    parking_area_removed = "Parking Area Removed",
    building_footprint = "Building Footprint",
    building_envelope = "Building Envelope",
    modeled_unit_capacity = "Modeled Unit Capacity",
    dwelling_units_per_acre_limit = "Dwelling Units per Acre Limit",
    max_lot_coverage_limit = "Max Lot Coverage Limit",
    lot_area_per_dwelling_unit_limit = "Lot Area per Dwelling Unit Limit",
    far_limit = "FAR Limit",
    max_units_per_lot_limit = "Max Units per Lot Limit",
    non_conforming_lot = "Non-Conforming Lot?",
    max_units_graduated_lots = "Max units based on addt'l lot size requirements",
    final_unit_capacity = "Final Lot Multi-family Unit Capacity",
    du_per_ac = "DU per AC"
  )

  result <- list()

  for (r_name in names(col_map)) {
    excel_name <- col_map[[r_name]]

    if (excel_name %in% names(district_data)) {
      result[[r_name]] <- district_data[[excel_name]]
    } else {
      warning(paste0("Column not found in Excel data: ", excel_name))
      result[[r_name]] <- NA
    }
  }

  result
}

#' Load Community Reference Data
#'
#' Loads cached reference data for a community and district, extracting from
#' Excel if cache doesn't exist. Includes both calculation columns and zoning
#' parameters.
#'
#' @param community Community name (e.g., "Chelsea")
#' @param district District number (e.g., 1)
#' @param force_refresh Force re-extraction from Excel (default FALSE)
#' @return list with extracted calculation columns and zoning parameters
#' @export
load_community_reference <- function(community, district, force_refresh = FALSE) {
  cache_file <- testthat::test_path(
    "fixtures",
    paste0(tolower(community), "_district", district, ".rds")
  )

  # Return cached data if available and not forcing refresh
  if (!force_refresh && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  # Extract from Excel
  excel_files <- list_community_excel_files(community)

  # Use first Excel file (most communities have only one)
  excel_path <- excel_files[1]

  sheet_name <- paste0("District ", district)

  # Check if sheet exists
  available_sheets <- get_district_sheets(excel_path)
  if (!sheet_name %in% available_sheets) {
    stop(paste0(
      "District ", district, " not found in ", basename(excel_path),
      ". Available districts: ", paste(available_sheets, collapse = ", ")
    ))
  }

  # Read and extract data
  district_data <- read_district_data(excel_path, sheet_name)
  calc_columns <- extract_calculation_columns(district_data)

  # Extract zoning parameters
  zoning_params <- extract_zoning_parameters(excel_path, district)

  # Add metadata
  reference_data <- list(
    community = community,
    district = district,
    excel_file = basename(excel_path),
    extracted_date = Sys.Date(),
    n_parcels = nrow(district_data),
    calculations = calc_columns,
    zoning_parameters = zoning_params
  )

  # Save to cache
  saveRDS(reference_data, cache_file)

  reference_data
}
