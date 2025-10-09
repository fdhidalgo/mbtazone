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

#' Get District Sheet Names from Excel File (Robust)
#'
#' Handles multiple naming conventions:
#' - Standard: "District 1", "District 2"
#' - With suffixes: "District 1 MFOH", "District 2 MFOM Excl. HCID"
#' - Non-standard: "B5", custom district names (assigned to missing district numbers)
#'
#' @param excel_path Path to Excel compliance model file
#' @return data.frame with columns: sheet_name, district_num, is_standard
get_district_sheets <- function(excel_path) {
  all_sheets <- readxl::excel_sheets(excel_path)

  # Known non-district sheets to exclude
  exclude_patterns <- c(
    "^Introduction$",
    "^Checklist",
    "^Zoning Input",
    "^Summary$",
    "^Formula Matrix$",
    "^Community Info$"
  )

  # Filter out known non-district sheets
  candidate_sheets <- all_sheets
  for (pattern in exclude_patterns) {
    candidate_sheets <- candidate_sheets[!grepl(pattern, candidate_sheets, ignore.case = TRUE)]
  }

  # Separate standard and non-standard sheets
  standard_sheets <- list()
  nonstandard_sheets <- character()

  for (sheet in candidate_sheets) {
    # Check if it starts with "District" followed by a number
    if (grepl("^District\\s+\\d+", sheet)) {
      # Extract district number
      dist_num <- as.integer(sub("^District\\s+(\\d+).*", "\\1", sheet))
      is_std <- grepl("^District\\s+\\d+$", sheet)

      standard_sheets[[length(standard_sheets) + 1]] <- list(
        sheet_name = sheet,
        district_num = dist_num,
        is_standard = is_std
      )
    } else {
      # Non-standard naming
      nonstandard_sheets <- c(nonstandard_sheets, sheet)
    }
  }

  # Convert standard sheets to data frame
  if (length(standard_sheets) > 0) {
    standard_df <- do.call(rbind, lapply(standard_sheets, function(x) {
      data.frame(
        sheet_name = x$sheet_name,
        district_num = x$district_num,
        is_standard = x$is_standard,
        stringsAsFactors = FALSE
      )
    }))
  } else {
    standard_df <- data.frame(
      sheet_name = character(),
      district_num = integer(),
      is_standard = logical(),
      stringsAsFactors = FALSE
    )
  }

  # For non-standard sheets, assign to missing district numbers
  if (length(nonstandard_sheets) > 0) {
    # Find which district numbers are used
    used_nums <- standard_df$district_num
    max_dist <- if (length(used_nums) > 0) max(used_nums) else 0

    # Find missing district numbers from 1 to max
    all_nums <- 1:max(max_dist, length(nonstandard_sheets))
    missing_nums <- setdiff(all_nums, used_nums)

    # If we don't have enough missing numbers, extend the range
    if (length(missing_nums) < length(nonstandard_sheets)) {
      missing_nums <- c(
        missing_nums,
        (max_dist + 1):(max_dist + length(nonstandard_sheets) - length(missing_nums))
      )
    }

    # Assign non-standard sheets to missing numbers (in order of appearance)
    nonstandard_df <- data.frame(
      sheet_name = nonstandard_sheets,
      district_num = missing_nums[1:length(nonstandard_sheets)],
      is_standard = FALSE,
      stringsAsFactors = FALSE
    )

    # Combine
    results <- rbind(standard_df, nonstandard_df)
  } else {
    results <- standard_df
  }

  # Sort by district number
  results <- results[order(results$district_num), ]
  rownames(results) <- NULL

  return(results)
}

#' Find District Sheet Name by Number
#'
#' Looks up the sheet name for a given district number, handling both
#' standard and non-standard naming conventions.
#'
#' @param excel_path Path to Excel compliance model file
#' @param district District number
#' @return Sheet name or NULL if not found
find_district_sheet <- function(excel_path, district) {
  district_info <- get_district_sheets(excel_path)

  match_idx <- which(district_info$district_num == district)

  if (length(match_idx) > 0) {
    return(district_info$sheet_name[match_idx[1]])
  } else {
    return(NULL)
  }
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
#' R package calculations. Automatically converts numeric columns and handles
#' Excel special values like "N/A" and "<no limit>".
#'
#' @param district_data data.frame from read_district_data()
#' @return list with named calculation vectors
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

  # Columns that should always be numeric (not character or logical)
  numeric_cols <- c(
    "parcel_sf", "excluded_public", "excluded_nonpublic", "total_excluded_land",
    "total_sensitive_land", "developable_parcel_sf", "override_developable_sf",
    "developable_sf_for_unit_calc", "excluded_land_pct", "open_space_pct",
    "open_space_removed", "parking_area_removed", "building_footprint",
    "building_envelope", "modeled_unit_capacity", "dwelling_units_per_acre_limit",
    "max_lot_coverage_limit", "lot_area_per_dwelling_unit_limit", "far_limit",
    "max_units_per_lot_limit", "max_units_graduated_lots", "final_unit_capacity",
    "du_per_ac"
  )

  # Columns that should be logical
  logical_cols <- c("non_conforming_lot")

  result <- list()

  for (r_name in names(col_map)) {
    excel_name <- col_map[[r_name]]

    if (excel_name %in% names(district_data)) {
      value <- district_data[[excel_name]]

      # Convert to appropriate type
      if (r_name %in% numeric_cols) {
        # Convert Excel text strings to numeric NA
        if (is.character(value)) {
          value[value %in% c("N/A", "<no limit>", "")] <- NA
          value <- suppressWarnings(as.numeric(value))
        } else if (is.logical(value)) {
          # All NA logical becomes NA numeric
          value <- as.numeric(value)
        }
      } else if (r_name %in% logical_cols) {
        # Convert to logical
        if (is.character(value)) {
          value <- as.logical(value)
        }
      }

      result[[r_name]] <- value
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

  # Find district sheet (handles non-standard naming)
  sheet_name <- find_district_sheet(excel_path, district)

  # Check if sheet exists
  if (is.null(sheet_name)) {
    available_districts <- get_district_sheets(excel_path)
    stop(paste0(
      "District ", district, " not found in ", basename(excel_path),
      ". Available districts: ", paste(available_districts$district_num, collapse = ", ")
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
