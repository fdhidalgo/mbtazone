# Build Zoning Parameters Dataset for Package
#
# This script extracts zoning parameters from all available Excel compliance
# models and creates a structured dataset for inclusion in the mbtazone package.
#
# Output: data/zoning_parameters.rda (for lazy-loading in package)

library(data.table)
library(readxl)
library(purrr)
library(cli)

# Load package functions
if ("devtools" %in% loadedNamespaces() || requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else {
  library(mbtazone)
}

# ==============================================================================
# CONFIGURATION
# ==============================================================================

EXCEL_MODELS_DIR <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_models"

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Check if a district sheet exists and has valid parameters
#'
#' @param excel_path Path to Excel file
#' @param district District number (1-5)
#' @return TRUE if district has extractable parameters, FALSE otherwise
has_valid_district <- function(excel_path, district) {
  tryCatch({
    sheets <- excel_sheets(excel_path)

    # Check if Checklist Parameters sheet exists
    if (!"Checklist Parameters" %in% sheets) {
      return(FALSE)
    }

    # Try to extract parameters
    params <- extract_zoning_parameters(excel_path, district)

    # Check if at least min_lot_size was extracted
    !is.na(params$min_lot_size)

  }, error = function(e) {
    FALSE
  })
}

#' Extract zoning parameters with error handling and metadata
#'
#' @param excel_path Path to Excel file
#' @param municipality Municipality name
#' @param district District number
#' @return data.table row with parameters and metadata, or NULL if extraction fails
extract_parameters_safe <- function(excel_path, municipality, district) {
  tryCatch({
    params <- extract_zoning_parameters(excel_path, district)

    # Convert list to data.table row
    dt <- as.data.table(params)

    # Add metadata columns at the front
    dt[, `:=`(
      municipality = municipality,
      district = district,
      excel_file = basename(excel_path),
      extraction_date = as.character(Sys.Date())
    )]

    # Reorder columns (metadata first)
    setcolorder(dt, c(
      "municipality", "district", "excel_file", "extraction_date",
      "min_lot_size", "base_min_lot_size", "additional_lot_SF",
      "building_height", "FAR", "max_lot_coverage",
      "min_required_open_space", "parking_spaces_per_dwelling_unit",
      "lot_area_per_dwelling_unit", "max_dwelling_units_per_acre",
      "max_units_per_lot", "water_included"
    ))

    return(dt)

  }, error = function(e) {
    cli_alert_warning("Failed to extract from {municipality} District {district}: {e$message}")
    return(NULL)
  })
}

# ==============================================================================
# MAIN EXTRACTION FUNCTION
# ==============================================================================

#' Build zoning parameters dataset from all Excel models
build_zoning_parameters <- function() {
  cli_h1("Building Zoning Parameters Dataset")

  # Discover all Excel files
  excel_files <- list.files(
    EXCEL_MODELS_DIR,
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )

  cli_alert_info("Found {length(excel_files)} Excel files")

  # Extract municipality names from directory structure
  municipalities <- dirname(excel_files) |> basename()

  # Create lookup table
  excel_lookup <- data.table(
    municipality = municipalities,
    excel_path = excel_files
  )

  # For municipalities with multiple Excel files, prefer ones with "District 1" sheet
  excel_lookup[, has_district1 := vapply(excel_path, function(path) {
    tryCatch({
      sheets <- excel_sheets(path)
      "District 1" %in% sheets
    }, error = function(e) FALSE)
  }, logical(1))]

  # Sort by has_district1 (TRUE first), then keep first occurrence per municipality
  setorder(excel_lookup, municipality, -has_district1)
  excel_lookup <- excel_lookup[!duplicated(municipality)]
  excel_lookup[, has_district1 := NULL]

  cli_alert_success("Identified {nrow(excel_lookup)} unique municipalities")

  # Extract parameters for all districts in all municipalities
  all_params <- list()

  cli_h2("Extracting parameters")

  for (i in seq_len(nrow(excel_lookup))) {
    municipality <- excel_lookup$municipality[i]
    excel_path <- excel_lookup$excel_path[i]

    cli_alert_info("[{i}/{nrow(excel_lookup)}] Processing {municipality}")

    # Try each district (1-5)
    for (district in 1:5) {
      if (has_valid_district(excel_path, district)) {
        params_row <- extract_parameters_safe(excel_path, municipality, district)

        if (!is.null(params_row)) {
          all_params[[length(all_params) + 1]] <- params_row
          cli_alert_success("  District {district}: Extracted")
        }
      }
    }
  }

  # Combine all results
  zoning_parameters <- rbindlist(all_params, fill = TRUE)

  # Sort by municipality, then district
  setorder(zoning_parameters, municipality, district)

  # Print summary
  cli_h2("Extraction Summary")
  cli_alert_success("Extracted {nrow(zoning_parameters)} district parameter sets")
  cli_alert_info("From {uniqueN(zoning_parameters$municipality)} municipalities")
  cli_alert_info("Average {round(nrow(zoning_parameters) / uniqueN(zoning_parameters$municipality), 1)} districts per municipality")

  return(zoning_parameters)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Build the dataset
zoning_parameters <- build_zoning_parameters()

# Preview
cli_h2("Dataset Preview")
print(zoning_parameters[1:min(10, nrow(zoning_parameters))])

# Save to package data/ directory
cli_h2("Saving to package data")

# Ensure data directory exists
if (!dir.exists("data")) {
  dir.create("data")
  cli_alert_success("Created data/ directory")
}

# Save using usethis if available, otherwise use save()
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(zoning_parameters, overwrite = TRUE)
  cli_alert_success("Saved using usethis::use_data()")
} else {
  save(zoning_parameters, file = "data/zoning_parameters.rda", compress = "xz")
  cli_alert_success("Saved to data/zoning_parameters.rda")
  cli_alert_info("Install 'usethis' package for better data management")
}

# Print file size
file_info <- file.info("data/zoning_parameters.rda")
cli_alert_info("File size: {round(file_info$size / 1024, 1)} KB")

cli_h2("Next Steps")
cli_alert("1. Document dataset in R/data.R")
cli_alert("2. Run devtools::document() to generate help files")
cli_alert("3. Test with: data(zoning_parameters)")