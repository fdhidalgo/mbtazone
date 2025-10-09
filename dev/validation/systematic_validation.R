# Systematic Validation of mbtazone Package
# Validates R package outputs against all available Excel compliance models
# Linear issue: HID-83

# Load package (use devtools if in development)
if ("devtools" %in% loadedNamespaces() || requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else if (!("mbtazone" %in% loadedNamespaces())) {
  library(mbtazone)
}

library(sf)
library(data.table)
library(readxl)
library(purrr)
library(cli)

# ==============================================================================
# ROBUST EXCEL SHEET DETECTION
# ==============================================================================

#' Get district sheets from Excel file (robust version)
#'
#' Handles multiple naming conventions:
#' - Standard: "District 1", "District 2"
#' - With suffixes: "District 1 MFOH", "District 2 MFOM Excl. HCID"
#' - Non-standard: "B5", custom district names (assigned to missing district numbers)
#'
#' @param excel_path Path to Excel file
#' @return data.frame with columns: sheet_name, district_num, is_standard
get_district_sheets_robust <- function(excel_path) {
  all_sheets <- excel_sheets(excel_path)

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

#' Find district sheet by number (robust)
#'
#' @param excel_path Path to Excel file
#' @param district District number
#' @return Sheet name or NULL if not found
find_district_sheet_robust <- function(excel_path, district) {
  district_info <- get_district_sheets_robust(excel_path)

  match_idx <- which(district_info$district_num == district)

  if (length(match_idx) > 0) {
    return(district_info$sheet_name[match_idx[1]])
  } else {
    return(NULL)
  }
}

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# Data directories
EXCEL_MODELS_DIR <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_models"
DISTRICT_SHAPEFILES_DIR <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_shapefiles"
PARCEL_SHAPEFILES_DIR_1 <- "inst/extdata/parcels"
PARCEL_SHAPEFILES_DIR_2 <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/land_record_shapefiles/basic"

# Output files
RESULTS_FILE <- "dev/validation_results.rds"
ERRORS_FILE <- "dev/validation_errors.rds"

# ==============================================================================
# FILE DISCOVERY FUNCTIONS
# ==============================================================================

#' Discover all Excel compliance models
#'
#' For municipalities with multiple Excel files, prefer files containing
#' a "District 1" sheet (standard format) over non-standard naming.
#'
#' @return data.table with columns: municipality, excel_path
discover_excel_models <- function() {
  cli_h2("Discovering Excel compliance models")

  # Find all .xlsx files
  excel_files <- list.files(
    EXCEL_MODELS_DIR,
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE
  )

  cli_alert_info("Found {length(excel_files)} Excel files")

  # Extract municipality names from directory structure
  municipalities <- dirname(excel_files) |>
    basename()

  # Create lookup table
  dt <- data.table(
    municipality = municipalities,
    excel_path = excel_files
  )

  # For municipalities with multiple Excel files, prefer ones with "District 1" sheet
  dt[, has_district1 := vapply(excel_path, function(path) {
    tryCatch({
      sheets <- excel_sheets(path)
      "District 1" %in% sheets
    }, error = function(e) FALSE)
  }, logical(1))]

  # Sort by has_district1 (TRUE first), then keep first occurrence per municipality
  setorder(dt, municipality, -has_district1)
  dt <- dt[!duplicated(municipality)]
  dt[, has_district1 := NULL]

  cli_alert_success("Identified {nrow(dt)} unique municipalities")

  return(dt)
}


#' Discover district boundary shapefiles
#'
#' @return data.table with columns: municipality, shapefile_path
discover_district_shapefiles <- function() {
  cli_h2("Discovering district boundary shapefiles")

  # Find all .shp files
  shp_files <- list.files(
    DISTRICT_SHAPEFILES_DIR,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE
  )

  cli_alert_info("Found {length(shp_files)} shapefile candidates")

  # Extract municipality names (from directory or file name)
  municipalities <- basename(dirname(shp_files))

  # For files directly in subdirectories, use subdirectory name
  municipalities <- ifelse(
    municipalities == basename(DISTRICT_SHAPEFILES_DIR),
    tools::file_path_sans_ext(basename(shp_files)),
    municipalities
  )

  # Create lookup table
  dt <- data.table(
    municipality = municipalities,
    shapefile_path = shp_files
  )

  cli_alert_success("Identified {nrow(dt)} district shapefiles")

  return(dt)
}


#' Match shapefile to Excel model by municipality name
#'
#' @param municipality_name Name from Excel model directory
#' @param shapefile_dt data.table from discover_district_shapefiles()
#' @return Matched shapefile path or NA
match_shapefile <- function(municipality_name, shapefile_dt) {
  # Normalize names for matching
  normalize <- function(x) {
    tolower(gsub("[^a-z0-9]", "", x))
  }

  norm_muni <- normalize(municipality_name)
  norm_shapes <- normalize(shapefile_dt$municipality)

  # Exact match
  match_idx <- which(norm_shapes == norm_muni)

  if (length(match_idx) > 0) {
    return(shapefile_dt$shapefile_path[match_idx[1]])
  }

  # Partial match (municipality name contained in shapefile name)
  match_idx <- grep(norm_muni, norm_shapes, fixed = TRUE)

  if (length(match_idx) > 0) {
    return(shapefile_dt$shapefile_path[match_idx[1]])
  }

  # No match
  return(NA_character_)
}


#' Find parcel shapefile for municipality
#'
#' @param municipality_name Municipality name
#' @return Parcel shapefile path or NA
find_parcel_shapefile <- function(municipality_name) {
  # Search in both parcel directories
  parcel_dirs <- c(PARCEL_SHAPEFILES_DIR_1, PARCEL_SHAPEFILES_DIR_2)

  # Normalize municipality name
  norm_muni <- tolower(gsub("[^a-z0-9]", "", municipality_name))

  # Search each directory
  for (dir in parcel_dirs) {
    if (!dir.exists(dir)) next

    parcel_files <- list.files(
      dir,
      pattern = "\\.zip$",
      full.names = TRUE
    )

    # Check each file
    for (pf in parcel_files) {
      if (grepl(norm_muni, tolower(pf), fixed = TRUE)) {
        return(pf)
      }
    }
  }

  return(NA_character_)
}


# ==============================================================================
# VALIDATION HELPER FUNCTIONS
# ==============================================================================

#' Validate shapefile format
#'
#' @param shapefile_path Path to shapefile
#' @return List with valid (TRUE/FALSE) and error message
validate_shapefile_format <- function(shapefile_path) {
  tryCatch({
    # Try to read shapefile
    shp <- sf::st_read(shapefile_path, quiet = TRUE)

    # Check CRS
    crs_code <- sf::st_crs(shp)$epsg
    if (is.na(crs_code)) {
      return(list(valid = FALSE, error = "Missing CRS"))
    }

    if (crs_code != 26986) {
      # Try to transform
      shp <- sf::st_transform(shp, 26986)
      cli_alert_warning("Transformed CRS from {crs_code} to 26986")
    }

    # Check geometries
    validity <- sf::st_is_valid(shp)
    if (any(is.na(validity)) || !all(validity)) {
      n_invalid <- sum(!validity, na.rm = TRUE)
      cli_alert_warning("Found {n_invalid} invalid geometries, attempting repair")
      shp <- sf::st_make_valid(shp)
    }

    return(list(valid = TRUE, error = NA_character_, data = shp))

  }, error = function(e) {
    return(list(valid = FALSE, error = as.character(e$message)))
  })
}


#' Extract Excel summary statistics
#'
#' @param excel_path Path to Excel file
#' @param district District number (default NULL to auto-detect first district with data)
#' @return List with total_units, total_acres, gross_density, compliant
extract_excel_summary <- function(excel_path, district = NULL) {
  tryCatch({
    # If district not specified, find first district with data
    if (is.null(district)) {
      district_info <- find_first_district_with_data(excel_path)

      if (is.null(district_info)) {
        return(list(error = "No District sheet with data found"))
      }

      sheet_name <- district_info$sheet_name
    } else {
      # Find specific district sheet (handles non-standard naming)
      sheet_name <- find_district_sheet_robust(excel_path, district)

      if (is.null(sheet_name)) {
        return(list(error = "No District sheet found"))
      }
    }

    # Read from District sheet header (first 18 rows)
    # Note: We prefer this over Summary sheet because Summary may aggregate differently
    data <- read_excel(excel_path, sheet = sheet_name, n_max = 18, col_names = FALSE)

    # Look for summary metrics in header
    units_idx <- which(grepl("multi.*family.*unit.*capacity|total.*unit|unit.*capacity|final.*capacity", data[[1]], ignore.case = TRUE))
    total_units <- if (length(units_idx) > 0) {
      as.numeric(data[units_idx[1], 2])
    } else {
      NA_real_
    }

    # Prefer "Parcel Acreage" over "District Acreage Denominator"
    acres_idx <- which(grepl("^Parcel Acreage$|total.*acre|parcel.*acre", data[[1]], ignore.case = TRUE))
    if (length(acres_idx) == 0) {
      # Fallback to denominator if Parcel Acreage not found
      acres_idx <- which(grepl("acreage.*denominator|district.*acre", data[[1]], ignore.case = TRUE))
    }
    total_acres <- if (length(acres_idx) > 0) {
      as.numeric(data[acres_idx[1], 2])
    } else {
      NA_real_
    }

    density_idx <- which(grepl("^DU/AC$|gross.*density|density.*gross", data[[1]], ignore.case = TRUE))
    gross_density <- if (length(density_idx) > 0) {
      as.numeric(data[density_idx[1], 2])
    } else {
      NA_real_
    }

    # Compliance status - try to infer or leave as NA
    compliant <- NA

    return(list(
      total_units = total_units,
      total_acres = total_acres,
      gross_density = gross_density,
      compliant = compliant,
      error = NA_character_
    ))

  }, error = function(e) {
    return(list(error = as.character(e$message)))
  })
}


#' Find first district sheet with parcel data
#'
#' Checks all district sheets (District 1-5) and returns the first one
#' that contains actual parcel ID data. Useful for municipalities that
#' have empty District 1 sheets but data in other district sheets.
#'
#' @param excel_path Path to Excel file
#' @return List with district_num and sheet_name, or NULL if no data found
find_first_district_with_data <- function(excel_path) {
  district_info <- get_district_sheets_robust(excel_path)

  if (nrow(district_info) == 0) {
    return(NULL)
  }

  # Check each district sheet for parcel data
  for (i in seq_len(nrow(district_info))) {
    sheet_name <- district_info$sheet_name[i]
    district_num <- district_info$district_num[i]

    tryCatch({
      # Read parcel data
      data <- read_excel(excel_path, sheet = sheet_name, skip = 18)
      data <- data[!apply(is.na(data) | data == "", 1, all), ]

      # Look for LOC_ID column
      if ("LOC_ID" %in% names(data)) {
        n_ids <- sum(!is.na(data$LOC_ID) & data$LOC_ID != "")

        if (n_ids > 0) {
          return(list(
            district_num = district_num,
            sheet_name = sheet_name,
            n_parcels = n_ids
          ))
        }
      }
    }, error = function(e) {
      # Skip this sheet and continue
    })
  }

  return(NULL)
}


#' Extract parcel IDs from Excel
#'
#' @param excel_path Path to Excel file
#' @param district District number (default 1, or NULL to auto-detect first district with data)
#' @return Character vector of parcel IDs, or NULL if extraction fails
extract_excel_parcel_ids <- function(excel_path, district = NULL) {
  tryCatch({
    # If district not specified, find first district with data
    if (is.null(district)) {
      district_info <- find_first_district_with_data(excel_path)

      if (is.null(district_info)) {
        return(NULL)
      }

      sheet_name <- district_info$sheet_name
    } else {
      # Find specific district sheet (handles non-standard naming)
      sheet_name <- find_district_sheet_robust(excel_path, district)

      if (is.null(sheet_name)) {
        return(NULL)
      }
    }

    # Read parcel data (skip 18 header rows)
    data <- read_excel(
      excel_path,
      sheet = sheet_name,
      skip = 18
    )

    # Filter out empty rows
    data <- data[!apply(is.na(data) | data == "", 1, all), ]

    if (nrow(data) == 0) {
      return(NULL)
    }

    # Look for LOC_ID or similar column
    id_col <- names(data)[grepl("LOC_ID|OBJECTID|PARCEL.*ID|^ID$", names(data), ignore.case = TRUE)][1]

    if (is.na(id_col)) {
      return(NULL)
    }

    # Return unique IDs
    ids <- unique(data[[id_col]])
    ids <- ids[!is.na(ids) & ids != ""]

    return(as.character(ids))

  }, error = function(e) {
    return(NULL)
  })
}


# ==============================================================================
# METHOD 1: EXCEL PARCEL LIST VALIDATION
# ==============================================================================

#' Validate using Excel parcel list
#'
#' @param municipality_name Municipality name
#' @param excel_path Path to Excel compliance model
#' @param parcel_shapefile_path Path to parcel shapefile
#' @return List with validation results and errors
validate_method1_excel_parcels <- function(municipality_name, excel_path, parcel_shapefile_path) {
  cli_h3("Method 1: Excel Parcel List - {municipality_name}")

  result <- list(
    municipality = municipality_name,
    method = "excel_parcels",
    success = FALSE,
    error = NA_character_
  )

  # Check if parcel shapefile exists
  if (is.na(parcel_shapefile_path)) {
    result$error <- "No parcel shapefile available"
    cli_alert_danger("Skipped: No parcel shapefile")
    return(result)
  }

  # Extract Excel summary
  excel_summary <- extract_excel_summary(excel_path)
  if (!is.null(excel_summary$error) && !is.na(excel_summary$error)) {
    result$error <- paste0("Excel parsing: ", excel_summary$error)
    cli_alert_danger("Failed: {result$error}")
    return(result)
  }

  # Auto-detect which district has parcel data
  district_info <- find_first_district_with_data(excel_path)
  if (is.null(district_info)) {
    result$error <- "No District sheet with parcel data found"
    cli_alert_danger("Failed: {result$error}")
    return(result)
  }

  district_num <- district_info$district_num

  # Extract parcel IDs from Excel
  parcel_ids <- extract_excel_parcel_ids(excel_path)
  if (is.null(parcel_ids)) {
    result$error <- "Could not extract parcel IDs from Excel"
    cli_alert_danger("Failed: {result$error}")
    return(result)
  }

  cli_alert_info("Found {length(parcel_ids)} parcels in Excel model (District {district_num})")

  # Load and filter municipality parcels
  tryCatch({
    parcels <- load_municipality(parcel_shapefile_path, municipality_name)

    # Filter to Excel parcels
    parcels_filtered <- parcels[parcels$LOC_ID %in% parcel_ids, ]

    if (nrow(parcels_filtered) == 0) {
      result$error <- "No matching parcels found in shapefile"
      cli_alert_danger("Failed: {result$error}")
      return(result)
    }

    cli_alert_info("Matched {nrow(parcels_filtered)} parcels from shapefile")

    # Extract zoning parameters from the same district that has parcel data
    zoning_params <- extract_zoning_parameters(excel_path, district = district_num)

    # Calculate district capacity
    capacity <- calculate_district_capacity(
      parcels = parcels_filtered,
      zoning_params = zoning_params
    )

    # Aggregate parcel-level results to district summary
    r_total_units <- sum(capacity$final_unit_capacity, na.rm = TRUE)
    r_total_acres <- sum(capacity$ACRES, na.rm = TRUE)
    r_gross_density <- r_total_units / r_total_acres

    # Compare results
    result$success <- TRUE
    result$excel_units <- excel_summary$total_units
    result$excel_acres <- excel_summary$total_acres
    result$excel_density <- excel_summary$gross_density
    result$excel_compliant <- excel_summary$compliant
    result$r_units <- r_total_units
    result$r_acres <- r_total_acres
    result$r_density <- r_gross_density
    result$unit_diff <- result$r_units - result$excel_units
    result$unit_pct_diff <- (result$unit_diff / result$excel_units) * 100

    cli_alert_success("Validation complete: {round(result$r_units)} vs {round(result$excel_units)} units ({round(result$unit_pct_diff, 1)}% diff)")

    return(result)

  }, error = function(e) {
    result$error <- paste0("R package error: ", e$message)
    cli_alert_danger("Failed: {result$error}")
    return(result)
  })
}


# ==============================================================================
# METHOD 2: SHAPEFILE BOUNDARY VALIDATION
# ==============================================================================

#' Validate using shapefile boundary
#'
#' @param municipality_name Municipality name
#' @param excel_path Path to Excel compliance model
#' @param parcel_shapefile_path Path to parcel shapefile
#' @param district_shapefile_path Path to district boundary shapefile
#' @return List with validation results and errors
validate_method2_shapefile <- function(municipality_name, excel_path,
                                       parcel_shapefile_path, district_shapefile_path) {
  cli_h3("Method 2: Shapefile Boundary - {municipality_name}")

  result <- list(
    municipality = municipality_name,
    method = "shapefile_boundary",
    success = FALSE,
    error = NA_character_
  )

  # Check if shapefiles exist
  if (is.na(parcel_shapefile_path)) {
    result$error <- "No parcel shapefile available"
    cli_alert_danger("Skipped: No parcel shapefile")
    return(result)
  }

  if (is.na(district_shapefile_path)) {
    result$error <- "No district shapefile available"
    cli_alert_danger("Skipped: No district shapefile")
    return(result)
  }

  # Validate district shapefile format
  shp_validation <- validate_shapefile_format(district_shapefile_path)
  if (!shp_validation$valid) {
    result$error <- paste0("Shapefile validation: ", shp_validation$error)
    cli_alert_danger("Failed: {result$error}")
    return(result)
  }

  # Auto-detect which district has parcel data
  district_info <- find_first_district_with_data(excel_path)
  if (is.null(district_info)) {
    result$error <- "No District sheet with parcel data found"
    cli_alert_danger("Failed: {result$error}")
    return(result)
  }

  district_num <- district_info$district_num

  # Extract Excel summary
  excel_summary <- extract_excel_summary(excel_path)
  if (!is.null(excel_summary$error) && !is.na(excel_summary$error)) {
    result$error <- paste0("Excel parsing: ", excel_summary$error)
    cli_alert_danger("Failed: {result$error}")
    return(result)
  }

  # Load parcels and run evaluation
  tryCatch({
    parcels <- load_municipality(parcel_shapefile_path, municipality_name)
    district <- shp_validation$data

    # Extract zoning parameters from the same district that has parcel data
    zoning_params <- extract_zoning_parameters(excel_path, district = district_num)

    # Evaluate compliance
    evaluation <- evaluate_compliance(
      municipality = parcels,
      districts = district,
      zoning_params = zoning_params,
      community_type = "rapid_transit"  # Default assumption
    )

    # Compare parcel assignment to Excel
    excel_parcel_ids <- extract_excel_parcel_ids(excel_path)

    # Get R-assigned parcels (only those assigned to districts, not NA)
    r_parcels_assigned <- evaluation$parcel_detail[!is.na(evaluation$parcel_detail$district_id), ]
    r_parcel_ids <- unique(r_parcels_assigned$LOC_ID)

    if (!is.null(excel_parcel_ids)) {
      n_excel <- length(excel_parcel_ids)
      n_r <- length(r_parcel_ids)
      n_match <- length(intersect(excel_parcel_ids, r_parcel_ids))
      result$parcel_match_rate <- n_match / max(n_excel, n_r)
    } else {
      result$parcel_match_rate <- NA_real_
    }

    # Compare results
    result$success <- TRUE
    result$excel_units <- excel_summary$total_units
    result$excel_acres <- excel_summary$total_acres
    result$excel_density <- excel_summary$gross_density
    result$excel_compliant <- excel_summary$compliant
    result$r_units <- evaluation$summary$total_units
    result$r_acres <- evaluation$summary$total_acres
    result$r_density <- evaluation$summary$gross_density
    result$r_compliant <- evaluation$summary$compliant
    result$unit_diff <- result$r_units - result$excel_units
    result$unit_pct_diff <- (result$unit_diff / result$excel_units) * 100

    cli_alert_success("Validation complete: {round(result$r_units)} vs {round(result$excel_units)} units ({round(result$unit_pct_diff, 1)}% diff)")

    if (!is.na(result$parcel_match_rate)) {
      cli_alert_info("Parcel assignment match rate: {round(result$parcel_match_rate * 100, 1)}%")
    }

    return(result)

  }, error = function(e) {
    result$error <- paste0("R package error: ", e$message)
    cli_alert_danger("Failed: {result$error}")
    return(result)
  })
}


# ==============================================================================
# MAIN VALIDATION PIPELINE
# ==============================================================================

#' Run systematic validation across all municipalities
run_systematic_validation <- function() {
  cli_h1("MBTA Communities Package Systematic Validation")

  # Discover files
  excel_dt <- discover_excel_models()
  shapefile_dt <- discover_district_shapefiles()

  # Initialize results storage
  results_method1 <- list()
  results_method2 <- list()

  # Process each municipality
  for (i in seq_len(nrow(excel_dt))) {
    municipality <- excel_dt$municipality[i]
    excel_path <- excel_dt$excel_path[i]

    cli_rule(left = paste0("[{i}/{nrow(excel_dt)}] {municipality}"))

    # Find parcel shapefile
    parcel_shp <- find_parcel_shapefile(municipality)

    if (is.na(parcel_shp)) {
      cli_alert_warning("No parcel shapefile found - skipping municipality")
      next
    }

    # Find district shapefile
    district_shp <- match_shapefile(municipality, shapefile_dt)

    # Method 1: Excel Parcel List
    result1 <- validate_method1_excel_parcels(municipality, excel_path, parcel_shp)
    results_method1[[length(results_method1) + 1]] <- result1

    # Method 2: Shapefile Boundary (if district shapefile available)
    if (!is.na(district_shp)) {
      result2 <- validate_method2_shapefile(municipality, excel_path, parcel_shp, district_shp)
      results_method2[[length(results_method2) + 1]] <- result2
    } else {
      cli_alert_info("Method 2 skipped: No district shapefile")
    }

    cli_text("")  # Blank line for readability
  }

  # Convert to data.tables
  results_method1_dt <- rbindlist(results_method1, fill = TRUE)
  results_method2_dt <- rbindlist(results_method2, fill = TRUE)

  # Save results
  saveRDS(list(method1 = results_method1_dt, method2 = results_method2_dt), RESULTS_FILE)

  # Print summary
  cli_h2("Validation Summary")

  cli_alert_success("Method 1 (Excel Parcels): {sum(results_method1_dt$success)} / {nrow(results_method1_dt)} succeeded ({round(sum(results_method1_dt$success)/nrow(results_method1_dt)*100, 1)}%)")

  if (nrow(results_method2_dt) > 0) {
    cli_alert_success("Method 2 (Shapefile): {sum(results_method2_dt$success)} / {nrow(results_method2_dt)} succeeded ({round(sum(results_method2_dt$success)/nrow(results_method2_dt)*100, 1)}%)")
  }

  cli_alert_info("Results saved to {RESULTS_FILE}")

  return(list(method1 = results_method1_dt, method2 = results_method2_dt))
}


# ==============================================================================
# RUN VALIDATION
# ==============================================================================

if (!interactive()) {
  results <- run_systematic_validation()
}