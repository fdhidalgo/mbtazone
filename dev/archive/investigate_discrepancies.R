# Investigation of Validation Discrepancies
# Linear issue: HID-83
#
# This script investigates the discrepancies found in systematic validation:
# 1. Method 2: 0% parcel match rate (critical)
# 2. Method 1: Large unit capacity differences (Grafton, Westford, Worcester, etc.)

# Load package
if ("devtools" %in% loadedNamespaces() || requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)
} else if (!("mbtazone" %in% loadedNamespaces())) {
  library(mbtazone)
}

library(sf)
library(data.table)
library(readxl)
library(cli)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

EXCEL_MODELS_DIR <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_models"
DISTRICT_SHAPEFILES_DIR <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_shapefiles"
PARCEL_SHAPEFILES_DIR_1 <- "inst/extdata/parcels"
PARCEL_SHAPEFILES_DIR_2 <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/land_record_shapefiles/basic"

# ==============================================================================
# HELPER FUNCTIONS (from systematic_validation.R)
# ==============================================================================

find_parcel_shapefile <- function(municipality_name) {
  parcel_dirs <- c(PARCEL_SHAPEFILES_DIR_1, PARCEL_SHAPEFILES_DIR_2)
  norm_muni <- tolower(gsub("[^a-z0-9]", "", municipality_name))

  for (dir in parcel_dirs) {
    if (!dir.exists(dir)) next
    parcel_files <- list.files(dir, pattern = "\\.zip$", full.names = TRUE)
    for (pf in parcel_files) {
      if (grepl(norm_muni, tolower(pf), fixed = TRUE)) {
        return(pf)
      }
    }
  }
  return(NA_character_)
}

extract_excel_parcel_ids <- function(excel_path, district = 1) {
  tryCatch({
    sheets <- excel_sheets(excel_path)
    district_sheets <- sheets[grepl("^District \\d+$", sheets)]

    if (length(district_sheets) == 0) return(NULL)

    sheet_name <- paste0("District ", district)
    if (!sheet_name %in% district_sheets) {
      sheet_name <- district_sheets[1]
    }

    data <- read_excel(excel_path, sheet = sheet_name, skip = 18)
    data <- data[!apply(is.na(data) | data == "", 1, all), ]

    if (nrow(data) == 0) return(NULL)

    id_col <- names(data)[grepl("LOC_ID|OBJECTID|PARCEL.*ID|^ID$", names(data), ignore.case = TRUE)][1]

    if (is.na(id_col)) return(NULL)

    ids <- unique(data[[id_col]])
    ids <- ids[!is.na(ids) & ids != ""]

    return(as.character(ids))
  }, error = function(e) {
    return(NULL)
  })
}

# ==============================================================================
# INVESTIGATION 1: METHOD 2 PARCEL ASSIGNMENT (0% MATCH RATE)
# ==============================================================================

investigate_parcel_assignment <- function(municipality_name) {
  cli_h2("Investigating Parcel Assignment: {municipality_name}")

  # Find files
  excel_path <- file.path(EXCEL_MODELS_DIR, municipality_name,
                          list.files(file.path(EXCEL_MODELS_DIR, municipality_name),
                                    pattern = "\\.xlsx$")[1])

  parcel_shp <- find_parcel_shapefile(municipality_name)

  district_shp <- list.files(
    DISTRICT_SHAPEFILES_DIR,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Find matching district shapefile
  norm_muni <- tolower(gsub("[^a-z0-9]", "", municipality_name))
  district_shp <- district_shp[grepl(norm_muni, tolower(district_shp))][1]

  if (is.na(excel_path) || is.na(parcel_shp) || is.na(district_shp)) {
    cli_alert_danger("Missing required files")
    return(NULL)
  }

  # Extract Excel parcel IDs
  excel_parcel_ids <- extract_excel_parcel_ids(excel_path)
  cli_alert_info("Excel model has {length(excel_parcel_ids)} parcels")

  # Load parcels and district
  parcels <- load_municipality(parcel_shp, municipality_name)
  district <- st_read(district_shp, quiet = TRUE)

  # Transform district to match parcels CRS if needed
  if (st_crs(district)$epsg != 26986) {
    district <- st_transform(district, 26986)
  }

  # Manually assign parcels using package logic
  assigned <- assign_parcels_to_districts(
    municipality = parcels,
    districts = district
  )

  # Get R-assigned parcel IDs
  r_parcel_ids <- unique(assigned$LOC_ID[!is.na(assigned$district_id)])
  cli_alert_info("R package assigned {length(r_parcel_ids)} parcels")

  # Compare
  n_excel <- length(excel_parcel_ids)
  n_r <- length(r_parcel_ids)
  n_overlap <- length(intersect(excel_parcel_ids, r_parcel_ids))

  cli_alert_warning("Excel parcels: {n_excel}")
  cli_alert_warning("R parcels: {n_r}")
  cli_alert_warning("Overlap: {n_overlap} ({round(n_overlap/max(n_excel, n_r)*100, 1)}%)")

  # Show sample differences
  excel_only <- setdiff(excel_parcel_ids, r_parcel_ids)
  r_only <- setdiff(r_parcel_ids, excel_parcel_ids)

  cli_text("")
  cli_alert_info("Sample Excel-only parcels: {paste(head(excel_only, 5), collapse = ', ')}")
  cli_alert_info("Sample R-only parcels: {paste(head(r_only, 5), collapse = ', ')}")

  # Return diagnostic data
  list(
    municipality = municipality_name,
    excel_parcels = excel_parcel_ids,
    r_parcels = r_parcel_ids,
    excel_only = excel_only,
    r_only = r_only,
    overlap = intersect(excel_parcel_ids, r_parcel_ids),
    match_rate = n_overlap / max(n_excel, n_r)
  )
}

# ==============================================================================
# INVESTIGATION 2: GRAFTON (R CALCULATES 0 UNITS)
# ==============================================================================

investigate_grafton <- function() {
  cli_h2("Investigating Grafton: Why R calculates 0 units")

  excel_path <- file.path(EXCEL_MODELS_DIR, "Grafton",
                          list.files(file.path(EXCEL_MODELS_DIR, "Grafton"),
                                    pattern = "\\.xlsx$")[1])

  parcel_shp <- find_parcel_shapefile("Grafton")

  # Extract Excel data
  excel_parcel_ids <- extract_excel_parcel_ids(excel_path)
  cli_alert_info("Excel has {length(excel_parcel_ids)} parcels")

  # Load parcels
  parcels <- load_municipality(parcel_shp, "Grafton")
  parcels_filtered <- parcels[parcels$LOC_ID %in% excel_parcel_ids, ]

  cli_alert_info("Loaded {nrow(parcels_filtered)} parcels from shapefile")

  # Extract zoning parameters
  zoning_params <- extract_zoning_parameters(excel_path, district = 1)

  cli_text("")
  cli_alert_info("Zoning parameters:")
  print(str(zoning_params))

  # Check parcel data
  cli_text("")
  cli_alert_info("Parcel data columns:")
  print(names(parcels_filtered))

  cli_text("")
  cli_alert_info("Parcel details:")
  # Check which exclusion columns exist
  excl_cols <- c("LOC_ID", "ACRES")
  if ("EXCLUDE_AREA_SF" %in% names(parcels_filtered)) {
    excl_cols <- c(excl_cols, "EXCLUDE_AREA_SF")
  } else if ("Tot_Exclud" %in% names(parcels_filtered)) {
    excl_cols <- c(excl_cols, "Tot_Exclud", "Tot_Sensit")
  }
  print(parcels_filtered[, excl_cols])

  # Full capacity calculation using package function
  cli_text("")
  cli_alert_info("Running capacity calculation...")
  capacity <- calculate_district_capacity(
    parcels = parcels_filtered,
    zoning_params = zoning_params
  )

  cli_text("")
  cli_alert_info("Final unit capacity:")
  print(summary(capacity$final_unit_capacity))

  total_units <- sum(capacity$final_unit_capacity, na.rm = TRUE)
  cli_alert_warning("Total units calculated: {total_units} (Excel shows 550)")

  # Return diagnostic data
  list(
    municipality = "Grafton",
    excel_units = 550,
    r_units = total_units,
    parcels = parcels_filtered,
    capacity = capacity,
    zoning_params = zoning_params
  )
}

# ==============================================================================
# INVESTIGATION 3: WESTFORD (R CALCULATES 4X MORE UNITS)
# ==============================================================================

investigate_westford <- function() {
  cli_h2("Investigating Westford: Why R calculates 4x more units")

  excel_path <- file.path(EXCEL_MODELS_DIR, "Westford",
                          list.files(file.path(EXCEL_MODELS_DIR, "Westford"),
                                    pattern = "\\.xlsx$")[1])

  parcel_shp <- find_parcel_shapefile("Westford")

  # Extract Excel data
  excel_parcel_ids <- extract_excel_parcel_ids(excel_path)
  cli_alert_info("Excel has {length(excel_parcel_ids)} parcels")

  # Load parcels
  parcels <- load_municipality(parcel_shp, "Westford")
  parcels_filtered <- parcels[parcels$LOC_ID %in% excel_parcel_ids, ]

  cli_alert_info("Loaded {nrow(parcels_filtered)} parcels from shapefile")

  # Extract zoning parameters
  zoning_params <- extract_zoning_parameters(excel_path, district = 1)

  cli_text("")
  cli_alert_info("Zoning parameters:")
  print(str(zoning_params))

  # Run calculation
  capacity <- calculate_district_capacity(
    parcels = parcels_filtered,
    zoning_params = zoning_params
  )

  total_units <- sum(capacity$final_unit_capacity, na.rm = TRUE)

  cli_text("")
  cli_alert_warning("Total units calculated: {total_units} (Excel shows 601)")
  cli_alert_info("Unit capacity distribution:")
  print(summary(capacity$final_unit_capacity))

  # Check for outliers
  high_capacity <- capacity[capacity$final_unit_capacity > 50, ]
  if (nrow(high_capacity) > 0) {
    cli_text("")
    cli_alert_info("Parcels with >50 units ({nrow(high_capacity)} parcels):")
    print(high_capacity[, c("LOC_ID", "ACRES", "final_unit_capacity")])
  }

  # Return diagnostic data
  list(
    municipality = "Westford",
    excel_units = 601,
    r_units = total_units,
    parcels = parcels_filtered,
    capacity = capacity,
    zoning_params = zoning_params
  )
}

# ==============================================================================
# RUN INVESTIGATIONS
# ==============================================================================

cli_h1("Validation Discrepancy Investigation")

# Investigation 1: Method 2 parcel assignment (most critical)
cli_rule("Investigation 1: Method 2 Parcel Assignment")
abington_result <- investigate_parcel_assignment("Abington")
braintree_result <- investigate_parcel_assignment("Braintree")

# Investigation 2: Grafton (0 units)
cli_text("")
cli_rule("Investigation 2: Grafton (0 units calculated)")
grafton_result <- investigate_grafton()

# Investigation 3: Westford (4x units)
cli_text("")
cli_rule("Investigation 3: Westford (4x units calculated)")
westford_result <- investigate_westford()

# Save diagnostic results
cli_text("")
cli_alert_success("Saving diagnostic results to dev/discrepancy_diagnostics.rds")
saveRDS(
  list(
    parcel_assignment = list(
      abington = abington_result,
      braintree = braintree_result
    ),
    grafton = grafton_result,
    westford = westford_result
  ),
  "dev/discrepancy_diagnostics.rds"
)

cli_alert_success("Investigation complete!")
