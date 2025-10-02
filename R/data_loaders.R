#' Data Loading Functions
#'
#' Functions to load and validate parcel shapefiles and GIS calculation layers
#' for the MBTA Communities compliance model.
#'
#' @name data_loaders
NULL

#' Load Municipality Parcel Shapefile
#'
#' Load and validate municipality parcel shapefile with excluded/sensitive land
#' measurements. This function handles both file paths and pre-loaded sf objects,
#' ensures correct projection, and validates required columns.
#'
#' @param shapefile Either a file path to a shapefile (character) or an sf object.
#'   File paths can be .shp files or .zip archives containing shapefiles.
#' @param community_name Optional community name for metadata (character)
#' @param projection Target EPSG code for projection (default: 26986 for
#'   Massachusetts State Plane NAD83). Data will be transformed to this projection
#'   if needed.
#' @param validate Logical indicating whether to validate required columns and
#'   data types (default: TRUE)
#'
#' @return An sf object with standardized structure and metadata attributes:
#'   \itemize{
#'     \item All required columns for compliance model calculations
#'     \item Geometry in specified projection (default EPSG:26986)
#'     \item Metadata attributes: community_name, source_file, crs_epsg, load_date, n_parcels
#'     \item Additional class "mbtazone_municipality" for method dispatch
#'   }
#'
#' @details
#' Required columns (from state-provided Basic land map shapefiles):
#' \itemize{
#'   \item \strong{Identifiers}: LOC_ID, Address, Owner, UseCodes
#'   \item \strong{Areas}: SQFT (numeric), ACRES (numeric)
#'   \item \strong{Transit}: TRANSIT (character, "Y"/"N")
#'   \item \strong{Exclusions}: PublicInst, NonPubExc, Tot_Exclud, Tot_Sensit (numeric)
#' }
#'
#' The function will:
#' \enumerate{
#'   \item Extract zip files to temporary directory if needed
#'   \item Load shapefile using sf::st_read()
#'   \item Transform to target projection (26986) if needed
#'   \item Validate required columns and data types (if validate=TRUE)
#'   \item Add metadata attributes
#'   \item Return standardized sf object
#' }
#'
#' @examples
#' \dontrun{
#' # Load from zip file
#' cambridge <- load_municipality(
#'   "inst/extdata/parcels/49_CAMBRIDGE_basic.zip",
#'   community_name = "Cambridge"
#' )
#'
#' # Load from shapefile path
#' chelsea <- load_municipality(
#'   "path/to/57_CHELSEA_basic.shp",
#'   community_name = "Chelsea"
#' )
#'
#' # Load pre-loaded sf object (skip file operations)
#' data <- sf::st_read("parcels.shp")
#' validated <- load_municipality(data, validate = TRUE)
#' }
#'
#' @export
load_municipality <- function(shapefile,
                             community_name = NULL,
                             projection = 26986,
                             validate = TRUE) {

  # 1. Input Handling
  source_file <- NULL

  if (is.character(shapefile)) {
    # File path provided
    if (!file.exists(shapefile)) {
      cli::cli_abort("Shapefile not found: {.file {shapefile}}")
    }

    source_file <- shapefile

    # Handle zip files
    if (tools::file_ext(shapefile) == "zip") {
      # Create unique temp directory for this extraction
      temp_dir <- file.path(tempdir(), basename(tempfile()))
      dir.create(temp_dir, showWarnings = FALSE)
      utils::unzip(shapefile, exdir = temp_dir)

      # Find the .shp file in extracted contents
      shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
      if (length(shp_files) == 0) {
        cli::cli_abort("No .shp file found in zip archive: {.file {shapefile}}")
      }
      if (length(shp_files) > 1) {
        cli::cli_warn("Multiple .shp files found, using: {.file {basename(shp_files[1])}}")
      }

      shapefile_path <- shp_files[1]
    } else {
      shapefile_path <- shapefile
    }

    # Load shapefile
    data <- sf::st_read(shapefile_path, quiet = TRUE)

  } else if (inherits(shapefile, "sf")) {
    # sf object provided
    data <- shapefile

  } else {
    cli::cli_abort(
      "shapefile must be either a file path (character) or an sf object, not {.cls {class(shapefile)}}"
    )
  }

  # 2. CRS Transformation
  original_crs <- sf::st_crs(data)$epsg
  transformed <- FALSE

  if (is.na(original_crs)) {
    cli::cli_warn("CRS not set in input data. Assuming EPSG:{projection}")
    data <- sf::st_set_crs(data, projection)
  } else if (original_crs != projection) {
    data <- sf::st_transform(data, crs = projection)
    transformed <- TRUE
  }

  # 3. Column Validation
  if (validate) {
    required_cols <- c(
      # Identifiers
      "LOC_ID", "Address", "Owner", "UseCodes",
      # Areas
      "SQFT", "ACRES",
      # Transit
      "TRANSIT",
      # Exclusions
      "PublicInst", "NonPubExc", "Tot_Exclud", "Tot_Sensit"
    )

    validate_parcel_data(data, required_cols, strict = TRUE)
  }

  # 4. Column Type Validation (if validate=TRUE)
  if (validate) {
    # Check numeric columns
    numeric_cols <- c("SQFT", "ACRES", "PublicInst", "NonPubExc", "Tot_Exclud", "Tot_Sensit")
    for (col in numeric_cols) {
      if (!is.numeric(data[[col]])) {
        cli::cli_abort(
          "Column {.field {col}} must be numeric, got {.cls {class(data[[col]])}}"
        )
      }
    }

    # Check character columns
    character_cols <- c("LOC_ID", "Address", "Owner", "UseCodes", "TRANSIT")
    for (col in character_cols) {
      if (!is.character(data[[col]])) {
        cli::cli_warn(
          "Column {.field {col}} expected to be character, got {.cls {class(data[[col]])}}"
        )
      }
    }

    # Warn about excessive NAs in critical columns
    critical_cols <- c("SQFT", "ACRES", "Tot_Exclud")
    for (col in critical_cols) {
      na_pct <- sum(is.na(data[[col]])) / nrow(data)
      if (na_pct > 0.1) {
        cli::cli_warn(
          "Column {.field {col}} has {.val {round(na_pct * 100, 1)}%} NA values (>{.val 10%})"
        )
      }
    }
  }

  # 5. Add Metadata
  attr(data, "community_name") <- community_name
  attr(data, "source_file") <- source_file
  attr(data, "crs_epsg") <- projection
  attr(data, "load_date") <- Sys.Date()
  attr(data, "n_parcels") <- nrow(data)
  attr(data, "crs_transformed") <- transformed

  # 6. Return with custom class
  class(data) <- c("mbtazone_municipality", class(data))

  return(data)
}

#' Validate Parcel Data Structure
#'
#' Internal helper function to validate that an sf object has required columns
#' and valid structure for MBTA Communities compliance calculations.
#'
#' @param sf_object An sf object to validate
#' @param required_columns Character vector of required column names
#' @param strict Logical indicating whether to throw errors (TRUE) or warnings (FALSE)
#'
#' @return TRUE if validation passes, otherwise throws error or warning
#'
#' @keywords internal
validate_parcel_data <- function(sf_object,
                                 required_columns,
                                 strict = TRUE) {

  # Check sf object
  if (!inherits(sf_object, "sf")) {
    if (strict) {
      cli::cli_abort("Object must be an sf spatial object, got {.cls {class(sf_object)}}")
    } else {
      cli::cli_warn("Object is not an sf spatial object")
      return(FALSE)
    }
  }

  # Check CRS is set
  if (is.na(sf::st_crs(sf_object)$epsg)) {
    if (strict) {
      cli::cli_abort("CRS must be set before validation")
    } else {
      cli::cli_warn("CRS is not set")
      return(FALSE)
    }
  }

  # Check required columns
  missing_cols <- setdiff(required_columns, names(sf_object))

  if (length(missing_cols) > 0) {
    if (strict) {
      cli::cli_abort(
        "Missing required columns: {.field {missing_cols}}"
      )
    } else {
      cli::cli_warn(
        "Missing required columns: {.field {missing_cols}}"
      )
      return(FALSE)
    }
  }

  return(TRUE)
}

#' Load Transit Station Area Buffers
#'
#' Load statewide transit station area buffers (0.5-mile radius) for calculating
#' compliance location requirements. This layer represents all MBTA transit stations
#' with half-mile radius buffers.
#'
#' @param station_shapefile Optional path to transit station shapefile (character).
#'   If NULL (default), uses package-provided statewide layer.
#' @param projection Target EPSG code for projection (default: 26986 for
#'   Massachusetts State Plane NAD83). Data will be transformed to this projection
#'   if needed.
#'
#' @return An sf object containing transit station buffer geometry:
#'   \itemize{
#'     \item Single MULTIPOLYGON feature representing all 0.5-mile station buffers
#'     \item Geometry in specified projection (default EPSG:26986)
#'     \item Metadata attributes: source_file, crs_epsg, load_date
#'     \item Additional class "mbtazone_transit_stations" for method dispatch
#'   }
#'
#' @details
#' This function loads the statewide transit station areas layer provided by EOHLC.
#' The layer contains 0.5-mile (2640 feet) radius buffers around all MBTA stations,
#' used to determine whether parcels are within required transit area percentages
#' for Rapid Transit and Commuter Rail communities.
#'
#' The default shapefile is included in the package at
#' \code{inst/extdata/statewide/Transit_Station_Areas_Half_Mile_Radius.zip}.
#'
#' @examples
#' \dontrun{
#' # Load package-provided statewide layer
#' transit_buffers <- load_transit_stations()
#'
#' # Load custom shapefile
#' transit_buffers <- load_transit_stations(
#'   "path/to/Transit_Station_Areas.shp"
#' )
#'
#' # Use for intersection with parcels
#' library(sf)
#' parcels <- load_municipality("cambridge.zip")
#' in_transit <- st_intersects(parcels, transit_buffers, sparse = FALSE)
#' }
#'
#' @seealso \code{\link{load_municipality}}, \code{\link{load_density_deductions}}
#'
#' @export
load_transit_stations <- function(station_shapefile = NULL,
                                  projection = 26986) {

  # 1. Determine file path
  if (is.null(station_shapefile)) {
    # Use package default
    station_shapefile <- system.file(
      "extdata/statewide/Transit_Station_Areas_Half_Mile_Radius.zip",
      package = "mbtazone"
    )

    if (station_shapefile == "") {
      cli::cli_abort(
        "Package default transit station shapefile not found. Reinstall package or provide custom path."
      )
    }
  }

  # 2. Validate file exists
  if (!file.exists(station_shapefile)) {
    cli::cli_abort("Transit station shapefile not found: {.file {station_shapefile}}")
  }

  source_file <- station_shapefile

  # 3. Handle zip files
  if (tools::file_ext(station_shapefile) == "zip") {
    # Create unique temp directory for extraction
    temp_dir <- file.path(tempdir(), basename(tempfile()))
    dir.create(temp_dir, showWarnings = FALSE)
    utils::unzip(station_shapefile, exdir = temp_dir)

    # Find the .shp file
    shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_files) == 0) {
      cli::cli_abort("No .shp file found in zip archive: {.file {station_shapefile}}")
    }

    shapefile_path <- shp_files[1]
  } else {
    shapefile_path <- station_shapefile
  }

  # 4. Load shapefile
  data <- sf::st_read(shapefile_path, quiet = TRUE)

  # 5. CRS Transformation
  original_crs <- sf::st_crs(data)$epsg
  transformed <- FALSE

  if (is.na(original_crs)) {
    cli::cli_warn("CRS not set in transit station data. Assuming EPSG:{projection}")
    data <- sf::st_set_crs(data, projection)
  } else if (original_crs != projection) {
    data <- sf::st_transform(data, crs = projection)
    transformed <- TRUE
  }

  # 6. Validate geometry type
  geom_types <- unique(as.character(sf::st_geometry_type(data)))
  valid_types <- c("POLYGON", "MULTIPOLYGON")

  if (!all(geom_types %in% valid_types)) {
    cli::cli_abort(
      "Transit station data must contain POLYGON or MULTIPOLYGON geometry, got {.val {geom_types}}"
    )
  }

  # 7. Add metadata
  attr(data, "source_file") <- source_file
  attr(data, "crs_epsg") <- projection
  attr(data, "load_date") <- Sys.Date()
  attr(data, "crs_transformed") <- transformed
  attr(data, "n_features") <- nrow(data)

  # 8. Return with custom class
  class(data) <- c("mbtazone_transit_stations", class(data))

  return(data)
}

#' Load Gross Density Denominator Deduction Layer
#'
#' Load statewide gross density denominator deduction layer for calculating
#' district-level gross density. This layer represents areas that must be excluded
#' from the gross density denominator calculation.
#'
#' @param deduction_shapefile Optional path to density deduction shapefile (character).
#'   If NULL (default), uses package-provided statewide layer.
#' @param projection Target EPSG code for projection (default: 26986 for
#'   Massachusetts State Plane NAD83). Data will be transformed to this projection
#'   if needed.
#'
#' @return An sf object containing density deduction polygons:
#'   \itemize{
#'     \item Multiple POLYGON features representing areas excluded from density calculations
#'     \item Geometry in specified projection (default EPSG:26986)
#'     \item Metadata attributes: source_file, crs_epsg, load_date, n_features
#'     \item Additional class "mbtazone_density_deductions" for method dispatch
#'   }
#'
#' @details
#' This function loads the statewide Gross Density Denominator Deductions layer
#' provided by EOHLC. The layer contains polygons representing areas that must be
#' subtracted from total district area when calculating gross density
#' (units per acre requirement).
#'
#' The default shapefile is included in the package at
#' \code{inst/extdata/statewide/Density_Denominator_Deductions.zip}.
#'
#' \strong{Note:} This is a large dataset (87,000+ features, ~372MB) and may take
#' 5-10 seconds to load.
#'
#' @examples
#' \dontrun{
#' # Load package-provided statewide layer
#' deductions <- load_density_deductions()
#'
#' # Load custom shapefile
#' deductions <- load_density_deductions(
#'   "path/to/Density_Deductions.shp"
#' )
#'
#' # Use for calculating district gross density
#' library(sf)
#' district <- st_read("district_boundary.shp")
#' deductions_in_district <- st_intersection(district, deductions)
#' deduction_area <- sum(st_area(deductions_in_district))
#' }
#'
#' @seealso \code{\link{load_municipality}}, \code{\link{load_transit_stations}}
#'
#' @export
load_density_deductions <- function(deduction_shapefile = NULL,
                                    projection = 26986) {

  # 1. Determine file path
  if (is.null(deduction_shapefile)) {
    # Use package default
    deduction_shapefile <- system.file(
      "extdata/statewide/Density_Denominator_Deductions.zip",
      package = "mbtazone"
    )

    if (deduction_shapefile == "") {
      cli::cli_abort(
        "Package default density deduction shapefile not found. Reinstall package or provide custom path."
      )
    }
  }

  # 2. Validate file exists
  if (!file.exists(deduction_shapefile)) {
    cli::cli_abort("Density deduction shapefile not found: {.file {deduction_shapefile}}")
  }

  source_file <- deduction_shapefile

  # 3. Handle zip files
  if (tools::file_ext(deduction_shapefile) == "zip") {
    # Create unique temp directory for extraction
    temp_dir <- file.path(tempdir(), basename(tempfile()))
    dir.create(temp_dir, showWarnings = FALSE)
    utils::unzip(deduction_shapefile, exdir = temp_dir)

    # Find the .shp file
    shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_files) == 0) {
      cli::cli_abort("No .shp file found in zip archive: {.file {deduction_shapefile}}")
    }

    shapefile_path <- shp_files[1]
  } else {
    shapefile_path <- deduction_shapefile
  }

  # 4. Load shapefile (may take several seconds for large file)
  data <- sf::st_read(shapefile_path, quiet = TRUE)

  # 5. CRS Transformation
  original_crs <- sf::st_crs(data)$epsg
  transformed <- FALSE

  if (is.na(original_crs)) {
    cli::cli_warn("CRS not set in density deduction data. Assuming EPSG:{projection}")
    data <- sf::st_set_crs(data, projection)
  } else if (original_crs != projection) {
    data <- sf::st_transform(data, crs = projection)
    transformed <- TRUE
  }

  # 6. Validate geometry type
  geom_types <- unique(as.character(sf::st_geometry_type(data)))
  valid_types <- c("POLYGON", "MULTIPOLYGON")

  if (!all(geom_types %in% valid_types)) {
    cli::cli_abort(
      "Density deduction data must contain POLYGON or MULTIPOLYGON geometry, got {.val {geom_types}}"
    )
  }

  # 7. Add metadata
  attr(data, "source_file") <- source_file
  attr(data, "crs_epsg") <- projection
  attr(data, "load_date") <- Sys.Date()
  attr(data, "crs_transformed") <- transformed
  attr(data, "n_features") <- nrow(data)

  # 8. Return with custom class
  class(data) <- c("mbtazone_density_deductions", class(data))

  return(data)
}