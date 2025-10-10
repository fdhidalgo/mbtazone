#' MBTA Map Object for SMC Simulation
#'
#' The `mbta_map` class stores a complete zoning problem definition for
#' Sequential Monte Carlo simulation of MBTA overlay districts. This includes
#' parcel data, adjacency relationships, community requirements, and metadata.
#'
#' @name mbta_map
NULL

#' Create MBTA Map Object
#'
#' Constructor for `mbta_map` S3 class, which stores the zoning problem
#' definition for SMC simulation. This object contains all necessary data
#' for generating reference distributions of compliant zoning plans.
#'
#' @param data An sf data frame containing parcel polygons. Should be output
#'   from `load_municipality()` or have precomputed attributes from
#'   `precompute_spatial_attributes()`.
#' @param existing_plan Optional vector of district assignments for reference
#'   plan (same length as nrow(data)). Used for status-quo constraints. Default: NULL.
#' @param ndists Number of districts to simulate (default: 1). **Phase 1 MVP
#'   enforces ndists=1** for single-component overlays only.
#' @param total_capacity Name of column in `data` containing unit capacity
#'   values (default: "unit_capacity")
#' @param pop_tol Tolerance for capacity target as fraction (default: 0.25,
#'   meaning target ± 25%)
#' @param community_type MBTA community category. One of: "rapid_transit",
#'   "commuter_rail", "adjacent", "adjacent_small_town". Used to determine
#'   minimum requirements and station area coverage thresholds.
#' @param adj Optional adjacency list (output from `build_adjacency()`). If
#'   NULL, will be computed automatically using rook adjacency. Default: NULL.
#' @param precomputed Logical indicating whether `data` has precomputed spatial
#'   attributes from `precompute_spatial_attributes()`. Precomputation provides
#'   ~1000x speedup for SMC simulation. Default: FALSE.
#'
#' @return An `mbta_map` S3 object (list) with components:
#'   \item{data}{data.table with sf geometry column containing parcels}
#'   \item{ndists}{Number of districts (Phase 1: always 1)}
#'   \item{capacity_col}{Name of capacity column}
#'   \item{target_capacity}{Minimum required unit capacity}
#'   \item{target_area}{Minimum required land area (acres)}
#'   \item{pop_tol}{Capacity tolerance fraction}
#'   \item{community_type}{MBTA community category}
#'   \item{community_name}{Municipality name (from data attributes)}
#'   \item{station_area_unit_pct}{Required % of capacity in station areas}
#'   \item{station_area_land_pct}{Required % of area in station areas}
#'   \item{adj}{Adjacency list (list of integer vectors)}
#'   \item{precomputed}{Boolean indicating precomputed attributes}
#'   \item{existing_plan}{Reference plan assignments (if provided)}
#'   \item{component_info}{Component totals for infeasibility checking}
#'
#' @details
#' ## Community Requirements
#'
#' The function loads community-specific requirements from
#' `inst/extdata/community_info.csv`:
#' - `target_capacity`: Minimum housing units required
#' - `target_area`: Minimum land area (acres)
#' - `station_area_unit_pct`: Required % of capacity in station areas
#' - `station_area_land_pct`: Required % of area in station areas
#'
#' Requirements vary by community type:
#' - **Rapid Transit**: 90% station coverage (both capacity and area)
#' - **Commuter Rail**: 50-90% coverage (varies by developable station area)
#' - **Adjacent**: 0% coverage (no station requirement)
#' - **Adjacent Small Town**: 0% coverage, lower minimums
#'
#' ## Adjacency Graph
#'
#' If `adj=NULL`, the constructor automatically builds the adjacency graph using
#' **rook adjacency** (parcels must share an edge, not just a corner). This is
#' more robust for contiguity checking than queen adjacency.
#'
#' For large municipalities (>5000 parcels), building the adjacency graph can
#' take 10-30 seconds. Consider building once and caching:
#'
#' ```r
#' adj <- build_adjacency(parcels)
#' saveRDS(adj, "cached_adjacency.rds")
#' map <- mbta_map(parcels, adj = readRDS("cached_adjacency.rds"))
#' ```
#'
#' ## Precomputed Attributes
#'
#' For SMC simulation, precomputation provides ~1000x speedup by eliminating
#' repeated spatial intersections:
#'
#' ```r
#' # Setup phase (once per municipality)
#' parcels <- load_municipality("chelsea.zip")
#' stations <- load_station_areas()
#' parcels_precomputed <- precompute_spatial_attributes(
#'   parcels,
#'   station_areas = stations
#' )
#'
#' # Create map with precomputed data
#' map <- mbta_map(parcels_precomputed, precomputed = TRUE)
#' ```
#'
#' ## Phase 1 Limitations
#'
#' This implementation supports **single-component (fully contiguous) overlay
#' districts only** (ndists = 1). Multi-component overlays permitted under
#' 760 CMR 59.04(3)(d) are not yet supported. See PRD Section 15.2 for
#' rationale and future extension path.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' library(mbtazone)
#'
#' # Load municipality data
#' parcels <- load_municipality(
#'   "inst/extdata/parcels/57_CHELSEA_basic.zip",
#'   community_name = "Chelsea"
#' )
#'
#' # Create map object
#' chelsea_map <- mbta_map(
#'   parcels,
#'   community_type = "rapid_transit",
#'   total_capacity = "unit_capacity"
#' )
#'
#' print(chelsea_map)
#'
#' # With precomputed attributes (for performance)
#' stations <- load_station_areas()
#' parcels_precomp <- precompute_spatial_attributes(parcels, stations)
#'
#' chelsea_map <- mbta_map(
#'   parcels_precomp,
#'   community_type = "rapid_transit",
#'   precomputed = TRUE
#' )
#'
#' # Access components
#' chelsea_map$target_capacity  # Minimum units required
#' chelsea_map$adj              # Adjacency graph
#' summary(chelsea_map)         # Detailed statistics
#' }
#'
#' @seealso
#' \code{\link{build_adjacency}} for adjacency graph construction,
#' \code{\link{load_municipality}} for loading parcel data,
#' \code{\link{precompute_spatial_attributes}} for performance optimization
#'
#' @export
mbta_map <- function(data,
                     existing_plan = NULL,
                     ndists = 1,
                     total_capacity = "unit_capacity",
                     pop_tol = 0.25,
                     community_type = c("rapid_transit", "commuter_rail",
                                       "adjacent", "adjacent_small_town"),
                     adj = NULL,
                     precomputed = FALSE) {

  # ========== Input Validation ==========

  # Validate data is sf object
  if (!inherits(data, "sf")) {
    cli::cli_abort("{.arg data} must be an {.cls sf} object")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("{.arg data} is empty (0 rows)")
  }

  # Validate CRS (must be EPSG:26986 for Massachusetts)
  crs_epsg <- sf::st_crs(data)$epsg
  if (is.na(crs_epsg) || crs_epsg != 26986) {
    cli::cli_abort(c(
      "{.arg data} must be in EPSG:26986 (Massachusetts State Plane NAD83)",
      "x" = "Current CRS: {crs_epsg %||% 'unknown'}",
      "i" = "Transform with: sf::st_transform(data, 26986)"
    ))
  }

  # Validate capacity column exists
  if (!total_capacity %in% names(data)) {
    cli::cli_abort(c(
      "Capacity column {.field {total_capacity}} not found in {.arg data}",
      "i" = "Available columns: {.field {names(data)}}"
    ))
  }

  # Validate community_type
  community_type <- match.arg(community_type)

  # Validate ndists (Phase 1: enforce single district only)
  if (!is.numeric(ndists) || ndists != as.integer(ndists) || ndists < 1) {
    cli::cli_abort("{.arg ndists} must be a positive integer")
  }

  if (ndists != 1) {
    cli::cli_abort(c(
      "Phase 1 MVP supports single-component districts only (ndists = 1)",
      "x" = "Requested ndists = {ndists}",
      "i" = "Multi-component overlays will be supported in Phase 2",
      "i" = "See PRD Section 15.2 for details"
    ))
  }

  # Validate pop_tol
  if (!is.numeric(pop_tol) || pop_tol < 0 || pop_tol > 1) {
    cli::cli_abort("{.arg pop_tol} must be between 0 and 1")
  }

  # Validate existing_plan if provided
  if (!is.null(existing_plan)) {
    if (length(existing_plan) != nrow(data)) {
      cli::cli_abort(c(
        "{.arg existing_plan} length must match nrow(data)",
        "x" = "Length: {length(existing_plan)}, Expected: {nrow(data)}"
      ))
    }
  }

  # ========== Extract Community Information ==========

  # Try to get community name from data attributes
  community_name <- attr(data, "community_name")

  if (is.null(community_name)) {
    # Try to infer from source file attribute
    source_file <- attr(data, "source_file")
    if (!is.null(source_file)) {
      # Extract community name from filename (e.g., "57_CHELSEA_basic.zip")
      filename <- basename(source_file)
      parts <- strsplit(filename, "_")[[1]]
      if (length(parts) >= 2) {
        community_name <- parts[2]
      }
    }
  }

  # Load community requirements
  community_reqs <- load_community_requirements(community_name, community_type)

  # ========== Build or Validate Adjacency Graph ==========

  if (is.null(adj)) {
    cli::cli_alert_info("Building adjacency graph (this may take a moment)...")
    adj <- build_adjacency(data, quiet = FALSE)
  } else {
    # Validate provided adjacency list
    if (!is.list(adj)) {
      cli::cli_abort("{.arg adj} must be a list (adjacency list)")
    }
    if (length(adj) != nrow(data)) {
      cli::cli_abort(c(
        "{.arg adj} length must match nrow(data)",
        "x" = "Length: {length(adj)}, Expected: {nrow(data)}"
      ))
    }
  }

  # ========== Calculate Component Information ==========

  # Precompute component totals for Tier 2 infeasibility checking
  component_info <- calculate_component_totals(
    parcels = data,
    adj = adj,
    capacity_col = total_capacity,
    area_col = "SQFT"
  )

  # ========== Store Data (Keep as SF) ==========

  # Keep data as sf object
  # Note: We can add data.table functionality later if needed, but sf operations
  # are more critical for SMC and mixing the two classes causes issues
  data_dt <- data

  # ========== Construct mbta_map Object ==========

  map <- list(
    data = data_dt,
    ndists = as.integer(ndists),
    capacity_col = total_capacity,
    target_capacity = community_reqs$target_capacity,
    target_area = community_reqs$target_area,
    pop_tol = pop_tol,
    community_type = community_type,
    community_name = community_name,
    station_area_unit_pct = community_reqs$station_area_unit_pct,
    station_area_land_pct = community_reqs$station_area_land_pct,
    adj = adj,
    precomputed = precomputed,
    existing_plan = existing_plan,
    component_info = component_info
  )

  # Set S3 class
  class(map) <- c("mbta_map", "list")

  # Validate constructed object
  validate_mbta_map(map)

  cli::cli_alert_success(
    "Created mbta_map for {.strong {community_name %||% community_type}} ",
    "({nrow(data)} parcels)"
  )

  return(map)
}

#' Load Community Requirements from CSV
#'
#' Internal function to load community-specific requirements from
#' inst/extdata/community_info.csv
#'
#' @param community_name Community name (optional, can be NULL)
#' @param community_type Community type (required)
#'
#' @return List with target_capacity, target_area, station_area_unit_pct,
#'   station_area_land_pct
#'
#' @keywords internal
load_community_requirements <- function(community_name, community_type) {

  # Load community info CSV
  csv_path <- system.file(
    "extdata", "community_info.csv",
    package = "mbtazone",
    mustWork = FALSE
  )

  if (csv_path == "" || !file.exists(csv_path)) {
    cli::cli_abort(c(
      "Community info file not found",
      "x" = "Expected: inst/extdata/community_info.csv",
      "i" = "This file contains minimum requirements for each community"
    ))
  }

  community_info <- data.table::fread(csv_path)

  # Try to match by community name first
  if (!is.null(community_name)) {
    # Case-insensitive match
    match_row <- community_info[
      tolower(community_name) == tolower(community_info$community_name)
    ]

    if (nrow(match_row) == 1) {
      return(list(
        target_capacity = match_row$min_units,
        target_area = match_row$min_acres,
        station_area_unit_pct = match_row$station_area_unit_pct / 100,
        station_area_land_pct = match_row$station_area_land_pct / 100
      ))
    } else if (nrow(match_row) > 1) {
      cli::cli_warn(c(
        "Multiple matches found for community name {.val {community_name}}",
        "i" = "Using first match",
        "i" = "Check community_info.csv for duplicates"
      ))
      match_row <- match_row[1, ]
      return(list(
        target_capacity = match_row$min_units,
        target_area = match_row$min_acres,
        station_area_unit_pct = match_row$station_area_unit_pct / 100,
        station_area_land_pct = match_row$station_area_land_pct / 100
      ))
    } else {
      cli::cli_warn(c(
        "Community name {.val {community_name}} not found in community_info.csv",
        "i" = "Falling back to default values for {.val {community_type}}",
        "i" = "Consider adding community to community_info.csv for accurate requirements"
      ))
    }
  }

  # Fallback to default values by community type
  defaults <- get_default_requirements(community_type)
  return(defaults)
}

#' Get Default Requirements by Community Type
#'
#' Return typical requirements for each community type category. Used as fallback
#' when community-specific data is not available.
#'
#' @param community_type Community type string
#'
#' @return List with default requirements
#'
#' @keywords internal
get_default_requirements <- function(community_type) {

  defaults <- list(
    rapid_transit = list(
      target_capacity = 2500,
      target_area = 50,
      station_area_unit_pct = 0.90,
      station_area_land_pct = 0.90
    ),
    commuter_rail = list(
      target_capacity = 1500,
      target_area = 50,
      station_area_unit_pct = 0.50,
      station_area_land_pct = 0.50
    ),
    adjacent = list(
      target_capacity = 750,
      target_area = 50,
      station_area_unit_pct = 0,
      station_area_land_pct = 0
    ),
    adjacent_small_town = list(
      target_capacity = 200,
      target_area = NA_real_,  # Not required for small towns
      station_area_unit_pct = 0,
      station_area_land_pct = 0
    )
  )

  if (!community_type %in% names(defaults)) {
    cli::cli_abort("Unknown community_type: {.val {community_type}}")
  }

  cli::cli_inform(c(
    "i" = "Using default requirements for {.val {community_type}}:",
    " " = "Capacity: {defaults[[community_type]]$target_capacity} units",
    " " = "Area: {defaults[[community_type]]$target_area} acres",
    " " = "Station coverage: {defaults[[community_type]]$station_area_unit_pct * 100}%"
  ))

  return(defaults[[community_type]])
}

#' Validate mbta_map Object
#'
#' Internal function to validate that an mbta_map object has all required
#' components and is internally consistent.
#'
#' @param map An mbta_map object
#'
#' @return Invisibly returns TRUE if valid, otherwise throws error
#'
#' @keywords internal
validate_mbta_map <- function(map) {

  # Check class
  if (!inherits(map, "mbta_map")) {
    cli::cli_abort("{.arg map} must be an {.cls mbta_map} object")
  }

  # Check required components
  required_components <- c(
    "data", "ndists", "capacity_col", "target_capacity", "target_area",
    "pop_tol", "community_type", "station_area_unit_pct",
    "station_area_land_pct", "adj", "precomputed", "component_info"
  )

  missing <- setdiff(required_components, names(map))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "mbta_map object missing required components:",
      "x" = "Missing: {.field {missing}}"
    ))
  }

  # Validate internal consistency
  n_parcels <- nrow(map$data)

  if (length(map$adj) != n_parcels) {
    cli::cli_abort(c(
      "Adjacency list length does not match number of parcels",
      "x" = "Adjacency length: {length(map$adj)}, Parcels: {n_parcels}"
    ))
  }

  if (length(map$component_info$component_id) != n_parcels) {
    cli::cli_abort(c(
      "Component info length does not match number of parcels",
      "x" = "Component ID length: {length(map$component_info$component_id)}, ",
      "Parcels: {n_parcels}"
    ))
  }

  invisible(TRUE)
}

#' Print Method for mbta_map
#'
#' Display formatted summary of mbta_map object
#'
#' @param x An mbta_map object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns x
#'
#' @export
print.mbta_map <- function(x, ...) {

  # Header
  cli::cli_h1("MBTA Communities Zoning Map")
  cli::cli_text("{.cls mbta_map} object")
  cli::cli_text("")

  # Municipality info
  muni_name <- x$community_name %||% "Unknown"
  cli::cli_alert_info("Municipality: {.strong {muni_name}}")
  cli::cli_alert_info("Community type: {.val {x$community_type}}")
  cli::cli_alert_info("{x$ndists} district, {nrow(x$data)} parcels")
  cli::cli_text("")

  # Requirements
  cli::cli_h2("Requirements")
  cli::cli_text("Required capacity: {.val {x$target_capacity}} units")

  if (!is.na(x$target_area)) {
    cli::cli_text("Required area: {.val {x$target_area}} acres")
  } else {
    cli::cli_text("Required area: {.emph not required}")
  }

  # Station coverage requirements
  if (x$station_area_unit_pct > 0) {
    cli::cli_text(
      "Station area coverage required: {.val {x$station_area_unit_pct * 100}}% ",
      "(both capacity and area)"
    )
  } else {
    cli::cli_text("Station area coverage: {.emph not required}")
  }
  cli::cli_text("")

  # Constraints
  cli::cli_h2("Constraints")
  constraints <- c("contiguity", "min_capacity", "gross_density")
  if (!is.na(x$target_area)) {
    constraints <- c(constraints, "min_area")
  }
  if (x$station_area_unit_pct > 0) {
    constraints <- c(constraints, "station_coverage")
  }
  cli::cli_text("Active: {.field {constraints}}")
  cli::cli_text("")

  # Data status
  cli::cli_h2("Data")
  if (x$precomputed) {
    cli::cli_alert_success("Precomputed spatial attributes: {.strong Yes}")
    cli::cli_text("{.emph (1000x speedup for SMC simulation)}")
  } else {
    cli::cli_alert_info("Precomputed spatial attributes: No")
    cli::cli_text("{.emph (Consider using precompute_spatial_attributes() for performance)}")
  }

  # Adjacency stats
  adj_stats <- get_adjacency_stats(x$adj)
  cli::cli_text(
    "Adjacency graph: {adj_stats$n_edges} edges, ",
    "mean degree = {round(adj_stats$mean_degree, 1)}"
  )

  if (adj_stats$n_components > 1) {
    cli::cli_warn(
      "{adj_stats$n_components} disconnected components detected ",
      "({adj_stats$n_isolated} isolated parcels)"
    )
  }

  # Existing plan reference
  if (!is.null(x$existing_plan)) {
    cli::cli_text("")
    cli::cli_alert_info("Reference plan: Provided (for status-quo constraints)")
  }

  invisible(x)
}

#' Summary Method for mbta_map
#'
#' Display extended statistics for mbta_map object
#'
#' @param object An mbta_map object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns object
#'
#' @export
summary.mbta_map <- function(object, ...) {

  # Print basic info
  print(object)

  cli::cli_text("")
  cli::cli_h2("Extended Statistics")

  # Capacity statistics
  capacity <- object$data[[object$capacity_col]]
  capacity_stats <- c(
    Min = min(capacity, na.rm = TRUE),
    Q1 = quantile(capacity, 0.25, na.rm = TRUE),
    Median = median(capacity, na.rm = TRUE),
    Mean = mean(capacity, na.rm = TRUE),
    Q3 = quantile(capacity, 0.75, na.rm = TRUE),
    Max = max(capacity, na.rm = TRUE),
    NAs = sum(is.na(capacity))
  )

  cli::cli_h3("Parcel Capacity Distribution")
  cli::cli_text(
    "Min: {round(capacity_stats['Min'], 1)}, ",
    "Median: {round(capacity_stats['Median'], 1)}, ",
    "Mean: {round(capacity_stats['Mean'], 1)}, ",
    "Max: {round(capacity_stats['Max'], 1)}"
  )
  if (capacity_stats['NAs'] > 0) {
    cli::cli_warn("{capacity_stats['NAs']} parcel{?s} with NA capacity")
  }

  # Area statistics (if available)
  if ("SQFT" %in% names(object$data)) {
    area_acres <- object$data$SQFT / 43560
    area_stats <- c(
      Min = min(area_acres, na.rm = TRUE),
      Median = median(area_acres, na.rm = TRUE),
      Mean = mean(area_acres, na.rm = TRUE),
      Max = max(area_acres, na.rm = TRUE)
    )

    cli::cli_h3("Parcel Area Distribution (acres)")
    cli::cli_text(
      "Min: {round(area_stats['Min'], 2)}, ",
      "Median: {round(area_stats['Median'], 2)}, ",
      "Mean: {round(area_stats['Mean'], 2)}, ",
      "Max: {round(area_stats['Max'], 2)}"
    )
  }

  # Component statistics
  comp_info <- object$component_info
  cli::cli_h3("Connected Components")
  cli::cli_text("Number of components: {comp_info$n_components}")

  if (comp_info$n_components > 1) {
    # Largest component
    largest_comp <- which.max(comp_info$total_capacity)
    largest_n_parcels <- sum(comp_info$component_id == largest_comp)

    cli::cli_text(
      "Largest component: {largest_n_parcels} parcels ",
      "({round(comp_info$total_capacity[largest_comp], 0)} units, ",
      "{round(comp_info$total_area[largest_comp], 1)} acres)"
    )
  }

  invisible(object)
}