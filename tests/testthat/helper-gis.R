# GIS Operations Test Helpers
#
# Cache large spatial datasets to avoid repeated loading during tests

# Initialize cache environment
.gis_test_cache <- new.env(parent = emptyenv())

#' Get cached transit station areas
#'
#' Loads transit station areas once and caches for reuse across tests
get_cached_transit_stations <- function() {
  if (!exists("station_areas", envir = .gis_test_cache)) {
    .gis_test_cache$station_areas <- load_transit_stations()
  }
  return(.gis_test_cache$station_areas)
}

#' Get cached density deduction layers
#'
#' Loads density deduction layers once and caches for reuse across tests
get_cached_density_deductions <- function() {
  if (!exists("deduction_layers", envir = .gis_test_cache)) {
    .gis_test_cache$deduction_layers <- load_density_deductions()
  }
  return(.gis_test_cache$deduction_layers)
}
