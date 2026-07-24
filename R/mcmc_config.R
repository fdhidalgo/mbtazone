# config.R - Configuration constants and constraints for parcel MCMC
#
# Defines constants, constraints, and constraint names for the parcel
# MCMC zoning analysis pipeline.

# WIP: All constants are removed and should be replaced with functions. (or config.yml)

# ============================================================================
# CONSTRAINTS
# ============================================================================

#' Define constraints from district requirements
#'
#' @param district_data List containing district_requirements from data loading
#' @return List of constraints for MCMC
#' @export
define_constraints <- function(district_data) {
  req <- district_data$district_requirements
  list(
    min_capacity = req$min_units,
    min_area = if (is.na(req$min_acres)) 0 else req$min_acres,
    min_density = req$min_gross_density,
    min_lcc_fraction = 0.5, # Default LCC fraction requirement
    station_capacity_pct = req$station_area_unit_pct,
    station_area_pct = req$station_area_land_pct
  )
}
