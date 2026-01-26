# config.R - Configuration constants and constraints for parcel MCMC
#
# Defines constants, constraints, and constraint names for the parcel
# MCMC zoning analysis pipeline.

# WIP: All constants are removed and should be replaced with functions. (or config.yml)

# ============================================================================
# CONSTRAINTS
# ============================================================================

#' Define constraints from Norwood requirements
#'
#' @param norwood_data List containing norwood_requirements from data loading
#' @return List of constraints for MCMC
#' @export
define_constraints <- function(norwood_data) {
  req <- norwood_data$norwood_requirements
  list(
    min_capacity = req$min_units,
    min_area = req$min_acres,
    min_density = req$min_gross_density,
    min_lcc_fraction = 0.5, # Default LCC fraction requirement
    station_capacity_pct = req$station_area_unit_pct,
    station_area_pct = req$station_area_land_pct
  )
}
