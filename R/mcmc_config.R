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
#' @param district_data List from [load_district_data()]
#' @param parcel_graph_result List from [build_parcel_graph_target()] or
#'   [build_identity_parcel_graph()]
#' @return List of constraints for MCMC
#' @export
define_constraints <- function(district_data, parcel_graph_result) {
  req <- district_data$district_requirements

  # Build unit_id → LOC_ID lookup for GIS density denominator computation.
  pa <- parcel_graph_result$parcel_assignments
  unit_to_loc_ids <- split(pa$parcel_id, pa$unit_id)

  list(
    min_capacity               = req$min_units,
    min_area                   = if (is.na(req$min_acres)) 0 else req$min_acres,
    min_density                = req$min_gross_density,
    min_lcc_fraction           = 0.5,
    station_capacity_pct       = req$station_area_unit_pct,
    station_area_pct           = req$station_area_land_pct,
    unit_to_loc_ids            = unit_to_loc_ids,
    district_geometry          = district_data$district_geometry,
    local_deductions_dissolved = district_data$local_deductions_dissolved
  )
}
