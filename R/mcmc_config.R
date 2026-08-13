# config.R - Configuration constants and constraints for parcel MCMC
#
# Defines constants, constraints, and constraint names for the parcel
# MCMC zoning analysis pipeline.

# ============================================================================
# CONSTRAINTS
# ============================================================================

#' Define constraints from district requirements
#'
#' @param district_data List from [load_district_data()]
#' @param parcel_graph_result List from [build_parcel_graph_target()] or
#'   [build_identity_parcel_graph()]
#' @param row_fill_m Buffer distance (metres) for the morphological close in
#'   [compute_gis_density_denom()]; fills road right-of-way gaps narrower than
#'   `2*d` between adjacent parcels (e.g. 18.29 m fills gaps up to
#'   36.58 m ≈ 120 ft). Set to 0 to disable.
#' @param right_of_way_sf Optional sf object of right-of-way polygons (e.g.
#'   `district_data$district_right_of_way`). When supplied, the morphological
#'   close in [compute_gis_density_denom()] is ROW-constrained: fill area is
#'   only kept where it overlaps the ROW polygon, preventing excluded-parcel
#'   voids and non-ROW gaps from being counted. When `NULL` (default), falls
#'   back to the unconstrained morphological close.
#' @return List of constraints for MCMC
#' @export
define_constraints <- function(district_data, parcel_graph_result,
                               row_fill_m = 18.29,
                               right_of_way_sf = NULL) {
  req <- district_data$district_requirements

  # Maps each parcel graph unit to its constituent parcel LOC_IDs,
  # used to look up geometries when computing the GIS density denominator.
  pa <- parcel_graph_result$parcel_assignments
  unit_to_loc_ids <- split(pa$parcel_id, pa$unit_id)

  # Named sfc keyed by LOC_ID for geometry lookup in the GIS density
  # denominator. Stored as sfc rather than sf to survive qs serialization.
  dg <- district_data$district_geometry
  assert_crs_26986(dg, "`district_data$district_geometry`")

  geom_sfc <- sf::st_geometry(dg)
  names(geom_sfc) <- dg$LOC_ID

  assert_crs_26986(
    district_data$local_deductions_dissolved,
    "`district_data$local_deductions_dissolved`"
  )
  ded_sfc <- sf::st_geometry(district_data$local_deductions_dissolved)

  # Dissolve ROW geometry clipped to the district bounding box.
  # Stored as a single sfc for use in ROW-constrained fill.
  row_sfc <- NULL
  if (!is.null(right_of_way_sf) && nrow(right_of_way_sf) > 0) {
    assert_crs_26986(right_of_way_sf, "`right_of_way_sf`")
    bbox_geom <- sf::st_as_sfc(sf::st_bbox(dg))
    local_row <- suppressWarnings(
      sf::st_intersection(right_of_way_sf, sf::st_sf(geometry = bbox_geom))
    )
    if (nrow(local_row) > 0) {
      row_sfc <- sf::st_union(sf::st_geometry(local_row))
    }
  }

  list(
    min_capacity         = req$min_units,
    min_area             = if (is.na(req$min_acres)) 0 else req$min_acres,
    min_density          = req$min_gross_density,
    min_lcc_fraction     = 0.5,
    station_capacity_pct = req$station_area_unit_pct,
    station_area_pct     = req$station_area_land_pct,
    unit_to_loc_ids      = unit_to_loc_ids,
    geom_sfc             = geom_sfc,
    ded_sfc              = ded_sfc,
    row_fill_m           = row_fill_m,
    row_sfc              = row_sfc
  )
}

# ============================================================================
# TARGET SPEC (defines the sampled distribution)
# ============================================================================

#' Define the parcel MCMC target spec
#'
#' Composes the hard district constraints with the priors that shape the
#' sampled distribution. Constraints and priors are kept as separate
#' sub-fields rather than merged into one flat list: constraints gate
#' feasibility (hard, checked in `check_hard_constraints_only()`), while
#' priors reweight among feasible states (soft, applied via the MH ratio).
#' This is the single object that determines the target distribution
#' pi(state) — nothing about proposal/kernel tuning belongs here.
#'
#' @param district_data List containing district_requirements from data loading
#' @param capacity_prior_lambda Linear capacity-prior strength (penalizes
#'   capacity above min_capacity; larger values favor configurations closer
#'   to the minimum)
#' @param k_prior_lambda Geometric-prior rate on the number of secondary
#'   blocks (k); 0 is a flat/improper prior over k
#' @param right_of_way_sf Optional sf object of right-of-way polygons passed
#'   through to [define_constraints()] for ROW-constrained fill. Typically
#'   `district_data$district_right_of_way`.
#' @param row_fill_m Passed through to [define_constraints()].
#' @return List with `constraints` and `priors` sub-lists
#' @export
parcel_target_spec <- function(district_data, parcel_graph_result,
                               capacity_prior_lambda, k_prior_lambda,
                               right_of_way_sf = NULL,
                               row_fill_m = 18.29) {
  list(
    constraints = define_constraints(district_data, parcel_graph_result,
                                     row_fill_m      = row_fill_m,
                                     right_of_way_sf = right_of_way_sf),
    priors = list(
      capacity_prior_lambda = capacity_prior_lambda,
      k_prior_lambda        = k_prior_lambda
    )
  )
}

# ============================================================================
# DISCOVERY SPEC (library construction / discovery tuning)
# ============================================================================

#' Define the parcel MCMC discovery spec
#'
#' Bundles the tuning parameters for building the LCC and secondary block
#' libraries: tree enumeration (Wilson's algorithm) plus a BFS supplement,
#' for each library. Defaults mirror the values previously hardcoded in
#' `inst/targets/temp_targets_parcel_config.R`.
#'
#' @param discovery_capacity_multiplier Discovery-only capacity bound
#'   (multiplier of min_capacity); LCCs above this are skipped during tree
#'   enumeration purely for speed, not because they're infeasible
#' @param tree_lcc_n_trees Number of spanning trees to sample for LCC
#'   tree-cut enumeration
#' @param bfs_lcc_n_samples,bfs_lcc_n_seeds BFS-supplement sample/seed counts
#'   for LCC discovery
#' @param lcc_capacity_bands_relative List of `c(low, high)` capacity bands
#'   (relative to min_capacity) for capacity-stratified BFS discovery
#' @param lcc_band_samples_per_band Target number of LCC samples per capacity
#'   band
#' @param lcc_band_max_attempts Maximum BFS attempts per band before giving up
#' @param lcc_band_time_budget_s Wall-clock budget per band (seconds)
#' @param lcc_band_stall_attempts Consecutive attempts without a valid
#'   candidate before a discovery pass gives up on a band
#' @param lcc_discovery_max_unique Cap on unique LCCs tree enumeration will
#'   discover before it stops sampling more trees
#' @param lcc_library_max_size Final LCC library size cap
#' @param bfs_reservation_lcc Reserved LCC-library slots for BFS discoveries
#' @param sec_size_bands List of `c(min_acres, max_acres)` bands for secondary
#'   block discovery
#' @param library_density_threshold Minimum density (units/acre) for library
#'   blocks
#' @param tree_sec_n_trees Number of spanning trees to sample for secondary
#'   tree-cut enumeration
#' @param bfs_sec_quota_per_band BFS-supplement quota per secondary size band
#' @param sec_library_max_size Final secondary library size cap
#' @param bfs_reservation_sec Reserved secondary-library slots for BFS
#'   discoveries
#' @return Named list of discovery tuning parameters
#' @export
parcel_discovery_spec <- function(discovery_capacity_multiplier = 2.5,
                                   tree_lcc_n_trees = 500L,
                                   bfs_lcc_n_samples = 100L,
                                   bfs_lcc_n_seeds = 10L,
                                   lcc_capacity_bands_relative = list(
                                     c(0.5,  0.75),
                                     c(0.75, 1.0),
                                     c(1.0,  1.25),
                                     c(1.25, 1.5),
                                     c(1.5,  2.0)
                                   ),
                                   lcc_band_samples_per_band = 500L,
                                   lcc_band_max_attempts = 1000L,
                                   lcc_band_time_budget_s = 900,
                                   lcc_band_stall_attempts = 150L,
                                   lcc_discovery_max_unique = 50000L,
                                   lcc_library_max_size = 5000L,
                                   bfs_reservation_lcc = 500L,
                                   sec_size_bands = list(
                                     c(5, 8),
                                     c(8, 12),
                                     c(12, 20)
                                   ),
                                   library_density_threshold = 15,
                                   tree_sec_n_trees = 200L,
                                   bfs_sec_quota_per_band = 25L,
                                   sec_library_max_size = 500L,
                                   bfs_reservation_sec = 100L) {
  list(
    discovery_capacity_multiplier = discovery_capacity_multiplier,
    tree_lcc_n_trees              = tree_lcc_n_trees,
    bfs_lcc_n_samples              = bfs_lcc_n_samples,
    bfs_lcc_n_seeds                = bfs_lcc_n_seeds,
    lcc_capacity_bands_relative    = lcc_capacity_bands_relative,
    lcc_band_samples_per_band      = lcc_band_samples_per_band,
    lcc_band_max_attempts          = lcc_band_max_attempts,
    lcc_band_time_budget_s         = lcc_band_time_budget_s,
    lcc_band_stall_attempts        = lcc_band_stall_attempts,
    lcc_discovery_max_unique       = lcc_discovery_max_unique,
    lcc_library_max_size           = lcc_library_max_size,
    bfs_reservation_lcc            = bfs_reservation_lcc,
    sec_size_bands                 = sec_size_bands,
    library_density_threshold      = library_density_threshold,
    tree_sec_n_trees               = tree_sec_n_trees,
    bfs_sec_quota_per_band         = bfs_sec_quota_per_band,
    sec_library_max_size           = sec_library_max_size,
    bfs_reservation_sec            = bfs_reservation_sec
  )
}

# ============================================================================
# GRAPH SPEC (Parcel graph construction tuning)
# ============================================================================

#' Define the parcel graph construction spec
#'
#' Bundles the tuning for parcel graph construction: adjacency
#' detection (`build_adjacency_graph()`) and optional macro-parcel
#' aggregation (`build_parcel_graph_target()`). Runs before the parcel graph
#' or any MCMC objects exist.
#'
#' @param max_dist_ft Maximum boundary-to-boundary distance (ft) for two
#'   parcels to be considered for adjacency
#' @param touch_threshold_ft Boundary-to-boundary distance (ft) at or below
#'   which two parcels are treated as directly touching (no ROW crossing
#'   needed)
#' @param min_coverage_ratio Minimum fraction of the nearest-points line that
#'   must lie within right-of-way for a cross-ROW connection to be accepted
#' @param macro_scale Parcel aggregation scale factor (0 = raw parcels/no
#'   aggregation; > 0 = aggregation with scaled area targets)
#' @param macro_base_area_min,macro_base_area_max Base area target range
#'   (acres), scaled by `macro_scale`, for macro-parcel aggregation
#' @param macro_atomic_threshold Parcels at or above this area (acres) are
#'   preserved as singleton units regardless of `macro_scale`
#' @return Named list of graph-construction tuning parameters
#' @export
parcel_graph_spec <- function(max_dist_ft = 120,
                               touch_threshold_ft = 2,
                               min_coverage_ratio = 0.9,
                               macro_scale = 0,
                               macro_base_area_min = 0.25,
                               macro_base_area_max = 1.0,
                               macro_atomic_threshold = 5.0) {
  list(
    max_dist_ft             = max_dist_ft,
    touch_threshold_ft      = touch_threshold_ft,
    min_coverage_ratio      = min_coverage_ratio,
    macro_scale             = macro_scale,
    macro_base_area_min     = macro_base_area_min,
    macro_base_area_max     = macro_base_area_max,
    macro_atomic_threshold  = macro_atomic_threshold
  )
}
