# graph_building.R - Adjacency graph construction and boundary computation
#
# Functions for building parcel adjacency graphs with ROW support.
#
# Pipeline overview:
#
#   build_adjacency_graph()
#     |
#     |-- 1. Buffer parcels by max_dist_ft/2, find overlapping pairs (candidates)
#     |-- 2. Boundary-to-boundary distance: pairs <= touch_threshold_ft -> direct edges
#     |-- 3. Remaining candidates -> find_row_edges():
#     |         a. Filter to parcels within row_gap_tolerance_m of ROW
#     |         b. Build nearest-points lines (cap at max_dist_ft)
#     |         c. validate_row_crossing_lines(): ROW coverage ratio >= min_coverage_ratio
#     |-- 4. Merge direct + ROW edges -> igraph

# ============================================================================
# HELPERS
# ============================================================================

#' Build nearest-points lines between two sets of parcel geometries
#'
#' Uses st_nearest_points() so endpoints lie on parcel boundaries rather than
#' centroids — correct for irregular or concave parcels. Lines exceeding
#' max_dist_m are excluded before any further geometry work.
#'
#' @param from_geoms sfc of "from" parcel geometries
#' @param to_geoms   sfc of "to" parcel geometries (same length)
#' @param crs        CRS for the output sfc
#' @param max_dist_m Hard cap on boundary-to-boundary line length in metres
#' @return list(lines, lengths_m, keep) where keep is a logical index into the
#'   input vectors indicating which pairs survived the distance cap
build_nearest_point_lines <- function(from_geoms, to_geoms, crs,
                                      max_dist_m = Inf) {
  raw <- lapply(seq_along(from_geoms), function(i) {
    line <- sf::st_geometry(sf::st_nearest_points(from_geoms[i], to_geoms[i]))[[1]]
    len  <- as.numeric(sf::st_length(sf::st_sfc(line, crs = crs)))
    list(line = line, len = len)
  })

  lengths <- vapply(raw, `[[`, numeric(1), "len")
  keep    <- lengths <= max_dist_m

  list(
    lines     = sf::st_sfc(lapply(raw[keep], `[[`, "line"), crs = crs),
    lengths_m = lengths[keep],
    keep      = keep
  )
}

#' Validate nearest-points lines as genuine ROW crossings
#'
#' A line is valid if the fraction of its length that lies within the ROW
#' meets min_coverage_ratio. For a genuine cross-road connection the
#' nearest-points line runs directly from one parcel boundary across the road
#' to the other, so ROW_length / total_length should be close to 1.
#'
#' A line running along a street between same-side parcels scores low because
#' most of its length lies outside the ROW polygon.
#'
#' Endpoint-parcel lengths are intentionally excluded from the coverage
#' calculation: with nearest-points lines the endpoints sit on the parcel
#' boundary, so the within-parcel portion of the line is near-zero for valid
#' connections and including it would mask same-side false positives where the
#' line clips through a corner of the ROW polygon.
#'
#' @param lines_sfc      sfc of LINESTRINGs
#' @param line_lengths_m Pre-computed line lengths in metres (from
#'   build_nearest_point_lines — avoids recomputing)
#' @param row_union      Unioned ROW sfc
#' @param min_coverage_ratio Minimum ROW_length / total_length (default 0.9)
#' @return logical vector, TRUE where line is a valid ROW crossing
validate_row_crossing_lines <- function(lines_sfc, line_lengths_m,
                                        row_union,
                                        min_coverage_ratio = 0.9) {
  n     <- length(lines_sfc)
  valid <- logical(n)
  if (n == 0) return(valid)

  # Pre-filter: only intersect lines that actually cross the ROW at all
  crosses_row  <- sf::st_intersects(lines_sfc, row_union, sparse = FALSE)[, 1]
  crossing_idx <- which(crosses_row)
  if (length(crossing_idx) == 0) return(valid)

  # Vectorised intersection with ROW for all crossing lines at once
  crossing_lines    <- lines_sfc[crossing_idx]
  row_intersections <- sf::st_intersection(crossing_lines, row_union)

  row_lengths <- vapply(seq_along(row_intersections), function(i) {
    sum(as.numeric(sf::st_length(row_intersections[i])))
  }, numeric(1))

  coverage <- row_lengths / line_lengths_m[crossing_idx]
  passes   <- coverage >= min_coverage_ratio

  valid[crossing_idx[passes]] <- TRUE
  valid
}

# ============================================================================
# MAIN FUNCTIONS
# ============================================================================

#' Find cross-ROW edges for a set of candidate parcel pairs
#'
#' Given candidate pairs (already filtered to non-touching), validates them as
#' genuine cross-ROW connections using nearest-points lines and a ROW coverage
#' ratio. Parcels not within row_gap_tolerance_m of the ROW are excluded early
#' as they cannot participate in cross-ROW connections.
#'
#' @param parcels_sf       sf with LOC_ID and geometry for ALL district parcels
#' @param candidate_from   integer indices into parcels_sf for "from" parcels
#' @param candidate_to     integer indices into parcels_sf for "to" parcels
#' @param row_union        Pre-computed unioned ROW sfc
#' @param max_dist_m       Hard cap on nearest-points line length in metres
#' @param min_coverage_ratio Minimum ROW coverage fraction (default 0.9)
#' @param row_gap_tolerance_m Tolerance for parcel-to-ROW gap (data quality) (default 1m)
#' @param verbose          Print progress messages
#' @return logical vector (length == nrow candidates), TRUE for valid ROW edges
find_row_edges <- function(parcels_sf, candidate_from, candidate_to,
                           row_union, max_dist_m,
                           min_coverage_ratio = 0.9,
                           row_gap_tolerance_m = 1,
                           verbose = TRUE) {

  n_candidates <- length(candidate_from)
  valid <- logical(n_candidates)

  if (n_candidates == 0) return(valid)

  all_geoms <- sf::st_geometry(parcels_sf)

  # ---- Filter to pairs where BOTH parcels are near the ROW ----
  # Parcels with no proximity to ROW cannot have a cross-ROW connection.
  # Uses a small gap tolerance to handle hairline mismatches between parcel
  # and ROW datasets from different sources.
  near_row <- sf::st_is_within_distance(
    parcels_sf, row_union,
    dist   = row_gap_tolerance_m,
    sparse = FALSE
  )[, 1]

  both_near_row <- near_row[candidate_from] & near_row[candidate_to]
  row_candidate_idx <- which(both_near_row)

  n_near <- sum(both_near_row)
  n_excluded <- n_candidates - n_near
  if (verbose) message(glue::glue(
    "  {n_near} candidate pairs with both parcels near ROW",
    " ({n_excluded} excluded — not near ROW)"
  ))

  if (n_near == 0) return(valid)

  # ---- Build nearest-points lines with hard distance cap ----
  from_geoms <- all_geoms[candidate_from[row_candidate_idx]]
  to_geoms   <- all_geoms[candidate_to[row_candidate_idx]]

  np <- build_nearest_point_lines(
    from_geoms, to_geoms,
    crs        = sf::st_crs(parcels_sf),
    max_dist_m = max_dist_m
  )

  n_capped <- sum(!np$keep)
  if (verbose && n_capped > 0) message(glue::glue(
    "  {n_capped} pairs removed by distance cap"
  ))

  if (length(np$lines) == 0) return(valid)

  within_cap_idx <- row_candidate_idx[np$keep]

  # ---- Validate ROW coverage ratio ----
  row_valid <- validate_row_crossing_lines(
    lines_sfc          = np$lines,
    line_lengths_m     = np$lengths_m,
    row_union          = row_union,
    min_coverage_ratio = min_coverage_ratio
  )

  valid[within_cap_idx[row_valid]] <- TRUE
  valid
}

#' Build adjacency graph from parcel geometries
#'
#' Creates an igraph where parcels are vertices and edges represent genuine
#' spatial adjacency — either direct touching or cross-ROW connection.
#'
#' A single candidate generation step buffers all parcels by max_dist_ft/2 and
#' finds overlapping pairs. Touching pairs (boundary distance <= touch_threshold_ft)
#' become direct edges. Non-touching pairs are tested as potential cross-ROW
#' connections via find_row_edges().
#'
#' Note: Assumes geometry_sf uses EPSG:26986 (metres). All threshold parameters
#' are in feet and converted internally.
#'
#' @param geometry_sf      sf with LOC_ID, geometry, capacity, area
#' @param right_of_way_sf  Optional sf with ROW polygons
#' @param max_dist_ft      Maximum boundary-to-boundary distance (ft) for any
#'   connection — controls both candidate generation and the nearest-points cap.
#'   Default 120 ft.
#' @param touch_threshold_ft Distance (ft) below which parcels are considered
#'   directly touching (default 2 ft)
#' @param min_coverage_ratio Minimum fraction of nearest-points line within ROW
#'   to accept a cross-ROW connection (default 0.9)
#' @param verbose          Print progress messages
#' @return igraph with edge attribute is_row_crossing
#' @export
build_adjacency_graph <- function(geometry_sf,
                                  right_of_way_sf    = NULL,
                                  max_dist_ft        = 120,
                                  touch_threshold_ft = 2,
                                  min_coverage_ratio = 0.9,
                                  verbose            = TRUE) {
  FEET_TO_METERS <- 0.3048
  max_dist_m        <- max_dist_ft        * FEET_TO_METERS
  touch_threshold_m <- touch_threshold_ft * FEET_TO_METERS

  # ---- Candidate generation ----
  # Buffer by half the max distance so overlapping buffers <=> pairs within
  # max_dist_ft of each other (boundary-to-boundary).
  if (verbose) message("Building candidate adjacency edges...")

  geom_buf     <- sf::st_buffer(geometry_sf, dist = max_dist_m / 2)
  buf_pairs    <- sf::st_intersects(geom_buf, sparse = TRUE)

  candidates <- data.table::rbindlist(
    lapply(seq_along(buf_pairs), function(i) {
      j <- buf_pairs[[i]]
      j <- j[j > i]
      if (length(j) == 0) return(NULL)
      data.table::data.table(from_idx = i, to_idx = j)
    })
  )

  if (verbose) message(glue::glue("  {nrow(candidates)} candidate pairs"))

  if (nrow(candidates) == 0) {
    vertices_df <- as.data.frame(geometry_sf)
    vertices_df$geometry <- NULL
    return(igraph::graph_from_data_frame(
      data.frame(from = character(), to = character(), is_row_crossing = logical()),
      directed = FALSE, vertices = vertices_df
    ))
  }

  # ---- Boundary-to-boundary distances ----
  if (verbose) message("Computing boundary distances...")

  all_geoms  <- sf::st_geometry(geometry_sf)
  from_geoms <- all_geoms[candidates$from_idx]
  to_geoms   <- all_geoms[candidates$to_idx]

  distances <- as.numeric(
    sf::st_distance(from_geoms, to_geoms, by_element = TRUE)
  )

  is_touching    <- distances <= touch_threshold_m
  is_nontouching <- !is_touching

  n_touch    <- sum(is_touching)
  n_nontouch <- sum(is_nontouching)
  if (verbose) message(glue::glue(
    "  {n_touch} touching pairs, {n_nontouch} non-touching pairs to validate"
  ))

  # ---- Cross-ROW validation for non-touching pairs ----
  is_row_edge <- logical(nrow(candidates))

  if (n_nontouch > 0 && !is.null(right_of_way_sf) && nrow(right_of_way_sf) > 0) {

    if (verbose) message("Computing ROW union...")
    row_union <- sf::st_union(right_of_way_sf)

    nontouch_idx <- which(is_nontouching)

    if (verbose) message(glue::glue(
      "Validating {n_nontouch} non-touching pairs as cross-ROW edges..."
    ))

    row_valid <- find_row_edges(
      parcels_sf         = geometry_sf,
      candidate_from     = candidates$from_idx[nontouch_idx],
      candidate_to       = candidates$to_idx[nontouch_idx],
      row_union          = row_union,
      max_dist_m         = max_dist_m,
      min_coverage_ratio = min_coverage_ratio,
      verbose            = verbose
    )

    is_row_edge[nontouch_idx[row_valid]] <- TRUE
  }

  # ---- Assemble edge table ----
  keep <- is_touching | is_row_edge

  LOC_IDs <- geometry_sf$LOC_ID
  edges <- data.table::data.table(
    from            = LOC_IDs[candidates$from_idx[keep]],
    to              = LOC_IDs[candidates$to_idx[keep]],
    is_row_crossing = is_row_edge[keep]
  )

  if (verbose) message(glue::glue(
    "Total: {nrow(edges)} edges ",
    "({sum(is_touching)} direct, {sum(is_row_edge)} cross-ROW)"
  ))

  # Strip geometry before building igraph (significant memory/speed saving)
  vertices_df <- as.data.frame(geometry_sf)
  vertices_df$geometry <- NULL

  igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = vertices_df
  )
}
