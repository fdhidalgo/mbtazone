# graph_building.R - Adjacency graph construction and boundary computation
#
# Functions for building parcel adjacency graphs with ROW support
# and for computing/maintaining boundary state during MCMC.

# ============================================================================
# ADJACENCY GRAPH CONSTRUCTION
# ============================================================================

#' Validate direct adjacency edges (vectorized)
#'
#' Filters candidate edges from buffered intersection to only keep edges where
#' parcels either physically touch OR are separated by a valid ROW crossing.
#' This prevents spurious edges between parcels that are merely close but not
#' truly adjacent.
#'
#' Note: Assumes geometry_sf uses EPSG:26986 (meters). Thresholds are specified
#' in feet and converted internally.
#'
#' @param edges data.table with "from" and "to" columns (parcel IDs)
#' @param geometry_sf sf object with LOC_ID and geometry (must be in meters CRS)
#' @param right_of_way_sf Optional sf object with ROW polygons
#' @param touch_threshold_ft Max distance (ft) to consider as "touching" (default 2)
#' @param row_min_crossing_ft Min ROW crossing length (ft) to validate cross-ROW edge
#' @param verbose Print progress messages
#' @return Filtered data.table of validated edges
validate_direct_edges <- function(edges, geometry_sf, right_of_way_sf = NULL,
                                   touch_threshold_ft = 2,
                                   row_min_crossing_ft = ROW_MIN_CROSSING_LENGTH,
                                   verbose = TRUE) {
  if (nrow(edges) == 0) return(edges)

  if (verbose) message("Validating direct adjacency edges (vectorized)...")


  # ---- Unit conversion: feet to meters (EPSG:26986 uses meters) ----
  FEET_TO_METERS <- 0.3048

  touch_threshold_m <- touch_threshold_ft * FEET_TO_METERS
  row_min_crossing_m <- row_min_crossing_ft * FEET_TO_METERS

  # Pre-compute ROW union if available
  row_union <- NULL
  if (!is.null(right_of_way_sf) && nrow(right_of_way_sf) > 0) {
    if (verbose) message("  Computing ROW union...")
    row_union <- sf::st_union(right_of_way_sf)
  }

  n_edges <- nrow(edges)
  keep <- logical(n_edges)
  is_row_crossing <- logical(n_edges)

  # ---- Build index lookup ----
  geom_idx <- stats::setNames(seq_len(nrow(geometry_sf)), geometry_sf$LOC_ID)
  from_idx <- geom_idx[edges$from]
  to_idx <- geom_idx[edges$to]

  # Identify valid edges (both parcels have geometry)
  valid_geom <- !is.na(from_idx) & !is.na(to_idx)
  valid_indices <- which(valid_geom)

  if (length(valid_indices) == 0) {
    if (verbose) message("  No valid edges to process")
    result <- edges[keep, ]
    result$is_row_crossing <- logical(0)
    return(result)
  }

  # ---- Vectorized distance computation ----
  if (verbose) message(glue::glue("  Computing distances for {length(valid_indices)} edges..."))

  from_geoms <- sf::st_geometry(geometry_sf)[from_idx[valid_indices]]
  to_geoms <- sf::st_geometry(geometry_sf)[to_idx[valid_indices]]

  # Single vectorized distance call
  distances <- as.numeric(sf::st_distance(from_geoms, to_geoms, by_element = TRUE))

  # ---- First filter: touching edges ----
  is_touching <- distances <= touch_threshold_m
  keep[valid_indices[is_touching]] <- TRUE

  n_touching <- sum(is_touching)
  if (verbose) message(glue::glue("  Found {n_touching} touching edges"))

  # ---- ROW crossing validation for non-touching edges ----
  non_touching_mask <- !is_touching
  non_touching_local_idx <- which(non_touching_mask)
  non_touching_global_idx <- valid_indices[non_touching_mask]

  if (length(non_touching_global_idx) > 0 && !is.null(row_union)) {
    if (verbose) message(glue::glue("  Checking ROW crossings for {length(non_touching_global_idx)} non-touching edges..."))

    # Get geometries for non-touching edges
    from_geoms_nt <- from_geoms[non_touching_local_idx]
    to_geoms_nt <- to_geoms[non_touching_local_idx]

    # Compute centroids (vectorized)
    centroids_from <- sf::st_centroid(from_geoms_nt)
    centroids_to <- sf::st_centroid(to_geoms_nt)

    coords_from <- sf::st_coordinates(centroids_from)
    coords_to <- sf::st_coordinates(centroids_to)

    # Build all connecting lines at once
    n_check <- length(non_touching_global_idx)
    lines <- lapply(seq_len(n_check), function(i) {
      sf::st_linestring(matrix(c(
        coords_from[i, 1], coords_to[i, 1],
        coords_from[i, 2], coords_to[i, 2]
      ), ncol = 2))
    })
    lines_sfc <- sf::st_sfc(lines, crs = sf::st_crs(geometry_sf))

    # Single vectorized intersection test
    crosses_row <- sf::st_intersects(lines_sfc, row_union, sparse = FALSE)[, 1]
    crossing_idx <- which(crosses_row)

    if (length(crossing_idx) > 0) {
      if (verbose) message(glue::glue("  Validating {length(crossing_idx)} ROW crossings..."))

      # Compute intersection lengths only for crossing lines
      crossing_lines <- lines_sfc[crossing_idx]
      intersections <- sf::st_intersection(crossing_lines, row_union)

      # FIX: Sum all segment lengths for multi-segment intersections
      # st_intersection can return multiple segments; st_length returns a vector
      intersection_lengths <- vapply(seq_along(intersections), function(i) {
        sum(as.numeric(sf::st_length(intersections[i])))
      }, numeric(1))

      valid_crossings <- intersection_lengths >= row_min_crossing_m
      keep[non_touching_global_idx[crossing_idx[valid_crossings]]] <- TRUE
      is_row_crossing[non_touching_global_idx[crossing_idx[valid_crossings]]] <- TRUE

      n_valid_crossings <- sum(valid_crossings)
      if (verbose) message(glue::glue("  Found {n_valid_crossings} valid ROW crossings"))
    }
  }

  # ---- Report and return ----
  n_removed <- sum(!keep)
  n_kept <- sum(keep)
  if (verbose) {
    message(glue::glue("  Removed {n_removed} spurious edges (kept {n_kept})"))
  }

  result <- edges[keep, ]
  result$is_row_crossing <- is_row_crossing[keep]
  result
}

#' Find cross-ROW edges between parcels
#'
#' Finds pairs of parcels that are separated by right-of-way (roads/railways)
#' and should be considered adjacent.
#'
#' @param parcels_sf sf object with LOC_ID and geometry
#' @param row_sf sf object with ROW polygons
#' @param proximity_ft Maximum distance for cross-ROW adjacency
#' @param min_crossing_ft Minimum ROW crossing length
#' @param verbose Print progress messages
#' @return data.table with "from", "to", and "is_row_crossing" columns, or NULL if none found
find_cross_row_edges <- function(parcels_sf, row_sf, proximity_ft,
                                 min_crossing_ft = ROW_MIN_CROSSING_LENGTH,
                                 verbose = TRUE) {
  if (verbose) message("Finding parcels that touch ROW...")

 # ---- Unit conversion: feet to meters (EPSG:26986 uses meters) ----
  FEET_TO_METERS <- 0.3048
  proximity_m <- proximity_ft * FEET_TO_METERS
  min_crossing_m <- min_crossing_ft * FEET_TO_METERS

  row_union <- sf::st_union(row_sf)
  touches_row <- sf::st_intersects(parcels_sf, row_union, sparse = FALSE)[, 1]
  row_parcels <- parcels_sf[touches_row, ]
  n_row_parcels <- nrow(row_parcels)

  if (verbose) {
    msg <- "  {n_row_parcels} parcels touch ROW (of {nrow(parcels_sf)} total)"
    message(glue::glue(msg))
  }

  if (n_row_parcels < 2) return(NULL)

  if (verbose) message("Computing parcel centroids...")
  centroids <- sf::st_centroid(sf::st_geometry(row_parcels))

  if (verbose) {
    msg <- "Finding nearby pairs (within {proximity_ft} ft)..."
    message(glue::glue(msg))
  }
  centroid_buf <- sf::st_buffer(centroids, dist = proximity_m / 2)
  nearby_pairs <- sf::st_intersects(centroid_buf)

  candidates <- data.table::rbindlist(
    lapply(seq_along(nearby_pairs), function(i) {
      neighbors <- nearby_pairs[[i]]
      neighbors <- neighbors[neighbors > i]
      if (length(neighbors) == 0) return(NULL)
      data.table::data.table(i = i, j = neighbors)
    })
  )

  if (nrow(candidates) == 0) {
    if (verbose) message("  No candidate pairs found")
    return(NULL)
  }

  n_initial <- nrow(candidates)
  if (verbose) message(glue::glue("  {n_initial} nearby pairs found"))

  if (verbose) message("Filtering out already-adjacent pairs...")
  direct_adj <- sf::st_touches(row_parcels)

  keep <- sapply(seq_len(nrow(candidates)), function(idx) {
    i <- candidates$i[idx]
    j <- candidates$j[idx]
    !(j %in% direct_adj[[i]])
  })
  candidates <- candidates[keep, , drop = FALSE]

  if (nrow(candidates) == 0) {
    if (verbose) message("  No non-adjacent pairs found")
    return(NULL)
  }

  n_candidates <- nrow(candidates)
  if (verbose) {
    message(glue::glue("  {n_candidates} candidate pairs to test"))
  }

  if (verbose) message("Creating connecting lines (vectorized)...")
  coords <- sf::st_coordinates(centroids)

  lines <- lapply(seq_len(n_candidates), function(idx) {
    i <- candidates$i[idx]
    j <- candidates$j[idx]
    sf::st_linestring(matrix(c(coords[i, 1], coords[j, 1],
                                coords[i, 2], coords[j, 2]), ncol = 2))
  })
  lines_sfc <- sf::st_sfc(lines, crs = sf::st_crs(parcels_sf))

  if (verbose) message("Testing intersections with ROW (vectorized)...")
  crosses_row <- sf::st_intersects(lines_sfc, row_union, sparse = FALSE)[, 1]
  crossing_idx <- which(crosses_row)

  if (length(crossing_idx) == 0) {
    if (verbose) message("  No lines cross ROW")
    return(NULL)
  }

  if (verbose) message(glue::glue("  {length(crossing_idx)} lines cross ROW"))

  if (verbose) message(glue::glue("Filtering by intersection length (>= {min_crossing_ft} ft)..."))

  crossing_lines <- lines_sfc[crossing_idx]
  intersections <- sf::st_intersection(crossing_lines, row_union)
  intersection_lengths <- as.numeric(sf::st_length(intersections))

  valid <- intersection_lengths >= min_crossing_m
  final_idx <- crossing_idx[valid]

  n_found <- length(final_idx)
  if (verbose) {
    message(glue::glue("  {n_found} edges with >= {min_crossing_ft} ft crossing"))
  }

  if (n_found == 0) return(NULL)

  data.table::data.table(
    from = row_parcels$LOC_ID[candidates$i[final_idx]],
    to = row_parcels$LOC_ID[candidates$j[final_idx]],
    is_row_crossing = TRUE
  )
}

#' Build adjacency graph from parcel geometries
#'
#' Creates an igraph object with parcels as vertices and adjacency edges.
#' Edges are validated to ensure parcels either physically touch OR are
#' connected across a valid ROW crossing. This prevents spurious edges
#' between parcels that are merely close but not truly adjacent.
#'
#' @param geometry_sf sf object with LOC_ID, geometry, capacity, area
#' @param buffer_dist Buffer distance for candidate edge detection (default 10 feet)
#' @param right_of_way_sf Optional sf object with ROW polygons
#' @param row_proximity_ft Max distance for cross-ROW adjacency
#' @param row_min_crossing_ft Min ROW crossing length
#' @param touch_threshold_ft Max distance to consider as "touching" (default 2 feet)
#' @param verbose Print progress messages
#' @return igraph object with vertices from geometry_sf and edge attribute `is_row_crossing`
#' @export
build_adjacency_graph <- function(geometry_sf, buffer_dist = 10,
                                  right_of_way_sf = NULL,
                                  row_proximity_ft = ROW_PROXIMITY_THRESHOLD,
                                  row_min_crossing_ft = ROW_MIN_CROSSING_LENGTH,
                                  touch_threshold_ft = 2,
                                  verbose = TRUE) {
  if (verbose) message("Building candidate adjacency edges...")

  geometry_buf <- sf::st_buffer(geometry_sf, dist = buffer_dist)
  direct_adj <- sf::st_intersects(geometry_buf)

  edges <- data.table::rbindlist(
    lapply(seq_along(direct_adj), function(i) {
      neigh <- direct_adj[[i]]
      neigh <- neigh[neigh > i]
      if (length(neigh) == 0) return(NULL)
      data.table::data.table(
        from = geometry_sf$LOC_ID[i],
        to = geometry_sf$LOC_ID[neigh]
      )
    })
  )

  if (verbose) message(glue::glue("  Found {nrow(edges)} candidate edges"))

  # Validate edges: keep only those that physically touch OR cross ROW

  edges <- validate_direct_edges(
    edges = edges,
    geometry_sf = geometry_sf,
    right_of_way_sf = right_of_way_sf,
    touch_threshold_ft = touch_threshold_ft,
    row_min_crossing_ft = row_min_crossing_ft,
    verbose = verbose
  )

  if (verbose) message(glue::glue("After validation: {nrow(edges)} edges"))

  # Add additional cross-ROW edges for parcels beyond buffer distance
  # (These are parcels that are further apart but still across a road)
  if (!is.null(right_of_way_sf) && nrow(right_of_way_sf) > 0) {
    if (verbose) {
      message(glue::glue("Adding distant cross-ROW edges (proximity: {row_proximity_ft} ft)..."))
    }
    row_edges <- find_cross_row_edges(
      geometry_sf, right_of_way_sf, row_proximity_ft,
      min_crossing_ft = row_min_crossing_ft,
      verbose = verbose
    )
    if (!is.null(row_edges) && nrow(row_edges) > 0) {
      n_before <- nrow(edges)
      edges <- rbind(edges, row_edges)
      # Aggregate duplicates: is_row_crossing = TRUE if any source detected ROW crossing
      edges <- edges[, .(is_row_crossing = any(is_row_crossing)), by = .(from, to)]
      n_new <- nrow(edges) - n_before
      if (verbose) {
        message(glue::glue("  Added {n_new} new distant cross-ROW edges"))
      }
    }
  }

  if (verbose) message(glue::glue("Total: {nrow(edges)} edges"))

  # Optimization: Strip heavy geometry column to speed up igraph operations
  vertices_df <- as.data.frame(geometry_sf)
  if ("geometry" %in% names(vertices_df)) {
    vertices_df$geometry <- NULL
  }

  g <- igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = vertices_df
  )

  g
}
