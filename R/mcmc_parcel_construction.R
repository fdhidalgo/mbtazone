# parcel_construction.R - Parcel graph construction
#
# Implements optional coarsening of parcel graph into units via region-growing
# clustering. Large parcels (>= 5 acres) are preserved as singleton units.
#
# Two-level hierarchy:
# - Original parcels (many, small)
# - Units (fewer, ~0.25-1 acre each when aggregated; 1:1 when MACRO_SCALE=0)

# ============================================================================
# ATOMIC PARCEL IDENTIFICATION
# ============================================================================

#' Identify parcels to preserve as singleton units
#'
#' Parcels with area >= threshold are "atomic" - they become their own
#' unit without being merged with neighbors. This preserves geographic
#' granularity for large parcels that can serve as standalone secondary
#' components.
#'
#' @param g igraph object with area vertex attribute
#' @param area_threshold Minimum area (acres) for singleton treatment
#' @return Character vector of parcel IDs (LOC_IDs) to freeze as singletons
identify_atomic_parcels <- function(g, area_threshold = MACRO_ATOMIC_THRESHOLD) {
  areas <- igraph::V(g)$area
  names <- igraph::V(g)$name
  names[areas >= area_threshold]
}

# ============================================================================
# REGION-GROWING CLUSTERING
# ============================================================================

#' Cluster parcels into units via region-growing
#'
#' Uses BFS-based region growing to aggregate small parcels into units
#' with target area in [area_min, area_max]. Atomic parcels (large parcels)
#' are excluded from clustering and become singleton units.
#'
#' Algorithm:
#' 1. Assign atomic parcels as singleton units
#' 2. Pick random seed from unassigned parcels
#' 3. BFS expansion until target area reached
#' 4. Repeat until all parcels assigned
#' 5. Attach any leftover parcels to adjacent units
#'
#' @param g igraph object with area vertex attribute
#' @param atomic_parcels Character vector of parcels to freeze as singletons
#' @param target_area_min Minimum unit area (acres)
#' @param target_area_max Maximum unit area (acres)
#' @param seed Random seed for reproducibility
#' @return data.table with columns: parcel_id, unit_id
cluster_parcels_to_units <- function(g,
                                       atomic_parcels,
                                       target_area_min = MACRO_TARGET_AREA_MIN,
                                       target_area_max = MACRO_TARGET_AREA_MAX,
                                       seed = 123) {
  set.seed(seed)

  all_parcels <- igraph::V(g)$name
  n_parcels <- length(all_parcels)

  # Initialize assignments
  assignments <- data.table::data.table(
    parcel_id = all_parcels,
    unit_id = NA_character_
  )

  # Step 1: Assign atomic parcels as singletons (M0001, M0002, ...)
  # Track these IDs to protect them from merges
  unit_counter <- 0L
  atomic_unit_ids <- character(length(atomic_parcels))
  for (i in seq_along(atomic_parcels)) {
    ap <- atomic_parcels[i]
    unit_counter <- unit_counter + 1L
    mid <- sprintf("M%04d", unit_counter)
    assignments[parcel_id == ap, unit_id := mid]
    atomic_unit_ids[i] <- mid
  }

  # Parcels eligible for clustering
  eligible <- setdiff(all_parcels, atomic_parcels)
  unassigned <- eligible

  # Precompute areas for fast lookup
  area_lookup <- igraph::V(g)$area
  names(area_lookup) <- igraph::V(g)$name

  # Step 2-4: Region-growing for remaining parcels
  while (length(unassigned) > 0) {
    # Start new unit
    unit_counter <- unit_counter + 1L
    unit_id <- sprintf("M%04d", unit_counter)

    # BFS expansion (seed and frontier restricted to unassigned parcels)
    result <- bfs_grow_block(
      graph = g,
      metric_lookup = area_lookup,
      seed_pool = unassigned,
      eligible_pool = unassigned,
      target_min = target_area_min,
      target_max = target_area_max,
      check_max_before_add = TRUE
    )

    current_unit <- result$block
    current_area <- result$metric_total

    # Check if unit is undersized - if so, merge into adjacent non-atomic unit
    if (current_area < target_area_min) {
      # Find adjacent units that are already assigned
      all_neighbors <- character(0)
      for (p in current_unit) {
        nbrs <- igraph::neighbors(g, p)$name
        all_neighbors <- c(all_neighbors, nbrs)
      }
      all_neighbors <- unique(all_neighbors)
      all_neighbors <- setdiff(all_neighbors, current_unit)

      adjacent_assigned <- assignments[parcel_id %in% all_neighbors & !is.na(unit_id)]
      if (nrow(adjacent_assigned) > 0) {
        # Exclude atomic units from merge targets (preserve singletons)
        adjacent_units <- unique(adjacent_assigned$unit_id)
        adjacent_units <- setdiff(adjacent_units, atomic_unit_ids)

        if (length(adjacent_units) > 0) {
          # Compute areas of candidate units
          unit_areas <- vapply(adjacent_units, function(mid) {
            unit_parcels <- assignments[unit_id == mid, parcel_id]
            sum(area_lookup[unit_parcels])
          }, numeric(1))
          names(unit_areas) <- adjacent_units

          # Prefer neighbors where merge stays within bounds
          combined_areas <- unit_areas + current_area
          within_bounds <- adjacent_units[combined_areas <= target_area_max]

          if (length(within_bounds) > 0) {
            # Pick neighbor that brings combined area closest to target midpoint
            target_mid <- (target_area_min + target_area_max) / 2
            best_neighbor <- within_bounds[which.min(abs(combined_areas[within_bounds] - target_mid))]
          } else {
            # No neighbor stays within bounds - pick smallest to minimize overshoot
            best_neighbor <- adjacent_units[which.min(unit_areas)]
          }

          # Merge current parcels into the neighbor unit
          assignments[parcel_id %in% current_unit, unit_id := best_neighbor]
          unassigned <- setdiff(unassigned, current_unit)
          unit_counter <- unit_counter - 1L
          next
        }
      }
      # If no eligible adjacent units, proceed with undersized unit
    }

    # Assign parcels to this unit
    assignments[parcel_id %in% current_unit, unit_id := unit_id]
    unassigned <- setdiff(unassigned, current_unit)
  }

  # Step 5: Handle any leftover parcels (isolated or missed)
  # This shouldn't happen with connected graphs, but handle gracefully
  leftovers <- assignments[is.na(unit_id), parcel_id]
  if (length(leftovers) > 0) {
    warning(sprintf("Found %d leftover parcels after clustering", length(leftovers)))
    for (leftover in leftovers) {
      # Attach to adjacent non-atomic unit
      neighbors <- igraph::neighbors(g, leftover)$name
      neighbor_units <- assignments[parcel_id %in% neighbors & !is.na(unit_id), unit_id]
      # Exclude atomic units
      neighbor_units <- setdiff(neighbor_units, atomic_unit_ids)
      if (length(neighbor_units) > 0) {
        leftover_area <- area_lookup[leftover]
        unique_neighbors <- unique(neighbor_units)
        unit_areas <- vapply(unique_neighbors, function(mid) {
          sum(area_lookup[assignments[unit_id == mid, parcel_id]])
        }, numeric(1))
        names(unit_areas) <- unique_neighbors

        # Prefer neighbors where adding leftover stays within bounds
        combined_areas <- unit_areas + leftover_area
        within_bounds <- unique_neighbors[combined_areas <= target_area_max]

        if (length(within_bounds) > 0) {
          # Pick neighbor closest to target midpoint after merge
          target_mid <- (target_area_min + target_area_max) / 2
          chosen_unit <- within_bounds[
            which.min(abs(combined_areas[within_bounds] - target_mid))
          ]
        } else {
          # No neighbor stays within bounds - pick smallest to minimize overshoot
          chosen_unit <- unique_neighbors[which.min(unit_areas)]
        }
        assignments[parcel_id == leftover, unit_id := chosen_unit]
      } else {
        # Create new singleton unit (no eligible neighbors)
        unit_counter <- unit_counter + 1L
        new_id <- sprintf("M%04d", unit_counter)
        assignments[parcel_id == leftover, unit_id := new_id]
      }
    }
  }

  assignments
}

# ============================================================================
# PARCEL ATTRIBUTE COMPUTATION
# ============================================================================

#' Compute aggregated attributes for each unit
#'
#' Sums area and capacity over constituent parcels. Also tracks whether
#' a unit is a singleton (single large parcel preserved as-is).
#'
#' @param g igraph object with area and capacity vertex attributes
#' @param parcel_assignments data.table from cluster_parcels_to_units()
#' @param atomic_threshold Area threshold for singleton classification
#' @return data.table with unit_id, area, capacity, n_parcels, is_singleton, parcel_ids
compute_parcel_attributes <- function(g,
                                       parcel_assignments,
                                       atomic_threshold = MACRO_ATOMIC_THRESHOLD) {
  # Get parcel attributes
  parcel_attrs <- data.table::data.table(
    parcel_id = igraph::V(g)$name,
    area = igraph::V(g)$area,
    capacity = igraph::V(g)$capacity,
    area_in_station = igraph::V(g)$area_in_station,
    capacity_in_station = igraph::V(g)$capacity_in_station,
    in_station_bounds = igraph::V(g)$in_station_bounds
  )

  # Join with parcel assignments
  merged <- merge(parcel_assignments, parcel_attrs, by = "parcel_id")

  # Aggregate by unit
  unit_attrs <- merged[, .(
    area = sum(area),
    area_in_station = sum(area_in_station),
    capacity = sum(capacity),
    capacity_in_station = sum(capacity_in_station),
    n_parcels = .N,
    parcel_ids = list(parcel_id)
  ), by = unit_id]

  # Mark singletons (single parcel >= atomic threshold)
  unit_attrs[, is_singleton := n_parcels == 1 & area >= atomic_threshold]

  # Compute centroids (unweighted mean of parcel centroids)
  if ("centroid_x" %in% igraph::vertex_attr_names(g)) {
    centroid_lookup_x <- igraph::V(g)$centroid_x
    centroid_lookup_y <- igraph::V(g)$centroid_y
    names(centroid_lookup_x) <- igraph::V(g)$name
    names(centroid_lookup_y) <- igraph::V(g)$name

    unit_attrs[, centroid_x := mean(centroid_lookup_x[unlist(parcel_ids)]), by = unit_id]
    unit_attrs[, centroid_y := mean(centroid_lookup_y[unlist(parcel_ids)]), by = unit_id]
  }

  unit_attrs
}

# ============================================================================
# PARCEL GRAPH CONSTRUCTION
# ============================================================================

#' Build unit-level adjacency graph from parcel adjacencies
#'
#' Two units are adjacent if any of their constituent parcels are adjacent
#' in the original parcel graph.
#'
#' @param g igraph object (raw parcel graph)
#' @param parcel_assignments data.table from cluster_parcels_to_units()
#' @param parcel_attributes data.table from compute_parcel_attributes()
#' @return igraph object with unit_id as vertex names
build_parcel_graph <- function(g, parcel_assignments, parcel_attributes) {
  # Create parcel-to-unit lookup
  parcel_to_unit <- parcel_assignments$unit_id
  names(parcel_to_unit) <- parcel_assignments$parcel_id

  # Get all edges from raw parcel graph
  edges <- igraph::as_edgelist(g, names = TRUE)

  # Map to unit IDs
  unit_edges <- data.table::data.table(
    from_unit = parcel_to_unit[edges[, 1]],
    to_unit = parcel_to_unit[edges[, 2]]
  )

  # Remove self-loops (edges within same unit)
  unit_edges <- unit_edges[from_unit != to_unit]

  # Remove duplicates
  unit_edges <- unique(unit_edges)

  # Create edge list for igraph
  edge_vector <- as.vector(t(as.matrix(unit_edges)))

  # Create graph
  unit_ids <- parcel_attributes$unit_id
  parcel_graph <- igraph::make_empty_graph(n = length(unit_ids), directed = FALSE)
  igraph::V(parcel_graph)$name <- unit_ids

  # Add edges
  if (nrow(unit_edges) > 0) {
    parcel_graph <- igraph::add_edges(parcel_graph, edge_vector)
  }

  # Add vertex attributes
  attr_order <- match(igraph::V(parcel_graph)$name, parcel_attributes$unit_id)
  igraph::V(parcel_graph)$area <- parcel_attributes$area[attr_order]
  igraph::V(parcel_graph)$capacity <- parcel_attributes$capacity[attr_order]
  igraph::V(parcel_graph)$area_in_station <- parcel_attributes$area_in_station[attr_order]
  igraph::V(parcel_graph)$capacity_in_station <- parcel_attributes$capacity_in_station[attr_order]
  igraph::V(parcel_graph)$n_parcels <- parcel_attributes$n_parcels[attr_order]
  igraph::V(parcel_graph)$is_singleton <- parcel_attributes$is_singleton[attr_order]

  if ("centroid_x" %in% names(parcel_attributes)) {
    igraph::V(parcel_graph)$centroid_x <- parcel_attributes$centroid_x[attr_order]
    igraph::V(parcel_graph)$centroid_y <- parcel_attributes$centroid_y[attr_order]
  }

  parcel_graph
}

# ============================================================================
# IDENTITY PARCEL GRAPH (NO AGGREGATION)
# ============================================================================

#' Build identity parcel graph (no aggregation)
#'
#' Creates a parcel graph where each parcel is its own unit.
#' Used when MACRO_SCALE = 0 for raw parcel-level analysis.
#'
#' @param g igraph object (raw parcel graph) with area, capacity vertex attributes
#' @return List matching build_parcel_graph_target() structure for API compatibility
#' @export
build_identity_parcel_graph <- function(g) {
  cli::cli_h2("Building identity parcel graph (no aggregation)")

  parcel_names <- igraph::V(g)$name
  n <- length(parcel_names)

  # 1:1 mapping: each parcel becomes its own unit
  parcel_to_unit <- setNames(
    sprintf("M%04d", seq_len(n)),
    parcel_names
  )

  # Build parcel_assignments table (matches cluster_parcels_to_units output)
  parcel_assignments <- data.table::data.table(
    parcel_id = parcel_names,
    unit_id = parcel_to_unit[parcel_names]
  )

  # Clone graph and relabel vertices to unit IDs
  parcel_graph <- g
  igraph::V(parcel_graph)$name <- parcel_to_unit[igraph::V(g)$name]
  igraph::V(parcel_graph)$is_singleton <- TRUE
  igraph::V(parcel_graph)$n_parcels <- 1L

  # Attributes table (matches compute_parcel_attributes output)
  parcel_attributes <- data.table::data.table(
    unit_id = igraph::V(parcel_graph)$name,
    area = igraph::V(g)$area,
    area_in_station = igraph::V(g)$area_in_station,
    capacity = igraph::V(g)$capacity,
    capacity_in_station = igraph::V(g)$capacity_in_station,
    is_singleton = TRUE,
    n_parcels = 1L
  )

  # Build neighbor cache
  unit_names <- igraph::V(parcel_graph)$name
  neighbor_cache <- lapply(unit_names, function(m) {
    igraph::neighbors(parcel_graph, m)$name
  })
  names(neighbor_cache) <- unit_names

  cli::cli_alert_success(
    "Created {n} units from {n} parcels (compression ratio: 1.0x)"
  )
  cli::cli_alert_info("Unit area range: {round(min(parcel_attributes$area), 2)} - {round(max(parcel_attributes$area), 2)} acres")
  cli::cli_alert_info("All {n} units are singletons (1 parcel each)")

  list(
    parcel_graph = parcel_graph,
    parcel_assignments = parcel_assignments,
    parcel_attributes = parcel_attributes,
    adjacency_graph = g,
    neighbor_cache = neighbor_cache
  )
}

# ============================================================================
# TARGET WRAPPER
# ============================================================================

#' Build parcel graph pipeline (targets wrapper)
#'
#' Full pipeline to construct parcel representation from raw adjacency graph.
#'
#' @param adjacency_graph igraph object (raw parcel graph)
#' @param target_area_min Minimum unit area (acres)
#' @param target_area_max Maximum unit area (acres)
#' @param atomic_threshold Area threshold for singleton preservation
#' @param seed Random seed
#' @return List with parcel_graph, parcel_assignments, parcel_attributes, adjacency_graph, neighbor_cache
#' @export
build_parcel_graph_target <- function(adjacency_graph,
                                       target_area_min = MACRO_TARGET_AREA_MIN,
                                       target_area_max = MACRO_TARGET_AREA_MAX,
                                       atomic_threshold = MACRO_ATOMIC_THRESHOLD,
                                       seed = 123) {
  cli::cli_h2("Building parcel graph")

  # Step 1: Identify atomic parcels
  atomic_parcels <- identify_atomic_parcels(adjacency_graph, atomic_threshold)
  cli::cli_alert_info("Found {length(atomic_parcels)} atomic parcels (area >= {atomic_threshold} acres)")

  # Step 2: Cluster remaining parcels
  cli::cli_alert_info("Clustering parcels into units (target area: {target_area_min}-{target_area_max} acres)")
  parcel_assignments <- cluster_parcels_to_units(
    adjacency_graph,
    atomic_parcels,
    target_area_min,
    target_area_max,
    seed
  )

  n_units <- length(unique(parcel_assignments$unit_id))
  n_parcels <- nrow(parcel_assignments)
  cli::cli_alert_success("Created {n_units} units from {n_parcels} parcels (compression ratio: {round(n_parcels/n_units, 1)}x)")

  # Step 3: Compute parcel attributes
  parcel_attributes <- compute_parcel_attributes(
    adjacency_graph, parcel_assignments, atomic_threshold
  )

  # Report statistics
  cli::cli_alert_info("Unit area range: {round(min(parcel_attributes$area), 2)} - {round(max(parcel_attributes$area), 2)} acres")
  cli::cli_alert_info("Unit capacity range: {min(parcel_attributes$capacity)} - {max(parcel_attributes$capacity)} units")
  cli::cli_alert_info("Singletons: {sum(parcel_attributes$is_singleton)}")

  # Step 4: Build parcel graph
  parcel_graph <- build_parcel_graph(adjacency_graph, parcel_assignments, parcel_attributes)

  n_edges <- igraph::ecount(parcel_graph)
  avg_degree <- mean(igraph::degree(parcel_graph))
  cli::cli_alert_success("Parcel graph: {n_units} vertices, {n_edges} edges (avg degree: {round(avg_degree, 1)})")

  # Step 5: Build neighbor cache for fast lookups during MCMC
  unit_names <- igraph::V(parcel_graph)$name
  neighbor_cache <- lapply(unit_names, function(m) igraph::neighbors(parcel_graph, m)$name)
  names(neighbor_cache) <- unit_names
  cli::cli_alert_info("Built neighbor cache for {length(unit_names)} units")

  list(
    parcel_graph = parcel_graph,
    parcel_assignments = parcel_assignments,
    parcel_attributes = parcel_attributes,
    adjacency_graph = adjacency_graph,
    neighbor_cache = neighbor_cache
  )
}
