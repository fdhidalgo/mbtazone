#' Adjacency Graph Construction
#'
#' Functions to build and analyze adjacency graphs from parcel spatial data.
#' These graphs represent which parcels share physical boundaries, using rook
#' adjacency (shared edges only, not corner touches) for robust contiguity.
#'
#' @name adjacency
NULL

#' Build Adjacency Graph from Parcel Data
#'
#' Constructs an adjacency graph from parcel spatial data using rook adjacency
#' (parcels must share an edge, not just a corner). This produces more robust
#' contiguity relationships than queen adjacency for zoning applications.
#'
#' @param parcels An sf object containing parcel polygons
#' @param quiet Logical indicating whether to suppress progress messages
#'   (default: FALSE)
#'
#' @return A list of integer vectors representing the adjacency graph:
#'   - Element `i` contains indices of all parcels adjacent to parcel `i`
#'   - Uses 1-based indexing (R convention)
#'   - Isolated parcels have empty integer vectors
#'   - The adjacency relationship is symmetric: if `j` is in `adj[[i]]`,
#'     then `i` is in `adj[[j]]`
#'
#' @details
#' **Rook vs Queen Adjacency:**
#'
#' This function uses **rook adjacency** via `sf::st_relate()` with the
#' DE-9IM pattern `"F***1****"`:
#' - Rook: Parcels must share a boundary (edge intersection)
#' - Queen: Parcels can touch at a single point (corner)
#'
#' Rook adjacency is more robust for MBTA zoning applications because:
#' - Avoids fragile single-point connections that may violate contiguity intent
#' - More conservative definition aligns with legal contiguity requirements
#' - Reduces risk of artificial corridors through corner-touch connections
#'
#' **Performance:**
#' - Complexity: O(n²) worst case, but typically much faster for real parcels
#' - Sparse representation: Returns adjacency list, not dense matrix
#' - For large municipalities (>5000 parcels): ~10-30 seconds
#' - Results can be cached in mbta_map object for reuse
#'
#' **Edge Cases:**
#' - Isolated parcels (no neighbors): Returns empty integer vector `integer(0)`
#' - Invalid geometries: Detected and reported with warning
#' - Self-loops: Not included in adjacency lists
#'
#' @examples
#' \dontrun{
#' # Load municipality parcel data
#' parcels <- load_municipality(
#'   "inst/extdata/parcels/57_CHELSEA_basic.zip",
#'   community_name = "Chelsea"
#' )
#'
#' # Build adjacency graph
#' adj <- build_adjacency(parcels)
#'
#' # Examine adjacency for first parcel
#' neighbors <- adj[[1]]
#' cat("Parcel 1 has", length(neighbors), "neighbors\n")
#'
#' # Verify symmetry
#' stopifnot(all(sapply(seq_along(adj), function(i) {
#'   all(sapply(adj[[i]], function(j) i %in% adj[[j]]))
#' })))
#' }
#'
#' @export
build_adjacency <- function(parcels, quiet = FALSE) {

  # Input validation
  if (!inherits(parcels, "sf")) {
    cli::cli_abort("{.arg parcels} must be an {.cls sf} object")
  }

  # Check geometry type (st_is returns a vector, need to check all geometries)
  is_poly <- sf::st_is(parcels, "POLYGON")
  is_multi <- sf::st_is(parcels, "MULTIPOLYGON")

  if (!all(is_poly | is_multi)) {
    cli::cli_abort("{.arg parcels} must contain POLYGON or MULTIPOLYGON geometry")
  }

  n_parcels <- nrow(parcels)

  if (n_parcels == 0) {
    cli::cli_abort("{.arg parcels} is empty (0 rows)")
  }

  # Check for invalid geometries
  invalid <- !sf::st_is_valid(parcels)
  if (any(invalid)) {
    cli::cli_warn(c(
      "{sum(invalid)} invalid geometr{?y/ies} detected",
      "i" = "Attempting to repair with sf::st_make_valid()",
      "i" = "Consider repairing geometries before building adjacency graph"
    ))
    parcels <- sf::st_make_valid(parcels)
  }

  if (!quiet) {
    cli::cli_alert_info(
      "Building adjacency graph for {.val {n_parcels}} parcel{?s}..."
    )
  }

  # Build adjacency using rook adjacency (boundary intersection)
  # DE-9IM pattern "F***1****":
  #   F = exterior-exterior: no intersection
  #   * = any (don't care)
  #   1 = boundary-boundary: 1-dimensional intersection (shared edge)
  #
  # This ensures parcels share an edge, not just a point (rook vs queen)

  adjacency_sparse <- sf::st_relate(
    parcels,
    parcels,
    pattern = "F***1****"  # Rook adjacency: shared edge required
  )

  # Convert sparse matrix (sgbp format) to adjacency list
  # adjacency_sparse is a list where each element contains indices of neighbors
  # Already in 1-based R indexing
  adj_list <- lapply(adjacency_sparse, as.integer)

  # Remove self-loops (shouldn't exist with this pattern, but be safe)
  adj_list <- lapply(seq_along(adj_list), function(i) {
    neighbors <- adj_list[[i]]
    neighbors[neighbors != i]
  })

  if (!quiet) {
    # Calculate graph statistics
    mean_degree <- mean(sapply(adj_list, length))
    max_degree <- max(sapply(adj_list, length))
    n_isolated <- sum(sapply(adj_list, length) == 0)

    cli::cli_alert_success(
      "Adjacency graph built: mean degree = {.val {round(mean_degree, 1)}}, ",
      "max degree = {.val {max_degree}}"
    )

    if (n_isolated > 0) {
      cli::cli_warn(
        "{.val {n_isolated}} isolated parcel{?s} with no neighbors detected"
      )
    }
  }

  return(adj_list)
}

#' Get Adjacency Graph Statistics
#'
#' Calculate summary statistics for an adjacency graph, useful for diagnostics
#' and understanding graph structure.
#'
#' @param adj An adjacency list (output from build_adjacency())
#'
#' @return A named list with statistics:
#'   - `n_parcels`: Number of parcels (nodes)
#'   - `n_edges`: Number of edges (undirected)
#'   - `mean_degree`: Mean number of neighbors per parcel
#'   - `median_degree`: Median number of neighbors
#'   - `max_degree`: Maximum number of neighbors
#'   - `min_degree`: Minimum number of neighbors
#'   - `n_isolated`: Number of parcels with no neighbors
#'   - `n_components`: Number of connected components
#'
#' @examples
#' \dontrun{
#' parcels <- load_municipality("inst/extdata/parcels/57_CHELSEA_basic.zip")
#' adj <- build_adjacency(parcels)
#' stats <- get_adjacency_stats(adj)
#' print(stats)
#' }
#'
#' @export
get_adjacency_stats <- function(adj) {

  if (!is.list(adj)) {
    cli::cli_abort("{.arg adj} must be a list (adjacency list)")
  }

  n_parcels <- length(adj)
  degrees <- sapply(adj, length)

  # Count edges (each edge counted twice in adjacency list)
  n_edges <- sum(degrees) / 2

  # Identify connected components
  components <- identify_components(adj)

  list(
    n_parcels = n_parcels,
    n_edges = as.integer(n_edges),
    mean_degree = mean(degrees),
    median_degree = median(degrees),
    max_degree = max(degrees),
    min_degree = min(degrees),
    n_isolated = sum(degrees == 0),
    n_components = max(components)
  )
}

#' Identify Connected Components in Adjacency Graph
#'
#' Find connected components using depth-first search. Parcels in the same
#' component can reach each other through adjacent parcels.
#'
#' @param adj An adjacency list (output from build_adjacency())
#'
#' @return An integer vector of length `length(adj)` where element `i` is the
#'   component ID for parcel `i`. Component IDs are numbered 1, 2, 3, ...
#'   Isolated parcels get their own component ID.
#'
#' @details
#' This function is used internally for:
#' - Tier 2 infeasibility checking (ensuring particle can reach enough parcels)
#' - Diagnostic reporting (detecting disconnected parcel groups)
#'
#' The algorithm uses iterative depth-first search to avoid stack overflow
#' on large connected components.
#'
#' @examples
#' \dontrun{
#' parcels <- load_municipality("inst/extdata/parcels/57_CHELSEA_basic.zip")
#' adj <- build_adjacency(parcels)
#' components <- identify_components(adj)
#'
#' # Count parcels in each component
#' table(components)
#'
#' # Find largest component
#' largest_component <- which.max(table(components))
#' parcels_in_largest <- which(components == largest_component)
#' }
#'
#' @export
identify_components <- function(adj) {

  if (!is.list(adj)) {
    cli::cli_abort("{.arg adj} must be a list (adjacency list)")
  }

  n <- length(adj)
  component_id <- integer(n)
  current_component <- 0

  for (start in seq_len(n)) {
    if (component_id[start] != 0) {
      next  # Already visited
    }

    # Start new component
    current_component <- current_component + 1

    # Iterative DFS using explicit stack
    stack <- start
    component_id[start] <- current_component

    while (length(stack) > 0) {
      # Pop from stack
      current <- stack[length(stack)]
      stack <- stack[-length(stack)]

      # Visit neighbors
      neighbors <- adj[[current]]
      unvisited <- neighbors[component_id[neighbors] == 0]

      if (length(unvisited) > 0) {
        component_id[unvisited] <- current_component
        stack <- c(stack, unvisited)
      }
    }
  }

  return(component_id)
}

#' Calculate Component Totals for Infeasibility Checking
#'
#' Precompute total capacity and area for each connected component. This
#' enables O(1) infeasibility checks during SMC particle propagation (Tier 2).
#'
#' @param parcels An sf data frame with parcel data
#' @param adj An adjacency list (output from build_adjacency())
#' @param capacity_col Name of column containing unit capacity (default: "unit_capacity")
#' @param area_col Name of column containing area in square feet (default: "SQFT")
#'
#' @return A list with components:
#'   - `component_id`: Integer vector of component IDs for each parcel
#'   - `total_capacity`: Numeric vector of total capacity per component
#'   - `total_area`: Numeric vector of total area (acres) per component
#'   - `n_components`: Number of connected components
#'
#' @details
#' During SMC propagation, particles track which connected component they're in
#' and use these precomputed totals to determine if they can possibly reach
#' the minimum capacity/area requirements. This enables early termination of
#' infeasible particles (Tier 2 checking).
#'
#' @keywords internal
calculate_component_totals <- function(parcels,
                                      adj,
                                      capacity_col = "unit_capacity",
                                      area_col = "SQFT") {

  # Identify components
  component_id <- identify_components(adj)
  n_components <- max(component_id)

  # Validate columns exist
  if (!capacity_col %in% names(parcels)) {
    cli::cli_abort("Column {.field {capacity_col}} not found in {.arg parcels}")
  }
  if (!area_col %in% names(parcels)) {
    cli::cli_abort("Column {.field {area_col}} not found in {.arg parcels}")
  }

  # Extract capacity and area
  capacity <- parcels[[capacity_col]]
  area_sqft <- parcels[[area_col]]
  area_acres <- area_sqft / 43560  # Convert to acres

  # Replace NA with 0 for aggregation (conservative for infeasibility checks)
  capacity[is.na(capacity)] <- 0
  area_acres[is.na(area_acres)] <- 0

  # Aggregate by component
  total_capacity <- tapply(capacity, component_id, sum, na.rm = TRUE)
  total_area <- tapply(area_acres, component_id, sum, na.rm = TRUE)

  list(
    component_id = component_id,
    total_capacity = as.numeric(total_capacity),
    total_area = as.numeric(total_area),
    n_components = n_components
  )
}