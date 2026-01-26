# spanning_tree.R - Spanning tree algorithms for LCC proposal generation
#
# Implements uniform random spanning tree sampling via Wilson's algorithm
# and tree-cut enumeration for the spanning tree Replace-LCC kernel.
#
# Key property: When both current and proposed LCCs are valid cuts of
# the same spanning tree, the proposal ratio is exactly 1.

# ============================================================================
# WILSON'S ALGORITHM FOR UNIFORM SPANNING TREES
# ============================================================================

#' Sample a uniform random spanning tree using Wilson's algorithm
#'
#' Uses loop-erased random walks to generate a spanning tree uniformly
#' at random from the set of all spanning trees of the graph.
#'
#' @param graph igraph object (should be connected; disconnected vertices skipped)
#' @return List with:
#'   - tree: igraph tree object with connected vertices
#'   - success: TRUE if spanning tree was built successfully
#'   - n_in_tree: Number of vertices in tree
#' @references Wilson, D.B. (1996). Generating random spanning trees
#'   more quickly than the cover time.
sample_uniform_spanning_tree <- function(graph) {
  n <- igraph::vcount(graph)
  if (n == 0) {
    return(list(
      tree = igraph::make_empty_graph(0),
      success = TRUE,
      n_in_tree = 0L
    ))
  }
  if (n == 1) {
    tree <- igraph::make_empty_graph(1) |>
      igraph::set_vertex_attr("name", value = igraph::V(graph)$name)
    return(list(tree = tree, success = TRUE, n_in_tree = 1L))
  }

  # Work with vertex indices (1-based)
  # parent[v] = u means edge (v, u) is in tree; root has parent[root] = root

  parent <- integer(n)
  in_tree <- logical(n)

  # Build adjacency list for fast neighbor lookup
  adj_list <- lapply(seq_len(n), function(v) {
    as.integer(igraph::neighbors(graph, v))
  })

  # Find first non-isolated vertex as root
  root <- NA_integer_
  for (v in seq_len(n)) {
    if (length(adj_list[[v]]) > 0) {
      root <- v
      break
    }
  }

  if (is.na(root)) {
    # All vertices isolated - cannot build spanning tree
    return(list(
      tree = igraph::make_empty_graph(n),
      success = FALSE,
      n_in_tree = 0L
    ))
  }

  in_tree[root] <- TRUE
  parent[root] <- root

  # Pre-allocate path array and position-in-path tracker for O(1) loop detection
  path <- integer(n)
  path_pos <- integer(n) # path_pos[v] = position of v in path (0 if not in path)
  path_len <- 0L

  # Count non-isolated vertices (vertices with at least one neighbor)
  # For a connected graph, we must reach all of these
  n_non_isolated <- sum(vapply(adj_list, function(x) length(x) > 0, logical(1)))

  # Step limit: O(n * log(n)) for typical connected graphs
  # This is generous enough for most graph structures while avoiding
  # pathological cases that would take O(n²) steps
  max_walk_steps <- as.integer(n * log(n + 1) * 10)

  for (start in seq_len(n)) {
    if (in_tree[start]) next
    if (length(adj_list[[start]]) == 0) next # Skip isolated vertices

    # Initialize path
    path_len <- 1L
    path[1L] <- start
    path_pos[start] <- 1L
    current <- start
    walk_steps <- 0L

    while (!in_tree[current]) {
      neighbors <- adj_list[[current]]

      if (length(neighbors) == 0) {
        # Hit isolated vertex - clear path and break
        break
      }

      walk_steps <- walk_steps + 1L
      if (walk_steps > max_walk_steps) {
        # Likely in a different connected component - skip this vertex
        break
      }

      next_vertex <- neighbors[sample.int(length(neighbors), 1L)]

      # O(1) loop detection using path_pos
      if (path_pos[next_vertex] > 0L) {
        # Loop detected: erase loop by resetting path_pos for erased vertices
        loop_idx <- path_pos[next_vertex]
        for (i in (loop_idx + 1L):path_len) {
          path_pos[path[i]] <- 0L
        }
        path_len <- loop_idx
        current <- next_vertex
      } else if (in_tree[next_vertex]) {
        # Reached tree - DON'T add to path, just record where we landed
        current <- next_vertex
        # Loop will exit on next iteration
      } else {
        # Extend path with non-tree vertex
        path_len <- path_len + 1L
        path[path_len] <- next_vertex
        path_pos[next_vertex] <- path_len
        current <- next_vertex
      }
    }

    # Add loop-erased path to tree
    # current is in tree; path[1:path_len] are NEW vertices to add
    if (in_tree[current] && path_len > 0) {
      for (i in seq_len(path_len)) {
        v <- path[i]
        if (i < path_len) {
          u <- path[i + 1L]
        } else {
          # Last vertex in path connects to current (the tree vertex we landed on)
          u <- current
        }
        parent[v] <- u
        in_tree[v] <- TRUE
        path_pos[v] <- 0L # Reset for next walk
      }
    }
    # Reset path_pos for all vertices in current path
    for (i in seq_len(path_len)) {
      path_pos[path[i]] <- 0L
    }
    path_len <- 0L
  }

  # Convert parent array to edge list (vectorized)
  tree_vertices <- which(in_tree & (seq_len(n) != root))
  if (length(tree_vertices) > 0) {
    edges <- as.integer(rbind(tree_vertices, parent[tree_vertices]))
  } else {
    edges <- integer(0)
  }

  # Build tree igraph
  tree <- igraph::make_empty_graph(n, directed = FALSE)
  igraph::V(tree)$name <- igraph::V(graph)$name
  if (length(edges) > 0) {
    tree <- igraph::add_edges(tree, edges)
  }

  n_in_tree <- sum(in_tree)

  # Verify tree is complete: must have all non-isolated vertices
  # If not, random walk likely hit step limit (disconnected component or pathological case)
  if (n_in_tree < n_non_isolated) {
    return(list(
      tree = tree,
      success = FALSE,
      n_in_tree = n_in_tree,
      n_expected = n_non_isolated,
      reason = "incomplete_tree"
    ))
  }

  list(
    tree = tree,
    success = TRUE,
    n_in_tree = n_in_tree,
    in_tree = in_tree,
    root = root
  )
}


# ============================================================================
# SUBTREE AGGREGATES
# ============================================================================

#' Root a tree and compute subtree aggregates via BFS + bottom-up pass
#'
#' Computes capacity, area, and forbidden-zone membership for each subtree.
#' Used to efficiently enumerate valid cuts.
#'
#' @param tree igraph tree object
#' @param root Integer vertex index to use as root
#' @param parcel_graph Original parcel graph (for capacity/area attributes)
#' @param forbidden_parcels Character vector of parcel IDs that cannot be in LCC
#' @return List with:
#'   - parent: integer vector of parent indices (root has parent = root)
#'   - children: list of integer vectors (children of each vertex)
#'   - subtree_capacity: numeric vector of subtree capacities
#'   - subtree_area: numeric vector of subtree areas
#'   - subtree_forbidden_count: integer vector of forbidden parcels in each subtree
#'   - bfs_order: integer vector of vertices in BFS order
#'   - total_capacity: total capacity of all vertices
#'   - total_area: total area of all vertices
#'   - total_forbidden: total number of forbidden parcels
compute_subtree_aggregates <- function(tree, root, parcel_graph, forbidden_parcels) {
  n <- igraph::vcount(tree)
  parcel_names <- igraph::V(parcel_graph)$name
  tree_names <- igraph::V(tree)$name

  # Get capacity and area from parcel_graph
  capacity <- igraph::V(parcel_graph)$capacity
  area <- igraph::V(parcel_graph)$area

  # Map tree vertices to parcel_graph vertices
  tree_to_parcel <- match(tree_names, parcel_names)

  # Initialize arrays

  parent <- integer(n)
  parent[root] <- root
  children <- vector("list", n)
  for (i in seq_len(n)) children[[i]] <- integer(0)

  subtree_capacity <- numeric(n)
  subtree_area <- numeric(n)
  subtree_forbidden_count <- integer(n)

  # Build adjacency list for tree
  tree_adj <- lapply(seq_len(n), function(v) {
    as.integer(igraph::neighbors(tree, v))
  })

  # BFS to establish parent/children structure
  visited <- logical(n)
  visited[root] <- TRUE
  queue <- root
  bfs_order <- root

  head <- 1L
  while (head <= length(queue)) {
    v <- queue[head]
    head <- head + 1L

    for (u in tree_adj[[v]]) {
      if (!visited[u]) {
        visited[u] <- TRUE
        parent[u] <- v
        children[[v]] <- c(children[[v]], u)
        queue <- c(queue, u)
        bfs_order <- c(bfs_order, u)
      }
    }
  }

  # Mark forbidden vertices with integer flag for counting
  forbidden_set <- which(tree_names %in% forbidden_parcels)
  is_forbidden <- integer(n)
  is_forbidden[forbidden_set] <- 1L
  total_forbidden <- length(forbidden_set)

  # Bottom-up aggregation (reverse BFS order)
  for (v in rev(bfs_order)) {
    parcel_idx <- tree_to_parcel[v]

    # Start with own values
    subtree_capacity[v] <- capacity[parcel_idx]
    subtree_area[v] <- area[parcel_idx]
    subtree_forbidden_count[v] <- is_forbidden[v]

    # Add children's values
    for (child in children[[v]]) {
      subtree_capacity[v] <- subtree_capacity[v] + subtree_capacity[child]
      subtree_area[v] <- subtree_area[v] + subtree_area[child]
      subtree_forbidden_count[v] <- subtree_forbidden_count[v] + subtree_forbidden_count[child]
    }
  }

  list(
    parent = parent,
    children = children,
    subtree_capacity = subtree_capacity,
    subtree_area = subtree_area,
    subtree_forbidden_count = subtree_forbidden_count,
    bfs_order = bfs_order,
    total_capacity = subtree_capacity[root],
    total_area = subtree_area[root],
    total_forbidden = total_forbidden
  )
}


# ============================================================================
# OPTIMIZED SUBTREE AGGREGATES (FAST VERSION)
# ============================================================================

#' Compute subtree aggregates using igraph C-level BFS (optimized)
#'
#' Faster version that uses igraph::bfs() and preallocated vectors.
#' Accepts precomputed aligned vectors to avoid repeated match() calls.
#'
#' @param tree igraph tree object
#' @param root Integer vertex index to use as root
#' @param capacity_aligned Numeric vector of capacities aligned to tree vertices
#' @param area_aligned Numeric vector of areas aligned to tree vertices
#' @param forbidden_mask Logical vector (TRUE if vertex is forbidden)
#' @return List with aggregates and traversal metadata
compute_subtree_aggregates_fast <- function(
    tree,
    root,
    capacity_aligned,
    area_aligned,
    forbidden_mask
) {
  n <- igraph::vcount(tree)


  # Use igraph::bfs() for C-level traversal
  bfs_result <- igraph::bfs(
    tree,
    root = root,
    order = TRUE,
    parent = TRUE, # 'parent' in igraph 2.2.0+ (was 'father')
    unreachable = FALSE
  )
  bfs_order <- as.integer(bfs_result$order)
  # Use 'parent' result (igraph 2.2.0+) with fallback to 'father' (older versions)
  parent <- as.integer(if (!is.null(bfs_result$parent)) bfs_result$parent else bfs_result$father)
  parent[root] <- root # Root's parent is itself

  # Convert forbidden mask to integer for counting
  is_forbidden <- as.integer(forbidden_mask)

  # Initialize with vertex values (copy to allow mutation)
  subtree_capacity <- as.numeric(capacity_aligned)
  subtree_area <- as.numeric(area_aligned)
  subtree_forbidden_count <- is_forbidden

  # Bottom-up aggregation: add child values to parent
  # Process in reverse BFS order (leaves to root)
  for (v in rev(bfs_order)) {
    if (v != root) {
      p <- parent[v]
      subtree_capacity[p] <- subtree_capacity[p] + subtree_capacity[v]
      subtree_area[p] <- subtree_area[p] + subtree_area[v]
      subtree_forbidden_count[p] <- subtree_forbidden_count[p] + subtree_forbidden_count[v]
    }
  }

  list(
    parent = parent,
    bfs_order = bfs_order,
    subtree_capacity = subtree_capacity,
    subtree_area = subtree_area,
    subtree_forbidden_count = subtree_forbidden_count,
    total_capacity = subtree_capacity[root],
    total_area = subtree_area[root],
    total_forbidden = sum(is_forbidden)
  )
}


#' Compute DFS metadata for efficient subtree extraction
#'
#' Computes DFS order, entry times, and subtree sizes ONCE per tree.
#' This enables O(k) subtree extraction via array slicing instead of O(n) BFS.
#'
#' @param tree igraph tree object
#' @param root Integer vertex index to use as root
#' @return List with:
#'   - dfs_order: integer vector of vertices in DFS order
#'   - entry: entry[v] = position in dfs_order where v's subtree starts
#'   - subtree_size: subtree_size[v] = number of vertices in v's subtree
#'   - parent: parent[v] = parent vertex (root has parent = root)
compute_tree_dfs_metadata <- function(tree, root) {
  n <- igraph::vcount(tree)

  # Use igraph::dfs() for C-level traversal
  dfs_result <- igraph::dfs(
    tree,
    root = root,
    order = TRUE,
    parent = TRUE, # 'parent' in igraph 2.2.0+ (was 'father')
    unreachable = FALSE
  )

  dfs_order <- as.integer(dfs_result$order)
  # Use 'parent' result (igraph 2.2.0+) with fallback to 'father' (older versions)
  parent <- as.integer(if (!is.null(dfs_result$parent)) dfs_result$parent else dfs_result$father)
  parent[root] <- root # Root's parent is itself

  # Compute entry times (position in dfs_order)
  entry <- integer(n)
  entry[dfs_order] <- seq_len(n)

  # Compute subtree sizes via reverse DFS order
  subtree_size <- rep(1L, n)
  for (v in rev(dfs_order)) {
    if (v != root) {
      subtree_size[parent[v]] <- subtree_size[parent[v]] + subtree_size[v]
    }
  }

  list(
    dfs_order = dfs_order,
    entry = entry,
    subtree_size = subtree_size,
    parent = parent
  )
}


#' Extract parcel IDs using precomputed DFS metadata (O(k) instead of O(n))
#'
#' Uses DFS property: subtree vertices are contiguous in DFS order.
#' This enables O(k) extraction via array slicing instead of O(n) BFS.
#'
#' @param tree_names Character vector of all vertex names
#' @param cut_vertex Integer vertex defining the cut
#' @param cut_side "subtree" or "complement"
#' @param dfs_metadata Output from compute_tree_dfs_metadata()
#' @return Character vector of parcel IDs
extract_cut_parcels_fast <- function(tree_names, cut_vertex, cut_side, dfs_metadata) {
  entry_pos <- dfs_metadata$entry[cut_vertex]
  size <- dfs_metadata$subtree_size[cut_vertex]

  # Subtree vertices are contiguous in DFS order
  subtree_indices <- dfs_metadata$dfs_order[entry_pos:(entry_pos + size - 1L)]

  if (cut_side == "subtree") {
    tree_names[subtree_indices]
  } else {
    # Complement: all except subtree
    tree_names[-subtree_indices]
  }
}


# ============================================================================
# FIND VALID CUTS
# ============================================================================

#' Find all valid LCC cuts of a spanning tree
#'
#' Each non-root vertex v defines a potential cut: removing edge (v, parent[v])
#' splits the tree into the subtree rooted at v and its complement.
#' A cut is valid if the resulting component can serve as an LCC candidate
#' (min_area, min_density, and capacity >= min_capacity * min_lcc_fraction).
#' Note: LCC candidates need not be standalone-feasible; secondaries may
#' supply capacity to reach min_capacity in the final plan.
#'
#' @param tree igraph tree object
#' @param root Integer root vertex
#' @param aggregates Output from compute_subtree_aggregates()
#' @param constraints MBTA constraints list
#' @param max_discovery_capacity Optional upper bound for discovery efficiency.
#'   LCCs with capacity above this are skipped. Default NULL (no bound).
#' @return List of valid cut specifications, each containing:
#'   - vertex: the vertex whose parent edge is cut
#'   - side: "subtree" or "complement" indicating which side is the LCC
#'   - capacity: total capacity of the LCC
#'   - area: total area of the LCC
find_valid_cuts <- function(tree, root, aggregates, constraints,
                            max_discovery_capacity = NULL) {
  n <- igraph::vcount(tree)
  valid_cuts <- list()

  # Total values from root

  total_capacity <- aggregates$total_capacity
  total_area <- aggregates$total_area

  # Constraints
  # NOTE: For LCC discovery, we use min_lcc_capacity (not min_capacity) because
  # LCCs can have lower capacity when secondaries contribute to total capacity.
  # min_lcc_capacity = min_capacity * min_lcc_fraction (default 0.5)
  min_lcc_fraction <- constraints$min_lcc_fraction %||% 0.5
  min_lcc_capacity <- constraints$min_capacity * min_lcc_fraction
  min_area <- constraints$min_area
  min_density <- constraints$min_density

  # Total forbidden count for computing complement forbidden
  total_forbidden <- aggregates$total_forbidden

  # For each non-root vertex, check both sides of the cut
  for (v in seq_len(n)) {
    if (v == root) next

    # Subtree side
    sub_cap <- aggregates$subtree_capacity[v]
    sub_area <- aggregates$subtree_area[v]
    sub_forbidden_count <- aggregates$subtree_forbidden_count[v]

    # Complement side
    comp_cap <- total_capacity - sub_cap
    comp_area <- total_area - sub_area
    # Complement forbidden = total forbidden - subtree forbidden
    comp_forbidden_count <- total_forbidden - sub_forbidden_count

    # Check subtree side as LCC (no forbidden parcels allowed)
    if (sub_forbidden_count == 0L &&
      sub_cap >= min_lcc_capacity &&
      (is.null(max_discovery_capacity) || sub_cap <= max_discovery_capacity) &&
      sub_area >= min_area &&
      (sub_area <= 0 || sub_cap / sub_area >= min_density)) {
      valid_cuts[[length(valid_cuts) + 1L]] <- list(
        vertex = v,
        side = "subtree",
        capacity = sub_cap,
        area = sub_area
      )
    }

    # Check complement side as LCC (no forbidden parcels allowed)
    if (comp_forbidden_count == 0L &&
      comp_cap >= min_lcc_capacity &&
      (is.null(max_discovery_capacity) || comp_cap <= max_discovery_capacity) &&
      comp_area >= min_area &&
      (comp_area <= 0 || comp_cap / comp_area >= min_density)) {
      valid_cuts[[length(valid_cuts) + 1L]] <- list(
        vertex = v,
        side = "complement",
        capacity = comp_cap,
        area = comp_area
      )
    }
  }

  valid_cuts
}


# ============================================================================
# EXTRACT PARCEL SET FROM CUT
# ============================================================================

#' Extract parcel IDs for a given cut specification
#'
#' @param tree igraph tree object
#' @param cut_vertex Integer vertex defining the cut
#' @param cut_side "subtree" or "complement"
#' @param root Integer root vertex
#' @return Character vector of parcel IDs
extract_cut_parcels <- function(tree, cut_vertex, cut_side, root = 1L) {
  tree_names <- igraph::V(tree)$name
  n <- length(tree_names)

  # Build parent array via BFS from root
  parent <- integer(n)
  parent[root] <- root
  visited <- logical(n)
  visited[root] <- TRUE
  queue <- root

  tree_adj <- lapply(seq_len(n), function(x) {
    as.integer(igraph::neighbors(tree, x))
  })

  head <- 1L
  while (head <= length(queue)) {
    curr <- queue[head]
    head <- head + 1L
    for (neighbor in tree_adj[[curr]]) {
      if (!visited[neighbor]) {
        visited[neighbor] <- TRUE
        parent[neighbor] <- curr
        queue <- c(queue, neighbor)
      }
    }
  }

  # Get subtree of cut_vertex via BFS
  subtree_vertices <- cut_vertex
  sub_queue <- cut_vertex
  sub_head <- 1L

  while (sub_head <= length(sub_queue)) {
    curr <- sub_queue[sub_head]
    sub_head <- sub_head + 1L
    for (neighbor in tree_adj[[curr]]) {
      if (parent[neighbor] == curr) {
        subtree_vertices <- c(subtree_vertices, neighbor)
        sub_queue <- c(sub_queue, neighbor)
      }
    }
  }

  if (cut_side == "subtree") {
    return(tree_names[subtree_vertices])
  } else {
    # complement
    complement_vertices <- setdiff(seq_len(n), subtree_vertices)
    return(tree_names[complement_vertices])
  }
}


# ============================================================================
# LCC DISCOVERY VIA SPANNING TREE ENUMERATION
# ============================================================================

#' Discover LCCs via spanning tree enumeration (optimized)
#'
#' Samples uniform random spanning trees and enumerates all valid LCC cuts.
#' "Valid" means the cut component can serve as an LCC candidate (meets area,
#' density, and min_lcc_capacity thresholds). LCC candidates need not be
#' standalone-feasible; secondaries may supply capacity in the final plan.
#' This is much more efficient than MCMC exploration for populating an LCC library.
#'
#' Optimizations:
#' - Uses igraph::sample_spanning_tree() (C-level Wilson's algorithm)
#' - Precomputes invariant data (capacity, area, forbidden) once
#' - Computes DFS metadata once per tree for O(k) cut extraction
#' - Uses igraph::bfs() for C-level traversal in aggregates
#'
#' @param parcel_graph igraph parcel graph with capacity/area attributes
#' @param constraints MBTA constraints list (min_capacity, min_area, min_density)
#' @param n_trees Number of spanning trees to sample (default 500)
#' @param forbidden_parcels Optional character vector of parcels to exclude from LCCs
#' @param max_discovery_capacity Optional upper bound on LCC capacity for discovery.
#'   LCCs above this are skipped. Default: min_capacity * DISCOVERY_CAPACITY_MULTIPLIER.
#' @param verbose Print progress (default TRUE)
#' @return List with:
#'   - discovered_lccs: data.table with lcc_key, parcel_ids (list col), capacity, area, tree_count
#'   - n_trees_sampled: number of trees successfully sampled
#'   - n_trees_failed: number of trees that failed to build
#'   - total_cuts_found: total number of valid cuts found (before deduplication)
#'   - unique_lccs_found: number of unique LCCs after deduplication
#' @export
discover_lccs_from_trees <- function(
    parcel_graph,
    constraints,
    n_trees = 500L,
    forbidden_parcels = NULL,
    max_discovery_capacity = NULL,
    verbose = TRUE
) {
  parcel_names <- igraph::V(parcel_graph)$name
  n <- length(parcel_names)

  if (n == 0) {
    return(list(
      discovered_lccs = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        tree_count = integer(0)
      ),
      n_trees_sampled = 0L,
      n_trees_failed = 0L,
      total_cuts_found = 0L,
      unique_lccs_found = 0L
    ))
  }

  # =========================================================================
  # MULTI-COMPONENT DISCOVERY
  # =========================================================================
  # Find all components that could potentially contain valid LCCs
  # (component total capacity >= min_capacity)
  comps <- igraph::components(parcel_graph)

  # Compute total capacity per component
  vertex_caps <- igraph::V(parcel_graph)$capacity
  comp_capacities <- vapply(seq_len(comps$no), function(comp_id) {
    sum(vertex_caps[comps$membership == comp_id])
  }, numeric(1))

  # Filter to feasible components (can satisfy min_capacity)
  min_cap <- constraints$min_capacity
  feasible_comp_ids <- which(comp_capacities >= min_cap)

  # Compute discovery upper bound if not provided
  if (is.null(max_discovery_capacity)) {
    max_discovery_capacity <- min_cap * DISCOVERY_CAPACITY_MULTIPLIER
  }
  if (verbose) {
    cli::cli_alert_info("Discovery capacity bound: {round(max_discovery_capacity)}")
  }

  if (length(feasible_comp_ids) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No components have sufficient capacity (min={min_cap})")
    }
    return(list(
      discovered_lccs = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        tree_count = integer(0)
      ),
      n_trees_sampled = 0L,
      n_trees_failed = 0L,
      total_cuts_found = 0L,
      unique_lccs_found = 0L
    ))
  }

  # Allocate trees proportionally to component capacity
  # Handle edge case: fewer trees than components
  feasible_caps <- comp_capacities[feasible_comp_ids]
  n_feasible <- length(feasible_comp_ids)

  if (n_trees <= n_feasible) {
    # Not enough trees for all components - prioritize by capacity
    cap_order <- order(feasible_caps, decreasing = TRUE)
    trees_per_comp <- integer(n_feasible)
    trees_per_comp[cap_order[seq_len(n_trees)]] <- 1L
  } else {
    # Proportional allocation with guaranteed minimum of 1
    trees_per_comp <- pmax(1L, round(n_trees * feasible_caps / sum(feasible_caps)))
    # Adjust largest component to hit exact n_trees (ensure non-negative)
    adjustment <- n_trees - sum(trees_per_comp)
    largest_idx <- which.max(feasible_caps)
    trees_per_comp[largest_idx] <- max(1L, trees_per_comp[largest_idx] + adjustment)
  }

  if (verbose && length(feasible_comp_ids) > 1) {
    cli::cli_alert_info(
      "Graph has {comps$no} components; {length(feasible_comp_ids)} feasible for LCCs"
    )
    cli::cli_alert_info(
      "Tree allocation: {paste(sprintf('C%d=%d', feasible_comp_ids, trees_per_comp), collapse=', ')}"
    )
  } else if (verbose && comps$no > 1) {
    cli::cli_alert_warning(
      "Graph has {comps$no} components; using largest ({comps$csize[feasible_comp_ids[1]]}/{n} vertices)"
    )
  }

  # =========================================================================
  # GLOBAL STATE (shared across all components)
  # =========================================================================
  # Use environment for O(1) hash-based deduplication
  lcc_hash <- new.env(hash = TRUE, parent = emptyenv())
  # Track tree counts for each LCC
  lcc_tree_counts <- new.env(hash = TRUE, parent = emptyenv())

  n_trees_sampled <- 0L
  n_trees_failed <- 0L
  total_cuts_found <- 0L
  n_unique_lccs <- 0L  # Track count directly (avoids O(n) ls() calls)
  trees_processed <- 0L  # For progress bar

  # Precompute forbidden mask (global)
  if (is.null(forbidden_parcels)) {
    forbidden_parcels <- character(0)
  }

  if (verbose) {
    cli::cli_progress_bar(
      "Sampling spanning trees",
      total = n_trees,
      format = "{cli::pb_spin} Tree {trees_processed}/{n_trees} | Unique LCCs: {n_unique_lccs}"
    )
  }

  # =========================================================================
  # LOOP OVER FEASIBLE COMPONENTS
  # =========================================================================
  for (comp_idx in seq_along(feasible_comp_ids)) {
    comp_id <- feasible_comp_ids[comp_idx]
    n_trees_this_comp <- trees_per_comp[comp_idx]

    # Build subgraph for this component
    comp_mask <- comps$membership == comp_id
    comp_indices <- which(comp_mask)
    tree_graph <- igraph::induced_subgraph(parcel_graph, comp_indices)
    n_tree <- igraph::vcount(tree_graph)

    # Skip if component is too small (single vertex can't have valid cuts)
    if (n_tree < 2) {
      trees_processed <- trees_processed + n_trees_this_comp
      if (verbose) {
        for (i in seq_len(n_trees_this_comp)) cli::cli_progress_update()
      }
      next
    }

    # =========================================================================
    # PRECOMPUTE INVARIANT DATA (ONCE per component)
    # =========================================================================
    tree_names <- igraph::V(tree_graph)$name

    # Map tree_graph vertices to parcel_graph vertices for attribute lookup
    tree_to_parcel <- match(tree_names, parcel_names)

    # Precompute aligned capacity/area vectors
    capacity_aligned <- igraph::V(parcel_graph)$capacity[tree_to_parcel]
    area_aligned <- igraph::V(parcel_graph)$area[tree_to_parcel]

    # Forbidden mask for this component
    forbidden_mask <- tree_names %in% forbidden_parcels

    # =========================================================================
    # SAMPLE TREES FOR THIS COMPONENT
    # =========================================================================
    for (tree_idx in seq_len(n_trees_this_comp)) {
      trees_processed <- trees_processed + 1L
      if (verbose) cli::cli_progress_update()

      # Use igraph::sample_spanning_tree() (C-level Wilson's algorithm)
      # Randomize start vertex to avoid region bias in discovery order
      start_vid <- sample(igraph::vcount(tree_graph), 1)
      tree_edges <- tryCatch(
        igraph::sample_spanning_tree(tree_graph, vid = start_vid),
        error = function(e) NULL
      )

      if (is.null(tree_edges) || length(tree_edges) == 0) {
        n_trees_failed <- n_trees_failed + 1L
        next
      }

      # Build tree igraph from edges (preserves vertex names from tree_graph)
      tree <- igraph::subgraph_from_edges(tree_graph, tree_edges, delete.vertices = FALSE)

      n_trees_sampled <- n_trees_sampled + 1L

      # Pick random root for aggregation
      root <- sample.int(n_tree, 1L)

      # Compute subtree aggregates using fast BFS
      aggregates <- compute_subtree_aggregates_fast(
        tree = tree,
        root = root,
        capacity_aligned = capacity_aligned,
        area_aligned = area_aligned,
        forbidden_mask = forbidden_mask
      )

      # Find all valid cuts
      valid_cuts <- find_valid_cuts(
        tree = tree,
        root = root,
        aggregates = aggregates,
        constraints = constraints,
        max_discovery_capacity = max_discovery_capacity
      )

      if (length(valid_cuts) == 0) next

      total_cuts_found <- total_cuts_found + length(valid_cuts)

      # Compute DFS metadata ONCE per tree for O(k) cut extraction
      dfs_metadata <- compute_tree_dfs_metadata(tree, root)

      # Extract parcels for each valid cut using O(k) array slicing
      for (cut in valid_cuts) {
        lcc_parcels <- extract_cut_parcels_fast(
          tree_names = tree_names,
          cut_vertex = cut$vertex,
          cut_side = cut$side,
          dfs_metadata = dfs_metadata
        )

        # Create signature key using hash (full parcel string can exceed R's 10000 byte limit)
        sorted_parcels <- sort(lcc_parcels)
        lcc_key <- digest::digest(sorted_parcels, algo = "xxhash64")

        # Add to hash if new, or increment tree count
        if (!exists(lcc_key, envir = lcc_hash, inherits = FALSE)) {
          assign(lcc_key, list(
            parcel_ids = lcc_parcels,
            capacity = as.integer(cut$capacity),
            area = cut$area
          ), envir = lcc_hash)
          assign(lcc_key, 1L, envir = lcc_tree_counts)
          n_unique_lccs <- n_unique_lccs + 1L
        } else {
          # Increment tree count (LCC found in multiple trees)
          current_count <- get(lcc_key, envir = lcc_tree_counts)
          assign(lcc_key, current_count + 1L, envir = lcc_tree_counts)
        }
      }
    }
  }  # End component loop

  if (verbose) cli::cli_progress_done()

  # Convert hash to data.table
  lcc_keys <- ls(lcc_hash)
  n_unique <- length(lcc_keys)

  if (n_unique == 0) {
    return(list(
      discovered_lccs = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        tree_count = integer(0)
      ),
      n_trees_sampled = n_trees_sampled,
      n_trees_failed = n_trees_failed,
      total_cuts_found = total_cuts_found,
      unique_lccs_found = 0L
    ))
  }

  # Build data.table from hash
  discovered_lccs <- data.table::rbindlist(lapply(lcc_keys, function(key) {
    lcc_data <- get(key, envir = lcc_hash)
    tree_count <- get(key, envir = lcc_tree_counts)
    data.table::data.table(
      lcc_key = key,
      parcel_ids = list(lcc_data$parcel_ids),
      capacity = lcc_data$capacity,
      area = lcc_data$area,
      tree_count = tree_count
    )
  }))

  # Sort by tree_count (most frequently found first)
  data.table::setorder(discovered_lccs, -tree_count)

  if (verbose) {
    cli::cli_alert_success(
      "Discovered {n_unique} unique LCCs from {n_trees_sampled} trees ({total_cuts_found} total cuts)"
    )
  }

  list(
    discovered_lccs = discovered_lccs,
    n_trees_sampled = n_trees_sampled,
    n_trees_failed = n_trees_failed,
    total_cuts_found = total_cuts_found,
    unique_lccs_found = n_unique
  )
}

# ============================================================================
# SECONDARY BLOCK DISCOVERY VIA SPANNING TREE ENUMERATION
# ============================================================================

#' Find all valid secondary block cuts of a spanning tree
#'
#' Each non-root vertex v defines a potential cut: removing edge (v, parent[v])
#' splits the tree into the subtree rooted at v and its complement.
#' A cut is valid if the resulting component satisfies secondary block constraints
#' (area within band, density >= threshold).
#'
#' @param tree igraph tree object
#' @param root Integer root vertex
#' @param aggregates Output from compute_subtree_aggregates_fast()
#' @param size_bands List of c(min_area, max_area) tuples in acres
#' @param density_threshold Minimum density (units/acre) required
#' @return List of valid cut specifications, each containing:
#'   - vertex: the vertex whose parent edge is cut
#'   - side: "subtree" or "complement" indicating which side is the block
#'   - capacity: total capacity of the block
#'   - area: total area of the block
#'   - size_band: which band this block falls into (index)
find_valid_secondary_cuts <- function(
    tree,
    root,
    aggregates,
    size_bands,
    density_threshold
) {
  n <- igraph::vcount(tree)
  valid_cuts <- list()

  # Total values from root

  total_capacity <- aggregates$total_capacity
  total_area <- aggregates$total_area

  # Compute global area bounds for early filtering
  area_min_global <- min(vapply(size_bands, `[`, numeric(1), 1))
  area_max_global <- max(vapply(size_bands, `[`, numeric(1), 2))

  # For each non-root vertex, check both sides of the cut
  for (v in seq_len(n)) {
    if (v == root) next

    # Subtree side
    sub_cap <- aggregates$subtree_capacity[v]
    sub_area <- aggregates$subtree_area[v]

    # Complement side
    comp_cap <- total_capacity - sub_cap
    comp_area <- total_area - sub_area

    # Early skip if neither side can satisfy any band
    if (sub_area < area_min_global && comp_area < area_min_global) next
    if (sub_area > area_max_global && comp_area > area_max_global) next

    # Check subtree side against each size band
    if (sub_area >= area_min_global && sub_area <= area_max_global) {
      sub_density <- if (sub_area > 0) sub_cap / sub_area else 0
      if (sub_density >= density_threshold) {
        for (band_idx in seq_along(size_bands)) {
          band <- size_bands[[band_idx]]
          if (sub_area >= band[1] && sub_area <= band[2]) {
            valid_cuts[[length(valid_cuts) + 1L]] <- list(
              vertex = v,
              side = "subtree",
              capacity = as.integer(sub_cap),
              area = sub_area,
              size_band = band_idx
            )
            break # Only count in first matching band
          }
        }
      }
    }

    # Check complement side against each size band
    if (comp_area >= area_min_global && comp_area <= area_max_global) {
      comp_density <- if (comp_area > 0) comp_cap / comp_area else 0
      if (comp_density >= density_threshold) {
        for (band_idx in seq_along(size_bands)) {
          band <- size_bands[[band_idx]]
          if (comp_area >= band[1] && comp_area <= band[2]) {
            valid_cuts[[length(valid_cuts) + 1L]] <- list(
              vertex = v,
              side = "complement",
              capacity = as.integer(comp_cap),
              area = comp_area,
              size_band = band_idx
            )
            break # Only count in first matching band
          }
        }
      }
    }
  }

  valid_cuts
}


#' Discover secondary blocks via spanning tree enumeration
#'
#' Samples uniform random spanning trees and enumerates all valid secondary
#' block cuts. Parallel to discover_lccs_from_trees() but uses area bands
#' and density threshold instead of LCC constraints.
#'
#' @param parcel_graph igraph parcel graph with capacity/area attributes
#' @param size_bands List of c(min_area, max_area) tuples in acres
#'   Default: list(c(5, 8), c(8, 12), c(12, 20))
#' @param density_threshold Minimum density (units/acre) required. Default: 15
#' @param n_trees Number of spanning trees to sample (default 200)
#' @param verbose Print progress (default TRUE)
#' @return List with:
#'   - discovered_secondaries: data.table with sec_key, parcel_ids, capacity,
#'       area, size_band, tree_count, source
#'   - n_trees_sampled: number of trees successfully sampled
#'   - n_trees_failed: number of trees that failed to build
#'   - total_cuts_found: total number of valid cuts found (before deduplication)
#'   - unique_secondaries_found: number of unique blocks after deduplication
#'   - band_counts: named vector of counts per size band
#' @export
discover_secondaries_from_trees <- function(
    parcel_graph,
    size_bands = list(c(5, 8), c(8, 12), c(12, 20)),
    density_threshold = 15,
    n_trees = 200L,
    verbose = TRUE
) {
  parcel_names <- igraph::V(parcel_graph)$name
  n <- length(parcel_names)

  if (n == 0) {
    return(list(
      discovered_secondaries = data.table::data.table(
        sec_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        size_band = integer(0),
        tree_count = integer(0),
        source = character(0)
      ),
      n_trees_sampled = 0L,
      n_trees_failed = 0L,
      total_cuts_found = 0L,
      unique_secondaries_found = 0L,
      band_counts = setNames(rep(0L, length(size_bands)), paste0("band_", seq_along(size_bands)))
    ))
  }

  # =========================================================================
  # MULTI-COMPONENT HANDLING
  # =========================================================================
  # For secondaries, we can use any component with area >= min band
  comps <- igraph::components(parcel_graph)
  vertex_areas <- igraph::V(parcel_graph)$area

  # Compute total area per component

  comp_areas <- vapply(seq_len(comps$no), function(comp_id) {
    sum(vertex_areas[comps$membership == comp_id])
  }, numeric(1))

  # Minimum area needed (smallest band minimum)
  min_area_needed <- min(vapply(size_bands, `[`, numeric(1), 1))
  feasible_comp_ids <- which(comp_areas >= min_area_needed)

  if (length(feasible_comp_ids) == 0) {
    if (verbose) {
      cli::cli_alert_warning("No components have sufficient area (min={min_area_needed} acres)")
    }
    return(list(
      discovered_secondaries = data.table::data.table(
        sec_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        size_band = integer(0),
        tree_count = integer(0),
        source = character(0)
      ),
      n_trees_sampled = 0L,
      n_trees_failed = 0L,
      total_cuts_found = 0L,
      unique_secondaries_found = 0L,
      band_counts = setNames(rep(0L, length(size_bands)), paste0("band_", seq_along(size_bands)))
    ))
  }

  # Allocate trees proportionally to component area
  feasible_areas <- comp_areas[feasible_comp_ids]
  n_feasible <- length(feasible_comp_ids)

  if (n_trees <= n_feasible) {
    area_order <- order(feasible_areas, decreasing = TRUE)
    trees_per_comp <- integer(n_feasible)
    trees_per_comp[area_order[seq_len(n_trees)]] <- 1L
  } else {
    trees_per_comp <- pmax(1L, round(n_trees * feasible_areas / sum(feasible_areas)))
    adjustment <- n_trees - sum(trees_per_comp)
    largest_idx <- which.max(feasible_areas)
    trees_per_comp[largest_idx] <- max(1L, trees_per_comp[largest_idx] + adjustment)
  }

  if (verbose && length(feasible_comp_ids) > 1) {
    cli::cli_alert_info(
      "Graph has {comps$no} components; {length(feasible_comp_ids)} feasible for secondaries"
    )
  }

  # =========================================================================
  # GLOBAL STATE (shared across components)
  # =========================================================================
  sec_hash <- new.env(hash = TRUE, parent = emptyenv())
  sec_tree_counts <- new.env(hash = TRUE, parent = emptyenv())

  n_trees_sampled <- 0L
  n_trees_failed <- 0L
  total_cuts_found <- 0L
  n_unique_secs <- 0L
  trees_processed <- 0L

  # Band counts
  band_counts <- setNames(rep(0L, length(size_bands)), paste0("band_", seq_along(size_bands)))

  if (verbose) {
    cli::cli_progress_bar(
      "Sampling spanning trees (secondaries)",
      total = n_trees,
      format = "{cli::pb_spin} Tree {trees_processed}/{n_trees} | Unique blocks: {n_unique_secs}"
    )
  }

  # =========================================================================
  # LOOP OVER FEASIBLE COMPONENTS
  # =========================================================================
  for (comp_idx in seq_along(feasible_comp_ids)) {
    comp_id <- feasible_comp_ids[comp_idx]
    n_trees_this_comp <- trees_per_comp[comp_idx]

    # Build subgraph for this component
    comp_mask <- comps$membership == comp_id
    comp_indices <- which(comp_mask)
    tree_graph <- igraph::induced_subgraph(parcel_graph, comp_indices)
    n_tree <- igraph::vcount(tree_graph)

    if (n_tree < 2) {
      trees_processed <- trees_processed + n_trees_this_comp
      if (verbose) {
        for (i in seq_len(n_trees_this_comp)) cli::cli_progress_update()
      }
      next
    }

    # =========================================================================
    # PRECOMPUTE INVARIANT DATA (ONCE per component)
    # =========================================================================
    tree_names <- igraph::V(tree_graph)$name
    tree_to_parcel <- match(tree_names, parcel_names)

    capacity_aligned <- igraph::V(parcel_graph)$capacity[tree_to_parcel]
    area_aligned <- igraph::V(parcel_graph)$area[tree_to_parcel]

    # No forbidden mask for secondaries
    forbidden_mask <- rep(FALSE, n_tree)

    # =========================================================================
    # SAMPLE TREES FOR THIS COMPONENT
    # =========================================================================
    for (tree_idx in seq_len(n_trees_this_comp)) {
      trees_processed <- trees_processed + 1L
      if (verbose) cli::cli_progress_update()

      # Randomize start vertex to avoid region bias in discovery order
      start_vid <- sample(igraph::vcount(tree_graph), 1)
      tree_edges <- tryCatch(
        igraph::sample_spanning_tree(tree_graph, vid = start_vid),
        error = function(e) NULL
      )

      if (is.null(tree_edges) || length(tree_edges) == 0) {
        n_trees_failed <- n_trees_failed + 1L
        next
      }

      tree <- igraph::subgraph_from_edges(tree_graph, tree_edges, delete.vertices = FALSE)
      n_trees_sampled <- n_trees_sampled + 1L

      root <- sample.int(n_tree, 1L)

      aggregates <- compute_subtree_aggregates_fast(
        tree = tree,
        root = root,
        capacity_aligned = capacity_aligned,
        area_aligned = area_aligned,
        forbidden_mask = forbidden_mask
      )

      valid_cuts <- find_valid_secondary_cuts(
        tree = tree,
        root = root,
        aggregates = aggregates,
        size_bands = size_bands,
        density_threshold = density_threshold
      )

      if (length(valid_cuts) == 0) next

      total_cuts_found <- total_cuts_found + length(valid_cuts)

      # Compute DFS metadata ONCE per tree
      dfs_metadata <- compute_tree_dfs_metadata(tree, root)

      for (cut in valid_cuts) {
        sec_parcels <- extract_cut_parcels_fast(
          tree_names = tree_names,
          cut_vertex = cut$vertex,
          cut_side = cut$side,
          dfs_metadata = dfs_metadata
        )

        sec_key <- digest::digest(sort(sec_parcels), algo = "xxhash64")

        if (!exists(sec_key, envir = sec_hash, inherits = FALSE)) {
          assign(sec_key, list(
            parcel_ids = sec_parcels,
            capacity = cut$capacity,
            area = cut$area,
            size_band = cut$size_band
          ), envir = sec_hash)
          assign(sec_key, 1L, envir = sec_tree_counts)
          n_unique_secs <- n_unique_secs + 1L
          band_counts[cut$size_band] <- band_counts[cut$size_band] + 1L
        } else {
          current_count <- get(sec_key, envir = sec_tree_counts)
          assign(sec_key, current_count + 1L, envir = sec_tree_counts)
        }
      }
    }
  }

  if (verbose) cli::cli_progress_done()

  # Convert hash to data.table
  sec_keys <- ls(sec_hash)
  n_unique <- length(sec_keys)

  if (n_unique == 0) {
    return(list(
      discovered_secondaries = data.table::data.table(
        sec_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        size_band = integer(0),
        tree_count = integer(0),
        source = character(0)
      ),
      n_trees_sampled = n_trees_sampled,
      n_trees_failed = n_trees_failed,
      total_cuts_found = total_cuts_found,
      unique_secondaries_found = 0L,
      band_counts = band_counts
    ))
  }

  discovered_secondaries <- data.table::rbindlist(lapply(sec_keys, function(key) {
    sec_data <- get(key, envir = sec_hash)
    tree_count <- get(key, envir = sec_tree_counts)
    data.table::data.table(
      sec_key = key,
      parcel_ids = list(sec_data$parcel_ids),
      capacity = sec_data$capacity,
      area = sec_data$area,
      size_band = sec_data$size_band,
      tree_count = tree_count,
      source = "tree"
    )
  }))

  # Sort by tree_count (most frequently found first)
  data.table::setorder(discovered_secondaries, -tree_count)

  if (verbose) {
    cli::cli_alert_success(
      "Discovered {n_unique} unique secondary blocks from {n_trees_sampled} trees ({total_cuts_found} total cuts)"
    )
    cli::cli_alert_info(
      "Band distribution: {paste(sprintf('%s=%d', names(band_counts), band_counts), collapse=', ')}"
    )
  }

  list(
    discovered_secondaries = discovered_secondaries,
    n_trees_sampled = n_trees_sampled,
    n_trees_failed = n_trees_failed,
    total_cuts_found = total_cuts_found,
    unique_secondaries_found = n_unique,
    band_counts = band_counts
  )
}
