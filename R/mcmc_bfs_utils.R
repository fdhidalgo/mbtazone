# bfs_utils.R - BFS region-growing utilities
#
# Consolidated BFS functionality for growing connected blocks/components
# Used by parcel library construction and parcel clustering

#' Grow a connected block via randomized BFS
#'
#' Generic BFS region-growing that supports various termination conditions
#' and frontier filters. Consolidates multiple BFS patterns used across
#' the codebase for block/component generation.
#'
#' @param graph igraph object with named vertices
#' @param metric_lookup Named numeric vector (area or capacity per node)
#' @param seed_pool Character vector of eligible seed nodes (NULL = all vertices)
#' @param eligible_pool Character vector of nodes eligible for expansion (NULL = all vertices)
#' @param target_min Minimum metric threshold to reach
#' @param target_max Maximum metric threshold (optional, for pre-add rejection)
#' @param target_exact Optional exact target to grow toward (overrides target_min for termination)
#' @param check_max_before_add Logical: if TRUE, reject additions that would exceed target_max
#' @return List with:
#'   - block: character vector of node IDs in grown block
#'   - metric_total: final metric sum
#'   - success: logical (TRUE if target_min reached)
#'   - seed: the seed node used
#' @keywords internal
bfs_grow_block <- function(graph,
                           metric_lookup,
                           seed_pool = NULL,
                           eligible_pool = NULL,
                           target_min,
                           target_max = Inf,
                           target_exact = NULL,
                           check_max_before_add = FALSE) {

  all_nodes <- igraph::V(graph)$name

  # Default pools to all nodes
  if (is.null(seed_pool)) seed_pool <- all_nodes
  if (is.null(eligible_pool)) eligible_pool <- all_nodes

  # Seed must be in both seed_pool AND eligible_pool

  valid_seeds <- intersect(seed_pool, eligible_pool)
  if (length(valid_seeds) == 0) {
    stop("bfs_grow_block: No valid seeds (seed_pool and eligible_pool are disjoint or empty)")
  }

  # Use target_exact as termination target if provided, otherwise target_min
  effective_target <- if (!is.null(target_exact)) target_exact else target_min

  # Select random seed from valid seeds
  # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
  seed <- valid_seeds[sample.int(length(valid_seeds), 1)]
  current_block <- seed
  current_metric <- metric_lookup[seed]

  # Initialize frontier with seed's neighbors (excluding seed, restricted to eligible)
  frontier <- igraph::neighbors(graph, seed)$name
  frontier <- setdiff(frontier, seed)
  frontier <- intersect(frontier, eligible_pool)

  # BFS expansion
  while (length(frontier) > 0 && current_metric < effective_target) {
    # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
    next_node <- frontier[sample.int(length(frontier), 1)]
    next_metric <- metric_lookup[next_node]

    # Pre-add max check (for cluster_parcels_to_units pattern)
    if (check_max_before_add && (current_metric + next_metric > target_max)) {
      # Remove this node from frontier but don't add it to block
      frontier <- frontier[frontier != next_node]
      next
    }

    # Add node to block
    current_block <- c(current_block, next_node)
    current_metric <- current_metric + next_metric

    # Update frontier: remove just next_node (O(n) instead of O(n*m))
    frontier <- frontier[frontier != next_node]

    # Add new neighbors (excluding current block members)
    new_nbrs <- igraph::neighbors(graph, next_node)$name
    new_nbrs <- setdiff(new_nbrs, current_block)
    new_nbrs <- intersect(new_nbrs, eligible_pool)
    frontier <- unique(c(frontier, new_nbrs))
  }

  list(
    block = current_block,
    metric_total = unname(current_metric),
    success = (current_metric >= target_min),
    seed = seed
  )
}
