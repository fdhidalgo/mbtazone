# bfs_utils.R - BFS region-growing utilities
#
# Consolidated BFS functionality for growing connected blocks/components
# Used by parcel library construction and parcel clustering

#' Materialize a graph's adjacency as ascending-sorted integer node ids
#'
#' One \code{igraph::as_adj_list()} C call for the whole graph (~17x cheaper than
#' \code{n} per-vertex \code{igraph::neighbors()} calls), with each vertex's
#' neighbor ids sorted ascending. For a graph whose vertex ids are \code{1:n} in
#' \code{igraph::V(graph)$name} order, element \code{i} equals
#' \code{as.integer(igraph::neighbors(graph, i))} and, mapped through names,
#' reproduces \code{igraph::neighbors(graph, name_i)$name} exactly (verified
#' bit-for-bit). Callers depend on that order: it fixes the BFS frontier order
#' (and thus every \code{sample.int()} draw) and the neighbor-index layout, so it
#' must not change.
#'
#' @param graph igraph object with named vertices
#' @return Unnamed list; element \code{i} is the ascending integer neighbor ids
#'   of vertex \code{i}.
#' @keywords internal
sorted_adj_int <- function(graph) {
  unname(lapply(igraph::as_adj_list(graph, mode = "all"),
                function(x) sort(as.integer(x))))
}

#' Build a name-keyed cache of each vertex's neighbor names
#'
#' Equivalent to \code{setNames(lapply(V(graph)$name, function(m)
#' igraph::neighbors(graph, m)$name), V(graph)$name)} but built from a single
#' \code{\link{sorted_adj_int}} pass. Order within each entry matches
#' \code{neighbors()$name} (ascending vertex id), which downstream
#' neighbor-index construction relies on.
#'
#' @param graph igraph object with named vertices
#' @return Named list keyed by vertex name; each element is the neighbor names.
#' @keywords internal
build_neighbor_cache <- function(graph) {
  node_names <- igraph::V(graph)$name
  setNames(lapply(sorted_adj_int(graph), function(ids) node_names[ids]),
           node_names)
}

#' Build a reusable integer-indexed BFS context for a parcel graph
#'
#' Precomputes, once per graph, the integer adjacency list (\code{adj_int}, via
#' \code{\link{sorted_adj_int}}) plus an id-ordered metric vector and an
#' eligibility mask. \code{bfs_grow_block()} can then run entirely on integer set
#' ops instead of repeating igraph C-calls and character \code{intersect}/
#' \code{setdiff}/\code{unique} on every BFS step. Because \code{adj_int}
#' reproduces \code{neighbors()} order exactly, the frontier order — and every
#' \code{sample.int()} draw — is unchanged from the character path.
#'
#' \code{metric2_lookup} is optional and only used by the density-aware frontier
#' strategy in \code{\link{bfs_grow_block_ctx}}: it lets one context carry both
#' capacity (\code{metric_lookup}) and area (\code{metric2_lookup}) so the grower
#' can evaluate per-step density. Leaving it NULL (the default) is the standard
#' single-metric context and changes nothing for existing callers.
#'
#' @param graph igraph object with named vertices
#' @param metric_lookup Named numeric vector (area or capacity per node name)
#' @param eligible_pool Character vector of nodes eligible for expansion
#'   (NULL = all vertices)
#' @param metric2_lookup Optional named numeric vector of a second per-node metric
#'   (e.g. area when \code{metric_lookup} is capacity). Required for density-aware
#'   growth; NULL otherwise.
#' @return List with \code{node_names}, \code{id_of} (named integer name->id),
#'   \code{adj_int} (list of integer neighbor-id vectors), \code{n},
#'   \code{metric_by_id} (numeric, id-ordered), \code{metric2_by_id} (numeric,
#'   id-ordered, or NULL), \code{eligible_mask} (logical, id-ordered).
#' @keywords internal
bfs_build_context <- function(graph, metric_lookup, eligible_pool = NULL,
                              metric2_lookup = NULL) {
  node_names <- igraph::V(graph)$name
  n <- length(node_names)

  id_of <- seq_len(n)
  names(id_of) <- node_names

  adj_int <- sorted_adj_int(graph)

  # Metric aligned to node-id order (values identical to metric_lookup[name]).
  metric_by_id <- as.numeric(metric_lookup[node_names])

  # Optional second metric (id-ordered), for density-aware growth. NULL keeps the
  # context single-metric and behavior-identical for existing callers.
  metric2_by_id <- if (is.null(metric2_lookup)) {
    NULL
  } else {
    as.numeric(metric2_lookup[node_names])
  }

  if (is.null(eligible_pool)) {
    eligible_mask <- rep(TRUE, n)
  } else {
    eligible_mask <- logical(n)
    eids <- id_of[eligible_pool]
    eids <- eids[!is.na(eids)]
    eligible_mask[eids] <- TRUE
  }

  list(
    node_names    = node_names,
    id_of         = id_of,
    adj_int       = adj_int,
    n             = n,
    metric_by_id  = metric_by_id,
    metric2_by_id = metric2_by_id,
    eligible_mask = eligible_mask
  )
}

#' Grow a connected block via randomized BFS (integer fast path)
#'
#' Integer-indexed reimplementation of the \code{bfs_grow_block()} inner loop
#' that operates on a precomputed \code{\link{bfs_build_context}} object. With the
#' default \code{density_aware = FALSE} it reproduces the character-vector
#' implementation exactly — same frontier order, same \code{sample.int()} draw
#' sequence, same returned block — but replaces the per-step
#' \code{igraph::neighbors()} calls and character \code{setdiff}/\code{intersect}/
#' \code{unique} with integer lookups and logical-mask filters.
#'
#' With \code{density_aware = TRUE} the only change is the frontier choice: instead
#' of a uniform-random pick, the grower restricts the frontier to neighbors that
#' keep the block's aggregate density (capacity / area) at or above
#' \code{min_density}, then samples among them with a softmax weight that favors
#' the densest additions. Every other step — seeding, the \code{target_max}
#' pre-add check, neighbor bookkeeping, the success test — is identical. This
#' makes connected blocks that stay dense as they grow, which random growth
#' dilutes below \code{min_density} before reaching the capacity target in towns
#' where dense parcels are scarce and scattered. It requires a context built with
#' \code{metric2_lookup} (area) alongside the capacity \code{metric_lookup}.
#'
#' @param ctx A context from \code{\link{bfs_build_context}}
#' @param seed_pool Character vector of eligible seed nodes (NULL = all vertices)
#' @param target_min Minimum metric threshold to reach
#' @param target_max Maximum metric threshold (optional, for pre-add rejection)
#' @param target_exact Optional exact target to grow toward
#' @param check_max_before_add Logical: if TRUE, reject additions exceeding target_max
#' @param density_aware Logical: if TRUE, use the density-preserving frontier
#'   strategy described above. Requires \code{ctx$metric2_by_id} (area) and
#'   \code{min_density}. Default FALSE (uniform-random frontier).
#' @param min_density Minimum aggregate density to maintain at every step when
#'   \code{density_aware = TRUE} (capacity per area, e.g. 15 du/acre).
#' @param beta Softmax temperature on the per-candidate density surplus when
#'   \code{density_aware = TRUE}; larger is greedier toward denser additions.
#' @return Same list shape as \code{\link{bfs_grow_block}}
#' @keywords internal
bfs_grow_block_ctx <- function(ctx,
                               seed_pool = NULL,
                               target_min,
                               target_max = Inf,
                               target_exact = NULL,
                               check_max_before_add = FALSE,
                               density_aware = FALSE,
                               min_density = NULL,
                               beta = 8) {
  node_names    <- ctx$node_names
  id_of         <- ctx$id_of
  adj_int       <- ctx$adj_int
  n             <- ctx$n
  metric_by_id  <- ctx$metric_by_id
  eligible_mask <- ctx$eligible_mask

  # valid_seeds <- intersect(seed_pool, eligible_pool): keep seed_pool order,
  # filter to eligible, then dedup (intersect applies unique last).
  if (is.null(seed_pool)) {
    seed_ids <- seq_len(n)
  } else {
    seed_ids <- unname(id_of[seed_pool])
    seed_ids <- seed_ids[!is.na(seed_ids)]
  }
  seed_ids <- seed_ids[eligible_mask[seed_ids]]
  seed_ids <- seed_ids[!duplicated(seed_ids)]
  if (length(seed_ids) == 0) {
    stop("bfs_grow_block: No valid seeds (seed_pool and eligible_pool are disjoint or empty)")
  }

  effective_target <- if (!is.null(target_exact)) target_exact else target_min

  # Select random seed (sample.int + index to avoid sample(n,1) gotcha). Seeding
  # is shared by both frontier strategies, so the first draw is identical.
  seed_id <- seed_ids[sample.int(length(seed_ids), 1)]
  in_block <- logical(n)
  in_block[seed_id] <- TRUE
  block_ids <- seed_id
  current_metric <- metric_by_id[seed_id]

  # Initial frontier: setdiff(neighbors(seed), seed) then intersect(., eligible).
  fr <- adj_int[[seed_id]]
  fr <- fr[!duplicated(fr)]
  fr <- fr[fr != seed_id]
  fr <- fr[eligible_mask[fr]]

  if (density_aware) {
    area_by_id <- ctx$metric2_by_id
    if (is.null(area_by_id)) {
      stop("bfs_grow_block: density_aware = TRUE requires a context built with metric2_lookup (area)")
    }
    if (is.null(min_density)) {
      stop("bfs_grow_block: density_aware = TRUE requires min_density")
    }
    current_area <- area_by_id[seed_id]

    while (length(fr) > 0 && current_metric < effective_target) {
      cand_cap  <- metric_by_id[fr]
      cand_area <- area_by_id[fr]
      # Capacity/area the block would have after adding each candidate.
      new_cap <- current_metric + cand_cap
      resulting_density <- new_cap / (current_area + cand_area)
      keep <- is.finite(resulting_density) & resulting_density >= min_density
      # Pre-add max check (mirrors the random path's target_max rejection).
      if (check_max_before_add) {
        keep <- keep & (new_cap <= target_max)
      }
      if (!any(keep)) break  # cannot grow further without dropping below min_density

      fk  <- fr[keep]
      # Softmax over the density surplus, stabilised by subtracting the max so a
      # very dense candidate cannot overflow exp(); denser additions are favored
      # but all density-feasible candidates keep positive probability (diversity).
      z <- beta * (resulting_density[keep] - min_density)
      w <- exp(z - max(z))
      next_id <- fk[sample.int(length(fk), 1, prob = w)]

      block_ids <- c(block_ids, next_id)
      in_block[next_id] <- TRUE
      current_metric <- current_metric + metric_by_id[next_id]
      current_area   <- current_area + area_by_id[next_id]

      fr <- fr[fr != next_id]
      nb <- adj_int[[next_id]]
      nb <- nb[!duplicated(nb)]
      nb <- nb[!in_block[nb]]
      nb <- nb[eligible_mask[nb]]
      if (length(nb)) {
        fr <- c(fr, nb)
        fr <- fr[!duplicated(fr)]
      }
    }
  } else {
    while (length(fr) > 0 && current_metric < effective_target) {
      next_id <- fr[sample.int(length(fr), 1)]
      next_metric <- metric_by_id[next_id]

      # Pre-add max check (for cluster_parcels_to_units pattern)
      if (check_max_before_add && (current_metric + next_metric > target_max)) {
        fr <- fr[fr != next_id]
        next
      }

      # Add node to block
      block_ids <- c(block_ids, next_id)
      in_block[next_id] <- TRUE
      current_metric <- current_metric + next_metric

      # Remove just next_id from frontier
      fr <- fr[fr != next_id]

      # New neighbors: setdiff(., current_block) then intersect(., eligible)
      nb <- adj_int[[next_id]]
      nb <- nb[!duplicated(nb)]
      nb <- nb[!in_block[nb]]
      nb <- nb[eligible_mask[nb]]
      if (length(nb)) {
        fr <- c(fr, nb)
        fr <- fr[!duplicated(fr)]
      }
    }
  }

  list(
    block = node_names[block_ids],
    metric_total = unname(current_metric),
    success = (current_metric >= target_min),
    seed = node_names[seed_id]
  )
}

#' Grow a connected block via randomized BFS
#'
#' Generic BFS region-growing that supports various termination conditions
#' and frontier filters. Consolidates multiple BFS patterns used across
#' the codebase for block/component generation.
#'
#' When a precomputed \code{ctx} (from \code{\link{bfs_build_context}}) is
#' supplied, the work is delegated to the integer fast path
#' \code{\link{bfs_grow_block_ctx}}, which is behavior-identical but avoids the
#' per-step igraph neighbor lookups and character set ops. Callers in a tight
#' sampling loop should build the context once and pass it on every call. When
#' \code{ctx} is NULL the original character-vector implementation runs unchanged.
#'
#' @param graph igraph object with named vertices
#' @param metric_lookup Named numeric vector (area or capacity per node)
#' @param seed_pool Character vector of eligible seed nodes (NULL = all vertices)
#' @param eligible_pool Character vector of nodes eligible for expansion (NULL = all vertices)
#' @param target_min Minimum metric threshold to reach
#' @param target_max Maximum metric threshold (optional, for pre-add rejection)
#' @param target_exact Optional exact target to grow toward (overrides target_min for termination)
#' @param check_max_before_add Logical: if TRUE, reject additions that would exceed target_max
#' @param ctx Optional precomputed \code{\link{bfs_build_context}} object. When
#'   supplied, \code{graph}, \code{metric_lookup} and \code{eligible_pool} are
#'   ignored (they are baked into the context).
#' @param density_aware Logical: use the density-preserving frontier strategy of
#'   \code{\link{bfs_grow_block_ctx}}. Requires \code{ctx} (built with
#'   \code{metric2_lookup} = area) and \code{min_density}. Default FALSE.
#' @param min_density Minimum aggregate density to maintain when
#'   \code{density_aware = TRUE}.
#' @param beta Softmax temperature for density-aware frontier selection.
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
                           check_max_before_add = FALSE,
                           ctx = NULL,
                           density_aware = FALSE,
                           min_density = NULL,
                           beta = 8) {

  if (!is.null(ctx)) {
    return(bfs_grow_block_ctx(
      ctx                  = ctx,
      seed_pool            = seed_pool,
      target_min           = target_min,
      target_max           = target_max,
      target_exact         = target_exact,
      check_max_before_add = check_max_before_add,
      density_aware        = density_aware,
      min_density          = min_density,
      beta                 = beta
    ))
  }

  # Density-aware growth needs the id-ordered area metric carried by a context;
  # the character path does not maintain it, so require ctx for that mode.
  if (density_aware) {
    stop("bfs_grow_block: density_aware = TRUE requires a precomputed ctx (build with metric2_lookup = area)")
  }

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
