# ── Helpers ────────────────────────────────────────────────────────────────

# ── Per-component metric functions ────────────────────────────────────────
#
# All three shape metrics are computed per component then aggregated as a
# node-count-weighted mean. This ensures:
#   - a plan with 4 compact components scores well
#   - a single tendril component drags the score down proportionally
#   - scatter of components is irrelevant (only shape matters)
#
# Isoperimetric definition:
#   interior edge = both endpoints adopted (ANY component)
#   boundary edge = exactly one endpoint adopted, one non-adopted
# Edges between two adopted nodes in different components are interior —
# they represent genuine parcel adjacency within the plan.

#' Isoperimetric ratio for a single connected component.
#' pg          : full parcel graph
#' comp_nodes  : node names in this component
#' adopted_set : all adopted node names (across all components)
#'
#' We filter the FULL graph edge list for edges touching this component,
#' then classify each as interior (both endpoints adopted) or boundary
#' (exactly one endpoint adopted). Edges between two adopted components
#' count as interior — only edges to non-adopted nodes are boundary.
isoperimetric_one <- function(pg, comp_nodes, adopted_set, all_edges) {
  # Edges where at least one endpoint is in this component
  touches_comp <- all_edges[, 1] %in% comp_nodes | all_edges[, 2] %in% comp_nodes
  comp_edges   <- all_edges[touches_comp, , drop = FALSE]
  if (nrow(comp_edges) == 0) return(NA_real_)

  src_adopted <- comp_edges[, 1] %in% adopted_set
  dst_adopted <- comp_edges[, 2] %in% adopted_set

  n_interior <- sum( src_adopted &  dst_adopted)
  n_boundary <- sum( src_adopted & !dst_adopted) +
    sum(!src_adopted &  dst_adopted)

  total <- n_interior + n_boundary
  if (total == 0) return(NA_real_)
  n_interior / total
}

#' Fiedler value for a single connected component, normalised by node count.
fiedler_one <- function(comp_subgraph) {
  n <- igraph::vcount(comp_subgraph)
  if (n < 3) return(NA_real_)
  L      <- igraph::laplacian_matrix(comp_subgraph, sparse = FALSE)
  evals  <- sort(eigen(L, symmetric = TRUE, only.values = TRUE)$values)
  evals[2] / n
}

#' Diameter / sqrt(n) for a single connected component.
diameter_ratio_one <- function(comp_subgraph) {
  n <- igraph::vcount(comp_subgraph)
  if (n < 2) return(NA_real_)
  igraph::diameter(comp_subgraph, unconnected = FALSE) / sqrt(n)
}

#' Mean internal degree ratio for a single connected component.
#'
#' For each adopted node, compute the fraction of its edges (in the full graph)
#' that connect to other adopted nodes. Average across all nodes in the component.
#'
#' 1.0 = every neighbour of every node is also adopted (dense interior blob)
#' 0.0 = every node's edges all lead to non-adopted parcels (pure danglers)
#'
#' This directly catches the "dangling parcel" pattern: a node connected to the
#' plan by a single edge will have a very low internal degree ratio even if the
#' rest of the plan is compact.
internal_degree_ratio_one <- function(comp_nodes, adopted_set, all_edges) {
  ratios <- vapply(comp_nodes, function(node) {
    # All edges touching this node in the full graph
    touches <- all_edges[, 1] == node | all_edges[, 2] == node
    node_edges <- all_edges[touches, , drop = FALSE]
    if (nrow(node_edges) == 0L) return(NA_real_)
    # Neighbour is the other endpoint
    neighbours <- ifelse(node_edges[, 1] == node, node_edges[, 2], node_edges[, 1])
    sum(neighbours %in% adopted_set) / length(neighbours)
  }, numeric(1L))
  mean(ratios, na.rm = TRUE)
}

#' Node-count weighted mean of a per-component metric, ignoring NAs.
weighted_mean_metric <- function(values, sizes) {
  keep <- !is.na(values)
  if (!any(keep)) return(NA_real_)
  sum(values[keep] * sizes[keep]) / sum(sizes[keep])
}

#' Swiss cheese: non-adopted nodes whose every neighbour in pg is adopted.
count_holes <- function(pg, adopted_nodes) {
  non_adopted <- setdiff(igraph::V(pg)$name, adopted_nodes)
  if (length(non_adopted) == 0L) return(0L)
  sum(vapply(non_adopted, function(node) {
    nbrs <- igraph::neighbors(pg, node, mode = "all")$name
    length(nbrs) > 0L && all(nbrs %in% adopted_nodes)
  }, logical(1L)))
}

#' Wrapper: compute all metrics, iterating over components.
compute_graph_compactness <- function(pg, adopted_nodes) {
  subgraph   <- igraph::induced_subgraph(pg, adopted_nodes)
  comps      <- igraph::components(subgraph)
  n_nodes    <- igraph::vcount(subgraph)
  n_comps    <- comps$no
  adopted_set <- adopted_nodes  # for isoperimetric interior-edge rule

  # Split into per-component node lists and subgraphs
  comp_node_lists <- lapply(seq_len(n_comps), function(k) {
    igraph::V(subgraph)$name[comps$membership == k]
  })
  comp_sizes    <- vapply(comp_node_lists, length, integer(1L))
  comp_subgraphs <- lapply(comp_node_lists, function(nodes) {
    igraph::induced_subgraph(subgraph, nodes)
  })

  # Pre-compute full edge list once — reused for every component's isoperimetric
  all_edges   <- igraph::as_edgelist(pg, names = TRUE)

  # Per-component metrics
  iso_vals      <- mapply(isoperimetric_one,
                          comp_nodes = comp_node_lists,
                          MoreArgs   = list(pg = pg, adopted_set = adopted_set,
                                            all_edges = all_edges))
  fiedler_vals  <- vapply(comp_subgraphs, fiedler_one,          numeric(1L))
  diam_vals     <- vapply(comp_subgraphs, diameter_ratio_one,   numeric(1L))
  idr_vals      <- vapply(comp_node_lists, internal_degree_ratio_one,
                          numeric(1L),
                          adopted_set = adopted_set, all_edges = all_edges)

  list(
    n_nodes          = n_nodes,
    n_components     = n_comps,
    largest_comp_pct = round(max(comp_sizes) / n_nodes, 4),
    isoperimetric    = round(weighted_mean_metric(iso_vals,     comp_sizes), 4),
    fiedler_norm     = round(weighted_mean_metric(fiedler_vals, comp_sizes), 6),
    diameter_ratio   = round(weighted_mean_metric(diam_vals,    comp_sizes), 4),
    internal_deg_ratio = round(weighted_mean_metric(idr_vals,   comp_sizes), 4),
    n_holes          = count_holes(pg, adopted_nodes)
  )
}
