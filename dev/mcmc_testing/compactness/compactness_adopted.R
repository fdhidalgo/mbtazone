library(targets)
library(sf)
library(data.table)
library(igraph)

# ── Main loop ──────────────────────────────────────────────────────────────

community_info <- data.table::fread("inst/extdata/community_info.csv")
# Expected columns: community_name, community_type

results <- vector("list", nrow(community_info))

for (i in seq_len(nrow(community_info))) {
  district_name <- community_info$community_name[i]
  district_type <- community_info$community_type[i]
  store         <- paste0("ext/_targets_", gsub(" ", "_", district_name))

  cat(sprintf("\n[%d/%d] %s (%s)\n", i, nrow(community_info), district_name, district_type))

  # ── Check store exists ───────────────────────────────────────────────────
  if (!dir.exists(store)) {
    warning(sprintf("  ⚠ Store not found for '%s' — skipping.", district_name))
    next
  }

  # ── Load required targets ─────────────────────────────────────────────────
  loaded <- tryCatch({
    tar_load(parcel_graph_result, store = store, envir = environment())
    tar_load(district_data,       store = store, envir = environment())
    TRUE
  }, error = function(e) {
    warning(sprintf("  ⚠ Could not load targets for '%s': %s — skipping.",
                    district_name, conditionMessage(e)))
    FALSE
  })
  if (!loaded) next

  # ── Validate objects ──────────────────────────────────────────────────────
  pg <- parcel_graph_result$parcel_graph
  pa <- parcel_graph_result$parcel_assignments

  if (is.null(pg) || igraph::vcount(pg) == 0) {
    warning(sprintf("  ⚠ Empty parcel graph for '%s' — skipping.", district_name))
    next
  }

  adopted <- district_data$district_boundary
  if (is.null(adopted) || nrow(adopted) == 0) {
    warning(sprintf("  ⚠ No adopted boundary for '%s' — skipping.", district_name))
    next
  }

  # ── Resolve adopted node IDs (mirrors baseline script logic) ─────────────
  adopted_nodes <- tryCatch({
    adopted_proj  <- sf::st_transform(adopted, sf::st_crs(district_data$district_geometry))
    adopted_union <- sf::st_union(adopted_proj)
    centroids     <- sf::st_centroid(district_data$district_geometry)
    within_mask   <- sf::st_within(centroids, adopted_union, sparse = FALSE)[, 1]
    adopted_parcels <- district_data$district_geometry[within_mask, ]
    adopted_ids     <- adopted_parcels$LOC_ID
    adopted_units   <- pa[parcel_id %in% adopted_ids, unique(unit_id)]
    adopted_units[adopted_units %in% igraph::V(pg)$name]
  }, error = function(e) {
    warning(sprintf("  ⚠ Could not resolve adopted nodes for '%s': %s — skipping.",
                    district_name, conditionMessage(e)))
    NULL
  })

  if (is.null(adopted_nodes) || length(adopted_nodes) == 0) {
    warning(sprintf("  ⚠ No adopted nodes in graph for '%s' — skipping.", district_name))
    next
  }

  # ── Compute metrics ───────────────────────────────────────────────────────
  metrics <- tryCatch(
    compute_graph_compactness(pg, adopted_nodes),
    error = function(e) {
      warning(sprintf("  ⚠ Metric computation failed for '%s': %s — skipping.",
                      district_name, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(metrics)) next

  cat(sprintf("    nodes            : %d\n",   metrics$n_nodes))
  cat(sprintf("    components       : %d\n",   metrics$n_components))
  cat(sprintf("    largest comp %%   : %.1f%%\n", metrics$largest_comp_pct * 100))
  cat(sprintf("    isoperimetric    : %.4f\n", metrics$isoperimetric))
  cat(sprintf("    fiedler (norm.)  : %.6f\n", metrics$fiedler_norm))
  cat(sprintf("    diameter ratio   : %.4f\n", metrics$diameter_ratio))
  cat(sprintf("    internal deg ratio: %.4f\n", metrics$internal_deg_ratio))
  cat(sprintf("    holes            : %d\n",   metrics$n_holes))

  results[[i]] <- data.table(
    community_name     = district_name,
    community_type     = district_type,
    n_nodes            = metrics$n_nodes,
    n_components       = metrics$n_components,
    largest_comp_pct   = metrics$largest_comp_pct,
    isoperimetric      = metrics$isoperimetric,
    fiedler_norm       = metrics$fiedler_norm,
    diameter_ratio     = metrics$diameter_ratio,
    internal_deg_ratio = metrics$internal_deg_ratio,
    n_holes            = metrics$n_holes
  )
}

# ── Combine & summarise ────────────────────────────────────────────────────
compactness_dt <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

cat("\n\n══ Summary ══════════════════════════════════════════════════════════\n")
cat(sprintf("Districts processed : %d / %d\n", nrow(compactness_dt), nrow(community_info)))

if (nrow(compactness_dt) > 0) {
  cat("\nBy community type:\n")
  print(
    compactness_dt[, .(
      n                  = .N,
      isoperimetric      = round(mean(isoperimetric,        na.rm = TRUE), 3),
      fiedler_norm       = round(mean(fiedler_norm,         na.rm = TRUE), 5),
      diameter_ratio     = round(mean(diameter_ratio,       na.rm = TRUE), 3),
      internal_deg_ratio = round(mean(internal_deg_ratio,   na.rm = TRUE), 3),
      pct_with_holes     = round(mean(n_holes > 0)        * 100, 1),
      pct_fragmented     = round(mean(n_components > 1)   * 100, 1)
    ), by = community_type]
  )

  cat("\nFlagged districts (components > 1 OR holes > 0 OR diameter_ratio > 3):\n")
  flagged <- compactness_dt[n_components > 1 | n_holes > 0 | diameter_ratio > 3]
  if (nrow(flagged) > 0) print(flagged) else cat("  None\n")
}

data.table::fwrite(compactness_dt, "./dev/mcmc_testing/compactness/compactness_graph_results.csv")
cat("\nResults written to compactness_graph_results.csv\n")
