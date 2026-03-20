# parcel_library.R - Block library construction for parcel MCMC
#
# Builds two libraries of connected parcel-blocks:
# - Secondary library (L_sec): Blocks with area >= 5 acres for atomic birth/death
# - LCC library (L_LCC): Blocks satisfying global feasibility for LCC relocation
#
# Uses integer indices with R's native %in% for efficient set operations.

# ============================================================================
# INDEX UTILITIES
# ============================================================================

#' Compute active mask for library blocks
#'
#' Active entries are non-NULL, non-NA blocks that are not evicted.
#'
#' @param library Block library
#' @return Logical vector indicating active block positions
#' @keywords internal
compute_library_active_mask <- function(library) {
  n_blocks <- library$n_blocks
  if (n_blocks == 0) return(logical(0))

  blocks <- library$blocks
  blocks_valid <- vapply(seq_len(n_blocks), function(i) {
    block_i <- blocks[[i]]
    !(is.null(block_i) || length(block_i) == 0 || all(is.na(block_i)))
  }, logical(1))

  has_source <- !is.null(library$metadata$source)
  if (!has_source) return(blocks_valid)

  source_vec <- library$metadata$source
  not_evicted <- is.na(source_vec) | source_vec != "evicted"
  blocks_valid & not_evicted
}

#' Convert parcel IDs to integer indices
#'
#' @param parcel_ids Character or integer vector of parcel IDs
#' @param parcel_names Character vector of all parcel names
#' @return Integer vector of indices
parcel_ids_to_indices <- function(parcel_ids, parcel_names) {
  if (length(parcel_ids) == 0) return(integer(0))
  if (!is.character(parcel_ids)) {
    return(as.integer(parcel_ids[!is.na(parcel_ids)]))
  }
  idx <- match(parcel_ids, parcel_names)
  idx[!is.na(idx)]
}

#' Select blocks by coverage-aware greedy algorithm
#'
#' Selects blocks to maximize geographic coverage. Each block is scored by how
#' much it improves coverage of under-covered parcels: score = sum(1/(coverage[p]+1)).
#' This naturally favors blocks in underrepresented areas.
#'
#' Works with any block type (LCCs, secondaries) as long as the data.table has
#' a parcel_ids column containing character vectors of parcel IDs.
#'
#' @param candidate_blocks data.table with parcel_ids column (list of character vectors)
#' @param max_size Maximum number of blocks to select
#' @param parcel_names Character vector of all parcel names
#' @param verbose Print progress messages (default TRUE)
#' @return data.table of selected blocks
select_blocks_by_coverage <- function(candidate_blocks, max_size, parcel_names,
                                      verbose = TRUE) {
  n_parcels <- length(parcel_names)
  n_candidates <- nrow(candidate_blocks)

  if (n_candidates <= max_size) {
    if (verbose) {
      cli::cli_alert_info("All {n_candidates} blocks fit in library (max={max_size})")
    }
    return(candidate_blocks)
  }

  if (verbose) {
    cli::cli_alert_info(
      "Coverage-aware selection: choosing {max_size} from {n_candidates} blocks"
    )
  }

  # Build parcel index lookup
  parcel_idx <- setNames(seq_along(parcel_names), parcel_names)

  # Initialize coverage counts (how many selected blocks contain each parcel)
  coverage <- integer(n_parcels)

  # Precompute parcel indices for each block (once)
  # Sanitize: unique, non-NA indices only
  if (verbose) cli::cli_alert_info("  Precomputing parcel indices...")
  parcel_indices_list <- lapply(candidate_blocks$parcel_ids, function(pids) {
    idx <- parcel_idx[as.character(pids)]
    idx <- unique(idx[!is.na(idx)])
    idx
  })

  # Build inverted index: parcel -> list of candidate indices containing it
  # This enables incremental score updates (O(overlap) instead of O(n_candidates))
  if (verbose) cli::cli_alert_info("  Building inverted index...")
  parcel_to_candidates <- vector("list", n_parcels)
  for (j in seq_len(n_candidates)) {
    for (p in parcel_indices_list[[j]]) {
      parcel_to_candidates[[p]] <- c(parcel_to_candidates[[p]], j)
    }
  }

  # Precompute initial scores for all candidates
  # Score = sum(1 / (coverage[p] + 1)) for p in LCC
  # Initially coverage = 0, so score = number of parcels in LCC
  if (verbose) cli::cli_alert_info("  Computing initial scores...")
  scores <- vapply(parcel_indices_list, length, integer(1), USE.NAMES = FALSE)
  scores <- as.numeric(scores)

  # Track which LCCs are selected (by row index)
  selected_mask <- logical(n_candidates)
  n_selected <- 0L

  # Greedy selection loop with incremental updates
  if (verbose) cli::cli_alert_info("  Running greedy selection...")

  for (i in seq_len(max_size)) {
    # Find best unselected candidate (already have scores)
    # Set selected candidates to -Inf so they're never picked
    candidate_scores <- scores
    candidate_scores[selected_mask] <- -Inf

    best_idx <- which.max(candidate_scores)

    if (candidate_scores[best_idx] == -Inf) break

    # Select the best block
    selected_mask[best_idx] <- TRUE
    n_selected <- n_selected + 1L

    # Get parcels in selected block
    selected_parcels <- parcel_indices_list[[best_idx]]

    # Update coverage and incrementally update scores
    # For each parcel p in selected block:
    #   old contribution to any block containing p: 1/(coverage[p] + 1)
    #   new contribution: 1/(coverage[p] + 2)
    #   delta = 1/(k+2) - 1/(k+1) where k = coverage[p]
    for (p in selected_parcels) {
      k <- coverage[p]
      delta <- 1.0 / (k + 2) - 1.0 / (k + 1)

      # Update all candidates containing this parcel
      affected <- parcel_to_candidates[[p]]
      scores[affected] <- scores[affected] + delta

      # Update coverage
      coverage[p] <- k + 1L
    }

    if (verbose && (i %% 500 == 0 || i == max_size)) {
      min_cov <- if (any(coverage > 0)) min(coverage[coverage > 0]) else 0
      max_cov <- max(coverage)
      n_covered <- sum(coverage > 0)
      cli::cli_alert_info(
        "  Selected {i}/{max_size}: {n_covered} parcels covered, range [{min_cov}, {max_cov}]"
      )
    }
  }

  # Return selected blocks
  result <- candidate_blocks[selected_mask]

  if (verbose) {
    n_covered <- sum(coverage > 0)
    min_cov <- if (n_covered > 0) min(coverage[coverage > 0]) else 0
    cli::cli_alert_success(
      "Coverage selection complete: {n_selected} blocks, {n_covered}/{n_parcels} parcels covered, min coverage = {min_cov}"
    )
  }

  result
}

#' Select blocks by coverage-aware greedy algorithm, stratified by band
#'
#' Applies coverage-aware selection within each size band to ensure both
#' geographic diversity (via coverage) and size diversity (via band quotas).
#' This is more robust across municipalities with varying graph topologies.
#'
#' @param candidate_blocks data.table with parcel_ids and band_column columns
#' @param max_total Maximum total blocks to select across all bands
#' @param parcel_names Character vector of all parcel names
#' @param band_column Name of the column containing band identifiers (default "size_band")
#' @param verbose Print progress messages (default TRUE)
#' @return data.table of selected blocks from all bands
select_blocks_by_coverage_per_band <- function(candidate_blocks, max_total,
                                               parcel_names,
                                               band_column = "size_band",
                                               verbose = TRUE) {
  # Early return for empty input or zero quota
  if (nrow(candidate_blocks) == 0 || max_total <= 0) {
    return(candidate_blocks[0])
  }

  # Get unique bands
  if (!band_column %in% names(candidate_blocks)) {
    # No band column - fall back to global selection
    if (verbose) {
      cli::cli_alert_warning(
        "No '{band_column}' column found, using global coverage selection"
      )
    }
    return(select_blocks_by_coverage(candidate_blocks, max_total, parcel_names, verbose))
  }

  # Precompute normalized band keys (handle NA explicitly)
  band_key <- as.character(candidate_blocks[[band_column]])
  band_key[is.na(band_key)] <- "__NA__"

  bands <- unique(band_key)
  n_bands <- length(bands)

  if (n_bands == 0) {
    return(candidate_blocks[0])
  }

  if (verbose) {
    cli::cli_alert_info(
      "Per-band coverage selection: {max_total} slots across {n_bands} bands"
    )
  }

  # Count candidates per band (using precomputed keys)
  band_counts <- data.table::data.table(band = band_key)[, .N, by = band]

  # Allocate quota proportionally to candidates
  # When max_total < n_bands, only top bands get slots (no minimum guarantee)
  total_candidates <- sum(band_counts$N)
  band_counts[, quota := round(N / total_candidates * max_total)]

  # Ensure at least some bands get slots when max_total > 0
  if (sum(band_counts$quota) == 0 && max_total > 0) {
    # Give 1 slot to the largest band
    data.table::setorder(band_counts, -N)
    band_counts$quota[1] <- 1L
  }

  # Adjust if total quota exceeds max_total (due to rounding)
  while (sum(band_counts$quota) > max_total) {
    # Reduce smallest non-zero quota
    nonzero_idx <- which(band_counts$quota > 0)
    min_idx <- nonzero_idx[which.min(band_counts$quota[nonzero_idx])]
    band_counts$quota[min_idx] <- band_counts$quota[min_idx] - 1L
  }

  # Adjust if total quota is less than max_total (redistribute to largest bands)
  remaining <- max_total - sum(band_counts$quota)
  if (remaining > 0) {
    data.table::setorder(band_counts, -N)
    for (i in seq_len(min(remaining, nrow(band_counts)))) {
      band_counts$quota[i] <- band_counts$quota[i] + 1L
    }
  }

  # Filter to bands with non-zero quota
  band_counts <- band_counts[quota > 0]

  if (verbose) {
    for (i in seq_len(nrow(band_counts))) {
      display_band <- if (band_counts$band[i] == "__NA__") "NA" else band_counts$band[i]
      cli::cli_alert_info(
        "  Band {display_band}: {band_counts$N[i]} candidates -> {band_counts$quota[i]} slots"
      )
    }
  }

  # Select from each band using coverage-aware selection
  selected_list <- lapply(seq_len(nrow(band_counts)), function(i) {
    band_val <- band_counts$band[i]
    quota <- band_counts$quota[i]

    # Filter using precomputed band_key (O(n) scan, done once per band)
    band_blocks <- candidate_blocks[band_key == band_val]

    if (nrow(band_blocks) == 0) {
      return(band_blocks)
    }

    if (verbose) {
      display_band <- if (band_val == "__NA__") "NA" else band_val
      cli::cli_alert_info("  Selecting from band {display_band}...")
    }

    # Apply coverage-aware selection within this band
    select_blocks_by_coverage(
      band_blocks,
      max_size = quota,
      parcel_names = parcel_names,
      verbose = FALSE  # Suppress per-band verbosity to reduce noise
    )
  })

  # Combine results
  result <- data.table::rbindlist(selected_list, fill = TRUE)

  if (verbose) {
    cli::cli_alert_success(
      "Per-band selection complete: {nrow(result)} blocks selected"
    )
  }

  result
}

#' Get block indices from library
#'
#' @param library Block library
#' @param block_id Integer block ID
#' @return Integer vector of parcel indices
library_block_indices <- function(library, block_id) {
  block <- library$blocks[[block_id]]
  if (is.null(block) || length(block) == 0) return(integer(0))
  parcel_ids_to_indices(block, library$parcel_names)
}

#' Get block parcel IDs from library
#'
#' @param library Block library
#' @param block_id Integer block ID
#' @return Character vector of parcel IDs
library_block_parcels <- function(library, block_id) {
  idx <- library_block_indices(library, block_id)
  if (length(idx) == 0) return(character(0))
  library$parcel_names[idx]
}

#' Get parcel IDs for multiple block IDs
#'
#' @param library Block library
#' @param block_ids Integer vector of block IDs
#' @return Character vector of parcel IDs
library_blocks_parcels <- function(library, block_ids) {
  if (length(block_ids) == 0) return(character(0))
  blocks <- library$blocks[block_ids]
  if (length(blocks) == 0) return(character(0))
  parcel_names <- library$parcel_names
  if (is.character(blocks[[1]])) {
    return(unlist(blocks, use.names = FALSE))
  }
  unlist(lapply(blocks, function(b) parcel_names[b]), use.names = FALSE)
}

#' Compute neighbors of a set of parcels
#'
#' @param parcel_ids Character vector of parcel IDs
#' @param parcel_graph igraph object
#' @param neighbor_cache Optional named list of precomputed neighbors (for speed)
#' @return Character vector of neighbor parcel IDs (excluding input parcels)
get_parcel_set_neighbors <- function(parcel_ids, parcel_graph, neighbor_cache = NULL) {
  # Use lapply to avoid O(n²) vector growth from repeated c() calls
  all_nbrs <- lapply(parcel_ids, function(mid) {
    if (!is.null(neighbor_cache)) {
      neighbor_cache[[mid]]
    } else {
      igraph::neighbors(parcel_graph, mid)$name
    }
  })
  unique(setdiff(unlist(all_nbrs, use.names = FALSE), parcel_ids))
}

#' Compute station proximity floor thresholds from constraints
#'
#' Returns absolute minimums for capacity_in_station and area_in_station
#' that an LCC must meet. Only binds when station_*_pct > 50%.
#'
#' @param constraints MBTA constraints list
#' @return List with check_station_cap, check_station_area,
#'   station_cap_min, station_area_min
compute_station_lcc_thresholds <- function(constraints) {
  check_station_cap  <- !is.na(constraints$station_capacity_pct) &&
    constraints$station_capacity_pct > 50
  check_station_area <- !is.na(constraints$station_area_pct) &&
    constraints$station_area_pct > 50

  station_cap_fraction  <- if (check_station_cap)
    (constraints$station_capacity_pct - 50) / 100 else 0
  station_area_fraction <- if (check_station_area)
    (constraints$station_area_pct - 50) / 100 else 0

  list(
    check_station_cap  = check_station_cap,
    check_station_area = check_station_area,
    station_cap_min    = constraints$min_capacity * station_cap_fraction,
    station_area_min   = constraints$min_area     * station_area_fraction
  )
}

# ============================================================================
# LCC LIBRARY FROM TREE DISCOVERY
# ============================================================================

#' Combine tree and MCMC discovered LCCs
#'
#' Merges LCC discoveries from tree enumeration and MCMC exploration.
#' Deduplicates by lcc_key (signature) and tracks source (tree, mcmc, or both).
#'
#' @param tree_discovered_lccs Output from discover_lccs_from_trees()
#' @param mcmc_discovered_lccs Output from run_mcmc_discovery_supplement()
#' @return List with combined discovered_lccs in same format as tree discovery
#' @export
combine_discovered_lccs <- function(tree_discovered_lccs, mcmc_discovered_lccs) {
  tree_dt <- tree_discovered_lccs$discovered_lccs
  mcmc_dt <- mcmc_discovered_lccs$discovered_lccs

  # Add source column to tree discoveries if not present
  if (!"source" %in% names(tree_dt)) {
    tree_dt[, source := "tree"]
  }

  # Add tree_count to MCMC if not present (default to 0)
  if (!"tree_count" %in% names(mcmc_dt)) {
    mcmc_dt[, tree_count := 0L]
  }

  # Add tree_count to tree if not present
  if (!"tree_count" %in% names(tree_dt)) {
    tree_dt[, tree_count := 1L]
  }

  n_tree <- nrow(tree_dt)
  n_mcmc <- nrow(mcmc_dt)

  cli::cli_h2("Combining LCC Discoveries")
  cli::cli_alert_info("Tree discoveries: {n_tree}")
  cli::cli_alert_info("MCMC discoveries: {n_mcmc}")

  if (n_tree == 0 && n_mcmc == 0) {
    cli::cli_alert_warning("No LCCs discovered from either source")
    return(list(
      discovered_lccs = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        tree_count = integer(0),
        source = character(0)
      ),
      n_tree = 0L,
      n_mcmc = 0L,
      n_combined = 0L,
      n_overlap = 0L
    ))
  }

  if (n_mcmc == 0) {
    # Only tree discoveries
    cli::cli_alert_success("Combined: {n_tree} LCCs (tree only)")
    return(list(
      discovered_lccs = tree_dt,
      n_tree = n_tree,
      n_mcmc = 0L,
      n_combined = n_tree,
      n_overlap = 0L
    ))
  }

  if (n_tree == 0) {
    # Only MCMC discoveries
    cli::cli_alert_success("Combined: {n_mcmc} LCCs (MCMC only)")
    return(list(
      discovered_lccs = mcmc_dt,
      n_tree = 0L,
      n_mcmc = n_mcmc,
      n_combined = n_mcmc,
      n_overlap = 0L
    ))
  }

  # Both sources - merge by lcc_key
  # Find overlap
  overlap_keys <- intersect(tree_dt$lcc_key, mcmc_dt$lcc_key)
  n_overlap <- length(overlap_keys)

  # MCMC-only LCCs (not found by tree)
  mcmc_only <- mcmc_dt[!lcc_key %in% tree_dt$lcc_key]

  # For tree LCCs also found by MCMC, update source to "both"
  tree_dt[lcc_key %in% overlap_keys, source := "both"]

  # Combine: all tree + MCMC-only
  combined <- data.table::rbindlist(
    list(tree_dt, mcmc_only),
    fill = TRUE
  )

  n_combined <- nrow(combined)
  n_mcmc_only <- nrow(mcmc_only)

  cli::cli_alert_success(
    "Combined: {n_combined} unique LCCs (tree: {n_tree}, MCMC-only: {n_mcmc_only}, overlap: {n_overlap})"
  )

  list(
    discovered_lccs = combined,
    n_tree = n_tree,
    n_mcmc = n_mcmc,
    n_combined = n_combined,
    n_overlap = n_overlap
  )
}


#' Combine tree and BFS discovered blocks (generalized)
#'
#' Merges block discoveries from tree enumeration and BFS exploration.
#' Deduplicates by key column and tracks source (tree, bfs, or both).
#' Works for both LCC and secondary block discoveries.
#'
#' @param tree_discovered Output from discover_*_from_trees()
#' @param bfs_discovered Output from run_bfs_*_supplement()
#' @param key_column Column name for deduplication key ("lcc_key" or "sec_key")
#' @param block_type Character label for logging ("LCC" or "secondary")
#' @param verbose Print progress (default TRUE)
#' @return List with combined discovered_blocks in same format as tree discovery
#' @export
combine_discovered_blocks <- function(
    tree_discovered,
    bfs_discovered,
    key_column = "lcc_key",
    block_type = "LCC",
    verbose = TRUE
) {
  # Extract data.tables from list wrappers
  tree_dt <- if (data.table::is.data.table(tree_discovered)) {
    tree_discovered
  } else if ("discovered_lccs" %in% names(tree_discovered)) {
    tree_discovered$discovered_lccs
  } else if ("discovered_secondaries" %in% names(tree_discovered)) {
    tree_discovered$discovered_secondaries
  } else {
    tree_discovered[[1]]
  }

  bfs_dt <- if (data.table::is.data.table(bfs_discovered)) {
    bfs_discovered
  } else if ("discovered_lccs" %in% names(bfs_discovered)) {
    bfs_discovered$discovered_lccs
  } else if ("discovered_secondaries" %in% names(bfs_discovered)) {
    bfs_discovered$discovered_secondaries
  } else {
    bfs_discovered[[1]]
  }

  # Add source column if not present
  if (!"source" %in% names(tree_dt) && nrow(tree_dt) > 0) {
    tree_dt[, source := "tree"]
  }
  if (!"source" %in% names(bfs_dt) && nrow(bfs_dt) > 0) {
    bfs_dt[, source := "bfs"]
  }

  # Add tree_count to BFS if not present (default to 0)
  if (!"tree_count" %in% names(bfs_dt) && nrow(bfs_dt) > 0) {
    bfs_dt[, tree_count := 0L]
  }

  n_tree <- nrow(tree_dt)
  n_bfs <- nrow(bfs_dt)

  if (verbose) {
    cli::cli_h2("Combining {block_type} Discoveries")
    cli::cli_alert_info("Tree discoveries: {n_tree}")
    cli::cli_alert_info("BFS discoveries: {n_bfs}")
  }

  if (n_tree == 0 && n_bfs == 0) {
    if (verbose) cli::cli_alert_warning("No {block_type}s discovered from either source")
    return(list(
      discovered_blocks = tree_dt[0],  # Empty with same schema
      n_tree = 0L,
      n_bfs = 0L,
      n_combined = 0L,
      n_overlap = 0L
    ))
  }

  if (n_bfs == 0) {
    if (verbose) cli::cli_alert_success("Combined: {n_tree} {block_type}s (tree only)")
    return(list(
      discovered_blocks = tree_dt,
      n_tree = n_tree,
      n_bfs = 0L,
      n_combined = n_tree,
      n_overlap = 0L
    ))
  }

  if (n_tree == 0) {
    if (verbose) cli::cli_alert_success("Combined: {n_bfs} {block_type}s (BFS only)")
    return(list(
      discovered_blocks = bfs_dt,
      n_tree = 0L,
      n_bfs = n_bfs,
      n_combined = n_bfs,
      n_overlap = 0L
    ))
  }

  # Both sources - merge by key column
  tree_keys <- tree_dt[[key_column]]
  bfs_keys <- bfs_dt[[key_column]]

  overlap_keys <- intersect(tree_keys, bfs_keys)
  n_overlap <- length(overlap_keys)

  # BFS-only blocks (not found by tree)
  bfs_only <- bfs_dt[!get(key_column) %in% tree_keys]

  # For tree blocks also found by BFS, update source to "both"
  tree_dt[get(key_column) %in% overlap_keys, source := "both"]

  # Combine: all tree + BFS-only
  combined <- data.table::rbindlist(
    list(tree_dt, bfs_only),
    fill = TRUE
  )

  n_combined <- nrow(combined)
  n_bfs_only <- nrow(bfs_only)

  if (verbose) {
    cli::cli_alert_success(
      "Combined: {n_combined} unique {block_type}s (tree: {n_tree}, BFS-only: {n_bfs_only}, overlap: {n_overlap})"
    )
  }

  list(
    discovered_blocks = combined,
    n_tree = n_tree,
    n_bfs = n_bfs,
    n_combined = n_combined,
    n_overlap = n_overlap
  )
}


#' Combine all LCC discovery sources (tree, BFS boundary, BFS stratified)
#'
#' Extends combine_discovered_blocks to handle three LCC discovery sources:
#' - Tree enumeration (uniform coverage of tree-cut LCCs)
#' - BFS boundary supplement (multi-crossing LCCs)
#' - BFS capacity-stratified (targets low-capacity bands)
#'
#' Deduplicates by lcc_key and tracks source provenance.
#'
#' @param tree_discovered Output from discover_lccs_from_trees()
#' @param bfs_discovered Output from run_bfs_lcc_supplement()
#' @param bfs_stratified Output from discover_lccs_by_capacity_bands()
#' @param verbose Print progress. Default: TRUE.
#' @return List with:
#'   - discovered_blocks: combined data.table
#'   - n_tree: count from tree discovery
#'   - n_bfs: count from BFS boundary supplement
#'   - n_stratified: count from BFS capacity-stratified
#'   - n_combined: total unique after deduplication
#' @export
combine_all_lcc_discoveries <- function(
    tree_discovered,
    bfs_discovered,
    bfs_stratified,
    verbose = TRUE
) {
  # Helper to extract data.table from various wrapper formats
  extract_lcc_dt <- function(x, name) {
    if (is.null(x)) return(data.table::data.table())
    if (data.table::is.data.table(x)) return(x)
    if ("discovered_lccs" %in% names(x)) return(x$discovered_lccs)
    if ("discovered_blocks" %in% names(x)) return(x$discovered_blocks)
    if (length(x) > 0 && data.table::is.data.table(x[[1]])) return(x[[1]])
    data.table::data.table()
  }

  tree_dt <- extract_lcc_dt(tree_discovered, "tree")
  bfs_dt <- extract_lcc_dt(bfs_discovered, "bfs")
  strat_dt <- extract_lcc_dt(bfs_stratified, "stratified")

  # Add source columns if missing
  if (nrow(tree_dt) > 0 && !"source" %in% names(tree_dt)) {
    tree_dt[, source := "tree"]
  }
  if (nrow(bfs_dt) > 0 && !"source" %in% names(bfs_dt)) {
    bfs_dt[, source := "bfs"]
  }
  if (nrow(strat_dt) > 0 && !"source" %in% names(strat_dt)) {
    strat_dt[, source := "bfs_stratified"]
  }

  # Add tree_count if missing (for BFS sources)
  if (nrow(bfs_dt) > 0 && !"tree_count" %in% names(bfs_dt)) {
    bfs_dt[, tree_count := 0L]
  }
  if (nrow(strat_dt) > 0 && !"tree_count" %in% names(strat_dt)) {
    strat_dt[, tree_count := 0L]
  }

  n_tree <- nrow(tree_dt)
  n_bfs <- nrow(bfs_dt)
  n_stratified <- nrow(strat_dt)

  if (verbose) {
    cli::cli_h2("Combining All LCC Discoveries")
    cli::cli_alert_info("Tree discoveries: {n_tree}")
    cli::cli_alert_info("BFS boundary discoveries: {n_bfs}")
    cli::cli_alert_info("BFS capacity-stratified discoveries: {n_stratified}")
  }

  # Handle empty case
  if (n_tree == 0 && n_bfs == 0 && n_stratified == 0) {
    if (verbose) cli::cli_alert_warning("No LCCs discovered from any source")
    return(list(
      discovered_blocks = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        tree_count = integer(0),
        source = character(0)
      ),
      n_tree = 0L,
      n_bfs = 0L,
      n_stratified = 0L,
      n_combined = 0L
    ))
  }

  # Collect all keys for overlap tracking
  tree_keys <- if (n_tree > 0) tree_dt$lcc_key else character(0)
  bfs_keys <- if (n_bfs > 0) bfs_dt$lcc_key else character(0)
  strat_keys <- if (n_stratified > 0) strat_dt$lcc_key else character(0)

  # Priority: tree > bfs_boundary > bfs_stratified
  # BFS-only = not in tree
  # Stratified-only = not in tree and not in bfs_boundary
  bfs_only <- if (n_bfs > 0) bfs_dt[!lcc_key %in% tree_keys] else bfs_dt[0]
  strat_only <- if (n_stratified > 0) {
    strat_dt[!lcc_key %in% c(tree_keys, bfs_keys)]
  } else {
    strat_dt[0]
  }

  # Mark tree blocks also found by other sources
  if (n_tree > 0) {
    tree_dt[lcc_key %in% bfs_keys, source := "tree+bfs"]
    tree_dt[lcc_key %in% strat_keys, source := "tree+stratified"]
    tree_dt[lcc_key %in% intersect(bfs_keys, strat_keys), source := "tree+both"]
  }

  # Combine all unique discoveries
  combined <- data.table::rbindlist(
    list(tree_dt, bfs_only, strat_only),
    fill = TRUE
  )

  n_combined <- nrow(combined)
  n_bfs_only <- nrow(bfs_only)
  n_strat_only <- nrow(strat_only)

  if (verbose) {
    cli::cli_alert_success(
      "Combined: {n_combined} unique LCCs (tree: {n_tree}, bfs-only: {n_bfs_only}, stratified-only: {n_strat_only})"
    )
  }

  list(
    discovered_blocks = combined,
    n_tree = n_tree,
    n_bfs = n_bfs,
    n_stratified = n_stratified,
    n_combined = n_combined
  )
}


# ============================================================================
# BFS LCC SUPPLEMENT (REPLACES MCMC DISCOVERY)
# ============================================================================

#' Check if block has multiple boundary crossings (non-tree-cut)
#'
#' Tree cuts have exactly one edge crossing the boundary. Blocks with
#' multiple crossings cannot be represented as tree cuts and are the
#' primary target of BFS supplement exploration.
#'
#' @param block_parcels Character vector of parcel IDs in block
#' @param parcel_graph igraph object
#' @return Integer count of edges crossing the boundary
#' @keywords internal
count_boundary_crossings <- function(block_parcels, parcel_graph) {
  # Build membership mask
  all_parcels <- igraph::V(parcel_graph)$name
  in_block <- all_parcels %in% block_parcels

  # Count edges where exactly one endpoint is in block
  edges <- igraph::as_edgelist(parcel_graph, names = FALSE)
  n_crossing <- 0L
  for (i in seq_len(nrow(edges))) {
    u <- edges[i, 1]
    v <- edges[i, 2]
    if (in_block[u] != in_block[v]) {
      n_crossing <- n_crossing + 1L
    }
  }
  n_crossing
}


#' Discover non-tree-cut LCCs via BFS boundary exploration
#'
#' Replaces MCMC discovery supplement with faster BFS-based exploration.
#' Seeds from tree-discovered LCCs at various capacity quantiles, then
#' explores boundary perturbations to find LCCs with multiple boundary
#' crossings (which tree enumeration cannot discover).
#'
#' @param tree_discovered_lccs Output from discover_lccs_from_trees()
#' @param parcel_graph igraph object with capacity/area attributes
#' @param constraints MBTA constraints list
#' @param n_samples Total BFS explorations to perform (default 100)
#' @param n_seeds Number of seed LCCs to select from tree discoveries (default 10)
#' @param forbidden_parcels Character vector of parcel IDs to exclude (default NULL)
#' @param verbose Print progress (default TRUE)
#' @return List with:
#'   - discovered_lccs: data.table with lcc_key, parcel_ids, capacity, area, source
#'   - n_samples_attempted: number of BFS attempts
#'   - n_valid_found: number of valid LCCs found
#'   - n_unique_found: number of unique LCCs after deduplication
#'   - n_multi_crossing: number with multiple boundary crossings
#' @export
run_bfs_lcc_supplement <- function(
    tree_discovered_lccs,
    parcel_graph,
    constraints,
    n_samples = 100L,
    n_seeds = 10L,
    forbidden_parcels = NULL,
    verbose = TRUE
) {
  # Extract data.table from list wrapper
  tree_dt <- if (data.table::is.data.table(tree_discovered_lccs)) {
    tree_discovered_lccs
  } else {
    tree_discovered_lccs$discovered_lccs
  }

  all_parcels <- igraph::V(parcel_graph)$name
  n_parcels <- length(all_parcels)

  # Compute eligible parcels (excluding forbidden)
  if (!is.null(forbidden_parcels) && length(forbidden_parcels) > 0) {
    eligible_parcels <- setdiff(all_parcels, forbidden_parcels)
    forbidden_set <- forbidden_parcels
  } else {
    eligible_parcels <- all_parcels
    forbidden_set <- character(0)
  }

  # Constraint bounds (no max_capacity - capacity prior handles preference)
  min_lcc_fraction <- constraints$min_lcc_fraction %||% 0.5
  min_lcc_capacity <- constraints$min_capacity * min_lcc_fraction
  min_area <- constraints$min_area
  min_density <- constraints$min_density

  # Station thresholds
  st <- compute_station_lcc_thresholds(constraints)

  # Precompute lookups
  capacity_lookup <- igraph::V(parcel_graph)$capacity
  area_lookup <- igraph::V(parcel_graph)$area
  names(capacity_lookup) <- all_parcels
  names(area_lookup) <- all_parcels

  cap_in_station_lookup  <- igraph::V(parcel_graph)$capacity_in_station
  area_in_station_lookup <- igraph::V(parcel_graph)$area_in_station
  names(cap_in_station_lookup)  <- all_parcels
  names(area_in_station_lookup) <- all_parcels

  if (verbose) {
    cli::cli_h2("BFS LCC Supplement")
    cli::cli_alert_info("Tree discoveries available: {nrow(tree_dt)}")
    cli::cli_alert_info("Target samples: {n_samples} from {n_seeds} seeds")
  }

  if (nrow(tree_dt) == 0) {
    if (verbose) cli::cli_alert_warning("No tree discoveries to seed from")
    return(list(
      discovered_lccs = data.table::data.table(
        lcc_key = character(0),
        parcel_ids = list(),
        capacity = integer(0),
        area = numeric(0),
        tree_count = integer(0),
        source = character(0)
      ),
      n_samples_attempted = 0L,
      n_valid_found = 0L,
      n_unique_found = 0L,
      n_multi_crossing = 0L
    ))
  }

  # Select seed LCCs from capacity quantiles
  tree_dt_sorted <- data.table::copy(tree_dt)
  data.table::setorder(tree_dt_sorted, capacity)
  n_available <- nrow(tree_dt_sorted)

  # Pick seeds at evenly spaced quantiles (10%, 20%, ..., 100%)
  quantile_positions <- seq(0.1, 1.0, length.out = min(n_seeds, n_available))
  seed_indices <- unique(pmax(1L, round(quantile_positions * n_available)))
  seed_lccs <- tree_dt_sorted[seed_indices]

  actual_seeds <- nrow(seed_lccs)
  samples_per_seed <- ceiling(n_samples / actual_seeds)

  if (verbose) {
    cli::cli_alert_info("Using {actual_seeds} seed LCCs, {samples_per_seed} samples each")
  }

  # Hash for deduplication
  lcc_hash <- new.env(hash = TRUE, parent = emptyenv())

  # Also track tree-discovered keys to avoid duplicates
  tree_keys <- tree_dt$lcc_key
  for (key in tree_keys) {
    assign(key, TRUE, envir = lcc_hash)
  }

  n_samples_attempted <- 0L
  n_valid_found <- 0L
  n_unique_found <- 0L
  n_multi_crossing <- 0L

  discovered_list <- list()

  if (verbose) {
    cli::cli_progress_bar(
      "BFS LCC exploration",
      total = actual_seeds * samples_per_seed,
      format = "{cli::pb_spin} Sample {n_samples_attempted}/{actual_seeds * samples_per_seed} | Found: {n_unique_found} unique"
    )
  }

  for (seed_idx in seq_len(actual_seeds)) {
    seed_lcc <- seed_lccs[seed_idx]
    seed_parcels <- seed_lcc$parcel_ids[[1]]

    # Identify boundary parcels (parcels in LCC with neighbors outside LCC)
    seed_set <- seed_parcels
    boundary_parcels <- character(0)

    for (p in seed_parcels) {
      neighbors <- igraph::neighbors(parcel_graph, p)$name
      if (any(!neighbors %in% seed_set)) {
        boundary_parcels <- c(boundary_parcels, p)
      }
    }

    if (length(boundary_parcels) == 0) {
      # No boundary (shouldn't happen for valid LCCs)
      if (verbose) {
        for (i in seq_len(samples_per_seed)) cli::cli_progress_update()
      }
      n_samples_attempted <- n_samples_attempted + samples_per_seed
      next
    }

    # For each sample from this seed, explore a boundary perturbation
    for (sample_idx in seq_len(samples_per_seed)) {
      n_samples_attempted <- n_samples_attempted + 1L
      if (verbose) cli::cli_progress_update()

      # Pick random boundary parcel and grow a new LCC from it
      # NOTE: Use sample.int + indexing to avoid R's sample(n,1) gotcha when length=1
      start_parcel <- boundary_parcels[sample.int(length(boundary_parcels), 1)]

      # Target capacity similar to seed LCC (with some variation)
      # Use DISCOVERY_CAPACITY_MULTIPLIER as upper bound for exploration
      seed_capacity <- seed_lcc$capacity
      max_target <- constraints$min_capacity * DISCOVERY_CAPACITY_MULTIPLIER
      target_capacity <- runif(1, min_lcc_capacity, max_target)

      # BFS grow toward target capacity (using eligible parcels to exclude forbidden)
      result <- tryCatch(
        bfs_grow_block(
          graph = parcel_graph,
          metric_lookup = capacity_lookup,
          seed_pool = start_parcel,
          eligible_pool = eligible_parcels,
          target_min = min_lcc_capacity,
          target_exact = target_capacity
        ),
        error = function(e) NULL
      )

      if (is.null(result) || !result$success) next

      candidate_parcels <- result$block
      candidate_capacity <- result$metric_total
      candidate_area <- sum(area_lookup[candidate_parcels])
      candidate_cap_in_station <- sum(cap_in_station_lookup[candidate_parcels])
      candidate_area_in_station <- sum(area_in_station_lookup[candidate_parcels])

      # Reject candidates containing forbidden parcels (belt-and-suspenders check)
      if (length(forbidden_set) > 0 && any(candidate_parcels %in% forbidden_set)) next

      # Validate against LCC constraints
      if (candidate_capacity < min_lcc_capacity) next
      if (candidate_capacity > max_target) next  # Discovery bound

      if (candidate_area > 0 && candidate_capacity / candidate_area < min_density) next

      # Check station constraints
      if (st$check_station_cap && candidate_cap_in_station < st$station_cap_min) next
      if (st$check_station_area && candidate_area_in_station < st$station_area_min) next

      # Note: BFS guarantees connectivity by construction, no check needed

      n_valid_found <- n_valid_found + 1L

      # Deduplication (use hash to avoid R's 10000 byte variable name limit)
      lcc_key <- digest::digest(sort(candidate_parcels), algo = "xxhash64")
      if (exists(lcc_key, envir = lcc_hash, inherits = FALSE)) next

      assign(lcc_key, TRUE, envir = lcc_hash)
      n_unique_found <- n_unique_found + 1L

      # Check boundary crossings
      n_crossings <- count_boundary_crossings(candidate_parcels, parcel_graph)
      if (n_crossings > 1) {
        n_multi_crossing <- n_multi_crossing + 1L
      }

      discovered_list[[length(discovered_list) + 1L]] <- data.table::data.table(
        lcc_key = lcc_key,
        parcel_ids = list(candidate_parcels),
        capacity = as.integer(candidate_capacity),
        area = candidate_area,
        tree_count = 0L,
        source = "bfs"
      )
    }
  }

  if (verbose) cli::cli_progress_done()

  # Combine discoveries
  if (length(discovered_list) > 0) {
    discovered_lccs <- data.table::rbindlist(discovered_list)
  } else {
    discovered_lccs <- data.table::data.table(
      lcc_key = character(0),
      parcel_ids = list(),
      capacity = integer(0),
      area = numeric(0),
      tree_count = integer(0),
      source = character(0)
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      "BFS supplement: {n_unique_found} unique LCCs from {n_samples_attempted} attempts ({n_multi_crossing} multi-crossing)"
    )
  }

  list(
    discovered_lccs = discovered_lccs,
    n_samples_attempted = n_samples_attempted,
    n_valid_found = n_valid_found,
    n_unique_found = n_unique_found,
    n_multi_crossing = n_multi_crossing
  )
}


# ============================================================================
# CAPACITY-STRATIFIED BFS DISCOVERY
# ============================================================================

#' Discover LCCs via capacity-stratified BFS sampling
#'
#' Explicitly targets capacity bands to populate the library with LCCs
#' in ranges that tree enumeration under-represents. Tree discovery produces
#' 78% LCCs with capacity > 5000, but the capacity prior (lambda=0.005)
#' makes the posterior favor LCCs with capacity 1200-2000.
#'
#' Unlike run_bfs_lcc_supplement() which seeds from tree-discovered LCCs,
#' this function seeds from random parcels across the graph, enabling
#' discovery of LCCs in capacity ranges tree enumeration misses.
#'
#' Algorithm:
#' 1. For each capacity band [cap_low, cap_high]:
#'    a. Select random seed parcels from all eligible parcels
#'    b. BFS-grow toward target capacity within band
#'    c. Validate: capacity in band, area >= min_area, density >= min_density
#'    d. Check connectivity (guaranteed by BFS, but verified)
#'    e. Deduplicate via xxhash64
#' 2. Return all unique LCCs discovered across bands
#'
#' @param parcel_graph igraph object with capacity/area attributes
#' @param constraints MBTA constraints list (min_capacity, min_area, min_density,
#'   min_lcc_fraction)
#' @param capacity_bands_relative List of c(low_mult, high_mult) tuples.
#'   Each tuple defines a band as [low_mult * min_capacity, high_mult * min_capacity].
#'   Default: LCC_CAPACITY_BANDS_RELATIVE from parcel_config.R.
#' @param samples_per_band Number of BFS samples per band.
#'   Default: LCC_BAND_SAMPLES_PER_BAND.
#' @param max_attempts_per_band Maximum BFS attempts before moving on.
#'   Default: LCC_BAND_MAX_ATTEMPTS.
#' @param forbidden_parcels Character vector of parcels to exclude from LCCs.
#'   Default: NULL.
#' @param existing_keys Character vector of xxhash64 keys to skip (from prior
#'   discoveries like tree enumeration). Default: character(0).
#' @param verbose Print progress. Default: TRUE.
#' @return List with:
#'   - discovered_lccs: data.table with lcc_key, parcel_ids, capacity, area,
#'       capacity_band (integer), tree_count = 0, source = "bfs_stratified"
#'   - band_stats: data.table summarizing discoveries per band
#'   - n_total_attempts: total BFS attempts across all bands
#'   - n_unique_found: unique LCCs found
#' @export
discover_lccs_by_capacity_bands <- function(
    parcel_graph,
    constraints,
    capacity_bands_relative = LCC_CAPACITY_BANDS_RELATIVE,
    samples_per_band = LCC_BAND_SAMPLES_PER_BAND,
    max_attempts_per_band = LCC_BAND_MAX_ATTEMPTS,
    forbidden_parcels = NULL,
    existing_keys = character(0),
    verbose = TRUE
) {
  # Extract constraint values
  min_capacity <- constraints$min_capacity
  min_lcc_fraction <- constraints$min_lcc_fraction %||% 0.5
  min_lcc_capacity <- min_capacity * min_lcc_fraction
  min_density <- constraints$min_density

  # Station thresholds
  st <- compute_station_lcc_thresholds(constraints)

  # Convert relative bands to absolute capacity ranges
  bands <- lapply(capacity_bands_relative, function(mult) {
    c(max(min_lcc_capacity, mult[1] * min_capacity),
      mult[2] * min_capacity)
  })

  # Setup
  all_parcels <- igraph::V(parcel_graph)$name
  n_parcels <- length(all_parcels)

  # Compute eligible parcels (excluding forbidden)
  if (!is.null(forbidden_parcels) && length(forbidden_parcels) > 0) {
    eligible_parcels <- setdiff(all_parcels, forbidden_parcels)
    forbidden_set <- forbidden_parcels
  } else {
    eligible_parcels <- all_parcels
    forbidden_set <- character(0)
  }

  # Precompute lookups
  capacity_lookup <- igraph::V(parcel_graph)$capacity
  area_lookup <- igraph::V(parcel_graph)$area
  names(capacity_lookup) <- all_parcels
  names(area_lookup) <- all_parcels
  cap_in_station_lookup  <- igraph::V(parcel_graph)$capacity_in_station
  area_in_station_lookup <- igraph::V(parcel_graph)$area_in_station
  names(cap_in_station_lookup)  <- all_parcels
  names(area_in_station_lookup) <- all_parcels


  if (verbose) {
    cli::cli_h2("Capacity-Stratified BFS LCC Discovery")
    cli::cli_alert_info("Target bands: {length(bands)}")
    cli::cli_alert_info("Samples per band: {samples_per_band}")
    cli::cli_alert_info("Existing keys to skip: {length(existing_keys)}")
    for (i in seq_along(bands)) {
      cli::cli_alert_info("  Band {i}: [{round(bands[[i]][1])}, {round(bands[[i]][2])}] capacity")
    }
  }

  # Initialize deduplication hash with existing keys
  lcc_hash <- new.env(hash = TRUE, parent = emptyenv())
  for (key in existing_keys) {
    assign(key, TRUE, envir = lcc_hash)
  }

  # Track statistics
  n_total_attempts <- 0L
  n_unique_found <- 0L
  discovered_list <- list()
  band_stats_list <- list()

  for (band_idx in seq_along(bands)) {
    band <- bands[[band_idx]]
    cap_low <- band[1]
    cap_high <- band[2]

    band_attempts <- 0L
    band_found <- 0L
    band_valid <- 0L

    if (verbose) {
      cli::cli_progress_bar(
        name = sprintf("Band %d [%d-%d]", band_idx, round(cap_low), round(cap_high)),
        total = samples_per_band,
        format = "{cli::pb_spin} {cli::pb_name} | Found: {band_found}/{samples_per_band} | Attempts: {band_attempts}"
      )
    }

    while (band_found < samples_per_band && band_attempts < max_attempts_per_band) {
      band_attempts <- band_attempts + 1L
      n_total_attempts <- n_total_attempts + 1L

      if (verbose && band_attempts %% 100 == 0) {
        cli::cli_progress_update(set = band_found)
      }

      # Sample random target capacity within band
      target_capacity <- runif(1, cap_low, cap_high)

      # BFS grow from random seed toward target capacity
      result <- tryCatch(
        bfs_grow_block(
          graph = parcel_graph,
          metric_lookup = capacity_lookup,
          seed_pool = eligible_parcels,  # Random seed from all eligible
          eligible_pool = eligible_parcels,
          target_min = cap_low,
          target_exact = target_capacity,
          target_max = cap_high,
          check_max_before_add = TRUE  # Stay within band
        ),
        error = function(e) NULL
      )

      if (is.null(result) || !result$success) next

      candidate_parcels <- result$block
      candidate_capacity <- result$metric_total
      candidate_area <- sum(area_lookup[candidate_parcels])
      candidate_cap_in_station <- sum(cap_in_station_lookup[candidate_parcels])
      candidate_area_in_station <- sum(area_in_station_lookup[candidate_parcels])

      # Reject if contains forbidden parcels
      if (length(forbidden_set) > 0 && any(candidate_parcels %in% forbidden_set)) next

      # Validate capacity within band
      if (candidate_capacity < cap_low || candidate_capacity > cap_high) next

      # Validate density
      if (candidate_area > 0 && candidate_capacity / candidate_area < min_density) next

      # Check station constraints
      if (st$check_station_cap && candidate_cap_in_station < st$station_cap_min) next
      if (st$check_station_area && candidate_area_in_station < st$station_area_min) next

      # Note: BFS guarantees connectivity by construction, no check needed

      band_valid <- band_valid + 1L

      # Deduplication
      lcc_key <- digest::digest(sort(candidate_parcels), algo = "xxhash64")
      if (exists(lcc_key, envir = lcc_hash, inherits = FALSE)) next

      assign(lcc_key, TRUE, envir = lcc_hash)
      band_found <- band_found + 1L
      n_unique_found <- n_unique_found + 1L

      discovered_list[[length(discovered_list) + 1L]] <- data.table::data.table(
        lcc_key = lcc_key,
        parcel_ids = list(candidate_parcels),
        capacity = as.integer(candidate_capacity),
        area = candidate_area,
        capacity_band = band_idx,
        tree_count = 0L,
        source = "bfs_stratified"
      )

      if (verbose) {
        cli::cli_progress_update(set = band_found)
      }
    }

    if (verbose) {
      cli::cli_progress_done()
      cli::cli_alert_success(
        "Band {band_idx}: {band_found} unique LCCs from {band_attempts} attempts ({band_valid} valid)"
      )
    }

    band_stats_list[[band_idx]] <- data.table::data.table(
      band = band_idx,
      cap_low = cap_low,
      cap_high = cap_high,
      attempts = band_attempts,
      valid = band_valid,
      unique_found = band_found
    )
  }

  # Combine discoveries
  if (length(discovered_list) > 0) {
    discovered_lccs <- data.table::rbindlist(discovered_list)
  } else {
    discovered_lccs <- data.table::data.table(
      lcc_key = character(0),
      parcel_ids = list(),
      capacity = integer(0),
      area = numeric(0),
      capacity_band = integer(0),
      tree_count = integer(0),
      source = character(0)
    )
  }

  band_stats <- data.table::rbindlist(band_stats_list)

  if (verbose) {
    cli::cli_alert_success(
      "Capacity-stratified discovery: {n_unique_found} unique LCCs from {n_total_attempts} total attempts"
    )
  }

  list(
    discovered_lccs = discovered_lccs,
    band_stats = band_stats,
    n_total_attempts = n_total_attempts,
    n_unique_found = n_unique_found
  )
}


# ============================================================================
# BFS SECONDARY SUPPLEMENT
# ============================================================================

#' Discover secondary blocks via BFS region-growing (supplement to tree discovery)
#'
#' Samples connected blocks within size bands using BFS region-growing.
#' Skips blocks already found by tree enumeration to avoid duplicates.
#'
#' @param tree_discovered_secondaries Output from discover_secondaries_from_trees()
#' @param parcel_graph igraph object
#' @param size_bands List of c(min_area, max_area) tuples in acres
#' @param quota_per_band Number of blocks to sample per band (default 25)
#' @param density_threshold Minimum density (units/acre) required. Default: 15
#' @param max_attempts Maximum BFS attempts per band (default 500)
#' @param verbose Print progress (default TRUE)
#' @return List with:
#'   - discovered_secondaries: data.table with sec_key, parcel_ids, capacity, area,
#'       size_band, tree_count, source
#'   - n_attempts: total BFS attempts
#'   - n_found: blocks found per band
#' @export
run_bfs_secondary_supplement <- function(
    tree_discovered_secondaries,
    parcel_graph,
    size_bands = list(c(5, 8), c(8, 12), c(12, 20)),
    quota_per_band = 25L,
    density_threshold = 15,
    max_attempts = 500L,
    verbose = TRUE
) {
  # Extract data.table from list wrapper
  tree_dt <- if (data.table::is.data.table(tree_discovered_secondaries)) {
    tree_discovered_secondaries
  } else {
    tree_discovered_secondaries$discovered_secondaries
  }

  all_parcels <- igraph::V(parcel_graph)$name
  n_parcels <- length(all_parcels)

  # Precompute lookups
  area_lookup <- igraph::V(parcel_graph)$area
  capacity_lookup <- igraph::V(parcel_graph)$capacity
  names(area_lookup) <- all_parcels
  names(capacity_lookup) <- all_parcels

  if (verbose) {
    cli::cli_h2("BFS Secondary Supplement")
    cli::cli_alert_info("Tree discoveries: {nrow(tree_dt)}")
    cli::cli_alert_info("Quota per band: {quota_per_band}")
  }

  # Build hash of tree-discovered blocks for deduplication
  sec_hash <- new.env(hash = TRUE, parent = emptyenv())
  if (nrow(tree_dt) > 0) {
    for (key in tree_dt$sec_key) {
      assign(key, TRUE, envir = sec_hash)
    }
  }

  discovered_list <- list()
  band_counts <- setNames(rep(0L, length(size_bands)), paste0("band_", seq_along(size_bands)))
  total_attempts <- 0L

  for (band_idx in seq_along(size_bands)) {
    band <- size_bands[[band_idx]]
    min_area <- band[1]
    max_area <- band[2]
    band_name <- paste0("band_", band_idx)

    if (verbose) {
      cli::cli_alert_info("Band {band_idx}: [{min_area}, {max_area}] acres")
    }

    band_found <- 0L
    band_attempts <- 0L

    while (band_found < quota_per_band && band_attempts < max_attempts) {
      band_attempts <- band_attempts + 1L
      total_attempts <- total_attempts + 1L

      # Random target area within band
      target_area <- runif(1, min_area, max_area)

      # BFS grow
      result <- tryCatch(
        bfs_grow_block(
          graph = parcel_graph,
          metric_lookup = area_lookup,
          seed_pool = NULL,  # Any parcel
          eligible_pool = NULL,
          target_min = min_area,
          target_exact = target_area
        ),
        error = function(e) NULL
      )

      if (is.null(result) || !result$success) next

      block_parcels <- result$block
      block_area <- result$metric_total
      block_capacity <- sum(capacity_lookup[block_parcels])

      # Validate area in band
      if (block_area < min_area || block_area > max_area) next

      # Validate density
      block_density <- if (block_area > 0) block_capacity / block_area else 0
      if (block_density < density_threshold) next

      # Deduplication (use hash to avoid R's 10000 byte variable name limit)
      sec_key <- digest::digest(sort(block_parcels), algo = "xxhash64")
      if (exists(sec_key, envir = sec_hash, inherits = FALSE)) next

      assign(sec_key, TRUE, envir = sec_hash)
      band_found <- band_found + 1L
      band_counts[band_idx] <- band_counts[band_idx] + 1L

      discovered_list[[length(discovered_list) + 1L]] <- data.table::data.table(
        sec_key = sec_key,
        parcel_ids = list(block_parcels),
        capacity = as.integer(block_capacity),
        area = block_area,
        size_band = band_idx,
        tree_count = 0L,
        source = "bfs"
      )
    }

    if (verbose) {
      cli::cli_alert_info("  Found {band_found} blocks in {band_attempts} attempts")
    }
  }

  # Combine discoveries
  if (length(discovered_list) > 0) {
    discovered_secondaries <- data.table::rbindlist(discovered_list)
  } else {
    discovered_secondaries <- data.table::data.table(
      sec_key = character(0),
      parcel_ids = list(),
      capacity = integer(0),
      area = numeric(0),
      size_band = integer(0),
      tree_count = integer(0),
      source = character(0)
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      "BFS supplement: {nrow(discovered_secondaries)} blocks from {total_attempts} attempts"
    )
    cli::cli_alert_info(
      "Band distribution: {paste(sprintf('%s=%d', names(band_counts), band_counts), collapse=', ')}"
    )
  }

  list(
    discovered_secondaries = discovered_secondaries,
    n_attempts = total_attempts,
    n_found = band_counts
  )
}


#' Build LCC library from spanning tree discovery
#'
#' Converts LCCs discovered via spanning tree enumeration into the standard
#' library format for use in replace-LCC moves. Unlike MCMC discovery which
#' has visit counts, tree discovery uses tree_count (number of trees that
#' found each cut) for prioritization.
#'
#' When combining tree and BFS discoveries, this function reserves slots for
#' BFS-only LCCs to ensure the hybrid supplement is preserved even when
#' tree discoveries exceed the library size limit.
#'
#' Selection uses coverage-aware greedy algorithm that maximizes geographic
#' coverage by favoring LCCs that cover under-represented parcels.
#'
#' When capacity-stratified discoveries are present, fixed band quotas are used
#' instead of proportional allocation. This is necessary because tree discovery
#' is biased toward high-capacity LCCs (cuts near the root of large trees tend
#' to produce large subtrees). Fixed quotas ensure the library has enough
#' low-capacity LCCs for good mixing near the posterior mode.
#'
#' Band definitions are relative to min_capacity from constraints:
#'   band_1: < 0.75 * min_capacity  (below LCC minimum - rarely sampled)
#'   band_2: 0.75x - 1.0x           (near-minimum LCCs, posterior favored)
#'   band_3: 1.0x - 1.5x            (moderate capacity)
#'   band_4: > 1.5x                  (high capacity)
#'
#' @param discovered_lccs data.table from discover_lccs_from_trees() or
#'   combine_discovered_lccs(). Expected columns: lcc_key, parcel_ids (list),
#'   capacity, area, tree_count, source (optional)
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints list — must contain min_capacity
#' @param max_library_size Maximum number of LCCs to include (default 5000)
#' @param bfs_reservation Slots to reserve for BFS-only discoveries (default 500)
#' @return Block library structure matching build_lcc_library() output
#' @export
build_lcc_library_from_tree_discovery <- function(discovered_lccs,
                                                  parcel_graph,
                                                  constraints,
                                                  max_library_size = 5000,
                                                  bfs_reservation = 500) {
  if (is.null(constraints) || is.null(constraints$min_capacity)) {
    cli::cli_abort("constraints$min_capacity is required — something has gone seriously wrong")
  }

  min_cap <- constraints$min_capacity

  # Handle both list wrapper and direct data.table input
  if (is.list(discovered_lccs) && !data.table::is.data.table(discovered_lccs)) {
    if ("discovered_lccs" %in% names(discovered_lccs)) {
      discovered_lccs <- discovered_lccs$discovered_lccs
    }
  }

  cli::cli_alert_info("Building LCC library from {nrow(discovered_lccs)} discovered configurations")
  cli::cli_alert_info("min_capacity: {min_cap}")

  all_parcels <- igraph::V(parcel_graph)$name

  # Filter out empty LCCs and those with zero capacity
  valid_lccs <- discovered_lccs[capacity > 0]

  # Handle empty library case
  if (nrow(valid_lccs) == 0) {
    cli::cli_alert_warning("No valid LCCs discovered - returning empty library")
    return(list(
      blocks = list(),
      metadata = data.table::data.table(
        block_id = integer(0), area = numeric(0), capacity = integer(0),
        n_parcels = integer(0), size_band = character(0), density = numeric(0),
        source = character(0), spectral_region = character(0),
        area_in_station = numeric(0), capacity_in_station = numeric(0),
        centroid_x = numeric(0), centroid_y = numeric(0)
      ),
      neighbor_indices = list(),
      n_blocks = 0L,
      parcel_names = all_parcels,
      active_mask = logical(0),
      block_hashes = NULL
    ))
  }

  has_source     <- "source" %in% names(valid_lccs)
  has_stratified <- has_source && any(grepl("stratified", valid_lccs$source))

  if (has_stratified) {
    # Capacity-stratified library: use fixed quotas to balance representation.
    # Proportional allocation would give most slots to high-capacity LCCs
    # because tree discovery is biased toward them. Fixed quotas ensure the
    # library covers the full capacity range the posterior explores.
    cli::cli_alert_info("Using capacity-stratified selection with fixed quotas")

    # Assign bands relative to actual min_capacity
    valid_lccs[, capacity_band := data.table::fcase(
      capacity < 0.75 * min_cap, "band_1",
      capacity < min_cap,        "band_2",
      capacity < 1.5 * min_cap,  "band_3",
      default =                  "band_4"
    )]

    # Fixed quotas balancing posterior mode (bands 1-2) with low-k feasibility
    # (bands 3-4). Bands 3-4 supply LCCs that can meet min_capacity with few
    # or no secondaries — critical for exploring low-k states.
    band_quotas <- list(
      band_1 = round(max_library_size * 0.15),
      band_2 = round(max_library_size * 0.20),
      band_3 = round(max_library_size * 0.30),
      band_4 = max_library_size - round(max_library_size * 0.65)
    )

    cli::cli_alert_info(
      "Band quotas: band_1={band_quotas$band_1}, band_2={band_quotas$band_2}, band_3={band_quotas$band_3}, band_4={band_quotas$band_4}"
    )

    selected_list <- list()
    for (band_name in names(band_quotas)) {
      quota      <- band_quotas[[band_name]]
      band_lccs  <- valid_lccs[capacity_band == band_name]
      n_available <- nrow(band_lccs)

      if (n_available == 0) {
        cli::cli_alert_warning("No LCCs in {band_name}, skipping")
        next
      }

      actual_quota <- min(quota, n_available)
      band_selected <- select_blocks_by_coverage(
        band_lccs, actual_quota, all_parcels, verbose = FALSE
      )
      selected_list[[band_name]] <- band_selected
      cli::cli_alert_info(
        "  {band_name} (<{round(c(0.75, 1.0, 1.5, Inf)[match(band_name, names(band_quotas))] * min_cap)} cap): {nrow(band_selected)}/{n_available} selected (quota: {quota})"
      )
    }

    selected <- data.table::rbindlist(selected_list, fill = TRUE)

    band_counts <- table(selected$capacity_band)
    cli::cli_alert_success("Final library capacity distribution:")
    for (b in names(band_counts)) {
      cli::cli_alert_info("  {b}: {band_counts[[b]]} LCCs")
    }

  } else if (has_source) {
    # Hybrid discovery (tree + BFS): reserve slots for BFS-only LCCs.
    # BFS discoveries find multi-crossing LCCs that tree enumeration misses.
    # Without reservation they would be crowded out by the larger tree set.
    tree_lccs    <- valid_lccs[source %in% c("tree", "both", "tree+bfs",
                                             "tree+stratified", "tree+both")]
    bfs_only_lccs <- valid_lccs[source == "bfs"]

    n_tree <- nrow(tree_lccs)
    n_bfs  <- nrow(bfs_only_lccs)

    safe_reservation <- min(bfs_reservation, max_library_size, n_bfs)
    tree_slots       <- max_library_size - safe_reservation

    tree_selected <- if (n_tree > 0) {
      select_blocks_by_coverage(tree_lccs, tree_slots, all_parcels, verbose = TRUE)
    } else {
      tree_lccs[0]
    }

    bfs_slots_available <- max(0L, max_library_size - nrow(tree_selected))
    bfs_selected <- if (n_bfs > 0 && bfs_slots_available > 0) {
      bfs_only_lccs[seq_len(min(n_bfs, bfs_slots_available))]
    } else {
      bfs_only_lccs[0]
    }

    selected <- data.table::rbindlist(list(tree_selected, bfs_selected), fill = TRUE)
    cli::cli_alert_info(
      "Library allocation: {nrow(tree_selected)} tree slots, {nrow(bfs_selected)} BFS slots"
    )

  } else {
    # Tree-only discovery: use coverage-aware selection
    selected <- select_blocks_by_coverage(
      valid_lccs, max_library_size, all_parcels, verbose = TRUE
    )
  }

  n_blocks <- nrow(selected)
  cli::cli_alert_info("Processing {n_blocks} LCCs (from {nrow(valid_lccs)} valid)")

  blocks        <- selected$parcel_ids
  block_sources <- if (has_source && "source" %in% names(selected))
    selected$source else rep("tree_discovered", n_blocks)
  spectral_regions <- rep(NA_character_, n_blocks)

  # Compute station metrics for station constraint pre-filtering in replace-LCC
  area_in_station <- vapply(blocks, function(pids)
    sum(igraph::V(parcel_graph)[pids]$area_in_station), numeric(1))
  capacity_in_station <- vapply(blocks, function(pids)
    sum(igraph::V(parcel_graph)[pids]$capacity_in_station), numeric(1))

  # Compute centroids for max-min seed selection in chain initialisation
  centroid_x <- vapply(blocks, function(pids)
    mean(igraph::V(parcel_graph)[pids]$centroid_x), numeric(1))
  centroid_y <- vapply(blocks, function(pids)
    mean(igraph::V(parcel_graph)[pids]$centroid_y), numeric(1))

  metadata <- data.table::data.table(
    block_id            = seq_len(n_blocks),
    area                = selected$area,
    capacity            = selected$capacity,
    n_parcels           = vapply(blocks, length, integer(1)),
    size_band           = "tree_discovered",
    density             = selected$capacity / selected$area,
    source              = block_sources,
    spectral_region     = spectral_regions,
    area_in_station     = area_in_station,
    capacity_in_station = capacity_in_station,
    centroid_x          = centroid_x,
    centroid_y          = centroid_y
  )

  # Store blocks as integer indices into parcel_names
  blocks <- lapply(blocks, parcel_ids_to_indices, parcel_names = all_parcels)

  cli::cli_alert_info("Building neighbor indices ({n_blocks} blocks)...")
  cli::cli_alert_info("  Precomputing neighbor cache ({length(all_parcels)} parcels)...")
  neighbor_cache <- setNames(
    lapply(all_parcels, function(m) igraph::neighbors(parcel_graph, m)$name),
    all_parcels
  )

  cli::cli_alert_info("  Building neighbor indices with cached neighbors...")
  neighbor_indices <- vector("list", n_blocks)
  for (i in seq_len(n_blocks)) {
    neighbors <- get_parcel_set_neighbors(
      all_parcels[blocks[[i]]], parcel_graph, neighbor_cache = neighbor_cache
    )
    neighbor_indices[[i]] <- parcel_ids_to_indices(neighbors, all_parcels)
    if (i %% 2000 == 0 || i == n_blocks) {
      cli::cli_alert_info("    Progress: {i}/{n_blocks} ({round(100*i/n_blocks)}%)")
    }
  }

  cli::cli_alert_success(
    "LCC library: {n_blocks} blocks (from {sum(selected$tree_count)} total tree hits)"
  )

  list(
    blocks           = blocks,
    metadata         = metadata,
    neighbor_indices = neighbor_indices,
    n_blocks         = n_blocks,
    parcel_names     = all_parcels,
    active_mask      = rep(TRUE, n_blocks),
    block_hashes     = NULL,  # Set to NULL for serialization; rebuilt in hydrate_library
    # # Online enrichment tracking (for O(1) operations)
    n_online         = 0L, # Count of online entries (avoids O(n) sum() scan)
    online_queue     = integer(0) # FIFO queue of online block IDs for fast eviction
  )
}


# ============================================================================
# SECONDARY LIBRARY FROM DISCOVERY
# ============================================================================

#' Build secondary library from tree + BFS discovery
#'
#' Converts secondary blocks discovered via spanning tree enumeration and BFS
#' into the standard library format for use in birth/death moves.
#'
#' @param combined_discovered Output from combine_discovered_blocks() for secondaries
#' @param parcel_graph igraph object
#' @param max_library_size Maximum number of blocks to include (default 500)
#' @param bfs_reservation Slots to reserve for BFS-only discoveries (default 100)
#' @return Block library structure matching build_secondary_library() output
#' @export
build_secondary_library_from_discovery <- function(
    combined_discovered,
    parcel_graph,
    max_library_size = 500L,
    bfs_reservation = 100L
) {
  # Handle both list wrapper and direct data.table input
  if (is.list(combined_discovered) && !data.table::is.data.table(combined_discovered)) {
    if ("discovered_blocks" %in% names(combined_discovered)) {
      discovered <- combined_discovered$discovered_blocks
    } else if ("discovered_secondaries" %in% names(combined_discovered)) {
      discovered <- combined_discovered$discovered_secondaries
    } else {
      discovered <- combined_discovered[[1]]
    }
  } else {
    discovered <- combined_discovered
  }

  cli::cli_alert_info("Building secondary library from {nrow(discovered)} discovered blocks")

  all_parcels <- igraph::V(parcel_graph)$name

  # Filter out empty blocks

  valid_blocks <- discovered[vapply(parcel_ids, length, integer(1)) > 0]

  if (nrow(valid_blocks) == 0) {
    cli::cli_alert_warning("No valid secondary blocks discovered - returning empty library")
    return(list(
      blocks = list(),
      metadata = data.table::data.table(
        block_id = integer(0), area = numeric(0), capacity = integer(0),
        n_parcels = integer(0), size_band = character(0), density = numeric(0),
        source = character(0)
      ),
      neighbor_indices = list(),
      n_blocks = 0L,
      parcel_names = all_parcels
    ))
  }

  # Check if we have source column (hybrid discovery)
  has_source <- "source" %in% names(valid_blocks)

  if (has_source) {
    # Hybrid discovery: reserve slots for BFS-only blocks
    tree_blocks <- valid_blocks[source %in% c("tree", "both")]
    bfs_only_blocks <- valid_blocks[source == "bfs"]

    n_tree <- nrow(tree_blocks)
    n_bfs <- nrow(bfs_only_blocks)

    # Calculate slot allocation
    safe_reservation <- min(bfs_reservation, max_library_size, n_bfs)
    tree_slots <- max_library_size - safe_reservation

    # Select tree blocks using coverage-aware selection per size band
    if (n_tree > 0) {
      tree_selected <- select_blocks_by_coverage_per_band(
        tree_blocks,
        max_total = tree_slots,
        parcel_names = all_parcels,
        band_column = "size_band",
        verbose = TRUE
      )
    } else {
      tree_selected <- tree_blocks[0]
    }

    tree_slots_used <- nrow(tree_selected)
    bfs_slots_available <- max_library_size - tree_slots_used

    # Select BFS blocks using coverage-aware selection per size band
    if (n_bfs > 0 && bfs_slots_available > 0) {
      bfs_selected <- select_blocks_by_coverage_per_band(
        bfs_only_blocks,
        max_total = bfs_slots_available,
        parcel_names = all_parcels,
        band_column = "size_band",
        verbose = TRUE
      )
    } else {
      bfs_selected <- bfs_only_blocks[0]
    }

    selected <- data.table::rbindlist(list(tree_selected, bfs_selected), fill = TRUE)

    cli::cli_alert_info(
      "Library allocation: {nrow(tree_selected)} tree slots, {nrow(bfs_selected)} BFS slots"
    )
  } else {
    # Tree-only or BFS-only: use coverage-aware selection per size band
    selected <- select_blocks_by_coverage_per_band(
      valid_blocks,
      max_total = max_library_size,
      parcel_names = all_parcels,
      band_column = "size_band",
      verbose = TRUE
    )
  }

  n_blocks <- nrow(selected)
  cli::cli_alert_info("Processing {n_blocks} secondary blocks")

  # Build blocks list
  blocks <- selected$parcel_ids

  # Determine source for each block
  if (has_source && "source" %in% names(selected)) {
    block_sources <- selected$source
  } else {
    block_sources <- rep("discovered", n_blocks)
  }

  # Determine size_band (use integer if present, else convert to string)
  if ("size_band" %in% names(selected)) {
    size_bands <- if (is.numeric(selected$size_band)) {
      paste0("band_", selected$size_band)
    } else {
      selected$size_band
    }
  } else {
    size_bands <- rep("discovered", n_blocks)
  }

  # Build metadata
  metadata <- data.table::data.table(
    block_id = seq_len(n_blocks),
    area = selected$area,
    capacity = selected$capacity,
    n_parcels = vapply(blocks, length, integer(1)),
    size_band = size_bands,
    density = selected$capacity / selected$area,
    source = block_sources
  )

  # Store blocks as integer indices
  blocks <- lapply(blocks, parcel_ids_to_indices, parcel_names = all_parcels)

  # Build neighbor indices
  cli::cli_alert_info("Building neighbor indices ({n_blocks} blocks)...")

  neighbor_cache <- setNames(
    lapply(all_parcels, function(m) igraph::neighbors(parcel_graph, m)$name),
    all_parcels
  )

  neighbor_indices <- vector("list", n_blocks)
  for (i in seq_len(n_blocks)) {
    neighbors <- get_parcel_set_neighbors(all_parcels[blocks[[i]]],
                                          parcel_graph,
                                          neighbor_cache = neighbor_cache)
    neighbor_indices[[i]] <- parcel_ids_to_indices(neighbors, all_parcels)
  }

  tree_count_sum <- if ("tree_count" %in% names(selected)) sum(selected$tree_count) else n_blocks

  cli::cli_alert_success(
    "Discovered secondary library: {n_blocks} blocks (from {tree_count_sum} tree hits)"
  )

  list(
    blocks = blocks,
    metadata = metadata,
    neighbor_indices = neighbor_indices,
    n_blocks = n_blocks,
    parcel_names = all_parcels
  )
}


# ============================================================================
# ONLINE LCC LIBRARY ENRICHMENT
# ============================================================================

#' Add LCC to library (online enrichment)
#'
#' Adds the current LCC to the library if not already present.
#' Used during main MCMC to enable Replace-LCC moves from visited states.
#'
#' @param lcc_library Current LCC library (will be modified)
#' @param lcc_parcels Character vector of parcel IDs in the LCC
#' @param parcel_graph igraph object for neighbor computation
#' @param max_online Maximum online entries before FIFO eviction
#' @param neighbor_cache Optional precomputed neighbor cache for speed
#' @return List with updated lcc_library and added (logical)
add_lcc_to_library <- function(lcc_library, lcc_parcels, parcel_graph,
                               max_online = ONLINE_MAX_ENTRIES,
                               neighbor_cache = NULL) {
  all_parcels <- lcc_library$parcel_names
  lcc_indices <- parcel_ids_to_indices(lcc_parcels, all_parcels)

  # Skip empty or invalid LCCs (prevents corrupt library entries)
  if (length(lcc_indices) == 0) {
    return(list(lcc_library = lcc_library, added = FALSE))
  }

  # Hydrate library if block_hashes is NULL (ensures all cached fields are initialized:
  # block_hashes, active_mask, n_online, online_queue, neighbor_indices)
  # This handles deserialized libraries that weren't hydrated by the caller
  if (is.null(lcc_library$block_hashes)) {
    lcc_library <- hydrate_library(lcc_library)
  }

  # Fast hash lookup for duplicates (use digest to avoid R's 10000 byte limit)
  key <- digest::digest(sort(lcc_indices), algo = "xxhash64")
  if (exists(key, envir = lcc_library$block_hashes, inherits = FALSE)) {
    return(list(lcc_library = lcc_library, added = FALSE))
  }

  # Use cached n_online count (O(1) instead of O(n) sum() scan)
  n_online <- lcc_library$n_online %||% 0L

  # FIFO eviction if at capacity - use cached online_queue
  if (n_online >= max_online) {
    online_queue <- lcc_library$online_queue %||% integer(0)
    if (length(online_queue) > 0) {
      # Pop oldest from queue
      oldest_block_id <- online_queue[1]
      lcc_library$online_queue <- online_queue[-1]

      # Find metadata row for this block_id
      oldest_row_idx <- which(lcc_library$metadata$block_id == oldest_block_id)
      if (length(oldest_row_idx) > 0) {
        oldest_row_idx <- oldest_row_idx[1]

        # Remove from hash before NULLing block
        old_block <- lcc_library$blocks[[oldest_block_id]]
        if (!is.null(old_block) && length(old_block) > 0 && !all(is.na(old_block))) {
          old_key <- digest::digest(sort(old_block), algo = "xxhash64")
          if (exists(old_key, envir = lcc_library$block_hashes, inherits = FALSE)) {
            rm(list = old_key, envir = lcc_library$block_hashes)
          }
        }

        # Mark as evicted (in-place update, no copy)
        data.table::set(lcc_library$metadata, oldest_row_idx, "source", "evicted")

        # Free memory while preserving index position
        lcc_library$blocks[[oldest_block_id]] <- NA_integer_
        lcc_library$neighbor_indices[[oldest_block_id]] <- NA_integer_
        if (!is.null(lcc_library$active_mask) &&
            length(lcc_library$active_mask) >= oldest_block_id) {
          lcc_library$active_mask[oldest_block_id] <- FALSE
        }

        # Decrement cached count
        lcc_library$n_online <- n_online - 1L
      }
    }
  }

  # Compute attributes
  capacity <- sum(igraph::V(parcel_graph)[lcc_parcels]$capacity)
  area <- sum(igraph::V(parcel_graph)[lcc_parcels]$area)
  area_in_station <- sum(igraph::V(parcel_graph)[lcc_parcels]$area_in_station)
  capacity_in_station <- sum(igraph::V(parcel_graph)[lcc_parcels]$capacity_in_station)

  # Guard against zero/invalid area (would cause NaN density)
  if (is.na(area) || area <= 0) {
    return(list(lcc_library = lcc_library, added = FALSE))
  }

  # Assign new block_id
  new_id <- lcc_library$n_blocks + 1L

  # Update all structures
  lcc_library$blocks[[new_id]] <- sort(lcc_indices)

  # Ensure legacy libraries have station columns and centroids before rbind
  if (!"area_in_station" %in% names(lcc_library$metadata)) {
    lcc_library$metadata[, area_in_station := NA_real_]
  }
  if (!"capacity_in_station" %in% names(lcc_library$metadata)) {
    lcc_library$metadata[, capacity_in_station := NA_real_]
  }
  if (!"centroid_x" %in% names(lcc_library$metadata)) {
    lcc_library$metadata[, centroid_x := NA_real_]
  }
  if (!"centroid_y" %in% names(lcc_library$metadata)) {
    lcc_library$metadata[, centroid_y := NA_real_]
  }

  lcc_library$metadata <- rbind(lcc_library$metadata, data.table::data.table(
    block_id            = new_id,
    area                = area,
    capacity            = as.integer(capacity),
    n_parcels           = length(lcc_indices),
    size_band           = "online",
    density             = capacity / area,
    source              = "online",
    spectral_region     = NA_character_,
    area_in_station     = area_in_station,
    capacity_in_station = capacity_in_station,
    centroid_x          = mean(igraph::V(parcel_graph)[lcc_parcels]$centroid_x, na.rm = TRUE),
    centroid_y          = mean(igraph::V(parcel_graph)[lcc_parcels]$centroid_y, na.rm = TRUE)
  ))
  neighbors <- get_parcel_set_neighbors(lcc_parcels, parcel_graph, neighbor_cache)
  lcc_library$neighbor_indices[[new_id]] <- parcel_ids_to_indices(neighbors, all_parcels)
  lcc_library$n_blocks <- new_id
  if (!is.null(lcc_library$active_mask)) {
    if (length(lcc_library$active_mask) < new_id) {
      lcc_library$active_mask <- c(lcc_library$active_mask, TRUE)
    } else {
      lcc_library$active_mask[new_id] <- TRUE
    }
  } else {
    lcc_library$active_mask <- compute_library_active_mask(lcc_library)
  }
  assign(key, new_id, envir = lcc_library$block_hashes)

  # Update cached online tracking (O(1) instead of O(n) on next call)
  lcc_library$n_online <- (lcc_library$n_online %||% 0L) + 1L
  lcc_library$online_queue <- c(lcc_library$online_queue %||% integer(0), new_id)

  list(lcc_library = lcc_library, added = TRUE)
}

# ============================================================================
# STATION-FEASIBILITY PARTITIONING FOR MCMC SEEDING
# ============================================================================
#
# Replaces spectral partitioning with constraint-aware partitioning that
# guarantees all chains can satisfy the station coverage requirement (90%
# capacity within station areas). Finds connected components of station-area
# parcels and selects maximally distant viable components for chain seeding.

#' Find connected components of station-area parcels that can support valid LCCs
#'
#' A component is viable only if it can satisfy BOTH station constraints:
#' - station_capacity_pct: % of min_capacity that must be in station area
#' - station_area_pct: % of min_area that must be in station area
#'
#' @param parcel_graph igraph with capacity_in_station and area_in_station vertex attributes
#' @param constraints Constraint list with min_capacity, min_area, station_capacity_pct, station_area_pct
#' @return data.table with comp_id, n_parcels, station_cap, station_area, max_lcc_cap, max_lcc_area,
#'   centroid_x/y, parcels (list), or NULL if no station constraint is active
find_viable_station_components <- function(parcel_graph, constraints) {
  # Check if ANY station constraint is active
  station_cap_pct <- constraints$station_capacity_pct
  station_area_pct <- constraints$station_area_pct

  has_cap_constraint <- !is.null(station_cap_pct) && !is.na(station_cap_pct) && station_cap_pct > 0.5
  has_area_constraint <- !is.null(station_area_pct) && !is.na(station_area_pct) && station_area_pct > 0.5

  if (!has_cap_constraint && !has_area_constraint) {
    return(NULL)  # No station constraint - caller should use fallback partitioning
  }

  parcels <- igraph::V(parcel_graph)$name
  station_cap <- igraph::V(parcel_graph)$capacity_in_station
  station_area <- igraph::V(parcel_graph)$area_in_station
  parcel_x <- igraph::V(parcel_graph)$centroid_x
  parcel_y <- igraph::V(parcel_graph)$centroid_y
  names(station_cap) <- parcels
  names(station_area) <- parcels
  names(parcel_x) <- parcels
  names(parcel_y) <- parcels

  # Handle NA values
  station_cap[is.na(station_cap)] <- 0
  station_area[is.na(station_area)] <- 0

  min_cap <- constraints$min_capacity * (station_cap_pct / 100)
  min_area <- constraints$min_area * (station_area_pct / 100)

  # Find station parcels (have either capacity or area in station)
  station_parcels <- parcels[station_cap > 0 | station_area > 0]
  if (length(station_parcels) == 0) {
    return(NULL)  # No station parcels - caller should use fallback partitioning
  }

  station_subgraph <- igraph::induced_subgraph(parcel_graph, station_parcels)
  comps <- igraph::components(station_subgraph)

  # Build table of viable components
  viable_list <- list()
  for (i in seq_len(comps$no)) {
    comp_parcels <- station_parcels[comps$membership == i]
    comp_station_cap <- sum(station_cap[comp_parcels], na.rm = TRUE)
    comp_station_area <- sum(station_area[comp_parcels], na.rm = TRUE)

    # Compute max viable LCC size based on each constraint
    # For capacity: station_cap / required_pct gives max total capacity
    # For area: station_area / required_pct gives max total area
    max_lcc_cap_from_capacity <- if (has_cap_constraint) {
      comp_station_cap / (station_cap_pct / 100)
    } else {
      Inf
    }

    max_lcc_area_from_area <- if (has_area_constraint) {
      comp_station_area / (station_area_pct / 100)
    } else {
      Inf
    }

    # Component is viable only if it can satisfy BOTH constraints
    # For capacity constraint: max_lcc_cap >= min_cap
    # For area constraint: max_lcc_area >= min_area
    cap_viable <- !has_cap_constraint || max_lcc_cap_from_capacity >= min_cap
    area_viable <- !has_area_constraint || max_lcc_area_from_area >= min_area

    if (cap_viable && area_viable) {
      viable_list[[length(viable_list) + 1]] <- data.table::data.table(
        comp_id = i,
        n_parcels = length(comp_parcels),
        station_cap = comp_station_cap,
        station_area = comp_station_area,
        max_lcc_cap = round(max_lcc_cap_from_capacity),
        max_lcc_area = round(max_lcc_area_from_area, 2),
        centroid_x = mean(parcel_x[comp_parcels], na.rm = TRUE),
        centroid_y = mean(parcel_y[comp_parcels], na.rm = TRUE),
        parcels = list(comp_parcels)
      )
    }
  }

  if (length(viable_list) == 0) {
    return(NULL)  # No viable components - caller should use fallback partitioning
  }

  data.table::rbindlist(viable_list)
}

#' Select k maximally distant items using greedy max-min algorithm
#'
#' Vectorized implementation using dist() — O(n^2) memory but avoids
#' the O(n^2 * k) nested loop of the naive implementation.
#'
#' @param coords Numeric matrix with columns x, y (one row per candidate)
#' @param k Number of items to select
#' @return Integer vector of selected row indices
select_maxmin_distant_seeds <- function(coords, k) {
  n <- nrow(coords)
  if (k >= n) return(seq_len(n))

  # Compute full pairwise distance matrix once (vectorized)
  dist_matrix <- as.matrix(dist(coords))

  # Start with the most peripheral point (maximizes sum of distances to all others)
  selected <- which.max(rowSums(dist_matrix))

  # min_dist[i] = distance from i to its nearest selected point
  min_dist <- dist_matrix[, selected]

  for (iter in seq_len(k - 1)) {
    # Pick the unselected point with the largest min-distance to selected set
    min_dist[selected] <- -Inf
    next_idx <- which.max(min_dist)
    selected <- c(selected, next_idx)

    # Incrementally update min_dist — only need new column
    min_dist <- pmin(min_dist, dist_matrix[, next_idx])
  }

  selected
}


#' Select seed LCCs from library for MCMC chain initialisation
#'
#' Picks n_chains maximally distant LCCs from the library using centroid
#' coordinates stored in library metadata. All LCCs in the library already
#' satisfy station constraints by construction, so no further filtering
#' is needed.
#'
#' @param lcc_library LCC library from build_lcc_library_from_tree_discovery()
#' @param n_chains Number of chains (seeds) to select (default 4)
#' @return Integer vector of block_ids (length <= n_chains)
#' @export
select_seed_lccs <- function(lcc_library, n_chains = 4L) {
  meta <- lcc_library$metadata

  # Filter to active, non-evicted blocks with valid centroids
  active <- lcc_library$active_mask %||% rep(TRUE, nrow(meta))
  valid_centroid <- !is.na(meta$centroid_x) & !is.na(meta$centroid_y)
  candidates <- meta[active & valid_centroid]

  n_available <- nrow(candidates)

  if (n_available == 0) {
    cli::cli_abort("LCC library has no blocks with valid centroids for seeding")
  }

  if (n_available <= n_chains) {
    cli::cli_alert_warning(
      "Only {n_available} LCCs available, requested {n_chains} chains — using all"
    )
    return(candidates$block_id)
  }

  cli::cli_alert_info(
    "Selecting {n_chains} maximally distant seed LCCs from {n_available} candidates"
  )

  coords <- cbind(candidates$centroid_x, candidates$centroid_y)
  selected_rows <- select_maxmin_distant_seeds(coords, k = n_chains)

  selected_ids <- candidates$block_id[selected_rows]

  # Log selected LCC summaries
  for (i in seq_along(selected_ids)) {
    bid <- selected_ids[i]
    row <- candidates[block_id == bid]
    cli::cli_alert_info(
      "  Seed {i}: block_id={bid}, capacity={row$capacity}, area={round(row$area, 1)}, source={row$source}"
    )
  }

  selected_ids
}


#' Generate initial MCMC states from library LCCs
#'
#' Replaces generate_initial_parcel_state_in_region(). Selects n_chains
#' maximally distant LCCs from the library and uses each directly as the
#' initial LCC state, then adds compatible secondary blocks.
#'
#' All constraint satisfaction (capacity, area, density, station proximity)
#' is guaranteed by library construction — no BFS expansion needed.
#'
#' @param lcc_library LCC library from build_lcc_library_from_tree_discovery()
#' @param libraries Full libraries list (needs secondary_library)
#' @param parcel_graph igraph object
#' @param constraints MBTA constraints list
#' @param n_chains Number of chains to initialise (default 4)
#' @return List of length n_chains, each an initialised parcel MCMC state
#' @export
generate_initial_states_from_lccs <- function(
    lcc_library,
    libraries,
    parcel_graph,
    constraints,
    n_chains = 4L
) {
  cli::cli_h2("Generating Initial MCMC States from LCC Library")

  # Hydrate before use
  lcc_library                 <- hydrate_library(lcc_library)
  libraries$lcc_library       <- lcc_library
  libraries$secondary_library <- hydrate_library(libraries$secondary_library)

  # Full station thresholds for pre-filtering candidates
  required_station_cap  <- if (!is.null(constraints$station_capacity_pct) &&
                               !is.na(constraints$station_capacity_pct))
    (constraints$station_capacity_pct / 100) * constraints$min_capacity else 0
  required_station_area <- if (!is.null(constraints$station_area_pct) &&
                               !is.na(constraints$station_area_pct))
    (constraints$station_area_pct / 100) * constraints$min_area else 0

  check_station_cap  <- required_station_cap  > 0
  check_station_area <- required_station_area > 0

  # Filter library to station-valid candidates before max-min selection
  meta           <- lcc_library$metadata
  active         <- lcc_library$active_mask %||% rep(TRUE, nrow(meta))
  valid_centroid <- !is.na(meta$centroid_x) & !is.na(meta$centroid_y)
  station_valid  <- (!check_station_cap  | meta$capacity_in_station >= required_station_cap) &
    (!check_station_area | meta$area_in_station     >= required_station_area)

  candidates  <- meta[active & valid_centroid & station_valid]
  n_available <- nrow(candidates)
  n_total     <- sum(active & valid_centroid)

  cli::cli_alert_info(
    "{n_available} of {n_total} library LCCs satisfy full station thresholds for seeding"
  )

  if (n_available == 0) {
    cli::cli_abort("No LCCs in library satisfy full station constraints for seeding")
  }

  # Max-min selection on filtered candidates
  coords       <- cbind(candidates$centroid_x, candidates$centroid_y)
  selected_rows <- select_maxmin_distant_seeds(coords, k = min(n_chains, n_available))
  # Remaining candidates as ordered fallbacks (for post-secondary validation)
  fallback_rows <- setdiff(seq_len(n_available), selected_rows)
  ordered_ids   <- candidates$block_id[c(selected_rows, fallback_rows)]

  all_parcels    <- lcc_library$parcel_names
  initial_states <- vector("list", n_chains)
  used_block_ids <- integer(0)

  for (i in seq_len(n_chains)) {
    success <- FALSE

    for (bid in ordered_ids) {
      if (bid %in% used_block_ids) next

      lcc_indices <- lcc_library$blocks[[bid]]
      lcc_parcels <- all_parcels[lcc_indices]

      secondary_block_ids <- select_initial_secondary_blocks(
        lcc_parcels  = lcc_parcels,
        library      = libraries$secondary_library,
        parcel_graph = parcel_graph,
        constraints  = constraints
      )

      sec_parcels <- library_blocks_parcels(
        libraries$secondary_library,
        secondary_block_ids
      )

      state <- initialize_parcel_state(
        parcel_ids          = c(lcc_parcels, sec_parcels),
        secondary_block_ids = secondary_block_ids,
        library             = libraries$secondary_library,
        parcel_graph        = parcel_graph
      )

      # Use the runner's own feasibility check — single source of truth
      feas <- check_parcel_feasibility(
        state        = state,
        library      = libraries$secondary_library,
        parcel_graph = parcel_graph,
        constraints  = constraints
      )

      if (!feas$feasible) {
        if (!(bid %in% candidates$block_id[selected_rows[-seq_len(i-1)]])) {
          # Only warn for fallbacks to avoid noise from expected misses
        } else {
          cli::cli_alert_warning(
            "Chain {i}: preferred seed block_id={bid} fails '{feas$constraint_failed}' after secondaries, trying fallbacks..."
          )
        }
        next
      }

      initial_states[[i]] <- state
      used_block_ids       <- c(used_block_ids, bid)

      lcc_cap <- sum(igraph::V(parcel_graph)[lcc_parcels]$capacity)
      cli::cli_alert_success(
        "  Chain {i}: block_id={bid}, {length(lcc_parcels)} LCC parcels (cap={lcc_cap}), {length(secondary_block_ids)} secondary blocks"
      )

      success <- TRUE
      break
    }

    if (!success) {
      cli::cli_abort(
        "Chain {i}: no valid initial state found after trying all {length(ordered_ids)} station-valid LCCs"
      )
    }
  }

  cli::cli_alert_success("Initialised {n_chains} chains")
  initial_states
}


#' Generate k=0 (no-secondary) initial states from standalone-feasible LCCs
#'
#' Produces initial MCMC states containing only an LCC component (zero secondary
#' blocks). These states let some chains explore the low-k region of the
#' posterior, which is hard to reach from k>0 due to birth/death pool asymmetry.
#'
#' LCCs that satisfy all hard constraints on their own (capacity, area, density,
#' station proximity, connectivity) are identified via \code{find_feasible_lcc_ids()}.
#' The most geographically distant candidates are selected as seeds.
#'
#' @param lcc_library LCC library (will be hydrated internally)
#' @param secondary_library Secondary block library
#' @param parcel_graph igraph object with parcel attributes
#' @param constraints MBTA constraints list
#' @param n_k0_chains Number of k=0 chains to produce (default 2)
#' @return List of initialised parcel MCMC states (length 0 to \code{n_k0_chains}).
#'   Returns empty list with a warning if no standalone-feasible LCCs exist.
#' @export
generate_k0_initial_states <- function(
    lcc_library,
    secondary_library,
    parcel_graph,
    constraints,
    n_k0_chains = 2L
) {
  cli::cli_h2("Generating k=0 (No-Secondary) Initial States")


  # Hydrate libraries
  lcc_library       <- hydrate_library(lcc_library)
  secondary_library <- hydrate_library(secondary_library)

  # Find LCCs that meet ALL hard constraints on their own (no secondaries)
  feasible_ids <- find_feasible_lcc_ids(
    lcc_library,
    parcel_graph,
    secondary_library,
    constraints
  )

  if (length(feasible_ids) == 0) {
    cli::cli_alert_warning(
      "No standalone-feasible LCCs found — skipping k=0 chain initialisation"
    )
    return(list())
  }

  cli::cli_alert_info(
    "{length(feasible_ids)} standalone-feasible LCCs found for k=0 seeding"
  )

  # Filter metadata to feasible LCCs with valid centroids

  meta <- lcc_library$metadata
  feasible_meta <- meta[meta$block_id %in% feasible_ids]
  valid_centroid <- !is.na(feasible_meta$centroid_x) &
    !is.na(feasible_meta$centroid_y)
  feasible_meta <- feasible_meta[valid_centroid]

  if (nrow(feasible_meta) == 0) {
    cli::cli_alert_warning(
      "No standalone-feasible LCCs have valid centroids — skipping k=0 chains"
    )
    return(list())
  }

  n_select <- min(n_k0_chains, nrow(feasible_meta))

  # Select maximally distant seeds for geographic diversity
  coords <- cbind(feasible_meta$centroid_x, feasible_meta$centroid_y)
  selected_rows <- select_maxmin_distant_seeds(coords, k = n_select)
  selected_ids  <- feasible_meta$block_id[selected_rows]

  all_parcels    <- lcc_library$parcel_names
  initial_states <- vector("list", n_select)

  for (i in seq_len(n_select)) {
    bid         <- selected_ids[i]
    lcc_indices <- lcc_library$blocks[[bid]]
    lcc_parcels <- all_parcels[lcc_indices]

    state <- create_lcc_only_state(lcc_parcels, secondary_library, parcel_graph)

    lcc_cap <- sum(igraph::V(parcel_graph)[lcc_parcels]$capacity)
    cli::cli_alert_success(
      "  k=0 chain {i}: block_id={bid}, {length(lcc_parcels)} LCC parcels (cap={lcc_cap}), 0 secondary blocks"
    )

    initial_states[[i]] <- state
  }

  cli::cli_alert_success("Initialised {n_select} k=0 chains")
  initial_states
}


#' Partition parcels into quadrants for geographic diversity (fallback)
#'
#' Simple geographic partitioning when station constraints are not active.
#' Divides parcels into quadrants based on median x/y coordinates.
#'
#' @param parcel_graph igraph object with centroid_x, centroid_y vertex attributes
#' @param n_regions Number of regions to create (uses min(n_regions, 4))
#' @return List with same structure as partition_parcels_by_station_feasibility()
#' @keywords internal
partition_parcels_by_quadrant <- function(parcel_graph, n_regions = 4) {
  all_parcels <- igraph::V(parcel_graph)$name
  parcel_x <- igraph::V(parcel_graph)$centroid_x
  parcel_y <- igraph::V(parcel_graph)$centroid_y
  names(parcel_x) <- all_parcels
  names(parcel_y) <- all_parcels

  # Handle NA centroids
  valid_x <- !is.na(parcel_x)
  valid_y <- !is.na(parcel_y)
  valid_parcels <- all_parcels[valid_x & valid_y]

  if (length(valid_parcels) == 0) {
    stop("No parcels with valid centroids for geographic partitioning")
  }

  # Compute medians for quadrant division
  med_x <- median(parcel_x[valid_parcels], na.rm = TRUE)
  med_y <- median(parcel_y[valid_parcels], na.rm = TRUE)

  # Assign to quadrants
  region_assignments <- rep(NA_character_, length(all_parcels))
  names(region_assignments) <- all_parcels

  for (p in valid_parcels) {
    if (parcel_x[p] <= med_x && parcel_y[p] <= med_y) {
      region_assignments[p] <- "R1"  # SW
    } else if (parcel_x[p] > med_x && parcel_y[p] <= med_y) {
      region_assignments[p] <- "R2"  # SE
    } else if (parcel_x[p] <= med_x && parcel_y[p] > med_y) {
      region_assignments[p] <- "R3"  # NW
    } else {
      region_assignments[p] <- "R4"  # NE
    }
  }

  # Assign parcels with NA centroids to nearest region by valid neighbor
  invalid_parcels <- setdiff(all_parcels, valid_parcels)
  for (p in invalid_parcels) {
    neighbors <- igraph::neighbors(parcel_graph, p)$name
    valid_neighbors <- neighbors[neighbors %in% valid_parcels]
    if (length(valid_neighbors) > 0) {
      region_assignments[p] <- region_assignments[valid_neighbors[1]]
    } else {
      region_assignments[p] <- "R1"  # Fallback
    }
  }

  # Limit to requested number of regions
  actual_n <- min(n_regions, 4)
  if (actual_n < 4) {
    # Merge regions if fewer requested
    region_assignments[region_assignments %in% c("R3", "R4")] <- paste0("R", ((as.integer(gsub("R", "", region_assignments[region_assignments %in% c("R3", "R4")])) - 1) %% actual_n) + 1)
  }

  # Build seed pools (all parcels in each region are valid seeds)
  seed_pools <- list()
  for (i in seq_len(actual_n)) {
    region_id <- paste0("R", i)
    seed_pools[[region_id]] <- names(region_assignments)[region_assignments == region_id]
  }

  # Log summary
  region_table <- table(region_assignments)
  region_summary <- paste(
    sprintf("%s:%d", names(region_table), as.integer(region_table)),
    collapse = ", "
  )
  cli::cli_alert_success("Quadrant partitioning (no station constraint): {actual_n} regions ({region_summary})")

  list(
    region_assignments = region_assignments,
    seed_pools = seed_pools,
    viable_components = NULL,  # Not applicable for quadrant partitioning
    selected_indices = NULL,
    n_regions = actual_n
  )
}

#' Partition parcels into regions based on station constraint feasibility
#'
#' When station constraints are active, partitions based on viable station
#' components to guarantee all chains can satisfy station coverage requirements.
#' When station constraints are not active (NULL, NA, or 0), falls back to
#' simple geographic (quadrant) partitioning for chain diversity.
#'
#' @param parcel_graph igraph object with station coverage attributes
#' @param constraints Constraint list with min_capacity, station_capacity_pct
#' @param n_regions Number of regions (chains) to create
#' @return List with:
#'   - region_assignments: named vector (parcel_id -> "R1", "R2", etc.)
#'   - seed_pools: list of parcel ID vectors for each region's seed parcels
#'   - viable_components: data.table of viable components (NULL if quadrant fallback)
#'   - selected_indices: which components were selected (NULL if quadrant fallback)
#'   - n_regions: actual number of regions created
#' @export
partition_parcels_by_station_feasibility <- function(parcel_graph, constraints, n_regions = 4) {
  # Find viable station components (returns NULL if station constraint not active)
  viable <- find_viable_station_components(parcel_graph, constraints)

  # Fall back to quadrant partitioning if no station constraint or no viable components
  if (is.null(viable)) {
    cli::cli_alert_info("No station constraint active - using quadrant partitioning")
    return(partition_parcels_by_quadrant(parcel_graph, n_regions))
  }

  if (nrow(viable) < n_regions) {
    cli::cli_alert_warning(
      "Only {nrow(viable)} viable components found, requested {n_regions} regions. Falling back to quadrant partitioning"
    )
    return(partition_parcels_by_quadrant(parcel_graph, n_regions))
  }

  cli::cli_alert_info("Found {nrow(viable)} viable station components")

  # Select maximally distant seeds
  coords <- cbind(viable$centroid_x, viable$centroid_y)
  selected_idx <- select_maxmin_distant_seeds(coords, k = n_regions)
  selected <- viable[selected_idx]
  cli::cli_alert_info("Selected {length(selected_idx)} maximally distant components")

  # Create region assignments and seed pools
  all_parcels <- igraph::V(parcel_graph)$name
  region_assignments <- rep(NA_character_, length(all_parcels))
  names(region_assignments) <- all_parcels

  seed_pools <- list()
  for (i in seq_along(selected_idx)) {
    region_id <- paste0("R", i)
    comp_parcels <- selected$parcels[[i]]
    region_assignments[comp_parcels] <- region_id
    seed_pools[[region_id]] <- comp_parcels
  }

  # Assign remaining parcels to nearest region (for coverage tracking only)
  parcel_x <- igraph::V(parcel_graph)$centroid_x
  parcel_y <- igraph::V(parcel_graph)$centroid_y
  names(parcel_x) <- all_parcels
  names(parcel_y) <- all_parcels

  unassigned <- all_parcels[is.na(region_assignments)]
  for (p in unassigned) {
    dists <- sqrt((parcel_x[p] - selected$centroid_x)^2 +
                    (parcel_y[p] - selected$centroid_y)^2)
    nearest_idx <- which.min(dists)
    if (length(nearest_idx) == 0 || is.na(nearest_idx)) {
      nearest_idx <- 1  # Fallback if distance calculation fails
    }
    region_assignments[p] <- paste0("R", nearest_idx)
  }

  # Log summary
  region_table <- table(region_assignments)
  region_summary <- paste(
    sprintf("%s:%d", names(region_table), as.integer(region_table)),
    collapse = ", "
  )
  cli::cli_alert_success("Station-feasibility partitioning: {length(selected_idx)} regions ({region_summary})")

  list(
    region_assignments = region_assignments,
    seed_pools = seed_pools,
    viable_components = viable,
    selected_indices = selected_idx,
    n_regions = length(selected_idx)
  )
}

#' Generate initial parcel state starting in a specific region
#'
#' Builds initial LCC by BFS from a seed parcel in the target region,
#' growing until constraints are satisfied.
#'
#' @param parcel_graph igraph object
#' @param constraints Constraint list
#' @param libraries Block libraries (secondary_library, lcc_library)
#' @param region_assignments Named vector from partition_parcels_by_station_feasibility()
#' @param target_region Region to start from ("R1", "R2", etc.)
#' @param seed_pool Optional character vector of parcel IDs to seed BFS from.
#'   If provided, overrides region_assignments for seed selection. This allows
#'   station-feasibility partitioning to guarantee seeds from viable station components.
#' @param max_restarts Maximum BFS attempts
#' @param seed_with_secondaries If TRUE, add secondary blocks to enable smaller LCC exploration
#' @return Parcel MCMC state
generate_initial_parcel_state_in_region <- function(parcel_graph,
                                                    constraints,
                                                    libraries,
                                                    region_assignments,
                                                    target_region,
                                                    seed_pool = NULL,
                                                    max_restarts = 100,
                                                    seed_with_secondaries = FALSE) {
  # Use seed_pool if provided, otherwise fall back to region parcels
  region_parcels <- if (!is.null(seed_pool)) {
    seed_pool
  } else {
    names(region_assignments)[region_assignments == target_region]
  }
  if (length(region_parcels) == 0) {
    stop("No parcels in region: ", target_region)
  }

  capacity_lookup <- igraph::V(parcel_graph)$capacity
  names(capacity_lookup) <- igraph::V(parcel_graph)$name
  area_lookup <- igraph::V(parcel_graph)$area
  names(area_lookup) <- igraph::V(parcel_graph)$name
  capacity_in_station_lookup <- igraph::V(parcel_graph)$capacity_in_station
  names(capacity_in_station_lookup) <- igraph::V(parcel_graph)$name
  area_in_station_lookup <- igraph::V(parcel_graph)$area_in_station
  names(area_in_station_lookup) <- igraph::V(parcel_graph)$name

  min_cap <- constraints$min_capacity
  min_area <- constraints$min_area
  # Use discovery bound to prevent high-cap initial states that cause mode-trapping
  max_cap <- min_cap * DISCOVERY_CAPACITY_MULTIPLIER

  # Station constraint thresholds (if specified)
  required_station_cap <- if (!is.null(constraints$station_capacity_pct) &&
                              !is.na(constraints$station_capacity_pct)) {
    (constraints$station_capacity_pct / 100) * min_cap
  } else NULL
  required_station_area <- if (!is.null(constraints$station_area_pct) &&
                               !is.na(constraints$station_area_pct)) {
    (constraints$station_area_pct / 100) * min_area
  } else NULL

  for (attempt in seq_len(max_restarts)) {
    # BFS expansion from region seed with capacity bound
    # Sample target within valid range to get varied initial states
    target_cap <- runif(1, min_cap, max_cap)
    result <- bfs_grow_block(
      graph = parcel_graph,
      metric_lookup = capacity_lookup,
      seed_pool = region_parcels,
      target_min = min_cap,
      target_exact = target_cap
    )

    current_lcc <- result$block
    current_cap <- result$metric_total
    current_area <- sum(area_lookup[current_lcc], na.rm = TRUE)

    # Check basic feasibility with both lower and upper bounds
    if (!(current_cap >= min_cap && current_cap <= max_cap && current_area >= min_area)) {
      next
    }

    # Check station constraints if specified
    if (!is.null(required_station_cap)) {
      current_station_cap <- sum(capacity_in_station_lookup[current_lcc], na.rm = TRUE)
      if (current_station_cap < required_station_cap) next
    }
    if (!is.null(required_station_area)) {
      current_station_area <- sum(area_in_station_lookup[current_lcc], na.rm = TRUE)
      if (current_station_area < required_station_area) next
    }

    # All constraints satisfied - optionally add secondary blocks
    if (seed_with_secondaries) {
      secondary_block_ids <- select_initial_secondary_blocks(
        lcc_parcels = current_lcc,
        library = libraries$secondary_library,
        parcel_graph = parcel_graph,
        constraints = constraints
      )
      sec_parcels <- library_blocks_parcels(libraries$secondary_library, secondary_block_ids)
    } else {
      secondary_block_ids <- integer(0)
      sec_parcels <- character(0)
    }

    # Create state with LCC and any selected secondaries
    return(initialize_parcel_state(
      parcel_ids = c(current_lcc, sec_parcels),
      secondary_block_ids = secondary_block_ids,
      library = libraries$secondary_library,
      parcel_graph = parcel_graph
    ))
  }

  stop("Failed to generate feasible initial state in region ", target_region,
       " after ", max_restarts, " attempts")
}

#' Select initial secondary blocks compatible with an LCC
#'
#' Greedily selects secondary blocks that are disjoint and non-adjacent to
#' both the LCC and each other, targeting approximately 50% LCC fraction.
#'
#' @param lcc_parcels Character vector of parcel IDs in the LCC
#' @param library Secondary library
#' @param parcel_graph igraph object
#' @param constraints Constraints list with min_lcc_fraction
#' @return Integer vector of selected block IDs
select_initial_secondary_blocks <- function(lcc_parcels, library, parcel_graph,
                                            constraints) {
  if (library$n_blocks == 0) return(integer(0))

  # Compute LCC capacity
  lcc_capacity <- sum(igraph::V(parcel_graph)[lcc_parcels]$capacity)

  # Max secondary capacity to achieve ~min_lcc_fraction
  # LCC / (LCC + SEC) >= min_lcc_fraction  =>  SEC <= LCC * (1 - f) / f
  # Note: No max_capacity constraint - capacity prior handles preference
  min_frac <- constraints$min_lcc_fraction
  max_sec_capacity <- lcc_capacity * (1 - min_frac) / min_frac

  if (max_sec_capacity <= 0) return(integer(0))

  parcel_names <- library$parcel_names
  n_parcels <- length(parcel_names)

  # Build LCC logical and neighbor logical for compatibility checking
  lcc_indices <- match(lcc_parcels, parcel_names)
  # Guard against NA if lcc_parcels aren't all in library$parcel_names
  lcc_indices <- lcc_indices[!is.na(lcc_indices)]
  if (length(lcc_indices) == 0) {
    cli::cli_warn("No LCC parcels found in library - skipping secondary seeding")
    return(integer(0))
  }
  lcc_logical <- logical(n_parcels)
  lcc_logical[lcc_indices] <- TRUE

  # Get LCC neighbors
  lcc_neighbor_names <- unique(unlist(
    lapply(lcc_parcels, function(m) igraph::neighbors(parcel_graph, m)$name),
    use.names = FALSE
  ))
  lcc_neighbor_names <- setdiff(lcc_neighbor_names, lcc_parcels)
  lcc_nbr_indices <- match(lcc_neighbor_names, parcel_names)
  lcc_nbr_indices <- lcc_nbr_indices[!is.na(lcc_nbr_indices)]
  lcc_nbr_logical <- logical(n_parcels)
  lcc_nbr_logical[lcc_nbr_indices] <- TRUE

  # Find LCC-compatible blocks
  blocks <- library$blocks
  compatible_ids <- integer(0)
  for (i in seq_len(library$n_blocks)) {
    block_indices <- blocks[[i]]
    # Block must be disjoint from LCC
    if (any(lcc_logical[block_indices])) next
    # Block must be non-adjacent to LCC
    if (any(lcc_nbr_logical[block_indices])) next
    compatible_ids <- c(compatible_ids, i)
  }

  if (length(compatible_ids) == 0) return(integer(0))

  # Sort by capacity (largest first for greedy fill)
  block_caps <- library$metadata$capacity[compatible_ids]
  order_idx <- order(block_caps, decreasing = TRUE)
  compatible_ids <- compatible_ids[order_idx]
  block_caps <- block_caps[order_idx]

  # Greedy selection maintaining mutual disjointness and non-adjacency
  selected <- integer(0)
  selected_union <- logical(n_parcels)  # Union of selected block parcels
  selected_nbrs <- logical(n_parcels)   # Neighbors of selected blocks
  current_sec_capacity <- 0


  for (i in seq_along(compatible_ids)) {
    bid <- compatible_ids[i]
    bcap <- block_caps[i]

    # Check capacity limit
    if (current_sec_capacity + bcap > max_sec_capacity) next

    block_indices <- blocks[[bid]]

    # Check disjoint from already-selected blocks
    if (any(selected_union[block_indices])) next

    # Check non-adjacent to already-selected blocks
    if (any(selected_nbrs[block_indices])) next

    # Accept this block
    selected <- c(selected, bid)
    selected_union[block_indices] <- TRUE
    current_sec_capacity <- current_sec_capacity + bcap

    # Update neighbor set for next iteration
    block_parcels <- parcel_names[block_indices]
    block_nbr_names <- unique(unlist(
      lapply(block_parcels, function(m) igraph::neighbors(parcel_graph, m)$name),
      use.names = FALSE
    ))
    block_nbr_names <- setdiff(block_nbr_names, block_parcels)
    block_nbr_idx <- match(block_nbr_names, parcel_names)
    block_nbr_idx <- block_nbr_idx[!is.na(block_nbr_idx)]
    selected_nbrs[block_nbr_idx] <- TRUE
  }

  selected
}

# ============================================================================
# LIBRARY HYDRATION
# ============================================================================

#' Hydrate library
#'
#' Ensures libraries are in the compact representation:
#' - blocks stored as integer indices into parcel_names
#' - neighbor_indices stored as integer indices
#' - block_hashes rebuilt when missing
#'
#' @param library Block library object
#' @return Hydrated library with hash index
#' @export
hydrate_library <- function(library) {
  if (is.null(library)) return(NULL)
  if (library$n_blocks == 0) return(library)

  # Convert blocks to integer indices if needed
  first_block <- NULL
  for (x in library$blocks) {
    if (!is.null(x) && length(x) > 0 && !all(is.na(x))) {
      first_block <- x
      break
    }
  }
  if (!is.null(first_block) && is.character(first_block)) {
    library$blocks <- lapply(library$blocks, parcel_ids_to_indices,
                             parcel_names = library$parcel_names)
  }

  # Rebuild block_hashes environment from blocks (for O(1) lookup)
  # Use digest for keys to avoid R's 10000 byte variable name limit
  if (is.null(library$block_hashes) && length(library$blocks) > 0) {
    library$block_hashes <- new.env(hash = TRUE, parent = emptyenv())
    has_source <- !is.null(library$metadata$source)
    for (i in seq_along(library$blocks)) {
      block_i <- library$blocks[[i]]
      if (is.null(block_i) || length(block_i) == 0 || all(is.na(block_i))) next
      if (has_source && library$metadata[i, source] == "evicted") next
      key <- digest::digest(sort(block_i), algo = "xxhash64")
      assign(key, i, envir = library$block_hashes)
    }
  }

  # Ensure active mask is present and sized correctly
  if (is.null(library$active_mask) || length(library$active_mask) != library$n_blocks) {
    library$active_mask <- compute_library_active_mask(library)
  }

  # Ensure online enrichment tracking fields are present
  # These enable O(1) operations instead of O(n) metadata scans
  if (is.null(library$n_online)) {
    # Count existing online entries (one-time cost)
    if (!is.null(library$metadata$source)) {
      library$n_online <- sum(library$metadata$source == "online", na.rm = TRUE)
    } else {
      library$n_online <- 0L
    }
  }
  if (is.null(library$online_queue)) {
    # Rebuild FIFO queue from metadata (one-time cost)
    if (!is.null(library$metadata$source) && library$n_online > 0) {
      online_rows <- which(library$metadata$source == "online")
      library$online_queue <- library$metadata$block_id[online_rows]
    } else {
      library$online_queue <- integer(0)
    }
  }

  library
}

#' Hydrate all block libraries
#'
#' Wrapper to hydrate both secondary and LCC libraries.
#'
#' @param libraries List containing secondary_library and lcc_library
#' @return Hydrated libraries
#' @export
hydrate_libraries <- function(libraries) {
  libraries$secondary_library <- hydrate_library(libraries$secondary_library)
  libraries$lcc_library <- hydrate_library(libraries$lcc_library)
  libraries
}
