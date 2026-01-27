# _targets.R - Parcel MCMC pipeline for zoning analysis
#
# This file defines the targets pipeline for parcel MCMC analysis
# on the Norwood zoning data.
#
# Usage:
#   library(targets)
#   targets::tar_make(script = "inst/targets/_targets.R") # Run the pipeline
#   tar_visnetwork() # Visualize dependencies
#   tar_read(parcel_metrics)  # Read a result
#
# The pipeline has the following structure:
#
#   Tier 1: Data Preparation
#     norwood_data -> adjacency_graph -> constraints
#
#   Tier 2: Parcel Graph Construction
#     parcel_graph_result
#
#   Tier 3A: LCC Discovery (Parallel Tree + BFS)
#     tree_discovered_lccs -> bfs_discovered_lccs -> combined_discovered_lccs
#       -> discovered_lcc_library
#
#   Tier 3B: Secondary Discovery (Parallel Tree + BFS)
#     tree_discovered_secondaries -> bfs_discovered_secondaries
#       -> combined_discovered_secondaries -> discovered_secondary_library
#
#   Tier 4: Main Parcel MCMC (4 chains)
#     parcel_chain_results -> all_parcel_chain_results -> parcel_metrics
#     (Uses coda for proper multi-chain ESS computation)
#
#   Tier 5: Quarto Report

library(targets)
library(tarchetypes)
library(crew)



# Delete old worker logs
if (dir.exists("worker_logs")) {
  unlink("worker_logs", recursive = TRUE)
}

# As we are in a package we no longer source the functions with tar_source() here. Instead, we
# load the package and then source only the config files.
library(mbtazone)

source("inst/targets/temp_targets_config.R", local = TRUE)
source("inst/targets/temp_targets_parcel_config.R", local = TRUE)

# Global options with crew controller for parallel execution
tar_option_set(
  packages = c(
    "mbtazone",
    "data.table",
    "sf",
    "igraph",
    "ggplot2",
    "glue",
    "Matrix",
    "purrr",
    "devtools",
    "cli",
    "coda" # For multi-chain ESS computation
  ),
  format = "qs", # Fast serialization format
  memory = "transient", # Unload after use
  garbage_collection = TRUE, # Run gc() between targets
  controller = crew_controller_local(
    workers = DEFAULT_CREW_WORKERS, # Auto-detect cores, leave reserve for system
    seconds_idle = 60, # Shut down idle workers after 60s
    options_local = crew_options_local(
      log_directory = "worker_logs" # Capture worker stdout/stderr for monitoring
    )
  )
)

# Pipeline definition
list(
  # ============================================================================
  # TIER 1: DATA PREPARATION
  # ============================================================================

  tar_target(
    norwood_data,
    load_norwood_data(
      data_root = "~/code/zoning_mcmc/data",
      row_path = "~/code/zoning_mcmc/data/Right_of_Way/Excluded_Land_Right_of_Way.shp"
      )
  ),

  tar_target(
    adjacency_graph,
    build_adjacency_graph(
      geometry_sf = norwood_data$norwood_geometry,
      right_of_way_sf = norwood_data$norwood_right_of_way,
      row_proximity_ft = ROW_PROXIMITY_THRESHOLD
    )
  ),

  tar_target(
    constraints,
    define_constraints(norwood_data)
  ),

  # ============================================================================
  # TIER 2: PARCEL GRAPH CONSTRUCTION
  # ============================================================================

  # Parcel graph from parcel adjacency
  # MACRO_SCALE = 0: identity mapping (raw parcels)
  # MACRO_SCALE > 0: aggregation with scaled area targets
  tar_target(
    parcel_graph_result,
    if (MACRO_SCALE == 0) {
      build_identity_parcel_graph(adjacency_graph)
    } else {
      build_parcel_graph_target(
        adjacency_graph,
        target_area_min = MACRO_BASE_AREA_MIN * MACRO_SCALE,
        target_area_max = MACRO_BASE_AREA_MAX * MACRO_SCALE,
        atomic_threshold = MACRO_ATOMIC_THRESHOLD,
        seed = 123
      )
    }
  ),

  # Main MCMC config - can change freely
  # Changes here only rebuild parcel_mcmc_result
  # Using "lifted" config for non-reversible MCMC with momentum
  # (improves mixing in k dimension via direction persistence)
  tar_target(
    parcel_main_config,
    define_parcel_kernel_configs(n_steps = MCMC_STEPS_MACRO)[["lifted"]]
  ),

  # ============================================================================
  # TIER 3A: LCC DISCOVERY (Parallel Tree + BFS Architecture)
  # ============================================================================
  #
  # Two-stage discovery for LCCs:
  # 1. Tree Enumeration (Wilson's algorithm) - uniform coverage of tree-cuts
  # 2. BFS Supplement - exploration of non-tree-cut configurations
  #
  # Tree enumeration finds ~100 valid cuts per tree, much faster than MCMC.
  # BFS supplement explores boundary perturbations to find multi-crossing LCCs.

  # Compute station-feasibility regions for MCMC seeding
  # Finds viable station components and selects maximally distant seeds
  tar_target(
    parcel_station_regions,
    partition_parcels_by_station_feasibility(
      parcel_graph_result$parcel_graph,
      constraints,
      n_regions = 4
    )
  ),

  # LCC Stage 1: Tree enumeration
  # Each tree yields multiple valid cuts; n_trees controls coverage
  # Fast: ~30s for 500 trees, finds ~5000 LCCs
  tar_target(
    tree_discovered_lccs,
    discover_lccs_from_trees(
      parcel_graph = parcel_graph_result$parcel_graph,
      constraints = constraints,
      n_trees = TREE_LCC_N_TREES,
      forbidden_parcels = NULL,
      verbose = TRUE
    )
  ),

  # LCC Stage 2a: BFS boundary supplement (replaces MCMC discovery)
  # Seeds from tree LCCs at capacity quantiles, explores boundary perturbations
  # Finds LCCs with multiple boundary crossings that tree enumeration misses
  tar_target(
    bfs_discovered_lccs,
    run_bfs_lcc_supplement(
      tree_discovered_lccs = tree_discovered_lccs,
      parcel_graph = parcel_graph_result$parcel_graph,
      constraints = constraints,
      n_samples = BFS_LCC_N_SAMPLES,
      n_seeds = BFS_LCC_N_SEEDS,
      forbidden_parcels = NULL,
      verbose = TRUE
    )
  ),

  # LCC Stage 2b: Capacity-stratified BFS discovery
  # Tree discovery is biased toward high-capacity LCCs (78% have capacity > 5000).
  # The capacity prior (lambda=0.005) makes the posterior favor capacity 1200-2000.
  # This stage explicitly targets low-capacity bands to balance the library.
  tar_target(
    bfs_stratified_lccs,
    discover_lccs_by_capacity_bands(
      parcel_graph = parcel_graph_result$parcel_graph,
      constraints = constraints,
      capacity_bands_relative = LCC_CAPACITY_BANDS_RELATIVE,
      samples_per_band = LCC_BAND_SAMPLES_PER_BAND,
      max_attempts_per_band = LCC_BAND_MAX_ATTEMPTS,
      forbidden_parcels = NULL,
      existing_keys = c(
        tree_discovered_lccs$discovered_lccs$lcc_key,
        bfs_discovered_lccs$discovered_lccs$lcc_key
      ),
      verbose = TRUE
    )
  ),

  # LCC Stage 3: Combine all LCC discoveries (tree, bfs boundary, bfs stratified)
  tar_target(
    combined_discovered_lccs,
    combine_all_lcc_discoveries(
      tree_discovered = tree_discovered_lccs,
      bfs_discovered = bfs_discovered_lccs,
      bfs_stratified = bfs_stratified_lccs,
      verbose = TRUE
    )
  ),

  # LCC Stage 4: Build LCC library from combined discoveries
  # Uses coverage-aware selection to ensure geographic diversity
  tar_target(
    discovered_lcc_library,
    build_lcc_library_from_tree_discovery(
      combined_discovered_lccs$discovered_blocks,
      parcel_graph_result$parcel_graph,
      max_library_size = LCC_LIBRARY_MAX_SIZE,
      bfs_reservation = BFS_RESERVATION_LCC
    )
  ),

  # ============================================================================
  # TIER 3B: SECONDARY DISCOVERY (Parallel Tree + BFS Architecture)
  # ============================================================================
  #
  # Two-stage discovery for secondary blocks (parallel to LCC discovery):
  # 1. Tree Enumeration - uniform coverage of tree-cut secondaries
  # 2. BFS Supplement - fills gaps not covered by tree enumeration

  # Secondary Stage 1: Tree enumeration
  # Enumerates cuts within area bands [5-8], [8-12], [12-20] acres
  tar_target(
    tree_discovered_secondaries,
    discover_secondaries_from_trees(
      parcel_graph = parcel_graph_result$parcel_graph,
      size_bands = SEC_SIZE_BANDS,
      density_threshold = LIBRARY_DENSITY_THRESHOLD,
      n_trees = TREE_SEC_N_TREES,
      verbose = TRUE
    )
  ),

  # Secondary Stage 2: BFS supplement
  # Fills gaps in size band coverage not found by tree enumeration
  tar_target(
    bfs_discovered_secondaries,
    run_bfs_secondary_supplement(
      tree_discovered_secondaries = tree_discovered_secondaries,
      parcel_graph = parcel_graph_result$parcel_graph,
      size_bands = SEC_SIZE_BANDS,
      quota_per_band = BFS_SEC_QUOTA_PER_BAND,
      density_threshold = LIBRARY_DENSITY_THRESHOLD,
      verbose = TRUE
    )
  ),

  # Secondary Stage 3: Combine tree and BFS discoveries
  tar_target(
    combined_discovered_secondaries,
    combine_discovered_blocks(
      tree_discovered = tree_discovered_secondaries,
      bfs_discovered = bfs_discovered_secondaries,
      key_column = "sec_key",
      block_type = "secondary",
      verbose = TRUE
    )
  ),

  # Secondary Stage 4: Build secondary library from combined discoveries
  tar_target(
    discovered_secondary_library,
    build_secondary_library_from_discovery(
      combined_discovered = combined_discovered_secondaries,
      parcel_graph = parcel_graph_result$parcel_graph,
      max_library_size = SEC_LIBRARY_MAX_SIZE,
      bfs_reservation = BFS_RESERVATION_SEC
    )
  ),

  # ============================================================================
  # TIER 3C: PARCEL FEASIBILITY ANALYSIS
  # ============================================================================
  #
  # Offline analysis to determine which parcels can theoretically be part
  # of any valid configuration. Helps explain low-visit parcels in MCMC.
  # This runs before MCMC and answers: is parcel stickiness constraint-driven
  # or a sampler failure?

  tar_target(
    parcel_feasibility,
    analyze_parcel_feasibility(
      parcel_graph = parcel_graph_result$parcel_graph,
      lcc_library = discovered_lcc_library,
      secondary_library = discovered_secondary_library,
      constraints = constraints
    )
  ),

  tar_target(
    parcel_feasibility_summary,
    summarize_parcel_feasibility(
      feasibility_result = parcel_feasibility,
      parcel_graph = parcel_graph_result$parcel_graph
    )
  ),

  # ============================================================================
  # TIER 4: MAIN PARCEL MCMC (4 chains)
  # ============================================================================
  #
  # Run chains seeded from viable station components as the main MCMC result.
  # All chains share the discovered LCC library and are used for both sampling
  # and convergence diagnostics. Uses station-feasibility partitioning to guarantee
  # each chain can satisfy the 90% station capacity constraint.

  # Region partition for parcels (uses station-feasibility partitioning)
  tar_target(
    parcel_region_assignments,
    parcel_station_regions$region_assignments
  ),

  # Configuration for parcel multi-chain analysis
  # Uses actual region IDs from station-feasibility partitioning
  tar_target(
    parcel_multichain_config,
    define_parcel_multichain_configs(
      base_config = parcel_main_config,
      n_chains = parcel_station_regions$n_regions,
      n_steps = MCMC_STEPS_MACRO,
      region_ids = names(parcel_station_regions$seed_pools)
    )
  ),

  # Parcel chain grid for dynamic branching
  tar_target(
    parcel_chain_grid,
    data.frame(chain_id = seq_along(parcel_multichain_config))
  ),

  # Individual parcel chain runs (parallel via dynamic branching)
  tar_target(
    parcel_chain_results,
    run_parcel_chain_from_region(
      config = parcel_multichain_config[[parcel_chain_grid$chain_id]],
      parcel_graph_result = parcel_graph_result,
      constraints = constraints,
      secondary_library = discovered_secondary_library,
      lcc_library = discovered_lcc_library,
      region_assignments = parcel_region_assignments,
      seed_pools = parcel_station_regions$seed_pools,
      verbose = TRUE
    ),
    pattern = map(parcel_chain_grid),
    iteration = "list",
    packages = c("data.table", "igraph", "purrr", "cli", "mbtazone")
  ),

  # Combine all parcel chain results into named list
  tar_target(
    all_parcel_chain_results,
    setNames(
      parcel_chain_results,
      paste0("chain_", seq_along(parcel_chain_results))
    )
  ),

  # Multi-chain mixing metrics (uses coda for proper ESS)
  tar_target(
    parcel_metrics,
    compute_multichain_parcel_metrics(
      all_parcel_chain_results,
      parcel_graph_result$parcel_graph
    )
  ),

  # Parcel convergence diagnostics
  tar_target(
    parcel_rhat_table,
    compute_parcel_multichain_rhat(all_parcel_chain_results)
  ),

  tar_target(
    parcel_geographic_coverage,
    summarize_parcel_geographic_coverage(
      all_parcel_chain_results,
      parcel_graph_result$parcel_graph,
      parcel_region_assignments
    )
  ),

  tar_target(
    parcel_chain_separation,
    detect_parcel_chain_separation(
      all_parcel_chain_results,
      parcel_graph_result$parcel_graph
    )
  ),

  tar_target(
    parcel_irreducibility_report,
    create_parcel_irreducibility_report(
      all_parcel_chain_results,
      parcel_graph_result$parcel_graph,
      parcel_region_assignments
    )
  ),

  # Plot objects for Quarto report
  tar_target(
    parcel_rhat_plot_obj,
    plot_parcel_rhat_summary(parcel_rhat_table)
  ),

  # ============================================================================
  # TIER 5: QUARTO REPORT
  # ============================================================================

  tar_quarto(
    mcmc_diagnostics_report,
    system.file("reports/mcmc_diagnostics.qmd", package = "mbtazone"),
    quiet = FALSE
  ),

  tar_quarto(
    mcmc_diagnostics_llm_report,
    system.file("reports/mcmc_diagnostics_llm.qmd", package = "mbtazone"),
    quiet = FALSE
  )
)
