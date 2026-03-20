load_joint_refresh_test_config <- function() {
  sys.source(
    testthat::test_path("..", "..", "inst", "targets", "temp_targets_config.R"),
    envir = .GlobalEnv
  )
  sys.source(
    testthat::test_path("..", "..", "inst", "targets", "temp_targets_parcel_config.R"),
    envir = .GlobalEnv
  )

  assign("DEBUG_INVARIANT_CHECKS", FALSE, envir = .GlobalEnv)
  assign("ENABLE_ONLINE_ENRICHMENT", FALSE, envir = .GlobalEnv)
  assign("ONLINE_ENRICHMENT_INTERVAL", 100L, envir = .GlobalEnv)
  assign("ONLINE_MAX_ENTRIES", 0L, envir = .GlobalEnv)
  assign("ENRICHMENT_BURN_IN", NULL, envir = .GlobalEnv)
  assign("SAMPLE_MAX_STORED", 1000L, envir = .GlobalEnv)
  assign("STORE_LCC_SIGNATURES", FALSE, envir = .GlobalEnv)
}

make_test_graph <- function(parcel_names, edge_pairs, capacities, areas = NULL) {
  if (is.null(areas)) {
    areas <- rep(1, length(parcel_names))
  }

  graph <- igraph::make_empty_graph(
    n = length(parcel_names),
    directed = FALSE
  )
  graph <- igraph::set_vertex_attr(graph, "name", value = parcel_names)

  if (length(edge_pairs) > 0) {
    edge_indices <- unlist(lapply(edge_pairs, function(pair) {
      match(pair, parcel_names)
    }), use.names = FALSE)
    graph <- igraph::add_edges(graph, edge_indices)
  }

  graph <- igraph::set_vertex_attr(graph, "capacity", value = capacities)
  graph <- igraph::set_vertex_attr(graph, "area", value = areas)
  graph <- igraph::set_vertex_attr(graph, "capacity_in_station", value = rep(0, length(parcel_names)))
  graph <- igraph::set_vertex_attr(graph, "area_in_station", value = rep(0, length(parcel_names)))
  graph <- igraph::set_vertex_attr(graph, "centroid_x", value = seq_along(parcel_names))
  graph <- igraph::set_vertex_attr(graph, "centroid_y", value = rep(1, length(parcel_names)))

  graph
}

make_neighbor_cache <- function(graph) {
  parcel_names <- igraph::V(graph)$name
  stats::setNames(lapply(parcel_names, function(parcel_id) {
    igraph::neighbors(graph, parcel_id)$name
  }), parcel_names)
}

make_block_library <- function(graph, blocks, spectral_regions = NULL) {
  parcel_names <- igraph::V(graph)$name
  block_indices <- lapply(blocks, function(block) {
    sort(match(block, parcel_names))
  })

  neighbor_indices <- lapply(blocks, function(block) {
    block_neighbors <- unique(unlist(lapply(block, function(parcel_id) {
      igraph::neighbors(graph, parcel_id)$name
    }), use.names = FALSE))
    block_neighbor_idx <- match(block_neighbors, parcel_names)
    block_neighbor_idx <- block_neighbor_idx[!is.na(block_neighbor_idx)]
    sort(setdiff(block_neighbor_idx, match(block, parcel_names)))
  })

  if (is.null(spectral_regions)) {
    spectral_regions <- rep(NA_character_, length(blocks))
  }

  metadata <- data.table::rbindlist(lapply(seq_along(blocks), function(i) {
    block <- blocks[[i]]
    data.table::data.table(
      capacity = sum(igraph::V(graph)[block]$capacity),
      area = sum(igraph::V(graph)[block]$area),
      area_in_station = sum(igraph::V(graph)[block]$area_in_station),
      capacity_in_station = sum(igraph::V(graph)[block]$capacity_in_station),
      n_parcels = length(block),
      density = sum(igraph::V(graph)[block]$capacity) /
        sum(igraph::V(graph)[block]$area),
      size_band = "test",
      source = "test",
      spectral_region = spectral_regions[[i]]
    )
  }))

  list(
    n_blocks = length(blocks),
    blocks = block_indices,
    neighbor_indices = neighbor_indices,
    parcel_names = parcel_names,
    metadata = metadata,
    active_mask = rep(TRUE, length(blocks)),
    block_hashes = NULL
  )
}

make_test_constraints <- function(
  min_capacity,
  min_area,
  min_density = 1,
  min_lcc_fraction = 0.5
) {
  list(
    min_capacity = min_capacity,
    min_area = min_area,
    min_density = min_density,
    min_lcc_fraction = min_lcc_fraction,
    station_area_pct = NA_real_,
    station_capacity_pct = NA_real_
  )
}

make_add_scan_fixture <- function() {
  graph <- make_test_graph(
    parcel_names = c("p1", "p2", "p4", "p5", "p6"),
    edge_pairs = list(c("p1", "p2"), c("p4", "p5")),
    capacities = c(20, 20, 15, 10, 12)
  )
  secondary_library <- make_block_library(
    graph,
    blocks = list(c("p4"), c("p5"), c("p6"))
  )

  list(
    graph = graph,
    secondary_library = secondary_library,
    base_state = create_lcc_only_state(
      c("p1", "p2"),
      secondary_library,
      graph
    ),
    constraints = make_test_constraints(
      min_capacity = 50,
      min_area = 2,
      min_density = 1,
      min_lcc_fraction = 0.5
    )
  )
}

make_joint_refresh_fixture <- function() {
  graph <- make_test_graph(
    parcel_names = c("p1", "p2", "p3", "p4", "p5", "p6", "p7"),
    edge_pairs = list(c("p1", "p2"), c("p3", "p4")),
    capacities = c(45, 40, 35, 45, 20, 15, 10)
  )

  secondary_library <- make_block_library(
    graph,
    blocks = list(c("p5"), c("p6"), c("p7"))
  )
  lcc_library <- make_block_library(
    graph,
    blocks = list(c("p1", "p2"), c("p3", "p4")),
    spectral_regions = c("R1", "R2")
  )

  list(
    graph = graph,
    secondary_library = secondary_library,
    lcc_library = lcc_library,
    neighbor_cache = make_neighbor_cache(graph),
    constraints = make_test_constraints(
      min_capacity = 100,
      min_area = 2,
      min_density = 1,
      min_lcc_fraction = 0.5
    ),
    initial_state = initialize_parcel_state(
      parcel_ids = c("p1", "p2", "p5", "p6"),
      secondary_block_ids = c(1L, 2L),
      library = secondary_library,
      parcel_graph = graph
    )
  )
}

test_that("joint refresh core remove-one samples correctly", {
  load_joint_refresh_test_config()

  # With empty source, always returns empty with log_prob = 0
  result <- joint_refresh_core_remove_one(integer(0), p_keep = 0.5)
  expect_equal(result$retained_ids, integer(0))
  expect_null(result$removed_id)
  expect_equal(result$log_prob, 0)

  # With p_keep = 1, always keeps all
  set.seed(42)
  result <- joint_refresh_core_remove_one(c(1L, 2L, 3L), p_keep = 1.0)
  expect_equal(result$retained_ids, c(1L, 2L, 3L))
  expect_null(result$removed_id)
  expect_equal(result$log_prob, log(1.0))

  # With p_keep = 0, always removes one
  set.seed(42)
  result <- joint_refresh_core_remove_one(c(1L, 2L, 3L), p_keep = 0.0)
  expect_length(result$retained_ids, 2)
  expect_true(!is.null(result$removed_id))
  expect_true(result$removed_id %in% c(1L, 2L, 3L))
  expect_equal(result$log_prob, log(1.0 / 3))
})

test_that("joint refresh core remove-one logprob matches sampling", {
  load_joint_refresh_test_config()

  source_ids <- c(1L, 2L, 3L)
  p_keep <- 0.5

  # Remove nothing
  lp_keep <- joint_refresh_core_remove_one_logprob(source_ids, NULL, p_keep)
  expect_equal(lp_keep, log(0.5))

  # Remove block 2
  lp_remove <- joint_refresh_core_remove_one_logprob(source_ids, 2L, p_keep)
  expect_equal(lp_remove, log(0.5 / 3))

  # Remove block not in source → -Inf
  lp_bad <- joint_refresh_core_remove_one_logprob(source_ids, 99L, p_keep)
  expect_equal(lp_bad, -Inf)
})

test_that("joint refresh categorical add returns valid log-probs", {
  load_joint_refresh_test_config()
  fixture <- make_add_scan_fixture()

  mean_secondary_cap <- joint_refresh_mean_secondary_capacity(
    fixture$secondary_library
  )

  set.seed(42)
  result <- joint_refresh_categorical_add(
    base_state = fixture$base_state,
    source_secondary_ids = integer(0),
    secondary_library = fixture$secondary_library,
    parcel_graph = fixture$graph,
    constraints = fixture$constraints,
    mean_secondary_cap = mean_secondary_cap
  )

  expect_true(is.finite(result$log_prob))
  expect_true(result$log_prob < 0)
  expect_true(is.null(result$added_id) || result$added_id %in% seq_len(fixture$secondary_library$n_blocks))
})

test_that("joint refresh categorical add logprob matches forward sampling", {
  load_joint_refresh_test_config()
  fixture <- make_add_scan_fixture()

  mean_secondary_cap <- joint_refresh_mean_secondary_capacity(
    fixture$secondary_library
  )

  # Get logprob for "add nothing"
  lp_nothing <- joint_refresh_categorical_add_logprob(
    target_id = NULL,
    base_state = fixture$base_state,
    source_secondary_ids = integer(0),
    secondary_library = fixture$secondary_library,
    parcel_graph = fixture$graph,
    constraints = fixture$constraints,
    mean_secondary_cap = mean_secondary_cap
  )
  expect_true(is.finite(lp_nothing))

  # Get logprob for adding block 1
  lp_block1 <- joint_refresh_categorical_add_logprob(
    target_id = 1L,
    base_state = fixture$base_state,
    source_secondary_ids = integer(0),
    secondary_library = fixture$secondary_library,
    parcel_graph = fixture$graph,
    constraints = fixture$constraints,
    mean_secondary_cap = mean_secondary_cap
  )
  expect_true(is.finite(lp_block1))

  # Non-candidate block → -Inf (block already in source_secondary_ids)
  lp_impossible <- joint_refresh_categorical_add_logprob(
    target_id = 1L,
    base_state = fixture$base_state,
    source_secondary_ids = c(1L, 2L, 3L),
    secondary_library = fixture$secondary_library,
    parcel_graph = fixture$graph,
    constraints = fixture$constraints,
    mean_secondary_cap = mean_secondary_cap
  )
  expect_equal(lp_impossible, -Inf)
})

test_that("joint refresh LCC candidates depend only on the retained core", {
  load_joint_refresh_test_config()
  fixture <- make_joint_refresh_fixture()

  state_a <- fixture$initial_state
  state_b <- initialize_parcel_state(
    parcel_ids = c("p3", "p4", "p5", "p7"),
    secondary_block_ids = c(1L, 3L),
    library = fixture$secondary_library,
    parcel_graph = fixture$graph
  )

  retained_core_a <- intersect(state_a$secondary_blocks, 1L)
  retained_core_b <- intersect(state_b$secondary_blocks, 1L)

  cand_a <- joint_refresh_candidate_lccs(
    retained_core_ids = retained_core_a,
    lcc_library = fixture$lcc_library,
    secondary_library = fixture$secondary_library,
    parcel_graph = fixture$graph,
    constraints = fixture$constraints
  )
  cand_b <- joint_refresh_candidate_lccs(
    retained_core_ids = retained_core_b,
    lcc_library = fixture$lcc_library,
    secondary_library = fixture$secondary_library,
    parcel_graph = fixture$graph,
    constraints = fixture$constraints
  )

  expect_identical(cand_a$candidate_ids, cand_b$candidate_ids)
  expect_equal(cand_a$log_weights, cand_b$log_weights)
  expect_equal(cand_a$required_cap, cand_b$required_cap)
})

test_that("run_parcel_mcmc accepts joint refresh proposals that change LCC and secondaries", {
  load_joint_refresh_test_config()
  fixture <- make_joint_refresh_fixture()

  config <- list(
    name = "Joint refresh smoke test",
    seed = 123,
    n_steps = 200L,
    p_lcc_local = 0,
    p_symmetric_birth_death = 0,
    p_swap = 0,
    p_replace_lcc = 1,
    use_lifted = FALSE
  )

  set.seed(config$seed)
  result <- run_parcel_mcmc(
    parcel_graph = fixture$graph,
    initial_state = fixture$initial_state,
    constraints = fixture$constraints,
    secondary_library = fixture$secondary_library,
    lcc_library = fixture$lcc_library,
    config = config,
    parcel_assignments = data.table::data.table(parcel_id = igraph::V(fixture$graph)$name),
    neighbor_cache = fixture$neighbor_cache,
    enable_online_enrichment = FALSE,
    enrichment_interval = 100L,
    max_online_entries = 0L,
    enrichment_burn_in = NULL,
    max_stored_samples = 1000L,
    store_lcc_signatures = FALSE,
    verbose = FALSE
  )

  replace_stats <- result$stats[move_type == "replace_lcc"]
  expect_equal(replace_stats$n_attempted, config$n_steps)
  expect_gt(replace_stats$n_accepted, 0)

  expect_true("joint_refresh_reasons" %in% names(result$diagnostics))
  expect_true("joint_refresh_accept_prob" %in% names(result$diagnostics))
  expect_true("joint_refresh_core_size" %in% names(result$diagnostics))
  expect_gt(length(result$diagnostics$joint_refresh_accept_prob), 0)

  parcel_samples <- result$parcel_samples
  changed_both <- vapply(seq.int(2, length(parcel_samples)), function(i) {
    prev <- parcel_samples[[i - 1L]]
    cur <- parcel_samples[[i]]
    !setequal(prev$lcc_parcels, cur$lcc_parcels) &&
      !setequal(prev$secondary_blocks, cur$secondary_blocks)
  }, logical(1))
  expect_true(any(changed_both))
})
