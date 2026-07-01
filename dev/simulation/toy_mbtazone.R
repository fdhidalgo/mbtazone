# dev/simulation/toy_mbtazone.R
#
# Parallel implementation of toy.R using the mbtazone MCMC infrastructure.
# Same scenarios, same analysis goals — different sampler.
#
# Key differences from toy.R:
#   1. State = (LCC_id, {secondary_block_ids}) rather than a flat parcel set.
#      One parcel plan in toy.R can correspond to several mbtazone states
#      (e.g., {1,2,5} with LCC={1,2} vs LCC={2,5}).
#   2. No hard max_capacity. The capacity prior (CAPACITY_PRIOR_LAMBDA) softly
#      penalises excess; K_PRIOR_LAMBDA (geometric prior on k) penalises extra
#      secondary blocks.
#   3. No analytical transition matrix. mbtazone's state space is larger and
#      not directly comparable to toy.R's detailed-balance check.
#   4. Convergence via Gelman-Rubin R-hat across 4 chains rather than exact
#      matrix analysis.
#   5. density_threshold = 0 throughout: toy parcels have capacity=1, area=6
#      → density ≈ 0.17, far below the real-data default of 15. Must override.
#
# Usage: source this file from an R session at the package root:
#   devtools::load_all()   # exposes unexported internals like run_parcel_mcmc
#   source("dev/simulation/toy_mbtazone.R")

library(data.table)
library(igraph)
library(purrr)
library(glue)
library(ggplot2)

devtools::load_all()
source("inst/targets/temp_targets_config.R")       # K_PRIOR_LAMBDA, CAPACITY_PRIOR_LAMBDA, ...
source("inst/targets/temp_targets_parcel_config.R") # BFS_LCC_N_SAMPLES, BFS_LCC_N_SEEDS, ...

# ============================================================================
# GRAPH CONSTRUCTION
# ============================================================================
# Required igraph vertex attributes:
#   name, capacity, area, capacity_in_station, area_in_station,
#   centroid_x, centroid_y  (used by max-min seed selection in generate_initial_states_from_lccs)

create_scenario1_graph_mbtazone <- function() {
  # Scenario 1: Bridge — connected chain version of toy.R's disconnected bridge.
  # toy.R:         P1—P2  [gap]  P5—P6
  # here:          P1—P2—P5—P6  (adds P2-P5 edge)
  # Spanning-tree discovery requires a connected graph; the added edge does not
  # change the feasible plan set (same 4 plans under the same constraints).
  parcel_ids <- c("1", "2", "5", "6")
  g <- make_empty_graph(n = 4, directed = FALSE)
  V(g)$name <- parcel_ids
  g <- add_edges(g, c("1", "2",  "2", "5",  "5", "6"))

  V(g)$capacity            <- 1L
  V(g)$area                <- 6.0
  V(g)$capacity_in_station <- 0L
  V(g)$area_in_station     <- 0.0
  V(g)$centroid_x          <- c(0, 1, 2, 3)
  V(g)$centroid_y          <- c(0, 0, 0, 0)
  g
}

create_scenario2_graph_mbtazone <- function() {
  # Scenario 2: Connected Chain — identical to toy.R
  # P1—P2—P3—P4—P5—P6—P7—P8
  parcel_ids <- as.character(1:8)
  g <- make_empty_graph(n = 8, directed = FALSE)
  V(g)$name <- parcel_ids
  edges <- unlist(lapply(1:7, function(i) c(as.character(i), as.character(i + 1))))
  g <- add_edges(g, edges)

  V(g)$capacity            <- 1L
  V(g)$area                <- 6.0
  V(g)$capacity_in_station <- 0L
  V(g)$area_in_station     <- 0.0
  V(g)$centroid_x          <- 0:7
  V(g)$centroid_y          <- rep(0, 8)
  g
}

# ============================================================================
# CONSTRAINTS
# ============================================================================
# station_*_pct = NA disables station-area checks — no station areas in toy scenarios.

make_toy_constraints <- function(min_capacity, min_area, min_density,
                                 min_lcc_fraction = 0.5) {
  list(
    min_capacity         = min_capacity,
    min_area             = min_area,
    min_density          = min_density,
    min_lcc_fraction     = min_lcc_fraction,
    station_capacity_pct = NA_real_,
    station_area_pct     = NA_real_
  )
}

# ============================================================================
# LIBRARY BUILDING
# ============================================================================

build_toy_libraries <- function(parcel_graph, constraints,
                                n_lcc_trees = 300L, n_sec_trees = 200L) {
  parcel_names   <- V(parcel_graph)$name
  neighbor_cache <- setNames(
    lapply(parcel_names, function(p) neighbors(parcel_graph, p)$name),
    parcel_names
  )

  # --- LCC discovery: tree enumeration + BFS supplement ---
  tree_lccs <- discover_lccs_from_trees(
    parcel_graph           = parcel_graph,
    constraints            = constraints,
    n_trees                = n_lcc_trees,
    max_discovery_capacity = constraints$min_capacity * DISCOVERY_CAPACITY_MULTIPLIER
  )
  bfs_lccs <- run_bfs_lcc_supplement(
    tree_discovered_lccs = tree_lccs,
    parcel_graph         = parcel_graph,
    constraints          = constraints,
    n_samples            = BFS_LCC_N_SAMPLES,
    n_seeds              = min(BFS_LCC_N_SEEDS, 5L)
  )
  combined_lccs <- combine_all_lcc_discoveries(
    tree_discovered = tree_lccs,
    bfs_discovered  = bfs_lccs,
    bfs_stratified  = NULL
  )
  lcc_library <- build_lcc_library_from_tree_discovery(
    discovered_lccs = combined_lccs$discovered_blocks,
    parcel_graph    = parcel_graph,
    constraints     = constraints
  )

  # --- Secondary discovery: tree enumeration + BFS supplement ---
  # density_threshold = 0: toy parcels (capacity=1, area=6) have density ≈ 0.17,
  # far below the production default of 15. size_bands cover single (6 ac) and
  # pair (12 ac) blocks within [5,8] and [8,12].
  tree_secs <- discover_secondaries_from_trees(
    parcel_graph      = parcel_graph,
    size_bands        = list(c(5, 8), c(8, 12)),
    density_threshold = 0,
    n_trees           = n_sec_trees
  )
  bfs_secs <- run_bfs_secondary_supplement(
    tree_discovered_secondaries = tree_secs,
    parcel_graph                = parcel_graph,
    size_bands                  = list(c(5, 8), c(8, 12)),
    quota_per_band              = 20L,
    density_threshold           = 0
  )
  combined_secs     <- combine_discovered_blocks(
    tree_discovered = tree_secs,
    bfs_discovered  = bfs_secs,
    key_column      = "sec_key",
    block_type      = "secondary"
  )
  secondary_library <- build_secondary_library_from_discovery(
    combined_discovered = combined_secs,
    parcel_graph        = parcel_graph
  )

  list(
    lcc_library       = lcc_library,
    secondary_library = secondary_library,
    neighbor_cache    = neighbor_cache
  )
}

# ============================================================================
# SAMPLE EXTRACTION
# ============================================================================
# state$X is the character vector of all selected parcel IDs (LCC + secondaries).
# This is the mbtazone equivalent of a "plan" in toy.R.

extract_parcel_plan <- function(state) {
  if (is.null(state)) return(NULL)
  sort(state$X)
}

# ============================================================================
# FEASIBILITY HELPERS (from toy.R — for ground-truth plan enumeration)
# ============================================================================

SECONDARY_AREA_THRESHOLD <- 5

find_components_toy <- function(g, selected_parcels) {
  comp <- decompose(induced_subgraph(g, selected_parcels))
  list(
    components   = comp,
    capacities   = sapply(comp, \(c) sum(V(c)$capacity)),
    areas        = sapply(comp, \(c) sum(V(c)$area)),
    n_components = length(comp)
  )
}

identify_primary_component_toy <- function(component_list, capacities) {
  candidates <- which(capacities == max(capacities))
  if (length(candidates) == 1) return(candidates)
  candidates[which.min(sapply(component_list[candidates], \(c) min(V(c)$name)))]
}

is_feasible_toy <- function(g, selected_parcels, constraints) {
  if (length(selected_parcels) == 0) return(FALSE)
  comp <- find_components_toy(g, selected_parcels)
  total_cap  <- sum(comp$capacities)
  total_area <- sum(comp$areas)

  if (total_cap < constraints$min_capacity) return(FALSE)

  primary_idx   <- identify_primary_component_toy(comp$components, comp$capacities)
  eligible_area <- comp$areas[primary_idx]
  if (comp$n_components > 1) {
    sec_areas     <- comp$areas[-primary_idx]
    eligible_area <- eligible_area + sum(sec_areas[sec_areas >= SECONDARY_AREA_THRESHOLD])
  }
  if (eligible_area < constraints$min_area)                                     return(FALSE)
  if (total_cap / total_area < constraints$min_density)                         return(FALSE)
  if (comp$capacities[primary_idx] < constraints$min_lcc_fraction * total_cap) return(FALSE)
  TRUE
}

find_feasible_plans <- function(g, k, constraints) {
  all_parcels    <- V(g)$name
  plans_matrix   <- combn(all_parcels, k)
  all_plans      <- map(seq_len(ncol(plans_matrix)), \(i) plans_matrix[, i])
  feasible_plans <- keep(all_plans, \(p) is_feasible_toy(g, p, constraints))
  list(all_plans = all_plans, feasible_plans = feasible_plans,
       n_plans = length(all_plans), n_feasible = length(feasible_plans))
}

match_plan <- function(sample_plan, feasible_plans) {
  s <- paste(sort(sample_plan), collapse = ", ")
  f <- map_chr(feasible_plans$feasible_plans, \(p) paste(sort(p), collapse = ", "))
  match(s, f)
}

# ============================================================================
# SCENARIO 1: BRIDGE (mbtazone)
# ============================================================================
# Expected: uniform over 4 feasible plans — same as toy.R Scenario 1.
# Note: mbtazone samples over (LCC, secondaries) states, not parcel sets directly.
# Multiple states can map to the same plan, so the empirical plan distribution
# will differ from toy.R's unless the priors are very weak.

cat("=== Scenario 1: Bridge (mbtazone) ===\n")

g1           <- create_scenario1_graph_mbtazone()
constraints1 <- make_toy_constraints(min_capacity = 3, min_area = 10, min_density = 0.1)

feasible1 <- find_feasible_plans(g1, k = 3, constraints = constraints1)
cat("Feasible plans:", feasible1$n_feasible, "\n")
walk(seq_len(feasible1$n_feasible), \(i) {
  cat(glue("  Plan {i}: {{{paste(feasible1$feasible_plans[[i]], collapse=', ')}}}\n\n"))
})

libs1 <- build_toy_libraries(g1, constraints1, n_lcc_trees = 300L)
cat(glue("\nLCC library:       {libs1$lcc_library$n_blocks} blocks\n"))
cat(glue("Secondary library: {libs1$secondary_library$n_blocks} blocks\n\n"))

init_states1 <- generate_initial_states_from_lccs(
  lcc_library  = libs1$lcc_library,
  libraries    = list(
    secondary_library = libs1$secondary_library,
    lcc_library       = libs1$lcc_library
  ),
  parcel_graph = g1,
  constraints  = constraints1,
  n_chains     = 4L
)

config1 <- define_parcel_kernel_configs(n_steps = 5000L)$default

chain_results1 <- lapply(seq_along(init_states1), function(i) {
  run_parcel_mcmc(
    parcel_graph             = g1,
    initial_state            = init_states1[[i]],
    constraints              = constraints1,
    secondary_library        = libs1$secondary_library,
    lcc_library              = libs1$lcc_library,
    config                   = config1,
    parcel_assignments       = NULL,
    neighbor_cache           = libs1$neighbor_cache,
    enable_online_enrichment = FALSE,
    max_stored_samples       = 500L,
    verbose                  = FALSE
  )
})

plan_ids1 <- lapply(chain_results1, function(chain) {
  map_int(chain$parcel_samples, function(s) {
    if (is.null(s)) return(NA_integer_)
    match_plan(extract_parcel_plan(s), feasible1)
  })
})

cat("Acceptance rates (chain 1):\n")
print(chain_results1[[1]]$stats)

props1 <- prop.table(table(plan_ids1[[1]], useNA = "no"))
cat("\nEmpirical plan proportions (chain 1, expected ~0.25 each):\n")
print(props1)

trace1 <- data.table(step = seq_along(plan_ids1[[1]]), plan_id = plan_ids1[[1]])

trace_plot1 <- ggplot(trace1[!is.na(plan_id)], aes(x = step, y = plan_id)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 1:4, linetype = "dashed", alpha = 0.3) +
  scale_y_continuous(breaks = 1:4) +
  labs(title = "Scenario 1 (mbtazone): MCMC Trace", x = "Step", y = "Plan ID")
trace_plot1

hist_plot1 <- ggplot(trace1[!is.na(plan_id)], aes(x = factor(plan_id))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "red") +
  ylim(0, 0.6) +
  labs(title = "Scenario 1 (mbtazone): Empirical Distribution",
       subtitle = "Red = uniform expectation (0.25)", x = "Plan ID", y = "Proportion")
hist_plot1

rhat1 <- compute_parcel_multichain_rhat(chain_results1)
cat("\nGelman-Rubin R-hat (< 1.1 = converged):\n")
print(rhat1)

# ============================================================================
# SCENARIO 2: CONNECTED CHAIN (mbtazone)
# ============================================================================

cat("\n=== Scenario 2: Connected Chain (mbtazone) ===\n")

g2           <- create_scenario2_graph_mbtazone()
constraints2 <- make_toy_constraints(min_capacity = 5, min_area = 10, min_density = 0.1)

feasible2 <- find_feasible_plans(g2, k = 5, constraints = constraints2)
cat("Total k=5 subsets:", feasible2$n_plans, "\n")
cat("Feasible plans:   ", feasible2$n_feasible, "\n")

libs2 <- build_toy_libraries(g2, constraints2, n_lcc_trees = 500L, n_sec_trees = 300L)
cat(glue("\nLCC library:       {libs2$lcc_library$n_blocks} blocks\n"))
cat(glue("Secondary library: {libs2$secondary_library$n_blocks} blocks\n\n"))

init_states2 <- generate_initial_states_from_lccs(
  lcc_library  = libs2$lcc_library,
  libraries    = list(
    secondary_library = libs2$secondary_library,
    lcc_library       = libs2$lcc_library
  ),
  parcel_graph = g2,
  constraints  = constraints2,
  n_chains     = 4L
)

config2 <- define_parcel_kernel_configs(n_steps = 25000L)$default

chain_results2 <- lapply(seq_along(init_states2), function(i) {
  run_parcel_mcmc(
    parcel_graph             = g2,
    initial_state            = init_states2[[i]],
    constraints              = constraints2,
    secondary_library        = libs2$secondary_library,
    lcc_library              = libs2$lcc_library,
    config                   = config2,
    parcel_assignments       = NULL,
    neighbor_cache           = libs2$neighbor_cache,
    enable_online_enrichment = FALSE,
    max_stored_samples       = 1000L,
    verbose                  = FALSE
  )
})

plan_ids2 <- lapply(chain_results2, function(chain) {
  map_int(chain$parcel_samples, function(s) {
    if (is.null(s)) return(NA_integer_)
    match_plan(extract_parcel_plan(s), feasible2)
  })
})

cat("Acceptance rates (chain 1):\n")
print(chain_results2[[1]]$stats)

props2            <- prop.table(table(plan_ids2[[1]], useNA = "no"))
props2
expected_uniform2 <- 1 / feasible2$n_feasible
cat(glue("\nTop 10 most-visited plans (chain 1), uniform expectation = {round(expected_uniform2, 3)}:\n"))
print(head(sort(props2, decreasing = TRUE), 10))

trace2 <- data.table(step = seq_along(plan_ids2[[1]]), plan_id = plan_ids2[[1]])

trace_plot2 <- ggplot(trace2[!is.na(plan_id)], aes(x = step, y = plan_id)) +
  geom_line(alpha = 0.5) +
  labs(title = "Scenario 2 (mbtazone): MCMC Trace", x = "Step", y = "Plan ID")
trace_plot2

hist_plot2 <- ggplot(trace2[!is.na(plan_id)], aes(x = factor(plan_id))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  geom_hline(yintercept = expected_uniform2, linetype = "dashed", color = "red") +
  labs(title = "Scenario 2 (mbtazone): Empirical Distribution",
       subtitle = glue("Red = uniform expectation ({round(expected_uniform2, 3)})"),
       x = "Plan ID", y = "Proportion")
hist_plot2

rhat2 <- compute_parcel_multichain_rhat(chain_results2)
cat("\nGelman-Rubin R-hat:\n")
print(rhat2)

# ============================================================================
# SCENARIO 3: 5-PARCEL CHAIN (mbtazone mixes)
# ============================================================================
# Minimal graph where mbtazone's disjoint + non-adjacent secondary rule still
# permits the chain to traverse multiple feasible plans.
#
# Graph: P1—P2—P3—P4—P5 (single chain)
#
# Why Scenario 1 (4-parcel bridge) traps the chain and this one doesn't:
#   - On the bridge, a 2-parcel LCC at one end has exactly one non-adjacent
#     parcel available as a secondary (the far end), and all replace-LCC or
#     death moves are blocked by the min_capacity floor or by adjacency.
#   - On a 5-chain, a 2-parcel LCC at one end (e.g. {1,2}) has TWO non-adjacent
#     secondary candidates ({4} and {5}), so secondary-swap is non-trivial.
#     The chain can also transit through 4-parcel k=1 intermediate states
#     (e.g. LCC={1,2,3} + sec={5}), which bridge k=0 plans and disconnected
#     k=1 plans.
#
# Note: match_plan() projects to k=3 plans only, so 4-parcel intermediate
# states appear as NA. That's expected — the point is that the chain visits
# multiple non-NA plan IDs, not uniformity. The distribution is NOT uniform:
# the capacity prior favors cap=3 (k=0 with 3-parcel LCC, or k=1 with
# 2-parcel LCC + 1-parcel sec), and plans with multiple (LCC, sec)
# decompositions get extra mass.

cat("\n=== Scenario 3: 5-Parcel Chain (mbtazone mixes) ===\n")

create_scenario3_graph_mbtazone <- function() {
  parcel_ids <- as.character(1:5)
  g <- make_empty_graph(n = 5, directed = FALSE)
  V(g)$name <- parcel_ids
  edges <- unlist(lapply(1:4, function(i) c(as.character(i), as.character(i + 1))))
  g <- add_edges(g, edges)

  V(g)$capacity            <- 1L
  V(g)$area                <- 6.0
  V(g)$capacity_in_station <- 0L
  V(g)$area_in_station     <- 0.0
  V(g)$centroid_x          <- 0:4
  V(g)$centroid_y          <- rep(0, 5)
  g
}

g3           <- create_scenario3_graph_mbtazone()
constraints3 <- make_toy_constraints(min_capacity = 3, min_area = 10, min_density = 0.1)

feasible3 <- find_feasible_plans(g3, k = 3, constraints = constraints3)
cat("Feasible k=3 plans:", feasible3$n_feasible, "\n")
walk(seq_len(feasible3$n_feasible), \(i) {
  cat(glue("  Plan {i}: {{{paste(feasible3$feasible_plans[[i]], collapse=', ')}}}\n"))
})

libs3 <- build_toy_libraries(g3, constraints3, n_lcc_trees = 400L, n_sec_trees = 250L)
cat(glue("\nLCC library:       {libs3$lcc_library$n_blocks} blocks\n"))
cat(glue("Secondary library: {libs3$secondary_library$n_blocks} blocks\n\n"))

init_states3 <- generate_initial_states_from_lccs(
  lcc_library  = libs3$lcc_library,
  libraries    = list(
    secondary_library = libs3$secondary_library,
    lcc_library       = libs3$lcc_library
  ),
  parcel_graph = g3,
  constraints  = constraints3,
  n_chains     = 4L
)

config3 <- define_parcel_kernel_configs(n_steps = 10000L)$default

chain_results3 <- lapply(seq_along(init_states3), function(i) {
  run_parcel_mcmc(
    parcel_graph             = g3,
    initial_state            = init_states3[[i]],
    constraints              = constraints3,
    secondary_library        = libs3$secondary_library,
    lcc_library              = libs3$lcc_library,
    config                   = config3,
    parcel_assignments       = NULL,
    neighbor_cache           = libs3$neighbor_cache,
    enable_online_enrichment = FALSE,
    max_stored_samples       = 1000L,
    verbose                  = FALSE
  )
})

plan_ids3 <- lapply(chain_results3, function(chain) {
  map_int(chain$parcel_samples, function(s) {
    if (is.null(s)) return(NA_integer_)
    match_plan(extract_parcel_plan(s), feasible3)
  })
})

cat("Acceptance rates (chain 1):\n")
print(chain_results3[[1]]$stats)

cat("\nPlans visited per chain (k=3 only; NAs are 4-parcel intermediates):\n")
print(lapply(plan_ids3, function(p) sort(unique(p[!is.na(p)]))))

props3 <- prop.table(table(plan_ids3[[1]], useNA = "ifany"))
cat("\nEmpirical distribution including NA (chain 1):\n")
print(props3)

trace3 <- data.table(step = seq_along(plan_ids3[[1]]), plan_id = plan_ids3[[1]])

trace_plot3 <- ggplot(trace3[!is.na(plan_id)], aes(x = step, y = plan_id)) +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = seq_len(feasible3$n_feasible),
             linetype = "dashed", alpha = 0.3) +
  scale_y_continuous(breaks = seq_len(feasible3$n_feasible)) +
  labs(title = "Scenario 3 (mbtazone): MCMC Trace",
       subtitle = "5-parcel chain — non-adjacency rule still permits mixing",
       x = "Step", y = "Plan ID")
print(trace_plot3)

hist_plot3 <- ggplot(trace3[!is.na(plan_id)], aes(x = factor(plan_id))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  labs(title = "Scenario 3 (mbtazone): Empirical Distribution",
       subtitle = "Not expected to be uniform — capacity & k priors shape it",
       x = "Plan ID", y = "Proportion")
print(hist_plot3)

rhat3 <- compute_parcel_multichain_rhat(chain_results3)
cat("\nGelman-Rubin R-hat:\n")
print(rhat3)

# ============================================================================
# SCENARIO 3 (cont.): Empirical vs. expected under the priors
# ============================================================================
# The target distribution on states is
#   pi(state) ∝ exp(-CAPACITY_PRIOR_LAMBDA * max(0, cap - min_cap)
#                   - K_PRIOR_LAMBDA * k)
# on hard-feasible (LCC, secondaries) tuples. Each plan = union(LCC, secs) may
# be reached via several distinct decompositions, so the marginal over plans is
#   P(plan) ∝ sum over (LCC, secs) decompositions of plan { exp(-...) }
#
# Enumerating all feasible states is tractable on this 5-parcel chain
# (LCC library × secondary subsets). We compute the exact P(plan) and compare
# to the chain's empirical distribution. Pool all 4 chains for the empirical
# estimate to mitigate any residual orbit-isolation effects.

cat("\n--- Scenario 3: empirical vs. expected ---\n")

set_neighbors <- function(g, parcels) {
  if (length(parcels) == 0) return(character(0))
  nbrs <- unique(unlist(lapply(parcels, \(p) igraph::neighbors(g, p)$name)))
  setdiff(nbrs, parcels)
}

enumerate_feasible_states <- function(parcel_graph, lcc_library,
                                      secondary_library, constraints) {
  pnames   <- igraph::V(parcel_graph)$name
  caps     <- setNames(igraph::V(parcel_graph)$capacity, pnames)
  areas    <- setNames(igraph::V(parcel_graph)$area,     pnames)
  lcc_sets <- lapply(lcc_library$blocks,       \(idx) sort(pnames[idx]))
  sec_sets <- lapply(secondary_library$blocks, \(idx) sort(pnames[idx]))

  states <- list()

  for (lcc in lcc_sets) {
    lcc_forbidden <- c(lcc, set_neighbors(parcel_graph, lcc))
    compat_ids    <- which(vapply(sec_sets,
                                  \(s) length(intersect(s, lcc_forbidden)) == 0,
                                  logical(1)))

    # k = 0 (no secondaries) first
    plan_parcels <- lcc
    if (anyDuplicated(plan_parcels)) next                    # defensive: malformed LCC block
    cap   <- sum(caps[plan_parcels])
    area  <- sum(areas[plan_parcels])
    lcap  <- sum(caps[lcc])
    if (cap >= constraints$min_capacity &&
        area >= constraints$min_area &&
        cap / area >= constraints$min_density &&
        lcap >= constraints$min_lcc_fraction * cap) {
      states[[length(states) + 1]] <- list(
        plan = paste(sort(plan_parcels), collapse = ","),
        k = 0L, cap = cap
      )
    }

    if (length(compat_ids) == 0) next

    # k >= 1: enumerate non-empty subsets of compat_ids, enforcing pairwise
    # disjoint + non-adjacent between secondaries.
    for (k in seq_along(compat_ids)) {
      combos <- utils::combn(compat_ids, k, simplify = FALSE)
      for (combo in combos) {
        secs       <- sec_sets[combo]
        sec_union  <- unlist(secs)
        if (anyDuplicated(sec_union)) next                        # disjoint
        ok <- TRUE
        if (k > 1L) {
          for (i in 1:(k - 1L)) for (j in (i + 1L):k) {
            if (length(intersect(set_neighbors(parcel_graph, secs[[i]]),
                                 secs[[j]])) > 0) { ok <- FALSE; break }
          }
        }
        if (!ok) next

        plan_parcels <- c(lcc, sec_union)
        if (anyDuplicated(plan_parcels)) next                # defensive: no parcel reused
        cap   <- sum(caps[plan_parcels])
        area  <- sum(areas[plan_parcels])
        lcap  <- sum(caps[lcc])
        if (cap  < constraints$min_capacity)                 next
        if (area < constraints$min_area)                     next
        if (cap / area < constraints$min_density)            next
        if (lcap < constraints$min_lcc_fraction * cap)       next

        states[[length(states) + 1]] <- list(
          plan = paste(sort(plan_parcels), collapse = ","),
          k = k, cap = cap
        )
      }
    }
  }
  states
}

states3 <- enumerate_feasible_states(g3, libs3$lcc_library,
                                     libs3$secondary_library, constraints3)
cat(glue("Enumerated {length(states3)} feasible (LCC, secondaries) states.\n"))

# Log-weights under target: -lambda_cap * excess - lambda_k * k
log_w <- vapply(states3, function(s) {
  -CAPACITY_PRIOR_LAMBDA * max(0, s$cap - constraints3$min_capacity) -
    K_PRIOR_LAMBDA * s$k
}, numeric(1))
w     <- exp(log_w - max(log_w))  # numerically stable normalization
plans <- vapply(states3, \(s) s$plan, character(1))

expected_by_plan <- tapply(w, plans, sum)
expected_by_plan <- expected_by_plan / sum(expected_by_plan)

# Pool empirical across all 4 chains, keyed by sorted parcel-set string.
empirical_keys <- unlist(lapply(chain_results3, function(chain) {
  vapply(chain$parcel_samples, function(s) {
    if (is.null(s)) NA_character_ else paste(sort(s$X), collapse = ",")
  }, character(1))
}))
empirical_keys <- empirical_keys[!is.na(empirical_keys)]
empirical_by_plan <- prop.table(table(empirical_keys))

# Join into one comparison table, ordered by expected mass.
all_keys <- union(names(expected_by_plan), names(empirical_by_plan))
compare3 <- data.table(
  plan      = all_keys,
  expected  = as.numeric(expected_by_plan[all_keys]),
  empirical = as.numeric(empirical_by_plan[all_keys])
)
compare3[is.na(expected),  expected  := 0]
compare3[is.na(empirical), empirical := 0]
compare3 <- compare3[order(-expected)]

cat("\nTop plans by expected mass (pooled empirical from all 4 chains):\n")
print(head(compare3, 15))

# Diagnostic: total variation distance between empirical and expected.
tv3 <- 0.5 * sum(abs(compare3$expected - compare3$empirical))
cat(glue("\nTotal variation distance (empirical vs. expected): {round(tv3, 4)}\n"))
cat("  (0 = perfect agreement; <0.1 typical for well-mixed chains)\n")

compare_long3 <- melt(compare3, id.vars = "plan",
                      measure.vars = c("expected", "empirical"),
                      variable.name = "source", value.name = "prob")
compare_long3[, plan := factor(plan, levels = compare3$plan)]

compare_plot3 <- ggplot(compare_long3, aes(x = plan, y = prob, fill = source)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Scenario 3: Empirical vs. Expected Plan Distribution",
       subtitle = glue("Total variation = {round(tv3, 3)}; ",
                       "expected weighted by # of (LCC, sec) decompositions ",
                       "× target priors"),
       x = "Parcel plan (sorted parcel IDs)", y = "Probability",
       fill = NULL)
print(compare_plot3)

# ============================================================================
# SCENARIO 3 (cont.): Restricted comparison — 3-parcel plans only
# ============================================================================
# Renormalize both distributions to the 3-parcel feasible plans (the same set
# `feasible3` enumerates). This isolates the question "does the chain visit the
# 3-parcel plans in the right proportions?" from the orthogonal question of how
# much mass lives at 4- or 5-parcel plans.

cat("\n--- Scenario 3: restricted to 3-parcel plans ---\n")

feasible3_keys <- vapply(feasible3$feasible_plans,
                         \(p) paste(sort(p), collapse = ","), character(1))

compare3_k3 <- compare3[plan %in% feasible3_keys]
compare3_k3[, expected  := expected  / sum(expected)]
compare3_k3[, empirical := empirical / sum(empirical)]
compare3_k3 <- compare3_k3[order(-expected)]

tv3_k3 <- 0.5 * sum(abs(compare3_k3$expected - compare3_k3$empirical))
cat(glue("Total variation (3-parcel plans only): {round(tv3_k3, 4)}\n"))
print(compare3_k3)

compare_long3_k3 <- melt(compare3_k3, id.vars = "plan",
                         measure.vars = c("expected", "empirical"),
                         variable.name = "source", value.name = "prob")
compare_long3_k3[, plan := factor(plan, levels = compare3_k3$plan)]

compare_plot3_k3 <- ggplot(compare_long3_k3,
                           aes(x = plan, y = prob, fill = source)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Scenario 3: Empirical vs. Expected — 3-Parcel Plans Only",
       subtitle = glue("Total variation = {round(tv3_k3, 3)}; ",
                       "both distributions renormalized over the 9 k=3 plans"),
       x = "Parcel plan (sorted parcel IDs)", y = "Probability (conditional on k=3)",
       fill = NULL)
print(compare_plot3_k3)
