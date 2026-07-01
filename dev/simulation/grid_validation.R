# dev/simulation/grid_validation.R
#
# Validates the mbtazone MCMC sampler on a 4x4 grid graph.
#
# The mbtazone sampler's stationary distribution is NOT uniform over plans.
# Each state is an (LCC, {secondary blocks}) tuple drawn from the libraries,
# and the target density is:
#
#   pi(LCC, secs) ∝ exp(-λ_cap * excess - λ_k * k) / C(n_pool, k)
#
# where:
#   excess = max(0, total_capacity - min_capacity)
#   k      = number of secondary blocks
#   n_pool = total blocks in the secondary library
#   λ_cap  = CAPACITY_PRIOR_LAMBDA  (penalizes excess capacity)
#   λ_k    = K_PRIOR_LAMBDA         (geometric prior on k)
#
# The 1/C(n_pool, k) term is the reference measure correction from the
# birth/death kernel (see mcmc_parcel_mcmc_kernels.R lines 805-806).
# It cancels the combinatorial volume C(n_pool, k) implicit in the
# uniform proposal over k-subsets, so the marginal on k matches the
# intended geometric prior exp(-λ_k * k).
#
# A "plan" is the union of LCC and secondary parcels. Multiple (LCC, secs)
# decompositions can produce the same plan, so:
#
#   P(plan) ∝ Σ_{(LCC, secs) → plan} exp(-λ_cap * excess - λ_k * k) / C(n_pool, k)
#
# Plans with more valid decompositions get higher probability.
#
# Usage:
#   devtools::load_all()
#   source("dev/simulation/grid_validation.R")

library(data.table)
library(igraph)
library(purrr)
library(ggplot2)

devtools::load_all()
source("inst/targets/temp_targets_config.R")
source("inst/targets/temp_targets_parcel_config.R")

# ============================================================================
# 1. GRAPH: 4x4 GRID
# ============================================================================

create_scenario <- function(n, edges, capacity = 1L, area = 6.0,
                            centroid_x = NULL, centroid_y = NULL,
                            capacity_in_station = 0L, area_in_station = 0.0) {
  ids <- as.character(seq_len(n))
  g <- make_empty_graph(n = n, directed = FALSE)
  V(g)$name <- ids

  if (NROW(edges) > 0) {
    edge_vec <- as.character(t(edges[, 1:2]))
    g <- add_edges(g, edge_vec)
  }

  V(g)$capacity           <- rep_len(as.integer(capacity), n)
  V(g)$area               <- rep_len(as.double(area), n)
  V(g)$capacity_in_station <- rep_len(as.integer(capacity_in_station), n)
  V(g)$area_in_station     <- rep_len(as.double(area_in_station), n)

  if (is.null(centroid_x) || is.null(centroid_y)) {
    layout <- layout_with_fr(g)
    V(g)$centroid_x <- layout[, 1]
    V(g)$centroid_y <- layout[, 2]
  } else {
    V(g)$centroid_x <- rep_len(as.double(centroid_x), n)
    V(g)$centroid_y <- rep_len(as.double(centroid_y), n)
  }
  g
}

grid_edges <- function(nrow, ncol) {
  edges <- list()
  for (r in seq_len(nrow)) {
    for (c in seq_len(ncol)) {
      id <- (r - 1) * ncol + c
      if (c < ncol) edges <- c(edges, list(c(id, id + 1)))
      if (r < nrow) edges <- c(edges, list(c(id, id + ncol)))
    }
  }
  do.call(rbind, edges)
}

grid_coords <- function(nrow, ncol) {
  list(
    x = rep(seq_len(ncol), times = nrow) - 1,
    y = rep(seq(nrow, 1), each = ncol)
  )
}

scenario_grid <- function(nrow = 4, ncol = 4, capacity = 1L, area = 6.0,
                          random = FALSE, seed = 42L) {
  n <- nrow * ncol
  if (random) {
    set.seed(seed)
    capacity <- sample(1L:5L, n, replace = TRUE)
    area     <- round(runif(n, min = 3, max = 10), 1)
  }
  coords <- grid_coords(nrow, ncol)
  create_scenario(
    n = n, edges = grid_edges(nrow, ncol),
    capacity = capacity, area = area,
    centroid_x = coords$x, centroid_y = coords$y
  )
}

# uniform grid
#  1 — 2 — 3 — 4
#  |   |   |   |
#  5 — 6 — 7 — 8
#  |   |   |   |
#  9 —10 —11 —12
#  |   |   |   |
# 13 —14 —15 —16

# g <- scenario_grid(4, 4)

# bottlenecked island
#  1 — 2 — 3   4
#  |   |   |   |
#  5 — 6 — 7   8
#  |   |   |   |
#  9 —10 —11  12
#  |   |   |   |
# 13 —14 —15 —16

# e <- grid_edges(4, 4)
# e <- e[!(e[,1] == 3 & e[,2] == 4) & !(e[,1] == 7 & e[,2] == 8) & !(e[,1] == 11 & e[,2] == 12), ]  # cut 6-7
# g <- create_scenario(16, e, centroid_x = grid_coords(4,4)$x, centroid_y = grid_coords(4,4)$y)

# two cities
#  1 — 2   3   4
#  |   |   |   |
#  5 — 6   7   8
#  |   |   |   |
#  9 —10 —11  12
#  |   |   |   |
# 13 —14  15 —16

# e <- grid_edges(4, 4)
# e <- e[!(e[,1] == 2 & e[,2] == 3) & !(e[,1] == 6 & e[,2] == 7) & !(e[,1] == 14 & e[,2] == 15), ]  # cut 6-7
# g <- create_scenario(16, e, centroid_x = grid_coords(4,4)$x, centroid_y = grid_coords(4,4)$y)

# station at top-left — parcels 1,2,5,6 are within station radius
# plans must draw >=25% of min_capacity from station parcels
#
#  [1]—[2]— 3 — 4
#   |   |   |   |
#  [5]—[6]— 7 — 8
#   |   |   |   |
#   9 —10 —11 —12
#   |   |   |   |
#  13 —14 —15 —16

station_parcels <- c(1, 2, 5, 6)
g <- scenario_grid(4, 4)
V(g)$capacity_in_station[station_parcels] <- V(g)$capacity[station_parcels]
V(g)$area_in_station[station_parcels]     <- V(g)$area[station_parcels]


cat("Grid:", vcount(g), "parcels,", ecount(g), "edges\n")

# ============================================================================
# 2. CONSTRAINTS
# ============================================================================

constraints <- list(
  min_capacity         = 4,
  min_area             = 10,
  min_density          = 0.1,
  min_lcc_fraction     = 0.5,
  station_capacity_pct = NA_real_,  # set to e.g. 25 to activate station constraint
  station_area_pct     = NA_real_
)

# ============================================================================
# 3. LIBRARY BUILDING
# ============================================================================

build_grid_libraries <- function(g, constraints) {
  pnames <- V(g)$name
  neighbor_cache <- setNames(
    lapply(pnames, function(p) neighbors(g, p)$name), pnames
  )

  tree_lccs <- discover_lccs_from_trees(
    parcel_graph           = g,
    constraints            = constraints,
    n_trees                = 800L,
    max_discovery_capacity = constraints$min_capacity * DISCOVERY_CAPACITY_MULTIPLIER
  )
  bfs_lccs <- run_bfs_lcc_supplement(
    tree_discovered_lccs = tree_lccs,
    parcel_graph         = g,
    constraints          = constraints,
    n_samples            = BFS_LCC_N_SAMPLES,
    n_seeds              = min(BFS_LCC_N_SEEDS, 8L)
  )
  combined_lccs <- combine_all_lcc_discoveries(
    tree_discovered = tree_lccs,
    bfs_discovered  = bfs_lccs,
    bfs_stratified  = NULL
  )
  lcc_library <- build_lcc_library_from_tree_discovery(
    discovered_lccs = combined_lccs$discovered_blocks,
    parcel_graph    = g,
    constraints     = constraints
  )

  # density_threshold = 0: toy parcels have density 1/6 ≈ 0.17, far below
  # the production default of 15.
  tree_secs <- discover_secondaries_from_trees(
    parcel_graph      = g,
    size_bands        = list(c(5, 8), c(8, 12), c(12, 20)),
    density_threshold = 0,
    n_trees           = 400L
  )
  bfs_secs <- run_bfs_secondary_supplement(
    tree_discovered_secondaries = tree_secs,
    parcel_graph                = g,
    size_bands                  = list(c(5, 8), c(8, 12), c(12, 20)),
    quota_per_band              = 30L,
    density_threshold           = 0
  )
  combined_secs <- combine_discovered_blocks(
    tree_discovered = tree_secs,
    bfs_discovered  = bfs_secs,
    key_column      = "sec_key",
    block_type      = "secondary"
  )
  secondary_library <- build_secondary_library_from_discovery(
    combined_discovered = combined_secs,
    parcel_graph        = g
  )

  list(
    lcc_library       = lcc_library,
    secondary_library = secondary_library,
    neighbor_cache    = neighbor_cache
  )
}

cat("Building libraries...\n")
libs <- build_grid_libraries(g, constraints)
cat("LCC library:      ", libs$lcc_library$n_blocks, "blocks\n")
cat("Secondary library:", libs$secondary_library$n_blocks, "blocks (n_pool)\n")

# ============================================================================
# 4. STATE ENUMERATION
# ============================================================================
#
# Two enumerations:
#   4a. Library completeness: enumerate ALL connected subgraphs exhaustively,
#       compare to library contents. How many true blocks/plans did discovery find?
#   4b. Library-restricted: enumerate (LCC, {secondary subset}) tuples from the
#       libraries, compute target weights. This is the MCMC's actual state space.
#
# Weight formula:
#   log w = -λ_cap * max(0, cap - min_cap) - λ_k * k - log C(n_pool, k)
#
# Caps enumeration at k_max secondaries. With the 1/C(n_pool, k) factor,
# k >= 3 contributes negligible mass for any library of moderate size.

nbr_cache <- setNames(
  lapply(V(g)$name, function(p) neighbors(g, p)$name), V(g)$name
)
parcels_and_neighbors <- function(parcels) {
  c(parcels, unique(unlist(nbr_cache[parcels])))
}

enumerate_states <- function(g, lcc_lib, sec_lib, constraints, k_max = 3L) {
  pnames <- V(g)$name
  caps   <- setNames(V(g)$capacity, pnames)
  areas  <- setNames(V(g)$area, pnames)

  lcc_sets <- lapply(lcc_lib$blocks, \(idx) sort(pnames[idx]))
  sec_sets <- lapply(sec_lib$blocks, \(idx) sort(pnames[idx]))
  n_pool   <- sec_lib$n_blocks

  states <- vector("list", length(lcc_sets) * 4L)
  n_states <- 0L

  for (li in seq_along(lcc_sets)) {
    lcc <- lcc_sets[[li]]
    lcc_cap  <- sum(caps[lcc])
    lcc_area <- sum(areas[lcc])
    lcc_forbidden <- parcels_and_neighbors(lcc)

    # k = 0
    if (lcc_cap >= constraints$min_capacity &&
        lcc_area >= constraints$min_area &&
        lcc_cap / lcc_area >= constraints$min_density) {
      n_states <- n_states + 1L
      states[[n_states]] <- list(
        plan_key = paste(lcc, collapse = ","),
        k = 0L, cap = lcc_cap, n_pool = n_pool
      )
    }

    # Find compatible secondary blocks
    compat <- which(vapply(sec_sets, function(s) {
      if (any(s %in% lcc_forbidden)) return(FALSE)
      sec_nbrs <- unique(unlist(nbr_cache[s]))
      !any(sec_nbrs %in% lcc)
    }, logical(1)))

    if (length(compat) == 0) next

    for (k in seq_len(min(k_max, length(compat)))) {
      combos <- utils::combn(compat, k, simplify = FALSE)
      for (combo in combos) {
        secs      <- sec_sets[combo]
        sec_union <- unlist(secs)
        if (anyDuplicated(sec_union)) next

        # Pairwise non-adjacency between secondaries
        if (k > 1L) {
          ok <- TRUE
          for (i in 1:(k - 1L)) {
            i_nbrs <- unique(unlist(nbr_cache[secs[[i]]]))
            for (j in (i + 1L):k) {
              if (any(secs[[j]] %in% i_nbrs)) { ok <- FALSE; break }
            }
            if (!ok) break
          }
          if (!ok) next
        }

        plan <- sort(c(lcc, sec_union))
        if (anyDuplicated(plan)) next
        cap  <- sum(caps[plan])
        area <- sum(areas[plan])

        if (cap < constraints$min_capacity) next
        if (area < constraints$min_area) next
        if (cap / area < constraints$min_density) next
        if (lcc_cap < constraints$min_lcc_fraction * cap) next

        n_states <- n_states + 1L
        states[[n_states]] <- list(
          plan_key = paste(plan, collapse = ","),
          k = k, cap = cap, n_pool = n_pool
        )
      }
    }
  }

  states[seq_len(n_states)]
}

# ============================================================================
# 4a. LIBRARY COMPLETENESS CHECK (toy graph only)
# ============================================================================
#
# On a small graph, enumerate ALL connected subgraphs exhaustively and measure
# what fraction the library discovery found. This separates two questions:
#   (1) Does the MCMC sample correctly from its library-defined state space?
#   (2) Does the library cover the relevant part of the true state space?

cat("\n--- Library Completeness Check ---\n")
cat("Enumerating all connected subgraphs...\n")
t0 <- proc.time()

n_nodes <- vcount(g)
all_connected <- vector("list", 10000L)
n_connected <- 0L

for (mask in seq_len(2^n_nodes - 1L)) {
  bits <- which(bitwAnd(mask, 2L^(0:(n_nodes - 1L))) > 0L)
  nodes <- V(g)$name[bits]
  if (length(bits) == 1L || is_connected(induced_subgraph(g, nodes))) {
    n_connected <- n_connected + 1L
    all_connected[[n_connected]] <- list(
      parcels  = sort(nodes),
      indices  = sort(bits),
      capacity = sum(V(g)$capacity[bits]),
      area     = sum(V(g)$area[bits])
    )
  }
}
all_connected <- all_connected[seq_len(n_connected)]
cat("Found", n_connected, "connected subgraphs in",
    round((proc.time() - t0)[3], 1), "s\n")

# Classify as potential LCCs and secondaries
min_lcc_cap <- constraints$min_capacity * constraints$min_lcc_fraction
true_lccs <- Filter(function(sg) sg$capacity >= min_lcc_cap, all_connected)
true_secs <- Filter(function(sg) sg$area >= SECONDARY_AREA_THRESHOLD, all_connected)
cat("True LCC candidates:      ", length(true_lccs),
    "(capacity >=", min_lcc_cap, ")\n")
cat("True secondary candidates:", length(true_secs),
    "(area >=", SECONDARY_AREA_THRESHOLD, "acres)\n")

# Compare library contents to true sets
make_block_key <- function(parcels) paste(sort(parcels), collapse = ",")

true_lcc_keys <- vapply(true_lccs, function(sg) make_block_key(sg$parcels), character(1))
true_sec_keys <- vapply(true_secs, function(sg) make_block_key(sg$parcels), character(1))

lib_lcc_keys <- vapply(
  libs$lcc_library$blocks,
  function(idx) make_block_key(V(g)$name[idx]),
  character(1)
)
lib_sec_keys <- vapply(
  libs$secondary_library$blocks,
  function(idx) make_block_key(V(g)$name[idx]),
  character(1)
)

cat("\nLibrary recall (block level):\n")
cat("  LCC:       ", sum(true_lcc_keys %in% lib_lcc_keys), "/", length(true_lcc_keys),
    "(", round(100 * mean(true_lcc_keys %in% lib_lcc_keys), 1), "%)\n")
cat("  Secondary: ", sum(true_sec_keys %in% lib_sec_keys), "/", length(true_sec_keys),
    "(", round(100 * mean(true_sec_keys %in% lib_sec_keys), 1), "%)\n")

# Enumerate all feasible plans from true sets
true_lcc_lib <- list(
  blocks   = lapply(true_lccs, function(sg) sg$indices),
  n_blocks = length(true_lccs)
)
true_sec_lib <- list(
  blocks   = lapply(true_secs, function(sg) sg$indices),
  n_blocks = length(true_secs)
)

# cat("\nEnumerating plans from ALL true blocks (k_max = 3)...\n")
# t0 <- proc.time()
# true_states <- enumerate_states(g, true_lcc_lib, true_sec_lib, constraints)
# true_plan_keys <- unique(vapply(true_states, function(s) s$plan_key, character(1)))
# cat("Found", length(true_states), "true states,", length(true_plan_keys),
#     "distinct true plans in", round((proc.time() - t0)[3], 1), "s\n")

# ============================================================================
# 4b. LIBRARY-RESTRICTED STATE ENUMERATION + WEIGHT CALCULATION
# ============================================================================

cat("\nEnumerating feasible states from library (k_max = 3)...\n")
t0 <- proc.time()
states <- enumerate_states(g, libs$lcc_library, libs$secondary_library, constraints)
cat("Found", length(states), "feasible states in",
    round((proc.time() - t0)[3], 1), "s\n")

# Compute weights under the target distribution
state_dt <- rbindlist(lapply(states, function(s) {
  excess <- max(0, s$cap - constraints$min_capacity)
  log_w  <- -CAPACITY_PRIOR_LAMBDA * excess -
             K_PRIOR_LAMBDA * s$k -
             lchoose(s$n_pool, s$k)
  data.table(plan_key = s$plan_key, k = s$k, cap = s$cap, log_w = log_w)
}))
state_dt[, w := exp(log_w - max(log_w))]

# Weight breakdown by k
cat("\nWeight mass by k:\n")
k_mass <- state_dt[, .(total_w = sum(w), n_states = .N), by = k][order(k)]
k_mass[, frac := total_w / sum(total_w)]
print(k_mass)

# Aggregate to plan-level expected distribution
plan_expected <- state_dt[, .(
  weight           = sum(w),
  n_decompositions = .N,
  cap              = cap[1]
), by = plan_key]
plan_expected[, prob := weight / sum(weight)]
plan_expected <- plan_expected[order(-prob)]

cat("\n", nrow(plan_expected), "distinct library plans\n")

# Plan-level library coverage (compared to true enumeration)
# lib_plan_coverage <- mean(true_plan_keys %in% plan_expected$plan_key)
# cat("Library plan coverage:", sum(true_plan_keys %in% plan_expected$plan_key), "/",
#     length(true_plan_keys), "true plans (",
#     round(100 * lib_plan_coverage, 1), "%)\n\n")

# ============================================================================
# 5. MCMC EXECUTION
# ============================================================================

n_chains <- 4L
n_steps  <- 10000L

init_states <- generate_initial_states_from_lccs(
  lcc_library  = libs$lcc_library,
  libraries    = list(
    secondary_library = libs$secondary_library,
    lcc_library       = libs$lcc_library
  ),
  parcel_graph = g,
  constraints  = constraints,
  n_chains     = n_chains
)

# Disable LCC-local kernel: it creates non-library LCC configurations that
# the enumeration doesn't cover. With p_lcc_local = 0, LCCs only change via
# replace-LCC (library-based), so the enumerated state space is exact.
config <- define_parcel_kernel_configs(n_steps = n_steps)$default
config$p_lcc_local <- 0
config$p_replace_lcc <- config$p_replace_lcc + 0.12

cat("\nRunning", n_chains, "chains x", n_steps, "steps...\n")
chain_results <- lapply(seq_along(init_states), function(i) {
  cat("  Chain", i, "\n")
  run_parcel_mcmc(
    parcel_graph             = g,
    initial_state            = init_states[[i]],
    constraints              = constraints,
    secondary_library        = libs$secondary_library,
    lcc_library              = libs$lcc_library,
    config                   = config,
    parcel_assignments       = NULL,
    neighbor_cache           = libs$neighbor_cache,
    enable_online_enrichment = FALSE,
    max_stored_samples       = 50000L,
    verbose                  = FALSE
  )
})

# ============================================================================
# 6. COMPARISON
# ============================================================================

# Empirical plan frequencies (pooled across all chains)
empirical_keys <- unlist(lapply(chain_results, function(ch) {
  vapply(ch$parcel_samples, function(s) {
    if (is.null(s)) NA_character_ else paste(sort(s$X), collapse = ",")
  }, character(1))
}))
empirical_keys <- empirical_keys[!is.na(empirical_keys)]
empirical_tab  <- prop.table(table(empirical_keys))

empirical_caps <- unlist(lapply(chain_results, function(ch) {
  vapply(ch$parcel_samples, function(s) {
    if (is.null(s)) NA_real_ else s$total_capacity
  }, numeric(1))
}))
empirical_caps <- empirical_caps[!is.na(empirical_caps)]

# Join expected and empirical
all_keys <- union(plan_expected$plan_key, names(empirical_tab))
compare <- data.table(
  plan     = all_keys,
  expected = plan_expected[match(all_keys, plan_key), prob],
  observed = as.numeric(empirical_tab[all_keys])
)
compare[is.na(expected), expected := 0]
compare[is.na(observed), observed := 0]
compare <- compare[order(-expected)]

n_samples <- length(empirical_keys)
tv <- 0.5 * sum(abs(compare$expected - compare$observed))

# Chi-squared goodness-of-fit: accounts for sampling variance.
# Only include plans with expected count >= 5 (standard chi-sq assumption).
compare[, expected_count := expected * n_samples]
compare[, observed_count := observed * n_samples]
chisq_dt <- compare[expected_count >= 5]
chisq_stat <- sum((chisq_dt$observed_count - chisq_dt$expected_count)^2 /
                    chisq_dt$expected_count)
chisq_df <- nrow(chisq_dt) - 1L
chisq_p  <- pchisq(chisq_stat, df = chisq_df, lower.tail = FALSE)

cat("\n=== RESULTS ===\n")
cat("Total MCMC samples:            ", n_samples, "\n")
cat("Plans in expected distribution:", nrow(plan_expected), "\n")
cat("Plans visited by MCMC:         ", sum(compare$observed > 0), "\n")
p_vec_all <- plan_expected$prob
iid_noise_tv <- sqrt(1 / (2 * pi)) * sum(sqrt(p_vec_all * (1 - p_vec_all))) / sqrt(n_samples)
cat("Total variation distance:      ", round(tv, 4), "\n")
cat("  (expected from iid noise:   ~", round(iid_noise_tv, 4), ")\n")
cat("  TV / noise floor:            ", round(tv / iid_noise_tv, 2), "x\n")
cat("\nChi-squared goodness-of-fit (plans with expected count >= 5):\n")
cat("  Plans tested:", nrow(chisq_dt), "\n")
cat("  Chi-sq stat: ", round(chisq_stat, 1), " (df =", chisq_df, ")\n")
cat("  p-value:     ", format.pval(chisq_p, digits = 3), "\n")
cat("  (p > 0.05 = cannot reject that MCMC matches expected distribution)\n\n")

cat("Top 20 plans:\n")
print(head(compare[, .(plan, expected, observed, expected_count, observed_count)], 20))

# Acceptance rates (chain 1)
cat("\nAcceptance rates (chain 1):\n")
print(chain_results[[1]]$stats)

# Gelman-Rubin R-hat
rhat <- compute_parcel_multichain_rhat(chain_results)
cat("\nGelman-Rubin R-hat (< 1.1 = converged):\n")
print(rhat)

# KS test on plan capacity (summary observable)
cap_probs <- plan_expected[, .(prob = sum(prob)), by = cap][order(cap)]
cap_probs[, cdf := cumsum(prob)]
expected_cap_cdf <- stepfun(cap_probs$cap, c(0, cap_probs$cdf))
ks_result <- ks.test(empirical_caps, expected_cap_cdf)
cat("\nKolmogorov-Smirnov test on plan capacity:\n")
cat("  D statistic:", round(ks_result$statistic, 4), "\n")
cat("  p-value:    ", format.pval(ks_result$p.value, digits = 3), "\n")
cat("  (conservative for discrete distributions)\n")

# Effective Sample Size from autocorrelation (per-chain on capacity trace)
compute_ess <- function(x) {
  n <- length(x)
  if (n < 10) return(NA_real_)
  max_lag <- min(n - 1L, as.integer(10 * log10(n)))
  acf_vals <- acf(x, lag.max = max_lag, plot = FALSE)$acf[-1]
  first_neg <- which(acf_vals < 0)[1]
  if (!is.na(first_neg) && first_neg > 1) acf_vals <- acf_vals[seq_len(first_neg - 1)]
  n / (1 + 2 * sum(pmax(acf_vals, 0)))
}
chain_caps <- lapply(chain_results, function(ch) {
  caps <- vapply(ch$parcel_samples, function(s) {
    if (is.null(s)) NA_real_ else s$total_capacity
  }, numeric(1))
  caps[!is.na(caps)]
})
chain_ess <- vapply(chain_caps, compute_ess, numeric(1))
cat("\nEffective Sample Size (capacity):\n")
cat("  Per chain:", paste(round(chain_ess), collapse = ", "), "\n")
cat("  Total ESS:", round(sum(chain_ess)), "\n")
cat("  ESS / N:  ", round(sum(chain_ess) / n_samples, 3), "\n")

# Coverage: fraction of enumerated plans visited by MCMC
n_visited <- sum(compare$observed > 0)
n_enumerated <- nrow(plan_expected)
cat("\nCoverage:\n")
cat("  Plans visited / enumerated:", n_visited, "/", n_enumerated,
    "(", round(100 * n_visited / n_enumerated, 1), "%)\n")

# ============================================================================
# 7. VISUALIZATION
# ============================================================================

# --- Plot 1: Capacity density overlay (canonical redistricting validation figure) ---
# Overlays the expected distribution of plan capacity (from enumeration weights)
# with the empirical distribution from MCMC. If the sampler is correct, the two
# should match. This is a 1D projection — convergence on observables happens
# orders of magnitude faster than on the full distribution (Autry et al. 2023).
set.seed(123)
expected_caps_sample <- sample(
  plan_expected$cap, size = n_samples, replace = TRUE, prob = plan_expected$prob
)
cap_plot_dt <- data.table(
  cap    = c(empirical_caps, expected_caps_sample),
  source = rep(c("MCMC (observed)", "Target (expected)"), each = n_samples)
)

p_density <- ggplot(cap_plot_dt, aes(x = cap, fill = source, color = source)) +
  geom_density(alpha = 0.3, adjust = 1.5) +
  labs(
    title = "Plan Capacity: MCMC vs Target Distribution",
    subtitle = paste0("KS D = ", round(ks_result$statistic, 4),
                      ", p = ", format.pval(ks_result$p.value, digits = 2),
                      " | N = ", format(n_samples, big.mark = ",")),
    x = "Total plan capacity", y = "Density", fill = NULL, color = NULL
  )
print(p_density)

# --- Plot 2: TV convergence curve ---
tv_checkpoints <- seq(1000L, n_samples, by = 1000L)
tv_curve <- vapply(tv_checkpoints, function(n) {
  sub_tab <- prop.table(table(empirical_keys[seq_len(n)]))
  sub_compare <- data.table(
    plan     = union(plan_expected$plan_key, names(sub_tab)),
    expected = plan_expected[match(union(plan_expected$plan_key, names(sub_tab)),
                                   plan_key), prob],
    observed = as.numeric(sub_tab[union(plan_expected$plan_key, names(sub_tab))])
  )
  sub_compare[is.na(expected), expected := 0]
  sub_compare[is.na(observed), observed := 0]
  0.5 * sum(abs(sub_compare$expected - sub_compare$observed))
}, numeric(1))

# Distribution-aware noise floor: E[TV] ≈ (1/√N) × √(1/2π) × Σ √(pᵢ(1-pᵢ))
# The naive sqrt(M/2N) assumes uniform distribution over M plans, which vastly
# overestimates when most plans have negligible probability.
p_vec <- plan_expected$prob
noise_scale <- sqrt(1 / (2 * pi)) * sum(sqrt(p_vec * (1 - p_vec)))
noise_floor <- noise_scale / sqrt(tv_checkpoints)

tv_curve_dt <- data.table(
  n = tv_checkpoints, tv = tv_curve, noise_floor = noise_floor
)

p_convergence <- ggplot(tv_curve_dt, aes(x = n)) +
  geom_line(aes(y = tv), linewidth = 0.8) +
  geom_line(aes(y = noise_floor), linetype = "dashed", color = "blue", linewidth = 0.6) +
  labs(
    title = "TV Distance vs Sample Size",
    subtitle = "Dashed blue = expected TV from iid sampling noise",
    x = "Samples", y = "Total variation distance"
  )
print(p_convergence)

# --- Plot 3: Standardized residuals ---
# (observed - expected) / sqrt(expected / N), should look ~N(0,1) for correct sampler.
# Chi-sq is anti-conservative here due to MCMC autocorrelation, but the residual
# SHAPE (symmetry, no heavy tails) is diagnostic even with correlated samples.
resid_dt <- compare[expected_count >= 5]
resid_dt[, std_resid := (observed_count - expected_count) / sqrt(expected_count)]

p_resid_hist <- ggplot(resid_dt, aes(x = std_resid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "grey70", color = "black") +
  stat_function(fun = dnorm, color = "red", linewidth = 0.8) +
  labs(
    title = "Standardized Residuals (plans with E >= 5)",
    subtitle = paste0("Mean = ", round(mean(resid_dt$std_resid), 3),
                      ", SD = ", round(sd(resid_dt$std_resid), 3),
                      " (expect ~N(0,1) if iid; wider SD from autocorrelation)"),
    x = "(Observed - Expected) / sqrt(Expected)", y = "Density"
  )
print(p_resid_hist)

# --- Plot 4: Observed/Expected ratio vs plan rank ---
# For plans with enough expected mass. Points should scatter around 1.0.
ratio_dt <- compare[expected_count >= 5][order(-expected)]
ratio_dt[, rank := .I]
ratio_dt[, ratio := observed / expected]

# Approximate 95% CI assuming iid (will be slightly too narrow due to autocorrelation)
ratio_dt[, ci_lo := 1 - 1.96 / sqrt(expected_count)]
ratio_dt[, ci_hi := 1 + 1.96 / sqrt(expected_count)]

p_ratio <- ggplot(ratio_dt, aes(x = rank, y = ratio)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "blue") +
  geom_point(alpha = 0.3, size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_cartesian(ylim = c(0, 2.5)) +
  labs(
    title = "Observed / Expected Ratio by Plan Rank",
    subtitle = paste0(sum(ratio_dt$ratio > ratio_dt$ci_lo & ratio_dt$ratio < ratio_dt$ci_hi),
                      " / ", nrow(ratio_dt),
                      " plans within 95% CI (blue band)"),
    x = "Plan rank (by expected probability)", y = "Observed / Expected"
  )
print(p_ratio)

# --- Plot 5: Log-log scatter with confidence bands ---
# The primary validation plot. Each point is a plan; diagonal = perfect match.
# Grey band = 95% pointwise CI from binomial sampling noise (iid assumption,
# so band is slightly too narrow due to MCMC autocorrelation).
scatter_dt <- compare[expected > 0 & observed > 0]
scatter_dt[, `:=`(
  ci_lo = qbinom(0.025, size = n_samples, prob = expected) / n_samples,
  ci_hi = qbinom(0.975, size = n_samples, prob = expected) / n_samples
)]
pct_in_ci <- round(100 * mean(scatter_dt$observed >= scatter_dt$ci_lo &
                                scatter_dt$observed <= scatter_dt$ci_hi), 1)

p_loglog <- ggplot(scatter_dt, aes(x = expected, y = observed)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, fill = "blue") +
  geom_point(alpha = 0.3, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "MCMC Sampling Distribution vs Target (log scale)",
    subtitle = paste0("TV = ", round(tv, 3),
                      " | ", pct_in_ci, "% of plans within 95% CI",
                      " | N = ", format(n_samples, big.mark = ",")),
    x = "Expected probability", y = "Observed probability"
  )
print(p_loglog)
