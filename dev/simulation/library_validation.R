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

g <- scenario_grid(4, 4)

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

# station_parcels <- c(1, 2, 5, 6)
# g <- scenario_grid(4, 4)
# V(g)$capacity_in_station[station_parcels] <- V(g)$capacity[station_parcels]
# V(g)$area_in_station[station_parcels]     <- V(g)$area[station_parcels]


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
# 4a. HOW WELL DOES THE LCC SEARCH DO?
# ============================================================================
#
# Brute-force enumerate ALL connected subgraphs, then ask what fraction of the
# true LCCs the search (tree + BFS) actually found.
#
# Two things make a naive recall number misleading, and both are fixed below.
#
# (1) THE DENOMINATOR. Scoring against every connected subgraph with
#     capacity >= min_capacity * min_lcc_fraction counts blocks that the
#     pipeline excludes ON PURPOSE, so the number is floored by design
#     decisions rather than search failure. Two exclusions matter:
#
#       a. The capacity ceiling. discover_lccs_from_trees() is called with
#          max_discovery_capacity = min_capacity * DISCOVERY_CAPACITY_MULTIPLIER,
#          and build_lcc_library_from_tree_discovery() re-applies the same
#          2.5x cap (mcmc_parcel_library.R:1629). Anything above it was never
#          a target.
#       b. Viability. An LCC that cannot form a single feasible plan — even
#          with the best available secondaries — is not part of the posterior,
#          so missing it costs nothing.
#
#     We report recall against all three denominators so the effect of each
#     exclusion is visible rather than baked in.
#
# (2) THE WEIGHTING. Unweighted recall treats a missed LCC carrying 1e-9 of
#     posterior mass the same as a missed mode. What actually matters is the
#     share of target mass the library covers. For each true LCC we sum the
#     target weight of every feasible state built on it:
#
#       weight(LCC) = sum over k, over compatible secondary subsets S of
#                     exp(-lambda_cap * excess - lambda_k * k) / C(n_pool, k)
#
#     Secondaries are drawn from the actual secondary library, because that is
#     what the sampler has available. Weighted recall is then the fraction of
#     total target mass sitting on LCCs the library contains — the number that
#     answers "is this library good enough".
#
# CAVEAT ON TOY SCALE: CAPACITY_PRIOR_LAMBDA is calibrated for real
# municipalities where capacity runs to the thousands. Here total grid capacity
# is 16, so excess is single-digit and exp(-0.002 * excess) ~ 0.99 — the prior
# is very nearly inert. "Posterior mass" on this graph is therefore dominated by
# the NUMBER of feasible states a block participates in, not by any preference
# for low capacity. Read the mass columns as combinatorial weight. To exercise
# the prior, raise CAPACITY_PRIOR_LAMBDA to ~0.2 for toy runs.

cat("\n--- LCC Search Quality ---\n")
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

make_block_key <- function(parcels) paste(sort(parcels), collapse = ",")

# ----------------------------------------------------------------------------
# 4a.1 THE THREE DENOMINATORS
# ----------------------------------------------------------------------------

min_lcc_cap <- constraints$min_capacity * constraints$min_lcc_fraction
max_discovery_capacity <- constraints$min_capacity * DISCOVERY_CAPACITY_MULTIPLIER

# D1: every connected subgraph that clears the LCC capacity floor. This is the
#     naive denominator — it includes blocks the pipeline never targets.
cand_lccs <- Filter(function(sg) sg$capacity >= min_lcc_cap, all_connected)

# D2: D1 minus blocks above the discovery/library capacity ceiling.
bounded_lccs <- Filter(
  function(sg) sg$capacity <= max_discovery_capacity, cand_lccs
)

n_over_cap <- length(cand_lccs) - length(bounded_lccs)
cat("\nGround truth:\n")
cat("  D1 capacity >=", min_lcc_cap, ":                ",
    length(cand_lccs), "\n")
cat("  D2 also <=", max_discovery_capacity,
    "(discovery ceiling):", length(bounded_lccs),
    sprintf("  [%d excluded by design, %.1f%% of D1]\n",
            n_over_cap, 100 * n_over_cap / max(length(cand_lccs), 1L)))

# ----------------------------------------------------------------------------
# 4a.2 POSTERIOR MASS PER LCC
# ----------------------------------------------------------------------------
#
# For each candidate LCC, sum the target weight over every feasible state built
# on it, using the real secondary library as the pool. LCCs that admit no
# feasible state get weight 0 and define D3 (the viable set).

#' Total unnormalised target weight for each LCC
#'
#' @param lcc_list List of ground-truth blocks (each with $parcels, $capacity,
#'   $area), as produced by the connected-subgraph enumeration.
#' @return data.table(lcc_key, capacity, n_states, log_w_max, weight)
lcc_target_weights <- function(g, lcc_list, sec_lib, constraints, nbr_cache,
                               k_max = 3L, verbose = TRUE) {
  pnames <- V(g)$name
  caps <- setNames(V(g)$capacity, pnames)
  areas <- setNames(V(g)$area, pnames)

  sec_sets <- lapply(sec_lib$blocks, \(idx) sort(sec_lib$parcel_names[idx]))
  n_pool <- sec_lib$n_blocks

  # Precompute each secondary's parcel set and neighbour set once. The original
  # enumerate_states() recomputed these inside the LCC loop, which is O(n_lcc *
  # n_sec) redundant work — prohibitive with thousands of ground-truth LCCs.
  sec_nbrs <- lapply(sec_sets, \(s) unique(unlist(nbr_cache[s])))
  sec_caps <- vapply(sec_sets, \(s) sum(caps[s]), numeric(1))
  sec_areas <- vapply(sec_sets, \(s) sum(areas[s]), numeric(1))

  lam_cap <- CAPACITY_PRIOR_LAMBDA
  lam_k <- K_PRIOR_LAMBDA
  min_cap <- constraints$min_capacity
  min_area <- constraints$min_area
  min_dens <- constraints$min_density
  theta <- constraints$min_lcc_fraction

  n_lcc <- length(lcc_list)
  out_key <- character(n_lcc)
  out_cap <- numeric(n_lcc)
  out_n <- integer(n_lcc)
  out_lw <- rep(-Inf, n_lcc) # running log-sum-exp of state weights

  for (li in seq_len(n_lcc)) {
    if (verbose && li %% 2000L == 0L) {
      cat("    ", li, "/", n_lcc, "\n")
      flush.console()
    }
    sg <- lcc_list[[li]]
    lcc <- sg$parcels
    lcc_cap <- sg$capacity
    lcc_area <- sg$area
    out_key[li] <- make_block_key(lcc)
    out_cap[li] <- lcc_cap

    lcc_forbidden <- c(lcc, unique(unlist(nbr_cache[lcc])))
    log_ws <- numeric(0)

    # k = 0: the LCC standing alone
    if (lcc_cap >= min_cap && lcc_area >= min_area &&
      lcc_cap / lcc_area >= min_dens) {
      log_ws <- c(log_ws, -lam_cap * max(0, lcc_cap - min_cap))
    }

    # Secondaries that neither overlap nor touch the LCC
    compat <- which(vapply(seq_along(sec_sets), function(j) {
      !any(sec_sets[[j]] %in% lcc_forbidden) && !any(sec_nbrs[[j]] %in% lcc)
    }, logical(1)))

    if (length(compat) > 0) {
      for (k in seq_len(min(k_max, length(compat)))) {
        combos <- utils::combn(compat, k, simplify = FALSE)
        for (combo in combos) {
          # Secondaries must be mutually disjoint and non-adjacent
          if (k > 1L) {
            ok <- TRUE
            for (a in 1:(k - 1L)) {
              for (b in (a + 1L):k) {
                if (any(sec_sets[[combo[b]]] %in% sec_sets[[combo[a]]]) ||
                  any(sec_sets[[combo[b]]] %in% sec_nbrs[[combo[a]]])) {
                  ok <- FALSE
                  break
                }
              }
              if (!ok) break
            }
            if (!ok) next
          }

          cap <- lcc_cap + sum(sec_caps[combo])
          area <- lcc_area + sum(sec_areas[combo])
          if (cap < min_cap) next
          if (area < min_area) next
          if (cap / area < min_dens) next
          if (lcc_cap < theta * cap) next

          log_ws <- c(
            log_ws,
            -lam_cap * max(0, cap - min_cap) - lam_k * k - lchoose(n_pool, k)
          )
        }
      }
    }

    out_n[li] <- length(log_ws)
    if (length(log_ws) > 0) {
      m <- max(log_ws)
      out_lw[li] <- m + log(sum(exp(log_ws - m)))
    }
  }

  dt <- data.table(
    lcc_key = out_key, capacity = out_cap,
    n_states = out_n, log_w = out_lw
  )
  # Normalise on the log scale, then exponentiate, so tiny weights survive
  dt[, weight := 0]
  if (any(is.finite(dt$log_w))) {
    mx <- max(dt$log_w[is.finite(dt$log_w)])
    dt[is.finite(log_w), weight := exp(log_w - mx)]
  }
  dt[]
}

cat("\nComputing posterior mass for", length(cand_lccs), "candidate LCCs...\n")
t0 <- proc.time()
lcc_w <- lcc_target_weights(
  g, cand_lccs, libs$secondary_library, constraints, nbr_cache
)
cat("Done in", round((proc.time() - t0)[3], 1), "s\n")

lcc_w[, in_bound := capacity <= max_discovery_capacity]
lcc_w[, viable := n_states > 0]

# D3: viable AND within the capacity ceiling — the LCCs that actually carry
#     posterior mass and that the pipeline is trying to find.
cat("  D3 also viable (>=1 feasible plan):  ",
    lcc_w[in_bound & viable, .N], "\n")

# ----------------------------------------------------------------------------
# 4a.3 RECALL
# ----------------------------------------------------------------------------

lib_lcc_keys <- vapply(
  libs$lcc_library$blocks,
  function(idx) make_block_key(libs$lcc_library$parcel_names[idx]),
  character(1)
)
lcc_w[, found := lcc_key %in% lib_lcc_keys]

recall_tbl <- rbindlist(list(
  lcc_w[, .(
    denominator = "D1  capacity floor only (naive)",
    n = .N, found = sum(found)
  )],
  lcc_w[in_bound == TRUE, .(
    denominator = "D2  + within discovery ceiling",
    n = .N, found = sum(found)
  )],
  lcc_w[in_bound == TRUE & viable == TRUE, .(
    denominator = "D3  + viable (carries mass)",
    n = .N, found = sum(found)
  )]
))
recall_tbl[, recall_pct := round(100 * found / n, 1)]

cat("\n=== UNWEIGHTED RECALL (every LCC counts equally) ===\n")
print(recall_tbl)

# Weighted recall must use the SAME denominators as the unweighted table,
# otherwise mass sitting above the discovery ceiling is scored as a miss —
# reintroducing exactly the bias the denominators were built to remove.
mass_recall <- function(subset_dt) {
  tot <- subset_dt[, sum(weight)]
  if (tot <= 0) return(NA_real_)
  subset_dt[found == TRUE, sum(weight)] / tot
}

weighted_tbl <- data.table(
  denominator = recall_tbl$denominator,
  mass_recall_pct = round(100 * c(
    mass_recall(lcc_w),
    mass_recall(lcc_w[in_bound == TRUE]),
    mass_recall(lcc_w[in_bound == TRUE & viable == TRUE])
  ), 2)
)
weighted_tbl[, unweighted_pct := recall_tbl$recall_pct]

cat("\n=== WEIGHTED RECALL (share of target mass covered) ===\n")
print(weighted_tbl)

mass_over_ceiling <- lcc_w[in_bound == FALSE, sum(weight)] / lcc_w[, sum(weight)]
cat("\n  Target mass above the discovery ceiling (excluded by design):",
    sprintf("%.1f%%\n", 100 * mass_over_ceiling))

# Headline number: mass covered within the set the pipeline actually targets.
weighted_recall <- mass_recall(lcc_w[in_bound == TRUE & viable == TRUE])
total_mass <- lcc_w[in_bound == TRUE & viable == TRUE, sum(weight)]

# ----------------------------------------------------------------------------
# 4a.4 WHAT WAS MISSED, AND DOES IT MATTER?
# ----------------------------------------------------------------------------

# Restricted to the targeted set: LCCs above the ceiling are missing by design,
# so listing them as "misses" would be noise.
missed <- lcc_w[found == FALSE & viable == TRUE & in_bound == TRUE][order(-weight)]
missed[, mass_share := weight / total_mass]

cat("\nTop 15 missed LCCs by posterior mass:\n")
if (nrow(missed) > 0) {
  print(head(missed[, .(
    lcc_key, capacity, n_states,
    mass_share = sprintf("%.3e", mass_share)
  )], 15))
  cat("\n  Missed LCCs:", nrow(missed), "| their combined mass share:",
      sprintf("%.4f%%", 100 * missed[, sum(mass_share)]), "\n")
  cat("  Largest single miss:",
      sprintf("%.4f%%", 100 * missed[1, mass_share]), "of total mass\n")
} else {
  cat("  None — every viable LCC is in the library.\n")
}

# Where in the capacity range does the search struggle?
# mass_share is normalised over ALL viable LCCs (including those above the
# ceiling) so the design exclusion stays visible as its own band; mass_found is
# within-row and so unaffected by the choice of total.
total_mass_all <- lcc_w[viable == TRUE, sum(weight)]
cap_recall <- lcc_w[viable == TRUE, .(
  n = .N,
  found = sum(found),
  recall_pct = round(100 * mean(found), 1),
  mass_share = sum(weight) / total_mass_all,
  mass_found = sum(weight[found]) / max(sum(weight), .Machine$double.eps)
), by = .(capacity, in_bound)][order(capacity)]

cat("\nRecall by LCC capacity (viable LCCs only):\n")
print(cap_recall[, .(
  capacity, in_bound, n, found, recall_pct,
  mass_share = sprintf("%.3f", mass_share),
  mass_recall = sprintf("%.3f", mass_found)
)])

# ----------------------------------------------------------------------------
# 4a.5 PLOTS
# ----------------------------------------------------------------------------

plot_dt <- cap_recall[in_bound == TRUE]

# Count-based recall against mass-weighted recall, per capacity level. Where
# the two diverge, block counts are a poor proxy for what the sampler needs.
p_recall <- ggplot(plot_dt, aes(x = capacity)) +
  geom_col(aes(y = mass_share), fill = "grey80") +
  geom_line(aes(y = recall_pct / 100, color = "Recall (count)"), linewidth = 0.9) +
  geom_point(aes(y = recall_pct / 100, color = "Recall (count)")) +
  geom_line(aes(y = mass_found, color = "Recall (mass)"), linewidth = 0.9) +
  geom_point(aes(y = mass_found, color = "Recall (mass)")) +
  scale_y_continuous(
    "Recall", limits = c(0, 1),
    sec.axis = sec_axis(~., name = "Share of target mass (bars)")
  ) +
  scale_color_manual(values = c(
    "Recall (count)" = "steelblue", "Recall (mass)" = "firebrick"
  )) +
  labs(
    title = "LCC Search Recall by Capacity",
    subtitle = paste0(
      "Grey bars = where the posterior mass actually is | weighted recall = ",
      sprintf("%.2f%%", 100 * weighted_recall)
    ),
    x = "LCC capacity", color = NULL
  )
print(p_recall)

# Cumulative mass covered, walking LCCs from heaviest to lightest. A curve that
# saturates immediately means the library holds everything that matters.
cum_dt <- lcc_w[viable == TRUE][order(-weight)]
cum_dt[, `:=`(
  rank = .I,
  cum_mass = cumsum(weight) / total_mass,
  cum_mass_found = cumsum(weight * found) / total_mass
)]

p_cum <- ggplot(cum_dt, aes(x = rank)) +
  geom_line(aes(y = cum_mass, color = "All viable LCCs"), linewidth = 0.9) +
  geom_line(aes(y = cum_mass_found, color = "In library"), linewidth = 0.9) +
  scale_color_manual(values = c(
    "All viable LCCs" = "grey40", "In library" = "firebrick"
  )) +
  labs(
    title = "Cumulative Target Mass Covered by the LCC Library",
    subtitle = "Gap between the curves is mass the sampler can never reach",
    x = "LCC rank (by posterior mass)", y = "Cumulative share of mass",
    color = NULL
  )
print(p_cum)

cat("\n--- Secondary library (reference) ---\n")
true_secs <- Filter(
  function(sg) sg$area >= SECONDARY_AREA_THRESHOLD, all_connected
)
true_sec_keys <- vapply(true_secs, function(sg) make_block_key(sg$parcels), character(1))
lib_sec_keys <- vapply(
  libs$secondary_library$blocks,
  function(idx) make_block_key(libs$secondary_library$parcel_names[idx]),
  character(1)
)
cat("  Secondary recall (unweighted):",
    sum(true_sec_keys %in% lib_sec_keys), "/", length(true_sec_keys),
    sprintf("(%.1f%%)\n", 100 * mean(true_sec_keys %in% lib_sec_keys)))