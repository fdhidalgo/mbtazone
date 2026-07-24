# dev/simulation/sweep_validation.R
#
# Sweeps the mbtazone MCMC sampler across two axes on toy 4x4 grids:
#
#   1. min_capacity      — how binding the hard capacity constraint is
#   2. disconnectedness  — 5 grids with progressively more edge cuts,
#                          ending at a fully severed graph
#
# For each (grid, min_capacity) cell we run 4 chains x 5000 steps and compute
# the total variation distance between the MCMC's empirical plan distribution
# and the exact enumerated target — using the same TV definition as
# grid_validation.R (0.5 * sum |expected - observed| over the union of plans).
#
# WHY p_lcc_local = 0
# -------------------
# The lcc_local kernel edits the LCC at the parcel level, so it can occupy LCC
# configurations that were never in the library. enumerate_states() only ranges
# over library LCCs, so those states would carry expected probability 0 and
# inflate TV for reasons unrelated to sampler correctness. grid_validation.R
# disables the kernel for the same reason. With p_lcc_local = 0 the LCC only
# changes via replace_lcc (library-based) and the enumerated state space is
# exact.
#
# Online enrichment must also stay off: it grows the library mid-run, making
# the kernel time-inhomogeneous with no fixed target to compare against.
#
# Usage:
#   devtools::load_all()
#   source("dev/simulation/sweep_validation.R")
#
# Results are written to dev/simulation/output/sweep_results.{rds,csv}.

library(data.table)
library(igraph)
library(purrr)
library(ggplot2)

devtools::load_all()
source("inst/targets/temp_targets_config.R")
source("inst/targets/temp_targets_parcel_config.R")

# ============================================================================
# 0. SWEEP CONFIGURATION
# ============================================================================

N_CHAINS <- 4L
N_STEPS <- 2500L

# Uniform parcels (capacity = 1, area = 6), matching grid_validation.R's
# current setting. Total grid capacity = 16; each half of the severed grid
# holds 8, so min_capacity up to 7 stays feasible on every grid.
MIN_CAPACITY_VALUES <- c(3, 4, 5, 6, 7)

# Base constraints; min_capacity is overridden per cell.
BASE_CONSTRAINTS <- list(
  min_capacity = NA_real_,
  min_area = 10,
  min_density = 0.1,
  min_lcc_fraction = 0.5,
  station_capacity_pct = NA_real_,
  station_area_pct = NA_real_
)

# Discard the first BURN_IN stored samples per chain before comparing.
# Set to 0 to reproduce grid_validation.R exactly (it uses all samples).
BURN_IN <- 0L

# State invariants are validated after every accepted move. Left on because a
# corrupted state would silently invalidate every TV number in the sweep; the
# overhead is worth it here. Set FALSE to trade that safety net for speed.
DEBUG_INVARIANT_CHECKS <- TRUE

OUTPUT_DIR <- "dev/simulation/output"

# ============================================================================
# 1. GRAPH CONSTRUCTION
# ============================================================================
# Helpers duplicated from grid_validation.R rather than sourced, because that
# script executes a full validation run (libraries, MCMC, plots) on load.

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

  V(g)$capacity <- rep_len(as.integer(capacity), n)
  V(g)$area <- rep_len(as.double(area), n)
  V(g)$capacity_in_station <- rep_len(as.integer(capacity_in_station), n)
  V(g)$area_in_station <- rep_len(as.double(area_in_station), n)

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

# --- The disconnectedness ladder -------------------------------------------
#
# The four edges crossing the column-2 | column-3 boundary are cut one at a
# time, from the top down. The cut sets are NESTED, so this is a genuine
# ladder: each grid is the previous grid with one more edge removed.
#
#   n_cuts = 0        n_cuts = 2        n_cuts = 3        n_cuts = 4
#   1—2—3—4           1—2 3—4           1—2 3—4           1—2 3—4
#   | | | |           | | | |           | | | |           | | | |
#   5—6—7—8           5—6 7—8           5—6 7—8           5—6 7—8
#   | | | |           | | | |           | | | |           | | | |
#   9—a—b—c           9—a—b—c           9—a b—c           9—a b—c
#   | | | |           | | | |           | | | |           | | | |
#   d—e—f—g           d—e—f—g           d—e—f—g           d—e f—g
#   (full)            (2 crossings)     (1 bridge)        (severed)
#
# At n_cuts = 4 the graph is two disjoint 4x2 components. An LCC must be
# connected, so it lives entirely within one component — the state space is
# genuinely truncated rather than merely slow to traverse.

CROSSING_EDGES <- list(c(2, 3), c(6, 7), c(10, 11), c(14, 15))

make_cut_grid <- function(n_cuts) {
  e <- grid_edges(4, 4)
  if (n_cuts > 0) {
    for (i in seq_len(n_cuts)) {
      cut <- CROSSING_EDGES[[i]]
      e <- e[!(e[, 1] == cut[1] & e[, 2] == cut[2]), , drop = FALSE]
    }
  }
  coords <- grid_coords(4, 4)
  create_scenario(
    n = 16, edges = e,
    centroid_x = coords$x, centroid_y = coords$y
  )
}

GRID_SPECS <- data.table(
  n_cuts = 0:4,
  grid_label = c(
    "0 cuts (full)",
    "1 cut",
    "2 cuts",
    "3 cuts (bridge)",
    "4 cuts (severed)"
  )
)

# ============================================================================
# 2. LIBRARY BUILDING
# ============================================================================
# Same two-stage discovery (spanning tree + BFS supplement) the real MCMC uses.
# density_threshold = 0 because toy parcels have density 1/6 = 0.17, far below
# the production default of 15.

build_grid_libraries <- function(g, constraints) {
  pnames <- V(g)$name
  neighbor_cache <- setNames(
    lapply(pnames, function(p) neighbors(g, p)$name), pnames
  )

  tree_lccs <- discover_lccs_from_trees(
    parcel_graph = g,
    constraints = constraints,
    n_trees = 800L,
    max_discovery_capacity = constraints$min_capacity *
      DISCOVERY_CAPACITY_MULTIPLIER,
    verbose = FALSE
  )
  bfs_lccs <- run_bfs_lcc_supplement(
    tree_discovered_lccs = tree_lccs,
    parcel_graph = g,
    constraints = constraints,
    n_samples = BFS_LCC_N_SAMPLES,
    n_seeds = min(BFS_LCC_N_SEEDS, 8L),
    verbose = FALSE
  )
  combined_lccs <- combine_all_lcc_discoveries(
    tree_discovered = tree_lccs,
    bfs_discovered = bfs_lccs,
    bfs_stratified = NULL
  )
  lcc_library <- build_lcc_library_from_tree_discovery(
    discovered_lccs = combined_lccs$discovered_blocks,
    parcel_graph = g,
    constraints = constraints
  )

  size_bands <- list(c(5, 8), c(8, 12), c(12, 20))
  tree_secs <- discover_secondaries_from_trees(
    parcel_graph = g,
    size_bands = size_bands,
    density_threshold = 0,
    n_trees = 400L,
    verbose = FALSE
  )
  bfs_secs <- run_bfs_secondary_supplement(
    tree_discovered_secondaries = tree_secs,
    parcel_graph = g,
    size_bands = size_bands,
    quota_per_band = 30L,
    density_threshold = 0,
    verbose = FALSE
  )
  combined_secs <- combine_discovered_blocks(
    tree_discovered = tree_secs,
    bfs_discovered = bfs_secs,
    key_column = "sec_key",
    block_type = "secondary"
  )
  secondary_library <- build_secondary_library_from_discovery(
    combined_discovered = combined_secs,
    parcel_graph = g
  )

  list(
    lcc_library = lcc_library,
    secondary_library = secondary_library,
    neighbor_cache = neighbor_cache
  )
}

# ============================================================================
# 3. STATE ENUMERATION
# ============================================================================
# Ported from grid_validation.R, with one change: nbr_cache is now an explicit
# argument instead of a global. The sweep uses a different graph in every cell,
# so a global cache would silently apply the wrong adjacency.

enumerate_states <- function(g, lcc_lib, sec_lib, constraints, nbr_cache,
                             k_max = 3L) {
  pnames <- V(g)$name
  caps <- setNames(V(g)$capacity, pnames)
  areas <- setNames(V(g)$area, pnames)

  parcels_and_neighbors <- function(parcels) {
    c(parcels, unique(unlist(nbr_cache[parcels])))
  }

  lcc_sets <- lapply(lcc_lib$blocks, \(idx) sort(pnames[idx]))
  sec_sets <- lapply(sec_lib$blocks, \(idx) sort(pnames[idx]))
  n_pool <- sec_lib$n_blocks

  states <- vector("list", length(lcc_sets) * 4L)
  n_states <- 0L

  for (li in seq_along(lcc_sets)) {
    lcc <- lcc_sets[[li]]
    lcc_cap <- sum(caps[lcc])
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

    # Secondary blocks that neither overlap nor touch the LCC
    compat <- which(vapply(sec_sets, function(s) {
      if (any(s %in% lcc_forbidden)) return(FALSE)
      sec_nbrs <- unique(unlist(nbr_cache[s]))
      !any(sec_nbrs %in% lcc)
    }, logical(1)))

    if (length(compat) == 0) next

    for (k in seq_len(min(k_max, length(compat)))) {
      combos <- utils::combn(compat, k, simplify = FALSE)
      for (combo in combos) {
        secs <- sec_sets[combo]
        sec_union <- unlist(secs)
        if (anyDuplicated(sec_union)) next

        # Pairwise non-adjacency between secondaries
        if (k > 1L) {
          ok <- TRUE
          for (i in 1:(k - 1L)) {
            i_nbrs <- unique(unlist(nbr_cache[secs[[i]]]))
            for (j in (i + 1L):k) {
              if (any(secs[[j]] %in% i_nbrs)) {
                ok <- FALSE
                break
              }
            }
            if (!ok) break
          }
          if (!ok) next
        }

        plan <- sort(c(lcc, sec_union))
        if (anyDuplicated(plan)) next
        cap <- sum(caps[plan])
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

#' Aggregate enumerated states to a plan-level target distribution
#'
#' log w = -lambda_cap * excess - lambda_k * k - log C(n_pool, k)
#'
#' The -log C(n_pool, k) term is the reference measure correction from the
#' birth/death kernel; it cancels the combinatorial volume implicit in the
#' uniform proposal over k-subsets.
compute_plan_expected <- function(states, constraints) {
  state_dt <- rbindlist(lapply(states, function(s) {
    excess <- max(0, s$cap - constraints$min_capacity)
    log_w <- -CAPACITY_PRIOR_LAMBDA * excess -
      K_PRIOR_LAMBDA * s$k -
      lchoose(s$n_pool, s$k)
    data.table(plan_key = s$plan_key, k = s$k, cap = s$cap, log_w = log_w)
  }))
  state_dt[, w := exp(log_w - max(log_w))]

  plan_expected <- state_dt[, .(
    weight = sum(w),
    n_decompositions = .N,
    cap = cap[1]
  ), by = plan_key]
  plan_expected[, prob := weight / sum(weight)]
  plan_expected[order(-prob)]
}

# ============================================================================
# 4. SINGLE-CELL RUN
# ============================================================================

compute_ess <- function(x) {
  n <- length(x)
  if (n < 10) return(NA_real_)
  max_lag <- min(n - 1L, as.integer(10 * log10(n)))
  acf_vals <- acf(x, lag.max = max_lag, plot = FALSE)$acf[-1]
  first_neg <- which(acf_vals < 0)[1]
  if (!is.na(first_neg) && first_neg > 1) {
    acf_vals <- acf_vals[seq_len(first_neg - 1)]
  }
  n / (1 + 2 * sum(pmax(acf_vals, 0)))
}

#' Run one (grid, min_capacity) cell end to end
#'
#' Returns a one-row data.table of diagnostics, plus the full comparison table
#' as an attribute for later inspection.
run_cell <- function(n_cuts, min_capacity, seed = 1L) {
  t_start <- proc.time()[3]

  g <- make_cut_grid(n_cuts)
  constraints <- BASE_CONSTRAINTS
  constraints$min_capacity <- min_capacity

  nbr_cache <- setNames(
    lapply(V(g)$name, function(p) neighbors(g, p)$name), V(g)$name
  )

  set.seed(seed)

  # --- Libraries ---
  libs <- suppressMessages(build_grid_libraries(g, constraints))
  n_lcc <- libs$lcc_library$n_blocks
  n_sec <- libs$secondary_library$n_blocks

  if (n_lcc == 0 || n_sec == 0) {
    return(data.table(
      n_cuts = n_cuts, min_capacity = min_capacity,
      status = "empty_library", n_lcc_blocks = n_lcc, n_sec_blocks = n_sec
    ))
  }

  # --- Exact target ---
  states <- enumerate_states(
    g, libs$lcc_library, libs$secondary_library, constraints, nbr_cache
  )
  if (length(states) == 0) {
    return(data.table(
      n_cuts = n_cuts, min_capacity = min_capacity,
      status = "no_feasible_states", n_lcc_blocks = n_lcc, n_sec_blocks = n_sec
    ))
  }
  plan_expected <- compute_plan_expected(states, constraints)

  # --- MCMC ---
  init_states <- suppressMessages(generate_initial_states_from_lccs(
    lcc_library = libs$lcc_library,
    libraries = list(
      secondary_library = libs$secondary_library,
      lcc_library = libs$lcc_library
    ),
    parcel_graph = g,
    constraints = constraints,
    n_chains = N_CHAINS
  ))
  if (length(init_states) == 0) {
    return(data.table(
      n_cuts = n_cuts, min_capacity = min_capacity,
      status = "no_initial_states", n_lcc_blocks = n_lcc, n_sec_blocks = n_sec
    ))
  }

  # p_lcc_local = 0 so the enumerated state space is exact; its mass is folded
  # into replace_lcc (see header note).
  config <- define_parcel_kernel_configs(n_steps = N_STEPS)$default
  config$p_replace_lcc <- config$p_replace_lcc + config$p_lcc_local
  config$p_lcc_local <- 0

  # max_stored_samples = N_STEPS gives thin_interval = 1, i.e. every step is
  # stored. suppressMessages silences the ungated [DEBUG] message() calls in
  # run_parcel_mcmc (mcmc_parcel_mcmc_runner.R:285, :528), which would
  # otherwise emit 8 lines per cell.
  chain_results <- lapply(seq_along(init_states), function(i) {
    set.seed(seed + i * 1000L)
    suppressMessages(run_parcel_mcmc(
      parcel_graph = g,
      initial_state = init_states[[i]],
      constraints = constraints,
      secondary_library = libs$secondary_library,
      lcc_library = libs$lcc_library,
      config = config,
      parcel_assignments = NULL,
      neighbor_cache = libs$neighbor_cache,
      enable_online_enrichment = FALSE,
      max_stored_samples = N_STEPS,
      verbose = FALSE
    ))
  })

  # --- Empirical distribution (pooled across chains, burn-in dropped) ---
  drop_burn_in <- function(x) {
    if (BURN_IN > 0L && length(x) > BURN_IN) x[-seq_len(BURN_IN)] else x
  }

  empirical_keys <- unlist(lapply(chain_results, function(ch) {
    keys <- vapply(ch$parcel_samples, function(s) {
      if (is.null(s)) NA_character_ else paste(sort(s$X), collapse = ",")
    }, character(1))
    drop_burn_in(keys)
  }))
  empirical_keys <- empirical_keys[!is.na(empirical_keys)]

  chain_caps <- lapply(chain_results, function(ch) {
    caps <- vapply(ch$parcel_samples, function(s) {
      if (is.null(s)) NA_real_ else s$total_capacity
    }, numeric(1))
    caps <- drop_burn_in(caps)
    caps[!is.na(caps)]
  })

  if (length(empirical_keys) == 0) {
    return(data.table(
      n_cuts = n_cuts, min_capacity = min_capacity,
      status = "no_samples", n_lcc_blocks = n_lcc, n_sec_blocks = n_sec
    ))
  }

  empirical_tab <- prop.table(table(empirical_keys))

  # --- TV, computed exactly as in grid_validation.R ---
  all_keys <- union(plan_expected$plan_key, names(empirical_tab))
  compare <- data.table(
    plan = all_keys,
    expected = plan_expected[match(all_keys, plan_key), prob],
    observed = as.numeric(empirical_tab[all_keys])
  )
  compare[is.na(expected), expected := 0]
  compare[is.na(observed), observed := 0]
  compare <- compare[order(-expected)]

  n_samples <- length(empirical_keys)
  tv <- 0.5 * sum(abs(compare$expected - compare$observed))

  # Distribution-aware iid noise floor:
  #   E[TV] ~ (1/sqrt(N)) * sqrt(1/2pi) * sum sqrt(p_i (1 - p_i))
  p_vec <- plan_expected$prob
  iid_noise_tv <- sqrt(1 / (2 * pi)) * sum(sqrt(p_vec * (1 - p_vec))) /
    sqrt(n_samples)

  # Chi-squared goodness of fit (anti-conservative under autocorrelation)
  compare[, expected_count := expected * n_samples]
  compare[, observed_count := observed * n_samples]
  chisq_dt <- compare[expected_count >= 5]
  chisq_stat <- sum((chisq_dt$observed_count - chisq_dt$expected_count)^2 /
    chisq_dt$expected_count)
  chisq_df <- max(nrow(chisq_dt) - 1L, 0L)
  chisq_p <- if (chisq_df > 0) {
    pchisq(chisq_stat, df = chisq_df, lower.tail = FALSE)
  } else {
    NA_real_
  }

  # Mass the sampler never placed — on severed grids this is the floor imposed
  # by states the chains cannot reach, not evidence of a biased kernel.
  unvisited_mass <- compare[observed == 0, sum(expected)]

  rhat_dt <- tryCatch(
    compute_parcel_multichain_rhat(chain_results),
    error = function(e) NULL
  )
  rhat_cap <- if (!is.null(rhat_dt) && nrow(rhat_dt) > 0 &&
    "capacity" %in% rhat_dt$metric) {
    rhat_dt[metric == "capacity", rhat][1]
  } else {
    NA_real_
  }

  result <- data.table(
    n_cuts = n_cuts,
    min_capacity = min_capacity,
    status = "ok",
    n_lcc_blocks = n_lcc,
    n_sec_blocks = n_sec,
    n_states = length(states),
    n_plans_expected = nrow(plan_expected),
    n_plans_visited = sum(compare$observed > 0),
    coverage = sum(compare$observed > 0) / nrow(plan_expected),
    n_samples = n_samples,
    tv = tv,
    iid_noise_tv = iid_noise_tv,
    tv_ratio = tv / iid_noise_tv,
    unvisited_mass = unvisited_mass,
    chisq_p = chisq_p,
    n_chisq_plans = nrow(chisq_dt),
    rhat_capacity = rhat_cap,
    total_ess = sum(vapply(chain_caps, compute_ess, numeric(1)), na.rm = TRUE),
    elapsed_s = proc.time()[3] - t_start
  )
  attr(result, "compare") <- compare
  attr(result, "plan_expected") <- plan_expected
  result
}

# ============================================================================
# 5. SWEEP
# ============================================================================

sweep_design <- CJ(n_cuts = GRID_SPECS$n_cuts, min_capacity = MIN_CAPACITY_VALUES)
sweep_design <- merge(sweep_design, GRID_SPECS, by = "n_cuts")
setorder(sweep_design, n_cuts, min_capacity)

cat("Sweep design:", nrow(sweep_design), "cells (",
  length(GRID_SPECS$n_cuts), "grids x", length(MIN_CAPACITY_VALUES),
  "min_capacity values )\n")
cat("Per cell:", N_CHAINS, "chains x", N_STEPS, "steps\n\n")

cell_details <- vector("list", nrow(sweep_design))
results <- vector("list", nrow(sweep_design))

for (i in seq_len(nrow(sweep_design))) {
  row <- sweep_design[i]
  cat(sprintf(
    "[%2d/%2d] %-18s min_capacity = %g ... ",
    i, nrow(sweep_design), row$grid_label, row$min_capacity
  ))
  flush.console()

  res <- tryCatch(
    run_cell(row$n_cuts, row$min_capacity, seed = 1000L + i),
    error = function(e) {
      data.table(
        n_cuts = row$n_cuts, min_capacity = row$min_capacity,
        status = paste0("error: ", conditionMessage(e))
      )
    }
  )

  if (identical(res$status, "ok")) {
    cat(sprintf(
      "TV = %.4f (%.1fx noise), %d/%d plans, %.0fs\n",
      res$tv, res$tv_ratio, res$n_plans_visited, res$n_plans_expected,
      res$elapsed_s
    ))
  } else {
    cat(res$status, "\n")
  }

  cell_details[[i]] <- list(
    compare = attr(res, "compare"),
    plan_expected = attr(res, "plan_expected")
  )
  results[[i]] <- res
}

sweep_results <- rbindlist(results, fill = TRUE)
sweep_results <- merge(sweep_results, GRID_SPECS, by = "n_cuts", all.x = TRUE)
setorder(sweep_results, n_cuts, min_capacity)

# ============================================================================
# 6. OUTPUT
# ============================================================================

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(results = sweep_results, details = cell_details, design = sweep_design),
  file.path(OUTPUT_DIR, "sweep_results.rds")
)
fwrite(sweep_results, file.path(OUTPUT_DIR, "sweep_results.csv"))

cat("\n=== SWEEP RESULTS ===\n")
print(sweep_results[, .(
  grid_label, min_capacity, n_plans_expected, n_plans_visited,
  tv = round(tv, 4), tv_ratio = round(tv_ratio, 2),
  unvisited_mass = round(unvisited_mass, 4),
  rhat = round(rhat_capacity, 3)
)])

ok <- sweep_results[status == "ok"]

if (nrow(ok) > 0) {
  cat("\nTV / noise floor, by grid and min_capacity:\n")
  print(dcast(ok, grid_label ~ min_capacity, value.var = "tv_ratio"))

  # --- Plot 1: TV vs min_capacity, one line per grid ---
  # Values near the noise floor (ratio ~ 1) indicate a correct sampler; a
  # ratio that climbs with cuts localises where mixing degrades.
  p_tv <- ggplot(ok, aes(
    x = min_capacity, y = tv,
    color = factor(n_cuts), group = n_cuts
  )) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = iid_noise_tv), linetype = "dashed", alpha = 0.5) +
    labs(
      title = "Total Variation Distance by Constraint and Disconnectedness",
      subtitle = paste0(
        "Dashed = iid noise floor | ", N_CHAINS, " chains x ",
        format(N_STEPS, big.mark = ","), " steps"
      ),
      x = "min_capacity", y = "Total variation distance",
      color = "Cuts"
    )
  print(p_tv)

  # --- Plot 2: TV relative to its own noise floor ---
  # Divides out the fact that cells differ in plan count and sample size, so
  # cells are directly comparable. 1.0 = indistinguishable from iid sampling.
  p_ratio <- ggplot(ok, aes(
    x = factor(n_cuts), y = factor(min_capacity), fill = tv_ratio
  )) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", tv_ratio)), size = 3) +
    scale_fill_gradient2(
      low = "steelblue", mid = "white", high = "firebrick",
      midpoint = 1, name = "TV / noise"
    ) +
    labs(
      title = "TV Relative to iid Noise Floor",
      subtitle = "1.0 = indistinguishable from independent sampling",
      x = "Number of cuts", y = "min_capacity"
    )
  print(p_ratio)

  # --- Plot 3: unreachable-state mass ---
  # Expected mass on plans the chains never visited. On severed grids this is
  # a structural floor under TV, not evidence of a biased kernel.
  p_unvisited <- ggplot(ok, aes(
    x = min_capacity, y = unvisited_mass,
    color = factor(n_cuts), group = n_cuts
  )) +
    geom_line() +
    geom_point() +
    labs(
      title = "Expected Mass on Never-Visited Plans",
      subtitle = "A floor under TV where the state space is unreachable",
      x = "min_capacity", y = "Unvisited expected mass",
      color = "Cuts"
    )
  print(p_unvisited)
}

cat("\nWrote", file.path(OUTPUT_DIR, "sweep_results.rds"), "and .csv\n")
