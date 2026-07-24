# frontier_enumeration.R — exact enumeration/counting of connected subgraphs
# via a frontier sweep (the "ZDD" / frontier-based search idea).
#
# WHY THIS EXISTS
# ---------------
# grid_validation.R answers two questions exactly on a toy grid:
#   (1) does the MCMC sample from its target?   (2) is the block library complete?
# Question (2) currently loops over all 2^n subsets, which caps the toy at a
# 4x4 grid (65,535 subsets). This file replaces that loop with a sweep whose
# cost scales with the number of *feasible* subgraphs, not with 2^n.
#
# THE IDEA IN ONE PARAGRAPH
# -------------------------
# Decide parcels one at a time in a fixed order: in-district or out. After
# deciding the first i parcels, the only thing the *future* can still touch is
# the set of decided parcels that still have an undecided neighbour — call that
# the FRONTIER. Everything behind it is buried and unreachable. So two different
# prefixes are interchangeable if they agree on (a) which frontier parcels are
# in, (b) which of those are already connected to each other through buried
# parcels, and (c) any running totals a constraint needs. Memoise on that and
# the 2^n tree collapses into a small DAG. A subgraph is then a path through
# the DAG; walking the paths enumerates, and a DP over the DAG counts.
#
# HONEST LIMITS
# -------------
# * COUNTING scales a long way (7x7, 8x8 are fine) because it never materialises
#   anything. Use it to get exact denominators for library-recall stats.
# * ENUMERATION costs O(number of solutions). A 6x6 grid has 1.73e9 connected
#   subgraphs, so unconstrained enumeration is hopeless there no matter what
#   algorithm you use. With real thresholds it collapses fast. ALWAYS COUNT
#   FIRST (fe_count) and only enumerate if the number is sane.
# * Running totals in the state key cost node sharing. Integer-ish capacities
#   and areas keep the DAG small; continuous weights inflate it badly. The toy
#   grid's integer weights are ideal — keep them that way.
#
# Author's note: min_lcc_fraction is deliberately NOT handled here. It is a
# ratio (lcc_capacity / total_capacity), and ratios wreck the sweep — you cannot
# prune on them, so the state explodes. The fix is to CONDITION on the LCC:
# fix a candidate LCC, and `cap(S_secondaries) <= cap(LCC)` becomes a plain
# upper bound. See fe_enumerate_secondaries() at the bottom.

suppressPackageStartupMessages(library(igraph))

# ============================================================================
# 1. CORE: BUILD THE DAG
# ============================================================================

#' Build the frontier DAG for connected subgraphs of `g` meeting constraints.
#'
#' @param g igraph with vertex attrs `capacity` and `area`.
#' @param k_max Max number of connected components the selected set may have.
#'   1 = a single connected blob (use this for LCC / secondary candidates).
#' @param min_capacity,min_area Global thresholds on the selected set.
#' @param min_density Enforces sum(capacity) / sum(area) >= min_density.
#' @param min_component_area Per-component floor, applied to EACH connected
#'   component as it closes (this is where a "5 acre contiguous" rule goes).
#' @param order Sweep order (vertex indices). Default 1:n. A bandwidth-reducing
#'   order gives a narrower frontier and a smaller DAG.
#' @return list(root, lo, hi, order, n) — a decision DAG. Terminals are the
#'   sentinels FE_FALSE (0L) and FE_TRUE (1L).
FE_FALSE <- 0L
FE_TRUE  <- 1L

fe_build <- function(g, k_max = 1L,
                     min_capacity = 0, min_area = 0, min_density = 0,
                     min_component_area = 0, order = NULL) {

  n   <- vcount(g)
  ord <- if (is.null(order)) seq_len(n) else order
  cap <- as.numeric(V(g)$capacity)[ord]
  ar  <- as.numeric(V(g)$area)[ord]

  # adjacency in *sweep positions*, not vertex ids
  pos <- integer(n); pos[ord] <- seq_len(n)
  adj <- lapply(seq_len(n), function(p) sort(pos[as.integer(neighbors(g, ord[p]))]))

  # frontier[[i]] = positions <= i that still have a neighbour > i
  frontier <- vector("list", n + 1L)
  frontier[[1L]] <- integer(0)                       # before anything is decided
  for (i in seq_len(n)) {
    f <- integer(0)
    for (p in seq_len(i)) if (any(adj[[p]] > i)) f <- c(f, p)
    frontier[[i + 1L]] <- f
  }

  # suffix sums, for "can this branch still reach the thresholds?" pruning
  rem_cap  <- c(rev(cumsum(rev(cap))), 0)
  rem_area <- c(rev(cumsum(rev(ar))),  0)
  dpos     <- pmax(0, cap - min_density * ar)        # only the helpful part
  rem_dpos <- c(rev(cumsum(rev(dpos))), 0)

  track_comp_area <- min_component_area > 0

  # A state is: labels over the frontier (0 = OUT, 1..k = component id,
  # canonicalised by first appearance), number of closed components, running
  # capacity and area, and (optionally) per-live-component area.
  key_of <- function(labs, closed, c_cap, c_area, comp_area) {
    paste(paste(labs, collapse = ","), closed,
          round(c_cap, 6), round(c_area, 6),
          if (track_comp_area) paste(round(comp_area, 6), collapse = ",") else "",
          sep = "|")
  }

  lo <- integer(0); hi <- integer(0)
  nxt_id <- 2L                                       # 0,1 are the terminals

  root_state <- list(labs = integer(0), closed = 0L, cap = 0, area = 0,
                     comp_area = numeric(0))
  root <- nxt_id; nxt_id <- nxt_id + 1L
  layer <- list(); layer[[key_of(integer(0), 0L, 0, 0, numeric(0))]] <-
    list(id = root, st = root_state)

  for (i in seq_len(n)) {
    fi_prev <- frontier[[i]]
    fi      <- frontier[[i + 1L]]
    new_layer <- new.env(hash = TRUE, parent = emptyenv())
    new_keys  <- character(0)

    for (entry in layer) {
      st <- entry$st
      for (x in c(0L, 1L)) {
        labs <- st$labs; names(labs) <- as.character(fi_prev)
        closed <- st$closed; c_cap <- st$cap; c_area <- st$area
        comp_area <- st$comp_area
        dead <- FALSE

        if (x == 0L) {
          labs[as.character(i)] <- 0L                # parcel i is OUT
        } else {
          # every already-decided neighbour of i is on the previous frontier
          dn <- adj[[i]][adj[[i]] < i]
          nb <- unique(labs[as.character(dn)])
          nb <- nb[!is.na(nb) & nb > 0L]

          if (length(nb) == 0L) {                    # opens a new component
            newg <- 1L
            used <- unique(labs[labs > 0L])
            while (newg %in% used) newg <- newg + 1L
            labs[as.character(i)] <- newg
            if (track_comp_area) comp_area[newg] <- ar[i]
          } else {                                   # joins (and maybe MERGES)
            keep <- min(nb)
            if (track_comp_area) {
              comp_area[keep] <- sum(comp_area[nb], na.rm = TRUE) + ar[i]
              for (gdrop in setdiff(nb, keep)) comp_area[gdrop] <- NA_real_
            }
            labs[labs %in% nb] <- keep
            labs[as.character(i)] <- keep
          }
          c_cap  <- c_cap  + cap[i]
          c_area <- c_area + ar[i]
        }

        # --- components that have fully left the frontier are CLOSED forever
        alive <- unique(labs[names(labs) %in% as.character(fi) & labs > 0L])
        for (gg in setdiff(unique(labs[labs > 0L]), alive)) {
          if (track_comp_area && !is.na(comp_area[gg]) &&
              comp_area[gg] < min_component_area) { dead <- TRUE; break }
          closed <- closed + 1L
          if (track_comp_area) comp_area[gg] <- NA_real_
        }
        if (!dead && closed > k_max) dead <- TRUE

        # --- can this branch still reach the thresholds?
        if (!dead) {
          if (c_cap  + rem_cap[i + 1L]  < min_capacity) dead <- TRUE
          if (c_area + rem_area[i + 1L] < min_area)     dead <- TRUE
          if ((c_cap - min_density * c_area) + rem_dpos[i + 1L] < 0) dead <- TRUE
        }

        if (dead) {
          child <- FE_FALSE
        } else if (i == n) {                          # sweep over: accept?
          ok <- closed >= 1L && closed <= k_max &&
                c_cap >= min_capacity && c_area >= min_area &&
                (c_cap - min_density * c_area) >= 0
          child <- if (ok) FE_TRUE else FE_FALSE
        } else {
          labs <- labs[names(labs) %in% as.character(fi)]
          labs <- labs[order(as.integer(names(labs)))]
          # canonicalise: relabel components by first appearance so that
          # equivalent states collide in the hash and MERGE.
          remap <- integer(0); cnt <- 0L; out <- labs
          for (j in seq_along(labs)) {
            if (labs[j] == 0L) { out[j] <- 0L; next }
            key <- as.character(labs[j])
            if (is.null(remap[key]) || is.na(remap[key])) {
              cnt <- cnt + 1L; remap[key] <- cnt
            }
            out[j] <- remap[key]
          }
          ca <- if (track_comp_area) {
            v <- numeric(cnt)
            for (j in seq_along(labs)) if (labs[j] > 0L) v[out[j]] <- comp_area[labs[j]]
            v
          } else numeric(0)

          k <- key_of(out, closed, c_cap, c_area, ca)
          if (!is.null(new_layer[[k]])) {
            child <- new_layer[[k]]$id                # <- the merge
          } else {
            child <- nxt_id; nxt_id <- nxt_id + 1L
            new_layer[[k]] <- list(id = child,
                                   st = list(labs = unname(out), closed = closed,
                                             cap = c_cap, area = c_area,
                                             comp_area = ca))
            new_keys <- c(new_keys, k)
          }
        }
        if (x == 0L) lo[entry$id] <- child else hi[entry$id] <- child
      }
    }
    layer <- mget(new_keys, envir = new_layer)
  }

  list(root = root, lo = lo, hi = hi, order = ord, n = n, n_nodes = nxt_id - 2L)
}

# ============================================================================
# 2. USING THE DAG: COUNT, ENUMERATE, SAMPLE
# ============================================================================

#' Exact count of solutions. Cheap — never materialises anything.
#' RUN THIS BEFORE fe_enumerate().
fe_count <- function(dag) {
  memo <- new.env(hash = TRUE, parent = emptyenv())
  rec <- function(nd) {
    if (nd == FE_FALSE) return(0)
    if (nd == FE_TRUE)  return(1)
    k <- as.character(nd)
    if (!is.null(memo[[k]])) return(memo[[k]])
    v <- rec(dag$lo[nd]) + rec(dag$hi[nd])
    memo[[k]] <- v
    v
  }
  rec(dag$root)
}

#' Walk every solution. Returns the same shape grid_validation.R's brute-force
#' loop produces: list(parcels=<chr>, indices=<int>, capacity=, area=).
fe_enumerate <- function(dag, g, limit = 5e6) {
  total <- fe_count(dag)
  if (total > limit) {
    stop(sprintf(paste0("fe_enumerate: %s solutions exceeds limit=%s.\n",
                        "  Tighten the constraints, or raise `limit` if you ",
                        "really want them all."),
                 format(total, big.mark = ","), format(limit, big.mark = ",")))
  }
  cap <- as.numeric(V(g)$capacity); ar <- as.numeric(V(g)$area)
  nm  <- V(g)$name
  out <- vector("list", total); k <- 0L

  rec <- function(nd, depth, chosen) {
    if (nd == FE_FALSE) return(invisible(NULL))
    if (nd == FE_TRUE) {
      idx <- sort(dag$order[chosen])
      k <<- k + 1L
      out[[k]] <<- list(parcels  = sort(nm[idx]),
                        indices  = idx,
                        capacity = sum(cap[idx]),
                        area     = sum(ar[idx]))
      return(invisible(NULL))
    }
    rec(dag$lo[nd], depth + 1L, chosen)                  # parcel `depth` OUT
    rec(dag$hi[nd], depth + 1L, c(chosen, depth))        # parcel `depth` IN
  }
  rec(dag$root, 1L, integer(0))
  out[seq_len(k)]
}

#' Draw an exactly-uniform solution. No burn-in, no autocorrelation — useful as
#' an independent reference sample to hold the MCMC against.
fe_sample <- function(dag, g, n_draws = 1L) {
  memo <- new.env(hash = TRUE, parent = emptyenv())
  cnt <- function(nd) {
    if (nd == FE_FALSE) return(0)
    if (nd == FE_TRUE)  return(1)
    k <- as.character(nd)
    if (is.null(memo[[k]])) memo[[k]] <- cnt(dag$lo[nd]) + cnt(dag$hi[nd])
    memo[[k]]
  }
  cnt(dag$root)
  cap <- as.numeric(V(g)$capacity); ar <- as.numeric(V(g)$area); nm <- V(g)$name

  replicate(n_draws, simplify = FALSE, {
    nd <- dag$root; depth <- 1L; chosen <- integer(0)
    while (nd != FE_TRUE && nd != FE_FALSE) {
      a <- cnt(dag$lo[nd]); b <- cnt(dag$hi[nd])
      if (runif(1) < b / (a + b)) { chosen <- c(chosen, depth); nd <- dag$hi[nd] }
      else                        { nd <- dag$lo[nd] }
      depth <- depth + 1L
    }
    idx <- sort(dag$order[chosen])
    list(parcels = sort(nm[idx]), indices = idx,
         capacity = sum(cap[idx]), area = sum(ar[idx]))
  })
}

# ============================================================================
# 3. THE min_lcc_fraction TRICK: CONDITION ON THE LCC
# ============================================================================
#
# min_lcc_fraction is a ratio and cannot be pruned on. But your state carries a
# *designated* LCC, so fix it. With cap(L) known, the constraint
#     cap(L) >= f * (cap(L) + cap(S))
# rearranges to a plain upper bound on the secondaries:
#     cap(S) <= cap(L) * (1 - f) / f
# and every other constraint becomes a threshold with a constant right-hand side.
# Secondaries must also avoid N[L] (the LCC plus its neighbours), or they would
# merge into the LCC instead of being separate components.

#' Enumerate all valid secondary sets for a FIXED candidate LCC.
fe_enumerate_secondaries <- function(g, lcc_idx, constraints, k_max = 3L,
                                     min_component_area = 0, limit = 5e6) {
  cap <- as.numeric(V(g)$capacity); ar <- as.numeric(V(g)$area)
  cap_L <- sum(cap[lcc_idx]); area_L <- sum(ar[lcc_idx])

  f <- constraints$min_lcc_fraction
  cap_S_max <- cap_L * (1 - f) / f

  forbidden <- unique(c(lcc_idx,
                        as.integer(unlist(adjacent_vertices(g, lcc_idx)))))
  eligible  <- setdiff(seq_len(vcount(g)), forbidden)
  if (!length(eligible)) return(list())

  h <- induced_subgraph(g, eligible)
  dag <- fe_build(h, k_max = k_max,
                  min_capacity = max(0, constraints$min_capacity - cap_L),
                  min_area     = max(0, constraints$min_area     - area_L),
                  min_density  = 0,          # applied jointly with L below
                  min_component_area = min_component_area)

  sets <- fe_enumerate(dag, h, limit = limit)
  keep <- Filter(function(s) {
    if (s$capacity > cap_S_max) return(FALSE)                       # LCC fraction
    tot_c <- cap_L + s$capacity; tot_a <- area_L + s$area
    tot_c / tot_a >= constraints$min_density                        # joint density
  }, sets)
  lapply(keep, function(s) {
    idx <- sort(c(lcc_idx, eligible[s$indices]))
    list(lcc = sort(lcc_idx), secondaries = eligible[s$indices],
         indices = idx, capacity = sum(cap[idx]), area = sum(ar[idx]))
  })
}

# ============================================================================
# 4. DEMO / SELF-TEST
# ============================================================================

if (sys.nframe() == 0L) {

  make_grid <- function(nrow, ncol, seed = 1L) {
    set.seed(seed)
    n <- nrow * ncol
    e <- list()
    for (r in seq_len(nrow)) for (cc in seq_len(ncol)) {
      id <- (r - 1) * ncol + cc
      if (cc < ncol) e <- c(e, list(c(id, id + 1)))
      if (r < nrow)  e <- c(e, list(c(id, id + ncol)))
    }
    g <- make_empty_graph(n = n, directed = FALSE)
    V(g)$name <- as.character(seq_len(n))
    g <- add_edges(g, as.character(t(do.call(rbind, e))))
    V(g)$capacity <- sample(1:5, n, replace = TRUE)   # integer weights keep the
    V(g)$area     <- sample(3:10, n, replace = TRUE)  # DAG small — keep them so
    g
  }

  brute_connected <- function(g) {                     # the 2^n loop, for checking
    n <- vcount(g); found <- 0L
    for (mask in seq_len(2^n - 1L)) {
      bits <- which(bitwAnd(mask, 2L^(0:(n - 1L))) > 0L)
      if (length(bits) == 1L ||
          is_connected(induced_subgraph(g, V(g)$name[bits]))) found <- found + 1L
    }
    found
  }

  cat("=== 1. agreement with brute force (4x4) ===\n")
  g4 <- make_grid(4, 4)
  d4 <- fe_build(g4, k_max = 1L)
  cat(sprintf("  frontier : %s connected subgraphs (%s DAG nodes)\n",
              format(fe_count(d4), big.mark = ","), format(d4$n_nodes, big.mark = ",")))
  cat(sprintf("  brute    : %s\n", format(brute_connected(g4), big.mark = ",")))
  subs <- fe_enumerate(d4, g4)
  cat(sprintf("  walked   : %s, all connected: %s\n", format(length(subs), big.mark = ","),
              all(vapply(subs, function(s)
                is_connected(induced_subgraph(g4, V(g4)$name[s$indices])), logical(1)))))

  cat("\n=== 2. counting scales past what brute force can reach ===\n")
  cat(sprintf("  %-8s %14s %12s %10s\n", "grid", "connected", "DAG nodes", "secs"))
  for (m in 4:6) {
    g <- make_grid(m, m); t0 <- proc.time()
    d <- fe_build(g, k_max = 1L); ct <- fe_count(d)
    cat(sprintf("  %-8s %14s %12s %10.1f\n", sprintf("%dx%d", m, m),
                format(ct, big.mark = ",", scientific = FALSE),
                format(d$n_nodes, big.mark = ","), (proc.time() - t0)[3]))
  }
  cat("  (2^36 = 6.9e10 — the 6x6 row is unreachable by the brute-force loop)\n")

  cat("\n=== 3. constraints make ENUMERATION feasible at 5x5 ===\n")
  g5 <- make_grid(5, 5)
  for (mc in c(0, 20, 30)) {
    d <- fe_build(g5, k_max = 1L, min_capacity = mc, min_area = 40, min_density = 0.3)
    cat(sprintf("  min_capacity >= %-3s : %12s districts, %8s DAG nodes\n",
                mc, format(fe_count(d), big.mark = ","),
                format(d$n_nodes, big.mark = ",")))
  }

  cat("\n=== 4. multi-component districts (k_max = 2), 4x4 ===\n")
  for (k in 1:2) {
    d <- fe_build(g4, k_max = k, min_capacity = 15, min_area = 30,
                  min_density = 0.3, min_component_area = 10)
    cat(sprintf("  k_max = %d : %s feasible districts\n", k,
                format(fe_count(d), big.mark = ",")))
  }

  cat("\n=== 5. exactly-uniform draws (no burn-in, no autocorrelation) ===\n")
  set.seed(42)
  d <- fe_build(g4, k_max = 1L, min_capacity = 15, min_area = 30, min_density = 0.3)
  for (s in fe_sample(d, g4, 3)) {
    grid <- matrix(".", 4, 4)
    grid[s$indices] <- "#"
    cat("   ", paste(apply(t(grid), 1, paste, collapse = " "), collapse = "  |  "),
        sprintf("  cap=%2d area=%2d\n", s$capacity, s$area))
  }
}
