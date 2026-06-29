# Equivalence guard for the bfs_grow_block() integer fast path.
#
# bfs_grow_block() has two implementations behind one signature: the original
# character-vector body (ctx = NULL) and the integer fast path bfs_grow_block_ctx()
# (reached when a bfs_build_context() object is supplied). They must produce
# bit-identical output under the same RNG seed -- the discovery pipeline's
# hash-gated reproducibility depends on it. These tests pin that invariant on
# small graphs so a future change to either path (or to neighbor ordering) fails
# loudly here rather than silently diverging in production.

make_named_graph <- function(dims = c(4, 4)) {
  g <- igraph::make_lattice(dimvector = dims)
  igraph::V(g)$name <- paste0("p", seq_len(igraph::vcount(g)))
  g
}

# The two paths must agree on everything the discovery pipeline observes: the
# grown block, its metric total, the success flag's VALUE, and the seed. The
# character slow path additionally leaves a vestigial name on `success` (the
# seed's name rides along through `current_metric <- current_metric +
# next_metric`); no caller reads that name and it never reaches the output hash,
# so we compare unnamed values rather than bake the wart into the fast path.
bfs_payload <- function(r) {
  list(block        = r$block,
       metric_total = unname(r$metric_total),
       success      = unname(r$success),
       seed         = unname(r$seed))
}

test_that("ctx fast path matches the character slow path (no pools)", {
  g <- make_named_graph()
  set.seed(99)
  metric <- stats::setNames(runif(igraph::vcount(g), 1, 5), igraph::V(g)$name)
  ctx <- bfs_build_context(g, metric, eligible_pool = NULL)

  for (s in 1:30) {
    set.seed(s)
    slow <- bfs_grow_block(graph = g, metric_lookup = metric, target_min = 15)
    set.seed(s)
    fast <- bfs_grow_block(ctx = ctx, target_min = 15)
    expect_identical(bfs_payload(fast), bfs_payload(slow))
  }
})

test_that("ctx fast path matches with restricted seed_pool and eligible_pool", {
  g <- make_named_graph()
  set.seed(7)
  metric <- stats::setNames(runif(igraph::vcount(g), 1, 5), igraph::V(g)$name)
  eligible <- paste0("p", c(1:6, 9:14))   # drop a couple of rows of the lattice
  seeds    <- paste0("p", c(1, 2, 5, 6))
  ctx <- bfs_build_context(g, metric, eligible_pool = eligible)

  for (s in 1:30) {
    set.seed(s)
    slow <- bfs_grow_block(graph = g, metric_lookup = metric,
                           seed_pool = seeds, eligible_pool = eligible,
                           target_min = 12)
    set.seed(s)
    fast <- bfs_grow_block(ctx = ctx, seed_pool = seeds, target_min = 12)
    expect_identical(bfs_payload(fast), bfs_payload(slow))
  }
})

test_that("ctx fast path matches with target_max + check_max_before_add", {
  g <- make_named_graph()
  set.seed(123)
  metric <- stats::setNames(runif(igraph::vcount(g), 1, 5), igraph::V(g)$name)
  ctx <- bfs_build_context(g, metric, eligible_pool = NULL)

  for (s in 1:30) {
    set.seed(s)
    slow <- bfs_grow_block(graph = g, metric_lookup = metric,
                           target_min = 8, target_exact = 14, target_max = 16,
                           check_max_before_add = TRUE)
    set.seed(s)
    fast <- bfs_grow_block(ctx = ctx, target_min = 8, target_exact = 14,
                           target_max = 16, check_max_before_add = TRUE)
    expect_identical(bfs_payload(fast), bfs_payload(slow))
  }
})

test_that("ctx fast path matches with a single-node seed_pool", {
  g <- make_named_graph()
  set.seed(42)
  metric <- stats::setNames(runif(igraph::vcount(g), 1, 5), igraph::V(g)$name)
  ctx <- bfs_build_context(g, metric, eligible_pool = NULL)

  for (start in paste0("p", c(1, 8, 11, 16))) {
    set.seed(5)
    slow <- bfs_grow_block(graph = g, metric_lookup = metric,
                           seed_pool = start, target_min = 13)
    set.seed(5)
    fast <- bfs_grow_block(ctx = ctx, seed_pool = start, target_min = 13)
    expect_identical(bfs_payload(fast), bfs_payload(slow))
  }
})

test_that("build_neighbor_cache reproduces the per-vertex neighbors() cache", {
  g <- make_named_graph()
  nm <- igraph::V(g)$name
  reference <- stats::setNames(
    lapply(nm, function(m) igraph::neighbors(g, m)$name), nm
  )
  expect_identical(build_neighbor_cache(g), reference)
})
