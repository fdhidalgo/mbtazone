# Tests for Adjacency Graph Functions

# Helper function to create simple test geometries
create_grid_parcels <- function(n_rows, n_cols) {
  # Create a simple grid of square parcels
  parcels_list <- list()
  id <- 1

  for (row in 1:n_rows) {
    for (col in 1:n_cols) {
      # Create square parcel at position (row, col)
      xmin <- (col - 1) * 100
      xmax <- col * 100
      ymin <- (row - 1) * 100
      ymax <- row * 100

      parcel <- sf::st_polygon(list(matrix(
        c(xmin, ymin,
          xmax, ymin,
          xmax, ymax,
          xmin, ymax,
          xmin, ymin),
        ncol = 2, byrow = TRUE
      )))

      parcels_list[[id]] <- parcel
      id <- id + 1
    }
  }

  # Create sf object
  parcels_sf <- sf::st_sf(
    parcel_id = 1:length(parcels_list),
    geometry = sf::st_sfc(parcels_list, crs = 26986)
  )

  return(parcels_sf)
}

# Helper function to create disconnected parcels
create_disconnected_parcels <- function() {
  # Create two parcels with no shared boundary
  parcel1 <- sf::st_polygon(list(matrix(
    c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0),
    ncol = 2, byrow = TRUE
  )))

  parcel2 <- sf::st_polygon(list(matrix(
    c(200, 200, 300, 200, 300, 300, 200, 300, 200, 200),
    ncol = 2, byrow = TRUE
  )))

  parcels_sf <- sf::st_sf(
    parcel_id = c(1, 2),
    geometry = sf::st_sfc(parcel1, parcel2, crs = 26986)
  )

  return(parcels_sf)
}

# ============================================================================
# build_adjacency() tests
# ============================================================================

test_that("build_adjacency works with 2x2 grid", {
  parcels <- create_grid_parcels(2, 2)
  adj <- build_adjacency(parcels, quiet = TRUE)

  # Check structure
  expect_type(adj, "list")
  expect_length(adj, 4)

  # Expected adjacency for 2x2 grid:
  # 1 -- 2
  # |    |
  # 3 -- 4
  #
  # Adjacencies (0-indexed internally, but returned as 1-indexed):
  # Parcel 1: neighbors 2, 3
  # Parcel 2: neighbors 1, 4
  # Parcel 3: neighbors 1, 4
  # Parcel 4: neighbors 2, 3

  expect_setequal(adj[[1]], c(2, 3))
  expect_setequal(adj[[2]], c(1, 4))
  expect_setequal(adj[[3]], c(1, 4))
  expect_setequal(adj[[4]], c(2, 3))
})

test_that("build_adjacency works with 3x3 grid", {
  parcels <- create_grid_parcels(3, 3)
  adj <- build_adjacency(parcels, quiet = TRUE)

  expect_type(adj, "list")
  expect_length(adj, 9)

  # Center parcel (5) should have 4 neighbors
  # 1 2 3
  # 4 5 6
  # 7 8 9
  expect_length(adj[[5]], 4)
  expect_setequal(adj[[5]], c(2, 4, 6, 8))

  # Corner parcel (1) should have 2 neighbors
  expect_length(adj[[1]], 2)
  expect_setequal(adj[[1]], c(2, 4))

  # Edge parcel (2) should have 3 neighbors
  expect_length(adj[[2]], 3)
  expect_setequal(adj[[2]], c(1, 3, 5))
})

test_that("build_adjacency detects disconnected parcels", {
  parcels <- create_disconnected_parcels()

  # Should warn about isolated parcels
  expect_warning(
    adj <- build_adjacency(parcels, quiet = FALSE),
    "isolated"
  )

  # Each parcel should have no neighbors
  expect_length(adj[[1]], 0)
  expect_length(adj[[2]], 0)
})

test_that("build_adjacency enforces rook adjacency (no corner touches)", {
  # Create diagonal parcels that only touch at corners
  # □ ■
  # ■ □
  parcel1 <- sf::st_polygon(list(matrix(
    c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0),
    ncol = 2, byrow = TRUE
  )))

  parcel2 <- sf::st_polygon(list(matrix(
    c(100, 100, 200, 100, 200, 200, 100, 200, 100, 100),
    ncol = 2, byrow = TRUE
  )))

  parcels_sf <- sf::st_sf(
    parcel_id = c(1, 2),
    geometry = sf::st_sfc(parcel1, parcel2, crs = 26986)
  )

  adj <- build_adjacency(parcels_sf, quiet = TRUE)

  # Parcels should NOT be adjacent (rook adjacency requires shared edge)
  expect_length(adj[[1]], 0)
  expect_length(adj[[2]], 0)
})

test_that("build_adjacency verifies symmetry", {
  parcels <- create_grid_parcels(3, 3)
  adj <- build_adjacency(parcels, quiet = TRUE)

  # If i is adjacent to j, then j must be adjacent to i
  for (i in seq_along(adj)) {
    neighbors <- adj[[i]]
    for (j in neighbors) {
      expect_true(
        i %in% adj[[j]],
        info = sprintf("Parcel %d adjacent to %d, but not vice versa", i, j)
      )
    }
  }
})

test_that("build_adjacency has no self-loops", {
  parcels <- create_grid_parcels(3, 3)
  adj <- build_adjacency(parcels, quiet = TRUE)

  # No parcel should be adjacent to itself
  for (i in seq_along(adj)) {
    expect_false(
      i %in% adj[[i]],
      info = sprintf("Parcel %d has self-loop", i)
    )
  }
})

test_that("build_adjacency handles single parcel", {
  parcel <- sf::st_polygon(list(matrix(
    c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0),
    ncol = 2, byrow = TRUE
  )))

  parcels_sf <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_sfc(parcel, crs = 26986)
  )

  expect_warning(
    adj <- build_adjacency(parcels_sf, quiet = FALSE),
    "isolated"
  )

  expect_length(adj, 1)
  expect_length(adj[[1]], 0)  # No neighbors
})

test_that("build_adjacency validates inputs", {
  # Non-sf object
  expect_error(
    build_adjacency(data.frame(x = 1:3)),
    "must be an.*sf.*object"
  )

  # Empty sf object
  empty_sf <- sf::st_sf(
    parcel_id = integer(0),
    geometry = sf::st_sfc(crs = 26986)
  )
  expect_error(
    build_adjacency(empty_sf),
    "empty"
  )
})

test_that("build_adjacency handles invalid geometries", {
  # Create invalid geometry (self-intersecting polygon)
  invalid_polygon <- sf::st_polygon(list(matrix(
    c(0, 0, 100, 100, 100, 0, 0, 100, 0, 0),  # Bowtie shape
    ncol = 2, byrow = TRUE
  )))

  parcels_sf <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_sfc(invalid_polygon, crs = 26986)
  )

  # Should warn and attempt to repair
  expect_warning(
    adj <- build_adjacency(parcels_sf, quiet = TRUE),
    "invalid"
  )
})

test_that("build_adjacency with real data (Maynard)", {
  skip_if_not(file.exists(test_path("../../inst/extdata/parcels/163_MAYNARD_basic.zip")),
              "Maynard test data not available")

  maynard <- load_municipality(
    test_path("../../inst/extdata/parcels/163_MAYNARD_basic.zip"),
    community_name = "Maynard"
  )

  adj <- build_adjacency(maynard, quiet = TRUE)

  # Basic sanity checks
  expect_length(adj, nrow(maynard))
  expect_type(adj, "list")

  # Mean degree should be reasonable (parcels typically have 3-6 neighbors)
  mean_degree <- mean(sapply(adj, length))
  expect_true(mean_degree >= 2 && mean_degree <= 10)

  # Verify symmetry
  for (i in seq_along(adj)) {
    for (j in adj[[i]]) {
      expect_true(i %in% adj[[j]])
    }
  }
})

# ============================================================================
# get_adjacency_stats() tests
# ============================================================================

test_that("get_adjacency_stats calculates correct statistics", {
  parcels <- create_grid_parcels(3, 3)
  adj <- build_adjacency(parcels, quiet = TRUE)
  stats <- get_adjacency_stats(adj)

  expect_type(stats, "list")
  expect_named(stats, c(
    "n_parcels", "n_edges", "mean_degree", "median_degree",
    "max_degree", "min_degree", "n_isolated", "n_components"
  ))

  # 3x3 grid statistics
  expect_equal(stats$n_parcels, 9)
  expect_equal(stats$n_edges, 12)  # 4 horizontal + 4 vertical + 4 internal = 12
  expect_equal(stats$max_degree, 4)  # Center parcel has 4 neighbors
  expect_equal(stats$min_degree, 2)  # Corner parcels have 2 neighbors
  expect_equal(stats$n_isolated, 0)
  expect_equal(stats$n_components, 1)  # All connected
})

test_that("get_adjacency_stats detects multiple components", {
  parcels <- create_disconnected_parcels()
  adj <- build_adjacency(parcels, quiet = TRUE)
  stats <- get_adjacency_stats(adj)

  expect_equal(stats$n_components, 2)  # Two disconnected components
  expect_equal(stats$n_isolated, 2)    # Both parcels isolated
  expect_equal(stats$n_edges, 0)       # No edges
})

test_that("get_adjacency_stats validates input", {
  expect_error(
    get_adjacency_stats("not a list"),
    "must be a list"
  )
})

# ============================================================================
# identify_components() tests
# ============================================================================

test_that("identify_components finds single component", {
  parcels <- create_grid_parcels(3, 3)
  adj <- build_adjacency(parcels, quiet = TRUE)
  components <- identify_components(adj)

  expect_length(components, 9)
  expect_equal(unique(components), 1)  # All in component 1
})

test_that("identify_components finds multiple components", {
  parcels <- create_disconnected_parcels()
  adj <- build_adjacency(parcels, quiet = TRUE)
  components <- identify_components(adj)

  expect_length(components, 2)
  expect_equal(length(unique(components)), 2)  # Two components
  expect_true(all(components %in% c(1, 2)))
})

test_that("identify_components handles linear arrangement", {
  # Create linear chain: 1 -- 2 -- 3 -- 4
  parcels_list <- list()
  for (i in 1:4) {
    xmin <- (i - 1) * 100
    xmax <- i * 100
    parcel <- sf::st_polygon(list(matrix(
      c(xmin, 0, xmax, 0, xmax, 100, xmin, 100, xmin, 0),
      ncol = 2, byrow = TRUE
    )))
    parcels_list[[i]] <- parcel
  }

  parcels_sf <- sf::st_sf(
    parcel_id = 1:4,
    geometry = sf::st_sfc(parcels_list, crs = 26986)
  )

  adj <- build_adjacency(parcels_sf, quiet = TRUE)
  components <- identify_components(adj)

  expect_equal(unique(components), 1)  # All in one component
})

test_that("identify_components validates input", {
  expect_error(
    identify_components("not a list"),
    "must be a list"
  )
})

# ============================================================================
# calculate_component_totals() tests
# ============================================================================

test_that("calculate_component_totals computes correct totals", {
  # Create simple 2-component graph
  parcels <- rbind(
    create_grid_parcels(2, 2),  # Component 1: 4 parcels
    create_disconnected_parcels()  # Component 2: 2 disconnected parcels
  )

  # Shift second group to avoid overlap
  parcels$geometry[5:6] <- parcels$geometry[5:6] + c(1000, 1000)

  # Add capacity and area columns
  parcels$unit_capacity <- c(10, 20, 30, 40, 50, 60)
  parcels$SQFT <- c(5000, 5000, 5000, 5000, 10000, 10000)

  adj <- build_adjacency(parcels, quiet = TRUE)
  totals <- calculate_component_totals(parcels, adj)

  expect_type(totals, "list")
  expect_named(totals, c("component_id", "total_capacity", "total_area", "n_components"))

  # Component IDs should be assigned
  expect_length(totals$component_id, 6)

  # Should have 3 components (4 connected + 2 isolated)
  expect_equal(totals$n_components, 3)

  # Check totals for first component (parcels 1-4)
  comp1_id <- totals$component_id[1]
  expect_equal(totals$total_capacity[comp1_id], 10 + 20 + 30 + 40)
  expect_equal(totals$total_area[comp1_id], 4 * 5000 / 43560, tolerance = 0.01)
})

test_that("calculate_component_totals handles NA values", {
  parcels <- create_grid_parcels(2, 2)
  parcels$unit_capacity <- c(10, NA, 30, 40)
  parcels$SQFT <- c(5000, 5000, NA, 5000)

  adj <- build_adjacency(parcels, quiet = TRUE)
  totals <- calculate_component_totals(parcels, adj)

  # NA should be treated as 0 for conservative infeasibility checks
  comp_id <- totals$component_id[1]
  expect_equal(totals$total_capacity[comp_id], 10 + 0 + 30 + 40)
})

test_that("calculate_component_totals validates inputs", {
  parcels <- create_grid_parcels(2, 2)
  parcels$unit_capacity <- c(10, 20, 30, 40)
  parcels$SQFT <- c(5000, 5000, 5000, 5000)
  adj <- build_adjacency(parcels, quiet = TRUE)

  # Missing capacity column
  expect_error(
    calculate_component_totals(parcels, adj, capacity_col = "missing"),
    "not found"
  )

  # Missing area column
  expect_error(
    calculate_component_totals(parcels, adj, area_col = "missing"),
    "not found"
  )
})