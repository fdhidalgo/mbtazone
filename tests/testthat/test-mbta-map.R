# Tests for mbta_map S3 Class

# Helper function to create minimal test parcels
create_test_parcels <- function(n = 10, community_name = NULL) {
  # Create simple grid of parcels
  n_cols <- ceiling(sqrt(n))
  n_rows <- ceiling(n / n_cols)

  parcels_list <- list()
  id <- 1

  for (row in 1:n_rows) {
    for (col in 1:n_cols) {
      if (id > n) break

      xmin <- (col - 1) * 100
      xmax <- col * 100
      ymin <- (row - 1) * 100
      ymax <- row * 100

      parcel <- sf::st_polygon(list(matrix(
        c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
        ncol = 2, byrow = TRUE
      )))

      parcels_list[[id]] <- parcel
      id <- id + 1
    }
  }

  # Create sf object with required columns
  parcels_sf <- sf::st_sf(
    LOC_ID = 1:n,
    SQFT = rep(10000, n),
    ACRES = rep(10000 / 43560, n),
    TRANSIT = rep("N", n),
    unit_capacity = seq(10, 10 + (n-1) * 5, by = 5),
    geometry = sf::st_sfc(parcels_list, crs = 26986)
  )

  # Add community_name attribute only if provided
  # (avoid conflicts with real communities in community_info.csv)
  if (!is.null(community_name)) {
    attr(parcels_sf, "community_name") <- community_name
  }

  return(parcels_sf)
}

# ============================================================================
# mbta_map() constructor tests
# ============================================================================

test_that("mbta_map creates valid object with minimal inputs", {
  parcels <- create_test_parcels(10)

  map <- mbta_map(
    data = parcels,
    community_type = "commuter_rail"
  )

  # Check class
  expect_s3_class(map, "mbta_map")
  expect_type(map, "list")

  # Check required components
  expect_true(all(c(
    "data", "ndists", "capacity_col", "target_capacity", "target_area",
    "pop_tol", "community_type", "adj", "precomputed"
  ) %in% names(map)))

  # Check defaults
  expect_equal(map$ndists, 1)
  expect_equal(map$capacity_col, "unit_capacity")
  expect_equal(map$pop_tol, 0.25)
  expect_equal(map$community_type, "commuter_rail")
  expect_false(map$precomputed)

  # Check adjacency graph was built
  expect_type(map$adj, "list")
  expect_length(map$adj, 10)
})

test_that("mbta_map works with all community types", {
  parcels <- create_test_parcels(10)

  for (ctype in c("rapid_transit", "commuter_rail", "adjacent", "adjacent_small_town")) {
    map <- mbta_map(
      data = parcels,
      community_type = ctype
    )

    expect_s3_class(map, "mbta_map")
    expect_equal(map$community_type, ctype)

    # Check station requirements vary by type
    if (ctype == "rapid_transit") {
      expect_equal(map$station_area_unit_pct, 0.90)
    } else if (ctype %in% c("adjacent", "adjacent_small_town")) {
      expect_equal(map$station_area_unit_pct, 0)
    }
  }
})

test_that("mbta_map accepts pre-built adjacency graph", {
  parcels <- create_test_parcels(10)
  adj <- build_adjacency(parcels, quiet = TRUE)

  map <- mbta_map(
    data = parcels,
    community_type = "commuter_rail",
    adj = adj
  )

  expect_identical(map$adj, adj)
})

test_that("mbta_map accepts existing plan", {
  parcels <- create_test_parcels(10)
  existing <- c(rep(1, 5), rep(0, 5))  # First 5 parcels in district

  map <- mbta_map(
    data = parcels,
    community_type = "commuter_rail",
    existing_plan = existing
  )

  expect_equal(map$existing_plan, existing)
})

test_that("mbta_map handles precomputed flag", {
  parcels <- create_test_parcels(10)

  map <- mbta_map(
    data = parcels,
    community_type = "commuter_rail",
    precomputed = TRUE
  )

  expect_true(map$precomputed)
})

test_that("mbta_map with real data (Maynard)", {
  skip_if_not(file.exists(test_path("../../inst/extdata/parcels/163_MAYNARD_basic.zip")),
              "Maynard test data not available")

  maynard <- load_municipality(
    test_path("../../inst/extdata/parcels/163_MAYNARD_basic.zip"),
    community_name = "Maynard"
  )

  map <- mbta_map(
    data = maynard,
    community_type = "commuter_rail"
  )

  expect_s3_class(map, "mbta_map")
  expect_equal(map$community_name, "Maynard")
  expect_length(map$adj, nrow(maynard))

  # Should have valid component info
  expect_equal(
    length(map$component_info$component_id),
    nrow(maynard)
  )
})

# ============================================================================
# Input validation tests
# ============================================================================

test_that("mbta_map validates data is sf object", {
  expect_error(
    mbta_map(data.frame(x = 1:10), community_type = "commuter_rail"),
    "must be an.*sf.*object"
  )
})

test_that("mbta_map validates CRS is EPSG:26986", {
  parcels <- create_test_parcels(10)

  # Transform to wrong CRS
  parcels_wrong_crs <- sf::st_transform(parcels, 4326)

  expect_error(
    mbta_map(parcels_wrong_crs, community_type = "commuter_rail"),
    "must be in EPSG:26986"
  )
})

test_that("mbta_map validates capacity column exists", {
  parcels <- create_test_parcels(10)

  expect_error(
    mbta_map(
      parcels,
      community_type = "commuter_rail",
      total_capacity = "missing_column"
    ),
    "not found"
  )
})

test_that("mbta_map validates community_type", {
  parcels <- create_test_parcels(10)

  expect_error(
    mbta_map(parcels, community_type = "invalid_type"),
    "should be one of"
  )
})

test_that("mbta_map enforces ndists = 1 (Phase 1 limitation)", {
  parcels <- create_test_parcels(10)

  expect_error(
    mbta_map(parcels, community_type = "commuter_rail", ndists = 2),
    "single-component districts only"
  )

  expect_error(
    mbta_map(parcels, community_type = "commuter_rail", ndists = 0),
    "positive integer"
  )

  expect_error(
    mbta_map(parcels, community_type = "commuter_rail", ndists = 1.5),
    "positive integer"
  )
})

test_that("mbta_map validates pop_tol range", {
  parcels <- create_test_parcels(10)

  expect_error(
    mbta_map(parcels, community_type = "commuter_rail", pop_tol = -0.1),
    "between 0 and 1"
  )

  expect_error(
    mbta_map(parcels, community_type = "commuter_rail", pop_tol = 1.5),
    "between 0 and 1"
  )
})

test_that("mbta_map validates existing_plan length", {
  parcels <- create_test_parcels(10)

  expect_error(
    mbta_map(
      parcels,
      community_type = "commuter_rail",
      existing_plan = c(1, 0, 1)  # Wrong length
    ),
    "length must match"
  )
})

test_that("mbta_map validates adjacency list", {
  parcels <- create_test_parcels(10)

  # Non-list adjacency
  expect_error(
    mbta_map(
      parcels,
      community_type = "commuter_rail",
      adj = matrix(0, 10, 10)
    ),
    "must be a list"
  )

  # Wrong length adjacency
  expect_error(
    mbta_map(
      parcels,
      community_type = "commuter_rail",
      adj = list(1, 2, 3)  # Only 3 elements, not 10
    ),
    "length must match"
  )
})

test_that("mbta_map rejects empty data", {
  empty_sf <- sf::st_sf(
    LOC_ID = integer(0),
    unit_capacity = numeric(0),
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_error(
    mbta_map(empty_sf, community_type = "commuter_rail"),
    "empty"
  )
})

# ============================================================================
# Community requirements loading tests
# ============================================================================

test_that("load_community_requirements finds known communities", {
  # Chelsea should be in community_info.csv
  skip_if_not(file.exists(system.file("extdata", "community_info.csv", package = "mbtazone")),
              "community_info.csv not found")

  reqs <- load_community_requirements("Chelsea", "rapid_transit")

  expect_type(reqs, "list")
  expect_named(reqs, c(
    "target_capacity", "target_area",
    "station_area_unit_pct", "station_area_land_pct"
  ))

  # Chelsea is rapid transit, should have specific requirements
  expect_true(reqs$target_capacity > 0)
  expect_true(reqs$target_area > 0)
  expect_true(reqs$station_area_unit_pct > 0)
})

test_that("load_community_requirements handles case-insensitive matching", {
  skip_if_not(file.exists(system.file("extdata", "community_info.csv", package = "mbtazone")),
              "community_info.csv not found")

  reqs1 <- load_community_requirements("CHELSEA", "rapid_transit")
  reqs2 <- load_community_requirements("chelsea", "rapid_transit")
  reqs3 <- load_community_requirements("Chelsea", "rapid_transit")

  expect_equal(reqs1, reqs2)
  expect_equal(reqs2, reqs3)
})

test_that("load_community_requirements falls back to defaults for unknown community", {
  # Use a very obviously fake name that won't match anything
  suppressWarnings({
    reqs <- load_community_requirements("ZZZ_NonexistentTown_999", "commuter_rail")
  })

  expect_type(reqs, "list")
  expect_true(reqs$target_capacity > 0)
})

test_that("get_default_requirements returns sensible defaults", {
  defaults_rt <- get_default_requirements("rapid_transit")
  expect_equal(defaults_rt$station_area_unit_pct, 0.90)
  expect_true(defaults_rt$target_capacity > 0)

  defaults_adj <- get_default_requirements("adjacent")
  expect_equal(defaults_adj$station_area_unit_pct, 0)

  defaults_small <- get_default_requirements("adjacent_small_town")
  expect_true(is.na(defaults_small$target_area))  # Not required
})

# ============================================================================
# Component calculation tests
# ============================================================================

test_that("mbta_map calculates component info correctly", {
  # Create two disconnected groups (use offset to avoid CRS issues)
  group1 <- create_test_parcels(5)

  # Create group2 with shifted geometry in one step
  parcels_list2 <- list()
  for (i in 1:3) {
    xmin <- (i - 1) * 100 + 10000
    xmax <- i * 100 + 10000
    ymin <- 0 + 10000
    ymax <- 100 + 10000

    parcel <- sf::st_polygon(list(matrix(
      c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
      ncol = 2, byrow = TRUE
    )))

    parcels_list2[[i]] <- parcel
  }

  group2 <- sf::st_sf(
    LOC_ID = 6:8,
    SQFT = rep(10000, 3),
    ACRES = rep(10000 / 43560, 3),
    TRANSIT = rep("N", 3),
    unit_capacity = 1:3,
    geometry = sf::st_sfc(parcels_list2, crs = 26986)
  )

  # Combine (both have same CRS now)
  parcels <- rbind(group1, group2)
  parcels$unit_capacity <- 1:8

  map <- mbta_map(parcels, community_type = "commuter_rail")

  # Should have 2 components (group1 connected, group2 connected)
  # Plus possibly some isolated parcels depending on grid layout
  expect_true(map$component_info$n_components >= 2)

  # Component IDs should be assigned
  expect_length(map$component_info$component_id, 8)

  # Component totals should be calculated
  expect_length(map$component_info$total_capacity, map$component_info$n_components)
  expect_length(map$component_info$total_area, map$component_info$n_components)
})

# ============================================================================
# Print and summary method tests
# ============================================================================

test_that("print.mbta_map displays correctly", {
  parcels <- create_test_parcels(10)
  map <- mbta_map(parcels, community_type = "rapid_transit")

  # Should not error (cli messages don't always get captured by expect_output)
  expect_no_error(print(map))
})

test_that("summary.mbta_map provides extended statistics", {
  parcels <- create_test_parcels(20)
  map <- mbta_map(parcels, community_type = "commuter_rail")

  # Should not error (cli messages don't always get captured by expect_output)
  expect_no_error(summary(map))
})

test_that("print.mbta_map shows precomputation status", {
  parcels <- create_test_parcels(10)

  map_precomp <- mbta_map(parcels, community_type = "commuter_rail", precomputed = TRUE)
  expect_no_error(print(map_precomp))
  expect_true(map_precomp$precomputed)

  map_no_precomp <- mbta_map(parcels, community_type = "commuter_rail", precomputed = FALSE)
  expect_no_error(print(map_no_precomp))
  expect_false(map_no_precomp$precomputed)
})

test_that("print.mbta_map handles disconnected components", {
  # Create disconnected parcels
  group1 <- create_test_parcels(3)

  # Create group2 with shifted geometry in one step
  parcels_list2 <- list()
  for (i in 1:2) {
    xmin <- (i - 1) * 100 + 10000
    xmax <- i * 100 + 10000
    ymin <- 0 + 10000
    ymax <- 100 + 10000

    parcel <- sf::st_polygon(list(matrix(
      c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
      ncol = 2, byrow = TRUE
    )))

    parcels_list2[[i]] <- parcel
  }

  group2 <- sf::st_sf(
    LOC_ID = 4:5,
    SQFT = rep(10000, 2),
    ACRES = rep(10000 / 43560, 2),
    TRANSIT = rep("N", 2),
    unit_capacity = seq(10, 15, by = 5),
    geometry = sf::st_sfc(parcels_list2, crs = 26986)
  )

  # Combine (both have same CRS)
  parcels <- rbind(group1, group2)

  map <- mbta_map(parcels, community_type = "commuter_rail")

  # Should handle disconnected components without error
  expect_no_error(print(map))

  # Should detect multiple components
  expect_true(map$component_info$n_components >= 2)
})

# ============================================================================
# Data storage tests
# ============================================================================

test_that("mbta_map stores data as sf object", {
  parcels <- create_test_parcels(10)
  map <- mbta_map(parcels, community_type = "commuter_rail")

  # Should be sf
  expect_s3_class(map$data, "sf")

  # Should work with standard subsetting
  expect_no_error({
    subset <- map$data[map$data$unit_capacity > 20, ]
  })
})

# ============================================================================
# validate_mbta_map() tests
# ============================================================================

test_that("validate_mbta_map accepts valid objects", {
  parcels <- create_test_parcels(10)
  map <- mbta_map(parcels, community_type = "commuter_rail")

  expect_true(validate_mbta_map(map))
})

test_that("validate_mbta_map rejects invalid objects", {
  # Non-mbta_map object
  expect_error(
    validate_mbta_map(list(a = 1)),
    "must be an.*mbta_map"
  )

  # Missing required components
  incomplete_map <- list(data = create_test_parcels(10))
  class(incomplete_map) <- c("mbta_map", "list")

  expect_error(
    validate_mbta_map(incomplete_map),
    "missing required components"
  )

  # Inconsistent adjacency length
  parcels <- create_test_parcels(10)
  map <- mbta_map(parcels, community_type = "commuter_rail")
  map$adj <- list(1, 2, 3)  # Wrong length

  expect_error(
    validate_mbta_map(map),
    "does not match number of parcels"
  )
})

# ============================================================================
# Integration tests
# ============================================================================

test_that("mbta_map workflow with precomputation (Chelsea)", {
  skip_if_not(file.exists(test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip")),
              "Chelsea test data not available")

  # Load municipality
  chelsea <- load_municipality(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    community_name = "Chelsea"
  )

  # Note: Chelsea data doesn't have unit_capacity column pre-calculated
  # For this test, we'll use create_test_parcels with community name
  parcels <- create_test_parcels(10, community_name = "Chelsea")

  # Create map without precomputation
  map1 <- mbta_map(
    parcels,
    community_type = "rapid_transit",
    precomputed = FALSE
  )

  expect_s3_class(map1, "mbta_map")
  expect_false(map1$precomputed)

  # Create map with precomputation flag
  # (Note: actual precompute_spatial_attributes() not called here,
  # just testing flag handling)
  map2 <- mbta_map(
    parcels,
    community_type = "rapid_transit",
    precomputed = TRUE
  )

  expect_s3_class(map2, "mbta_map")
  expect_true(map2$precomputed)
})

test_that("mbta_map end-to-end with existing plan", {
  parcels <- create_test_parcels(20)

  # Create synthetic existing plan (first 10 parcels in district)
  existing_plan <- c(rep(1, 10), rep(0, 10))

  map <- mbta_map(
    parcels,
    community_type = "commuter_rail",
    existing_plan = existing_plan
  )

  expect_equal(map$existing_plan, existing_plan)

  # Print should work with reference plan
  expect_no_error(print(map))
})