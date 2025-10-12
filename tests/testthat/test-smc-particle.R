# Test: Particle State Tracking for SMC Algorithm
# Tests for R/smc_particle.R
#
# Tests Tier 1 (running sums), Tier 2 (infeasibility), and Tier 3 (completion)
# checking functions for SMC particle propagation.

# ==============================================================================
# Setup: Create Test Fixtures
# ==============================================================================

# Helper function to create minimal test map
create_test_map <- function(n_parcels = 10, community_type = "adjacent") {

  # Create simple grid of parcels
  # Build data frame first
  # Use "adjacent" by default to avoid station coverage requirements
  df <- data.frame(
    LOC_ID = paste0("TEST_", 1:n_parcels),
    SQFT = rep(10000, n_parcels),  # ~0.23 acres each
    unit_capacity = rep(5, n_parcels),  # 5 units each
    in_station_area = c(rep(1, min(9, n_parcels)), rep(0, max(0, n_parcels - 9))),  # First 9 in station area (90%)
    x = rep(1:ceiling(sqrt(n_parcels)), length.out = n_parcels),
    y = rep(1:ceiling(sqrt(n_parcels)), each = ceiling(n_parcels / ceiling(sqrt(n_parcels))), length.out = n_parcels)
  )

  # Convert to sf using coords parameter properly
  parcels_sf <- sf::st_as_sf(df, coords = c("x", "y"), crs = 26986)

  # Create simple adjacency (grid topology)
  adj <- lapply(1:n_parcels, function(i) {
    neighbors <- integer(0)
    # Grid neighbors (simplified)
    if (i > 1) neighbors <- c(neighbors, i - 1)
    if (i < n_parcels) neighbors <- c(neighbors, i + 1)
    neighbors
  })

  # Create minimal mbta_map
  component_info <- list(
    component_id = rep(1L, n_parcels),  # All in one component
    total_capacity = sum(parcels_sf$unit_capacity, na.rm = TRUE),
    total_area = sum(parcels_sf$SQFT, na.rm = TRUE) / 43560,
    n_components = 1L
  )

  map <- list(
    data = parcels_sf,
    ndists = 1L,
    capacity_col = "unit_capacity",
    target_capacity = 20,  # Need 20 units (4 parcels minimum)
    target_area = 1.0,     # Need 1 acre (~4.5 parcels)
    pop_tol = 0.25,
    community_type = community_type,
    community_name = "TestCity",
    station_area_unit_pct = if (community_type == "rapid_transit") 0.9 else 0.0,
    station_area_land_pct = if (community_type == "rapid_transit") 0.9 else 0.0,
    adj = adj,
    precomputed = FALSE,
    existing_plan = NULL,
    component_info = component_info
  )

  class(map) <- c("mbta_map", "list")
  return(map)
}

# ==============================================================================
# Test: new_particle_state()
# ==============================================================================

test_that("new_particle_state() initializes particle from seed parcel", {

  map <- create_test_map(n_parcels = 10)

  # Initialize from seed parcel 1
  particle <- new_particle_state(seed_parcel_id = 1, map = map, seed_prob = 1.0)

  # Check structure
  expect_s3_class(particle, "ParticleState")
  expect_true(is.list(particle))

  # Check zone composition
  expect_equal(particle$parcel_ids, 1L)
  expect_equal(particle$n_parcels, 1L)
  expect_equal(particle$component_id, 1L)

  # Check Tier 1 sums (seed parcel has 5 units, 10000 sqft)
  expect_equal(particle$total_capacity, 5)
  expect_equal(particle$total_area, 10000 / 43560)

  # Check station area (parcel 1 is in station area)
  expect_equal(particle$station_capacity, 5)
  expect_equal(particle$station_area, 10000 / 43560)

  # Check Tier 2 remaining (total - seed)
  expected_remaining_capacity <- sum(map$data$unit_capacity) - 5
  expected_remaining_area <- sum(map$data$SQFT) / 43560 - 10000 / 43560
  expect_equal(particle$remaining_capacity, expected_remaining_capacity)
  expect_equal(particle$remaining_area, expected_remaining_area)

  # Check RSIS tracking
  expect_equal(particle$log_proposal_path, log(1.0))
  expect_true(is.numeric(particle$valuation_score))
  expect_true(particle$active)

  # Check status
  expect_equal(particle$n_steps, 0L)
})

test_that("new_particle_state() handles biased seed selection", {

  map <- create_test_map(n_parcels = 10)

  # Seed with probability 0.5
  particle <- new_particle_state(seed_parcel_id = 2, map = map, seed_prob = 0.5)

  # Log proposal path should account for seed probability
  expect_equal(particle$log_proposal_path, log(0.5))
})

test_that("new_particle_state() handles NA capacity values", {

  map <- create_test_map(n_parcels = 10)
  map$data$unit_capacity[1] <- NA_real_

  # Should treat NA as 0
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  expect_equal(particle$total_capacity, 0)
})

test_that("new_particle_state() validates inputs", {

  map <- create_test_map(n_parcels = 10)

  # Invalid seed_parcel_id
  expect_error(
    new_particle_state(seed_parcel_id = 0, map = map),
    "must be between 1 and 10"
  )
  expect_error(
    new_particle_state(seed_parcel_id = 11, map = map),
    "must be between 1 and 10"
  )

  # Invalid seed_prob
  expect_error(
    new_particle_state(seed_parcel_id = 1, map = map, seed_prob = 0),
    "must be between 0 and 1"
  )
  expect_error(
    new_particle_state(seed_parcel_id = 1, map = map, seed_prob = 1.5),
    "must be between 0 and 1"
  )

  # Invalid map
  expect_error(
    new_particle_state(seed_parcel_id = 1, map = list()),
    "must be an.*mbta_map"
  )
})

# ==============================================================================
# Test: add_parcel_to_particle()
# ==============================================================================

test_that("add_parcel_to_particle() updates running sums correctly", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Record initial state
  initial_capacity <- particle$total_capacity
  initial_area <- particle$total_area
  initial_remaining_capacity <- particle$remaining_capacity
  initial_remaining_area <- particle$remaining_area

  # Add parcel 2 (also in station area)
  proposal_prob <- 0.5
  particle <- add_parcel_to_particle(particle, parcel_id = 2, map = map, proposal_prob = proposal_prob)

  # Check zone composition updated
  expect_equal(particle$parcel_ids, c(1L, 2L))
  expect_equal(particle$n_parcels, 2L)

  # Check Tier 1 totals incremented
  expect_equal(particle$total_capacity, initial_capacity + 5)  # Parcel 2 has 5 units
  expect_equal(particle$total_area, initial_area + 10000 / 43560)

  # Check Tier 2 remaining decremented
  expect_equal(particle$remaining_capacity, initial_remaining_capacity - 5)
  expect_equal(particle$remaining_area, initial_remaining_area - 10000 / 43560)

  # Check log proposal path updated
  expected_log_path <- log(1.0) + log(proposal_prob)
  expect_equal(particle$log_proposal_path, expected_log_path)

  # Check steps incremented
  expect_equal(particle$n_steps, 1L)
})

test_that("add_parcel_to_particle() handles station area correctly", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  initial_station_capacity <- particle$station_capacity
  initial_station_area <- particle$station_area

  # Add parcel 2 (in station area)
  particle <- add_parcel_to_particle(particle, parcel_id = 2, map = map, proposal_prob = 0.5)

  # Station metrics should increase
  expect_equal(particle$station_capacity, initial_station_capacity + 5)
  expect_equal(particle$station_area, initial_station_area + 10000 / 43560)

  # Add parcel 6 (NOT in station area)
  particle <- add_parcel_to_particle(particle, parcel_id = 6, map = map, proposal_prob = 0.5)

  # Station metrics should NOT increase
  expect_equal(particle$station_capacity, initial_station_capacity + 5)
  expect_equal(particle$station_area, initial_station_area + 10000 / 43560)
})

test_that("add_parcel_to_particle() handles NA values", {

  map <- create_test_map(n_parcels = 10)
  map$data$unit_capacity[2] <- NA_real_
  map$data$SQFT[2] <- NA_real_

  particle <- new_particle_state(seed_parcel_id = 1, map = map)
  initial_capacity <- particle$total_capacity
  initial_area <- particle$total_area

  # Add parcel with NA values (treated as 0)
  particle <- add_parcel_to_particle(particle, parcel_id = 2, map = map, proposal_prob = 0.5)

  # Totals should not increase (NA treated as 0)
  expect_equal(particle$total_capacity, initial_capacity)
  expect_equal(particle$total_area, initial_area)
})

test_that("add_parcel_to_particle() validates inputs", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Invalid parcel_id
  expect_error(
    add_parcel_to_particle(particle, parcel_id = 0, map = map, proposal_prob = 0.5),
    "must be between 1 and 10"
  )

  # Parcel already in zone
  expect_error(
    add_parcel_to_particle(particle, parcel_id = 1, map = map, proposal_prob = 0.5),
    "already in zone"
  )

  # Invalid proposal_prob
  expect_error(
    add_parcel_to_particle(particle, parcel_id = 2, map = map, proposal_prob = 0),
    "must be between 0 and 1"
  )
})

# ==============================================================================
# Test: check_infeasibility()
# ==============================================================================

test_that("check_infeasibility() detects capacity infeasibility", {

  map <- create_test_map(n_parcels = 10)
  map$target_capacity <- 100  # Impossible to reach with 10 parcels × 5 units

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Should be infeasible (max = 50 units, need 100)
  result <- check_infeasibility(particle, map)

  expect_true(result$infeasible)
  expect_equal(result$reason, "capacity")
})

test_that("check_infeasibility() detects area infeasibility", {

  map <- create_test_map(n_parcels = 10)
  map$target_area <- 10.0  # Impossible to reach with 10 parcels × 0.23 acres

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Should be infeasible (max = 2.3 acres, need 10)
  result <- check_infeasibility(particle, map)

  expect_true(result$infeasible)
  expect_equal(result$reason, "area")
})

test_that("check_infeasibility() detects station coverage infeasibility", {

  map <- create_test_map(n_parcels = 10, community_type = "rapid_transit")
  # Rapid transit requires 90% station coverage
  # Only 5/10 parcels are in station area

  # Start from parcel 6 (NOT in station area)
  particle <- new_particle_state(seed_parcel_id = 6, map = map)

  # Add more parcels outside station area
  particle <- add_parcel_to_particle(particle, 7, map, 0.5)
  particle <- add_parcel_to_particle(particle, 8, map, 0.5)
  particle <- add_parcel_to_particle(particle, 9, map, 0.5)
  particle <- add_parcel_to_particle(particle, 10, map, 0.5)

  # Now particle has 5 parcels outside station area
  # Max station coverage = 5/10 = 50% (need 90%)
  result <- check_infeasibility(particle, map)

  expect_true(result$infeasible)
  expect_equal(result$reason, "station_coverage")
})

test_that("check_infeasibility() detects gross density infeasibility", {

  map <- create_test_map(n_parcels = 10)

  # Create low-density parcels (large area, low capacity)
  map$data$SQFT <- rep(100000, 10)  # 2.3 acres each
  map$data$unit_capacity <- rep(1, 10)  # 1 unit each

  # Rebuild component totals after changing data
  map$component_info <- calculate_component_totals(
    parcels = map$data,
    adj = map$adj,
    capacity_col = "unit_capacity",
    area_col = "SQFT"
  )

  # Max density = 10 units / 23 acres = 0.43 units/acre (need 15)

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  result <- check_infeasibility(particle, map, check_density = TRUE)

  expect_true(result$infeasible)
  expect_equal(result$reason, "gross_density")
})

test_that("check_infeasibility() passes when feasible", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Should be feasible (can reach 50 units, 2.3 acres)
  result <- check_infeasibility(particle, map)

  expect_false(result$infeasible)
  expect_true(is.na(result$reason))
})

test_that("check_infeasibility() handles NA target_area", {

  map <- create_test_map(n_parcels = 10, community_type = "adjacent_small_town")
  map$target_area <- NA_real_  # Small towns have no area requirement

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Should not fail on area check
  result <- check_infeasibility(particle, map)

  expect_false(result$infeasible)
})

test_that("check_infeasibility() can disable density check", {

  map <- create_test_map(n_parcels = 10)
  map$data$SQFT <- rep(100000, 10)  # Low density
  map$data$unit_capacity <- rep(1, 10)

  # Rebuild component totals
  map$component_info <- calculate_component_totals(
    parcels = map$data,
    adj = map$adj,
    capacity_col = "unit_capacity",
    area_col = "SQFT"
  )

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # With density check disabled, should pass
  result <- check_infeasibility(particle, map, check_density = FALSE)

  expect_false(result$infeasible)
})

# ==============================================================================
# Test: check_completion()
# ==============================================================================

test_that("check_completion() detects when minimums are met", {

  map <- create_test_map(n_parcels = 10)
  map$target_capacity <- 15  # Need 15 units (3 parcels)
  map$target_area <- 0.5     # Need 0.5 acres (~2.2 parcels)

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Initially incomplete
  result <- check_completion(particle, map)
  expect_false(result$complete)

  # Add parcels to meet minimums
  particle <- add_parcel_to_particle(particle, 2, map, 0.5)  # 10 units, 0.46 acres
  particle <- add_parcel_to_particle(particle, 3, map, 0.5)  # 15 units, 0.69 acres

  # Now should be complete
  result <- check_completion(particle, map)
  expect_true(result$complete)
  expect_true(result$capacity_met)
  expect_true(result$area_met)
})

test_that("check_completion() checks both capacity and area", {

  map <- create_test_map(n_parcels = 10)
  map$target_capacity <- 30  # Need 30 units
  map$target_area <- 0.5     # Need 0.5 acres

  particle <- new_particle_state(seed_parcel_id = 1, map = map)
  particle <- add_parcel_to_particle(particle, 2, map, 0.5)
  particle <- add_parcel_to_particle(particle, 3, map, 0.5)

  # Has 15 units, 0.69 acres (capacity not met, area met)
  result <- check_completion(particle, map)

  expect_false(result$complete)
  expect_false(result$capacity_met)
  expect_true(result$area_met)
})

test_that("check_completion() handles NA target_area", {

  map <- create_test_map(n_parcels = 10)
  map$target_capacity <- 15
  map$target_area <- NA_real_  # No area requirement

  particle <- new_particle_state(seed_parcel_id = 1, map = map)
  particle <- add_parcel_to_particle(particle, 2, map, 0.5)
  particle <- add_parcel_to_particle(particle, 3, map, 0.5)

  # Should be complete (capacity met, area not required)
  result <- check_completion(particle, map)

  expect_true(result$complete)
  expect_true(result$capacity_met)
  expect_true(result$area_met)  # Should be TRUE when not required
})

# ==============================================================================
# Test: calculate_valuation()
# ==============================================================================

test_that("calculate_valuation() computes V(x) correctly", {

  map <- create_test_map(n_parcels = 10, community_type = "adjacent")
  map$target_capacity <- 20  # Need 20 units

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Reachable capacity = 5 (current) + 45 (remaining) = 50
  # Capacity ratio = 50 / 20 = 2.5
  # No station requirement, so V(x) = 2.5^1 = 2.5
  valuation <- calculate_valuation(particle, map, alpha_V = 1.0)

  expected <- 50 / 20
  expect_equal(valuation, expected)
})

test_that("calculate_valuation() includes station coverage for rapid transit", {

  map <- create_test_map(n_parcels = 10, community_type = "rapid_transit")
  map$target_capacity <- 20

  # Start from station area
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Valuation should include station coverage component
  valuation <- calculate_valuation(particle, map, alpha_V = 1.0, beta_V = 1.0)

  # Should be > 0
  expect_true(valuation > 0)
  expect_true(is.finite(valuation))
})

test_that("calculate_valuation() returns 0 for zero capacity component", {

  map <- create_test_map(n_parcels = 10)
  map$data$unit_capacity <- rep(0, 10)  # No capacity

  # Rebuild component totals
  map$component_info <- calculate_component_totals(
    parcels = map$data,
    adj = map$adj,
    capacity_col = "unit_capacity",
    area_col = "SQFT"
  )

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  valuation <- calculate_valuation(particle, map)

  expect_equal(valuation, 0)
})

test_that("calculate_valuation() is non-negative", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Add several parcels
  for (i in 2:5) {
    particle <- add_parcel_to_particle(particle, i, map, 0.5)
  }

  valuation <- calculate_valuation(particle, map)

  expect_true(valuation >= 0)
})

# ==============================================================================
# Test: get_boundary_parcels()
# ==============================================================================

test_that("get_boundary_parcels() finds adjacent parcels", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  boundary <- get_boundary_parcels(particle, map)

  # Parcel 1 is adjacent to parcel 2 (grid topology)
  expect_true(2L %in% boundary)

  # Parcel 1 should not be in boundary (already in zone)
  expect_false(1L %in% boundary)
})

test_that("get_boundary_parcels() updates as zone grows", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Initial boundary
  boundary1 <- get_boundary_parcels(particle, map)

  # Add a parcel
  particle <- add_parcel_to_particle(particle, 2, map, 0.5)

  # New boundary (parcel 2 no longer boundary, parcel 3 is now)
  boundary2 <- get_boundary_parcels(particle, map)

  expect_false(2L %in% boundary2)
  expect_true(3L %in% boundary2)
})

test_that("get_boundary_parcels() returns empty for isolated particle", {

  # Create map with disconnected parcels
  map <- create_test_map(n_parcels = 5)
  map$adj <- lapply(1:5, function(i) integer(0))  # No adjacencies

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  boundary <- get_boundary_parcels(particle, map)

  expect_equal(length(boundary), 0)
})

# ==============================================================================
# Test: copy_particle_state()
# ==============================================================================

test_that("copy_particle_state() creates deep copy", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)
  particle <- add_parcel_to_particle(particle, 2, map, 0.5)

  # Create copy
  copy <- copy_particle_state(particle)

  # Check all state is copied
  expect_equal(copy$parcel_ids, particle$parcel_ids)
  expect_equal(copy$n_parcels, particle$n_parcels)
  expect_equal(copy$total_capacity, particle$total_capacity)
  expect_equal(copy$log_proposal_path, particle$log_proposal_path)

  # Modify original
  particle <- add_parcel_to_particle(particle, 3, map, 0.5)

  # Copy should not be affected
  expect_equal(length(copy$parcel_ids), 2L)
  expect_equal(length(particle$parcel_ids), 3L)
})

test_that("copy_particle_state() preserves all RSIS state", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Set specific log_proposal_path
  particle$log_proposal_path <- -2.5

  copy <- copy_particle_state(particle)

  # CRITICAL: log_proposal_path must be copied
  expect_equal(copy$log_proposal_path, -2.5)
})

# ==============================================================================
# Test: Print Method
# ==============================================================================

test_that("print.ParticleState() displays particle information", {

  map <- create_test_map(n_parcels = 10)
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Capture output
  output <- capture.output(print(particle))
  combined_output <- paste(output, collapse = "\n")

  # Should contain key information
  expect_true(grepl("Particle State", combined_output))
  expect_true(grepl("Active|Dead", combined_output))
})

# ==============================================================================
# Test: Integration - Complete Particle Lifecycle
# ==============================================================================

test_that("particle can propagate from seed to completion", {

  map <- create_test_map(n_parcels = 10)
  map$target_capacity <- 20  # Need 20 units (4 parcels)
  map$target_area <- 1.0     # Need 1 acre (~4.5 parcels)

  # Initialize
  particle <- new_particle_state(seed_parcel_id = 1, map = map)
  expect_true(particle$active)

  # Propagate until complete
  max_steps <- 20
  for (step in 1:max_steps) {

    # Check infeasibility
    feasibility <- check_infeasibility(particle, map)
    if (feasibility$infeasible) {
      fail(paste("Particle died unexpectedly:", feasibility$reason))
    }

    # Check completion
    completion <- check_completion(particle, map)
    if (completion$complete) {
      # Success!
      expect_true(particle$total_capacity >= map$target_capacity)
      expect_true(particle$total_area >= map$target_area)
      break
    }

    # Add next parcel
    boundary <- get_boundary_parcels(particle, map)
    if (length(boundary) == 0) {
      fail("Ran out of boundary parcels before completion")
    }

    next_parcel <- boundary[1]
    particle <- add_parcel_to_particle(particle, next_parcel, map, 1 / length(boundary))
  }

  # Should have completed within max_steps
  completion <- check_completion(particle, map)
  expect_true(completion$complete)
})

test_that("particle dies when capacity becomes infeasible", {

  map <- create_test_map(n_parcels = 5)
  map$target_capacity <- 100  # Impossible with 5 parcels × 5 units

  # Start and add all parcels
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Eventually should detect infeasibility
  became_infeasible <- FALSE
  for (i in 2:5) {
    particle <- add_parcel_to_particle(particle, i, map, 0.5)

    feasibility <- check_infeasibility(particle, map)
    if (feasibility$infeasible) {
      became_infeasible <- TRUE
      expect_equal(feasibility$reason, "capacity")
      break
    }
  }

  # Should have detected infeasibility before adding all parcels
  expect_true(became_infeasible)
})

# ==============================================================================
# Test: Edge Cases
# ==============================================================================

test_that("particle handles all NA capacities in component", {

  map <- create_test_map(n_parcels = 5)
  map$data$unit_capacity <- rep(NA_real_, 5)

  # Rebuild component totals (will treat NA as 0)
  map$component_info <- calculate_component_totals(
    parcels = map$data,
    adj = map$adj,
    capacity_col = "unit_capacity",
    area_col = "SQFT"
  )

  # Should not error, but will have zero capacity
  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  expect_equal(particle$total_capacity, 0)
  expect_equal(particle$remaining_capacity, 0)

  # Valuation should be 0
  valuation <- calculate_valuation(particle, map)
  expect_equal(valuation, 0)
})

test_that("particle handles single-parcel component", {

  map <- create_test_map(n_parcels = 5)
  # Make parcel 1 isolated
  map$adj[[1]] <- integer(0)

  # Rebuild component info (parcel 1 now in its own component)
  map$component_info <- calculate_component_totals(
    parcels = map$data,
    adj = map$adj,
    capacity_col = "unit_capacity",
    area_col = "SQFT"
  )

  particle <- new_particle_state(seed_parcel_id = 1, map = map)

  # Boundary should be empty
  boundary <- get_boundary_parcels(particle, map)
  expect_equal(length(boundary), 0)

  # Should be immediately infeasible if minimums > 1 parcel
  # Parcel 1 has 5 units, target is 20 units
  if (map$target_capacity > 5) {
    feasibility <- check_infeasibility(particle, map)
    expect_true(feasibility$infeasible)
  }
})
