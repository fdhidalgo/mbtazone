# Excel Regression Tests
#
# Tests that validate R package calculations against Excel compliance model outputs

test_that("Chelsea District 1: calculate_developable_area matches Excel", {
  # Load cached reference data
  ref_data <- readRDS(test_path("fixtures/chelsea_district1.rds"))

  calc <- ref_data$calculations

  # Run R package calculation
  # Note: Using a reasonable min_lot_size of 5000 sq ft (common zoning requirement)
  r_result <- calculate_developable_area(
    lot_area = calc$parcel_sf,
    excluded_area = calc$total_excluded_land,
    min_lot_size = 5000
  )

  # Compare with Excel column "Developable Parcel sf"
  # Use tolerance for floating point comparison
  expect_equal(
    r_result,
    calc$developable_parcel_sf,
    tolerance = 1,  # Within 1 sq ft due to rounding
    info = "Chelsea District 1 developable area calculation"
  )
})

test_that("Somerville District 1: calculate_developable_area matches Excel", {
  ref_data <- readRDS(test_path("fixtures/somerville_district1.rds"))
  calc <- ref_data$calculations

  r_result <- calculate_developable_area(
    lot_area = calc$parcel_sf,
    excluded_area = calc$total_excluded_land,
    min_lot_size = 5000
  )

  expect_equal(
    r_result,
    calc$developable_parcel_sf,
    tolerance = 1,
    info = "Somerville District 1 developable area calculation"
  )
})

test_that("Cambridge District 1: calculate_developable_area matches Excel", {
  ref_data <- readRDS(test_path("fixtures/cambridge_district1.rds"))
  calc <- ref_data$calculations

  r_result <- calculate_developable_area(
    lot_area = calc$parcel_sf,
    excluded_area = calc$total_excluded_land,
    min_lot_size = 5000
  )

  expect_equal(
    r_result,
    calc$developable_parcel_sf,
    tolerance = 1,
    info = "Cambridge District 1 developable area calculation"
  )
})

# NOTE: Final unit capacity tests require all 7 unit calculation methods to be
# implemented. These tests are commented out until those functions are available.

# test_that("Chelsea District 1: calculate_final_unit_capacity matches Excel", {
#   ref_data <- readRDS(test_path("fixtures/chelsea_district1.rds"))
#   calc <- ref_data$calculations
#
#   # These inputs will come from other calculation functions once implemented
#   r_result <- calculate_final_unit_capacity(
#     units_building_capacity = calc$modeled_unit_capacity,  # Column X
#     units_density_limits = calc$dwelling_units_per_acre_limit,  # Column Y
#     units_lot_coverage = # Column Z - not yet implemented
#     units_lot_area_req = calc$lot_area_per_dwelling_unit_limit,  # Column AA
#     units_far_limits = calc$far_limit,  # Column AB
#     units_max_cap = calc$max_units_per_lot_limit,  # Column AC
#     units_graduated_lots = calc$max_units_graduated_lots  # Column AE
#   )
#
#   expect_equal(
#     r_result,
#     calc$final_unit_capacity,
#     tolerance = 0.01,
#     info = "Chelsea District 1 final unit capacity calculation"
#   )
# })
