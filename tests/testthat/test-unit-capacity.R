test_that("calculate_developable_area handles normal cases correctly", {
  # Single parcel with developable area
  result <- calculate_developable_area(
    lot_area = 10000,
    excluded_area = 1000, 
    min_lot_size = 5000
  )
  expect_equal(result, 9000)
  
  # Multiple parcels
  result <- calculate_developable_area(
    lot_area = c(10000, 8000, 12000),
    excluded_area = c(1000, 2000, 500),
    min_lot_size = 5000
  )
  expect_equal(result, c(9000, 6000, 11500))
})

test_that("calculate_developable_area handles minimum lot size correctly", {
  # Parcel below minimum lot size
  result <- calculate_developable_area(
    lot_area = 3000,
    excluded_area = 500,
    min_lot_size = 5000
  )
  expect_equal(result, 0)
  
  # Mixed parcels - some below minimum
  result <- calculate_developable_area(
    lot_area = c(3000, 8000, 4000),
    excluded_area = c(500, 1000, 200),
    min_lot_size = 5000
  )
  expect_equal(result, c(0, 7000, 0))
})

test_that("calculate_developable_area handles exclusions exceeding lot area", {
  # Exclusions larger than lot area
  result <- calculate_developable_area(
    lot_area = 8000,
    excluded_area = 10000,
    min_lot_size = 5000
  )
  expect_equal(result, 0)
  
  # Mixed cases
  result <- calculate_developable_area(
    lot_area = c(8000, 10000, 6000),
    excluded_area = c(10000, 5000, 8000),
    min_lot_size = 5000
  )
  expect_equal(result, c(0, 5000, 0))
})

test_that("calculate_developable_area preserves NA values", {
  # NA lot area
  result <- calculate_developable_area(
    lot_area = c(10000, NA, 8000),
    excluded_area = c(1000, 500, 2000),
    min_lot_size = 5000
  )
  expect_equal(result, c(9000, NA_real_, 6000))
  
  # NA excluded area  
  result <- calculate_developable_area(
    lot_area = c(10000, 8000, 12000),
    excluded_area = c(1000, NA, 2000),
    min_lot_size = 5000
  )
  expect_equal(result, c(9000, NA_real_, 10000))
  
  # Both NA
  result <- calculate_developable_area(
    lot_area = c(10000, NA),
    excluded_area = c(1000, NA),
    min_lot_size = 5000
  )
  expect_equal(result, c(9000, NA_real_))
})

test_that("calculate_developable_area validates critical assumptions", {
  # Vector length mismatch
  expect_error(
    calculate_developable_area(
      lot_area = c(10000, 8000),
      excluded_area = c(1000),
      min_lot_size = 5000
    )
  )

  # Multiple min_lot_size values
  expect_error(
    calculate_developable_area(
      lot_area = 10000,
      excluded_area = 1000,
      min_lot_size = c(5000, 6000)
    )
  )
})

test_that("calculate_final_unit_capacity handles normal cases correctly", {
  # Single parcel with all methods applicable
  result <- calculate_final_unit_capacity(
    units_building_capacity = 5.2,
    units_density_limits = 4.8,
    units_lot_coverage = 6.1,
    units_lot_area_req = 3.9,
    units_far_limits = 4.2,
    units_max_cap = 5.0,
    units_graduated_lots = 4.5
  )
  expect_equal(result, 4)  # min is 3.9, rounded to 4
  
  # Multiple parcels
  result <- calculate_final_unit_capacity(
    units_building_capacity = c(5.2, 8.1, 3.8),
    units_density_limits = c(4.8, 7.5, 4.2),
    units_lot_coverage = c(6.1, 9.0, 5.1),
    units_lot_area_req = c(3.9, 6.8, 2.9),
    units_far_limits = c(4.2, 8.2, 4.5),
    units_max_cap = c(5.0, 8.0, 3.5),
    units_graduated_lots = c(4.5, 7.8, 3.2)
  )
  expect_equal(result, c(4, 7, 3))  # mins: 3.9→4, 6.8→7, 2.9→3
})

test_that("calculate_final_unit_capacity handles threshold rules correctly", {
  # Below 2.5 threshold
  result <- calculate_final_unit_capacity(
    units_building_capacity = 2.2,
    units_density_limits = 2.4,
    units_lot_coverage = 2.1,
    units_lot_area_req = 2.3,
    units_far_limits = 2.0,
    units_max_cap = 2.2,
    units_graduated_lots = 2.4
  )
  expect_equal(result, 0)  # min is 2.0, below 2.5 threshold
  
  # Between 2.5 and 3.0
  result <- calculate_final_unit_capacity(
    units_building_capacity = 3.2,
    units_density_limits = 2.8,
    units_lot_coverage = 3.1,
    units_lot_area_req = 2.9,
    units_far_limits = 3.0,
    units_max_cap = 2.7,
    units_graduated_lots = 3.1
  )
  expect_equal(result, 3)  # min is 2.7, between 2.5-3.0 → 3
})

test_that("calculate_final_unit_capacity handles NA values correctly", {
  # Some methods NA (treated as unlimited)
  result <- calculate_final_unit_capacity(
    units_building_capacity = 5.2,
    units_density_limits = NA,
    units_lot_coverage = 6.1,
    units_lot_area_req = 3.9,
    units_far_limits = NA,
    units_max_cap = 5.0,
    units_graduated_lots = 4.5
  )
  expect_equal(result, 4)  # min of non-NA values: 3.9 → 4
  
  # All methods NA
  result <- calculate_final_unit_capacity(
    units_building_capacity = NA,
    units_density_limits = NA,
    units_lot_coverage = NA,
    units_lot_area_req = NA,
    units_far_limits = NA,
    units_max_cap = NA,
    units_graduated_lots = NA
  )
  expect_equal(result, NA_real_)
  
  # Mixed parcels with different NA patterns
  result <- calculate_final_unit_capacity(
    units_building_capacity = c(5.2, NA, 3.8),
    units_density_limits = c(4.8, NA, NA),
    units_lot_coverage = c(6.1, NA, 5.1),
    units_lot_area_req = c(3.9, NA, 2.9),
    units_far_limits = c(4.2, NA, 4.5),
    units_max_cap = c(5.0, NA, 3.5),
    units_graduated_lots = c(4.5, NA, 3.2)
  )
  expect_equal(result, c(4, NA_real_, 3))
})