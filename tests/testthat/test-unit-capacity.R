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

test_that("calculate_net_developable_area handles no override correctly", {
  # All NA overrides - use calculated values
  result <- calculate_net_developable_area(
    developable_area = c(9000, 7000, 11500),
    override_developable_sf = c(NA, NA, NA)
  )
  expect_equal(result, c(9000, 7000, 11500))

  # Single parcel, no override
  result <- calculate_net_developable_area(
    developable_area = 9000,
    override_developable_sf = NA
  )
  expect_equal(result, 9000)
})

test_that("calculate_net_developable_area handles overrides correctly", {
  # All overrides specified
  result <- calculate_net_developable_area(
    developable_area = c(9000, 7000, 11500),
    override_developable_sf = c(8500, 7500, 10000)
  )
  expect_equal(result, c(8500, 7500, 10000))

  # Mixed - some overrides, some calculated
  result <- calculate_net_developable_area(
    developable_area = c(9000, 7000, 11500, 6000),
    override_developable_sf = c(NA, 8500, NA, 5500)
  )
  expect_equal(result, c(9000, 8500, 11500, 5500))
})

test_that("calculate_net_developable_area preserves NA in developable_area", {
  # NA in developable_area with no override
  result <- calculate_net_developable_area(
    developable_area = c(9000, NA, 11500),
    override_developable_sf = c(NA, NA, NA)
  )
  expect_equal(result, c(9000, NA, 11500))

  # Override replaces NA developable_area
  result <- calculate_net_developable_area(
    developable_area = c(9000, NA, 11500),
    override_developable_sf = c(NA, 8500, NA)
  )
  expect_equal(result, c(9000, 8500, 11500))
})

test_that("calculate_net_developable_area validates inputs", {
  # Length mismatch
  expect_error(
    calculate_net_developable_area(
      developable_area = c(9000, 7000),
      override_developable_sf = c(NA)
    )
  )
})

test_that("calculate_exclusion_ratio handles normal cases correctly", {
  # Normal ratios
  result <- calculate_exclusion_ratio(
    excluded_area = c(1000, 500, 2000),
    lot_area = c(10000, 8000, 12000)
  )
  expect_equal(result, c(0.1, 0.0625, 2000/12000), tolerance = 1e-6)

  # No exclusions
  result <- calculate_exclusion_ratio(
    excluded_area = c(0, 0, 0),
    lot_area = c(10000, 8000, 12000)
  )
  expect_equal(result, c(0, 0, 0))

  # Complete exclusions
  result <- calculate_exclusion_ratio(
    excluded_area = c(10000, 8000),
    lot_area = c(10000, 8000)
  )
  expect_equal(result, c(1, 1))
})

test_that("calculate_exclusion_ratio handles division by zero", {
  # Zero lot area
  result <- calculate_exclusion_ratio(
    excluded_area = c(1000, 500),
    lot_area = c(10000, 0)
  )
  expect_equal(result, c(0.1, 0))

  # All zero lot areas
  result <- calculate_exclusion_ratio(
    excluded_area = c(1000, 500, 200),
    lot_area = c(0, 0, 0)
  )
  expect_equal(result, c(0, 0, 0))
})

test_that("calculate_exclusion_ratio handles NA values correctly", {
  # NA in lot_area - Excel returns 0
  result <- calculate_exclusion_ratio(
    excluded_area = c(1000, 500, 200),
    lot_area = c(10000, NA, 8000)
  )
  expect_equal(result, c(0.1, 0, 0.025))

  # NA in excluded_area - preserve NA
  result <- calculate_exclusion_ratio(
    excluded_area = c(1000, NA, 200),
    lot_area = c(10000, 8000, 12000)
  )
  expect_equal(result, c(0.1, NA_real_, 200/12000), tolerance = 1e-6)

  # Both NA
  result <- calculate_exclusion_ratio(
    excluded_area = c(1000, NA),
    lot_area = c(10000, NA)
  )
  expect_equal(result, c(0.1, 0))
})

test_that("calculate_exclusion_ratio validates inputs", {
  # Length mismatch
  expect_error(
    calculate_exclusion_ratio(
      excluded_area = c(1000, 500),
      lot_area = c(10000)
    )
  )
})

test_that("calculate_open_space_requirement applies MBTA 20% minimum", {
  # Below 20% - use 20% minimum
  expect_equal(calculate_open_space_requirement(0.15), 0.2)
  expect_equal(calculate_open_space_requirement(0.1), 0.2)
  expect_equal(calculate_open_space_requirement(0), 0.2)

  # Above 20% - use local requirement
  expect_equal(calculate_open_space_requirement(0.25), 0.25)
  expect_equal(calculate_open_space_requirement(0.3), 0.3)

  # NA or invalid - use 20%
  expect_equal(calculate_open_space_requirement(NA), 0.2)
  expect_equal(calculate_open_space_requirement(1.5), 0.2)
})

test_that("calculate_required_open_space_area handles no override correctly", {
  # Water not included - sum exclusion and open space
  result <- calculate_required_open_space_area(
    lot_area = 10000,
    exclusion_ratio = 0.1,
    open_space_requirement = 0.2,
    net_developable_area = 9000,
    override_developable_sf = NA,
    water_included = "N"
  )
  expect_equal(result, (0.1 + 0.2) * 10000)

  # Water included - use maximum
  result <- calculate_required_open_space_area(
    lot_area = 10000,
    exclusion_ratio = 0.25,
    open_space_requirement = 0.2,
    net_developable_area = 9000,
    override_developable_sf = NA,
    water_included = "Y"
  )
  expect_equal(result, 0.25 * 10000)
})

test_that("calculate_required_open_space_area handles override correctly", {
  # Override with positive net developable
  result <- calculate_required_open_space_area(
    lot_area = 10000,
    exclusion_ratio = 0.1,
    open_space_requirement = 0.2,
    net_developable_area = 8500,
    override_developable_sf = 8500,
    water_included = "N"
  )
  expect_equal(result, 8500 * 0.2)

  # Override with zero net developable
  result <- calculate_required_open_space_area(
    lot_area = 10000,
    exclusion_ratio = 0.1,
    open_space_requirement = 0.2,
    net_developable_area = 0,
    override_developable_sf = 0,
    water_included = "N"
  )
  expect_equal(result, 0)
})

test_that("calculate_parking_area applies correct factors", {
  # No parking
  result <- calculate_parking_area(10000, 2000, 8000, 0)
  expect_equal(result, 0)

  # 0.5 spaces per unit (30% factor)
  result <- calculate_parking_area(10000, 2000, 8000, 0.5)
  expect_equal(result, (10000 - 2000) * 0.3)

  # 1.0 space per unit (45% factor)
  result <- calculate_parking_area(10000, 2000, 8000, 1.0)
  expect_equal(result, (10000 - 2000) * 0.45)

  # 1.5 spaces per unit (60% factor)
  result <- calculate_parking_area(10000, 2000, 8000, 1.5)
  expect_equal(result, (10000 - 2000) * 0.6)

  # 2.0 spaces per unit (65% factor)
  result <- calculate_parking_area(10000, 2000, 8000, 2.0)
  expect_equal(result, (10000 - 2000) * 0.65)
})

test_that("calculate_parking_area handles non-developable parcels", {
  # Zero net developable
  result <- calculate_parking_area(10000, 2000, 0, 1.0)
  expect_equal(result, 0)

  # NA net developable
  result <- calculate_parking_area(10000, 2000, NA, 1.0)
  expect_equal(result, 0)
})

test_that("calculate_building_footprint works correctly", {
  # Normal case
  result <- calculate_building_footprint(10000, 2000, 3600, 8000)
  expect_equal(result, 10000 - 2000 - 3600)

  # Zero net developable
  result <- calculate_building_footprint(10000, 2000, 3600, 0)
  expect_equal(result, 0)

  # Multiple parcels
  result <- calculate_building_footprint(
    lot_area = c(10000, 8000, 12000),
    required_open_space = c(2000, 1600, 2400),
    parking_area = c(3600, 2880, 4320),
    net_developable_area = c(8000, 6400, 9600)
  )
  expect_equal(result, c(4400, 3520, 5280))
})

test_that("calculate_building_floor_area works correctly", {
  # Normal case
  result <- calculate_building_floor_area(4400, 7)
  expect_equal(result, 4400 * 7)

  # Zero footprint
  result <- calculate_building_floor_area(0, 7)
  expect_equal(result, 0)

  # Negative footprint
  result <- calculate_building_floor_area(-100, 7)
  expect_equal(result, 0)

  # Multiple parcels
  result <- calculate_building_floor_area(c(4400, 0, 3520), 7)
  expect_equal(result, c(30800, 0, 24640))
})