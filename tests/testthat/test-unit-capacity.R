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

# Tests for Column X: calculate_units_from_building_capacity()

test_that("calculate_units_from_building_capacity handles normal cases correctly", {
  # Above 3 units - should floor
  result <- calculate_units_from_building_capacity(5200)
  expect_equal(result, 5)

  # Exactly 3 units
  result <- calculate_units_from_building_capacity(3000)
  expect_equal(result, 3)

  # Multiple parcels
  result <- calculate_units_from_building_capacity(c(5200, 4100, 8900))
  expect_equal(result, c(5, 4, 8))
})

test_that("calculate_units_from_building_capacity handles threshold rules", {
  # Below 2.5 threshold
  result <- calculate_units_from_building_capacity(2100)
  expect_equal(result, 0)

  # At 2.5 - returns 3
  result <- calculate_units_from_building_capacity(2500)
  expect_equal(result, 3)

  # Between 2.5 and 3
  result <- calculate_units_from_building_capacity(2700)
  expect_equal(result, 3)

  # Just above 3
  result <- calculate_units_from_building_capacity(3100)
  expect_equal(result, 3)

  # Zero floor area
  result <- calculate_units_from_building_capacity(0)
  expect_equal(result, 0)
})

test_that("calculate_units_from_building_capacity handles NA correctly", {
  result <- calculate_units_from_building_capacity(c(5200, NA, 2100))
  expect_equal(result, c(5, NA_real_, 0))
})

# Tests for Column Y: calculate_units_from_density_limits()

test_that("calculate_units_from_density_limits works correctly", {
  # Normal case - 10,000 sq ft with 20 units/acre
  result <- calculate_units_from_density_limits(10000, 20)
  expect_equal(result, (10000 / 43560) * 20, tolerance = 1e-6)

  # Multiple parcels
  result <- calculate_units_from_density_limits(c(10000, 8000, 12000), 20)
  expect_equal(result, c((10000/43560)*20, (8000/43560)*20, (12000/43560)*20), tolerance = 1e-6)
})

test_that("calculate_units_from_density_limits handles unlimited density", {
  # NA max_dwelling_units_per_acre means unlimited
  result <- calculate_units_from_density_limits(10000, NA)
  expect_equal(result, NA_real_)

  # Multiple parcels with NA
  result <- calculate_units_from_density_limits(c(10000, 8000), NA)
  expect_equal(result, c(NA_real_, NA_real_))
})

test_that("calculate_units_from_density_limits handles NA lot_area", {
  result <- calculate_units_from_density_limits(c(10000, NA, 8000), 20)
  expect_equal(result[1], (10000/43560)*20, tolerance = 1e-6)
  expect_equal(result[2], NA_real_)
  expect_equal(result[3], (8000/43560)*20, tolerance = 1e-6)
})

# Tests for Column Z: calculate_units_from_lot_coverage()

test_that("calculate_units_from_lot_coverage works correctly", {
  # 10,000 sq ft, 70% coverage, 7 stories
  result <- calculate_units_from_lot_coverage(10000, 0.7, 7)
  expect_equal(result, (10000 * 0.7 * 7) / 1000)

  # Multiple parcels
  result <- calculate_units_from_lot_coverage(c(10000, 8000, 12000), 0.7, 7)
  expect_equal(result, c(49, 39.2, 58.8))
})

test_that("calculate_units_from_lot_coverage handles no coverage limit", {
  result <- calculate_units_from_lot_coverage(10000, NA, 7)
  expect_equal(result, NA_real_)
})

test_that("calculate_units_from_lot_coverage handles NA lot_area", {
  result <- calculate_units_from_lot_coverage(c(10000, NA, 8000), 0.7, 7)
  expect_equal(result, c(49, NA_real_, 39.2))
})

# Tests for Column AA: calculate_units_from_lot_area_requirement()

test_that("calculate_units_from_lot_area_requirement works correctly", {
  # 10,000 sq ft with 2,000 sq ft per unit
  result <- calculate_units_from_lot_area_requirement(10000, 2000)
  expect_equal(result, 5)

  # Multiple parcels
  result <- calculate_units_from_lot_area_requirement(c(10000, 8000, 12000), 2000)
  expect_equal(result, c(5, 4, 6))
})

test_that("calculate_units_from_lot_area_requirement handles no requirement", {
  # NA means no lot area requirement
  result <- calculate_units_from_lot_area_requirement(10000, NA)
  expect_equal(result, NA_real_)

  # Zero or negative also means no requirement
  result <- calculate_units_from_lot_area_requirement(10000, 0)
  expect_equal(result, NA_real_)

  result <- calculate_units_from_lot_area_requirement(10000, -100)
  expect_equal(result, NA_real_)
})

test_that("calculate_units_from_lot_area_requirement handles NA lot_area", {
  result <- calculate_units_from_lot_area_requirement(c(10000, NA, 8000), 2000)
  expect_equal(result, c(5, NA_real_, 4))
})

# Tests for Column AB: calculate_units_from_far_limits()

test_that("calculate_units_from_far_limits works correctly", {
  # 10,000 sq ft with FAR of 2.0
  result <- calculate_units_from_far_limits(10000, 2.0)
  expect_equal(result, 20)

  # Multiple parcels
  result <- calculate_units_from_far_limits(c(10000, 8000, 12000), 2.0)
  expect_equal(result, c(20, 16, 24))
})

test_that("calculate_units_from_far_limits handles no FAR limit", {
  result <- calculate_units_from_far_limits(10000, NA)
  expect_equal(result, NA_real_)
})

test_that("calculate_units_from_far_limits handles NA lot_area", {
  result <- calculate_units_from_far_limits(c(10000, NA, 8000), 2.0)
  expect_equal(result, c(20, NA_real_, 16))
})

# Tests for Column AC: calculate_units_with_max_cap()

test_that("calculate_units_with_max_cap applies cap correctly", {
  # Building capacity 8, cap at 6
  result <- calculate_units_with_max_cap(8, 6)
  expect_equal(result, 6)

  # Building capacity 5, cap at 6 (no effect)
  result <- calculate_units_with_max_cap(5, 6)
  expect_equal(result, 5)

  # Multiple parcels
  result <- calculate_units_with_max_cap(c(8, 5, 10), 6)
  expect_equal(result, c(6, 5, 6))
})

test_that("calculate_units_with_max_cap handles below threshold cap", {
  # Cap of 2 (below threshold of 3) with building capacity of 8
  result <- calculate_units_with_max_cap(8, 2)
  expect_equal(result, 0)

  # Cap of 1 with building capacity of 5
  result <- calculate_units_with_max_cap(5, 1)
  expect_equal(result, 0)
})

test_that("calculate_units_with_max_cap handles no cap", {
  # NA means no cap - should floor building capacity
  result <- calculate_units_with_max_cap(8.7, NA)
  expect_equal(result, 8)

  result <- calculate_units_with_max_cap(c(8.7, 5.2, 3.8), NA)
  expect_equal(result, c(8, 5, 3))
})

test_that("calculate_units_with_max_cap handles NA building capacity", {
  result <- calculate_units_with_max_cap(c(8, NA, 5), 6)
  expect_equal(result, c(6, NA_real_, 5))
})

# Tests for Column AD: calculate_below_minimum_lot_flag()

test_that("calculate_below_minimum_lot_flag identifies parcels correctly", {
  # Mix of below and above minimum
  result <- calculate_below_minimum_lot_flag(c(3000, 8000, 4500), 5000)
  expect_equal(result, c("Y", NA_character_, "Y"))

  # All above minimum
  result <- calculate_below_minimum_lot_flag(c(6000, 8000, 10000), 5000)
  expect_equal(result, c(NA_character_, NA_character_, NA_character_))

  # All below minimum
  result <- calculate_below_minimum_lot_flag(c(3000, 4000, 2000), 5000)
  expect_equal(result, c("Y", "Y", "Y"))
})

test_that("calculate_below_minimum_lot_flag handles zero and NA lot_area", {
  # Zero lot area is not below minimum
  result <- calculate_below_minimum_lot_flag(c(0, 8000), 5000)
  expect_equal(result, c(NA_character_, NA_character_))

  # NA lot area
  result <- calculate_below_minimum_lot_flag(c(3000, NA, 8000), 5000)
  expect_equal(result, c("Y", NA_character_, NA_character_))
})

test_that("calculate_below_minimum_lot_flag handles exactly at minimum", {
  # Exactly at minimum is not below
  result <- calculate_below_minimum_lot_flag(c(4999, 5000, 5001), 5000)
  expect_equal(result, c("Y", NA_character_, NA_character_))
})

# Tests for Column AE: calculate_units_from_graduated_lots()

test_that("calculate_units_from_graduated_lots calculates correctly", {
  # 10,000 sq ft: base 5000, additional 2000 per unit
  # (10000 - 5000) / 2000 + 1 = 2.5 + 1 = floor(2.5) + 1 = 3
  result <- calculate_units_from_graduated_lots(10000, NA_character_, 5000, 2000)
  expect_equal(result, 3)

  # 15,000 sq ft: (15000 - 5000) / 2000 + 1 = 5 + 1 = 6
  result <- calculate_units_from_graduated_lots(15000, NA_character_, 5000, 2000)
  expect_equal(result, 6)

  # Multiple parcels
  result <- calculate_units_from_graduated_lots(c(10000, 15000, 8000), c(NA_character_, NA_character_, NA_character_), 5000, 2000)
  expect_equal(result, c(3, 6, 2))
})

test_that("calculate_units_from_graduated_lots handles below minimum", {
  # Below minimum lot size returns 0
  result <- calculate_units_from_graduated_lots(3000, "Y", 5000, 2000)
  expect_equal(result, 0)

  # Mixed
  result <- calculate_units_from_graduated_lots(c(3000, 10000, 4000), c("Y", NA_character_, "Y"), 5000, 2000)
  expect_equal(result, c(0, 3, 0))
})

test_that("calculate_units_from_graduated_lots handles no graduated lot sizing", {
  # NA additional_lot_SF means no graduated lot sizing
  result <- calculate_units_from_graduated_lots(10000, NA_character_, 5000, NA)
  expect_equal(result, NA_real_)
})

test_that("calculate_units_from_graduated_lots handles NA lot_area", {
  result <- calculate_units_from_graduated_lots(c(10000, NA, 8000), c(NA_character_, NA_character_, NA_character_), 5000, 2000)
  expect_equal(result, c(3, NA_real_, 2))
})

# Tests for Column AG: calculate_units_per_acre()

test_that("calculate_units_per_acre calculates correctly", {
  # 10,000 sq ft with 5 units
  # (43560 / 10000) * 5 = 21.78
  result <- calculate_units_per_acre(10000, 5)
  expect_equal(result, (43560 / 10000) * 5, tolerance = 1e-6)

  # Multiple parcels
  result <- calculate_units_per_acre(c(10000, 8000, 12000), c(5, 4, 6))
  expected <- c((43560/10000)*5, (43560/8000)*4, (43560/12000)*6)
  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("calculate_units_per_acre handles zero and NA lot_area", {
  # Zero lot area
  result <- calculate_units_per_acre(0, 5)
  expect_equal(result, 0)

  # NA lot area
  result <- calculate_units_per_acre(c(10000, NA, 8000), c(5, 4, 3))
  expect_equal(result[1], (43560/10000)*5, tolerance = 1e-6)
  expect_equal(result[2], 0)
  expect_equal(result[3], (43560/8000)*3, tolerance = 1e-6)
})

test_that("calculate_units_per_acre handles NA final_unit_capacity", {
  result <- calculate_units_per_acre(c(10000, 8000, 12000), c(5, NA, 6))
  expect_equal(result[1], (43560/10000)*5, tolerance = 1e-6)
  expect_equal(result[2], NA_real_)
  expect_equal(result[3], (43560/12000)*6, tolerance = 1e-6)
})

test_that("calculate_units_per_acre handles zero units", {
  result <- calculate_units_per_acre(10000, 0)
  expect_equal(result, 0)
})