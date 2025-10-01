# Tests for Zoning Parameter Extraction and Management

# Test extract_zoning_parameters() ----

test_that("extract_zoning_parameters extracts Chelsea District 1 correctly", {
  skip_if_not(dir.exists(test_path("excel_reference")),
              "Excel reference files not available")

  excel_path <- file.path(
    test_path("excel_reference"),
    "Chelsea",
    "Chelsea - Compliance Model 20231118 REDACTED.xlsx"
  )

  params <- extract_zoning_parameters(excel_path, district = 1)

  # Check structure
  expect_type(params, "list")
  expect_named(params, c(
    "min_lot_size", "base_min_lot_size", "additional_lot_SF",
    "building_height", "FAR", "max_lot_coverage", "min_required_open_space",
    "parking_spaces_per_dwelling_unit", "lot_area_per_dwelling_unit",
    "max_dwelling_units_per_acre", "max_units_per_lot", "water_included"
  ))

  # Check metadata attributes
  expect_equal(attr(params, "source"), "excel")
  expect_equal(attr(params, "district"), 1)

  # Check specific known values for Chelsea District 1
  expect_equal(params$min_lot_size, 5000)
  expect_equal(params$base_min_lot_size, 5000)
  expect_equal(params$additional_lot_SF, 950)
  expect_equal(params$building_height, 7)

  # Check that values are numeric (or NA)
  expect_true(is.numeric(params$min_lot_size))
  expect_true(is.numeric(params$building_height))
})

test_that("extract_zoning_parameters extracts Somerville District 1 correctly", {
  skip_if_not(dir.exists(test_path("excel_reference")),
              "Excel reference files not available")

  excel_path <- file.path(
    test_path("excel_reference"),
    "Somerville",
    "Somerville - CM - 20241222 REDACTED.xlsx"
  )

  params <- extract_zoning_parameters(excel_path, district = 1)

  # Check structure
  expect_type(params, "list")

  # Check specific known values for Somerville District 1
  # This is the key test - Somerville has different min_lot_size than Chelsea
  expect_equal(params$min_lot_size, 2244)

  # Check metadata
  expect_equal(attr(params, "source"), "excel")
  expect_equal(attr(params, "district"), 1)
})

test_that("extract_zoning_parameters handles different districts", {
  skip_if_not(dir.exists(test_path("excel_reference")),
              "Excel reference files not available")

  excel_path <- file.path(
    test_path("excel_reference"),
    "Chelsea",
    "Chelsea - Compliance Model 20231118 REDACTED.xlsx"
  )

  # Extract district 1
  params1 <- extract_zoning_parameters(excel_path, district = 1)
  expect_equal(attr(params1, "district"), 1)

  # Extract district 2
  # Note: Some districts may have NA for min_lot_size if not populated in Excel
  suppressWarnings({
    params2 <- extract_zoning_parameters(excel_path, district = 2)
  })
  expect_equal(attr(params2, "district"), 2)
  expect_type(params2, "list")
})

test_that("extract_zoning_parameters validates inputs correctly", {
  skip_if_not(dir.exists(test_path("excel_reference")),
              "Excel reference files not available")

  # Use a real Excel file for validation tests to avoid file-not-found errors
  excel_path <- file.path(
    test_path("excel_reference"),
    "Chelsea",
    "Chelsea - Compliance Model 20231118 REDACTED.xlsx"
  )

  # Non-existent file
  expect_error(
    extract_zoning_parameters("/nonexistent/file.xlsx", district = 1),
    "Excel file not found"
  )

  # Invalid district number
  expect_error(
    extract_zoning_parameters(excel_path, district = 0),
    "district must be an integer between 1 and 5"
  )

  expect_error(
    extract_zoning_parameters(excel_path, district = 6),
    "district must be an integer between 1 and 5"
  )

  expect_error(
    extract_zoning_parameters(excel_path, district = "invalid"),
    "district must be an integer between 1 and 5"
  )
})

test_that("extract_zoning_parameters handles NA values correctly", {
  skip_if_not(dir.exists(test_path("excel_reference")),
              "Excel reference files not available")

  excel_path <- file.path(
    test_path("excel_reference"),
    "Chelsea",
    "Chelsea - Compliance Model 20231118 REDACTED.xlsx"
  )

  params <- extract_zoning_parameters(excel_path, district = 1)

  # Some parameters may be NA (not applicable) for certain districts
  # Check that they are properly typed as NA_real_ not just NULL
  if (is.na(params$FAR)) {
    expect_type(params$FAR, "double")
    expect_true(is.na(params$FAR))
  }
})

# Test create_zoning_parameters() ----

test_that("create_zoning_parameters creates valid parameter list", {
  params <- create_zoning_parameters(min_lot_size = 5000)

  # Check structure
  expect_type(params, "list")
  expect_named(params, c(
    "min_lot_size", "base_min_lot_size", "additional_lot_SF",
    "building_height", "FAR", "max_lot_coverage", "min_required_open_space",
    "parking_spaces_per_dwelling_unit", "lot_area_per_dwelling_unit",
    "max_dwelling_units_per_acre", "max_units_per_lot", "water_included"
  ))

  # Check required value
  expect_equal(params$min_lot_size, 5000)

  # Check defaults
  expect_equal(params$min_required_open_space, 0.2)
  expect_equal(params$water_included, "N")

  # Check that other values are NA
  expect_true(is.na(params$base_min_lot_size))
  expect_true(is.na(params$FAR))

  # Check metadata
  expect_equal(attr(params, "source"), "manual")
})

test_that("create_zoning_parameters accepts all parameters", {
  params <- create_zoning_parameters(
    min_lot_size = 5000,
    base_min_lot_size = 5000,
    additional_lot_SF = 950,
    building_height = 7,
    FAR = 2.0,
    max_lot_coverage = 0.5,
    min_required_open_space = 0.15,
    parking_spaces_per_dwelling_unit = 1.0,
    lot_area_per_dwelling_unit = 950,
    max_dwelling_units_per_acre = 40,
    max_units_per_lot = 10,
    water_included = "Y"
  )

  # Check all values are set correctly
  expect_equal(params$min_lot_size, 5000)
  expect_equal(params$building_height, 7)
  expect_equal(params$FAR, 2.0)
  expect_equal(params$water_included, "Y")
  expect_equal(params$max_units_per_lot, 10)
})

test_that("create_zoning_parameters validates min_lot_size", {
  # Missing min_lot_size
  expect_error(
    create_zoning_parameters(),
    "min_lot_size is required"
  )

  # NA min_lot_size
  expect_error(
    create_zoning_parameters(min_lot_size = NA),
    "min_lot_size is required and cannot be NA"
  )

  # Negative min_lot_size
  expect_error(
    create_zoning_parameters(min_lot_size = -100),
    "min_lot_size must be a non-negative numeric value"
  )

  # Non-numeric min_lot_size
  expect_error(
    create_zoning_parameters(min_lot_size = "invalid"),
    "min_lot_size must be a non-negative numeric value"
  )
})

test_that("create_zoning_parameters validates water_included", {
  # Invalid value
  expect_error(
    create_zoning_parameters(min_lot_size = 5000, water_included = "X"),
    "water_included must be 'Y' or 'N'"
  )

  # Valid values work
  expect_no_error(
    create_zoning_parameters(min_lot_size = 5000, water_included = "Y")
  )

  expect_no_error(
    create_zoning_parameters(min_lot_size = 5000, water_included = "N")
  )
})

test_that("create_zoning_parameters ensures proper types", {
  params <- create_zoning_parameters(
    min_lot_size = 5000,
    building_height = "7",  # Pass as string
    max_lot_coverage = "0.5"  # Pass as string
  )

  # Should be coerced to numeric
  expect_type(params$building_height, "double")
  expect_type(params$max_lot_coverage, "double")
  expect_equal(params$building_height, 7)
  expect_equal(params$max_lot_coverage, 0.5)
})

# Integration tests ----

test_that("extracted parameters work with calculation functions", {
  params <- create_zoning_parameters(
    min_lot_size = 5000,
    building_height = 7,
    min_required_open_space = 0.15
  )

  # Use with calculate_developable_area
  result <- calculate_developable_area(
    lot_area = c(10000, 8000, 3000),
    excluded_area = c(1000, 500, 200),
    min_lot_size = params$min_lot_size
  )

  # Check that calculation works
  expect_equal(result, c(9000, 7500, 0))
})

test_that("Excel-extracted parameters match expected structure", {
  skip_if_not(dir.exists(test_path("excel_reference")),
              "Excel reference files not available")

  excel_path <- file.path(
    test_path("excel_reference"),
    "Chelsea",
    "Chelsea - Compliance Model 20231118 REDACTED.xlsx"
  )

  excel_params <- extract_zoning_parameters(excel_path, district = 1)
  manual_params <- create_zoning_parameters(min_lot_size = 5000)

  # Both should have same names (structure)
  expect_identical(names(excel_params), names(manual_params))
})
