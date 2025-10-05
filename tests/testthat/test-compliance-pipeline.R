test_that("assign_parcels_to_districts works with sf polygon", {
  skip_if_not_installed("sf")

  # Create mock parcel data
  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("P001", "P002", "P003"),
      SQFT = c(10000, 15000, 12000),
      ACRES = c(0.23, 0.34, 0.28),
      Tot_Exclud = c(1000, 2000, 1500),
      x = c(1, 2, 3),
      y = c(1, 2, 3)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  # Create mock district polygon
  district <- sf::st_as_sf(
    data.frame(
      district_id = "D1",
      name = "District 1"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 4, 0, 4, 4, 0, 4, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 26986
    )
  )

  result <- assign_parcels_to_districts(parcels, district)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("LOC_ID", "district_id", "district_name") %in% names(result)))
  expect_equal(result$LOC_ID, c("P001", "P002", "P003"))
})

test_that("assign_parcels_to_districts works with column name", {
  skip_if_not_installed("sf")

  # Create mock parcel data with district column
  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("P001", "P002", "P003"),
      SQFT = c(10000, 15000, 12000),
      ACRES = c(0.23, 0.34, 0.28),
      Tot_Exclud = c(1000, 2000, 1500),
      ZONING_DIST = c("D1", "D1", "D2"),
      x = c(1, 2, 3),
      y = c(1, 2, 3)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  result <- assign_parcels_to_districts(parcels, "ZONING_DIST")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$district_id, c("D1", "D1", "D2"))
})

test_that("assign_parcels_to_districts works with logical column (TRUE/FALSE filter)", {
  skip_if_not_installed("sf")

  # Create mock parcel data with logical district indicator column
  # This tests the bug fix where logical columns should filter (not create two districts)
  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("P001", "P002", "P003", "P004"),
      SQFT = c(10000, 15000, 12000, 8000),
      ACRES = c(0.23, 0.34, 0.28, 0.18),
      Tot_Exclud = c(1000, 2000, 1500, 500),
      in_district = c(TRUE, TRUE, FALSE, FALSE),  # Only first 2 parcels in district
      x = c(1, 2, 3, 4),
      y = c(1, 2, 3, 4)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  result <- assign_parcels_to_districts(parcels, "in_district")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)

  # TRUE values should get the column name as district_id
  expect_equal(result$district_id[1], "in_district")
  expect_equal(result$district_id[2], "in_district")

  # FALSE values should get NA (not "FALSE" as a district)
  expect_true(is.na(result$district_id[3]))
  expect_true(is.na(result$district_id[4]))

  # Should warn about missing assignments
  expect_warning(
    assign_parcels_to_districts(parcels, "in_district"),
    "2 parcels have missing or empty district assignments"
  )

  # Only 2 parcels should be in the district (not all 4)
  n_in_district <- sum(!is.na(result$district_id))
  expect_equal(n_in_district, 2)
})

test_that("assign_parcels_to_districts works with named list", {
  skip_if_not_installed("sf")

  # Create mock parcel data
  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("P001", "P002", "P003"),
      SQFT = c(10000, 15000, 12000),
      ACRES = c(0.23, 0.34, 0.28),
      Tot_Exclud = c(1000, 2000, 1500),
      x = c(1, 2, 3),
      y = c(1, 2, 3)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  districts <- list(
    "District_1" = c("P001", "P002"),
    "District_2" = c("P003")
  )

  result <- assign_parcels_to_districts(parcels, districts)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$district_id[result$LOC_ID == "P001"], "District_1")
  expect_equal(result$district_id[result$LOC_ID == "P003"], "District_2")
})

test_that("calculate_district_capacity chains all calculations", {
  skip_if_not_installed("sf")

  # Create mock parcel data
  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("P001", "P002"),
      SQFT = c(10000, 15000),
      ACRES = c(0.23, 0.34),
      Tot_Exclud = c(1000, 2000),
      x = c(1, 2),
      y = c(1, 2)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  # Create mock zoning parameters
  zoning_params <- list(
    min_lot_size = 5000,
    base_min_lot_size = 5000,
    additional_lot_SF = 2000,
    building_height = 7,
    FAR = 1.5,
    max_lot_coverage = 0.5,
    min_required_open_space = 0.2,
    parking_spaces_per_dwelling_unit = 1,
    lot_area_per_dwelling_unit = 2000,
    max_dwelling_units_per_acre = 20,
    max_units_per_lot = NA,
    water_included = "Y"
  )

  result <- calculate_district_capacity(parcels, zoning_params)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 2)

  # Check all 18 calculation columns are present
  # Note: open_space_requirement is a scalar (district-level), not a column
  expected_cols <- c(
    "developable_area", "net_developable_area", "exclusion_ratio",
    "required_open_space_area", "parking_area",
    "building_footprint", "building_floor_area", "units_building_capacity",
    "units_density_limits", "units_lot_coverage", "units_lot_area_req",
    "units_far_limits", "units_max_cap", "below_minimum_lot",
    "units_graduated_lots", "final_unit_capacity", "units_per_acre",
    "in_station_area"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_community_requirements returns correct structure", {
  reqs <- get_community_requirements(
    community_type = "rapid_transit",
    custom_requirements = list(min_units = 6240, min_acres = 50)
  )

  expect_type(reqs, "list")
  expect_true(all(c("min_units", "min_acres", "min_gross_density", "community_type") %in% names(reqs)))
  expect_equal(reqs$min_units, 6240)
  expect_equal(reqs$min_acres, 50)
  expect_equal(reqs$min_gross_density, 15)
  expect_equal(reqs$community_type, "rapid_transit")
})

test_that("get_community_requirements validates community_type", {
  expect_error(
    get_community_requirements(community_type = "invalid_type"),
    "community_type must be one of"
  )

  expect_error(
    get_community_requirements(community_type = NULL),
    "community_type must be specified"
  )
})

test_that("get_community_requirements loads data from CSV for known communities", {
  # Test Cambridge (rapid transit)
  reqs_cambridge <- get_community_requirements(
    community_name = "Cambridge",
    community_type = "rapid_transit"
  )
  expect_equal(reqs_cambridge$min_units, 13477)
  expect_equal(reqs_cambridge$min_acres, 32)
  expect_equal(reqs_cambridge$min_station_area_acres, 100)
  expect_equal(reqs_cambridge$station_area_land_pct, 90)
  expect_equal(reqs_cambridge$station_area_unit_pct, 90)
  expect_equal(reqs_cambridge$community_name, "Cambridge")

  # Test Somerville (rapid transit)
  reqs_somerville <- get_community_requirements(
    community_name = "Somerville",
    community_type = "rapid_transit"
  )
  expect_equal(reqs_somerville$min_units, 9067)
  expect_equal(reqs_somerville$min_acres, 24)
  expect_equal(reqs_somerville$station_area_land_pct, 90)
  expect_equal(reqs_somerville$station_area_unit_pct, 90)

  # Test Arlington (adjacent community)
  reqs_arlington <- get_community_requirements(
    community_name = "Arlington",
    community_type = "adjacent"
  )
  expect_equal(reqs_arlington$min_units, 2046)
  expect_equal(reqs_arlington$min_acres, 32)
  expect_true(is.na(reqs_arlington$min_station_area_acres))
  expect_equal(reqs_arlington$station_area_land_pct, 0)
  expect_equal(reqs_arlington$station_area_unit_pct, 0)

  # Test Ashby (adjacent small town)
  reqs_ashby <- get_community_requirements(
    community_name = "Ashby",
    community_type = "adjacent_small_town"
  )
  expect_equal(reqs_ashby$min_units, 62)
  expect_true(is.na(reqs_ashby$min_acres))
  expect_true(is.na(reqs_ashby$min_station_area_acres))
})

test_that("get_community_requirements handles unknown communities", {
  expect_warning(
    reqs <- get_community_requirements(
      community_name = "Faketown",
      community_type = "rapid_transit"
    ),
    "not found in community data"
  )
  # Should return default values for the community type
  expect_true(is.na(reqs$min_units))
  expect_equal(reqs$min_acres, 50)  # Default for rapid_transit
  expect_equal(reqs$min_station_area_acres, 100)
})

test_that("get_community_requirements is case-insensitive", {
  reqs_upper <- get_community_requirements(
    community_name = "CAMBRIDGE",
    community_type = "rapid_transit"
  )
  reqs_lower <- get_community_requirements(
    community_name = "cambridge",
    community_type = "rapid_transit"
  )
  reqs_mixed <- get_community_requirements(
    community_name = "CaMbRiDgE",
    community_type = "rapid_transit"
  )

  expect_equal(reqs_upper$min_units, 13477)
  expect_equal(reqs_lower$min_units, 13477)
  expect_equal(reqs_mixed$min_units, 13477)
})

test_that("get_community_requirements respects custom_requirements override", {
  reqs <- get_community_requirements(
    community_name = "Cambridge",
    community_type = "rapid_transit",
    custom_requirements = list(min_units = 20000, min_acres = 100)
  )

  # Custom values should override CSV values
  expect_equal(reqs$min_units, 20000)
  expect_equal(reqs$min_acres, 100)

  # Other values should still come from CSV
  expect_equal(reqs$station_area_land_pct, 90)
  expect_equal(reqs$min_station_area_acres, 100)
})

test_that("check_compliance_requirements evaluates all requirements", {
  metrics <- list(
    total_units = 6500,
    total_acres = 145,
    developable_acres = 120,
    gross_density = 16.2,
    developable_station_acres = 110,
    station_area_units = 4200,
    station_area_acres = 95
  )

  requirements <- list(
    min_units = 6240,
    min_acres = 50,
    min_station_area_acres = 100,
    station_area_unit_pct = 60,
    station_area_land_pct = 50,
    min_gross_density = 15
  )

  result <- check_compliance_requirements(metrics, requirements, "rapid_transit")

  expect_type(result, "list")
  expect_true(all(c("compliant", "requirements_met", "summary") %in% names(result)))
  expect_type(result$compliant, "logical")
  expect_s3_class(result$summary, "data.frame")
})

test_that("check_compliance_requirements detects failures", {
  metrics <- list(
    total_units = 5000,  # Below minimum
    total_acres = 145,
    developable_acres = 120,
    gross_density = 14.5,  # Below minimum
    developable_station_acres = 110,
    station_area_units = 4200,
    station_area_acres = 95
  )

  requirements <- list(
    min_units = 6240,
    min_acres = 50,
    min_station_area_acres = 100,
    station_area_unit_pct = 60,
    station_area_land_pct = 50,
    min_gross_density = 15
  )

  result <- check_compliance_requirements(metrics, requirements, "rapid_transit")

  expect_false(result$compliant)
  expect_false(result$requirements_met$min_unit_capacity)
  expect_false(result$requirements_met$min_gross_density)
  expect_type(result$failure_reasons, "character")
  expect_gt(length(result$failure_reasons), 0)
})

test_that("evaluate_compliance validates inputs", {
  skip_if_not_installed("sf")

  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("P001"),
      SQFT = c(10000),
      ACRES = c(0.23),
      Tot_Exclud = c(1000),
      x = c(1),
      y = c(1)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  zoning_params <- list(
    min_lot_size = 5000,
    base_min_lot_size = 5000,
    additional_lot_SF = 2000,
    building_height = 7,
    FAR = 1.5,
    max_lot_coverage = 0.5,
    min_required_open_space = 0.2,
    parking_spaces_per_dwelling_unit = 1,
    lot_area_per_dwelling_unit = 2000,
    max_dwelling_units_per_acre = 20,
    max_units_per_lot = NA,
    water_included = "Y"
  )

  # Should work with valid inputs
  expect_no_error(
    evaluate_compliance(
      municipality = parcels,
      districts = list("D1" = "P001"),
      zoning_params = zoning_params,
      community_type = "rapid_transit",
      custom_requirements = list(min_units = 10, min_acres = 0.1),
      verbose = FALSE
    )
  )
})
