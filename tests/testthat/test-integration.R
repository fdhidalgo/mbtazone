# Integration Tests for Compliance Pipeline
# These tests verify the complete end-to-end compliance evaluation workflow

# Helper function to create realistic synthetic parcel data
create_synthetic_parcels <- function(n_parcels = 50, seed = 12345) {
  set.seed(seed)

  # Create spatial grid of parcels in EPSG:26986
  x_coords <- runif(n_parcels, min = 200000, max = 210000)
  y_coords <- runif(n_parcels, min = 900000, max = 910000)

  parcels <- sf::st_as_sf(
    data.frame(
      LOC_ID = sprintf("P%04d", 1:n_parcels),
      SQFT = sample(5000:50000, n_parcels, replace = TRUE),
      ACRES = NA,  # Will calculate
      Tot_Exclud = sample(0:5000, n_parcels, replace = TRUE),
      x = x_coords,
      y = y_coords
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  # Calculate ACRES from SQFT
  parcels$ACRES <- parcels$SQFT / 43560

  return(parcels)
}

# Helper function to create realistic zoning parameters
create_standard_zoning <- function() {
  list(
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
}

# Helper function to create district boundary polygon
create_district_polygon <- function(district_id, name, xmin, xmax, ymin, ymax) {
  sf::st_as_sf(
    data.frame(
      district_id = district_id,
      name = name
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
        ncol = 2, byrow = TRUE
      ))),
      crs = 26986
    )
  )
}

# Test 1: End-to-end pipeline with minimal realistic data
test_that("evaluate_compliance runs complete pipeline with synthetic data", {
  skip_if_not_installed("sf")

  # Create synthetic municipality with ~50 parcels
  municipality <- create_synthetic_parcels(n_parcels = 50, seed = 101)

  # Create single district encompassing all parcels
  district <- create_district_polygon(
    district_id = "D1",
    name = "Test District",
    xmin = 199000, xmax = 211000,
    ymin = 899000, ymax = 911000
  )

  # Create realistic zoning parameters
  zoning_params <- create_standard_zoning()

  # Run complete pipeline (with reduced requirements for small test)
  result <- evaluate_compliance(
    municipality = municipality,
    districts = district,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 50, min_acres = 10),
    verbose = FALSE
  )

  # Verify output structure
  expect_type(result, "list")
  expect_true(all(c("summary", "by_district", "parcel_detail", "compliance", "metadata") %in% names(result)))

  # Verify summary structure
  expect_type(result$summary, "list")
  expect_true(all(c("total_units", "total_acres", "developable_acres", "gross_density",
                    "compliant", "requirements_met") %in% names(result$summary)))

  # Verify by_district data frame
  expect_s3_class(result$by_district, "data.frame")
  expect_equal(nrow(result$by_district), 1)  # Single district
  expect_true(all(c("district_id", "district_name", "total_units", "total_acres",
                    "developable_acres", "gross_density") %in% names(result$by_district)))

  # Verify parcel_detail is sf object with calculations
  expect_s3_class(result$parcel_detail, "sf")
  expect_equal(nrow(result$parcel_detail), 50)
  expect_true(all(c("developable_area", "final_unit_capacity", "district_id") %in%
                    names(result$parcel_detail)))

  # Verify compliance structure
  expect_type(result$compliance, "list")
  expect_true(all(c("compliant", "requirements_met", "summary") %in% names(result$compliance)))

  # Verify metadata
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$community_type, "adjacent")

  # Verify calculations produce reasonable results
  expect_true(result$summary$total_units > 0)
  expect_true(result$summary$total_acres > 0)
  expect_false(any(is.na(result$parcel_detail$final_unit_capacity)))
  expect_true(all(result$parcel_detail$final_unit_capacity >= 0))
})

# Test 2: Multiple districts
test_that("evaluate_compliance handles multiple districts correctly", {
  skip_if_not_installed("sf")

  # Create synthetic municipality with 60 parcels
  municipality <- create_synthetic_parcels(n_parcels = 60, seed = 202)

  # Create 3 districts side-by-side
  district1 <- create_district_polygon("D1", "District 1",
                                       200000, 203000, 900000, 910000)
  district2 <- create_district_polygon("D2", "District 2",
                                       203000, 206000, 900000, 910000)
  district3 <- create_district_polygon("D3", "District 3",
                                       206000, 210000, 900000, 910000)

  districts <- rbind(district1, district2, district3)

  zoning_params <- create_standard_zoning()

  # Run pipeline
  result <- evaluate_compliance(
    municipality = municipality,
    districts = districts,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 30, min_acres = 5),
    verbose = FALSE
  )

  # Verify multiple districts detected
  expect_equal(result$summary$n_districts, 3)
  expect_equal(nrow(result$by_district), 3)

  # Verify all districts have data
  expect_true(all(result$by_district$total_units > 0))
  expect_true(all(result$by_district$total_acres > 0))

  # Verify parcels assigned to districts
  expect_true(all(!is.na(result$parcel_detail$district_id)))
  expect_true(all(result$parcel_detail$district_id[!is.na(result$parcel_detail$district_id)] %in% c("D1", "D2", "D3")))

  # Verify district-level metrics sum to overall
  expect_equal(
    sum(result$by_district$total_units),
    result$summary$total_units,
    tolerance = 0.1
  )
  expect_equal(
    sum(result$by_district$total_acres),
    result$summary$total_acres,
    tolerance = 0.01
  )
})

# Test 3: Station area calculations (rapid_transit community)
test_that("evaluate_compliance calculates station area metrics correctly", {
  skip_if_not_installed("sf")

  # Create synthetic municipality
  municipality <- create_synthetic_parcels(n_parcels = 40, seed = 303)

  # Create district
  district <- create_district_polygon("D1", "Transit District",
                                     199000, 211000, 899000, 911000)

  # Create transit station buffer (circular area around a point)
  station_point <- sf::st_sfc(
    sf::st_point(c(205000, 905000)),
    crs = 26986
  )
  station_buffer <- sf::st_buffer(station_point, dist = 2640)  # 0.5 mile buffer

  zoning_params <- create_standard_zoning()

  # Run pipeline with station areas
  result <- evaluate_compliance(
    municipality = municipality,
    districts = district,
    zoning_params = zoning_params,
    community_type = "rapid_transit",
    transit_stations = station_buffer,
    custom_requirements = list(
      min_units = 100,
      min_acres = 10,
      min_station_area_acres = 5,
      station_area_unit_pct = 50,
      station_area_land_pct = 40
    ),
    verbose = FALSE
  )

  # Verify in_station_area flags are set
  expect_true("in_station_area" %in% names(result$parcel_detail))
  expect_true(any(result$parcel_detail$in_station_area))
  expect_false(all(result$parcel_detail$in_station_area))

  # Verify station area metrics calculated
  expect_true(result$summary$station_area_units >= 0)
  expect_true(result$summary$station_area_acres > 0)
  expect_true(result$summary$developable_station_acres >= 0)

  # Verify station metrics are subset of total
  expect_true(result$summary$station_area_units <= result$summary$total_units)
  expect_true(result$summary$station_area_acres <= result$summary$total_acres)

  # Verify compliance checks include station requirements
  expect_true("station_area_land_ratio" %in% names(result$compliance$requirements_met))
  expect_true("station_area_unit_ratio" %in% names(result$compliance$requirements_met))
})

# Test 4: Compliance pass/fail scenarios
test_that("evaluate_compliance correctly identifies compliant and non-compliant scenarios", {
  skip_if_not_installed("sf")

  # Create small municipality that will fail requirements
  municipality_small <- create_synthetic_parcels(n_parcels = 10, seed = 404)
  district <- create_district_polygon("D1", "Small District",
                                     199000, 211000, 899000, 911000)
  zoning_params <- create_standard_zoning()

  # Test 1: Non-compliant (insufficient units)
  result_fail <- evaluate_compliance(
    municipality = municipality_small,
    districts = district,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 1000, min_acres = 10),  # Unrealistic high
    verbose = FALSE
  )

  expect_false(result_fail$summary$compliant)
  expect_false(result_fail$compliance$requirements_met$min_unit_capacity)
  expect_type(result_fail$compliance$failure_reasons, "character")
  expect_true(length(result_fail$compliance$failure_reasons) > 0)

  # Test 2: Compliant (achievable requirements)
  result_pass <- evaluate_compliance(
    municipality = municipality_small,
    districts = district,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 5, min_acres = 1),  # Achievable
    verbose = FALSE
  )

  expect_true(result_pass$summary$compliant)
  expect_true(result_pass$compliance$requirements_met$min_unit_capacity)
  expect_true(result_pass$compliance$requirements_met$min_land_area)
})

# Test 5: Edge cases
test_that("evaluate_compliance handles edge cases correctly", {
  skip_if_not_installed("sf")

  # Edge case 1: District with all water (high exclusion)
  parcels_water <- sf::st_as_sf(
    data.frame(
      LOC_ID = c("W001", "W002", "W003"),
      SQFT = c(20000, 25000, 30000),
      ACRES = c(20000/43560, 25000/43560, 30000/43560),
      Tot_Exclud = c(20000, 25000, 30000),  # 100% excluded
      x = c(205000, 205100, 205200),
      y = c(905000, 905100, 905200)
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  district <- create_district_polygon("D1", "Water District",
                                     204000, 206000, 904000, 906000)
  zoning_params <- create_standard_zoning()

  result_water <- evaluate_compliance(
    municipality = parcels_water,
    districts = district,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 1, min_acres = 0.1),
    verbose = FALSE
  )

  expect_equal(nrow(result_water$parcel_detail), 3)
  expect_true(all(result_water$parcel_detail$developable_area == 0 |
                    is.na(result_water$parcel_detail$developable_area)))
  expect_true(result_water$summary$total_units == 0 |
                result_water$summary$total_units < 1)

  # Edge case 2: Single parcel district
  parcel_single <- sf::st_as_sf(
    data.frame(
      LOC_ID = "SINGLE001",
      SQFT = 50000,
      ACRES = 50000/43560,
      Tot_Exclud = 5000,
      x = 205000,
      y = 905000
    ),
    coords = c("x", "y"),
    crs = 26986
  )

  result_single <- evaluate_compliance(
    municipality = parcel_single,
    districts = list("D1" = "SINGLE001"),
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 5, min_acres = 0.5),
    verbose = FALSE
  )

  expect_equal(nrow(result_single$parcel_detail), 1)
  expect_equal(nrow(result_single$by_district), 1)
  expect_true(result_single$summary$total_units > 0)

  # Edge case 3: District with no transit stations (adjacent community)
  municipality <- create_synthetic_parcels(n_parcels = 20, seed = 505)

  result_no_stations <- suppressWarnings(evaluate_compliance(
    municipality = municipality,
    districts = district,
    zoning_params = zoning_params,
    community_type = "adjacent",  # No station requirements
    transit_stations = NULL,      # No stations provided
    custom_requirements = list(min_units = 20, min_acres = 5),
    verbose = FALSE
  ))

  # Verify all in_station_area flags are FALSE
  expect_true(all(!result_no_stations$parcel_detail$in_station_area))
  expect_equal(result_no_stations$summary$station_area_units, 0)
  expect_equal(result_no_stations$summary$station_area_acres, 0)

  # Verify compliance checks don't fail on missing station requirements
  expect_false("station_area_land_pct" %in% names(result_no_stations$compliance$requirements_met))
})

# Test 6: Input validation
test_that("evaluate_compliance validates inputs correctly", {
  skip_if_not_installed("sf")

  municipality <- create_synthetic_parcels(n_parcels = 10, seed = 606)
  district <- create_district_polygon("D1", "Test", 199000, 211000, 899000, 911000)
  zoning_params <- create_standard_zoning()

  # Missing required parcel columns
  bad_parcels <- municipality
  bad_parcels$SQFT <- NULL

  expect_error(
    evaluate_compliance(
      municipality = bad_parcels,
      districts = district,
      zoning_params = zoning_params,
      community_type = "adjacent"
    ),
    "missing required columns"
  )

  # Invalid community type
  expect_error(
    suppressWarnings(evaluate_compliance(
      municipality = municipality,
      districts = district,
      zoning_params = zoning_params,
      community_type = "invalid_type"
    )),
    "community_type must be one of"
  )
})

# Test 7: District assignment methods
test_that("evaluate_compliance works with all district assignment methods", {
  skip_if_not_installed("sf")

  municipality <- create_synthetic_parcels(n_parcels = 15, seed = 707)
  zoning_params <- create_standard_zoning()

  # Method 1: Spatial polygon
  district_poly <- create_district_polygon("D1", "Spatial District",
                                          199000, 211000, 899000, 911000)
  result1 <- evaluate_compliance(
    municipality = municipality,
    districts = district_poly,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 10, min_acres = 2),
    verbose = FALSE
  )
  expect_equal(nrow(result1$parcel_detail), 15)

  # Method 2: Column name
  municipality$ZONING_DIST <- sample(c("D1", "D2"), nrow(municipality), replace = TRUE)
  result2 <- evaluate_compliance(
    municipality = municipality,
    districts = "ZONING_DIST",
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 10, min_acres = 2),
    verbose = FALSE
  )
  expect_equal(nrow(result2$by_district), 2)

  # Method 3: Named list
  parcel_ids <- municipality$LOC_ID
  district_list <- list(
    "District_A" = parcel_ids[1:8],
    "District_B" = parcel_ids[9:15]
  )
  result3 <- evaluate_compliance(
    municipality = municipality,
    districts = district_list,
    zoning_params = zoning_params,
    community_type = "adjacent",
    custom_requirements = list(min_units = 10, min_acres = 2),
    verbose = FALSE
  )
  expect_equal(nrow(result3$by_district), 2)
  expect_true(all(c("District_A", "District_B") %in% result3$by_district$district_id))
})

# Test 8: Metadata accuracy
test_that("evaluate_compliance generates accurate metadata", {
  skip_if_not_installed("sf")

  municipality <- create_synthetic_parcels(n_parcels = 10, seed = 808)
  district <- create_district_polygon("D1", "Test", 199000, 211000, 899000, 911000)
  zoning_params <- create_standard_zoning()

  result <- evaluate_compliance(
    municipality = municipality,
    districts = district,
    zoning_params = zoning_params,
    community_type = "commuter_rail",
    community_name = "Test Town",
    custom_requirements = list(min_units = 5, min_acres = 1),
    verbose = FALSE
  )

  # Verify metadata structure
  expect_type(result$metadata, "list")
  expect_equal(result$metadata$community_name, "Test Town")
  expect_equal(result$metadata$community_type, "commuter_rail")
  expect_true(inherits(result$metadata$evaluation_date, "Date"))
  expect_equal(result$metadata$evaluation_date, Sys.Date())
  expect_true("package_version" %in% names(result$metadata))
})