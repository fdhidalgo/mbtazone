# GIS Operations Tests
#
# Tests for spatial analysis functions using submitted district shapefiles.
# These tests validate GIS operations against real-world compliance districts.

# Test communities with available district shapefiles
# Note: Chelsea and Cambridge fixtures not yet created
TEST_COMMUNITIES_GIS <- list(
  "Somerville" = list(
    name = "Somerville",
    expected_area_acres = 99.43,
    expected_features = 7,
    expected_contiguous_pct = 0.824,
    expected_n_portions = 13,
    expected_passes_50pct = TRUE,
    expected_has_small_portions = TRUE
  ),
  "Maynard" = list(
    name = "Maynard",
    expected_area_acres = 3.44,
    expected_features = 1,
    expected_contiguous_pct = 1.0,
    expected_n_portions = 1,
    expected_passes_50pct = TRUE,
    expected_has_small_portions = FALSE
  ),
  "Newton" = list(
    name = "Newton",
    expected_area_acres = 22.58,
    expected_features = 8,
    expected_contiguous_pct = 0.531,
    expected_n_portions = 8,
    expected_passes_50pct = TRUE,
    expected_has_small_portions = TRUE
  ),
  "Lincoln" = list(
    name = "Lincoln",
    expected_area_acres = 6.65,
    expected_features = 4,
    expected_contiguous_pct = 1.0,
    expected_n_portions = 1,
    expected_passes_50pct = TRUE,
    expected_has_small_portions = FALSE
  )
)

# Helper to load district shapefile from fixture
get_district_shapefile <- function(community_name) {
  fixture_file <- test_path("fixtures", paste0(tolower(community_name), "_district1.rds"))
  fixture <- readRDS(fixture_file)

  if (!"district_shapefile" %in% names(fixture)) {
    skip(paste(community_name, "fixture does not include district shapefile"))
  }

  return(fixture$district_shapefile)
}

# ============================================================================
# calculate_district_area() Tests
# ============================================================================

test_that("calculate_district_area returns correct area for submitted districts", {
  for (comm_info in TEST_COMMUNITIES_GIS) {
    district_sf <- get_district_shapefile(comm_info$name)

    # Test acres output (default)
    area_acres <- calculate_district_area(district_sf, output_unit = "acres")
    expect_equal(
      area_acres,
      comm_info$expected_area_acres,
      tolerance = 0.1,
      info = paste(comm_info$name, "district area in acres")
    )

    # Test square feet output
    area_sqft <- calculate_district_area(district_sf, output_unit = "sqft")
    expect_equal(
      area_sqft,
      comm_info$expected_area_acres * 43560,
      tolerance = 4356,  # 0.1 acres in sqft
      info = paste(comm_info$name, "district area in square feet")
    )
  }
})

test_that("calculate_district_area validates CRS", {
  district_sf <- get_district_shapefile("Maynard")

  # Should work with correct CRS
  expect_no_error(calculate_district_area(district_sf))

  # Should error with wrong CRS
  district_wrong_crs <- sf::st_transform(district_sf, 4326)  # WGS84
  expect_error(
    calculate_district_area(district_wrong_crs),
    regexp = "26986",
    info = "Should validate CRS is EPSG:26986"
  )
})

test_that("calculate_district_area handles empty geometries", {
  # Create empty sf object with correct CRS
  empty_sf <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_warning(
    result <- calculate_district_area(empty_sf),
    regexp = "empty geometry"
  )
  expect_true(is.na(result))
})

test_that("calculate_district_area requires sf object", {
  expect_error(
    calculate_district_area(data.frame(x = 1)),
    regexp = "must be an sf object"
  )
})

test_that("calculate_district_area requires defined CRS", {
  district_sf <- get_district_shapefile("Maynard")
  # Remove CRS
  sf::st_crs(district_sf) <- NA

  expect_error(
    calculate_district_area(district_sf),
    regexp = "no CRS"
  )
})

# ============================================================================
# validate_contiguity() Tests
# ============================================================================

test_that("validate_contiguity correctly identifies contiguous districts", {
  # Maynard and Lincoln are 100% contiguous districts
  for (comm_name in c("Maynard", "Lincoln")) {
    district_sf <- get_district_shapefile(comm_name)

    result <- validate_contiguity(district_sf)

    expect_type(result, "list")
    expect_true(result$is_valid, info = paste(comm_name, "should be contiguous"))
    expect_equal(result$contiguous_pct, 1.0, tolerance = 0.01)
    expect_equal(result$n_portions, 1)
    expect_length(result$violations, 0)
  }
})

test_that("validate_contiguity returns correct structure", {
  district_sf <- get_district_shapefile("Somerville")

  result <- validate_contiguity(district_sf)

  # Check return structure
  expect_type(result, "list")
  expect_named(result, c("is_valid", "contiguous_pct", "n_portions", "portion_areas", "violations"))

  expect_type(result$is_valid, "logical")
  expect_type(result$contiguous_pct, "double")
  expect_type(result$n_portions, "integer")
  expect_type(result$portion_areas, "double")
  expect_type(result$violations, "character")

  # contiguous_pct should be between 0 and 1
  expect_gte(result$contiguous_pct, 0)
  expect_lte(result$contiguous_pct, 1)

  # n_portions should match length of portion_areas
  expect_equal(result$n_portions, length(result$portion_areas))

  # portion_areas should be sorted largest to smallest
  expect_equal(result$portion_areas, sort(result$portion_areas, decreasing = TRUE))
})

test_that("validate_contiguity validates against expected contiguity percentages", {
  for (comm_info in TEST_COMMUNITIES_GIS) {
    district_sf <- get_district_shapefile(comm_info$name)

    result <- validate_contiguity(district_sf)

    # Check contiguous percentage matches expected
    expect_equal(
      result$contiguous_pct,
      comm_info$expected_contiguous_pct,
      tolerance = 0.01,
      info = paste(comm_info$name, "contiguous percentage")
    )

    # Check number of portions
    expect_equal(
      result$n_portions,
      comm_info$expected_n_portions,
      info = paste(comm_info$name, "number of portions")
    )

    # Check 50% requirement
    if (comm_info$expected_passes_50pct) {
      expect_gte(result$contiguous_pct, 0.5, info = paste(comm_info$name, "should meet 50%"))
    }
  }
})

test_that("validate_contiguity respects min_contiguous_pct parameter", {
  district_sf <- get_district_shapefile("Newton")

  # Newton is 53.1% - should pass 50% but fail 75%
  result_50 <- validate_contiguity(district_sf, min_contiguous_pct = 0.5)
  expect_true(result_50$is_valid)

  result_75 <- validate_contiguity(district_sf, min_contiguous_pct = 0.75)
  expect_false(result_75$is_valid)
  expect_true(any(grepl("75%", result_75$violations)))
})

test_that("validate_contiguity identifies minimum portion size violations", {
  # Somerville and Newton have portions < 5 acres
  for (comm_name in c("Somerville", "Newton")) {
    district_sf <- get_district_shapefile(comm_name)

    result <- validate_contiguity(district_sf, min_portion_acres = 5)

    # Check that violations are reported for portions < 5 acres
    small_portions <- result$portion_areas[result$portion_areas < 5]
    if (length(small_portions) > 0) {
      expect_false(result$is_valid)
      expect_true(any(grepl("5 acres", result$violations)))
      expect_true(any(grepl(as.character(length(small_portions)), result$violations)))
    }
  }
})

test_that("validate_contiguity handles empty geometries", {
  # Create empty sf object with correct CRS
  empty_sf <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  result <- validate_contiguity(empty_sf)

  expect_true(is.na(result$is_valid))
  expect_true(is.na(result$contiguous_pct))
  expect_equal(result$n_portions, 0)
  expect_true(any(grepl("empty", result$violations)))
})

test_that("validate_contiguity validates CRS", {
  district_sf <- get_district_shapefile("Maynard")

  # Should work with correct CRS
  expect_no_error(validate_contiguity(district_sf))

  # Should error with wrong CRS
  district_wrong_crs <- sf::st_transform(district_sf, 4326)
  expect_error(
    validate_contiguity(district_wrong_crs),
    regexp = "26986"
  )
})

test_that("validate_contiguity validates parameter ranges", {
  district_sf <- get_district_shapefile("Maynard")

  # Invalid min_contiguous_pct
  expect_error(
    validate_contiguity(district_sf, min_contiguous_pct = 1.5),
    regexp = "between 0 and 1"
  )

  expect_error(
    validate_contiguity(district_sf, min_contiguous_pct = -0.1),
    regexp = "between 0 and 1"
  )

  # Invalid min_portion_acres
  expect_error(
    validate_contiguity(district_sf, min_portion_acres = -1),
    regexp = "non-negative"
  )
})

# ============================================================================
# calculate_station_area_percentage() Tests
# ============================================================================

test_that("calculate_station_area_percentage returns valid percentages", {
  # Use cached transit station areas
  station_areas <- get_cached_transit_stations()

  # Test with a district (Somerville is in transit areas per docs)
  district_sf <- get_district_shapefile("Somerville")

  pct <- calculate_station_area_percentage(district_sf, station_areas)

  expect_type(pct, "double")
  expect_gte(pct, 0)
  expect_lte(pct, 100)
})

test_that("calculate_station_area_percentage returns 0 for districts outside transit areas", {
  # Use cached transit station areas
  station_areas <- get_cached_transit_stations()

  # Maynard has no direct MBTA service (per DISTRICT_PROPERTIES.md)
  district_sf <- get_district_shapefile("Maynard")

  pct <- calculate_station_area_percentage(district_sf, station_areas)

  # Should return 0 (not NA) for no transit overlap
  expect_equal(pct, 0)
})

test_that("calculate_station_area_percentage handles empty station areas", {
  district_sf <- get_district_shapefile("Maynard")

  # Create empty station areas
  empty_stations <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_warning(
    pct <- calculate_station_area_percentage(district_sf, empty_stations),
    regexp = "empty"
  )
  expect_equal(pct, 0)
})

test_that("calculate_station_area_percentage handles empty district", {
  station_areas <- get_cached_transit_stations()

  # Create empty district
  empty_district <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_warning(
    pct <- calculate_station_area_percentage(empty_district, station_areas),
    regexp = "empty"
  )
  expect_true(is.na(pct))
})

test_that("calculate_station_area_percentage validates CRS", {
  district_sf <- get_district_shapefile("Maynard")
  station_areas <- get_cached_transit_stations()

  # Should work with correct CRS
  expect_no_error(calculate_station_area_percentage(district_sf, station_areas))

  # Should error with wrong CRS on district
  district_wrong_crs <- sf::st_transform(district_sf, 4326)
  expect_error(
    calculate_station_area_percentage(district_wrong_crs, station_areas),
    regexp = "26986"
  )

  # Should error with wrong CRS on stations
  stations_wrong_crs <- sf::st_transform(station_areas, 4326)
  expect_error(
    calculate_station_area_percentage(district_sf, stations_wrong_crs),
    regexp = "26986"
  )
})

test_that("calculate_station_area_percentage requires sf objects", {
  district_sf <- get_district_shapefile("Maynard")

  expect_error(
    calculate_station_area_percentage(data.frame(x = 1), district_sf),
    regexp = "must be an sf object"
  )

  expect_error(
    calculate_station_area_percentage(district_sf, data.frame(x = 1)),
    regexp = "must be an sf object"
  )
})

# ============================================================================
# calculate_station_intersection() Tests
# ============================================================================

test_that("calculate_station_intersection adds station_area_sf column", {
  # Create synthetic test parcels within Somerville district
  district_sf <- get_district_shapefile("Somerville")

  # Create simple parcel geometries (squares within district)
  parcel_geoms <- sf::st_make_grid(
    sf::st_buffer(district_sf, -500),  # Slightly inset from district
    n = c(3, 3),
    what = "polygons"
  )

  parcels <- sf::st_sf(
    parcel_id = 1:length(parcel_geoms),
    geometry = parcel_geoms,
    crs = 26986
  )

  # Use cached transit station areas
  station_areas <- get_cached_transit_stations()

  # Calculate intersection
  result <- calculate_station_intersection(parcels, station_areas, output_unit = "sqft")

  # Check that column was added
  expect_true("station_area_sf" %in% names(result))
  expect_type(result$station_area_sf, "double")
  expect_true(all(result$station_area_sf >= 0))
  expect_equal(nrow(result), nrow(parcels))
})

test_that("calculate_station_intersection handles parcels with no transit overlap", {
  # Create a parcel outside all transit areas (far western MA)
  parcel_geom <- sf::st_sfc(
    sf::st_point(c(200000, 900000)),  # Coordinates outside transit areas
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_buffer(parcel_geom, 1000),
    crs = 26986
  )

  # Use cached transit station areas
  station_areas <- get_cached_transit_stations()

  # Calculate intersection
  result <- calculate_station_intersection(parcels, station_areas, output_unit = "sqft")

  # Parcels outside station areas should get 0 (not NA)
  expect_equal(result$station_area_sf[1], 0)
  expect_false(is.na(result$station_area_sf[1]))
})

test_that("calculate_station_intersection handles empty parcels", {
  # Create empty parcels
  empty_parcels <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  station_areas <- get_cached_transit_stations()

  expect_warning(
    result <- calculate_station_intersection(empty_parcels, station_areas),
    regexp = "empty"
  )

  expect_true("station_area_sf" %in% names(result))
})

test_that("calculate_station_intersection handles empty station areas", {
  # Create simple parcel
  parcel_geom <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_buffer(parcel_geom, 1000),
    crs = 26986
  )

  # Create empty station areas
  empty_stations <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_warning(
    result <- calculate_station_intersection(parcels, empty_stations),
    regexp = "empty"
  )

  expect_equal(result$station_area_sf[1], 0)
})

test_that("calculate_station_intersection validates CRS", {
  # Create simple parcel
  parcel_geom <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_buffer(parcel_geom, 1000),
    crs = 26986
  )

  station_areas <- get_cached_transit_stations()

  # Should work with correct CRS
  expect_no_error(calculate_station_intersection(parcels, station_areas))

  # Should error with wrong CRS on parcels
  parcels_wrong_crs <- sf::st_transform(parcels, 4326)
  expect_error(
    calculate_station_intersection(parcels_wrong_crs, station_areas),
    regexp = "26986"
  )

  # Should error with wrong CRS on stations
  stations_wrong_crs <- sf::st_transform(station_areas, 4326)
  expect_error(
    calculate_station_intersection(parcels, stations_wrong_crs),
    regexp = "26986"
  )
})

test_that("calculate_station_intersection converts units correctly", {
  # Create simple parcel
  parcel_geom <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_buffer(parcel_geom, 1000),
    crs = 26986
  )

  station_areas <- get_cached_transit_stations()

  # Get result in both units
  result_sqft <- calculate_station_intersection(parcels, station_areas, output_unit = "sqft")
  result_acres <- calculate_station_intersection(parcels, station_areas, output_unit = "acres")

  # Check conversion (1 acre = 43560 sqft)
  if (result_sqft$station_area_sf[1] > 0) {
    expect_equal(
      result_acres$station_area_sf[1],
      result_sqft$station_area_sf[1] / 43560,
      tolerance = 0.0001
    )
  }
})

# ============================================================================
# calculate_density_denominator() Tests
# ============================================================================

test_that("calculate_density_denominator returns area minus deductions", {
  # Use cached density deductions
  deduction_layers <- get_cached_density_deductions()

  # Test with Newton which has deductions per DISTRICT_PROPERTIES.md
  district_sf <- get_district_shapefile("Newton")

  result_acres <- calculate_density_denominator(
    district_sf,
    deduction_layers,
    output_unit = "acres"
  )

  # Should be less than or equal to total district area
  total_area <- calculate_district_area(district_sf, output_unit = "acres")
  expect_lte(result_acres, total_area)
  expect_gte(result_acres, 0)

  # Test square feet output
  result_sqft <- calculate_density_denominator(
    district_sf,
    deduction_layers,
    output_unit = "sqft"
  )

  expect_equal(
    result_acres,
    result_sqft / 43560,
    tolerance = 0.001
  )
})

test_that("calculate_density_denominator handles districts with no deductions", {
  deduction_layers <- get_cached_density_deductions()

  # Maynard is a small district, may have minimal deductions
  district_sf <- get_district_shapefile("Maynard")

  result <- calculate_density_denominator(
    district_sf,
    deduction_layers,
    output_unit = "acres"
  )

  total_area <- calculate_district_area(district_sf, output_unit = "acres")

  # Result should be close to total area (allowing for small deductions)
  expect_lte(result, total_area)
  expect_gte(result, 0)
})

test_that("calculate_density_denominator handles empty deduction layers", {
  district_sf <- get_district_shapefile("Maynard")

  # Create empty deduction layers
  empty_deductions <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_message(
    result <- calculate_density_denominator(district_sf, empty_deductions),
    regexp = "No deductions"
  )

  # Should return full district area
  total_area <- calculate_district_area(district_sf, output_unit = "acres")
  expect_equal(result, total_area, tolerance = 0.001)
})

test_that("calculate_density_denominator handles empty district", {
  deduction_layers <- get_cached_density_deductions()

  # Create empty district
  empty_district <- sf::st_sf(
    geometry = sf::st_sfc(crs = 26986)
  )

  expect_warning(
    result <- calculate_density_denominator(empty_district, deduction_layers),
    regexp = "NA"
  )

  expect_true(is.na(result))
})

test_that("calculate_density_denominator validates CRS", {
  district_sf <- get_district_shapefile("Maynard")
  deduction_layers <- get_cached_density_deductions()

  # Should work with correct CRS
  expect_no_error(calculate_density_denominator(district_sf, deduction_layers))

  # Should error with wrong CRS on district
  district_wrong_crs <- sf::st_transform(district_sf, 4326)
  expect_error(
    calculate_density_denominator(district_wrong_crs, deduction_layers),
    regexp = "26986"
  )

  # Should error with wrong CRS on deductions
  deductions_wrong_crs <- sf::st_transform(deduction_layers, 4326)
  expect_error(
    calculate_density_denominator(district_sf, deductions_wrong_crs),
    regexp = "26986"
  )
})

test_that("calculate_density_denominator requires sf objects", {
  district_sf <- get_district_shapefile("Maynard")

  expect_error(
    calculate_density_denominator(data.frame(x = 1), district_sf),
    regexp = "must be an sf object"
  )

  expect_error(
    calculate_density_denominator(district_sf, data.frame(x = 1)),
    regexp = "must be an sf object"
  )
})

# ============================================================================
# identify_excluded_land() Tests
# ============================================================================

test_that("identify_excluded_land returns total excluded area", {
  # Create synthetic parcels with exclusion data
  parcel_geoms <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    sf::st_point(c(751000, 950000)),
    sf::st_point(c(752000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1:3,
    Tot_Exclud = c(5000, 10000, 0),  # Square feet
    geometry = sf::st_buffer(parcel_geoms, 500),
    crs = 26986
  )

  result <- identify_excluded_land(parcels, by_category = FALSE)

  expect_type(result, "double")
  expect_equal(result, 15000)  # 5000 + 10000 + 0
})

test_that("identify_excluded_land aggregates excluded land by category", {
  # Create synthetic parcels with category exclusion data
  parcel_geoms <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    sf::st_point(c(751000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1:2,
    Tot_Exclud = c(10000, 20000),
    PublicInst = c(3000, 5000),
    NonPubExc = c(7000, 15000),
    geometry = sf::st_buffer(parcel_geoms, 500),
    crs = 26986
  )

  result <- identify_excluded_land(parcels, by_category = TRUE)

  expect_type(result, "double")
  expect_named(result, c("PublicInst", "NonPubExc", "Total"))
  expect_equal(result["PublicInst"], 8000)   # 3000 + 5000
  expect_equal(result["NonPubExc"], 22000)   # 7000 + 15000
  expect_equal(result["Total"], 30000)       # 10000 + 20000
})

test_that("identify_excluded_land handles NA values", {
  # Create parcels with some NA exclusions
  parcel_geoms <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    sf::st_point(c(751000, 950000)),
    sf::st_point(c(752000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1:3,
    Tot_Exclud = c(5000, NA, 10000),
    geometry = sf::st_buffer(parcel_geoms, 500),
    crs = 26986
  )

  # Should treat NA as 0
  result <- identify_excluded_land(parcels, by_category = FALSE)

  expect_equal(result, 15000)  # 5000 + 0 + 10000
})

test_that("identify_excluded_land warns about high NA percentage", {
  # Create parcels with >10% NA exclusions
  parcel_geoms <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    sf::st_point(c(751000, 950000)),
    sf::st_point(c(752000, 950000)),
    sf::st_point(c(753000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1:4,
    Tot_Exclud = c(5000, NA, NA, 10000),  # 50% NA
    geometry = sf::st_buffer(parcel_geoms, 500),
    crs = 26986
  )

  expect_warning(
    result <- identify_excluded_land(parcels),
    regexp = "NA exclusion"
  )
})

test_that("identify_excluded_land requires Tot_Exclud column", {
  # Create parcels without Tot_Exclud column
  parcel_geoms <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1,
    geometry = sf::st_buffer(parcel_geoms, 500),
    crs = 26986
  )

  expect_error(
    identify_excluded_land(parcels),
    regexp = "Tot_Exclud"
  )
})

test_that("identify_excluded_land warns when category columns missing", {
  # Create parcels without category columns
  parcel_geoms <- sf::st_sfc(
    sf::st_point(c(750000, 950000)),
    crs = 26986
  )

  parcels <- sf::st_sf(
    parcel_id = 1,
    Tot_Exclud = 5000,
    geometry = sf::st_buffer(parcel_geoms, 500),
    crs = 26986
  )

  expect_warning(
    result <- identify_excluded_land(parcels, by_category = TRUE),
    regexp = "No exclusion category columns"
  )

  # Should return total only
  expect_named(result, "Total")
  expect_equal(result["Total"], 5000)
})

test_that("identify_excluded_land works with data.frame (not just sf)", {
  # Create regular data.frame (no spatial component)
  parcels <- data.frame(
    parcel_id = 1:3,
    Tot_Exclud = c(5000, 10000, 15000)
  )

  result <- identify_excluded_land(parcels, by_category = FALSE)

  expect_equal(result, 30000)
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("GIS operations work together in a typical workflow", {
  # Load all necessary data
  district_sf <- get_district_shapefile("Somerville")
  station_areas <- get_cached_transit_stations()
  deduction_layers <- get_cached_density_deductions()

  # Calculate district properties
  total_area <- calculate_district_area(district_sf, output_unit = "acres")
  contiguity <- validate_contiguity(district_sf)
  station_pct <- calculate_station_area_percentage(district_sf, station_areas)
  density_denom <- calculate_density_denominator(district_sf, deduction_layers)

  # All results should be valid
  expect_gt(total_area, 0)
  expect_type(contiguity$is_valid, "logical")
  expect_gte(station_pct, 0)
  expect_lte(station_pct, 100)
  expect_lte(density_denom, total_area)
  expect_gte(density_denom, 0)
})

test_that("All GIS functions work on all test communities", {
  # Quick smoke test to ensure all functions work on all fixtures
  station_areas <- get_cached_transit_stations()
  deduction_layers <- get_cached_density_deductions()

  for (comm_info in TEST_COMMUNITIES_GIS) {
    district_sf <- get_district_shapefile(comm_info$name)

    # All functions should work without error
    expect_no_error(
      calculate_district_area(district_sf),
      info = paste(comm_info$name, "- calculate_district_area")
    )

    expect_no_error(
      validate_contiguity(district_sf),
      info = paste(comm_info$name, "- validate_contiguity")
    )

    expect_no_error(
      calculate_station_area_percentage(district_sf, station_areas),
      info = paste(comm_info$name, "- calculate_station_area_percentage")
    )

    expect_no_error(
      calculate_density_denominator(district_sf, deduction_layers),
      info = paste(comm_info$name, "- calculate_density_denominator")
    )
  }
})

# ============================================================================
# Performance Tests
# ============================================================================

test_that("GIS operations complete quickly on test datasets", {
  # Test with largest district (Somerville: 99.43 acres, 7 features)
  district_sf <- get_district_shapefile("Somerville")
  station_areas <- get_cached_transit_stations()
  deduction_layers <- get_cached_density_deductions()

  # Basic operations should be fast
  time_start <- Sys.time()
  area <- calculate_district_area(district_sf)
  contiguity <- validate_contiguity(district_sf)
  station_pct <- calculate_station_area_percentage(district_sf, station_areas)
  density_denom <- calculate_density_denominator(district_sf, deduction_layers)
  time_elapsed <- as.numeric(Sys.time() - time_start, units = "secs")

  # Should complete in reasonable time (generous limit for CI environments)
  expect_lt(time_elapsed, 10.0, info = "Should complete quickly for small districts")
})