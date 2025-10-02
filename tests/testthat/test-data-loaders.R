# Tests for Data Loading Functions

test_that("load_municipality loads zip file correctly", {
  # Test with Chelsea Basic shapefile
  chelsea <- load_municipality(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    community_name = "Chelsea"
  )

  # Check structure
  expect_s3_class(chelsea, "sf")
  expect_s3_class(chelsea, "mbtazone_municipality")

  # Check CRS
  expect_equal(sf::st_crs(chelsea)$epsg, 26986)

  # Check metadata
  expect_equal(attr(chelsea, "community_name"), "Chelsea")
  expect_equal(attr(chelsea, "crs_epsg"), 26986)
  expect_true(is.numeric(attr(chelsea, "n_parcels")))

  # Check required columns present
  expect_true(all(c("LOC_ID", "SQFT", "ACRES", "TRANSIT") %in% names(chelsea)))
})

test_that("load_municipality handles sf objects", {
  # Load shapefile first
  temp_dir <- tempdir()
  utils::unzip(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    exdir = temp_dir
  )
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  data <- sf::st_read(shp_file, quiet = TRUE)

  # Pass sf object to load_municipality
  result <- load_municipality(data, community_name = "Chelsea")

  expect_s3_class(result, "sf")
  expect_s3_class(result, "mbtazone_municipality")
  expect_equal(attr(result, "community_name"), "Chelsea")
})

test_that("load_municipality validates required columns", {
  # Create invalid sf object (missing columns)
  temp_dir <- tempdir()
  utils::unzip(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    exdir = temp_dir
  )
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  data <- sf::st_read(shp_file, quiet = TRUE)

  # Remove required column
  data$SQFT <- NULL

  # Should error on validation
  expect_error(
    load_municipality(data, validate = TRUE),
    "Missing required columns.*SQFT"
  )
})

test_that("load_municipality skips validation when requested", {
  # Create sf object missing columns
  temp_dir <- tempdir()
  utils::unzip(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    exdir = temp_dir
  )
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  data <- sf::st_read(shp_file, quiet = TRUE)
  data$SQFT <- NULL

  # Should not error when validate=FALSE
  expect_no_error(
    load_municipality(data, validate = FALSE)
  )
})

test_that("load_municipality transforms CRS correctly", {
  # Load data
  temp_dir <- tempdir()
  utils::unzip(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    exdir = temp_dir
  )
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  data <- sf::st_read(shp_file, quiet = TRUE)

  # Transform to different CRS
  data_4326 <- sf::st_transform(data, crs = 4326)

  # Load with target projection
  result <- load_municipality(data_4326, projection = 26986)

  # Should be transformed back
  expect_equal(sf::st_crs(result)$epsg, 26986)
  expect_true(attr(result, "crs_transformed"))
})

test_that("load_municipality handles missing file gracefully", {
  expect_error(
    load_municipality("nonexistent.shp"),
    "Shapefile not found"
  )
})

test_that("load_municipality validates data types", {
  # Load valid data
  chelsea <- load_municipality(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    community_name = "Chelsea",
    validate = TRUE
  )

  # Check numeric columns are numeric
  expect_type(chelsea$SQFT, "double")
  expect_type(chelsea$ACRES, "double")
  expect_type(chelsea$PublicInst, "double")
  expect_type(chelsea$NonPubExc, "double")
  expect_type(chelsea$Tot_Exclud, "double")
  expect_type(chelsea$Tot_Sensit, "double")

  # Check character columns are character
  expect_type(chelsea$LOC_ID, "character")
  expect_type(chelsea$TRANSIT, "character")
})

test_that("load_municipality warns about excessive NAs", {
  # Load data
  temp_dir <- tempdir()
  utils::unzip(
    test_path("../../inst/extdata/parcels/57_CHELSEA_basic.zip"),
    exdir = temp_dir
  )
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  data <- sf::st_read(shp_file, quiet = TRUE)

  # Add excessive NAs to SQFT (>10%)
  data$SQFT[1:500] <- NA

  # Should warn
  expect_warning(
    load_municipality(data, validate = TRUE),
    "SQFT.*NA values"
  )
})

# Test with multiple communities
test_that("load_municipality works with Cambridge", {
  cambridge <- load_municipality(
    test_path("../../inst/extdata/parcels/49_CAMBRIDGE_basic.zip"),
    community_name = "Cambridge"
  )

  expect_s3_class(cambridge, "mbtazone_municipality")
  expect_equal(attr(cambridge, "community_name"), "Cambridge")
  expect_true(nrow(cambridge) > 0)
})

test_that("load_municipality works with Somerville", {
  somerville <- load_municipality(
    test_path("../../inst/extdata/parcels/274_SOMERVILLE_basic.zip"),
    community_name = "Somerville"
  )

  expect_s3_class(somerville, "mbtazone_municipality")
  expect_equal(attr(somerville, "community_name"), "Somerville")
  expect_true(nrow(somerville) > 0)
})

test_that("load_municipality works with Wellesley", {
  wellesley <- load_municipality(
    test_path("../../inst/extdata/parcels/317_WELLESLEY_basic.zip"),
    community_name = "Wellesley"
  )

  expect_s3_class(wellesley, "mbtazone_municipality")
  expect_true(nrow(wellesley) > 0)
})

test_that("load_municipality works with Newton", {
  newton <- load_municipality(
    test_path("../../inst/extdata/parcels/207_NEWTON_basic.zip"),
    community_name = "Newton"
  )

  expect_s3_class(newton, "mbtazone_municipality")
  expect_true(nrow(newton) > 0)
})

test_that("load_municipality works with Lincoln", {
  lincoln <- load_municipality(
    test_path("../../inst/extdata/parcels/157_LINCOLN_basic.zip"),
    community_name = "Lincoln"
  )

  expect_s3_class(lincoln, "mbtazone_municipality")
  expect_true(nrow(lincoln) > 0)
})

test_that("load_municipality works with Maynard", {
  maynard <- load_municipality(
    test_path("../../inst/extdata/parcels/174_MAYNARD_basic.zip"),
    community_name = "Maynard"
  )

  expect_s3_class(maynard, "mbtazone_municipality")
  expect_true(nrow(maynard) > 0)
})

# Validation helper tests
test_that("validate_parcel_data checks sf object type", {
  expect_error(
    validate_parcel_data(data.frame(), c("col1"), strict = TRUE),
    "must be an sf spatial object"
  )
})

test_that("validate_parcel_data checks CRS", {
  # Create sf object without CRS
  data <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    )
  )

  expect_error(
    validate_parcel_data(data, c("id"), strict = TRUE),
    "CRS must be set"
  )
})

test_that("validate_parcel_data checks required columns", {
  # Create valid sf object
  data <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 26986
    )
  )

  expect_error(
    validate_parcel_data(data, c("id", "missing_col"), strict = TRUE),
    "Missing required columns.*missing_col"
  )
})

test_that("validate_parcel_data returns TRUE for valid data", {
  # Create valid sf object
  data <- sf::st_sf(
    id = 1:3,
    name = c("a", "b", "c"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2)),
      crs = 26986
    )
  )

  expect_true(
    validate_parcel_data(data, c("id", "name"), strict = TRUE)
  )
})

# Tests for load_transit_stations()

test_that("load_transit_stations loads with default path", {
  transit <- load_transit_stations()

  # Check structure
  expect_s3_class(transit, "sf")
  expect_s3_class(transit, "mbtazone_transit_stations")

  # Check CRS
  expect_equal(sf::st_crs(transit)$epsg, 26986)

  # Check metadata
  expect_equal(attr(transit, "crs_epsg"), 26986)
  expect_true(is.numeric(attr(transit, "n_features")))
})

test_that("load_transit_stations validates geometry type", {
  transit <- load_transit_stations()

  # Check geometry types
  geom_types <- unique(as.character(sf::st_geometry_type(transit)))
  expect_true(all(geom_types %in% c("POLYGON", "MULTIPOLYGON")))
})

test_that("load_transit_stations returns expected feature count", {
  transit <- load_transit_stations()

  # Should have 1 feature (combined buffers)
  expect_equal(nrow(transit), 1)
  expect_equal(attr(transit, "n_features"), 1)
})

test_that("load_transit_stations handles custom path", {
  custom_path <- system.file(
    "extdata/statewide/Transit_Station_Areas_Half_Mile_Radius.zip",
    package = "mbtazone"
  )

  transit <- load_transit_stations(custom_path)

  expect_s3_class(transit, "mbtazone_transit_stations")
  expect_equal(attr(transit, "source_file"), custom_path)
})

test_that("load_transit_stations handles missing file", {
  expect_error(
    load_transit_stations("nonexistent.shp"),
    "Transit station shapefile not found"
  )
})

test_that("load_transit_stations adds correct metadata", {
  transit <- load_transit_stations()

  expect_true(!is.null(attr(transit, "source_file")))
  expect_equal(attr(transit, "crs_epsg"), 26986)
  expect_s3_class(attr(transit, "load_date"), "Date")
  expect_true(is.logical(attr(transit, "crs_transformed")))
})

test_that("load_transit_stations transforms CRS if needed", {
  # Load with different target projection
  # (This won't actually transform since source is 26986, but tests the logic)
  transit <- load_transit_stations(projection = 26986)

  expect_equal(sf::st_crs(transit)$epsg, 26986)
})

test_that("load_transit_stations has expected columns", {
  transit <- load_transit_stations()

  # Should have Shape_Leng, Shape_Area, geometry
  expect_true("Shape_Leng" %in% names(transit) || "SHAPE_Leng" %in% names(transit))
  expect_true("Shape_Area" %in% names(transit) || "SHAPE_Area" %in% names(transit))
  expect_true("geometry" %in% names(transit))
})

# Tests for load_density_deductions()

test_that("load_density_deductions loads with default path", {
  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  deductions <- load_density_deductions()

  # Check structure
  expect_s3_class(deductions, "sf")
  expect_s3_class(deductions, "mbtazone_density_deductions")

  # Check CRS
  expect_equal(sf::st_crs(deductions)$epsg, 26986)

  # Check metadata
  expect_equal(attr(deductions, "crs_epsg"), 26986)
  expect_true(is.numeric(attr(deductions, "n_features")))
})

test_that("load_density_deductions validates geometry type", {
  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  deductions <- load_density_deductions()

  # Check geometry types
  geom_types <- unique(as.character(sf::st_geometry_type(deductions)))
  expect_true(all(geom_types %in% c("POLYGON", "MULTIPOLYGON")))
})

test_that("load_density_deductions returns many features", {
  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  deductions <- load_density_deductions()

  # Should have many features (>80000)
  expect_gt(nrow(deductions), 80000)
  expect_equal(attr(deductions, "n_features"), nrow(deductions))
})

test_that("load_density_deductions handles custom path", {
  custom_path <- system.file(
    "extdata/statewide/Density_Denominator_Deductions.zip",
    package = "mbtazone"
  )

  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  deductions <- load_density_deductions(custom_path)

  expect_s3_class(deductions, "mbtazone_density_deductions")
  expect_equal(attr(deductions, "source_file"), custom_path)
})

test_that("load_density_deductions handles missing file", {
  expect_error(
    load_density_deductions("nonexistent.shp"),
    "Density deduction shapefile not found"
  )
})

test_that("load_density_deductions adds correct metadata", {
  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  deductions <- load_density_deductions()

  expect_true(!is.null(attr(deductions, "source_file")))
  expect_equal(attr(deductions, "crs_epsg"), 26986)
  expect_s3_class(attr(deductions, "load_date"), "Date")
  expect_true(is.logical(attr(deductions, "crs_transformed")))
})

test_that("load_density_deductions transforms CRS if needed", {
  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  # Load with different target projection
  deductions <- load_density_deductions(projection = 26986)

  expect_equal(sf::st_crs(deductions)$epsg, 26986)
})

test_that("load_density_deductions has expected columns", {
  skip_if(
    Sys.getenv("SKIP_LARGE_TESTS") == "true",
    "Skipping large file test"
  )

  deductions <- load_density_deductions()

  # Should have SHAPE_Leng, SHAPE_Area, geometry
  expect_true("SHAPE_Leng" %in% names(deductions) || "Shape_Leng" %in% names(deductions))
  expect_true("SHAPE_Area" %in% names(deductions) || "Shape_Area" %in% names(deductions))
  expect_true("geometry" %in% names(deductions))
})