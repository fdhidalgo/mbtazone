test_that("get_district_paths returns the gpkg path for a district", {
  root <- tempfile("mbta_pipeline_data")
  dir.create(root, recursive = TRUE)
  gpkg_path <- file.path(root, "TestTown.gpkg")
  file.create(gpkg_path)

  paths <- get_district_paths("TestTown", "commuter_rail", pipeline_data_dir = root)

  expect_equal(paths$district_name, "TestTown")
  expect_equal(paths$district_type, "commuter_rail")
  expect_equal(paths$gpkg, gpkg_path)
})

test_that("get_district_paths replaces spaces with underscores in the gpkg filename", {
  root <- tempfile("mbta_pipeline_data")
  dir.create(root, recursive = TRUE)
  gpkg_path <- file.path(root, "Test_Town.gpkg")
  file.create(gpkg_path)

  paths <- get_district_paths("Test Town", "adjacent", pipeline_data_dir = root)

  expect_equal(paths$gpkg, gpkg_path)
})

test_that("get_district_paths errors when no GeoPackage exists for the district", {
  root <- tempfile("mbta_pipeline_data")
  dir.create(root, recursive = TRUE)

  expect_error(
    get_district_paths("MissingTown", "commuter_rail", pipeline_data_dir = root),
    "No GeoPackage found"
  )
})
