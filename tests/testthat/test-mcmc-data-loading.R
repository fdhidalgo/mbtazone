test_that("get_district_paths uses parcels_subdir argument", {
  root <- tempfile("mbta")
  dir.create(file.path(root, "custom_parcels"), recursive = TRUE)
  dir.create(file.path(root, "mbta_district_shapefiles", "TestTown"), recursive = TRUE)
  dir.create(file.path(root, "mbta_district_models"), recursive = TRUE)
  zip_path <- file.path(root, "custom_parcels", "99_TESTTOWN_basic.zip")
  file.create(zip_path)
  file.create(file.path(root, "mbta_district_shapefiles", "TestTown", "d.shp"))
  file.create(file.path(root, "mbta_district_models", "TestTown - CM model.xlsx"))
  paths <- get_district_paths(
    "TestTown",
    "commuter_rail",
    data_root = root,
    parcels_subdir = "custom_parcels"
  )
  expect_equal(paths$parcels, zip_path)
})

test_that("get_district_paths uses MBTAZONE_PARCELS_SUBDIR when parcels_subdir is NULL", {
  root <- tempfile("mbta")
  dir.create(file.path(root, "env_parcels"), recursive = TRUE)
  dir.create(file.path(root, "mbta_district_shapefiles", "TestTown"), recursive = TRUE)
  dir.create(file.path(root, "mbta_district_models"), recursive = TRUE)
  zip_path <- file.path(root, "env_parcels", "1_TESTTOWN_basic.zip")
  file.create(zip_path)
  file.create(file.path(root, "mbta_district_shapefiles", "TestTown", "d.shp"))
  file.create(file.path(root, "mbta_district_models", "TestTown - CM foo.xlsx"))

  old <- Sys.getenv("MBTAZONE_PARCELS_SUBDIR", unset = NA_character_)
  Sys.setenv(MBTAZONE_PARCELS_SUBDIR = "env_parcels")
  on.exit(
    {
      if (is.na(old)) {
        Sys.unsetenv("MBTAZONE_PARCELS_SUBDIR")
      } else {
        Sys.setenv(MBTAZONE_PARCELS_SUBDIR = old)
      }
    },
    add = TRUE
  )

  paths <- get_district_paths("TestTown", "commuter_rail", data_root = root)
  expect_equal(paths$parcels, zip_path)
})
