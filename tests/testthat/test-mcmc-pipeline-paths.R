test_that("mbtazone_pipeline_paths errors when MBTAZONE_DATA_ROOT is unset", {
  old_d <- Sys.getenv("MBTAZONE_DATA_ROOT", unset = NA_character_)
  old_r <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = NA_character_)
  Sys.unsetenv("MBTAZONE_DATA_ROOT")
  Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY")
  on.exit(
    {
      if (is.na(old_d)) Sys.unsetenv("MBTAZONE_DATA_ROOT") else Sys.setenv(MBTAZONE_DATA_ROOT = old_d)
      if (is.na(old_r)) Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY") else Sys.setenv(MBTAZONE_RIGHT_OF_WAY = old_r)
    },
    add = TRUE
  )
  expect_error(mbtazone_pipeline_paths(), "MBTAZONE_DATA_ROOT")
})

test_that("mbtazone_pipeline_paths returns list when required env vars are set", {
  old_d <- Sys.getenv("MBTAZONE_DATA_ROOT", unset = NA_character_)
  old_r <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = NA_character_)
  old_p <- Sys.getenv("MBTAZONE_PARCELS_SUBDIR", unset = NA_character_)
  Sys.setenv(
    MBTAZONE_DATA_ROOT = "/tmp/mbta_test_data",
    MBTAZONE_RIGHT_OF_WAY = "/tmp/row.shp"
  )
  Sys.unsetenv("MBTAZONE_PARCELS_SUBDIR")
  on.exit(
    {
      if (is.na(old_d)) Sys.unsetenv("MBTAZONE_DATA_ROOT") else Sys.setenv(MBTAZONE_DATA_ROOT = old_d)
      if (is.na(old_r)) Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY") else Sys.setenv(MBTAZONE_RIGHT_OF_WAY = old_r)
      if (is.na(old_p)) Sys.unsetenv("MBTAZONE_PARCELS_SUBDIR") else Sys.setenv(MBTAZONE_PARCELS_SUBDIR = old_p)
    },
    add = TRUE
  )
  p <- mbtazone_pipeline_paths()
  expect_equal(p$data_root, "/tmp/mbta_test_data")
  expect_equal(p$right_of_way, "/tmp/row.shp")
  expect_equal(p$parcels_subdir, "land_record_shapefiles/basic")
})
