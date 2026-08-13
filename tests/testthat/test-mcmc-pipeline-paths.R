# Every test sets or unsets all three pipeline env vars, so results do not
# depend on what the running machine happens to have in its .Renviron.

test_that("mbtazone_pipeline_paths errors when MBTAZONE_PIPELINE_DATA is unset", {
  old_p <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = NA_character_)
  old_r <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = NA_character_)
  old_d <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS", unset = NA_character_)
  Sys.unsetenv("MBTAZONE_PIPELINE_DATA")
  Sys.setenv(
    MBTAZONE_RIGHT_OF_WAY = "/tmp/row.shp",
    MBTAZONE_DENSITY_DEDUCTIONS = "/tmp/deductions.shp"
  )
  on.exit(
    {
      if (is.na(old_p)) Sys.unsetenv("MBTAZONE_PIPELINE_DATA") else Sys.setenv(MBTAZONE_PIPELINE_DATA = old_p)
      if (is.na(old_r)) Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY") else Sys.setenv(MBTAZONE_RIGHT_OF_WAY = old_r)
      if (is.na(old_d)) Sys.unsetenv("MBTAZONE_DENSITY_DEDUCTIONS") else Sys.setenv(MBTAZONE_DENSITY_DEDUCTIONS = old_d)
    },
    add = TRUE
  )
  expect_error(mbtazone_pipeline_paths(), "MBTAZONE_PIPELINE_DATA")
})

test_that("mbtazone_pipeline_paths errors when MBTAZONE_RIGHT_OF_WAY is unset", {
  old_p <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = NA_character_)
  old_r <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = NA_character_)
  old_d <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS", unset = NA_character_)
  Sys.setenv(
    MBTAZONE_PIPELINE_DATA = "/tmp/mbta_pipeline_data",
    MBTAZONE_DENSITY_DEDUCTIONS = "/tmp/deductions.shp"
  )
  Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY")
  on.exit(
    {
      if (is.na(old_p)) Sys.unsetenv("MBTAZONE_PIPELINE_DATA") else Sys.setenv(MBTAZONE_PIPELINE_DATA = old_p)
      if (is.na(old_r)) Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY") else Sys.setenv(MBTAZONE_RIGHT_OF_WAY = old_r)
      if (is.na(old_d)) Sys.unsetenv("MBTAZONE_DENSITY_DEDUCTIONS") else Sys.setenv(MBTAZONE_DENSITY_DEDUCTIONS = old_d)
    },
    add = TRUE
  )
  expect_error(mbtazone_pipeline_paths(), "MBTAZONE_RIGHT_OF_WAY")
})

test_that("mbtazone_pipeline_paths errors when MBTAZONE_DENSITY_DEDUCTIONS is unset", {
  old_p <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = NA_character_)
  old_r <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = NA_character_)
  old_d <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS", unset = NA_character_)
  Sys.setenv(
    MBTAZONE_PIPELINE_DATA = "/tmp/mbta_pipeline_data",
    MBTAZONE_RIGHT_OF_WAY = "/tmp/row.shp"
  )
  Sys.unsetenv("MBTAZONE_DENSITY_DEDUCTIONS")
  on.exit(
    {
      if (is.na(old_p)) Sys.unsetenv("MBTAZONE_PIPELINE_DATA") else Sys.setenv(MBTAZONE_PIPELINE_DATA = old_p)
      if (is.na(old_r)) Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY") else Sys.setenv(MBTAZONE_RIGHT_OF_WAY = old_r)
      if (is.na(old_d)) Sys.unsetenv("MBTAZONE_DENSITY_DEDUCTIONS") else Sys.setenv(MBTAZONE_DENSITY_DEDUCTIONS = old_d)
    },
    add = TRUE
  )
  expect_error(mbtazone_pipeline_paths(), "MBTAZONE_DENSITY_DEDUCTIONS")
})

test_that("mbtazone_pipeline_paths returns list when required env vars are set", {
  old_p <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = NA_character_)
  old_r <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = NA_character_)
  old_d <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS", unset = NA_character_)
  Sys.setenv(
    MBTAZONE_PIPELINE_DATA = "/tmp/mbta_pipeline_data",
    MBTAZONE_RIGHT_OF_WAY = "/tmp/row.shp",
    MBTAZONE_DENSITY_DEDUCTIONS = "/tmp/deductions.shp"
  )
  on.exit(
    {
      if (is.na(old_p)) Sys.unsetenv("MBTAZONE_PIPELINE_DATA") else Sys.setenv(MBTAZONE_PIPELINE_DATA = old_p)
      if (is.na(old_r)) Sys.unsetenv("MBTAZONE_RIGHT_OF_WAY") else Sys.setenv(MBTAZONE_RIGHT_OF_WAY = old_r)
      if (is.na(old_d)) Sys.unsetenv("MBTAZONE_DENSITY_DEDUCTIONS") else Sys.setenv(MBTAZONE_DENSITY_DEDUCTIONS = old_d)
    },
    add = TRUE
  )
  p <- mbtazone_pipeline_paths()
  expect_equal(p$pipeline_data_dir, "/tmp/mbta_pipeline_data")
  expect_equal(p$right_of_way, "/tmp/row.shp")
  expect_equal(p$density_deductions, "/tmp/deductions.shp")
})
