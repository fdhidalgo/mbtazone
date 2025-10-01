# Excel Regression Tests
#
# Tests that validate R package calculations against Excel compliance model outputs
#
# Note: The R package preserves NA values for missing input data (following the
# "NA preservation philosophy" in CLAUDE.md), while the Excel model converts
# missing inputs to 0. These tests filter out rows with missing input data to
# focus on validating the calculation logic for complete data.

# Helper function for developable area regression testing
test_developable_area_regression <- function(community, district) {
  ref_data <- readRDS(test_path(
    "fixtures",
    paste0(tolower(community), "_district", district, ".rds")
  ))

  calc <- ref_data$calculations

  r_result <- calculate_developable_area(
    lot_area = calc$parcel_sf,
    excluded_area = calc$total_excluded_land,
    min_lot_size = ref_data$zoning_parameters$min_lot_size
  )

  # Filter to parcels with complete input data for comparison
  # (Excel treats NA as 0, R preserves NA - both are valid design choices)
  complete_data <- !is.na(calc$parcel_sf) & !is.na(calc$total_excluded_land)

  expect_equal(
    r_result[complete_data],
    calc$developable_parcel_sf[complete_data],
    tolerance = 1,
    info = paste(community, "District", district, "developable area")
  )
}

test_that("calculate_developable_area matches Excel across communities", {
  purrr::walk(
    list(
      list("Chelsea", 1),
      list("Somerville", 1),
      list("Cambridge", 1)
    ),
    ~ test_developable_area_regression(.x[[1]], .x[[2]])
  )
})

# NOTE: Final unit capacity tests require all 7 unit calculation methods to be
# implemented. These tests are commented out until those functions are available.

# test_that("Chelsea District 1: calculate_final_unit_capacity matches Excel", {
#   ref_data <- readRDS(test_path("fixtures/chelsea_district1.rds"))
#   calc <- ref_data$calculations
#
#   # These inputs will come from other calculation functions once implemented
#   r_result <- calculate_final_unit_capacity(
#     units_building_capacity = calc$modeled_unit_capacity,  # Column X
#     units_density_limits = calc$dwelling_units_per_acre_limit,  # Column Y
#     units_lot_coverage = # Column Z - not yet implemented
#     units_lot_area_req = calc$lot_area_per_dwelling_unit_limit,  # Column AA
#     units_far_limits = calc$far_limit,  # Column AB
#     units_max_cap = calc$max_units_per_lot_limit,  # Column AC
#     units_graduated_lots = calc$max_units_graduated_lots  # Column AE
#   )
#
#   expect_equal(
#     r_result,
#     calc$final_unit_capacity,
#     tolerance = 0.01,
#     info = "Chelsea District 1 final unit capacity calculation"
#   )
# })
