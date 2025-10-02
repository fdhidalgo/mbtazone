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

test_that("calculate_net_developable_area matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations

    r_result <- calculate_net_developable_area(
      developable_area = calc$developable_parcel_sf,
      override_developable_sf = calc$override_developable_sf
    )

    complete_data <- !is.na(calc$developable_parcel_sf)

    expect_equal(
      r_result[complete_data],
      calc$developable_sf_for_unit_calc[complete_data],
      tolerance = 1,
      info = paste(community, "District", district, "net developable area")
    )
  }
})

test_that("calculate_exclusion_ratio matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations

    r_result <- calculate_exclusion_ratio(
      excluded_area = calc$total_excluded_land,
      lot_area = calc$parcel_sf
    )

    complete_data <- !is.na(calc$parcel_sf) & !is.na(calc$total_excluded_land)

    expect_equal(
      r_result[complete_data],
      calc$excluded_land_pct[complete_data],
      tolerance = 1e-6,
      info = paste(community, "District", district, "exclusion ratio")
    )
  }
})

test_that("calculate_open_space_requirement matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_open_space_requirement(params$min_required_open_space)

    # Open space requirement is constant across all parcels
    excel_value <- unique(calc$open_space_pct[!is.na(calc$open_space_pct)])[1]

    expect_equal(
      r_result,
      excel_value,
      tolerance = 1e-6,
      info = paste(community, "District", district, "open space requirement")
    )
  }
})

test_that("calculate_required_open_space_area matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    open_space_req <- calculate_open_space_requirement(params$min_required_open_space)
    exclusion_ratio <- calculate_exclusion_ratio(calc$total_excluded_land, calc$parcel_sf)
    net_dev <- calculate_net_developable_area(calc$developable_parcel_sf, calc$override_developable_sf)

    r_result <- calculate_required_open_space_area(
      lot_area = calc$parcel_sf,
      exclusion_ratio = exclusion_ratio,
      open_space_requirement = open_space_req,
      net_developable_area = net_dev,
      override_developable_sf = calc$override_developable_sf,
      water_included = params$water_included
    )

    complete_data <- !is.na(calc$parcel_sf) & !is.na(calc$total_excluded_land)

    expect_equal(
      r_result[complete_data],
      calc$open_space_removed[complete_data],
      tolerance = 10,
      info = paste(community, "District", district, "required open space area")
    )
  }
})

test_that("calculate_parking_area matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    net_dev <- calculate_net_developable_area(calc$developable_parcel_sf, calc$override_developable_sf)

    r_result <- calculate_parking_area(
      lot_area = calc$parcel_sf,
      required_open_space = calc$open_space_removed,
      net_developable_area = net_dev,
      parking_spaces_per_unit = params$parking_spaces_per_dwelling_unit
    )

    complete_data <- !is.na(calc$parcel_sf) & !is.na(calc$open_space_removed) &
                     net_dev > 0 & !is.na(calc$parking_area_removed)

    expect_equal(
      r_result[complete_data],
      calc$parking_area_removed[complete_data],
      tolerance = 10,
      info = paste(community, "District", district, "parking area")
    )
  }
})

test_that("calculate_building_footprint matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations

    net_dev <- calculate_net_developable_area(calc$developable_parcel_sf, calc$override_developable_sf)

    r_result <- calculate_building_footprint(
      lot_area = calc$parcel_sf,
      required_open_space = calc$open_space_removed,
      parking_area = calc$parking_area_removed,
      net_developable_area = net_dev
    )

    complete_data <- !is.na(calc$parcel_sf) & !is.na(calc$open_space_removed) &
                     !is.na(calc$parking_area_removed) & net_dev > 0

    expect_equal(
      r_result[complete_data],
      calc$building_footprint[complete_data],
      tolerance = 10,
      info = paste(community, "District", district, "building footprint")
    )
  }
})

test_that("calculate_building_floor_area matches Excel across communities", {
  for (community_district in list(list("Chelsea", 1), list("Somerville", 1), list("Cambridge", 1))) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_building_floor_area(
      building_footprint = calc$building_footprint,
      building_height = params$building_height
    )

    complete_data <- !is.na(calc$building_footprint) & calc$building_footprint > 0

    expect_equal(
      r_result[complete_data],
      calc$building_envelope[complete_data],
      tolerance = 10,
      info = paste(community, "District", district, "building floor area")
    )
  }
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
