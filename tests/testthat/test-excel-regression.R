# Excel Regression Tests
#
# Tests that validate R package calculations against Excel compliance model outputs
#
# Note: The R package preserves NA values for missing input data (following the
# "NA preservation philosophy" in CLAUDE.md), while the Excel model converts
# missing inputs to 0. These tests filter out rows with missing input data to
# focus on validating the calculation logic for complete data.

# Helper function to convert Excel text strings to numeric NA
# Excel uses "N/A" and "<no limit>" to represent unlimited/no constraint conditions
# R functions return NA_real_ for the same conditions
convert_excel_to_numeric <- function(x) {
  if (is.character(x)) {
    # Convert special Excel strings to NA
    x[x %in% c("N/A", "<no limit>")] <- NA
    # Convert remaining strings to numeric
    x <- suppressWarnings(as.numeric(x))
  }
  return(x)
}

# Test communities and districts for regression testing
# To add new communities, simply add them to this list
TEST_COMMUNITIES <- list(
  list("Chelsea", 1),
  list("Somerville", 1),
  list("Cambridge", 1),
  list("Wellesley", 1),
  list("Newton", 1),
  list("Lincoln", 1),
  list("Maynard", 1)
)

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
    TEST_COMMUNITIES,
    ~ test_developable_area_regression(.x[[1]], .x[[2]])
  )
})

test_that("calculate_net_developable_area matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
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
  for (community_district in TEST_COMMUNITIES) {
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
  for (community_district in TEST_COMMUNITIES) {
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
  for (community_district in TEST_COMMUNITIES) {
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
  for (community_district in TEST_COMMUNITIES) {
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
  for (community_district in TEST_COMMUNITIES) {
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
  for (community_district in TEST_COMMUNITIES) {
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

test_that("calculate_units_from_building_capacity matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations

    r_result <- calculate_units_from_building_capacity(calc$building_envelope)

    complete_data <- !is.na(calc$building_envelope) & calc$building_envelope > 0

    expect_equal(
      r_result[complete_data],
      calc$modeled_unit_capacity[complete_data],
      tolerance = 1,
      info = paste(community, "District", district, "units from building capacity")
    )
  }
})

test_that("calculate_units_from_density_limits matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_units_from_density_limits(
      lot_area = calc$parcel_sf,
      max_dwelling_units_per_acre = params$max_dwelling_units_per_acre
    )

    # Convert Excel text strings to numeric
    excel_density_limit <- convert_excel_to_numeric(calc$dwelling_units_per_acre_limit)

    # Only compare parcels where both have numeric values
    complete_data <- !is.na(calc$parcel_sf) & !is.na(excel_density_limit)

    expect_equal(
      r_result[complete_data],
      excel_density_limit[complete_data],
      tolerance = 0.01,
      info = paste(community, "District", district, "units from density limits")
    )
  }
})

test_that("calculate_units_from_lot_coverage matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_units_from_lot_coverage(
      lot_area = calc$parcel_sf,
      max_lot_coverage = params$max_lot_coverage,
      building_height = params$building_height
    )

    # Convert Excel text strings to numeric
    excel_lot_coverage <- convert_excel_to_numeric(calc$max_lot_coverage_limit)

    # Only compare parcels where both have numeric values
    complete_data <- !is.na(calc$parcel_sf) & !is.na(excel_lot_coverage)

    expect_equal(
      r_result[complete_data],
      excel_lot_coverage[complete_data],
      tolerance = 0.01,
      info = paste(community, "District", district, "units from lot coverage")
    )
  }
})

test_that("calculate_units_from_lot_area_requirement matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_units_from_lot_area_requirement(
      lot_area = calc$parcel_sf,
      lot_area_per_dwelling_unit = params$lot_area_per_dwelling_unit
    )

    # Convert Excel text strings to numeric
    excel_lot_area_req <- convert_excel_to_numeric(calc$lot_area_per_dwelling_unit_limit)

    # Only compare parcels where both have numeric values
    complete_data <- !is.na(calc$parcel_sf) & !is.na(excel_lot_area_req)

    expect_equal(
      r_result[complete_data],
      excel_lot_area_req[complete_data],
      tolerance = 0.01,
      info = paste(community, "District", district, "units from lot area requirement")
    )
  }
})

test_that("calculate_units_from_far_limits matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_units_from_far_limits(
      lot_area = calc$parcel_sf,
      FAR = params$FAR
    )

    # Convert Excel text strings to numeric
    excel_far_limit <- convert_excel_to_numeric(calc$far_limit)

    # Only compare parcels where both have numeric values
    complete_data <- !is.na(calc$parcel_sf) & !is.na(excel_far_limit)

    expect_equal(
      r_result[complete_data],
      excel_far_limit[complete_data],
      tolerance = 0.01,
      info = paste(community, "District", district, "units from FAR limits")
    )
  }
})

test_that("calculate_units_with_max_cap matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_units_with_max_cap(
      units_building_capacity = calc$modeled_unit_capacity,
      max_units_per_lot = params$max_units_per_lot
    )

    # Only compare parcels where Excel has a value
    complete_data <- !is.na(calc$modeled_unit_capacity) & !is.na(calc$max_units_per_lot_limit)

    expect_equal(
      r_result[complete_data],
      calc$max_units_per_lot_limit[complete_data],
      tolerance = 1,
      info = paste(community, "District", district, "units with max cap")
    )
  }
})

test_that("calculate_below_minimum_lot_flag matches Excel across communities", {
  skip("Excel models do not populate the Non-Conforming Lot column - all values are NA")

  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    r_result <- calculate_below_minimum_lot_flag(
      lot_area = calc$parcel_sf,
      min_lot_size = params$min_lot_size
    )

    complete_data <- !is.na(calc$parcel_sf)

    expect_equal(
      r_result[complete_data],
      calc$non_conforming_lot[complete_data],
      info = paste(community, "District", district, "below minimum lot flag")
    )
  }
})

test_that("calculate_units_from_graduated_lots matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations
    params <- ref_data$zoning_parameters

    below_min_flag <- calculate_below_minimum_lot_flag(calc$parcel_sf, params$min_lot_size)

    r_result <- calculate_units_from_graduated_lots(
      lot_area = calc$parcel_sf,
      below_minimum_lot_flag = below_min_flag,
      base_min_lot_size = params$base_min_lot_size,
      additional_lot_SF = params$additional_lot_SF
    )

    # Convert Excel text strings to numeric
    excel_graduated_lots <- convert_excel_to_numeric(calc$max_units_graduated_lots)

    # Only compare parcels where both have numeric values
    complete_data <- !is.na(calc$parcel_sf) & !is.na(excel_graduated_lots)

    expect_equal(
      r_result[complete_data],
      excel_graduated_lots[complete_data],
      tolerance = 1,
      info = paste(community, "District", district, "units from graduated lots")
    )
  }
})

test_that("calculate_units_per_acre matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations

    r_result <- calculate_units_per_acre(
      lot_area = calc$parcel_sf,
      final_unit_capacity = calc$final_unit_capacity
    )

    complete_data <- !is.na(calc$parcel_sf) & calc$parcel_sf > 0 &
                     !is.na(calc$final_unit_capacity) & !is.na(calc$du_per_ac)

    expect_equal(
      r_result[complete_data],
      calc$du_per_ac[complete_data],
      tolerance = 0.01,
      info = paste(community, "District", district, "units per acre")
    )
  }
})

# NOTE: Final unit capacity tests now ready - all 7 unit calculation methods implemented!

test_that("calculate_final_unit_capacity matches Excel across communities", {
  for (community_district in TEST_COMMUNITIES) {
    community <- community_district[[1]]
    district <- community_district[[2]]

    ref_data <- readRDS(test_path("fixtures", paste0(tolower(community), "_district", district, ".rds")))
    calc <- ref_data$calculations

    # Convert Excel text strings to numeric for all unit calculation columns
    # (some fixtures have character columns, others have numeric)
    units_building <- convert_excel_to_numeric(calc$modeled_unit_capacity)
    units_density <- convert_excel_to_numeric(calc$dwelling_units_per_acre_limit)
    units_coverage <- convert_excel_to_numeric(calc$max_lot_coverage_limit)
    units_lot_req <- convert_excel_to_numeric(calc$lot_area_per_dwelling_unit_limit)
    units_far <- convert_excel_to_numeric(calc$far_limit)
    units_max_cap <- convert_excel_to_numeric(calc$max_units_per_lot_limit)
    units_graduated <- convert_excel_to_numeric(calc$max_units_graduated_lots)

    # Call calculate_final_unit_capacity with all 7 methods (all converted to numeric)
    r_result <- calculate_final_unit_capacity(
      units_building_capacity = units_building,
      units_density_limits = units_density,
      units_lot_coverage = units_coverage,
      units_lot_area_req = units_lot_req,
      units_far_limits = units_far,
      units_max_cap = units_max_cap,
      units_graduated_lots = units_graduated
    )

    # Filter to parcels with at least one non-NA method (using converted values)
    has_any_method <- !is.na(units_building) |
                      !is.na(units_density) |
                      !is.na(units_coverage) |
                      !is.na(units_lot_req) |
                      !is.na(units_far) |
                      !is.na(units_max_cap) |
                      !is.na(units_graduated)

    complete_data <- has_any_method & !is.na(calc$final_unit_capacity)

    expect_equal(
      r_result[complete_data],
      calc$final_unit_capacity[complete_data],
      tolerance = 1,
      info = paste(community, "District", district, "final unit capacity")
    )
  }
})

# NOTE: Previous commented-out test removed - replaced with working test above

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
