#!/usr/bin/env Rscript
# Example workflow for mbtazone package
# This script demonstrates loading data, calculating capacity, and evaluating compliance
#
# Example: Maynard (Adjacent Community)
# - 3,467 parcels (smaller dataset)
# - Real zoning parameters from submitted Excel compliance model
# - Requirements: 474 units minimum, 21 acres minimum, 15 units/acre density
#
# Note: This demo applies District 1 zoning to ALL parcels for demonstration.
# In reality, the compliance district includes only a subset of parcels.

# Load development version of package
devtools::load_all(".")
library(sf)

# ============================================================================
# Step 1: Load municipality parcel data
# ============================================================================
cat("\n=== Step 1: Loading municipality data ===\n")

parcels <- load_municipality(
  "inst/extdata/parcels/174_MAYNARD_basic.zip",
  community_name = "Maynard"
)

cat("Parcels loaded:", nrow(parcels), "parcels\n")
cat("Columns:", paste(names(parcels), collapse = ", "), "\n")
print(head(parcels, 3))

# ============================================================================
# Step 2: Extract zoning parameters from actual compliance model
# ============================================================================
cat("\n=== Step 2: Extracting zoning parameters from Excel model ===\n")

# Extract actual zoning parameters from Maynard's submitted compliance model
zoning_params <- extract_zoning_parameters(
  excel_path = "../data/mbta_district_models/Maynard/136051023_Maynard_3AComplianceModel_2024-09-12.xlsx",
  district = 1
)

cat("Zoning parameters extracted from Excel model:\n")
print(zoning_params)

# ============================================================================
# Step 3: Load calculation layers
# ============================================================================
cat("\n=== Step 3: Loading calculation layers ===\n")

# Load statewide density deduction layer
# Note: This is a large file (~87K features) and may take 5-10 seconds
cat("  Loading density deductions...")
density_deductions <- load_density_deductions(
  deduction_shapefile = "../data/calculation_shapefiles/Density_Denominator_Deductions.zip"
)
cat(" done (", nrow(density_deductions), "features)\n", sep = "")

# Load transit station areas (0.5-mile buffers)
cat("  Loading transit station areas...")
transit_stations <- load_transit_stations(
  station_shapefile = "inst/extdata/statewide/Transit_Station_Areas_Half_Mile_Radius.zip"
)
cat(" done (", nrow(transit_stations), "features)\n", sep = "")

# ============================================================================
# Step 4: Calculate district capacity
# ============================================================================
cat("\n=== Step 4: Calculating district capacity ===\n")

# Apply unit capacity calculations to all parcels
capacity_results <- calculate_district_capacity(
  parcels = parcels,
  zoning_params = zoning_params
)

cat("Capacity calculation complete\n")
cat("Total parcels processed:", nrow(capacity_results), "\n")
cat("New columns added:",
    paste(setdiff(names(capacity_results), names(parcels)), collapse = ", "), "\n")

# Show summary statistics
cat("\nCapacity summary:\n")
cat("  Total final unit capacity:", sum(capacity_results$final_unit_capacity, na.rm = TRUE), "units\n")
cat("  Parcels with capacity > 0:", sum(capacity_results$final_unit_capacity > 0, na.rm = TRUE), "\n")
cat("  Mean capacity per parcel:", mean(capacity_results$final_unit_capacity, na.rm = TRUE), "units\n")

# Show sample of results
cat("\nSample capacity results (first 5 parcels):\n")
print(capacity_results[1:5, c("LOC_ID", "SQFT", "developable_area",
                               "final_unit_capacity")], n = 5)

# ============================================================================
# Step 5: Evaluate full compliance
# ============================================================================
cat("\n=== Step 5: Evaluating compliance ===\n")

# For full compliance evaluation, we need to define districts
#
# NOTE: This example uses all parcels as a single district for demonstration.
# In reality, Maynard's compliance district includes only a subset of parcels
# as defined in their Excel model. The actual district boundary would need to be:
# 1. Loaded from a shapefile (if available), or
# 2. Defined by extracting the parcel list from the Excel District 1 sheet
#
# For this demo, we'll mark all parcels as in district 1
parcels$district_1 <- TRUE

compliance_results <- evaluate_compliance(
  municipality = parcels,
  districts = "district_1",
  zoning_params = zoning_params,
  community_type = "adjacent",  # Maynard is an adjacent community
  community_name = "Maynard",   # Load specific requirements
  transit_stations = transit_stations,  # For station area calculations
  density_deductions = density_deductions  # For accurate gross density
)

cat("Compliance evaluation complete\n")

# ============================================================================
# Step 6: Examine compliance results
# ============================================================================
cat("\n=== Step 6: Compliance results ===\n")

cat("\nOverall Compliance Status:", compliance_results$summary$compliant, "\n")

cat("\nSummary Metrics:\n")
cat("  Total units:", round(compliance_results$summary$total_units), "\n")
cat("  Total acres:", round(compliance_results$summary$total_acres, 1), "\n")
cat("  Gross density:", round(compliance_results$summary$gross_density, 2), "units/acre\n")
cat("  Number of districts:", compliance_results$summary$n_districts, "\n")
cat("  Number of parcels:", compliance_results$summary$n_parcels, "\n")

cat("\nStation Area Metrics:\n")
cat("  Station area units:", round(compliance_results$summary$station_area_units), "\n")
cat("  Station area acres:", round(compliance_results$summary$station_area_acres, 1), "\n")
cat("  Developable station acres:", round(compliance_results$summary$developable_station_acres, 1), "\n")
cat("  Parcels in station area:", sum(capacity_results$in_station_area), "\n")

cat("\nCompliance Requirements:\n")
print(compliance_results$compliance$summary)

# ============================================================================
# Additional exploration
# ============================================================================
cat("\n=== Additional outputs for exploration ===\n")

# Show distribution of unit capacity
cat("\nUnit capacity distribution:\n")
print(summary(capacity_results$final_unit_capacity))

# Show parcels with highest capacity
cat("\nTop 10 parcels by unit capacity:\n")
top_parcels <- capacity_results[order(-capacity_results$final_unit_capacity), ]
print(top_parcels[1:10, c("LOC_ID", "SQFT", "developable_area",
                          "final_unit_capacity")], n = 10)

# Show intermediate calculation results
cat("\nSample of intermediate calculations (first 5 parcels):\n")
intermediate_cols <- c("LOC_ID", "developable_area", "building_footprint_area",
                       "building_floor_area", "units_from_building_capacity",
                       "units_from_density_limit", "final_unit_capacity")
available_cols <- intersect(intermediate_cols, names(capacity_results))
print(capacity_results[1:5, available_cols], n = 5)

cat("\n=== Workflow complete ===\n")
