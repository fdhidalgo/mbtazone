#!/usr/bin/env Rscript
# Maynard Compliance Model - Using ACTUAL District from Excel
# This script replicates Maynard's submitted compliance model results

# Load development version of package
devtools::load_all(".")
library(sf)

cat("\n=== Maynard MBTA Compliance District Analysis ===\n")
cat("Comparing our calculations to submitted Excel model\n\n")

# ============================================================================
# Step 1: Load municipality parcel data
# ============================================================================
cat("Step 1: Loading Maynard parcel data...\n")

parcels <- load_municipality(
  "inst/extdata/parcels/174_MAYNARD_basic.zip",
  community_name = "Maynard"
)

cat("  Total parcels in Maynard:", nrow(parcels), "\n")

# ============================================================================
# Step 2: Define ACTUAL compliance district (4 parcels from Excel)
# ============================================================================
cat("\nStep 2: Defining actual compliance district from Excel model...\n")

# These are the ONLY 4 parcels included in Maynard's District 1
district_parcels <- c(
  "M_205397_909487",  # 111 Parker St (Office Building)
  "M_205509_909629",  # 0 Sudbury St (Parking Lot)
  "M_205379_909328",  # 0 Old Marlboro Rd (Potentially Developable)
  "M_205614_909401"   # 0 Old Marlboro Rd (Parking Lot)
)

# Create district indicator column
parcels$in_district1 <- parcels$LOC_ID %in% district_parcels

cat("  Parcels in compliance district:", sum(parcels$in_district1), "\n")
cat("  Total acres in district:", round(sum(parcels$ACRES[parcels$in_district1]), 1), "\n")

# ============================================================================
# Step 3: Extract zoning parameters from Excel model
# ============================================================================
cat("\nStep 3: Extracting zoning parameters from Excel model...\n")

zoning_params <- extract_zoning_parameters(
  excel_path = "../data/mbta_district_models/Maynard/136051023_Maynard_3AComplianceModel_2024-09-12.xlsx",
  district = 1
)

cat("  Min lot size:", zoning_params$min_lot_size, "sq ft\n")
cat("  Building height:", zoning_params$building_height, "stories\n")
cat("  Max DU/acre:", zoning_params$max_dwelling_units_per_acre, "\n")
cat("  Lot area per DU:", zoning_params$lot_area_per_dwelling_unit, "sq ft\n")

# ============================================================================
# Step 4: Load calculation layers
# ============================================================================
cat("\nStep 4: Loading calculation layers...\n")

cat("  Loading density deductions...")
density_deductions <- load_density_deductions(
  deduction_shapefile = "../data/calculation_shapefiles/Density_Denominator_Deductions.zip"
)
cat(" done\n")

cat("  Loading transit station areas...")
transit_stations <- load_transit_stations(
  station_shapefile = "inst/extdata/statewide/Transit_Station_Areas_Half_Mile_Radius.zip"
)
cat(" done\n")

# ============================================================================
# Step 5: Calculate capacity for district
# ============================================================================
cat("\nStep 5: Calculating unit capacity...\n")

capacity_results <- calculate_district_capacity(
  parcels = parcels,
  zoning_params = zoning_params,
  station_areas = transit_stations
)

# ============================================================================
# Step 6: Evaluate compliance
# ============================================================================
cat("\nStep 6: Evaluating compliance...\n")

compliance_results <- evaluate_compliance(
  municipality = parcels,
  districts = "in_district1",
  zoning_params = zoning_params,
  community_type = "adjacent",
  community_name = "Maynard",
  transit_stations = transit_stations,
  density_deductions = density_deductions,
  verbose = FALSE
)

# ============================================================================
# Step 7: Compare to Excel model
# ============================================================================
cat("\n", strrep("=", 70), "\n", sep="")
cat("RESULTS COMPARISON: Our Package vs Excel Model\n")
cat(strrep("=", 70), "\n\n", sep="")

# Excel model values (from Summary sheet)
excel_total_units <- 615
excel_total_acres <- 37.0
excel_density_denominator <- 35.2
excel_density <- 17.47

# Our calculated values
our_total_units <- round(compliance_results$summary$total_units)
our_total_acres <- round(compliance_results$summary$total_acres, 1)
our_density <- round(compliance_results$summary$gross_density, 2)

cat("Metric                    Excel Model    Our Package    Difference\n")
cat(strrep("-", 70), "\n", sep="")
cat(sprintf("Total Units               %-14d %-14d %+d\n",
            excel_total_units, our_total_units, our_total_units - excel_total_units))
cat(sprintf("Total Acres               %-14.1f %-14.1f %+.1f\n",
            excel_total_acres, our_total_acres, our_total_acres - excel_total_acres))
cat(sprintf("Gross Density (DU/acre)   %-14.2f %-14.2f %+.2f\n",
            excel_density, our_density, our_density - excel_density))

cat("\n")

# Compliance status
cat("Compliance Requirements:\n")
cat(strrep("-", 70), "\n", sep="")
cat("  Minimum Units:  474 → ", ifelse(our_total_units >= 474, "✓ PASS", "✗ FAIL"),
    " (", our_total_units, " units)\n", sep="")
cat("  Minimum Acres:   21 → ", ifelse(our_total_acres >= 21, "✓ PASS", "✗ FAIL"),
    " (", our_total_acres, " acres)\n", sep="")
cat("  Min Density:     15 → ", ifelse(our_density >= 15, "✓ PASS", "✗ FAIL"),
    " (", our_density, " DU/acre)\n", sep="")

cat("\nOverall Status: ", ifelse(compliance_results$summary$compliant, "✓ COMPLIANT", "✗ NOT COMPLIANT"), "\n", sep="")

cat("\n", strrep("=", 70), "\n\n", sep="")

# Show district parcels detail
cat("District Parcels Detail:\n")
cat(strrep("-", 70), "\n", sep="")
district_detail <- capacity_results[capacity_results$in_district1,
                                    c("LOC_ID", "Address", "SQFT", "ACRES",
                                      "developable_area", "final_unit_capacity")]
print(district_detail, n = 10)

cat("\n=== Analysis Complete ===\n")
