# Test complete validation on Maynard
# This tests the full Method 1 workflow

devtools::load_all()
source("dev/systematic_validation.R")

cat("\n=== Complete Maynard Validation Test ===\n\n")

# Setup paths
maynard_excel <- "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_models/Maynard/136051023_Maynard_3AComplianceModel_2024-09-12.xlsx"
maynard_parcel_shp <- find_parcel_shapefile("Maynard")
maynard_district_shp <- match_shapefile("Maynard", discover_district_shapefiles())

cat("Paths:\n")
cat("  Excel:", maynard_excel, "\n")
cat("  Parcel shapefile:", maynard_parcel_shp, "\n")
cat("  District shapefile:", maynard_district_shp, "\n\n")

# Test Method 1
cat("=== METHOD 1: Excel Parcel List Validation ===\n\n")
result1 <- validate_method1_excel_parcels(
  municipality_name = "Maynard",
  excel_path = maynard_excel,
  parcel_shapefile_path = maynard_parcel_shp
)

cat("\nResult Summary:\n")
cat("  Success:", result1$success, "\n")
if (!result1$success) {
  cat("  Error:", result1$error, "\n")
} else {
  cat("  Excel Units:", result1$excel_units, "\n")
  cat("  R Units:", result1$r_units, "\n")
  cat("  Difference:", result1$unit_diff, "units (", round(result1$unit_pct_diff, 2), "%)\n")
  cat("  Excel Acres:", result1$excel_acres, "\n")
  cat("  R Acres:", result1$r_acres, "\n")
}

# Test Method 2 if shapefile available
if (!is.na(maynard_district_shp)) {
  cat("\n\n=== METHOD 2: Shapefile Boundary Validation ===\n\n")
  result2 <- validate_method2_shapefile(
    municipality_name = "Maynard",
    excel_path = maynard_excel,
    parcel_shapefile_path = maynard_parcel_shp,
    district_shapefile_path = maynard_district_shp
  )

  cat("\nResult Summary:\n")
  cat("  Success:", result2$success, "\n")
  if (!result2$success) {
    cat("  Error:", result2$error, "\n")
  } else {
    cat("  Excel Units:", result2$excel_units, "\n")
    cat("  R Units:", result2$r_units, "\n")
    cat("  Difference:", result2$unit_diff, "units (", round(result2$unit_pct_diff, 2), "%)\n")
    cat("  Parcel Match Rate:", round(result2$parcel_match_rate * 100, 1), "%\n")
  }
}

cat("\n=== Test Complete ===\n")
