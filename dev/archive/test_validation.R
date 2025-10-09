# Test validation script on a single municipality
# Tests the systematic_validation.R functions before running full pipeline

# Load package in development mode
devtools::load_all()

# Source the validation script
source("dev/systematic_validation.R")

# Test on Maynard (known to work from HID-81)
cat("\n=== Testing Validation Functions on Maynard ===\n\n")

# Test file discovery
cat("1. Testing file discovery...\n")
excel_dt <- discover_excel_models()
shapefile_dt <- discover_district_shapefiles()

cat("   - Found", nrow(excel_dt), "Excel models\n")
cat("   - Found", nrow(shapefile_dt), "district shapefiles\n")

# Find Maynard
maynard_excel <- excel_dt[grepl("Maynard", municipality, ignore.case = TRUE), ]
cat("\n2. Maynard Excel model:\n")
print(maynard_excel)

# Find parcel shapefile
cat("\n3. Looking for Maynard parcel shapefile...\n")
maynard_parcel_shp <- find_parcel_shapefile("Maynard")
cat("   - Found:", maynard_parcel_shp, "\n")

# Find district shapefile
cat("\n4. Looking for Maynard district shapefile...\n")
maynard_district_shp <- match_shapefile("Maynard", shapefile_dt)
cat("   - Found:", maynard_district_shp, "\n")

# Test Method 1 on Maynard
if (nrow(maynard_excel) > 0 && !is.na(maynard_parcel_shp)) {
  cat("\n5. Testing Method 1 on Maynard...\n")
  result1 <- validate_method1_excel_parcels(
    municipality_name = "Maynard",
    excel_path = maynard_excel$excel_path[1],
    parcel_shapefile_path = maynard_parcel_shp
  )

  cat("\n   Method 1 Result:\n")
  print(str(result1))
}

# Test Method 2 on Maynard
if (nrow(maynard_excel) > 0 && !is.na(maynard_parcel_shp) && !is.na(maynard_district_shp)) {
  cat("\n6. Testing Method 2 on Maynard...\n")
  result2 <- validate_method2_shapefile(
    municipality_name = "Maynard",
    excel_path = maynard_excel$excel_path[1],
    parcel_shapefile_path = maynard_parcel_shp,
    district_shapefile_path = maynard_district_shp
  )

  cat("\n   Method 2 Result:\n")
  print(str(result2))
}

cat("\n=== Test Complete ===\n")