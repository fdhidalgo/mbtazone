# Data_load_test.R
# Diagnostic script for verifying the new gpkg-based data loading.
# Run interactively from the package root with devtools::load_all() active.
#
# Set DISTRICT to any community name present in mbta_pipeline_data/.
# Set PIPELINE_DATA and RIGHT_OF_WAY to your local paths, or let them
# fall through to the env vars used by the pipeline.

DISTRICT      <- "Norwood"
DISTRICT_TYPE <- "commuter_rail"
PIPELINE_DATA <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
RIGHT_OF_WAY  <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY")

# ============================================================================
# 1. Path resolution
# ============================================================================
paths <- get_district_paths(
  district_name     = DISTRICT,
  district_type     = DISTRICT_TYPE,
  pipeline_data_dir = PIPELINE_DATA
)
cat("gpkg:", paths$gpkg, "\n")

# ============================================================================
# 2. Raw layer inspection (before load_district_data)
# ============================================================================
library(sf)
layers <- st_layers(paths$gpkg)
cat("\nLayers in gpkg:\n")
print(layers[, c("name", "features", "fields")])

parcels_raw   <- st_read(paths$gpkg, layer = "parcels",   quiet = TRUE)
districts_raw <- st_read(paths$gpkg, layer = "districts", quiet = TRUE)

cat("\n--- parcels layer ---\n")
cat("Rows:", nrow(parcels_raw), "\n")
cat("NA loc_id:        ", sum(is.na(parcels_raw$loc_id)), "\n")
cat("in_district TRUE: ", sum(parcels_raw$in_district %in% TRUE), "\n")
cat("transit_station T:", sum(parcels_raw$transit_station %in% TRUE), "\n")
cat("NA capacity:      ", sum(is.na(parcels_raw$final_lot_multi_family_unit_capacity)), "\n")
cat("NA parcel_acres:  ", sum(is.na(parcels_raw$parcel_acres)), "\n")

cat("\n--- districts layer ---\n")
cat("Rows:", nrow(districts_raw), "\n")
print(st_drop_geometry(districts_raw)[, c(
  "district_num", "final_unit_capacity_per_district",
  "min_lot_size", "stories", "far", "open_space_pct", "parking_per_du"
)])

# ============================================================================
# 3. Full load via load_district_data
# ============================================================================
district_data <- load_district_data(
  district_name = DISTRICT,
  district_type = DISTRICT_TYPE,
  gpkg          = paths$gpkg,
  right_of_way  = RIGHT_OF_WAY
)

# ============================================================================
# 4. district_parcels checks
# ============================================================================
dp <- district_data$district_parcels
cat("\n--- district_parcels ---\n")
cat("Rows:", nrow(dp), "\n")
cat("NA LOC_ID:    ", sum(is.na(dp$LOC_ID)), "\n")
cat("NA area_acres:", sum(is.na(dp$area_acres)), "\n")
cat("NA capacity:  ", sum(is.na(dp$capacity)), "\n")
cat("in_district T:", sum(dp$in_district %in% TRUE), "\n")
cat("in_station  T:", sum(dp$in_station_bounds %in% TRUE), "\n")
cat("Total capacity (in_district):",
    sum(dp$capacity[dp$in_district %in% TRUE], na.rm = TRUE), "\n")
cat("Districts layer total capacity:",
    sum(districts_raw$final_unit_capacity_per_district, na.rm = TRUE), "\n")

# ============================================================================
# 5. district_geometry checks
# ============================================================================
dg <- district_data$district_geometry
cat("\n--- district_geometry ---\n")
cat("Rows:", nrow(dg), "\n")
cat("NA LOC_ID:   ", sum(is.na(dg$LOC_ID)), "\n")
cat("CRS epsg:    ", st_crs(dg)$epsg, "\n")
cat("Geometry type:", unique(as.character(st_geometry_type(dg))), "\n")
cat("NA centroid_x:", sum(is.na(dg$centroid_x)), "\n")

# ============================================================================
# 6. ROW and district boundary
# ============================================================================
cat("\n--- district_right_of_way ---\n")
cat("Rows:", nrow(district_data$district_right_of_way), "\n")

cat("\n--- district_boundary ---\n")
cat("Geometry type:", as.character(st_geometry_type(district_data$district_boundary)), "\n")
cat("CRS epsg:     ", st_crs(district_data$district_boundary)$epsg, "\n")

cat("\nAll checks complete.\n")
