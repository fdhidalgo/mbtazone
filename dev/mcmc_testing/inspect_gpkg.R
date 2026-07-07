# inspect_gpkg.R
#
# Detailed inspection of a single community GeoPackage.
# Set COMMUNITY below and run from the package root.
#
# Requires MBTAZONE_PIPELINE_DATA env var.

library(sf)
library(data.table)

COMMUNITY <- "Norwell"   # <-- change this

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")

gpkg <- file.path(pipeline_data_dir, paste0(gsub(" ", "_", COMMUNITY), ".gpkg"))
if (!file.exists(gpkg)) stop("No GeoPackage found at: ", gpkg)

cat("=== GeoPackage:", gpkg, "===\n\n")

# ---- Layers -----------------------------------------------------------------
cat("--- Layers ---\n")
print(sf::st_layers(gpkg))

# ---- Load both layers -------------------------------------------------------
parcels   <- sf::st_read(gpkg, layer = "parcels",   quiet = TRUE)
districts <- sf::st_read(gpkg, layer = "districts", quiet = TRUE)

# ---- Districts layer --------------------------------------------------------
cat("\n--- Districts layer (", nrow(districts), "row(s)) ---\n")
cat("Columns:", paste(names(districts), collapse = ", "), "\n\n")
cat("Key fields:\n")
print(sf::st_drop_geometry(districts)[, c(
  "building_height", "FAR", "max_lot_coverage",
  "parking_spaces_per_dwelling_unit", "lot_area_per_dwelling_unit",
  "min_lot_size"
), drop = FALSE])
cat("\nDistrict polygon area (acres):", as.numeric(sf::st_area(sf::st_union(districts))) / 4047, "\n")

# ---- Parcels layer overview -------------------------------------------------
in_d <- parcels[parcels$in_district == TRUE, ]
cat("\n--- Parcels layer ---\n")
cat("Total parcels:      ", nrow(parcels), "\n")
cat("In-district parcels:", nrow(in_d), "\n")
cat("Parcel columns:", paste(names(parcels), collapse = ", "), "\n")

# ---- Per-parcel table (in-district only) ------------------------------------
cat("\n--- In-district parcel detail ---\n")
dt <- as.data.table(sf::st_drop_geometry(in_d))[, .(
  LOC_ID,
  ACRES,
  SQFT,
  Tot_Exclud,
  Hydrology,
  Wetlands,
  TitleV,
  SurfWatBC,
  Wellhead1,
  dd_sf        = Hydrology + Wetlands + TitleV + SurfWatBC + Wellhead1,
  capacity     = final_lot_multi_family_unit_capacity
)]
print(dt, nrow = 200)

# ---- Consistency checks on exclusion fields ---------------------------------
cat("\n--- Exclusion field consistency checks ---\n")
cat("sum ACRES (acres):        ", round(sum(dt$ACRES, na.rm=TRUE), 4), "\n")
cat("sum SQFT (sq ft):         ", round(sum(dt$SQFT,  na.rm=TRUE), 0), "\n")
cat("sum Tot_Exclud (sq ft):   ", round(sum(dt$Tot_Exclud, na.rm=TRUE), 0), "\n")
cat("sum Tot_Exclud (acres):   ", round(sum(dt$Tot_Exclud, na.rm=TRUE) / 43560, 4), "\n")
cat("sum dd_sf (sq ft):        ", round(sum(dt$dd_sf, na.rm=TRUE), 0), "\n")
cat("sum dd_sf (acres):        ", round(sum(dt$dd_sf, na.rm=TRUE) / 43560, 4), "\n")
cat("\nParcels where dd_sf > SQFT (impossible):", sum(dt$dd_sf > dt$SQFT, na.rm=TRUE), "\n")
cat("Parcels where dd_sf > Tot_Exclud:        ", sum(dt$dd_sf > dt$Tot_Exclud, na.rm=TRUE), "\n")

# Show offending parcels if any
bad <- dt[dd_sf > SQFT]
if (nrow(bad) > 0) {
  cat("\nParcels where dd_sf > SQFT:\n")
  print(bad)
}

# ---- Density denominator summary --------------------------------------------
poly_acres    <- as.numeric(sf::st_area(sf::st_union(districts))) / 4047
raw_acres     <- sum(dt$ACRES, na.rm=TRUE)
tot_excl_ac   <- sum(dt$Tot_Exclud, na.rm=TRUE) / 43560
dd_acres      <- sum(dt$dd_sf, na.rm=TRUE) / 43560
correct_denom <- poly_acres - dd_acres
capacity      <- sum(dt$capacity, na.rm=TRUE)

cat("\n--- Density denominator summary ---\n")
cat("District polygon area (acres): ", round(poly_acres,    4), "\n")
cat("Sum parcel ACRES:              ", round(raw_acres,     4), "\n")
cat("Sum Tot_Exclud (acres):        ", round(tot_excl_ac,   4), "\n")
cat("Sum dd fields (acres):         ", round(dd_acres,      4), "\n")
cat("Correct denom (poly - dd):     ", round(correct_denom, 4), "\n")
cat("Total capacity:                ", capacity, "\n")
cat("\nDensity (raw_acres):           ", round(capacity / raw_acres,     2), "\n")
cat("Density (correct_denom):       ", round(capacity / correct_denom, 2), "\n")
if (!is.na(suppressWarnings(as.numeric(districts$Summary__2[1])))) {
  cat("Atlas density (Summary__2):    ", round(as.numeric(districts$Summary__2[1]), 2), "\n")
}

# ---- Atlas summary fields ---------------------------------------------------
atlas_cols <- grep("^Summary__", names(districts), value = TRUE)
if (length(atlas_cols) > 0) {
  cat("\n--- Atlas summary fields (Summary__*) ---\n")
  print(t(sf::st_drop_geometry(districts)[1, atlas_cols, drop = FALSE]))
}
