# density_denominator_comparison.R
#
# Compares three approaches to computing the density denominator for each
# adopted district, and benchmarks each against the MA Zoning Atlas values.
#
# The three approaches are:
#   1. raw_acres       - sum of parcel ACRES (current MCMC approach)
#   2. adj_tot_excl    - sum(ACRES) - sum(Tot_Exclud)/43560
#   3. correct_denom   - district polygon area - sum(density deduction fields)/43560
#                        matching the official compliance model methodology
#
# The density deduction fields per state documentation are:
#   Hydrology + Wetlands + TitleV + SurfWatBC + Wellhead1
#
# Run from the mbtazone package root. Requires MBTAZONE_PIPELINE_DATA env var.

library(sf)
library(data.table)

pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
if (nchar(pipeline_data_dir) == 0) {
  stop("MBTAZONE_PIPELINE_DATA env var not set. Set it to the directory containing .gpkg files.")
}

single_zone_csv <- "inst/extdata/single_zone_communities.csv"
if (!file.exists(single_zone_csv)) {
  stop(
    "Single-zone community list not found at ", single_zone_csv, ".\n",
    "Generate it first by running: Rscript inst/targets/check_single_zone_districts.R"
  )
}
single_zone <- data.table::fread(single_zone_csv)
cat("Loaded", nrow(single_zone), "single-zone communities from", single_zone_csv, "\n\n")

all_gpkg_files <- list.files(pipeline_data_dir, pattern = "\\.gpkg$", full.names = TRUE)
gpkg_files <- all_gpkg_files[
  tools::file_path_sans_ext(basename(all_gpkg_files)) %in%
    gsub(" ", "_", single_zone$community_name)
]
cat("Matched", length(gpkg_files), "GeoPackage files\n\n")

results <- lapply(gpkg_files, function(gpkg) {
  community <- tools::file_path_sans_ext(basename(gpkg))
  tryCatch({
    parcels   <- sf::st_read(gpkg, layer = "parcels",   quiet = TRUE)
    districts <- sf::st_read(gpkg, layer = "districts", quiet = TRUE)
    in_d      <- parcels[parcels$in_district == TRUE, ]

    if (nrow(in_d) == 0) return(NULL)

    # ---- Three area measures ------------------------------------------------

    # 1. Raw parcel ACRES sum (current MCMC approach)
    raw_acres <- sum(in_d$ACRES, na.rm = TRUE)

    # 2. Parcel sum minus all Tot_Exclud
    adj_tot_excl <- raw_acres - sum(in_d$Tot_Exclud, na.rm = TRUE) / 43560

    # 3. District polygon area minus density-denominator-specific deductions
    #    (Hydrology, Wetlands, TitleV, SurfWatBC, Wellhead1 per state documentation)
    polygon_acres <- as.numeric(sf::st_area(sf::st_union(districts))) / 4047
    dd_sf <- rowSums(sf::st_drop_geometry(in_d)[,
      c("Hydrology", "Wetlands", "TitleV", "SurfWatBC", "Wellhead1"),
      drop = FALSE], na.rm = TRUE)
    dd_acres      <- sum(dd_sf) / 43560
    correct_denom <- polygon_acres - dd_acres

    # ---- Capacity and atlas reference values --------------------------------
    capacity      <- sum(in_d$final_lot_multi_family_unit_capacity, na.rm = TRUE)
    atlas_capacity <- suppressWarnings(as.numeric(districts$Summary__F[1]))
    atlas_denom    <- suppressWarnings(as.numeric(districts$Summary__1[1]))
    atlas_density  <- suppressWarnings(as.numeric(districts$Summary__2[1]))

    # ---- Densities for each approach ----------------------------------------
    density_raw     <- if (raw_acres     > 0) capacity / raw_acres     else NA_real_
    density_tot_excl <- if (adj_tot_excl > 0) capacity / adj_tot_excl  else NA_real_
    density_correct  <- if (correct_denom > 0) capacity / correct_denom else NA_real_

    data.frame(
      community       = community,
      n_parcels       = nrow(in_d),
      capacity        = capacity,
      atlas_capacity  = atlas_capacity,

      # Area measures
      raw_acres       = round(raw_acres,       3),
      adj_tot_excl    = round(adj_tot_excl,    3),
      polygon_acres   = round(polygon_acres,   3),
      dd_acres        = round(dd_acres,        3),
      correct_denom   = round(correct_denom,   3),
      atlas_denom     = round(atlas_denom,     3),

      # Densities
      density_raw     = round(density_raw,     2),
      density_tot_excl= round(density_tot_excl,2),
      density_correct = round(density_correct, 2),
      atlas_density   = round(atlas_density,   2),

      # Gaps vs correct_denom
      raw_vs_correct  = round(raw_acres    - correct_denom, 3),
      excl_vs_correct = round(adj_tot_excl - correct_denom, 3),
      correct_vs_atlas= round(correct_denom - atlas_denom,  3),

      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    message("  Skipping ", community, ": ", conditionMessage(e))
    NULL
  })
})

dt <- data.table::rbindlist(Filter(Negate(is.null), results))
data.table::setorder(dt, community)

cat("==========================================================================\n")
cat("DENSITY DENOMINATOR COMPONENTS —", nrow(dt), "single-zone communities\n")
cat("==========================================================================\n")
cat("Columns:\n")
cat("  raw_acres    : sum of in-district parcel ACRES (current MCMC denominator)\n")
cat("  tot_excl_ac  : sum of Tot_Exclud / 43560 (all exclusion types)\n")
cat("  poly_acres   : district polygon area from geometry (st_area / 4047)\n")
cat("  dd_acres     : sum of density-deductible fields / 43560\n")
cat("                 (Hydrology + Wetlands + TitleV + SurfWatBC + Wellhead1)\n")
cat("  denom_raw    : raw_acres                (current MCMC)\n")
cat("  denom_excl   : raw_acres - tot_excl_ac  (parcel sum minus all exclusions)\n")
cat("  denom_correct: poly_acres - dd_acres    (official methodology)\n")
cat("  denom_atlas  : Summary__1 from GeoPackage (atlas reference)\n")
cat("  flag         : * if denom_correct < 0 (data issue)\n\n")

out <- dt[, .(
  community,
  capacity,
  raw_acres,
  tot_excl_ac  = round(raw_acres - adj_tot_excl, 3),
  poly_acres   = round(polygon_acres, 3),
  dd_acres     = round(dd_acres, 3),
  denom_raw    = round(raw_acres, 3),
  denom_excl   = round(adj_tot_excl, 3),
  denom_correct= round(correct_denom, 3),
  denom_atlas  = round(atlas_denom, 3),
  flag         = ifelse(correct_denom < 0, "*", "")
)]

print(out, nrow = 200)
