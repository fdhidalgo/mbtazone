## Screen transit-station communities for "paper compliance" candidates
##
## Computes net capacity (zoned capacity - existing housing units) for the
## adopted plan of each single-district commuter rail / rapid transit community,
## then ranks by slack ratio (adopted_net / min_units).
##
## Low slack ratio → paper compliance candidate → good for grant figure.

library(data.table)
library(arrow)
library(mbtazone)
library(cli)

# ── Paths ────────────────────────────────────────────────────────────────────
DATA_ROOT <- Sys.getenv("MBTAZONE_DATA_ROOT")
RIGHT_OF_WAY <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY")

RESIDENSITY_PATH <- file.path(
  DATA_ROOT,
  "ioc_mbta_communities_hackathon/residensity_mbta_communities.parquet"
)

# ── Load reference data ──────────────────────────────────────────────────────
community_info <- fread(
  system.file("extdata", "community_info.csv", package = "mbtazone")
)

# Filter to communities with station area requirements
transit_communities <- community_info[
  community_type %in% c("commuter_rail", "rapid_transit") &
    !is.na(min_station_area_acres)
]

# Identify single-district communities (one Excel model)
excel_dir <- file.path(DATA_ROOT, "mbta_district_models")
transit_communities[, n_excel := vapply(community_name, function(nm) {
  length(list.files(excel_dir, pattern = paste0("^", nm, " - CM.*\\.xlsx$")))
}, integer(1))]
single_district <- transit_communities[n_excel == 1]

cli_alert_info(

  "{nrow(single_district)} single-district transit communities to screen"
)

# Load residensity data (existing housing units)
resid <- read_parquet(RESIDENSITY_PATH)
setDT(resid)

# ── Screen each community ────────────────────────────────────────────────────
screen_one <- function(community_name, community_type) {
  cli_h2("Screening {community_name} ({community_type})")

  # Check data availability
  paths <- tryCatch(
    get_district_paths(
      district_name = community_name,
      district_type = community_type,
      data_root = DATA_ROOT
    ),
    error = function(e) {
      cli_alert_warning("Missing data for {community_name}: {e$message}")
      return(NULL)
    }
  )
  if (is.null(paths)) return(NULL)

  # Load district data (parcels, boundaries, capacity)
  dd <- tryCatch(
    load_district_data(
      district_name = community_name,
      district_type = community_type,
      parcels = paths$parcels,
      district = paths$district,
      excel_model = paths$excel_model,
      right_of_way = RIGHT_OF_WAY
    ),
    error = function(e) {
      cli_alert_warning("Failed to load {community_name}: {e$message}")
      return(NULL)
    }
  )
  if (is.null(dd)) return(NULL)

  dp <- dd$district_parcels

  # Merge with existing housing units
  hu_lookup <- resid[City == community_name, .(LOC_ID, baseline_hu = residentialunits)]
  hu_lookup[is.na(baseline_hu), baseline_hu := 0]

  parcel_data <- merge(
    dp[, .(LOC_ID, capacity, in_district, in_station_bounds)],
    hu_lookup,
    by = "LOC_ID", all.x = TRUE
  )
  parcel_data[is.na(baseline_hu), baseline_hu := 0]
  parcel_data[, net_per_parcel := pmax(0, capacity - baseline_hu)]

  # Adopted plan metrics
  adopted <- parcel_data[in_district == TRUE]
  adopted_gross <- sum(adopted$capacity, na.rm = TRUE)
  adopted_net <- sum(adopted$net_per_parcel, na.rm = TRUE)
  adopted_hu <- sum(adopted$baseline_hu, na.rm = TRUE)
  n_parcels <- nrow(adopted)

  # Station area metrics (within adopted district)
  station_adopted <- adopted[in_station_bounds == TRUE]
  station_gross <- sum(station_adopted$capacity, na.rm = TRUE)
  station_net <- sum(station_adopted$net_per_parcel, na.rm = TRUE)
  station_hu <- sum(station_adopted$baseline_hu, na.rm = TRUE)

  # All parcels (theoretical maximum)
  all_gross <- sum(parcel_data$capacity, na.rm = TRUE)
  all_net <- sum(parcel_data$net_per_parcel, na.rm = TRUE)

  cli_alert_success(
    "Adopted: {adopted_gross} gross / {adopted_net} net capacity ({n_parcels} parcels)"
  )

  data.table(
    community_name = community_name,
    community_type = community_type,
    adopted_gross = adopted_gross,
    adopted_net = adopted_net,
    adopted_hu = adopted_hu,
    n_parcels = n_parcels,
    station_gross = station_gross,
    station_net = station_net,
    station_hu = station_hu,
    all_parcels_gross = all_gross,
    all_parcels_net = all_net
  )
}

results_list <- lapply(seq_len(nrow(single_district)), function(i) {
  screen_one(
    single_district$community_name[i],
    single_district$community_type[i]
  )
})

results <- rbindlist(results_list[!vapply(results_list, is.null, logical(1))])

# ── Compute screening metrics ────────────────────────────────────────────────
results <- merge(
  results,
  community_info[, .(community_name, min_units, min_acres,
                      station_area_unit_pct, station_area_land_pct)],
  by = "community_name"
)

results[, `:=`(
  net_slack = adopted_net / min_units,
  gross_slack = adopted_gross / min_units,
  pct_capacity_used = adopted_gross / all_parcels_gross,
  station_pct_of_gross = station_gross / adopted_gross,
  station_pct_of_net = fifelse(adopted_net > 0, station_net / adopted_net, NA_real_)
)]

# ── Print ranked results ─────────────────────────────────────────────────────
setorder(results, net_slack)

cli_h1("Screening Results — Ranked by Net Slack (low = paper compliance)")
print(
  results[, .(
    community_name, community_type,
    min_units, adopted_net, net_slack = round(net_slack, 2),
    adopted_gross, gross_slack = round(gross_slack, 2),
    adopted_hu,
    station_pct = round(station_pct_of_gross * 100, 0),
    pct_capacity_used = round(pct_capacity_used * 100, 0)
  )],
  nrows = 50
)
