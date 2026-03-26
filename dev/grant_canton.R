## Grant figure: Canton (commuter rail with transit station)
##
## Canton is a commuter rail community with gross_slack=0.97 and
## net_slack=0.92 — barely compliant even on gross capacity. 100% of
## adopted capacity is in the station area. Illustrates "paper compliance"
## where the entire district sits within the station buffer.

library(targets)
library(sf)
library(scales)
library(arrow)
library(data.table)
library(mbtazone)
library(purrr)
library(ggplot2)
library(patchwork)

# ── Configuration ────────────────────────────────────────────────────────────
COMMUNITY <- "Canton"
COMMUNITY_TYPE <- "commuter_rail"
STORE <- "ext/_targets_Canton"

RESIDENSITY_PATH <- file.path(
  "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data",
  "ioc_mbta_communities_hackathon/residensity_mbta_communities.parquet"
)

# ── Summarize a set of plan LOC_IDs against the parcel lookup ────────────────
summarize_plan <- function(plan_locs, parcel_lookup) {
  matched <- parcel_lookup[.(plan_locs), nomatch = NULL]
  list(
    total_capacity = sum(matched$capacity, na.rm = TRUE),
    net_capacity = sum(matched$net_per_parcel, na.rm = TRUE),
    baseline_hu = sum(matched$baseline_hu, na.rm = TRUE),
    n_parcels = nrow(matched),
    n_with_hu = sum(matched$baseline_hu > 0)
  )
}

# ── Load MCMC results ────────────────────────────────────────────────────────
district_data <- targets::tar_read("district_data", store = STORE)
parcel_graph_result <- targets::tar_read("parcel_graph_result", store = STORE)
chain_results <- targets::tar_read("all_parcel_chain_results", store = STORE)

dp <- district_data$district_parcels
pa <- parcel_graph_result$parcel_assignments
data.table::setDT(pa)
data.table::setkey(pa, "unit_id")

# ── Build parcel lookup with existing housing units ──────────────────────────
cap_lookup <- dp[, .(LOC_ID, capacity, in_station_bounds)]

resid <- arrow::read_parquet(RESIDENSITY_PATH)
data.table::setDT(resid)
hu_lookup <- resid[City == COMMUNITY, .(LOC_ID, baseline_hu = residentialunits)]
hu_lookup[is.na(baseline_hu), baseline_hu := 0]

parcel_lookup <- merge(cap_lookup, hu_lookup, by = "LOC_ID", all.x = TRUE)
parcel_lookup[is.na(baseline_hu), baseline_hu := 0]
parcel_lookup[, net_per_parcel := pmax(0, capacity - baseline_hu)]
data.table::setkey(parcel_lookup, "LOC_ID")

# ── Adopted plan metrics ─────────────────────────────────────────────────────
adopted_locs <- dp[in_district == TRUE, LOC_ID]
adopted_metrics <- summarize_plan(adopted_locs, parcel_lookup)

# ── MCMC sample metrics ─────────────────────────────────────────────────────
all_chains <- purrr::list_flatten(purrr::map(chain_results, "parcel_samples"))

calc_sample_metrics <- function(sample) {
  parcel_ids_in_sample <- pa[unit_id %in% sample$X, unique(parcel_id)]
  sample_locs <- dp[LOC_ID %in% parcel_ids_in_sample]
  summarize_plan(sample_locs$LOC_ID, parcel_lookup)
}

all_sample_metrics <- purrr::map(all_chains, calc_sample_metrics) |>
  data.table::rbindlist()

adopted_percentile <- mean(
  adopted_metrics$net_capacity > all_sample_metrics$net_capacity
)

cat(sprintf(
  "\n%s: adopted net capacity = %s (%.0f%% percentile of %d samples)\n",
  COMMUNITY,
  format(adopted_metrics$net_capacity, big.mark = ","),
  adopted_percentile * 100,
  nrow(all_sample_metrics)
))

# ── Three-Panel Grant Figure ─────────────────────────────────────────────────

# 1. Prepare spatial base layers
dg <- district_data$district_geometry
parcels_sf <- merge(
  dg, dp[, .(LOC_ID, in_district)],
  by = "LOC_ID", all.x = TRUE
)
parcels_sf$in_district[is.na(parcels_sf$in_district)] <- FALSE

muni_boundary <- sf::st_union(parcels_sf) |>
  sf::st_buffer(300) |>
  sf::st_buffer(-300)

# Load transit station areas and clip to municipality
transit_stations <- load_transit_stations()
station_clipped <- sf::st_intersection(transit_stations, muni_boundary)

# 2. Helper: get LOC_IDs for a single MCMC sample
get_sample_locs <- function(sample) {
  pa[unit_id %in% sample$X, unique(parcel_id)]
}

# 3. Map-making function with station area overlay
make_district_map <- function(parcels_sf, muni_boundary, station_clipped,
                              in_locs, subtitle = "") {
  # Exclude zero-capacity parcels (ROW/road slivers) from map rendering
  map_parcels <- parcels_sf[parcels_sf$LOC_ID %in% in_locs &
                              parcels_sf$capacity > 0, ]
  district_sf <- sf::st_union(map_parcels)

  ggplot() +
    # Parcel outlines create implicit street network
    geom_sf(data = parcels_sf, fill = "grey92", color = "white",
            linewidth = 0.05) +
    # Municipality border
    geom_sf(data = muni_boundary, fill = NA, color = "grey30",
            linewidth = 0.4) +
    # Station area buffer
    geom_sf(data = station_clipped, fill = "#FDAE61", color = "#E08214",
            alpha = 0.35, linewidth = 0.3) +
    # District parcels
    geom_sf(data = district_sf, fill = "#2166AC", color = "#053061",
            linewidth = 0.2) +
    labs(subtitle = subtitle) +
    theme_void() +
    theme(
      plot.margin = margin(2, 2, 2, 2),
      plot.subtitle = element_text(size = 8, color = "grey30", hjust = 0,
                                   margin = margin(t = 2))
    )
}

# 4. Panel A: Adopted Plan
panel_a <- make_district_map(
  parcels_sf, muni_boundary, station_clipped, adopted_locs,
  subtitle = paste0(
    format(adopted_metrics$net_capacity, big.mark = ","), " net units"
  )
)

# 5. Panel B: Select 4 strategic MCMC samples
all_sample_locs <- purrr::map(all_chains, get_sample_locs)

net_caps <- all_sample_metrics$net_capacity
pick_quantile <- function(q) {
  target <- quantile(net_caps, q)
  which.min(abs(net_caps - target))
}
idx_low <- which.min(net_caps)
idx_med <- pick_quantile(0.50)
idx_high <- which.max(net_caps)

# Geographic diversity: max Jaccard distance from adopted plan
jaccard_dist <- purrr::map_dbl(all_sample_locs, function(s) {
  1 - length(intersect(s, adopted_locs)) / length(union(s, adopted_locs))
})
idx_geo <- which.max(jaccard_dist)

sample_indices <- c(idx_low, idx_med, idx_high, idx_geo)

panel_b_maps <- purrr::map(sample_indices, function(idx) {
  locs <- all_sample_locs[[idx]]
  cap <- format(net_caps[idx], big.mark = ",")
  make_district_map(
    parcels_sf, muni_boundary, station_clipped, locs,
    subtitle = paste0(cap, " net units")
  )
})

# 6. Panel C: Histogram
pct_label <- paste0(
  "Adopted plan: ",
  scales::ordinal(max(1, round(adopted_percentile * 100))),
  " percentile"
)

panel_c <- ggplot(all_sample_metrics, aes(x = net_capacity)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  geom_vline(
    xintercept = adopted_metrics$net_capacity,
    linetype = "dashed", color = "red", linewidth = 0.8
  ) +
  annotate(
    "label",
    x = adopted_metrics$net_capacity,
    y = Inf, vjust = 1.3, hjust = -0.05,
    label = pct_label,
    color = "red", size = 2.5, fontface = "bold",
    fill = "white", label.padding = unit(0.15, "lines")
  ) +
  scale_x_continuous(
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  labs(x = "Net Housing Capacity (units)", y = "Count") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5)
  )

# 7. Compose final figure
title_theme <- theme(
  plot.title = element_text(face = "bold", size = 9, hjust = 0)
)

design <- "
  ABCF
  ABCF
  ADEF
  ADEF
"

panel_a_titled <- panel_a + ggtitle("A. Adopted Plan") + title_theme
b1 <- panel_b_maps[[1]] + ggtitle("B. Sampled Alternatives") + title_theme
b2 <- panel_b_maps[[2]]
b3 <- panel_b_maps[[3]]
b4 <- panel_b_maps[[4]]
n_samples <- format(nrow(all_sample_metrics), big.mark = ",")
panel_c_titled <- panel_c +
  ggtitle(
    "C. Distribution of Net Capacity",
    subtitle = paste0("across ", n_samples, " simulated compliant configurations")
  ) +
  title_theme +
  theme(plot.subtitle = element_text(size = 7, color = "grey40"))

final_figure <- panel_a_titled + b1 + b2 + b3 + b4 + panel_c_titled +
  plot_layout(design = design, widths = c(3, 2, 2, 3)) +
  plot_annotation(
    title = paste0(
      COMMUNITY,
      ": Adopted Zoning Plan vs. Feasible Alternatives"
    ),
    caption = paste0(
      "Net units = zoned capacity minus existing housing units on each parcel. ",
      "Orange shading = 0.5-mile transit station buffer."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.caption = element_text(size = 7, color = "grey40", hjust = 0)
    )
  )

ggsave(
  "dev/grant_canton_figure.png",
  plot = final_figure,
  width = 11, height = 4.5, units = "in", dpi = 300
)
ggsave(
  "dev/grant_canton_figure.pdf",
  plot = final_figure,
  width = 11, height = 4.5, units = "in"
)
