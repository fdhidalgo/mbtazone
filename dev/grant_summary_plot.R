## Grant summary figure: Adopted plan percentile across municipalities
##
## Reads completed MCMC target stores for all grant municipalities,
## computes each adopted plan's net-capacity percentile, and produces
## a Cleveland dot plot colored by community type.
##
## Run after grant_batch_run.R completes (or after enough municipalities
## have finished to make a useful figure).

library(targets)
library(arrow)
library(data.table)
library(purrr)
library(ggplot2)
library(scales)

# ── Configuration ────────────────────────────────────────────────────────────
RESIDENSITY_PATH <- file.path(
  "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data",
  "ioc_mbta_communities_hackathon/residensity_mbta_communities.parquet"
)

# All 25 grant municipalities (same list as grant_batch_run.R)
grant_municipalities <- c(
  "Braintree", "Everett",
  "Canton", "Leominster",
  "Abington", "Cohasset", "Foxborough", "Norwood",
  "Reading", "Rockport", "Salem", "Wakefield", "Whitman",
  "Peabody",
  "Burlington", "Chelmsford", "Danvers", "Grafton", "Hanover", "Marlborough",
  "Ashby", "Boxborough", "Bourne", "Groton", "Topsfield"
)

# Load community metadata for types
community_info <- data.table::fread("inst/extdata/community_info.csv")

# Load residensity data once (shared across all municipalities)
resid <- arrow::read_parquet(RESIDENSITY_PATH)
data.table::setDT(resid)

# ── Helper: summarize a set of plan LOC_IDs against the parcel lookup ────────
summarize_plan <- function(plan_locs, parcel_lookup) {
  matched <- parcel_lookup[.(plan_locs), nomatch = NULL]
  list(
    total_capacity = sum(matched$capacity, na.rm = TRUE),
    net_capacity = sum(matched$net_per_parcel, na.rm = TRUE),
    baseline_hu = sum(matched$baseline_hu, na.rm = TRUE),
    n_parcels = nrow(matched)
  )
}

# ── Extract percentile for each municipality ─────────────────────────────────
extract_percentile <- function(community_name) {
  store <- paste0("ext/_targets_", gsub(" ", "_", community_name))

  # Load targets
  district_data <- targets::tar_read("district_data", store = store)
  parcel_graph_result <- targets::tar_read("parcel_graph_result", store = store)
  chain_results <- targets::tar_read("all_parcel_chain_results", store = store)

  dp <- district_data$district_parcels
  pa <- parcel_graph_result$parcel_assignments
  data.table::setDT(pa)
  data.table::setkey(pa, "unit_id")

  # Build parcel lookup with existing housing units
  cap_lookup <- dp[, .(LOC_ID, capacity)]
  hu_lookup <- resid[City == community_name, .(LOC_ID, baseline_hu = residentialunits)]
  hu_lookup[is.na(baseline_hu), baseline_hu := 0]

  parcel_lookup <- merge(cap_lookup, hu_lookup, by = "LOC_ID", all.x = TRUE)
  parcel_lookup[is.na(baseline_hu), baseline_hu := 0]
  parcel_lookup[, net_per_parcel := pmax(0, capacity - baseline_hu)]
  data.table::setkey(parcel_lookup, "LOC_ID")

  # Adopted plan metrics
  adopted_locs <- dp[in_district == TRUE, LOC_ID]
  adopted_metrics <- summarize_plan(adopted_locs, parcel_lookup)

  # MCMC sample metrics
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

  list(
    community_name = community_name,
    adopted_net_capacity = adopted_metrics$net_capacity,
    adopted_percentile = adopted_percentile,
    n_samples = nrow(all_sample_metrics),
    median_sample_capacity = median(all_sample_metrics$net_capacity)
  )
}

# ── Loop over municipalities, skipping those without completed stores ────────
cat("Extracting percentiles from completed target stores...\n\n")
results <- list()

for (name in grant_municipalities) {
  store <- paste0("ext/_targets_", gsub(" ", "_", name))
  ok <- tryCatch({
    targets::tar_read("all_parcel_chain_results", store = store)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    cat(sprintf("  SKIP: %s (no completed target store)\n", name))
    next
  }

  cat(sprintf("  Extracting: %s ... ", name))
  res <- tryCatch(
    extract_percentile(name),
    error = function(e) {
      cat(sprintf("ERROR: %s\n", conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(res)) {
    results <- c(results, list(res))
    cat(sprintf(
      "percentile = %.0f%% (%d samples)\n",
      res$adopted_percentile * 100, res$n_samples
    ))
  }
}

if (length(results) == 0) {
  stop("No completed municipalities found. Run grant_batch_run.R first.")
}

summary_dt <- data.table::rbindlist(results)

# Merge community type
summary_dt <- merge(
  summary_dt,
  community_info[, .(community_name, community_type)],
  by = "community_name"
)

cat(sprintf(
  "\n%d municipalities extracted successfully.\n\n",
  nrow(summary_dt)
))
print(summary_dt[order(adopted_percentile),
  .(community_name, community_type, adopted_percentile, n_samples)
])

# ── Cleveland dot plot ───────────────────────────────────────────────────────

# Clean community type labels for display
type_labels <- c(
  "rapid_transit"      = "Rapid Transit",
  "commuter_rail"      = "Commuter Rail",
  "adjacent"           = "Adjacent Community",
  "adjacent_small_town" = "Adjacent Small Town"
)
summary_dt[, community_type_label := type_labels[community_type]]

# Order municipalities by percentile (lowest at left)
summary_dt[, community_name := factor(
  community_name,
  levels = summary_dt[order(adopted_percentile), community_name]
)]

# Tol qualitative palette — colorblind-safe, distinct from blue/orange in Fig 1
type_colors <- c(
  "Rapid Transit"      = "#332288",
  "Commuter Rail"      = "#CC6677",
  "Adjacent Community"  = "#44AA99",
  "Adjacent Small Town" = "#DDCC77"
)

# Order factor levels so legend reads top-to-bottom matching typical interest
summary_dt[, community_type_label := factor(
  community_type_label,
  levels = c("Rapid Transit", "Commuter Rail",
             "Adjacent Community", "Adjacent Small Town")
)]

n_munis <- nrow(summary_dt)

grant_dot_plot <- ggplot(
  summary_dt,
  aes(
    x = community_name,
    y = adopted_percentile * 100
  )
) +
  geom_hline(
    yintercept = 50, linetype = "dashed", color = "grey60", linewidth = 0.4
  ) +
  geom_segment(
    aes(xend = community_name, y = 0, yend = adopted_percentile * 100),
    linewidth = 0.4, color = "grey70"
  ) +
  geom_point(size = 3, color = "#332288") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    y = "Adopted Plan Percentile",
    x = NULL,
    title = "Where Do Adopted Plans Fall Among Feasible Alternatives?",
    subtitle = paste0(
      "Percentile rank of each municipality's adopted plan within its ",
      "distribution of feasible alternatives (N = ", n_munis, ")"
    ),
    caption = paste0(
      "Lower percentiles indicate municipalities whose adopted plans enable ",
      "less new housing than most legally available alternatives.\n",
      "Net capacity = zoned unit capacity minus existing housing units per parcel."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    plot.caption = element_text(size = 7, color = "grey40", hjust = 0),
    plot.margin = margin(10, 15, 5, 10)
  )

# ── Save ─────────────────────────────────────────────────────────────────────
fig_width <- 7.5
fig_height <- 4

ggsave(
  "dev/grant_summary_figure.png",
  plot = grant_dot_plot,
  width = fig_width, height = fig_height, units = "in", dpi = 300
)
ggsave(
  "dev/grant_summary_figure.pdf",
  plot = grant_dot_plot,
  width = fig_width, height = fig_height, units = "in"
)

cat(sprintf(
  "\nFigure saved to dev/grant_summary_figure.png (%.1f x %.1f in)\n",
  fig_width, fig_height
))








