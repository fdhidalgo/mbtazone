library(targets)
library(data.table)
library(igraph)
library(ggplot2)

# ── Configuration ──────────────────────────────────────────────────────────

district_name <- "Andover"
exports_dir   <- "~/code/mbtazone/ext/exports"
output_dir    <- "~/code/mbtazone/dev/mcmc_testing/compactness/outputs/"
store         <- paste0("ext/_targets_", gsub(" ", "_", district_name))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ── Load compactness helpers ───────────────────────────────────────────────
# Source the compactness_helpers.R script to get the metric functions
# (isoperimetric_one, fiedler_one, diameter_ratio_one,
#  internal_degree_ratio_one, weighted_mean_metric, count_holes,
#  compute_graph_compactness)
source("~/code/mbtazone/dev/mcmc_testing/compactness/compactness_helpers.R")

# ── Load district targets ──────────────────────────────────────────────────
cat(sprintf("Loading targets for %s...\n", district_name))

tar_load(parcel_graph_result, store = store, envir = environment())
tar_load(district_data,       store = store, envir = environment())

pg <- parcel_graph_result$parcel_graph
pa <- parcel_graph_result$parcel_assignments

# ── Resolve adopted plan nodes ─────────────────────────────────────────────
adopted        <- district_data$district_boundary
adopted_proj   <- sf::st_transform(adopted, sf::st_crs(district_data$district_geometry))
adopted_union  <- sf::st_union(adopted_proj)
centroids      <- sf::st_centroid(district_data$district_geometry)
within_mask    <- sf::st_within(centroids, adopted_union, sparse = FALSE)[, 1]
adopted_ids    <- district_data$district_geometry[within_mask, ]$LOC_ID
adopted_units  <- pa[parcel_id %in% adopted_ids, unique(unit_id)]
adopted_nodes  <- adopted_units[adopted_units %in% igraph::V(pg)$name]

cat(sprintf("  Adopted plan: %d nodes\n", length(adopted_nodes)))

# ── Compute adopted plan metrics ───────────────────────────────────────────
adopted_metrics <- compute_graph_compactness(pg, adopted_nodes)
adopted_dt <- as.data.table(adopted_metrics)
adopted_dt[, plan_id := "adopted"]
cat("  Adopted metrics computed\n")

# ── Load exported sampled plans ────────────────────────────────────────────
plans_path <- file.path(exports_dir, sprintf("%s_plans.csv", district_name))
cat(sprintf("Loading sampled plans from %s...\n", plans_path))

plans_wide <- fread(plans_path)

# Identify plan columns
plan_cols <- grep("^plan_", names(plans_wide), value = TRUE)
n_plans   <- length(plan_cols)
cat(sprintf("  Found %d sampled plans, %d parcels\n", n_plans, nrow(plans_wide)))

# Build parcel -> unit lookup (1-1 or many-to-1)
# pa has columns: parcel_id, unit_id
parcel_to_unit <- pa[, .(unit_id = unique(unit_id)), by = parcel_id]

# ── Compute metrics for each sampled plan ──────────────────────────────────
cat(sprintf("Computing compactness for %d sampled plans...\n", n_plans))

sampled_results <- vector("list", n_plans)

for (j in seq_along(plan_cols)) {
  col <- plan_cols[j]

  # Parcels included in this plan
  included_parcels <- plans_wide[get(col) == 1L, parcel_id]
  if (length(included_parcels) == 0L) next

  # Resolve to graph unit IDs
  included_units <- parcel_to_unit[parcel_id %in% included_parcels, unique(unit_id)]
  plan_nodes     <- included_units[included_units %in% igraph::V(pg)$name]
  if (length(plan_nodes) == 0L) next

  metrics <- tryCatch(
    compute_graph_compactness(pg, plan_nodes),
    error = function(e) NULL
  )
  if (is.null(metrics)) next

  sampled_results[[j]] <- data.table(
    plan_id            = col,
    n_nodes            = metrics$n_nodes,
    n_components       = metrics$n_components,
    largest_comp_pct   = metrics$largest_comp_pct,
    isoperimetric      = metrics$isoperimetric,
    fiedler_norm       = metrics$fiedler_norm,
    diameter_ratio     = metrics$diameter_ratio,
    internal_deg_ratio = metrics$internal_deg_ratio,
    n_holes            = metrics$n_holes
  )

  if (j %% 10 == 0) cat(sprintf("  %d / %d plans done\n", j, n_plans))
}

sampled_dt <- rbindlist(sampled_results, use.names = TRUE, fill = TRUE)
cat(sprintf("  %d plans successfully computed\n", nrow(sampled_dt)))

# ── Summary ────────────────────────────────────────────────────────────────
cat("\n══ Sampled plan summary ════════════════════════════════════════════\n")
metrics_to_show <- c("isoperimetric", "internal_deg_ratio", "diameter_ratio",
                     "fiedler_norm", "n_components", "n_holes")
for (m in metrics_to_show) {
  v <- sampled_dt[[m]]
  cat(sprintf("\n%s:\n", m))
  cat(sprintf("  adopted : %.4f\n", adopted_metrics[[m]]))
  cat(sprintf("  mean    : %.4f\n", mean(v, na.rm = TRUE)))
  cat(sprintf("  median  : %.4f\n", median(v, na.rm = TRUE)))
  cat(sprintf("  p10-p90 : %.4f - %.4f\n",
              quantile(v, .1, na.rm = TRUE),
              quantile(v, .9, na.rm = TRUE)))
}

# ── Plots ──────────────────────────────────────────────────────────────────

# Melt to long for faceting
plot_metrics <- c("isoperimetric", "internal_deg_ratio", "diameter_ratio",
                  "fiedler_norm")

plot_dt <- melt(
  sampled_dt[, c("plan_id", plot_metrics), with = FALSE],
  id.vars    = "plan_id",
  variable.name = "metric",
  value.name    = "value"
)

# Adopted reference lines
adopted_ref <- data.table(
  metric = plot_metrics,
  value  = c(adopted_metrics$isoperimetric,
             adopted_metrics$internal_deg_ratio,
             adopted_metrics$diameter_ratio,
             adopted_metrics$fiedler_norm)
)

# Friendly labels
metric_labels <- c(
  isoperimetric      = "Isoperimetric ratio",
  internal_deg_ratio = "Internal degree ratio",
  diameter_ratio     = "Diameter ratio",
  fiedler_norm       = "Fiedler (normalised)"
)
plot_dt[,     metric := factor(metric, levels = plot_metrics,
                               labels = metric_labels)]
adopted_ref[, metric := factor(metric, levels = plot_metrics,
                               labels = metric_labels)]

p_dist <- ggplot(plot_dt, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#4e79a7", colour = "white",
                 linewidth = 0.2, alpha = 0.85) +
  geom_density(colour = "#4e79a7", linewidth = 0.7) +
  geom_vline(data = adopted_ref,
             aes(xintercept = value),
             colour = "#e15759", linewidth = 1, linetype = "dashed") +
  facet_wrap(~metric, scales = "free", ncol = 2) +
  labs(
    title    = sprintf("%s: sampled vs adopted plan compactness", district_name),
    subtitle = sprintf("%d sampled plans | red dashed = adopted plan value", nrow(sampled_dt)),
    x        = NULL,
    y        = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Component count and holes as bar charts
p_comps <- ggplot(sampled_dt, aes(x = factor(pmin(n_components, 10L)))) +
  geom_bar(fill = "#4e79a7", alpha = 0.85) +
  geom_vline(xintercept = which(levels(factor(pmin(sampled_dt$n_components, 10L))) ==
                                  as.character(min(adopted_metrics$n_components, 10L))),
             colour = "#e15759", linewidth = 1, linetype = "dashed") +
  scale_x_discrete(labels = function(x) ifelse(x == "10", "10+", x)) +
  labs(title = "Component count", x = "Components", y = "Count",
       subtitle = sprintf("Adopted = %d (red)", adopted_metrics$n_components)) +
  theme_minimal(base_size = 12)

p_holes <- ggplot(sampled_dt, aes(x = factor(pmin(n_holes, 10L)))) +
  geom_bar(fill = "#f28e2b", alpha = 0.85) +
  geom_vline(xintercept = which(levels(factor(pmin(sampled_dt$n_holes, 10L))) ==
                                  as.character(min(adopted_metrics$n_holes, 10L))),
             colour = "#e15759", linewidth = 1, linetype = "dashed") +
  scale_x_discrete(labels = function(x) ifelse(x == "10", "10+", x)) +
  labs(title = "Hole count", x = "Holes", y = "Count",
       subtitle = sprintf("Adopted = %d (red)", adopted_metrics$n_holes)) +
  theme_minimal(base_size = 12)

# Save
ggsave(
  file.path(output_dir, sprintf("%s_compactness_comparison.pdf", district_name)),
  patchwork::wrap_plots(p_dist, patchwork::wrap_plots(p_comps, p_holes), ncol = 1,
                        heights = c(2, 1)),
  width = 12, height = 14
)
ggsave(
  file.path(output_dir, sprintf("%s_compactness_comparison.png", district_name)),
  patchwork::wrap_plots(p_dist, patchwork::wrap_plots(p_comps, p_holes), ncol = 1,
                        heights = c(2, 1)),
  width = 12, height = 14, dpi = 150
)

cat(sprintf("\nPlots saved to %s\n", file.path(output_dir, sprintf("%s_compactness_comparison.pdf/.png", district_name))))

# Save sampled metrics
fwrite(sampled_dt, file.path(output_dir, sprintf("%s_sampled_compactness.csv", district_name)))
cat(sprintf("Sampled metrics saved to %s\n", file.path(output_dir, sprintf("%s_sampled_compactness.csv", district_name))))

