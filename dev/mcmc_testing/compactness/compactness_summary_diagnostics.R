library(data.table)
library(ggplot2)
library(patchwork)  # install.packages("patchwork") if needed

dt <- fread("./dev/mcmc_testing/compactness/compactness_graph_results.csv")

# Exclude degenerate single-node districts from shape metric summaries
# (they score 0 on everything and are not meaningful plan shapes)
dt_valid <- dt[n_nodes > 1]
cat(sprintf("Total districts: %d | Single-node (excluded from shape stats): %d\n\n",
            nrow(dt), nrow(dt) - nrow(dt_valid)))

# ── 1. Summary stats ───────────────────────────────────────────────────────

cat("══ Summary statistics (all districts, n_nodes > 1) ══════════════════\n\n")

metrics <- c("isoperimetric", "internal_deg_ratio", "diameter_ratio",
             "fiedler_norm", "n_components", "n_holes")

stat_list <- lapply(metrics, function(m) {
  v <- dt_valid[[m]]
  data.table(
    metric = m,
    n      = sum(!is.na(v)),
    min    = round(min(v,    na.rm = TRUE), 4),
    p10    = round(quantile(v, .10, na.rm = TRUE), 4),
    p25    = round(quantile(v, .25, na.rm = TRUE), 4),
    median = round(median(v, na.rm = TRUE), 4),
    mean   = round(mean(v,   na.rm = TRUE), 4),
    p75    = round(quantile(v, .75, na.rm = TRUE), 4),
    p90    = round(quantile(v, .90, na.rm = TRUE), 4),
    max    = round(max(v,    na.rm = TRUE), 4),
    n_na   = sum(is.na(v))
  )
})
print(rbindlist(stat_list))

cat("\n══ By community type ════════════════════════════════════════════════\n\n")
print(
  dt_valid[, .(
    n                  = .N,
    iso_med            = round(median(isoperimetric,       na.rm = TRUE), 3),
    idr_med            = round(median(internal_deg_ratio,  na.rm = TRUE), 3),
    diam_med           = round(median(diameter_ratio,      na.rm = TRUE), 3),
    fiedler_med        = round(median(fiedler_norm,        na.rm = TRUE), 5),
    pct_fragmented     = round(mean(n_components > 1)    * 100, 1),
    pct_holes          = round(mean(n_holes > 0)         * 100, 1),
    med_components     = median(n_components)
  ), by = community_type][order(community_type)]
)

# ── 2. Flagged districts ───────────────────────────────────────────────────

cat("\n══ Single-node districts (degenerate) ═══════════════════════════════\n")
print(dt[n_nodes == 1, .(community_name, community_type, n_nodes, n_holes)])

cat("\n══ Highly fragmented (n_components >= 5) ════════════════════════════\n")
print(
  dt_valid[n_components >= 5,
           .(community_name, community_type, n_nodes, n_components,
             largest_comp_pct = round(largest_comp_pct, 3),
             isoperimetric, internal_deg_ratio, diameter_ratio)
  ][order(-n_components)]
)

cat("\n══ Worst shape: internal_deg_ratio (bottom 10, n_nodes > 1) ════════\n")
print(
  dt_valid[order(internal_deg_ratio)][
    1:min(10, .N),
    .(community_name, community_type, n_nodes, n_components,
      isoperimetric, internal_deg_ratio, diameter_ratio)
  ]
)

cat("\n══ Most elongated: diameter_ratio (top 10) ══════════════════════════\n")
print(
  dt_valid[!is.na(diameter_ratio)][order(-diameter_ratio)][
    1:min(10, .N),
    .(community_name, community_type, n_nodes, n_components,
      isoperimetric, internal_deg_ratio, diameter_ratio)
  ]
)

cat("\n══ Districts with holes ═════════════════════════════════════════════\n")
print(
  dt[n_holes > 0][order(-n_holes),
                  .(community_name, community_type, n_nodes, n_components,
                    n_holes, isoperimetric, internal_deg_ratio)]
)

# ── 3. Plots ───────────────────────────────────────────────────────────────

type_colours <- c(
  adjacent            = "#4e79a7",
  adjacent_small_town = "#f28e2b",
  commuter_rail       = "#59a14f",
  rapid_transit       = "#e15759"
)

# Helper: histogram + density per metric, faceted by community type
plot_metric <- function(data, col, xlab, binwidth = NULL, log_x = FALSE) {
  p <- ggplot(data[!is.na(get(col))], aes(x = get(col), fill = community_type)) +
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = binwidth, colour = "white", linewidth = 0.2,
                   position = "identity", alpha = 0.85) +
    geom_density(aes(colour = community_type), linewidth = 0.7, fill = NA) +
    facet_wrap(~community_type, ncol = 2, scales = "free_y") +
    scale_fill_manual(values  = type_colours, guide = "none") +
    scale_colour_manual(values = type_colours, guide = "none") +
    labs(x = xlab, y = "Density", title = xlab) +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank())
  if (log_x) p <- p + scale_x_log10()
  p
}

p_iso  <- plot_metric(dt_valid, "isoperimetric",     "Isoperimetric ratio",    binwidth = 0.05)
p_idr  <- plot_metric(dt_valid, "internal_deg_ratio","Internal degree ratio",  binwidth = 0.05)
p_diam <- plot_metric(dt_valid[!is.na(diameter_ratio)],
                      "diameter_ratio",  "Diameter ratio",         binwidth = 0.2)
p_fied <- plot_metric(dt_valid[!is.na(fiedler_norm) & fiedler_norm > 0],
                      "fiedler_norm",    "Fiedler (norm, log scale)", log_x = TRUE)

# Component count bar chart
p_comp <- ggplot(dt_valid, aes(x = factor(pmin(n_components, 10L)),
                               fill = community_type)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = type_colours, name = "Type") +
  scale_x_discrete(labels = function(x) ifelse(x == "10", "10+", x)) +
  labs(x = "Number of components (capped at 10+)",
       y = "Count", title = "Component count") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# Scatter: isoperimetric vs internal_deg_ratio, sized by n_nodes, coloured by type
p_scatter <- ggplot(dt_valid,
                    aes(x = isoperimetric, y = internal_deg_ratio,
                        colour = community_type, size = log1p(n_nodes))) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  scale_colour_manual(values = type_colours, name = "Type") +
  scale_size_continuous(range = c(1, 6), guide = "none") +
  labs(x = "Isoperimetric ratio", y = "Internal degree ratio",
       title = "Isoperimetric vs internal degree ratio",
       subtitle = "Point size ∝ log(n_nodes) | dashed line = perfect agreement") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# Combine and save
full_plot <- (p_iso | p_idr) / (p_diam | p_fied) / (p_comp | p_scatter)
ggsave("./dev/mcmc_testing/compactness/compactness_diagnostics.pdf", full_plot,
       width = 14, height = 16, device = "pdf")
ggsave("./dev/mcmc_testing/compactness/compactness_diagnostics.png", full_plot,
       width = 14, height = 16, dpi = 150)

cat("\nPlots saved to compactness_diagnostics.pdf / .png\n")
