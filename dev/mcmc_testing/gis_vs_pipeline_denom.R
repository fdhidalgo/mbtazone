# gis_vs_pipeline_denom.R
#
# Compares the two density denominator methods head-to-head:
#
#   pipeline  — st_union(in-district parcel polygons) − GIS deductions
#               What compute_gis_density_denom() does in the live MCMC.
#
#   district  — adopted district polygon − GIS deductions
#               What the Excel model uses. Requires an existing district boundary.
#
# Reads density_denominator_all.csv produced by density_denominator_comparison.R.
# Run that script first.

library(ggplot2)
library(data.table)

dt <- data.table::fread("dev/mcmc_testing/density_denominator_all.csv")

dt[, district_minus_pipeline := round(denom_district - denom_pipeline, 3)]
dt[, pipeline_closer_excel   := abs(denom_pipeline - denom_excel) <
                                  abs(denom_district - denom_excel)]

has_excel <- dt[!is.na(denom_excel) & !is.na(denom_district)]

cat("=== District denominator vs pipeline denominator ===\n\n")
print(has_excel[order(-abs(district_minus_pipeline)),
                .(community, denom_district, denom_pipeline, denom_excel,
                  district_minus_pipeline,
                  district_vs_excel = round(denom_district - denom_excel, 3),
                  pipeline_vs_excel = round(denom_pipeline - denom_excel, 3),
                  pipeline_closer_excel)],
      nrow = 100)

n_pipe <- sum(has_excel$pipeline_closer_excel, na.rm = TRUE)
n_dist <- sum(!has_excel$pipeline_closer_excel, na.rm = TRUE)
cat(sprintf("\nPipeline closer to Excel:  %d / %d\n", n_pipe, nrow(has_excel)))
cat(sprintf("District closer to Excel:  %d / %d\n", n_dist, nrow(has_excel)))

p <- ggplot(has_excel, aes(x = denom_pipeline, y = denom_district)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey50", linetype = "dashed") +
  geom_point(aes(colour = pipeline_closer_excel), size = 2.5) +
  ggrepel::geom_text_repel(
    data = has_excel[abs(district_minus_pipeline) > 5],
    aes(label = community), size = 2.8, segment.colour = "grey60"
  ) +
  scale_colour_manual(
    values = c(`TRUE` = "#2171b5", `FALSE` = "#d73027"),
    labels = c(`TRUE` = "Pipeline closer to Excel",
               `FALSE` = "District closer to Excel"),
    name = NULL
  ) +
  labs(
    title    = "District denominator vs pipeline denominator",
    subtitle = paste0(
      "Dashed = identical. Points above line: district > pipeline ",
      "(gaps/roads excluded by parcel union).\n",
      "Colour: which method is closer to Excel ground truth."
    ),
    x = "Pipeline denominator — parcel union (ac)",
    y = "District denominator — boundary polygon (ac)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("dev/mcmc_testing/district_vs_pipeline_denom.png",
       p, width = 8, height = 7, dpi = 150)
cat("Saved: dev/mcmc_testing/district_vs_pipeline_denom.png\n")
