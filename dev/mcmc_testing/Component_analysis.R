library(mapgl)
library(sf)
library(data.table)

pg <- parcel_graph_result$parcel_graph
comp <- igraph::components(pg)

# Build component summary
comp_dt <- data.table(
  comp_id   = seq_len(comp$no),
  n_parcels = comp$csize,
  capacity  = vapply(seq_len(comp$no), function(i)
    sum(igraph::V(pg)$capacity[comp$membership == i]), numeric(1))
)
comp_dt[, label := paste0("C", comp_id, "\ncap=", scales::comma(round(capacity)),
                          "\nn=", scales::comma(n_parcels))]

# Add component membership to parcel sf
parcel_sf_comp <- district_data$district_geometry |>
  sf::st_transform(4326)

# Match parcels to components via parcel_assignments
pa <- parcel_graph_result$parcel_assignments
parcel_comp <- merge(
  pa[, .(parcel_id, unit_id)],
  data.table(unit_id = igraph::V(pg)$name,
             comp_id = comp$membership),
  by = "unit_id"
)
parcel_sf_comp <- merge(parcel_sf_comp,
                        parcel_comp[, .(LOC_ID = parcel_id, comp_id)],
                        by = "LOC_ID", all.x = TRUE)
parcel_sf_comp <- sf::st_as_sf(parcel_sf_comp)
parcel_sf_comp$comp_label <- paste0("C", parcel_sf_comp$comp_id)

# Compute component centroids for labels
comp_centroids <- parcel_sf_comp |>
  dplyr::group_by(comp_id) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  sf::st_centroid() |>
  dplyr::mutate(
    label = comp_dt$label[match(comp_id, comp_dt$comp_id)],
    lon = sf::st_coordinates(geometry)[,1],
    lat = sf::st_coordinates(geometry)[,2]
  )

# Assign a color per component (top 5 by capacity get distinct colors)
top_comps <- comp_dt[order(-capacity)][1:min(5, .N), comp_id]
comp_colors <- setNames(
  c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#999999"),
  c(as.character(top_comps), "other")
)
parcel_sf_comp$color_key <- ifelse(
  parcel_sf_comp$comp_id %in% top_comps,
  as.character(parcel_sf_comp$comp_id),
  "other"
)

maplibre(style = carto_style("positron"), bounds = parcel_sf_comp) |>
  add_fill_layer(
    id           = "components",
    source       = parcel_sf_comp,
    fill_color   = match_expr(
      column = "color_key",
      values = names(comp_colors),
      stops  = unname(comp_colors)
    ),
    fill_opacity = 0.6,
    tooltip      = concat(
      "Component: C", get_column("comp_id"),
      "<br>Parcel: ", get_column("LOC_ID")
    )
  ) |>
  add_line_layer(
    id           = "outlines",
    source       = parcel_sf_comp,
    line_color   = "#333333",
    line_width   = 0.2,
    line_opacity = 0.4
  ) |>
  add_categorical_legend(
    legend_title = "Components",
    values       = c(paste0("C", top_comps), "other"),
    colors       = unname(comp_colors)
  ) |>
  add_fullscreen_control(position = "top-left") |>
  add_navigation_control()
