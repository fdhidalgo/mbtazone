library(targets)
library(mapgl)
library(sf)
library(data.table)

district_name <- "Abington"
district_type <- "adjacent" #One of: "rapid_transit", "commuter_rail", "adjacent", and "adjacent_small_town"
store <- paste0("ext/_targets_", gsub(" ", "_", district_name))

Sys.setenv(DISTRICT_NAME = district_name, DISTRICT_TYPE = district_type)

tar_load(constraints, store = store)
tar_load(district_data, store = store)
tar_load(parcel_graph_result, store = store)
tar_load(district_paths, store = store)

# ── Setup ──────────────────────────────────────────────────────────────────
adopted     <- district_data$district_boundary
pg          <- parcel_graph_result$parcel_graph
pa          <- parcel_graph_result$parcel_assignments

adopted_proj <- sf::st_transform(adopted, sf::st_crs(district_data$district_geometry))

adopted_union <- sf::st_union(adopted_proj)

# Now use centroid within on the unioned boundary
centroids <- sf::st_centroid(district_data$district_geometry)
centroid_within <- sf::st_within(centroids, adopted_union, sparse = FALSE)[, 1]
adopted_parcels <- district_data$district_geometry[centroid_within, ]
adopted_ids <- adopted_parcels$LOC_ID
adopted_units   <- pa[parcel_id %in% adopted_ids, unique(unit_id)]
adopted_in_graph <- adopted_units[adopted_units %in% igraph::V(pg)$name]

adopted_cap          <- sum(igraph::V(pg)[adopted_in_graph]$capacity)
adopted_area         <- sum(igraph::V(pg)[adopted_in_graph]$area)
adopted_density      <- adopted_cap / adopted_area
adopted_station_cap  <- sum(igraph::V(pg)[adopted_in_graph]$capacity_in_station)
adopted_station_area <- sum(igraph::V(pg)[adopted_in_graph]$area_in_station)

required_station_cap  <- (constraints$station_capacity_pct / 100) * constraints$min_capacity
required_station_area <- (constraints$station_area_pct / 100) * constraints$min_area

cat("Adopted boundary metrics:\n")
cat("  capacity:", adopted_cap, "vs min_capacity:", constraints$min_capacity,
    "→", ifelse(adopted_cap >= constraints$min_capacity, "✓", "✗"), "\n")
cat("  area:", round(adopted_area, 1), "vs min_area:", constraints$min_area,
    "→", ifelse(adopted_area >= constraints$min_area, "✓", "✗"), "\n")
cat("  density:", round(adopted_density, 2), "vs min_density:", constraints$min_density,
    "→", ifelse(adopted_density >= constraints$min_density, "✓", "✗"), "\n")
cat("  station_cap:", round(adopted_station_cap), "vs required:", round(required_station_cap),
    "→", ifelse(adopted_station_cap >= required_station_cap, "✓", "✗"), "\n")
cat("  station_area:", round(adopted_station_area, 1), "vs required:", round(required_station_area, 1),
    "→", ifelse(adopted_station_area >= required_station_area, "✓", "✗"), "\n")

adopted_subgraph <- igraph::induced_subgraph(pg, adopted_in_graph)
cat("  connected:", igraph::is_connected(adopted_subgraph),
    "| components:", igraph::components(adopted_subgraph)$no, "\n")

# ── Prepare map layers ─────────────────────────────────────────────────────
# Load raw parcels for tooltips
raw_parcels <- load_municipality(
  shapefile        = file.path(district_paths$parcels),
  community_name   = district_paths$district_name,
  projection       = 26986,
  validate         = TRUE
)

inst_lookup <- data.frame(
  LOC_ID           = raw_parcels$LOC_ID,
  is_institutional = raw_parcels$PublicInst > 0,
  use_desc         = raw_parcels$UseDesc
)

district_parcels <- district_data$district_geometry |>
  sf::st_simplify(dTolerance = 10) |>
  sf::st_transform(4326)

district_parcels <- merge(district_parcels, inst_lookup, by = "LOC_ID", all.x = TRUE)
district_parcels <- sf::st_as_sf(district_parcels)
district_parcels$is_institutional[is.na(district_parcels$is_institutional)] <- FALSE
district_parcels$fill_opacity <- ifelse(district_parcels$is_institutional, 0, 0.8)

# Mark adopted boundary parcels
district_parcels$in_adopted <- district_parcels$LOC_ID %in% adopted_ids

# Capacity palette
positive_cap <- district_parcels[district_parcels$capacity > 0, ]
cap_95 <- quantile(positive_cap$capacity, 0.95)
district_parcels$capacity_capped <- pmin(district_parcels$capacity, cap_95)
capped_data <- district_parcels[district_parcels$capacity_capped > 0, ]
capacity_palette <- interpolate_palette(
  data    = capped_data,
  column  = "capacity_capped",
  palette = colorRampPalette(c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#084594"))
)

# Adopted parcels sf
adopted_sf <- district_parcels[district_parcels$in_adopted, ]
adopted_wgs <- sf::st_transform(
  sf::st_make_valid(adopted_sf), 4326
)

adopted_wgs$density      <- round(adopted_wgs$capacity / adopted_wgs$area, 1)
adopted_wgs$in_station   <- adopted_wgs$LOC_ID %in%
  pa[unit_id %in% adopted_in_graph[igraph::V(pg)[adopted_in_graph]$capacity_in_station > 0],
     parcel_id]

# ── Map ────────────────────────────────────────────────────────────────────
maplibre(
  style  = carto_style("positron"),
  bounds = district_parcels
) |>
  # Background capacity layer
  add_fill_layer(
    id           = "capacity_fill",
    source       = district_parcels,
    fill_color   = capacity_palette$expression,
    fill_opacity = interpolate(
      column = "fill_opacity",
      values = c(0, 0.8),
      stops  = c(0, 0.5)
    )
  ) |>
  add_line_layer(
    id           = "parcel_outline",
    source       = district_parcels,
    line_color   = "#333333",
    line_width   = 0.2,
    line_opacity = 0.3
  ) |>
  # Station areas
  add_fill_layer(
    id           = "station_areas",
    source       = sf::st_transform(district_data$transit_stations, 4326),
    fill_color   = "orange",
    fill_opacity = 0.2
  ) |>
  # Adopted boundary parcels (highlighted)
  add_fill_layer(
    id           = "adopted_parcels",
    source       = adopted_wgs,
    fill_color   = "#e41a1c",
    fill_opacity = 0.7,
    tooltip      = concat(
      "Parcel: ",    get_column("LOC_ID"),
      "<br>Cap: ",   get_column("capacity"),
      "<br>Density: ", get_column("density"),
      "<br>Use: ",   get_column("use_desc")
    )
  ) |>
  # Adopted boundary outline
  add_line_layer(
    id         = "adopted_boundary",
    source     = sf::st_transform(adopted, 4326),
    line_color = "purple",
    line_width = 4
  ) |>
  add_categorical_legend(
    legend_title = "Layers",
    values       = c("Adopted Boundary Parcels", "Station Areas", "Adopted Boundary"),
    colors       = c("#e41a1c", "orange", "purple")
  ) |>
  add_fullscreen_control(position = "top-left") |>
  add_navigation_control()
