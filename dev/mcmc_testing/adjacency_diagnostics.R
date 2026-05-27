# plot_adjacency_diagnostic.R
#
# Interactive diagnostic map of the adjacency graph for a given district.
#
# Shows (all togglable as map layers):
#   - ROW polygons clipped to the district bounding box
#   - All parcels in the district
#   - Isolated parcels (degree 0) highlighted separately
#   - Direct touching edges
#   - ROW-crossing edges
#
# Edge endpoints use st_point_on_surface() rather than st_centroid(), so
# the anchor point is always guaranteed to lie inside the parcel polygon —
# important for irregular or concave lots where the centroid falls outside.
#
# Usage:
#   1. Set `store` (and optionally `district_name`) before sourcing, e.g.:
#        store         <- "ext/_targets_Norwood"
#        district_name <- "Norwood"
#        source("plot_adjacency_diagnostic.R")
#   2. The interactive map is saved as an HTML file to dev/mcmc_testing/plots/
#      and opened in the viewer.
#   3. Isolated parcel details are available in `adjacency_diagnostics` in
#      the calling environment — no other files are written.
#
# Dependencies: targets, sf, mapgl, igraph, glue, htmlwidgets, mbtazone

# ============================================================================
# CONFIGURATION
# ============================================================================

if (!exists("store")) {
  stop(paste(
    "Please set `store` before sourcing this file.",
    "e.g.  store <- 'ext/_targets_Norwood'",
    sep = "\n"
  ))
}

if (!exists("district_name")) {
  district_name <- gsub(".*_targets_", "", store)
  message(glue::glue(
    "district_name not set — inferred as '{district_name}' from store path"
  ))
}

# ============================================================================
# LOAD PIPELINE ARTIFACTS
# ============================================================================

message("Reading targets artifacts...")

district_data   <- targets::tar_read(district_data,   store = store)
adjacency_graph <- targets::tar_read(adjacency_graph, store = store)

parcel_sf <- district_data$district_geometry      # sf, EPSG:26986
row_sf    <- district_data$district_right_of_way  # sf or NULL

# ============================================================================
# EXTRACT GRAPH PROPERTIES
# ============================================================================

message("Extracting edges from adjacency graph...")

el <- igraph::as_data_frame(adjacency_graph, what = "edges")
# Columns: from, to, is_row_crossing

deg          <- igraph::degree(adjacency_graph)
isolated_ids <- names(deg[deg == 0])
n_isolated   <- length(isolated_ids)

message(glue::glue(
  "Graph: {igraph::vcount(adjacency_graph)} vertices, ",
  "{igraph::ecount(adjacency_graph)} edges, ",
  "{n_isolated} isolated (degree-0) parcels"
))

# ============================================================================
# BUILD EDGE GEOMETRIES
# ============================================================================
# st_point_on_surface() guarantees the anchor point lies within the polygon,
# unlike st_centroid() which can fall outside for concave or irregular lots.
# This prevents edge lines from appearing to start/end in empty space.

message("Building edge geometries (using point-on-surface anchors)...")

loc_idx <- stats::setNames(seq_len(nrow(parcel_sf)), parcel_sf$LOC_ID)

# Suppress the "point may not be on surface" warning that can fire for
# certain degenerate geometries — st_make_valid() handles those cases.
surface_pts <- suppressWarnings(
  sf::st_point_on_surface(sf::st_make_valid(sf::st_geometry(parcel_sf)))
)
coords <- sf::st_coordinates(surface_pts)

from_idx <- loc_idx[el$from]
to_idx   <- loc_idx[el$to]
valid    <- !is.na(from_idx) & !is.na(to_idx)

el_valid       <- el[valid, ]
from_idx_valid <- from_idx[valid]
to_idx_valid   <- to_idx[valid]

edge_lines <- lapply(seq_len(nrow(el_valid)), function(i) {
  sf::st_linestring(matrix(
    c(coords[from_idx_valid[i], 1], coords[to_idx_valid[i], 1],
      coords[from_idx_valid[i], 2], coords[to_idx_valid[i], 2]),
    ncol = 2
  ))
})

edges_sfc <- sf::st_sfc(edge_lines, crs = sf::st_crs(parcel_sf))

edges_sf <- sf::st_sf(
  data.frame(
    from            = el_valid$from,
    to              = el_valid$to,
    is_row_crossing = el_valid$is_row_crossing
  ),
  geometry = edges_sfc
)

direct_edges_sf <- edges_sf[!edges_sf$is_row_crossing, ]
row_edges_sf    <- edges_sf[edges_sf$is_row_crossing, ]

n_direct <- nrow(direct_edges_sf)
n_row    <- nrow(row_edges_sf)

# ============================================================================
# CLIP ROW TO DISTRICT BOUNDING BOX
# ============================================================================

row_clipped <- NULL
if (!is.null(row_sf) && nrow(row_sf) > 0) {
  message("Clipping ROW to district extent...")
  district_bbox <- sf::st_as_sfc(sf::st_bbox(parcel_sf))
  row_clipped   <- sf::st_intersection(sf::st_make_valid(row_sf), district_bbox)
  if (nrow(row_clipped) == 0) row_clipped <- NULL
}

# ============================================================================
# ISOLATED PARCEL LAYER
# ============================================================================

isolated_sf     <- parcel_sf[parcel_sf$LOC_ID %in% isolated_ids, ]
non_isolated_sf <- parcel_sf[!parcel_sf$LOC_ID %in% isolated_ids, ]

# ============================================================================
# TRANSFORM TO WGS84 FOR MAPGL
# ============================================================================

message("Transforming to WGS84...")

to_wgs84 <- function(x) sf::st_transform(x, 4326)

non_isolated_wgs <- to_wgs84(non_isolated_sf)
isolated_wgs     <- to_wgs84(isolated_sf)
direct_edges_wgs <- to_wgs84(direct_edges_sf)
row_edges_wgs    <- to_wgs84(row_edges_sf)
row_clipped_wgs  <- if (!is.null(row_clipped)) to_wgs84(row_clipped) else NULL

# Map bounds from full parcel extent
bounds <- as.numeric(sf::st_bbox(to_wgs84(parcel_sf)))

# ============================================================================
# BUILD TOOLTIP COLUMNS
# ============================================================================

# Parcels: show LOC_ID + a few attributes if present
parcel_tooltip_cols <- intersect(
  c("LOC_ID", "capacity", "area", "USE_CODE", "SITE_ADDR"),
  names(non_isolated_wgs)
)

add_tooltip <- function(sf_obj, cols) {
  sf_obj[["tooltip"]] <- apply(
    as.data.frame(sf_obj)[, cols, drop = FALSE], 1,
    function(row) paste(names(row), row, sep = ": ", collapse = "<br>")
  )
  sf_obj
}

non_isolated_wgs <- add_tooltip(non_isolated_wgs, parcel_tooltip_cols)
if (n_isolated > 0) {
  isolated_wgs <- add_tooltip(
    isolated_wgs,
    intersect(parcel_tooltip_cols, names(isolated_wgs))
  )
}

# Edges: show endpoint IDs and crossing type
direct_edges_wgs[["tooltip"]] <- paste0(
  "Direct edge<br>",
  direct_edges_wgs$from, " \u2194 ", direct_edges_wgs$to
)
if (n_row > 0) {
  row_edges_wgs[["tooltip"]] <- paste0(
    "ROW-crossing edge<br>",
    row_edges_wgs$from, " \u2194 ", row_edges_wgs$to
  )
}

# ============================================================================
# BUILD INTERACTIVE MAP
# ============================================================================

message("Building interactive map...")

m <- mapgl::maplibre(
  style  = mapgl::carto_style("positron"),
  bounds = bounds
)

# --- ROW polygons ---
if (!is.null(row_clipped_wgs)) {
  m <- m |>
    mapgl::add_fill_layer(
      id              = "row_fill",
      source          = row_clipped_wgs,
      fill_color      = "#FB8C00",
      fill_opacity    = 0.25
    ) |>
    mapgl::add_line_layer(
      id           = "row_border",
      source       = row_clipped_wgs,
      line_color   = "#FB8C00",
      line_width   = 1,
      line_opacity = 0.7
    )
}

# --- All non-isolated parcels ---
m <- m |>
  mapgl::add_fill_layer(
    id           = "parcels",
    source       = non_isolated_wgs,
    fill_color   = "#B0BEC5",
    fill_opacity = 0.25,
    tooltip      = "tooltip"
  ) |>
  mapgl::add_line_layer(
    id           = "parcel_borders",
    source       = non_isolated_wgs,
    line_color   = "#607D8B",
    line_width   = 0.5,
    line_opacity = 0.6
  )

# --- Isolated parcels ---
if (n_isolated > 0) {
  m <- m |>
    mapgl::add_fill_layer(
      id           = "isolated_parcels",
      source       = isolated_wgs,
      fill_color   = "#7B1FA2",
      fill_opacity = 0.5,
      tooltip      = "tooltip"
    ) |>
    mapgl::add_line_layer(
      id           = "isolated_borders",
      source       = isolated_wgs,
      line_color   = "#4A148C",
      line_width   = 1.5,
      line_opacity = 0.9
    )
}

# --- Direct touching edges ---
m <- m |>
  mapgl::add_line_layer(
    id           = "direct_edges",
    source       = direct_edges_wgs,
    line_color   = "#1565C0",
    line_width   = 1,
    line_opacity = 0.5,
    tooltip      = "tooltip"
  )

# --- ROW-crossing edges ---
if (n_row > 0) {
  m <- m |>
    mapgl::add_line_layer(
      id           = "row_edges",
      source       = row_edges_wgs,
      line_color   = "#C62828",
      line_width   = 2,
      line_opacity = 0.85,
      line_dasharray = c(4, 2),
      tooltip      = "tooltip"
    )
}

# --- Controls & legend ---
legend_values <- c("Parcel", "Direct edge")
legend_colors <- c("#B0BEC5", "#1565C0")

if (!is.null(row_clipped_wgs)) {
  legend_values <- c("Right-of-Way", legend_values)
  legend_colors <- c("#FB8C00",      legend_colors)
}
if (n_row > 0) {
  legend_values <- c(legend_values, "ROW-crossing edge")
  legend_colors <- c(legend_colors, "#C62828")
}
if (n_isolated > 0) {
  legend_values <- c(legend_values, "Isolated parcel")
  legend_colors <- c(legend_colors, "#7B1FA2")
}

m <- m |>
  mapgl::add_navigation_control(position = "top-left") |>
  mapgl::add_fullscreen_control(position = "top-left") |>
  mapgl::add_categorical_legend(
    legend_title = glue::glue(
      "{district_name} — Adjacency Graph | ",
      "{igraph::vcount(adjacency_graph)} parcels, ",
      "{n_direct} direct edges, ",
      "{n_row} ROW edges, ",
      "{n_isolated} isolated"
    ),
    values = legend_values,
    colors = legend_colors,
    position = "bottom-left"
  )

# ============================================================================
# SAVE HTML
# ============================================================================

out_dir <- "dev/mcmc_testing/plots"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_file <- file.path(
  out_dir,
  glue::glue("adjacency_diagnostic_{gsub(' ', '_', district_name)}.html")
)

htmlwidgets::saveWidget(
  widget    = m,
  file      = normalizePath(out_file, mustWork = FALSE),
  selfcontained = TRUE,  # single portable HTML file, no external deps
  title     = glue::glue("Adjacency Diagnostic — {district_name}")
)

message(glue::glue("Interactive map saved to: {out_file}"))

print(m)

# ============================================================================
# DIAGNOSTICS OBJECT
# ============================================================================
# Assigned to the calling environment via <<-. No additional files written.

iso_detail          <- as.data.frame(isolated_sf)
iso_detail$geometry <- NULL

adjacency_diagnostics <<- list(
  district        = district_name,
  n_parcels       = igraph::vcount(adjacency_graph),
  n_edges_direct  = n_direct,
  n_edges_row     = n_row,
  n_isolated      = n_isolated,
  isolated_ids    = isolated_ids,
  isolated_detail = iso_detail  # full attribute table, no geometry
)

if (n_isolated > 0) {
  message(glue::glue(
    "\n--- {n_isolated} isolated parcel(s) — ",
    "see `adjacency_diagnostics$isolated_detail` ---"
  ))
  keep_cols <- intersect(
    c("LOC_ID", "area", "capacity", "USE_CODE", "SITE_ADDR"),
    names(iso_detail)
  )
  print(iso_detail[, keep_cols, drop = FALSE])
} else {
  message("No isolated parcels.")
}

invisible(m)
