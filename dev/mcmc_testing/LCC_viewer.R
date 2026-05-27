library(mapgl)
library(sf)
library(data.table)

# set store with run_single_district.R first
district_data <- tar_read(district_data, store = store)
parcel_graph_result <- tar_read(parcel_graph_result, store = store)
combined_discovered_lccs <- tar_read(combined_discovered_lccs, store = store)
# ── Configuration ─────────────────────────────────────────────────────────────
# Set these to match your environment:
parcel_sf    <- district_data$district_geometry
parcel_id_col <- "LOC_ID"          # column name in parcel_sf that holds parcel IDs

# ── Helper: look up a single LCC by index or lcc_key ─────────────────────────
get_lcc <- function(combined, index = NULL, key = NULL) {
  blocks <- combined$discovered_blocks
  if (!is.null(index)) {
    row <- blocks[index, ]
  } else if (!is.null(key)) {
    row <- blocks[lcc_key == key, ]
  } else {
    stop("Provide either index or key")
  }
  row
}

make_hull <- function(sf_obj) {
  sf_obj |>
    sf::st_union() |>
    sf::st_convex_hull() |>
    sf::st_as_sf()
}

# Helper: resolve unit_ids → parcel_ids → geometry ─────────────────
lcc_to_sf <- function(lcc_row, parcel_sf, parcel_assignments,
                      parcel_id_col = "LOC_ID") {
  unit_ids   <- lcc_row$parcel_ids[[1]]

  # Translate M-prefixed unit IDs → F-prefixed parcel IDs
  parcel_ids <- parcel_assignments[unit_id %in% unit_ids, parcel_id]

  matched <- parcel_sf[parcel_sf[[parcel_id_col]] %in% parcel_ids, ]

  if (nrow(matched) == 0) stop("No parcels matched after ID translation.")

  matched$lcc_key  <- lcc_row$lcc_key
  matched$capacity <- lcc_row$capacity
  matched$area     <- lcc_row$area
  matched$source   <- lcc_row$source
  matched
}

# ── Updated map_lcc to pass parcel_assignments through ───────────────────────
map_lcc <- function(combined, index = NULL, key = NULL,
                    parcel_sf, parcel_assignments,
                    parcel_id_col = "LOC_ID",
                    style = "positron") {

  # Get LCC
  lcc_row <- get_lcc(combined, index = index, key = key)

  # LCC parcels
  parcels_sf <- lcc_to_sf(lcc_row, parcel_sf, parcel_assignments, parcel_id_col)

  parcels_wgs <- sf::st_transform(parcels_sf, 4326)

  # District parcels (reference layer)
  district_wgs <- parcel_sf |>
    sf::st_simplify(dTolerance = 5) |>
    sf::st_transform(4326)

  # Hull
  hull_sf  <- make_hull(parcels_sf)
  hull_wgs <- sf::st_transform(hull_sf, 4326)

  # Map bounds based on LCC area
  bounds <- sf::st_bbox(parcels_wgs)

  cat(sprintf(
    "LCC key: %s | Parcels: %d | Capacity: %s | Area: %.1f | Source: %s\n",
    lcc_row$lcc_key,
    nrow(parcels_sf),
    lcc_row$capacity,
    lcc_row$area,
    lcc_row$source
  ))

  maplibre(style = carto_style(style), bounds = bounds) |>

    # Background district parcels
    add_line_layer(
      id           = "all_parcels",
      source       = district_wgs,
      line_color   = "#999999",
      line_width   = 0.3,
      line_opacity = 0.4
    ) |>

    # Hull fill
    add_fill_layer(
      id           = "hull_fill",
      source       = hull_wgs,
      fill_color   = "#1b9e77",
      fill_opacity = 0.15
    ) |>

    # LCC parcels
    add_fill_layer(
      id           = "parcels",
      source       = parcels_wgs,
      fill_color   = "#1b9e77",
      fill_opacity = 0.75,
      tooltip      = concat(
        "Parcel: ", get_column(parcel_id_col),
        "<br>Capacity: ", get_column("capacity"),
        "<br>Area: ", get_column("area")
      )
    ) |>

    # Hull outline
    add_line_layer(
      id         = "hull_border",
      source     = hull_wgs,
      line_color = "#0d5c49",
      line_width = 1.5
    ) |>

    add_categorical_legend(
      legend_title = paste0("LCC: ", lcc_row$lcc_key),
      values       = c("LCC Parcels"),
      colors       = c("#1b9e77")
    ) |>

    add_fullscreen_control(position = "top-left") |>
    add_navigation_control()
}

# ── Usage ─────────────────────────────────────────────────────────────────────
map_lcc(combined_discovered_lccs, index = 1,
        parcel_sf          = parcel_sf,
        parcel_assignments = parcel_graph_result$parcel_assignments,
        parcel_id_col      = "LOC_ID")
