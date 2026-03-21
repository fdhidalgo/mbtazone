# grant_peabody.R — Pipeline runner and analysis for Peabody grant application
#
# Interactive usage (source from package root):
#   source("dev/grant_peabody.R")
#
#   # Run MCMC pipeline (invalidate chains, reuse cached discovery)
#   run_peabody()
#
#   # Full rebuild from scratch
#   run_peabody(mode = "full")
#
#   # Generate adopted plan map
#   grant_adopted_plan("Peabody", store = PEABODY_STORE)
#
#   # Compute capacity metrics
#   adopted <- compute_adopted_plan_metrics(store = PEABODY_STORE, city_name = "Peabody")
#   mcmc    <- compute_net_capacity_distribution(store = PEABODY_STORE, city_name = "Peabody")

# ============================================================================
# SETUP & CONFIG
# ============================================================================

suppressPackageStartupMessages({
  library(targets)
  library(tmap)
  library(sf)
  library(scales)
  library(arrow)
  library(data.table)
})

if (!requireNamespace("mbtazone", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Install devtools or load mbtazone (devtools::load_all() at package root).", call. = FALSE)
  }
  suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
}

PEABODY_STORE <- "ext/_targets_Peabody"
PEABODY_TYPE  <- "adjacent"

RESIDENSITY_PATH <- file.path(
  "/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data",
  "ioc_mbta_communities_hackathon/residensity_mbta_communities.parquet"
)

# ============================================================================
# PIPELINE HELPERS
# ============================================================================

install_package <- function() {
  cat("Installing package...\n")
  r_bin <- file.path(R.home("bin"), "R")
  status <- system2(
    r_bin,
    c("CMD", "INSTALL", "--no-multiarch", shQuote(normalizePath("."))),
    stdout = FALSE, stderr = FALSE
  )
  if (!identical(status, 0L)) stop("Failed to install mbtazone package.")
  cat("Package installed.\n")
  invisible(TRUE)
}

#' Run the MCMC pipeline for Peabody.
#'
#' @param mode "chains" (default) invalidates chain results only;
#'   "full" destroys entire store for a full rebuild.
#' @param install If TRUE (default), installs the package first so crew
#'   workers see current code.
run_peabody <- function(mode = "chains", install = TRUE) {
  if (install) install_package()

  # Clear stale lock
  lf <- file.path(PEABODY_STORE, "meta/process")
  if (file.exists(lf)) file.remove(lf)

  # Invalidate or destroy targets

if (mode == "full") {
    tryCatch(tar_destroy(store = PEABODY_STORE), error = function(e) NULL)
    cat("Peabody: store destroyed (full rebuild)\n")
  } else {
    tryCatch(
      tar_delete(
        names = c("parcel_chain_results", "all_parcel_chain_results"),
        store = PEABODY_STORE
      ),
      error = function(e) NULL
    )
    cat("Peabody: chain results invalidated\n")
  }

  # Run pipeline
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat(" Peabody (", PEABODY_TYPE, ")\n")
  cat(" Time:", format(Sys.time()), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  start_time <- Sys.time()
  Sys.setenv(DISTRICT_NAME = "Peabody", DISTRICT_TYPE = PEABODY_TYPE)
  on.exit({
    Sys.unsetenv("DISTRICT_NAME")
    Sys.unsetenv("DISTRICT_TYPE")
  })

  tar_make(store = PEABODY_STORE, script = "inst/targets/_targets.R")

  elapsed <- difftime(Sys.time(), start_time, units = "mins")
  cat(sprintf("\nCompleted in %.1f minutes\n", elapsed))
  invisible(TRUE)
}

# ============================================================================
# PLOT & ANALYSIS FUNCTIONS
# ============================================================================

# Parcel footprint outline (union of parcels).
parcel_union_outline <- function(parcels_sf) {
  u <- sf::st_union(sf::st_geometry(parcels_sf))
  sf::st_as_sf(sf::st_sfc(u, crs = sf::st_crs(parcels_sf)))
}

# Detect station buffers that intersect the municipal parcel footprint.
find_station_buffers <- function(muni_sf, max_buffers = 4L) {
  all <- mbtazone::load_transit_stations()
  parts <- suppressWarnings(sf::st_cast(sf::st_make_valid(all), "POLYGON"))
  muni_sf <- sf::st_make_valid(muni_sf)
  hit <- lengths(sf::st_intersects(parts, muni_sf)) > 0L
  parts <- parts[hit, , drop = FALSE]
  if (nrow(parts) == 0L) return(parts)
  if (nrow(parts) <= max_buffers) return(parts)
  overlap <- vapply(seq_len(nrow(parts)), function(i) {
    as.numeric(sf::st_area(suppressWarnings(sf::st_intersection(parts[i, ], muni_sf))))
  }, numeric(1))
  parts[order(overlap, decreasing = TRUE)[seq_len(max_buffers)], , drop = FALSE]
}

# Build parcel-level lookup: LOC_ID -> capacity, baseline_hu, net metrics.
build_parcel_lookup <- function(district_parcels, residensity_path, city_name) {
  cap_lookup <- district_parcels[, .(LOC_ID, capacity)]

  resid <- arrow::read_parquet(residensity_path)
  data.table::setDT(resid)
  hu_lookup <- resid[City == city_name, .(LOC_ID, baseline_hu = residentialunits)]
  hu_lookup[is.na(baseline_hu), baseline_hu := 0]

  parcel_lookup <- merge(cap_lookup, hu_lookup, by = "LOC_ID", all.x = TRUE)
  parcel_lookup[is.na(baseline_hu), baseline_hu := 0]
  parcel_lookup[, net_per_parcel := pmax(0, capacity - baseline_hu)]
  parcel_lookup[, net_vacant := fifelse(baseline_hu == 0, capacity, 0)]
  data.table::setkey(parcel_lookup, "LOC_ID")

  parcel_lookup
}

# Summarize a set of plan LOC_IDs against the parcel lookup.
summarize_plan <- function(plan_locs, parcel_lookup) {
  matched <- parcel_lookup[.(plan_locs), nomatch = NULL]
  list(
    total_capacity  = sum(matched$capacity, na.rm = TRUE),
    net_capacity    = sum(matched$net_per_parcel, na.rm = TRUE),
    net_vacant      = sum(matched$net_vacant, na.rm = TRUE),
    baseline_hu     = sum(matched$baseline_hu, na.rm = TRUE),
    n_parcels       = nrow(matched),
    n_vacant        = sum(matched$baseline_hu == 0),
    n_with_hu       = sum(matched$baseline_hu > 0)
  )
}

# Compute metrics for the adopted plan.
compute_adopted_plan_metrics <- function(
    store = PEABODY_STORE,
    residensity_path = RESIDENSITY_PATH,
    city_name = "Peabody") {
  district_data <- targets::tar_read("district_data", store = store)
  dp <- district_data$district_parcels
  parcel_lookup <- build_parcel_lookup(dp, residensity_path, city_name)

  adopted_locs <- dp[in_district == TRUE, LOC_ID]
  metrics <- summarize_plan(adopted_locs, parcel_lookup)
  as.data.table(c(list(plan = "adopted"), metrics))
}

# Compute net capacity distribution from MCMC chain samples.
compute_net_capacity_distribution <- function(
    store = PEABODY_STORE,
    residensity_path = RESIDENSITY_PATH,
    city_name = "Peabody",
    burn_in_frac = 0.5) {
  if (!dir.exists(store)) {
    stop("Missing targets store: ", store, call. = FALSE)
  }

  district_data <- targets::tar_read("district_data", store = store)
  parcel_graph_result <- targets::tar_read("parcel_graph_result", store = store)
  chain_results <- targets::tar_read("all_parcel_chain_results", store = store)

  dp <- district_data$district_parcels
  pa <- parcel_graph_result$parcel_assignments
  data.table::setDT(pa)
  data.table::setkey(pa, "unit_id")

  parcel_lookup <- build_parcel_lookup(dp, residensity_path, city_name)

  valid_chains <- Filter(
    function(x) !isTRUE(x$initialization_failed) && !is.null(x$parcel_samples),
    chain_results
  )
  if (length(valid_chains) == 0L) {
    stop("No valid chains with samples in ", store, call. = FALSE)
  }

  data.table::rbindlist(lapply(names(valid_chains), function(cname) {
    samples <- valid_chains[[cname]]$parcel_samples
    n <- length(samples)
    burn_in_end <- floor(n * burn_in_frac)
    if (burn_in_end >= n) return(NULL)
    post_burn <- seq(burn_in_end + 1L, n)

    data.table::rbindlist(lapply(post_burn, function(i) {
      state <- samples[[i]]
      if (is.null(state)) return(NULL)

      plan_locs <- pa[.(state$X), unique(parcel_id), nomatch = NULL]
      metrics <- summarize_plan(plan_locs, parcel_lookup)

      as.data.table(c(list(chain = cname, sample_index = i), metrics))
    }))
  }))
}

# Adopted plan map: inclusion-colored parcels, station buffers, capacity annotation.
grant_adopted_plan <- function(
    community_name = "Peabody",
    store = PEABODY_STORE,
    out_path = NULL,
    station_labels = NULL,
    preview = interactive(),
    save = !interactive(),
    width = 6,
    height = 8,
    dpi = 300) {
  if (!file.exists("DESCRIPTION")) {
    stop("Run from package root (directory containing DESCRIPTION).", call. = FALSE)
  }
  if (!dir.exists(store)) {
    stop("Missing targets store: ", store, call. = FALSE)
  }
  if (is.null(out_path)) {
    out_path <- file.path("ext/figures",
      paste0("grant_", tolower(community_name), "_adopted.png"))
  }

  district_data <- targets::tar_read("district_data", store = store)

  parcels <- district_data$district_geometry
  dp <- district_data$district_parcels
  parcels$in_district <- dp$in_district[match(parcels$LOC_ID, dp$LOC_ID)]
  parcels$status <- factor(
    ifelse(parcels$in_district, "In overlay district", "Not in overlay"),
    levels = c("In overlay district", "Not in overlay")
  )

  parcels <- sf::st_simplify(parcels, dTolerance = 8)
  muni <- parcel_union_outline(parcels)

  n_labels <- if (!is.null(station_labels)) length(station_labels) else 4L
  stations <- find_station_buffers(muni, max_buffers = n_labels)
  stations <- sf::st_simplify(stations, dTolerance = 8)
  boundary <- sf::st_simplify(district_data$district_boundary, dTolerance = 8)

  total_units <- sum(dp$capacity[dp$in_district %in% TRUE], na.rm = TRUE)
  cap_txt <- paste0(comma(round(total_units)), " units zoned capacity")
  has_stations <- nrow(stations) > 0
  credits_txt <- cap_txt
  if (has_stations) {
    credits_txt <- paste0(credits_txt, "\nShaded ring: MBTA half-mile station area")
  }

  has_labels <- !is.null(station_labels) && has_stations
  if (has_labels) {
    pts <- sf::st_point_on_surface(sf::st_geometry(stations))
    lab <- rep(station_labels, length.out = nrow(stations))
    station_pts <- sf::st_sf(label = lab, geometry = pts, crs = sf::st_crs(stations))
  }

  suppressMessages(tmap_mode("plot"))

  parcel_fill_in <- "#FEE391"
  parcel_fill_out <- "#D5D5D0"
  map <- tm_shape(muni) +
    tm_borders(col = "gray35", lwd = 0.4)

  if (has_stations) {
    map <- map +
      tm_shape(stations) +
      tm_polygons(
        fill = "#9ECAE1", fill_alpha = 0.35,
        col = "steelblue", lwd = 0.6
      )
  }

  map <- map +
    tm_shape(parcels) +
    tm_polygons(
      fill = "status",
      fill.scale = tm_scale_categorical(values = c(parcel_fill_in, parcel_fill_out)),
      col = "gray60", lwd = 0.08,
      fill.legend = tm_legend("Parcel inclusion")
    ) +
    tm_shape(boundary) +
    tm_borders(col = "black", lwd = 0.8)

  if (has_stations) {
    map <- map +
      tm_shape(stations) +
      tm_borders(col = "steelblue", lwd = 1)
  }

  if (has_labels) {
    map <- map +
      tm_shape(station_pts) +
      tm_text(text = "label", size = 0.85, col = "gray15")
  }

  map <- map +
    tm_title(paste0(community_name, " \u2014 Adopted Plan"),
             position = c("left", "top"), size = 1.2) +
    tm_layout(
      frame = FALSE,
      inner.margins = c(0.02, 0.02, 0.06, 0.02),
      legend.position = c("left", "bottom"),
      legend.bg.color = "white",
      legend.bg.alpha = 0.8
    ) +
    tm_credits(credits_txt, position = c("right", "bottom"))

  if (isTRUE(preview)) print(map)

  if (isTRUE(save) && !is.null(out_path)) {
    dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
    tmap_save(map, out_path, width = width, height = height, units = "in", dpi = dpi)
  }

  invisible(map)
}

# ============================================================================
# INTERACTIVE USAGE EXAMPLES
# ============================================================================
if (FALSE) {
  # Run the MCMC pipeline
  run_peabody()
  run_peabody(mode = "full")  # full rebuild

  # Adopted plan map
  grant_adopted_plan("Peabody", store = PEABODY_STORE)

  # Capacity metrics
  adopted <- compute_adopted_plan_metrics()
  mcmc    <- compute_net_capacity_distribution()
}
