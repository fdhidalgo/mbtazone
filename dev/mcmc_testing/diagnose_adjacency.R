# diagnose_adjacency.R
#
# Diagnoses why specific parcel pairs are not connected in the adjacency graph.
# For each queried pair, traces through every stage of build_adjacency_graph()
# and reports exactly where and why the connection was dropped.
#
# Usage:
#   # Set store and district_name first, then:
#   pairs_to_check <- list(
#     c("M_224204_879378", "M_224198_879426"),
#     c("M_111111_222222", "M_333333_444444")
#   )
#   source("./dev/mcmc_testing/diagnose_adjacency.R")
#   # Results available in `pair_diagnostics`
#
# Dependencies: targets, sf, data.table, glue, mbtazone

# ============================================================================
# CONFIGURATION
# ============================================================================

if (!exists("store")) {
  stop("Please set `store` before sourcing.\ne.g. store <- 'ext/_targets_Norwood'")
}
if (!exists("pairs_to_check") || length(pairs_to_check) == 0) {
  stop(paste(
    "Please set `pairs_to_check` before sourcing. Example:",
    "pairs_to_check <- list(c('LOC_ID_A', 'LOC_ID_B'))",
    sep = "\n"
  ))
}
if (!exists("district_name")) {
  district_name <- gsub(".*_targets_", "", store)
  message(glue::glue("district_name inferred as '{district_name}'"))
}

# ============================================================================
# LOAD ARTIFACTS
# ============================================================================

message("Reading pipeline artifacts...")
district_data <- targets::tar_read(district_data, store = store)

parcel_sf <- district_data$district_geometry
row_sf    <- district_data$district_right_of_way

FEET_TO_METERS    <- 0.3048
ROW_GAP_TOLERANCE_M <- 1

# Parameters — match whatever build_adjacency_graph() is called with
MAX_DIST_FT        <- 120
TOUCH_THRESHOLD_FT <- 2
MIN_COVERAGE_RATIO <- 0.9

max_dist_m        <- MAX_DIST_FT        * FEET_TO_METERS
touch_threshold_m <- TOUCH_THRESHOLD_FT * FEET_TO_METERS

# ============================================================================
# PRE-COMPUTE SHARED OBJECTS
# ============================================================================

message("Pre-computing ROW union and parcel index...")

loc_idx   <- stats::setNames(seq_len(nrow(parcel_sf)), parcel_sf$LOC_ID)
all_geoms <- sf::st_geometry(parcel_sf)

row_union <- NULL
near_row  <- NULL
if (!is.null(row_sf) && nrow(row_sf) > 0) {
  row_union <- sf::st_union(row_sf)
  near_row  <- sf::st_is_within_distance(
    parcel_sf, row_union,
    dist = ROW_GAP_TOLERANCE_M, sparse = FALSE
  )[, 1]
}

# ============================================================================
# CANDIDATE CHECK — were both parcels even in the same candidate pair?
# ============================================================================

message("Building candidate pair set...")

geom_buf  <- sf::st_buffer(parcel_sf, dist = max_dist_m / 2)
buf_pairs <- sf::st_intersects(geom_buf, sparse = TRUE)

# Store as a hash set of "minID|maxID" strings for O(1) lookup
candidate_set <- local({
  pairs <- data.table::rbindlist(
    lapply(seq_along(buf_pairs), function(i) {
      j <- buf_pairs[[i]]
      j <- j[j > i]
      if (length(j) == 0) return(NULL)
      data.table::data.table(i = i, j = j)
    })
  )
  paste0(pairs$i, "|", pairs$j)
})

message(glue::glue("  {length(candidate_set)} candidate pairs generated"))

# ============================================================================
# DIAGNOSE EACH PAIR
# ============================================================================

diagnose_pair <- function(id_a, id_b) {
  result <- list(
    from   = id_a,
    to     = id_b,
    stages = list()
  )

  add_stage <- function(name, passed, detail = NULL) {
    result$stages[[name]] <<- list(passed = passed, detail = detail)
  }

  # ---- Stage 0: Both parcels exist ----
  idx_a <- loc_idx[id_a]
  idx_b <- loc_idx[id_b]

  if (is.na(idx_a)) {
    add_stage("parcel_exists", FALSE, glue::glue("'{id_a}' not found in geometry"))
    result$verdict <- "MISSING_PARCEL"
    return(result)
  }
  if (is.na(idx_b)) {
    add_stage("parcel_exists", FALSE, glue::glue("'{id_b}' not found in geometry"))
    result$verdict <- "MISSING_PARCEL"
    return(result)
  }
  add_stage("parcel_exists", TRUE)

  # Ensure consistent ordering (lower index first)
  if (idx_a > idx_b) {
    tmp <- idx_a; idx_a <- idx_b; idx_b <- tmp
    tmp <- id_a;  id_a  <- id_b;  id_b  <- tmp
  }

  geom_a <- all_geoms[idx_a]
  geom_b <- all_geoms[idx_b]

  # ---- Stage 1: Candidate generation ----
  key <- paste0(idx_a, "|", idx_b)
  in_candidates <- key %in% candidate_set
  dist_bbox <- as.numeric(sf::st_distance(
    sf::st_as_sfc(sf::st_bbox(geom_a)),
    sf::st_as_sfc(sf::st_bbox(geom_b))
  ))
  add_stage(
    "candidate_generation", in_candidates,
    glue::glue(
      "Buffer overlap check (max_dist_ft={MAX_DIST_FT}). ",
      "Bbox distance ~ {round(dist_bbox / FEET_TO_METERS, 1)} ft"
    )
  )

  if (!in_candidates) {
    # Compute actual boundary distance for context
    actual_dist_m <- as.numeric(sf::st_distance(geom_a, geom_b))
    result$verdict <- "DROPPED_AT_CANDIDATE_GENERATION"
    result$boundary_dist_ft <- round(actual_dist_m / FEET_TO_METERS, 1)
    result$note <- glue::glue(
      "Boundary-to-boundary distance is {result$boundary_dist_ft} ft. ",
      "Increase max_dist_ft above {MAX_DIST_FT} to include this pair."
    )
    return(result)
  }

  # ---- Stage 2: Touch test ----
  boundary_dist_m  <- as.numeric(sf::st_distance(geom_a, geom_b))
  boundary_dist_ft <- boundary_dist_m / FEET_TO_METERS
  is_touching      <- boundary_dist_m <= touch_threshold_m

  add_stage(
    "touch_test", is_touching,
    glue::glue(
      "Boundary distance = {round(boundary_dist_ft, 2)} ft ",
      "(threshold = {TOUCH_THRESHOLD_FT} ft)"
    )
  )

  if (is_touching) {
    result$verdict          <- "CONNECTED_DIRECT_TOUCH"
    result$boundary_dist_ft <- round(boundary_dist_ft, 2)
    return(result)
  }

  result$boundary_dist_ft <- round(boundary_dist_ft, 2)

  # ---- Stage 3: ROW availability ----
  if (is.null(row_union)) {
    add_stage("row_available", FALSE, "No ROW data provided")
    result$verdict <- "DROPPED_NO_ROW_DATA"
    return(result)
  }
  add_stage("row_available", TRUE)

  # ---- Stage 4: Both parcels near ROW ----
  a_near <- near_row[idx_a]
  b_near <- near_row[idx_b]
  both_near <- a_near & b_near

  add_stage(
    "both_near_row", both_near,
    glue::glue(
      "'{id_a}' near ROW: {a_near}  |  '{id_b}' near ROW: {b_near}  ",
      "(tolerance = {ROW_GAP_TOLERANCE_M} m)"
    )
  )

  if (!both_near) {
    result$verdict <- "DROPPED_NOT_NEAR_ROW"
    result$note <- glue::glue(
      "{if (!a_near) id_a else id_b} is more than {ROW_GAP_TOLERANCE_M} m ",
      "from any ROW polygon. The parcel may be interior with no road frontage, ",
      "or there may be a data gap between the parcel and ROW boundaries."
    )
    return(result)
  }

  # ---- Stage 5: Nearest-points line distance cap ----
  np_line     <- sf::st_geometry(sf::st_nearest_points(geom_a, geom_b))[[1]]
  np_length_m <- as.numeric(sf::st_length(sf::st_sfc(np_line, crs = sf::st_crs(parcel_sf))))
  np_length_ft <- np_length_m / FEET_TO_METERS
  within_cap   <- np_length_m <= max_dist_m

  add_stage(
    "distance_cap", within_cap,
    glue::glue(
      "Nearest-points line length = {round(np_length_ft, 1)} ft ",
      "(cap = {MAX_DIST_FT} ft)"
    )
  )

  if (!within_cap) {
    result$verdict <- "DROPPED_DISTANCE_CAP"
    result$np_dist_ft <- round(np_length_ft, 1)
    result$note <- glue::glue(
      "Nearest-points distance ({round(np_length_ft, 1)} ft) exceeds max_dist_ft ",
      "({MAX_DIST_FT} ft). Increase max_dist_ft to include this pair."
    )
    return(result)
  }

  result$np_dist_ft <- round(np_length_ft, 1)

  # ---- Stage 6: ROW intersection ----
  np_sfc       <- sf::st_sfc(np_line, crs = sf::st_crs(parcel_sf))
  crosses_row  <- isTRUE(sf::st_intersects(np_sfc, row_union, sparse = FALSE)[1, 1])

  add_stage(
    "crosses_row", crosses_row,
    if (crosses_row) "Line intersects ROW polygon" else
      "Line does not intersect any ROW polygon — parcels may not face each other across a road"
  )

  if (!crosses_row) {
    result$verdict <- "DROPPED_NO_ROW_INTERSECTION"
    result$note <- paste(
      "The nearest-points line between these parcels does not cross any ROW polygon.",
      "Possible causes: parcels are on the same side of the road; the gap between",
      "them is unclassified land (neither parcel nor ROW); or the ROW dataset has",
      "a gap at this location."
    )
    return(result)
  }

  # ---- Stage 7: Coverage ratio ----
  row_int    <- sf::st_intersection(np_sfc, row_union)
  row_len_m  <- sum(as.numeric(sf::st_length(row_int)))
  coverage   <- row_len_m / np_length_m
  passes_cov <- coverage >= MIN_COVERAGE_RATIO

  add_stage(
    "coverage_ratio", passes_cov,
    glue::glue(
      "ROW coverage = {round(coverage * 100, 1)}% ",
      "(threshold = {MIN_COVERAGE_RATIO * 100}%)  |  ",
      "ROW crossing = {round(row_len_m / FEET_TO_METERS, 1)} ft of ",
      "{round(np_length_ft, 1)} ft total"
    )
  )

  if (!passes_cov) {
    result$verdict  <- "DROPPED_COVERAGE_RATIO"
    result$coverage <- round(coverage, 3)
    result$note <- glue::glue(
      "ROW coverage ({round(coverage * 100, 1)}%) is below the threshold ",
      "({MIN_COVERAGE_RATIO * 100}%). The nearest-points line crosses the ROW ",
      "but spends too much of its length outside it — likely same-side parcels ",
      "or a line clipping a corner of the ROW polygon. ",
      "Decrease min_coverage_ratio to include this pair."
    )
    return(result)
  }

  result$verdict  <- "CONNECTED_ROW_CROSSING"
  result$coverage <- round(coverage, 3)
  result
}

# ============================================================================
# RUN DIAGNOSTICS
# ============================================================================

message(glue::glue("\nDiagnosing {length(pairs_to_check)} pair(s)...\n"))

pair_diagnostics <- lapply(pairs_to_check, function(pair) {
  id_a <- pair[1]
  id_b <- pair[2]
  message(glue::glue("--- {id_a}  <-->  {id_b} ---"))

  d <- diagnose_pair(id_a, id_b)

  # Print summary
  message(glue::glue("Verdict: {d$verdict}"))
  for (nm in names(d$stages)) {
    s      <- d$stages[[nm]]
    symbol <- if (isTRUE(s$passed)) cli::col_green("\u2713") else cli::col_red("\u2717")
    detail <- if (!is.null(s$detail)) glue::glue(" — {s$detail}") else ""
    message(glue::glue("  {symbol} {nm}{detail}"))
  }
  if (!is.null(d$note)) message(glue::glue("  Note: {d$note}"))
  message("")

  d
})

names(pair_diagnostics) <- vapply(pairs_to_check, function(p) {
  paste(p, collapse = " <-> ")
}, character(1))

# ============================================================================
# SUMMARY TABLE
# ============================================================================

summary_dt <- data.table::data.table(
  from             = vapply(pair_diagnostics, `[[`, character(1), "from"),
  to               = vapply(pair_diagnostics, `[[`, character(1), "to"),
  verdict          = vapply(pair_diagnostics, `[[`, character(1), "verdict"),
  boundary_dist_ft = vapply(pair_diagnostics, function(d) {
    as.numeric(d$boundary_dist_ft %||% NA_real_)
  }, numeric(1)),
  np_dist_ft       = vapply(pair_diagnostics, function(d) {
    as.numeric(d$np_dist_ft %||% NA_real_)
  }, numeric(1)),
  coverage         = vapply(pair_diagnostics, function(d) {
    as.numeric(d$coverage %||% NA_real_)
  }, numeric(1))
)

message("=== Summary ===")
print(summary_dt)

message(glue::glue(
  "\nFull diagnostics available in `pair_diagnostics`"
))

`%||%` <- function(x, y) if (is.null(x)) y else x

pair_diagnostics <<- pair_diagnostics
summary_dt       <<- summary_dt

invisible(pair_diagnostics)
