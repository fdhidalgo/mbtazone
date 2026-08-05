# diag_row_fill_sweep_all.R
#
# Runs the row_fill_m buffer sweep across all single-zone municipalities and
# aggregates the errors to find the best global value.
#
# For each community × buffer value, computes:
#   pipeline denominator = morphological_close(parcel_union, d) − deductions
# and records the error vs the district denominator and, where available, the
# Excel ground-truth denominator.
#
# Aggregates per buffer value:
#   mae_dist — mean absolute error vs district denominator  (all communities)
#   mae_xl   — mean absolute error vs Excel  (communities with Excel data only,
#              shown for reference only — district denom is the primary target)
#   n_best   — number of communities where this buffer is the best district fit
#
# Run from the mbtazone package root.
# Requires: MBTAZONE_PIPELINE_DATA, MBTAZONE_DENSITY_DEDUCTIONS
# Optional: MBTAZONE_EXCEL_MODELS (without this, only district reference used)

library(sf)
library(readxl)
library(data.table)

# ---- Configuration ----------------------------------------------------------
# Adjacency graph default (from parcel_graph_spec())
ADJ_MAX_DIST_FT <- 120
FEET_TO_M       <- 0.3048
adj_equiv_m     <- ADJ_MAX_DIST_FT * FEET_TO_M / 2   # 18.29 m

SWEEP_M <- sort(unique(c(0, 5, 10, 15, adj_equiv_m, 20, 25, 30, 40, 50, 60)))

# ---- Paths ------------------------------------------------------------------
pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA")
density_ded_path  <- Sys.getenv("MBTAZONE_DENSITY_DEDUCTIONS")
excel_models_dir  <- Sys.getenv("MBTAZONE_EXCEL_MODELS")
if (!nzchar(pipeline_data_dir)) stop("MBTAZONE_PIPELINE_DATA not set.")
if (!nzchar(density_ded_path))  stop("MBTAZONE_DENSITY_DEDUCTIONS not set.")
if (!nzchar(excel_models_dir)) {
  message("MBTAZONE_EXCEL_MODELS not set — will use district denominator only.")
}

# ---- Load deductions once ---------------------------------------------------
cat("Loading deductions shapefile (loaded once, clipped per community)...\n")
deductions_all <- sf::st_make_valid(
  sf::st_transform(sf::st_read(density_ded_path, quiet = TRUE), 26986)
)
cat(sprintf("  %d deduction features loaded.\n\n", nrow(deductions_all)))

# ---- Helpers ----------------------------------------------------------------
extract_excel_denom <- function(community_name, models_dir) {
  if (!nzchar(models_dir)) return(NA_real_)
  community_clean <- gsub("_", " ", community_name)
  dirs <- list.dirs(models_dir, full.names = TRUE, recursive = FALSE)
  idx  <- which(tolower(basename(dirs)) == tolower(community_clean))
  if (length(idx) > 0) {
    xlsx_files <- list.files(dirs[idx[1]], pattern = "\\.xlsx$",
                             full.names = TRUE, ignore.case = TRUE)
  } else {
    all_xlsx <- list.files(models_dir, pattern = "\\.xlsx$",
                           full.names = TRUE, ignore.case = TRUE)
    pat <- paste0(
      "^",
      gsub("([.()^$*+?{}|\\[\\]])", "\\\\\\1", community_clean),
      "\\s*-\\s*CM"
    )
    xlsx_files <- all_xlsx[grepl(pat, basename(all_xlsx), ignore.case = TRUE)]
  }
  xlsx_files <- xlsx_files[!grepl("^~\\$", basename(xlsx_files))]
  for (f in xlsx_files) {
    tryCatch({
      if (!"Summary" %in% readxl::excel_sheets(f)) next
      s   <- readxl::read_excel(f, sheet = "Summary", col_names = FALSE,
                                .name_repair = "minimal")
      row <- grep("District Density Denominator", as.character(s[[1]]),
                  ignore.case = TRUE)[1]
      if (!is.na(row)) return(suppressWarnings(as.numeric(s[[2]][row])))
    }, error = function(e) NULL)
  }
  NA_real_
}

clip_deductions <- function(parcels_sf, deductions_sf) {
  bbox_sfc <- sf::st_as_sfc(sf::st_bbox(parcels_sf))
  sf::st_crs(bbox_sfc) <- 26986
  sf::st_make_valid(
    suppressWarnings(
      sf::st_intersection(deductions_sf, sf::st_sf(geometry = bbox_sfc))
    )
  )
}

denom_from_pipeline <- function(parcel_union, d, district_sf, local_ded) {
  if (d > 0) {
    pg <- parcel_union |> sf::st_buffer(d) |> sf::st_buffer(-d)
  } else {
    pg <- parcel_union
  }
  pipe_sf    <- sf::st_sf(geometry = pg)
  pipe_acres <- as.numeric(sf::st_area(pg)) / 4047

  ded_pipe_acres <- 0
  if (nrow(local_ded) > 0) {
    ded_clipped <- suppressWarnings(sf::st_intersection(pipe_sf, local_ded))
    if (nrow(ded_clipped) > 0) {
      ded_pipe_acres <- as.numeric(
        sf::st_area(sf::st_union(ded_clipped))
      ) / 4047
    }
  }

  gap_acres <- tryCatch({
    g <- suppressWarnings(sf::st_difference(district_sf, pipe_sf))
    a <- as.numeric(sf::st_area(sf::st_make_valid(g))) / 4047
    if (is.finite(a) && a >= 0) a else 0
  }, error = function(e) 0)

  list(
    denom      = pipe_acres - ded_pipe_acres,
    pipe_acres = pipe_acres,
    gap_acres  = gap_acres
  )
}

# ---- Discover single-zone communities --------------------------------------
cat("Discovering single-zone GeoPackages...\n")
gpkg_files <- list.files(pipeline_data_dir, pattern = "\\.gpkg$",
                         full.names = TRUE)
cat(sprintf("  %d GeoPackages found.\n", length(gpkg_files)))

single_zone_gpkgs <- Filter(function(f) {
  lyrs <- tryCatch(sf::st_layers(f), error = function(e) NULL)
  if (is.null(lyrs)) return(FALSE)
  n <- lyrs$features[lyrs$name == "districts"]
  length(n) == 1L && !is.na(n) && n == 1L
}, gpkg_files)

community_names <- tools::file_path_sans_ext(basename(single_zone_gpkgs))
cat(sprintf("  %d single-zone communities.\n\n", length(single_zone_gpkgs)))

# ---- Per-community sweep ---------------------------------------------------
cat(sprintf("Sweeping %d buffer values × %d communities...\n\n",
            length(SWEEP_M), length(single_zone_gpkgs)))

all_results <- rbindlist(lapply(seq_along(single_zone_gpkgs), function(i) {
  gpkg    <- single_zone_gpkgs[i]
  cname   <- community_names[i]

  # Load spatial data
  parcels <- tryCatch(
    sf::st_make_valid(sf::st_read(gpkg, layer = "parcels",   quiet = TRUE)),
    error = function(e) { message("  SKIP ", cname, ": ", e$message); NULL }
  )
  if (is.null(parcels)) return(NULL)
  districts <- tryCatch(
    sf::st_make_valid(sf::st_read(gpkg, layer = "districts", quiet = TRUE)),
    error = function(e) NULL
  )
  if (is.null(districts)) return(NULL)

  if (is.na(sf::st_crs(parcels)$epsg) || sf::st_crs(parcels)$epsg != 26986) {
    parcels   <- sf::st_transform(parcels,   26986)
    districts <- sf::st_transform(districts, 26986)
  }

  in_d <- parcels[!is.na(parcels$in_district) & parcels$in_district == TRUE, ]
  if (nrow(in_d) == 0) return(NULL)

  parcel_union  <- sf::st_union(in_d)
  district_poly <- sf::st_union(districts)
  district_sf   <- sf::st_sf(geometry = district_poly)
  polygon_acres <- as.numeric(sf::st_area(district_poly)) / 4047

  # District denominator (reference)
  local_ded <- tryCatch(clip_deductions(parcels, deductions_all),
                        error = function(e) districts[0, ])
  ded_in_dist <- if (nrow(local_ded) > 0)
    suppressWarnings(sf::st_intersection(district_sf, local_ded))
  else local_ded[0, ]
  ded_dist_acres <- if (nrow(ded_in_dist) > 0)
    as.numeric(sf::st_area(sf::st_union(ded_in_dist))) / 4047 else 0
  denom_dist <- polygon_acres - ded_dist_acres

  excel_denom <- extract_excel_denom(cname, excel_models_dir)

  cat(sprintf("  [%d/%d] %-30s  dist=%.1f ac  excel=%s\n",
              i, length(single_zone_gpkgs), cname,
              denom_dist,
              if (is.na(excel_denom)) "n/a" else sprintf("%.1f ac", excel_denom)))

  # Sweep buffers
  rbindlist(lapply(SWEEP_M, function(d) {
    r <- tryCatch(
      denom_from_pipeline(parcel_union, d, district_sf, local_ded),
      error = function(e) list(denom = NA_real_, pipe_acres = NA_real_,
                               gap_acres = NA_real_)
    )
    list(
      community   = cname,
      fill_m      = d,
      denom_dist  = denom_dist,
      denom_xl    = excel_denom,
      denom_pipe  = r$denom,
      pipe_acres  = r$pipe_acres,
      gap_acres   = r$gap_acres,
      err_vs_dist = r$denom - denom_dist,
      err_vs_xl   = if (!is.na(excel_denom)) r$denom - excel_denom else NA_real_
    )
  }))
}))

# ---- Aggregate by buffer value ---------------------------------------------
cat("\n\n=== Aggregate results by row_fill_m ===\n\n")

agg <- all_results[, .(
  n_communities = .N,
  mae_dist      = mean(abs(err_vs_dist), na.rm = TRUE),
  me_dist       = mean(err_vs_dist,      na.rm = TRUE),
  mae_xl        = mean(abs(err_vs_xl),   na.rm = TRUE),
  me_xl         = mean(err_vs_xl,        na.rm = TRUE),
  n_xl          = sum(!is.na(err_vs_xl))
), by = fill_m][order(fill_m)]

# For each community: which buffer gives smallest |err_vs_dist|?
best_per_community <- all_results[,
  .SD[which.min(abs(err_vs_dist)), .(best_fill_m = fill_m)],
  by = community
]

n_best <- best_per_community[, .N, by = best_fill_m]
setnames(n_best, "N", "n_best")
agg <- merge(agg, n_best, by.x = "fill_m", by.y = "best_fill_m", all.x = TRUE)
agg[is.na(n_best), n_best := 0L]

adj_label <- sprintf("%.2f*", adj_equiv_m)
agg[, note := ifelse(abs(fill_m - adj_equiv_m) < 0.01, adj_label, "")]

cat(sprintf(
  "%-8s  %-10s  %-10s  %-10s  %-10s  %-6s  %-6s  note\n",
  "fill_m", "mae_dist", "me_dist", "mae_xl", "me_xl", "n_xl", "n_best"
))
cat(strrep("-", 80), "\n")
for (k in seq_len(nrow(agg))) {
  r <- agg[k]
  cat(sprintf(
    "%-8.2f  %-10.3f  %-10.3f  %-10s  %-10s  %-6d  %-6d  %s\n",
    r$fill_m, r$mae_dist, r$me_dist,
    if (r$n_xl > 0) sprintf("%-10.3f", r$mae_xl) else "    n/a   ",
    if (r$n_xl > 0) sprintf("%-10.3f", r$me_xl)  else "    n/a   ",
    r$n_xl, r$n_best, r$note
  ))
}
cat(sprintf("  * %.2f m = adj graph equiv (max_dist_ft=%d / 2 * %.4f m/ft)\n\n",
            adj_equiv_m, ADJ_MAX_DIST_FT, FEET_TO_M))

# ---- Best overall ----------------------------------------------------------
n_total      <- agg[1L, n_communities]
best_by_dist <- agg[which.min(mae_dist)]
cat(sprintf("Best by mae_dist (n=%d communities): fill_m = %.2f m  (mae=%.3f ac, me=%+.3f ac)\n",
            n_total, best_by_dist$fill_m,
            best_by_dist$mae_dist, best_by_dist$me_dist))
if (any(agg$n_xl > 0)) {
  best_by_xl <- agg[n_xl > 0][which.min(mae_xl)]
  cat(sprintf("Best by mae_xl   (n=%d communities): fill_m = %.2f m  (mae=%.3f ac, me=%+.3f ac)\n",
              best_by_xl$n_xl, best_by_xl$fill_m, best_by_xl$mae_xl, best_by_xl$me_xl))
}

# ---- Per-community best fit summary ----------------------------------------
cat("\n=== Per-community best buffer vs district denominator ===\n\n")
cat(sprintf("%-35s  %-8s  %-10s  %-10s  %-8s  %-8s\n",
            "community", "best_m", "denom_pipe", "denom_dist", "err_dist", "err_xl"))
cat(strrep("-", 90), "\n")
for (cname in best_per_community$community) {
  b   <- best_per_community[community == cname]
  row <- all_results[community == cname & fill_m == b$best_fill_m]
  cat(sprintf("%-35s  %-8.2f  %-10.3f  %-10.3f  %+8.3f  %s\n",
              cname, b$best_fill_m, row$denom_pipe, row$denom_dist,
              row$err_vs_dist,
              if (!is.na(row$err_vs_xl)) sprintf("%+8.3f", row$err_vs_xl) else "     n/a"))
}

# ---- Save full results ------------------------------------------------------
out_path <- "dev/mcmc_testing/density_denominator_all_buffers.csv"
fwrite(all_results, out_path)
cat(sprintf("\nFull results written to %s\n", out_path))
