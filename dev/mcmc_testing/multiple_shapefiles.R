library(data.table)
library(sf)

data_root <- "/home/k.conyngham/new_data"
district_shp_dir <- file.path(data_root, "mbta_district_shapefiles")

# Find all districts with multiple .shp files
all_districts <- list.dirs(district_shp_dir, recursive = FALSE, full.names = FALSE)

multi_shp_districts <- lapply(all_districts, function(district_name) {
  shp_files <- list.files(
    file.path(district_shp_dir, district_name),
    pattern = "\\.shp$",
    full.names = TRUE
  )
  if (length(shp_files) > 1) {
    list(district = district_name, shp_files = shp_files)
  }
}) |> Filter(Negate(is.null), x = _)

cat(sprintf("Found %d districts with multiple .shp files\n\n", length(multi_shp_districts)))

# Diagnose each one
diagnose_results <- lapply(multi_shp_districts, function(item) {
  district_name <- item$district
  shp_files <- item$shp_files

  cat(sprintf("=== %s (%d files) ===\n", district_name, length(shp_files)))

  # Read all shapefiles
  layers <- lapply(shp_files, function(f) {
    tryCatch(st_read(f, quiet = TRUE), error = function(e) {
      cat(sprintf("  ✗ Could not read %s: %s\n", basename(f), conditionMessage(e)))
      NULL
    })
  })

  valid <- !sapply(layers, is.null)
  layers <- layers[valid]
  shp_files_valid <- shp_files[valid]

  if (length(layers) < 2) {
    cat("  Could not read enough files to compare.\n\n")
    return(data.table(district = district_name, verdict = "unreadable", notes = "read error"))
  }

  # Compare CRS
  crs_list <- sapply(layers, function(l) st_crs(l)$input)
  crs_match <- length(unique(crs_list)) == 1

  # Compare column names
  col_list <- lapply(layers, function(l) sort(names(l)[names(l) != "geometry"]))
  cols_match <- length(unique(lapply(col_list, paste, collapse = ","))) == 1

  # Bounding boxes — do they overlap or are they spatially distinct?
  bboxes <- lapply(layers, st_bbox)

  # Check geometry overlap between all pairs
  # Union each layer to a single geometry first for speed
  unioned <- lapply(layers, function(l) {
    tryCatch(st_union(l), error = function(e) NULL)
  })

  overlap_matrix <- matrix(NA, length(layers), length(layers))
  for (a in seq_along(unioned)) {
    for (b in seq_along(unioned)) {
      if (a >= b || is.null(unioned[[a]]) || is.null(unioned[[b]])) next
      tryCatch({
        intersection_area <- st_area(st_intersection(unioned[[a]], unioned[[b]]))
        area_a <- st_area(unioned[[a]])
        overlap_pct <- as.numeric(intersection_area / area_a * 100)
        overlap_matrix[a, b] <- overlap_pct
      }, error = function(e) { overlap_matrix[a, b] <<- NA })
    }
  }

  max_overlap <- max(overlap_matrix, na.rm = TRUE)

  # Row counts and feature counts per file
  for (j in seq_along(shp_files_valid)) {
    l <- layers[[j]]
    bbox <- bboxes[[j]]
    cat(sprintf("  [%d] %s\n", j, basename(shp_files_valid[[j]])))
    cat(sprintf("      Features: %d | Cols: %s\n", nrow(l), paste(names(l)[names(l) != "geometry"], collapse = ", ")))
    cat(sprintf("      CRS: %s\n", crs_list[[j]]))
    cat(sprintf("      BBox: xmin=%.4f ymin=%.4f xmax=%.4f ymax=%.4f\n",
                bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))
  }

  cat(sprintf("  CRS match: %s | Column match: %s | Max spatial overlap: %.1f%%\n",
              crs_match, cols_match, max_overlap))

  # Verdict
  verdict <- if (max_overlap > 90) {
    "likely_duplicate"      # Heavily overlapping → same area repeated
  } else if (max_overlap < 5) {
    "likely_multipart"      # Spatially distinct → separate parts of district
  } else {
    "ambiguous"             # Partial overlap → needs manual review
  }

  cat(sprintf("  → VERDICT: %s\n\n", verdict))

  data.table(
    district       = district_name,
    n_files        = length(shp_files),
    crs_match      = crs_match,
    cols_match     = cols_match,
    max_overlap_pct = round(max_overlap, 1),
    verdict        = verdict,
    files          = paste(basename(shp_files_valid), collapse = " | ")
  )
})

results_dt <- rbindlist(diagnose_results)

cat("\n=== SUMMARY ===\n")
print(results_dt[, .(district, n_files, max_overlap_pct, verdict)])

cat(sprintf("\nLikely multipart:  %d\n", sum(results_dt$verdict == "likely_multipart")))
cat(sprintf("Likely duplicate:  %d\n", sum(results_dt$verdict == "likely_duplicate")))
cat(sprintf("Ambiguous:         %d\n", sum(results_dt$verdict == "ambiguous")))

fwrite(results_dt, file.path(data_root, "multi_shp_diagnosis.csv"))
cat("\nFull results written to data/multi_shp_diagnosis.csv\n")
