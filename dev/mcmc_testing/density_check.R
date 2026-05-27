library(data.table)
library(targets)
library(mbtazone)

districts <- fread("~/code/mbtazone/inst/extdata/community_info.csv")

feasibility_results <- vector("list", nrow(districts))

for (i in seq_len(nrow(districts))) {
  name  <- districts$community_name[i]
  type  <- districts$community_type[i]
  store <- paste0("ext/_targets_", gsub(" ", "_", name))

  cat(sprintf("\n[%d/%d] %s (%s)... ", i, nrow(districts), name, type))

  Sys.setenv(DISTRICT_NAME = name, DISTRICT_TYPE = type)

  if (name == "Lowell") { # Temporary skip for Lowell
    cat(sprintf("\n[%d/%d] Lowell — skipped (known hang)\n", i, nrow(districts)))
    feasibility_results[[i]] <- data.table(
      community_name            = name,
      community_type            = type,
      load_error                = "skipped — adjacency graph hangs",
      total_capacity            = NA_real_,
      min_capacity              = NA_real_,
      pass_capacity             = NA,
      total_area                = NA_real_,
      min_area                  = NA_real_,
      pass_area                 = NA,
      max_density               = NA_real_,
      mean_density              = NA_real_,
      pct_parcels_above_density = NA_real_,
      min_density               = NA_real_,
      pass_density              = NA,
      feasible                  = FALSE
    )
    next
  }

  # Run just through parcel_graph_result
  preflight_ok <- tryCatch({
    tar_make(
      names  = c("district_paths", "district_data", "adjacency_graph",
                   "constraints", "parcel_graph_result"),
      script   = "inst/targets/_targets.R",
      store    = store,
      reporter = "silent"
    )
    TRUE
  }, error = function(e) {
    feasibility_results[[i]] <<- data.table(
      community_name            = name,
      community_type            = type,
      load_error                = conditionMessage(e),
      total_capacity            = NA_real_,
      min_capacity              = NA_real_,
      pass_capacity             = NA,
      total_area                = NA_real_,
      min_area                  = NA_real_,
      pass_area                 = NA,
      max_density               = NA_real_,
      mean_density              = NA_real_,
      pct_parcels_above_density = NA_real_,
      min_density               = NA_real_,
      pass_density              = NA,
      feasible                  = FALSE
    )
    FALSE
  })

  if (!preflight_ok) {
    cat("LOAD ERROR\n")
    next
  }

  # Compute checks from stored targets
  tryCatch({
    pg  <- tar_read(parcel_graph_result, store = store)$parcel_graph
    con <- tar_read(constraints,         store = store)

    cap     <- igraph::V(pg)$capacity
    area    <- igraph::V(pg)$area
    density <- cap / area

    total_cap    <- sum(cap,  na.rm = TRUE)
    total_area   <- sum(area, na.rm = TRUE)
    max_density  <- max(density, na.rm = TRUE)
    mean_density <- total_cap / total_area
    pct_above    <- 100 * mean(density >= con$min_density, na.rm = TRUE)

    pass_cap     <- total_cap  >= con$min_capacity
    pass_area    <- total_area >= con$min_area
    pass_density <- max_density >= con$min_density
    feasible     <- pass_cap && pass_area && pass_density

    feasibility_results[[i]] <- data.table(
      community_name            = name,
      community_type            = type,
      load_error                = NA_character_,
      total_capacity            = round(total_cap),
      min_capacity              = con$min_capacity,
      pass_capacity             = pass_cap,
      total_area                = round(total_area, 1),
      min_area                  = con$min_area,
      pass_area                 = pass_area,
      max_density               = round(max_density, 2),
      mean_density              = round(mean_density, 2),
      pct_parcels_above_density = round(pct_above, 1),
      min_density               = con$min_density,
      pass_density              = pass_density,
      feasible                  = feasible
    )

    cat(if (feasible) "OK\n" else sprintf("FAIL (%s)\n", paste(c(
      if (!pass_cap)     sprintf("cap %d<%d",       round(total_cap),  con$min_capacity),
      if (!pass_area)    sprintf("area %.1f<%.1f",   total_area,        con$min_area),
      if (!pass_density) sprintf("density %.2f<%.2f", max_density,      con$min_density)
    ), collapse = ", ")))

  }, error = function(e) {
    feasibility_results[[i]] <<- data.table(
      community_name            = name,
      community_type            = type,
      load_error                = conditionMessage(e),
      total_capacity            = NA_real_,
      min_capacity              = NA_real_,
      pass_capacity             = NA,
      total_area                = NA_real_,
      min_area                  = NA_real_,
      pass_area                 = NA,
      max_density               = NA_real_,
      mean_density              = NA_real_,
      pct_parcels_above_density = NA_real_,
      min_density               = NA_real_,
      pass_density              = NA,
      feasible                  = FALSE
    )
    cat(sprintf("CHECK ERROR: %s\n", conditionMessage(e)))
  })
}

Sys.unsetenv("DISTRICT_NAME")
Sys.unsetenv("DISTRICT_TYPE")

# Write and summarise
feasibility_dt <- data.table::rbindlist(
  Filter(Negate(is.null), feasibility_results), fill = TRUE
)

fwrite(feasibility_dt, "~/code/mbtazone/ext/district_feasibility.csv")

cat("\n\n=== SUMMARY ===\n")
print(feasibility_dt[, .(community_name, community_type, feasible,
                         pass_capacity, pass_area, pass_density,
                         pct_parcels_above_density)])
cat(sprintf("\nFeasible:   %d / %d\n", sum(feasibility_dt$feasible,  na.rm = TRUE), nrow(districts)))
cat(sprintf("Infeasible: %d / %d\n",  sum(!feasibility_dt$feasible, na.rm = TRUE), nrow(districts)))
cat(sprintf("Errors:     %d / %d\n",  sum(!is.na(feasibility_dt$load_error)), nrow(districts)))
