# Diagnostic: Re-run tree discovery with higher capacity cap (2.5x vs 1.5x)
# to check if larger LCCs would improve low-k feasibility

library(targets)
library(data.table)

# Load package functions
devtools::load_all()

store <- "ext/_targets_Norwood"

# Load existing data from targets
tar_load(parcel_graph_result, store = store)
tar_load(constraints, store = store)

parcel_graph <- parcel_graph_result$parcel_graph
parcel_attrs <- parcel_graph_result$parcel_attributes

min_cap <- constraints$min_capacity  # 2045
req_station_cap <- 0.9 * min_cap     # 1840.5
req_station_area <- 0.9 * constraints$min_area  # 45 acres

cat("=== BASELINE (1.5x cap = ", 1.5 * min_cap, ") ===\n")
cat("Running tree discovery with 2.5x cap =", 2.5 * min_cap, "\n\n")

# Re-run tree discovery with higher cap
set.seed(42)
result_25x <- discover_lccs_from_trees(
  parcel_graph = parcel_graph,
  constraints = constraints,
  n_trees = 500L,
  max_discovery_capacity = 2.5 * min_cap,  # 5112.5 instead of 3067.5
  verbose = TRUE
)

cat("\n=== RESULTS ===\n")
cat("Trees sampled:", result_25x$n_trees_sampled, "\n")
cat("Total cuts found:", result_25x$total_cuts_found, "\n")
cat("Unique LCCs:", result_25x$unique_lccs_found, "\n\n")

lccs <- result_25x$discovered_lccs
cat("Capacity range:", min(lccs$capacity), "-", max(lccs$capacity), "\n")

# Bin by capacity
cat("\n=== CAPACITY DISTRIBUTION ===\n")
breaks <- c(0, 1500, 2000, 2500, 3000, 3500, 4000, 5000, Inf)
lccs[, cap_bin := cut(capacity, breaks = breaks)]
print(lccs[, .N, by = cap_bin][order(cap_bin)])

# Now compute station overlap for all discovered LCCs
# Need to compute capacity_in_station and area_in_station per LCC
cat("\n=== COMPUTING STATION OVERLAP ===\n")

# Get per-parcel station attributes
pa <- parcel_attrs
setkey(pa, unit_id)

# For each LCC, sum up station attributes
compute_station_stats <- function(parcel_ids_list) {
  n <- length(parcel_ids_list)
  cap_in_station <- numeric(n)
  area_in_station <- numeric(n)
  total_area <- numeric(n)

  for (i in seq_len(n)) {
    pids <- parcel_ids_list[[i]]
    rows <- pa[.(pids), nomatch = NULL]
    cap_in_station[i] <- sum(rows$capacity_in_station)
    area_in_station[i] <- sum(rows$area_in_station)
    total_area[i] <- sum(rows$area)
  }

  list(
    capacity_in_station = cap_in_station,
    area_in_station = area_in_station,
    total_area = total_area
  )
}

stats <- compute_station_stats(lccs$parcel_ids)
lccs[, capacity_in_station := stats$capacity_in_station]
lccs[, area_in_station := stats$area_in_station]
lccs[, total_area := stats$total_area]

# Standalone feasibility
lccs[, standalone_feasible := capacity >= min_cap &
       total_area >= constraints$min_area &
       capacity_in_station >= req_station_cap &
       area_in_station >= req_station_area]

cat("\n=== STANDALONE FEASIBILITY BY CAPACITY BAND ===\n")
print(lccs[, .(
  N = .N,
  n_feasible = sum(standalone_feasible),
  pct_feasible = round(100 * mean(standalone_feasible), 1),
  mean_station_cap = round(mean(capacity_in_station)),
  pct_station_cap_ok = round(100 * mean(capacity_in_station >= req_station_cap), 1)
), by = cap_bin][order(cap_bin)])

# Compare: what's NEW above the old 1.5x cap?
cat("\n=== NEW LCCs ABOVE OLD CAP (>3068) ===\n")
new_lccs <- lccs[capacity > 1.5 * min_cap]
cat("Count:", nrow(new_lccs), "\n")
if (nrow(new_lccs) > 0) {
  cat("Capacity range:", min(new_lccs$capacity), "-", max(new_lccs$capacity), "\n")
  cat("Standalone feasible:", sum(new_lccs$standalone_feasible),
      "(", round(100 * mean(new_lccs$standalone_feasible), 1), "%)\n")
  cat("Station cap >= 1841:", sum(new_lccs$capacity_in_station >= req_station_cap),
      "(", round(100 * mean(new_lccs$capacity_in_station >= req_station_cap), 1), "%)\n")
  cat("Station area >= 45:", sum(new_lccs$area_in_station >= req_station_area),
      "(", round(100 * mean(new_lccs$area_in_station >= req_station_area), 1), "%)\n")

  cat("\nCapacity distribution of new LCCs:\n")
  print(new_lccs[, .(
    N = .N,
    n_feasible = sum(standalone_feasible),
    pct_feasible = round(100 * mean(standalone_feasible), 1),
    mean_station_cap = round(mean(capacity_in_station))
  ), by = cut(capacity, breaks = seq(3000, ceiling(max(capacity)/500)*500, 500))][order(cut)])
}

# Summary comparison
cat("\n=== SUMMARY ===\n")
cat("Old cap (1.5x = 3068): standalone feasible in library = 118/3250 (3.6%)\n")
cat("New pool (2.5x = 5113):\n")
cat("  Total unique LCCs:", nrow(lccs), "\n")
cat("  Above old cap:", nrow(new_lccs), "\n")
cat("  Standalone feasible (all):", sum(lccs$standalone_feasible),
    "(", round(100 * mean(lccs$standalone_feasible), 1), "%)\n")
cat("  Standalone feasible (above old cap):", sum(new_lccs$standalone_feasible),
    "(", round(100 * mean(new_lccs$standalone_feasible), 1), "%)\n")
