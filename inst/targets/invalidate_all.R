library(data.table)
library(targets)

# Find all existing district stores
existing_stores <- list.dirs("ext", recursive = FALSE, full.names = TRUE)
district_stores <- existing_stores[grepl("^ext/_targets_", existing_stores)]

cat(sprintf("Found %d existing district stores\n", length(district_stores)))

if (length(district_stores) == 0) {
  cat("No stores found. Nothing to invalidate\n")
} else {
  invalidate_results <- vector("list", length(district_stores))

  for (i in seq_along(district_stores)) {
    store <- district_stores[i]
    cat(sprintf("\n[%d/%d] Invalidating: %s\n", i, length(district_stores), store))

    tryCatch({
      tar_invalidate(
        names = c(
          "parcel_main_config",
          "parcel_multichain_config",
          "parcel_chain_results",
          "all_parcel_chain_results",
          "parcel_metrics",
          "parcel_rhat_table",
          "parcel_geographic_coverage",
          "parcel_chain_separation",
          "parcel_irreducibility_report",
          "parcel_rhat_plot_obj",
          "mcmc_plan_export"
        ),
        store = store
      )
      invalidate_results[[i]] <- data.table(store = store, status = "invalidated")
      cat(sprintf("  ✓ Done\n"))
    }, error = function(e) {
      invalidate_results[[i]] <<- data.table(store = store, status = paste("FAILED:", conditionMessage(e)))
      cat(sprintf("  ✗ Failed: %s\n", conditionMessage(e)))
    })
  }

  summary_dt <- rbindlist(invalidate_results)
  cat("\n=== INVALIDATE SUMMARY ===\n")
  print(summary_dt)
  cat(sprintf("\nInvalidated: %d / %d\n", sum(grepl("^invalidated$", summary_dt$status)), nrow(summary_dt)))
}
