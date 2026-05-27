library(data.table)
library(targets)

# Find all existing district stores
existing_stores <- list.dirs("ext", recursive = FALSE, full.names = TRUE)
district_stores <- existing_stores[grepl("^ext/_targets_", existing_stores)]

cat(sprintf("Found %d existing district stores\n", length(district_stores)))

if (length(district_stores) == 0) {
  cat("No stores found. Nothing to destroy.\n")
} else {
  destroy_results <- vector("list", length(district_stores))

  for (i in seq_along(district_stores)) {
    store <- district_stores[i]
    cat(sprintf("\n[%d/%d] Destroying: %s\n", i, length(district_stores), store))

    tryCatch({
      tar_destroy(
        destroy = "all",
        store = store,
        ask = FALSE
      )
      destroy_results[[i]] <- data.table(store = store, status = "destroyed")
      cat(sprintf("  ✓ Done\n"))
    }, error = function(e) {
      destroy_results[[i]] <<- data.table(store = store, status = paste("FAILED:", conditionMessage(e)))
      cat(sprintf("  ✗ Failed: %s\n", conditionMessage(e)))
    })
  }

  summary_dt <- rbindlist(destroy_results)
  cat("\n=== DESTROY SUMMARY ===\n")
  print(summary_dt)
  cat(sprintf("\nDestroyed: %d / %d\n", sum(grepl("^destroyed$", summary_dt$status)), nrow(summary_dt)))
}
