# destroy_selected_districts.R
#
# Deletes the targets store for a named list of communities, forcing a clean
# re-run next time the pipeline is executed.  Edit COMMUNITIES below before
# running.
#
# Usage (from package root):
#   Rscript inst/targets/destroy_selected_districts.R

library(data.table)
library(targets)

# ---- Edit this list --------------------------------------------------------
COMMUNITIES <- c(
  "Ashburnham",
  "Ashby",
  "Berkley",
  "Boxborough",
  "Carlisle",
  "Cohasset",
  "Dover",
  "Essex"
)
# ----------------------------------------------------------------------------

stores <- paste0("ext/_targets_", gsub(" ", "_", COMMUNITIES))

cat(sprintf("Communities to destroy: %d\n\n", length(stores)))

results <- vector("list", length(stores))

for (i in seq_along(stores)) {
  store   <- stores[i]
  community <- COMMUNITIES[i]

  if (!dir.exists(store)) {
    cat(sprintf("[%d/%d] SKIPPED (no store found): %s\n", i, length(stores), community))
    results[[i]] <- data.table(community = community, store = store, status = "skipped")
    next
  }

  cat(sprintf("[%d/%d] Destroying: %s  (%s)\n", i, length(stores), community, store))
  tryCatch({
    tar_destroy(destroy = "all", store = store, ask = FALSE)
    results[[i]] <- data.table(community = community, store = store, status = "destroyed")
    cat("  ✓ Done\n")
  }, error = function(e) {
    results[[i]] <<- data.table(community = community, store = store,
                                status = paste("FAILED:", conditionMessage(e)))
    cat(sprintf("  ✗ Failed: %s\n", conditionMessage(e)))
  })
}

summary_dt <- rbindlist(results)
cat("\n=== SUMMARY ===\n")
print(summary_dt[, .(community, status)])
cat(sprintf("\nDestroyed: %d  Skipped: %d  Failed: %d\n",
  sum(summary_dt$status == "destroyed"),
  sum(summary_dt$status == "skipped"),
  sum(grepl("^FAILED", summary_dt$status))
))
