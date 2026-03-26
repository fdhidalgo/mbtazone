## Batch MCMC pipeline runner for grant summary figure
##
## Runs the MCMC pipeline on a curated set of 22 single-district municipalities
## (25 total including 3 already completed: Canton, Leominster, Peabody).
## Skips municipalities that already have a completed target store.
## Logs success/failure to dev/grant_batch_summary.csv.
##
## Expected runtime: ~45-70 min per municipality, ~16-26 hours total (sequential).
## Can be left running unattended overnight.

library(data.table)
library(targets)

# ── Setup ────────────────────────────────────────────────────────────────────
pkg_root <- Sys.getenv(
  "MBTAZONE_PACKAGE_ROOT",
  unset = normalizePath(getwd(), winslash = "/", mustWork = TRUE)
)
owd <- setwd(pkg_root)
on.exit(setwd(owd), add = TRUE)

# Install current package so targets picks up latest code
r_bin <- file.path(R.home("bin"), "R")
install_status <- system2(
  r_bin,
  c("CMD", "INSTALL", "--no-multiarch", shQuote(pkg_root))
)
if (!identical(install_status, 0L)) {
  stop("Failed to install current mbtazone package before running targets.")
}

# ── Define target municipalities ─────────────────────────────────────────────
# All single-district, all 3 data files confirmed present.
# Covers: 4 rapid_transit, 10 commuter_rail, 7 adjacent, 4 adjacent_small_town
grant_municipalities <- data.table(
  community_name = c(
    # Rapid Transit (2) — only ones with district shapefiles available
    "Braintree", "Everett",
    # Commuter Rail (11) — Canton & Leominster already done
    "Canton", "Leominster",
    "Abington", "Cohasset", "Foxborough", "Norwood",
    "Reading", "Rockport", "Salem", "Wakefield", "Whitman",
    # Adjacent (7) — Peabody already done
    "Peabody",
    "Burlington", "Chelmsford", "Danvers", "Grafton", "Hanover", "Marlborough",
    # Adjacent Small Town (5)
    "Ashby", "Boxborough", "Bourne", "Groton", "Topsfield"
  )
)

# Merge with community_info to get community_type
all_districts <- fread("inst/extdata/community_info.csv")
grant_districts <- merge(grant_municipalities, all_districts, by = "community_name")

cat(sprintf("\nGrant batch: %d municipalities selected\n", nrow(grant_districts)))

# ── Run pipeline, skipping already-completed stores ──────────────────────────
results <- vector("list", nrow(grant_districts))

for (i in seq_len(nrow(grant_districts))) {
  name <- grant_districts$community_name[i]
  type <- grant_districts$community_type[i]
  store <- paste0("ext/_targets_", gsub(" ", "_", name))

  cat("\n========================================\n")
  cat(sprintf(
    "Municipality %d/%d: %s (%s)\n",
    i, nrow(grant_districts), name, type
  ))
  cat("========================================\n")

  # Skip if target store already has completed chain results
  store_exists <- dir.exists(store)
  if (store_exists) {
    has_chains <- tryCatch({
      targets::tar_read("all_parcel_chain_results", store = store)
      TRUE
    }, error = function(e) FALSE)

    if (has_chains) {
      cat(sprintf("SKIP: %s — target store already complete\n", name))
      results[[i]] <- data.table(
        district_name = name,
        district_type = type,
        status = "skipped",
        error = NA_character_,
        timestamp = Sys.time()
      )
      next
    }
  }

  Sys.setenv(DISTRICT_NAME = name, DISTRICT_TYPE = type)

  tryCatch({
    tar_make(
      script = "inst/targets/_targets.R",
      store = store,
      reporter = "timestamp"
    )
    results[[i]] <- data.table(
      district_name = name,
      district_type = type,
      status = "success",
      error = NA_character_,
      timestamp = Sys.time()
    )
    cat(sprintf("SUCCESS: %s\n", name))

  }, error = function(e) {
    results[[i]] <<- data.table(
      district_name = name,
      district_type = type,
      status = "failed",
      error = conditionMessage(e),
      timestamp = Sys.time()
    )
    cat(sprintf("FAILED: %s\n  Error: %s\n", name, conditionMessage(e)))
  })
}

# ── Write summary ────────────────────────────────────────────────────────────
summary_dt <- rbindlist(results)
fwrite(summary_dt, "dev/grant_batch_summary.csv")

cat("\n\n=== BATCH SUMMARY ===\n")
print(summary_dt[, .(district_name, district_type, status)])
cat(sprintf(
  "\nSucceeded: %d  |  Skipped: %d  |  Failed: %d  |  Total: %d\n",
  sum(summary_dt$status == "success"),
  sum(summary_dt$status == "skipped"),
  sum(summary_dt$status == "failed"),
  nrow(summary_dt)
))

Sys.unsetenv("DISTRICT_NAME")
Sys.unsetenv("DISTRICT_TYPE")
