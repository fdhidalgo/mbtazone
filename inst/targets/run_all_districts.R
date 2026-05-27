library(data.table)
library(targets)

# Run from package root, or set MBTAZONE_PACKAGE_ROOT to an absolute path to mbtazone
pkg_root <- Sys.getenv("MBTAZONE_PACKAGE_ROOT", unset = normalizePath(getwd(), winslash = "/", mustWork = TRUE))
owd <- setwd(pkg_root)
on.exit(setwd(owd), add = TRUE)
r_bin <- file.path(R.home("bin"), "R")
install_status <- system2(
  r_bin,
  c("CMD", "INSTALL", "--no-multiarch", shQuote(pkg_root))
)
if (!identical(install_status, 0L)) {
  stop("Failed to install current mbtazone package before running targets.")
}
districts <- fread("inst/extdata/community_info.csv")

results <- vector("list", nrow(districts))

for (i in seq_len(nrow(districts))) {
  name <- districts$community_name[i]
  type <- districts$community_type[i]
  store <- paste0("ext/_targets_", gsub(" ", "_", name))

  cat("\n========================================\n")
  cat(sprintf("Running district %d/%d: %s (%s)\n", i, nrow(districts), name, type))
  cat("========================================\n")

  # Set env vars so _targets.R picks them up
  Sys.setenv(DISTRICT_NAME = name, DISTRICT_TYPE = type)

  tryCatch({
    tar_make(
      script = "inst/targets/_targets.R",
      store = store,
      reporter = "timestamp"  # Shows progress with timestamps
    )
    results[[i]] <- data.table(
      district_name = name,
      district_type = type,
      status = "success",
      error = NA_character_,
      timestamp = Sys.time()
    )
    cat(sprintf("✓ SUCCESS: %s\n", name))

  }, error = function(e) {
    results[[i]] <<- data.table(
      district_name = name,
      district_type = type,
      status = "failed",
      error = conditionMessage(e),
      timestamp = Sys.time()
    )
    cat(sprintf("✗ FAILED: %s\n  Error: %s\n", name, conditionMessage(e)))
  })
}

# Write summary
summary_dt <- rbindlist(results)
dir.create("targets", recursive = TRUE, showWarnings = FALSE)
fwrite(summary_dt, "targets/district_run_summary.csv")

cat("\n\n=== SUMMARY ===\n")
print(summary_dt[, .(district_name, district_type, status)])
cat(sprintf("\nSucceeded: %d / %d\n", sum(summary_dt$status == "success"), nrow(summary_dt)))
cat(sprintf("Failed:    %d / %d\n", sum(summary_dt$status == "failed"), nrow(summary_dt)))

# Make sure to run this after to prevent manual runs from defaulting to the last district you ran
Sys.unsetenv("DISTRICT_NAME")
Sys.unsetenv("DISTRICT_TYPE")
