#!/usr/bin/env Rscript
#
# Generate Test Fixtures for Regression Testing
#
# This script extracts calculation data and zoning parameters from Excel
# compliance models and saves them as .rds fixture files for regression testing.
#
# Usage:
#   Rscript tests/testthat/generate-fixtures.R

# Load the package (for extract_zoning_parameters and other functions)
devtools::load_all()

# Source helper functions
source("tests/testthat/helper-excel-extraction.R")

# Communities and districts to generate fixtures for
# Add new entries here to generate additional fixtures
FIXTURE_COMMUNITIES <- list(
  list("Chelsea", 1),
  list("Somerville", 1),
  list("Cambridge", 1),
  list("Wellesley", 1),
  list("Newton", 1),
  list("Lincoln", 1),
  list("Maynard", 1)
)

cat("Generating test fixtures for", length(FIXTURE_COMMUNITIES), "communities...\n\n")

# Generate each fixture
for (community_district in FIXTURE_COMMUNITIES) {
  community <- community_district[[1]]
  district <- community_district[[2]]

  cat("Processing:", community, "District", district, "... ")

  tryCatch({
    # This will extract from Excel and save to fixtures/
    ref_data <- load_community_reference(community, district, force_refresh = TRUE)

    cat("✓ Created", paste0(tolower(community), "_district", district, ".rds"),
        "(", ref_data$n_parcels, "parcels)\n")
  }, error = function(e) {
    cat("✗ FAILED:", conditionMessage(e), "\n")
  })
}

cat("\nFixture generation complete!\n")
cat("Fixtures saved to: tests/testthat/fixtures/\n")
