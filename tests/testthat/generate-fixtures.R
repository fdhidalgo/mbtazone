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
# Proportionally sampled across community types (27 total)
FIXTURE_COMMUNITIES <- list(
  # Existing rapid_transit (4)
  list("Chelsea", 1),
  list("Somerville", 1),
  list("Cambridge", 1),
  list("Newton", 1),
  # Existing commuter_rail (1)
  list("Wellesley", 1),
  # Existing adjacent (1)
  list("Maynard", 1),
  # Existing adjacent_small_town (1)
  list("Lincoln", 1),
  # New commuter_rail (+9)
  list("Beverly", 1),
  list("Brockton", 1),
  list("Lowell", 1),
  list("Worcester", 1),
  list("Salem", 1),
  list("Reading", 1),
  list("Attleboro", 1),
  list("Haverhill", 1),
  list("Billerica", 1),
  # New adjacent (+7)
  list("Arlington", 1),
  list("Burlington", 1),
  list("Framingham", 1),
  list("Amesbury", 1),
  list("Auburn", 1),
  list("Bedford", 1),
  list("Chelmsford", 1),
  # New adjacent_small_town (+4)
  list("Ashby", 1),
  list("Bourne", 1),
  list("Carlisle", 1),
  list("Groton", 1)
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
