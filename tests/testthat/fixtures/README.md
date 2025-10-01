# Test Fixtures

This directory contains cached reference data extracted from Excel compliance models for regression testing.

## Files

- `chelsea_district1.rds` - Chelsea District 1 reference data (2001 parcels)
- `somerville_district1.rds` - Somerville District 1 reference data (10001 parcels)
- `cambridge_district1.rds` - Cambridge District 1 reference data (2001 parcels)

## Data Structure

Each `.rds` file contains a list with:

- `community` - Community name
- `district` - District number
- `excel_file` - Source Excel filename
- `extracted_date` - Date of extraction
- `n_parcels` - Number of parcels
- `calculations` - List of calculation column vectors:
  - `parcel_sf` - Parcel area in square feet
  - `total_excluded_land` - Total excluded land area
  - `developable_parcel_sf` - Developable parcel area (Excel column N)
  - `final_unit_capacity` - Final unit capacity (Excel column AF)
  - And 20+ other calculation columns

## Regenerating Fixtures

To regenerate these fixtures from the Excel models:

```r
# From package root directory
source("tests/testthat/helper-excel-extraction.R")

ref_data <- load_community_reference(
  community = "Chelsea",
  district = 1,
  force_refresh = TRUE
)
```

Or use the extraction script:

```bash
Rscript /tmp/extract_reference_data.R
```

## Source Data

Excel compliance models are located at:
`/Users/dhidalgo/MIT Dropbox/Fernando Hidalgo/projects/mbta_communities/data/mbta_district_models`

Accessed via symlink at `tests/testthat/excel_reference/`
