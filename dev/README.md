# Development Directory

This directory contains development resources, validation reports, and reference documentation for the `mbtazone` R package.

## Directory Structure

### `/validation/`
**Main validation framework and results**

- `systematic_validation.R` - Main validation script comparing R package vs Excel models
- `validation_report.qmd` - Quarto validation report source
- `validation_report.html` - Rendered HTML validation report
- `validation_report_files/` - Supporting files for HTML report
- `validation_results.rds` - Validation results data (55/58 municipalities, 94.8% success rate)

**Key Results:**
- 38 municipalities with exact matches (0% difference)
- 3 municipalities fixed in HID-84 (Brookline, Hull, Lincoln)
- 8 municipalities with >25% difference (documented limitations)

### `/investigations/`
**Detailed investigation documents for validation discrepancies**

- `DISCREPANCY_SUMMARY.md` - Overall summary of all discrepancy investigations
- `DISCREPANCY_INVESTIGATION.md` - Initial discrepancy investigation
- `GRAFTON_INVESTIGATION.md` - Grafton custom Excel formula analysis
- `WESTFORD_INVESTIGATION.md` - Westford data source mismatch
- `GEOMETRY_MISMATCH_INVESTIGATION.md` - Systematic geometry mismatch analysis
- `MAYNARD_VALIDATION.md` - Maynard validation case study

**Key Findings:**
- 2 municipalities use custom Excel formulas (Grafton, Worcester)
- 4 municipalities have data source mismatches (Westford, Harvard, Wayland, Wellesley)
- No calculation bugs identified in R package

### `/archive/`
**Temporary scripts and old data files from investigations**

Contains one-off test scripts, investigation scripts, and intermediate data files that are no longer actively used but preserved for reference.

### `/compliance_model_docs/`
**Official Excel compliance model documentation**

- `Compliance_Model_User_Guide.pdf` - Official EOHLC Excel model user guide
- `Compliance_Model_User_Guide_Summary.md` - Comprehensive markdown summary

### `/proof_of_concept/`
**Student-written prototype code (reference only)**

Original proof-of-concept implementation used as reference for understanding Excel model calculations. Not production code.

## Key Files at Root

- `PRD.md` - Product Requirements Document for the core R package (Phases 1-2, now complete)
- `PRD_SMC_Algorithm.md` - Product Requirements Document for SMC simulation algorithm (planned feature)
- `build_zoning_data.R` - Script to rebuild the `zoning_parameters` dataset
- `README.md` - This file

## Running Validation

To run the complete validation:

```r
setwd("/path/to/mbtazone")
source("dev/validation/systematic_validation.R")
```

To render the validation report:

```bash
cd dev/validation
quarto render validation_report.qmd
```

## Recent Updates (October 2025)

**HID-84: Multi-District Parameter Extraction Bug**
- Fixed validation script to auto-detect district number for parameter extraction
- Previously hardcoded `district = 1`, causing failures for municipalities with parcels in Districts 2-5
- Improved success rate from 86% to 94.8%
- Added perfect matches for Brookline, Hull, and Lincoln

See `validation/validation_report.html` for complete validation results and documentation.
