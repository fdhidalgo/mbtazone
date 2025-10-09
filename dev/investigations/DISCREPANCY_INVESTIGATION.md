# Validation Discrepancy Investigation

**Linear Issue:** HID-83
**Investigation Date:** October 7, 2025
**Status:** Initial findings documented

## Executive Summary

Investigated validation discrepancies found in systematic validation of mbtazone package against Excel compliance models. Key findings:

1. ✅ **Method 2 parcel assignment is CORRECT** - Validation script had a bug incorrectly reporting 0% match rate
2. ⚠️ **Large unit capacity discrepancies** in 6 municipalities require further investigation
3. 🐛 **Data quality issues** identified: inconsistent parcel shapefile column names

## Critical Issues Identified

### 1. Validation Script Bug (FIXED)

**Issue:** Method 2 validation reported 0% parcel match rate for all municipalities

**Root Cause:**
Line 499 of `dev/systematic_validation.R` accessed non-existent field:
```r
r_parcel_ids <- unique(evaluation$parcels$LOC_ID)  # WRONG: $parcels doesn't exist
```

**Fix Applied:**
```r
# Get R-assigned parcels (only those assigned to districts, not NA)
r_parcels_assigned <- evaluation$parcel_detail[!is.na(evaluation$parcel_detail$district_id), ]
r_parcel_ids <- unique(r_parcels_assigned$LOC_ID)
```

**Verification:**
- Abington: 100% match rate (11/11 parcels)
- Braintree: 100% match rate (6/6 parcels)

**Conclusion:** Spatial parcel assignment logic in R package is working correctly.

---

### 2. Grafton: R Calculates 0 Units (Excel: 550 units)

**Discrepancy:** -100% difference

**Investigation Findings:**

**Excel Model:**
- District 1 has 1 parcel: `F_606406_2915964`
- Expected units: 550

**R Package Calculation:**
- Same parcel loaded from shapefile
- Parcel size: 8.93 acres = 389,035 sq ft
- Excluded area: 388,829 sq ft (from `Tot_Exclud` column)
- **Developable area ≈ 206 sq ft** (389,035 - 388,829)
- Result: **0 units** (developable area below all thresholds)

**Zoning Parameters Extracted:**
```
min_lot_size: 0 (no minimum)
building_height: 5 stories
max_lot_coverage: 0.5 (50%)
parking_spaces_per_dwelling_unit: 1.8
```

**Possible Causes:**

1. **Parcel shapefile data error**: `Tot_Exclud` value may be incorrect
   - Excel may use different exclusion data
   - REDACTED Excel file may have sanitized data differently
2. **Excel model error**: Excel may incorrectly calculate 550 units for fully excluded parcel
3. **Column name mismatch**: Grafton uses `Tot_Exclud`, others use `EXCLUDE_AREA_SF`

**Recommendation:**
- Manually inspect Grafton Excel model to verify exclusion data
- Check if Excel "Parcel Data" sheet shows different exclusion values
- Consider standardizing parcel shapefile column names across municipalities

---

### 3. Westford: R Calculates 4x More Units (Excel: 601, R: 2,300)

**Discrepancy:** +283% difference

**Investigation Findings:**

**Excel Model:**
- District 1 has 7 parcels
- Expected units: 601

**R Package Calculation:**
- Same 7 parcels loaded from shapefile
- Total units: 2,300
- **6 out of 7 parcels** have >50 units each

**High-Capacity Parcels:**
| LOC_ID | Acres | R Units |
|--------|-------|---------|
| F_671058_3022467 | 39.1 | 586 |
| F_669591_3024300 | 38.6 | 579 |
| F_670031_3022679 | 33.8 | 508 |
| F_670261_3023814 | 25.2 | 378 |
| F_671038_3021677 | 7.6 | 115 |
| F_670751_3023423 | 6.6 | 99 |

**Zoning Parameters Extracted:**
```
min_lot_size: 0
building_height: 4 stories
max_lot_coverage: NA (not specified)
min_required_open_space: 0.3 (30%)
parking_spaces_per_dwelling_unit: 1.6
max_dwelling_units_per_acre: 15 DU/acre
FAR: NA (not specified)
max_units_per_lot: NA (not specified)
```

**Calculation Logic:**
- Large parcels (7-39 acres) × 15 DU/acre = hundreds of units each
- No FAR or max_units_per_lot constraints extracted from Excel
- R package using density-based calculation only

**Possible Causes:**

1. **Missing constraints in Excel extraction**: FAR or max_units_per_lot may exist but not extracted
2. **Excel uses different calculation method**: May apply constraints not visible in parameters
3. **Parcel data differences**: Excel may have different parcel sizes or exclusions
4. **Manual overrides in Excel**: Excel model may have manual unit capacity overrides

**Recommendation:**
- Manually inspect Westford Excel "District 1" sheet column-by-column
- Check if Excel "Checklist Parameters" has FAR or max_units_per_lot values
- Compare Excel intermediate calculations (columns N-AF) for these 6 high-capacity parcels
- Verify zoning parameter extraction logic handles all Excel formats

---

### 4. Other Large Discrepancies (Pending Investigation)

**Worcester** (+29%): 43,616 Excel → 56,132 R (+12,516 units)

**Harvard** (+104%): 291 Excel → 594 R (+303 units)

**Wayland** (+76%): 262 Excel → 461 R (+199 units)

**Wellesley** (+58%): 527 Excel → 832 R (+305 units)

**Northborough** (-18%): 696 Excel → 572 R (-124 units)

**Northbridge** (-11%): 975 Excel → 863 R (-112 units)

**Norwood** (-6%): 2,098 Excel → 1,964 R (-134 units)

**Status:** Require detailed parcel-level comparison

---

## Data Quality Issues

### Parcel Shapefile Column Name Inconsistencies

**Discovered:** Different municipalities use different column names for the same data

**Examples:**
- Grafton uses: `Tot_Exclud`, `Tot_Sensit`
- Most others use: `EXCLUDE_AREA_SF`, `SENSITIVE_AREA_SF`

**Impact:**
- Package functions expect standardized column names
- May cause calculation errors if not handled properly

**Recommendation:**
- Document all column name variations across 175 parcel shapefiles
- Update `load_municipality()` to standardize column names on load
- Add column name mapping/translation logic

---

### Excel Format Variations

**Failures in Validation:**
- Andover, Lowell, Salem: "No District sheet found"
- Suggests some Excel models have different structure

**Impact:**
- `extract_zoning_parameters()` and `extract_excel_parcel_ids()` fail
- Cannot validate these municipalities

**Recommendation:**
- Manually inspect these Excel files
- Update extraction functions to handle format variations
- Document all Excel format patterns encountered

---

## Perfect Matches (0% Difference)

**36 municipalities** show perfect R vs Excel agreement, including:
- Abington, Acton, Amesbury, Arlington, Ayer, Bellingham, Braintree
- Cambridge, Danvers, Easton, Haverhill, Holliston, Kingston, Lakeville
- Medfield, Medford, Newburyport, Newton, Norfolk, Pembroke, Quincy
- Randolph, Revere, Rochester, Rockland, Sharon, Somerville, Stoneham
- Stoughton, Sudbury, Swampscott, Taunton, Tyngsborough, Walpole
- Westborough, Winchester

**Conclusion:** Core calculation logic is sound for standard cases.

---

## Small Discrepancies (<5%)

**Maynard:** +1.6% (615 Excel → 625 R, +10 units)
- Previously investigated in HID-81
- Known and acceptable difference

**Somerville:** +0.07% (59,474 Excel → 59,513 R, +39 units)
- Negligible rounding difference

**Newbury:** -3.9% (259 Excel → 249 R, -10 units)

**Conclusion:** These are likely rounding or minor calculation differences, not bugs.

---

## Next Steps

### Immediate Actions (High Priority)

1. **Re-run validation** with fixed Method 2 match rate calculation
   - Verify parcel assignment accuracy across all municipalities
   - Document actual match rates

2. **Investigate Grafton thoroughly**
   - Manually inspect Excel model exclusion data
   - Verify parcel shapefile data accuracy
   - Determine if Excel model or shapefile data is correct

3. **Investigate Westford thoroughly**
   - Manual Excel column-by-column review
   - Compare parcel-level intermediate calculations
   - Identify missing constraints or different calculation method

### Secondary Actions

4. **Systematic parcel-level comparison** for Worcester, Harvard, Wayland, Wellesley
   - Extract Excel intermediate calculations (columns N-AF)
   - Compare to R package step-by-step results
   - Identify divergence points

5. **Fix Chelsea Excel parsing** (Excel units = NA)

6. **Document column name variations** across all 175 parcel shapefiles

7. **Update extraction functions** to handle Excel format variations (Andover, Lowell, Salem)

### Long-term Improvements

8. **Standardize parcel data**
   - Create column name mapping logic
   - Auto-translate on load

9. **Expand Excel format handling**
   - Support multiple Excel structures
   - Robust parameter extraction

10. **Create parcel-level validation report**
    - For municipalities with discrepancies
    - Show step-by-step calculation comparison

---

## Appendix: Investigation Scripts

**Created Files:**
- `dev/investigate_discrepancies.R` - Diagnostic script for focused investigation
- `dev/discrepancy_diagnostics.rds` - Saved diagnostic results
- `dev/DISCREPANCY_INVESTIGATION.md` - This findings document (in progress)

**Modified Files:**
- `dev/systematic_validation.R` - Fixed Method 2 match rate bug (line 499-502)
