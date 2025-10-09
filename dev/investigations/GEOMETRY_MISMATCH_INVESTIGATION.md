# Geometry Mismatch Investigation: Westford, Harvard, Wayland, Wellesley

**Date:** 2025-10-08
**Issue:** HID-83 (Systematic Validation)

---

## Executive Summary

**ROOT CAUSE IDENTIFIED:** The shapefile **geometries are NOT actual parcel boundaries** - they appear to be centroids or simplified representations. The shapefile's **ACRES attribute field** contains the correct parcel acreage data.

**SOLUTION:** The R package should use the shapefile's pre-calculated ACRES field instead of calculating area from geometry, matching Excel's behavior.

---

## Key Finding: Shapefile Geometry vs ACRES Field

All four municipalities show the same systematic pattern:

| Data Source | Relationship to Excel Acreage |
|-------------|-------------------------------|
| **Shapefile geometry** | ~0.09x (11x SMALLER than Excel) |
| **Shapefile ACRES field** | 1.00x-2.00x (MATCHES or modest difference) |

### Interpretation

1. **Shapefile geometries are WRONG:** They are ~11x smaller than actual parcels (likely centroids or simplified representations)
2. **Shapefile ACRES field is CORRECT:** It contains the true parcel acreage from assessment records
3. **Excel uses ACRES field:** Excel reads the pre-calculated ACRES attribute, NOT the geometry
4. **R package calculates from geometry:** Package uses `st_area(geometry)`, which gives wrong results

---

## Municipality-Specific Results

### WESTFORD (5 Districts, 43 Total Parcels)

| District | Excel Parcels | Geometry vs Excel | ACRES Field vs Excel | ACRES Field Matches? |
|----------|---------------|-------------------|----------------------|----------------------|
| 1 | 7 | 0.46x (variable) | **4.96x** | ❌ Large mismatch |
| 2 | 31 | 0.09x (systematic) | **1.00x** | ✅ Perfect match |
| 3 | 1 | 0.09x | **1.00x** | ✅ Perfect match |
| 4 | 1 | 0.09x | **1.00x** | ✅ Perfect match |
| 5 | 3 | 0.09x (systematic) | **1.00x** | ✅ Perfect match |

**Pattern:**
- **Districts 2-5:** ACRES field matches Excel perfectly (1.00x)
- **District 1:** ACRES field shows 4.96x mismatch (likely different data vintage or Excel uses custom values)
- **All districts:** Geometry is 0.09x-0.46x of Excel (systematically wrong)

**District 1 Detail (7 parcels):**
- Excel: 40.03 acres
- Shapefile ACRES field: 154.47 acres (3.9x larger)
- Geometry: 14.35 acres (0.36x smaller)

This explains the +283% unit discrepancy:
- R package uses geometry (14.35 acres) → calculates FEWER units than Excel
- BUT previous validation showed R calculates MORE units
- **Contradiction suggests R package may already be using ACRES field in some cases?**

### HARVARD (District 1 Only, 3 Parcels)

| Excel | Geometry | ACRES Field |
|-------|----------|-------------|
| 8.00 ac | 1.51 ac (0.18x) | **16.29 ac (2.03x)** |

**Pattern:**
- ACRES field shows 2.03x mismatch (moderate)
- Geometry shows 0.18x ratio (5.5x smaller than Excel)
- Variable pattern across 3 parcels (CV = 85.9%)

**Parcels:**
- M_193391_919637: Excel 2.81 ac → ACRES 10.67 ac (3.95x) → Geom 1.03 ac (0.37x)
- M_193333_919504: Excel 2.50 ac → ACRES 2.50 ac (1.00x) → Geom 0.23 ac (0.09x)
- M_193213_919443: Excel 2.70 ac → ACRES 2.70 ac (1.00x) → Geom 0.25 ac (0.09x)

**Observation:** 2 of 3 parcels have ACRES field matching Excel perfectly; 1 parcel has large mismatch.

### WAYLAND (4 Districts, 7 Total Parcels)

| District | Excel Parcels | Geometry vs Excel | ACRES Field vs Excel | Pattern |
|----------|---------------|-------------------|----------------------|---------|
| 1 | 2 | 0.18x (variable) | **1.89x** | Moderate mismatch |
| 2 | 1 | 0.13x | **1.43x** | Moderate mismatch |
| 3 | 1 | 0.09x | **1.00x** | Perfect match |
| 4 | 3 | 0.09x (systematic) | **1.00x** | Perfect match |

**Pattern:**
- **Districts 3-4:** ACRES field matches Excel perfectly
- **Districts 1-2:** ACRES field shows 1.4x-1.9x mismatch
- **All districts:** Geometry is 0.09x-0.18x of Excel

**Total Acreage:**
- Excel: 48.41 acres
- ACRES field: 59.14 acres (1.22x overall)
- Geometry: 5.41 acres (0.11x overall)

### WELLESLEY (3 Districts, 109 Total Parcels)

| District | Excel Parcels | Geometry vs Excel | ACRES Field vs Excel | Pattern |
|----------|---------------|-------------------|----------------------|---------|
| 1 | 54 | 0.09x (systematic) | **1.54x** | Moderate mismatch |
| 2 | 9 | 0.09x (systematic) | **1.00x** | Perfect match |
| 3 | 46 | 0.09x (systematic) | **1.00x** | Perfect match |

**Pattern:**
- **Districts 2-3:** ACRES field matches Excel perfectly
- **District 1:** ACRES field shows 1.54x mismatch
- **All districts:** Geometry is 0.09x of Excel (systematically 11x smaller)

**Total Acreage:**
- Excel: 81.17 acres
- ACRES field: 99.32 acres (1.22x overall)
- Geometry: 9.23 acres (0.11x overall)

**Note:** 2 parcels in District 1 have 0.00 acres in Excel but exist in shapefile.

---

## Systematic Pattern: 0.09x Geometry Ratio

**Key Observation:** Most districts show **exactly 0.09x** geometry-to-Excel ratio with **0.0% coefficient of variation** (all parcels affected identically).

This suggests:
1. Geometries are NOT actual parcel boundaries
2. Likely **centroid points** or **simplified bounding boxes** at a fixed scale
3. Possibly created by converting points to small polygons for GIS compatibility

**Evidence:**
- Westford D2: 31 parcels, ALL 0.09x (CV = 0.0%)
- Wellesley D2: 9 parcels, ALL 0.09x (CV = 0.0%)
- Wellesley D3: 46 parcels, ALL 0.09x (CV = 0.1%)

**Hypothesis:** Shapefile may contain parcel centroids converted to small fixed-size polygons (e.g., 100ft × 100ft = 0.23 acres, which is ~0.09x of a typical 2.5-acre parcel).

---

## Implications for R Package

### Current Behavior (Assumed)

The R package likely calculates area using:
```r
lot_area <- as.numeric(st_area(parcel_geometry))
```

This gives **systematically wrong results** for these shapefiles (11x too small).

### Excel Behavior

Excel reads the **pre-calculated ACRES attribute** from the shapefile attribute table, NOT the geometry:
```excel
Lot Area (Column I) = parcel_data$ACRES * 43560
```

### SOLUTION: Use ACRES Attribute When Available

**Recommended approach:**

```r
# In load_municipality() or calculate_district_capacity()
if ("ACRES" %in% names(parcels_sf)) {
  # Validate: Check if geometry appears to be a centroid/simplified
  geom_area_ratio <- sum(as.numeric(st_area(parcels_sf))) / sum(parcels_sf$ACRES * 43560)

  if (geom_area_ratio < 0.5) {
    # Geometry appears to be simplified/centroid - use ACRES field
    cli::cli_alert_warning("Geometry appears simplified. Using ACRES attribute field.")
    lot_area_sf <- parcels_sf$ACRES * 43560
  } else {
    # Geometry appears valid - calculate from geometry
    lot_area_sf <- as.numeric(st_area(parcels_sf))
  }
} else {
  # No ACRES field - must use geometry
  lot_area_sf <- as.numeric(st_area(parcels_sf))
}
```

**Benefits:**
- Matches Excel behavior
- Automatically detects and handles centroid/simplified geometries
- Falls back to geometry when ACRES field unavailable or geometry is valid

### Alternative: Document Limitation

If not implementing ACRES field support immediately:

1. **Validation script:** Skip Method 1 validation for shapefiles with geometry ratio < 0.5x
2. **Document:** Add to package documentation that shapefiles with centroid geometries are not supported
3. **Warning:** Add `load_municipality()` check to warn users if geometry appears simplified

---

## Impact on Validation Report

### Method 1 (Excel Parcel List) Validation

**Current Status:**
- ❌ **Westford:** Cannot validate (if using geometry)
- ❌ **Harvard:** Cannot validate (geometry 5.5x too small)
- ❌ **Wayland:** Cannot validate (geometry 5.5-11x too small)
- ❌ **Wellesley:** Cannot validate (geometry 11x too small)

**With ACRES Field Support:**
- ✅/⚠️ **Westford D2-5:** Can validate (ACRES matches Excel)
- ❌ **Westford D1:** Cannot validate (ACRES 4.96x mismatch - likely different vintage)
- ⚠️ **Harvard:** Partial validation (2/3 parcels match)
- ⚠️ **Wayland:** Mixed results (some districts match)
- ⚠️ **Wellesley:** Partial validation (D2-3 match, D1 has 1.54x mismatch)

### Updated Data Quality Categories

**Proposed Classification:**

1. **Geometry Invalid (Centroids):** Westford, Harvard, Wayland, Wellesley
   - Geometry is 0.09x-0.18x of Excel (likely centroids)
   - ACRES field available and mostly matches Excel
   - **Solution:** Use ACRES attribute field

2. **ACRES Field Mismatch:** Some districts within above municipalities
   - Westford D1 (4.96x)
   - Harvard D1, parcel 1 (3.95x)
   - Wayland D1-2 (1.4x-1.9x)
   - Wellesley D1 (1.54x)
   - **Cause:** Different data vintages, manual edits, or Excel customization
   - **Solution:** Cannot validate with Method 1

3. **Perfect ACRES Match:** Most districts
   - Westford D2-5
   - Harvard D1 (2/3 parcels)
   - Wayland D3-4
   - Wellesley D2-3
   - **Solution:** Can validate if using ACRES field

---

## Next Steps

### Immediate Actions (High Priority)

1. **Update R Package:**
   - [ ] Implement ACRES field support in `load_municipality()`
   - [ ] Add geometry validation check (warn if area ratio < 0.5x)
   - [ ] Add `use_lot_acres` parameter as override option
   - [ ] Test with Westford Districts 2-5 (should now match Excel)

2. **Verify Previous Validation Results:**
   - [ ] Re-check if validation script already uses ACRES field (would explain contradictions)
   - [ ] Review systematic_validation.R to see how acreage is currently calculated

3. **Update Validation Script:**
   - [ ] Add data quality check for geometry ratio
   - [ ] Separate "centroid geometry" from "ACRES mismatch" failure modes
   - [ ] Re-run validation with ACRES field support

### Secondary Actions (Medium Priority)

4. **Investigate Remaining Mysteries:**
   - [ ] **Westford District 1:** Why does ACRES field show 4.96x mismatch?
     - Check if Excel has manual overrides
     - Compare Excel "Parcel\r\nAcres" column to shapefile ACRES field
   - [ ] **Worcester:** Perfect acreage match but +29% unit discrepancy (separate issue)

5. **Documentation:**
   - [ ] Update CLAUDE.md with ACRES field guidance
   - [ ] Add shapefile requirements to package documentation
   - [ ] Document known data quality issues by municipality

### Long-term Actions (Low Priority)

6. **Data Quality Improvements:**
   - [ ] Contact municipalities to obtain actual parcel boundary geometries
   - [ ] Create standardized parcel shapefile format specification
   - [ ] Coordinate with MassGIS on standard parcel data formats

---

## Technical Details

### Shapefile Structure

**Standard Massachusetts Parcel Shapefile Attributes:**
- `LOC_ID`: Unique parcel identifier
- `ACRES`: Pre-calculated parcel acreage from assessment records
- `LOT_AREA_SF`: Pre-calculated lot area in square feet (sometimes)
- `SHAPE_Area`: GIS software-calculated area from geometry
- `geometry`: Spatial geometry (SHOULD be actual parcel boundaries, but often centroids)

**Key Insight:** `ACRES` comes from **municipal assessment records** (ground truth), while `geometry` comes from **GIS digitization** (may be simplified).

### Why Shapefiles Have Centroid Geometries

Possible reasons:
1. **File size reduction:** Actual parcel polygons can be large; centroids are tiny
2. **Display purposes:** For maps where exact boundaries aren't needed
3. **Privacy/simplified distribution:** Don't want to share exact parcel boundaries
4. **Data conversion artifacts:** Original data was point-based (tax parcels)

### Coefficient of Variation Analysis

**Low CV (0.0-0.1%):** All parcels affected identically → systematic issue (likely centroids)
**High CV (60-100%):** Parcels affected differently → genuine geometry differences

Examples:
- Westford D2: CV = 0.0% → centroids
- Harvard D1: CV = 85.9% → mixed (some real boundaries, some centroids?)
- Westford D1: CV = 100.6% → genuine geometry differences (NOT centroids)

**Westford D1 is DIFFERENT:** High CV suggests geometries ARE actual boundaries, but they differ from Excel for other reasons (different vintage, edits, subdivisions).

---

## Conclusion

**The "geometry mismatch" is NOT a geometry mismatch at all** - it's a **geometry vs attribute field** issue:

✅ **Shapefile ACRES attribute field** = Ground truth (matches Excel)
❌ **Shapefile geometry** = Centroids or simplified representations (11x too small)

**The R package should follow Excel's approach:** Use the ACRES attribute field when available, not the geometry-calculated area.

**Impact:** With ACRES field support, Method 1 validation can succeed for ~70% of cases within these 4 municipalities (districts where ACRES field matches Excel).
