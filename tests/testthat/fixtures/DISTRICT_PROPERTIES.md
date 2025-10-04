# Test District Spatial Properties

This document describes the expected spatial properties of submitted zone shapefiles used for GIS operations regression testing.

## Purpose

These properties serve as regression test expectations for the GIS operations functions in `test-gis-operations.R`. They validate that spatial calculations on real-world compliance districts produce expected results.

## Test Districts

### Chelsea District 1

**Source:** `ChelseaTCOD2023.zip` (Transit-Oriented Commercial Development District)

**Spatial Properties:**
- **Features:** 1 polygon
- **Total Area:** 14.15 acres (616,194 sq ft)
- **Contiguity:** 100% (single contiguous district)
- **Community Category:** Rapid Transit Community
- **Transit Station:** Adjacent to Chelsea MBTA station (Silver Line/Commuter Rail)

**Use for Testing:**
- `calculate_district_area()` - Simple single-polygon area calculation
- `validate_contiguity()` - Trivial case (100% contiguous)
- `calculate_station_area_percentage()` - High transit overlap expected

---

### Cambridge District 1

**Source:** `cambridge_136051026_Selected_Districts.zip` → `District_01_BA.shp`

**Spatial Properties:**
- **Features:** 8 polygons (all BA - Business A zoning)
- **Total Area:** 9.01 acres (392,556 sq ft)
- **Contiguity:** **8 separate portions, 33.2% in largest** ⚠️
- **Community Category:** Rapid Transit Community
- **Transit Station:** 100% within station areas (PcInStatio = 1)

**Compliance Status:** ⚠️ **Non-Compliant (if standalone district)**
- ✗ Largest contiguous portion: 2.99 acres (33.2%) - FAILS 50% requirement
- ✗ All 8 portions < 5 acres - FAILS minimum portion size

**Use for Testing:**
- `validate_contiguity()` - **Negative test case** for <50% contiguity
- `validate_contiguity()` - Portion size violations (<5 acres)
- `calculate_district_area()` - Multi-polygon area calculation
- `calculate_station_area_percentage()` - 100% transit overlap

**Notes:**
- Cambridge submitted **22 separate district shapefiles** across 5 compliance models
- This represents District 1 (BA zoning) only - one of many complying areas
- The 8 non-contiguous features may represent:
  - Individual parcels within the district (aggregate to single zone)
  - OR separate complying lots (each counts separately)
- **Useful as negative test case** to verify validation catches compliance failures

---

### Somerville District 1

**Source:** `somerville_274_136051026_nr_test_district.zip` → `NR_Transit_Only_Clip.shp`

**Spatial Properties:**
- **Features:** 7 MULTIPOLYGON features (NR - Neighborhood Residential zoning)
- **Total Area:** 99.43 acres (4,331,551 sq ft)
- **Contiguity:** **13 separate portions, 82.4% in largest** ⚠️
- **Community Category:** Rapid Transit Community
- **Transit Station:** All features within station areas

**Compliance Status:** ⚠️ **Partial Compliance**
- ✓ Largest contiguous portion: 81.96 acres (82.4%) - PASSES 50% requirement
- ✗ Has 11 portions < 5 acres - FAILS minimum portion size (3.56, 2.41, 2.04... down to 0.00 acres)
- ⚠️ Filename includes "test_district" - may be preliminary/draft version

**Geometry Notes:**
- 7 features → 13 portions means **multipolygons with internal holes**
- Complex geometry useful for testing st_cast() and polygon handling

**Use for Testing:**
- `validate_contiguity()` - Passes 50% but fails portion size requirement
- `calculate_district_area()` - Largest test district (performance benchmarking)
- Complex MULTIPOLYGON geometry handling
- Edge case: More portions than features

**Notes:**
- Largest test district by area (99.43 acres)
- Good stress test for spatial operations
- Demonstrates that feature count ≠ contiguous portion count
- **Useful for testing** that validation catches portion size violations

---

### Maynard District 1

**Source:** `maynard_136051026_MBTAcommDistricts_20240912.zip`

**Spatial Properties:**
- **Features:** 1 polygon
- **Total Area:** 3.44 acres (149,846 sq ft)
- **Contiguity:** 100% (single contiguous district)
- **Community Category:** Commuter Rail Community
- **Transit Station:** Maynard does not have direct MBTA service

**Use for Testing:**
- `calculate_district_area()` - Small single-polygon district
- `validate_contiguity()` - Trivial case (100% contiguous)
- `calculate_station_area_percentage()` - Should return 0% (no transit)

**Notes:**
- Smallest test district by area (3.44 acres)
- Good test case for districts outside transit station areas
- Maynard is categorized as "Adjacent Small Town" under MBTA Communities Act

---

### Newton District 1

**Source:** `Newton/Newton 3A_Compliance Workbooks-Shapefile_January 2024 Update.zip` → `All Districts.shp`

**Spatial Properties:**
- **Features:** 8 polygons
- **Total Area:** 22.58 acres (983,605 sq ft)
- **Contiguity:** **8 separate portions, 53.1% in largest** ⚠️
- **Community Category:** Rapid Transit/Commuter Rail Community
- **Transit Station:** Multiple Green Line stations

**Compliance Status:** ⚠️ **Partial Compliance**
- ✓ Largest contiguous portion: 11.99 acres (53.1%) - **BARELY PASSES** 50% requirement
- ✗ Has 7 portions < 5 acres - FAILS minimum portion size (2.21, 2.15, 2.15, 1.32, 1.18, 1.09, 0.50 acres)

**Available Sub-Districts:**
- All Districts.shp (combined - what we use)
- MRT District.shp
- VC2 Resi-Only District.shp
- VC3 MU District.shp
- VC3 Resi-Only District.shp

**Use for Testing:**
- `validate_contiguity()` - **Edge case**: Just barely passes 50% (53.1%)
- `validate_contiguity()` - Multiple portion size violations
- `calculate_density_denominator()` - Has dd_deduc and dd_area attributes
- `calculate_district_area()` - Medium-sized multi-polygon district

**Notes:**
- Newton submitted multiple separate districts (like Cambridge and Lincoln)
- "All Districts.shp" combines them all
- Originally in EPSG:6492 (NAD83(2011) Massachusetts Mainland) - transformed to 26986
- Shapefile includes pre-calculated density denominator deductions (dd_deduc, dd_area)
- **Anomaly:** Shapefile `acres` column shows much larger values (129, 23, 5 acres) than calculated totals - needs investigation
- **Useful for testing** near-boundary contiguity cases (just above 50% threshold)

---

### Lincoln District 1

**Source:** `Lincoln 3A and MMU Subdistricts.zip` (nested structure)

**Spatial Properties:**
- **Features:** 4 polygons (4 sub-districts)
- **Total Area:** 6.65 acres (289,719 sq ft)
- **Contiguity:** 100% (all sub-districts form single contiguous area)
- **Community Category:** Adjacent Small Town
- **Transit Station:** No direct MBTA service

**Sub-Districts:**
1. **Codman Rd:** 2.20 acres
2. **Lincoln Rd Lewis St:** 1.89 acres
3. **Lincoln Woods:** 1.90 acres
4. **Village Center MMU:** 0.66 acres

**Use for Testing:**
- `calculate_district_area()` - Multi-polygon district with sub-district tracking
- `validate_contiguity()` - Interesting case: 4 features but 100% contiguous
- `calculate_density_denominator()` - Has DD_Deduc and DD_Area_ac attributes
- Nested zip extraction workflow validation

**Notes:**
- Lincoln submitted 4 separate sub-district zips within main zip
- Despite being 4 separate features, they form a single contiguous area
- Each sub-district has pre-calculated density denominator deductions
- Smallest total area among multi-polygon districts (6.65 acres)
- All shapefiles already in EPSG:26986

---

### Wellesley District 1

**Source:** No submitted district shapefile available

**Spatial Properties:**
- **Status:** ⚠️ No submitted shapefile
- **Features:** N/A
- **Total Area:** N/A (Excel model exists)
- **Community Category:** Commuter Rail Community

**Use for Testing:**
- Excluded from GIS operations tests (no shapefile data)
- Still used for Excel regression testing (unit capacity calculations)

**Notes:**
- Wellesley has Excel compliance model but no submitted district shapefile in our data collection
- Could potentially reconstruct district from parcel-level data if needed
- Not critical for initial GIS operations testing

---

## Data Quality Notes

### CRS Validation
All districts are transformed to **EPSG:26986** (NAD83 Massachusetts State Plane Mainland) for consistent area calculations:
- Chelsea: NA → 26986 (assumed WGS84)
- Cambridge: 26986 (already correct)
- Somerville: NA → 26986 (assumed WGS84)
- Maynard: 26986 (already correct)
- Newton: 6492 → 26986 (NAD83(2011) transformed)

### Area Calculation Method
All areas calculated using:
```r
area_sqft <- as.numeric(sf::st_area(district_sf))
area_acres <- area_sqft / 43560
```

Using planar coordinates in Massachusetts State Plane (US Survey Feet).

### Expected Tolerances

For regression tests:
- **Area calculations:** ± 0.1 acres (acceptable rounding differences)
- **Contiguity percentages:** ± 0.01 (1% tolerance)
- **Station area percentages:** ± 1% (accounting for buffer edge effects)

---

## Test Coverage Summary

### Compliance Status Distribution

| District | Contiguity % | Meets 50%? | Meets 5-acre? | Overall Status |
|----------|--------------|------------|---------------|----------------|
| **Chelsea** | 100% | ✓ | ✓ | ✓ Compliant |
| **Maynard** | 100% | ✓ | ✓ | ✓ Compliant |
| **Lincoln** | 100% | ✓ | ✓ | ✓ Compliant |
| **Somerville** | 82.4% | ✓ | ✗ | ⚠️ Partial |
| **Newton** | 53.1% | ✓ | ✗ | ⚠️ Partial |
| **Cambridge** | 33.2% | ✗ | ✗ | ✗ Non-Compliant |

### Test Case Categories

**✓ Fully Compliant (3 districts):**
- Chelsea: Single polygon, transit area
- Maynard: Single polygon, no transit
- Lincoln: 4 sub-districts, 100% contiguous

**⚠️ Partial Compliance (2 districts):**
- Somerville: Passes 50% (82.4%) but has small portions
- Newton: Barely passes 50% (53.1%) but has small portions

**✗ Non-Compliant (1 district):**
- Cambridge: Fails 50% requirement (33.2%)

### Why Non-Compliant Districts Are Valuable

The partially compliant and non-compliant districts are **intentionally included** because they:

1. **Test validation logic** - Verify that `validate_contiguity()` correctly identifies violations
2. **Provide negative test cases** - Ensure error handling works properly
3. **Test edge cases** - Newton's 53.1% tests near-boundary conditions
4. **Represent reality** - Real submissions may have issues that need detection

### Geometry Complexity

| Feature | District | Use Case |
|---------|----------|----------|
| Single polygon | Chelsea, Maynard | Simple case |
| Multi-polygon contiguous | Lincoln (4) | Sub-districts that touch |
| Multi-polygon non-contiguous | Cambridge (8), Newton (8) | Separate portions |
| MULTIPOLYGON with holes | Somerville (7→13) | Complex geometry |

---

## Using This Documentation

When implementing GIS operations functions (HID-71), use these properties to:

1. **Validate calculation accuracy** - Compare function outputs to documented areas
2. **Test edge cases** - Use variety of feature counts (1, 4, 7, 8 polygons)
3. **Verify CRS handling** - Ensure functions work with EPSG:26986
4. **Test contiguity logic** - Full range: 33.2% to 100%
5. **Test validation** - Verify portion size checks work (all 3 partially/non-compliant have small portions)
6. **Benchmark performance** - Use Somerville (99.43 acres) for performance tests

## Updating This Documentation

When adding new test communities or discovering new properties:

1. Extract shapefile using `dev/prepare_district_shapefiles.R`
2. Calculate spatial properties
3. Add section above with all relevant properties
4. Update test expectations in `test-gis-operations.R`
5. Commit both the enhanced fixture (.rds) and this documentation

---

**Last Updated:** 2025-10-02
**Related Issues:** HID-71 (Implement GIS operations)
**Related Files:**
- `tests/testthat/test-gis-operations.R`
- `tests/testthat/fixtures/*_district1.rds`
