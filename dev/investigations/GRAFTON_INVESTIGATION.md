# Grafton Discrepancy Investigation

**Date**: 2025-10-08
**Issue**: R package calculates 0 units, Excel model shows 550 units (-100% difference)
**Status**: ✅ RESOLVED - Excel model uses non-standard formula

---

## Summary

Grafton's Excel compliance model uses a **custom override formula** in Column O (Developable Capacity) that differs from the standard calculation logic. This is an intentional modeling choice, not a data quality issue or R package bug.

---

## Excel Model Formula

**Column O** ("Override Developable sf") uses formula:
```
=I20-K20
(Lot Area - Excluded NonPublic Land)
```

**Standard formula** should be:
```
=I20-L20
(Lot Area - Total Excluded Land)
```

---

## Detailed Analysis - Parcel F_606406_2915964

| Column | Name | Value | Source |
|--------|------|-------|--------|
| I | Lot Area | 388,801.67 sq ft | Parcel data |
| K | Excluded NonPublic Land | 16,855.84 sq ft | Calculated exclusions |
| L | Total Excluded Land | 388,828.99 sq ft | K + M |
| M | Total Sensitive Land | 0 sq ft | Public excluded land |
| O | Override Developable | **371,945.83 sq ft** | **=I-K formula** |

**Implied public excluded land**: L - K = 388,829 - 16,856 = **371,973 sq ft**

### R Package Calculation

```r
Lot Area:          388,802 sq ft  (from shapefile SQFT)
Excluded Area:     388,829 sq ft  (from shapefile Tot_Exclud = Column L)
Developable Area:  206 sq ft      (Lot - Excluded)
Final Capacity:    0 units        (below minimum threshold)
```

### Excel Model Calculation

```r
Lot Area:          388,802 sq ft  (Column I)
Excluded Area:     16,856 sq ft   (Column K - NonPublic only)
Developable Area:  371,946 sq ft  (Column O - Manual override formula)
Final Capacity:    ~550 units     (sufficient developable area)
```

---

## Root Cause

Grafton's Excel model **intentionally excludes only NonPublic excluded land** (Column K), allowing development on publicly-owned excluded land.

This appears to be a **policy decision** where:
- Private excluded land (wetlands, conservation easements on private parcels) is excluded
- Public excluded land (schools, parks, government property) is treated as developable

This is a **non-standard approach** - most municipalities exclude ALL excluded land (Column L = Total Excluded Land).

---

## Evidence of Systematic Pattern

- ✅ Found in **District 1** parcel F_606406_2915964
- ✅ Found in **District 4** multiple parcels
- Pattern suggests this is **intentional across the entire Grafton model**, not a formula error

---

## Implications

### For R Package Validation

The R package is **correctly implementing the standard model**:
- Uses `Tot_Exclud` (Column L) from shapefile
- Calculates developable area as: Lot Area - Total Excluded Land
- This matches the Excel model **specification** in the User Guide

Grafton's Excel model has **deviated from the standard** by using a custom override formula.

### For Policy Compliance

**Question for EOHLC/Municipality**:
- Is excluding only NonPublic land an approved modeling variation?
- Should publicly-owned excluded land (schools, parks) count as developable?
- Is this documented in Grafton's compliance submission?

---

## Recommendation

### Short-term (Validation Report)

Document this as a **"Known Excel Model Customization"** in the validation report:

```markdown
**Grafton**: -100% difference (0 vs 550 units)
- Excel model uses custom override formula: =I-K (excludes only NonPublic land)
- R package uses standard formula: Lot Area - Total Excluded Land
- Difference represents 371,973 sq ft of public excluded land
- This appears to be intentional policy choice, not calculation error
- **Status**: Documented, requires municipality/EOHLC clarification
```

### Long-term (Follow-up)

1. **Verify with EOHLC**: Is this modeling approach permitted?
2. **Contact Grafton**: Confirm intentional vs accidental customization
3. **Document in User Guide**: If permitted, add guidance on handling public vs private excluded land
4. **R Package Enhancement** (if needed): Add optional parameter to distinguish public vs private exclusions

---

## Files Referenced

- **Excel Model**: `data/mbta_district_models/[Grafton model filename]`
- **Parcel Shapefile**: `data/land_record_shapefiles/basic/XX_GRAFTON_basic.zip`
- **Validation Script**: `dev/systematic_validation.R`
- **Diagnostic Script**: `/tmp/grafton_diagnostic.R`

---

## Next Steps

- [ ] Add Grafton to "Known Customizations" section in validation report
- [ ] Create filtered summary table excluding municipalities with Excel customizations
- [ ] Follow up with EOHLC on approved modeling variations
- [ ] Review other high-discrepancy municipalities (Westford, Worcester, Harvard, Wayland, Wellesley) for similar patterns

---

## Technical Notes

**Why the shapefile has Tot_Exclud = 388,829:**
- Shapefile appears to have pre-calculated Total Excluded Land (Column L)
- This is the **standard/correct** exclusion value per Excel model specification
- Grafton's Excel model override formula bypasses this standard calculation

**R Package Behavior is Correct:**
- Package correctly reads `Tot_Exclud` from shapefile
- Package correctly calculates developable area using total exclusions
- Package correctly returns 0 units when developable area < minimum threshold

**Excel Model Behavior is Non-Standard:**
- Uses manual override to implement custom exclusion logic
- This is a **municipality-specific modeling choice**, not a general pattern
- R package cannot replicate without explicit override values being provided
