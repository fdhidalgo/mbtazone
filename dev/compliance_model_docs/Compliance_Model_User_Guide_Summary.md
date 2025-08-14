# Section 3A Compliance Model User Guide Summary

## Overview

The Section 3A Compliance Model is a Microsoft Excel-based tool designed to evaluate Massachusetts MBTA Communities Act compliance. It automates zoning compliance assessment for multi-family housing districts, replacing manual Excel-based workflows with robust calculations and interactive planning capabilities.

## Key Definitions and Terms

### Community Categories
- **Rapid Transit Community**: Has ≥100 acres of developable station area near subway/Silver Line BRT stations
- **Commuter Rail Community**: Has ≥100 acres of developable station area near commuter rail stations  
- **Adjacent Community**: Has <100 acres of developable station area, not an adjacent small town
- **Adjacent Small Town**: Has <100 acres of developable station area AND either <500 persons/sq mi density or ≤7,000 residents

### Land Classifications
- **Developable Land**: All privately-owned land except excluded land, plus certain public land
- **Excluded Land**: Land where multi-family housing cannot be built:
  - Public land (except developable public land)
  - Water bodies and wetlands
  - Title 5 setbacks and wellhead protection zones
  - Protected open space
  - Rights-of-way
  - Educational/institutional uses
- **Sensitive Land**: Developable but environmentally constrained land (flood zones, rare species habitat, agricultural soils, etc.)

### Key Requirements
- **Multi-family Housing**: Building with 3+ units OR 2+ buildings with 1+ unit each
- **As of Right**: Development allowed without special permits, variances, or discretionary approvals
- **Gross Density**: Units per acre including rights-of-way and non-residential uses
- **Housing Suitable for Families**: No age restrictions, bedroom size limits, or occupancy restrictions

## Model Components and Process

### Three Primary Components

1. **Land Map and Parcel Exports** (GIS-based)
   - Draw district boundaries
   - Calculate compliance measurements
   - Export parcel data

2. **Zoning Information and Checklist** (Excel tabs 1-5)
   - District identification
   - Use permissions and restrictions
   - Zoning parameters

3. **District Modeling and Summary** (Excel tabs 6-11)
   - Parcel-by-parcel calculations
   - Unit capacity estimates
   - Compliance verification

## Land Map Workflow (GIS Required)

### Step 1: Access Land Map Data
- Download municipality-specific shapefiles from Compliance Model & Components page
- Two versions available:
  - **Basic**: Essential columns for Excel model
  - **Detailed**: Additional constraint information for exploration

### Step 2: Open Required Files
- Municipality parcel shapefile with excluded/sensitive land measurements
- Individual constraint layers (excluded and sensitive land)
- Aggregated constraint layers
- Transit station areas (0.5-mile radius)
- Density denominator deductions layer

### Step 3: Create/Import District Boundary
- District must use whole parcels (no partial parcels allowed)
- Must be contiguous or meet contiguity requirements
- Review against excluded/sensitive land overlaps
- Consider relocating if excessive constraints present

### Step 4: Calculate District Measurements

#### Required GIS Calculations:
1. **Land area of each non-contiguous portion** (must be ≥5 acres each)
2. **Total district land area** (sum of all portions)  
3. **Percentage within transit station areas** (for Rapid Transit/Commuter Rail communities)
4. **Gross density denominator** (total area minus specific excluded land types)

#### Compliance Checks:
- **Contiguity**: ≥50% of district must be contiguous
- **Minimum Size**: Community-specific requirements
- **Location**: Community-specific percentage within transit areas

### Step 5: Export Data
- Select parcels within district boundaries
- Export to Excel format using "Table to Excel"
- Save district shapefile with calculated attributes

## Excel Model Structure

### Tab 1: Introduction
- Community selection dropdown
- Auto-populated compliance requirements
- Contact information entry

### Tab 2: Checklist District ID
- **Table 1**: Existing zoning districts by transit type
- **Table 2**: Existing overlay districts
- **Table 3**: Other development-controlling overlays
- **Table 4**: Acreage calculations from GIS work

### Tab 3: Checklist Uses
- **Table 5**: Multi-family housing permissions (must be "as of right")
- **Table 6**: Multi-family housing conditions/restrictions

### Tab 4: Checklist Parameters
Captures zoning parameters in 5 sections:

1. **Allowable Building Types**
   - Building types permitted (3+ family, 4+ family, 5+ units)
   - Maximum units per lot restrictions

2. **Lot Sizes**
   - Minimum lot size (enter 0 if none)
   - Additional requirements per dwelling unit
   - Building type variations

3. **Building Volume Restrictions**
   - Building height (enter in stories, not feet)
   - Floor Area Ratio (as decimal)
   - Stepback requirements
   - Height restrictions based on proximity

4. **Building Footprint Restrictions**
   - Lot coverage maximums
   - Open space minimums (20% default)
   - Setback requirements
   - Parking requirements per unit

5. **Dwelling Unit Restrictions**
   - Lot area per dwelling unit
   - Units per acre limits
   - District-wide unit caps

### Tab 5: Zoning Input Summary
- Auto-generated summary of parameters
- Review for accuracy before proceeding

### Tabs 6-10: District Calculations (1-5)
- Paste Land Map parcel data starting at cell A20
- Override columns for public land development
- Model calculations for each parcel:
  - Building footprint after removing open space/parking
  - Building envelope (footprint × height)
  - Unit capacity tests (7 different methods)
  - Final unit capacity (minimum of all tests)

### Tab 11: Summary
Three key sections:
1. **Unit Capacity per District Table**: Comparison of calculation methods
2. **Summary Table**: Key metrics for compliance evaluation
3. **Comparison Table**: Requirements vs. modeled results

## Unit Capacity Calculation Method

The model uses the most restrictive of 7 unit capacity tests:

1. **Model Calculation**: Building envelope ÷ 1,000 SF per unit
2. **Dwelling Units per Acre Limit**: Based on zoning restrictions
3. **Maximum Lot Coverage**: Coverage limit × height ÷ 1,000
4. **Lot Area per Unit**: Total parcel area ÷ required SF per unit
5. **Floor Area Ratio**: FAR × parcel area ÷ 1,000
6. **Maximum Units per Lot**: Direct zoning limit
7. **Lot Size Conformance**: Zero capacity if below minimum lot size

## Key Model Assumptions

- **Unit Size**: 1,000 SF average per multi-family unit
- **Parking**: All surface parking (400 SF per space)
- **Open Space**: 20% minimum even if zoning requires less
- **Building Height**: Must be entered in stories (divide feet by 10)
- **Coordinate System**: NAD83 Massachusetts State Plane (EPSG:26986)

## Compliance Evaluation

### Size Requirements
- Minimum land area (community-specific)
- Minimum unit capacity (community-specific)
- ≥15 dwelling units per acre gross density

### Location Requirements (Rapid Transit/Commuter Rail only)
- Specified percentage of land area within 0.5 miles of transit
- Specified percentage of unit capacity within transit areas

### Contiguity Requirements
- ≥50% of total district area must be contiguous
- Non-contiguous areas must be ≥5 acres each

### Use Requirements
- Multi-family housing allowed as of right
- No age restrictions or family-unsuitable limitations
- Site plan review acceptable if meeting specific standards

## Data Sources and Accuracy

- **Parcel Data**: MassGIS statewide standardized assessor data
- **Excluded/Sensitive Land**: Publicly available datasets with varying accuracy/vintage
- **Transit Stations**: MBTA system with year-round service
- **Error Reporting**: Contact DHCD3A@mass.gov for discrepancies

## Best Practices and Tips

### GIS Workflow
- Use "basic" Land Map files for Excel import
- Validate coordinate system (EPSG:26986) before calculations
- Select parcels by centroid to avoid boundary alignment issues
- Document any public land overrides with justification

### Excel Model Usage
- Freeze panes in Parameters tab to navigate district columns
- Complete all checklist information before reviewing District tabs
- Verify data accuracy in Zoning Input Summary tab
- Use Summary tab for final compliance evaluation

### Common Issues
- Non-conforming lots (below minimum size) get zero capacity
- High parking requirements significantly reduce capacity
- Excessive excluded land limits developable area
- Strict dimensional requirements can cap unit potential

## Technical Requirements

### GIS Software
- ArcGIS Pro or QGIS with intermediate skill level required
- Ability to perform spatial analysis and attribute calculations
- Export capabilities to Excel format

### Model Limitations
- Maximum 1,000 parcels per district tab
- Five district maximum per workbook
- Surface parking assumption may overestimate land use
- Fixed 1,000 SF per unit assumption

## Integration with R Package Development

This user guide provides the foundation for understanding the Excel model that the `mbtazone` R package will replicate and enhance. Key implementation considerations:

- **Spatial Analysis**: All GIS calculations must be reproduced in R using `sf` package
- **Unit Capacity Logic**: Seven-test minimum approach needs R implementation
- **Validation**: Package should validate against Excel model results
- **Data Pipeline**: Automated data processing from MassGIS sources
- **Interactive Tools**: Shiny interface for district boundary exploration
- **Error Handling**: Robust validation of inputs and spatial data quality

The R package should maintain parity with the Excel model's calculations while adding automation, reproducibility, and enhanced user experience through interactive tools.