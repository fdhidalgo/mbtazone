# Zoning Compliance Evaluator


## Overview

The `create_zoning_evaluator()` function creates a compliance evaluator that:

1. Takes a redistricting plan with 2 districts
2. Identifies the overlay district (smaller district)
3. Extracts required zoning data for the overlay district
4. Uses the Excel compliance model to evaluate zoning compliance
5. Returns 0 for compliant plans, 1 for non-compliant plans

## Files

### Core Implementation
- `create_zoning_evaluator.R` - Main evaluator function
- `excel_model.R` - Excel compliance model implementation
- `compliance_utils.R` - Utility functions for compliance calculations
- `shapefile_utils.R` - GIS utilities for area calculations and projections
- `calc_layers.R` - Loads key calculation layers (transit stations, density deductions)

### Test Files
- `test_0730_zoning_evaluator.R`
- `0730_model.Rmd`

## Data Files

```
data/
├── cambridge.shp                           # Cambridge parcel shapefile
├── cambridge.shx                           # Shapefile index
├── cambridge.dbf                           # Shapefile attributes
├── Transit_Station_Areas_Half_Mile_Radius.shp  # Transit station buffers
├── Transit_Station_Areas_Half_Mile_Radius.shx
├── Transit_Station_Areas_Half_Mile_Radius.dbf
├── Density_Denominator_Deductions.shp     # Gross Density Denominator deductions
├── Density_Denominator_Deductions.shx
├── Density_Denominator_Deductions.dbf
└── community_info.csv                     # Community zoning parameters
```

## Required Columns in Cambridge Shapefile

The Cambridge shapefile must contain these exact column names:

- `LOC_ID` - Unique location identifier
- `Address` - Property address
- `Owner` - Property owner
- `UseCodes` - Land use codes
- `UseDesc` - Land use descriptions  
- `TRANSIT` - Transit accessibility ("Y"/"N")
- `ACRES` - Property area in acres
- `SQFT` - Property area in square feet
- `PublicInst` - Public institution flag
- `NonPubExc` - Non-public exclusion flag
- `Tot_Exclud` - Total exclusion area
- `Tot_Sensit` - Total sensitive area

## Usage

### Basic Usage

```r
library(redist)
source("simulation/create_zoning_evaluator.R")

cambridge_shp <- st_read("data/cambridge.shp")
cambridge_shp <- st_set_crs(cambridge_shp, 26986)

cambridge_buffered <- st_buffer(cambridge_shp, dist = 15)
adj_list <- redist.adjacency(cambridge_buffered)
empty_adj <- sapply(adj_list, length) == 0
cambridge_shp <- cambridge_shp[-which(empty_adj), ]
cambridge_buffered <- st_buffer(cambridge_shp, dist = 15)
adj_list <- redist.adjacency(cambridge_buffered)
disconnected_indices <- c(12708, 12772)
cambridge_shp_contiguous <- cambridge_shp[-disconnected_indices, ]
cambridge_buffered_contiguous <- st_buffer(cambridge_shp_contiguous, dist = 15)
adj_list_contiguous <- redist.adjacency(cambridge_buffered_contiguous)

# Create redist_map
cambridge_map <- redist_map(
    cambridge_shp_contiguous,
    pop_tol = 0.9,
    ndists = 2,
    total_pop = SQFT,
    adj = adj_list_contiguous
)

# Create evaluator
evaluator <- create_zoning_evaluator(cambridge_map, community_name = "Cambridge")

# Generate redistricting plans
constr <- redist_constr(cambridge_map)
cambridge_plans <- redist_smc(cambridge_map, nsims = 50, constraints = constr)

# Evaluate plans for compliance
plan_matrix <- get_plans_matrix(cambridge_plans)
scores <- apply(plan_matrix, 2, function(plan) evaluator(plan, cambridge_map))

# Filter to compliant plans
compliant_plans <- cambridge_plans[which(scores == 0), ]
```

### Integration with redist Constraints

```r
# Add zoning compliance as a constraint
source("R/redist_constr.R")

constr <- redist_constr(cambridge_map)
constr <- add_constr_zoning_compliance(constr, strength = 100, evaluator = evaluator)

# Generate plans with zoning constraint
cambridge_plans <- redist_smc(cambridge_map, nsims = 50, constraints = constr)
```

### Running the Complete Test

```r
# Run the full test suite
source("test_0730_zoning_evaluator.R")
```

## Output

The evaluator function returns:
- `0` - Plan is compliant with zoning requirements
- `1` - Plan is non-compliant with zoning requirements

### Debug Mode

Set environment variables for debugging:
```r
Sys.setenv(DEBUG_ZONING = "TRUE")
```