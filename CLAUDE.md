# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The `mbtazone` package implements Massachusetts' MBTA Communities Act compliance model in R, providing automated zoning compliance assessment tools. The package replaces manual Excel-based workflows with robust R code featuring comprehensive calculation functions, GIS operations, and validation against Excel model outputs.

**Current Status (October 2025):**

- Core compliance calculation engine: ✅ Complete
- Excel regression testing: ✅ Complete (7 communities validated)
- GIS operations: ✅ Complete
- Documentation: ✅ Complete (33 exported functions)
- Shiny interactive app: ⏸️ Not yet implemented (see PRD in `dev/PRD.md`)

## Package Architecture

### Core Modules (R/ directory)

The package consists of 6 main R files with 34 exported functions:

1. **`data_loaders.R`** (3 exports)

   - `load_municipality()`: Load and validate parcel shapefiles
   - `load_station_areas()`: Load MBTA station area boundaries
   - `load_calculation_layers()`: Load GIS calculation layers (exclusions, etc.)

2. **`zoning_parameters.R`** (2 exports)

   - `extract_zoning_parameters()`: Extract parameters from Excel compliance models
   - `create_zoning_parameters()`: Manually create zoning parameter lists

3. **`gis_operations.R`** (7 exports)

   - `calculate_district_area()`: Calculate total land area of compliance district
   - `calculate_density_denominator()`: Calculate density denominator with deductions
   - `validate_contiguity()`: Check 50% contiguity requirement
   - `calculate_station_area_overlap()`: Measure parcel overlap with station areas
   - `identify_excluded_parcels()`: Filter out ineligible parcels
   - `calculate_parcel_centroids()`: Generate parcel centroids for spatial operations
   - `precompute_spatial_attributes()`: Pre-compute spatial intersections for batch simulations

4. **`unit_capacity_calculations.R`** (18 exports)

   - Core calculation functions implementing Excel model logic
   - Each function corresponds to specific Excel columns (N, O, P, etc.)
   - Includes: `calculate_developable_area()`, `calculate_final_unit_capacity()`, etc.
   - See function list in R/unit_capacity_calculations.R:1-500 for complete inventory

5. **`compliance_pipeline.R`** (4 exports)

   - `assign_parcels_to_districts()`: Assign parcels to compliance districts using area-weighted spatial intersection
   - `calculate_district_capacity()`: Calculate unit capacity for entire district
   - `evaluate_compliance()`: Complete end-to-end compliance evaluation
   - `summarize_compliance_results()`: Generate compliance summary reports

6. **`mbtazone-package.R`**
   - Package-level documentation

## Development Commands

### Standard R Package Development

```r
# Load and develop the package
devtools::load_all()

# Run all tests
devtools::test()

# Run specific test file
devtools::test(filter = "compliance-pipeline")
devtools::test(filter = "excel-regression")

# Build and check package
devtools::check()
devtools::document()
devtools::install()
```

### Running Tests from Command Line

```bash
# Run all tests
R -e "devtools::test()"

# Run specific test file
R -e "devtools::test(filter = 'compliance-pipeline')"

# Check package
R -e "devtools::check()"
```

### Key File Locations

```
mbtazone/
├── R/                           # Source code (6 files, 33 exports)
│   ├── compliance_pipeline.R   # Main workflow functions
│   ├── unit_capacity_calculations.R  # 18 calculation functions
│   ├── gis_operations.R        # Spatial analysis functions
│   ├── data_loaders.R          # Data loading and validation
│   ├── zoning_parameters.R     # Parameter extraction from Excel
│   └── mbtazone-package.R      # Package documentation
│
├── tests/testthat/             # Test suite (~4,000 lines)
│   ├── test-compliance-pipeline.R
│   ├── test-excel-regression.R  # Excel model validation
│   ├── test-gis-operations.R
│   ├── test-unit-capacity.R
│   ├── test-integration.R
│   ├── test-data-loaders.R
│   ├── test-zoning-parameters.R
│   ├── fixtures/                # Excel reference data (7 communities)
│   ├── helper-excel-extraction.R
│   └── generate-fixtures.R
│
├── data/                       # Package datasets (lazy-loaded)
│   └── zoning_parameters.rda   # Pre-extracted zoning parameters (60 municipalities)
│
├── inst/extdata/               # Example data
│   ├── parcels/                # 7 municipality shapefiles (ZIP)
│   ├── community_info.csv      # Community metadata
│   └── statewide/              # Statewide GIS layers
│
├── dev/                        # Development resources
│   ├── PRD.md                  # Product Requirements Document
│   ├── build_zoning_data.R     # Script to rebuild zoning_parameters dataset
│   ├── compliance_model_docs/  # Excel model documentation
│   └── proof_of_concept/       # Student prototype (reference only)
│
├── man/                        # Generated documentation (43 files)
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions (auto-generated)
└── CLAUDE.md                   # This file
```

## Package Data

The package includes pre-extracted zoning parameters from real MBTA Communities compliance models:

### `zoning_parameters` Dataset

**Overview:**

- **138 district parameter sets** from **60 Massachusetts municipalities**
- Pre-extracted zoning regulations ready for use in calculations
- Automatically lazy-loaded when package is loaded
- File size: 18.6 KB (compressed .rda format)

**Structure:**

```r
# Access the dataset
library(mbtazone)
head(zoning_parameters)

# Data.table with 138 rows × 16 columns:
#   - municipality: Municipality name
#   - district: District number (1-5)
#   - excel_file: Source Excel filename
#   - extraction_date: When data was extracted
#   - 12 zoning parameters: min_lot_size, building_height, FAR, etc.
```

**Common Use Cases:**

```r
# Get parameters for specific municipality
chelsea_params <- zoning_parameters[municipality == "Chelsea" & district == 1]

# Use in calculations (convert row to list, drop metadata)
params <- as.list(chelsea_params[, -(1:4)])
capacity <- calculate_district_capacity(parcels, districts, params)

# Explore patterns across municipalities
hist(zoning_parameters$min_lot_size)
zoning_parameters[order(-max_dwelling_units_per_acre)]
```

**Documentation:**

- Help: `?zoning_parameters`
- Documented in: `R/data.R`
- Build script: `dev/build_zoning_data.R`

**Rebuilding the Dataset:**
If new Excel models are added or updated, rebuild with:

```r
source("dev/build_zoning_data.R")  # Extracts from all Excel models
devtools::document()                 # Regenerate help files
```

## Code Style and Conventions

### R Package Standards

- Use `snake_case` for function and variable names
- All exported functions must have complete Roxygen2 documentation
- Include `@examples` in function documentation
- Use `testthat` for all testing with >95% coverage target
- Follow tidyverse style guide
- Use data.table for all data manipulation

### Function Naming Patterns

- **Calculation Functions**: Use descriptive `calculate_*()` pattern instead of cryptic abbreviations
  - ✅ `calculate_developable_area()` vs ❌ `apply_N_func()`
  - ✅ `calculate_final_unit_capacity()` vs ❌ `apply_AF_func()`
- **Parameter Names**: Use full descriptive names over abbreviations
  - ✅ `lot_area, excluded_area, min_lot_size` vs ❌ `I, L, min_size`
- **Return Values**: Use descriptive variable names in calculations
  - ✅ `developable_area, final_capacity` vs ❌ `result, output`

### Data Handling Philosophy

- **NA Preservation**: Always preserve uncertainty rather than silent conversion
  - ✅ Return `NA_real_` for missing input data
  - ❌ Convert `NA` to `0` or other default values
  - Document NA handling behavior in function documentation

### Preferred Code Patterns

- **Conditional Logic**: Use `data.table::fcase()` over nested `ifelse()` or `case_when()`
- **Iteration**: Use `purrr` functions over explicit loops
- **Vector Operations**: Leverage vectorized operations over element-wise processing

### Spatial Data Conventions

- Always use EPSG:26986 (NAD83 Massachusetts State Plane) for calculations
- Validate CRS before processing: `sf::st_crs(data)$epsg == 26986`
- Use `units` package for explicit area/distance units
- Prefer vectorized operations over loops for GIS processing

### Typical Workflow

```r
library(mbtazone)

# 1. Load municipality parcel data
parcels <- load_municipality(
  "inst/extdata/parcels/57_CHELSEA_basic.zip",
  community_name = "Chelsea"
)

# 2. Define or extract zoning parameters
zoning <- extract_zoning_parameters(
  "path/to/Chelsea_Model.xlsx",
  district = 1
)

# 3. Define district boundaries (spatial polygon or column name)
district <- sf::st_read("district_boundary.shp")

# 4. Calculate district capacity
capacity <- calculate_district_capacity(
  municipality = parcels,
  districts = district,
  zoning_params = zoning
)

# 5. Evaluate full compliance
results <- evaluate_compliance(
  municipality = parcels,
  districts = district,
  zoning_params = zoning,
  community_type = "rapid_transit"
)
```

### Simulation Workflow (High-Performance Batch Processing)

For scenarios requiring evaluation of thousands of zoning parameter combinations on the same municipality (e.g., policy simulations, optimization), use the pre-computation workflow for ~1000x performance improvement:

```r
library(mbtazone)

# ==== SETUP PHASE (once per municipality) ====

# 1. Load municipality parcel data
parcels <- load_municipality(
  "inst/extdata/parcels/57_CHELSEA_basic.zip",
  community_name = "Chelsea"
)

# 2. Load spatial layers
transit_stations <- load_station_areas()
density_deductions <- load_density_deductions()

# 3. Pre-compute ALL spatial operations (one-time cost)
#    This performs expensive spatial intersections once
parcels_precomputed <- precompute_spatial_attributes(
  municipality = parcels,
  station_areas = transit_stations,
  density_deductions = density_deductions,
  verbose = TRUE
)

# 4. Save pre-computed parcels for reuse
saveRDS(parcels_precomputed, "chelsea_precomputed.rds")

# ==== SIMULATION PHASE (1000s of iterations) ====

# 5. Load pre-computed parcels (if starting new session)
parcels_precomputed <- readRDS("chelsea_precomputed.rds")

# 6. Define district boundaries (same for all iterations)
district <- sf::st_read("district_boundary.shp")

# 7. Run batch simulations (pure arithmetic, no spatial ops)
results_list <- lapply(1:10000, function(i) {

  # Generate or load zoning parameter variation i
  zoning_params <- create_zoning_parameters(
    min_lot_size = 5000 + i * 10,
    FAR = 2.0 + runif(1, -0.5, 0.5),
    # ... other parameters
  )

  # Evaluate compliance using pre-computed attributes
  # (skips spatial intersection, uses pre-computed columns)
  evaluate_compliance(
    municipality = parcels_precomputed,
    districts = district,
    zoning_params = zoning_params,
    community_type = "rapid_transit",
    precomputed = TRUE,  # KEY: Use pre-computed attributes
    verbose = FALSE
  )
})

# 8. Extract and analyze simulation results
simulation_df <- data.frame(
  iteration = 1:10000,
  compliant = sapply(results_list, function(x) x$summary$compliant),
  total_units = sapply(results_list, function(x) x$summary$total_units),
  gross_density = sapply(results_list, function(x) x$summary$gross_density)
)
```

**Performance Comparison:**

- **Standard mode** (10,000 simulations): ~83 hours (repeated spatial intersections)
- **Precomputed mode** (10,000 simulations): ~5 minutes (arithmetic only)
- **Speedup**: ~1000x for batch scenarios

**When to use precomputed mode:**

- Evaluating hundreds/thousands of zoning parameter combinations
- Optimization workflows (finding optimal zoning parameters)
- Monte Carlo simulations for sensitivity analysis
- Scenarios where municipality parcels and spatial layers are fixed

**When NOT to use precomputed mode:**

- Single or few evaluations (overhead not worth it)
- Changing district boundaries between evaluations
- Different municipalities each iteration

## Testing Infrastructure

### Test Organization (tests/testthat/)

The package has ~4,000 lines of test code across multiple test files:

- **`test-unit-capacity.R`** (~24k lines): Unit tests for all 18 calculation functions
- **`test-gis-operations.R`** (~28k lines): Spatial operation tests
- **`test-excel-regression.R`** (~21k lines): Validation against Excel model outputs
- **`test-compliance-pipeline.R`** (~11k lines): Integration tests for complete workflow
- **`test-integration.R`** (~16k lines): End-to-end pipeline tests with synthetic data
- **`test-data-loaders.R`** (~13k lines): Data loading and validation tests
- **`test-zoning-parameters.R`** (~7k lines): Parameter extraction tests

### Test Fixtures

Located in `tests/testthat/fixtures/`:

- Excel calculation data from 7 communities: Chelsea, Somerville, Cambridge, Wellesley, Newton, Lincoln, Maynard
- Each fixture is an RDS file containing zoning parameters and reference calculations
- Format: `{community}_district{N}.rds`

### Excel Regression Testing Approach

The package implements comprehensive regression testing against Excel model outputs:

1. **Fixture Generation**: `generate-fixtures.R` extracts reference data from Excel models
2. **Helper Functions**: `helper-excel-extraction.R` provides utilities for reading Excel models
3. **Validation Strategy**: Test each calculation function against Excel outputs for all 7 communities
4. **NA Handling**: R preserves NA values for missing data; Excel converts to 0. Tests filter to complete data for comparison.

### Example Data

Located in `inst/extdata/`:

- **`parcels/`**: 7 municipality parcel shapefiles (ZIP format)
  - Chelsea, Somerville, Cambridge, Wellesley, Newton, Lincoln, Maynard
- **`community_info.csv`**: Community metadata and compliance requirements
- **`statewide/`**: Statewide GIS layers (if applicable)

## Package Dependencies

**Core Dependencies** (from DESCRIPTION):

- `cli`: User-facing messages and error handling
- `data.table`: High-performance data manipulation
- `purrr`: Functional programming utilities
- `readxl`: Excel file reading for parameter extraction
- `rlang`: Metaprogramming utilities
- `sf`: Spatial data handling and GIS operations
- `tools`: File path utilities
- `utils`: General utilities (zip extraction, etc.)

**Suggested Dependencies:**

- `testthat` (>= 3.0.0): Testing framework

**Note**: The package does NOT use:

- `targets`: No automated pipeline (standard R package workflow)
- `shiny`: Interactive app not yet implemented (planned for future)
- `tidyverse`: Uses `data.table` for performance

## Understanding the Compliance Model

### Calculation Workflow

The compliance model calculates housing unit capacity through a 17-step process (Excel columns N through AF):

1. **Developable Area (Col N)**: Lot area minus exclusions, filtered by minimum lot size
2. **Net Developable Area (Col O)**: Accounts for manual overrides
3. **Exclusion Ratio (Col P-R)**: Proportion of excluded/sensitive land
4. **Open Space Requirement (Col S)**: District-level scalar (typically 20%)
5. **Required Open Space Area (Col T)**: Per-parcel open space calculation
6. **Parking Area (Col U)**: Space needed for required parking
7. **Building Footprint (Col V)**: Available area for building footprint
8. **Building Floor Area (Col W)**: Footprint × building height
9. **Units from Building Capacity (Col X)**: Floor area ÷ 1000 sq ft per unit
10. **Units from Density Limits (Col Y)**: Lot area × max units per acre
11. **Units from Lot Coverage (Col Z)**: Coverage × height ÷ 1000
12. **Units from Lot Area Requirement (Col AA)**: Lot area ÷ area per unit
13. **Units from FAR Limits (Col AB)**: Lot area × FAR ÷ 1000
14. **Adjusted Units with Max Cap (Col AC)**: Apply per-lot maximum
15. **Below Minimum Lot Flag (Col AD)**: Flag for graduated lot calculation
16. **Units from Graduated Lots (Col AE)**: Special calculation for small lots
17. **Final Unit Capacity (Col AF)**: Minimum of all applicable constraints

### Key Concepts

- **Zoning Parameters**: District-specific rules (min lot size, FAR, height, etc.)

  - Extracted from Excel "Checklist Parameters" sheet
  - Can be created manually for testing or new districts

- **District Assignment**: Parcels assigned to compliance districts using area-weighted intersection

  - Uses `sf::st_intersection()` to calculate actual spatial overlaps
  - Each parcel assigned to district containing majority of its area
  - Correctly handles boundary-straddling parcels and complex geometries (holes, narrow sections)
  - Parcels split across districts with no clear majority (<50% overlap) are flagged with warning
  - Aligns with MBTA regulation that "entire parcel capacity counts" in assigned district

- **Excluded/Sensitive Land**: Areas that cannot be developed

  - Public institutions, water bodies, wetlands, etc.
  - Measured via GIS overlay analysis or pre-calculated in parcel data

- **Station Area Overlap**: Parcels within 0.5 mile of MBTA stations

  - Affects compliance requirements for certain community types
  - Calculated using spatial intersection

- **Compliance Requirements**: Vary by community type
  - Rapid Transit, Commuter Rail, Adjacent, Adjacent Small Town
  - Minimum units, minimum acres, density requirements, etc.

### Excel Model Correspondence

Each R function maps to specific Excel model calculations:

- `calculate_developable_area()` → Column N
- `calculate_final_unit_capacity()` → Column AF
- `calculate_district_capacity()` → Full district-level aggregation
- `evaluate_compliance()` → Complete compliance checklist evaluation

See `dev/compliance_model_docs/Compliance_Model_User_Guide_Summary.md` for detailed Excel model documentation.

## Project Management

### Linear Integration

This project uses Linear for issue tracking and progress management. Key information:

- **Team:** Hidalgo Research (ID: `5f1d6e78-3907-430b-a5c4-d98b592374e7`)
- **Project:** MBTA Communities Compliance Model R Package (ID: `42a933c7-5ade-4c7d-8b06-14a60c0fea17`)
- **Assignee:** fdhidalgo / Daniel Hidalgo (ID: `93d9c10a-bad2-4baf-9fbd-47d65cfc4871`)

**Linear Usage Guidelines:**

- **ALWAYS** update Linear issues with progress comments as work proceeds
- Use Linear as the primary source of truth for development notes and decisions
- Create new issues for any significant features or bug fixes discovered
- Update issue status and add detailed comments when completing tasks
- Reference specific code locations in comments using `file_path:line_number` format

### Testing Guidelines

When developing new features:

1. **Write tests first** for calculation functions (TDD approach)
2. **Add Excel regression tests** if modifying existing calculations
3. **Create integration tests** for new pipeline steps
4. **Test edge cases**: NA values, empty geometries, boundary conditions
5. **Run full test suite** before commits: `devtools::test()`

**Target Coverage**: >95% for all calculation and GIS functions

### Common Development Tasks

**Adding a new calculation function:**

1. Add function to appropriate R file (e.g., `unit_capacity_calculations.R`)
2. Add `#' @export` to Roxygen documentation
3. Run `devtools::document()` to update NAMESPACE
4. Add unit tests in corresponding test file
5. Add Excel regression test if applicable
6. Update this CLAUDE.md if it represents a new calculation step

**Modifying Excel model correspondence:**

1. Update fixture generation if Excel structure changes
2. Regenerate fixtures: `source("tests/testthat/generate-fixtures.R")`
3. Run regression tests: `devtools::test(filter = "excel-regression")`
4. Document any intentional deviations from Excel model

**Troubleshooting test failures:**

1. Check for CRS issues (all spatial data must be EPSG:26986)
2. Verify NA handling (R preserves NA; Excel may convert to 0)
3. Review tolerance settings for numeric comparisons (typically 1 sq ft)
4. Check that test fixtures are up to date

## Development Workflow

### Reference Documentation

The `dev/compliance_model_docs/` directory contains critical reference materials:

- **`Compliance_Model_User_Guide.pdf`**: Official EOHLC Excel model documentation
- **`Compliance_Model_User_Guide_Summary.md`**: Comprehensive markdown summary of the user guide covering:
  - Model components and workflow
  - GIS calculation requirements
  - Excel model structure and logic
  - Unit capacity calculation methods
  - Compliance evaluation criteria
  - Integration guidance for R package development

### Using Proof of Concept as Reference

The `dev/proof_of_concept/` directory contains student-written prototype code. Use this as a reference for:

- Understanding the Excel model calculations
- Identifying required GIS operations
- Learning the compliance logic flow

**Do not directly copy** proof of concept code. Instead:

1. Understand the underlying algorithms
2. Rewrite using proper R package conventions
3. Add comprehensive input validation
4. Implement robust error handling
5. Create thorough unit tests

## Spatial Data Standards

### Coordinate Reference System

- **Always use EPSG:26986** (NAD83 Massachusetts State Plane, Mainland zone)
- All calculations assume planar coordinates in US Survey Feet
- Validate CRS on input: `stopifnot(sf::st_crs(data)$epsg == 26986)`
