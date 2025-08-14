
## Product Requirements Document: `mbtazone` R Package

### 1. Executive Summary

The `mbtazone` package will provide a reproducible, transparent, and user-friendly implementation of Massachusetts' MBTA Communities Act compliance model, replacing the current Excel-based workflow with robust R code and adding interactive planning capabilities through Shiny.

### 2. Problem Statement

**Current State:**
- Municipalities use a complex Excel model with manual GIS workflows to assess zoning compliance
- The process is error-prone, non-reproducible, and lacks version control
- No interactive tools exist for exploring alternative zoning configurations
- The existing proof-of-concept has critical bugs (area intersection failures) and lacks proper testing

**Target State:**
- Automated, reproducible compliance assessment pipeline
- Interactive tools for zoning scenario planning
- Full parity with official Excel model calculations
- Academic-grade documentation and validation

### 3. Core Requirements

#### 3.1 Compliance Engine

**Priority: P0 (Critical)**

- **Calculation Methods**: Implement all 7 unit capacity calculation methods from the Excel model:
  - Building envelope method (base calculation)
  - Dwelling units per acre limit
  - Maximum lot coverage limit
  - Lot area per dwelling unit limit
  - FAR limit
  - Maximum units per lot limit
  - Non-conforming lot identification

- **Compliance Criteria**: Evaluate all 6 requirements:
  1. Minimum multi-family unit capacity
  2. Minimum land area
  3. Developable station area (where applicable)
  4. Gross density (≥15 units/acre)
  5. Unit capacity ratio within station areas
  6. Land area ratio within station areas

- **Community Categories**: Support all four categories with appropriate parameters:
  - Rapid Transit
  - Commuter Rail
  - Adjacent Community
  - Adjacent Small Town

#### 3.2 Data Processing Pipeline

**Priority: P0**

```r
# Example API
library(mbtazone)

# Load municipal data
cambridge <- load_municipality("Cambridge", 
                              shapefile = "path/to/parcels.shp",
                              projection = "EPSG:26986")

# Define zoning parameters
zoning_params <- set_zoning_parameters(
  min_lot_size = 5000,
  building_height = 4,
  open_space_pct = 0.20,
  parking_ratio = 1.0,
  far = 2.0
)

# Evaluate compliance
results <- evaluate_compliance(
  municipality = cambridge,
  districts = district_boundaries,
  zoning = zoning_params
)
```

#### 3.3 GIS Operations

**Priority: P0**

- **Required Functions**:
  - `calculate_station_intersection()`: Fixed implementation of buggy area calculation
  - `calculate_density_denominator()`: Handle deductions correctly
  - `validate_contiguity()`: Check 50% contiguity requirement
  - `identify_excluded_land()`: Process all exclusion categories
  - `calculate_sensitive_land()`: Optional overlay analysis

- **Performance Requirements**:
  - Process 20,000 parcels in <30 seconds
  - Support parallel processing for multiple districts
  - Cache intermediate calculations

#### 3.4 Interactive Shiny Application

**Priority: P1 (High)**

```r
# Launch interactive planner
launch_compliance_app()
```

**Features**:
- **Drawing Tools**: 
  - Freehand district boundary drawing
  - Snap-to-parcel-boundary mode
  - Multi-district support with different zoning parameters
  
- **Real-time Feedback**:
  - Live compliance metrics as boundaries change
  - Heat maps showing unit capacity by parcel
  - Transit station area overlays
  - Excluded/sensitive land visualization

- **Scenario Comparison**:
  - Save multiple scenarios
  - Side-by-side comparison view
  - Export scenarios to reproducible R code

#### 3.5 Validation & Testing Framework

**Priority: P0**

- **Regression Testing**:
  ```r
  # Validate against Excel model outputs
  validate_against_excel(
    municipality = "Cambridge",
    excel_results = "path/to/excel_output.xlsx",
    tolerance = 0.001
  )
  ```

- **Unit Tests**: 
  - 100% coverage for calculation functions
  - Edge case testing (no transit, all water, single parcel)
  - Projection transformation accuracy

- **Integration Tests**:
  - Full pipeline for each community category
  - Multi-district scenarios
  - Override handling

### 4. Data Requirements

#### 4.1 Input Data Specifications

- **Shapefile Requirements**:
  ```r
  required_columns <- c(
    "LOC_ID", "Address", "Owner", "UseCodes", 
    "Transit", "Acres", "SQFT", "PublicInst",
    "NonPubExc", "Tot_Exclud", "Tot_Sensit"
  )
  ```

- **Calculation Layers**:
  - Transit station areas (0.5-mile buffers)
  - Density denominator deductions
  - Excluded land categories
  - Sensitive land overlays

#### 4.2 Output Specifications

```r
# Compliance report structure
report <- list(
  summary = list(
    compliant = TRUE/FALSE,
    unit_capacity = 1234,
    gross_density = 16.5,
    land_area = 143.4
  ),
  
  by_district = data.frame(
    district_id = c(1, 2),
    parcels = c(450, 380),
    units = c(750, 484),
    density = c(18.2, 14.1)
  ),
  
  parcel_detail = sf_object_with_calculations,
  
  validation = list(
    min_capacity_met = TRUE,
    min_area_met = TRUE,
    station_area_met = TRUE,
    density_met = TRUE
  )
)
```

### 5. Technical Architecture

#### 5.1 Package Structure

```
mbtazone/
├── R/
│   ├── compliance_engine.R    # Core calculations
│   ├── gis_operations.R       # Spatial functions
│   ├── data_loaders.R         # Import utilities
│   ├── validators.R           # Input validation
│   ├── visualizations.R       # Plotting functions
│   └── shiny_modules.R        # Interactive components
├── inst/
│   ├── shiny/                 # Shiny app files
│   ├── extdata/              # Sample data
│   └── excel_validation/      # Excel comparison tools
├── tests/
│   ├── testthat/
│   └── validation/           # Excel parity tests
├── vignettes/
│   ├── getting_started.Rmd
│   ├── compliance_guide.Rmd
│   └── shiny_tutorial.Rmd
└── data/
    └── community_parameters.rda
```

#### 5.2 Dependencies

```r
# DESCRIPTION file
Imports:
  sf (>= 1.0),
  dplyr (>= 1.1),
  tidyr,
  purrr,
  rlang,
  cli,
  units
Suggests:
  shiny (>= 1.7),
  leaflet,
  leaflet.extras,
  DT,
  plotly,
  testthat (>= 3.0),
  knitr,
  rmarkdown
```

### 6. Performance Requirements

- **Calculation Speed**: <2 seconds for single district evaluation
- **Memory Usage**: <2GB for typical municipality (15,000 parcels)
- **Shiny Responsiveness**: <100ms for boundary updates
- **Scalability**: Support municipalities up to 50,000 parcels

### 7. Documentation Requirements

- **Function Documentation**: Roxygen2 for all exported functions
- **Vignettes**:
  1. "Getting Started with mbtazone"
  2. "Understanding Compliance Calculations"
  3. "Interactive Planning with Shiny"
  4. "Validating Against Excel Models"
  
- **Academic Paper**: Methods paper describing algorithms and validation

### 8. Quality Assurance

#### 8.1 Automated Testing

```yaml
# GitHub Actions CI/CD
- R CMD check
- Code coverage >95%
- Excel validation suite
- Performance benchmarks
```

#### 8.2 Validation Criteria

- **Numerical Accuracy**: Within 0.1% of Excel model
- **Legal Compliance**: Reviewed by EOHLC staff
- **Edge Cases**: Handle all 178 MBTA communities
- **Reproducibility**: Identical results with set.seed()

### 9. Release Plan

#### Phase 1: Core Engine (Weeks 1-4)
- Fix critical bugs from proof-of-concept
- Implement calculation pipeline
- Build test framework
- Validate against Cambridge test case

#### Phase 2: Full Compliance (Weeks 5-8)
- Add all community categories
- Implement all calculation methods
- Complete Excel validation suite
- Documentation and vignettes

#### Phase 3: Interactive Tools (Weeks 9-12)
- Basic Shiny application
- Drawing and editing tools
- Scenario comparison
- Export functionality

#### Phase 4: Polish & Release (Weeks 13-16)
- Performance optimization
- Academic paper draft
- CRAN submission preparation
- User testing with 3-5 municipalities

### 10. Success Metrics

- **Adoption**: Used by >10 Massachusetts municipalities within 6 months
- **Accuracy**: 100% parity with Excel model calculations
- **Performance**: 5x faster than manual Excel process
- **Academic Impact**: Published methods paper, >20 citations within 2 years
- **Code Quality**: CRAN acceptance, >95% test coverage

### 11. Risk Mitigation

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Excel model changes | Medium | High | Version control, parameterize rules |
| GIS data inconsistencies | High | Medium | Robust validation, clear error messages |
| Performance issues with large municipalities | Low | Medium | Spatial indexing, parallel processing |
| Legal interpretation disputes | Low | High | Close collaboration with EOHLC |

### 12. Future Enhancements (Post-v1.0)

- **Machine Learning**: Optimal district boundary suggestions
- **Optimization**: Automated compliance-maximizing algorithms
- **Regional Analysis**: Multi-municipality coordination tools
- **Policy Simulation**: What-if analysis for regulation changes
- **API Service**: RESTful API for web integration

### 13. Example Usage

```r
library(mbtazone)
library(sf)

# Load and prepare data
cambridge <- prepare_municipality(
  parcels = "data/cambridge_parcels.shp",
  community_type = "rapid_transit"
)

# Define district boundaries (or use interactive tool)
districts <- draw_districts_interactive(cambridge)

# Set zoning parameters
zoning <- zoning_parameters(
  min_lot_size_sf = 5000,
  height_stories = 4,
  open_space_pct = 0.20,
  parking_per_unit = 1.0,
  far = 2.0
)

# Run compliance check
results <- check_compliance(
  municipality = cambridge,
  districts = districts,
  zoning = zoning,
  verbose = TRUE
)

# Generate report
generate_compliance_report(
  results,
  output_dir = "reports/",
  format = c("html", "pdf", "xlsx")
)

# Launch interactive planner
if (interactive()) {
  plan_districts(cambridge)
}
```

This PRD provides a comprehensive roadmap for building a robust, academic-quality R package that addresses both the technical debt in the current proof-of-concept and adds significant value through interactive planning tools. The phased approach allows for iterative development while ensuring core compliance calculations are validated early.