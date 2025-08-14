# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The `mbtazone` package implements Massachusetts' MBTA Communities Act compliance model in R, providing automated zoning compliance assessment and interactive planning tools. The package replaces manual Excel-based workflows with robust R code and adds Shiny-based interactive capabilities.

## Development Commands

### Standard R Package Development
```r
# Load and develop the package
devtools::load_all()

# Build and check package
devtools::check()
devtools::document()
devtools::install()

# Run tests
devtools::test()

```

## Code Style and Conventions

### R Package Standards
- Use `snake_case` for function and variable names
- All exported functions must have complete Roxygen2 documentation
- Include `@examples` in function documentation
- Use `testthat` for all testing with >95% coverage target
- Follow tidyverse style guide
- Use data.table for all data manipulation

### Spatial Data Conventions
- Always use EPSG:26986 (NAD83 Massachusetts State Plane) for calculations
- Validate CRS before processing: `sf::st_crs(data)$epsg == 26986`
- Use `units` package for explicit area/distance units
- Prefer vectorized operations over loops for GIS processing

## Package Dependencies



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

### Testing Framework (per PRD)
- Use `testthat` (>= 3.0) with >95% code coverage
- **Unit Tests**: 100% coverage for calculation functions
- **Integration Tests**: Full pipeline for each community category
- **Regression Tests**: `validate_against_excel()` for Excel model parity  
- Edge case testing (no transit, all water, single parcel)



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

### Avoid Escaping Issues


#### ✅ **RECOMMENDED: Use Write Tool for Complex Scripts**
```bash
# BEST: Create R scripts with Write tool (avoids ALL escape issues)
Write /tmp/analysis.R
Rscript /tmp/analysis.R
```

#### ✅ **RECOMMENDED: Direct Commands for Simple Operations**
```bash
# GOOD: Simple operations with -e flag
R -e "library(targets); tar_load('data'); cat('Records:', nrow(data))"
```

#### ✅ **RECOMMENDED: Alternative Syntax to Avoid Special Characters**
```r
# AVOID: result <- data[!is.na(column)]     # ! causes bash issues
# USE:   result <- data[is.na(column) == FALSE]
# OR:    result <- data[complete.cases(column)]
# OR:    result <- subset(data, is.na(column) == FALSE)
```

#### ✅ **RECOMMENDED: Incremental Testing Approach**
```bash
# Instead of one 30-line diagnostic script, use 3 focused scripts:
Write /tmp/step1_load.R      # Test data loading only
Write /tmp/step2_process.R   # Test processing only  
Write /tmp/step3_analyze.R   # Test analysis only
```

#### ❌ **AVOID: Bash Heredocs with Special Characters**
```bash
# PROBLEMATIC: Even quoted heredocs process some escapes
cat > /tmp/script.R <<'EOF'
data[!is.na(x)]  # This ! can still cause issues
EOF
```

#### **Why These Practices Matter**
- **Escape Character Issues**: `!`, `$`, `\` in bash heredocs cause failures
- **Debug Difficulty**: Large failing scripts are hard to troubleshoot
- **Reliability**: Write tool and simple -e commands eliminate bash interaction issues