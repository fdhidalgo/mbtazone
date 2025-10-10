# Repository Guidelines

## Project Structure & Module Organization
- Six R modules in `R/` cover loaders, zoning parameters, GIS, unit capacity, the pipeline, and package docs—keep exports paired with roxygen blocks.
- Package datasets live in `data/`; curated reference inputs stay in `inst/extdata/` (`parcels/`, `statewide/`, `community_info.csv`).
- Tests sit in `tests/testthat/` with helpers, fixtures, and `_snaps/`; document fixture provenance in-file.
- `dev/` holds validation scripts, investigations, PRD, and reference material; treat `proof_of_concept/` as historical guidance only.

## Build, Test, and Development Commands
- `Rscript -e "devtools::load_all()"` for interactive development, `document()` to refresh man pages and `NAMESPACE`.
- `Rscript -e "devtools::test()"` (optionally `filter = "excel-regression"` or `"compliance-pipeline"`) and `check()` before every PR.
- `Rscript -e "devtools::install()"` when you need the package downstream.
- `quarto render dev/validation/validation_report.qmd` after compliance or GIS logic changes.

## Coding Style & Naming Conventions
- Tidyverse defaults: two-space indents, `snake_case` identifiers, Title Case headings in roxygen, constants in ALL_CAPS.
- Exported functions need complete roxygen (`@param`, `@return`, `@examples`, `@export`) and regenerated docs prior to commit.
- Prefer verb-first names (`calculate_district_capacity`) and explicit returns; reuse established `data.table` and `sf` idioms.

## Testing Guidelines
- Mirror source layout with focused `test_that()` blocks and named municipality scenarios.
- Sustain >95% coverage on calculation and GIS modules; add regression tests when touching Excel-aligned logic.
- When spreadsheet structure shifts, run `source("tests/testthat/generate-fixtures.R")`, rerun `devtools::test(filter = "excel-regression")`, then update `_snaps/`.
- Keep fixtures small, reproducible, and annotated with source metadata.

## Commit & Pull Request Guidelines
- Use imperative, concise commit subjects with ticket tags like `(HID-82)` when applicable.
- PRs must describe intent, list validation or test commands executed, share compliance metrics or screenshots for UI/report changes, and link the relevant Linear issue (include `file_path:line` references in the Linear comment).
- Call out required follow-up steps such as rerunning municipal validations or refreshing datasets.

## GIS & Data Standards
- Enforce EPSG:26986 (NAD83 / MA Mainland, US survey feet) on all spatial inputs; convert on load and assert CRS.
- Keep parcel shapefiles zipped under `inst/extdata/parcels/` with matching README notes; align exclusions, station buffers, and centroids before pipeline runs.
- Rerun `precompute_spatial_attributes()` whenever geometry schemas or tolerances change.

## Validation Workflow Tips
- After parcel assignment or capacity updates, execute `dev/validation/systematic_validation.R` and report success rates alongside code changes.
- Log unresolved quirks per municipality in `dev/investigations/` so the knowledge base stays current.
