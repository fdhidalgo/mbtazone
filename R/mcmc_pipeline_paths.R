# Resolve machine-specific directories for the MCMC / targets pipeline via env vars.
# See inst/targets/.Renviron.example

#' Paths for MCMC targets pipeline (from environment)
#'
#' Reads \env{MBTAZONE_PIPELINE_DATA} (path to the `mbta_pipeline_data/`
#' directory of per-municipality GeoPackages) and \env{MBTAZONE_RIGHT_OF_WAY}
#' (path to the statewide right-of-way shapefile). Both are required.
#' Set variables in the package root `.Renviron` or `~/.Renviron`
#' (see `inst/targets/.Renviron.example`).
#'
#' @return Named list with `pipeline_data_dir` and `right_of_way`.
#' @export
mbtazone_pipeline_paths <- function() {
  pipeline_data_dir <- Sys.getenv("MBTAZONE_PIPELINE_DATA", unset = "")
  right_of_way      <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY",  unset = "")

  if (!nzchar(pipeline_data_dir)) {
    cli::cli_abort(c(
      "Missing {.envvar MBTAZONE_PIPELINE_DATA}.",
      "i" = "Set it to the absolute path of the mbta_pipeline_data directory (see {.file inst/targets/.Renviron.example})."
    ))
  }
  if (!nzchar(right_of_way)) {
    cli::cli_abort(c(
      "Missing {.envvar MBTAZONE_RIGHT_OF_WAY}.",
      "i" = "Set it to the absolute path of {.file Excluded_Land_Right_of_Way.shp}."
    ))
  }

  list(
    pipeline_data_dir = pipeline_data_dir,
    right_of_way      = right_of_way
  )
}
