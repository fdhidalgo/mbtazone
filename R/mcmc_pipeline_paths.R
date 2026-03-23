# Resolve machine-specific directories for the MCMC / targets pipeline via env vars.
# See inst/targets/.Renviron.example

#' Paths for MCMC targets pipeline (from environment)
#'
#' Reads \env{MBTAZONE_DATA_ROOT} and \env{MBTAZONE_RIGHT_OF_WAY} (required).
#' Reads \env{MBTAZONE_PARCELS_SUBDIR} for the parcel-ZIP subfolder under
#' `data_root`; defaults to `"land_record_shapefiles/basic"` when unset.
#' Set variables in the package root `.Renviron` or `~/.Renviron`
#' (see `inst/targets/.Renviron.example`).
#'
#' @return Named list with `data_root`, `right_of_way`, and `parcels_subdir`.
#' @export
mbtazone_pipeline_paths <- function() {
  data_root <- Sys.getenv("MBTAZONE_DATA_ROOT", unset = "")
  right_of_way <- Sys.getenv("MBTAZONE_RIGHT_OF_WAY", unset = "")
  parcels_subdir <- Sys.getenv(
    "MBTAZONE_PARCELS_SUBDIR",
    unset = "land_record_shapefiles/basic"
  )

  if (!nzchar(data_root)) {
    cli::cli_abort(c(
      "Missing {.envvar MBTAZONE_DATA_ROOT}.",
      "i" = "Set it to an absolute path to your MBTA data tree (see {.file inst/targets/.Renviron.example} for a template)."
    ))
  }
  if (!nzchar(right_of_way)) {
    cli::cli_abort(c(
      "Missing {.envvar MBTAZONE_RIGHT_OF_WAY}.",
      "i" = "Set it to the absolute path of {.file Excluded_Land_Right_of_Way.shp}."
    ))
  }

  list(
    data_root = data_root,
    right_of_way = right_of_way,
    parcels_subdir = parcels_subdir
  )
}
