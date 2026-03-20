# A helper script to run a single district and store the targets output in the correponding folder.
# Requires MBTAZONE_DATA_ROOT and MBTAZONE_RIGHT_OF_WAY (see inst/targets/.Renviron.example).
# I.e. Norwood will be stored in _targets_Norwood. This is for consistancy with run_all_districts.R

library(targets)

pkg_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
r_bin <- file.path(R.home("bin"), "R")
install_status <- system2(
  r_bin,
  c("CMD", "INSTALL", "--no-multiarch", shQuote(pkg_root))
)
if (!identical(install_status, 0L)) {
  stop("Failed to install current mbtazone package before running targets.")
}

district_name <- "Norwood"
district_type <- "commuter_rail" #One of: "rapid_transit", "commuter_rail", "adjacent", and "adjacent_small_town
store <- paste0("ext/_targets_", gsub(" ", "_", district_name))

Sys.setenv(DISTRICT_NAME = district_name, DISTRICT_TYPE = district_type)

tar_make(
  script = "inst/targets/_targets.R",
  store = store
)

#targets::tar_make(script = "inst/targets/_targets.R", store = store)
#   targets::tar_load(object_name, store = store)
#   targets::tar_destroy(store = store)
