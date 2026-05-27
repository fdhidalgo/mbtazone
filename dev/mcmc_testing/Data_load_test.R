# Temp: for testing use the above functions to load the Norwood data

norwood_paths <- get_district_paths("Acton", data_root = '../../data')
getwd()
norwood_data <- load_district_data(
  district_name = "Acton",
  parcels = norwood_paths$parcels,
  district = norwood_paths$district,
  excel_model = norwood_paths$excel_model,
  right_of_way = "../../data/Right_of_Way/Excluded_Land_Right_of_Way.shp"
)

norwood_data_og <- load_norwood_data(
  data_root = '../../data',
  row_path = "../../data/Right_of_Way/Excluded_Land_Right_of_Way.shp"
)

identical(norwood_data, norwood_data_og)

all.equal(norwood_data, norwood_data_og)

norwood_paths <- get_district_paths("Acton", data_root = '../../data')

district_name = "Acton"
parcels = norwood_paths$parcels
district = norwood_paths$district
excel_model = norwood_paths$excel_model
right_of_way = "../../data/Right_of_Way/Excluded_Land_Right_of_Way.shp"

paths <- list(
  parcels = parcels,
  district = district,
  excel_model = excel_model,
  right_of_way = right_of_way
)

# Step 1: Load District Parcels
parcels_sf <- load_municipality(
  shapefile = paths$parcels,
  community_name = district_name,
  projection = 26986,
  validate = TRUE
)

# Step 2: Load MBTA District Boundary
district_path = paths$district
# Handle zip files
if (tools::file_ext(district_path) == "zip") {
  # Create unique temp directory for this extraction
  temp_dir <- file.path(tempdir(), basename(tempfile()))
  dir.create(temp_dir, showWarnings = FALSE)
  utils::unzip(district_path, exdir = temp_dir)

  # Find the .shp file in extracted contents
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp_files) == 0) {
    cli::cli_abort("No .shp file found in zip archive: {.file {district_path}}")
  }
  if (length(shp_files) > 1) {
    cli::cli_warn("Multiple .shp files found, using: {.file {basename(shp_files[1])}}")
  }

  district_path <- shp_files[1]
} else {
  district_path <- shapefile
}

district_sf <- sf::st_read(district_path, quiet = TRUE)
if (
  is.na(sf::st_crs(district_sf)$epsg) ||
  sf::st_crs(district_sf)$epsg != 26986
) {
  district_sf <- sf::st_transform(district_sf, 26986)
}
