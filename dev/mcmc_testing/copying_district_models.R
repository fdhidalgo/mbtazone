library(stringr)

source_dir <- "/home/k.conyngham/data/mbta_district_models"
target_dir <- "/home/k.conyngham/new_data/mbta_district_models"

# District folders
district_dirs <- list.dirs(source_dir, full.names = TRUE, recursive = FALSE)
district_names <- basename(district_dirs)

# Excel files already in new folder
existing_files <- list.files(target_dir, pattern = "\\.xlsx?$", full.names = FALSE)

# Extract district names
existing_districts <- str_trim(sub(" - CM.*$", "", existing_files))

# Identify missing districts
missing_districts <- setdiff(district_names, existing_districts)

# Track which districts get copied
copied_districts <- c()

for (d in missing_districts) {

  files_to_copy <- list.files(
    file.path(source_dir, d),
    pattern = "\\.xlsx?$",
    full.names = TRUE
  )

  if (length(files_to_copy) > 0) {
    file.copy(files_to_copy, target_dir)
    copied_districts <- c(copied_districts, d)
  }
}

# Final list
copied_districts
