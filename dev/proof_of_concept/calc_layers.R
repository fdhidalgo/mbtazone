# Provides the 'Key Calculation Layers' from 
# https://www.mass.gov/info-details/mbta-communities-compliance-model-components
# These are used in the pre-processing for the compliance model

library(sf)
library(zip)
source("simulation/shapefile_utils.R")

# Load HALF_MILE_GDF
shp_file <- file.path("data/Transit_Station_Areas_Half_Mile_Radius.shp")
gdf <- st_read(shp_file)
gdf <- st_set_crs(gdf, 26986)
HALF_MILE_GDF <- area_projection(gdf)

# Load GDDD_GDF
shp_file <- file.path("data/Density_Denominator_Deductions.shp")
gdf <- st_read(shp_file)
gdf <- st_set_crs(gdf, 26986)
GDDD_GDF <- area_projection(gdf)
