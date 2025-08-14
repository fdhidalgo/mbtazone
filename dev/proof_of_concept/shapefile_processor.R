# Once zoned, this file is used to pre-process stuff and
# get information to use in the compliance model

library(sf)
library(dplyr)
library(zip)
library(jsonlite)

source("simulation/shapefile_utils.R")
source("simulation/calc_layers.R")

EMPTY_DF <- data.frame(
    A = character(0), B = character(0), C = character(0), D = character(0),
    E = character(0), F = character(0), G = character(0), H = character(0),
    I = character(0), J = character(0), K = character(0), L = character(0),
    M = character(0),
    stringsAsFactors = FALSE
)

column_name_mapper <- list(
    "index" = "A",
    "LOC_ID" = "B",
    "Address" = "C",
    "Owner" = "D",
    "UseCodes" = "E",
    "UseDesc" = "F",
    "TRANSIT" = "G",
    "ACRES" = "H",
    "SQFT" = "I",
    "PublicInst" = "J",
    "NonPubExc" = "K",
    "Tot_Exclud" = "L",
    "Tot_Sensit" = "M"
)

#' Divide sf object into zones
#' 
#' Given an sf object, return a new sf object that uses zoning vector
#' to combine polygons into zones
divide_into_zones <- function(gdf, zoning) {
    gdf$zone_id <- zoning
    
    zoned_gdf <- gdf %>%
        group_by(zone_id) %>%
        summarise(geometry = st_union(geometry)) %>%
        ungroup()
    rownames(zoned_gdf) <- NULL

    zoned_gdf <- area_projection(zoned_gdf)
    
    return(zoned_gdf)
}

#' Create district sheets for the compliance model
return_district_sheets <- function(zoning_gdf, land_map_gdf) {
    district_sheets <- list()
    for (i in 1:5) {
        district_sheets[[paste0("District ", i)]] <- EMPTY_DF
    }
    
    for (idx in 1:nrow(zoning_gdf)) {
        district_gdf <- zoning_gdf[idx, , drop = FALSE]
        overlap <- st_intersection(land_map_gdf, district_gdf)

        overlap_no_geometry <- overlap %>%
            st_drop_geometry() %>%
            select(-any_of(c("area", "stn_area", "ddd"))) %>%
            mutate(index = row_number() - 1) %>%
            select(index, everything())
        
        for (old_name in names(column_name_mapper)) {
            if (old_name %in% names(overlap_no_geometry)) {
                names(overlap_no_geometry)[names(overlap_no_geometry) == old_name] <- column_name_mapper[old_name]
            }
        }

        sheet_name <- paste0("District ", idx)
        district_sheets[[sheet_name]] <- overlap_no_geometry
    }
    
    return(district_sheets)
}

#' Create district summary information for the compliance model
return_district_summaries <- function(final_zoning_gdf) {
    final_zoning_gdf <- area_projection(final_zoning_gdf, drop = FALSE)
    
    sheet_name <- "Checklist District ID"
    ret <- list()
    ret[[sheet_name]] <- list()
    
    for (idx in 1:nrow(final_zoning_gdf)) {
        row <- final_zoning_gdf[idx, ]
        
        ret[[sheet_name]][[paste0("B", 53 + idx)]] <- idx # District name/number
        ret[[sheet_name]][[paste0("C", 53 + idx)]] <- row$area
        ret[[sheet_name]][[paste0("D", 53 + idx)]] <- row$stn_area
        ret[[sheet_name]][[paste0("E", 53 + idx)]] <- row$ddd
    }
    
    return(ret)
}

#' Save the final zoning shapefile as a zip file
save_result_shp_file <- function(final_zoning_gdf, name = "output") {
    temp_dir <- "temp_shapefiles"
    
    if (!dir.exists(temp_dir)) {
        dir.create(temp_dir)
    }
    
    output_shapefile <- file.path(temp_dir, "zoned.shp")
    st_write(final_zoning_gdf, output_shapefile, delete_dsn = TRUE, quiet = TRUE)

    zip_output_path <- paste0(name, ".zip")
    shp_files <- list.files(temp_dir, pattern = "zoned\\.", full.names = TRUE)
    zip::zip(zip_output_path, files = shp_files, mode = "cherry-pick")
    unlink(temp_dir, recursive = TRUE)
}

#' Process shapefile - ACTUAL CODE STARTS
process_shapefile <- function(city_shp_file_path, zoning, output_filename = "output") {
    
    land_map_gdf <- st_read(city_shp_file_path)
    # land_map_gdf <- land_map_gdf %>% 
    #   filter(Owner == "MASSACHUSETTS INSTITUTE OF TECHNOLOGY")
    
    gdf <- land_map_gdf
    final_zoning_gdf <- gross_ddd(area_intersection(divide_into_zones(gdf, zoning), HALF_MILE_GDF))
    
    save_result_shp_file(final_zoning_gdf, output_filename)
    
    return(list(
        return_district_summaries(final_zoning_gdf),
        return_district_sheets(final_zoning_gdf, land_map_gdf)
    ))
}
