# Provides utils for pre-processing information from shapefiles
# to use the compliance model

library(sf)
library(dplyr)

#' Given an sf object with polygons, return an sf object that calculates
#' area based on NAD83 projection (EPSG:26986 for Massachusetts)
area_projection <- function(gdf, drop = TRUE) {
    
    # Transform to NAD83 Massachusetts State Plane (EPSG:26986)
    gdf <- st_transform(gdf, crs = 26986)
    
    # Calculate area in square meters, then convert to acres
    # 1 acre = 4046.8564224 square meters
    gdf$area <- as.numeric(st_area(gdf)) / 4046.8564224
    
    if (drop) {
        gdf <- gdf %>%
            select(geometry, area)
    }
    
    return(gdf)
}


#' Given an sf object and area field name, return the total area
total_area <- function(gdf, area_field = "area") {
    return(sum(gdf[[area_field]], na.rm = TRUE))
}

#' Calculate area intersection
#' 
#' Given two sf objects, returns the NAD83 projection of where the two
#' intersect, based on gdf1. The new station area is recorded into 'stn_area'
area_intersection <- function(gdf1, gdf2) {
    
    intersection <- st_intersection(gdf1, gdf2)
    intersection$intersection_area <- as.numeric(st_area(intersection))

    intersection_projected <- area_projection(intersection)
    gdf1$stn_area <- intersection_projected$area
    
    return(gdf1)
}

#' Gross density denominator deduction
gross_ddd <- function(gdf, gddd_gdf = NULL) {

    gddd_gdf <- GDDD_GDF
    
    gddd_gdf_transformed <- st_transform(gddd_gdf, st_crs(gdf))
    
    # Calculate intersection between district and deduction areas
    gdf$ddd_area <- 0  # Initialize deduction area
    
    # For each district polygon, calculate total deduction area
    for (i in seq_len(nrow(gdf))) {
        # Find intersections with deduction layer
        intersection <- try({
            st_intersection(gdf[i, ], gddd_gdf_transformed)
        }, silent = TRUE)
        
        if (inherits(intersection, "try-error") || nrow(intersection) == 0) {
            # No intersection found
            gdf$ddd_area[i] <- 0
        } else {
            # Calculate total area of intersections
            intersection_projected <- area_projection(intersection, drop = FALSE)
            gdf$ddd_area[i] <- sum(intersection_projected$area, na.rm = TRUE)
        }
    }
    
    # Gross Density Denominator = Total District Area - Deduction Area
    gdf$gross_density_denominator <- pmax(0, gdf$area - gdf$ddd_area)
    
    return(gdf)
}

