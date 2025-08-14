# create_zoning_evaluator.R
# Evaluates overlay district for compliance

library(sf)
library(dplyr)

# Source required files (following 0730 model structure)
source("simulation/excel_model.R")
source("simulation/compliance_utils.R")
source("simulation/shapefile_utils.R")
source("simulation/calc_layers.R")

#' Create a zoning compliance evaluator function
#'
#' @param map A redist_map object containing Cambridge shapefile data
#' @param community_name Name of the community for compliance checking
#' @return Function that evaluates zoning compliance for overlay district only
create_zoning_evaluator <- function(map, community_name = "Cambridge") {
    
    shp_data <- map
    
    projected_data <- area_projection(shp_data, drop = FALSE) # this works (?)
    
    # doesn't work -> area_intersection tries to assign intersection values back to projected_data but doesn't match. 
    # "I have 100 houses, but only 60 are near train stations. I calculated areas for those 60, but now I'm trying to assign those 60 values back to all 100 houses."
    station_coverage <- area_intersection(projected_data, HALF_MILE_GDF)
            
    ddd_data <- gross_ddd(station_coverage, GDDD_GDF)
    
    # Compliance model setup - works
    community_params <- get_community_info(community_name)
    base_model <- new_compliance_model()
    base_model <- fill_sheet(base_model, "Introduction", list("I3" = community_name))
    base_model <- populate_sheet(base_model, "Introduction")
    base_model <- fill_sheet(base_model, "Checklist Parameters", community_params)
    base_model <- populate_sheet(base_model, "Checklist Parameters")
    
    # Return the evaluator function
    function(plan, map) {
        
        # Validate inputs
        if (length(plan) != nrow(shp_data)) {
            stop("Plan length doesn't match number of units in shapefile")
        }
        
        # Get unique districts
        unique_districts <- unique(plan)
        if (length(unique_districts) != 2) {
            stop("Plan must have exactly 2 districts")
        }
        
        # Determine which district is the overlay district
        # overlay district is typically the smaller one
        district_sizes <- table(plan)
        overlay_district <- as.numeric(names(district_sizes)[which.min(district_sizes)])
        
        # Extract overlay district indices
        overlay_indices <- which(plan == overlay_district)
        
        # Clone the base model for this evaluation
        model <- new_compliance_model()
        model$big_dict <- base_model$big_dict
        
        # Calculate overlay district metrics from preprocessed data
        overlay_total_area <- sum(ddd_data$area[overlay_indices], na.rm = TRUE)
        overlay_station_area <- sum(ddd_data$stn_area[overlay_indices], na.rm = TRUE)
        overlay_ddd_area <- sum(ddd_data$ddd_area[overlay_indices], na.rm = TRUE)
        overlay_gross_density_denominator <- sum(ddd_data$gross_density_denominator[overlay_indices], na.rm = TRUE)
        
        # Fill Checklist District ID sheet - overlay district only
        district_summary <- list(
            "B54" = overlay_district, # Overlay district ID
            "C54" = overlay_total_area,
            "D54" = overlay_station_area,
            "E54" = overlay_ddd_area,
            "C43" = "Y"
        )
        
        model <- fill_sheet(model, "Checklist District ID", district_summary)
        model <- populate_sheet(model, "Checklist District ID")
        
        # Prepare overlay district data with required columns
        overlay_df <- data.frame(
            A = 0:(length(overlay_indices) - 1),
            B = shp_data$LOC_ID[overlay_indices],
            C = shp_data$Address[overlay_indices],
            D = shp_data$Owner[overlay_indices],
            E = shp_data$UseCodes[overlay_indices],
            F = shp_data$UseDesc[overlay_indices],
            G = ifelse(shp_data$Transit[overlay_indices] == "Y", TRUE, FALSE),
            H = shp_data$Acres[overlay_indices],
            I = shp_data$SQFT[overlay_indices],
            J = shp_data$PublicInst[overlay_indices],
            K = shp_data$NonPubExc[overlay_indices],
            L = shp_data$Tot_Exclud[overlay_indices],
            M = shp_data$Tot_Sensit[overlay_indices],
            stringsAsFactors = FALSE
        )
        
        # Populate district sheet for overlay district only
        district_sheet_name <- paste0("District ", overlay_district)
        model <- populate_sheet(model, district_sheet_name, overlay_df)
        model <- populate_sheet(model, "Summary")
        
        # Evaluate compliance using is_good_zoning from excel_model
        # This evaluates the overlay district for zoning compliance
        if (is_good_zoning(model)) {
            return(0)  # Compliant
        } else {
            return(1)  # Non-compliant
        }
    }
}
