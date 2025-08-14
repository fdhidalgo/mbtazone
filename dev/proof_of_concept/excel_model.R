# Excel Compliance Model in R
# Corresponds to the MBTA Excel compliance model from
# https://www.mass.gov/info-details/mbta-communities-compliance-model-components

library(tidyverse)
library(jsonlite)

INTRODUCTION <- "Introduction"
DISTRICT_ID <- "Checklist District ID"
PARAMETERS <- "Checklist Parameters"
SUMMARY <- "Summary"

EMPTY_DF <- data.frame(
    A = character(0), B = character(0), C = character(0), D = character(0),
    E = character(0), F = character(0), G = character(0), H = character(0),
    I = character(0), J = character(0), K = character(0), L = character(0),
    M = character(0),
    stringsAsFactors = FALSE
)

create_compliance_model <- function() {
    model <- new.env(parent = emptyenv())
    model$big_dict <- list(
        "Introduction" = list(),
        "Checklist District ID" = list(),
        "Checklist Parameters" = list(),
        "District 1" = list(),
        "District 2" = list(),
        "District 3" = list(),
        "District 4" = list(),
        "District 5" = list(),
        "Zoning Input Summary" = list(),
        "Summary" = list()
    )
    
    model$fill_sheet <- function(sheet_name, cell_map) {

        # TODO: ^ add checks to fail for extra cells (maybe the wrong sheet is
        #       being called somewhere)
        #       also add a failure condition if the data is the wrong type

        # TODO: add checks to make sure district info being filled out across
        #       different sheets matches up with districts defined
        
        fill_introduction <- function(cell_map) {
            # Sets municipality name at I3
            model$big_dict[[INTRODUCTION]][["I3"]] <- cell_map[["I3"]]
        }
        
        # Validates and stores "C43", processes from B54 to E58
        fill_checklist_district_id <- function(cell_map) {
            
            cat("here\n")
            if (!(cell_map[["C43"]] %in% c("Y", "N"))) {
                stop("Expected C43 to be 'Y' or 'N'")
            }
            model$big_dict[[DISTRICT_ID]][["C43"]] <- cell_map[["C43"]]
            for (x in c("B", "C", "D", "E")) {
                for (y in c(54, 55, 56, 57, 58)) {
                    cell <- paste0(x, y)
                    if (cell %in% names(cell_map)) {
                        model$big_dict[[DISTRICT_ID]][[cell]] <- cell_map[[cell]]
                    } else {
                        model$big_dict[[DISTRICT_ID]][[cell]] <- NULL
                    }
                }
            }
        }
        
        fill_checklist_parameters <- function(cell_map) {
            # Parameter rows that need to be filled
            for (row in c(16, 22, 24, 25, 35, 43, 58, 60, 86, 101, 102, 103)) {
                cell <- paste0("E", row)
                model$big_dict[[PARAMETERS]][[cell]] <- cell_map[[cell]]
            }
            
            # Fill other district columns (H, K, N, Q)
            for (col in c("H", "K", "N", "Q")) {
                for (row in c(16, 22, 24, 25, 35, 43, 58, 60, 86, 101, 102, 103)) {
                    cell <- paste0(col, row)
                    if (cell %in% names(cell_map)) {
                        model$big_dict[[PARAMETERS]][[cell]] <- cell_map[[cell]]
                    } else {
                        model$big_dict[[PARAMETERS]][[cell]] <- 0
                    }
                }
            }
        }
        
        fill_district_i <- function(i, cell_map) {
            # Fill in the 'District i' sheet based on the values in cell_map
            return(invisible(NULL))
        }
        
        # dispatcher logic
        if (sheet_name == "Introduction") {
            fill_introduction(cell_map)
        } else if (sheet_name == "Checklist District ID") {
            fill_checklist_district_id(cell_map)
        } else if (sheet_name == "Checklist Parameters") {
            fill_checklist_parameters(cell_map)
        } else if (strsplit(sheet_name, " ")[[1]][1] == "District") {
            district_num <- as.integer(strsplit(sheet_name, " ")[[1]][2])
            fill_district_i(district_num, cell_map)
        } else {
            stop(paste("Sheet", sheet_name, "not found"))
        }
        
    }
    
    model$populate_sheet <- function(sheet_name, df = EMPTY_DF) {
        
        populate_introduction <- function() {
            model$big_dict[["Introduction"]] <- c(model$big_dict[["Introduction"]], get_community_info(model$big_dict[["Introduction"]][["I3"]]))
        }
        
        populate_checklist_district_id <- function() {
            # Calculate sums for columns C, D, E from rows 54-58
            for (col in c("C", "D", "E")) {
                values <- sapply(54:58, function(row) {
                    cell_val <- model$big_dict[[DISTRICT_ID]][[paste0(col, row)]]
                    if (is.null(cell_val)) NA else cell_val
                })
                # sum
                model$big_dict[[DISTRICT_ID]][[paste0(col, "59")]] <- sum(values, na.rm = TRUE)
            }
            
            # Calculate E70, E71, E72, E74
            model$big_dict[[DISTRICT_ID]][["E70"]] <- model$big_dict[[INTRODUCTION]][["I7"]] * model$big_dict[[INTRODUCTION]][["I9"]]
            model$big_dict[[DISTRICT_ID]][["E71"]] <- model$big_dict[[DISTRICT_ID]][["D59"]]
            model$big_dict[[DISTRICT_ID]][["E72"]] <- model$big_dict[[DISTRICT_ID]][["E70"]] - model$big_dict[[DISTRICT_ID]][["D59"]]
            model$big_dict[[DISTRICT_ID]][["E74"]] <- model$big_dict[[INTRODUCTION]][["I6"]] * model$big_dict[[INTRODUCTION]][["I9"]]
        }
        
        populate_checklist_parameters <- function() {
            return(invisible(NULL))
        }
        
        populate_district_i <- function(i, df) {
            # TODO: check rounding for AC and X columns
            district <- paste0("District ", i)
            parameter_col_map <- list("1" = "E", "2" = "H", "3" = "K", "4" = "N", "5" = "Q")
            parameter_sheet_col <- parameter_col_map[[as.character(i)]]

            df <- apply_district_funcs(
                df,
                water_included = model$big_dict[[DISTRICT_ID]][["C43"]],
                max_units_per_lot = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "16")]],
                min_lot_size = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "22")]],
                base_min_lot_size = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "24")]],
                additional_lot_SF = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "25")]],
                building_height = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "35")]],
                FAR = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "43")]],
                max_lot_coverage = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "58")]],
                min_required_open_space = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "60")]],
                parking_spaces_per_dwelling_unit = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "86")]],
                lot_area_per_dwelling_unit = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "101")]],
                max_dwelling_units_per_acre = model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "102")]]
            )
            
            model$big_dict[[district]][["B9"]] <- model$big_dict[[DISTRICT_ID]][[paste0("E", 54 + i - 1)]]
            model$big_dict[[district]][["B10"]] <- nrow(df)
            model$big_dict[[district]][["B11"]] <- sum(df[["H"]], na.rm = TRUE)
            model$big_dict[[district]][["B12"]] <- sum(df[["W"]], na.rm = TRUE)
            model$big_dict[[district]][["B13"]] <- sum(df[["AF"]], na.rm = TRUE)
            model$big_dict[[district]][["B14"]] <- {
                b9_val <- model$big_dict[[district]][["B9"]]
                b13_val <- model$big_dict[[district]][["B13"]]
                if (is.null(b9_val) || is.na(b9_val) || b9_val == 0 || is.null(b13_val) || is.na(b13_val)) {
                    0
                } else {b13_val / b9_val}
            }
            
            model$big_dict[[district]][["F9"]] <- nrow(df[df[["AD"]] == "Y" & !is.na(df[["AD"]]), ])
            model$big_dict[[district]][["F10"]] <- sum(df$AF[df$G == "Y"], na.rm = TRUE)
            model$big_dict[[district]][["F11"]] <- sum(df[["L"]], na.rm = TRUE)
            model$big_dict[[district]][["F12"]] <- sum(df[["T"]], na.rm = TRUE) # check
            model$big_dict[[district]][["F13"]] <- sum(df[df[["U"]] > 0 & !is.na(df[["U"]]), "U"], na.rm = TRUE)
            model$big_dict[[district]][["F14"]] <- sum(df[["X"]], na.rm = TRUE) - sum(df[["AC"]], na.rm = TRUE)
            
            # Additional stuff needed to populate the summary page...
            model$big_dict[[district]][["X_sum"]] <- sum(df[["X"]], na.rm = TRUE)
            model$big_dict[[district]][["Y_sum"]] <- sum(df[["Y"]], na.rm = TRUE)
            model$big_dict[[district]][["Z_sum"]] <- sum(df[["Z"]], na.rm = TRUE)
            model$big_dict[[district]][["AA_sum"]] <- sum(df[["AA"]], na.rm = TRUE)
            model$big_dict[[district]][["AB_sum"]] <- sum(df[["AB"]], na.rm = TRUE)
            model$big_dict[[district]][["AC_sum"]] <- sum(df[["AC"]], na.rm = TRUE)
            model$big_dict[[district]][["AE_sum"]] <- sum(df[["AE"]], na.rm = TRUE)
            model$big_dict[[district]][["AF_sum"]] <- sum(df[["AF"]], na.rm = TRUE)
        }
        
        populate_summary <- function() {
            # map from district number to column in "Summary" sheet
            col_map <- list("1" = "C", "2" = "D", "3" = "E", "4" = "F", "5" = "G")
            parameter_col_map <- list("1" = "E", "2" = "H", "3" = "K", "4" = "N", "5" = "Q")
            
            for (i in 1:5) {
                parameter_sheet_col <- parameter_col_map[[as.character(i)]]
                col <- col_map[[as.character(i)]]
                district <- paste("District", i)
                
                model$big_dict[[SUMMARY]][[paste0(col, "5")]] <- model$big_dict[[DISTRICT_ID]][[paste0("B", 54 + i - 1)]]
                model$big_dict[[SUMMARY]][[paste0(col, "6")]] <- model$big_dict[[district]][["X_sum"]]
                model$big_dict[[SUMMARY]][[paste0(col, "7")]] <- model$big_dict[[district]][["Y_sum"]]
                model$big_dict[[SUMMARY]][[paste0(col, "8")]] <- model$big_dict[[PARAMETERS]][[paste0(parameter_sheet_col, "103")]]
                model$big_dict[[SUMMARY]][[paste0(col, "9")]] <- model$big_dict[[district]][["Z_sum"]]
                model$big_dict[[SUMMARY]][[paste0(col, "10")]] <- model$big_dict[[district]][["AA_sum"]]
                model$big_dict[[SUMMARY]][[paste0(col, "11")]] <- model$big_dict[[district]][["AC_sum"]]
                model$big_dict[[SUMMARY]][[paste0(col, "12")]] <- model$big_dict[[district]][["AB_sum"]]
                model$big_dict[[SUMMARY]][[paste0(col, "13")]] <- min(model$big_dict[[district]][["B13"]], model$big_dict[[SUMMARY]][[paste0(col, "8")]], na.rm = TRUE)
                
                model$big_dict[[SUMMARY]][[paste0(col, "18")]] <- model$big_dict[[DISTRICT_ID]][[paste0("B", 54 + i - 1)]]
                model$big_dict[[SUMMARY]][[paste0(col, "19")]] <- model$big_dict[[DISTRICT_ID]][[paste0("C", 54 + i - 1)]]
                model$big_dict[[SUMMARY]][[paste0(col, "20")]] <- model$big_dict[[DISTRICT_ID]][[paste0("E", 54 + i - 1)]]
                model$big_dict[[SUMMARY]][[paste0(col, "21")]] <- model$big_dict[[SUMMARY]][[paste0(col, "13")]]
                model$big_dict[[SUMMARY]][[paste0(col, "22")]] <- {
                    denominator <- as.numeric(model$big_dict[[SUMMARY]][[paste0(col, "20")]])
                    numerator <- as.numeric(model$big_dict[[SUMMARY]][[paste0(col, "21")]])
                    if (is.na(denominator) || denominator == 0) {
                        0
                    } else {
                        numerator / denominator
                    }
                }
                model$big_dict[[SUMMARY]][[paste0(col, "23")]] <- model$big_dict[[district]][["B11"]]
                model$big_dict[[SUMMARY]][[paste0(col, "24")]] <- model$big_dict[[district]][["B12"]]
                model$big_dict[[SUMMARY]][[paste0(col, "25")]] <- model$big_dict[[district]][["F10"]]
                model$big_dict[[SUMMARY]][[paste0(col, "26")]] <- model$big_dict[[district]][["F9"]]
                model$big_dict[[SUMMARY]][[paste0(col, "27")]] <- model$big_dict[[district]][["F11"]]
                model$big_dict[[SUMMARY]][[paste0(col, "28")]] <- model$big_dict[[district]][["F12"]]
                model$big_dict[[SUMMARY]][[paste0(col, "29")]] <- model$big_dict[[district]][["F13"]]
                model$big_dict[[SUMMARY]][[paste0(col, "30")]] <- model$big_dict[[district]][["F14"]]
            }
            
            for (row in 6:13) {
                values <- sapply(1:5, function(i) {
                    col <- col_map[[as.character(i)]]
                    val <- model$big_dict[[SUMMARY]][[paste0(col, row)]]
                    if (is.null(val)) NA else val
                })
                model$big_dict[[SUMMARY]][[paste0("H", row)]] <- sum(values, na.rm = TRUE)
            }
            
            values_h19 <- sapply(1:5, function(i) {
                col <- col_map[[as.character(i)]]
                val <- model$big_dict[[SUMMARY]][[paste0(col, "19")]]
                if (is.null(val)) NA else val
            })
            model$big_dict[[SUMMARY]][["H19"]] <- sum(values_h19, na.rm = TRUE)
            
            model$big_dict[[SUMMARY]][["H20"]] <- model$big_dict[[DISTRICT_ID]][["E59"]]
            
            values_h21 <- sapply(1:5, function(i) {
                col <- col_map[[as.character(i)]]
                val <- model$big_dict[[SUMMARY]][[paste0(col, "21")]]
                if (is.null(val)) NA else val
            })
            model$big_dict[[SUMMARY]][["H21"]] <- sum(values_h21, na.rm = TRUE)
            
            model$big_dict[[SUMMARY]][["H22"]] <- if (model$big_dict[[SUMMARY]][["H20"]] != 0) {
                model$big_dict[[SUMMARY]][["H21"]] / model$big_dict[[SUMMARY]][["H20"]]
            } else {
                0
            }
            
            for (row in 23:30) {
                values <- sapply(1:5, function(i) {
                    col <- col_map[[as.character(i)]]
                    val <- model$big_dict[[SUMMARY]][[paste0(col, row)]]
                    if (is.null(val)) NA else val
                })
                model$big_dict[[SUMMARY]][[paste0("H", row)]] <- sum(values, na.rm = TRUE)
            }
        }
        
        if (sheet_name == "Introduction") {
            populate_introduction()
        } else if (sheet_name == "Checklist District ID") {
            populate_checklist_district_id()
        } else if (sheet_name == "Checklist Parameters") {
            populate_checklist_parameters()
        } else if (strsplit(sheet_name, " ")[[1]][1] == "District") {
            district_num <- as.integer(strsplit(sheet_name, " ")[[1]][2])
            populate_district_i(district_num, df)
        } else if (sheet_name == "Summary") {
            populate_summary()
        }
    }
    
    model$save_all_data <- function(path_to_file) {
        json_data <- toJSON(model$big_dict, pretty = TRUE, auto_unbox = TRUE)
        writeLines(json_data, path_to_file)
    }
    
    model$save_zoning_stats <- function(path_to_file) {
        output_lines <- c(
            paste("Required:", model$big_dict[[INTRODUCTION]][["I6"]], "\t\t\t\t\tModeled:", model$big_dict[[SUMMARY]][["H21"]]),
            paste("Required:", model$big_dict[[INTRODUCTION]][["I7"]], "\t\t\t\t\tModeled:", model$big_dict[[SUMMARY]][["H19"]]),
            paste("Required:", model$big_dict[[INTRODUCTION]][["I9"]], "\t\t\t\t\tModeled:", 
                  model$big_dict[[SUMMARY]][["H25"]] / (model$big_dict[[INTRODUCTION]][["I6"]] * model$big_dict[[INTRODUCTION]][["I9"]])),
            paste("Required:", model$big_dict[[INTRODUCTION]][["I9"]], "\t\t\t\t\tModeled:", 
                  model$big_dict[[DISTRICT_ID]][["E71"]] / (model$big_dict[[INTRODUCTION]][["I7"]] * model$big_dict[[INTRODUCTION]][["I9"]]))
        )
        
        writeLines(output_lines, path_to_file)
        
        # Write summary data to separate file
        writeLines(capture.output(str(model$big_dict[[SUMMARY]])), path_to_file)
    }
    
    model$is_good_zoning <- function() {
        tryCatch({
            # Helper function equivalent to Python's comparator
            comparator <- function(intro_cell, summary_cell) {
                intro_val <- model$big_dict[[INTRODUCTION]][[intro_cell]]
                summary_val <- model$big_dict[[SUMMARY]][[summary_cell]]
                if (is.null(intro_val) || is.null(summary_val)) return(FALSE)
                return(intro_val < summary_val)
            }
            
            # All compliance checks as defined in the Excel sheet
            min_multi_family_unit_capacity <- comparator("I6", "H21")
            min_land_area <- comparator("I7", "H19")
            developable_station_area <- TRUE  # Always TRUE as in Python version
            
            # Gross density check
            h22_val <- model$big_dict[[SUMMARY]][["H22"]]
            gross_density <- if (!is.null(h22_val)) h22_val >= 15 else FALSE
            
            # Unit capacity ratio within station area
            i9_val <- model$big_dict[[INTRODUCTION]][["I9"]]
            h25_val <- model$big_dict[[SUMMARY]][["H25"]]
            i6_val <- model$big_dict[[INTRODUCTION]][["I6"]]
            
            unit_capacity_ratio_within_station_area <- FALSE
            if (!is.null(i9_val) && !is.null(h25_val) && !is.null(i6_val) && i6_val != 0 && i9_val != 0) {
                ratio <- h25_val / (i6_val * i9_val)
                unit_capacity_ratio_within_station_area <- i9_val < ratio
            }
            
            # Land area ratio within station area
            e71_val <- model$big_dict[[DISTRICT_ID]][["E71"]]
            i7_val <- model$big_dict[[INTRODUCTION]][["I7"]]
            
            land_area_ratio_within_station_area <- FALSE
            if (!is.null(i9_val) && !is.null(e71_val) && !is.null(i7_val) && i7_val != 0 && i9_val != 0) {
                ratio <- e71_val / (i7_val * i9_val)
                land_area_ratio_within_station_area <- i9_val < ratio
            }
            
            # Return overall compliance status
            return(
                min_multi_family_unit_capacity &&
                min_land_area &&
                developable_station_area &&
                unit_capacity_ratio_within_station_area &&
                land_area_ratio_within_station_area &&
                gross_density
            )
            
        }, error = function(e) {
            stop("You haven't provided all relevant details to the model")
        })
    }
    
    # Method: __getitem__ equivalent
    model$get_sheet <- function(sheet_name) {
        if (!(sheet_name %in% names(model$big_dict))) {
            stop(paste("Sheet", sheet_name, "does not exist"))
        }
        # Return a copy of the sheet data (equivalent to Python's copy())
        return(as.list(model$big_dict[[sheet_name]]))
    }
    return(model)
}










