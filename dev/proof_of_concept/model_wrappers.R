# Helper functions to work with excel_model.R

#' @return A new compliance model instance
new_compliance_model <- function() {
    return(create_compliance_model())
}

fill_sheet <- function(model, sheet_name, cell_map) {
    model$fill_sheet(sheet_name, cell_map)
    return(model)
}

populate_sheet <- function(model, sheet_name, df = NULL) {
    if (is.null(df)) {
        model$populate_sheet(sheet_name)
    } else {
        model$populate_sheet(sheet_name, df)
    }
    return(model)
}

#' Check if zoning is compliant
is_good_zoning <- function(model) {
    return(model$is_good_zoning())
}

save_zoning_stats <- function(model, path_to_file) {
    model$save_zoning_stats(path_to_file)
}

#' Save all data to a JSON file
save_all_data <- function(model, path_to_file) {
    model$save_all_data(path_to_file)
}
