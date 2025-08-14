# Test script for create_zoning_evaluator function
# with Cambridge data

library(redist)
library(dplyr)
library(sf)
library(ggplot2)
library(geomander)

source("simulation/shapefile_utils.R")
source("simulation/create_zoning_evaluator.R")
source("R/redist_constr.R")
source("simulation/excel_model.R")
source("simulation/compliance_utils.R")

cat("=== Testing create_zoning_evaluator with 0730 preprocessing ===\n")

# Cambridge shapefile
cambridge_shp <- st_read("data/cambridge.shp")
cambridge_shp <- st_set_crs(cambridge_shp, 26986)
cat("Cambridge shapefile with", nrow(cambridge_shp), "parcels\n")

# preprocessing - kinda patchy...
cambridge_buffered <- st_buffer(cambridge_shp, dist = 15)
adj_list <- redist.adjacency(cambridge_buffered)
# Remove parcels with empty adjacency
empty_adj <- sapply(adj_list, length) == 0
cambridge_shp <- cambridge_shp[-which(empty_adj), ]
cambridge_buffered <- st_buffer(cambridge_shp, dist = 15)
adj_list <- redist.adjacency(cambridge_buffered)
# Remove specific disconnected indices
disconnected_indices <- c(12708, 12772)
cambridge_shp_contiguous <- cambridge_shp[-disconnected_indices, ]
cambridge_buffered_contiguous <- st_buffer(cambridge_shp_contiguous, dist = 15)
adj_list_contiguous <- redist.adjacency(cambridge_buffered_contiguous)

# Create redist_map - 
cat("\nCreating redist_map...\n")
cambridge_map <- redist_map(
    cambridge_shp_contiguous,
    pop_tol = 0.9,  # high because we don't care if the 2 districts have different sizes
    ndists = 2,
    total_pop = SQFT,
    adj = adj_list_contiguous
)
cat("redist_map with", nrow(cambridge_map), "units and", attr(cambridge_map, "ndists"), "districts\n")

# Generate redistricting plans
cat("\nGenerating redistricting plans...\n")
constr <- redist_constr(cambridge_map)
cambridge_plans <- redist_smc(cambridge_map, nsims = 50, constraints = constr)  # Exact 0730 parameter
cat("Generated", nrow(cambridge_plans), "plans using redist_smc\n")

# Create zoning evaluator
cat("\nCreating zoning evaluator...\n")

tryCatch({
    evaluator <- create_zoning_evaluator(cambridge_map, community_name = "Cambridge")
    cat("Zoning evaluator created successfully\n")
}, error = function(e) {
    stop("Evaluator creation failed")
})

# test evaluator on individual plans
cat("\nTesting evaluator on Cambridge plans...\n")

plan_matrix <- get_plans_matrix(cambridge_plans)
cat("Plan matrix dimensions:", dim(plan_matrix), "\n")

# Test first few plans
test_plans <- min(10, ncol(plan_matrix))
compliant_count <- 0
non_compliant_count <- 0
evaluation_errors <- 0

cat("Evaluating first", test_plans, "plans...\n")

for (i in 1:test_plans) {
    plan <- plan_matrix[, i]
    
    # Show district composition
    district_composition <- table(plan)
    overlay_district <- as.numeric(names(district_composition)[which.min(district_composition)])
    overlay_size <- min(district_composition)
    other_size <- max(district_composition)
    
    tryCatch({
        score <- evaluator(plan, cambridge_map)
        
        if (score == 0) {
            compliant_count <- compliant_count + 1
            status <- "COMPLIANT"
        } else {
            non_compliant_count <- non_compliant_count + 1
            status <- "NON-COMPLIANT"
        }
        
        cat(sprintf("Plan %2d: %s (overlay district %d: %d parcels, other: %d parcels)\n", 
                   i, status, overlay_district, overlay_size, other_size))
        
    }, error = function(e) {
        evaluation_errors <- evaluation_errors + 1
        cat(sprintf("Plan %2d: ERROR - %s\n", i, e$message))
    })
}

# Evaluate all plans and find compliant ones
cat("\nEvaluating all", ncol(plan_matrix), "plans for compliance...\n")

all_scores <- numeric(ncol(plan_matrix))
evaluation_times <- numeric(ncol(plan_matrix))

for (i in 1:ncol(plan_matrix)) {
    plan <- plan_matrix[, i]
    
    start_time <- Sys.time()
    tryCatch({
        all_scores[i] <- evaluator(plan, cambridge_map)
        end_time <- Sys.time()
        evaluation_times[i] <- as.numeric(end_time - start_time, units = "secs")
    }, error = function(e) {
        all_scores[i] <- NA
        evaluation_times[i] <- NA
    })
    
    if (i %% 10 == 0) {
        cat("Evaluated", i, "/", ncol(plan_matrix), "plans...\n")
    }
}

# Step 7: Summary and analysis
cat("\n=== EVALUATION SUMMARY ===\n")

valid_scores <- all_scores[!is.na(all_scores)]
compliant_plans <- which(all_scores == 0)
non_compliant_plans <- which(all_scores == 1)
error_plans <- which(is.na(all_scores))

cat("Total plans:", ncol(plan_matrix), "\n")
cat("Successfully evaluated:", length(valid_scores), "\n")
cat("Evaluation errors:", length(error_plans), "\n")
cat("Compliant plans (score=0):", length(compliant_plans), "\n")
cat("Non-compliant plans (score=1):", length(non_compliant_plans), "\n")

if (length(valid_scores) > 0) {
    compliance_rate <- length(compliant_plans) / length(valid_scores) * 100
    cat(sprintf("Compliance rate: %.1f%%\n", compliance_rate))
}

# Performance analysis
valid_times <- evaluation_times[!is.na(evaluation_times)]
if (length(valid_times) > 0) {
    cat("\nPerformance Analysis:\n")
    cat(sprintf("  Mean evaluation time: %.4f seconds\n", mean(valid_times)))
    cat(sprintf("  Time range: %.4f - %.4f seconds\n", min(valid_times), max(valid_times)))
    cat(sprintf("  Total evaluation time: %.2f seconds\n", sum(valid_times)))
}

# Filter to compliant plans
if (length(compliant_plans) > 0) {
    cat("\nStep 8: Creating subset of compliant plans...\n")
    compliant_cambridge_plans <- cambridge_plans[compliant_plans, ]
    cat("Filtered to", nrow(compliant_cambridge_plans), "compliant plans\n")
    
    # Show plan IDs
    cat("Compliant plan IDs:", paste(compliant_plans[1:min(10, length(compliant_plans))], collapse = ", "))
    if (length(compliant_plans) > 10) cat(", ...")
    cat("\n")
    
    # Analyze overlay district characteristics of compliant plans
    cat("\nOverlay District Analysis for Compliant Plans:\n")
    overlay_sizes <- numeric(length(compliant_plans))
    
    for (i in 1:min(5, length(compliant_plans))) {
        plan_idx <- compliant_plans[i]
        plan <- plan_matrix[, plan_idx]
        district_composition <- table(plan)
        overlay_size <- min(district_composition)
        overlay_sizes[i] <- overlay_size
        cat(sprintf("  Plan %d: overlay district has %d parcels\n", plan_idx, overlay_size))
    }
    
} else {
    cat("\n No compliant plans found in this sample\n")
}

cat("Ready to filter cambridge_plans for compliant overlay districts\n")