# parcel_feasibility.R - Offline parcel feasibility analysis
#
# Determines which parcels can theoretically be part of any valid configuration.
# Runs before MCMC as a pipeline target to explain low-visit parcels.
#
# This addresses the observation that ~95% of parcels are visited <5% of the time.
# By analyzing library membership offline, we can distinguish:
# - Constraint-driven low visits (parcels that CANNOT be part of any valid config)
# - Sampler failure low visits (feasible parcels that are rarely visited)

# ============================================================================
# MAIN ANALYSIS FUNCTION
# ============================================================================

#' Analyze parcel feasibility
#'
#' For each parcel, determines if it can be part of any valid configuration
#' by checking membership in LCC and secondary libraries.
#'
#' @param parcel_graph igraph object from parcel_graph_result
#' @param lcc_library Discovered LCC library
#' @param secondary_library Discovered secondary library
#' @param constraints MBTA constraints
#' @return data.table with parcel-level feasibility classification
#' @export
analyze_parcel_feasibility <- function(parcel_graph, lcc_library, secondary_library,
                                        constraints) {
  cli::cli_h2("Parcel Feasibility Analysis")

  all_parcels <- igraph::V(parcel_graph)$name
  n_parcels <- length(all_parcels)

  # Get parcel attributes

capacity_vec <- igraph::V(parcel_graph)$capacity
  area_vec <- igraph::V(parcel_graph)$area
  names(capacity_vec) <- all_parcels
  names(area_vec) <- all_parcels

  # Initialize result table
  result <- data.table::data.table(
    parcel_id = all_parcels,
    capacity = capacity_vec[all_parcels],
    area = area_vec[all_parcels],
    in_any_lcc = FALSE,
    in_any_secondary = FALSE,
    n_lcc_blocks = 0L,
    n_secondary_blocks = 0L,
    classification = NA_character_,
    reason = NA_character_
  )

  cli::cli_alert_info("Checking {n_parcels} parcels against libraries")

  # Step 1: Check LCC library membership
  cli::cli_alert("Scanning LCC library ({lcc_library$n_blocks} blocks)...")
  lcc_parcel_names <- lcc_library$parcel_names

  # Build lookup: parcel_id -> row index in result
  parcel_to_row <- seq_len(n_parcels)
  names(parcel_to_row) <- all_parcels

  for (i in seq_len(lcc_library$n_blocks)) {
    block_indices <- lcc_library$blocks[[i]]
    if (is.null(block_indices) || length(block_indices) == 0) next
    block_parcels <- lcc_parcel_names[block_indices]

    row_indices <- parcel_to_row[block_parcels]
    row_indices <- row_indices[!is.na(row_indices)]

    if (length(row_indices) > 0) {
      data.table::set(result, i = row_indices, j = "in_any_lcc", value = TRUE)
      result[row_indices, n_lcc_blocks := n_lcc_blocks + 1L]
    }
  }

  # Step 2: Check secondary library membership
  cli::cli_alert("Scanning secondary library ({secondary_library$n_blocks} blocks)...")
  sec_parcel_names <- secondary_library$parcel_names

  for (i in seq_len(secondary_library$n_blocks)) {
    block_indices <- secondary_library$blocks[[i]]
    if (is.null(block_indices) || length(block_indices) == 0) next
    block_parcels <- sec_parcel_names[block_indices]

    row_indices <- parcel_to_row[block_parcels]
    row_indices <- row_indices[!is.na(row_indices)]

    if (length(row_indices) > 0) {
      data.table::set(result, i = row_indices, j = "in_any_secondary", value = TRUE)
      result[row_indices, n_secondary_blocks := n_secondary_blocks + 1L]
    }
  }

  # Step 3: Classify each parcel
  result[, classification := data.table::fcase(
    in_any_lcc & in_any_secondary, "lcc_and_secondary",
    in_any_lcc & !in_any_secondary, "lcc_only",
    !in_any_lcc & in_any_secondary, "secondary_only",
    default = "unreachable"
  )]

  # Step 4: Determine reason for unreachable parcels
  min_density <- constraints$min_density
  min_area_sec <- SECONDARY_AREA_THRESHOLD # 5 acres from config.R

  unreachable_idx <- which(result$classification == "unreachable")
  if (length(unreachable_idx) > 0) {
    cli::cli_alert("Analyzing {length(unreachable_idx)} unreachable parcels...")

    reasons <- character(length(unreachable_idx))
    for (j in seq_along(unreachable_idx)) {
      idx <- unreachable_idx[j]
      parcel_area <- result$area[idx]
      parcel_cap <- result$capacity[idx]
      parcel_density <- if (parcel_area > 0) parcel_cap / parcel_area else 0

      # Check potential reasons (in order of likelihood)
      if (parcel_density < min_density) {
        reasons[j] <- "low_density"
      } else if (parcel_area < min_area_sec) {
        reasons[j] <- "small_area"
      } else {
        reasons[j] <- "not_in_discovered_blocks"
      }
    }
    data.table::set(result, i = unreachable_idx, j = "reason", value = reasons)
  }

  # Summary statistics
  cli::cli_alert_success("Feasibility classification complete:")
  summary_dt <- result[, .(count = .N), by = classification]
  data.table::setorder(summary_dt, -count)
  for (i in seq_len(nrow(summary_dt))) {
    pct <- round(100 * summary_dt$count[i] / n_parcels, 1)
    cli::cli_alert_info("  {summary_dt$classification[i]}: {scales::comma(summary_dt$count[i])} ({pct}%)")
  }

  # Breakdown of unreachable reasons
  if (nrow(result[classification == "unreachable"]) > 0) {
    reason_dt <- result[classification == "unreachable", .(count = .N), by = reason]
    data.table::setorder(reason_dt, -count)
    cli::cli_alert_info("Unreachable breakdown:")
    for (i in seq_len(nrow(reason_dt))) {
      cli::cli_alert_info("    {reason_dt$reason[i]}: {scales::comma(reason_dt$count[i])}")
    }
  }

  result
}

# ============================================================================
# SUMMARY FOR REPORTING
# ============================================================================

#' Summarize feasibility analysis for reporting
#'
#' @param feasibility_result Output from analyze_parcel_feasibility()
#' @param parcel_graph igraph object
#' @return List with summary statistics for Quarto report
#' @export
summarize_parcel_feasibility <- function(feasibility_result, parcel_graph) {
  n_total <- nrow(feasibility_result)

  # Classification counts
  class_summary <- feasibility_result[, .(
    count = .N,
    pct = round(100 * .N / n_total, 1)
  ), by = classification]
  data.table::setorder(class_summary, -count)

  # Unreachable reason breakdown
  n_unreachable <- nrow(feasibility_result[classification == "unreachable"])
  if (n_unreachable > 0) {
    unreachable_reasons <- feasibility_result[
      classification == "unreachable",
      .(
        count = .N,
        pct = round(100 * .N / n_unreachable, 1)
      ),
      by = reason
    ]
    data.table::setorder(unreachable_reasons, -count)
  } else {
    unreachable_reasons <- data.table::data.table(
      reason = character(0),
      count = integer(0),
      pct = numeric(0)
    )
  }

  # Capacity contribution of each class
  total_capacity <- sum(feasibility_result$capacity)
  capacity_by_class <- feasibility_result[, .(
    total_capacity = sum(capacity),
    pct_capacity = round(100 * sum(capacity) / total_capacity, 1)
  ), by = classification]
  data.table::setorder(capacity_by_class, -total_capacity)

  # Area contribution
  total_area <- sum(feasibility_result$area)
  area_by_class <- feasibility_result[, .(
    total_area = sum(area),
    pct_area = round(100 * sum(area) / total_area, 1)
  ), by = classification]
  data.table::setorder(area_by_class, -total_area)

  list(
    n_total = n_total,
    class_summary = class_summary,
    unreachable_reasons = unreachable_reasons,
    capacity_by_class = capacity_by_class,
    area_by_class = area_by_class,
    n_feasible = nrow(feasibility_result[classification != "unreachable"]),
    n_unreachable = n_unreachable,
    pct_unreachable = round(100 * n_unreachable / n_total, 1),
    total_capacity = total_capacity,
    total_area = total_area
  )
}
