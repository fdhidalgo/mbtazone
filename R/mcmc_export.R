#' Export thinned MCMC samples as a wide parcel-plan indicator table
#'
#' Produces a wide-format data.table with one row per parcel and one binary
#' indicator column per sampled plan. Suitable for joining to parcel-level
#' data and computing inclusion frequencies directly.
#'
#' @param chain_results Named list from all_parcel_chain_results
#' @param parcel_graph_result Result from build_parcel_graph_target()
#' @param secondary_library Hydrated secondary library
#' @param district_name Character district name for ID construction
#' @param n_samples Number of plans to sample across all chains (default 100)
#' @param seed Random seed for reproducibility (default 42)
#' @return data.table with columns:
#'   parcel_id, district, inclusion_frequency, plan_0001, plan_0002, ...
#'   where plan columns are 1 if parcel is in that plan, 0 otherwise
#' @export
export_mcmc_plans <- function(chain_results,
                              parcel_graph_result,
                              secondary_library,
                              district_name,
                              n_samples = 100L,
                              seed = 42L) {
  set.seed(seed)

  pa  <- parcel_graph_result$parcel_assignments
  sec <- hydrate_library(secondary_library)

  # All parcels in district
  all_parcel_ids <- unique(pa$parcel_id)

  valid_chains <- Filter(
    function(x) !isTRUE(x$initialization_failed) && !is.null(x$parcel_samples),
    chain_results
  )

  if (length(valid_chains) == 0) {
    cli::cli_alert_warning("No valid chains with samples for {district_name}")
    return(data.table::data.table(parcel_id = all_parcel_ids,
                                  district = district_name))
  }

  # Build index of all available samples
  sample_index_dt <- data.table::rbindlist(lapply(names(valid_chains), function(cname) {
    n <- length(valid_chains[[cname]]$parcel_samples)
    if (n == 0) return(NULL)
    data.table::data.table(chain = cname, sample_index = seq_len(n))
  }))

  n_available <- nrow(sample_index_dt)
  n_draw      <- min(n_samples, n_available)

  if (n_draw < n_samples) {
    cli::cli_alert_warning(
      "Only {n_available} samples available, requested {n_samples}"
    )
  }

  selected <- sample_index_dt[sample(n_available, n_draw)]

  cli::cli_alert_info(
    "Exporting {n_draw} plans from {district_name} ({length(valid_chains)} chains)"
  )

  # Build parcel index for fast lookup
  parcel_idx <- setNames(seq_along(all_parcel_ids), all_parcel_ids)
  n_parcels  <- length(all_parcel_ids)

  # Matrix: rows = parcels, cols = plans (integer for memory efficiency)
  plan_matrix <- matrix(0L, nrow = n_parcels, ncol = n_draw)
  plan_ids    <- character(n_draw)

  for (i in seq_len(n_draw)) {
    cname <- selected$chain[i]
    sidx  <- selected$sample_index[i]
    state <- valid_chains[[cname]]$parcel_samples[[sidx]]

    if (is.null(state)) next

    plan_ids[i] <- sprintf("plan_%04d", i)

    # LCC parcels
    lcc_parcels <- pa[unit_id %in% state$lcc_parcels, parcel_id]

    # Secondary parcels
    sec_parcels <- character(0)
    if (length(state$secondary_blocks) > 0) {
      for (k in seq_along(state$secondary_blocks)) {
        bid <- state$secondary_blocks[k]
        block_indices <- sec$blocks[[bid]]
        if (is.null(block_indices) || length(block_indices) == 0) next
        block_units <- sec$parcel_names[block_indices]
        sec_parcels <- c(sec_parcels, pa[unit_id %in% block_units, parcel_id])
      }
    }

    all_in_plan <- unique(c(lcc_parcels, sec_parcels))
    matched_idx <- parcel_idx[all_in_plan]
    matched_idx <- matched_idx[!is.na(matched_idx)]
    plan_matrix[matched_idx, i] <- 1L
  }

  # Build result data.table
  result <- data.table::data.table(
    parcel_id           = all_parcel_ids,
    district            = district_name,
    inclusion_frequency = rowMeans(plan_matrix)
  )

  # Add plan columns
  colnames(plan_matrix) <- plan_ids
  result <- cbind(result, data.table::as.data.table(plan_matrix))

  n_plans_with_data <- sum(colSums(plan_matrix) > 0)
  cli::cli_alert_success(
    "Exported {n_plans_with_data} plans, {n_parcels} parcels -> {ncol(result)} columns"
  )

  result
}
