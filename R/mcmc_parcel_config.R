# parcel_config.R - Constants and configuration for parcel MCMC
#
# This module defines parameters for:
# - Parcel construction (optional coarsening)
# - Block library generation
# - MCMC kernel probabilities

# WIP: All constants are removed and should be replaced with functions. (or config.yml)

# ============================================================================
# KERNEL CONFIGURATION
# ============================================================================

#' Define parcel kernel configurations
#'
#' Returns a named list of kernel configurations for parcel MCMC runs.
#' Each configuration specifies move probabilities and iteration counts.
#'
#' @param n_steps Number of MCMC iterations
#' @return Named list of kernel configurations
#' @export
define_parcel_kernel_configs <- function(n_steps = MCMC_STEPS_MACRO) {
  list(
    # Default config: 4 active kernels with capacity prior
    # Capacity prior penalizes capacity above min_capacity (soft preference, not rejection)
    # Hard constraints: min_capacity, min_area, min_density, min_lcc_fraction
    # min_lcc_fraction (0.5) is a hard legal constraint, so LCC capacity must be
    # sufficient before symmetric_birth_death can add secondaries without violation.
    #
    # Note: symmetric_birth_death replaces separate birth/death kernels to fix
    # the proposal asymmetry that caused 5.4% death acceptance (k/N ratio).
    default = list(
      name = "Parcel Default (Soft Constraints)",
      seed = 123,
      n_steps = n_steps,
      p_lcc_local = 0.12, # LCC boundary expansion (creates slack for secondaries)
      p_symmetric_birth_death = 0.305, # Unified birth/death for k-mixing
      p_swap = 0.12, # Geographic mixing (capacity-neutral)
      p_replace_lcc = 0.455, # Global LCC relocation (mode mixing)
      use_lifted = FALSE # Use standard symmetric birth/death
    ),

    # Lifted MCMC config: Non-reversible birth/death with momentum
    # Improves mixing in k (secondary count) dimension by maintaining
    # direction across moves. On acceptance, keeps direction (momentum).
    # On rejection, flips direction (bounce). This creates "runs" of
    # same-direction moves that traverse k faster.
    lifted = list(
      name = "Lifted MCMC (Non-Reversible)",
      seed = 789,
      n_steps = n_steps,
      p_lcc_local = 0.10,
      p_symmetric_birth_death = 0.35, # Used as lifted_birth_death (balanced)
      p_swap = 0.10,
      p_replace_lcc = 0.45,
      use_lifted = TRUE # Use lifted birth/death with momentum
    ),

    # Discovery config: 100% lcc_local for MCMC-based LCC exploration
    # Used to supplement tree discovery by finding non-tree-cut LCCs
    # (LCCs with multiple boundary crossings that tree enumeration misses)
    discovery = list(
      name = "Discovery (LCC-local only)",
      seed = 456,
      n_steps = 2000L,
      p_lcc_local = 1.0, # 100% lcc_local for boundary exploration
      p_symmetric_birth_death = 0.0,
      p_swap = 0.0,
      p_replace_lcc = 0.0, # No library moves - we're building the library
      use_lifted = FALSE
    )
  )
}

#' Validate kernel configuration
#'
#' Checks that all 4 kernel probabilities are present, sum to 1, and are non-negative.
#' Required kernels: lcc_local, symmetric_birth_death, swap, replace_lcc.
#'
#' @param config Kernel configuration list
#' @return TRUE if valid, error otherwise
validate_kernel_config <- function(config) {
  # All configs require 4 kernels
  required_fields <- c(
    "p_lcc_local",
    "p_symmetric_birth_death",
    "p_swap",
    "p_replace_lcc"
  )

  # Check all fields present
  missing <- setdiff(required_fields, names(config))
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing kernel config fields: %s",
      paste(missing, collapse = ", ")
    ))
  }

  # Compute probability sum
  prob_sum <- config$p_lcc_local +
    config$p_symmetric_birth_death +
    config$p_swap +
    config$p_replace_lcc

  # Check probabilities sum to 1
  if (abs(prob_sum - 1.0) > 1e-6) {
    stop(sprintf("Kernel probabilities sum to %.4f, must equal 1.0", prob_sum))
  }

  # Check all probabilities are non-negative
  for (field in required_fields) {
    if (config[[field]] < 0) {
      stop(sprintf(
        "Kernel probability %s is negative: %.4f",
        field,
        config[[field]]
      ))
    }
  }

  invisible(TRUE)
}