# parcel_config.R - Constants and configuration for parcel MCMC
#
# This module defines parameters for:
# - Parcel construction (optional coarsening)
# - Block library generation
# - MCMC kernel probabilities

# ============================================================================
# SAMPLER SPEC (proposal/kernel tuning — does not affect the target distribution)
# ============================================================================

#' Define a parcel MCMC sampler spec
#'
#' Base constructor for the sampler/proposal tuning spec: kernel mix,
#' non-reversibility, and the proposal knobs that previously leaked in as
#' free globals (`swap_cap_tolerance`, `birth_tilt_lambda`,
#' `debug_invariant_checks`, and the online-enrichment/sample-storage/
#' burn-in cluster below). None of these fields affect the target
#' distribution pi(state) — only how the chain proposes moves, what gets
#' recorded, and how burn-in is applied when diagnostics summarize the
#' trajectory afterward. `birth_tilt_lambda` defaults to `capacity_prior_lambda`
#' (pass in `target_spec$priors$capacity_prior_lambda`) to preserve the
#' "cancel the capacity prior on births" coupling explicitly rather than via
#' a shared global.
#'
#' Callers needing a one-off deviation from a preset (e.g. disabling online
#' enrichment for a discovery-only run) construct the preset and then mutate
#' the returned list before calling — the same pattern already used for
#' per-chain `seed`/`n_steps` overrides.
#'
#' @param name Human-readable label for the config
#' @param seed RNG seed for the chain
#' @param n_steps Number of MCMC iterations
#' @param p_lcc_local,p_symmetric_birth_death,p_swap,p_replace_lcc Kernel mix
#'   probabilities (must sum to 1; see `validate_kernel_config()`)
#' @param use_lifted Use lifted (non-reversible) birth/death instead of
#'   standard symmetric birth/death
#' @param swap_cap_tolerance Capacity tolerance for secondary-swap
#'   similar-capacity sampling
#' @param birth_tilt_lambda Tilt strength for capacity-weighted birth
#'   proposals (0 = uniform)
#' @param debug_invariant_checks Validate state invariants after every
#'   accepted move (expensive; FALSE for production runs)
#' @param mcmc_burn_in Number of leading samples discarded from each chain's
#'   trajectory before computing diagnostics (ESS, R-hat)
#' @param enable_online_enrichment Add visited LCCs to the library during the
#'   main run, enabling Replace-LCC to propose jumps from recently-visited
#'   states
#' @param enrichment_interval Add the current LCC to the library every N steps
#'   when online enrichment is enabled
#' @param max_online_entries Maximum online library entries before FIFO
#'   eviction
#' @param enrichment_burn_in If set, freeze the library after this many steps
#'   for strict stationarity; NULL enriches continuously
#' @param max_stored_samples Target maximum number of thinned states stored
#'   for visualization/metrics
#' @param store_lcc_signatures Store an LCC signature at every step for
#'   discovery deduplication (only needed by the discovery supplement)
#' @return Named list sampler spec
#' @export
parcel_sampler_spec <- function(name,
                                 seed,
                                 n_steps,
                                 p_lcc_local,
                                 p_symmetric_birth_death,
                                 p_swap,
                                 p_replace_lcc,
                                 use_lifted,
                                 swap_cap_tolerance,
                                 birth_tilt_lambda,
                                 debug_invariant_checks = FALSE,
                                 mcmc_burn_in = 1000L,
                                 enable_online_enrichment = TRUE,
                                 enrichment_interval = 10L,
                                 max_online_entries = 5000L,
                                 enrichment_burn_in = mcmc_burn_in,
                                 max_stored_samples = 500L,
                                 store_lcc_signatures = FALSE) {
  config <- list(
    name = name,
    seed = seed,
    n_steps = n_steps,
    p_lcc_local = p_lcc_local,
    p_symmetric_birth_death = p_symmetric_birth_death,
    p_swap = p_swap,
    p_replace_lcc = p_replace_lcc,
    use_lifted = use_lifted,
    swap_cap_tolerance = swap_cap_tolerance,
    birth_tilt_lambda = birth_tilt_lambda,
    debug_invariant_checks = debug_invariant_checks,
    mcmc_burn_in = mcmc_burn_in,
    enable_online_enrichment = enable_online_enrichment,
    enrichment_interval = enrichment_interval,
    max_online_entries = max_online_entries,
    enrichment_burn_in = enrichment_burn_in,
    max_stored_samples = max_stored_samples,
    store_lcc_signatures = store_lcc_signatures
  )
  validate_kernel_config(config)
  config
}

#' Default parcel sampler spec preset
#'
#' 4 active kernels with capacity prior. Capacity prior penalizes capacity
#' above min_capacity (soft preference, not rejection). Hard constraints
#' (min_capacity, min_area, min_density, min_lcc_fraction) live in
#' `target_spec`, not here. min_lcc_fraction (0.5) is a hard legal
#' constraint, so LCC capacity must be sufficient before symmetric_birth_death
#' can add secondaries without violation.
#'
#' Note: symmetric_birth_death replaces separate birth/death kernels to fix
#' the proposal asymmetry that caused 5.4% death acceptance (k/N ratio).
#'
#' @param n_steps Number of MCMC iterations
#' @param swap_cap_tolerance Capacity tolerance for secondary-swap sampling
#' @param birth_tilt_lambda Tilt strength for capacity-weighted birth proposals
#' @param debug_invariant_checks Validate state invariants after every
#'   accepted move
#' @param ... Additional fields forwarded to `parcel_sampler_spec()`
#'   (`mcmc_burn_in`, `enable_online_enrichment`, `enrichment_interval`,
#'   `max_online_entries`, `enrichment_burn_in`, `max_stored_samples`,
#'   `store_lcc_signatures`) — see there for defaults
#' @return Named list sampler spec
#' @export
parcel_sampler_spec_default <- function(n_steps,
                                         swap_cap_tolerance,
                                         birth_tilt_lambda,
                                         debug_invariant_checks = FALSE,
                                         ...) {
  parcel_sampler_spec(
    name = "Parcel Default (Soft Constraints)",
    seed = 123,
    n_steps = n_steps,
    p_lcc_local = 0.12, # LCC boundary expansion (creates slack for secondaries)
    p_symmetric_birth_death = 0.305, # Unified birth/death for k-mixing
    p_swap = 0.12, # Geographic mixing (capacity-neutral)
    p_replace_lcc = 0.455, # Global LCC relocation (mode mixing)
    use_lifted = FALSE, # Use standard symmetric birth/death
    swap_cap_tolerance = swap_cap_tolerance,
    birth_tilt_lambda = birth_tilt_lambda,
    debug_invariant_checks = debug_invariant_checks,
    ...
  )
}

#' Lifted (non-reversible) parcel sampler spec preset
#'
#' Non-reversible birth/death with momentum. Improves mixing in k (secondary
#' count) dimension by maintaining direction across moves. On acceptance,
#' keeps direction (momentum). On rejection, flips direction (bounce). This
#' creates "runs" of same-direction moves that traverse k faster.
#'
#' @inheritParams parcel_sampler_spec_default
#' @return Named list sampler spec
#' @export
parcel_sampler_spec_lifted <- function(n_steps,
                                        swap_cap_tolerance,
                                        birth_tilt_lambda,
                                        debug_invariant_checks = FALSE,
                                        ...) {
  parcel_sampler_spec(
    name = "Lifted MCMC (Non-Reversible)",
    seed = 789,
    n_steps = n_steps,
    p_lcc_local = 0.35,
    p_symmetric_birth_death = 0.30, # Used as lifted_birth_death (balanced)
    p_swap = 0.10,
    p_replace_lcc = 0.25,
    use_lifted = TRUE, # Use lifted birth/death with momentum
    swap_cap_tolerance = swap_cap_tolerance,
    birth_tilt_lambda = birth_tilt_lambda,
    debug_invariant_checks = debug_invariant_checks,
    ...
  )
}

#' Discovery-only parcel sampler spec preset
#'
#' 100% lcc_local for MCMC-based LCC exploration. Used to supplement tree
#' discovery by finding non-tree-cut LCCs (LCCs with multiple boundary
#' crossings that tree enumeration misses).
#'
#' @param swap_cap_tolerance Capacity tolerance for secondary-swap sampling
#'   (unused by this preset's kernel mix, but required by the base
#'   constructor's validation)
#' @param birth_tilt_lambda Tilt strength for capacity-weighted birth
#'   proposals (unused by this preset's kernel mix)
#' @param debug_invariant_checks Validate state invariants after every
#'   accepted move
#' @param ... Additional fields forwarded to `parcel_sampler_spec()`. Callers
#'   typically mutate `enable_online_enrichment`/`store_lcc_signatures` on the
#'   returned spec afterward rather than passing them here (see
#'   `run_mcmc_discovery_supplement()`).
#' @return Named list sampler spec
#' @export
parcel_sampler_spec_discovery <- function(swap_cap_tolerance,
                                           birth_tilt_lambda,
                                           debug_invariant_checks = FALSE,
                                           ...) {
  parcel_sampler_spec(
    name = "Discovery (LCC-local only)",
    seed = 456,
    n_steps = 2000L,
    p_lcc_local = 1.0, # 100% lcc_local for boundary exploration
    p_symmetric_birth_death = 0.0,
    p_swap = 0.0,
    p_replace_lcc = 0.0, # No library moves - we're building the library
    use_lifted = FALSE,
    swap_cap_tolerance = swap_cap_tolerance,
    birth_tilt_lambda = birth_tilt_lambda,
    debug_invariant_checks = debug_invariant_checks,
    ...
  )
}

#' Validate kernel configuration
#'
#' Checks that all 4 kernel probabilities are present, sum to 1, and are
#' non-negative, and that the non-kernel-mix tuning fields are present.
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

  # Also required: proposal tuning fields that used to be free globals
  required_tuning_fields <- c(
    "swap_cap_tolerance",
    "birth_tilt_lambda",
    "debug_invariant_checks",
    "mcmc_burn_in",
    "enable_online_enrichment",
    "enrichment_interval",
    "max_online_entries",
    "enrichment_burn_in",
    "max_stored_samples",
    "store_lcc_signatures"
  )

  # Check all fields present
  missing <- setdiff(c(required_fields, required_tuning_fields), names(config))
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