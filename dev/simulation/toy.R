library(data.table)
library(igraph)
library(purrr)
library(glue)
library(ggplot2)

# Constants
SECONDARY_AREA_THRESHOLD <- 5  # Minimum acres for secondary components to count
DEFAULT_P_FLIP <- 0.7  # Default probability of using flip kernel vs. swap kernel
DEFAULT_P_ADD <- 0.5   # Default probability of attempting add vs. remove in flip kernel

find_components <- function(g, selected_parcels) {
  subgraph <- induced_subgraph(g, selected_parcels)

  ##Decompose into connected components
  component_list <- decompose(subgraph)

  ##For each component, calculate its total capabilities
  component_capacities <- sapply(component_list, \(comp) sum(V(comp)$capacity))

  ##For each component, calculate total area
  component_areas <- sapply(component_list, \(comp) sum(V(comp)$area))

  list(
    components = component_list,
    capacities = component_capacities,
    areas = component_areas,
    n_components = length(component_list)
  )
}

identify_primary_component <- function(component_list, capacities) {
  # Returns index of primary component (largest connected component)
  # Tie-breaking: component with lexicographically smallest parcel ID
  max_capacity <- max(capacities)
  candidates <- which(capacities == max_capacity)

  if (length(candidates) == 1) {
    return(candidates)
  }

  # Multiple components tied for largest capacity
  # Select component with smallest parcel ID
  min_parcel_ids <- sapply(component_list[candidates], \(comp) {
    min(V(comp)$name)
  })
  return(candidates[which.min(min_parcel_ids)])
}

is_feasible <- function(g, selected_parcels, constraints) {
  if (length(selected_parcels) == 0) {
    return(FALSE)
  }

  comp_info <- find_components(g, selected_parcels)
  total_capacity <- sum(comp_info$capacities)
  total_area <- sum(comp_info$areas)

  ## Check capacity bounds

  if (
    total_capacity < constraints$min_capacity ||
      total_capacity > constraints$max_capacity
  ) {
    return(FALSE)
  }

  ## Check area (primary always counts, secondaries need >= threshold acres)
  ## Statute: secondary components only count toward area requirement if >= threshold
  ## This prevents pathological fragmentation while allowing realistic multi-district plans
  primary_idx <- identify_primary_component(comp_info$components, comp_info$capacities)
  eligible_area <- comp_info$areas[primary_idx]
  if (comp_info$n_components > 1) {
    secondary_areas <- comp_info$areas[-primary_idx]
    eligible_area <- eligible_area + sum(secondary_areas[secondary_areas >= SECONDARY_AREA_THRESHOLD])
  }
  if (eligible_area < constraints$min_area) {
    return(FALSE)
  }

  ## Check density
  density <- total_capacity / total_area
  if (density < constraints$min_density) {
    return(FALSE)
  }

  ## Check LCC (largest connected component must contain >= min fraction of capacity)
  lcc_capacity <- comp_info$capacities[primary_idx]
  if (lcc_capacity < constraints$min_lcc_fraction * total_capacity) {
    return(FALSE)
  }
  TRUE
}

enumerate_all_plans <- function(g, k) {
  ## Function to enumerate all possible k-parcel plans

  all_parcels <- V(g)$name

  ## Generate all combinations of size k
  plans_matrix <- combn(all_parcels, k)

  map(seq_len(ncol(plans_matrix)), \(i) plans_matrix[, i])
}

find_feasible_plans <- function(g, k, constraints) {
  all_plans <- enumerate_all_plans(g, k)

  feasible_plans <- keep(all_plans, \(plan) is_feasible(g, plan, constraints))

  list(
    all_plans = all_plans,
    feasible_plans = feasible_plans,
    n_plans = length(all_plans),
    n_feasible = length(feasible_plans)
  )
}


make_feasible_plan_dt <- function(g, feasible_plans) {
  # Create data table with one row for each feasible plan
  # Columns:
  # - plan: vector of parcel IDs
  # - parcels: character vector of parcel IDs
  # - total_capacity: total capacity of the plan
  # - total_area: total area of the plan
  # - density: density of the plan
  # - n_components: number of components in the plan
  # - lcc_capacity: capacity of the largest component in the plan
  comp_info <- map(feasible_plans$feasible_plans, \(plan) {
    find_components(g, plan)
  })

  dt <- data.table(
    plan = seq_along(feasible_plans$feasible_plans),
    parcels = map_chr(feasible_plans$feasible_plans, \(plan) {
      paste(sort(plan), collapse = ", ")
    }),
    total_capacity = map_dbl(comp_info, \(info) sum(info$capacities)),
    total_area = map_dbl(comp_info, \(info) sum(info$areas)),
    density = map_dbl(comp_info, \(info) {
      sum(info$capacities) / sum(info$areas)
    }),
    n_components = map_int(comp_info, \(info) info$n_components),
    lcc_capacity = map_dbl(comp_info, \(info) {
      max(info$capacities)
    })
  )
  dt
}

match_plan <- function(sample_plan, feasible_plans) {
  sample_sorted <- paste(sort(sample_plan), collapse = ", ")
  feasible_sorted <- map(feasible_plans$feasible_plans, \(plan) {
    paste(sort(plan), collapse = ", ")
  })
  match(sample_sorted, feasible_sorted)
}

# Function to generate all valid swap proposals from a current state
generate_swap_proposals <- function(g, current_plan, constraints) {
  # current_plan: character vector of parcel IDs currently selected

  # Parcels we could potentially add (those not in current plan)
  parcels_to_add <- setdiff(V(g)$name, current_plan)

  # Parcels we could potentially remove (those in current plan)
  parcels_to_remove <- current_plan

  # Create all combinations of (add, remove) pairs
  swap_combinations <- expand.grid(
    parcels_to_add,
    parcels_to_remove,
    stringsAsFactors = FALSE
  )

  # Test each swap and keep valid ones
  valid_swaps <- map(seq_len(nrow(swap_combinations)), .f = function(swap_idx) {
    parcel_add <- swap_combinations$Var1[swap_idx]
    parcel_remove <- swap_combinations$Var2[swap_idx]
    proposed_plan <- c(setdiff(current_plan, parcel_remove), parcel_add)
    if (is_feasible(g, proposed_plan, constraints)) {
      list(
        add = parcel_add,
        remove = parcel_remove,
        proposed_plan = proposed_plan
      )
    } else {
      NULL
    }
  }) |>
    compact()
  valid_swaps
}

swap_move <- function(g, current_plan, constraints) {
  # Generate swap proposals
  valid_swaps <- generate_swap_proposals(g, current_plan, constraints)

  # If no valid swaps, stay at current plan
  if (length(valid_swaps) == 0) {
    return(list(
      plan = current_plan,
      accepted = FALSE,
      move_type = "swap",
      proposal_failed = TRUE
    ))
  }

  # Propose a swap uniformly at random
  n_current_swaps <- length(valid_swaps)
  swap_idx <- sample(seq_len(n_current_swaps), size = 1)
  proposed_plan <- valid_swaps[[swap_idx]]$proposed_plan

  # Calculate acceptance probability
  # M-H acceptance ratio: A = min(1, q(X'->X) / q(X->X'))
  # For swap moves: ratio = |Q(X)| / |Q(X')| where Q(X) = valid swaps from X
  valid_swaps_reverse <- generate_swap_proposals(
    g,
    proposed_plan,
    constraints
  )
  n_proposed_swaps <- length(valid_swaps_reverse)

  acceptance_ratio <- n_current_swaps / n_proposed_swaps
  acceptance_prob <- min(1, acceptance_ratio)

  # Accept or reject
  accepted <- runif(1) < acceptance_prob

  list(
    plan = if (accepted) proposed_plan else current_plan,
    accepted = accepted,
    move_type = "swap",
    acceptance_prob = acceptance_prob,
    proposal_failed = FALSE
  )
}

generate_flip_proposals <- function(g, current_plan, constraints) {
  ## Parcels we could potentially add (those not in current plan)
  parcels_to_add <- setdiff(V(g)$name, current_plan)

  ## Parcels we could potentially remove (those in current plan)
  parcels_to_remove <- current_plan

  ## Test which additions are feasible
  valid_additions <- map(parcels_to_add, \(parcel) {
    proposed_plan <- c(current_plan, parcel)
    if (is_feasible(g, proposed_plan, constraints)) {
      list(
        type = "add",
        parcel = parcel,
        proposed_plan = proposed_plan
      )
    } else {
      NULL
    }
  }) |>
    compact()

  # Test which removals are feasible
  valid_removals <- map(parcels_to_remove, \(parcel) {
    proposed_plan <- setdiff(current_plan, parcel)
    if (is_feasible(g, proposed_plan, constraints)) {
      list(
        type = "remove",
        parcel = parcel,
        proposed_plan = proposed_plan
      )
    } else {
      NULL
    }
  }) |>
    compact()

  list(
    additions = valid_additions,
    removals = valid_removals,
    n_additions = length(valid_additions),
    n_removals = length(valid_removals)
  )
}

flip_move <- function(g, current_plan, constraints, p_add = .5) {
  # Generate valid flip proposals
  flip_proposals <- generate_flip_proposals(g, current_plan, constraints)

  #Choose add or remove
  attempt_add <- runif(1) < p_add

  if (attempt_add) {
    ## Attempt to add a parcel
    if (flip_proposals$n_additions == 0) {
      #No valid additions, stay at current plan
      return(list(
        plan = current_plan,
        accepted = FALSE,
        move_type = "add",
        proposal_failed = TRUE
      ))
    }

    # Select one addition uniformly at random
    add_idx <- sample(seq_len(flip_proposals$n_additions), size = 1)
    proposed_plan <- flip_proposals$additions[[add_idx]]$proposed_plan

    # Calculate acceptance probability
    # M-H acceptance ratio for ADD move: A = min(1, q(X'->X) / q(X->X'))
    # Birth-death symmetry: q(X'->X) = (1-p_add)/|S_rem(X')|, q(X->X') = p_add/|S_add(X)|
    # Ratio simplifies to: |S_add(X)| / |S_rem(X')|
    flip_proposals_reverse <- generate_flip_proposals(
      g,
      proposed_plan,
      constraints
    )
    acceptance_ratio <- flip_proposals$n_additions /
      flip_proposals_reverse$n_removals
    acceptance_prob <- min(1, acceptance_ratio)
  } else {
    ## Attempt to remove a parcel
    if (flip_proposals$n_removals == 0) {
      #No valid removals, stay at current plan
      return(list(
        plan = current_plan,
        accepted = FALSE,
        move_type = "remove",
        proposal_failed = TRUE
      ))
    }

    # Select one removal uniformly at random
    remove_idx <- sample(seq_len(flip_proposals$n_removals), size = 1)
    proposed_plan <- flip_proposals$removals[[remove_idx]]$proposed_plan

    # Calculate acceptance probability
    # M-H acceptance ratio for REMOVE move: A = min(1, q(X'->X) / q(X->X'))
    # Birth-death symmetry: q(X'->X) = p_add/|S_add(X')|, q(X->X') = (1-p_add)/|S_rem(X)|
    # Ratio simplifies to: |S_rem(X)| / |S_add(X')|
    flip_proposals_reverse <- generate_flip_proposals(
      g,
      proposed_plan,
      constraints
    )

    acceptance_ratio <- flip_proposals$n_removals /
      flip_proposals_reverse$n_additions
    acceptance_prob <- min(1, acceptance_ratio)
  }

  # Accept or reject
  accepted <- runif(1) < acceptance_prob
  return(list(
    plan = if (accepted) proposed_plan else current_plan,
    accepted = accepted,
    move_type = if (attempt_add) "add" else "remove",
    proposal_failed = FALSE
  ))
}

create_scenario1_graph <- function() {
  parcel_ids <- c(1, 2, 5, 6)
  g <- make_empty_graph(n = length(parcel_ids), directed = FALSE)
  V(g)$name <- as.character(parcel_ids)
  g <- add_edges(g, c("1", "2", "5", "6"))
  V(g)$capacity <- 1
  V(g)$area <- 6
  g
}

create_scenario2_graph <- function() {
  # Scenario 2: Connected Chain
  # 8 parcels in linear chain: P1—P2—P3—P4—P5—P6—P7—P8
  # Purpose: Test basic chain mixing without geographic complications
  parcel_ids <- 1:8
  g <- make_empty_graph(n = length(parcel_ids), directed = FALSE)
  V(g)$name <- as.character(parcel_ids)

  # Create linear chain edges: (1,2), (2,3), (3,4), ..., (7,8)
  edges <- c()
  for (i in 1:7) {
    edges <- c(edges, as.character(i), as.character(i + 1))
  }
  g <- add_edges(g, edges)

  # All parcels have capacity = 1, area = 6 acres
  V(g)$capacity <- 1
  V(g)$area <- 6

  g
}

g <- create_scenario1_graph()
constraints <- list(
  min_capacity = 3,
  max_capacity = 3,
  min_area = 10,
  min_density = .1,
  min_lcc_fraction = .5
)


feasible_plans <- find_feasible_plans(g, 3, constraints)
make_feasible_plan_dt(g, feasible_plans)
match_plan(c("1", "2", "5"), feasible_plans)

valid_swaps <- generate_swap_proposals(g, c("1", "2", "5"), constraints)


test_config <- c("1", "2", "5")
for (i in seq_along(valid_swaps)) {
  swap <- valid_swaps[[i]]
  print(glue("Swap {i}: {swap$add} -> {swap$remove}"))
  print(paste(swap$proposed_plan, collapse = ", "))
}


# Main MCMC sampler

run_mcmc <- function(
  g,
  initial_plan,
  constraints,
  n_steps,
  p_flip = DEFAULT_P_FLIP,
  p_add = DEFAULT_P_ADD,
  verbose = TRUE
) {
  ## Storage for samples
  samples <- vector("list", n_steps)

  # Track statistics by move type
  stats <- data.table(
    move_type = c("flip_add", "flip_remove", "swap"),
    n_proposed = 0,
    n_accepted = 0
  )
  setkey(stats, move_type)

  ## Initialize chain at initial plan
  current_plan <- initial_plan
  samples[[1]] <- current_plan

  # Main MCMC loop
  for (t in 2:n_steps) {
    # Decide which kernel to use
    use_flip <- runif(1) < p_flip

    if (use_flip) {
      # Use flip kernel
      result <- flip_move(g, current_plan, constraints, p_add)
      move_key <- paste0("flip_", result$move_type)
    } else {
      result <- swap_move(g, current_plan, constraints)
      move_key <- "swap"
    }

    # Update statistics
    if (!result$proposal_failed) {
      stats[move_key, n_proposed := n_proposed + 1]
      if (result$accepted) {
        stats[move_key, n_accepted := n_accepted + 1]
      }
    }

    # Update current plan
    current_plan <- result$plan
    samples[[t]] <- current_plan

    if (verbose && t %% 500 == 0) {
      cat(glue("Step {t}\n"), "\n")
      print(stats[, .(
        move_type,
        acceptance_rate = n_accepted / pmax(n_proposed, 1)
      )])
    }
  }

  # Return samples
  list(
    samples = samples,
    stats = stats
  )
}


#Function to compute the transition matrix

compute_transition_matrix <- function(
  g,
  feasible_plans,
  constraints,
  p_flip = DEFAULT_P_FLIP,
  p_add = DEFAULT_P_ADD
) {
  n_plans <- length(feasible_plans$feasible_plans)

  # Initialize transition matrix for each kernel
  T_flip <- matrix(0, nrow = n_plans, ncol = n_plans)
  T_swap <- matrix(0, nrow = n_plans, ncol = n_plans)

  # For each state i, compute transition probabilities
  for (i in seq_len(n_plans)) {
    current_plan <- feasible_plans$feasible_plans[[i]]

    # ====== FLIP MOVE TRANSITIONS ======
    flip_proposals <- generate_flip_proposals(g, current_plan, constraints)

    # Process addition moves
    if (flip_proposals$n_additions > 0) {
      for (add_idx in seq_len(flip_proposals$n_additions)) {
        proposed_plan <- flip_proposals$additions[[add_idx]]$proposed_plan
        j <- match_plan(proposed_plan, feasible_plans)

        # Proposal probability: p_add * 1 / |S_add(current_plan)|
        q_forward <- p_add * (1 / flip_proposals$n_additions)

        # Reverse proposal probability: (1-p_add) * 1 / |S_rem(proposed_plan)|
        flip_proposals_reverse <- generate_flip_proposals(
          g,
          proposed_plan,
          constraints
        )
        q_reverse <- (1 - p_add) * (1 / flip_proposals_reverse$n_removals)

        # Acceptance probability
        accept_prob <- min(1, q_reverse / q_forward)

        # Transition probability
        T_flip[i, j] <- T_flip[i, j] + q_forward * accept_prob
      }
    }

    # Process removal moves
    if (flip_proposals$n_removals > 0) {
      for (remove_idx in seq_len(flip_proposals$n_removals)) {
        proposed_plan <- flip_proposals$removals[[remove_idx]]$proposed_plan
        j <- match_plan(proposed_plan, feasible_plans)

        # Proposal probability: (1-p_add) * 1 / |S_rem(current_plan)|
        q_forward <- (1 - p_add) * (1 / flip_proposals$n_removals)

        # Reverse proposal probability: p_add * 1 / |S_add(proposed_plan)|
        flip_proposals_reverse <- generate_flip_proposals(
          g,
          proposed_plan,
          constraints
        )
        q_reverse <- p_add * (1 / flip_proposals_reverse$n_additions)

        # Acceptance probability
        accept_prob <- min(1, q_reverse / q_forward)

        # Transition probability
        T_flip[i, j] <- T_flip[i, j] + q_forward * accept_prob
      }
    }

    # ====== SWAP MOVE TRANSITIONS ======
    swap_proposals <- generate_swap_proposals(g, current_plan, constraints)
    n_swaps_current <- length(swap_proposals)

    if (n_swaps_current > 0) {
      # Proposal probability: uniform over valid swaps
      proposal_prob <- 1 / n_swaps_current

      for (swap_idx in seq_len(n_swaps_current)) {
        proposed_plan <- swap_proposals[[swap_idx]]$proposed_plan
        j <- match_plan(proposed_plan, feasible_plans)

        # Calculate acceptance probability
        valid_swaps_proposed <- generate_swap_proposals(
          g,
          proposed_plan,
          constraints
        )
        n_swaps_proposed <- length(valid_swaps_proposed)

        accept_prob <- min(1, n_swaps_current / n_swaps_proposed)

        # Add to swap transition matrix
        T_swap[i, j] <- T_swap[i, j] + proposal_prob * accept_prob
      }
    }
  }

  # Set diagonal elements (self-loops from rejected proposals)
  # Ensures row-stochastic matrix: each row sums to 1
  for (i in seq_len(n_plans)) {
    T_flip[i, i] <- 1 - sum(T_flip[i, -i])
    T_swap[i, i] <- 1 - sum(T_swap[i, -i])
  }

  # Combine kernels with mixture weights
  T_mixed <- p_flip * T_flip + (1 - p_flip) * T_swap

  # Verify rows sum to 1
  row_sums <- rowSums(T_mixed)
  if (!all(abs(row_sums - 1) < 1e-6)) {
    warning("Transition matrix rows do not sum to 1")
    print(row_sums)
  }

  list(
    matrix = T_mixed,
    T_flip = T_flip,
    T_swap = T_swap,
    p_flip = p_flip
  )
}

# Function to check detailed balance
check_detailed_balance <- function(T, pi = NULL) {
  n_states <- nrow(T)

  #If no stationary distribution provided, use uniform
  if (is.null(pi)) {
    pi <- rep(1 / n_states, n_states)
  }

  # Compute the detailed balance violations
  # For each pair (i, j), check if pi[i] * T[i, j] == pi[j] * T[j, i]
  violations <- data.table(expand.grid(seq_len(n_states), seq_len(n_states)))
  setnames(violations, c("i", "j"))
  violations <- violations[i < j]
  violations[, forward_flow := pi[i] * T[cbind(i, j)]]
  violations[, reverse_flow := pi[j] * T[cbind(j, i)]]
  violations[, violation := abs(forward_flow - reverse_flow)]
  violations[,
    relative_violation := violation / (forward_flow + reverse_flow + 1e-10)
  ]
  max_violation <- max(violations$violation)

  cat("=== Detailed Balance Check ===\n")
  cat("Max violation: ", max_violation, "\n")
  if (max_violation < 1e-12) {
    cat("Detailed balance holds\n")
  } else {
    cat("Detailed balance violated\n")
  }

  if (max_violation > 1e-10) {
    cat("\nWorst violation: \n")
    print(violations[order(relative_violation, decreasing = TRUE)[1:5],
      c(
        "i",
        "j",
        "forward_flow",
        "reverse_flow",
        "violation",
        "relative_violation"
      )
    ])
  }
  return(violations)
}

#Function to check irreducibility
check_irreducibility <- function(T) {
  n_states <- nrow(T)

  #Create a directed graph from the transition matrix
  # There's an edge i -> j if T[i, j] > 0
  # Build adjacent matrix (1 if transition possible, 0 otherwise)
  adj_matrix <- (T > 1e-10) * 1
  diag(adj_matrix) <- 0

  # Create igraph object
  state_graph <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
  V(state_graph)$name <- paste0("plan_", seq_len(n_states))

  # Check strong connectivity (every state is reachable from every other state)
  is_strongly_connected <- is_connected(state_graph, mode = "strong")

  cat("=== Irreducibility Check ===\n")
  cat("Number of states: ", n_states, "\n")
  cat("Strongly connected: ", is_strongly_connected, "\n")

  if (!is_strongly_connected) {
    cat("\nStates that are not strongly connected: \n")
    components <- components(state_graph, mode = "strong")
    cat("\nNumber of strongly connected components: ", components$no, "\n")

    #Show which states are in which component
    for (comp_idx in seq_len(components$no)) {
      states_in_components <- which(components$membership == comp_idx)
      cat(
        paste0("Component ", comp_idx, ": "),
        paste(states_in_components, collapse = ", "),
        "\n"
      )
    }
  }
  # Visualize the state graph
  plot(
    state_graph,
    vertex.size = 30,
    vertex.label = 1:n_states,
    edge.arrow.size = .5,
    main = "State Space Connectivity"
  )
  list(
    is_irreducible = is_strongly_connected,
    state_graph = state_graph
  )
}

# ============================================================================
# SCENARIO 1: BRIDGE
# ============================================================================
# Purpose: Test geographic transitions across spatial gaps
# - 4 parcels in two disconnected regions: P1—P2 [GAP] P5—P6
# - Two "stations": North (P1-P2) and South (P5-P6)
# - Homogeneous capacities (c_i = 1) and areas (a_i = 6)
# - Target capacity: exactly 3 units
# - 50% LCC requirement: C_LCC >= 1.5 (so need >= 2 units in largest component)
# - Expected feasible space: 4 states
#   * X_A = {1, 2, 5}: North-heavy (LCC in north)
#   * X_B = {1, 2, 6}: North-heavy
#   * X_C = {1, 5, 6}: South-heavy (LCC in south)
#   * X_D = {2, 5, 6}: South-heavy

# Setup
g <- create_scenario1_graph()
constraints <- list(
  min_capacity = 3,
  max_capacity = 3,
  min_area = 10,
  min_density = .1,
  min_lcc_fraction = .5
)
feasible_plans <- find_feasible_plans(g, 3, constraints)

# Run MCMC
initial_plan <- c("1", "2", "5")
n_steps <- 5000
results <- run_mcmc(
  g,
  initial_plan,
  constraints,
  n_steps,
  p_flip = .7,
  p_add = .5,
  verbose = TRUE
)

## Acceptance Rates by Move Type
print(results$stats)

plan_ids <- map_int(results$samples, \(plan) match_plan(plan, feasible_plans))

# Observed frequencies
plan_freqs <- table(plan_ids)
plan_props <- prop.table(plan_freqs)
plan_props # Should be around .25 for each plan

# MCMC Trace Plot
mcmc_trace <- data.table(
  step = seq_along(results$samples),
  plan_id = plan_ids
)


trace_plot <- ggplot(mcmc_trace, aes(x = step, y = plan_id)) +
  geom_line(alpha = 0.6) +
  geom_hline(yintercept = 1:4, linetype = "dashed", alpha = 0.3) +
  labs(title = "MCMC Trace", x = "Step", y = "Plan ID") +
  scale_y_continuous(breaks = 1:4)

trace_plot

# Histogram of Plans# Histogram of samples
hist_samples <- ggplot(mcmc_trace, aes(x = factor(plan_id))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "red") +
  labs(
    title = "Empirical Distribution of Plans",
    x = "Plan ID",
    y = "Proportion",
    subtitle = "Red line = uniform expectation (0.25)"
  ) +
  ylim(0, 0.5)

hist_samples

## Check Transition Matrix
T_results <- compute_transition_matrix(
  g,
  feasible_plans,
  constraints,
  p_flip = .7,
  p_add = .5
)
T_mixed <- T_results$matrix
T_flip <- T_results$T_flip
T_swap <- T_results$T_swap

round(T_swap, 3)
round(T_flip, 3)
round(T_mixed, 3)
# Check detailed balance
violations <- check_detailed_balance(T_mixed)

# Check irreducibility
irreducibility_results <- check_irreducibility(T_mixed)

# ============================================================================
# SCENARIO 2: CONNECTED CHAIN
# ============================================================================
# Purpose: Test basic chain mixing without geographic complications
# - 8 parcels in linear chain: P1—P2—P3—P4—P5—P6—P7—P8
# - Homogeneous capacities (c_i = 1) and areas (a_i = 6)
# - Target capacity: exactly 5 units
# - 50% LCC requirement: C_LCC >= 2.5 (so need >= 3 units in largest component)
# - Expected feasible space: ~40 states

# Setup
g_sc2 <- create_scenario2_graph()
constraints_sc2 <- list(
  min_capacity = 5,
  max_capacity = 5,
  min_area = 10,
  min_density = .1,
  min_lcc_fraction = .5
)
feasible_plans_sc2 <- find_feasible_plans(g_sc2, 5, constraints_sc2)

cat("=== Scenario 2: Connected Chain ===\n")
cat("Total plans:", feasible_plans_sc2$n_plans, "\n")
cat("Feasible plans:", feasible_plans_sc2$n_feasible, "\n")

# Run MCMC
initial_plan_sc2 <- c("1", "2", "3", "4", "5")
n_steps_sc2 <- 25000
results_sc2 <- run_mcmc(
  g_sc2,
  initial_plan_sc2,
  constraints_sc2,
  n_steps_sc2,
  p_flip = .7,
  p_add = .5,
  verbose = TRUE
)

## Acceptance Rates by Move Type
print(results_sc2$stats)

plan_ids_sc2 <- map_int(results_sc2$samples, \(plan) {
  match_plan(plan, feasible_plans_sc2)
})

# Observed frequencies
plan_freqs_sc2 <- table(plan_ids_sc2)
plan_props_sc2 <- prop.table(plan_freqs_sc2)
print(head(plan_props_sc2, 10))

# MCMC Trace Plot
mcmc_trace_sc2 <- data.table(
  step = seq_along(results_sc2$samples),
  plan_id = plan_ids_sc2
)

trace_plot_sc2 <- ggplot(mcmc_trace_sc2, aes(x = step, y = plan_id)) +
  geom_line(alpha = 0.6) +
  labs(title = "Scenario 2: MCMC Trace", x = "Step", y = "Plan ID")

trace_plot_sc2

# Histogram of Plans
expected_uniform_sc2 <- 1 / feasible_plans_sc2$n_feasible
hist_samples_sc2 <- ggplot(mcmc_trace_sc2, aes(x = factor(plan_id))) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  geom_hline(
    yintercept = expected_uniform_sc2,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "Scenario 2: Empirical Distribution of Plans",
    x = "Plan ID",
    y = "Proportion",
    subtitle = glue(
      "Red line = uniform expectation ({round(expected_uniform_sc2, 3)})"
    )
  )

hist_samples_sc2

## Check Transition Matrix
T_results_sc2 <- compute_transition_matrix(
  g_sc2,
  feasible_plans_sc2,
  constraints_sc2,
  p_flip = .7,
  p_add = .5
)
T_mixed_sc2 <- T_results_sc2$matrix
T_flip_sc2 <- T_results_sc2$T_flip
T_swap_sc2 <- T_results_sc2$T_swap

# Check detailed balance
violations_sc2 <- check_detailed_balance(T_mixed_sc2)

# Check irreducibility
irreducibility_results_sc2 <- check_irreducibility(T_mixed_sc2)
