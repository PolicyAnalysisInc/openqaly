#' Calculate Incremental Cost-Effectiveness Acceptability Curves from PSA
#'
#' Calculates the probability that each strategy is the most cost-effective
#' at different willingness-to-pay (WTP) thresholds based on probabilistic
#' sensitivity analysis results. This performs incremental (multi-way) comparison
#' where all strategies are compared simultaneously.
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param wtp Numeric vector of WTP thresholds to evaluate. Default is
#'   seq(0, 100000, by = 5000)
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (triggers separate calculations per group)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A tibble with columns:
#'   \item{wtp}{Willingness-to-pay threshold}
#'   \item{strategy}{Strategy name}
#'   \item{probability}{Probability of being most cost-effective}
#'   \item{group}{Group name (if applicable)}
#'
#' @details
#' The incremental CEAC shows the probability that each strategy has the highest
#' net monetary benefit (NMB) at each WTP threshold in a multi-way comparison.
#' NMB is calculated as: \code{NMB = outcome * wtp - cost}
#'
#' This represents the absolute NMB versus doing nothing (0 cost, 0 outcome),
#' not pairwise comparisons between strategies. For pairwise comparisons, use
#' \code{\link{calculate_pairwise_ceac}}.
#'
#' For each WTP threshold and simulation, the strategy with the highest NMB
#' is identified. The probability for each strategy is the proportion of
#' simulations where it has the highest NMB.
#'
#' When multiple groups are present, the calculation is performed separately
#' for each group and the group column identifies which group each probability
#' corresponds to.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Calculate incremental CEAC
#' ceac <- calculate_incremental_ceac(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP range
#' ceac <- calculate_incremental_ceac(psa_results, "total_qalys", "total_cost",
#'                                     wtp = seq(0, 200000, by = 10000))
#' }
#'
#' @export
calculate_incremental_ceac <- function(results,
                                       outcome_summary,
                                       cost_summary,
                                       wtp = seq(0, 100000, by = 5000),
                                       group = "aggregated",
                                       strategies = NULL,
                                       discounted = FALSE) {

  # Check if results have PSA simulations
  if (!"simulation" %in% names(results$aggregated) &&
      !"simulation" %in% names(results$segments)) {
    stop(paste("Results do not contain PSA simulations.",
               "Use run_psa() instead of run_model()."))
  }

  # Extract PSA simulation data
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Get unique groups and strategies
  groups <- unique(psa_data$group)
  all_strategies <- unique(psa_data$strategy)

  # Initialize results
  ceac_results <- list()

  # Process each group
  for (grp in groups) {
    grp_data <- psa_data %>%
      filter(group == grp)

    # Get simulations for this group
    simulations <- unique(grp_data$simulation)
    n_sim <- length(simulations)

    # Initialize probability matrix
    prob_matrix <- matrix(0,
                          nrow = length(wtp),
                          ncol = length(all_strategies))
    colnames(prob_matrix) <- all_strategies

    # Calculate probabilities for each WTP threshold
    for (w_idx in seq_along(wtp)) {
      wtp_val <- wtp[w_idx]

      # Calculate NMB for each strategy in each simulation
      nmb_data <- grp_data %>%
        mutate(nmb = outcome * wtp_val - cost) %>%
        select(simulation, strategy, nmb)

      # Find strategy with max NMB in each simulation
      max_nmb <- nmb_data %>%
        group_by(simulation) %>%
        slice_max(nmb, n = 1, with_ties = FALSE) %>%
        ungroup()

      # Count wins for each strategy
      wins <- table(max_nmb$strategy)

      # Calculate probabilities
      for (strat in all_strategies) {
        if (strat %in% names(wins)) {
          prob_matrix[w_idx, strat] <- wins[strat] / n_sim
        }
      }
    }

    # Convert to tibble
    grp_ceac <- expand.grid(
      wtp = wtp,
      strategy = all_strategies,
      stringsAsFactors = FALSE
    ) %>%
      as_tibble() %>%
      mutate(
        group = grp,
        probability = as.vector(prob_matrix)
      )

    ceac_results[[length(ceac_results) + 1]] <- grp_ceac
  }

  # Combine results
  bind_rows(ceac_results) %>%
    select(wtp, strategy, probability, group) %>%
    arrange(group, wtp, desc(probability))
}


#' Extract PSA Simulation Data
#'
#' Helper function to extract simulation-level costs and outcomes from PSA results.
#'
#' @inheritParams calculate_incremental_ceac
#'
#' @return A tibble with columns: simulation, strategy, group, cost, outcome
#'
#' @keywords internal
get_psa_simulations <- function(results,
                               outcome_summary,
                               cost_summary,
                               group = "aggregated",
                               strategies = NULL,
                               discounted = FALSE) {

  # Determine source data
  if (!is.null(group) && group == "aggregated") {
    source_data <- results$aggregated
    if (!"group" %in% names(source_data)) {
      source_data <- source_data %>% mutate(group = "Aggregated")
    }
  } else if (is.null(group) || group == "all") {
    # Combine aggregated and segments
    source_data <- bind_rows(
      results$aggregated %>% mutate(group = "Aggregated"),
      results$segments
    )
  } else {
    # Specific group(s) - validate using helper
    for (g in group) {
      check_group_exists(g, results)
    }
    if (!is.null(results$segments) && nrow(results$segments) > 0) {
      # Use technical names for filtering
      source_data <- results$segments %>%
        filter(group %in% group)
    } else {
      # Groups were validated to exist, but no segments data
      stop(sprintf("No segment data available for groups: %s", paste(group, collapse = ", ")))
    }
  }

  # Filter strategies if specified
  if (!is.null(strategies)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies, results$metadata)
    source_data <- source_data %>%
      filter(strategy %in% strategies)
  }

  # Check for simulation column
  if (!"simulation" %in% names(source_data)) {
    stop("No simulation data found. Ensure model was run with run_psa().")
  }

  # Extract cost summaries
  cost_data <- extract_psa_summaries(
    source_data,
    summary_name = cost_summary,
    value_type = "cost",
    discounted = discounted
  ) %>%
    rename(cost = amount)

  # Extract outcome summaries
  outcome_data <- extract_psa_summaries(
    source_data,
    summary_name = outcome_summary,
    value_type = "outcome",
    discounted = discounted
  ) %>%
    rename(outcome = amount)

  # Combine cost and outcome data
  psa_data <- cost_data %>%
    inner_join(outcome_data,
               by = c("simulation", "strategy", "group"))

  # Map display names if metadata available
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      psa_data$strategy <- map_names(psa_data$strategy,
                                     results$metadata$strategies,
                                     "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      psa_data$group <- map_names(psa_data$group,
                                  results$metadata$groups,
                                  "display_name")
    }
  }

  psa_data
}


#' Extract PSA Summary Values
#'
#' Helper function to extract summary values from PSA simulation data.
#'
#' @param source_data PSA results data with simulation column
#' @param summary_name Name of the summary to extract
#' @param value_type Type of value: "cost" or "outcome"
#' @param discounted Logical. Use discounted values?
#'
#' @return A tibble with simulation, strategy, group, and amount columns
#'
#' @keywords internal
extract_psa_summaries <- function(source_data,
                                 summary_name,
                                 value_type,
                                 discounted = FALSE) {

  # Determine which column to use
  summary_col <- if (discounted) "summaries_discounted" else "summaries"

  # Check if summary column exists
  if (!summary_col %in% names(source_data)) {
    return(tibble(
      simulation = source_data$simulation,
      strategy = source_data$strategy,
      group = source_data$group,
      amount = NA_real_
    ))
  }

  # Vectorized extraction and aggregation
  result <- source_data %>%
    select(simulation, strategy, group, summary_data = !!sym(summary_col)) %>%
    unnest(summary_data, keep_empty = TRUE) %>%
    filter(summary == summary_name | is.na(summary)) %>%
    group_by(simulation, strategy, group) %>%
    summarize(amount = sum(amount, na.rm = TRUE), .groups = "drop")

  # Handle rows where summary wasn't found
  # If a (simulation, strategy, group) combo is missing, fill with NA
  all_combos <- source_data %>%
    distinct(simulation, strategy, group)

  result <- all_combos %>%
    left_join(result, by = c("simulation", "strategy", "group")) %>%
    mutate(amount = if_else(is.na(amount), NA_real_, amount))

  result
}


#' Calculate Incremental Cost-Effectiveness Acceptability Frontier
#'
#' Identifies which strategy has the highest expected net monetary benefit
#' at each willingness-to-pay threshold in a multi-way (incremental) comparison.
#'
#' @inheritParams calculate_incremental_ceac
#'
#' @return A tibble with columns:
#'   \item{wtp}{Willingness-to-pay threshold}
#'   \item{optimal_strategy}{Strategy with highest expected NMB}
#'   \item{probability}{Probability that optimal strategy is best}
#'   \item{expected_nmb}{Expected NMB of optimal strategy}
#'   \item{group}{Group name (if applicable)}
#'
#' @details
#' The incremental CE acceptability frontier shows which strategy would be
#' chosen at each WTP threshold based on expected (mean) NMB across simulations
#' in a multi-way comparison. It also provides the probability that this optimal
#' choice is actually the best.
#'
#' When multiple groups are present, the optimal strategy is identified separately
#' for each group, and the group column identifies which group each result
#' corresponds to.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Calculate frontier
#' frontier <- calculate_incremental_ceac_frontier(psa_results, "total_qalys", "total_cost")
#' }
#'
#' @export
calculate_incremental_ceac_frontier <- function(results,
                                                outcome_summary,
                                                cost_summary,
                                                wtp = seq(0, 100000, by = 5000),
                                                group = "aggregated",
                                                strategies = NULL,
                                                discounted = FALSE) {

  # First get CEAC probabilities
  ceac <- calculate_incremental_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Get PSA data for expected values
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Calculate expected costs and outcomes
  expected_values <- psa_data %>%
    group_by(strategy, group) %>%
    summarize(
      expected_cost = mean(cost, na.rm = TRUE),
      expected_outcome = mean(outcome, na.rm = TRUE),
      .groups = "drop"
    )

  # Process each group
  frontier_results <- list()
  groups <- unique(ceac$group)

  for (grp in groups) {
    grp_ceac <- ceac %>% filter(group == grp)
    grp_expected <- expected_values %>% filter(group == grp)

    # For each WTP, find optimal strategy
    grp_frontier <- lapply(wtp, function(w) {
      # Calculate expected NMB for each strategy
      nmb_values <- grp_expected %>%
        mutate(expected_nmb = expected_outcome * w - expected_cost) %>%
        arrange(desc(expected_nmb))

      # Get optimal strategy
      optimal_strat <- nmb_values$strategy[1]
      optimal_nmb <- nmb_values$expected_nmb[1]

      # Get probability from CEAC
      prob <- grp_ceac %>%
        filter(wtp == w, strategy == optimal_strat) %>%
        pull(probability)

      tibble(
        wtp = w,
        optimal_strategy = optimal_strat,
        probability = prob,
        expected_nmb = optimal_nmb,
        group = grp
      )
    })

    frontier_results[[length(frontier_results) + 1]] <- bind_rows(grp_frontier)
  }

  # Combine and return
  bind_rows(frontier_results)
}


#' Calculate Pairwise CEAC
#'
#' Calculates the probability that one strategy is more cost-effective than
#' another at different WTP thresholds.
#'
#' @inheritParams calculate_incremental_ceac
#' @param intervention Reference strategy for comparison (intervention perspective: A vs. B, A vs. C).
#'   Mutually exclusive with comparator.
#' @param comparator Reference strategy for comparison (comparator perspective: B vs. A, C vs. A).
#'   Mutually exclusive with intervention.
#'
#' @return A tibble with columns:
#'   \item{wtp}{Willingness-to-pay threshold}
#'   \item{strategy}{Strategy being compared}
#'   \item{probability}{Probability based on comparison direction}
#'   \item{group}{Group name (if applicable)}
#'   \item{comparator}{Reference strategy name}
#'
#' @details
#' Pairwise comparisons calculate P(intervention > comparator) for each pair.
#' The comparison direction is determined by which parameter is used:
#'
#' \strong{Intervention mode} (intervention = 'A'): A is the intervention in all comparisons
#' - Calculates P(A > B), P(A > C), etc.
#' - Output rows show other strategies as comparators
#'
#' \strong{Comparator mode} (comparator = 'A'): A is the comparator in all comparisons
#' - Calculates P(B > A), P(C > A), etc.
#' - Output rows show other strategies as interventions
#'
#' The function accepts both technical names (e.g., "treatment_a") and display
#' names (e.g., "Treatment A") for the fixed strategy.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Comparator perspective: How do other strategies compare to control?
#' pairwise_ceac <- calculate_pairwise_ceac(
#'   psa_results, "total_qalys", "total_cost",
#'   comparator = "control"
#' )
#'
#' # Intervention perspective: How does new treatment compare to others?
#' pairwise_ceac <- calculate_pairwise_ceac(
#'   psa_results, "total_qalys", "total_cost",
#'   intervention = "new_treatment"
#' )
#' }
#'
#' @export
calculate_pairwise_ceac <- function(results,
                                   outcome_summary,
                                   cost_summary,
                                   intervention = NULL,
                                   comparator = NULL,
                                   wtp = seq(0, 100000, by = 5000),
                                   group = "aggregated",
                                   strategies = NULL,
                                   discounted = FALSE) {

  # Validate that exactly one of intervention or comparator is provided
  if (is.null(intervention) && is.null(comparator)) {
    stop("One of 'intervention' or 'comparator' must be provided")
  }
  if (!is.null(intervention) && !is.null(comparator)) {
    stop("Only one of 'intervention' or 'comparator' should be provided, not both")
  }

  # Determine which strategy is fixed and the comparison mode
  fixed_strategy <- if (!is.null(comparator)) comparator else intervention
  use_intervention <- !is.null(intervention)

  # Get PSA simulation data
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Check fixed strategy exists
  all_strategies <- unique(psa_data$strategy)

  # Try to find fixed strategy by matching against display names
  if (!fixed_strategy %in% all_strategies) {
    # If not found, try mapping technical name to display name
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      # Try mapping: technical name -> display name
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == fixed_strategy]

      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        fixed_strategy <- matched_display[1]
      } else {
        stop(sprintf("Fixed strategy '%s' not found in results", fixed_strategy))
      }
    } else {
      stop(sprintf("Fixed strategy '%s' not found in results", fixed_strategy))
    }
  }

  # Get unique groups and other strategies
  groups <- unique(psa_data$group)
  all_strategies <- unique(psa_data$strategy)
  other_strategies <- setdiff(all_strategies, fixed_strategy)

  if (length(other_strategies) == 0) {
    stop("No other strategies to compare")
  }

  # Create grid of all (simulation, group, comparison) combinations
  n_sim <- length(unique(psa_data$simulation))
  n_groups <- length(groups)
  n_comparisons <- length(other_strategies)

  comparison_grid <- expand.grid(
    simulation = unique(psa_data$simulation),
    group = groups,
    other_strategy = other_strategies,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Join with fixed strategy data
  fixed_data <- psa_data %>%
    filter(strategy == fixed_strategy) %>%
    select(simulation, group, cost_fixed = cost, outcome_fixed = outcome)

  comparison_data <- comparison_grid %>%
    left_join(fixed_data, by = c("simulation", "group"))

  # Join with other strategies data
  other_data <- psa_data %>%
    filter(strategy != fixed_strategy) %>%
    select(simulation, group, strategy, cost_other = cost, outcome_other = outcome)

  comparison_data <- comparison_data %>%
    left_join(other_data, by = c("simulation", "group", "other_strategy" = "strategy"))

  # Assign intervention and comparator based on mode
  if (use_intervention) {
    comparison_data <- comparison_data %>%
      mutate(
        cost_intervention = cost_fixed,
        cost_comp = cost_other,
        outcome_intervention = outcome_fixed,
        outcome_comp = outcome_other
      )
  } else {
    comparison_data <- comparison_data %>%
      mutate(
        cost_intervention = cost_other,
        cost_comp = cost_fixed,
        outcome_intervention = outcome_other,
        outcome_comp = outcome_fixed
      )
  }

  comparison_data <- comparison_data %>%
    arrange(simulation, group, other_strategy)

  # Reshape to matrices for vectorized calculation
  n_cols <- n_groups * n_comparisons
  cost_intervention_mat <- matrix(comparison_data$cost_intervention, nrow = n_sim, ncol = n_cols, byrow = TRUE)
  cost_comp_mat <- matrix(comparison_data$cost_comp, nrow = n_sim, ncol = n_cols, byrow = TRUE)
  outcome_intervention_mat <- matrix(comparison_data$outcome_intervention, nrow = n_sim, ncol = n_cols, byrow = TRUE)
  outcome_comp_mat <- matrix(comparison_data$outcome_comp, nrow = n_sim, ncol = n_cols, byrow = TRUE)

  # Vectorized probability calculation across all WTP values
  # Result: matrix with rows = (group, comparison) pairs, columns = WTP values
  probs_matrix <- sapply(wtp, function(w) {
    # Calculate incremental NMB: P(intervention > comparator)
    delta_nmb <- (outcome_intervention_mat - outcome_comp_mat) * w - (cost_intervention_mat - cost_comp_mat)
    colMeans(delta_nmb > 0)
  })

  # Convert back to tibble with proper labels
  comparison_labels <- comparison_grid %>%
    distinct(group, other_strategy) %>%
    arrange(group, other_strategy)

  # Expand to include all WTP values with correct probability extraction
  result <- comparison_labels %>%
    mutate(row_id = row_number()) %>%
    crossing(tibble(wtp = wtp, wtp_idx = seq_along(wtp))) %>%
    mutate(probability = probs_matrix[cbind(row_id, wtp_idx)]) %>%
    transmute(
      wtp = wtp,
      strategy = other_strategy,
      probability = probability,
      group = group,
      comparator = fixed_strategy
    )

  # Map display names if metadata available and requested
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      result$strategy <- map_names(result$strategy,
                                   results$metadata$strategies,
                                   "display_name")
      result$comparator <- map_names(result$comparator,
                                     results$metadata$strategies,
                                     "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      result$group <- map_names(result$group,
                               results$metadata$groups,
                               "display_name")
    }
  }

  # For intervention mode, prepend "vs. " to clarify these are comparators
  if (use_intervention) {
    result$strategy <- paste0("vs. ", result$strategy)
  }

  result %>%
    select(wtp, strategy, probability, group, comparator)
}



#' Calculate Expected Value of Perfect Information (EVPI)
#'
#' Calculates the Expected Value of Perfect Information from probabilistic
#' sensitivity analysis results. EVPI represents the expected value of
#' eliminating all parameter uncertainty - the maximum amount a decision-maker
#' should be willing to pay for perfect information about all uncertain parameters.
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param wtp Numeric vector of WTP thresholds to evaluate. Default is
#'   seq(0, 100000, by = 5000)
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (triggers separate calculations per group)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A tibble with columns:
#'   \item{wtp}{Willingness-to-pay threshold}
#'   \item{evpi}{Expected value of perfect information}
#'   \item{group}{Group name (if applicable)}
#'
#' @details
#' EVPI is calculated using the following algorithm:
#'
#' 1. For each WTP threshold:
#'    - Calculate NMB for each strategy in each simulation: NMB = outcome * wtp - cost
#'    - Identify the strategy with highest expected NMB across all simulations (overall optimal)
#'    - For each simulation, find the strategy with highest NMB (optimal with perfect information)
#'    - Calculate the opportunity loss: max(0, NMB_sim_optimal - NMB_sim_overall_optimal)
#'    - EVPI = mean opportunity loss across all simulations
#'
#' EVPI represents the expected cost of making decisions under uncertainty. It provides
#' an upper bound on the value of conducting further research to reduce uncertainty.
#'
#' When EVPI = 0, the same strategy is optimal in all simulations (no uncertainty
#' affects the decision). Higher EVPI indicates greater decision uncertainty and
#' potentially higher value of additional research.
#'
#' When multiple groups are present, EVPI is calculated separately for each group.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Calculate EVPI
#' evpi <- calculate_evpi(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP range
#' evpi <- calculate_evpi(psa_results, "total_qalys", "total_cost",
#'                        wtp = seq(0, 200000, by = 10000))
#' }
#'
#' @export
calculate_evpi <- function(results,
                           outcome_summary,
                           cost_summary,
                           wtp = seq(0, 100000, by = 5000),
                           group = "aggregated",
                           strategies = NULL,
                           discounted = FALSE) {

  # Check if results have PSA simulations
  if (!"simulation" %in% names(results$aggregated) &&
      !"simulation" %in% names(results$segments)) {
    stop(paste("Results do not contain PSA simulations.",
               "Use run_psa() instead of run_model()."))
  }

  # Extract PSA simulation data
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Get unique groups
  groups <- unique(psa_data$group)

  # Initialize results
  evpi_results <- list()

  # Process each group
  for (grp in groups) {
    grp_data <- psa_data %>%
      filter(group == grp)

    # Get simulations for this group
    simulations <- unique(grp_data$simulation)
    n_sim <- length(simulations)

    # Calculate EVPI for each WTP threshold
    evpi_values <- sapply(wtp, function(wtp_val) {
      # Calculate NMB for each strategy in each simulation
      nmb_data <- grp_data %>%
        mutate(nmb = outcome * wtp_val - cost) %>%
        select(simulation, strategy, nmb)

      # Calculate expected NMB for each strategy (mean across simulations)
      expected_nmb <- nmb_data %>%
        group_by(strategy) %>%
        summarize(expected_nmb = mean(nmb, na.rm = TRUE), .groups = "drop")

      # Identify overall optimal strategy (highest expected NMB)
      overall_optimal_strategy <- expected_nmb %>%
        slice_max(expected_nmb, n = 1, with_ties = FALSE) %>%
        pull(strategy)

      # For each simulation, calculate opportunity loss
      opportunity_loss <- nmb_data %>%
        group_by(simulation) %>%
        summarize(
          # NMB of best strategy in this simulation
          max_nmb = max(nmb, na.rm = TRUE),
          # NMB of overall optimal strategy in this simulation
          overall_optimal_nmb = nmb[strategy == overall_optimal_strategy][1],
          .groups = "drop"
        ) %>%
        mutate(
          # Opportunity loss is the difference (cannot be negative)
          loss = pmax(0, max_nmb - overall_optimal_nmb)
        )

      # EVPI is the mean opportunity loss
      mean(opportunity_loss$loss, na.rm = TRUE)
    })

    # Build result for this group
    grp_evpi <- tibble(
      wtp = wtp,
      evpi = evpi_values,
      group = grp
    )

    evpi_results[[length(evpi_results) + 1]] <- grp_evpi
  }

  # Combine results
  bind_rows(evpi_results) %>%
    select(wtp, evpi, group) %>%
    arrange(group, wtp)
}


#' Extract Sampled Parameter Values from PSA Results
#'
#' Extracts sampled input parameter values from PSA results, handling
#' strategy/group-specific variables with appropriate labeling.
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param variables Character vector of variable names to extract (NULL for all)
#' @param group Group selection with three modes:
#'   \itemize{
#'     \item NULL or "all": include all groups
#'     \item Single value: filter to that group
#'     \item Vector same length as variables: specify group for each variable
#'   }
#' @param strategies Strategy selection with three modes:
#'   \itemize{
#'     \item NULL: include all strategies
#'     \item Single value: filter to that strategy
#'     \item Vector same length as variables: specify strategy for each variable
#'   }
#'
#' @return A tibble with columns:
#'   \item{simulation}{Simulation number}
#'   \item{...}{One column per unique (variable, strategy, group) combination}
#'
#' @details
#' This function extracts sampled parameter values from PSA results and creates
#' appropriately labeled columns using variable display names. For strategy/group-specific
#' variables, ensure each has a unique display_name defined in add_variable().
#'
#' Each simulation appears once in the output, with all sampled parameters from
#' all strategy/group combinations included as separate columns.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Extract all sampled parameters
#' sampled_params <- get_sampled_parameters(psa_results)
#'
#' # Extract specific variables (all strategies/groups)
#' sampled_params <- get_sampled_parameters(
#'   psa_results,
#'   variables = c("p_transition", "utility_well", "cost_treatment")
#' )
#'
#' # Filter to specific strategy
#' sampled_params <- get_sampled_parameters(
#'   psa_results,
#'   variables = c("p_transition", "utility_well"),
#'   strategies = "treatment_a"
#' )
#'
#' # Extract specific (variable, strategy, group) combinations
#' sampled_params <- get_sampled_parameters(
#'   psa_results,
#'   variables = c("p_well_to_sick", "util_well", "cost_med"),
#'   strategies = c("treatment_a", "treatment_b", "treatment_c"),
#'   group = c("group_1", "group_1", "group_1")
#' )
#' # This extracts:
#' # - p_well_to_sick from treatment_a, group_1
#' # - util_well from treatment_b, group_1
#' # - cost_med from treatment_c, group_1
#' }
#'
#' @export
get_sampled_parameters <- function(results,
                                   variables = NULL,
                                   group = NULL,
                                   strategies = NULL) {

  # Check if results have PSA simulations
  if (!"simulation" %in% names(results$segments)) {
    stop("Results do not contain PSA simulations. Use run_psa() instead of run_model().")
  }

  # Check for parameter_overrides column
  if (!"parameter_overrides" %in% names(results$segments)) {
    stop("Results do not contain parameter override values.")
  }

  # Check if using tuple specification (vector-form strategies/group)
  use_tuple_specification <- FALSE
  desired_tuples <- NULL

  if (!is.null(variables)) {
    # Check if strategies is a vector matching variables length
    if (!is.null(strategies) && length(strategies) > 1) {
      if (length(strategies) != length(variables)) {
        stop("When 'strategies' is a vector, it must have the same length as 'variables'")
      }
      use_tuple_specification <- TRUE
    }

    # Check if group is a vector matching variables length
    if (!is.null(group) && length(group) > 1 && !group[1] %in% c("aggregated", "all")) {
      if (length(group) != length(variables)) {
        stop("When 'group' is a vector, it must have the same length as 'variables'")
      }
      use_tuple_specification <- TRUE
    }

    # Create desired tuples if using tuple specification
    if (use_tuple_specification) {
      desired_tuples <- tibble(
        variable = variables,
        strategy = if (!is.null(strategies) && length(strategies) == length(variables))
          strategies else rep(NA_character_, length(variables)),
        group = if (!is.null(group) && length(group) == length(variables))
          group else rep(NA_character_, length(variables))
      )
    }
  } else if (!is.null(strategies) && length(strategies) > 1) {
    stop("Cannot use vector-form 'strategies' when 'variables' is NULL")
  } else if (!is.null(group) && length(group) > 1) {
    stop("Cannot use vector-form 'group' when 'variables' is NULL")
  }

  # Filter source data by group and strategies (only if NOT using tuple specification)
  source_data <- results$segments

  if (!use_tuple_specification) {
    # Use global filtering when not using tuple specification
    if (!is.null(group) && !group %in% c("aggregated", "all")) {
      # Validate groups exist using helper
      for (g in group) {
        check_group_exists(g, results)
      }

      # Filter to specific group(s)
      source_data <- source_data %>%
        filter(group %in% !!group)
    }

    # Filter strategies if specified
    if (!is.null(strategies)) {
      source_data <- source_data %>%
        filter(strategy %in% strategies)

      if (nrow(source_data) == 0) {
        stop("No data found for specified strategies")
      }
    }
  }
  # If using tuple specification, we'll filter at the variable level in the loop

  # Get metadata for name mappings
  metadata <- results$metadata

  # Create name mapping functions
  map_var_name <- function(name) {
    if (!is.null(metadata$variables)) {
      var_meta <- metadata$variables %>% filter(name == !!name)
      if (nrow(var_meta) > 0 && !is.na(var_meta$display_name[1])) {
        return(var_meta$display_name[1])
      }
    }
    return(name)
  }

  map_strategy_name <- function(name) {
    if (!is.null(metadata$strategies)) {
      strat_meta <- metadata$strategies %>% filter(name == !!name)
      if (nrow(strat_meta) > 0 && !is.na(strat_meta[["display_name"]][1])) {
        return(strat_meta[["display_name"]][1])
      }
    }
    return(name)
  }

  map_group_name <- function(name) {
    if (!is.null(metadata$groups)) {
      group_meta <- metadata$groups %>% filter(name == !!name)
      if (nrow(group_meta) > 0 && !is.na(group_meta[["display_name"]][1])) {
        return(group_meta[["display_name"]][1])
      }
    }
    return(name)
  }

  # Extract all sampled values and create properly labeled columns
  all_params <- list()

  for (i in 1:nrow(source_data)) {
    row <- source_data[i, ]
    sim <- row$simulation
    strat <- row$strategy
    grp <- row$group
    override_vals <- row$parameter_overrides[[1]]

    if (length(override_vals) == 0) next

    # Get display names
    strat_display <- map_strategy_name(strat)
    grp_display <- map_group_name(grp)

    for (var_name in names(override_vals)) {
      # Filter based on specification mode
      if (use_tuple_specification) {
        # Check if this (variable, strategy, group) tuple is in desired_tuples
        matching_tuple <- desired_tuples %>%
          filter(
            variable == var_name,
            (is.na(strategy) | strategy == strat),
            (is.na(group) | group == grp)
          )

        if (nrow(matching_tuple) == 0) next  # Skip this combination
      } else {
        # Use simple variable filter
        if (!is.null(variables) && !var_name %in% variables) next
      }

      var_value <- override_vals[[var_name]]

      # Get variable display name (no automatic suffix addition)
      col_label <- map_var_name(var_name)

      # Create internal technical name for uniqueness
      col_key <- paste(var_name, strat, grp, sep = "__")

      # Store value
      if (is.null(all_params[[col_key]])) {
        all_params[[col_key]] <- list(
          label = col_label,
          values = list()
        )
      }

      all_params[[col_key]]$values[[as.character(sim)]] <- var_value
    }
  }

  # Validate unique display names before creating tibble
  labels <- sapply(all_params, function(p) p$label)
  dup_labels <- unique(labels[duplicated(labels)])

  if (length(dup_labels) > 0) {
    # Build error message showing which parameters have duplicate labels
    dup_errors <- character()
    for (dup_label in dup_labels) {
      dup_keys <- names(all_params)[labels == dup_label]
      # Parse col_key back to name__strategy__group
      param_details <- sapply(dup_keys, function(key) {
        parts <- strsplit(key, "__")[[1]]
        if (length(parts) == 3) {
          sprintf("variable=%s, strategy=%s, group=%s", parts[1], parts[2], parts[3])
        } else {
          key
        }
      })
      dup_errors <- c(dup_errors, sprintf(
        "Display name '%s' is used by multiple sampled parameters:\n  %s\n",
        dup_label,
        paste(param_details, collapse = "\n  ")
      ))
    }
    stop(
      paste(dup_errors, collapse = "\n"),
      "\nEach sampled parameter must have a unique display name. ",
      "Specify different display names in add_variable() for strategy/group-specific variables.",
      call. = FALSE
    )
  }

  # Convert to tibble format
  simulations <- unique(source_data$simulation)
  result <- tibble(simulation = simulations)

  for (col_key in names(all_params)) {
    param_info <- all_params[[col_key]]
    col_label <- param_info$label
    values <- param_info$values

    # Create vector of values for all simulations
    col_values <- sapply(simulations, function(sim) {
      val <- values[[as.character(sim)]]
      if (is.null(val)) NA_real_ else val
    })

    result[[col_label]] <- col_values
  }

  return(result)
}