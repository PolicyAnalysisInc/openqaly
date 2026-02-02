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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategy names to include (NULL for all)
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
                                       groups = "overall",
                                       strategies = NULL) {

  # Check if results have PSA simulations
  if (!"simulation" %in% names(results$aggregated) &&
      !"simulation" %in% names(results$segments)) {
    stop(paste("Results do not contain PSA simulations.",
               "Use run_psa() instead of run_model()."))
  }

  # Extract PSA simulation data (always discounted for CE analysis)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Get unique groups and strategies
  unique_groups <- unique(psa_data$group)
  all_strategies <- unique(psa_data$strategy)

  # Initialize results
  ceac_results <- list()

  # Process each group
  for (grp in unique_groups) {
    grp_data <- psa_data %>%
      filter(.data$group == grp)

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
        mutate(nmb = .data$outcome * wtp_val - .data$cost) %>%
        select("simulation", "strategy", nmb = "nmb")

      # Find strategy with max NMB in each simulation
      max_nmb <- nmb_data %>%
        group_by(.data$simulation) %>%
        slice_max(.data$nmb, n = 1, with_ties = FALSE) %>%
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
    select("wtp", "strategy", "probability", "group") %>%
    arrange(.data$group, .data$wtp, desc(.data$probability))
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
                               groups = "overall",
                               strategies = NULL) {

  # Determine source data based on groups selection
  # Handle special values: "overall", "all", "all_groups", NULL
  if (!is.null(groups) && length(groups) == 1 && groups %in% c("aggregated", "overall")) {
    # Single overall/aggregated selection
    source_data <- results$aggregated
    if (!"group" %in% names(source_data) || all(source_data$group == "_aggregated")) {
      source_data <- source_data %>% mutate(group = "Overall")
    }
  } else if (is.null(groups) || (length(groups) == 1 && groups == "all")) {
    # All groups + overall
    source_data <- bind_rows(
      results$aggregated %>% mutate(group = "Overall"),
      results$segments
    )
  } else if (length(groups) == 1 && groups == "all_groups") {
    # All groups without overall
    source_data <- results$segments
  } else {
    # Specific group(s) - may include "overall"
    has_overall <- "overall" %in% tolower(groups)
    specific_groups <- groups[!tolower(groups) %in% c("overall", "aggregated")]

    source_data <- NULL

    # Include overall if requested
    if (has_overall) {
      overall_data <- results$aggregated
      if (!"group" %in% names(overall_data)) {
        overall_data <- overall_data %>% mutate(group = "Overall")
      }
      source_data <- overall_data
    }

    # Include specific groups if any
    if (length(specific_groups) > 0) {
      # Validate specific groups exist
      for (g in specific_groups) {
        check_group_exists(g, results)
      }
      if (!is.null(results$segments) && nrow(results$segments) > 0) {
        segment_data <- results$segments %>%
          filter(.data$group %in% specific_groups)
        source_data <- if (is.null(source_data)) segment_data else bind_rows(source_data, segment_data)
      } else if (!has_overall) {
        stop(sprintf("No segment data available for groups: %s", paste(specific_groups, collapse = ", ")))
      }
    }

    if (is.null(source_data) || nrow(source_data) == 0) {
      stop("No data found for specified groups")
    }
  }

  # Filter strategies if specified
  if (!is.null(strategies)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies, results$metadata)
    source_data <- source_data %>%
      filter(.data$strategy %in% !!strategies)
  }

  # Check for simulation column
  if (!"simulation" %in% names(source_data)) {
    stop("No simulation data found. Ensure model was run with run_psa().")
  }

  # Extract cost summaries (always discounted for CE analysis)
  cost_data <- extract_psa_summaries(
    source_data,
    summary_name = cost_summary,
    value_type = "cost"
  ) %>%
    rename(cost = "amount")

  # Extract outcome summaries (always discounted for CE analysis)
  outcome_data <- extract_psa_summaries(
    source_data,
    summary_name = outcome_summary,
    value_type = "outcome"
  ) %>%
    rename(outcome = "amount")

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


#' Get PSA Outcome Simulations
#'
#' Extract simulation-level outcome data from PSA results for density plots.
#' Supports both absolute mode (raw outcomes per strategy) and difference mode
#' (outcome differences between intervention/comparator pairs).
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param outcome_summary Name of the outcome summary to extract (e.g., "total_qalys")
#' @param interventions Reference strategies for comparison (intervention perspective).
#'   Cannot be used with \code{strategies}.
#' @param comparators Reference strategies for comparison (comparator perspective).
#'   Cannot be used with \code{strategies}.
#' @param groups Which groups to include. Options: "overall" (default), "all",
#'   "all_groups", or a character vector of specific group names.
#' @param strategies Character vector of strategies to include. For absolute outcome
#'   values. Cannot be used with \code{interventions} or \code{comparators}.
#' @param discounted Logical. Use discounted outcome values? Default TRUE.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{simulation}: simulation index
#'     \item \code{strategy} (absolute mode) or \code{comparison} (difference mode): strategy/comparison label
#'     \item \code{group}: group name
#'     \item \code{outcome}: outcome value (raw or difference)
#'   }
#'
#' @export
get_psa_outcome_simulations <- function(results,
                                        outcome_summary,
                                        interventions = NULL,
                                        comparators = NULL,
                                        groups = "overall",
                                        strategies = NULL,
                                        discounted = TRUE) {

  # Validate mutual exclusivity of strategies vs interventions/comparators

  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'. ",
         "Use strategies for absolute values or interventions/comparators for differences.")
  }

  # Require at least one mode to be specified
  if (is.null(strategies) && is.null(interventions) && is.null(comparators)) {
    stop("Either 'strategies' (for absolute values) or at least one of ",
         "'interventions'/'comparators' (for differences) must be provided.")
  }

  # Determine source data based on groups selection (same logic as get_psa_simulations)
  if (!is.null(groups) && length(groups) == 1 && groups %in% c("aggregated", "overall")) {
    source_data <- results$aggregated
    if (!"group" %in% names(source_data) || all(source_data$group == "_aggregated")) {
      source_data <- source_data %>% mutate(group = "Overall")
    }
  } else if (is.null(groups) || (length(groups) == 1 && groups == "all")) {
    source_data <- bind_rows(
      results$aggregated %>% mutate(group = "Overall"),
      results$segments
    )
  } else if (length(groups) == 1 && groups == "all_groups") {
    source_data <- results$segments
  } else {
    has_overall <- "overall" %in% tolower(groups)
    specific_groups <- groups[!tolower(groups) %in% c("overall", "aggregated")]

    source_data <- NULL

    if (has_overall) {
      overall_data <- results$aggregated
      if (!"group" %in% names(overall_data)) {
        overall_data <- overall_data %>% mutate(group = "Overall")
      }
      source_data <- overall_data
    }

    if (length(specific_groups) > 0) {
      for (g in specific_groups) {
        check_group_exists(g, results)
      }
      if (!is.null(results$segments) && nrow(results$segments) > 0) {
        segment_data <- results$segments %>%
          filter(.data$group %in% specific_groups)
        source_data <- if (is.null(source_data)) segment_data else bind_rows(source_data, segment_data)
      } else if (!has_overall) {
        stop(sprintf("No segment data available for groups: %s", paste(specific_groups, collapse = ", ")))
      }
    }

    if (is.null(source_data) || nrow(source_data) == 0) {
      stop("No data found for specified groups")
    }
  }

  # Check for simulation column
  if (!"simulation" %in% names(source_data)) {
    stop("No simulation data found. Ensure model was run with run_psa().")
  }

  # Extract outcome summaries
  outcome_data <- extract_psa_summaries(
    source_data,
    summary_name = outcome_summary,
    value_type = "outcome",
    discounted = discounted
  ) %>%
    rename(outcome = "amount")

  # Map display names if metadata available
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      outcome_data$strategy <- map_names(outcome_data$strategy,
                                         results$metadata$strategies,
                                         "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      outcome_data$group <- map_names(outcome_data$group,
                                      results$metadata$groups,
                                      "display_name")
    }
  }

  # Apply consistent group ordering
  group_levels <- get_group_order(unique(outcome_data$group), results$metadata)
  outcome_data <- outcome_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Get all strategies (already display names)
  all_strategies <- unique(outcome_data$strategy)

  # Helper function to resolve strategy names (technical -> display if needed)
  resolve_strategy <- function(strat_name) {
    if (strat_name %in% all_strategies) {
      return(strat_name)
    }
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == strat_name]
      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        return(matched_display[1])
      }
    }
    stop(sprintf("Strategy '%s' not found in results", strat_name))
  }

  # ABSOLUTE MODE: Return raw outcomes per strategy
  if (!is.null(strategies)) {
    # Resolve and filter strategies
    resolved_strategies <- sapply(strategies, resolve_strategy, USE.NAMES = FALSE)
    psa_data <- outcome_data %>%
      filter(.data$strategy %in% resolved_strategies) %>%
      mutate(strategy = factor(.data$strategy, levels = resolved_strategies))

    return(psa_data)
  }

  # DIFFERENCE MODE: Calculate pairwise outcome differences
  # Resolve user-provided interventions/comparators to display names
  if (!is.null(interventions)) {
    interventions <- sapply(interventions, resolve_strategy, USE.NAMES = FALSE)
  }
  if (!is.null(comparators)) {
    comparators <- sapply(comparators, resolve_strategy, USE.NAMES = FALSE)
  }

  # Determine comparison pairs
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N x M explicit comparisons
    for (int_strat in interventions) {
      for (comp_strat in comparators) {
        if (int_strat != comp_strat) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = int_strat,
            comparator = comp_strat
          )
        }
      }
    }
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }
  } else if (!is.null(interventions)) {
    # Intervention only: each intervention vs all others
    for (int_strat in interventions) {
      other_strategies <- setdiff(all_strategies, int_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = other
        )
      }
    }
  } else {
    # Comparator only: all others vs each comparator
    for (comp_strat in comparators) {
      other_strategies <- setdiff(all_strategies, comp_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = other,
          comparator = comp_strat
        )
      }
    }
  }

  # Calculate outcome differences for each comparison pair
  diff_data_list <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    # Create comparison label
    comp_label <- paste0(int_strat, " vs. ", comp_strat)

    # Get data for intervention and comparator
    int_data <- outcome_data %>%
      filter(.data$strategy == int_strat) %>%
      select("simulation", "group", int_outcome = "outcome")

    comp_data <- outcome_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("simulation", "group", comp_outcome = "outcome")

    # Join and calculate outcome difference
    pair_data <- int_data %>%
      inner_join(comp_data, by = c("simulation", "group")) %>%
      mutate(
        outcome = .data$int_outcome - .data$comp_outcome,
        comparison = comp_label
      ) %>%
      select("simulation", "group", "comparison", "outcome")

    diff_data_list[[length(diff_data_list) + 1]] <- pair_data
  }

  # Combine all comparison data
  plot_data <- bind_rows(diff_data_list)

  # Preserve comparison order as factor
  comparison_order <- unique(plot_data$comparison)
  plot_data <- plot_data %>%
    mutate(comparison = factor(.data$comparison, levels = comparison_order))

  plot_data
}


#' Extract PSA Summary Values
#'
#' Helper function to extract summary values from PSA simulation data.
#'
#' @param source_data PSA results data with simulation column
#' @param summary_name Name of the summary to extract
#' @param value_type Type of value: "cost" or "outcome"
#' @param discounted Logical. Use discounted values? Default TRUE.
#'
#' @return A tibble with simulation, strategy, group, and amount columns
#'
#' @keywords internal
extract_psa_summaries <- function(source_data,
                                 summary_name,
                                 value_type,
                                 discounted = TRUE) {

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
    select("simulation", "strategy", "group", summary_data = all_of(summary_col)) %>%
    unnest("summary_data", keep_empty = TRUE) %>%
    filter(.data$summary == summary_name | is.na(.data$summary)) %>%
    group_by(.data$simulation, .data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  # Handle rows where summary wasn't found
  # If a (simulation, strategy, group) combo is missing, fill with NA
  all_combos <- source_data %>%
    distinct(.data$simulation, .data$strategy, .data$group)

  result <- all_combos %>%
    left_join(result, by = c("simulation", "strategy", "group")) %>%
    mutate(amount = if_else(is.na(.data$amount), NA_real_, .data$amount))

  result
}


#' Calculate Pairwise CEAC
#'
#' Calculates the probability that one strategy is more cost-effective than
#' another at different WTP thresholds.
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param interventions Reference strategies for comparison (intervention perspective: A vs. B, A vs. C).
#' @param comparators Reference strategies for comparison (comparator perspective: B vs. A, C vs. A).
#' @param wtp Numeric vector of WTP thresholds to evaluate. Default is
#'   seq(0, 100000, by = 5000)
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
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
                                   interventions = NULL,
                                   comparators = NULL,
                                   wtp = seq(0, 100000, by = 5000),
                                   groups = "overall") {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Get PSA simulation data (always discounted for CE analysis)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = NULL
  )

  # Get all strategies in the data
  all_strategies <- unique(psa_data$strategy)

 # Helper function to resolve strategy names (technical -> display if needed)
  resolve_strategy <- function(strat_name) {
    if (strat_name %in% all_strategies) {
      return(strat_name)
    }
    # Try mapping technical name to display name
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == strat_name]
      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        return(matched_display[1])
      }
    }
    stop(sprintf("Strategy '%s' not found in results", strat_name))
  }

  # Resolve all provided strategy names
  if (!is.null(interventions)) {
    interventions <- sapply(interventions, resolve_strategy, USE.NAMES = FALSE)
  }
  if (!is.null(comparators)) {
    comparators <- sapply(comparators, resolve_strategy, USE.NAMES = FALSE)
  }

  # Generate comparison pairs using same logic as calculate_pairwise_ce
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N×M explicit comparisons
    for (int_strat in interventions) {
      for (comp_strat in comparators) {
        # Skip self-comparisons
        if (int_strat != comp_strat) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = int_strat,
            comparator = comp_strat
          )
        }
      }
    }
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }
  } else if (!is.null(interventions)) {
    # Interventions only: each intervention vs all others
    for (int_strat in interventions) {
      for (comp_strat in setdiff(all_strategies, int_strat)) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = comp_strat
        )
      }
    }
  } else {
    # Comparators only: all others vs each comparator
    for (comp_strat in comparators) {
      for (int_strat in setdiff(all_strategies, comp_strat)) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = comp_strat
        )
      }
    }
  }

  if (length(comparison_pairs) == 0) {
    stop("No valid comparison pairs generated")
  }

  # Get unique groups
  unique_groups <- unique(psa_data$group)
  n_sim <- length(unique(psa_data$simulation))
  n_groups <- length(unique_groups)
  n_comparisons <- length(comparison_pairs)

  # Create grid of all (simulation, group, comparison_idx) combinations
  comparison_grid <- expand.grid(
    simulation = unique(psa_data$simulation),
    group = unique_groups,
    comparison_idx = seq_along(comparison_pairs),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Add intervention and comparator names from comparison pairs
  comparison_grid <- comparison_grid %>%
    mutate(
      intervention = sapply(.data$comparison_idx, function(i) comparison_pairs[[i]]$intervention),
      comparator = sapply(.data$comparison_idx, function(i) comparison_pairs[[i]]$comparator)
    )

  # Join with intervention data
  int_data <- psa_data %>%
    select("simulation", "group", "strategy", cost_int = "cost", outcome_int = "outcome")

  comparison_data <- comparison_grid %>%
    left_join(int_data, by = c("simulation", "group", "intervention" = "strategy"))

  # Join with comparator data
  comp_data <- psa_data %>%
    select("simulation", "group", "strategy", cost_comp = "cost", outcome_comp = "outcome")

  comparison_data <- comparison_data %>%
    left_join(comp_data, by = c("simulation", "group", "comparator" = "strategy"))

  comparison_data <- comparison_data %>%
    arrange(.data$simulation, .data$group, .data$comparison_idx)

  # Reshape to matrices for vectorized calculation
  n_cols <- n_groups * n_comparisons
  cost_int_mat <- matrix(comparison_data$cost_int, nrow = n_sim, ncol = n_cols, byrow = TRUE)
  cost_comp_mat <- matrix(comparison_data$cost_comp, nrow = n_sim, ncol = n_cols, byrow = TRUE)
  outcome_int_mat <- matrix(comparison_data$outcome_int, nrow = n_sim, ncol = n_cols, byrow = TRUE)
  outcome_comp_mat <- matrix(comparison_data$outcome_comp, nrow = n_sim, ncol = n_cols, byrow = TRUE)

  # Vectorized probability calculation across all WTP values
  probs_list <- lapply(wtp, function(w) {
    # Calculate incremental NMB: P(intervention > comparator)
    delta_nmb <- (outcome_int_mat - outcome_comp_mat) * w - (cost_int_mat - cost_comp_mat)
    colMeans(delta_nmb > 0)
  })
  probs_matrix <- do.call(cbind, probs_list)
  # Ensure matrix even with single row (when only 1 comparison)
  if (!is.matrix(probs_matrix)) {
    probs_matrix <- matrix(probs_matrix, nrow = 1)
  }

  # Convert back to tibble with proper labels
  comparison_labels <- comparison_grid %>%
    distinct(.data$group, .data$comparison_idx, .data$intervention, .data$comparator) %>%
    arrange(.data$group, .data$comparison_idx)

  # Expand to include all WTP values with correct probability extraction
  result <- comparison_labels %>%
    mutate(row_id = row_number()) %>%
    crossing(tibble(wtp = wtp, wtp_idx = seq_along(wtp))) %>%
    mutate(probability = probs_matrix[cbind(.data$row_id, .data$wtp_idx)]) %>%
    transmute(
      wtp = .data$wtp,
      intervention = .data$intervention,
      comparator = .data$comparator,
      probability = .data$probability,
      group = .data$group
    )

  # Map display names if metadata available
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      result$intervention <- map_names(result$intervention,
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

  # Create comparison label for faceting/coloring
  result <- result %>%
    mutate(comparison = paste(.data$intervention, "vs.", .data$comparator))

  result %>%
    select("wtp", "intervention", "comparator", "comparison", "probability", "group")
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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategy names to include (NULL for all)
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
                           groups = "overall",
                           strategies = NULL) {

  # Check if results have PSA simulations
  if (!"simulation" %in% names(results$aggregated) &&
      !"simulation" %in% names(results$segments)) {
    stop(paste("Results do not contain PSA simulations.",
               "Use run_psa() instead of run_model()."))
  }

  # Extract PSA simulation data (always discounted for CE analysis)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Get unique groups
  unique_groups <- unique(psa_data$group)

  # Initialize results
  evpi_results <- list()

  # Process each group
  for (grp in unique_groups) {
    grp_data <- psa_data %>%
      filter(.data$group == grp)

    # Get simulations for this group
    simulations <- unique(grp_data$simulation)
    n_sim <- length(simulations)

    # Calculate EVPI for each WTP threshold
    evpi_values <- sapply(wtp, function(wtp_val) {
      # Calculate NMB for each strategy in each simulation
      nmb_data <- grp_data %>%
        mutate(nmb = .data$outcome * wtp_val - .data$cost) %>%
        select("simulation", "strategy", nmb = "nmb")

      # Calculate expected NMB for each strategy (mean across simulations)
      expected_nmb <- nmb_data %>%
        group_by(.data$strategy) %>%
        summarize(expected_nmb = mean(.data$nmb, na.rm = TRUE), .groups = "drop")

      # Identify overall optimal strategy (highest expected NMB)
      overall_optimal_strategy <- expected_nmb %>%
        slice_max(.data$expected_nmb, n = 1, with_ties = FALSE) %>%
        pull(.data$strategy)

      # For each simulation, calculate opportunity loss
      opportunity_loss <- nmb_data %>%
        group_by(.data$simulation) %>%
        summarize(
          # NMB of best strategy in this simulation
          max_nmb = max(.data$nmb, na.rm = TRUE),
          # NMB of overall optimal strategy in this simulation
          overall_optimal_nmb = .data$nmb[.data$strategy == overall_optimal_strategy][1],
          .groups = "drop"
        ) %>%
        mutate(
          # Opportunity loss is the difference (cannot be negative)
          loss = pmax(0, .data$max_nmb - .data$overall_optimal_nmb)
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
    select("wtp", "evpi", "group") %>%
    arrange(.data$group, .data$wtp)
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
    if (!is.null(group) && length(group) > 1 && !group[1] %in% c("aggregated", "overall", "all")) {
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
    if (!is.null(group) && !group %in% c("aggregated", "overall", "all")) {
      # Validate groups exist using helper
      for (g in group) {
        check_group_exists(g, results)
      }

      # Filter to specific group(s)
      source_data <- source_data %>%
        filter(.data$group %in% !!group)
    }

    # Filter strategies if specified
    if (!is.null(strategies)) {
      source_data <- source_data %>%
        filter(.data$strategy %in% !!strategies)

      if (nrow(source_data) == 0) {
        stop("No data found for specified strategies")
      }
    }
  }
  # If using tuple specification, we'll filter at the variable level in the loop

  # Get metadata for name mappings
  metadata <- results$metadata

  # Create name mapping functions
  map_var_name <- function(var_name) {
    if (!is.null(metadata$variables)) {
      var_meta <- metadata$variables %>% filter(.data$name == var_name)
      if (nrow(var_meta) > 0 && !is.na(var_meta$display_name[1])) {
        return(var_meta$display_name[1])
      }
    }
    return(var_name)
  }

  map_strategy_name <- function(strat_name) {
    if (!is.null(metadata$strategies)) {
      strat_meta <- metadata$strategies %>% filter(.data$name == strat_name)
      if (nrow(strat_meta) > 0 && !is.na(strat_meta[["display_name"]][1])) {
        return(strat_meta[["display_name"]][1])
      }
    }
    return(strat_name)
  }

  map_group_name <- function(grp_name) {
    if (!is.null(metadata$groups)) {
      group_meta <- metadata$groups %>% filter(.data$name == grp_name)
      if (nrow(group_meta) > 0 && !is.na(group_meta[["display_name"]][1])) {
        return(group_meta[["display_name"]][1])
      }
    }
    return(grp_name)
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
            .data$variable == var_name,
            (is.na(.data$strategy) | .data$strategy == strat),
            (is.na(.data$group) | .data$group == grp)
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