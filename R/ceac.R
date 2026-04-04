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
