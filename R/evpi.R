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
          max_nmb = max(.data$nmb, na.rm = TRUE),
          overall_optimal_nmb = .data$nmb[.data$strategy == overall_optimal_strategy][1],
          .groups = "drop"
        ) %>%
        mutate(loss = pmax(0, .data$max_nmb - .data$overall_optimal_nmb))

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
