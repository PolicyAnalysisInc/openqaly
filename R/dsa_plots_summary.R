#' Plot DSA Outcomes as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter from
#' its low to high value on a specified summary.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param outcome Name of the outcome to plot
#' @param groups Group selection
#' @param strategies Character vector of strategy names to include
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param discounted Logical. Use discounted values?
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels?
#' @param drop_zero_impact Logical. Remove parameters with zero impact?
#' @param top_n Integer or NULL. Top N parameters by impact range.
#' @param axis_decimals Fixed decimal places for axis labels, or NULL for auto-precision
#' @param label_decimals Fixed decimal places for value labels, or NULL for auto-precision
#' @param abbreviate Logical. Use abbreviated number format?
#' @return A ggplot2 object
#' @export
dsa_outcomes_plot <- function(results,
                              outcome,
                              groups = "overall",
                              strategies = NULL,
                              interventions = NULL,
                              comparators = NULL,
                              discounted = TRUE,
                              show_parameter_values = TRUE,
                              drop_zero_impact = TRUE,
                              top_n = NULL,
                              axis_decimals = NULL,
                              label_decimals = NULL,
                              abbreviate = FALSE) {

  dsa_summary_plot_impl(
    results = results,
    summary_name = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = show_parameter_values,
    drop_zero_impact = drop_zero_impact,
    top_n = top_n,
    value_type = "outcome",
    currency = FALSE,
    axis_decimals = axis_decimals,
    label_decimals = label_decimals,
    abbreviate = abbreviate
  )
}

#' DSA Summary Plot Implementation
#'
#' Shared implementation for DSA summary tornado plots.
#'
#' @param results A openqaly DSA results object
#' @param summary_name Name of the summary to plot
#' @param groups Group selection
#' @param strategies Character vector of strategy names to include
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param discounted Logical. Use discounted values?
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels?
#' @param drop_zero_impact Logical. Remove parameters with zero impact?
#' @param top_n Integer or NULL. Top N parameters by impact range.
#' @param value_type Type of summary values
#' @param currency Logical. Format values as currency?
#' @param axis_decimals Fixed decimal places for axis labels, or NULL for auto-precision
#' @param label_decimals Fixed decimal places for value labels, or NULL for auto-precision
#' @param abbreviate Logical. Use abbreviated number format?
#' @return A ggplot2 object
#' @keywords internal
dsa_summary_plot_impl <- function(results,
                                  summary_name,
                                  groups = "overall",
                                  strategies = NULL,
                                  interventions = NULL,
                                  comparators = NULL,
                                  discounted = TRUE,
                                  show_parameter_values = TRUE,
                                  drop_zero_impact = TRUE,
                                  top_n = NULL,
                                  value_type = "outcome",
                                  currency = FALSE,
                                  axis_decimals = NULL,
                                  label_decimals = NULL,
                                  abbreviate = FALSE) {
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'. Use interventions/comparators vectors to specify exact comparisons.")
  }

  locale <- get_results_locale(results)

  tornado_data <- prepare_dsa_triplet_data(
    results = results,
    summary_name = summary_name,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = show_parameter_values,
    locale = locale,
    value_type = value_type
  )

  if (is.null(tornado_data) || nrow(tornado_data) == 0) {
    stop("No data available for tornado plot with specified parameters")
  }

  if (drop_zero_impact) {
    n_params_before <- nrow(tornado_data)
    tornado_data <- tornado_data %>%
      filter(abs(.data$range) > .Machine$double.eps * 100) %>%
      droplevels()

    if (nrow(tornado_data) == 0) {
      stop(sprintf("All %d parameters have zero impact on results. No data to plot.",
                   n_params_before))
    }
  }

  if (!is.null(top_n)) {
    tornado_data <- tornado_data %>%
      group_by(.data$strategy, .data$group) %>%
      slice_max(order_by = abs(.data$range), n = top_n, with_ties = FALSE) %>%
      ungroup()
  }

  strategy_levels <- attr(tornado_data, "strategy_order")
  if (is.null(strategy_levels)) {
    strategy_levels <- unique(tornado_data$strategy)
  }

  group_levels <- attr(tornado_data, "group_order")
  if (is.null(group_levels)) {
    group_levels <- unique(tornado_data$group)
  }

  tornado_data <- tornado_data %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels),
      group = factor(.data$group, levels = group_levels)
    )

  summary_label <- summary_name
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    summary_label <- map_names(summary_name, results$metadata$summaries, "display_name")
  }

  if (!is.null(interventions) || !is.null(comparators)) {
    summary_label <- paste0("Difference in ", summary_label)
  }

  render_tornado_plot(
    tornado_data,
    summary_label,
    currency = currency,
    locale = locale,
    axis_decimals = axis_decimals,
    label_decimals = label_decimals,
    abbreviate = abbreviate
  )
}

#' Plot DSA Costs as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter on
#' a specified cost summary.
#'
#' @inheritParams dsa_outcomes_plot
#' @return A ggplot2 object
#' @export
dsa_costs_plot <- function(results,
                           outcome,
                           groups = "overall",
                           strategies = NULL,
                           interventions = NULL,
                           comparators = NULL,
                           discounted = TRUE,
                           show_parameter_values = TRUE,
                           drop_zero_impact = TRUE,
                           top_n = NULL,
                           axis_decimals = NULL,
                           label_decimals = NULL,
                           abbreviate = FALSE) {

  dsa_summary_plot_impl(
    results = results,
    summary_name = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = show_parameter_values,
    drop_zero_impact = drop_zero_impact,
    top_n = top_n,
    value_type = "cost",
    currency = TRUE,
    axis_decimals = axis_decimals,
    label_decimals = label_decimals,
    abbreviate = abbreviate
  )
}
