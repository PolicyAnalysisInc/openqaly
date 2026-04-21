#' Plot DSA Net Monetary Benefit as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter on
#' Net Monetary Benefit.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of the health outcome summary to use
#' @param cost_outcome Name of the cost summary to use
#' @param groups Group selection
#' @param wtp Optional override for willingness-to-pay
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels?
#' @param drop_zero_impact Logical. Remove parameters with zero impact on NMB?
#' @param top_n Integer or NULL. Top N parameters by impact range.
#' @param axis_decimals Fixed decimal places for axis labels, or NULL for auto-precision
#' @param label_decimals Fixed decimal places for value labels, or NULL for auto-precision
#' @param abbreviate Logical. Use abbreviated number format?
#' @return A ggplot2 object
#' @export
dsa_nmb_plot <- function(results,
                         health_outcome,
                         cost_outcome,
                         groups = "overall",
                         wtp = NULL,
                         interventions = NULL,
                         comparators = NULL,
                         show_parameter_values = TRUE,
                         drop_zero_impact = TRUE,
                         top_n = NULL,
                         axis_decimals = NULL,
                         label_decimals = NULL,
                         abbreviate = FALSE) {
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation")
  }

  if (is.null(wtp)) {
    if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Metadata not available. Provide explicit wtp parameter.")
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome))
    }
    wtp <- outcome_meta[["wtp"]][1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.", health_outcome))
    }
  }

  locale <- get_results_locale(results)

  outcome_tornado <- prepare_dsa_triplet_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  ) %>%
    mutate(
      low = .data$low * wtp,
      base = .data$base * wtp,
      high = .data$high * wtp,
      range = abs(.data$high - .data$low)
    )

  strategy_order_nmb <- attr(outcome_tornado, "strategy_order")
  group_order_nmb <- attr(outcome_tornado, "group_order")

  cost_tornado <- prepare_dsa_triplet_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  ) %>%
    mutate(
      low = -.data$low,
      base = -.data$base,
      high = -.data$high,
      range = abs(.data$high - .data$low)
    )

  nmb_tornado <- outcome_tornado %>%
    full_join(
      cost_tornado,
      by = c("strategy", "group", "parameter", "parameter_display_name"),
      suffix = c("_outcome", "_cost")
    ) %>%
    mutate(
      low_outcome = replace_na(.data$low_outcome, 0),
      base_outcome = replace_na(.data$base_outcome, 0),
      high_outcome = replace_na(.data$high_outcome, 0),
      low_cost = replace_na(.data$low_cost, 0),
      base_cost = replace_na(.data$base_cost, 0),
      high_cost = replace_na(.data$high_cost, 0),
      low = .data$low_outcome + .data$low_cost,
      base = .data$base_outcome + .data$base_cost,
      high = .data$high_outcome + .data$high_cost,
      range = pmax(.data$low, .data$base, .data$high) - pmin(.data$low, .data$base, .data$high)
    ) %>%
    select("strategy", "group", "parameter", "parameter_display_name", "low", "base", "high", "range")

  if (show_parameter_values) {
    param_values <- extract_dsa_parameter_values(
      results = results,
      data = nmb_tornado,
      interventions = interventions,
      comparators = comparators
    )
    nmb_tornado <- enhance_dsa_parameter_labels(
      data = nmb_tornado,
      results = results,
      locale = locale,
      parameter_values = param_values
    )
  }

  if (is.null(nmb_tornado) || nrow(nmb_tornado) == 0) {
    stop("No data available for NMB tornado plot with specified parameters")
  }

  if (drop_zero_impact) {
    n_params_before <- nrow(nmb_tornado)
    nmb_tornado <- nmb_tornado %>%
      filter(abs(.data$range) > .Machine$double.eps * 100)

    if (nrow(nmb_tornado) == 0) {
      stop(sprintf("All %d parameters have zero impact on NMB. No data to plot.",
                   n_params_before))
    }
  }

  if (!is.null(top_n)) {
    nmb_tornado <- nmb_tornado %>%
      group_by(.data$strategy, .data$group) %>%
      slice_max(order_by = abs(.data$range), n = top_n, with_ties = FALSE) %>%
      ungroup()
  }

  strategy_levels_nmb <- strategy_order_nmb
  if (is.null(strategy_levels_nmb)) {
    strategy_levels_nmb <- unique(nmb_tornado$strategy)
  }

  group_levels_nmb <- group_order_nmb
  if (is.null(group_levels_nmb)) {
    group_levels_nmb <- unique(nmb_tornado$group)
  }

  nmb_tornado <- nmb_tornado %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels_nmb),
      group = factor(.data$group, levels = group_levels_nmb)
    )

  outcome_label <- health_outcome
  cost_label <- cost_outcome
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    outcome_label <- map_names(health_outcome, results$metadata$summaries, "display_name")
    cost_label <- map_names(cost_outcome, results$metadata$summaries, "display_name")
  }

  wtp_formatted <- oq_format(wtp, locale = locale, currency = TRUE)
  nmb_label <- glue("Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})")

  render_tornado_plot(
    nmb_tornado,
    nmb_label,
    currency = TRUE,
    locale = locale,
    axis_decimals = axis_decimals,
    label_decimals = label_decimals,
    abbreviate = abbreviate
  )
}
