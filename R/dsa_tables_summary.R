#' Prepare DSA Summary Table Data
#'
#' Internal helper function that prepares DSA summary data for table rendering.
#'
#' @param results A openqaly DSA results object
#' @param outcome Name of outcome to display
#' @param groups Group selection
#' @param strategies Character vector of strategies to include
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param decimals Number of decimal places
#' @param discounted Logical. Use discounted values?
#' @param font_size Font size for rendering
#' @param top_n Integer or NULL for top N parameters
#' @param show_parameter_values Logical. Include input parameter values in row labels?
#' @param abbreviate Logical. Use abbreviated number format?
#' @param value_type Type of summary values
#' @param currency Logical. Format values as currency?
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_dsa_summary_table_data <- function(results,
                                           outcome,
                                           groups = "overall",
                                           strategies = NULL,
                                           interventions = NULL,
                                           comparators = NULL,
                                           decimals = NULL,
                                           discounted = TRUE,
                                           font_size = 11,
                                           top_n = NULL,
                                           show_parameter_values = FALSE,
                                           abbreviate = FALSE,
                                           value_type = "all",
                                           currency = FALSE) {
  if (is.null(decimals) && identical(value_type, "outcome")) {
    decimals <- 2
  }

  locale <- get_results_locale(results)

  combined_data <- prepare_dsa_triplet_data(
    results = results,
    summary_name = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = show_parameter_values,
    locale = locale,
    value_type = value_type
  )

  if (!is.null(top_n)) {
    combined_data <- combined_data %>%
      mutate(.range = abs(.data$high - .data$low)) %>%
      group_by(.data$strategy, .data$group) %>%
      slice_max(order_by = .data$.range, n = top_n, with_ties = FALSE) %>%
      ungroup() %>%
      select(-".range")
  }

  formatted_data <- combined_data %>%
    mutate(
      low = oq_format(.data$low, decimals = decimals, locale = locale, abbreviate = abbreviate, currency = currency),
      base = oq_format(.data$base, decimals = decimals, locale = locale, abbreviate = abbreviate, currency = currency),
      high = oq_format(.data$high, decimals = decimals, locale = locale, abbreviate = abbreviate, currency = currency)
    )

  build_dsa_triplet_table_spec(
    data = formatted_data,
    groups = groups,
    metadata = results$metadata,
    font_size = font_size
  )
}

#' Format DSA Outcomes as Summary Table
#'
#' Creates a table showing DSA outcomes with low, base case, and high values.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param outcome Name of outcome to display
#' @param groups Group selection
#' @param strategies Character vector of strategies to include when showing absolute values
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param decimals Number of decimal places
#' @param discounted Logical. Use discounted values?
#' @param font_size Font size for rendering
#' @param table_format Character. Backend to use
#' @param top_n Integer or NULL. Show only the top N parameters by impact range.
#' @param show_parameter_values Logical. Include input parameter values in row labels?
#' @param abbreviate Logical. Use abbreviated number format?
#' @return A table object
#' @export
dsa_outcomes_table <- function(results,
                               outcome,
                               groups = "overall",
                               strategies = NULL,
                               interventions = NULL,
                               comparators = NULL,
                               decimals = 2,
                               discounted = TRUE,
                               font_size = 11,
                               table_format = c("flextable", "kable"),
                               top_n = NULL,
                               show_parameter_values = FALSE,
                               abbreviate = FALSE) {
  table_format <- match.arg(table_format)

  if (!is.null(interventions) && !is.null(comparators)) {
    stop("Only one of 'interventions' or 'comparators' should be provided, not both")
  }

  prepared <- prepare_dsa_summary_table_data(
    results = results,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    discounted = discounted,
    font_size = font_size,
    top_n = top_n,
    show_parameter_values = show_parameter_values,
    abbreviate = abbreviate,
    value_type = "outcome",
    currency = FALSE
  )

  render_table(prepared, format = table_format)
}

#' Format DSA Costs as Summary Table
#'
#' Creates a table showing DSA cost outcomes with low, base case, and high values.
#'
#' @inheritParams dsa_outcomes_table
#' @return A table object
#' @export
dsa_costs_table <- function(results,
                            outcome,
                            groups = "overall",
                            strategies = NULL,
                            interventions = NULL,
                            comparators = NULL,
                            decimals = NULL,
                            discounted = TRUE,
                            font_size = 11,
                            table_format = c("flextable", "kable"),
                            top_n = NULL,
                            show_parameter_values = FALSE,
                            abbreviate = FALSE) {
  table_format <- match.arg(table_format)

  if (!is.null(interventions) && !is.null(comparators)) {
    stop("Only one of 'interventions' or 'comparators' should be provided, not both")
  }

  prepared <- prepare_dsa_summary_table_data(
    results = results,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    discounted = discounted,
    font_size = font_size,
    top_n = top_n,
    show_parameter_values = show_parameter_values,
    abbreviate = abbreviate,
    value_type = "cost",
    currency = TRUE
  )

  render_table(prepared, format = table_format)
}
