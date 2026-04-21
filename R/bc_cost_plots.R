#' Plot Cost Values as Bar Charts
#'
#' Creates a bar chart showing cost values by value component and strategy.
#' Can display absolute values or differences between strategies.
#' Values are formatted as currency.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome Name of the cost summary to plot (e.g., "total_costs")
#' @param groups Which groups to include in the plot. By default, the overall population (weighted average over all groups) is shown. Other options include 'all_groups' (displays results for each group separately), 'all' (displays results for each group and overall), or a character vector of specific group names
#' @param strategies Strategies to include (when plotting absolute values).
#' @param interventions Strategies used as interventions (when plotting differences)
#' @param comparators Strategies used as comparators (when plotting differences)
#' @param value_labels Logical. If TRUE (default), display numeric value labels at bar edges.
#' @param label_decimals Numeric or NULL. Number of decimal places for value labels. NULL for auto.
#' @param axis_decimals Numeric or NULL. Number of decimal places for axis labels. NULL for auto.
#' @param abbreviate Logical. If TRUE, use abbreviated number formatting (e.g., 1K, 1M). Default FALSE.
#'
#' @return A ggplot2 object
#'
#' @details
#' Whether absolute values or differences are shown, which strategies are included, and the direction of comparisons depends on which of the `strategies`, `interventions`, or `comparators` arguments are provided:
#' 1. (Default) Absolute values for specified strategies. Use `strategies` argument to specify which strategies to include.
#' 2. When passing a single strategy to `interventions`, shows difference between that strategy and other comparators. Use `comparators` argument to specify which strategies to compare against.
#' 3. When passing a single strategy to `comparators`, shows difference between other strategies and the comparator. Use `interventions` argument to specify which strategies will be compared against comparator.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Absolute cost values
#' costs_plot_bar(results, "total_costs")
#'
#' # Differences vs control (comparator perspective)
#' costs_plot_bar(results, "total_costs", comparators = "control")
#'
#' # New treatment vs others (intervention perspective)
#' costs_plot_bar(results, "total_costs", interventions = "new_treatment")
#' }
#'
#' @export
costs_plot_bar <- function(res, outcome,
                        groups = "overall",
                        strategies = NULL,
                        interventions = NULL,
                        comparators = NULL,
                        value_labels = TRUE,
                        label_decimals = NULL,
                        axis_decimals = NULL,
                        abbreviate = FALSE) {
  summary_plot_bar_impl(
    res = res,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    value_labels = value_labels,
    label_decimals = label_decimals,
    axis_decimals = axis_decimals,
    abbreviate = abbreviate,
    value_type = "cost",
    currency = TRUE
  )
}


#' Plot Cost Values as Line Chart Over Time
#'
#' Creates a line chart showing cost values over time (cycles or other time units).
#' Displays each cost component as a separate line, plus a total line.
#' Can display absolute values or differences between strategies.
#' Values are formatted as currency.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome Name of the cost summary to plot (e.g., "total_costs")
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of reference strategies for comparator perspective (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param time_unit Time unit for x-axis: "cycle" (default), "day", "week", "month", "year"
#' @param cumulative Logical. If TRUE (default), shows cumulative costs over time.
#'   If FALSE, shows per-cycle costs.
#' @param discounted Logical. Use discounted values? (default: TRUE)
#' @param axis_decimals Number of decimal places for y-axis labels (NULL for auto)
#' @param abbreviate Logical. If TRUE, abbreviate large numbers (e.g., 1K, 1M). Default FALSE.
#'
#' @return A ggplot2 object
#'
#' @details
#' When neither intervention nor comparator is specified, shows absolute cost values.
#'
#' When intervention is specified (intervention perspective):
#' - Creates N-1 comparisons showing (intervention - each_other_strategy)
#' - Example with 3 strategies (A, B, C) and intervention=A: shows "A vs. B" and "A vs. C"
#'
#' When comparator is specified (comparator perspective):
#' - Creates N-1 comparisons showing (each_other_strategy - comparator)
#' - Example with 3 strategies (A, B, C) and comparator=C: shows "A vs. C" and "B vs. C"
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Cumulative cost values over time
#' costs_plot_line(results, outcome = "total_costs")
#'
#' # Per-cycle costs
#' costs_plot_line(results, outcome = "total_costs", cumulative = FALSE)
#'
#' # Differences vs control (comparator perspective)
#' costs_plot_line(results, outcome = "total_costs", comparators = "control")
#'
#' # New treatment vs others (intervention perspective)
#' costs_plot_line(results, outcome = "total_costs", interventions = "new_treatment")
#' }
#'
#' @export
costs_plot_line <- function(res, outcome,
                        groups = "overall",
                        strategies = NULL,
                        interventions = NULL,
                        comparators = NULL,
                        time_unit = "cycle",
                        cumulative = TRUE,
                        discounted = TRUE,
                        axis_decimals = NULL,
                        abbreviate = FALSE) {
  summary_plot_line_impl(
    res = res,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    time_unit = time_unit,
    cumulative = cumulative,
    discounted = discounted,
    axis_decimals = axis_decimals,
    abbreviate = abbreviate,
    currency = TRUE
  )
}
