#' Format Costs as Summary Breakdown Table
#'
#' Creates a table showing cost summary totals broken down by component values.
#' Supports both flextable and kableExtra backends for flexible output formatting.
#'
#' @param results A openqaly model results object
#' @param outcome Name of cost summary to display (e.g., "total_costs")
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param strategies Character vector of strategies to include when showing
#'   absolute values (NULL for all). Cannot be combined with interventions or
#'   comparators.
#' @param interventions Character vector of reference strategies for intervention
#'   perspective. Use for differences; cannot be combined with strategies.
#' @param comparators Character vector of reference strategies for comparator
#'   perspective. Use for differences; cannot be combined with strategies.
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: NULL for auto-precision)
#' @param abbreviate Logical. Use abbreviated number format (K/M/B/T)? (default: FALSE)
#' @param discounted Logical. Use discounted values? (default: TRUE)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Summary table for overall
#' ft <- costs_table(results, "total_costs")
#'
#' # Compare across groups
#' ft <- costs_table(results, "total_costs", groups = "all")
#' }
#'
#' @export
costs_table <- function(results,
                        outcome,
                        groups = "overall",
                        strategies = NULL,
                        interventions = NULL,
                        comparators = NULL,
                        show_total = TRUE,
                        decimals = NULL,
                        abbreviate = FALSE,
                        discounted = TRUE,
                        font_size = 11,
                        table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Validate that strategies cannot be used with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("Cannot specify 'strategies' together with 'interventions' or 'comparators'. Use either 'strategies' for absolute values, or 'interventions'/'comparators' for differences.")
  }

  # Prepare data
  prepared <- prepare_summary_table_data(
    results = results,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    show_total = show_total,
    decimals = decimals,
    abbreviate = abbreviate,
    discounted = discounted,
    value_type = "cost",
    currency = TRUE,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
