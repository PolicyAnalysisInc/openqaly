#' Plot Outcome Values as Bar Charts
#'
#' Creates a bar chart showing outcome values by value component and strategy.
#' Can display absolute values or differences between strategies.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome Name of the outcome summary to plot (e.g., "total_qalys")
#' @param groups Which groups to include in the plot. By default, the overall population (weighted average over all groups) is shown. Other options include 'all_groups' (displays results for each group separately), 'all' (displays results for each group and overall), or a character vector of specific group names
#' @param strategies Strategies to include (when plotting absolute values).
#' @param interventions Strategies used as interventions (when plotting differences)
#' @param comparators Strategies used as commparators (when plotting differences)
#'
#' @return A ggplot2 object
#'
#' @details
#' Whether absolute values or differences are shown, which strategies are included, and the direction of comparisons depends on which of the `strategies`, `interventions`, or `comparators` arguments are provided:
#' 1. (Default) Absolute values for specified strategies. Use `strategies` argument to specify which strategies to include.
#' 2. When passing a single strategy to `interventions`, shows different between that strategy and other comparators. Use `comparators` argument to specify which strategies to compare against.
#' 3. When passing a single strategy to `comparators`, shows difference other strategies and the comparator. Use `interventions` argument to specify which strategies will be compared against comparator.
#' 
#' Models containing multiple groups
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Absolute outcome values
#' outcomes_plot_bar(results, "total_qalys")
#'
#' # Differences vs control (comparator perspective)
#' outcomes_plot_bar(results, "total_qalys", comparators = "control")
#'
#' # New treatment vs others (intervention perspective)
#' outcomes_plot_bar(results, "total_qalys", interventions = "new_treatment")
#' }
#'
#' @export
outcomes_plot_bar <- function(res, outcome,
                         groups = "overall",
                         strategies = NULL,
                         interventions = NULL,
                         comparators = NULL) {
  # Validate mutual exclusivity
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("The 'strategies' argument cannot be provided when also providing 'interventions' or 'comparators'")
  }

  # get_summaries now validates summaries internally with check_summary_exists()
  # so we don't need redundant checking here
  summaries <- get_summaries(
    res,
    groups = groups,
    strategies = strategies,
    summaries = outcome,
    use_display_names = TRUE,
    interventions = interventions,
    comparators = comparators
  )

  # Get values for the specified outcome summary (in model-defined order)
  summary_values <- res$metadata$summaries |>
    filter(.data$name == outcome) |>
    pull(.data$values) |>
    str_split(pattern = "[,\\s]+") |>
    unlist()

  summaries <- summaries %>%
    mutate(
      strategy = factor(.data$strategy, levels = unique(.data$strategy)),
      group = factor(.data$group, levels = unique(.data$group))
    )

  # Map summary name for axis label
  outcome_label <- outcome  # default to technical name
  if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
    outcome_label <- map_names(outcome, res$metadata$summaries, "display_name")
  }

  # Add prefix if showing differences
  if (!is.null(interventions) || !is.null(comparators)) {
    outcome_label <- paste0("Difference in ", outcome_label)
  }

  n_groups <- length(unique(summaries$group))
  n_strategies <- length(unique(summaries$strategy))

  facet_component <- facet_grid(rows = vars(.data$group), cols = vars(.data$strategy))
  if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group))
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy))
  } else if ((n_strategies == 1) && (n_groups == 1)) {
    facet_component <- NULL
  }

  totals <- summaries %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount), .groups = 'drop') %>%
    mutate(value = "Total")

  summaries_with_total <- bind_rows(summaries, totals) %>%
    mutate(
      # Model order with Total last, reversed for bar chart (Total at top)
      value = factor(.data$value, levels = rev(c(
        map_value_names(summary_values, res$metadata, "display_name"),
        "Total"
      ))),
      .pos_or_neg = ifelse(.data$amount >= 0, "Positive", "Negative")
    )

  # Calculate axis breaks and limits to include 0 and extend beyond data
  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, summaries_with_total$amount))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)

  summaries_with_total %>%
    ggplot(aes(fill=.data$.pos_or_neg, x=.data$amount, y=.data$value)) +
    geom_bar(stat="identity", position = "dodge") +
    facet_component +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, labels = comma) +
    guides(fill = "none") +
    labs(y = NULL, x = outcome_label) +
    theme_bw()
}
