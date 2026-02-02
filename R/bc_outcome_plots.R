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


#' Plot Outcome Values as Line Chart Over Time
#'
#' Creates a line chart showing outcome values over time (cycles or other time units).
#' Displays each outcome component as a separate line, plus a total line.
#' Can display absolute values or differences between strategies.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome Name of the outcome summary to plot (e.g., "total_qalys")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#'   (all groups plus aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of reference strategies for comparator perspective (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param time_unit Time unit for x-axis: "cycle" (default), "day", "week", "month", "year"
#' @param cumulative Logical. If TRUE (default), shows cumulative outcomes over time.
#'   If FALSE, shows per-cycle outcomes.
#' @param discounted Logical. Use discounted values? (default: TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' When neither intervention nor comparator is specified, shows absolute outcome values.
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
#' # Cumulative outcome values over time
#' outcomes_plot_line(results, outcome = "total_qalys")
#'
#' # Per-cycle outcomes
#' outcomes_plot_line(results, outcome = "total_qalys", cumulative = FALSE)
#'
#' # Differences vs control (comparator perspective)
#' outcomes_plot_line(results, outcome = "total_qalys", comparator = "control")
#'
#' # New treatment vs others (intervention perspective)
#' outcomes_plot_line(results, outcome = "total_qalys", intervention = "new_treatment")
#' }
#'
#' @export
outcomes_plot_line <- function(res, outcome,
                        groups = "overall",
                        strategies = NULL,
                        interventions = NULL,
                        comparators = NULL,
                        time_unit = "cycle",
                        cumulative = TRUE,
                        discounted = TRUE) {

  # Validate mutual exclusivity
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' cannot be used with 'interventions' or 'comparators'")
  }

  # Get values for the specified outcome summary
  summary_values <- res$metadata$summaries |>
    filter(.data$name == outcome) |>
    pull(.data$values) |>
    str_split(pattern = "[,\\s]+") |>
    unlist()

  # Get time-series data for the values in this summary
  values_data <- get_values(
    res,
    format = "long",
    groups = groups,
    strategies = strategies,
    values = summary_values,  # Pass the extracted value names
    value_type = "all",  # Support both cost and outcome summaries
    time_unit = time_unit,
    discounted = discounted,
    use_display_names = TRUE,
    interventions = interventions,
    comparators = comparators
  )

  # Get time column name
  time_col <- switch(time_unit,
                    "cycle" = "cycle",
                    "day" = "day",
                    "week" = "week",
                    "month" = "month",
                    "year" = "year",
                    "cycle")

  # Check if time column exists in data
  if (!time_col %in% colnames(values_data)) {
    warning(paste("Time unit", time_unit, "not available, using cycle instead"))
    time_col <- "cycle"
  }

  time_label <- switch(time_col,
                      "cycle" = "Cycle",
                      "day" = "Days",
                      "week" = "Weeks",
                      "month" = "Months",
                      "year" = "Years",
                      "Time")

  # Calculate cumulative if requested
  if (cumulative) {
    values_data <- values_data %>%
      group_by(.data$strategy, .data$group, .data$value_name) %>%
      arrange(!!sym(time_col)) %>%
      mutate(amount = cumsum(.data$amount)) %>%
      ungroup()
  }

  # Calculate totals for each strategy-group-time combination
  totals <- values_data %>%
    group_by(!!sym(time_col), .data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = 'drop') %>%
    mutate(value_name = "Total")

  # Combine values with totals
  values_with_total <- bind_rows(values_data, totals)

  # Create outcome label
  outcome_label <- outcome  # Use the summary name
  # Map outcome label if metadata available
  if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
    outcome_label <- map_names(outcome, res$metadata$summaries, "display_name")
  }

  # Prepend "Difference in" if interventions/comparators was provided (get_values handles the calculations)
  if (!is.null(interventions) || !is.null(comparators)) {
    outcome_label <- paste0("Difference in ", outcome_label)
  }

  # Apply consistent value ordering (model order, then Total last)
  # summary_values already contains the values in model-defined order
  value_levels <- c(
    map_value_names(summary_values, res$metadata, "display_name"),
    "Total"
  )

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(values_with_total$group), res$metadata)
  values_with_total <- values_with_total %>%
    mutate(
      group = factor(.data$group, levels = group_levels),
      value_name = factor(.data$value_name, levels = value_levels)
    )

  # Determine faceting
  n_groups <- length(unique(values_with_total$group))
  n_value_names <- length(unique(values_with_total$value_name))

  facet_component <- facet_grid(rows = vars(.data$value_name), cols = vars(.data$group), scales = "free_y")
  if ((n_groups > 1) && (n_value_names == 1)) {
    facet_component <- facet_wrap(vars(.data$group), scales = "free_y")
  } else if ((n_value_names > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$value_name), scales = "free_y")
  } else if ((n_value_names == 1) && (n_groups == 1)) {
    facet_component <- NULL
  }

  # Determine if we have multiple strategies
  n_strategies <- length(unique(values_with_total$strategy))

  # Create the plot - only use color aesthetic if multiple strategies
  if (n_strategies > 1) {
    p <- ggplot(values_with_total,
                aes(x = !!sym(time_col), y = .data$amount, color = .data$strategy)) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      labs(
        x = time_label,
        y = if (cumulative) {
          paste0("Cumulative ", outcome_label)
        } else {
          paste0("Per-", tolower(time_label), " ", outcome_label)
        },
        color = "Strategy"
      )
  } else {
    p <- ggplot(values_with_total,
                aes(x = !!sym(time_col), y = .data$amount)) +
      geom_line(linewidth = 1) +
      scale_y_continuous(labels = comma) +
      theme_bw() +
      labs(
        x = time_label,
        y = if (cumulative) {
          paste0("Cumulative ", outcome_label)
        } else {
          paste0("Per-", tolower(time_label), " ", outcome_label)
        }
      )
  }

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}
