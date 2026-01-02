

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
#' @param discounted Logical. Use discounted values? (default: FALSE)
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
                         discounted = FALSE) {

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

  # Create the plot
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

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Plot Net Monetary Benefit as Bar Chart
#'
#' Creates a bar chart showing Net Monetary Benefit (NMB) by strategy comparison and group.
#' NMB = (Difference in Outcomes * WTP) - Difference in Costs
#'
#' @param res A openqaly model results object (output from run_model)
#' @param health_outcome Name of the health outcome summary to use (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param wtp Optional override for willingness-to-pay. If NULL, extracts from outcome summary metadata.
#' @param interventions Character vector of intervention strategies (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of comparator strategies (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param discounted Logical. Use discounted values?
#'
#' @return A ggplot2 object
#'
#' @details
#' Either `interventions` or `comparators` must be specified (one is mandatory).
#' WTP is automatically extracted from the health outcome summary metadata if available,
#' but can be overridden using the `wtp` parameter.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # NMB with WTP from metadata (comparator perspective)
#' nmb_plot_bar(results, "total_qalys", "total_cost", comparator = "control")
#'
#' # NMB with explicit WTP (intervention perspective)
#' nmb_plot_bar(results, "total_qalys", "total_cost", intervention = "new_treatment", wtp = 50000)
#' }
#'
#' @export
nmb_plot_bar <- function(res,
                     health_outcome,
                     cost_outcome,
                     groups = "overall",
                     wtp = NULL,
                     interventions = NULL,
                     comparators = NULL,
                     discounted = FALSE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Get WTP if needed (before calling get_summaries)
  if (is.null(wtp)) {
    if (is.null(res$metadata) || is.null(res$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Metadata not available.")
    }
    outcome_meta <- res$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome))
    }
    wtp <- outcome_meta$wtp[1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.", health_outcome))
    }
  }

  # Get outcome components with differences (follows outcomes_plot pattern)
  outcome_components <- get_summaries(
    res,
    groups = groups,
    summaries = health_outcome,
    value_type = "outcome",
    discounted = discounted,
    interventions = interventions,
    comparators = comparators,
    use_display_names = TRUE
  ) %>%
    mutate(amount = .data$amount * wtp)  # Multiply by WTP

  # Get cost components with differences
  cost_components <- get_summaries(
    res,
    groups = groups,
    summaries = cost_outcome,
    value_type = "cost",
    discounted = discounted,
    interventions = interventions,
    comparators = comparators,
    use_display_names = TRUE
  ) %>%
    mutate(amount = -.data$amount)  # Negate costs (we subtract them)

  # Combine outcome and cost components
  all_components <- bind_rows(outcome_components, cost_components)

  # Calculate total (like outcomes_plot lines 93-96)
  totals <- all_components %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount), .groups = 'drop') %>%
    mutate(value = "Total")

  # Combine with totals (like outcomes_plot line 98)
  nmb_data <- bind_rows(all_components, totals) %>%
    mutate(
      strategy = factor(.data$strategy, levels = unique(c(all_components$strategy, totals$strategy))),
      group = factor(.data$group, levels = unique(c(all_components$group, totals$group))),
      value = factor(.data$value, levels = rev(unique(c(all_components$value, totals$value)))),
      .pos_or_neg = ifelse(.data$amount >= 0, "Positive", "Negative")
    )

  # Create outcome label with display names
  outcome_label <- map_names(health_outcome, res$metadata$summaries, "display_name")
  cost_label <- map_names(cost_outcome, res$metadata$summaries, "display_name")
  wtp_formatted <- format(wtp, big.mark = ",")
  nmb_label <- glue("Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})")

  n_groups <- length(unique(nmb_data$group))
  n_strategies <- length(unique(nmb_data$strategy))

  facet_component <- facet_grid(rows = vars(.data$group), cols = vars(.data$strategy))
  if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group))
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy))
  } else if ((n_strategies == 1) && (n_groups == 1)) {
    facet_component <- NULL
  }

  # Create the plot (exactly like outcomes_plot, lines 104-111)
  nmb_data %>%
    ggplot(aes(fill = .data$.pos_or_neg, x = .data$amount, y = .data$value)) +
    geom_bar(stat = "identity", position = "dodge") +
    annotate("segment", x = 0, xend = 0, y = -Inf, yend = Inf,
             linetype = "dashed", color = "black") +
    facet_component +
    scale_x_continuous(labels = comma) +
    guides(fill = "none") +
    labs(y = NULL, x = nmb_label) +
    theme_bw()
}


#' Plot Net Monetary Benefit Over Time
#'
#' Creates a line chart showing cumulative or per-cycle Net Monetary Benefit over time.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param health_outcome Name of the health outcome summary to use (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param wtp Optional override for willingness-to-pay. If NULL, extracts from outcome summary metadata.
#' @param interventions Character vector of intervention strategies (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of comparator strategies (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param time_unit Time unit for x-axis: "cycle" (default), "day", "week", "month", "year"
#' @param cumulative Logical. If TRUE (default), shows cumulative NMB over time.
#'   If FALSE, shows per-cycle NMB.
#' @param discounted Logical. Use discounted values?
#'
#' @return A ggplot2 object
#'
#' @details
#' Either `interventions` or `comparators` must be specified (one is mandatory).
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Cumulative NMB over time
#' nmb_plot_line(results, "total_qalys", "total_cost", comparators = "control")
#'
#' # Per-cycle NMB
#' nmb_plot_line(
#'   results, "total_qalys", "total_cost",
#'   interventions = "new_treatment", cumulative = FALSE
#' )
#' }
#'
#' @export
nmb_plot_line <- function(res,
                          health_outcome,
                          cost_outcome,
                          groups = "overall",
                          wtp = NULL,
                          interventions = NULL,
                          comparators = NULL,
                          time_unit = "cycle",
                          cumulative = TRUE,
                          discounted = FALSE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Get WTP if needed (before calling get_values)
  if (is.null(wtp)) {
    if (is.null(res$metadata) || is.null(res$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Metadata not available.")
    }
    outcome_meta <- res$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome))
    }
    wtp <- outcome_meta$wtp[1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.", health_outcome))
    }
  }

  # Get outcome components with differences (like outcomes_plot_time)
  outcome_components <- get_values(
    res,
    format = "long",
    groups = groups,
    value_type = "outcome",
    time_unit = time_unit,
    discounted = discounted,
    interventions = interventions,
    comparators = comparators,
    use_display_names = TRUE
  ) %>%
    mutate(amount = .data$amount * wtp)  # Multiply by WTP

  # Get cost components with differences
  cost_components <- get_values(
    res,
    format = "long",
    groups = groups,
    value_type = "cost",
    time_unit = time_unit,
    discounted = discounted,
    interventions = interventions,
    comparators = comparators,
    use_display_names = TRUE
  ) %>%
    mutate(amount = -.data$amount)  # Negate costs (we subtract them)

  # Get time column name
  time_cols <- c("cycle", "day", "week", "month", "year")
  time_col <- switch(time_unit,
                    "cycle" = "cycle",
                    "day" = "day",
                    "week" = "week",
                    "month" = "month",
                    "year" = "year",
                    "cycle")

  if (!time_col %in% colnames(outcome_components)) {
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

  # Combine outcome and cost components
  all_components <- bind_rows(outcome_components, cost_components)

  # Calculate cumulative if requested
  if (cumulative) {
    all_components <- all_components %>%
      group_by(.data$strategy, .data$group, .data$value_name) %>%
      arrange(!!sym(time_col)) %>%
      mutate(amount = cumsum(.data$amount)) %>%
      ungroup()
  }

  # Add Total line (like outcomes_plot_time)
  totals <- all_components %>%
    group_by(!!sym(time_col), .data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = 'drop') %>%
    mutate(value_name = "Total")

  # Combine with totals
  values_with_total <- bind_rows(all_components, totals)

  # Create NMB label with display names
  outcome_label <- map_names(health_outcome, res$metadata$summaries,
                             "display_name")
  cost_label <- map_names(cost_outcome, res$metadata$summaries,
                          "display_name")
  wtp_formatted <- format(wtp, big.mark = ",")
  nmb_label <- glue(
    "Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})"
  )

  # Determine faceting (like outcomes_plot_time)
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

  # Create the plot (like outcomes_plot_time)
  p <- ggplot(values_with_total,
              aes(x = !!sym(time_col), y = .data$amount, color = .data$strategy)) +
    geom_line(linewidth = 1) +
    annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0,
             linetype = "dashed", color = "black") +
    scale_y_continuous(labels = comma) +
    theme_bw() +
    labs(
      x = time_label,
      y = if (cumulative) {
        paste0("Cumulative ", nmb_label)
      } else {
        paste0("Per-", tolower(time_label), " ", nmb_label)
      },
      color = "Comparison"
    )

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Plot Incremental Cost-Effectiveness Frontier
#'
#' Creates a cost-effectiveness plane showing all strategies as points with line
#' segments connecting strategies on the efficiency frontier.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The cost-effectiveness plane plots outcome (x-axis) vs. cost (y-axis).
#' Strategies on the efficiency frontier are connected with line segments.
#' Dominated and extended dominated strategies are shown with different styling.
#'
#' When \code{group = NULL}, creates faceted plots for each group and aggregated results.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # CE frontier for aggregated population
#' incremental_ce_plot(results, "total_qalys", "total_cost")
#'
#' # For all groups with faceting
#' incremental_ce_plot(results, "total_qalys", "total_cost", group = NULL)
#' }
#'
#' @export
incremental_ce_plot <- function(res,
                                outcome_summary,
                                cost_summary,
                                groups = "overall",
                                strategies = NULL,
                                discounted = FALSE) {

  # Calculate incremental CE
  ce_data <- calculate_incremental_ce(
    res,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies,
    discounted = discounted
  )

  # Create status factor for coloring/shaping
  ce_data <- ce_data %>%
    mutate(
      status = case_when(
        .data$strictly_dominated ~ "Dominated",
        .data$extendedly_dominated ~ "Extended Dominated",
        .data$on_frontier ~ "On Frontier",
        TRUE ~ "Other"
      ),
      status = factor(.data$status, levels = c("On Frontier", "Extended Dominated", "Dominated", "Other"))
    )

  # Get frontier data for line segments
  frontier_data <- ce_data %>%
    filter(.data$on_frontier) %>%
    arrange(.data$group, .data$cost)

  # Map summary names for axis labels
  outcome_label <- outcome_summary
  cost_label <- cost_summary
  if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
    outcome_label <- map_names(outcome_summary, res$metadata$summaries, "display_name")
    cost_label <- map_names(cost_summary, res$metadata$summaries, "display_name")
  }

  # Determine faceting
  n_groups <- length(unique(ce_data$group))

  facet_component <- NULL
  if (n_groups > 1) {
    facet_component <- facet_wrap(~ group, scales = "free")
  }

  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, ce_data$outcome))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, ce_data$cost))
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # Create the plot
  p <- ggplot(ce_data, aes(x = .data$outcome, y = .data$cost)) +
    geom_point(aes(color = .data$strategy)) +
    geom_line(data = frontier_data, aes(group = .data$group)) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits
    ) +
    # labs(x = outcome_label, y = cost_label) +
    theme_bw()

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Plot Pairwise Cost-Effectiveness Comparisons
#'
#' Creates a cost-effectiveness plane showing pairwise comparisons against a single
#' reference strategy. Uses delta axes (incremental cost vs. incremental outcome)
#' with the reference strategy at the origin.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL (all groups + aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Character vector of intervention strategies (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of comparator strategies (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The cost-effectiveness plane plots incremental outcome (x-axis) vs. incremental cost (y-axis).
#' The origin (0,0) represents the reference strategy.
#'
#' When comparators is specified:
#' - All other strategies are plotted as points showing their incremental values vs. comparator
#' - Lines connect origin to each point, with slope representing ICER
#' - Single plot (or faceted by group if group=NULL)
#'
#' When interventions is specified:
#' - Each comparison gets its own facet panel showing intervention vs. one other strategy
#' - One point per panel at the intervention's incremental position
#' - Line shows ICER slope from origin
#' - If group=NULL: uses facet_grid with group on rows, comparison on columns
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Pairwise CE plane vs control (comparator perspective)
#' pairwise_ce_plot(results, "total_qalys", "total_cost", comparators = "control")
#'
#' # New treatment vs others (intervention perspective)
#' pairwise_ce_plot(results, "total_qalys", "total_cost", interventions = "new_treatment")
#'
#' # For all groups
#' pairwise_ce_plot(results, "total_qalys", "total_cost", group = NULL,
#'                  comparators = "control")
#' }
#'
#' @export
pairwise_ce_plot <- function(res,
                             outcome_summary,
                             cost_summary,
                             groups = "overall",
                             strategies = NULL,
                             interventions = NULL,
                             comparators = NULL,
                             discounted = FALSE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Calculate pairwise CE
  ce_data <- calculate_pairwise_ce(
    res,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Map summary names for axis labels
  outcome_label <- outcome_summary
  cost_label <- cost_summary
  if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
    outcome_label <- map_names(outcome_summary, res$metadata$summaries, "display_name")
    cost_label <- map_names(cost_summary, res$metadata$summaries, "display_name")
  }

  # Get unique groups and comparisons
  n_groups <- length(unique(ce_data$group))
  n_comparisons <- nrow(ce_data) / n_groups  # Approximate

  # Create comparison label
  ce_data <- ce_data %>%
    mutate(comparison = paste(.data$strategy, "vs.", .data$comparator))

  # Determine plot mode based on number of comparisons
  if (n_comparisons <= 5) {
    # Few comparisons: all on one plot with color by comparison
    p <- ggplot(ce_data, aes(x = .data$doutcome, y = .data$dcost)) +
      geom_point(aes(color = .data$comparison), size = 3) +
      geom_segment(aes(xend = .data$doutcome, yend = .data$dcost, color = .data$comparison),
                   x = 0, y = 0, alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = paste0("\u0394 ", outcome_label),
           y = paste0("\u0394 ", cost_label),
           color = "Comparison") +
      theme_bw()

    # Add faceting if multiple groups
    if (n_groups > 1) {
      p <- p + facet_wrap(~ group, scales = "free")
    }

  } else {
    # Many comparisons: separate panel per comparison
    p <- ggplot(ce_data, aes(x = .data$doutcome, y = .data$dcost)) +
      geom_point(color = "blue", size = 3) +
      geom_segment(aes(xend = .data$doutcome, yend = .data$dcost),
                   x = 0, y = 0, color = "gray50", alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      labs(x = paste0("\u0394 ", outcome_label),
           y = paste0("\u0394 ", cost_label)) +
      theme_bw()

    # Add faceting
    if (n_groups > 1) {
      # Multi-group: facet_grid with group on rows, comparison on columns
      p <- p + facet_grid(group ~ comparison, scales = "free")
    } else {
      # Single group: facet_wrap by comparison
      p <- p + facet_wrap(~ comparison, scales = "free")
    }
  }

  p
}