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
#'
#' @return A ggplot2 object
#'
#' @details
#' Either `interventions` or `comparators` must be specified (one is mandatory).
#' WTP is automatically extracted from the health outcome summary metadata if available,
#' but can be overridden using the `wtp` parameter.
#'
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
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
                     comparators = NULL) {

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
  # Always use discounted values for NMB (cost-effectiveness measure)
  outcome_components <- get_summaries(
    res,
    groups = groups,
    summaries = health_outcome,
    value_type = "outcome",
    discounted = TRUE,
    interventions = interventions,
    comparators = comparators,
    use_display_names = TRUE
  ) %>%
    mutate(amount = .data$amount * wtp)  # Multiply by WTP

  # Get cost components with differences
  # Always use discounted values for NMB (cost-effectiveness measure)
  cost_components <- get_summaries(
    res,
    groups = groups,
    summaries = cost_outcome,
    value_type = "cost",
    discounted = TRUE,
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

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(c(all_components$group, totals$group)), res$metadata)

  # Get value order from metadata (model-defined order)
  # Outcomes first (in model order), then costs (in model order), then Total
  outcome_values <- res$metadata$values %>%
    filter(.data$type == "outcome") %>%
    pull(.data$name)
  cost_values <- res$metadata$values %>%
    filter(.data$type == "cost") %>%
    pull(.data$name)

  # Map to display names
  outcome_values_display <- map_value_names(outcome_values, res$metadata, "display_name")
  cost_values_display <- map_value_names(cost_values, res$metadata, "display_name")

  # Correct visual order (top to bottom): outcomes, costs, Total
  # ggplot displays y-axis factor levels from bottom to top, so reverse for correct display
  value_levels <- rev(c(outcome_values_display, cost_values_display, "Total"))

  # Combine with totals (like outcomes_plot line 98)
  nmb_data <- bind_rows(all_components, totals) %>%
    mutate(
      strategy = factor(.data$strategy, levels = unique(c(all_components$strategy, totals$strategy))),
      group = factor(.data$group, levels = group_levels),
      value = factor(.data$value, levels = value_levels),
      .pos_or_neg = ifelse(.data$amount >= 0, "Positive", "Negative")
    )

  # Create outcome label with display names
  outcome_label <- map_names(health_outcome, res$metadata$summaries, "display_name")
  cost_label <- map_names(cost_outcome, res$metadata$summaries, "display_name")
  wtp_formatted <- scales::comma(wtp)
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
#'
#' @return A ggplot2 object
#'
#' @details
#' Either `interventions` or `comparators` must be specified (one is mandatory).
#'
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
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
                          cumulative = TRUE) {

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

  # Extract value names from the specified summaries
  outcome_value_names <- res$metadata$summaries %>%
    filter(.data$name == health_outcome) %>%
    pull(.data$values) %>%
    str_split(pattern = "[,\\s]+") %>%
    unlist()

  cost_value_names <- res$metadata$summaries %>%
    filter(.data$name == cost_outcome) %>%
    pull(.data$values) %>%
    str_split(pattern = "[,\\s]+") %>%
    unlist()

  # Get outcome components with differences - only values in the specified summary
  # Always use discounted values for NMB (cost-effectiveness measure)
  outcome_components <- get_values(
    res,
    format = "long",
    groups = groups,
    values = outcome_value_names,
    value_type = "outcome",
    time_unit = time_unit,
    discounted = TRUE,
    interventions = interventions,
    comparators = comparators,
    use_display_names = TRUE
  ) %>%
    mutate(amount = .data$amount * wtp)  # Multiply by WTP

  # Get cost components with differences - only values in the specified summary
  # Always use discounted values for NMB (cost-effectiveness measure)
  cost_components <- get_values(
    res,
    format = "long",
    groups = groups,
    values = cost_value_names,
    value_type = "cost",
    time_unit = time_unit,
    discounted = TRUE,
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

  # Get value order from metadata (model-defined order)
  # Outcomes first (in model order), then costs (in model order), then Total
  outcome_values <- res$metadata$values %>%
    filter(.data$type == "outcome") %>%
    pull(.data$name)
  cost_values <- res$metadata$values %>%
    filter(.data$type == "cost") %>%
    pull(.data$name)

  # Map to display names
  outcome_values_display <- map_value_names(outcome_values, res$metadata, "display_name")
  cost_values_display <- map_value_names(cost_values, res$metadata, "display_name")

  # Correct order: outcomes, costs, Total
  value_levels <- c(outcome_values_display, cost_values_display, "Total")

  # Create NMB label with display names
  outcome_label <- map_names(health_outcome, res$metadata$summaries,
                             "display_name")
  cost_label <- map_names(cost_outcome, res$metadata$summaries,
                          "display_name")
  wtp_formatted <- scales::comma(wtp)
  nmb_label <- glue(
    "Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})"
  )

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(values_with_total$group), res$metadata)
  values_with_total <- values_with_total %>%
    mutate(
      group = factor(.data$group, levels = group_levels),
      value_name = factor(.data$value_name, levels = value_levels)
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
