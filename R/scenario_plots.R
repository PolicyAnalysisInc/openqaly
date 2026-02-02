#' Scenario Analysis Bar Chart Visualizations
#'
#' Functions for creating bar chart visualizations of scenario analysis results.
#' Horizontal bar charts with scenarios on Y-axis.
#'
#' @name scenario_plots
#' @importFrom ggplot2 ggplot aes geom_col geom_vline geom_text geom_segment geom_point
#' @importFrom ggplot2 geom_polygon geom_label geom_blank
#' @importFrom ggplot2 scale_fill_manual scale_x_continuous scale_y_discrete
#' @importFrom ggplot2 facet_wrap vars labs theme_bw theme element_text coord_cartesian
#' @importFrom ggplot2 expansion arrow unit
#' @importFrom scales comma pretty_breaks
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup row_number slice n all_of case_when
#' @importFrom dplyr rowwise
#' @importFrom tidyr replace_na unnest
#' @importFrom tibble tibble
#' @importFrom forcats fct_inorder fct_reorder fct_rev
#' @importFrom glue glue
NULL

#' Render Scenario Bar Plot
#'
#' Internal helper to create horizontal bar chart visualization for scenario data.
#' This function handles the plotting logic for scenario analysis bar charts.
#'
#' @param bar_data Prepared tibble with columns: scenario_name, value, strategy, group,
#'   is_base_case
#' @param x_label String for x-axis label
#' @param value_labels Logical. Show value labels on bars? (default: TRUE)
#'
#' @return A ggplot2 object
#' @keywords internal
render_scenario_bar_plot <- function(bar_data, x_label,
                                      value_labels = TRUE) {

  # Determine faceting
  n_groups <- length(unique(bar_data$group))
  n_strategies <- length(unique(bar_data$strategy))

  # Calculate number of facets and optimal column count
  n_facets <- if (n_groups > 1 && n_strategies > 1) {
    n_groups * n_strategies
  } else if (n_groups > 1) {
    n_groups
  } else if (n_strategies > 1) {
    n_strategies
  } else {
    1
  }
  ncol <- min(2, ceiling(n_facets / 3))

  facet_component <- NULL
  if ((n_groups > 1) && (n_strategies > 1)) {
    facet_component <- facet_wrap(vars(.data$strategy, .data$group), scales = "free_x", ncol = n_groups)
  } else if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group), scales = "free_x", ncol = ncol)
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy), scales = "free_x", ncol = ncol)
  }


  # Order scenarios by definition order (scenario_id), not by value

  # Base Case is always scenario_id=1, then user-defined scenarios in order
  # Arrange descending so Base Case (id=1) appears at TOP of plot
  # (ggplot draws y-axis factor levels from bottom to top)
  bar_data <- bar_data %>%
    arrange(desc(.data$scenario_id)) %>%
    mutate(scenario_name = fct_inorder(.data$scenario_name))

  # Pre-compute facet-level x-range info for zero-value label positioning
  bar_data <- bar_data %>%
    group_by(.data$strategy, .data$group) %>%
    mutate(
      facet_x_min = min(.data$value, 0, na.rm = TRUE),
      facet_x_max = max(.data$value, 0, na.rm = TRUE),
      facet_x_center = (.data$facet_x_min + .data$facet_x_max) / 2,
      # For zero values: if 0 is left of center, label goes right; otherwise left
      zero_label_right = 0 <= .data$facet_x_center
    ) %>%
    ungroup()

  # Calculate axis limits
  x_range <- range(c(0, bar_data$value), na.rm = TRUE)
  x_breaks <- pretty_breaks(n = 5)(x_range)

  # Determine expansion based on where values exist
  has_negative <- any(bar_data$value < 0, na.rm = TRUE)
  has_positive <- any(bar_data$value > 0, na.rm = TRUE)

  # Higher expansion (0.15) on sides with values for label room, lower (0.05) otherwise
  left_expand <- if (has_negative) 0.15 else 0.05
  right_expand <- if (has_positive) 0.15 else 0.05

  # Create base plot with uniform color
  p <- ggplot(bar_data, aes(
    y = .data$scenario_name,
    x = .data$value
  )) +
    geom_col(width = 0.7, fill = "#4A90D9", color = "black", linewidth = 0.2) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "gray30")

  # Add value labels with hjust for spacing (no x-offset)
  # hjust slightly below 0 (-0.1) places label to the right with gap
  # hjust slightly above 1 (1.1) places label to the left with gap
  if (value_labels) {
    p <- p +
      geom_text(
        aes(
          x = .data$value,
          label = scales::comma(.data$value, accuracy = 0.01),
          hjust = case_when(
            .data$value > 0 ~ -0.1,
            .data$value < 0 ~ 1.1,
            .data$zero_label_right ~ -0.1,
            TRUE ~ 1.1
          )
        ),
        size = 2.5,
        color = "black"
      )
  }

  p <- p +
    scale_x_continuous(breaks = x_breaks, labels = comma,
                       expand = expansion(mult = c(left_expand, right_expand))) +
    labs(y = "Scenario", x = x_label) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 8)
    )

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Prepare Scenario Bar Data
#'
#' Internal helper to prepare scenario data for bar charts.
#'
#' @param results Scenario results object from run_scenario()
#' @param summary_name Name of the summary to extract
#' @param groups Group selection
#' @param strategies Character vector of strategies
#' @param interventions Character vector of intervention strategies
#' @param comparators Character vector of comparator strategies
#' @param discounted Logical. Use discounted values?
#'
#' @return Tibble with scenario_name, value, strategy, group, is_base_case
#' @keywords internal
prepare_scenario_bar_data <- function(results,
                                       summary_name,
                                       groups,
                                       strategies,
                                       interventions,
                                       comparators,
                                       discounted) {

  # Extract scenario summaries
  scenario_data <- extract_scenario_summaries(
    results,
    summary_name = summary_name,
    value_type = "all",
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # If interventions/comparators specified, calculate differences
  if (!is.null(interventions) || !is.null(comparators)) {
    # Get all strategies
    all_strategies <- unique(scenario_data$strategy)

    # Determine comparison pairs
    comparison_pairs <- list()

    if (!is.null(interventions) && !is.null(comparators)) {
      # Both provided: N×M explicit comparisons
      for (int_strat in interventions) {
        for (comp_strat in comparators) {
          if (int_strat != comp_strat) {
            comparison_pairs[[length(comparison_pairs) + 1]] <- list(
              intervention = int_strat,
              comparator = comp_strat
            )
          }
        }
      }
    } else if (!is.null(interventions)) {
      # Intervention only: each intervention vs all others
      for (int_strat in interventions) {
        other_strategies <- setdiff(all_strategies, int_strat)
        for (other in other_strategies) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = int_strat,
            comparator = other
          )
        }
      }
    } else {
      # Comparator only: all others vs each comparator
      for (comp_strat in comparators) {
        other_strategies <- setdiff(all_strategies, comp_strat)
        for (other in other_strategies) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = other,
            comparator = comp_strat
          )
        }
      }
    }

    # Calculate differences for each comparison pair
    diff_data <- list()
    for (pair in comparison_pairs) {
      int_data <- scenario_data %>%
        filter(.data$strategy == pair$intervention) %>%
        select("scenario_id", "scenario_name", "scenario_description", "group",
               int_amount = "amount")

      comp_data <- scenario_data %>%
        filter(.data$strategy == pair$comparator) %>%
        select("scenario_id", "group", comp_amount = "amount")

      # Map to display names
      int_display <- map_names(pair$intervention, results$metadata$strategies, "display_name")
      comp_display <- map_names(pair$comparator, results$metadata$strategies, "display_name")
      comp_label <- paste0(int_display, " vs. ", comp_display)

      diff_df <- int_data %>%
        inner_join(comp_data, by = c("scenario_id", "group")) %>%
        mutate(
          value = .data$int_amount - .data$comp_amount,
          strategy = comp_label
        ) %>%
        select("scenario_id", "scenario_name", "scenario_description",
               "strategy", "group", "value")

      diff_data[[length(diff_data) + 1]] <- diff_df
    }

    bar_data <- bind_rows(diff_data)
  } else {
    # No comparison - just show absolute values
    bar_data <- scenario_data %>%
      rename(value = "amount")

    # Map strategy names to display names
    if (!is.null(results$metadata$strategies)) {
      bar_data$strategy <- map_names(bar_data$strategy, results$metadata$strategies, "display_name")
    }
  }

  # Map group names to display names
  if (!is.null(results$metadata$groups)) {
    bar_data$group <- map_names(bar_data$group, results$metadata$groups, "display_name")
  }

  # Add is_base_case flag
  bar_data <- bar_data %>%
    mutate(is_base_case = .data$scenario_name == "Base Case")

  bar_data
}


#' Plot Scenario Outcomes as Bar Chart
#'
#' Creates a horizontal bar chart showing the outcome value for each scenario.
#' Scenarios are displayed on the Y-axis with values on the X-axis.
#' The Base Case is always shown first and highlighted.
#'
#' @param results A openqaly scenario results object (output from run_scenario)
#' @param summary_name Name of the summary to plot (e.g., "total_qalys", "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups,
#'   or NULL (all groups plus aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all).
#'   Mutually exclusive with interventions/comparators.
#' @param interventions Character vector of intervention strategy name(s).
#'   Can be combined with comparators for N×M comparisons. When specified, shows
#'   differences (intervention - comparator).
#' @param comparators Character vector of comparator strategy name(s).
#'   Can be combined with interventions for N×M comparisons.
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") %>%
#'   add_scenario("Optimistic") %>%
#'   add_scenario_variable("Optimistic", "efficacy", 0.95)
#' results <- run_scenario(model)
#'
#' # Basic outcome bar chart
#' scenario_outcomes_plot(results, "total_qalys")
#'
#' # Show differences vs comparator
#' scenario_outcomes_plot(results, "total_qalys", comparators = "control")
#' }
#'
#' @export
scenario_outcomes_plot <- function(results,
                                    summary_name,
                                    groups = "overall",
                                    strategies = NULL,
                                    interventions = NULL,
                                    comparators = NULL,
                                    discounted = TRUE) {

  # Validate that strategies is mutually exclusive with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'.",
         call. = FALSE)
  }

  # Prepare bar data
  bar_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = summary_name,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Check if data is valid
  if (is.null(bar_data) || nrow(bar_data) == 0) {
    stop("No data available for scenario bar chart with specified parameters",
         call. = FALSE)
  }

  # Map summary name for axis label
  summary_label <- summary_name
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    summary_label <- map_names(summary_name, results$metadata$summaries, "display_name")
  }

  # Add prefix if showing differences
  if (!is.null(interventions) || !is.null(comparators)) {
    summary_label <- paste0("Difference in ", summary_label)
  }

  # Render bar plot
  render_scenario_bar_plot(bar_data, summary_label)
}


#' Plot Scenario Net Monetary Benefit as Bar Chart
#'
#' Creates a horizontal bar chart showing the incremental Net Monetary Benefit (NMB)
#' for each scenario. NMB = (Difference in Outcomes × WTP) - Difference in Costs.
#'
#' @param results A openqaly scenario results object (output from run_scenario)
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, or vector
#' @param wtp Optional override for willingness-to-pay. If NULL, extracts from outcome metadata.
#' @param interventions Character vector of intervention strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s).
#'   At least one of interventions or comparators must be specified.
#'
#' @return A ggplot2 object
#'
#' @details
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model)
#'
#' # NMB bar chart (comparator perspective)
#' scenario_nmb_plot(results, "total_qalys", "total_cost", comparators = "control")
#'
#' # NMB with explicit WTP
#' scenario_nmb_plot(results, "total_qalys", "total_cost",
#'                   interventions = "treatment", wtp = 50000)
#' }
#'
#' @export
scenario_nmb_plot <- function(results,
                               health_outcome,
                               cost_outcome,
                               groups = "overall",
                               wtp = NULL,
                               interventions = NULL,
                               comparators = NULL) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation",
         call. = FALSE)
  }

  # Get WTP if needed
  if (is.null(wtp)) {
    if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Provide explicit wtp parameter.",
           call. = FALSE)
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome),
           call. = FALSE)
    }
    wtp <- outcome_meta[["wtp"]][1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.",
                   health_outcome), call. = FALSE)
    }
  }

  # Prepare outcome bar data (always discounted for NMB)
  outcome_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Prepare cost bar data (always discounted for NMB)
  cost_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Calculate NMB: (delta_outcomes * WTP) - delta_costs
  nmb_data <- outcome_data %>%
    inner_join(
      cost_data %>% select("scenario_id", "strategy", "group", cost_value = "value"),
      by = c("scenario_id", "strategy", "group")
    ) %>%
    mutate(
      value = (.data$value * wtp) - .data$cost_value
    ) %>%
    select(-"cost_value")

  # Check if data is valid
  if (is.null(nmb_data) || nrow(nmb_data) == 0) {
    stop("No data available for NMB bar chart with specified parameters",
         call. = FALSE)
  }

  # Create NMB label
  outcome_label <- health_outcome
  cost_label <- cost_outcome
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    outcome_label <- map_names(health_outcome, results$metadata$summaries, "display_name")
    cost_label <- map_names(cost_outcome, results$metadata$summaries, "display_name")
  }

  wtp_formatted <- scales::comma(wtp)
  nmb_label <- glue("Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})")

  # Render bar plot
  render_scenario_bar_plot(nmb_data, nmb_label)
}


#' Plot Scenario Cost-Effectiveness (ICER) as Bar Chart
#'
#' Creates a horizontal bar chart showing the Incremental Cost-Effectiveness Ratio (ICER)
#' for each scenario. Handles edge cases: Dominated (more costly, less effective),
#' Dominant (less costly, more effective), and Equivalent (same costs and outcomes).
#'
#' @param results A openqaly scenario results object (output from run_scenario)
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, or vector
#' @param interventions Character vector of intervention strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s).
#'   At least one of interventions or comparators must be specified.
#'
#' @return A ggplot2 object
#'
#' @details
#' ICER = (Cost_intervention - Cost_comparator) / (Outcome_intervention - Outcome_comparator)
#'
#' Special cases:
#' - Dominated: Intervention is more costly and less effective (shown as arrow to right)
#' - Dominant: Intervention is less costly and more effective (ICER = 0 or negative)
#' - Equivalent: Identical outcomes result in undefined ICER (shown as text)
#'
#' ICER calculations always use discounted values.
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model)
#'
#' # CE bar chart
#' scenario_ce_plot(results, "total_qalys", "total_cost", comparators = "control")
#' }
#'
#' @export
scenario_ce_plot <- function(results,
                              health_outcome,
                              cost_outcome,
                              groups = "overall",
                              interventions = NULL,
                              comparators = NULL) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for CE calculation",
         call. = FALSE)
  }

  # Prepare outcome data (always discounted for CE)
  outcome_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Prepare cost data (always discounted for CE)
  cost_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Calculate ICER: delta_cost / delta_outcome
  ce_data <- outcome_data %>%
    inner_join(
      cost_data %>% select("scenario_id", "strategy", "group", delta_cost = "value"),
      by = c("scenario_id", "strategy", "group")
    ) %>%
    rename(delta_outcome = "value") %>%
    mutate(
      # Calculate ICER with special case handling
      # Match the logic in icer() function for consistency
      icer = case_when(
        # Identical: no differences at all
        abs(.data$delta_outcome) < .Machine$double.eps &
          abs(.data$delta_cost) < .Machine$double.eps ~ NaN,
        # Dominated (weakly): worse or equal effects at higher or equal cost
        # with at least one strict detriment
        (.data$delta_outcome < 0 & .data$delta_cost > 0) |
          (.data$delta_outcome < 0 & abs(.data$delta_cost) < .Machine$double.eps) |
          (abs(.data$delta_outcome) < .Machine$double.eps & .data$delta_cost > 0) ~ Inf,
        # Dominant (weakly): better or equal effects at lower or equal cost
        # with at least one strict improvement
        (.data$delta_outcome > 0 & .data$delta_cost < 0) |
          (.data$delta_outcome > 0 & abs(.data$delta_cost) < .Machine$double.eps) |
          (abs(.data$delta_outcome) < .Machine$double.eps & .data$delta_cost < 0) ~ 0,
        # Normal ICER for remaining cases
        TRUE ~ .data$delta_cost / .data$delta_outcome
      ),
      # Classify for display
      # Flipped (SW quadrant): intervention cheaper but worse (delta_cost < 0 AND delta_outcome < 0)
      # This produces positive ICER (negative/negative = positive), but semantically flipped
      ce_class = case_when(
        is.nan(.data$icer) ~ "identical",
        is.infinite(.data$icer) & .data$icer > 0 ~ "dominated",
        .data$icer == 0 ~ "dominant",
        .data$delta_outcome < 0 & .data$delta_cost < 0 ~ "flipped",  # SW quadrant (both negative)
        TRUE ~ "normal"
      ),
      # Display value for bar (use absolute value, handle special cases)
      value = case_when(
        .data$ce_class == "dominated" ~ NA_real_,   # No bar for dominated (arrow instead)
        .data$ce_class == "identical" ~ NA_real_,   # No bar for identical
        .data$ce_class == "dominant" ~ 0,           # Bar to 0
        .data$ce_class == "flipped" ~ abs(.data$icer),  # Show absolute value
        TRUE ~ .data$icer
      )
      # Note: Labels are assigned in render_scenario_ce_bar_plot() after footnote order is determined
    )

  # Check if data is valid
  if (is.null(ce_data) || nrow(ce_data) == 0) {
    stop("No data available for CE bar chart with specified parameters",
         call. = FALSE)
  }

  # Render CE bar plot with special handling
  render_scenario_ce_bar_plot(ce_data, results$metadata)
}


#' Render Scenario CE Bar Plot
#'
#' Internal helper to create horizontal bar chart for ICER data with special case handling.
#'
#' @param ce_data Prepared CE data with icer, ce_class, value columns
#' @param metadata Model metadata for display name mapping
#'
#' @return A ggplot2 object
#' @keywords internal
render_scenario_ce_bar_plot <- function(ce_data, metadata) {

  # Determine faceting
  n_groups <- length(unique(ce_data$group))
  n_strategies <- length(unique(ce_data$strategy))

  n_facets <- if (n_groups > 1 && n_strategies > 1) {
    n_groups * n_strategies
  } else if (n_groups > 1) {
    n_groups
  } else if (n_strategies > 1) {
    n_strategies
  } else {
    1
  }
  ncol <- min(2, ceiling(n_facets / 3))

  facet_component <- NULL
  if ((n_groups > 1) && (n_strategies > 1)) {
    facet_component <- facet_wrap(vars(.data$strategy, .data$group), scales = "free_x", ncol = n_groups)
  } else if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group), scales = "free_x", ncol = ncol)
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy), scales = "free_x", ncol = ncol)
  }

  # Order scenarios by definition order (scenario_id), not by value
  # Use ascending order so Base Case (id=1) gets y_numeric=1 and appears at TOP with scale_y_reverse
  ce_data <- ce_data %>%
    arrange(.data$scenario_id) %>%
    mutate(
      scenario_name = fct_inorder(.data$scenario_name),
      y_numeric = as.numeric(fct_inorder(.data$scenario_name))
    )

  # Extract intervention/comparator names from strategy label (e.g., "Treatment vs. Control")
  ce_data <- ce_data %>%
    mutate(
      parts = strsplit(as.character(.data$strategy), " vs\\. "),
      intervention_name = sapply(.data$parts, `[`, 1),
      comparator_name = sapply(.data$parts, `[`, 2)
    ) %>%
    select(-"parts")

  # Create y-axis label lookup (scenario_name by y_numeric position)
  y_labels <- ce_data %>%
    distinct(.data$scenario_name, .data$y_numeric) %>%
    arrange(.data$y_numeric)

  # Determine footnote order based on first occurrence in visual display order
  # Visual order: facets left-to-right then top-to-bottom, scenarios top-to-bottom within facet
  # Since y-axis uses scale_y_reverse, lower y_numeric = higher on screen (top)
  # So ascending y_numeric = top-to-bottom visual order
  special_cases <- ce_data %>%
    filter(.data$ce_class %in% c("flipped", "identical")) %>%
    arrange(.data$strategy, .data$group, .data$y_numeric) %>%
    pull(.data$ce_class) %>%
    unique()

  # Assign asterisks: first type gets *, second gets **
  asterisk_map <- if (length(special_cases) > 0) {
    stats::setNames(c("*", "**")[seq_along(special_cases)], special_cases)
  } else {
    character(0)
  }

  # Generate labels now that we have asterisk assignments
  ce_data <- ce_data %>%
    mutate(
      label = case_when(
        .data$ce_class == "dominated" ~ "Dominated",
        .data$ce_class == "identical" ~ paste0("Identical", asterisk_map["identical"]),
        .data$ce_class == "dominant" ~ "Dominant",
        .data$ce_class == "flipped" ~ paste0(
          scales::comma(abs(.data$icer), accuracy = 1),
          asterisk_map["flipped"]
        ),
        TRUE ~ scales::comma(.data$icer, accuracy = 1)
      )
    )

  # Separate data by ce_class for different visual treatments
  normal_data <- ce_data %>% filter(.data$ce_class %in% c("normal", "dominant", "flipped"))
  dominated_data <- ce_data %>% filter(.data$ce_class == "dominated")
  identical_data <- ce_data %>% filter(.data$ce_class == "identical")

  # Calculate axis limits from plottable data (normal, dominant, flipped)
  if (nrow(normal_data) > 0) {
    x_range <- range(c(0, normal_data$value), na.rm = TRUE)
  } else {
    x_range <- c(0, 100000)  # Default range if no plottable ICERs

  }
  x_breaks <- pretty_breaks(n = 5)(x_range)
  x_tick_max <- max(x_breaks)

  # Calculate dominated_position (15% past last X-axis tick)
  dominated_position <- x_tick_max * 1.15
  x_limits <- c(min(x_breaks), dominated_position)

  # Calculate arrow dimensions (DSA CE style)
  max_y <- length(unique(ce_data$scenario_name))
  arrow_notch_y <- 0.15
  arrow_head_width <- arrow_notch_y * 2 * (dominated_position / (max_y + 1))

  # Create base plot with numeric y-axis (will use scale_y_reverse for proper ordering)
  p <- ggplot(ce_data, aes(y = .data$y_numeric))

  # Add bars for normal, dominant, and flipped scenarios
  if (nrow(normal_data) > 0) {
    p <- p +
      geom_col(
        data = normal_data,
        aes(x = .data$value, y = .data$y_numeric),
        fill = "#4A90D9",
        width = 0.7, color = "black", linewidth = 0.2,
        orientation = "y"
      )
  }

  # Add notched polygon arrows for dominated scenarios (DSA CE style)
  if (nrow(dominated_data) > 0) {
    arrow_polygons <- dominated_data %>%
      rowwise() %>%
      mutate(
        polygon_data = list(tibble(
          x = c(
            0,                                          # Left bottom
            dominated_position - arrow_head_width,     # Right before arrow
            dominated_position - arrow_head_width,     # Arrow notch bottom
            dominated_position,                        # Arrow tip
            dominated_position - arrow_head_width,     # Arrow notch top
            dominated_position - arrow_head_width,     # Right after arrow
            0                                          # Left top
          ),
          y = c(
            y_numeric - 0.35,                          # Left bottom
            y_numeric - 0.35,                          # Right before arrow
            y_numeric - 0.35 - arrow_notch_y,          # Arrow notch bottom
            y_numeric,                                  # Arrow tip
            y_numeric + 0.35 + arrow_notch_y,          # Arrow notch top
            y_numeric + 0.35,                          # Right after arrow
            y_numeric + 0.35                           # Left top
          )
        ))
      ) %>%
      ungroup()

    arrow_expanded <- arrow_polygons %>%
      select("scenario_name", "strategy", "group", "polygon_data") %>%
      unnest("polygon_data")

    p <- p + geom_polygon(
      data = arrow_expanded,
      aes(x = .data$x, y = .data$y, group = interaction(.data$scenario_name, .data$strategy, .data$group)),
      fill = "#4A90D9", color = "black", linewidth = 0.2
    )
  }

  # Add labels for identical scenarios (no bar, just text)
  if (nrow(identical_data) > 0) {
    p <- p + geom_text(
      data = identical_data,
      aes(x = 0, y = .data$y_numeric, label = .data$label),
      hjust = -0.1, size = 2.5
    )
  }

  # Add value labels for normal/dominant/flipped/dominated scenarios
  label_data <- ce_data %>%
    filter(.data$ce_class != "identical")
  if (nrow(label_data) > 0) {
    p <- p +
      geom_text(
        data = label_data,
        aes(x = if_else(is.na(.data$value), dominated_position - arrow_head_width, .data$value),
            y = .data$y_numeric,
            label = .data$label,
            hjust = if_else(is.na(.data$value), 1.1, -0.1)),
        size = 2.5, color = "black"
      )
  }

  # Add vertical line at 0
  p <- p + geom_vline(xintercept = 0, linewidth = 0.5, color = "gray30")

  # Generate footnotes in display order
  all_footnotes <- c()
  for (case_type in special_cases) {
    asterisks <- asterisk_map[case_type]
    if (case_type == "flipped") {
      # One footnote per unique strategy pair
      flipped_footnotes <- ce_data %>%
        filter(.data$ce_class == "flipped") %>%
        distinct(.data$intervention_name, .data$comparator_name) %>%
        mutate(
          footnote = sprintf("%s %s is more costly & more effective than %s. ICER represents cost-effectiveness of %s vs. %s.",
                             asterisks, .data$comparator_name, .data$intervention_name,
                             .data$comparator_name, .data$intervention_name)
        ) %>%
        pull(.data$footnote)
      all_footnotes <- c(all_footnotes, flipped_footnotes)
    } else if (case_type == "identical") {
      all_footnotes <- c(all_footnotes, sprintf(
        "%s Bar could not be displayed because ICER is undefined when costs and outcomes are identical.",
        asterisks
      ))
    }
  }

  footnote_text <- if (length(all_footnotes) > 0) paste(all_footnotes, collapse = "\n") else NULL

  p <- p +
    scale_x_continuous(
      breaks = x_breaks,
      labels = comma,
      expand = expansion(mult = c(0.05, 0)),
      limits = x_limits
    ) +
    scale_y_reverse(
      breaks = y_labels$y_numeric,
      labels = y_labels$scenario_name,
      expand = expansion(mult = c(0, 0), add = c(0.5, 0.5))
    ) +
    labs(y = "Scenario", x = "Incremental Cost-Effectiveness Ratio (ICER)",
         caption = footnote_text) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 8),
      plot.caption = element_text(hjust = 0, size = 9, face = "italic")
    )

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}
