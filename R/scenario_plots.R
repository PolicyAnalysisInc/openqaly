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
                       expand = expansion(mult = c(0.05, 0.15))) +
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
                                    discounted = FALSE) {

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

  wtp_formatted <- format(wtp, big.mark = ",")
  nmb_label <- glue("Incremental Net Monetary Benefit (\u03bb = {wtp_formatted})")

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
      icer = case_when(
        abs(.data$delta_outcome) < .Machine$double.eps ~ NaN,  # Equivalent (undefined)
        .data$delta_outcome < 0 & .data$delta_cost > 0 ~ Inf,  # Dominated
        .data$delta_outcome > 0 & .data$delta_cost <= 0 ~ 0,   # Dominant (or cost-saving)
        TRUE ~ .data$delta_cost / .data$delta_outcome          # Normal ICER
      ),
      # Classify for display
      # SW quadrant: intervention cheaper but worse (delta_cost < 0 AND delta_outcome < 0)
      # This produces positive ICER (negative/negative = positive), but semantically flipped
      ce_class = case_when(
        is.nan(.data$icer) ~ "equivalent",
        is.infinite(.data$icer) & .data$icer > 0 ~ "dominated",
        .data$icer == 0 ~ "dominant",
        .data$delta_outcome < 0 & .data$delta_cost < 0 ~ "sw_quadrant",  # SW quadrant (both negative)
        TRUE ~ "normal"
      ),
      # Display value for bar (use absolute value, handle special cases)
      value = case_when(
        .data$ce_class == "dominated" ~ NA_real_,   # No bar for dominated (arrow instead)
        .data$ce_class == "equivalent" ~ NA_real_,  # No bar for equivalent
        .data$ce_class == "dominant" ~ 0,           # Bar to 0
        .data$ce_class == "sw_quadrant" ~ abs(.data$icer),  # Show absolute value
        TRUE ~ .data$icer
      ),
      # Label text (initial - will be updated for direction changes)
      label = case_when(
        .data$ce_class == "dominated" ~ "Dominated",
        .data$ce_class == "equivalent" ~ "Equivalent",
        .data$ce_class == "dominant" ~ "Dominant",
        .data$ce_class == "sw_quadrant" ~ sprintf("%s*", scales::comma(abs(.data$icer), accuracy = 1)),
        TRUE ~ scales::comma(.data$icer, accuracy = 1)
      )
    )

  # Determine base case classification for direction change detection
  base_case_class <- ce_data %>%
    filter(.data$scenario_name == "Base Case") %>%
    pull(.data$ce_class) %>%
    first()

  # Detect direction changes relative to base case
  # "Direction change" = scenarios where ICER direction is incompatible with base
  ce_data <- ce_data %>%
    mutate(
      has_direction_change = case_when(
        .data$scenario_name == "Base Case" ~ FALSE,
        # Base is normal/dominated (positive direction) and scenario is sw_quadrant (flipped direction)
        base_case_class %in% c("normal", "dominated") & .data$ce_class == "sw_quadrant" ~ TRUE,
        # Base is sw_quadrant and scenario is normal/dominated
        base_case_class == "sw_quadrant" & .data$ce_class %in% c("normal", "dominated") ~ TRUE,
        TRUE ~ FALSE
      )
    )

  # Override ce_class for direction changes - these get error labels, not bars
  ce_data <- ce_data %>%
    mutate(
      ce_class = if_else(.data$has_direction_change, "direction_change", .data$ce_class),
      value = if_else(.data$has_direction_change, NA_real_, .data$value),
      label = if_else(
        .data$has_direction_change,
        sprintf("%s*", scales::comma(abs(.data$icer), accuracy = 1)),
        .data$label
      )
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
#' @param ce_data Prepared CE data with icer, ce_class, value, label columns
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

  # Separate normal bars from special cases
  normal_data <- ce_data %>% filter(.data$ce_class %in% c("normal", "dominant", "sw_quadrant"))
  dominated_data <- ce_data %>% filter(.data$ce_class == "dominated")
  equivalent_data <- ce_data %>% filter(.data$ce_class == "equivalent")
  direction_change_data <- ce_data %>% filter(.data$ce_class == "direction_change")

  # Check if base case is identical (equivalent)
  base_case_identical <- ce_data %>%
    filter(.data$scenario_name == "Base Case", .data$ce_class == "equivalent") %>%
    nrow() > 0

  # Calculate axis limits from normal data only
  if (nrow(normal_data) > 0) {
    x_range <- range(c(0, normal_data$value), na.rm = TRUE)
  } else {
    x_range <- c(0, 100000)  # Default range if no normal ICERs
  }
  x_breaks <- pretty_breaks(n = 5)(x_range)
  x_tick_max <- max(x_breaks)
  x_offset <- diff(range(x_breaks)) * 0.02

  # Calculate dominated_position (15% past last X-axis tick)
  dominated_position <- x_tick_max * 1.15
  x_limits <- c(min(x_breaks), dominated_position)

  # Calculate arrow dimensions (DSA CE style)
  max_y <- length(unique(ce_data$scenario_name))
  arrow_notch_y <- 0.15
  arrow_head_width <- arrow_notch_y * 2 * (dominated_position / (max_y + 1))

  # Create base plot with numeric y-axis (will use scale_y_reverse for proper ordering)
  p <- ggplot(ce_data, aes(y = .data$y_numeric))

  # Add normal bars with uniform color
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
    # y_numeric is already computed on ce_data and inherited by dominated_data

    # Note: In rowwise() + mutate() with list(), .data$ doesn't work inside list()
    # Access columns directly by name instead
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

  # Add verbose error labels for equivalent scenarios (DSA CE style)
  if (nrow(equivalent_data) > 0 && !base_case_identical) {
    equivalent_data <- equivalent_data %>%
      mutate(
        error_label = sprintf(
          "Bar could not be displayed for %s scenario because\nICER is undefined when costs and outcomes are identical\nbetween %s and %s.",
          .data$scenario_name, .data$intervention_name, .data$comparator_name
        )
      )

    p <- p + geom_label(
      data = equivalent_data,
      aes(x = x_offset, y = .data$y_numeric, label = .data$error_label),
      hjust = 0, size = 2.5,
      fill = "white", color = "black",
      label.size = 0.3, label.padding = unit(0.15, "lines")
    )
  }

  # Add verbose error labels for direction change scenarios (DSA CE style)
  if (nrow(direction_change_data) > 0) {
    direction_change_data <- direction_change_data %>%
      mutate(
        error_label = sprintf(
          "Bar could not be displayed for %s scenario because\ndirectionality of ICER changed relative to base case.\nICER of %s reflects comparison of %s vs. %s.",
          .data$scenario_name,
          scales::comma(abs(.data$icer), accuracy = 1),
          .data$comparator_name, .data$intervention_name
        )
      )

    p <- p + geom_label(
      data = direction_change_data,
      aes(x = x_offset, y = .data$y_numeric, label = .data$error_label),
      hjust = 0, size = 2.5,
      fill = "white", color = "black",
      label.size = 0.3, label.padding = unit(0.15, "lines")
    )
  }

  # Add value labels for normal/dominant/dominated scenarios (exclude direction changes which have error labels)
  label_data <- ce_data %>%
    filter(!is.na(.data$label), !.data$ce_class %in% c("equivalent", "direction_change"))
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

  # Generate footnotes for direction change scenarios
  footnote_text <- NULL
  if (nrow(direction_change_data) > 0) {
    footnotes <- direction_change_data %>%
      distinct(.data$intervention_name, .data$comparator_name) %>%
      mutate(
        footnote = sprintf("* %s is more costly & more effective than %s. ICER represents cost-effectiveness of %s vs. %s.",
                           .data$comparator_name, .data$intervention_name,
                           .data$comparator_name, .data$intervention_name)
      ) %>%
      pull(.data$footnote) %>%
      unique()

    footnote_text <- paste(footnotes, collapse = "\n")
  }

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

  # Add full-chart error for identical base case
  if (base_case_identical) {
    # Get intervention/comparator names from first row
    first_row <- ce_data[1, ]
    full_chart_error <- sprintf(
      "Scenario analysis chart for %s vs. %s cannot be displayed\nbecause the difference in outcomes and costs in the base case is zero,\nresulting in an undefined ICER.",
      first_row$intervention_name, first_row$comparator_name
    )

    # Add placeholder to ensure facet panel appears (use middle of y-axis)
    y_mid <- (max_y + 1) / 2
    p <- p + geom_blank(aes(x = dominated_position / 2, y = y_mid))

    p <- p + geom_label(
      data = data.frame(
        x = dominated_position / 2,
        y = y_mid,
        strategy = first_row$strategy,
        group = first_row$group
      ),
      aes(x = .data$x, y = .data$y),
      label = full_chart_error,
      size = 5, fill = "white", color = "black",
      label.size = 0.5, label.padding = unit(0.25, "lines"),
      hjust = 0.5, vjust = 0
    )
  }

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}
