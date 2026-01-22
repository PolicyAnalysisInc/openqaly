#' Format Parameter Value for Display
#'
#' Internal helper to format parameter values for DSA plot labels.
#' Uses comma + decimal notation instead of scientific notation.
#'
#' @param value Numeric value to format
#' @param digits Number of significant figures (default: 4)
#'
#' @return Formatted string with commas and no scientific notation
#' @keywords internal
format_param_value <- function(value, digits = 4) {
  # Use signif() to preserve significant figures for all magnitudes
  rounded <- signif(value, digits)

  # Format with commas, no scientific notation
  formatted <- format(rounded, big.mark = ",", scientific = FALSE, trim = TRUE)

  # Remove unnecessary trailing zeros after decimal point
  # This regex removes .00, .0, etc. but keeps .5, .50 when significant
  formatted <- sub("(\\.0+)$", "", formatted)

  formatted
}

#' Abbreviate Time Unit for Display
#'
#' Internal helper to convert time unit names to abbreviated forms for display.
#'
#' @param unit Character string of time unit (e.g., "years", "months")
#' @return Abbreviated unit string (e.g., "yrs", "mos")
#' @keywords internal
abbreviate_time_unit <- function(unit) {
  if (is.null(unit) || is.na(unit) || unit == "") {
    return("")
  }
  unit_lower <- tolower(unit)
  switch(unit_lower,
    "years" = "yrs",
    "year" = "yrs",
    "months" = "mos",
    "month" = "mos",
    "weeks" = "wks",
    "week" = "wks",
    "days" = "days",
    "day" = "days",
    "cycles" = "cycles",
    "cycle" = "cycles",
    unit  # Return as-is if no match
  )
}

#' Get Unit Suffix for DSA Setting Parameter
#'
#' Internal helper to determine the appropriate unit suffix for a DSA setting parameter.
#'
#' @param param_name Name of the setting parameter
#' @param settings Model settings list containing timeframe_unit, cycle_length_unit, etc.
#' @return Unit suffix string (e.g., "%", "yrs", "") or empty string if no unit
#' @keywords internal
get_setting_unit_suffix <- function(param_name, settings) {
  if (is.null(settings)) {
    return("")
  }

  switch(param_name,
    "discount_cost" = "%",
    "discount_outcomes" = "%",
    "timeframe" = abbreviate_time_unit(settings$timeframe_unit),
    "cycle_length" = abbreviate_time_unit(settings$cycle_length_unit),
    ""  # No suffix for: timeframe_unit, cycle_length_unit, half_cycle_method,
        # reduce_state_cycle, days_per_year
  )
}

#' Render Tornado Plot from Prepared Data
#'
#' Internal helper to create tornado plot visualization from prepared tornado data.
#' This function handles the plotting logic for both outcome and NMB tornado plots.
#'
#' @param tornado_data Prepared tibble with columns: parameter_display_name, strategy,
#'   group, low, base, high, range
#' @param summary_label String for x-axis label
#' @param facet_component Optional faceting component (if NULL, determined automatically)
#'
#' @return A ggplot2 object
#' @keywords internal
render_tornado_plot <- function(tornado_data, summary_label, facet_component = NULL) {

  # Determine faceting if not provided
  n_groups <- length(unique(tornado_data$group))
  n_strategies <- length(unique(tornado_data$strategy))

  # Calculate number of facets and optimal column count
  # Fill at least 3 rows before adding a column
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

  if (is.null(facet_component)) {
    if ((n_groups > 1) && (n_strategies > 1)) {
      facet_component <- facet_wrap(vars(.data$strategy, .data$group), scales = "free_y", ncol = n_groups)
    } else if ((n_groups > 1) && (n_strategies == 1)) {
      facet_component <- facet_wrap(vars(.data$group), scales = "free_y", ncol = ncol)
    } else if ((n_strategies > 1) && (n_groups == 1)) {
      facet_component <- facet_wrap(vars(.data$strategy), scales = "free_y", ncol = ncol)
    } else {
      facet_component <- NULL
    }
  }

  # Create facet grouping variable for reorder_within (must match faceting granularity)
  if (n_groups > 1 && n_strategies > 1) {
    tornado_data <- tornado_data %>%
      mutate(reorder_group = interaction(.data$strategy, .data$group, drop = TRUE))
  } else if (n_groups > 1) {
    tornado_data <- tornado_data %>%
      mutate(reorder_group = .data$group)
  } else if (n_strategies > 1) {
    tornado_data <- tornado_data %>%
      mutate(reorder_group = .data$strategy)
  } else {
    tornado_data <- tornado_data %>%
      mutate(reorder_group = factor("all"))
  }

  # Y-axis spacing multiplier (matches CE tornado for visual consistency)
  y_spacing <- 1.2

  # Detect same-side parameters (both low and high on same side of base case)
  tornado_data <- tornado_data %>%
    mutate(same_side = (.data$low > .data$base & .data$high > .data$base) |
                       (.data$low < .data$base & .data$high < .data$base))

  # Reshape to long format for two bars per parameter
  tornado_long <- tornado_data %>%
    pivot_longer(
      cols = c("low", "high"),
      names_to = "variation",
      values_to = "value"
    ) %>%
    mutate(
      variation = factor(.data$variation, levels = c("low", "high"),
                        labels = c("Low", "High")),
      xmin = pmin(.data$base, .data$value),
      xmax = pmax(.data$base, .data$value),
      x_center = (.data$xmin + .data$xmax) / 2,
      x_width = .data$xmax - .data$xmin,
      # For same-side bars, use smaller height and offset vertically
      bar_height = if_else(.data$same_side, 0.4, 0.8),
      y_offset = case_when(
        .data$same_side & .data$variation == "High" ~ -0.2,
        .data$same_side & .data$variation == "Low" ~ 0.2,
        TRUE ~ 0
      )
    )

  # Create base case data for vertical lines (one per strategy-group combination)
  base_case_data <- tornado_long %>%
    distinct(.data$strategy, .data$group, .data$base, .data$reorder_group)

  # Create base case labels for display
  base_case_labels <- base_case_data %>%
    mutate(
      base_label = paste0("Base Case: ", scales::comma(.data$base, accuracy = 0.01)),
      y_pos = 0.5 * y_spacing  # Position above top bar
    )

  # Calculate axis breaks and limits to include 0 and extend beyond data
  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, tornado_long$xmin, tornado_long$xmax))
  x_breaks <- breaks_fn(x_range)
  x_tick_max <- max(abs(x_breaks))

  # Calculate left margin needed for left-side labels to prevent clipping
  left_label_margin <- x_tick_max * 0.08
  x_limits <- c(min(x_breaks) - left_label_margin, max(x_breaks))

  # Create ordered y-factor within each facet group
  # Compute y positions as numeric values within each facet so each panel has
  # its own 1, 2, 3... sequence. This avoids duplicate labels caused by global factor levels.
  # Largest range gets y=1 (top position on standard y-axis)
  tornado_long <- tornado_long %>%
    group_by(.data$reorder_group) %>%
    mutate(
      # Create numeric y position ordered by range (largest = 1, smallest = N) within each group
      y_base = dense_rank(desc(.data$range)) * y_spacing,
      # Apply offset for same-side bars
      y_numeric = .data$y_base + .data$y_offset
    ) %>%
    ungroup()

  # Create a label lookup table for y-axis labels (one entry per parameter per facet group)
  y_labels <- tornado_long %>%
    distinct(.data$reorder_group, .data$parameter_display_name, .data$y_base) %>%
    arrange(.data$reorder_group, .data$y_base)

  # Get max y for axis limits
  max_y <- max(tornado_long$y_base)

  # Prepare bar value labels with edge case handling
  # Labels show the outcome value at each bar endpoint
  label_data <- tornado_long %>%
    mutate(
      # Format the value as the label text
      label = scales::comma(.data$value, accuracy = 0.01),
      # Detect no-impact bars (value equals base case)
      is_no_impact = abs(.data$value - .data$base) < .Machine$double.eps * 100,
      # Natural direction based on value vs base
      natural_goes_right = .data$value > .data$base,
      natural_goes_left = .data$value < .data$base
    ) %>%
    # Get info about the OTHER variation for each parameter (for no-impact tiebreaker)
    group_by(.data$reorder_group, .data$parameter_display_name) %>%
    mutate(
      other_goes_right = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$natural_goes_right),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$natural_goes_right),
        TRUE ~ FALSE
      ),
      other_goes_left = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$natural_goes_left),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$natural_goes_left),
        TRUE ~ FALSE
      ),
      other_is_no_impact = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$is_no_impact),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$is_no_impact),
        TRUE ~ FALSE
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Determine label direction with tiebreaker for no-impact bars
      goes_right = case_when(
        !.data$is_no_impact ~ .data$natural_goes_right,
        # No-impact bar: go opposite of other variation
        .data$is_no_impact & .data$other_goes_left ~ TRUE,
        .data$is_no_impact & .data$other_goes_right ~ FALSE,
        # Both no-impact: High goes right, Low goes left
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "High" ~ TRUE,
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "Low" ~ FALSE,
        TRUE ~ .data$natural_goes_right
      ),
      # Position labels at bar endpoints with small offset
      label_x = if_else(
        .data$goes_right,
        .data$xmax + x_tick_max * 0.01,
        .data$xmin - x_tick_max * 0.01
      ),
      # Horizontal justification: left-align for right-side, right-align for left-side
      label_hjust = if_else(.data$goes_right, 0, 1)
    )

  # Create tornado plot using geom_tile with numeric y-axis
  p <- ggplot(tornado_long, aes(
    y = .data$y_numeric,
    x = .data$x_center,
    width = .data$x_width,
    height = .data$bar_height,
    fill = .data$variation
  )) +
    geom_tile(color = "black", linewidth = 0.2) +
    geom_vline(data = base_case_data, aes(xintercept = .data$base), linewidth = 0.5) +
    geom_label(
      data = base_case_labels,
      aes(x = .data$base, y = .data$y_pos, label = .data$base_label),
      vjust = 0,
      size = 2,
      fontface = "bold",
      fill = "white",
      linewidth = 0.5,
      label.padding = unit(0.25, "lines"),
      inherit.aes = FALSE
    ) +
    geom_text(
      data = label_data,
      aes(x = .data$label_x, y = .data$y_numeric, label = .data$label,
          hjust = .data$label_hjust),
      vjust = 0,
      size = 2,
      inherit.aes = FALSE
    ) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, labels = comma) +
    scale_y_reverse(
      breaks = seq_len(ceiling(max_y / y_spacing)) * y_spacing,
      labels = function(y) {
        # For each y value, look up the label from the first matching entry
        sapply(y, function(yval) {
          match_row <- y_labels %>% filter(.data$y_base == yval) %>% slice(1)
          if (nrow(match_row) > 0) match_row$parameter_display_name else ""
        })
      },
      expand = expansion(mult = c(0, 0), add = c(0.1, 0.7))
    ) +
    scale_fill_manual(values = c("Low" = "#F8766D", "High" = "#00BFC4")) +
    labs(y = NULL, x = summary_label, fill = "Parameter Value") +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Extract Parameter Values from DSA Results
#'
#' Internal helper to extract the actual low and high parameter values
#' from DSA segments for display in tornado plot labels.
#'
#' @param results DSA results object
#' @param tornado_data Tornado data tibble with parameter, strategy, group columns
#' @param interventions Intervention strategies (if showing differences)
#' @param comparators Comparator strategies (if showing differences)
#'
#' @return Tibble with parameter, strategy, group, param_low_value, param_high_value
#' @keywords internal
extract_parameter_values <- function(results, tornado_data, interventions, comparators) {
  # Get unique parameter-strategy-group combinations
  param_combos <- tornado_data %>%
    distinct(.data$parameter, .data$strategy, .data$group)

  # Extract parameter values for each combination
  param_values_list <- list()

  for (i in seq_len(nrow(param_combos))) {
    param_name <- param_combos$parameter[i]
    strat <- param_combos$strategy[i]
    grp <- param_combos$group[i]

    # Get the parameter info from dsa_metadata
    param_info <- results$dsa_metadata %>%
      filter(.data$parameter == param_name)

    if (nrow(param_info) == 0) {
      next  # Skip if parameter not found
    }

    # Get run_ids for low and high variations
    low_run_id <- param_info %>% filter(.data$variation == "low") %>% pull(.data$run_id)
    high_run_id <- param_info %>% filter(.data$variation == "high") %>% pull(.data$run_id)

    if (length(low_run_id) == 0 || length(high_run_id) == 0) {
      next  # Skip if run_ids not found
    }

    low_run_id <- low_run_id[1]
    high_run_id <- high_run_id[1]

    # When showing differences, extract values from the base strategies
    extract_strategy <- strat
    if (!is.null(interventions) || !is.null(comparators)) {
      # Strategy name is a comparison label; extract from interventions/comparators
      if (!is.null(interventions)) {
        extract_strategy <- interventions
      } else {
        # Extract first strategy name from comparison label
        extract_strategy <- results$metadata$strategies$name[1]
      }
    }

    # Extract parameter values from segments
    # For aggregated data, any segment will have the same parameter values
    # Try to find a segment matching strategy and group
    search_group <- grp
    # If group is "_aggregated", "Aggregated", or "Overall", use the actual segment group
    if (search_group %in% c("_aggregated", "Aggregated", "Overall")) {
      # Just get any segment for this strategy and run_id
      low_segment <- results$segments %>%
        filter(.data$run_id == low_run_id, .data$strategy == extract_strategy) %>%
        slice(1)
      high_segment <- results$segments %>%
        filter(.data$run_id == high_run_id, .data$strategy == extract_strategy) %>%
        slice(1)
    } else {
      # For specific groups, try to match the group
      low_segment <- results$segments %>%
        filter(.data$run_id == low_run_id, .data$strategy == extract_strategy, .data$group == search_group) %>%
        slice(1)
      high_segment <- results$segments %>%
        filter(.data$run_id == high_run_id, .data$strategy == extract_strategy, .data$group == search_group) %>%
        slice(1)

      # If segments not found with group filter, try without
      if (nrow(low_segment) == 0) {
        low_segment <- results$segments %>%
          filter(.data$run_id == low_run_id, .data$strategy == extract_strategy) %>%
          slice(1)
      }
      if (nrow(high_segment) == 0) {
        high_segment <- results$segments %>%
          filter(.data$run_id == high_run_id, .data$strategy == extract_strategy) %>%
          slice(1)
      }
    }

    # Extract parameter value from overrides
    param_type <- param_info$parameter_type[1]
    low_value <- NA_real_
    high_value <- NA_real_

    if (nrow(low_segment) > 0) {
      if (param_type == "variable") {
        param_overrides <- low_segment$parameter_overrides[[1]]
        if (!is.null(param_overrides) && param_name %in% names(param_overrides)) {
          low_value <- param_overrides[[param_name]]
        }
      } else if (param_type == "setting") {
        setting_overrides <- low_segment$setting_overrides[[1]]
        if (!is.null(setting_overrides) && param_name %in% names(setting_overrides)) {
          low_value <- setting_overrides[[param_name]]
        }
      }
    }

    if (nrow(high_segment) > 0) {
      if (param_type == "variable") {
        param_overrides <- high_segment$parameter_overrides[[1]]
        if (!is.null(param_overrides) && param_name %in% names(param_overrides)) {
          high_value <- param_overrides[[param_name]]
        }
      } else if (param_type == "setting") {
        setting_overrides <- high_segment$setting_overrides[[1]]
        if (!is.null(setting_overrides) && param_name %in% names(setting_overrides)) {
          high_value <- setting_overrides[[param_name]]
        }
      }
    }

    # Add to list
    param_values_list[[i]] <- tibble(
      parameter = param_name,
      strategy = strat,
      group = grp,
      param_low_value = low_value,
      param_high_value = high_value,
      param_type = param_type
    )
  }

  # Combine all parameter values
  if (length(param_values_list) > 0) {
    bind_rows(param_values_list)
  } else {
    tibble(
      parameter = character(),
      strategy = character(),
      group = character(),
      param_low_value = numeric(),
      param_high_value = numeric(),
      param_type = character()
    )
  }
}


#' Prepare DSA Tornado Data
#'
#' Internal helper to reshape DSA summary data into tornado plot format
#' with low, base, and high values for each parameter.
#'
#' @param results DSA results object from run_dsa()
#' @param summary_name Name of the summary to plot
#' @param groups Group selection: "aggregated" (default), specific group name, or NULL
#'   (all groups plus aggregated)
#' @param strategies Character vector of strategies (used only when interventions/comparators are NULL)
#' @param interventions Character vector of intervention strategy name(s) (technical names).
#'   Can be combined with comparators for N×M comparisons.
#' @param comparators Character vector of comparator strategy name(s) (technical names).
#'   Can be combined with interventions for N×M comparisons.
#' @param discounted Use discounted values?
#' @param show_parameter_values Logical. Include parameter values in labels? (default: TRUE)
#'
#' @return A tibble with parameter_display, strategy, group, low, base, high, range
#' @keywords internal
prepare_dsa_tornado_data <- function(results,
                                     summary_name,
                                     groups,
                                     strategies,
                                     interventions,
                                     comparators,
                                     discounted,
                                     show_parameter_values = TRUE) {

  # Extract DSA summaries for needed strategies
  dsa_data <- extract_dsa_summaries(
    results,
    summary_name = summary_name,
    value_type = "all",
    group = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Separate base case (run_id = 1) from variations
  base_data <- dsa_data %>%
    filter(.data$run_id == 1) %>%
    select("strategy", "group", base = "amount")

  # Separate low and high variations
  low_data <- dsa_data %>%
    filter(.data$variation == "low") %>%
    select("strategy", "group", "parameter", "parameter_display_name", low = "amount")

  high_data <- dsa_data %>%
    filter(.data$variation == "high") %>%
    select("strategy", "group", "parameter", "parameter_display_name", high = "amount")

  # Combine into tornado format
  tornado_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group")) %>%
    mutate(range = pmax(.data$low, .data$base, .data$high) - pmin(.data$low, .data$base, .data$high))

  # Calculate differences if interventions/comparators provided
  differences_created <- FALSE
  strategy_order <- NULL  # Will be set to preserve correct strategy order
  if (!is.null(interventions) || !is.null(comparators)) {
    # Pivot to get strategies as columns
    tornado_wide <- tornado_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = c("low", "base", "high", "range"),
        id_cols = c("group", "parameter", "parameter_display_name")
      )

    # Get all strategies in model definition order (from metadata)
    # This ensures comparisons are created in the correct order
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      # Use model definition order
      all_strategies <- results$metadata$strategies$name
      # Filter to only strategies present in the data
      strategies_in_data <- unique(dsa_data$strategy)
      all_strategies <- all_strategies[all_strategies %in% strategies_in_data]
    } else {
      # Fallback to data order if no metadata
      all_strategies <- unique(dsa_data$strategy)
    }

    # Determine comparison pairs
    comparison_pairs <- list()

    if (!is.null(interventions) && !is.null(comparators)) {
      # Both provided: N×M explicit comparisons
      for (int_strat in interventions) {
        for (comp_strat in comparators) {
          # Skip self-comparisons
          if (int_strat != comp_strat) {
            comparison_pairs[[length(comparison_pairs) + 1]] <- list(
              intervention = int_strat,
              comparator = comp_strat
            )
          }
        }
      }

      if (length(comparison_pairs) == 0) {
        stop("No valid comparisons after excluding self-comparisons")
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
    for (i in seq_along(comparison_pairs)) {
      pair <- comparison_pairs[[i]]
      int_strat <- pair$intervention
      comp_strat <- pair$comparator

      # Create comparison label using display names
      # int_strat and comp_strat are technical names (from interventions/comparators params)
      int_mapped <- map_names(int_strat, results$metadata$strategies, "display_name")
      comp_mapped <- map_names(comp_strat, results$metadata$strategies, "display_name")
      comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

      # Column names with strategy suffixes
      low_int_col <- paste0("low_", int_strat)
      low_comp_col <- paste0("low_", comp_strat)
      base_int_col <- paste0("base_", int_strat)
      base_comp_col <- paste0("base_", comp_strat)
      high_int_col <- paste0("high_", int_strat)
      high_comp_col <- paste0("high_", comp_strat)

      # Calculate differences (intervention - comparator)
      diff_df <- tornado_wide %>%
        mutate(
          strategy = comp_label,
          low = !!sym(low_int_col) - !!sym(low_comp_col),
          base = !!sym(base_int_col) - !!sym(base_comp_col),
          high = !!sym(high_int_col) - !!sym(high_comp_col),
          range = pmax(.data$low, .data$base, .data$high) - pmin(.data$low, .data$base, .data$high)
        ) %>%
        select("group", "parameter", "parameter_display_name", "strategy", "low", "base", "high", "range")

      diff_data[[i]] <- diff_df
    }

    tornado_data <- bind_rows(diff_data)
    differences_created <- TRUE

    # Capture strategy order immediately after bind_rows to preserve comparison order
    # This is the correct order based on model definition
    strategy_order <- unique(tornado_data$strategy)
  }

  # Add parameter values to labels if requested (BEFORE name mapping)
  if (show_parameter_values) {
    # Extract parameter values from segments (using technical names)
    param_values <- extract_parameter_values(results, tornado_data, interventions, comparators)

    # Join with tornado data
    tornado_data <- tornado_data %>%
      left_join(param_values, by = c("parameter", "strategy", "group"))
  }

  # Map display names ONLY if differences were NOT created
  # (When differences are created, comparison labels are already in display format)
  if (!differences_created && !is.null(results$metadata)) {
    # Capture strategy order before name mapping for non-difference case
    if (is.null(strategy_order)) {
      strategy_order <- unique(tornado_data$strategy)
    }
    if (!is.null(results$metadata$strategies)) {
      tornado_data$strategy <- map_names(tornado_data$strategy, results$metadata$strategies, "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      tornado_data$group <- map_names(tornado_data$group, results$metadata$groups, "display_name")
    }
    # Update strategy_order with display names
    if (!is.null(results$metadata$strategies)) {
      strategy_order <- map_names(strategy_order, results$metadata$strategies, "display_name")
    }
  } else if (differences_created && !is.null(results$metadata)) {
    # For differences, only map group names (strategy names are already comparison labels)
    if (!is.null(results$metadata$groups)) {
      tornado_data$group <- map_names(tornado_data$group, results$metadata$groups, "display_name")
    }
  }

  # Create enhanced labels with parameter values (AFTER name mapping)
  if (show_parameter_values) {
    # Get settings for unit lookup
    settings <- results$metadata$settings

    # Build labels with unit suffixes for settings
    tornado_data <- tornado_data %>%
      rowwise() %>%
      mutate(
        parameter_display_name = if_else(
          !is.na(.data$param_low_value) & !is.na(.data$param_high_value),
          {
            # Determine unit suffix based on parameter type
            unit_suffix <- if (!is.na(.data$param_type) && .data$param_type == "setting") {
              get_setting_unit_suffix(.data$parameter, settings)
            } else {
              ""
            }

            # Format values with unit suffix
            if (unit_suffix != "") {
              sprintf("%s (%s%s - %s%s)",
                      .data$parameter_display_name,
                      format_param_value(.data$param_low_value),
                      unit_suffix,
                      format_param_value(.data$param_high_value),
                      unit_suffix)
            } else {
              sprintf("%s (%s - %s)",
                      .data$parameter_display_name,
                      format_param_value(.data$param_low_value),
                      format_param_value(.data$param_high_value))
            }
          },
          .data$parameter_display_name
        )
      ) %>%
      ungroup() %>%
      select(-"param_low_value", -"param_high_value", -"param_type")
  }

  # Attach strategy_order as an attribute to preserve correct ordering
  if (!is.null(strategy_order)) {
    attr(tornado_data, "strategy_order") <- strategy_order
  }

  # Attach group_order as an attribute to preserve correct ordering (overall first, then model order)
  group_order <- get_group_order(unique(tornado_data$group), results$metadata)
  attr(tornado_data, "group_order") <- group_order

  tornado_data
}


#' Plot DSA Outcomes as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter from
#' its low to high value on a specified summary (outcome or cost). Shows horizontal
#' bars representing the range of variation with a vertical line at the base case.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param summary_name Name of the summary to plot (e.g., "total_qalys", "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#'   (all groups plus aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all).
#'   Mutually exclusive with interventions/comparators.
#' @param interventions Character vector of intervention strategy name(s) (e.g., "new_treatment").
#'   Can be a single value or vector. Can be combined with comparators for N×M comparisons.
#' @param comparators Character vector of comparator strategy name(s) (e.g., "control").
#'   Can be a single value or vector. Can be combined with interventions for N×M comparisons.
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels? (default: TRUE)
#'   When TRUE, labels show "Parameter Name (low - high)" format with evaluated parameter values.
#' @param drop_zero_impact Logical. Remove parameters with zero impact on results? (default: TRUE)
#'   A parameter has zero impact when its range (abs(high - low)) is effectively zero.
#'   If all parameters have zero impact, an error is thrown.
#'
#' @return A ggplot2 object
#'
#' @details
#' The tornado plot displays each DSA parameter as a horizontal bar showing the range
#' from low to high value. Parameters are sorted by the magnitude of their impact
#' (largest range first). A vertical line indicates the base case value.
#'
#' **Comparison Modes:**
#'
#' When neither interventions nor comparators is specified, shows absolute outcome values
#' for strategies specified in the `strategies` parameter.
#'
#' When interventions and/or comparators are specified:
#' - Both as single values: shows one comparison (interventions - comparators)
#' - One as vector, other NULL: shows each vs. all others
#'   - interventions=c(A,B), comparators=NULL: A vs. all others, B vs. all others
#'   - comparators=c(C,D), interventions=NULL: all others vs. C, all others vs. D
#' - Both as vectors (N×M mode): shows all pairwise comparisons (excluding self-comparisons)
#'   - interventions=c(A,B), comparators=c(C,D): A vs. C, A vs. D, B vs. C, B vs. D
#'
#' Faceting follows the standard pattern:
#' - Multiple strategies/comparisons only: facet_wrap(~ strategy)
#' - Multiple groups only: facet_wrap(~ group)
#' - Both: facet_wrap(~ strategy + group)
#' - Single: no faceting
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#' dsa_results <- run_dsa(model)
#'
#' # Basic tornado plot
#' dsa_outcomes_plot(dsa_results, "total_qalys")
#'
#' # Show differences vs control
#' dsa_outcomes_plot(dsa_results, "total_qalys", comparators = "control")
#'
#' # Cost summary tornado plot
#' dsa_outcomes_plot(dsa_results, "total_cost", value_type = "cost")
#'
#' # Tornado plot without parameter values in labels
#' dsa_outcomes_plot(dsa_results, "total_qalys", show_parameter_values = FALSE)
#' }
#'
#' @export
dsa_outcomes_plot <- function(results,
                              summary_name,
                              groups = "overall",
                              strategies = NULL,
                              interventions = NULL,
                              comparators = NULL,
                              discounted = FALSE,
                              show_parameter_values = TRUE,
                              drop_zero_impact = TRUE) {

  # Validate that strategies is mutually exclusive with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'. Use interventions/comparators vectors to specify exact comparisons.")
  }

  # Prepare tornado data
  tornado_data <- prepare_dsa_tornado_data(
    results = results,
    summary_name = summary_name,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = show_parameter_values
  )

  # Check if data is valid
  if (is.null(tornado_data) || nrow(tornado_data) == 0) {
    stop("No data available for tornado plot with specified parameters")
  }

  # Filter out zero-impact parameters if requested
  if (drop_zero_impact) {
    n_params_before <- nrow(tornado_data)
    tornado_data <- tornado_data %>%
      filter(abs(.data$range) > .Machine$double.eps * 100) %>%
      droplevels()  # Drop unused factor levels

    if (nrow(tornado_data) == 0) {
      stop(sprintf("All %d parameters have zero impact on results. No data to plot.",
                   n_params_before))
    }
  }

  # Factorize for proper ordering
  # Use strategy_order attribute if available to preserve model definition order
  strategy_levels <- attr(tornado_data, "strategy_order")
  if (is.null(strategy_levels)) {
    strategy_levels <- unique(tornado_data$strategy)
  }

  # Use group_order attribute if available (overall first, then model order)
  group_levels <- attr(tornado_data, "group_order")
  if (is.null(group_levels)) {
    group_levels <- unique(tornado_data$group)
  }

  tornado_data <- tornado_data %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels),
      group = factor(.data$group, levels = group_levels)
    )

  # Map summary name for axis label
  summary_label <- summary_name
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    summary_label <- map_names(summary_name, results$metadata$summaries, "display_name")
  }

  # Add prefix if showing differences
  if (!is.null(interventions) || !is.null(comparators)) {
    summary_label <- paste0("Difference in ", summary_label)
  }

  # Render tornado plot
  render_tornado_plot(tornado_data, summary_label)
}


#' Plot DSA Net Monetary Benefit as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter on
#' Net Monetary Benefit (NMB). NMB = (Difference in Outcomes × WTP) - Difference in Costs.
#' Shows horizontal bars representing the range of NMB variation with a vertical line at
#' the base case.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of the health outcome summary to use (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#'   (all groups plus aggregated)
#' @param wtp Optional override for willingness-to-pay. If NULL, extracts from outcome summary metadata.
#' @param interventions Character vector of intervention strategy name(s) (e.g., "new_treatment").
#'   Can be a single value or vector. Can be combined with comparators for N×M comparisons.
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s) (e.g., "control").
#'   Can be a single value or vector. Can be combined with interventions for N×M comparisons.
#'   At least one of interventions or comparators must be specified.
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels? (default: TRUE)
#'   When TRUE, labels show "Parameter Name (low - high)" format with evaluated parameter values.
#' @param drop_zero_impact Logical. Remove parameters with zero impact on NMB? (default: TRUE)
#'   A parameter has zero impact when its NMB range is effectively zero.
#'
#' @return A ggplot2 object
#'
#' @details
#' The NMB tornado plot displays each DSA parameter as a horizontal bar showing the range
#' of Net Monetary Benefit from low to high parameter value. Parameters are sorted by the
#' magnitude of their impact (largest range first). A vertical line indicates the base case NMB.
#'
#' **Comparison Requirements:**
#'
#' Unlike dsa_outcomes_plot, NMB always requires comparing strategies. At least one of
#' interventions or comparators must be specified.
#'
#' **Comparison Modes:**
#'
#' - Both as single values: shows one comparison (interventions - comparators)
#' - One as vector, other NULL: shows each vs. all others
#'   - interventions=c(A,B), comparators=NULL: A vs. all others, B vs. all others
#'   - comparators=c(C,D), interventions=NULL: all others vs. C, all others vs. D
#' - Both as vectors (N×M mode): shows all pairwise comparisons (excluding self-comparisons)
#'   - interventions=c(A,B), comparators=c(C,D): A vs. C, A vs. D, B vs. C, B vs. D
#'
#' Faceting follows the standard pattern:
#' - Multiple comparisons only: facet_wrap(~ strategy)
#' - Multiple groups only: facet_wrap(~ group)
#' - Both: facet_wrap(~ strategy + group)
#' - Single: no faceting
#'
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#' dsa_results <- run_dsa(model)
#'
#' # NMB tornado plot (comparator perspective)
#' dsa_nmb_plot(dsa_results, "total_qalys", "total_cost", comparators = "control")
#'
#' # NMB with explicit WTP (intervention perspective)
#' dsa_nmb_plot(dsa_results, "total_qalys", "total_cost",
#'              interventions = "new_treatment", wtp = 50000)
#'
#' # N×M comparisons
#' dsa_nmb_plot(dsa_results, "total_qalys", "total_cost",
#'              interventions = c("treatment_a", "treatment_b"),
#'              comparators = c("control", "standard_care"))
#' }
#'
#' @export
dsa_nmb_plot <- function(results,
                         health_outcome,
                         cost_outcome,
                         groups = "overall",
                         wtp = NULL,
                         interventions = NULL,
                         comparators = NULL,
                         show_parameter_values = TRUE,
                         drop_zero_impact = TRUE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation")
  }

  # Get WTP if needed
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

  # Prepare tornado data for outcomes
  # Always use discounted values for NMB (cost-effectiveness measure)
  outcome_tornado <- prepare_dsa_tornado_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE  # We'll add parameter values to combined data later
  )

  # Capture strategy and group order from outcome tornado data
  strategy_order_nmb <- attr(outcome_tornado, "strategy_order")
  group_order_nmb <- attr(outcome_tornado, "group_order")

  # Multiply by WTP
  outcome_tornado <- outcome_tornado %>%
    mutate(
      low = .data$low * wtp,
      base = .data$base * wtp,
      high = .data$high * wtp,
      range = abs(.data$high - .data$low)
    )

  # Prepare tornado data for costs
  # Always use discounted values for NMB (cost-effectiveness measure)
  cost_tornado <- prepare_dsa_tornado_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  )

  # Negate costs (we subtract them)
  cost_tornado <- cost_tornado %>%
    mutate(
      low = -.data$low,
      base = -.data$base,
      high = -.data$high,
      range = abs(.data$high - .data$low)
    )

  # Combine outcomes and costs to get NMB
  # Use full_join to include both cost-only and outcome-only parameters
  # Cost-only parameters (e.g., med_cost) have zero range in outcomes
  # Outcome-only parameters have zero range in costs
  nmb_tornado <- outcome_tornado %>%
    full_join(
      cost_tornado,
      by = c("strategy", "group", "parameter", "parameter_display_name"),
      suffix = c("_outcome", "_cost")
    ) %>%
    mutate(
      # Replace NA with 0 for missing outcome or cost components
      low_outcome = replace_na(.data$low_outcome, 0),
      base_outcome = replace_na(.data$base_outcome, 0),
      high_outcome = replace_na(.data$high_outcome, 0),
      low_cost = replace_na(.data$low_cost, 0),
      base_cost = replace_na(.data$base_cost, 0),
      high_cost = replace_na(.data$high_cost, 0),
      # Calculate NMB
      low = .data$low_outcome + .data$low_cost,
      base = .data$base_outcome + .data$base_cost,
      high = .data$high_outcome + .data$high_cost,
      range = pmax(.data$low, .data$base, .data$high) - pmin(.data$low, .data$base, .data$high)
    ) %>%
    select("strategy", "group", "parameter", "parameter_display_name", "low", "base", "high", "range")

  # Add parameter values to labels if requested
  if (show_parameter_values) {
    param_values <- extract_parameter_values(results, nmb_tornado, interventions, comparators)

    # Get settings for unit lookup
    settings <- results$metadata$settings

    nmb_tornado <- nmb_tornado %>%
      left_join(param_values, by = c("parameter", "strategy", "group")) %>%
      rowwise() %>%
      mutate(
        parameter_display_name = if_else(
          !is.na(.data$param_low_value) & !is.na(.data$param_high_value),
          {
            # Determine unit suffix based on parameter type
            unit_suffix <- if (!is.na(.data$param_type) && .data$param_type == "setting") {
              get_setting_unit_suffix(.data$parameter, settings)
            } else {
              ""
            }

            # Format values with unit suffix
            if (unit_suffix != "") {
              sprintf("%s (%s%s - %s%s)",
                      .data$parameter_display_name,
                      format_param_value(.data$param_low_value),
                      unit_suffix,
                      format_param_value(.data$param_high_value),
                      unit_suffix)
            } else {
              sprintf("%s (%s - %s)",
                      .data$parameter_display_name,
                      format_param_value(.data$param_low_value),
                      format_param_value(.data$param_high_value))
            }
          },
          .data$parameter_display_name
        )
      ) %>%
      ungroup() %>%
      select(-"param_low_value", -"param_high_value", -"param_type")
  }

  # Check if data is valid
  if (is.null(nmb_tornado) || nrow(nmb_tornado) == 0) {
    stop("No data available for NMB tornado plot with specified parameters")
  }

  # Filter out zero-impact parameters if requested
  if (drop_zero_impact) {
    n_params_before <- nrow(nmb_tornado)
    nmb_tornado <- nmb_tornado %>%
      filter(abs(.data$range) > .Machine$double.eps * 100)

    if (nrow(nmb_tornado) == 0) {
      stop(sprintf("All %d parameters have zero impact on NMB. No data to plot.",
                   n_params_before))
    }
  }

  # Factorize for proper ordering
  # Use strategy_order captured from prepare_dsa_tornado_data to preserve model definition order
  strategy_levels_nmb <- strategy_order_nmb
  if (is.null(strategy_levels_nmb)) {
    strategy_levels_nmb <- unique(nmb_tornado$strategy)
  }

  # Use group_order captured from prepare_dsa_tornado_data (overall first, then model order)
  group_levels_nmb <- group_order_nmb
  if (is.null(group_levels_nmb)) {
    group_levels_nmb <- unique(nmb_tornado$group)
  }

  nmb_tornado <- nmb_tornado %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels_nmb),
      group = factor(.data$group, levels = group_levels_nmb)
    )

  # Create NMB label with display names
  outcome_label <- health_outcome
  cost_label <- cost_outcome
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    outcome_label <- map_names(health_outcome, results$metadata$summaries, "display_name")
    cost_label <- map_names(cost_outcome, results$metadata$summaries, "display_name")
  }

  wtp_formatted <- format(wtp, big.mark = ",")
  nmb_label <- glue("Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})")

  # Render tornado plot
  render_tornado_plot(nmb_tornado, nmb_label)
}


# ============================================================================
# DSA Cost-Effectiveness Plot Functions
# ============================================================================


#' Classify Base Case ICER Type
#'
#' Internal helper to classify the type of base case ICER for per-facet handling.
#'
#' @param icer_value Numeric ICER value (may be Inf, -Inf, NaN, NA, or finite)
#' @return Character classification: "normal", "dominated", "dominant", "flipped", "identical", "reference"
#' @keywords internal
classify_base_case <- function(icer_value) {
  if (length(icer_value) == 0) {
    return("reference")
  }
  # Check NaN BEFORE NA because is.na(NaN) returns TRUE
  if (is.nan(icer_value)) {
    return("identical")           # NaN = identical costs and outcomes
  }
  if (is.na(icer_value)) {
    return("reference")           # NA = reference strategy
  }
  if (is.infinite(icer_value) && icer_value > 0) {
    return("dominated")           # +Inf = dominated
  }
  if (icer_value == 0) {
    return("dominant")            # 0 = dominant
  }
  if (icer_value > 0) {
    return("normal")              # +number = normal ICER
  }
  if (icer_value < 0) {
    return("flipped")             # -number = flipped comparison
  }
  "reference"
}


#' Detect Variation Error State
#'
#' Internal helper to detect error states for a parameter variation.
#'
#' @param base_class Character base case classification
#' @param variation_icer Numeric ICER for the variation
#' @return List with type, show_bar, bar_type, label components
#' @keywords internal
detect_variation_error <- function(base_class, variation_icer) {
  # Identical base case = full chart error (handled at facet level)
  if (base_class == "identical") {
    return(list(type = "full_chart_error", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
  }

  # NaN variation = identical outcomes error
  if (is.nan(variation_icer)) {
    return(list(type = "identical", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
  }

  # Direction change detection
  base_is_positive <- base_class %in% c("normal", "dominated")
  variation_is_negative <- is.finite(variation_icer) && variation_icer < 0

  base_is_negative <- base_class == "flipped"
  variation_is_positive <- is.finite(variation_icer) && variation_icer > 0

  if ((base_is_positive && variation_is_negative) ||
      (base_is_negative && variation_is_positive)) {
    return(list(type = "direction_change", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
  }

  # Dominated variation (+Inf)
  if (is.infinite(variation_icer) && variation_icer > 0) {
    # If base is also dominated, no bar (same as base)
    if (base_class == "dominated") {
      return(list(type = NULL, show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
    }
    return(list(type = NULL, show_bar = TRUE, bar_type = "arrow", label = "Dominated"))
  }

  # Dominant variation (0)
  if (variation_icer == 0) {
    # If base is also dominant, no bar (same as base)
    if (base_class == "dominant") {
      return(list(type = NULL, show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
    }
    return(list(type = NULL, show_bar = TRUE, bar_type = "to_zero", label = "Dominant"))
  }

  # Normal displayable variation
  return(list(type = NULL, show_bar = TRUE, bar_type = "standard", label = NA_character_))
}


#' Generate Error Message for DSA CE Variation
#'
#' Internal helper to generate error tooltip messages.
#'
#' @param error_type Character error type: "direction_change" or "identical"
#' @param variation Character "Low" or "High"
#' @param icer_value Numeric ICER value (for direction change message)
#' @param intervention_name Character display name of intervention
#' @param comparator_name Character display name of comparator
#' @return Character error message or NA_character_
#' @keywords internal
generate_ce_error_message <- function(error_type, variation, icer_value,
                                      intervention_name, comparator_name) {
  if (is.null(error_type) || is.na(error_type)) {
    return(NA_character_)
  }

  if (error_type == "direction_change") {
    sprintf(
      "%s value of parameter changes the directionality of ICER and cannot be displayed. ICER of %s reflects comparison of %s vs. %s",
      variation,
      format(abs(icer_value), big.mark = ",", nsmall = 0, scientific = FALSE),
      comparator_name,
      intervention_name
    )
  } else if (error_type == "identical") {
    sprintf(
      "%s value of parameter results in identical outcomes and costs for %s and %s, resulting in undefined ICER",
      variation,
      intervention_name,
      comparator_name
    )
  } else {
    NA_character_
  }
}


#' Generate Footnote for Flipped Base Case
#'
#' Internal helper to generate footnote text for flipped facets.
#'
#' @param comparator_name Character display name of comparator
#' @param intervention_name Character display name of intervention
#' @return Character footnote text
#' @keywords internal
generate_ce_footnote <- function(comparator_name, intervention_name) {
  sprintf(
    "* %s is more costly & more effective than %s. ICER represents cost-effectiveness of %s vs. %s.",
    comparator_name,
    intervention_name,
    comparator_name,
    intervention_name
  )
}


#' Format ICER Value for Display
#'
#' Internal helper to format ICER values for tornado plot labels.
#'
#' @param icer_value Numeric ICER value
#' @param is_flipped Logical whether facet has flipped base case
#' @param decimals Number of decimal places (default: 0)
#' @return Formatted string
#' @keywords internal
format_icer_label <- function(icer_value, is_flipped = FALSE, decimals = 0) {
  # Check NaN BEFORE NA because is.na(NaN) returns TRUE
  if (is.nan(icer_value)) {
    return("Equivalent")
  }
  if (is.na(icer_value)) {
    return("")
  }
  if (is.infinite(icer_value) && icer_value > 0) {
    return("Dominated")
  }
  if (icer_value == 0) {
    return("Dominant")
  }

  # Format finite value
  formatted <- prettyNum(round(abs(icer_value), decimals),
                         big.mark = ",",
                         scientific = FALSE)

  # Add asterisk if flipped facet
  if (is_flipped) {
    formatted <- paste0(formatted, "*")
  }

  formatted
}


#' Calculate Displayable Range for Parameter Ordering
#'
#' Internal helper to calculate range for parameter ordering, handling special ICER values.
#'
#' @param low_icer Numeric low ICER value
#' @param base_icer Numeric base ICER value
#' @param high_icer Numeric high ICER value
#' @param low_show_bar Logical whether low bar is shown
#' @param high_show_bar Logical whether high bar is shown
#' @param base_class Character base case classification
#' @return Numeric displayable range
#' @keywords internal
calculate_displayable_range <- function(low_icer, base_icer, high_icer,
                                        low_show_bar, high_show_bar,
                                        base_class) {
  # If base is identical (NaN), range is undefined
  if (base_class == "identical") {
    return(NA_real_)
  }

  # Collect displayable values
  displayable_values <- numeric(0)

  # Base display value
  if (base_class == "dominated") {
    # For ordering purposes, treat dominated as very large but finite
    displayable_values <- c(displayable_values, 1e12)
  } else if (base_class == "dominant") {
    displayable_values <- c(displayable_values, 0)
  } else if (base_class %in% c("normal", "flipped")) {
    displayable_values <- c(displayable_values, abs(base_icer))
  }

  # Low variation
  if (low_show_bar && !is.na(low_icer) && !is.nan(low_icer)) {
    if (is.infinite(low_icer)) {
      displayable_values <- c(displayable_values, 1e12)
    } else if (low_icer == 0) {
      displayable_values <- c(displayable_values, 0)
    } else {
      displayable_values <- c(displayable_values, abs(low_icer))
    }
  }

  # High variation (same logic)
  if (high_show_bar && !is.na(high_icer) && !is.nan(high_icer)) {
    if (is.infinite(high_icer)) {
      displayable_values <- c(displayable_values, 1e12)
    } else if (high_icer == 0) {
      displayable_values <- c(displayable_values, 0)
    } else {
      displayable_values <- c(displayable_values, abs(high_icer))
    }
  }

  # Calculate range
  if (length(displayable_values) == 0) {
    return(0)
  }

  max(displayable_values) - min(displayable_values)
}


#' Prepare DSA Cost-Effectiveness Tornado Data
#'
#' Internal helper to prepare DSA cost-effectiveness data for tornado plot rendering.
#' Calculates ICERs for each parameter variation and classifies error states.
#'
#' @param results DSA results object from run_dsa()
#' @param health_outcome Name of health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of cost summary (e.g., "total_cost")
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param show_parameter_values Logical. Include parameter values in labels?
#'
#' @return A list with tornado_data tibble and facet_metadata tibble
#' @keywords internal
prepare_dsa_ce_tornado_data <- function(results,
                                        health_outcome,
                                        cost_outcome,
                                        groups,
                                        interventions,
                                        comparators,
                                        show_parameter_values = TRUE) {

  # Extract cost summaries (always discounted for CE)
  cost_data <- extract_dsa_summaries(
    results,
    summary_name = cost_outcome,
    value_type = "all",
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Extract outcome summaries (always discounted for CE)
  outcome_data <- extract_dsa_summaries(
    results,
    summary_name = health_outcome,
    value_type = "all",
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Combine cost and outcome data
  combined_data <- cost_data %>%
    inner_join(
      outcome_data %>% select("run_id", "strategy", "group", outcome = "amount"),
      by = c("run_id", "strategy", "group"),
      suffix = c("", "_outcome")
    ) %>%
    rename(cost = amount)

  # Get all strategies in the data
  all_strategies <- unique(combined_data$strategy)

  # Validate interventions/comparators
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for CE plot")
  }

  # Determine comparison pairs (same logic as prepare_dsa_tornado_data)
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

  if (length(comparison_pairs) == 0) {
    stop("No valid comparisons after excluding self-comparisons")
  }

  # Calculate ICERs for each comparison pair and run
  all_icer_data <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    # Get display names for comparison label
    int_mapped <- map_names(int_strat, results$metadata$strategies, "display_name")
    comp_mapped <- map_names(comp_strat, results$metadata$strategies, "display_name")
    comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

    # Get data for intervention and comparator
    int_data <- combined_data %>%
      filter(.data$strategy == int_strat) %>%
      select("run_id", "group", "parameter", "parameter_display_name", "variation",
             cost_int = "cost", outcome_int = "outcome")

    comp_data <- combined_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("run_id", "group", cost_comp = "cost", outcome_comp = "outcome")

    # Join and calculate deltas
    icer_data <- int_data %>%
      inner_join(comp_data, by = c("run_id", "group")) %>%
      mutate(
        strategy = comp_label,
        intervention_name = int_mapped,
        comparator_name = comp_mapped,
        dcost = .data$cost_int - .data$cost_comp,
        doutcome = .data$outcome_int - .data$outcome_comp,
        icer_value = icer(.data$dcost, .data$doutcome)
      ) %>%
      select("run_id", "group", "parameter", "parameter_display_name", "variation",
             "strategy", "intervention_name", "comparator_name", "icer_value")

    all_icer_data[[length(all_icer_data) + 1]] <- icer_data
  }

  combined_icer <- bind_rows(all_icer_data)

  # Map group names
  if (!is.null(results$metadata) && !is.null(results$metadata$groups)) {
    combined_icer$group <- map_names(combined_icer$group, results$metadata$groups, "display_name")
  }

  # Separate base case from variations
  # Note: combined_icer has one row per parameter for the base case, but all rows
  # have identical base_icer values (same base case run). We use distinct() to get
  # exactly one row per (strategy, group) to avoid Cartesian product in the join.
  base_data <- combined_icer %>%
    filter(.data$variation == "base") %>%
    distinct(.data$strategy, .data$group, .keep_all = TRUE) %>%
    select("strategy", "group", "intervention_name", "comparator_name",
           base_icer = "icer_value")

  low_data <- combined_icer %>%
    filter(.data$variation == "low") %>%
    select("strategy", "group", "parameter", "parameter_display_name",
           low_icer = "icer_value")

  high_data <- combined_icer %>%
    filter(.data$variation == "high") %>%
    select("strategy", "group", "parameter", "parameter_display_name",
           high_icer = "icer_value")

  # Combine into tornado format
  tornado_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group")) %>%
    # Convert icer class to numeric to avoid class mixing issues in bind_rows
    mutate(
      low_icer = as.numeric(.data$low_icer),
      high_icer = as.numeric(.data$high_icer),
      base_icer = as.numeric(.data$base_icer)
    )

  # Classify base case and detect error states for each row
  # Process each row to extract error info and compute derived columns
  processed_rows <- lapply(seq_len(nrow(tornado_data)), function(i) {
    row <- tornado_data[i, ]

    base_class <- classify_base_case(row$base_icer)
    is_flipped_facet <- (base_class == "flipped")

    # Low variation error detection
    low_error_info <- detect_variation_error(base_class, row$low_icer)
    low_error_type <- low_error_info$type
    low_show_bar <- low_error_info$show_bar
    low_bar_type <- low_error_info$bar_type
    low_label_auto <- low_error_info$label

    # High variation error detection
    high_error_info <- detect_variation_error(base_class, row$high_icer)
    high_error_type <- high_error_info$type
    high_show_bar <- high_error_info$show_bar
    high_bar_type <- high_error_info$bar_type
    high_label_auto <- high_error_info$label

    # Generate labels
    low_label <- if (is.na(low_label_auto)) {
      format_icer_label(row$low_icer, is_flipped_facet)
    } else {
      low_label_auto
    }
    high_label <- if (is.na(high_label_auto)) {
      format_icer_label(row$high_icer, is_flipped_facet)
    } else {
      high_label_auto
    }

    # Generate error messages
    low_error_message <- generate_ce_error_message(
      low_error_type, "Low", row$low_icer,
      row$intervention_name, row$comparator_name
    )
    high_error_message <- generate_ce_error_message(
      high_error_type, "High", row$high_icer,
      row$intervention_name, row$comparator_name
    )

    # Combined error state
    has_error <- !is.null(low_error_type) || !is.null(high_error_type)
    combined_error_message <- if (has_error) {
      msgs <- c(low_error_message, high_error_message)
      msgs <- msgs[!is.na(msgs)]
      if (length(msgs) > 0) paste(msgs, collapse = "\n\n") else NA_character_
    } else {
      NA_character_
    }

    # Calculate displayable range
    displayable_range <- calculate_displayable_range(
      row$low_icer, row$base_icer, row$high_icer,
      low_show_bar, high_show_bar, base_class
    )

    # Display values (absolute for positioning)
    display_low <- if (is.finite(row$low_icer)) abs(row$low_icer) else NA_real_
    display_base <- if (is.finite(row$base_icer)) {
      abs(row$base_icer)
    } else if (base_class == "dominant") {
      0
    } else {
      NA_real_
    }
    display_high <- if (is.finite(row$high_icer)) abs(row$high_icer) else NA_real_

    tibble(
      base_class = base_class,
      is_flipped_facet = is_flipped_facet,
      low_error_type = if (is.null(low_error_type)) NA_character_ else low_error_type,
      low_show_bar = low_show_bar,
      low_bar_type = if (is.na(low_bar_type)) NA_character_ else low_bar_type,
      high_error_type = if (is.null(high_error_type)) NA_character_ else high_error_type,
      high_show_bar = high_show_bar,
      high_bar_type = if (is.na(high_bar_type)) NA_character_ else high_bar_type,
      low_label = low_label,
      high_label = high_label,
      low_error_message = low_error_message,
      high_error_message = high_error_message,
      has_error = has_error,
      combined_error_message = combined_error_message,
      displayable_range = displayable_range,
      display_low = display_low,
      display_base = display_base,
      display_high = display_high
    )
  })

  # Combine processed data with original data
  processed_df <- bind_rows(processed_rows)
  tornado_data <- bind_cols(tornado_data, processed_df)

  # Add parameter values if requested
  if (show_parameter_values) {
    param_values <- extract_parameter_values(results, tornado_data, interventions, comparators)

    if (nrow(param_values) > 0) {
      # Get settings for unit lookup
      settings <- results$metadata$settings

      tornado_data <- tornado_data %>%
        left_join(param_values, by = c("parameter", "strategy", "group")) %>%
        rowwise() %>%
        mutate(
          parameter_display_name = if_else(
            !is.na(.data$param_low_value) & !is.na(.data$param_high_value),
            {
              unit_suffix <- if (!is.na(.data$param_type) && .data$param_type == "setting") {
                get_setting_unit_suffix(.data$parameter, settings)
              } else {
                ""
              }

              if (unit_suffix != "") {
                sprintf("%s (%s%s - %s%s)",
                        .data$parameter_display_name,
                        format_param_value(.data$param_low_value),
                        unit_suffix,
                        format_param_value(.data$param_high_value),
                        unit_suffix)
              } else {
                sprintf("%s (%s - %s)",
                        .data$parameter_display_name,
                        format_param_value(.data$param_low_value),
                        format_param_value(.data$param_high_value))
              }
            },
            .data$parameter_display_name
          )
        ) %>%
        ungroup()

      # Remove param value columns if they exist
      if ("param_low_value" %in% names(tornado_data)) {
        tornado_data <- tornado_data %>%
          select(-any_of(c("param_low_value", "param_high_value", "param_type")))
      }
    }
  }

  # Create facet metadata
  facet_metadata <- tornado_data %>%
    distinct(.data$strategy, .data$group, .data$intervention_name, .data$comparator_name,
             .data$base_icer, .data$base_class, .data$is_flipped_facet) %>%
    rowwise() %>%
    mutate(
      has_full_chart_error = (.data$base_class == "identical"),
      full_chart_error_msg = if_else(
        .data$has_full_chart_error,
        sprintf(
          "Tornado plot for %s vs. %s cannot be displayed because the\ndifference in outcomes and costs in the base case is zero,\nresulting in an undefined ICER.",
          .data$intervention_name, .data$comparator_name
        ),
        NA_character_
      ),
      footnote_text = if_else(
        .data$is_flipped_facet,
        generate_ce_footnote(.data$comparator_name, .data$intervention_name),
        NA_character_
      ),
      base_label = case_when(
        .data$base_class == "dominated" ~ "Base Case: Dominated",
        .data$base_class == "dominant" ~ "Base Case: Dominant",
        .data$base_class == "flipped" ~ paste0("Base Case: ", format_icer_label(.data$base_icer, TRUE)),
        .data$base_class == "normal" ~ paste0("Base Case: ", format_icer_label(.data$base_icer, FALSE)),
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()

  # Attach ordering attributes
  strategy_order <- unique(tornado_data$strategy)
  group_order <- get_group_order(unique(tornado_data$group), results$metadata)
  attr(tornado_data, "strategy_order") <- strategy_order
  attr(tornado_data, "group_order") <- group_order

  list(
    tornado_data = tornado_data,
    facet_metadata = facet_metadata
  )
}


#' Render DSA Cost-Effectiveness Tornado Plot
#'
#' Internal helper to create CE tornado plot visualization from prepared data.
#' Handles ICER-specific requirements including arrow bars, error annotations,
#' and asterisk labels.
#'
#' @param tornado_data Prepared tornado data tibble
#' @param facet_metadata Facet metadata tibble
#' @param dominated_position Optional X position for dominated base case line
#'
#' @return A ggplot2 object
#' @keywords internal
render_dsa_ce_tornado_plot <- function(tornado_data, facet_metadata, dominated_position = NULL) {

  # Identify identical base case facets (will show error message instead of bars)
  valid_facets <- facet_metadata %>%
    filter(.data$base_class != "identical")

  identical_facets <- facet_metadata %>%
    filter(.data$base_class == "identical")

  # Filter tornado data to valid facets only (identical facets get placeholder rows)
  if (nrow(valid_facets) > 0) {
    tornado_data <- tornado_data %>%
      semi_join(valid_facets, by = c("strategy", "group"))
  } else {
    # All facets are identical - no valid bar data
    tornado_data <- tornado_data %>%
      filter(FALSE)  # Empty the data frame
  }

  # If all facets are identical and we have no valid data, return empty plot with error
  if (nrow(tornado_data) == 0 && nrow(identical_facets) == 0) {
    stop("No valid data for tornado plot")
  }

  # Determine faceting (use facet_metadata to include identical facets)
  n_groups <- length(unique(facet_metadata$group))
  n_strategies <- length(unique(facet_metadata$strategy))

  # Calculate number of facets and optimal column count
  # Fill at least 3 rows before adding a column
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
    facet_component <- facet_wrap(vars(.data$strategy, .data$group), scales = "free_y", ncol = n_groups)
  } else if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group), scales = "free_y", ncol = ncol)
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy), scales = "free_y", ncol = ncol)
  }

  # Create facet grouping variable
  if (nrow(tornado_data) > 0) {
    if (n_groups > 1 && n_strategies > 1) {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = interaction(.data$strategy, .data$group, drop = TRUE))
    } else if (n_groups > 1) {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = .data$group)
    } else if (n_strategies > 1) {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = .data$strategy)
    } else {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = factor("all"))
    }
  } else {
    # Add empty reorder_group column for consistency when tornado_data is empty
    tornado_data <- tornado_data %>%
      mutate(reorder_group = factor())
  }

  # Calculate X-axis range
  finite_icers <- c(
    tornado_data$display_low[is.finite(tornado_data$display_low)],
    tornado_data$display_base[is.finite(tornado_data$display_base)],
    tornado_data$display_high[is.finite(tornado_data$display_high)]
  )

  if (length(finite_icers) == 0) {
    x_data_max <- 100000  # Default for all-dominated scenarios
  } else {
    x_data_max <- max(finite_icers, na.rm = TRUE)
  }

  # Calculate X-axis breaks and limits
  breaks_fn <- pretty_breaks(n = 5)
  x_breaks <- breaks_fn(c(0, x_data_max))
  x_breaks <- x_breaks[x_breaks >= 0]  # No negative ticks
  x_tick_max <- max(x_breaks)

  # Position for dominated base case / arrow bars (15% beyond last tick)
  if (is.null(dominated_position)) {
    dominated_position <- x_tick_max * 1.15
  }
  # Calculate left margin needed for left-side labels
 # Labels positioned at xmin - x_tick_max * 0.02 need space to not be clipped
  left_label_margin <- x_tick_max * 0.08
  x_limits <- c(-left_label_margin, dominated_position)

  # Calculate base case display position for each facet
  facet_metadata <- facet_metadata %>%
    mutate(
      base_display_value = case_when(
        .data$base_class == "dominated" ~ dominated_position,
        .data$base_class == "dominant" ~ 0,
        .data$base_class == "identical" ~ NA_real_,
        TRUE ~ abs(as.numeric(.data$base_icer))
      )
    )

  # Join facet metadata with tornado data
  tornado_data <- tornado_data %>%
    left_join(
      facet_metadata %>% select("strategy", "group", "base_display_value"),
      by = c("strategy", "group")
    )

  # Detect same-side parameters
  # Note: For dominated/dominant variations, display_low or display_high may be NA.

  # These can never be "same-side" since one bar goes to infinity/zero.
  # Use coalesce to treat NA as FALSE in the comparison.
  tornado_data <- tornado_data %>%
    mutate(
      same_side = (.data$low_show_bar & .data$high_show_bar) &
        dplyr::coalesce(
          (.data$display_low > .data$base_display_value & .data$display_high > .data$base_display_value) |
            (.data$display_low < .data$base_display_value & .data$display_high < .data$base_display_value),
          FALSE
        )
    )

  # Reshape to long format for bars
  tornado_long <- tornado_data %>%
    pivot_longer(
      cols = c("low_icer", "high_icer"),
      names_to = "variation_raw",
      values_to = "icer_value"
    ) %>%
    mutate(
      # Convert icer_value to numeric to avoid class mixing issues
      icer_value_num = as.numeric(.data$icer_value),
      variation = if_else(.data$variation_raw == "low_icer", "Low", "High"),
      show_bar = if_else(.data$variation == "Low", .data$low_show_bar, .data$high_show_bar),
      bar_type = dplyr::case_when(
        .data$variation == "Low" ~ .data$low_bar_type,
        TRUE ~ .data$high_bar_type
      ),
      label = if_else(.data$variation == "Low", .data$low_label, .data$high_label),
      error_type = if_else(.data$variation == "Low", .data$low_error_type, .data$high_error_type),
      display_value = if_else(
        is.finite(.data$icer_value_num),
        abs(.data$icer_value_num),
        if_else(.data$bar_type == "arrow", dominated_position, NA_real_)
      )
    )

  # Calculate y positions
  # Use parameter_display_name as tiebreaker when displayable_range values are tied
  # Y spacing factor to prevent arrow bars from touching
  # Total arrow height is bar_height + 2*arrow_notch_y = 0.8 + 0.3 = 1.1
  # With y_spacing = 1.2, gaps will be 1.2 - 1.1 = 0.1 (about 10% of bar height)
  y_spacing <- 1.2

  tornado_long <- tornado_long %>%
    group_by(.data$reorder_group) %>%
    mutate(
      y_base = dense_rank(desc(interaction(.data$displayable_range, .data$parameter_display_name, lex.order = TRUE))) * y_spacing
    ) %>%
    ungroup()

  # Handle same-side bars
  tornado_long <- tornado_long %>%
    mutate(
      bar_height = if_else(.data$same_side, 0.4, 0.8),
      y_offset = case_when(
        .data$same_side & .data$variation == "High" ~ -0.2,
        .data$same_side & .data$variation == "Low" ~ 0.2,
        TRUE ~ 0
      ),
      y_numeric = .data$y_base + .data$y_offset
    )

  # Calculate bar positions
  tornado_long <- tornado_long %>%
    mutate(
      xmin = case_when(
        .data$bar_type == "standard" ~ pmin(.data$base_display_value, .data$display_value),
        .data$bar_type == "to_zero" ~ 0,
        .data$bar_type == "arrow" ~ .data$base_display_value,
        TRUE ~ NA_real_
      ),
      xmax = case_when(
        .data$bar_type == "standard" ~ pmax(.data$base_display_value, .data$display_value),
        .data$bar_type == "to_zero" ~ .data$base_display_value,
        .data$bar_type == "arrow" ~ dominated_position,
        TRUE ~ NA_real_
      ),
      x_center = (.data$xmin + .data$xmax) / 2,
      x_width = .data$xmax - .data$xmin
    )

  # Create y-axis labels lookup
  y_labels <- tornado_long %>%
    distinct(.data$reorder_group, .data$parameter_display_name, .data$y_base) %>%
    arrange(.data$reorder_group, .data$y_base)

  max_y <- max(tornado_long$y_base, na.rm = TRUE)

  # Safety check: if max_y is not finite, default to 1
  # This can happen if tornado_long is empty after filtering
  if (!is.finite(max_y) || max_y <= 0) {
    max_y <- 1
  }

  # Filter to displayable bars only
  standard_bars <- tornado_long %>%
    filter(.data$show_bar, .data$bar_type == "standard")

  to_zero_bars <- tornado_long %>%
    filter(.data$show_bar, .data$bar_type == "to_zero")

  arrow_bars <- tornado_long %>%
    filter(.data$show_bar, .data$bar_type == "arrow")

  # Create base case line data (exclude dominated - no line for dominated base case)
  base_case_lines <- facet_metadata %>%
    filter(.data$base_class != "identical", .data$base_class != "dominated") %>%
    select("strategy", "group", "base_display_value")

  # Create base case label data
  base_case_labels <- facet_metadata %>%
    filter(.data$base_class != "identical") %>%
    select("strategy", "group", "base_display_value", "base_label", "base_class") %>%
    mutate(
      y_pos = 0.5 * y_spacing,  # Above top bar
      # Left-justify when base case is in the left 15% of the x-axis to prevent cutoff
      near_left_edge = .data$base_display_value < dominated_position * 0.15,
      label_hjust = case_when(
        .data$base_class == "dominated" ~ 1,
        .data$near_left_edge ~ 0,
        TRUE ~ 0.5
      ),
      label_x = case_when(
        .data$base_class == "dominated" ~ .data$base_display_value - (dominated_position * 0.02),
        .data$near_left_edge ~ .data$base_display_value,
        TRUE ~ .data$base_display_value
      )
    )

  # Create error message data for identical facets
  # These facets will show only an error message, no bars
  if (nrow(identical_facets) > 0) {
    identical_facet_errors <- identical_facets %>%
      select("strategy", "group", "full_chart_error_msg") %>%
      mutate(
        # Position error message in center of facet panel
        x = dominated_position / 2,
        y = max_y / 2
      )
  } else {
    identical_facet_errors <- NULL
  }

  # Build the plot
  p <- ggplot()

  # Layer 0: Placeholder for identical facets (ensures facet panels appear)
  if (!is.null(identical_facet_errors)) {
    p <- p + geom_blank(
      data = identical_facet_errors,
      aes(x = .data$x, y = .data$y)
    )
  }

  # Layer 1: Standard bars
  if (nrow(standard_bars) > 0) {
    p <- p + geom_tile(
      data = standard_bars,
      aes(
        y = .data$y_numeric,
        x = .data$x_center,
        width = .data$x_width,
        height = .data$bar_height,
        fill = .data$variation
      ),
      color = "black",
      linewidth = 0.2
    )
  }

  # Layer 2: To-zero bars
  if (nrow(to_zero_bars) > 0) {
    p <- p + geom_tile(
      data = to_zero_bars,
      aes(
        y = .data$y_numeric,
        x = .data$x_center,
        width = .data$x_width,
        height = .data$bar_height,
        fill = .data$variation
      ),
      color = "black",
      linewidth = 0.2
    )
  }

  # Arrow head dimensions - maintain consistent visual aspect ratio

  # The notch extends this far beyond the bar in y-coordinates
 arrow_notch_y <- 0.15
  # Calculate x-width to match y-extent when scaled by data ranges
  # x_range = dominated_position (0 to dominated_position)
  # y_range = max_y + 1 (accounts for 0.5 padding on each side)
  arrow_head_width <- arrow_notch_y * 2 * (dominated_position / (max_y + 1))

  # Layer 3: Arrow bars (using geom_polygon for arrow shape)
  if (nrow(arrow_bars) > 0) {

    # Note: In rowwise() + mutate() with list(), .data$ doesn't work inside list()
    # Access columns directly by name instead
    arrow_polygons <- arrow_bars %>%
      rowwise() %>%
      mutate(
        polygon_data = list(tibble(
          x = c(
            xmin,                                          # Left bottom
            dominated_position - arrow_head_width,         # Right before arrow
            dominated_position - arrow_head_width,         # Arrow notch bottom
            dominated_position,                            # Arrow tip
            dominated_position - arrow_head_width,         # Arrow notch top
            dominated_position - arrow_head_width,         # Right after arrow
            xmin                                           # Left top
          ),
          y = c(
            y_numeric - bar_height/2,
            y_numeric - bar_height/2,
            y_numeric - bar_height/2 - arrow_notch_y,
            y_numeric,
            y_numeric + bar_height/2 + arrow_notch_y,
            y_numeric + bar_height/2,
            y_numeric + bar_height/2
          )
        ))
      ) %>%
      ungroup()

    arrow_polygons_expanded <- arrow_polygons %>%
      select("parameter", "variation", "strategy", "group", "polygon_data") %>%
      unnest("polygon_data")

    p <- p + geom_polygon(
      data = arrow_polygons_expanded,
      aes(x = .data$x, y = .data$y,
          group = interaction(.data$parameter, .data$variation, .data$strategy, .data$group),
          fill = .data$variation),
      color = "black",
      linewidth = 0.2
    )
  }

  # Layer 4: Base case lines
  p <- p + geom_vline(
    data = base_case_lines,
    aes(xintercept = .data$base_display_value),
    linewidth = 0.5,
    color = "black"
  )

  # Layer 5: Base case labels
  p <- p + geom_label(
    data = base_case_labels,
    aes(x = .data$label_x, y = .data$y_pos, label = .data$base_label, hjust = .data$label_hjust),
    vjust = 0,
    size = 2,
    fontface = "bold",
    fill = "white",
    label.size = 0.5,
    label.padding = unit(0.25, "lines")
  )

  # Layer 6: Bar labels
  # Need to handle no-impact bars (display_value == base_display_value) to prevent
  # label overprinting when both Low and High would place labels on the same side
  label_data <- tornado_long %>%
    filter(.data$show_bar, !is.na(.data$label)) %>%
    mutate(
      is_no_impact = abs(.data$display_value - .data$base_display_value) < .Machine$double.eps * 100,
      natural_goes_right = .data$display_value > .data$base_display_value,
      natural_goes_left = .data$display_value < .data$base_display_value
    ) %>%
    # Get info about the OTHER variation for each parameter
    group_by(.data$strategy, .data$group, .data$parameter_display_name) %>%
    mutate(
      # For Low variation, check what High does; for High, check what Low does
      other_goes_right = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$natural_goes_right),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$natural_goes_right),
        TRUE ~ FALSE
      ),
      other_goes_left = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$natural_goes_left),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$natural_goes_left),
        TRUE ~ FALSE
      ),
      other_is_no_impact = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$is_no_impact),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$is_no_impact),
        TRUE ~ FALSE
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Determine label direction with tiebreaker for no-impact bars
      goes_right = case_when(
        # Normal case: bar has impact, use actual direction
        !.data$is_no_impact ~ .data$natural_goes_right,
        # No-impact bar, other has impact going left: this goes right
        .data$is_no_impact & .data$other_goes_left ~ TRUE,
        # No-impact bar, other has impact going right: this goes left
        .data$is_no_impact & .data$other_goes_right ~ FALSE,
        # Both no-impact: High goes right, Low goes left (arbitrary tiebreaker)
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "High" ~ TRUE,
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "Low" ~ FALSE,
        # Fallback (shouldn't reach here)
        TRUE ~ .data$natural_goes_right
      ),
      is_arrow = .data$bar_type == "arrow",
      is_to_zero = .data$bar_type == "to_zero",
      label_x = case_when(
        .data$is_arrow ~ dominated_position - arrow_head_width - x_tick_max * 0.01,
        .data$is_to_zero ~ .data$xmin + x_tick_max * 0.01,
        .data$goes_right ~ .data$xmax + x_tick_max * 0.01,
        TRUE ~ .data$xmin - x_tick_max * 0.01
      ),
      label_hjust = case_when(
        .data$is_arrow ~ 1,
        .data$is_to_zero ~ 0,
        .data$goes_right ~ 0,
        TRUE ~ 1
      )
    )

  if (nrow(label_data) > 0) {
    p <- p + geom_text(
      data = label_data,
      aes(x = .data$label_x, y = .data$y_numeric, label = .data$label,
          hjust = .data$label_hjust),
      vjust = 0,
      size = 2
    )
  }

  # Layer 7: Error labels for variations that can't display bars
  # Verbose labels explaining why bar isn't shown, with ICER for direction change
  error_label_data <- tornado_long %>%
    filter(!.data$show_bar, !is.na(.data$error_type)) %>%
    mutate(
      # Format ICER value for direction change message
      formatted_icer = scales::comma(abs(.data$icer_value_num), accuracy = 1),
      error_label = dplyr::case_when(
        .data$error_type == "direction_change" ~
          paste0("Bar could not be displayed for ", tolower(.data$variation),
                 " parameter value because\ndirectionality of ICER changed relative to base case.\nICER of ",
                 .data$formatted_icer, " reflects comparison of ",
                 if_else(.data$is_flipped_facet,
                         paste0(.data$intervention_name, " vs. ", .data$comparator_name),
                         paste0(.data$comparator_name, " vs. ", .data$intervention_name)),
                 "."),
        .data$error_type == "identical" ~
          paste0("Bar could not be displayed for ", tolower(.data$variation),
                 " parameter value because\nICER is undefined when costs and outcomes are identical\nbetween ",
                 .data$intervention_name, " and ", .data$comparator_name, "."),
        TRUE ~ NA_character_
      ),
      # Position: offset from base case line so label outline doesn't overlap
      # Low variations go left, High go right
      label_x = if_else(.data$variation == "Low",
        .data$base_display_value - x_tick_max * 0.02,
        .data$base_display_value + x_tick_max * 0.02
      ),
      label_hjust = if_else(.data$variation == "Low", 1, 0)
    ) %>%
    filter(!is.na(.data$error_label))

  if (nrow(error_label_data) > 0) {
    p <- p + geom_label(
      data = error_label_data,
      aes(x = .data$label_x, y = .data$y_numeric, label = .data$error_label,
          hjust = .data$label_hjust),
      vjust = 0,
      size = 2,
      fill = "white",
      color = "black",
      label.size = 0.3,
      label.padding = unit(0.15, "lines")
    )
  }

  # Layer 8: Error message for identical facets
  if (!is.null(identical_facet_errors)) {
    p <- p + geom_label(
      data = identical_facet_errors,
      aes(x = .data$x, y = .data$y, label = .data$full_chart_error_msg),
      size = 5,
      color = "black",
      fill = "white",
      label.size = 0.5,
      label.padding = unit(0.25, "lines"),
      hjust = 0.5,
      vjust = 0
    )
  }

  # Scales
  p <- p +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = scales::comma_format(),
      expand = expansion(mult = c(0, 0), add = c(0, 0))
    ) +
    scale_y_reverse(
      breaks = seq_len(ceiling(max_y / y_spacing)) * y_spacing,
      labels = function(y) {
        sapply(y, function(yval) {
          match_row <- y_labels %>% filter(.data$y_base == yval) %>% slice(1)
          if (nrow(match_row) > 0) match_row$parameter_display_name else ""
        })
      },
      expand = expansion(mult = c(0, 0), add = c(0.1, 0.7))
    )

  # Only add fill scale if there's data that uses it (avoids warning)
  has_fill_data <- (nrow(standard_bars) > 0 || nrow(to_zero_bars) > 0 ||
                      nrow(arrow_bars) > 0)
  if (has_fill_data) {
    p <- p + scale_fill_manual(values = c("Low" = "#F8766D", "High" = "#00BFC4"))
  }

  # Generate footnotes
  footnotes <- facet_metadata %>%
    filter(!is.na(.data$footnote_text)) %>%
    pull(.data$footnote_text) %>%
    unique()

  # Add error footnotes if any
  if (nrow(error_label_data) > 0) {
    error_footnotes <- tornado_data %>%
      filter(.data$has_error) %>%
      pull(.data$combined_error_message) %>%
      unique() %>%
      na.omit()
    footnotes <- c(footnotes, error_footnotes)
  }

  footnote_text <- if (length(footnotes) > 0) paste(footnotes, collapse = "\n") else NULL

  # Labels
  p <- p +
    labs(
      y = NULL,
      x = "ICER",
      fill = "Parameter Value",
      caption = footnote_text
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0, size = 9, face = "italic")
    )

  # Add faceting
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Plot DSA Cost-Effectiveness as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter on
#' the Incremental Cost-Effectiveness Ratio (ICER). Shows horizontal bars representing
#' the range of ICER variation with special handling for dominated, dominant, and
#' flipped scenarios.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups,
#'   or NULL (all groups plus aggregated)
#' @param interventions Character vector of intervention strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels? (default: TRUE)
#' @param drop_zero_impact Logical. Remove parameters with zero impact on ICER? (default: TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' The CE tornado plot displays each DSA parameter as a horizontal bar showing the range
#' of ICER values from low to high parameter value. Parameters are sorted by the magnitude
#' of their impact on ICER (largest range first).
#'
#' **ICER Encoding:**
#' - Positive finite: Standard ICER (intervention more costly and more effective)
#' - Dominated (Inf): Intervention more costly with less/equal effect
#' - Dominant (0): Intervention less costly with more/equal effect
#' - Flipped (negative): Comparator more costly and more effective (displays with asterisk)
#' - Equivalent (NaN): Identical costs and outcomes
#'
#' **Error Handling:**
#' - Direction changes (ICER sign flip): No bar, warning indicator
#' - Identical outcomes: No bar, warning indicator
#' - Identical base case: Full chart error message
#'
#' **Comparison Modes:**
#' - Both as single values: shows one comparison
#' - One as vector, other NULL: shows each vs. all others
#' - Both as vectors (N×M mode): shows all pairwise comparisons
#'
#' ICER calculations always use discounted values.
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#' dsa_results <- run_dsa(model)
#'
#' # CE tornado plot (comparator perspective)
#' dsa_ce_plot(dsa_results, "total_qalys", "total_cost", comparators = "control")
#'
#' # CE tornado plot (intervention perspective)
#' dsa_ce_plot(dsa_results, "total_qalys", "total_cost", interventions = "new_treatment")
#'
#' # N×M comparisons
#' dsa_ce_plot(dsa_results, "total_qalys", "total_cost",
#'             interventions = c("treatment_a", "treatment_b"),
#'             comparators = c("control", "standard_care"))
#' }
#'
#' @export
dsa_ce_plot <- function(results,
                        health_outcome,
                        cost_outcome,
                        groups = "overall",
                        interventions = NULL,
                        comparators = NULL,
                        show_parameter_values = TRUE,
                        drop_zero_impact = TRUE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for CE plot")
  }

  # Prepare CE tornado data
  prepared <- prepare_dsa_ce_tornado_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    show_parameter_values = show_parameter_values
  )

  tornado_data <- prepared$tornado_data
  facet_metadata <- prepared$facet_metadata

  # Check if data is valid
  if (is.null(tornado_data) || nrow(tornado_data) == 0) {
    stop("No data available for CE tornado plot with specified parameters")
  }

  # Filter out zero-impact parameters if requested
  if (drop_zero_impact) {
    n_params_before <- nrow(tornado_data)
    tornado_data <- tornado_data %>%
      filter(!is.na(.data$displayable_range) & abs(.data$displayable_range) > .Machine$double.eps * 100)

    if (nrow(tornado_data) == 0) {
      stop(sprintf("All %d parameters have zero impact on ICER. No data to plot.",
                   n_params_before))
    }
  }

  # Factorize for proper ordering
  strategy_levels <- attr(prepared$tornado_data, "strategy_order")
  if (is.null(strategy_levels)) {
    strategy_levels <- unique(tornado_data$strategy)
  }

  group_levels <- attr(prepared$tornado_data, "group_order")
  if (is.null(group_levels)) {
    group_levels <- unique(tornado_data$group)
  }

  tornado_data <- tornado_data %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels),
      group = factor(.data$group, levels = group_levels)
    )

  # Update facet metadata ordering
  facet_metadata <- facet_metadata %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels),
      group = factor(.data$group, levels = group_levels)
    )

  # Render CE tornado plot
  render_dsa_ce_tornado_plot(tornado_data, facet_metadata)
}
