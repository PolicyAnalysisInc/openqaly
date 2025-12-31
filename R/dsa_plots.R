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
  if (is.null(facet_component)) {
    n_groups <- length(unique(tornado_data$group))
    n_strategies <- length(unique(tornado_data$strategy))

    facet_component <- facet_grid(rows = vars(group), cols = vars(strategy), scales = "free")
    if ((n_groups > 1) && (n_strategies == 1)) {
      facet_component <- facet_wrap(~ group, scales = "free")
    } else if ((n_strategies > 1) && (n_groups == 1)) {
      facet_component <- facet_wrap(~ strategy, scales = "free", ncol = 1)
    } else if ((n_strategies == 1) && (n_groups == 1)) {
      facet_component <- NULL
    }
  }

  # Reshape to long format for two bars per parameter
  tornado_long <- tornado_data %>%
    pivot_longer(
      cols = c(low, high),
      names_to = "variation",
      values_to = "value"
    ) %>%
    # Reverse factor levels so highest impact is at top
    mutate(
      parameter_display_name = factor(parameter_display_name, levels = rev(levels(parameter_display_name))),
      variation = factor(variation, levels = c("low", "high"),
                        labels = c("Low", "High"))
    ) %>%
    # Detect if both bars are on same side of base case
    group_by(parameter_display_name, strategy, group) %>%
    mutate(
      same_side = (min(value) > unique(base)) | (max(value) < unique(base)),
      stack_pos = if_else(same_side,
                         if_else(variation == "Low", "bottom", "top"),
                         "full")
    ) %>%
    ungroup() %>%
    # Calculate bar positions for geom_rect
    mutate(
      param_position = as.numeric(parameter_display_name),
      ymin = case_when(
        stack_pos == "full" ~ param_position - 0.45,
        stack_pos == "bottom" ~ param_position - 0.45,
        stack_pos == "top" ~ param_position + 0.05
      ),
      ymax = case_when(
        stack_pos == "full" ~ param_position + 0.45,
        stack_pos == "bottom" ~ param_position - 0.05,
        stack_pos == "top" ~ param_position + 0.45
      ),
      xmin = pmin(base, value),
      xmax = pmax(base, value)
    )

  # Create base case data for vertical lines (one per strategy-group combination)
  base_case_data <- tornado_long %>%
    distinct(strategy, group, base)

  # Calculate axis breaks and limits to include 0 and extend beyond data
  breaks_fn <- scales::pretty_breaks(n = 5)
  x_range <- range(c(0, tornado_long$xmin, tornado_long$xmax))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)

  # Create tornado plot with bars
  p <- ggplot(tornado_long, aes(y = parameter_display_name, fill = variation)) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              color = "black", linewidth = 0.2) +
    geom_vline(data = base_case_data, aes(xintercept = base), linewidth = 0.2) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, labels = comma) +
    labs(y = NULL, x = summary_label, fill = "Parameter Value") +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 10),
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
    distinct(parameter, strategy, group)

  # Extract parameter values for each combination
  param_values_list <- list()

  for (i in seq_len(nrow(param_combos))) {
    param_name <- param_combos$parameter[i]
    strat <- param_combos$strategy[i]
    grp <- param_combos$group[i]

    # Get the parameter info from dsa_metadata
    param_info <- results$dsa_metadata %>%
      filter(parameter == param_name)

    if (nrow(param_info) == 0) {
      next  # Skip if parameter not found
    }

    # Get run_ids for low and high variations
    low_run_id <- param_info %>% filter(variation == "low") %>% pull(run_id)
    high_run_id <- param_info %>% filter(variation == "high") %>% pull(run_id)

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
    # If group is "_aggregated" or "Aggregated", use the actual segment group
    if (search_group %in% c("_aggregated", "Aggregated")) {
      # Just get any segment for this strategy and run_id
      low_segment <- results$segments %>%
        filter(run_id == low_run_id, strategy == extract_strategy) %>%
        slice(1)
      high_segment <- results$segments %>%
        filter(run_id == high_run_id, strategy == extract_strategy) %>%
        slice(1)
    } else {
      # For specific groups, try to match the group
      low_segment <- results$segments %>%
        filter(run_id == low_run_id, strategy == extract_strategy, group == search_group) %>%
        slice(1)
      high_segment <- results$segments %>%
        filter(run_id == high_run_id, strategy == extract_strategy, group == search_group) %>%
        slice(1)

      # If segments not found with group filter, try without
      if (nrow(low_segment) == 0) {
        low_segment <- results$segments %>%
          filter(run_id == low_run_id, strategy == extract_strategy) %>%
          slice(1)
      }
      if (nrow(high_segment) == 0) {
        high_segment <- results$segments %>%
          filter(run_id == high_run_id, strategy == extract_strategy) %>%
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
      param_high_value = high_value
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
      param_high_value = numeric()
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
#' @param group Group selection
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
                                     group,
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
    group = group,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Separate base case (run_id = 1) from variations
  base_data <- dsa_data %>%
    filter(run_id == 1) %>%
    select(strategy, group, base = amount)

  # Separate low and high variations
  low_data <- dsa_data %>%
    filter(variation == "low") %>%
    select(strategy, group, parameter, parameter_display_name, low = amount)

  high_data <- dsa_data %>%
    filter(variation == "high") %>%
    select(strategy, group, parameter, parameter_display_name, high = amount)

  # Combine into tornado format
  tornado_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group")) %>%
    mutate(range = abs(high - low))

  # Calculate differences if interventions/comparators provided
  differences_created <- FALSE
  strategy_order <- NULL  # Will be set to preserve correct strategy order
  if (!is.null(interventions) || !is.null(comparators)) {
    # Pivot to get strategies as columns
    tornado_wide <- tornado_data %>%
      pivot_wider(
        names_from = strategy,
        values_from = c(low, base, high, range),
        id_cols = c(group, parameter, parameter_display_name)
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
          range = abs(high - low)
        ) %>%
        select(group, parameter, parameter_display_name, strategy, low, base, high, range)

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
    tornado_data <- tornado_data %>%
      mutate(
        parameter_display_name = if_else(
          !is.na(param_low_value) & !is.na(param_high_value),
          sprintf("%s (%s - %s)",
                  parameter_display_name,
                  format_param_value(param_low_value),
                  format_param_value(param_high_value)),
          parameter_display_name
        )
      ) %>%
      select(-param_low_value, -param_high_value)
  }

  # Sort by range (largest impact first) within each strategy-group
  # Use .by_group = TRUE to ensure sorting happens within groups, not globally
  # This preserves the strategy order while sorting parameters within each strategy
  tornado_data <- tornado_data %>%
    group_by(strategy, group) %>%
    arrange(desc(range), .by_group = TRUE) %>%
    mutate(parameter_display_name = factor(parameter_display_name, levels = unique(parameter_display_name))) %>%
    ungroup()

  # Attach strategy_order as an attribute to preserve correct ordering
  if (!is.null(strategy_order)) {
    attr(tornado_data, "strategy_order") <- strategy_order
  }

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
#' @param group Group selection: "aggregated" (default), specific group name, or NULL
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
#' - Both: facet_grid(group ~ strategy)
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
                              group = "aggregated",
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
    group = group,
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
      filter(abs(range) > .Machine$double.eps * 100) %>%
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

  tornado_data <- tornado_data %>%
    mutate(
      strategy = factor(strategy, levels = strategy_levels),
      group = factor(group, levels = unique(group))
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
#' @param group Group selection: "aggregated" (default), specific group name, or NULL
#'   (all groups plus aggregated)
#' @param wtp Optional override for willingness-to-pay. If NULL, extracts from outcome summary metadata.
#' @param interventions Character vector of intervention strategy name(s) (e.g., "new_treatment").
#'   Can be a single value or vector. Can be combined with comparators for N×M comparisons.
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s) (e.g., "control").
#'   Can be a single value or vector. Can be combined with interventions for N×M comparisons.
#'   At least one of interventions or comparators must be specified.
#' @param discounted Logical. Use discounted values? (default: FALSE)
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
#' - Both: facet_grid(group ~ strategy)
#' - Single: no faceting
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
                         group = "aggregated",
                         wtp = NULL,
                         interventions = NULL,
                         comparators = NULL,
                         discounted = FALSE,
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
      filter(name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome))
    }
    wtp <- outcome_meta$wtp[1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.", health_outcome))
    }
  }

  # Prepare tornado data for outcomes
  outcome_tornado <- prepare_dsa_tornado_data(
    results = results,
    summary_name = health_outcome,
    group = group,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = FALSE  # We'll add parameter values to combined data later
  )

  # Capture strategy order from outcome tornado data
  strategy_order_nmb <- attr(outcome_tornado, "strategy_order")

  # Multiply by WTP
  outcome_tornado <- outcome_tornado %>%
    mutate(
      low = low * wtp,
      base = base * wtp,
      high = high * wtp,
      range = abs(high - low)
    )

  # Prepare tornado data for costs
  cost_tornado <- prepare_dsa_tornado_data(
    results = results,
    summary_name = cost_outcome,
    group = group,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = FALSE
  )

  # Negate costs (we subtract them)
  cost_tornado <- cost_tornado %>%
    mutate(
      low = -low,
      base = -base,
      high = -high,
      range = abs(high - low)
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
      low_outcome = replace_na(low_outcome, 0),
      base_outcome = replace_na(base_outcome, 0),
      high_outcome = replace_na(high_outcome, 0),
      low_cost = replace_na(low_cost, 0),
      base_cost = replace_na(base_cost, 0),
      high_cost = replace_na(high_cost, 0),
      # Calculate NMB
      low = low_outcome + low_cost,
      base = base_outcome + base_cost,
      high = high_outcome + high_cost,
      range = abs(high - low)
    ) %>%
    select(strategy, group, parameter, parameter_display_name, low, base, high, range)

  # Add parameter values to labels if requested
  if (show_parameter_values) {
    param_values <- extract_parameter_values(results, nmb_tornado, interventions, comparators)

    nmb_tornado <- nmb_tornado %>%
      left_join(param_values, by = c("parameter", "strategy", "group")) %>%
      mutate(
        parameter_display_name = if_else(
          !is.na(param_low_value) & !is.na(param_high_value),
          sprintf("%s (%s - %s)",
                  parameter_display_name,
                  format_param_value(param_low_value),
                  format_param_value(param_high_value)),
          parameter_display_name
        )
      ) %>%
      select(-param_low_value, -param_high_value)
  }

  # Check if data is valid
  if (is.null(nmb_tornado) || nrow(nmb_tornado) == 0) {
    stop("No data available for NMB tornado plot with specified parameters")
  }

  # Filter out zero-impact parameters if requested
  if (drop_zero_impact) {
    n_params_before <- nrow(nmb_tornado)
    nmb_tornado <- nmb_tornado %>%
      filter(abs(range) > .Machine$double.eps * 100)

    if (nrow(nmb_tornado) == 0) {
      stop(sprintf("All %d parameters have zero impact on NMB. No data to plot.",
                   n_params_before))
    }
  }

  # Sort by range (largest impact first) within each strategy-group
  nmb_tornado <- nmb_tornado %>%
    group_by(strategy, group) %>%
    arrange(desc(range)) %>%
    mutate(parameter_display_name = factor(parameter_display_name, levels = unique(parameter_display_name))) %>%
    ungroup()

  # Factorize for proper ordering
  # Use strategy_order captured from prepare_dsa_tornado_data to preserve model definition order
  strategy_levels_nmb <- strategy_order_nmb
  if (is.null(strategy_levels_nmb)) {
    strategy_levels_nmb <- unique(nmb_tornado$strategy)
  }

  nmb_tornado <- nmb_tornado %>%
    mutate(
      strategy = factor(strategy, levels = strategy_levels_nmb),
      group = factor(group, levels = unique(group))
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
