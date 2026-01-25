#' DSA with Value-Based Pricing Add-on
#'
#' Functions for combining Deterministic Sensitivity Analysis (DSA) with
#' Value-Based Pricing (VBP) analysis. For each DSA parameter variation,
#' VBP sub-simulations are run to calculate the VBP relationship at each
#' parameter variation.
#'
#' @name dsa_vbp
#' @importFrom dplyr filter mutate arrange select bind_rows group_by summarize pull left_join
#' @importFrom purrr map map2 map_dbl map_dfr
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
#' @importFrom stats lm coef predict
NULL

#' Validate DSA+VBP Specification
#'
#' Validates that the VBP parameters for DSA+VBP analysis are valid.
#'
#' @param model Parsed model object
#' @param vbp_price_variable Name of the price variable
#' @param vbp_intervention Intervention strategy name
#' @param vbp_outcome_summary Outcome summary name
#' @param vbp_cost_summary Cost summary name
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_dsa_vbp_spec <- function(model,
                                   vbp_price_variable,
                                   vbp_intervention,
                                   vbp_outcome_summary,
                                   vbp_cost_summary) {
  # Check intervention strategy exists
  if (!vbp_intervention %in% model$strategies$name) {
    stop(sprintf("VBP intervention strategy '%s' not found in model strategies",
                vbp_intervention))
  }

  # Check price variable exists
  if (!vbp_price_variable %in% model$variables$name) {
    stop(sprintf("VBP price variable '%s' not found in model variables",
                vbp_price_variable))
  }

  # Check summaries exist using validation helper
  metadata_for_check <- list(summaries = model$summaries)
  check_summary_exists(vbp_outcome_summary, metadata_for_check)
  check_summary_exists(vbp_cost_summary, metadata_for_check)

  TRUE
}

#' Build DSA+VBP Segments
#'
#' Creates segments for combined DSA + VBP analysis. For each DSA run,
#' creates 3 price level sub-runs to enable VBP equation calculation.
#'
#' @param model Parsed model object
#' @param vbp_spec VBP specification list with price_variable, intervention_strategy,
#'   outcome_summary, cost_summary, and price_values
#' @return Tibble with all segments including vbp_price_level column
#' @keywords internal
build_dsa_vbp_segments <- function(model, vbp_spec) {
  # Get base DSA segments (includes run_id for base case + variations)
  dsa_segments <- build_dsa_segments(model)

  # Get unique DSA run_ids
  dsa_run_ids <- unique(dsa_segments$run_id)

  all_segments <- list()

  # For each DSA run_id, create 3 VBP price level sub-runs
  for (rid in dsa_run_ids) {
    run_segs <- dsa_segments %>%
      filter(.data$run_id == rid)

    for (pl in seq_along(vbp_spec$price_values)) {
      price_val <- vbp_spec$price_values[pl]

      segments_with_vbp <- run_segs %>%
        mutate(
          vbp_price_level = pl,
          price_value = price_val,
          # Merge VBP price override with existing DSA parameter overrides
          parameter_overrides = map2(
            .data$parameter_overrides,
            .data$strategy,
            function(existing_overrides, strat) {
              # Only apply price override to intervention strategy
              if (strat == vbp_spec$intervention_strategy) {
                # Add VBP price override to existing DSA overrides
                combined <- existing_overrides
                combined[[vbp_spec$price_variable]] <- price_val
                combined
              } else {
                existing_overrides
              }
            }
          )
        )

      all_segments[[length(all_segments) + 1]] <- segments_with_vbp
    }
  }

  bind_rows(all_segments)
}

#' Analyze DSA+VBP Results
#'
#' Calculates VBP equations for each DSA run_id. For each run, extracts
#' costs and outcomes at the 3 price levels and fits the VBP linear model.
#'
#' @param segments Segment-level results with price variations
#' @param aggregated Aggregated results by strategy and run_id
#' @param vbp_spec VBP specification list
#' @param dsa_metadata DSA metadata with parameter info per run_id
#' @param model Parsed model object
#' @return Tibble with VBP equations for each DSA run, including:
#'   run_id, parameter, parameter_display_name, variation,
#'   comparator, group, vbp_slope, vbp_intercept, outcome_difference
#' @keywords internal
analyze_dsa_vbp_results <- function(segments,
                                     aggregated,
                                     vbp_spec,
                                     dsa_metadata,
                                     model) {
  # Get all strategies except intervention
  all_strategies <- unique(aggregated$strategy)
  comparators <- setdiff(all_strategies, vbp_spec$intervention_strategy)

  # Get all groups from segments
  all_groups <- unique(segments$group)

  # Get unique DSA run_ids
  dsa_run_ids <- unique(dsa_metadata$run_id)

  # Calculate VBP equations for each DSA run, group, and comparator
  all_equations <- list()

  for (rid in dsa_run_ids) {
    # Get DSA metadata for this run
    run_meta <- dsa_metadata %>%
      filter(.data$run_id == rid)

    for (grp in all_groups) {
      for (comparator in comparators) {
        # Extract group-specific results for each price level
        intervention_data <- segments %>%
          filter(.data$strategy == vbp_spec$intervention_strategy,
                 .data$group == grp,
                 .data$run_id == rid) %>%
          arrange(.data$vbp_price_level)

        comparator_data <- segments %>%
          filter(.data$strategy == comparator,
                 .data$group == grp,
                 .data$run_id == rid) %>%
          arrange(.data$vbp_price_level)

        # Skip if no data for this combination
        if (nrow(intervention_data) == 0 || nrow(comparator_data) == 0) {
          next
        }

        # Extract costs and outcomes
        int_costs <- extract_segment_summary_values(intervention_data, vbp_spec$cost_summary)
        comp_costs <- extract_segment_summary_values(comparator_data, vbp_spec$cost_summary)
        int_outcomes <- extract_segment_summary_values(intervention_data, vbp_spec$outcome_summary)
        comp_outcomes <- extract_segment_summary_values(comparator_data, vbp_spec$outcome_summary)

        # Calculate incremental values
        delta_costs <- int_costs - comp_costs
        delta_outcomes <- int_outcomes - comp_outcomes

        # Use shared VBP calculation from vbp.R
        vbp_eq <- calculate_vbp_equation(
          delta_costs = delta_costs,
          delta_outcomes = delta_outcomes,
          price_values = vbp_spec$price_values,
          intervention = vbp_spec$intervention_strategy,
          comparator = comparator,
          context = sprintf("in group %s, run_id %d", grp, rid)
        )

        # Calculate VBP equation
        all_equations[[length(all_equations) + 1]] <- tibble(
          run_id = rid,
          parameter = run_meta$parameter[1],
          parameter_display_name = run_meta$parameter_display_name[1],
          parameter_type = run_meta$parameter_type[1],
          variation = run_meta$variation[1],
          override_value = run_meta$override_value[1],
          comparator = comparator,
          group = grp,
          outcome_difference = vbp_eq$outcome_difference,
          cost_slope = vbp_eq$cost_slope,
          cost_intercept = vbp_eq$cost_intercept,
          vbp_slope = vbp_eq$vbp_slope,
          vbp_intercept = vbp_eq$vbp_intercept,
          weight = intervention_data$weight[1]
        )
      }
    }
  }

  # Also calculate overall VBP equations from aggregated results
  for (rid in dsa_run_ids) {
    run_meta <- dsa_metadata %>%
      filter(.data$run_id == rid)

    for (comparator in comparators) {
      # Extract aggregated results for each price level
      intervention_data <- aggregated %>%
        filter(.data$strategy == vbp_spec$intervention_strategy,
               .data$run_id == rid) %>%
        arrange(.data$vbp_price_level)

      comparator_data <- aggregated %>%
        filter(.data$strategy == comparator,
               .data$run_id == rid) %>%
        arrange(.data$vbp_price_level)

      if (nrow(intervention_data) == 0 || nrow(comparator_data) == 0) {
        next
      }

      # Extract aggregated costs and outcomes
      int_costs <- extract_summary_values(intervention_data, vbp_spec$cost_summary)
      comp_costs <- extract_summary_values(comparator_data, vbp_spec$cost_summary)
      int_outcomes <- extract_summary_values(intervention_data, vbp_spec$outcome_summary)
      comp_outcomes <- extract_summary_values(comparator_data, vbp_spec$outcome_summary)

      # Calculate incremental values
      delta_costs <- int_costs - comp_costs
      delta_outcomes <- int_outcomes - comp_outcomes

      # Use shared VBP calculation from vbp.R
      vbp_eq <- calculate_vbp_equation(
        delta_costs = delta_costs,
        delta_outcomes = delta_outcomes,
        price_values = vbp_spec$price_values,
        intervention = vbp_spec$intervention_strategy,
        comparator = comparator,
        context = sprintf("(aggregated), run_id %d", rid)
      )

      all_equations[[length(all_equations) + 1]] <- tibble(
        run_id = rid,
        parameter = run_meta$parameter[1],
        parameter_display_name = run_meta$parameter_display_name[1],
        parameter_type = run_meta$parameter_type[1],
        variation = run_meta$variation[1],
        override_value = run_meta$override_value[1],
        comparator = comparator,
        group = "overall",
        outcome_difference = vbp_eq$outcome_difference,
        cost_slope = vbp_eq$cost_slope,
        cost_intercept = vbp_eq$cost_intercept,
        vbp_slope = vbp_eq$vbp_slope,
        vbp_intercept = vbp_eq$vbp_intercept,
        weight = NA_real_
      )
    }
  }

  bind_rows(all_equations)
}

#' Calculate DSA+VBP Price at Given WTP
#'
#' Calculates the value-based price for a specific DSA run using
#' the VBP equation: Price = slope * WTP + intercept
#'
#' @param results DSA+VBP results object from run_dsa() with VBP enabled
#' @param wtp Willingness-to-pay threshold
#' @param comparator Name of the comparator strategy. If NULL (default),
#'   returns the minimum VBP across all comparators.
#' @param run_id DSA run_id (1 = base case, default)
#' @param group Group name or "overall" (default)
#' @return Numeric value representing the VBP price
#'
#' @examples
#' \dontrun{
#' # Calculate VBP at base case
#' vbp_price <- calculate_dsa_vbp_price(results, 50000)
#'
#' # Calculate VBP for a specific DSA variation
#' vbp_price <- calculate_dsa_vbp_price(results, 50000, run_id = 3)
#'
#' # Calculate VBP vs specific comparator
#' vbp_price <- calculate_dsa_vbp_price(results, 50000, "chemo", run_id = 1)
#' }
#'
#' @export
calculate_dsa_vbp_price <- function(results, wtp, comparator = NULL,
                                     run_id = 1, group = "overall") {
  # Check that VBP equations exist
  if (is.null(results$dsa_vbp_equations) || nrow(results$dsa_vbp_equations) == 0) {
    stop("No VBP equations found. Ensure run_dsa() was called with VBP parameters.")
  }

  # Filter equations
  equations <- results$dsa_vbp_equations %>%
    filter(.data$run_id == !!run_id, .data$group == !!group)

  if (nrow(equations) == 0) {
    stop(sprintf("No VBP equations found for run_id: %d, group: %s", run_id, group))
  }

  # When comparator is NULL, return minimum VBP across all comparators
  if (is.null(comparator)) {
    vbp_prices <- equations$vbp_slope * wtp + equations$vbp_intercept
    return(min(vbp_prices))
  }

  # Filter to specific comparator
  equation <- equations %>%
    filter(.data$comparator == !!comparator)

  if (nrow(equation) == 0) {
    stop(sprintf("No VBP equation found for comparator: %s, run_id: %d, group: %s",
                comparator, run_id, group))
  }

  equation$vbp_slope[1] * wtp + equation$vbp_intercept[1]
}
