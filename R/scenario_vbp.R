#' Scenario Analysis with Value-Based Pricing Add-on
#'
#' Functions for combining Scenario Analysis with Value-Based Pricing (VBP) analysis.
#' For each scenario, VBP sub-simulations are run to calculate the VBP relationship
#' for each scenario.
#'
#' @name scenario_vbp
#' @importFrom dplyr filter mutate arrange select bind_rows group_by summarize pull left_join
#' @importFrom purrr map map2 map_dbl map_dfr
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble
#' @importFrom stats lm coef predict
NULL

#' Validate Scenario+VBP Specification
#'
#' Validates that the VBP parameters for Scenario+VBP analysis are valid.
#'
#' @param model Parsed model object
#' @param vbp_price_variable Name of the price variable
#' @param vbp_intervention Intervention strategy name
#' @param vbp_outcome_summary Outcome summary name
#' @param vbp_cost_summary Cost summary name
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_scenario_vbp_spec <- function(model,
                                        vbp_price_variable,
                                        vbp_intervention,
                                        vbp_outcome_summary,
                                        vbp_cost_summary) {
  # Check intervention strategy exists
  if (!vbp_intervention %in% model$strategies$name) {
    stop(sprintf("VBP intervention strategy '%s' not found in model strategies",
                vbp_intervention), call. = FALSE)
  }

  # Check price variable exists
  if (!vbp_price_variable %in% model$variables$name) {
    stop(sprintf("VBP price variable '%s' not found in model variables",
                vbp_price_variable), call. = FALSE)
  }

  # Check summaries exist using validation helper
  metadata_for_check <- list(summaries = model$summaries)
  check_summary_exists(vbp_outcome_summary, metadata_for_check)
  check_summary_exists(vbp_cost_summary, metadata_for_check)

  TRUE
}

#' Build Scenario+VBP Segments
#'
#' Creates segments for combined Scenario + VBP analysis. For each scenario,
#' creates 3 price level sub-runs to enable VBP equation calculation.
#'
#' @param model Parsed model object
#' @param vbp_spec VBP specification list with price_variable, intervention_strategy,
#'   outcome_summary, cost_summary, and price_values
#' @return Tibble with all segments including vbp_price_level column
#' @keywords internal
build_scenario_vbp_segments <- function(model, vbp_spec) {
  # Get base scenario segments (includes scenario_id for base case + user scenarios)
  scenario_segments <- build_scenario_segments(model)

  # Get unique scenario_ids
  scenario_ids <- unique(scenario_segments$scenario_id)

  all_segments <- list()

  # For each scenario_id, create 3 VBP price level sub-runs
  for (sid in scenario_ids) {
    scenario_segs <- scenario_segments %>%
      filter(.data$scenario_id == sid)

    for (pl in seq_along(vbp_spec$price_values)) {
      price_val <- vbp_spec$price_values[pl]

      segments_with_vbp <- scenario_segs %>%
        mutate(
          vbp_price_level = pl,
          price_value = price_val,
          # Merge VBP price override with existing scenario parameter overrides
          parameter_overrides = map2(
            .data$parameter_overrides,
            .data$strategy,
            function(existing_overrides, strat) {
              # Only apply price override to intervention strategy
              if (strat == vbp_spec$intervention_strategy) {
                # Add VBP price override to existing scenario overrides
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

#' Analyze Scenario+VBP Results
#'
#' Calculates VBP equations for each scenario_id. For each scenario, extracts
#' costs and outcomes at the 3 price levels and fits the VBP linear model.
#'
#' @param segments Segment-level results with price variations
#' @param aggregated Aggregated results by strategy and scenario_id
#' @param vbp_spec VBP specification list
#' @param scenario_metadata Scenario metadata with name/description per scenario_id
#' @param model Parsed model object
#' @return Tibble with VBP equations for each scenario, including:
#'   scenario_id, scenario_name, scenario_description,
#'   comparator, group, vbp_slope, vbp_intercept, outcome_difference
#' @keywords internal
analyze_scenario_vbp_results <- function(segments,
                                          aggregated,
                                          vbp_spec,
                                          scenario_metadata,
                                          model) {
  # Get all strategies except intervention
  all_strategies <- unique(aggregated$strategy)
  comparators <- setdiff(all_strategies, vbp_spec$intervention_strategy)

  # Get all groups from segments
  all_groups <- unique(segments$group)

  # Get unique scenario_ids
  scenario_ids <- unique(scenario_metadata$scenario_id)

  # Calculate VBP equations for each scenario, group, and comparator
  all_equations <- list()

  for (sid in scenario_ids) {
    # Get scenario metadata for this scenario
    scenario_meta <- scenario_metadata %>%
      filter(.data$scenario_id == sid)

    for (grp in all_groups) {
      for (comparator in comparators) {
        # Extract group-specific results for each price level
        intervention_data <- segments %>%
          filter(.data$strategy == vbp_spec$intervention_strategy,
                 .data$group == grp,
                 .data$scenario_id == sid) %>%
          arrange(.data$vbp_price_level)

        comparator_data <- segments %>%
          filter(.data$strategy == comparator,
                 .data$group == grp,
                 .data$scenario_id == sid) %>%
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
          context = sprintf("in group %s, scenario '%s'", grp, scenario_meta$scenario_name[1])
        )

        # Calculate VBP equation
        all_equations[[length(all_equations) + 1]] <- tibble(
          scenario_id = sid,
          scenario_name = scenario_meta$scenario_name[1],
          scenario_description = scenario_meta$scenario_description[1],
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
  for (sid in scenario_ids) {
    scenario_meta <- scenario_metadata %>%
      filter(.data$scenario_id == sid)

    for (comparator in comparators) {
      # Extract aggregated results for each price level
      intervention_data <- aggregated %>%
        filter(.data$strategy == vbp_spec$intervention_strategy,
               .data$scenario_id == sid) %>%
        arrange(.data$vbp_price_level)

      comparator_data <- aggregated %>%
        filter(.data$strategy == comparator,
               .data$scenario_id == sid) %>%
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
        context = sprintf("(aggregated), scenario '%s'", scenario_meta$scenario_name[1])
      )

      all_equations[[length(all_equations) + 1]] <- tibble(
        scenario_id = sid,
        scenario_name = scenario_meta$scenario_name[1],
        scenario_description = scenario_meta$scenario_description[1],
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

#' Calculate Scenario+VBP Price at Given WTP
#'
#' Calculates the value-based price for a specific scenario using
#' the VBP equation: Price = slope * WTP + intercept
#'
#' @param results Scenario+VBP results object from run_scenario() with VBP enabled
#' @param wtp Willingness-to-pay threshold
#' @param comparator Name of the comparator strategy. If NULL (default),
#'   returns the minimum VBP across all comparators.
#' @param scenario_name Scenario name (default: "Base Case")
#' @param group Group name or "overall" (default)
#' @return Numeric value representing the VBP price
#'
#' @examples
#' \dontrun{
#' # Calculate VBP at base case
#' vbp_price <- calculate_scenario_vbp_price(results, 50000)
#'
#' # Calculate VBP for a specific scenario
#' vbp_price <- calculate_scenario_vbp_price(results, 50000, scenario_name = "Optimistic")
#'
#' # Calculate VBP vs specific comparator
#' vbp_price <- calculate_scenario_vbp_price(results, 50000, "chemo", scenario_name = "Base Case")
#' }
#'
#' @export
calculate_scenario_vbp_price <- function(results, wtp, comparator = NULL,
                                          scenario_name = "Base Case", group = "overall") {
  # Check that VBP equations exist
  if (is.null(results$scenario_vbp_equations) || nrow(results$scenario_vbp_equations) == 0) {
    stop("No VBP equations found. Ensure run_scenario() was called with VBP parameters.",
         call. = FALSE)
  }

  # Filter equations
  equations <- results$scenario_vbp_equations %>%
    filter(.data$scenario_name == !!scenario_name, .data$group == !!group)

  if (nrow(equations) == 0) {
    stop(sprintf("No VBP equations found for scenario: '%s', group: %s",
                scenario_name, group), call. = FALSE)
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
    stop(sprintf("No VBP equation found for comparator: %s, scenario: '%s', group: %s",
                comparator, scenario_name, group), call. = FALSE)
  }

  equation$vbp_slope[1] * wtp + equation$vbp_intercept[1]
}
