#' Value-Based Pricing Analysis
#'
#' Calculates value-based pricing (VBP) equations that determine the maximum price
#' an intervention can charge while remaining cost-effective at different
#' willingness-to-pay (WTP) thresholds.
#'
#' @name vbp
#' @importFrom dplyr filter mutate arrange select bind_rows group_by summarize pull
#' @importFrom purrr map map2 map_dbl map_dfr
#' @importFrom tidyr expand_grid pivot_wider
#' @importFrom tibble tibble
#' @importFrom stats lm coef predict residuals
#' @importFrom furrr future_map furrr_options
NULL

#' Run Value-Based Pricing Analysis
#'
#' Performs value-based pricing analysis by varying the price of an intervention
#' across three test values and calculating the linear relationship between WTP
#' and the price that maintains cost-effectiveness versus each comparator.
#'
#' @param model An openqaly model object or oq_model_builder object
#' @param price_variable Name of the variable representing the intervention's price
#' @param intervention_strategy Name of the intervention strategy
#' @param outcome_summary Name of the outcome summary to use (default: "total_qalys")
#' @param cost_summary Name of the cost summary to use (default: "total_cost")
#' @param ... Additional arguments passed to run_segment
#'
#' @return A list containing:
#'   \item{segments}{Raw segment results with price variations}
#'   \item{aggregated}{Aggregated results by strategy and price level}
#'   \item{vbp_equations}{Overall VBP equations (one per comparator)}
#'   \item{vbp_equations_by_group}{Group-specific VBP equations}
#'   \item{vbp_metadata}{Metadata about the analysis}
#'   \item{spec}{The VBP specification used}
#'
#' @details
#' VBP analysis works by:
#' \enumerate{
#'   \item Running the model at three price points (0, 1, 2)
#'   \item Verifying that price affects only costs (not outcomes)
#'   \item Calculating the linear relationship between price and incremental costs
#'   \item Deriving the VBP equation: Price = (Delta Outcome/cost_slope) * WTP - (cost_intercept/cost_slope)
#' }
#'
#' For models with groups, VBP equations are calculated for each group separately
#' and for the overall aggregated population. The overall equation should match
#' the weighted average of group equations.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/checkimab", package = "openqaly"))
#'
#' vbp_results <- run_vbp(
#'   model,
#'   price_variable = "c_check",
#'   intervention_strategy = "check",
#'   outcome_summary = "qalys",
#'   cost_summary = "costs"
#' )
#'
#' # View overall equations
#' vbp_results$vbp_equations
#'
#' # Calculate VBP at specific WTP
#' vbp_price <- calculate_vbp_price(vbp_results, "chemo", 50000)
#' }
#'
#' @export
run_vbp <- function(model,
                   price_variable,
                   intervention_strategy,
                   outcome_summary = "total_qalys",
                   cost_summary = "total_cost",
                   ...) {

  # Fixed test prices for linearity analysis
  # Using larger values to ensure meaningful cost differences
  price_values <- c(0, 1000, 2000)

  # Parse and validate model
  if ("oq_model_builder" %in% class(model)) {
    model <- normalize_and_validate_model(model, preserve_builder = FALSE)
  }
  parsed_model <- parse_model(model, ...)

  # Create VBP specification
  vbp_spec <- list(
    price_variable = price_variable,
    intervention_strategy = intervention_strategy,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    price_values = price_values
  )

  # Validate specification
  validate_vbp_spec(parsed_model, vbp_spec)

  # Build segments for 3 price points
  vbp_segments <- build_vbp_segments(parsed_model, vbp_spec)

  # Run all segments in parallel
  segment_results <- vbp_segments %>%
    rowwise() %>%
    group_split() %>%
    future_map(function(segment) run_segment(segment, parsed_model, ...),
               .progress = TRUE, .options = furrr_options(seed = 1)) %>%
    bind_rows()

  # Aggregate by price_level + strategy
  aggregated <- aggregate_segments(segment_results, parsed_model)

  # Attach metadata for display name mapping in tables/plots
  attr(aggregated, "metadata") <- parsed_model$metadata

  # Analyze VBP results at both group and overall levels
  vbp_analysis <- analyze_vbp_results(
    segment_results,  # Pass segments for group analysis
    aggregated,       # Pass aggregated for overall analysis
    vbp_spec,
    parsed_model
  )

  # Return results
  list(
    segments = segment_results,
    aggregated = aggregated,
    vbp_equations = vbp_analysis$equations,
    vbp_equations_by_group = vbp_analysis$equations_by_group,
    vbp_metadata = vbp_analysis$metadata,
    spec = vbp_spec
  )
}

#' Build VBP Segments with Price Variations
#'
#' Creates segments for VBP analysis by varying the price parameter
#' across three test values (0, 1, 2) for the intervention strategy.
#'
#' @param model Parsed model object
#' @param vbp_spec VBP specification list
#' @return Tibble of segments with price_level and parameter_overrides
#' @keywords internal
build_vbp_segments <- function(model, vbp_spec) {
  base_segments <- get_segments(model)

  vbp_segments <- list()

  for (i in seq_along(vbp_spec$price_values)) {
    segments_with_price <- base_segments %>%
      mutate(
        run_id = i,  # Use run_id to leverage existing aggregation logic
        price_level = i,
        price_value = vbp_spec$price_values[i],
        parameter_overrides = map2(.data$strategy, .data$group, function(s, g) {
          # Check if this is the intervention strategy
          # Note: price applies to all groups of the intervention strategy
          is_intervention <- (s == vbp_spec$intervention_strategy)

          if (is_intervention) {
            setNames(list(vbp_spec$price_values[i]),
                     vbp_spec$price_variable)
          } else {
            list()
          }
        })
      )
    vbp_segments[[i]] <- segments_with_price
  }

  bind_rows(vbp_segments)
}

#' Analyze VBP Results
#'
#' Calculates VBP equations for each group and for the overall aggregated population.
#' Verifies that price affects only costs (not outcomes) and that the relationship
#' is linear.
#'
#' @param segments Segment-level results with price variations
#' @param aggregated Aggregated results by strategy and price level
#' @param vbp_spec VBP specification list
#' @param model Parsed model object
#' @return List with equations (overall), equations_by_group, and metadata
#' @keywords internal
analyze_vbp_results <- function(segments, aggregated, vbp_spec, model) {
  # Get all strategies except intervention
  all_strategies <- unique(aggregated$strategy)
  comparators <- setdiff(all_strategies, vbp_spec$intervention_strategy)

  # Get all groups
  all_groups <- unique(segments$group)

  # Calculate VBP equations for each group
  equations_by_group <- list()

  for (grp in all_groups) {
    group_equations <- list()

    for (comparator in comparators) {
      # Extract group-specific results for each price point
      intervention_data <- segments %>%
        filter(.data$strategy == vbp_spec$intervention_strategy,
               .data$group == grp) %>%
        arrange(.data$price_level)

      comparator_data <- segments %>%
        filter(.data$strategy == comparator,
               .data$group == grp) %>%
        arrange(.data$price_level)

      # Skip if no data for this group
      if (nrow(intervention_data) == 0 || nrow(comparator_data) == 0) {
        next
      }

      # Extract costs and outcomes for this group
      int_costs <- extract_segment_summary_values(intervention_data, vbp_spec$cost_summary)
      comp_costs <- extract_segment_summary_values(comparator_data, vbp_spec$cost_summary)
      int_outcomes <- extract_segment_summary_values(intervention_data, vbp_spec$outcome_summary)
      comp_outcomes <- extract_segment_summary_values(comparator_data, vbp_spec$outcome_summary)

      # Calculate incremental values
      delta_costs <- int_costs - comp_costs
      delta_outcomes <- int_outcomes - comp_outcomes

      # Use shared VBP calculation
      vbp_eq <- calculate_vbp_equation(
        delta_costs = delta_costs,
        delta_outcomes = delta_outcomes,
        price_values = vbp_spec$price_values,
        intervention = vbp_spec$intervention_strategy,
        comparator = comparator,
        context = sprintf("in group %s", grp)
      )

      # Calculate VBP equation for this group
      group_equations[[comparator]] <- tibble(
        group = grp,
        intervention = vbp_spec$intervention_strategy,
        comparator = comparator,
        outcome_difference = vbp_eq$outcome_difference,
        cost_slope = vbp_eq$cost_slope,
        cost_intercept = vbp_eq$cost_intercept,
        vbp_slope = vbp_eq$vbp_slope,
        vbp_intercept = vbp_eq$vbp_intercept,
        weight = intervention_data$weight[1]  # Store group weight
      )
    }

    if (length(group_equations) > 0) {
      equations_by_group[[grp]] <- bind_rows(group_equations)
    }
  }

  # Combine all group equations
  all_group_equations <- if (length(equations_by_group) > 0) {
    bind_rows(equations_by_group)
  } else {
    tibble()
  }

  # Calculate overall VBP equations from aggregated results
  overall_equations <- list()

  for (comparator in comparators) {
    # Extract aggregated results for each price point
    intervention_data <- aggregated %>%
      filter(.data$strategy == vbp_spec$intervention_strategy) %>%
      arrange(.data$run_id)

    comparator_data <- aggregated %>%
      filter(.data$strategy == comparator) %>%
      arrange(.data$run_id)

    # Extract aggregated costs and outcomes
    int_costs <- extract_summary_values(intervention_data, vbp_spec$cost_summary)
    comp_costs <- extract_summary_values(comparator_data, vbp_spec$cost_summary)
    int_outcomes <- extract_summary_values(intervention_data, vbp_spec$outcome_summary)
    comp_outcomes <- extract_summary_values(comparator_data, vbp_spec$outcome_summary)

    # Calculate incremental values
    delta_costs <- int_costs - comp_costs
    delta_outcomes <- int_outcomes - comp_outcomes

    # Use shared VBP calculation
    vbp_eq <- calculate_vbp_equation(
      delta_costs = delta_costs,
      delta_outcomes = delta_outcomes,
      price_values = vbp_spec$price_values,
      intervention = vbp_spec$intervention_strategy,
      comparator = comparator,
      context = "(aggregated)"
    )

    overall_equations[[comparator]] <- tibble(
      group = "overall",
      intervention = vbp_spec$intervention_strategy,
      comparator = comparator,
      outcome_difference = vbp_eq$outcome_difference,
      cost_slope = vbp_eq$cost_slope,
      cost_intercept = vbp_eq$cost_intercept,
      vbp_slope = vbp_eq$vbp_slope,
      vbp_intercept = vbp_eq$vbp_intercept
    )
  }

  list(
    equations = bind_rows(overall_equations),
    equations_by_group = all_group_equations,
    metadata = list(
      price_values_tested = vbp_spec$price_values,
      linearity_verified = TRUE,
      groups = all_groups
    )
  )
}

#' Extract Summary Values from Segment Data
#'
#' Helper function to extract summary values from segment-level results.
#' Used for group-specific analysis.
#'
#' @param data Segment data with summaries column
#' @param summary_name Name of the summary to extract
#' @return Numeric vector of summary values
#' @keywords internal
extract_segment_summary_values <- function(data, summary_name) {
  data %>%
    mutate(total = map_dbl(.data$summaries, function(s) {
      if (is.null(s)) return(NA_real_)
      s %>%
        filter(.data$summary == summary_name) %>%
        pull(.data$amount) %>%
        sum()
    })) %>%
    pull(.data$total)
}

#' Extract Summary Values from Aggregated Data
#'
#' Helper function to extract summary values from aggregated results.
#' Used for overall population analysis.
#'
#' @param data Aggregated data with summaries column
#' @param summary_name Name of the summary to extract
#' @return Numeric vector of summary values
#' @keywords internal
extract_summary_values <- function(data, summary_name) {
  data %>%
    mutate(total = map_dbl(.data$summaries, function(s) {
      s %>%
        filter(.data$summary == summary_name) %>%
        pull(.data$amount) %>%
        sum()
    })) %>%
    pull(.data$total)
}

#' Calculate VBP Price at Given WTP
#'
#' Calculates the value-based price for a given WTP threshold using
#' the VBP equation: Price = slope * WTP + intercept
#'
#' @param vbp_results Results from run_vbp()
#' @param wtp Willingness-to-pay threshold
#' @param comparator Name of the comparator strategy. If NULL (default),
#'   returns the minimum VBP across all comparators (the price that is
#'   cost-effective vs ALL alternatives).
#' @param group Group name or "overall" (default)
#' @return Numeric value representing the VBP price
#'
#' @examples
#' \dontrun{
#' # Calculate minimum VBP across all comparators at WTP of $50,000
#' vbp_price <- calculate_vbp_price(vbp_results, 50000)
#'
#' # Calculate VBP at WTP of $50,000 vs specific comparator
#' vbp_price <- calculate_vbp_price(vbp_results, 50000, comparator = "chemo")
#'
#' # Calculate VBP for a specific group
#' vbp_price_group <- calculate_vbp_price(vbp_results, 50000, "chemo", "elderly")
#' }
#'
#' @export
calculate_vbp_price <- function(vbp_results, wtp, comparator = NULL, group = "overall") {
  # Get equations based on group
  if (group == "overall") {
    equations <- vbp_results$vbp_equations
  } else {
    equations <- vbp_results$vbp_equations_by_group %>%
      filter(.data$group == !!group)
  }

  if (nrow(equations) == 0) {
    stop(sprintf("No VBP equations found for group: %s", group))
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
    stop(sprintf("No VBP equation found for comparator: %s, group: %s",
                comparator, group))
  }

  equation$vbp_slope[1] * wtp + equation$vbp_intercept[1]
}

#' Validate VBP Specification
#'
#' Validates that the VBP specification is valid for the given model.
#' Checks that strategy, variable, and summary names exist.
#'
#' @param model Parsed model object
#' @param vbp_spec VBP specification list
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_vbp_spec <- function(model, vbp_spec) {
  # Check intervention strategy exists
  if (!vbp_spec$intervention_strategy %in% model$strategies$name) {
    stop(sprintf("Intervention strategy '%s' not found in model strategies",
                vbp_spec$intervention_strategy))
  }

  # Check price variable exists
  if (!vbp_spec$price_variable %in% model$variables$name) {
    stop(sprintf("Price variable '%s' not found in model variables",
                vbp_spec$price_variable))
  }

  # Check summaries exist using validation helper
  # Note: Using model$summaries as metadata since this is during model building
  metadata_for_check <- list(summaries = model$summaries)
  check_summary_exists(vbp_spec$outcome_summary, metadata_for_check)
  check_summary_exists(vbp_spec$cost_summary, metadata_for_check)

  TRUE
}

#' Helper function for near equality checks
#' @keywords internal
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

#' Calculate VBP Equation from Cost and Outcome Data
#'
#' Core VBP calculation logic shared between run_vbp() and run_dsa() with VBP.
#' Fits a linear model to incremental costs across price levels and derives
#' the VBP equation.
#'
#' @param delta_costs Numeric vector of incremental costs at each price level
#' @param delta_outcomes Numeric vector of incremental outcomes at each price level
#' @param price_values Numeric vector of price values tested
#' @param intervention Intervention strategy name (for error messages)
#' @param comparator Comparator strategy name (for error messages)
#' @param context Additional context for error messages (e.g., "group X, run_id Y")
#'
#' @return List with: outcome_difference, cost_slope, cost_intercept, vbp_slope, vbp_intercept
#' @keywords internal
calculate_vbp_equation <- function(delta_costs,
                                    delta_outcomes,
                                    price_values,
                                    intervention,
                                    comparator,
                                    context = "") {
  # Build context string for error messages
  ctx <- if (context != "") paste0(" ", context) else ""

  # Verify outcomes don't change with price
  if (!all(near(delta_outcomes, delta_outcomes[1], tol = 1e-10))) {
    stop(sprintf("VBP error: Price affects outcomes for %s vs %s%s",
                intervention, comparator, ctx))
  }

  # Fit linear model
  cost_model <- lm(delta_costs ~ price_values)

  # Verify linearity using residuals

  predicted <- predict(cost_model)
  residuals <- delta_costs - predicted
  if (max(abs(residuals)) > 1e-8) {
    stop(sprintf("VBP error: Cost not linear with price for %s vs %s%s",
                intervention, comparator, ctx))
  }

  # Extract coefficients
  cost_slope <- coef(cost_model)[2]
  cost_intercept <- coef(cost_model)[1]

  # Calculate VBP equation coefficients
  list(
    outcome_difference = delta_outcomes[1],
    cost_slope = cost_slope,
    cost_intercept = cost_intercept,
    vbp_slope = delta_outcomes[1] / cost_slope,
    vbp_intercept = -cost_intercept / cost_slope
  )
}