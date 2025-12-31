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
#'   \item Deriving the VBP equation: Price = (ΔOutcome/cost_slope) * WTP - (cost_intercept/cost_slope)
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
    furrr::future_map(function(segment) run_segment(segment, parsed_model, ...),
               .progress = TRUE, .options = furrr::furrr_options(seed = 1)) %>%
    bind_rows()

  # Aggregate by price_level + strategy
  aggregated <- aggregate_segments(segment_results, parsed_model)

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
        parameter_overrides = purrr::map2(strategy, group, function(s, g) {
          # Check if this is the intervention strategy
          # Note: price applies to all groups of the intervention strategy
          is_intervention <- (s == vbp_spec$intervention_strategy)

          if (is_intervention) {
            list(setNames(list(vbp_spec$price_values[i]),
                         vbp_spec$price_variable))
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
        filter(strategy == vbp_spec$intervention_strategy,
               group == grp) %>%
        arrange(price_level)

      comparator_data <- segments %>%
        filter(strategy == comparator,
               group == grp) %>%
        arrange(price_level)

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

      # Verify outcomes don't change with price
      if (!all(near(delta_outcomes, delta_outcomes[1], tol = 1e-10))) {
        stop(sprintf("VBP error: Price affects outcomes for %s vs %s in group %s",
                    vbp_spec$intervention_strategy, comparator, grp))
      }

      # Fit linear model
      price_vals <- vbp_spec$price_values
      cost_model <- lm(delta_costs ~ price_vals)

      # Verify linearity using the third point
      predicted <- predict(cost_model)
      residuals <- delta_costs - predicted
      if (max(abs(residuals)) > 1e-8) {
        stop(sprintf("VBP error: Cost not linear with price for %s vs %s in group %s",
                    vbp_spec$intervention_strategy, comparator, grp))
      }

      # Extract coefficients
      cost_slope <- coef(cost_model)[2]
      cost_intercept <- coef(cost_model)[1]

      # Calculate VBP equation for this group
      group_equations[[comparator]] <- tibble(
        group = grp,
        intervention = vbp_spec$intervention_strategy,
        comparator = comparator,
        outcome_difference = delta_outcomes[1],
        cost_slope = cost_slope,
        cost_intercept = cost_intercept,
        vbp_slope = delta_outcomes[1] / cost_slope,
        vbp_intercept = -cost_intercept / cost_slope,
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
      filter(strategy == vbp_spec$intervention_strategy) %>%
      arrange(run_id)

    comparator_data <- aggregated %>%
      filter(strategy == comparator) %>%
      arrange(run_id)

    # Extract aggregated costs and outcomes
    int_costs <- extract_summary_values(intervention_data, vbp_spec$cost_summary)
    comp_costs <- extract_summary_values(comparator_data, vbp_spec$cost_summary)
    int_outcomes <- extract_summary_values(intervention_data, vbp_spec$outcome_summary)
    comp_outcomes <- extract_summary_values(comparator_data, vbp_spec$outcome_summary)

    # Calculate incremental values
    delta_costs <- int_costs - comp_costs
    delta_outcomes <- int_outcomes - comp_outcomes

    # Verify outcomes don't change with price
    if (!all(near(delta_outcomes, delta_outcomes[1], tol = 1e-10))) {
      stop(sprintf("VBP error: Price affects aggregated outcomes for %s vs %s",
                  vbp_spec$intervention_strategy, comparator))
    }

    # Fit linear model
    price_vals <- vbp_spec$price_values
    cost_model <- lm(delta_costs ~ price_vals)

    # Verify linearity
    predicted <- predict(cost_model)
    residuals <- delta_costs - predicted
    if (max(abs(residuals)) > 1e-8) {
      stop(sprintf("VBP error: Aggregated cost not linear with price for %s vs %s",
                  vbp_spec$intervention_strategy, comparator))
    }

    # Extract coefficients
    cost_slope <- coef(cost_model)[2]
    cost_intercept <- coef(cost_model)[1]

    overall_equations[[comparator]] <- tibble(
      group = "overall",
      intervention = vbp_spec$intervention_strategy,
      comparator = comparator,
      outcome_difference = delta_outcomes[1],
      cost_slope = cost_slope,
      cost_intercept = cost_intercept,
      vbp_slope = delta_outcomes[1] / cost_slope,
      vbp_intercept = -cost_intercept / cost_slope
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
    mutate(total = purrr::map_dbl(summaries, function(s) {
      if (is.null(s)) return(NA_real_)
      s %>%
        filter(summary == summary_name) %>%
        pull(amount) %>%
        sum()
    })) %>%
    pull(total)
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
    mutate(total = purrr::map_dbl(summaries, function(s) {
      s %>%
        filter(summary == summary_name) %>%
        pull(amount) %>%
        sum()
    })) %>%
    pull(total)
}

#' Calculate VBP Price at Given WTP
#'
#' Calculates the value-based price for a given WTP threshold using
#' the VBP equation: Price = slope * WTP + intercept
#'
#' @param vbp_results Results from run_vbp()
#' @param comparator Name of the comparator strategy
#' @param wtp Willingness-to-pay threshold
#' @param group Group name or "overall" (default)
#' @return Numeric value representing the VBP price
#'
#' @examples
#' \dontrun{
#' # Calculate VBP at WTP of $50,000 vs chemo
#' vbp_price <- calculate_vbp_price(vbp_results, "chemo", 50000)
#'
#' # Calculate VBP for a specific group
#' vbp_price_group <- calculate_vbp_price(vbp_results, "chemo", 50000, "elderly")
#' }
#'
#' @export
calculate_vbp_price <- function(vbp_results, comparator, wtp, group = "overall") {
  if (group == "overall") {
    equation <- vbp_results$vbp_equations %>%
      filter(comparator == !!comparator)
  } else {
    equation <- vbp_results$vbp_equations_by_group %>%
      filter(comparator == !!comparator, group == !!group)
  }

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

#' Test VBP NMB Consistency
#'
#' Tests that VBP calculations are correct by verifying that at the calculated
#' VBP price, the Net Monetary Benefit (NMB) equals zero (i.e., the intervention
#' is exactly cost-effective at the given WTP threshold).
#'
#' @return TRUE if all tests pass
#'
#' @examples
#' \dontrun{
#' test_vbp_nmb_consistency()
#' }
#'
#' @export
test_vbp_nmb_consistency <- function() {
  library(testthat)
  library(openqaly)

  # Step 1: Run VBP for checkimab model
  model <- read_model(system.file("models", "checkimab", package = "openqaly"))

  vbp_results <- run_vbp(
    model,
    price_variable = "med_cost_per_month",
    intervention_strategy = "check",
    outcome_summary = "qalys",
    cost_summary = "costs"
  )

  # Test 1: Overall VBP consistency
  wtp_test <- 50000
  comparator_test <- "chemo"

  vbp_price_overall <- calculate_vbp_price(vbp_results, comparator_test, wtp_test, "overall")

  # Run base case with calculated VBP price
  model_at_vbp <- model
  model_at_vbp$variables <- model_at_vbp$variables %>%
    mutate(formula = ifelse(
      name == "med_cost_per_month" & strategy == "check",
      as.character(vbp_price_overall),
      formula
    ))

  results_at_vbp <- run_model(model_at_vbp)

  # Calculate NMB for overall
  nmb_results <- calculate_nmb(
    results_at_vbp,
    outcome_summary = "qalys",
    cost_summary = "costs",
    interventions = "check",
    comparators = "chemo",
    wtp = wtp_test,
    groups = "overall"
  )

  total_nmb <- nmb_results %>%
    filter(grepl("check.*chemo", strategy)) %>%
    pull(amount) %>%
    sum()

  expect_true(
    abs(total_nmb) < 1e-6,
    info = sprintf("Overall NMB should be ~0 at VBP price, got: %f", total_nmb)
  )

  message(sprintf("✓ Overall VBP test passed: NMB = %.2e (effectively zero)", total_nmb))

  # Test 2: Group-level VBP consistency (if groups exist)
  if (nrow(model$groups) > 1) {
    # Test first group
    first_group <- model$groups$name[1]

    vbp_price_group <- calculate_vbp_price(vbp_results, comparator_test, wtp_test, first_group)

    # Run model with group-specific VBP price
    model_at_vbp_group <- model
    model_at_vbp_group$variables <- model_at_vbp_group$variables %>%
      mutate(formula = ifelse(
        name == "med_cost_per_month" & strategy == "check",
        as.character(vbp_price_group),
        formula
      ))

    results_at_vbp_group <- run_model(model_at_vbp_group)

    # Calculate NMB for specific group
    nmb_results_group <- calculate_nmb(
      results_at_vbp_group,
      outcome_summary = "qalys",
      cost_summary = "costs",
      interventions = "check",
      comparators = "chemo",
      wtp = wtp_test,
      groups = first_group
    )

    group_nmb <- nmb_results_group %>%
      filter(grepl("check.*chemo", strategy)) %>%
      pull(amount) %>%
      sum()

    expect_true(
      abs(group_nmb) < 1e-6,
      info = sprintf("Group %s NMB should be ~0 at VBP price, got: %f", first_group, group_nmb)
    )

    message(sprintf("✓ Group '%s' VBP test passed: NMB = %.2e (effectively zero)", first_group, group_nmb))
  }

  TRUE
}

#' Helper function for near equality checks
#' @keywords internal
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}