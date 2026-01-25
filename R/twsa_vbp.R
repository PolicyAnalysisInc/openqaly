#' TWSA Value-Based Pricing Integration
#'
#' Functions for integrating Value-Based Pricing (VBP) analysis with
#' two-way sensitivity analysis (TWSA).
#'
#' @name twsa_vbp
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup summarize slice n
#' @importFrom tibble tibble
#' @importFrom glue glue
NULL

#' Validate TWSA VBP Specification
#'
#' Validates VBP parameters for TWSA analysis.
#'
#' @param model Parsed model object
#' @param vbp_price_variable Name of price variable
#' @param vbp_intervention Intervention strategy name
#' @param vbp_outcome_summary Outcome summary name
#' @param vbp_cost_summary Cost summary name
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_twsa_vbp_spec <- function(model,
                                    vbp_price_variable,
                                    vbp_intervention,
                                    vbp_outcome_summary,
                                    vbp_cost_summary) {

  # Check price variable exists
  if (!(vbp_price_variable %in% model$variables$name)) {
    stop(glue("VBP price variable '{vbp_price_variable}' not found in model variables"),
         call. = FALSE)
  }

  # Check intervention strategy exists
  strategy_names <- model$strategies$name
  if (!(vbp_intervention %in% strategy_names)) {
    stop(glue("VBP intervention strategy '{vbp_intervention}' not found. ",
              "Available strategies: {paste(strategy_names, collapse=', ')}"),
         call. = FALSE)
  }

  # Check outcome summary exists
  summary_names <- model$summaries$name
  if (!(vbp_outcome_summary %in% summary_names)) {
    stop(glue("VBP outcome summary '{vbp_outcome_summary}' not found. ",
              "Available summaries: {paste(summary_names, collapse=', ')}"),
         call. = FALSE)
  }

  # Check cost summary exists
  if (!(vbp_cost_summary %in% summary_names)) {
    stop(glue("VBP cost summary '{vbp_cost_summary}' not found. ",
              "Available summaries: {paste(summary_names, collapse=', ')}"),
         call. = FALSE)
  }

  TRUE
}

#' Build TWSA VBP Segments
#'
#' Creates segments for TWSA with VBP analysis. Expands each TWSA run
#' with 3 VBP price levels.
#'
#' @param model Parsed model object
#' @param vbp_spec VBP specification list
#' @return Tibble with segments including vbp_price_level column
#' @keywords internal
build_twsa_vbp_segments <- function(model, vbp_spec) {
  # Build base TWSA segments
  twsa_segments <- build_twsa_segments(model)

  # Get VBP parameters
  price_variable <- vbp_spec$price_variable
  intervention <- vbp_spec$intervention_strategy
  price_values <- vbp_spec$price_values

  all_segments <- list()

  for (seg_idx in seq_len(nrow(twsa_segments))) {
    segment <- twsa_segments[seg_idx, ]
    seg_strategy <- segment$strategy[[1]]

    # For each VBP price level
    for (price_level in seq_along(price_values)) {
      price_value <- price_values[price_level]

      # Copy segment
      vbp_seg <- segment

      # Add VBP columns
      vbp_seg$vbp_price_level <- price_level
      vbp_seg$price_value <- price_value

      # If this is the intervention strategy, add price override
      if (seg_strategy == intervention) {
        # Merge VBP price override into existing parameter overrides
        existing_overrides <- vbp_seg$parameter_overrides[[1]]
        existing_overrides[[price_variable]] <- price_value
        vbp_seg$parameter_overrides[[1]] <- existing_overrides
      }

      all_segments[[length(all_segments) + 1]] <- vbp_seg
    }
  }

  bind_rows(all_segments)
}

#' Analyze TWSA VBP Results
#'
#' Calculates VBP equations for each TWSA run.
#'
#' @param segments Segment results
#' @param aggregated Aggregated results
#' @param vbp_spec VBP specification
#' @param twsa_metadata TWSA metadata tibble
#' @param model Parsed model object
#' @return Tibble with VBP equations per run_id
#' @keywords internal
analyze_twsa_vbp_results <- function(segments, aggregated, vbp_spec, twsa_metadata, model) {
  # Get parameters
  intervention <- vbp_spec$intervention_strategy
  outcome_summary <- vbp_spec$outcome_summary
  cost_summary <- vbp_spec$cost_summary
  price_values <- vbp_spec$price_values

  # Get comparator strategies (all non-intervention)
  all_strategies <- unique(aggregated$strategy)
  comparators <- setdiff(all_strategies, intervention)

  # Get unique groups
  all_groups <- unique(aggregated$group)

  # Get unique run_ids from metadata
  run_ids <- unique(twsa_metadata$run_id)

  equations <- list()

  for (rid in run_ids) {
    for (comparator in comparators) {
      for (grp in all_groups) {
        # Extract intervention data for this run_id and group
        intervention_data <- aggregated %>%
          filter(
            .data$run_id == rid,
            .data$group == grp,
            .data$strategy == intervention
          ) %>%
          arrange(.data$vbp_price_level)

        # Extract comparator data for this run_id and group
        comparator_data <- aggregated %>%
          filter(
            .data$run_id == rid,
            .data$group == grp,
            .data$strategy == comparator
          ) %>%
          arrange(.data$vbp_price_level)

        # Need data for all 3 price levels for both strategies
        if (nrow(intervention_data) < 3 || nrow(comparator_data) < 3) next

        # Extract costs and outcomes using shared helper function
        int_costs <- extract_summary_values(intervention_data, cost_summary)
        comp_costs <- extract_summary_values(comparator_data, cost_summary)
        int_outcomes <- extract_summary_values(intervention_data, outcome_summary)
        comp_outcomes <- extract_summary_values(comparator_data, outcome_summary)

        # Calculate incremental values
        delta_costs <- int_costs - comp_costs
        delta_outcomes <- int_outcomes - comp_outcomes

        # Calculate VBP equation using shared function
        equation <- tryCatch({
          calculate_vbp_equation(
            delta_costs = delta_costs,
            delta_outcomes = delta_outcomes,
            price_values = price_values,
            intervention = intervention,
            comparator = comparator,
            context = sprintf("in group %s, run_id %d", grp, rid)
          )
        }, error = function(e) {
          list(vbp_slope = NA_real_, vbp_intercept = NA_real_)
        })

        equations[[length(equations) + 1]] <- tibble(
          run_id = rid,
          comparator = comparator,
          group = grp,
          vbp_slope = equation$vbp_slope,
          vbp_intercept = equation$vbp_intercept
        )
      }
    }
  }

  if (length(equations) == 0) {
    return(tibble(
      run_id = integer(),
      comparator = character(),
      group = character(),
      vbp_slope = numeric(),
      vbp_intercept = numeric()
    ))
  }

  # Join with TWSA metadata
  result <- bind_rows(equations) %>%
    left_join(
      twsa_metadata %>% select("run_id", "twsa_name", "x_value", "y_value"),
      by = "run_id"
    )

  result
}

#' Calculate TWSA VBP Price
#'
#' Calculates the value-based price for a specific TWSA run at a given WTP.
#'
#' @param results TWSA results object with VBP
#' @param wtp Willingness-to-pay threshold
#' @param comparator Comparator strategy name (NULL for minimum across all)
#' @param run_id Specific run_id (default: 1 for base case)
#' @param group Group name (default: "_aggregated")
#' @return Numeric VBP price
#' @export
calculate_twsa_vbp_price <- function(results, wtp,
                                      comparator = NULL,
                                      run_id = 1,
                                      group = "_aggregated") {
  if (is.null(results$twsa_vbp_equations)) {
    stop("No VBP equations found. Run run_twsa() with vbp_price_variable specified.",
         call. = FALSE)
  }

  equations <- results$twsa_vbp_equations %>%
    filter(.data$run_id == !!run_id, .data$group == !!group)

  if (!is.null(comparator)) {
    equations <- equations %>%
      filter(.data$comparator == !!comparator)
  }

  if (nrow(equations) == 0) {
    stop("No VBP equation found for specified run_id/comparator/group",
         call. = FALSE)
  }

  # Calculate price for each comparator
  prices <- equations$vbp_slope * wtp + equations$vbp_intercept

  # Return minimum price (most conservative)
  if (is.null(comparator)) {
    return(min(prices, na.rm = TRUE))
  } else {
    return(prices[1])
  }
}
