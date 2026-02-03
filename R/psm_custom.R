#' Parse Custom PSM Model
#'
#' Validates the structure of a Custom PSM model. Custom PSM models allow
#' arbitrary numbers of states with direct probability formulas.
#'
#' @param model An openqaly model object
#' @return The model with psm_custom class
#' @keywords internal
parse_psm_custom <- function(model) {
  # Validate states: minimum 2, no maximum
  if (nrow(model$states) < 2) {
    stop(glue("Custom PSM models require at least 2 states (got {nrow(model$states)})."))
  }

  # Validate transitions table exists
  if (is.null(model$transitions) || nrow(model$transitions) == 0) {
    stop("Custom PSM models require transitions table with state probability formulas for each state")
  }

  # Validate structure: must have state and formula columns
  required_cols <- c("state", "formula")
  missing_cols <- setdiff(required_cols, colnames(model$transitions))
  if (length(missing_cols) > 0) {
    stop(glue("Custom PSM transitions missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }

  # Validate: each state has exactly one formula
  state_names <- model$states$name
  formula_states <- model$transitions$state

  # Check for missing states
  missing_states <- setdiff(state_names, formula_states)
  if (length(missing_states) > 0) {
    stop(glue("Custom PSM: Missing probability formulas for states: {paste(missing_states, collapse = ', ')}"))
  }

  # Check for duplicate formulas
  duplicate_states <- formula_states[duplicated(formula_states)]
  if (length(duplicate_states) > 0) {
    stop(glue("Custom PSM: Duplicate probability formulas for states: {paste(unique(duplicate_states), collapse = ', ')}"))
  }

  # Check for formulas referencing non-existent states
  extra_states <- setdiff(formula_states, state_names)
  if (length(extra_states) > 0) {
    stop(glue("Custom PSM: Probability formulas reference undefined states: {paste(extra_states, collapse = ', ')}"))
  }

  # Validate values: no transitional values allowed
  if (!is.null(model$values) && nrow(model$values) > 0) {
    transitional_values <- model$values %>%
      filter(!is.na(.data$state) & !is.na(.data$destination))

    if (nrow(transitional_values) > 0) {
      invalid_names <- unique(transitional_values$name)
      stop(glue("Custom PSM models do not support transitional values (state + destination). Invalid values: {paste(invalid_names, collapse = ', ')}. Use residency values (state only) or model-level values instead."))
    }
  }

  # Check for complement operator usage
  complement_states <- character(0)
  for (i in 1:nrow(model$transitions)) {
    formula_str <- trimws(as.character(model$transitions$formula[i]))
    # Check if formula is exactly "C" (case-insensitive)
    if (toupper(formula_str) == "C") {
      complement_states <- c(complement_states, model$transitions$state[i])
    }
  }

  if (length(complement_states) > 1) {
    stop(glue("Custom PSM: Only one state can use the complement operator 'C'. Found in states: {paste(complement_states, collapse = ', ')}"))
  }

  define_object_(model, class = 'psm_custom')
}

#' Run Segment for Custom PSM Model
#'
#' Executes a single segment for a Custom PSM model. Custom PSM models
#' evaluate state probability formulas directly for each state at each cycle.
#'
#' @param segment A segment (strategy × group combination)
#' @param model The parsed Custom PSM model
#' @param env The model environment
#' @param ... Additional arguments
#' @return The segment with trace and values calculated
#' @keywords internal
#' @export
run_segment.psm_custom <- function(segment, model, env, ...) {

  # Capture the extra arguments provided to function
  dots <- list(...)

  # Apply setting overrides if present (DSA mode)
  model <- apply_setting_overrides(segment, model)

  # Calculate n_cycles AFTER apply_setting_overrides to ensure DSA timeframe overrides work correctly
  model$settings$cycle_length_days <- get_cycle_length_days(model$settings)
  model$settings$n_cycles <- get_n_cycles(model$settings)

  # Parse the specification tables provided for states and variables
  uneval_states <- parse_states(model$states, model$settings$cycle_length_days, model$settings$days_per_year, model_type = "psm_custom")
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)
  uneval_values <- parse_values(model$values, uneval_states, uneval_vars)

  # Determine model_value_names safely for parse_summaries
  model_value_names <- character(0)
  if (nrow(model$values) > 0 && "name" %in% colnames(model$values)) {
    valid_names <- model$values$name[!is.na(model$values$name)]
    if (length(valid_names) > 0) {
      model_value_names <- unique(valid_names)
    }
  }

  # Parse summaries if they exist
  if (!is.null(model$summaries) && nrow(model$summaries) > 0) {
    parsed_summaries <- parse_summaries(model$summaries, model_value_names)
  } else {
    parsed_summaries <- tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      values = character(0),
      type = character(0),
      wtp = numeric(0),
      parsed_values = list()
    )
  }

  # Create a namespace which will contain evaluated variables
  # For PSM Custom, include cycle 0 for trace calculation (state probabilities at t=0)
  ns <- create_namespace(model, segment, include_cycle_zero = TRUE)

  # Apply parameter overrides if present (PSA, DSA, or VBP mode)
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # Evaluate variables
  eval_vars <- eval_variables(uneval_vars, ns)

  # Custom PSM doesn't use initial state probabilities (determined by formulas at cycle 0)
  eval_states <- NULL

  # Parse state probability formulas from transitions
  state_prob_formulas <- parse_state_probability_formulas(model$transitions, unique(model$states$name))

  # Calculate Custom PSM trace and values
  calculated_trace_and_values <- calculate_psm_custom_trace_and_values(
    state_prob_formulas,
    uneval_values,
    eval_vars,
    model_value_names,
    unique(model$states$name),
    model$settings$n_cycles,
    model$settings$half_cycle_method
  )

  # Apply discounting to values
  n_cycles <- model$settings$n_cycles
  discount_cost <- model$settings$discount_cost
  discount_outcomes <- model$settings$discount_outcomes

  # Calculate cycle length in years for discounting
  cycle_length_days <- model$settings$cycle_length_days
  days_per_year <- if (!is.null(model$settings$days_per_year)) model$settings$days_per_year else 365.25
  cycle_length_years <- cycle_length_days / days_per_year

  # Calculate discount factors
  discount_factors_cost <- calculate_discount_factors(n_cycles, discount_cost, cycle_length_years)
  discount_factors_outcomes <- calculate_discount_factors(n_cycles, discount_outcomes, cycle_length_years)

  # Get value types from uneval_values
  type_mapping <- setNames(uneval_values$type, uneval_values$name)
  type_mapping <- type_mapping[!is.na(names(type_mapping))]

  # Apply discounting to get discounted values
  calculated_trace_and_values$values_discounted <- apply_discounting(
    calculated_trace_and_values$values,
    discount_factors_cost,
    discount_factors_outcomes,
    type_mapping
  )

  # Create the object to return
  # In override mode (PSA/DSA), store only parameter overrides instead of full eval_vars
  if ("parameter_overrides" %in% names(segment)) {
    # Override mode: parameter_overrides already in segment from resample() or DSA
    # Don't store heavy objects: eval_vars, uneval_vars, initial_state, trace_and_values
  } else {
    # Base case mode: keep current behavior
    segment$uneval_vars <- list(uneval_vars)
    segment$eval_vars <- list(eval_vars)
    segment$inital_state <- list(eval_states)
    segment$trace_and_values <- list(calculated_trace_and_values)
  }
  # Add time variables to the trace
  # Generate time columns based on the actual cycle numbers from row names
  n_trace_rows <- nrow(calculated_trace_and_values$trace)

  # The row names indicate the actual cycle numbers (0, 1, 2, ...)
  cycle_numbers <- as.numeric(rownames(calculated_trace_and_values$trace))
  if (any(is.na(cycle_numbers))) {
    # If row names aren't numeric, use sequence
    cycle_numbers <- seq(0, n_trace_rows - 1)
  }

  # Get cycle length info from model settings
  cycle_length_days <- model$settings$cycle_length_days
  days_per_year <- if (!is.null(model$settings$days_per_year)) model$settings$days_per_year else 365.25

  if (!is.na(cycle_length_days) && cycle_length_days > 0) {
    # Generate time columns based on cycle numbers and cycle length
    # Use consistent conversion factors with time.R
    days_per_month <- days_per_year / 12
    time_vars_df <- data.frame(
      cycle = cycle_numbers,
      day = cycle_numbers * cycle_length_days,
      week = cycle_numbers * cycle_length_days / 7,
      month = cycle_numbers * cycle_length_days / days_per_month,
      year = cycle_numbers * cycle_length_days / days_per_year
    )

    # Ensure row names match before cbind
    rownames(time_vars_df) <- rownames(calculated_trace_and_values$trace)

    # Combine time columns with trace matrix
    trace_with_time <- cbind(time_vars_df, calculated_trace_and_values$trace)
  } else {
    # No cycle length info, just add cycle numbers
    time_vars_df <- data.frame(cycle = cycle_numbers)
    rownames(time_vars_df) <- rownames(calculated_trace_and_values$trace)
    trace_with_time <- cbind(time_vars_df, calculated_trace_and_values$trace)
  }

  segment$collapsed_trace <- list(trace_with_time)
  # PSM doesn't have expanded states, so expanded_trace is the same as collapsed_trace
  segment$expanded_trace <- list(trace_with_time)

  # Calculate summaries for both discounted and undiscounted values
  if (!is.null(parsed_summaries)) {
    summaries_undiscounted <- calculate_summaries(
      parsed_summaries,
      calculated_trace_and_values$values
    )
    summaries_discounted <- calculate_summaries(
      parsed_summaries,
      calculated_trace_and_values$values_discounted
    )
    segment$summaries <- list(summaries_undiscounted)
    segment$summaries_discounted <- list(summaries_discounted)
  } else {
    empty_summary <- tibble(summary = character(), value = character(), amount = numeric())
    segment$summaries <- list(empty_summary)
    segment$summaries_discounted <- list(empty_summary)
  }

  # Calculate segment weight
  segment$weight <- calculate_segment_weight(segment, model, eval_vars)

  # Reorder columns to have strategy, group, weight first
  col_order <- c("strategy", "group", "weight")
  other_cols <- setdiff(names(segment), col_order)
  segment <- segment[, c(col_order, other_cols)]

  segment
}

#' Parse State Probability Formulas
#'
#' Extracts state probability formulas from the transitions table for Custom PSM.
#'
#' @param transitions The transitions dataframe with state and formula columns
#' @param state_names Vector of state names
#' @return Named list of oq_formula objects by state
#' @keywords internal
parse_state_probability_formulas <- function(transitions, state_names) {
  # Create a named list of formulas by state
  formulas <- setNames(
    vector("list", length(state_names)),
    state_names
  )

  for (i in 1:nrow(transitions)) {
    state <- transitions$state[i]
    formula_str <- as.character(transitions$formula[i])
    formulas[[state]] <- as.oq_formula(formula_str)
  }

  formulas
}

#' Calculate Custom PSM Trace and Values
#'
#' Calculates the trace (state occupancy) and values for a Custom PSM model.
#' State probabilities are evaluated directly from formulas (vectorized),
#' with special handling for the complement operator "C" (evaluates to -pi).
#'
#' @param state_prob_formulas Named list of oq_formula objects by state
#' @param uneval_values Unevaluated values specifications
#' @param namespace Namespace with evaluated variables (must include cycle 0)
#' @param value_names Vector of value names
#' @param state_names Vector of state names
#' @param n_cycles Number of cycles to simulate
#' @param half_cycle_method Half-cycle correction method
#' @return A list containing trace matrix and values matrix
#' @keywords internal
calculate_psm_custom_trace_and_values <- function(
  state_prob_formulas,
  uneval_values,
  namespace,
  value_names,
  state_names,
  n_cycles,
  half_cycle_method = "start"
) {
  tol <- 10 * sqrt(.Machine$double.eps)
  n_rows <- n_cycles + 1  # cycles 0 to n_cycles

  # Initialize trace matrix
  trace <- matrix(NA_real_, nrow = n_rows, ncol = length(state_names))
  colnames(trace) <- state_names
  rownames(trace) <- 0:n_cycles

  # Evaluate each state formula once (vectorized), store in matrix
  # C evaluates to -pi as sentinel for complement
  for (i in seq_along(state_names)) {
    formula <- state_prob_formulas[[state_names[i]]]
    result <- eval_formula(formula, namespace)

    if (is_oq_error(result)) {
      accumulate_oq_error(result, context_msg = glue("State probability for '{state_names[i]}'"))
      result <- rep(0, n_rows)
    }

    # Handle scalar -> replicate to vector
    if (length(result) == 1) {
      result <- rep(result, n_rows)
    }

    # Check length matches expected
    if (length(result) != n_rows) {
      accumulate_oq_error(
        define_error(glue("State probability for '{state_names[i]}' has length {length(result)}, expected {n_rows}")),
        context_msg = "Custom PSM probability validation"
      )
      result <- rep(0, n_rows)
    }

    trace[, i] <- result
  }

  # Find complement cells (where value == -pi)
  complement_mask <- trace == -pi

  # Check: multiple -pi in same row is an error
  complement_per_row <- rowSums(complement_mask)
  if (any(complement_per_row > 1)) {
    bad_rows <- which(complement_per_row > 1)
    accumulate_oq_error(
      define_error(glue("Multiple complement (C) values in cycles: {paste(bad_rows - 1, collapse=', ')}")),
      context_msg = "Custom PSM complement validation"
    )
  }

  # Calculate complements vectorized
  # Zero out -pi cells, compute row sums, then replace -pi with 1 - row_sum
  trace[complement_mask] <- 0
  row_sums <- rowSums(trace)
  trace[complement_mask] <- 1 - row_sums[row(trace)[complement_mask]]

  # Validate bounds
  out_of_bounds <- which(trace < -tol | trace > 1 + tol, arr.ind = TRUE)
  if (nrow(out_of_bounds) > 0) {
    for (k in 1:nrow(out_of_bounds)) {
      r <- out_of_bounds[k, 1]
      c <- out_of_bounds[k, 2]
      accumulate_oq_error(
        define_error(glue("State '{state_names[c]}' probability out of bounds at cycle {r - 1}: {trace[r, c]}")),
        context_msg = "Custom PSM probability validation"
      )
    }
    # Clamp values while preserving matrix structure
    trace[] <- pmax(0, pmin(1, trace))
  }

  # Validate row sums
  row_sums <- rowSums(trace)
  bad_rows <- which(abs(row_sums - 1.0) > tol)
  for (r in bad_rows) {
    accumulate_oq_error(
      define_error(glue("State probabilities do not sum to 1 at cycle {r - 1} (sum = {row_sums[r]})")),
      context_msg = "Custom PSM trace validation"
    )
  }

  oq_error_checkpoint()

  # Calculate values (residency and model-level only)
  # Values are for cycles 1:n_cycles, so prune cycle 0 from namespace
  values_ns <- prune_namespace_cycle_zero(namespace)
  values_matrix <- calculate_psm_custom_values(
    uneval_values,
    values_ns,
    trace,
    value_names,
    state_names,
    n_cycles,
    half_cycle_method
  )

  list(trace = trace, values = values_matrix)
}

#' Prune Cycle Zero from Namespace
#'
#' Removes the first row (cycle 0) from the namespace data frame.
#' Used to prepare namespace for values calculation which only uses cycles 1:n_cycles.
#'
#' @param namespace A namespace object containing cycle 0
#' @return A namespace object with cycle 0 removed from df
#' @keywords internal
prune_namespace_cycle_zero <- function(namespace) {
  pruned <- clone_namespace(namespace)
  pruned$df <- pruned$df[-1, , drop = FALSE]
  pruned
}

#' Calculate Custom PSM Values
#'
#' Calculates residency and model-level values for Custom PSM models.
#' Transitional values are not supported.
#'
#' @param uneval_values Unevaluated values specifications
#' @param namespace Evaluated variables namespace
#' @param trace State occupancy trace matrix
#' @param value_names Vector of value names
#' @param state_names Vector of state names
#' @param n_cycles Number of cycles
#' @param half_cycle_method Half-cycle correction method
#' @return A matrix of calculated values
#' @keywords internal
calculate_psm_custom_values <- function(
  uneval_values,
  namespace,
  trace,
  value_names,
  state_names,
  n_cycles,
  half_cycle_method = "start"
) {
  # Initialize values matrix
  values_matrix <- matrix(0, nrow = n_cycles, ncol = length(value_names))
  colnames(values_matrix) <- value_names
  rownames(values_matrix) <- 1:n_cycles

  if (length(value_names) == 0 || nrow(uneval_values) == 0) {
    return(values_matrix)
  }

  # Process each value (vectorized evaluation)
  for (i in 1:nrow(uneval_values)) {
    value_row <- uneval_values[i, ]
    value_name <- value_row$name

    if (is.na(value_name) || !(value_name %in% value_names)) {
      next
    }

    # Validate: no transitional values
    if (!is.na(value_row$destination)) {
      accumulate_oq_error(
        define_error(glue("Transitional value '{value_name}' not allowed in Custom PSM")),
        context_msg = "Custom PSM value validation"
      )
      next
    }

    # Evaluate formula once (returns vector)
    evaluated <- eval_formula(value_row$formula[[1]], namespace)

    if (is_oq_error(evaluated)) {
      accumulate_oq_error(evaluated, context_msg = glue("Value '{value_name}'"))
      evaluated <- rep(0, n_cycles)
    }

    # Handle scalar -> replicate to vector
    if (length(evaluated) == 1) {
      evaluated <- rep(evaluated, n_cycles)
    }

    # Check length
    if (length(evaluated) != n_cycles) {
      accumulate_oq_error(
        define_error(glue("Value '{value_name}' has length {length(evaluated)}, expected {n_cycles}")),
        context_msg = "Custom PSM value validation"
      )
      evaluated <- rep(0, n_cycles)
    }

    if (!is.na(value_row$state)) {
      # Residency value - multiply by state probs
      state_idx <- which(state_names == value_row$state)
      if (length(state_idx) == 1) {
        # Get state probs based on half-cycle method
        state_probs <- if (half_cycle_method == "end") {
          trace[2:(n_cycles + 1), state_idx]
        } else if (half_cycle_method == "life-table") {
          avg <- (trace[1:n_cycles, state_idx] + trace[2:(n_cycles + 1), state_idx]) / 2
          avg[n_cycles] <- trace[n_cycles + 1, state_idx]
          avg
        } else {  # "start"
          trace[1:n_cycles, state_idx]
        }
        values_matrix[, value_name] <- values_matrix[, value_name] + evaluated * state_probs
      }
    } else {
      # Model-level value
      values_matrix[, value_name] <- values_matrix[, value_name] + evaluated
    }
  }

  oq_error_checkpoint()

  values_matrix
}
