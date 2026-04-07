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
      stop(glue("Custom PSM models do not support transitional values (state + destination). Invalid values: {paste(invalid_names, collapse = ', ')}. Use residency values (state only) or model start values instead."))
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

  class(model) <- c("oq_custom_psm", "oq_model")
  model
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
run_segment.oq_custom_psm <- function(segment, model, env, ...) {

  tick <- make_progress(...)
  diagnostics_policy <- get_diagnostics_policy(...)

  # Segments in analysis modes (DSA, scenarios) can override model settings
  # like timeframe or discount rates. Apply those before computing anything.
  model <- apply_setting_overrides(segment, model)

  # Derive cycle_length_days, n_cycles, and days_per_year from raw settings.
  # These are computed once here and passed explicitly to avoid mutating
  # model$settings with derived values.
  time_ctx <- compute_time_context(model$settings, dt_duration_days = get_dt_duration_days(model))

  # --- Parse phase ---
  # Custom PSMs allow 2+ states (unlike standard PSM's fixed 3).
  # model_type = "psm_custom" skips initial-probability parsing — state
  # occupancy is determined by user-defined formulas, not initial distributions.
  uneval_states <- parse_states(model$states, time_ctx$cycle_length_days, time_ctx$days_per_year, model_type = "psm_custom")
  # Variables: filtered to this segment's strategy/group, with formulas parsed
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)
  # Values: costs/outcomes per state (residency only — Custom PSM does not
  # support transitional values since there's no explicit transition structure)
  uneval_values <- parse_values(model$values, uneval_states, uneval_vars)
  value_names <- get_value_names(model$values)
  # Summaries: user-defined aggregations (e.g. "Total QALYs" = sum of named values)
  parsed_summaries <- parse_summaries(model$summaries, value_names)
  tick()

  # --- Namespace + override phase ---
  # include_cycle_zero = TRUE because state probability formulas are evaluated
  # from t=0 onward to build the full trace. (Same reasoning as standard PSM.)
  ns <- create_namespace(model, segment, include_cycle_zero = TRUE, n_cycles = time_ctx$n_cycles)

  # Parameter overrides (from DSA/PSA) inject fixed values into the namespace,
  # replacing the variable's formula.
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # --- Evaluation phase ---
  # Variables are evaluated first so they're available when state probability
  # formulas reference them (e.g. a formula might use a survival distribution
  # variable to compute occupancy).
  eval_vars <- eval_variables(uneval_vars, ns)
  tick()

  # Custom PSMs don't use initial state probabilities — state occupancy at
  # every cycle is fully determined by the per-state formulas. Set to NULL
  # for store_segment_diagnostics.
  eval_states <- NULL

  # Parse each state's probability formula from the transitions table into
  # oq_formula objects. Unlike standard PSM (which has PFS/OS endpoints),
  # Custom PSM has one formula per state. A formula of "C" (complement)
  # means "1 minus the sum of all other states" — parsed as -pi sentinel.
  state_prob_formulas <- parse_state_probability_formulas(model$transitions, unique(model$states$name))
  tick()

  # Core Custom PSM engine: evaluates each state's formula at every cycle to
  # build the trace. Complement states are resolved after all other formulas
  # are evaluated. Then evaluates value formulas weighted by state occupancy
  # (residency values only — no transition flows) and applies half-cycle
  # correction.
  calculated <- calculate_psm_custom_trace_and_values(
    state_prob_formulas, uneval_values, eval_vars, value_names,
    unique(model$states$name), time_ctx$n_cycles, model$settings$half_cycle_method
  )
  tick()

  # --- Post-processing: discount and store ---
  # Apply time-value-of-money discounting with separate cost/outcome rates.
  calculated$values_discounted <- apply_segment_discounting(
    model, eval_vars, uneval_values, calculated$values, calculated$corrected_trace,
    n_cycles = time_ctx$n_cycles, cycle_length_days = time_ctx$cycle_length_days
  )
  tick()

  # Attach all results to the segment row for downstream aggregation.
  segment <- clear_segment_diagnostics(segment)
  segment <- store_segment_diagnostics(
    segment, calculated, uneval_vars, eval_vars, eval_states,
    policy = diagnostics_policy
  )
  # Custom PSMs have no tunnel states, so the same trace is passed for both
  # the "collapsed" and "expanded" slots (both are identical).
  segment <- store_segment_traces(segment, time_ctx$cycle_length_days, time_ctx$days_per_year, calculated$trace, calculated$trace, calculated$corrected_trace, calculated$corrected_trace)
  tick()
  # Compute user-defined summary totals (e.g. sum of all QALY values)
  segment <- store_segment_summaries(segment, parsed_summaries, calculated$values, calculated$values_discounted)
  tick()
  # Calculate group weight and reorder columns to standard layout
  segment <- finalize_segment(segment, model, eval_vars)
  tick()

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

  # Calculate values (residency and model start only)
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

  # Pre-compute corrected trace for discounting_override trace() function
  corrected_trace <- switch(half_cycle_method,
    "end" = trace[2:(n_cycles + 1), , drop = FALSE],
    "life-table" = (trace[1:n_cycles, , drop = FALSE] + trace[2:(n_cycles + 1), , drop = FALSE]) / 2,
    trace[1:n_cycles, , drop = FALSE]  # "start" default
  )
  rownames(corrected_trace) <- 1:n_cycles

  list(trace = trace, values = values_matrix, corrected_trace = corrected_trace)
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
#' Calculates residency and model start values for Custom PSM models.
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

  # Pre-compute corrected trace (half-cycle corrected state probs, n_cycles rows)
  corrected_trace <- switch(half_cycle_method,
    "end" = trace[2:(n_cycles + 1), , drop = FALSE],
    "life-table" = (trace[1:n_cycles, , drop = FALSE] + trace[2:(n_cycles + 1), , drop = FALSE]) / 2,
    trace[1:n_cycles, , drop = FALSE]  # "start" default
  )

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
    } else if (!is.numeric(evaluated)) {
      accumulate_oq_error(
        define_error(glue("Value '{value_name}' must evaluate to a numeric value.")),
        context_msg = "Custom PSM value validation"
      )
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

    if (!is.na(value_row$state) && value_row$state == "decision_tree") {
      # Decision tree value - first cycle only, no state multiplication
      values_matrix[1, value_name] <- values_matrix[1, value_name] + evaluated[1]
    } else if (!is.na(value_row$state)) {
      # Residency value - multiply by pre-computed corrected state probs
      state_idx <- which(state_names == value_row$state)
      if (length(state_idx) == 1) {
        state_probs <- corrected_trace[, state_idx]
        values_matrix[, value_name] <- values_matrix[, value_name] + evaluated * state_probs
      }
    } else {
      # model start value
      values_matrix[1, value_name] <- values_matrix[1, value_name] + evaluated[1]
    }
  }

  oq_error_checkpoint()

  values_matrix
}
