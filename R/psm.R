parse_psm <- function(model) {
  # Validate PSM models have exactly 3 states
  if (nrow(model$states) != 3) {
    stop(glue("PSM models require exactly 3 states (got {nrow(model$states)}). States should represent progression-free, post-progression, and dead."))
  }

  # Validate PSM models have transitions table
  if (is.null(model$transitions) || nrow(model$transitions) == 0) {
    stop("PSM models require transitions table with PFS and OS endpoint definitions")
  }

  # For PSM models, transitions contain survival endpoint definitions
  # Validate PSM-specific requirements
  if (!is.null(model$transitions) && nrow(model$transitions) > 0) {
    # Check required columns for PSM transitions
    required_cols <- c("endpoint", "time_unit", "formula")
    missing_cols <- setdiff(required_cols, colnames(model$transitions))
    if (length(missing_cols) > 0) {
      stop(glue("PSM transitions missing required columns: {paste(missing_cols, collapse = ', ')}"))
    }

    # Check for required endpoints
    endpoints <- toupper(model$transitions$endpoint)
    if (!"PFS" %in% endpoints) {
      stop("PSM model missing PFS endpoint definition")
    }
    if (!"OS" %in% endpoints) {
      stop("PSM model missing OS endpoint definition")
    }

    # Check for duplicate PFS/OS definitions
    if (sum(endpoints == "PFS") > 1) {
      stop("PSM model has multiple PFS endpoint definitions")
    }
    if (sum(endpoints == "OS") > 1) {
      stop("PSM model has multiple OS endpoint definitions")
    }
  }

  define_object_(model, class = 'psm')
}

#' @export
run_segment.psm <- function(segment, model, env, ...) {

  # Capture the extra arguments provided to function
  dots <- list(...)

  # Apply setting overrides if present (DSA mode)
  model <- apply_setting_overrides(segment, model)

  # Calculate n_cycles AFTER apply_setting_overrides to ensure DSA timeframe overrides work correctly
  model$settings$cycle_length_days <- get_cycle_length_days(model$settings)
  model$settings$n_cycles <- get_n_cycles(model$settings)

  # Parse the specification tables provided for states and variables
  uneval_states <- parse_states(model$states, model$settings$cycle_length_days, model$settings$days_per_year, model_type = "psm")
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
      parsed_values = list()
    )
  }
  
  # Create a namespace which will contain evaluated variables
  ns <- create_namespace(model, segment)

  # Apply parameter overrides if present (PSA, DSA, or VBP mode)
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # Evaluate variables
  eval_vars <- eval_variables(uneval_vars, ns)

  # PSM doesn't use initial state probabilities (determined by survival functions)
  # Set to NULL for PSM
  eval_states <- NULL
  
  # For PSM, parse and evaluate survival distributions from transitions
  survival_distributions <- parse_and_eval_psm_transitions(model$transitions, segment, eval_vars)
  
  # Calculate PSM trace and values
  calculated_trace_and_values <- calculate_psm_trace_and_values(
    survival_distributions,
    uneval_values,
    eval_vars,
    model_value_names,
    unique(model$states$name),
    model$settings$n_cycles,
    model$settings$half_cycle_method
  )

  # Apply discounting to values
  n_cycles <- model$settings$n_cycles
  discount_cost <- if (!is.null(model$settings$discount_cost)) model$settings$discount_cost else 0.035
  discount_outcomes <- if (!is.null(model$settings$discount_outcomes)) model$settings$discount_outcomes else 0.035

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

#' Parse and Evaluate PSM Transitions
#'
#' Parses the transitions table for PSM models and evaluates the survival distributions.
#' For PSM models, transitions contain endpoint definitions with time units and formulas
#' that reference variables containing the actual survival distributions.
#'
#' @param transitions The transitions dataframe containing endpoint definitions
#' @param segment The current segment
#' @param namespace The evaluated variables namespace
#'
#' @return A list containing pfs and os survival distributions
#' @keywords internal
parse_and_eval_psm_transitions <- function(transitions, segment, namespace) {

  if (is.null(transitions) || nrow(transitions) == 0) {
    error_msg <- "PSM models require transitions table with PFS and OS endpoint definitions."
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM transitions parsing")
    oq_error_checkpoint()
    return(list(pfs = NULL, os = NULL))
  }
  
  # Extract PFS and OS definitions
  pfs_def <- transitions %>%
    filter(toupper(.data$endpoint) == "PFS")
  os_def <- transitions %>%
    filter(toupper(.data$endpoint) == "OS")
  
  # Validate we have exactly one PFS and one OS definition
  if (nrow(pfs_def) == 0) {
    error_msg <- "PSM model missing PFS endpoint definition in transitions."
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM transitions validation")
    oq_error_checkpoint()
    return(list(pfs = NULL, os = NULL))
  }
  if (nrow(os_def) == 0) {
    error_msg <- "PSM model missing OS endpoint definition in transitions."
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM transitions validation")
    oq_error_checkpoint()
    return(list(pfs = NULL, os = NULL))
  }
  if (nrow(pfs_def) > 1) {
    error_msg <- glue("PSM model has multiple PFS endpoint definitions ({nrow(pfs_def)}). Only one is allowed.")
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM transitions validation")
    oq_error_checkpoint()
  }
  if (nrow(os_def) > 1) {
    error_msg <- glue("PSM model has multiple OS endpoint definitions ({nrow(os_def)}). Only one is allowed.")
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM transitions validation")
    oq_error_checkpoint()
  }
  
  # Take first definition if multiple exist
  pfs_def <- pfs_def[1, ]
  os_def <- os_def[1, ]
  
  # Parse and evaluate the survival distribution formulas
  pfs_dist <- evaluate_psm_endpoint(pfs_def, "PFS", namespace)
  os_dist <- evaluate_psm_endpoint(os_def, "OS", namespace)

  # Check for any errors accumulated during endpoint evaluation
  oq_error_checkpoint()

  list(pfs = pfs_dist, os = os_dist)
}

#' Evaluate PSM Endpoint Definition
#'
#' Evaluates a single PSM endpoint definition to create a survival distribution.
#' The formula in the endpoint definition should reference a variable that contains
#' the survival distribution (e.g., "pfs_dist"), which can vary by strategy/group.
#'
#' @param endpoint_def A single row from transitions containing endpoint definition
#' @param endpoint_name The name of the endpoint (PFS or OS) for error messages
#' @param namespace The evaluated variables namespace
#'
#' @return A survival distribution object
#' @keywords internal
evaluate_psm_endpoint <- function(endpoint_def, endpoint_name, namespace) {
  
  # Convert formula to oq_formula
  formula <- as.oq_formula(as.character(endpoint_def$formula))
  
  # Evaluate the formula - this should reference a variable containing the survival distribution
  dist <- eval_formula(formula, namespace)
  
  # Check for errors
  if (is_oq_error(dist)) {
    accumulate_oq_error(dist, context_msg = glue("Evaluation of {endpoint_name} survival distribution"))
    return(NULL)
  }
  
  # Validate the result is a survival distribution
  if (!inherits(dist, "surv_dist")) {
    error_msg <- glue("{endpoint_name} formula must evaluate to a survival distribution object (got {class(dist)[1]}).")
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM endpoint validation")
    return(NULL)
  }
  
  # Store the time unit for later use
  attr(dist, "time_unit") <- endpoint_def$time_unit
  
  dist
}

#' Calculate PSM Trace and Values
#'
#' Calculates the trace (state occupancy over time) and values for a partitioned survival model.
#' Uses PFS and OS distributions to determine state probabilities.
#'
#' @param survival_distributions List containing pfs and os distributions
#' @param uneval_values Unevaluated values specifications
#' @param namespace Evaluated variables namespace
#' @param value_names Vector of value names
#' @param state_names Vector of state names (should be 3 states)
#' @param n_cycles Number of cycles to simulate
#'
#' @return A list containing trace matrix and values matrix
#' @keywords internal
calculate_psm_trace_and_values <- function(survival_distributions, uneval_values, namespace, value_names, state_names, n_cycles, half_cycle_method = "start") {

  # Validate we have exactly 3 states
  if (length(state_names) != 3) {
    error_msg <- glue("PSM models require exactly 3 states (got {length(state_names)}). States should represent progression-free, post-progression, and dead.")
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM state validation")
    oq_error_checkpoint()
  }
  
  # Extract distributions
  pfs_dist <- survival_distributions$pfs
  os_dist <- survival_distributions$os
  
  if (is.null(pfs_dist) || is.null(os_dist)) {
    # Return empty results if distributions are missing
    empty_trace <- matrix(0, nrow = n_cycles + 1, ncol = 3)
    colnames(empty_trace) <- state_names
    rownames(empty_trace) <- 0:n_cycles
    
    empty_values <- matrix(0, nrow = n_cycles, ncol = length(value_names))
    colnames(empty_values) <- value_names
    rownames(empty_values) <- 1:n_cycles
    
    return(list(trace = empty_trace, values = empty_values))
  }
  
  # Calculate survival probabilities for each cycle
  # Need to convert cycle times to the appropriate time unit for each distribution
  cycle_times <- 0:n_cycles
  
  # Get time unit for each distribution
  pfs_time_unit <- attr(pfs_dist, "time_unit")
  os_time_unit <- attr(os_dist, "time_unit")
  
  # Convert cycle times to appropriate units
  # Use the namespace to access time conversion variables
  pfs_times <- convert_cycles_to_time_unit(cycle_times, pfs_time_unit, namespace)
  os_times <- convert_cycles_to_time_unit(cycle_times, os_time_unit, namespace)
  
  # Get survival probabilities
  pfs_surv <- surv_prob(pfs_dist, pfs_times)
  os_surv <- surv_prob(os_dist, os_times)
  
  # Calculate state probabilities
  # State 1 (progression-free): min(PFS(t), OS(t))
  # State 3 (dead): 1 - OS(t)
  # State 2 (post-progression): 1 - State1 - State3
  prob_pfs <- pmin(pfs_surv, os_surv)
  prob_dead <- 1 - os_surv
  prob_post_prog <- pmax(0, 1 - prob_pfs - prob_dead)  # Ensure non-negative
  
  # Create trace matrix
  trace <- matrix(0, nrow = n_cycles + 1, ncol = 3)
  trace[, 1] <- prob_pfs
  trace[, 2] <- prob_post_prog  
  trace[, 3] <- prob_dead
  
  colnames(trace) <- state_names
  rownames(trace) <- 0:n_cycles

  # Validate trace probabilities
  # Use robust tolerance based on machine epsilon (following R best practices)
  # For probabilities, use absolute tolerance since comparing to fixed value (1.0)
  tol <- 10 * sqrt(.Machine$double.eps)  # ~1.49e-07

  for (cycle_idx in 1:(n_cycles + 1)) {
    row_sum <- sum(trace[cycle_idx, ])
    if (abs(row_sum - 1.0) > tol) {
      error_msg <- glue("PSM trace probabilities do not sum to 1 at cycle {cycle_idx - 1} (sum = {row_sum})")
      accumulate_oq_error(define_error(error_msg), context_msg = "PSM trace validation")
    }
    # Allow small floating point errors beyond [0, 1] bounds
    if (any(trace[cycle_idx, ] < -tol | trace[cycle_idx, ] > 1 + tol)) {
      error_msg <- glue("PSM trace contains invalid probabilities at cycle {cycle_idx - 1} (must be in [0, 1])")
      accumulate_oq_error(define_error(error_msg), context_msg = "PSM trace validation")
    }
  }
  oq_error_checkpoint()

  # Calculate transition probabilities for transitional values
  # Transitions: PFS -> Post-prog, Post-prog -> Dead
  trans_pfs_to_pp <- c(0, diff(-prob_pfs))  # Decrease in PFS
  trans_pp_to_dead <- c(0, diff(prob_dead))  # Increase in dead
  
  # Calculate values
  values_matrix <- calculate_psm_values(
    uneval_values,
    namespace,
    trace,
    trans_pfs_to_pp,
    trans_pp_to_dead,
    value_names,
    state_names,
    n_cycles,
    half_cycle_method
  )
  
  list(trace = trace, values = values_matrix)
}

#' Convert Cycles to Time Unit
#'
#' Converts cycle numbers to the appropriate time unit for survival distributions.
#'
#' @param cycles Vector of cycle numbers
#' @param time_unit The target time unit (days/weeks/months/years)
#' @param namespace The namespace containing time variables
#'
#' @return Vector of times in the specified unit
#' @keywords internal
convert_cycles_to_time_unit <- function(cycles, time_unit, namespace) {
  
  if (is.null(time_unit)) {
    warning("No time unit specified for survival distribution. Assuming cycles.")
    return(cycles)
  }
  
  time_unit <- tolower(time_unit)
  
  # For cycle 0, time is always 0
  # For other cycles, use the appropriate time variable from namespace
  result <- numeric(length(cycles))
  
  for (i in seq_along(cycles)) {
    if (cycles[i] == 0) {
      result[i] <- 0
    } else {
      # Get the time value for this cycle from the namespace
      # The namespace df should have time variables for each cycle
      cycle_row <- namespace$df[namespace$df$cycle == cycles[i], ]
      if (nrow(cycle_row) > 0) {
        result[i] <- switch(time_unit,
          "days" = cycle_row$day[1],
          "weeks" = cycle_row$week[1],
          "months" = cycle_row$month[1],
          "years" = cycle_row$year[1],
          cycles[i]  # Default to cycle number
        )
      } else {
        # Fallback: estimate based on cycle length
        cycle_length_var <- switch(time_unit,
          "days" = namespace["cycle_length_days"],
          "weeks" = namespace["cycle_length_weeks"],
          "months" = namespace["cycle_length_months"],
          "years" = namespace["cycle_length_years"],
          1
        )
        result[i] <- cycles[i] * as.numeric(cycle_length_var)
      }
    }
  }
  
  result
}

calculate_psm_values <- function(uneval_values, namespace, trace, trans_pfs_to_pp, trans_pp_to_dead, value_names, state_names, n_cycles, half_cycle_method = "start") {
  
  # Initialize values matrix
  values_matrix <- matrix(0, nrow = n_cycles, ncol = length(value_names))
  colnames(values_matrix) <- value_names
  rownames(values_matrix) <- 1:n_cycles
  
  if (length(value_names) == 0 || nrow(uneval_values) == 0) {
    return(values_matrix)
  }
  
  # Evaluate values for each cycle
  for (cycle in 1:n_cycles) {
    
    # Set current cycle in namespace
    cycle_ns <- clone_namespace(namespace)
    cycle_ns$df$cycle <- cycle
    cycle_ns$df$state_cycle <- cycle  # PSM doesn't use state_cycle differently
    
    # Process each value
    for (i in 1:nrow(uneval_values)) {
      value_row <- uneval_values[i, ]
      value_name <- value_row$name
      
      if (is.na(value_name) || !(value_name %in% value_names)) {
        next
      }
      
      # Evaluate the formula
      evaluated_value <- eval_formula(value_row$formula[[1]], cycle_ns)
      
      if (is_oq_error(evaluated_value)) {
        accumulate_oq_error(evaluated_value, context_msg = glue("Evaluation of value '{value_name}' in cycle {cycle}"))
        evaluated_value <- 0
      } else if (!is.numeric(evaluated_value) || length(evaluated_value) != 1) {
        error_msg <- glue("Value '{value_name}' in cycle {cycle} must evaluate to a single numeric value.")
        accumulate_oq_error(define_error(error_msg), context_msg = "PSM value validation")
        evaluated_value <- 0
      }
      
      # Apply value based on type (residency vs transitional)
      if (!is.na(value_row$state) && is.na(value_row$destination)) {
        # Residency value
        state_index <- which(state_names == value_row$state)
        if (length(state_index) == 1) {
          # Calculate state probability based on half-cycle method
          # Note: PSM trace is 0-indexed (cycle 0 = initial, cycle 1 = first cycle, etc.)
          state_prob <- if (half_cycle_method == "end") {
            trace[cycle + 1, state_index]  # End of cycle
          } else if (half_cycle_method == "life-table") {
            if (cycle == 1) {
              # First cycle: average of initial (row 1) and end of cycle 1 (row 2)
              (trace[1, state_index] + trace[2, state_index]) / 2
            } else if (cycle == n_cycles) {
              # Last cycle: just use end probability
              trace[cycle + 1, state_index]
            } else {
              # Middle cycles: average of start (row cycle) and end (row cycle+1)
              (trace[cycle, state_index] + trace[cycle + 1, state_index]) / 2
            }
          } else {  # "start" or default
            trace[cycle, state_index]  # Start of cycle
          }

          # Multiply by state occupancy probability
          values_matrix[cycle, value_name] <- values_matrix[cycle, value_name] +
            evaluated_value * state_prob
        }
      } else if (!is.na(value_row$state) && !is.na(value_row$destination)) {
        # Transitional value
        from_state <- value_row$state
        to_state <- value_row$destination
        
        # Determine transition probability
        trans_prob <- 0
        if (from_state == state_names[1] && to_state == state_names[2]) {
          # PFS to post-progression
          trans_prob <- trans_pfs_to_pp[cycle]
        } else if (from_state == state_names[2] && to_state == state_names[3]) {
          # Post-progression to dead
          trans_prob <- trans_pp_to_dead[cycle]
        }
        # Note: PFS to dead transition is assumed to be 0 as per specification
        
        values_matrix[cycle, value_name] <- values_matrix[cycle, value_name] + 
          evaluated_value * trans_prob
      } else if (is.na(value_row$state) && is.na(value_row$destination)) {
        # Model-level value (applied regardless of state)
        values_matrix[cycle, value_name] <- values_matrix[cycle, value_name] + evaluated_value
      }
    }
  }
  
  oq_error_checkpoint()

  values_matrix
}

# ============================================================================
# Custom PSM Implementation
# ============================================================================

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
      parsed_values = list()
    )
  }

  # Create a namespace which will contain evaluated variables
  ns <- create_namespace(model, segment)

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
  discount_cost <- if (!is.null(model$settings$discount_cost)) model$settings$discount_cost else 0.035
  discount_outcomes <- if (!is.null(model$settings$discount_outcomes)) model$settings$discount_outcomes else 0.035

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
#' State probabilities are evaluated directly from formulas at each cycle,
#' with special handling for the complement operator "C".
#'
#' @param state_prob_formulas Named list of oq_formula objects by state
#' @param uneval_values Unevaluated values specifications
#' @param namespace Evaluated variables namespace
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

  # Initialize trace matrix
  trace <- matrix(0, nrow = n_cycles + 1, ncol = length(state_names))
  colnames(trace) <- state_names
  rownames(trace) <- 0:n_cycles

  # Tolerance for probability validation
  tol <- 10 * sqrt(.Machine$double.eps)

  # Identify complement state (formula = "C")
  complement_state <- NULL
  for (state_name in state_names) {
    formula_obj <- state_prob_formulas[[state_name]]
    formula_str <- trimws(as.character(formula_obj))
    if (toupper(formula_str) == "C") {
      complement_state <- state_name
      break
    }
  }

  # Evaluate state probabilities for each cycle
  for (cycle in 0:n_cycles) {
    # Clone namespace and set cycle
    cycle_ns <- clone_namespace(namespace)
    cycle_ns$df$cycle <- cycle

    sum_other_probs <- 0

    # First, evaluate non-complement states
    for (i in seq_along(state_names)) {
      state_name <- state_names[i]

      # Skip complement state for now
      if (!is.null(complement_state) && state_name == complement_state) {
        next
      }

      formula <- state_prob_formulas[[state_name]]
      prob <- eval_formula(formula, cycle_ns)

      # Error handling
      if (is_oq_error(prob)) {
        error_msg <- glue("Error evaluating state probability for '{state_name}' at cycle {cycle}")
        accumulate_oq_error(prob, context_msg = error_msg)
        prob <- 0  # Fallback
      } else if (!is.numeric(prob) || length(prob) != 1) {
        error_msg <- glue("State probability for '{state_name}' at cycle {cycle} must evaluate to a single numeric value (got {class(prob)[1]} of length {length(prob)}).")
        accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM probability validation")
        prob <- 0  # Fallback
      } else if (prob < -tol || prob > 1 + tol) {
        error_msg <- glue("State probability for '{state_name}' at cycle {cycle} is out of bounds: {prob} (must be in [0, 1]).")
        accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM probability validation")
        prob <- max(0, min(1, prob))  # Clamp to valid range
      }

      trace[cycle + 1, i] <- prob
      sum_other_probs <- sum_other_probs + prob
    }

    # Now calculate complement if needed
    if (!is.null(complement_state)) {
      complement_idx <- which(state_names == complement_state)
      complement_prob <- 1 - sum_other_probs

      if (complement_prob < -tol || complement_prob > 1 + tol) {
        error_msg <- glue("Complement probability for state '{complement_state}' at cycle {cycle} is out of bounds: {complement_prob}. Sum of other states: {sum_other_probs}")
        accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM complement validation")
        complement_prob <- max(0, min(1, complement_prob))
      }

      trace[cycle + 1, complement_idx] <- complement_prob
    }

    # Validate probabilities sum to 1
    row_sum <- sum(trace[cycle + 1, ])
    if (abs(row_sum - 1.0) > tol) {
      state_prob_str <- paste(sprintf("%s=%.6f", state_names, trace[cycle + 1, ]), collapse = ", ")
      error_msg <- glue("Custom PSM state probabilities do not sum to 1 at cycle {cycle} (sum = {row_sum}). State probabilities: {state_prob_str}")
      accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM trace validation")
    }

    # Check for negative probabilities
    if (any(trace[cycle + 1, ] < -tol)) {
      invalid_states <- state_names[trace[cycle + 1, ] < -tol]
      error_msg <- glue("Custom PSM has negative probabilities at cycle {cycle} for states: {paste(invalid_states, collapse = ', ')}")
      accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM trace validation")
    }
  }

  # Check for accumulated errors
  oq_error_checkpoint()

  # Calculate values (residency and model-level only)
  values_matrix <- calculate_psm_custom_values(
    uneval_values,
    namespace,
    trace,
    value_names,
    state_names,
    n_cycles,
    half_cycle_method
  )

  list(trace = trace, values = values_matrix)
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

  # Evaluate values for each cycle
  for (cycle in 1:n_cycles) {
    cycle_ns <- clone_namespace(namespace)
    cycle_ns$df$cycle <- cycle
    cycle_ns$df$state_cycle <- cycle

    # Process each value
    for (i in 1:nrow(uneval_values)) {
      value_row <- uneval_values[i, ]
      value_name <- value_row$name

      if (is.na(value_name) || !(value_name %in% value_names)) {
        next
      }

      # Validate: no transitional values
      if (!is.na(value_row$destination)) {
        error_msg <- glue("Transitional value '{value_name}' (state: {value_row$state}, destination: {value_row$destination}) is not allowed in Custom PSM models.")
        accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM value validation")
        next
      }

      # Evaluate formula
      evaluated_value <- eval_formula(value_row$formula[[1]], cycle_ns)

      if (is_oq_error(evaluated_value)) {
        accumulate_oq_error(evaluated_value, context_msg = glue("Evaluation of value '{value_name}' in cycle {cycle}"))
        evaluated_value <- 0
      } else if (!is.numeric(evaluated_value) || length(evaluated_value) != 1) {
        error_msg <- glue("Value '{value_name}' in cycle {cycle} must evaluate to a single numeric value.")
        accumulate_oq_error(define_error(error_msg), context_msg = "Custom PSM value validation")
        evaluated_value <- 0
      }

      # Apply value based on type
      if (!is.na(value_row$state)) {
        # Residency value
        state_index <- which(state_names == value_row$state)
        if (length(state_index) == 1) {
          # Calculate state probability based on half-cycle method
          state_prob <- if (half_cycle_method == "end") {
            trace[cycle + 1, state_index]
          } else if (half_cycle_method == "life-table") {
            if (cycle == 1) {
              (trace[1, state_index] + trace[2, state_index]) / 2
            } else if (cycle == n_cycles) {
              trace[cycle + 1, state_index]
            } else {
              (trace[cycle, state_index] + trace[cycle + 1, state_index]) / 2
            }
          } else {  # "start"
            trace[cycle, state_index]
          }

          values_matrix[cycle, value_name] <- values_matrix[cycle, value_name] +
            evaluated_value * state_prob
        }
      } else {
        # Model-level value
        values_matrix[cycle, value_name] <- values_matrix[cycle, value_name] + evaluated_value
      }
    }
  }

  oq_error_checkpoint()

  values_matrix
}