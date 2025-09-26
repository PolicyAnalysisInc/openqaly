parse_psm <- function(model) {
  # For PSM models, transitions contain survival endpoint definitions
  # Validate PSM-specific requirements
  if (!is.null(model$transitions) && nrow(model$transitions) > 0) {
    # Check required columns for PSM transitions
    required_cols <- c("endpoint", "time_unit", "formula")
    missing_cols <- setdiff(required_cols, colnames(model$transitions))
    if (length(missing_cols) > 0) {
      stop(paste0("PSM transitions missing required columns: ", paste(missing_cols, collapse = ", ")))
    }
  }
  
  define_object_(model, class = 'psm')
}

run_segment.psm <- function(segment, model, env, ...) {
  
  # Capture the extra arguments provided to function
  dots <- list(...)
  
  # Parse the specification tables provided for states and variables
  uneval_states <- parse_states(model$states, model$settings$cycle_length_days, model$settings$days_per_year)
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
    parsed_summaries <- tibble::tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      values = character(0),
      parsed_values = list()
    )
  }
  
  # Create a namespace which will contain evaluated variables
  ns <- create_namespace(model, segment)
  
  # Evaluate variables and initial state probabilities
  eval_vars <- eval_variables(uneval_vars, ns)
  eval_states <- eval_states(uneval_states, eval_vars)
  
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
  value_type_mapping <- setNames(uneval_values$value_type, uneval_values$name)
  value_type_mapping <- value_type_mapping[!is.na(names(value_type_mapping))]

  # Apply discounting to get discounted values
  calculated_trace_and_values$values_discounted <- apply_discounting(
    calculated_trace_and_values$values,
    discount_factors_cost,
    discount_factors_outcomes,
    value_type_mapping
  )

  # Create the object to return
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment$inital_state <- list(eval_states)
  segment$trace_and_values <- list(calculated_trace_and_values)
  segment$collapsed_trace <- list(calculated_trace_and_values$trace)

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
    empty_summary <- tibble::tibble(summary = character(), value = character(), amount = numeric())
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
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM transitions parsing")
    hero_error_checkpoint()
    return(list(pfs = NULL, os = NULL))
  }
  
  # Extract PFS and OS definitions
  pfs_def <- transitions %>%
    filter(toupper(endpoint) == "PFS")
  os_def <- transitions %>%
    filter(toupper(endpoint) == "OS")
  
  # Validate we have exactly one PFS and one OS definition
  if (nrow(pfs_def) == 0) {
    error_msg <- "PSM model missing PFS endpoint definition in transitions."
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM transitions validation")
    hero_error_checkpoint()
    return(list(pfs = NULL, os = NULL))
  }
  if (nrow(os_def) == 0) {
    error_msg <- "PSM model missing OS endpoint definition in transitions."
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM transitions validation")
    hero_error_checkpoint()
    return(list(pfs = NULL, os = NULL))
  }
  if (nrow(pfs_def) > 1) {
    error_msg <- glue("PSM model has multiple PFS endpoint definitions ({nrow(pfs_def)}). Only one is allowed.")
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM transitions validation")
    hero_error_checkpoint()
  }
  if (nrow(os_def) > 1) {
    error_msg <- glue("PSM model has multiple OS endpoint definitions ({nrow(os_def)}). Only one is allowed.")
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM transitions validation")
    hero_error_checkpoint()
  }
  
  # Take first definition if multiple exist
  pfs_def <- pfs_def[1, ]
  os_def <- os_def[1, ]
  
  # Parse and evaluate the survival distribution formulas
  pfs_dist <- evaluate_psm_endpoint(pfs_def, "PFS", namespace)
  os_dist <- evaluate_psm_endpoint(os_def, "OS", namespace)
  
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
  
  # Convert formula to heRoFormula
  formula <- as.heRoFormula(as.character(endpoint_def$formula))
  
  # Evaluate the formula - this should reference a variable containing the survival distribution
  dist <- eval_formula(formula, namespace)
  
  # Check for errors
  if (is_hero_error(dist)) {
    accumulate_hero_error(dist, context_msg = glue("Evaluation of {endpoint_name} survival distribution"))
    return(NULL)
  }
  
  # Validate the result is a survival distribution
  if (!inherits(dist, "surv_dist")) {
    error_msg <- glue("{endpoint_name} formula must evaluate to a survival distribution object (got {class(dist)[1]}).")
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM endpoint validation")
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
    accumulate_hero_error(define_error(error_msg), context_msg = "PSM state validation")
    hero_error_checkpoint()
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
  pfs_surv <- herosurv::surv_prob(pfs_dist, pfs_times)
  os_surv <- herosurv::surv_prob(os_dist, os_times)
  
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

#' Calculate PSM Values
#'
#' Calculates residency and transitional values for PSM models.
#'
#' @param uneval_values Unevaluated values specifications
#' @param namespace Evaluated variables namespace
#' @param trace State occupancy trace matrix
#' @param trans_pfs_to_pp Transition probabilities from PFS to post-progression
#' @param trans_pp_to_dead Transition probabilities from post-progression to dead
#' @param value_names Vector of value names
#' @param state_names Vector of state names
#' @param n_cycles Number of cycles
#'
#' @return A matrix of calculated values
#' @keywords internal
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
      
      if (is_hero_error(evaluated_value)) {
        accumulate_hero_error(evaluated_value, context_msg = glue("Evaluation of value '{value_name}' in cycle {cycle}"))
        evaluated_value <- 0
      } else if (!is.numeric(evaluated_value) || length(evaluated_value) != 1) {
        error_msg <- glue("Value '{value_name}' in cycle {cycle} must evaluate to a single numeric value.")
        accumulate_hero_error(define_error(error_msg), context_msg = "PSM value validation")
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
  
  hero_error_checkpoint()
  
  values_matrix
}