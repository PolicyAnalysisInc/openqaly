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
  .progress_callback <- dots$.progress_callback

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
      type = character(0),
      wtp = numeric(0),
      parsed_values = list()
    )
  }

  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # Create a namespace which will contain evaluated variables
  # Include cycle 0 for survival probability calculation at t=0
  ns <- create_namespace(model, segment, include_cycle_zero = TRUE)

  # Apply parameter overrides if present (PSA, DSA, or VBP mode)
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # Evaluate variables
  eval_vars <- eval_variables(uneval_vars, ns)
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # PSM doesn't use initial state probabilities (determined by survival functions)
  # Set to NULL for PSM
  eval_states <- NULL

  # For PSM, parse and evaluate survival distributions from transitions
  survival_distributions <- parse_and_eval_psm_transitions(model$transitions, segment, eval_vars)
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

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
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

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
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

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
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

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
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # Calculate segment weight
  segment$weight <- calculate_segment_weight(segment, model, eval_vars)

  # Reorder columns to have strategy, group, weight first
  col_order <- c("strategy", "group", "weight")
  other_cols <- setdiff(names(segment), col_order)
  segment <- segment[, c(col_order, other_cols)]
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

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
  
  # Map time_unit to the pre-computed column in namespace$df
  time_col <- function(unit) {
    switch(tolower(unit),
      "days" = "day", "weeks" = "week",
      "months" = "month", "years" = "year", NULL
    )
  }
  pfs_col <- if (!is.null(pfs_time_unit)) time_col(pfs_time_unit) else NULL
  os_col  <- if (!is.null(os_time_unit))  time_col(os_time_unit)  else NULL
  pfs_times <- if (!is.null(pfs_col)) namespace$df[[pfs_col]] else cycle_times
  os_times  <- if (!is.null(os_col))  namespace$df[[os_col]]  else cycle_times
  
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

  # Validate bounds
  out_of_bounds <- which(trace < -tol | trace > 1 + tol, arr.ind = TRUE)
  if (nrow(out_of_bounds) > 0) {
    for (k in 1:nrow(out_of_bounds)) {
      r <- out_of_bounds[k, 1]
      error_msg <- glue("PSM trace contains invalid probabilities at cycle {r - 1} (must be in [0, 1])")
      accumulate_oq_error(define_error(error_msg), context_msg = "PSM trace validation")
    }
  }

  # Validate row sums
  row_sums <- rowSums(trace)
  bad_rows <- which(abs(row_sums - 1.0) > tol)
  for (r in bad_rows) {
    error_msg <- glue("PSM trace probabilities do not sum to 1 at cycle {r - 1} (sum = {row_sums[r]})")
    accumulate_oq_error(define_error(error_msg), context_msg = "PSM trace validation")
  }
  oq_error_checkpoint()

  # Calculate transition probabilities for transitional values
  # Transitions: PFS -> Post-prog, Post-prog -> Dead
  trans_pfs_to_pp <- c(0, diff(-prob_pfs))  # Decrease in PFS
  trans_pp_to_dead <- c(0, diff(prob_dead))  # Increase in dead
  
  # Values are for cycles 1:n_cycles, so prune cycle 0 from namespace
  values_ns <- prune_namespace_cycle_zero(namespace)
  values_matrix <- calculate_psm_values(
    uneval_values,
    values_ns,
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

calculate_psm_values <- function(uneval_values, namespace, trace, trans_pfs_to_pp, trans_pp_to_dead, value_names, state_names, n_cycles, half_cycle_method = "start") {

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

    # Evaluate formula once (returns vector of length n_cycles)
    evaluated <- eval_formula(value_row$formula[[1]], namespace)

    if (is_oq_error(evaluated)) {
      accumulate_oq_error(evaluated, context_msg = glue("Value '{value_name}'"))
      evaluated <- rep(0, n_cycles)
    } else if (!is.numeric(evaluated)) {
      accumulate_oq_error(
        define_error(glue("Value '{value_name}' must evaluate to a single numeric value.")),
        context_msg = "PSM value validation"
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
        context_msg = "PSM value validation"
      )
      evaluated <- rep(0, n_cycles)
    }

    # Apply value based on type (residency vs transitional)
    if (!is.na(value_row$state) && is.na(value_row$destination)) {
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
    } else if (!is.na(value_row$state) && !is.na(value_row$destination)) {
      # Transitional value
      from_state <- value_row$state
      to_state <- value_row$destination

      # Determine transition probability vector
      trans_probs <- rep(0, n_cycles)
      if (from_state == state_names[1] && to_state == state_names[2]) {
        # PFS to post-progression
        trans_probs <- trans_pfs_to_pp[1:n_cycles]
      } else if (from_state == state_names[2] && to_state == state_names[3]) {
        # Post-progression to dead
        trans_probs <- trans_pp_to_dead[1:n_cycles]
      }
      # Note: PFS to dead transition is assumed to be 0 as per specification

      values_matrix[, value_name] <- values_matrix[, value_name] + evaluated * trans_probs
    } else if (is.na(value_row$state) && is.na(value_row$destination)) {
      # Model-level value
      values_matrix[, value_name] <- values_matrix[, value_name] + evaluated
    }
  }

  oq_error_checkpoint()

  values_matrix
}