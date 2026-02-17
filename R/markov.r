parse_markov <- function(model) {
  define_object_(model, class = 'markov')
}

run_segment <- function(segment, model, env, ...) {
  UseMethod('run_segment', model)
}

#' Evaluate Group Weight
#'
#' Takes a groups dataframe row and a namespace object and uses eval_formula 
#' to evaluate the group's weight in the namespace.
#'
#' @param group_row A single row from the groups dataframe containing group information
#' @param namespace A namespace object containing evaluated variables
#'
#' @return The evaluated weight as a numeric value, or NA if evaluation fails
#' @keywords internal
evaluate_group_weight <- function(group_row, namespace) {
  # Convert weight to formula (handles both numeric and character inputs)
  weight_formula <- as.oq_formula(as.character(group_row$weight))

  # Evaluate the formula in the namespace
  evaluated_weight <- eval_formula(weight_formula, namespace)

  # Check for errors in evaluation
  if (is_oq_error(evaluated_weight)) {
    accumulate_oq_error(evaluated_weight, context_msg = glue("Evaluation of weight for group '{group_row$name}'"))
    return(NA_real_)
  }

  # Validate the evaluated result
  if (!is.numeric(evaluated_weight)) {
    error_msg <- glue("Weight for group '{group_row$name}' must be numeric (got {class(evaluated_weight)[1]}).")
    accumulate_oq_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }

  if (length(evaluated_weight) != 1) {
    error_msg <- glue("Weight for group '{group_row$name}' must be length 1 (got length {length(evaluated_weight)}). Weight formulas cannot be time-dependent.")
    accumulate_oq_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }

  weight_value <- as.numeric(evaluated_weight)

  if (is.na(weight_value)) {
    error_msg <- glue("Weight for group '{group_row$name}' evaluated to NA.")
    accumulate_oq_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }

  if (!is.finite(weight_value)) {
    error_msg <- glue("Weight for group '{group_row$name}' must be finite (got {weight_value}).")
    accumulate_oq_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }
  
  # Return the validated weight
  return(weight_value)
}

#' Calculate Segment Weight
#'
#' Calculates the weight for a segment based on its group and the model's groups definition.
#' Returns the evaluated weight for the segment.
#'
#' @param segment The segment to calculate weight for
#' @param model The model containing groups information
#' @param namespace The namespace containing evaluated variables
#'
#' @return The evaluated weight as a numeric value, or NA if evaluation fails
#' @keywords internal
calculate_segment_weight <- function(segment, model, namespace) {
  # Check if groups information is available
  if (is.null(model$groups) || nrow(model$groups) == 0) {
    return(1)  # Default weight if no groups defined
  }
  
  # Find the group row for this segment
  group_row <- model$groups %>%
    filter(.data$name == segment$group) %>%
    slice(1)
  
  if (nrow(group_row) == 0) {
    warning(glue("Group '{segment$group}' not found in model groups. Using default weight of 1."))
    return(1)  # Default weight if group not found
  }
  
  # Evaluate and return the weight
  return(evaluate_group_weight(group_row, namespace))
}

#' @export
run_segment.markov <- function(segment, model, env, ...) {

  # Capture the extra arguments provided to function
  dots <- list(...)

  # Apply setting overrides if present (DSA mode)
  model <- apply_setting_overrides(segment, model)

  # Calculate n_cycles AFTER apply_setting_overrides to ensure DSA timeframe overrides work correctly
  model$settings$cycle_length_days <- get_cycle_length_days(model$settings)
  model$settings$n_cycles <- get_n_cycles(model$settings)

  # Parse the specification tables provided for states,
  # variables, transitions, values, and summaries
  uneval_states <- parse_states(model$states, model$settings$cycle_length_days, model$settings$days_per_year)
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)
  uneval_trans <- parse_trans_markov(model$transitions, uneval_states, uneval_vars)
  uneval_values <- parse_values(model$values, uneval_states, uneval_vars)
  
  # Determine model_value_names safely for parse_summaries
  model_value_names <- character(0)
  if (nrow(model$values) > 0 && "name" %in% colnames(model$values)) {
    # Filter out NA names before unique, as unique(NA) is NA, which can cause issues
    valid_names <- model$values$name[!is.na(model$values$name)]
    if (length(valid_names) > 0) {
      model_value_names <- unique(valid_names)
    }
  }

  # Parse summaries if they exist, ensuring parsed_summaries is always a structured tibble
  if (!is.null(model$summaries) && nrow(model$summaries) > 0) { # model$summaries is now a structured tibble
    parsed_summaries <- parse_summaries(model$summaries, model_value_names) 
  } else {
    # Create an empty structured tibble for summaries, including the parsed_values column
    parsed_summaries <- tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      values = character(0),
      type = character(0),
      wtp = numeric(0),
      parsed_values = list() # parse_summaries adds this, so we ensure it exists
    )
  }

  # Check inside the variables, transitions, & values for
  # any state-time dependency since this will inform the
  # creation of tunnel states
  state_time_use <- check_state_time(
    uneval_vars,
    uneval_states,
    uneval_trans,
    uneval_values
  )
  
  # Create a "namespace" which will contain evaluated
  # variables so that they can be referenced.
  ns <- create_namespace(model, segment)

  # Apply parameter overrides if present (PSA, DSA, or VBP mode)
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # Evaluate variables, initial state probabilities, transitions,
  # values, & summaries.
  eval_vars <- eval_variables(uneval_vars, ns)
  eval_states <- eval_states(uneval_states, eval_vars)

  eval_trans <- eval_trans_markov_lf(uneval_trans, eval_vars, isTRUE(model$settings$reduce_state_cycle)) #770ms

  # Determine value_names safely for evaluate_values and cppMarkovTransitionsAndTrace
  value_names <- character(0)
  if (nrow(model$values) > 0 && "name" %in% colnames(model$values)) {
    valid_names <- model$values$name[!is.na(model$values$name)]
    if (length(valid_names) > 0) {
      value_names <- unique(valid_names)
    }
  }
  state_names <- unique(model$states$name) # Assuming model$states is always non-empty and has a name column

  eval_values <- evaluate_values(
    uneval_values,
    eval_vars,
    value_names,
    state_names,
    isTRUE(model$settings$reduce_state_cycle)
  ) #900ms

#322 ms
  expanded <- handle_state_expansion(eval_states, eval_trans, eval_values, state_time_use)

#340 ms
  calculated_trace_and_values <- calculate_trace_and_values(
    expanded$init,
    expanded$transitions,
    expanded$values,
    value_names,
    expanded$expanded_state_map,
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

  # Create the object to return that will summarize the results of
  # this segment.
  # Always store trace_and_values (needed for per-cycle output and aggregation)
  segment$trace_and_values <- list(calculated_trace_and_values)
  # In override mode (PSA/DSA), skip heavy diagnostic objects
  if (!"parameter_overrides" %in% names(segment)) {
    segment$uneval_vars <- list(uneval_vars)
    segment$eval_vars <- list(eval_vars)
    segment$inital_state <- list(eval_states)
  }

  # Extract the expanded trace matrix
  expanded_trace_matrix <- calculated_trace_and_values[[1]]

  # Calculate the collapsed trace using the expanded_state_map from handle_state_expansion
  collapsed_trace <- calculate_collapsed_trace(
    calculated_trace_and_values, # This is the list from cppMarkovTransitionsAndTrace
    expanded$expanded_state_map
  )

  # Add time variables to both traces
  # Generate time columns based on the actual cycle numbers from row names
  n_trace_rows <- nrow(collapsed_trace)

  # The row names indicate the actual cycle numbers (0, 1, 2, ...)
  cycle_numbers <- as.numeric(rownames(collapsed_trace))

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
    rownames(time_vars_df) <- rownames(collapsed_trace)

    # Convert collapsed_trace matrix to data frame and add time columns
    collapsed_trace_with_time <- cbind(time_vars_df, collapsed_trace)
  } else {
    # No cycle length info, just add cycle numbers
    time_vars_df <- data.frame(cycle = cycle_numbers)
    rownames(time_vars_df) <- rownames(collapsed_trace)
    collapsed_trace_with_time <- cbind(time_vars_df, collapsed_trace)
  }

  # Also create expanded trace with time variables
  if (!is.na(cycle_length_days) && cycle_length_days > 0) {
    # Reuse the same time_vars_df that was created above
    expanded_trace_with_time <- cbind(time_vars_df, expanded_trace_matrix)
  } else {
    # Reuse the simple time_vars_df with just cycle
    expanded_trace_with_time <- cbind(time_vars_df, expanded_trace_matrix)
  }

  segment$collapsed_trace <- list(collapsed_trace_with_time)
  segment$expanded_trace <- list(expanded_trace_with_time)

  # Calculate summaries: parsed_summaries is now guaranteed to be a tibble (possibly 0-row)
  # calculate_summaries should be robust to a 0-row parsed_summaries or 0-col/0-row trace values.
  if (!is.null(parsed_summaries)) { # This check might be redundant if parsed_summaries is always a tibble
                                   # but good for safety if parse_summaries could return NULL.
                                   # parse_summaries should ideally return empty structured tibble, not NULL.
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
    # Fallback: ensure summaries field exists as an empty list or tibble
    # This path should ideally not be taken if parsed_summaries is always initialized properly.
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

handle_state_expansion <- function(init, transitions, values, state_time_use) {

  # Determine number of cycles
  n_cycles <- max(transitions$cycle)

  # Calculate maximum number of tunnels needed for each state
  # Join with state_time_use to only expand states that use state time
  st_maxes <- get_st_max(transitions, values, n_cycles) %>%
    left_join(state_time_use, by = "state") %>%
    mutate(
      # If uses_st is FALSE or NA, set max_st to 1 (no expansion)
      max_st = ifelse(is.na(.data$uses_st) | !.data$uses_st, 1, .data$max_st)
    ) %>%
    select("state", "max_st")

  # Expand initial state probabilities to include tunnel states
  expand_init <- expand_init_states(init, st_maxes)
  state_names <- colnames(expand_init)

  # Filter transition probabilities to only include required tunnel states
  eval_trans_limited <- select(transitions, -"max_st") %>%
    left_join(st_maxes, by = c('from_state' = 'state')) %>%
    filter(.data$state_cycle <= .data$max_st)

  expanded_transitions <- eval_trans_limited %>%
    group_by(.data$from_state) %>%
    mutate(.max_st = max(.data$state_cycle)) %>%
    ungroup() %>%
    mutate(
      .end = .data$state_cycle == .data$.max_st,
      .from_e = expand_state_name(.data$from_state, .data$state_cycle)
    )
    lv_sg_i <- (!expanded_transitions$share_state_time) | (expanded_transitions$from_state_group != expanded_transitions$to_state_group)
    lv_i <- expanded_transitions$from_state != expanded_transitions$to_state & lv_sg_i
    ls_i <- expanded_transitions$.end & !lv_i
    nx_i <- !(lv_i | ls_i)
    expanded_transitions$.to_e <- NA
    expanded_transitions$.to_e[lv_i] <- expand_state_name(expanded_transitions$to_state[lv_i], 1)
    expanded_transitions$.to_e[ls_i] <- expand_state_name(expanded_transitions$to_state[ls_i], expanded_transitions$.max_st[ls_i])
    expanded_transitions$.to_e[nx_i] <- expand_state_name(expanded_transitions$to_state[nx_i], expanded_transitions$state_cycle[nx_i] + 1)

  # Generate data structure of transition probabilities to pass to rcpp function
  expanded_trans_matrix <- lf_to_lf_mat(expanded_transitions, state_names)

  # Create the mapping of original state names to their actual expanded names used
  expanded_state_map <- expanded_transitions %>%
    select("from_state", ".from_e") %>%
    distinct()

  expand_trans_first_cycle <- select(
    filter(expanded_transitions, .data$cycle == 1),
    "from_state", "to_state", ".to_e", "state_cycle"
  )

  model_start_values <- filter(values, is.na(.data$state), is.na(.data$destination), .data$state_cycle == 1)

  # Filter values to only include required tunnel states
  values_expanded <- select(values, -"max_st") %>%
    left_join(st_maxes, by = c('state' = 'state')) %>%
    filter(
      .data$state_cycle <= .data$max_st,
      !(is.na(.data$state) & is.na(.data$destination) & .data$state_cycle > 1)
    ) %>%
    mutate(
      .state_e = expand_state_name(.data$state, .data$state_cycle)
    ) %>%
    left_join(
      expand_trans_first_cycle,
      by = c(
        "state" = "from_state",
        "destination" = "to_state",
        "state_cycle" = "state_cycle"
      )
    ) %>%
    transmute(
      state = .data$.state_e,
      destination = .data$.to_e,
      max_st = .data$max_st,
      values_list = .data$values_list
    ) %>%
    bind_rows(model_start_values)

  list(
    init = expand_init,
    transitions = expanded_trans_matrix,
    values = values_expanded,
    expanded_state_map = expanded_state_map
  )
}

calculate_trace_and_values <- function(init, transitions, values, value_names, expanded_state_map, half_cycle_method = "start") {
  # Extract expanded state names from init columns (critical!)
  state_names <- colnames(init)  # These are the expanded state names

  # Convert inputs to proper types
  init_numeric <- as.numeric(init)
  trans_matrix <- as.matrix(transitions)

  # Validate all inputs before calling C++
  validate_cpp_inputs(
    init_numeric,
    trans_matrix,
    values,
    value_names,
    state_names,
    expanded_state_map,
    half_cycle_method
  )

  # All processing now happens in optimized C++
  cppCalculateTraceAndValues(
    init_numeric,
    trans_matrix,
    values,
    value_names,
    state_names,  # Pass expanded state names separately
    expanded_state_map,
    half_cycle_method
  )
}

calculate_collapsed_trace <- function(expanded_results, expanded_state_map) {
  
  # The trace is the first element of the list returned by cppMarkovTransitionsAndTrace
  expanded_trace_matrix <- expanded_results[[1]] 
  
  # Get all actual expanded state names that are column names in the trace matrix
  actual_trace_col_names <- colnames(expanded_trace_matrix)
  
  # Get unique original state names from the mapping
  # These will be the columns in our collapsed trace
  original_state_names <- unique(expanded_state_map$from_state)
  
  # Initialize the collapsed trace matrix
  num_cycles <- nrow(expanded_trace_matrix)
  if (is.null(num_cycles)) num_cycles <- 0 # Handle empty trace matrix
  
  collapsed_trace <- matrix(
    0.0, 
    nrow = num_cycles, 
    ncol = length(original_state_names),
    dimnames = list(rownames(expanded_trace_matrix), original_state_names)
  )
  
  # Iterate over each original state name
  for (original_name in original_state_names) {
    # Get the list of expanded names corresponding to this original_name from the map
    child_expanded_names_from_map <- expanded_state_map$.from_e[expanded_state_map$from_state == original_name]
    
    # Filter this list to include only those expanded names that actually exist as columns in the trace matrix
    # This is a safeguard, as ideally, .from_e names used in transitions should match trace columns.
    actual_children_in_trace <- intersect(child_expanded_names_from_map, actual_trace_col_names)
    
    if (length(actual_children_in_trace) > 0) {
      # Select the sub-matrix of these children's columns from the expanded trace
      # drop = FALSE ensures it remains a matrix even if only one column is selected
      sub_matrix <- expanded_trace_matrix[, actual_children_in_trace, drop = FALSE]
      
      # Sum the rows of this sub-matrix and assign to the collapsed_trace
      collapsed_trace[, original_name] <- rowSums(sub_matrix, na.rm = TRUE)
    }
    # If no actual_children_in_trace found for this original_name, its column in collapsed_trace remains 0.
  }
  
  return(collapsed_trace)
}

get_st_max <- function(trans, values, n_cycles) {

  # Combine transitions and values into a single data frame
  combined_transitions_and_values <- rbind(
    rename(trans[ , c('from_state', 'max_st')], state = "from_state"),
    filter(values, !is.na(.data$state))[ , c('state', 'max_st')]
  )

  # Group by state and get the maximum state time. For any
  # states where the maximum is infinite, set it to the
  # maximum number of cycles
  st_maxes <- combined_transitions_and_values %>%
    group_by(.data$state) %>%
    summarize(max_st = max(.data$max_st)) %>%
    mutate(max_st = ifelse(is.infinite(.data$max_st), n_cycles, .data$max_st))

  st_maxes
}

# Markov
#
# Markov models specified using longform table structure for
# transition matrices.
# --------------------------------------------------

# Parse a Markov Transitions Specification
#
# Takes a longform transitions specifications table, a states
# specification, and a variables specification and returns
# an unevalted transitions object.
parse_trans_markov <- function(x, states, vars) {
  
  # Extract the state names
  state_names <- states$name
  
  # Check transitions definition
  check_trans_markov(x, state_names)

  # Construct the transitions object
  x$formula <- map(x$formula, as.oq_formula)
  x$name <- glue("{x$from_state}\u2192{x$to_state}")

  res <- sort_variables(x, vars) %>%
    select("name", "from_state", "to_state", "formula") %>%
    left_join(
      transmute(
        states, name = .data$name,
        from_state_group = .data$state_group,
        share_state_time = .data$share_state_time,
        max_st = ifelse(.data$max_state_time == 0, Inf, .data$max_state_time)
      ),
      by = c('from_state' = 'name')
    ) %>%
    left_join(
      transmute(
        states,
        name = .data$name,
        to_state_group = .data$state_group
      ),
      by = c('to_state' = 'name')
    )
  
  # Return result
  as.lf_markov_trans(res)
}

# Check a Markov Transitions Specification
#
# Takes a Markov transitions specifications table and a
# vector of state names and checks that the specification is
# valid.
check_trans_markov <- function(x, state_names) {
  
  error_msg <- ''
  
  # Check column names
  missing_cols <- check_missing_colnames(x, trans_markov_cols)

  if (length(missing_cols) > 0) {
    plural <- if (length(missing_cols) > 1) 's' else ''
    missing_msg <- paste(missing_cols, collapse = ', ')
    error_msg <- glue('Transitions definition was missing column{plural}: {missing_msg}.')
    stop(error_msg, call. = F)
  }
  
  # Check that all from states are represented
  missing_states <- which(!(state_names %in% x$from_state))
  if (length(missing_states) > 0) {
    missing_state_names <- state_names[missing_states]
    plural <- if (length(missing_state_names) > 1) 's' else ''
    missing_state_msg <- paste(missing_state_names, collapse = ', ')
    error_msg <- glue('Transitions definition missing state{plural}: {missing_state_msg}.')
    stop(error_msg, call. = F)
  }

  # Check that no transitions are duplicated
  trans_names <- glue("{x$from_state}\u2192{x$to_state}")
  dupe <- duplicated(trans_names)
  if (any(dupe)) {
    dupe_names <- unique(trans_names[dupe])
    plural <- if (length(dupe_names) > 1) 's' else ''
    dupe_msg <- paste(dupe_names, collapse = ', ')
    error_msg <- glue('Transitions definition contains duplicate enties for transition{plural}: {dupe_msg}.')
    stop(error_msg, call. = F)
  }
  
  # Check that formulas are not blank
  blank_index <- which(any(x$formula == '' | is.na(x$formula)))
  if (length(blank_index) > 0) {
    plural <- if (length(blank_index) > 1) 's' else ''
    blank_names <- glue("{x$from_state[blank_index]}\u2192{x$to_state[blank_index]}")
    blank_msg <- paste(blank_names, collapse = ', ')
    error_msg <- glue('Transitions definition contained blank formula for transitions{plural}: {blank_msg}.')
    stop(error_msg, call. = F)
  }
  
}




#' Evaluate a Longform Transition Matrix
#'
#' @param df A data frame with transition specifications
#' @param ns A namespace environment containing evaluated variables
#' @param simplify Logical indicating whether to simplify the result
#'
#' @return A data frame with evaluated transitions
#' @keywords internal
eval_trans_markov_lf <- function(df, ns, simplify = FALSE) {

  # Loop through each row in transitions, evaluate, then
  # combine results into a single dataframe
  n_transitions <- nrow(df)
  trans_list <- vector(mode = 'list', length = n_transitions)

  for (i in seq_len(n_transitions)) {
      row <- df[i, ]
      time_df <- filter(ns$df[ ,c('cycle', 'state_cycle')], .data$state_cycle <= row$max_st)
      time_df$from_state <- row$from_state
      time_df$to_state <- row$to_state
      time_df$from_state_group <- row$from_state_group
      time_df$to_state_group <- row$to_state_group
      time_df$share_state_time <- row$share_state_time
      time_df$value <- NA
      time_df$error <- NA
      
      # Evalulate transition formula
      value <- eval_formula(row$formula[[1]], ns, max_st = row$max_st)
      is_error <- is_oq_error(value)
      # Check if value was an error in evaluating the formula
      if (is_error) {
        accumulate_oq_error(value, context_msg = glue("Evaluation of transition '{row$name}'"))
        # Construct the error message using the transition name
        error_msg <- glue("Error evaluating transition '{row$name}': {paste0(value)}")
        # Check global option: stop or record error?
        if (getOption("openqaly.stop_on_error", default = FALSE)) {
          stop(error_msg, call. = FALSE)
        } else {
          # Original behavior: record the error message
          time_df$error <- value$message
        }
      }

      # Validate that the result is numeric and a valid probability
      if (!is_error) {
        tryCatch({
          # Use the validation function to check type and range
          value <- validate_transition_result(
            value,
            from_state = row$from_state,
            to_state = row$to_state,
            trans_name = row$name,
            formula_text = as.character(row$formula[[1]])
          )
          time_df$value <- as.numeric(value)
        }, error = function(e) {
          # Always stop on type/range validation errors - these are critical
          stop(e$message, call. = FALSE)
        })
      }
      if (isTRUE(simplify) && !is_error) {
        # Transform to matrix to check st-dependency
        val_mat <- lf_to_arr(time_df, c('state_cycle', 'cycle'), 'value')
        time_df$max_st <- min(row$max_st, arr_last_unique(val_mat, 1), na.rm = TRUE)
      } else {
        time_df$max_st <- row$max_st
      }
      
      # Return
      trans_list[[i]] <- time_df
  }
  res <- bind_rows(trans_list)
  # res <- rowwise(df) %>%
  #   group_split() %>%
  #   map(function(row, ns, simplify) {
  #       browser()
  #     # Populate at dataframe with time, from_state, to_state
  #     time_df <- ns$df[ ,c('cycle', 'state_cycle')]
  #     time_df$from_state <- row$from_state
  #     time_df$to_state <- row$to_state
  #     time_df$from_state_group <- row$from_state_group
  #     time_df$to_state_group <- row$to_state_group
  #     time_df$share_state_time <- row$share_state_time
  #     time_df$value <- NA
  #     time_df$error <- NA
      
  #     # Evalulate transition formula
  #     value <- eval_formula(row$formula[[1]], ns)
  #     is_error <- is_oq_error(value)
  #     # Check if value was an error in evaluating the formula
  #     if (is_error) {
  #       accumulate_oq_error(value, context_msg = glue("Evaluation of transition '{row$name}'"))
  #       # Construct the error message using the transition name
  #       error_msg <- glue("Error evaluating transition '{row$name}': {paste0(value)}")
  #       # Check global option: stop or record error?
  #       if (getOption("openqaly.stop_on_error", default = FALSE)) {
  #         stop(error_msg, call. = FALSE)
  #       } else {
  #         # Original behavior: record the error message
  #         time_df$error <- value$message
  #       }
  #     }

  #     # Check if value is numeric
  #     if (any(class(value) %in% c('numeric', 'integer'))) {
  #       time_df$value <- as.numeric(value)
  #     } else {
  #       # If not numeric, check if it's already an error handled above
  #       if (!is_oq_error(value)) {
  #           # Handle non-numeric result
  #           type <- class(value)[1]
  #           error_msg <- glue("Error evaluating transition '{row$name}': Result was type '{type}', expected numeric.")
  #           # Check global option: stop or record error?
  #           if (getOption("openqaly.stop_on_error", default = FALSE)) {
  #               stop(error_msg, call. = FALSE)
  #           } else {
  #               # Original behavior: record the error message
  #               time_df$error <- error_msg
  #           }
  #       }
  #       # If it IS an oq_error, it was handled by the previous if block
  #     }
  #     if (isTRUE(simplify) && !is_error) {
  #       # Transform to matrix to check st-dependency
  #       val_mat <- lf_to_arr(time_df, c('state_cycle', 'cycle'), 'value')
  #       time_df$max_st <- min(row$max_st, arr_last_unique(val_mat, 1), na.rm = TRUE)
  #     } else {
  #       time_df$max_st <- row$max_st
  #     }
      
  #     # Return
  #     time_df
  #   }, ns, simplify = isTRUE(simplify)) %>%
  #   bind_rows()

  oq_error_checkpoint()

  res
}

#' Convert Longform Transitions Table to Matrix
#'
#' @param df A data frame with transition data
#' @param state_names Character vector of state names
#'
#' @return A matrix of transitions
#' @keywords internal
lf_to_lf_mat <- function(df, state_names) {

  df <- arrange(
    mutate(
      df,
      from = as.integer(factor(.data$.from_e, levels = state_names)),
      to = as.integer(factor(.data$.to_e, levels  = state_names))
    ),
    .data$cycle,
    .data$from,
    -.data$value
  ) %>% select(
    "cycle", "from", "to", "value"
  )
  mat <- as.matrix(df)

  mat
}



#' Calculate complementary probabilities in an evaluated transition matrix
#'
#' @param mat A transition matrix array
#'
#' @return The matrix with complementary probabilities calculated
#' @keywords internal
calc_compl_probs <- function(mat) {
  posC <- mat == C
  c_counts <- rowSums(posC, dims = 2)
  state_names <- dimnames(mat)[[2]]
  colnames(c_counts) <- state_names
  if (!all(c_counts <= 1)) {
    problem_states <- c_counts[, apply(c_counts, 2, function(z) any(z > 1))]
    problems <- lapply(seq_len(ncol(problem_states)), function(i) {
      cycles <- problem_states[ , i]
      problem_cycles <- which(cycles > 1)
      min_cycle <- min(problem_cycles)
      max_cycle <- max(problem_cycles)
      if (all(problem_cycles == min_cycle:max_cycle)) {
        problem_cycles = glue("{min_cycle}-{max_cycle}")
      }
      data.frame(
        state = colnames(problem_states)[i],
        cycles = paste(problem_cycles, collapse = ', '),
        stringsAsFactors = F
      )
    }) %>%
      bind_rows() %>%
      as.data.frame()
    
    message <- paste0(
      'Error in transition matrix, keyword "C" used more than once per state:\n',
      paste(capture.output(problems), collapse = "\n")
    )
    stop(message, call. = F)
  }
  
  mat[posC] <- 0
  
  valC <- 1 - rowSums(mat, dims = 2)[which(posC, arr.ind = TRUE)[, -3]] 
  mat[posC] <- valC
  mat
}


#' Coerce to Long-Format Markov Transitions
#'
#' S3 generic function that coerces objects to the `lf_markov_trans` class,
#' which represents Markov transition definitions in long format.
#'
#' @param x An object to coerce.
#'
#' @return An object of class `lf_markov_trans`.
#'
#' @export
as.lf_markov_trans <- function(x) {
  UseMethod('as.lf_markov_trans', x)
}

#' @rdname as.lf_markov_trans
#' @export
as.lf_markov_trans.lf_markov_trans <- function(x) x

#' @rdname as.lf_markov_trans
#' @export
as.lf_markov_trans.data.frame <- function(x) {
  class(x) <- c('lf_markov_trans', class(x))
  x
}

# Ensure this is defined if not already, e.g., from R/misc.R or a common utils file
# For this edit, assuming it might not be directly available and defining a local version for safety.
# If R/misc.R is always sourced/loaded first, this redefinition is not strictly needed here.
create_empty_summaries_stubs_with_parsed <- function() {
  tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    values = character(0),
    type = character(0),
    wtp = numeric(0),
    parsed_values = list() # Key addition for parse_summaries output consistency
  )
}

# Function to Calculate Summaries
calculate_summaries <- function(parsed_summaries, values_df) {
  empty_summary_result <- tibble(summary = character(), value = character(), amount = numeric())

  # If parsed_summaries is empty (0 rows), no summaries to calculate.
  if (nrow(parsed_summaries) == 0) {
    return(empty_summary_result)
  }

  # Expand the summaries dataframe using parsed values
  # This should be safe even if parsed_summaries has 0 rows (though caught above now),
  # or if a row in parsed_summaries has an empty list in its parsed_values cell.
  expanded_summaries <- parsed_summaries %>%
    select(summary_col = "name", pv = "parsed_values") %>%
    unnest(cols = c("pv")) %>%
    rename(summary = "summary_col", value = "pv")

  # If after unnesting, there are no value mappings, return empty result.
  if (nrow(expanded_summaries) == 0) {
    return(empty_summary_result)
  }

  # Convert matrix to data frame if needed (e.g., from Rcpp output)
  if (!is.data.frame(values_df)) {
    values_df <- as.data.frame(values_df)
  }
  
  value_amounts <- tibble(value = character(0), amount = numeric(0))

  # Check if values_df has columns to pivot (besides a potential 'cycle' column)
  if (ncol(values_df) > 0) {
    # cppMarkovTransitionsAndTrace already adds rownames "0" to nCycles for trace,
    # and "1" to nCycles for value matrices.
    # For safety, ensure 'cycle' exists if we expect it or handle its absence.
    # The original code just did rownames_to_column.
    if (nrow(values_df) > 0 && !("cycle" %in% colnames(values_df))) {
        values_df <- rownames_to_column(values_df, "cycle")
    }

    cols_to_pivot <- setdiff(colnames(values_df), "cycle")

    if (length(cols_to_pivot) > 0 && nrow(values_df) > 0) {
      value_amounts <- values_df %>%
        pivot_longer(
          cols = all_of(cols_to_pivot),
          names_to = "value",
          values_to = "amount"
        ) %>%
        group_by(.data$value) %>%
        summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = "drop")
    } else if (length(cols_to_pivot) == 0 && nrow(values_df) > 0) {
      # Handle case where values_df has rows but only a cycle column (no actual values)
      # value_amounts remains an empty tibble, which is correct.
    }
  } # If ncol(values_df) == 0, value_amounts remains empty.
  
  # Join the expanded summaries with the value amounts
  result <- expanded_summaries %>%
    left_join(value_amounts, by = "value") %>%
    mutate(amount = ifelse(is.na(.data$amount), 0, .data$amount)) %>%
    select("summary", "value", "amount")
  
  return(result)
}

#' Parse and validate summaries
#'
#' @param summaries A dataframe containing summary definitions
#' @param model_values Vector of value names available in the model
#'
#' @return A validated summaries dataframe with parsed values
parse_summaries <- function(summaries, model_values) {
  # summaries is now guaranteed by read_model/convert_json_dataframes to be a tibble (possibly 0-row)
  # with columns: name, display_name, description, values.

  # If summaries is a 0-row tibble, return the standard empty structure for parsed summaries.
  if (nrow(summaries) == 0) {
    return(create_empty_summaries_stubs_with_parsed())
  }
  
  # Check required columns (already ensured by stubs, but good for direct calls)
  # This check can be removed if we fully trust the input now.
  required_cols <- c("name", "display_name", "description", "values")
  missing_cols <- setdiff(required_cols, colnames(summaries))
  if (length(missing_cols) > 0) {
    stop(glue("Summaries table is missing required columns: {paste(missing_cols, collapse = ', ')}"))
  }
  
  # Parse the comma-separated values in the 'values' column
  parsed_s <- summaries %>%
    mutate(
      parsed_values = map(.data$values, function(val_str) {
        if (is.na(val_str) || val_str == "") {
          return(character(0)) # Return empty character vector for empty/NA strings
        }
        trimws(unlist(strsplit(val_str, ",")))
      })
    )
  
  # Validate that all referenced values exist in model_values
  # model_values could be character(0) if no values in model
  all_referenced_values_in_summaries <- unique(unlist(parsed_s$parsed_values))
  
  if (length(all_referenced_values_in_summaries) > 0) { 
    if (length(model_values) > 0) { 
      unknown_values <- setdiff(all_referenced_values_in_summaries, model_values)
      if (length(unknown_values) > 0) {
        warning(paste0("The following values referenced in summaries do not exist in the model: ",
                      paste(unknown_values, collapse = ", ")))
      }
    } else {
      warning(glue("Summaries reference values (e.g., '{all_referenced_values_in_summaries[1]}'), but no values are defined in the model."))
    }
  }
  
  # Return the parsed and validated summaries (now includes parsed_values column)
  return(parsed_s)
}