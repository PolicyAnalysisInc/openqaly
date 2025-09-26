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
  weight_formula <- as.heRoFormula(as.character(group_row$weight))
  
  # Evaluate the formula in the namespace
  evaluated_weight <- eval_formula(weight_formula, namespace)
  
  # Check for errors in evaluation
  if (is_hero_error(evaluated_weight)) {
    accumulate_hero_error(evaluated_weight, context_msg = glue("Evaluation of weight for group '{group_row$name}'"))
    return(NA_real_)
  }
  
  # Validate the evaluated result
  if (!is.numeric(evaluated_weight)) {
    error_msg <- glue("Weight for group '{group_row$name}' must be numeric (got {class(evaluated_weight)[1]}).")
    accumulate_hero_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }
  
  if (length(evaluated_weight) != 1) {
    error_msg <- glue("Weight for group '{group_row$name}' must be length 1 (got length {length(evaluated_weight)}). Weight formulas cannot be time-dependent.")
    accumulate_hero_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }
  
  weight_value <- as.numeric(evaluated_weight)
  
  if (is.na(weight_value)) {
    error_msg <- glue("Weight for group '{group_row$name}' evaluated to NA.")
    accumulate_hero_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
    return(NA_real_)
  }
  
  if (!is.finite(weight_value)) {
    error_msg <- glue("Weight for group '{group_row$name}' must be finite (got {weight_value}).")
    accumulate_hero_error(define_error(error_msg), context_msg = glue("Group '{group_row$name}' weight validation"))
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
    filter(name == segment$group) %>%
    slice(1)
  
  if (nrow(group_row) == 0) {
    warning(glue("Group '{segment$group}' not found in model groups. Using default weight of 1."))
    return(1)  # Default weight if group not found
  }
  
  # Evaluate and return the weight
  return(evaluate_group_weight(group_row, namespace))
}

run_segment.markov <- function(segment, model, env, ...) {

  # Capture the extra arguments provided to function
  dots <- list(...)

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
    parsed_summaries <- tibble::tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      values = character(0),
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
  
  # Evaluate variables, initial state probabilities, transitions,
  # values, & summaries.
  eval_vars <- eval_variables(uneval_vars, ns)
  eval_states <- eval_states(uneval_states, eval_vars)
  eval_trans <- eval_trans_markov_lf(uneval_trans, eval_vars, model$settings$reduce_state_cycle)

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
    model$settings$reduce_state_cycle
  )

  expanded <- handle_state_expansion(eval_states, eval_trans, eval_values, state_time_use)

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

  # Create the object to return that will summarize the results of
  # this segment.
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment$inital_state <- list(eval_states)
  segment$trace_and_values <- list(calculated_trace_and_values)
  
  # Calculate the collapsed trace using the expanded_state_map from handle_state_expansion
  collapsed_trace <- calculate_collapsed_trace(
    calculated_trace_and_values, # This is the list from cppMarkovTransitionsAndTrace
    expanded$expanded_state_map
  )
  segment$collapsed_trace <- list(collapsed_trace)
  
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

handle_state_expansion <- function(init, transitions, values, state_time_use) {

  # Determine number of cycles
  n_cycles <- max(transitions$cycle)

  # Calculate maximum number of tunnels needed for each state
  # Join with state_time_use to only expand states that use state time
  st_maxes <- get_st_max(transitions, values, n_cycles) %>%
    left_join(state_time_use, by = "state") %>%
    mutate(
      # If uses_st is FALSE or NA, set max_st to 1 (no expansion)
      max_st = ifelse(is.na(uses_st) | !uses_st, 1, max_st)
    ) %>%
    select(state, max_st)

  # Expand initial state probabilities to include tunnel states
  expand_init <- expand_init_states(init, st_maxes)
  state_names <- colnames(expand_init)

  # Filter transition probabilities to only include required tunnel states
  eval_trans_limited <- select(transitions, -max_st) %>%
    left_join(st_maxes, by = c('from' = 'state')) %>%
    filter(state_cycle <= max_st)

  expanded_transitions <- eval_trans_limited %>%
    group_by(from) %>%
    mutate(.max_st = max(state_cycle)) %>%
    ungroup() %>%
    mutate(
      .end = state_cycle == .max_st,
      .from_e = expand_state_name(from, state_cycle)
    )
    lv_sg_i <- (!expanded_transitions$share_state_time) | (expanded_transitions$from_state_group != expanded_transitions$to_state_group)
    lv_i <- expanded_transitions$from != expanded_transitions$to & lv_sg_i
    ls_i <- expanded_transitions$.end & !lv_i
    nx_i <- !(lv_i | ls_i)
    expanded_transitions$.to_e <- NA
    expanded_transitions$.to_e[lv_i] <- expand_state_name(expanded_transitions$to[lv_i], 1)
    expanded_transitions$.to_e[ls_i] <- expand_state_name(expanded_transitions$to[ls_i], expanded_transitions$.max_st[ls_i])
    expanded_transitions$.to_e[nx_i] <- expand_state_name(expanded_transitions$to[nx_i], expanded_transitions$state_cycle[nx_i] + 1)

  # Generate data structure of transition probabilities to pass to rcpp function
  expanded_trans_matrix <- lf_to_lf_mat(expanded_transitions, state_names)

  # Create the mapping of original state names to their actual expanded names used
  expanded_state_map <- expanded_transitions %>%
    select(from, .from_e) %>%
    distinct()

  expand_trans_first_cycle <- select(
    filter(expanded_transitions, cycle == 1),
    from, to, .to_e, state_cycle
  )

  model_start_values <- filter(values, is.na(state), is.na(destination), state_cycle == 1)

  # Filter values to only include required tunnel states
  values_expanded <- select(values, -max_st) %>%
    left_join(st_maxes, by = c('state' = 'state')) %>%
    filter(
      state_cycle <= max_st,
      !(is.na(state) & is.na(destination) & state_cycle > 1)
    ) %>%
    mutate(
      .state_e = expand_state_name(state, state_cycle)
    ) %>%
    left_join(
      expand_trans_first_cycle,
      by = c(
        "state" = "from",
        "destination" = "to", 
        "state_cycle" = "state_cycle"
      )
    ) %>%
    transmute(
      state = .state_e,
      destination = .to_e,
      max_st,
      values_list
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

  # --- Helper Function Definition ---
  format_ranges <- function(numbers) {
    if (is.null(numbers) || length(numbers) == 0) {
      return("N/A")
    }
    unique_sorted_numbers <- sort(unique(as.integer(numbers)))
    if (length(unique_sorted_numbers) == 0) {
      return("N/A")
    }
    
    range_strings <- c()
    if (length(unique_sorted_numbers) == 0) return("N/A") # Defensive

    current_block_start <- unique_sorted_numbers[1]
    
    for (i in seq_along(unique_sorted_numbers)) {
      is_last_element <- (i == length(unique_sorted_numbers))
      is_discontinuity <- FALSE
      if (!is_last_element) {
        is_discontinuity <- (unique_sorted_numbers[i+1] != unique_sorted_numbers[i] + 1)
      }

      if (is_last_element || is_discontinuity) {
        if (current_block_start == unique_sorted_numbers[i]) {
          range_strings <- c(range_strings, as.character(current_block_start))
        } else {
          range_strings <- c(range_strings, paste0(current_block_start, "-", unique_sorted_numbers[i]))
        }
        if (!is_last_element && is_discontinuity) {
          current_block_start <- unique_sorted_numbers[i+1]
        }
      }
    }
    return(paste(range_strings, collapse = ", "))
  }
  # --- End Helper Function Definition ---

  # Determine number of cycles
  n_cycles <- max(transitions[,1])
  transitional_values <- filter(values, !is.na(state), !is.na(destination))
  residency_values <- filter(values, !is.na(state), is.na(destination))
  model_start_values <- filter(values, is.na(state), is.na(destination))
  state_names <- colnames(init) # These are the expanded state names, used by C++ and for mapping below
  
  trace_transitions_values <- cppMarkovTransitionsAndTrace(
    transitions, # This is expanded_trans_matrix with 1-based indices for from/to
    transitional_values,
    residency_values,
    model_start_values,
    as.numeric(init),
    state_names, # Pass expanded state names to C++
    value_names,
    as.integer(n_cycles),
    -pi, # complementConstant
    half_cycle_method
  )
  
  # Use the correct name from the C++ return list
  errors_df <- trace_transitions_values$errors

  # Proceed only if errors_df exists and contains errors
  # Ensure errors_df has the same number of rows as the input transitions matrix if we are to bind columns
  if (!is.null(errors_df) && nrow(errors_df) > 0 && ncol(errors_df) > 0 && 
      !is.null(transitions) && nrow(transitions) == nrow(errors_df)) { 

    # Add 'cycle', 'from' (expanded state index), and 'to' (expanded state index) from the input C++ transitions matrix
    # The 'transitions' variable here is the input to this R function, which was expanded_trans_matrix
    # It should have columns named "cycle", "from", "to" (1-based index)
    errors_df <- dplyr::bind_cols(
      dplyr::tibble(
        cycle = transitions[, "cycle"], 
        from_idx = transitions[, "from"], # Renamed to from_idx to avoid clash with 'from' function
        to_idx = transitions[, "to"]      # Added to_idx
      ),
      errors_df
    )

    # Convert to tibble for further processing (if bind_cols didn't already make it one)
    errors_df <- tibble::as_tibble(errors_df)
    
    # Define expected columns *after* adding cycle, from_idx, and to_idx
    expected_error_cols <- c("cycle", "from_idx", "to_idx", "complement", "probLessThanZero", "probGreaterThanOne", "sumNotEqualOne", "NaOrNaN")

    if (all(expected_error_cols %in% colnames(errors_df))) {

      # --- 1. Process Raw Errors ---
      # Add expanded names using indices
      errors_df <- errors_df %>%
        dplyr::mutate(
          expanded_from_state_name = state_names[from_idx],
          expanded_to_state_name = state_names[to_idx] # Added expanded_to_state_name
        )

      # Parse collapsed name and state time for FROM state
      from_state_name_parts <- stringr::str_match(errors_df$expanded_from_state_name, "^(.*)\\.(\\d+)$") 
      errors_df <- errors_df %>%
        dplyr::mutate(
          collapsed_from_state_name = ifelse(is.na(from_state_name_parts[, 2]), expanded_from_state_name, from_state_name_parts[, 2]),
          from_state_time = ifelse(is.na(from_state_name_parts[, 3]), 1L, as.integer(from_state_name_parts[, 3]))
        )

      # Parse collapsed name and state time for TO state
      to_state_name_parts <- stringr::str_match(errors_df$expanded_to_state_name, "^(.*)\\.(\\d+)$")
      errors_df <- errors_df %>%
        dplyr::mutate(
          collapsed_to_state_name = ifelse(is.na(to_state_name_parts[, 2]), expanded_to_state_name, to_state_name_parts[, 2]),
          # to_state_time might not be directly used in the table if "State Times" refers to 'from'
          to_state_time = ifelse(is.na(to_state_name_parts[, 3]), 1L, as.integer(to_state_name_parts[, 3]))
        )

      # Determine which states involved in errors are expanded (based on FROM state)
      # This logic for 'is_expanded' remains based on the 'from' state perspective for the error.
      state_expansion_info <- errors_df %>%
        dplyr::filter(complement | probLessThanZero | probGreaterThanOne | sumNotEqualOne | NaOrNaN) %>%
        dplyr::group_by(collapsed_from_state_name) %>%
        dplyr::summarize(
           .max_st = if (any(!is.na(from_state_time))) max(from_state_time, na.rm = TRUE) else NA_real_,
           .groups = 'drop'
         ) %>%
        dplyr::mutate(is_expanded_calc = is.finite(.max_st) & .max_st > 1) %>%
        dplyr::select(collapsed_from_state_name, is_expanded_calc)

      # Pivot error flags to long format
      processed_errors <- errors_df %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(c("complement", "probLessThanZero", "probGreaterThanOne", "sumNotEqualOne", "NaOrNaN")),
          names_to = "error_raw",
          values_to = "is_error"
        ) %>%
        dplyr::filter(is_error == TRUE) %>%
        dplyr::mutate(
          error_type = dplyr::case_when(
            error_raw == "complement" ~ "Complement ('C') specified more than once for state/cycle",
            error_raw == "probLessThanZero" ~ "Transition probability less than 0",
            error_raw == "probGreaterThanOne" ~ "Transition probability greater than 1",
            error_raw == "sumNotEqualOne" ~ "Transition probabilities for state do not sum to 1",
            error_raw == "NaOrNaN" ~ "NA or NaN value found in transition probabilities",
            TRUE ~ paste("Unknown Error Type:", error_raw)
          ),
          # Determine final "To State" based on error type
          # For state-level errors (complement, sumNotEqualOne), "To State" is not applicable
          final_to_state = ifelse(
            error_raw %in% c("probLessThanZero", "probGreaterThanOne", "NaOrNaN"),
            collapsed_to_state_name,
            NA_character_ # Use NA, will be replaced by "" later for display
          )
        ) %>%
        dplyr::left_join(state_expansion_info, by = "collapsed_from_state_name") %>%
        dplyr::mutate(is_expanded = ifelse(is.na(is_expanded_calc), FALSE, is_expanded_calc)) %>%
        # Select relevant columns for consolidation
        dplyr::select(
            cycle, 
            collapsed_from_state_name, 
            from_state_time, 
            final_to_state, # Added
            error_type, 
            is_expanded
        ) %>%
        dplyr::distinct()

      if (nrow(processed_errors) > 0) {

        # --- 2. Consolidate State Time Ranges (for FROM state) ---
        cycle_st_errors <- processed_errors %>%
          dplyr::arrange(collapsed_from_state_name, final_to_state, error_type, cycle, from_state_time) %>%
          dplyr::group_by(collapsed_from_state_name, final_to_state, error_type, cycle, is_expanded) %>%
          dplyr::summarise(
            # Collect all from_state_time values for the group
            all_from_state_times = list(unique(from_state_time)), 
            is_expanded = dplyr::first(is_expanded), # Keep, refers to from_state expansion
            .groups = 'drop'
          ) %>%
          # Apply format_ranges to the collected state times
          dplyr::mutate(
            from_state_time_range = ifelse(is_expanded, sapply(all_from_state_times, format_ranges), "N/A")
          ) %>%
          dplyr::select(-all_from_state_times) # Remove the temporary list column

        # --- 3. Consolidate Cycle Ranges ---
        final_consolidated_errors <- cycle_st_errors %>%
          # Ensure types are correct before arrange/group for from_state_time_range
          dplyr::mutate(
             from_state_time_range = as.character(from_state_time_range) 
          ) %>% 
          dplyr::arrange(collapsed_from_state_name, final_to_state, error_type, from_state_time_range, cycle) %>%
          # Group by everything EXCEPT cycle to collect all cycles for these groups
          dplyr::group_by(collapsed_from_state_name, final_to_state, error_type, from_state_time_range, is_expanded) %>%
          # Summarize to get the formatted cycle_range string
          dplyr::summarise(
            cycle_range = format_ranges(cycle), # Use the helper function here
            # is_expanded is constant within this group, take the first
            is_expanded = dplyr::first(is_expanded),
            .groups = 'drop' 
          ) %>%
          # Rename and select final columns for reporting logic
          dplyr::select(
            `From State` = collapsed_from_state_name,
            `State Time` = from_state_time_range, # Renamed
            `To State` = final_to_state,                 # Added
            `Cycles` = cycle_range,
            `Error Message` = error_type,
            is_expanded # Keep for 'State Time' column inclusion logic
          )

        # --- 4. Format and Issue Warning ---
        if (nrow(final_consolidated_errors) > 0) {
          
          total_errors <- nrow(final_consolidated_errors)
          trunc_msg <- ""
          # Replace NA in 'To State' with empty string for display
          table_data_processed <- final_consolidated_errors %>%
            dplyr::mutate(`To State` = ifelse(is.na(`To State`), "", `To State`))

          if (total_errors > 40) {
            table_data_processed <- utils::head(table_data_processed, 40)
            trunc_msg <- paste0("\n... (showing top 40 of ", total_errors, " errors)")
          }

          # Check if 'State Time' Column is Needed (based on the FROM state's expansion)
          include_st_col <- any(final_consolidated_errors$is_expanded) # Based on original full error set

          if (include_st_col) {
            table_data <- table_data_processed %>% 
                dplyr::select(`From State`, `State Time`, `To State`, `Cycles`, `Error Message`)
          } else {
            table_data <- table_data_processed %>% 
                dplyr::select(`From State`, `To State`, `Cycles`, `Error Message`)
          }

          col_widths <- sapply(colnames(table_data), function(col) {
              pmax(nchar(col), if(nrow(table_data) > 0) max(nchar(as.character(table_data[[col]])), na.rm = TRUE) else 0)
          })

          header <- paste0("| ", paste(format(colnames(table_data), width = col_widths, justify = "left"), collapse = " | "), " |")
          separator <- paste0("|-", paste(sapply(col_widths, function(w) paste(rep("-", w), collapse = "")), collapse = "-|-"), "-|")

          rows <- apply(table_data, 1, function(row) {
            paste0("| ", paste(format(as.character(row), width = col_widths, justify = "left"), collapse = " | "), " |")
          })

          table_string <- paste(c(header, separator, rows), collapse = "\n")
          console_message <- paste0("Transition probability errors detected:\n\n", table_string, trunc_msg)
          
          error_mode <- getOption("heRomod2.error_mode", default = "warning")
          if (error_mode == "checkpoint") {
            stop(console_message, call. = FALSE)
          } else {
            warning(console_message, call. = FALSE)
          }
        } # End if final_consolidated_errors has rows
      } # End if processed_errors has rows
    } else {
       warning("Transition probability errors might exist, but the 'errors' table from C++ has unexpected columns.", call. = FALSE)
    }
  } else {
    # Errors might exist, but C++ returned unexpected columns or errors_df wasn't valid
    # Check if it's actually a matrix/df before warning
    if(is.matrix(errors_df) || is.data.frame(errors_df)) {
      # This case means errors_df existed but didn't have the expected columns
      warning("Transition probability errors might exist, but the 'errors' table from C++ has unexpected columns.", call. = FALSE)
    } else if (!is.null(errors_df)) {
      # If it's not NULL but not matrix/df, something else is wrong
      warning("Transition probability errors might exist, but the 'errors' object returned by C++ is not a matrix or data frame.", call. = FALSE)
    }
    # If errors_df was NULL or 0 rows initially, this outer 'if' is skipped, so no warning here
  }

  # Modify the transitions component of the result
  output_trans_matrix <- trace_transitions_values$transitions
  
  if (!is.null(output_trans_matrix) && is.matrix(output_trans_matrix) && nrow(output_trans_matrix) > 0) {
    # Ensure colnames are set if they are not (e.g. cycle, from, to, value)
    # The C++ function cppMarkovTransitionsAndTrace returns a matrix for 'transitions'
    # which is based on the input 'transitions' matrix structure. Assuming it has these cols.
    # Input 'transitions' to cppMarkovTransitionsAndTrace has columns: cycle, from, to, value.
    # 'from' and 'to' are 1-based indices.
    
    trans_df <- as_tibble(output_trans_matrix)
    # If C++ output matrix doesn't have names, assign them based on expected structure
    # Typically, the C++ function would preserve or set names. This is a fallback.
    if(is.null(colnames(trans_df)) || !all(c("cycle", "from", "to", "value") %in% colnames(trans_df))) {
        colnames(trans_df) <- c("cycle", "from", "to", "value")
    }

    # 'state_names' are the expanded state names corresponding to the 1-based indices
    trans_df_processed <- trans_df %>%
      mutate(
        from_expanded = state_names[from], # 'from' is 1-based index
        to_expanded = state_names[to]      # 'to' is 1-based index
      ) %>%
      # expanded_state_map has 'from' (original/collapsed) and '.from_e' (expanded)
      left_join(expanded_state_map %>% select(from_coll = from, from_exp_key = .from_e),
                by = c("from_expanded" = "from_exp_key")) %>%
      left_join(expanded_state_map %>% select(to_coll = from, to_exp_key = .from_e),
                by = c("to_expanded" = "to_exp_key")) %>%
      select(
        cycle,
        from_collapsed = from_coll,
        from_expanded,
        to_collapsed = to_coll,
        to_expanded,
        value
      )
      
    trace_transitions_values$transitions <- trans_df_processed
  } else {
    # If transitions matrix is empty or NULL, create an empty tibble with the new schema
    trace_transitions_values$transitions <- tibble::tibble(
      cycle = integer(0),
      from_collapsed = character(0),
      from_expanded = character(0),
      to_collapsed = character(0),
      to_expanded = character(0),
      value = numeric(0)
    )
  }
  
  trace_transitions_values
}

calculate_collapsed_trace <- function(expanded_results, expanded_state_map) {
  
  # The trace is the first element of the list returned by cppMarkovTransitionsAndTrace
  expanded_trace_matrix <- expanded_results[[1]] 
  
  # Get all actual expanded state names that are column names in the trace matrix
  actual_trace_col_names <- colnames(expanded_trace_matrix)
  
  # Get unique original state names from the mapping
  # These will be the columns in our collapsed trace
  original_state_names <- unique(expanded_state_map$from)
  
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
    child_expanded_names_from_map <- expanded_state_map$.from_e[expanded_state_map$from == original_name]
    
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
    rename(trans[ , c('from', 'max_st')], state = from),
    filter(values, !is.na(state))[ , c('state', 'max_st')]
  )
  
  # Group by state and get the maximum state time. For any
  # states where the maximum is infinite, set it to the
  # maximum number of cycles
  st_maxes <- combined_transitions_and_values %>%
    group_by(state) %>%
    summarize(max_st = max(max_st)) %>%
    mutate(max_st = ifelse(is.infinite(max_st), n_cycles, max_st))

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
  x$formula <- map(x$formula, as.heRoFormula)
  x$name <- paste0(x$from, '→', x$to)

  res <- sort_variables(x, vars) %>%
    select(name, from, to, formula) %>%
    left_join(
      transmute(
        states, name = name,
        from_state_group = state_group,
        share_state_time = share_state_time,
        max_st = ifelse(max_state_time == 0, Inf, max_state_time)
      ),
      by = c('from' = 'name')
    ) %>%
    left_join(
      transmute(
        states,
        name = name,
        to_state_group = state_group
      ),
      by = c('to' = 'name')
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
  missing_states <- which(!(state_names %in% x$from))
  if (length(missing_states) > 0) {
    missing_state_names <- state_names[missing_states]
    plural <- if (length(missing_state_names) > 1) 's' else ''
    missing_state_msg <- paste(missing_state_names, collapse = ', ')
    error_msg <- glue('Transitions definition missing state{plural}: {missing_state_msg}.')
    stop(error_msg, call. = F)
  }
  
  # Check that no transitions are duplicated
  trans_names <- paste0(x$from, '→', x$to)
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
    blank_names <- paste0(x$from[blank_index], '→', x$to[blank_index])
    blank_msg <- paste(blank_names, collapse = ', ')
    error_msg <- glue('Transitions definition contained blank formula for transitions{plural}: {blank_msg}.')
    stop(error_msg, call. = F)
  }
  
}


# Helpers --------------------------------------------------------------

# Helper function to format sorted numbers into comma-separated strings with ranges
format_ranges <- function(numbers) {
  if (is.null(numbers) || length(numbers) == 0) {
    return("N/A")
  }
  unique_sorted_numbers <- sort(unique(as.integer(numbers)))
  if (length(unique_sorted_numbers) == 0) {
    return("N/A")
  }
  
  range_strings <- c()
  if (length(unique_sorted_numbers) == 0) return("N/A") # Defensive

  current_block_start <- unique_sorted_numbers[1]
  
  for (i in seq_along(unique_sorted_numbers)) {
    is_last_element <- (i == length(unique_sorted_numbers))
    is_discontinuity <- FALSE
    if (!is_last_element) {
      is_discontinuity <- (unique_sorted_numbers[i+1] != unique_sorted_numbers[i] + 1)
    }

    if (is_last_element || is_discontinuity) {
      if (current_block_start == unique_sorted_numbers[i]) {
        range_strings <- c(range_strings, as.character(current_block_start))
      } else {
        range_strings <- c(range_strings, paste0(current_block_start, "-", unique_sorted_numbers[i]))
      }
      if (!is_last_element && is_discontinuity) {
        current_block_start <- unique_sorted_numbers[i+1]
      }
    }
  }
  return(paste(range_strings, collapse = ", "))
}

limit_state_time <- function(df, state_time_limits) {
  # Join data with state time limit
  left_join(df, state_time_limits, by = "state") %>%
    # Remove any entries that exceed limit
    filter(state_cycle <= st_limit)
}

#' Evaluate a Longform Transition Matrix
eval_trans_markov_lf <- function(df, ns, simplify = FALSE) {
  
  # Loop through each row in transitions, evaluate, then
  # combine results into a single dataframe
  res <- rowwise(df) %>%
    group_split() %>%
    map(function(row, ns, simplify = F) {
      # Populate at dataframe with time, from, to
      time_df <- ns$df[ ,c('cycle', 'state_cycle')]
      time_df$from <- row$from
      time_df$to <- row$to
      time_df$from_state_group <- row$from_state_group
      time_df$to_state_group <- row$to_state_group
      time_df$share_state_time <- row$share_state_time
      time_df$value <- NA
      time_df$error <- NA
      
      # Evalulate transition formula
      value <- eval_formula(row$formula[[1]], ns)
      is_error <- is_hero_error(value)
      # Check if value was an error in evaluating the formula
      if (is_error) {
        accumulate_hero_error(value, context_msg = glue("Evaluation of transition '{row$name}'"))
        # Construct the error message using the transition name
        error_msg <- glue("Error evaluating transition '{row$name}': {paste0(value)}")
        # Check global option: stop or record error?
        if (getOption("heRomod2.stop_on_error", default = FALSE)) {
          stop(error_msg, call. = FALSE)
        } else {
          # Original behavior: record the error message
          time_df$error <- value$message
        }
      }
      
      # Check if value is numeric
      if (any(class(value) %in% c('numeric', 'integer'))) {
        time_df$value <- as.numeric(value)
      } else {
        # If not numeric, check if it's already an error handled above
        if (!is_hero_error(value)) {
            # Handle non-numeric result
            type <- class(value)[1]
            error_msg <- glue("Error evaluating transition '{row$name}': Result was type '{type}', expected numeric.")
            # Check global option: stop or record error?
            if (getOption("heRomod2.stop_on_error", default = FALSE)) {
                stop(error_msg, call. = FALSE)
            } else {
                # Original behavior: record the error message
                time_df$error <- error_msg
            }
        }
        # If it IS a hero_error, it was handled by the previous if block
      }

      if (simplify && !is_error) {
        # Transform to matrix to check st-dependency
        val_mat <- lf_to_arr(time_df, c('state_cycle', 'cycle'), 'value')
        time_df$max_st <- min(row$max_st, arr_last_unique(val_mat, 1), na.rm = TRUE)
      } else {
        time_df$max_st <- row$max_st
      }
      
      # Return
      time_df
    }, ns, simplify = simplify) %>%
    bind_rows()

  hero_error_checkpoint()

  res
}

#' Convert Lonform Transitions Table to Matrix
lf_to_lf_mat <- function(df, state_names) {

  df <- arrange(
    mutate(
      df,
      from = as.integer(factor(.from_e, levels = state_names)),
      to = as.integer(factor(.to_e, levels  = state_names))
    ),
    cycle,
    from,
    -value
  ) %>% select(
    cycle, from, to, value
  )
  mat <- as.matrix(df)

  mat
}


#' Convert Lonform Transitions Table to Matrix
lf_to_tmat <- function(df) {
  df <- df %>%
    group_by(from) %>%
    mutate(.max_st = max(state_cycle)) %>%
    ungroup() %>%
    mutate(
      .end = state_cycle == .max_st,
      .from_e = expand_state_name(from, state_cycle)
    )
  lv_sg_i <- (!df$share_state_time) | (df$from_state_group != df$to_state_group)
  lv_i <- df$from != df$to & lv_sg_i
  ls_i <- df$.end & !lv_i
  nx_i <- !(lv_i | ls_i)
  df$.to_e <- NA
  df$.to_e[lv_i] <- expand_state_name(df$to[lv_i], 1)
  df$.to_e[ls_i] <- expand_state_name(df$to[ls_i], df$.max_st[ls_i])
  df$.to_e[nx_i] <- expand_state_name(df$to[nx_i], df$state_cycle[nx_i] + 1)
  e_state_names <- unique(df$.from_e)
  df$to <- factor(df$.to_e, levels = e_state_names)
  df$from <- factor(df$.from_e, levels  = e_state_names)
  df <- df[, c('cycle', 'state_cycle', 'from', 'to', 'value')]
  mat <- lf_to_arr(df, c('cycle', 'from', 'to'), 'value')
  dimnames(mat) <- list(
    unique(df$cycle),
    e_state_names,
    e_state_names
  )
  
  # Calculate complementary probabilities
  calc_compl_probs(mat)
}

#' Calculate complementary probabilities in an evaluated transition matrix
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
        problem_cycles = paste0(min_cycle, '-', max_cycle)
      }
      data.frame(
        state = colnames(problem_states)[i],
        cycles = paste(problem_cycles, collapse = ', '),
        stringsAsFactors = F
      )
    }) %>%
      dplyr::bind_rows() %>%
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

check_matrix_probs <- function(mat) {
  
}

# Type coercion methods
#' @export
as.lf_markov_trans <- function(x) {
  UseMethod('as.lf_markov_trans', x)
}

# lf_markov_trans => lf_markov_trans
#' @export
as.lf_markov_trans.lf_markov_trans <- function(x) x

# data.frame => lf_markov_trans
#' @export
as.lf_markov_trans.data.frame <- function(x) {
  class(x) <- c('lf_markov_trans', class(x))
  x
}

# Ensure this is defined if not already, e.g., from R/misc.R or a common utils file
# For this edit, assuming it might not be directly available and defining a local version for safety.
# If R/misc.R is always sourced/loaded first, this redefinition is not strictly needed here.
create_empty_summaries_stubs_with_parsed <- function() {
  tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    values = character(0),
    parsed_values = list() # Key addition for parse_summaries output consistency
  )
}

# Function to Calculate Summaries
calculate_summaries <- function(parsed_summaries, values_df) {
  empty_summary_result <- tibble::tibble(summary = character(), value = character(), amount = numeric())

  # If parsed_summaries is empty (0 rows), no summaries to calculate.
  if (nrow(parsed_summaries) == 0) {
    return(empty_summary_result)
  }

  # Expand the summaries dataframe using parsed values
  # This should be safe even if parsed_summaries has 0 rows (though caught above now),
  # or if a row in parsed_summaries has an empty list in its parsed_values cell.
  expanded_summaries <- parsed_summaries %>%
    dplyr::select(summary_col = name, pv = parsed_values) %>%
    tidyr::unnest(cols = pv) %>%
    dplyr::rename(summary = summary_col, value = pv)

  # If after unnesting, there are no value mappings, return empty result.
  if (nrow(expanded_summaries) == 0) {
    return(empty_summary_result)
  }

  # Convert matrix to data frame if needed (e.g., from Rcpp output)
  if (!is.data.frame(values_df)) {
    values_df <- as.data.frame(values_df)
  }
  
  value_amounts <- tibble::tibble(value = character(0), amount = numeric(0))

  # Check if values_df has columns to pivot (besides a potential 'cycle' column)
  if (ncol(values_df) > 0) {
    # Add rownames as 'cycle' column if it doesn't exist and there are rows
    # cppMarkovTransitionsAndTrace already adds rownames "0" to nCycles for trace,
    # and "1" to nCycles for value matrices.
    # For safety, ensure 'cycle' exists if we expect it or handle its absence.
    # The original code just did rownames_to_column.
    if (nrow(values_df) > 0 && !("cycle" %in% colnames(values_df))) {
        values_df <- tibble::rownames_to_column(values_df, "cycle")
    }

    cols_to_pivot <- setdiff(colnames(values_df), "cycle")

    if (length(cols_to_pivot) > 0 && nrow(values_df) > 0) {
      value_amounts <- values_df %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(cols_to_pivot),
          names_to = "value",
          values_to = "amount"
        ) %>%
        dplyr::group_by(value) %>%
        dplyr::summarize(amount = sum(amount, na.rm = TRUE), .groups = "drop")
    } else if (length(cols_to_pivot) == 0 && nrow(values_df) > 0) {
      # Handle case where values_df has rows but only a cycle column (no actual values)
      # value_amounts remains an empty tibble, which is correct.
    }
  } # If ncol(values_df) == 0, value_amounts remains empty.
  
  # Join the expanded summaries with the value amounts
  result <- expanded_summaries %>%
    dplyr::left_join(value_amounts, by = "value") %>%
    dplyr::mutate(amount = ifelse(is.na(amount), 0, amount)) %>%
    dplyr::select(summary, value, amount)
  
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
    stop(paste0("Summaries table is missing required columns: ", paste(missing_cols, collapse = ", ")))
  }
  
  # Parse the comma-separated values in the 'values' column
  parsed_s <- summaries %>%
    dplyr::mutate(
      parsed_values = purrr::map(values, function(val_str) {
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
      warning(paste0("Summaries reference values (e.g., '", all_referenced_values_in_summaries[1] ,"'), but no values are defined in the model."))
    }
  }
  
  # Return the parsed and validated summaries (now includes parsed_values column)
  return(parsed_s)
}