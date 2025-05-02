parse_markov <- function(model) {
  define_object_(model, class = 'markov')
}

run_segment <- function(segment, model, env, ...) {
  UseMethod('run_segment', model)
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
  
  # Parse summaries if they exist
  parsed_summaries <- NULL
  if (!is.null(model$summaries)) {
    parsed_summaries <- parse_summaries(model$summaries, unique(model$values$name))
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

  value_names <- unique(model$values$name)
  state_names <- unique(model$states$name)

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
    value_names
  )
  
  # Create the object to return that will summarize the results of
  # this segment.
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)
  segment$inital_state <- list(eval_states)
  segment$trace_and_values <- list(calculated_trace_and_values)
  
  # Calculate summaries if parsed_summaries exists
  if (!is.null(parsed_summaries)) {
    segment$summaries <- list(calculate_summaries(
      parsed_summaries, 
      calculated_trace_and_values$values
    ))
  }
  
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
    values = values_expanded
  )
}

calculate_trace_and_values <- function(init, transitions, values, value_names) {

  # Determine number of cycles
  n_cycles <- max(transitions[,1])
  transitional_values <- filter(values, !is.na(state), !is.na(destination))
  residency_values <- filter(values, !is.na(state), is.na(destination))
  model_start_values <- filter(values, is.na(state), is.na(destination))
  state_names <- colnames(init)
  
  # Rename states according to expanded state names using existing function but modifying it to take into account max_st
  # Have values sorted properly and make sure all are represented in each list, and list is ordered by states, and correct names are applied
  trace_transitions_values <- cppMarkovTransitionsAndTrace(
    transitions,
    transitional_values,
    residency_values,
    model_start_values,
    as.numeric(init),
    state_names,
    value_names,
    as.integer(n_cycles),
    -pi
  )
  
  # Calculate trace and values in rcpp
  # foo <- MarkovTraceAndValues(
  #   trans_lf_mat,
  #   values_list_of_lists,
  #   as.numeric(expand_init),
  #   as.integer(n_cycles),
  #   as.integer(length(expand_init)),
  #   length(eval_values_limited$values_list[[1]]),
  #   -pi
  # )

  trace_transitions_values

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
        from_state_group = state_group
      ),
      by = c('from' = 'name')
    ) %>%
    left_join(
      transmute(
        states,
        name = name,
        to_state_group = state_group,
        share_state_time = share_state_time,
        max_st = max_state_time
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
        time_df$max_st <- min(row$max_st, arr_last_unique(val_mat, 1))
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

# Function to Calculate Summaries

calculate_summaries <- function(parsed_summaries, values_df) {
  # Convert matrix to data frame if needed
  if (!is.data.frame(values_df)) {
    values_df <- as.data.frame(values_df)
  }
  
  # Expand the summaries dataframe using parsed values
  expanded_summaries <- parsed_summaries %>%
    select(summary = name, parsed_values) %>%
    unnest(parsed_values) %>%
    rename(value = parsed_values)
  
  # Convert values dataframe to long format and sum across cycles
  value_amounts <- values_df %>%
    rownames_to_column("cycle") %>%
    pivot_longer(
      cols = -cycle,
      names_to = "value",
      values_to = "amount"
    ) %>%
    group_by(value) %>%
    summarize(amount = sum(amount), .groups = "drop")
  
  # Join the expanded summaries with the value amounts
  result <- expanded_summaries %>%
    left_join(value_amounts, by = "value") %>%
    mutate(amount = ifelse(is.na(amount), 0, amount)) %>%
    select(summary, value, amount)
  
  return(result)
}

#' Parse and validate summaries
#'
#' @param summaries A dataframe containing summary definitions
#' @param model_values Vector of value names available in the model
#'
#' @return A validated summaries dataframe with parsed values
parse_summaries <- function(summaries, model_values) {
  # Check if summaries is NULL - if so, return NULL
  if (is.null(summaries)) {
    return(NULL)
  }
  
  # Check required columns
  required_cols <- c("name", "display_name", "description", "values")
  missing_cols <- setdiff(required_cols, colnames(summaries))
  
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required columns in summaries: ", 
                paste(missing_cols, collapse = ", ")))
  }
  
  # Parse the comma-separated values
  parsed_summaries <- summaries %>%
    mutate(
      parsed_values = map(values, function(val_str) {
        if (is.na(val_str) || val_str == "") {
          return(character(0))
        }
        trimws(unlist(strsplit(val_str, ",")))
      })
    )
  
  # Validate that all referenced values exist in model_values
  all_summary_values <- unique(unlist(parsed_summaries$parsed_values))
  unknown_values <- setdiff(all_summary_values, model_values)
  
  if (length(unknown_values) > 0) {
    warning(paste0("The following values referenced in summaries do not exist in the model: ",
                  paste(unknown_values, collapse = ", ")))
  }
  
  # Return the parsed and validated summaries
  return(parsed_summaries)
}