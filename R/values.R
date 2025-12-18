parse_values <- function(x, states, extra_vars) {
  
  # If x is NULL (should be handled by read_model now) or an empty tibble, 
  # return an appropriately structured empty tibble immediately.
  if (is.null(x) || nrow(x) == 0) {
    # This structure should match what `as.values(vars)` would produce for an empty set.
    # Key columns from `vars` after processing in the main path:
    # name, display_name, description, state, destination, formula (from x itself)
    # plus max_st (added later).
    # `sort_variables` might add `depends`, `depend_on_past`, `is_cycle_dep` etc.
    # For simplicity, we ensure the columns that would be selected and added.
    empty_parsed_values <- tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      state = character(0),
      destination = character(0),
      formula = list(), # Formulas are parsed to heRoFormula objects, so list() for empty case
      type = character(0),
      max_st = numeric(0),
      .rows = 0
    )
    return(as.values(empty_parsed_values)) # Assuming as.values can handle this structure
  }

  # Check that values definition is valid
  check_values_df(x)
  
  # Check for duplicate values by name, state, and destination
  duplicates <- x %>%
    group_by(name, state, destination) %>%
    filter(n() > 1) %>%
    ungroup()
  
  if (nrow(duplicates) > 0) {
    dup_values <- duplicates %>%
      select(name, state, destination) %>%
      distinct() %>%
      mutate(combined = glue("name: {name}, state: {state}, destination: {destination}"))
    
    stop("Duplicate values found. Values must be unique by name, state, and destination combination:\n",
         paste(dup_values$combined, collapse = "\n"))
  }

  # Parse values and sort
  vars <- x %>%
    group_by(state, destination) %>%
    do({
      as.data.frame(.) %>%
        select(name, display_name, description, state, destination, formula, type) %>%
        mutate(formula = map(formula, as.heRoFormula)) %>%
        sort_variables(extra_vars)
    }) %>%
    ungroup()

  # For Markov models, join max_state_time for tunnel state expansion
  # For PSM models, states don't have max_state_time, so set max_st = 1
  if ("max_state_time" %in% names(states)) {
    vars <- vars %>%
      left_join(select(states, name, max_st_from_state = max_state_time), by = c('state' = 'name')) %>%
      mutate(max_st = ifelse(is.na(max_st_from_state), 1, ifelse(max_st_from_state == 0, Inf, max_st_from_state)))
  } else {
    vars <- vars %>%
      mutate(max_st = 1)
  }
  
  # Construct Object & Return
  as.values(vars)
}

check_values_df <- function(x) {
  # At this point, x already has correct types from check_tbl() in read_model
  # This function adds business logic validation

  # Validate name format (only for non-NA names)
  non_na_names <- x$name[!is.na(x$name)]
  invalid_names <- non_na_names[!is_valid_name(non_na_names)]
  if (length(invalid_names) > 0) {
    stop(glue("Invalid value names: {err_name_string(invalid_names)}. Names must start with a letter and contain only letters, numbers, and underscores."))
  }

  # Validate formulas are parseable (only non-NA formulas)
  non_na_formulas <- which(!is.na(x$formula))
  for (i in non_na_formulas) {
    tryCatch({
      parse(text = x$formula[i])
    }, error = function(e) {
      stop(glue("Invalid formula syntax for value '{x$name[i]}': {e$message}"))
    })
  }
}

as.values <- function(x) x

# Helper function to format sorted numbers into comma-separated strings with ranges
# (Similar to the one in R/markov.R)
format_ranges_for_eval_values <- function(numbers) {
  if (is.null(numbers) || length(numbers) == 0 || all(is.na(numbers))) {
    return("N/A")
  }
  unique_sorted_numbers <- sort(unique(as.integer(na.omit(numbers))))
  if (length(unique_sorted_numbers) == 0) {
    return("N/A")
  }
  
  range_strings <- c()
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
        range_strings <- c(range_strings, glue("{current_block_start}-{unique_sorted_numbers[i]}"))
      }
      if (!is_last_element && is_discontinuity) {
        current_block_start <- unique_sorted_numbers[i+1]
      }
    }
  }
  return(paste(range_strings, collapse = ", "))
}

# Helper function to format a data frame into a markdown table string
format_na_table_to_markdown_for_eval_values <- function(table_data_df, message_prefix) {
  if (nrow(table_data_df) == 0) {
    return("")
  }

  # Ensure 'Destination' is character for consistent formatting if it contains actual NAs
  if ("Destination" %in% colnames(table_data_df)) {
    table_data_df <- mutate(table_data_df, Destination = as.character(Destination))
  }
  
  # Format NA values as "N/A" for display purposes
  cols_to_format_na <- intersect(colnames(table_data_df), c("Value Name", "State", "Destination", "Cycles", "State Cycles"))
  for(col_name in cols_to_format_na) {
    table_data_df[[col_name]] <- ifelse(is.na(table_data_df[[col_name]]), "N/A", table_data_df[[col_name]])
  }

  col_widths <- sapply(colnames(table_data_df), function(col) {
      max(nchar(col), if(nrow(table_data_df) > 0) max(nchar(as.character(table_data_df[[col]])), na.rm = TRUE) else 0, na.rm=TRUE)
  })

  header <- paste0("| ", paste(format(colnames(table_data_df), width = col_widths, justify = "left"), collapse = " | "), " |")
  separator <- paste0("|-", paste(sapply(col_widths, function(w) paste(rep("-", w), collapse = "")), collapse = "-|-"), "-|")

  rows <- apply(table_data_df, 1, function(row_vec) {
    paste0("| ", paste(format(as.character(row_vec), width = col_widths, justify = "left"), collapse = " | "), " |")
  })

  table_string <- paste(c(header, separator, rows), collapse = "\n")
  
  total_rows <- nrow(table_data_df) # Should be based on original before potential head()
  # For NA reporting, showing all might be better unless it's excessively long.
  # The prompt did not specify truncation for NA value errors, unlike transition errors.
  # We'll not truncate for now, but this could be added if needed.

  glue("{message_prefix}\n\n{table_string}")
}

#' @export
evaluate_values <- function(df, ns, value_names, state_names, simplify = FALSE) {

  # df is uneval_values. 
  # If model had no values, parse_values returns an empty, structured tibble:

  # Handle 0-row input df (e.g., from a model with no values defined)
  if (nrow(df) == 0) {
    # Return empty tibble with expected columns
    empty_evaluated_values <- tibble(
      state = character(0),
      destination = character(0),
      max_st = numeric(0),
      state_cycle = numeric(0), # Or integer(0) based on typical content
      values_list = list(),
      .rows = 0
    )

    # The arrange step on a 0-row tibble with a 'state' column is valid.
    # factor(character(0), levels=state_names) results in factor(0 levels=...)
    return(arrange(empty_evaluated_values, factor(state, levels = state_names)))
  }

  # Existing logic for when df has rows:
  
  # The list of tibbles, one for each group of (state, destination)
  grouped_df_list <- df %>%
    group_by(state, destination) %>% # destination can be NA
    group_split()

  # Process each group and collect NA information
  processed_groups_and_na_info <- map(grouped_df_list, function(x_group) {
      state_ns <- eval_variables(x_group, clone_namespace(ns), FALSE)
      state_res <- state_ns$df # This contains cycle, state_cycle, and evaluated variable columns


      # Determine which of the model's value_names are present as columns in state_res
      # These are the names of the values defined in this x_group
      value_cols_in_stateres <- intersect(colnames(state_res), value_names)

      # Collect NA details and type validation for the current group
      current_group_na_details_list <- list()
      if (nrow(state_res) > 0 && length(value_cols_in_stateres) > 0) {
          for (v_col_name in value_cols_in_stateres) {
              # Check if the column itself exists before trying to access it
              if (v_col_name %in% colnames(state_res)) {
                  col_data <- state_res[[v_col_name]]

                  # First check for non-numeric types (critical error)
                  if (!is.numeric(col_data) && !is.integer(col_data)) {
                      # Find the formula text for better error message
                      formula_text <- NULL
                      formula_row <- which(x_group$name == v_col_name)
                      if (length(formula_row) > 0) {
                          formula_text <- as.character(x_group$formula[[formula_row[1]]]$expr)
                      }

                      # Use validation helper to generate proper error
                      validate_value_result(
                          col_data,
                          value_name = v_col_name,
                          state = x_group$state[1],
                          destination = x_group$destination[1],
                          formula_text = formula_text
                      )
                  }

                  # Then check for NA values (existing behavior)
                  if (anyNA(col_data)) {
                      na_indices <- which(is.na(col_data))
                      if (length(na_indices) > 0) {
                          current_group_na_details_list[[length(current_group_na_details_list) + 1]] <-
                              tibble(
                                  value_name = v_col_name,
                                  state = x_group$state[1],
                                  # Handle destination being NA_character_ if x_group$destination[1] is NA
                                  destination = if (is.na(x_group$destination[1])) NA_character_ else as.character(x_group$destination[1]),
                                  cycle = state_res$cycle[na_indices],
                                  state_cycle = state_res$state_cycle[na_indices]
                              )
                      }
                  }
              }
          }
      }
      na_report_for_this_group <- bind_rows(current_group_na_details_list)

      # --- Resume original logic for processing state_res ---
      state_res$state <- x_group$state[1]
      
      value_names_in_df <- intersect(colnames(state_res), value_names) # Re-calc based on state_res with 'state'
      value_names_in_env <- intersect(names(state_ns$env), value_names)
      
      current_max_st <- x_group$max_st[1]

      if (simplify && length(value_names_in_df) > 0 && nrow(state_res) > 0) {
        cols_for_pivot <- c("state", "cycle", "state_cycle", value_names_in_df)
        missing_pivot_cols <- setdiff(cols_for_pivot, colnames(state_res))
        if (length(missing_pivot_cols) > 0) {
            warning(paste("evaluate_values simplify: missing columns in state_res:", paste(missing_pivot_cols, collapse=", ")))
        } else {
            simplified_state_res <- state_res[ ,cols_for_pivot, drop = FALSE]
            if (nrow(simplified_state_res) > 0 && length(value_names_in_df) > 0) {
                val_mat <- simplified_state_res %>%
                    pivot_longer(names_to = "variable", values_to = "value", all_of(value_names_in_df)) %>%
                    lf_to_arr(c('cycle', 'state_cycle','variable'), 'value')
                current_max_st <- min(current_max_st, arr_last_unique(val_mat, 2), na.rm = TRUE)
            }
        }
      }
      
      expanded_state_res_list <- state_res %>%
        group_by(state_cycle) %>%
        group_split()

      inner_mapped_rows <- map(expanded_state_res_list, function(state_cycle_df) {
          expanded_state_values_list <- append(
            as.list(state_cycle_df[ ,value_names_in_df, drop = FALSE]),
            as.list(state_ns$env)[value_names_in_env]
          )
          
          list(
            state = x_group$state[1],
            destination = x_group$destination[1], # This can be NA
            max_st = current_max_st,
            state_cycle = state_cycle_df$state_cycle[1],
            values_list = list(expanded_state_values_list)
          )
      })
      
      list(
          data_output = bind_rows(inner_mapped_rows),
          na_report = na_report_for_this_group
      )
  })
  
  # Extract the main data results and the NA reports
  mapped_results_list <- map(processed_groups_and_na_info, "data_output")
  all_na_reports_list <- map(processed_groups_and_na_info, "na_report")
  
  # Combine all NA reports into one tibble
  all_na_df <- bind_rows(all_na_reports_list)

  # If NA values were found, process and potentially throw error
  if (nrow(all_na_df) > 0) {
    # Consolidate NA report
    consolidated_na_summary <- all_na_df %>%
      group_by(value_name, state, destination) %>%
      summarise(
        Cycles = format_ranges_for_eval_values(cycle),
        State_Cycles = format_ranges_for_eval_values(state_cycle),
        .groups = 'drop'
      ) %>%
      select(
        `Value Name` = value_name,
        State = state,
        Destination = destination, # This column might contain NA_character_
        Cycles,
        `State Cycles` = State_Cycles
      )

    # Always use checkpoint behavior for NA value errors
    table_message <- format_na_table_to_markdown_for_eval_values(
      consolidated_na_summary,
      "NA values detected in evaluated model values:"
    )
    stop(table_message, call. = FALSE)
  }

  
  # Combine results from all groups (original logic)
  final_evaluated_values <- bind_rows(mapped_results_list)
  
  arrange(final_evaluated_values, factor(state, levels = state_names))
}

values_to_vmat <- function(df, state_names) {
  value_names <- setdiff(colnames(df), c('state', 'cycle', 'state_cycle', 'max_st'))
  lf <-  df %>%
    pivot_longer(names_to = "variable", values_to = "value", all_of(value_names)) %>%
    mutate(e_state = factor(expand_state_name(state, state_cycle), levels = state_names))
  mat <- lf_to_arr(lf, c('cycle', 'e_state', 'variable'), 'value')
  dimnames(mat) <- list(
    unique(df$cycle),
    state_names,
    value_names
  )
  
  mat
    
}

