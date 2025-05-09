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
    empty_parsed_values <- tibble::tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      state = character(0),
      destination = character(0),
      formula = list(), # Formulas are parsed to heRoFormula objects, so list() for empty case
      max_st = numeric(0),
      # Add other columns that `sort_variables` might consistently produce on an empty input
      # or that as.values expects. For now, these are the main ones.
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
      mutate(combined = paste0("name: ", name, ", state: ", state, ", destination: ", destination))
    
    stop("Duplicate values found. Values must be unique by name, state, and destination combination:\n",
         paste(dup_values$combined, collapse = "\n"))
  }

  # Parse values and sort
  vars <- x %>%
    group_by(state, destination) %>%
    do({
      as.data.frame(.) %>%
        select(name, display_name, description, state, destination, formula) %>%
        mutate(formula = map(formula, as.heRoFormula)) %>%
        sort_variables(extra_vars)
    }) %>%
    ungroup() %>%
    left_join(select(states, name, max_st = max_state_time), by = c('state' = 'name')) %>%
    mutate(max_st = ifelse(is.na(max_st), 1, max_st))
  
  # Construct Object & Return
  as.values(vars)
}

check_values_df <- function(x) {
  
}

as.values <- function(x) x

#' @export
evaluate_values <- function(df, ns, value_names, state_names, simplify = F) {

  # df is uneval_values. 
  # If model had no values, parse_values returns an empty, structured tibble:
  # tibble(name, display_name, description, state, destination, formula=list(), max_st, .rows = 0)

  # Handle 0-row input df (e.g., from a model with no values defined)
  if (nrow(df) == 0) {
    # The structure should match what the map/bind_rows would produce if there was data.
    # These are the columns returned by the inner map's bind_rows:
    # state, destination, max_st, state_cycle, values_list
    empty_evaluated_values <- tibble::tibble(
      state = character(0),
      destination = character(0),
      max_st = numeric(0),
      state_cycle = numeric(0), # Or integer(0) based on typical content
      values_list = list(),
      .rows = 0
    )
    # The arrange step on a 0-row tibble with a 'state' column is valid.
    # factor(character(0), levels=state_names) results in factor(0 levels=...)
    return(dplyr::arrange(empty_evaluated_values, factor(state, levels = state_names)))
  }

  # Existing logic for when df has rows:
  names_in_order <- unique(df$name) # df$name exists due to parse_values structure
  
  # The list of tibbles, one for each group of (state, destination)
  grouped_df_list <- df %>%
    dplyr::group_by(state, destination) %>%
    dplyr::group_split()

  # Process each group
  mapped_results_list <- purrr::map(grouped_df_list, function(x_group) {
      state_ns <- eval_variables(x_group, clone_namespace(ns), FALSE)
      state_res <- state_ns$df # This contains cycle, state_cycle, and evaluated variable columns
      state_res$state <- x_group$state[1] # Add the group's state to state_res
      
      value_names_in_df <- intersect(colnames(state_res), value_names)
      value_names_in_env <- intersect(names(state_ns$env), value_names)
      # value_names_missing <- setdiff(value_names, c(value_names_in_df, value_names_in_env)) # Not directly used for output structure here

      current_max_st <- x_group$max_st[1] # Initial max_st for the group

      if (simplify && length(value_names_in_df) > 0 && nrow(state_res) > 0) {
        # Ensure columns for pivot exist in state_res
        cols_for_pivot <- c("state", "cycle", "state_cycle", value_names_in_df)
        missing_pivot_cols <- setdiff(cols_for_pivot, colnames(state_res))
        if (length(missing_pivot_cols) > 0) {
            # This shouldn't happen if state_res is structured as expected
            warning(paste("evaluate_values simplify: missing columns in state_res:", paste(missing_pivot_cols, collapse=", ")))
        } else {
            simplified_state_res <- state_res[ ,cols_for_pivot, drop = FALSE]
            if (nrow(simplified_state_res) > 0 && length(value_names_in_df) > 0) {
                val_mat <- simplified_state_res %>%
                    tidyr::pivot_longer(names_to = "variable", values_to = "value", dplyr::all_of(value_names_in_df)) %>%
                    lf_to_arr(c('cycle', 'state_cycle'), 'value')
                current_max_st <- min(current_max_st, arr_last_unique(val_mat, 2), na.rm = TRUE)
            }
        }
      }
      # state_res$state <- x_group$state[1] # Already done above

      # Inner map over state_cycle within the current group
      expanded_state_res_list <- state_res %>%
        dplyr::group_by(state_cycle) %>%
        dplyr::group_split()
      
      inner_mapped_rows <- purrr::map(expanded_state_res_list, function(state_cycle_df) {
          expanded_state_values_list <- append(
            as.list(state_cycle_df[ ,value_names_in_df, drop = FALSE]),
            as.list(state_ns$env)[value_names_in_env]
          )
          
          tibble::tibble(
            state = x_group$state[1],
            destination = x_group$destination[1],
            max_st = current_max_st, # Use the (potentially) simplified max_st for the group
            state_cycle = state_cycle_df$state_cycle[1],
            values_list = list(expanded_state_values_list)
          )
      })
      dplyr::bind_rows(inner_mapped_rows)
  })
  
  # Combine results from all groups
  final_evaluated_values <- dplyr::bind_rows(mapped_results_list)
  
  # Final arrange step
  # If final_evaluated_values is 0-row (e.g. if df had rows but all groups led to empty inner_mapped_rows),
  # it should still have the columns: state, destination, max_st, state_cycle, values_list.
  # So, factor(state, ...) should be fine.
  dplyr::arrange(final_evaluated_values, factor(state, levels = state_names))
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

