parse_values <- function(x, states, extra_vars) {
  
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
    group_by(state) %>%
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

  names_in_order <- unique(df$name)
  evaluated_values <- df %>%
    group_by(state, destination) %>%
    group_split() %>%
    map(function(x) {
      state_ns <- eval_variables(x, clone_namespace(ns), F)
      state_res <- state_ns$df
      state_res$state <- x$state[1]
      value_names_in_df <- intersect(colnames(state_res), value_names)
      value_names_in_env <-intersect(names(state_ns$env), value_names)
      value_names_missing <- setdiff(value_names, c(value_names_in_df, value_names_in_env))

      if (simplify && length(value_names_in_df) > 0) {
        # Transform to matrix to check state-time-dependency
        state_res <- state_res[ ,c("state", "cycle", "state_cycle", value_names_in_df)]
        
        val_mat <- state_res %>%
          pivot_longer(names_to = "variable", values_to = "value", all_of(value_names_in_df)) %>%
          lf_to_arr(c('cycle', 'state_cycle'), 'value')
        x$max_st <- min(x$max_st[1], arr_last_unique(val_mat, 2))
      }
      state_res$state <- x$state[1]

      expanded_state_res <- state_res %>% 
        group_by(state_cycle) %>%
        group_split() %>%
        map(function(state_cycle_df) {
          expanded_state_values_list <- append(
            as.list(state_cycle_df[ ,value_names_in_df]),
            as.list(state_ns$env)[value_names_in_env]
          )
          #expanded_state_values_list[value_names_missing] <- 0
          #expanded_state_values_list <- expanded_state_values_list[value_names]
          x$values_list <- list(expanded_state_values_list)
          x$state_cycle <- state_cycle_df$state_cycle[1]
          x[1, c('state', 'destination', 'max_st', 'state_cycle', 'values_list')]
        }) %>%
        bind_rows()
      expanded_state_res
    }) %>%
    bind_rows() %>%
    arrange(factor(state, levels = state_names))

  evaluated_values
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

