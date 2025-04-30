


parse_states <- function(states, cycle_length_days, days_per_year) {
  
  # Check that variables definition is valid
  check_states(states)

  # If state time limit is unspecified, assume infinite
  states$state_cycle_limit <- ifelse(
    is.na(states$state_cycle_limit),
    Inf,
    states$state_cycle_limit
  )
  
  # If state time limit unit is undefined, use cycles
  states$state_cycle_limit_unit <- ifelse(
    is.na(states$state_cycle_limit_unit),
    'cycles',
    states$state_cycle_limit_unit
  )

  # Parse initial probability formulas, calculate maximum tunnel states
  parsed_states <- states %>%
    mutate(
      formula = map(initial_probability, as.heRoFormula),
      max_state_time = ceiling(
        ceiling(days_per_unit(state_cycle_limit_unit, cycle_length_days, days_per_year) * state_cycle_limit / cycle_length_days)
      ),
      state_group = ifelse(is.na(state_group), paste0('.', name), state_group),
      share_state_time = ifelse(is.na(share_state_time), F, share_state_time)
    ) %>%
    select(
      name,
      display_name,
      description,
      formula,
      state_group,
      share_state_time,
      max_state_time
    ) %>%
    sort_variables()
  
  # Construct Object & Return
  as.states(parsed_states)
}

check_states <- function(x) {

}

eval_states <- function(x, ns) {
  # Limit variables to first cycle
  cloned_ns <- clone_namespace(ns)
  cloned_ns$df <- cloned_ns$df[1, ]
  eval_variables(x, cloned_ns, T)$df[ ,x$name]
}

expand_init_states <- function(x, expand) {

  n_states_exp <- sum(ifelse(is.na(expand$max_st), 1, expand$max_st))
  init_mat <- matrix(numeric(n_states_exp), nrow = 1)
  col_names <- character(n_states_exp)
  index <- 1
  for (i in colnames(x)) {
    row <- expand[expand$state == i, ]
    indices <- seq(from = index, to = index + row$max_st[1] - 1)
    init_mat[1, index] <- x[[i]][1]
    col_names[indices] <- expand_state_name(
      rep(row$state[1], row$max_st[1]),
      seq(row$max_st[1])
    )
    index <- max(indices) + 1
  }
  colnames(init_mat) <- col_names
      
  init_mat
}

as.states <- function(x) {
  UseMethod('as.states', x)
}
as.states.states <- function(x) {
  x
}
as.states.data.frame <- function(x) {
  class(x) <- c('states', class(x))
  x
}

expand_state_name <- function(name, index) {
  paste0(name, '.', index)
}
