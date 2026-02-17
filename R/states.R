


parse_states <- function(states, cycle_length_days, days_per_year, model_type = "markov") {

  # Check that variables definition is valid
  check_states(states)

  if (tolower(model_type) %in% c("psm", "psm_custom")) {
    # PSM: Already has correct minimal structure from spec
    # No initial_probability, no tunnel states
    # Just pass through as states object (no sorting needed - no formulas)
    parsed_states <- states %>%
      select("name", "display_name", "description")

    return(as.states(parsed_states))
  }

  # Markov: Full processing with tunnel states
  # If state time limit is unspecified, assume infinite
  if ("state_cycle_limit" %in% names(states)) {
    states$state_cycle_limit <- ifelse(
      is.na(states$state_cycle_limit),
      Inf,
      states$state_cycle_limit
    )
  } else {
    states$state_cycle_limit <- Inf
  }

  # If state time limit unit is undefined, use cycles
  if ("state_cycle_limit_unit" %in% names(states)) {
    states$state_cycle_limit_unit <- ifelse(
      is.na(states$state_cycle_limit_unit),
      'cycles',
      states$state_cycle_limit_unit
    )
  } else {
    states$state_cycle_limit_unit <- 'cycles'
  }

  # Ensure optional columns exist
  if (!"state_group" %in% names(states)) {
    states$state_group <- NA_character_
  }
  if (!"share_state_time" %in% names(states)) {
    states$share_state_time <- FALSE
  }

  # Parse initial probability formulas, calculate maximum tunnel states
  parsed_states <- states %>%
    mutate(
      formula = map(.data$initial_probability, as.oq_formula),
      max_state_time = ceiling(
        ceiling(days_per_unit(.data$state_cycle_limit_unit, cycle_length_days, days_per_year) * .data$state_cycle_limit / cycle_length_days)
      ),
      state_group = ifelse(is.na(.data$state_group), glue(".{name}"), .data$state_group),
      share_state_time = ifelse(is.na(.data$share_state_time), F, .data$share_state_time)
    )

  # For shared state time groups, validate and propagate the group's cycle limit
  shared_groups <- parsed_states %>%
    filter(.data$share_state_time == TRUE)

  if (nrow(shared_groups) > 0) {
    # Find groups where members have explicit (finite) limits
    groups_with_limits <- shared_groups %>%
      filter(is.finite(.data$max_state_time)) %>%
      group_by(.data$state_group) %>%
      summarize(
        n_distinct_limits = n_distinct(.data$max_state_time),
        group_max_st = first(.data$max_state_time),
        .groups = "drop"
      )

    # Error if any group has conflicting limits
    conflicts <- groups_with_limits %>% filter(.data$n_distinct_limits > 1)
    if (nrow(conflicts) > 0) {
      conflict_group <- conflicts$state_group[1]
      conflict_states <- shared_groups %>%
        filter(.data$state_group == conflict_group, is.finite(.data$max_state_time))
      stop(
        glue("States in group '{conflict_group}' have conflicting state_cycle_limit values: ",
             "{paste(conflict_states$name, '=', conflict_states$max_state_time, collapse = ', ')}. ",
             "States that share state time must use the same state_cycle_limit."),
        call. = FALSE
      )
    }

    # Propagate the group limit to all members
    group_limits <- groups_with_limits %>% select("state_group", "group_max_st")
    if (nrow(group_limits) > 0) {
      parsed_states <- parsed_states %>%
        left_join(group_limits, by = "state_group") %>%
        mutate(
          max_state_time = ifelse(
            .data$share_state_time & !is.na(.data$group_max_st),
            .data$group_max_st,
            .data$max_state_time
          )
        ) %>%
        select(-"group_max_st")
    }
  }

  parsed_states <- parsed_states %>%
    select(
      "name",
      "display_name",
      "description",
      "formula",
      "state_group",
      "share_state_time",
      "max_state_time"
    ) %>%
    sort_variables()

  # Construct Object & Return
  as.states(parsed_states)
}

check_states <- function(x) {
  reserved <- intersect(x$name, c("All", "All Other"))
  if (length(reserved) > 0) {
    stop(glue("Reserved state name(s) found: {paste(reserved, collapse = ', ')}. ",
              "'All' and 'All Other' cannot be used as state names."), call. = FALSE)
  }
}

eval_states <- function(x, ns) {
  # Limit variables to first cycle
  cloned_ns <- clone_namespace(ns)
  cloned_ns$df <- cloned_ns$df[1, ]
  result <- eval_variables(x, cloned_ns, T)$df[ ,x$name, drop = FALSE]

  # Ensure the result has column names
  if (is.null(colnames(result))) {
    # If only one state and result is a matrix/dataframe without names
    if (ncol(result) == length(x$name)) {
      colnames(result) <- x$name
    } else {
      stop("eval_states: Cannot determine column names for initial state probabilities")
    }
  }

  # Validate that initial probabilities sum to 1
  prob_sum <- sum(result[1, ])
  tol <- sqrt(.Machine$double.eps)  # Standard tolerance for floating point comparison
  if (abs(prob_sum - 1.0) > tol) {
    error_msg <- glue("Initial state probabilities must sum to 1 (got {prob_sum})")
    accumulate_oq_error(define_error(error_msg), context_msg = "Initial state validation")
    oq_error_checkpoint()
  }

  return(result)
}

expand_init_states <- function(x, expand) {
  # Ensure x has column names
  if (is.null(colnames(x)) || length(colnames(x)) == 0) {
    # If single state in expand, use it
    if (nrow(expand) == 1) {
      colnames(x) <- expand$state[1]
    } else {
      stop("expand_init_states: Initial state matrix has no column names and cannot infer them")
    }
  }

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
#' @export
as.states.data.frame <- function(x) {
  class(x) <- c('states', class(x))
  x
}

expand_state_name <- function(name, index) {
  glue("{name}.{index}")
}
