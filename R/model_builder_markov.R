# ============================================================================
# Markov-specific Model Builder Methods
# ============================================================================

#' @export
add_state.oq_markov <- function(model, name, display_name = NULL,
                                 description = NULL, state_group = NULL,
                                 share_state_time = FALSE,
                                 state_cycle_limit = NULL,
                                 state_cycle_limit_unit = "cycles",
                                 initial_prob = NULL, ...) {
  validate_string(name, "State name")
  if (is.null(initial_prob)) stop("initial_prob is required for Markov models. Specify it in add_state() call.")
  new_state <- fast_tibble(
    name = name, initial_probability = as.character(initial_prob),
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    state_group = state_group, share_state_time = share_state_time,
    state_cycle_limit = state_cycle_limit %||% Inf,
    state_cycle_limit_unit = state_cycle_limit_unit
  )
  entity_add_tibble(model, .schema_state, new_state, .callbacks_add_state)
}

#' @export
edit_state.oq_markov <- function(model, name, display_name, description,
                                  state_group, share_state_time,
                                  state_cycle_limit, state_cycle_limit_unit,
                                  initial_prob, new_name, ...) {
  match_idx <- find_in_tibble(model$states, "name", list(name), "State")
  updates <- list()
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(state_group)) updates$state_group <- state_group
  if (!missing(share_state_time)) updates$share_state_time <- share_state_time
  if (!missing(state_cycle_limit)) updates$state_cycle_limit <- state_cycle_limit
  if (!missing(state_cycle_limit_unit)) updates$state_cycle_limit_unit <- state_cycle_limit_unit
  if (!missing(initial_prob)) updates$initial_probability <- as.character(initial_prob)
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_tibble(model, .schema_state, match_idx, updates, .callbacks_edit_state)
}

#' @export
remove_state.oq_markov <- function(model, name, error_on_dependencies = FALSE, ...) {
  match_idx <- find_in_tibble(model$states, "name", list(name), "State")
  entity_remove_tibble(model, .schema_state, match_idx, .callbacks_remove_state,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

#' @export
add_transition.oq_markov <- function(model, from_state, to_state, formula, ...) {
  formula_str <- capture_nse(rlang::enquo(formula))
  new_trans <- fast_tibble(from_state = from_state, to_state = to_state, formula = formula_str)
  entity_add_tibble(model, .schema_transition_markov, new_trans, .callbacks_add_transition_markov)
}

#' @export
edit_transition.oq_markov <- function(model, from_state, to_state, formula, ...) {
  match_idx <- find_in_tibble(model$transitions, c("from_state", "to_state"), list(from_state, to_state), "Markov transition")
  updates <- list(formula = capture_nse(rlang::enquo(formula)))
  entity_edit_tibble(model, .schema_transition_markov, match_idx, updates)
}

#' @export
remove_transition.oq_markov <- function(model, from_state, to_state, ...) {
  match_idx <- find_in_tibble(model$transitions, c("from_state", "to_state"), list(from_state, to_state), "Markov transition")
  entity_remove_tibble(model, .schema_transition_markov, match_idx)
}
