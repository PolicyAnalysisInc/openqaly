# ============================================================================
# PSM-specific Model Builder Methods
# ============================================================================

#' @export
add_state.oq_psm <- function(model, name, display_name = NULL,
                               description = NULL, ...) {
  validate_string(name, "State name")
  dots <- list(...)
  if (!is.null(dots$initial_prob)) stop("PSM models don't use initial_prob parameter. Remove it from add_state() call.")
  if (!is.null(dots$state_group)) stop("PSM models don't use state_group parameter. Remove it from add_state() call.")
  if (!is.null(dots$state_cycle_limit)) stop("PSM models don't use state_cycle_limit parameter. Remove it from add_state() call.")
  new_state <- fast_tibble(
    name = name, display_name = display_name %||% name,
    description = description %||% display_name %||% name
  )
  entity_add_tibble(model, .schema_state, new_state, .callbacks_add_state)
}

#' @export
edit_state.oq_psm <- function(model, name, display_name, description,
                                new_name, ...) {
  match_idx <- find_in_tibble(model$states, "name", list(name), "State")
  updates <- list()
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_tibble(model, .schema_state, match_idx, updates, .callbacks_edit_state)
}

#' @export
remove_state.oq_psm <- function(model, name, error_on_dependencies = FALSE, ...) {
  match_idx <- find_in_tibble(model$states, "name", list(name), "State")
  entity_remove_tibble(model, .schema_state, match_idx, .callbacks_remove_state,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

#' @export
add_transition.oq_psm <- function(model, endpoint, time_unit, formula, ...) {
  formula_str <- capture_nse(rlang::enquo(formula))
  new_trans <- fast_tibble(endpoint = endpoint, time_unit = time_unit, formula = formula_str)
  entity_add_tibble(model, .schema_transition_psm, new_trans)
}

#' @export
edit_transition.oq_psm <- function(model, endpoint, formula, time_unit, ...) {
  match_idx <- find_in_tibble(model$transitions, "endpoint", list(endpoint), "PSM transition")
  updates <- list()
  if (!missing(formula)) updates$formula <- capture_nse(rlang::enquo(formula))
  if (!missing(time_unit)) updates$time_unit <- time_unit
  entity_edit_tibble(model, .schema_transition_psm, match_idx, updates)
}

#' @export
remove_transition.oq_psm <- function(model, endpoint, ...) {
  match_idx <- find_in_tibble(model$transitions, "endpoint", list(endpoint), "PSM transition")
  entity_remove_tibble(model, .schema_transition_psm, match_idx)
}
