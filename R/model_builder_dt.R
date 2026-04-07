# ============================================================================
# Decision Tree-specific Model Builder Methods
# ============================================================================

#' @export
add_state.oq_decision_tree <- function(model, name, ...) {
  stop("Decision tree models do not support states.", call. = FALSE)
}

#' @export
edit_state.oq_decision_tree <- function(model, name, ...) {
  stop("Decision tree models do not support states.", call. = FALSE)
}

#' @export
remove_state.oq_decision_tree <- function(model, name, ...) {
  stop("Decision tree models do not support states.", call. = FALSE)
}

#' @export
add_transition.oq_decision_tree <- function(model, ...) {
  stop("Decision tree models do not support transitions.", call. = FALSE)
}

#' @export
edit_transition.oq_decision_tree <- function(model, ...) {
  stop("Decision tree models do not support transitions.", call. = FALSE)
}

#' @export
remove_transition.oq_decision_tree <- function(model, ...) {
  stop("Decision tree models do not support transitions.", call. = FALSE)
}
