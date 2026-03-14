#' Validate Decision Tree Configuration
#'
#' Validates that the model's decision tree configuration is correct.
#'
#' @param model The model object to validate
#' @return The model (invisibly), or throws an error
#' @keywords internal
validate_decision_tree <- function(model) {
  model_type <- tolower(model$settings$model_type %||% "markov")

  # Standalone DT model must have decision_tree config

  if (model_type == "decision_tree") {
    if (is.null(model$decision_tree)) {
      stop("Standalone decision tree models must have a decision_tree configuration. ",
           "Use set_decision_tree() to configure it.", call. = FALSE)
    }
    # Standalone DT must not have states
    if (!is.null(model$states) && is.data.frame(model$states) && nrow(model$states) > 0) {
      stop("Standalone decision tree models must not have states.", call. = FALSE)
    }
  }

  if (!is.null(model$decision_tree)) {
    dt <- model$decision_tree

    # tree_name must reference an existing tree
    if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
      available_trees <- unique(model$trees$name)
      if (!dt$tree_name %in% available_trees) {
        stop("Decision tree references tree '", dt$tree_name,
             "' which does not exist. Available trees: ",
             paste(available_trees, collapse = ", "), call. = FALSE)
      }
    } else {
      stop("Decision tree references tree '", dt$tree_name,
           "' but no trees are defined in the model.", call. = FALSE)
    }

    # Validate duration_unit
    valid_units <- c("days", "weeks", "months", "years")
    if (!dt$duration_unit %in% valid_units) {
      stop("Invalid decision tree duration_unit: '", dt$duration_unit,
           "'. Must be one of: ", paste(valid_units, collapse = ", "), call. = FALSE)
    }

    # Values with state = "decision_tree" must have destination = NA
    if (!is.null(model$values) && is.data.frame(model$values) && nrow(model$values) > 0) {
      dt_values <- model$values[!is.na(model$values$state) & model$values$state == "decision_tree", ]
      if (nrow(dt_values) > 0) {
        bad_dest <- dt_values[!is.na(dt_values$destination) & dt_values$destination != "NA", ]
        if (nrow(bad_dest) > 0) {
          stop("Decision tree values cannot have a destination. Invalid values: ",
               paste(bad_dest$name, collapse = ", "), call. = FALSE)
        }
      }
    }
  }

  invisible(model)
}

#' Evaluate Decision Tree Duration in Years
#'
#' Evaluates the decision tree duration formula and converts to years.
#'
#' @param model The model object with decision_tree config
#' @param namespace The evaluated namespace
#' @return Duration in years (numeric scalar), or 0 if no decision tree
#' @keywords internal
evaluate_dt_duration_years <- function(model, namespace = NULL) {
  if (is.null(model$decision_tree)) {
    return(0)
  }

  dt <- model$decision_tree
  days_per_year <- if (!is.null(model$settings$days_per_year)) model$settings$days_per_year else 365

  # Evaluate duration formula
  duration_formula <- as.oq_formula(dt$duration)
  if (!is.null(namespace)) {
    duration_val <- eval_formula(duration_formula, namespace)
  } else {
    # If no namespace, try to parse as numeric
    duration_val <- suppressWarnings(as.numeric(dt$duration))
  }

  if (is_oq_error(duration_val)) {
    accumulate_oq_error(duration_val, context_msg = "Decision tree duration evaluation")
    return(0)
  }

  duration_val <- as.numeric(duration_val[1])

  # Convert to years based on unit
  duration_years <- switch(dt$duration_unit,
    days = duration_val / days_per_year,
    weeks = duration_val * 7 / days_per_year,
    months = duration_val / 12,
    years = duration_val,
    0
  )

  duration_years
}

#' Get Decision Tree Duration in Days
#'
#' Evaluates the decision tree duration as a numeric value in days,
#' without requiring a namespace. Returns 0 if no decision tree is
#' configured or if the duration is a non-numeric formula (with a warning).
#'
#' @param model The model object with optional decision_tree config
#' @return Duration in days (numeric scalar), or 0
#' @keywords internal
get_dt_duration_days <- function(model) {
  if (is.null(model$decision_tree)) {
    return(0)
  }

  dt <- model$decision_tree
  days_per_year <- if (!is.null(model$settings$days_per_year)) model$settings$days_per_year else 365

  # Try to parse duration as numeric (no namespace available at this point)
  duration_val <- suppressWarnings(as.numeric(dt$duration))
  if (is.na(duration_val)) {
    warning("Decision tree duration '", dt$duration,
            "' is not a simple numeric value. ",
            "Cycle count will not be adjusted for decision tree duration.",
            call. = FALSE)
    return(0)
  }

  # Convert to days based on unit
  duration_days <- switch(dt$duration_unit,
    days = duration_val,
    weeks = duration_val * 7,
    months = duration_val * days_per_year / 12,
    years = duration_val * days_per_year,
    0
  )

  duration_days
}
