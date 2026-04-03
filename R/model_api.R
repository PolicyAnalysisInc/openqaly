# ============================================================================
# Model API - Public getters and setters for model objects
# ============================================================================

# Internal validation helper
.validate_model_arg <- function(model) {
  if (!inherits(model, c("oq_model", "oq_model_builder"))) {
    stop("model must be an oq_model or oq_model_builder object", call. = FALSE)
  }
}

# ============================================================================
# Getter Functions
# ============================================================================

#' Get Override Categories
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A list of override categories.
#' @export
get_override_categories <- function(model) {
  .validate_model_arg(model)
  model$override_categories %||% list()
}

#' Get Variables
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of variables.
#' @export
get_variables <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$variables) || nrow(model$variables) == 0) {
    return(tibble::tibble(
      name = character(0), formula = character(0),
      display_name = character(0), description = character(0),
      strategy = character(0), group = character(0),
      source = character(0), sampling = character(0)
    ))
  }
  model$variables
}

#' Get Variable Names
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A character vector of unique variable names.
#' @export
get_variable_names <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$variables) || !is.data.frame(model$variables) ||
      nrow(model$variables) == 0 || !"name" %in% names(model$variables)) {
    return(character(0))
  }
  unique(model$variables$name[!is.na(model$variables$name)])
}

#' Get Global Variables
#'
#' Returns variables where strategy and group are both empty or NA.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of global variables.
#' @export
get_global_variables <- function(model) {
  .validate_model_arg(model)
  vars <- model$variables
  if (is.null(vars) || nrow(vars) == 0) {
    return(tibble::tibble(
      name = character(0), formula = character(0),
      display_name = character(0), description = character(0),
      strategy = character(0), group = character(0),
      source = character(0), sampling = character(0)
    ))
  }
  is_global_strategy <- is.na(vars$strategy) | vars$strategy == ""
  is_global_group <- is.na(vars$group) | vars$group == ""
  vars[is_global_strategy & is_global_group, , drop = FALSE]
}

#' Get Variable Targeting
#'
#' Analyzes a model's variables to determine which variables have
#' strategy-specific or group-specific definitions.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A named list where each element is a list with \code{strategies}
#'   and \code{groups} character vectors (or NULL if the variable is global).
#' @export
get_variable_targeting <- function(model) {
  .validate_model_arg(model)
  vars <- model$variables
  if (is.null(vars) || nrow(vars) == 0) return(list())
  result <- list()
  for (vname in unique(vars$name)) {
    rows <- vars[vars$name == vname, , drop = FALSE]
    has_strategy <- any(!is.na(rows$strategy) & rows$strategy != "")
    has_group <- any(!is.na(rows$group) & rows$group != "")
    strats <- if (has_strategy) {
      unique(rows$strategy[!is.na(rows$strategy) & rows$strategy != ""])
    } else {
      NULL
    }
    grps <- if (has_group) {
      unique(rows$group[!is.na(rows$group) & rows$group != ""])
    } else {
      NULL
    }
    result[[vname]] <- list(strategies = strats, groups = grps)
  }
  result
}

#' Get Strategies
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of strategies.
#' @export
get_strategies <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$strategies) || nrow(model$strategies) == 0) {
    return(tibble::tibble(
      name = character(0), display_name = character(0),
      description = character(0), enabled = logical(0)
    ))
  }
  model$strategies
}

#' Get Groups
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of groups.
#' @export
get_groups <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$groups) || nrow(model$groups) == 0) {
    return(tibble::tibble(
      name = character(0), display_name = character(0),
      description = character(0), weight = character(0),
      enabled = logical(0)
    ))
  }
  model$groups
}

#' Get Settings
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A list of model settings.
#' @export
get_settings <- function(model) {
  .validate_model_arg(model)
  model$settings %||% list()
}

#' Get Model Type
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A string indicating the model type (e.g., "markov", "psm").
#' @export
get_model_type <- function(model) {
  .validate_model_arg(model)
  if (!is.null(model$settings$model_type)) {
    return(model$settings$model_type)
  }
  "markov"
}

#' Get DSA Parameters
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A dsa_parameters object (list with class "dsa_parameters").
#' @export
get_dsa_parameters <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$dsa_parameters) || length(model$dsa_parameters) == 0) {
    return(structure(list(), class = "dsa_parameters"))
  }
  model$dsa_parameters
}

#' Get Tables
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A named list of data frames.
#' @export
get_tables <- function(model) {
  .validate_model_arg(model)
  model$tables %||% list()
}

#' Get Table Names
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A character vector of table names.
#' @export
get_table_names <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$tables) || !is.list(model$tables) ||
      length(model$tables) == 0) {
    return(character(0))
  }
  names(model$tables)
}

#' Get Model Values
#'
#' Returns the value definitions from the model object (not results).
#' Use \code{get_values()} from model results to get computed values.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of values.
#' @export
get_model_values <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$values) || nrow(model$values) == 0) {
    return(tibble::tibble(
      name = character(0), formula = character(0),
      state = character(0), destination = character(0),
      display_name = character(0), description = character(0),
      type = character(0)
    ))
  }
  model$values
}

#' Get Model Value Names
#'
#' Returns the unique value definition names from the model object.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A character vector of unique value names.
#' @export
get_model_value_names <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$values) || !is.data.frame(model$values) ||
      nrow(model$values) == 0 || !"name" %in% names(model$values)) {
    return(character(0))
  }
  unique(model$values$name[!is.na(model$values$name)])
}

#' Get Trees
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A data frame of trees, or NULL if none.
#' @export
get_trees <- function(model) {
  .validate_model_arg(model)
  model$trees
}

#' Get Tree Names
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A character vector of unique tree names.
#' @export
get_tree_names <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$trees) || !is.data.frame(model$trees) ||
      nrow(model$trees) == 0 || !"name" %in% names(model$trees)) {
    return(character(0))
  }
  unique(model$trees$name[!is.na(model$trees$name)])
}

#' Get VBP Configuration
#'
#' Returns the value-based pricing (VBP) configuration from the model definition.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A list with fields \code{price_variable}, \code{intervention_strategy},
#'   \code{outcome_summary}, and \code{cost_summary}, or NULL if VBP is not configured.
#' @export
get_vbp <- function(model) {
  .validate_model_arg(model)
  model$vbp
}

#' Get Scenarios
#'
#' Returns the list of scenario definitions from the model.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A list of scenario objects, each with \code{name}, \code{description},
#'   \code{variable_overrides}, and \code{setting_overrides}.
#' @export
get_scenarios <- function(model) {
  .validate_model_arg(model)
  model$scenarios %||% list()
}

#' Get Scenario Names
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A character vector of scenario names.
#' @export
get_scenario_names <- function(model) {
  .validate_model_arg(model)
  scenarios <- model$scenarios %||% list()
  if (length(scenarios) == 0) return(character(0))
  vapply(scenarios, function(s) s$name, character(1))
}

#' Get Model Summaries
#'
#' Returns the summary definitions from the model object (not results).
#' Use \code{get_summaries()} from model results to get computed summaries.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of summary definitions.
#' @export
get_model_summaries <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$summaries) || nrow(model$summaries) == 0) {
    return(tibble::tibble(
      name = character(0), values = character(0),
      display_name = character(0), description = character(0),
      type = character(0), wtp = numeric(0)
    ))
  }
  model$summaries
}

#' Get States
#'
#' Returns the state definitions from the model. The tibble structure varies
#' by model type (Markov models have additional columns like
#' \code{initial_probability}).
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of state definitions.
#' @export
get_states <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$states) || nrow(model$states) == 0) {
    model_type <- model$settings$model_type %||% "markov"
    if (model_type %in% c("psm", "custom_psm", "decision_tree")) {
      return(tibble::tibble(
        name = character(0), display_name = character(0),
        description = character(0)
      ))
    }
    return(tibble::tibble(
      name = character(0), initial_probability = character(0),
      display_name = character(0), description = character(0),
      state_group = character(0), share_state_time = logical(0),
      state_cycle_limit = numeric(0), state_cycle_limit_unit = character(0)
    ))
  }
  model$states
}

#' Get Model Transitions
#'
#' Returns the transition definitions from the model object (not results).
#' Use \code{get_transitions()} from model results to get computed transitions.
#' The tibble structure varies by model type.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A tibble of transition definitions.
#' @export
get_model_transitions <- function(model) {
  .validate_model_arg(model)
  if (is.null(model$transitions) || nrow(model$transitions) == 0) {
    model_type <- model$settings$model_type %||% "markov"
    if (model_type == "psm") {
      return(tibble::tibble(
        endpoint = character(0), time_unit = character(0),
        formula = character(0)
      ))
    }
    if (model_type == "custom_psm") {
      return(tibble::tibble(
        state = character(0), formula = character(0)
      ))
    }
    return(tibble::tibble(
      from_state = character(0), to_state = character(0),
      formula = character(0)
    ))
  }
  model$transitions
}

#' Get Decision Tree Configuration
#'
#' Returns the decision tree configuration from the model.
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A list with \code{tree_name}, \code{duration}, and \code{duration_unit},
#'   or NULL if no decision tree is configured.
#' @export
get_decision_tree <- function(model) {
  .validate_model_arg(model)
  model$decision_tree
}

#' Get Scripts
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A named list of scripts, each with \code{code} and \code{description}.
#' @export
get_scripts <- function(model) {
  .validate_model_arg(model)
  model$scripts %||% list()
}

#' Get Script Names
#'
#' @param model An oq_model or oq_model_builder object.
#' @return A character vector of script names.
#' @export
get_script_names <- function(model) {
  .validate_model_arg(model)
  scripts <- model$scripts %||% list()
  if (length(scripts) == 0) return(character(0))
  names(scripts)
}

# ============================================================================
# Setter Functions
# ============================================================================

#' Set Override Categories
#'
#' Replaces override categories wholesale on a model object.
#'
#' @param model An oq_model or oq_model_builder object.
#' @param categories A list of override categories.
#' @return The modified model object.
#' @export
set_override_categories <- function(model, categories) {
  .validate_model_arg(model)

  if (!is.list(categories)) {
    stop("categories must be a list", call. = FALSE)
  }

  # Validate each category
  for (i in seq_along(categories)) {
    cat <- categories[[i]]
    if (!is.list(cat)) {
      stop(sprintf("Category %d must be a list", i), call. = FALSE)
    }
    if (is.null(cat$name) || !is.character(cat$name) || length(cat$name) != 1 ||
        nchar(trimws(cat$name)) == 0) {
      stop(sprintf("Category %d must have a non-empty 'name' string", i),
           call. = FALSE)
    }
    if (!is.null(cat$general) && !is.logical(cat$general)) {
      stop(sprintf("Category %d 'general' must be logical", i), call. = FALSE)
    }
    if (!is.null(cat$overrides) && !is.list(cat$overrides)) {
      stop(sprintf("Category %d 'overrides' must be a list", i), call. = FALSE)
    }

    # Validate each override in the category
    for (j in seq_along(cat$overrides)) {
      ovr <- cat$overrides[[j]]
      if (!is.list(ovr)) {
        stop(sprintf("Category %d, override %d must be a list", i, j),
             call. = FALSE)
      }
      if (is.null(ovr$name) || !is.character(ovr$name) || length(ovr$name) != 1 ||
          nchar(trimws(ovr$name)) == 0) {
        stop(sprintf("Category %d, override %d must have a non-empty 'name' string",
                     i, j), call. = FALSE)
      }
      if (!is.null(ovr$type) && !ovr$type %in% c("variable", "setting")) {
        stop(sprintf("Category %d, override %d 'type' must be 'variable' or 'setting'",
                     i, j), call. = FALSE)
      }
      valid_input_types <- c("numeric", "slider", "dropdown", "formula", "timeframe")
      if (!is.null(ovr$input_type) && !ovr$input_type %in% valid_input_types) {
        stop(sprintf("Category %d, override %d 'input_type' must be one of: %s",
                     i, j, paste(valid_input_types, collapse = ", ")),
             call. = FALSE)
      }
      # Setting overrides must have empty strategy/group
      if (!is.null(ovr$type) && ovr$type == "setting") {
        if ((!is.null(ovr$strategy) && ovr$strategy != "") ||
            (!is.null(ovr$group) && ovr$group != "")) {
          stop(sprintf(
            "Category %d, override %d: setting overrides cannot have strategy/group",
            i, j), call. = FALSE)
        }
      }
    }
  }

  model$override_categories <- categories
  model
}

#' Set Override Expression
#'
#' Sets the overridden expression for a specific override identified by
#' (name, strategy, group).
#'
#' @param model An oq_model or oq_model_builder object.
#' @param name The name of the override.
#' @param expression The new expression value.
#' @param strategy The strategy targeting (default "").
#' @param group The group targeting (default "").
#' @return The modified model object.
#' @export
set_override_expression <- function(model, name, expression,
                                    strategy = "", group = "") {
  .validate_model_arg(model)

  categories <- model$override_categories %||% list()

  for (i in seq_along(categories)) {
    for (j in seq_along(categories[[i]]$overrides)) {
      ovr <- categories[[i]]$overrides[[j]]
      ovr_strategy <- ovr$strategy %||% ""
      ovr_group <- ovr$group %||% ""
      if (ovr$name == name && ovr_strategy == strategy && ovr_group == group) {
        model$override_categories[[i]]$overrides[[j]]$overridden_expression <-
          as.character(expression)
        return(model)
      }
    }
  }

  stop(sprintf(
    "No override found matching name='%s', strategy='%s', group='%s'",
    name, strategy, group
  ), call. = FALSE)
}

#' Set Override Expressions (Batch)
#'
#' Sets overridden expressions for multiple overrides at once. Each entry
#' targets a specific override by (name, strategy, group).
#'
#' @param model An oq_model or oq_model_builder object.
#' @param overrides A list of lists, each with fields: \code{name} (string),
#'   \code{expression} (string), \code{strategy} (string, default ""),
#'   \code{group} (string, default "").
#' @return The modified model object.
#' @export
set_override_expressions <- function(model, overrides) {
  .validate_model_arg(model)

  if (!is.list(overrides)) {
    stop("overrides must be a list", call. = FALSE)
  }

  for (entry in overrides) {
    model <- set_override_expression(
      model,
      name = entry$name,
      expression = entry$expression,
      strategy = entry$strategy %||% "",
      group = entry$group %||% ""
    )
  }

  model
}
