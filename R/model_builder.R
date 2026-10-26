# ============================================================================
# Schema-Based Model Builder (v2)
# ============================================================================
# Parallel implementation of model_builder.R using the entity engine.
# All functions have "2" suffix to coexist with the original.
# ============================================================================

# --- Model Initialization (copied from original — not CRUD) -----------------

#' Define a New Model
#'
#' Initialize a new openqaly model object with the specified type.
#'
#' @param type Character string specifying the model type ("markov", "psm", or "custom_psm")
#'
#' @return An oq_model object that can be piped to other builder functions
#'
#' @examples
#' model <- define_model("markov")
#' model <- define_model("psm")
#' model <- define_model("custom_psm")
#' @export
define_model <- function(type = "markov") {
  type <- match.arg(tolower(type), c("markov", "psm", "custom_psm", "decision_tree"))

  states_init <- if (type %in% c("psm", "custom_psm", "decision_tree")) {
    fast_tibble(
      name = character(0),
      display_name = character(0),
      description = character(0)
    )
  } else {
    fast_tibble(
      name = character(0),
      initial_probability = character(0),
      display_name = character(0),
      description = character(0),
      state_group = character(0),
      share_state_time = logical(0),
      state_cycle_limit = numeric(0),
      state_cycle_limit_unit = character(0)
    )
  }

  transitions_init <- if (type == "decision_tree") {
    fast_tibble(from_state = character(0), to_state = character(0), formula = character(0))
  } else if (type == "psm") {
    fast_tibble(endpoint = character(0), time_unit = character(0), formula = character(0))
  } else if (type == "custom_psm") {
    fast_tibble(state = character(0), formula = character(0))
  } else {
    fast_tibble(from_state = character(0), to_state = character(0), formula = character(0))
  }

  model <- list(
    settings = list(
      model_type = type, days_per_year = 365, half_cycle_method = "start",
      discount_cost = 0, discount_outcomes = 0,
      discount_method = "by_cycle",
      reduce_state_cycle = FALSE
    ),
    states = states_init,
    transitions = transitions_init,
    values = fast_tibble(
      name = character(0), formula = character(0),
      state = character(0), destination = character(0),
      display_name = character(0), description = character(0),
      type = character(0)
    ),
    variables = fast_tibble(
      name = character(0), formula = character(0),
      display_name = character(0), description = character(0),
      strategy = character(0), group = character(0),
      source = character(0), sampling = character(0)
    ),
    strategies = fast_tibble(
      name = character(0), display_name = character(0),
      description = character(0), enabled = numeric(0)
    ),
    groups = fast_tibble(
      name = character(0), display_name = character(0),
      description = character(0), weight = character(0), enabled = numeric(0)
    ),
    summaries = fast_tibble(
      name = character(0), values = character(0),
      display_name = character(0), description = character(0),
      type = character(0), wtp = numeric(0)
    ),
    tables = list(), scripts = list(), trees = NULL, decision_tree = NULL,
    multivariate_sampling = list(),
    dsa_parameters = structure(list(), class = "dsa_parameters"),
    scenarios = list(), twsa_analyses = list(),
    override_categories = list(), threshold_analyses = list(),
    vbp = NULL, psa = NULL, documentation = NULL
  )
  class(model) <- switch(type,
    markov = c("oq_markov", "oq_model"),
    psm = c("oq_psm", "oq_model"),
    custom_psm = c("oq_custom_psm", "oq_model"),
    decision_tree = c("oq_decision_tree", "oq_model")
  )
  model
}

# --- Strategy ----------------------------------------------------------------

#' Add Strategies to Model
#'
#' Add one or more strategies to the model.
#'
#' @param model An oq_model object
#' @param name Character string for the strategy name
#' @param display_name Optional display name
#' @param description Optional description
#' @param enabled Numeric indicating if strategy is enabled (default: 1)
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_strategy("treatment_a", "Treatment A")
#' @export
add_strategy <- function(model, name, display_name = NULL,
                          description = NULL, enabled = 1) {
  validate_string(name, "Strategy name")
  new_row <- fast_tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    enabled = as.numeric(enabled)
  )
  entity_add_tibble(model, .schema_strategy, new_row)
}

#' Edit a Strategy in the Model
#'
#' Modify an existing strategy's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the strategy are updated (variables, DSA parameters,
#' scenarios, TWSA analyses, override categories, threshold analyses, and
#' multivariate sampling specifications).
#'
#' @param model An oq_model object
#' @param name Character string identifying the strategy to edit
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param enabled Optional new enabled flag (will be coerced to numeric)
#' @param new_name Optional new name for the strategy. If provided, the strategy
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#' @export
edit_strategy <- function(model, name, display_name, description,
                           enabled, new_name) {
  match_idx <- find_in_tibble(model$strategies, "name", list(name), "Strategy")
  updates <- list()
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(enabled)) updates$enabled <- as.numeric(enabled)
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_tibble(model, .schema_strategy, match_idx, updates)
}

#' Remove a Strategy from the Model
#'
#' Remove an existing strategy and optionally cascade-remove all downstream
#' references (variables, DSA parameters, scenario overrides, TWSA analyses,
#' override categories, threshold analyses, and multivariate sampling specs).
#'
#' @param model An oq_model object
#' @param name Character string identifying the strategy to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the strategy has
#'   downstream dependencies, an error of class \code{"strategy_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the strategy and all
#'   downstream references.
#'
#' @return The modified model object
#' @export
remove_strategy <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- find_in_tibble(model$strategies, "name", list(name), "Strategy")
  entity_remove_tibble(model, .schema_strategy, match_idx,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

# --- Group -------------------------------------------------------------------

#' Add Groups to Model
#'
#' Add one or more groups to the model.
#'
#' @param model An oq_model object
#' @param name Character string for the group name
#' @param display_name Optional display name
#' @param description Optional description
#' @param weight Weight expression (default: "1")
#' @param enabled Whether group is enabled (default: 1)
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_group("moderate", weight = "w_moderate")
#' @export
add_group <- function(model, name, display_name = NULL,
                       description = NULL, weight = "1", enabled = 1) {
  validate_string(name, "Group name")
  new_row <- fast_tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    weight = as.character(weight),
    enabled = as.numeric(enabled)
  )
  entity_add_tibble(model, .schema_group, new_row)
}

#' Edit a Group in the Model
#'
#' Modify an existing group's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the group are updated (variables, DSA parameters,
#' scenarios, TWSA analyses, override categories, threshold analyses, and
#' multivariate sampling specifications).
#'
#' @param model An oq_model object
#' @param name Character string identifying the group to edit
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param weight Optional new weight expression (will be coerced to character)
#' @param enabled Optional new enabled flag (will be coerced to numeric)
#' @param new_name Optional new name for the group. If provided, the group
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#' @export
edit_group <- function(model, name, display_name, description,
                        weight, enabled, new_name) {
  match_idx <- find_in_tibble(model$groups, "name", list(name), "Group")
  updates <- list()
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(weight)) updates$weight <- as.character(weight)
  if (!missing(enabled)) updates$enabled <- as.numeric(enabled)
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_tibble(model, .schema_group, match_idx, updates)
}

#' Remove a Group from the Model
#'
#' Remove an existing group and optionally cascade-remove all downstream
#' references (variables, DSA parameters, scenario overrides, TWSA analyses,
#' override categories, threshold analyses, and multivariate sampling specs).
#'
#' @param model An oq_model object
#' @param name Character string identifying the group to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the group has
#'   downstream dependencies, an error of class \code{"group_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the group and all
#'   downstream references.
#'
#' @return The modified model object
#' @export
remove_group <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- find_in_tibble(model$groups, "name", list(name), "Group")
  entity_remove_tibble(model, .schema_group, match_idx,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

# --- Summary -----------------------------------------------------------------

summary_values_are_blank <- function(values) {
  !is.character(values) || length(values) != 1 || is.na(values) || trimws(values) == ""
}

summary_value_tokens <- function(values) {
  tokens <- trimws(strsplit(values, ",")[[1]])
  tokens[nzchar(tokens)]
}

validate_summary_values_for_builder <- function(model, item) {
  available_values <- if (!is.null(model$values) && is.data.frame(model$values) && nrow(model$values) > 0) {
    unique(model$values$name)
  } else {
    character(0)
  }

  if (summary_values_are_blank(item$values)) {
    if (length(available_values) == 0) {
      stop(sprintf(
        "Cannot add summary '%s': no values are defined in this model. Add values first using add_value().",
        item$name
      ), call. = FALSE)
    }
    stop(sprintf(
      "Summary 'values' must be a comma-separated list of value names. Available values: %s",
      paste0("'", available_values, "'", collapse = ", ")
    ), call. = FALSE)
  }

  ref_vals <- summary_value_tokens(item$values)
  if (length(available_values) == 0) {
    stop(sprintf(
      "Cannot add summary '%s': no values are defined in this model. Add values first using add_value().",
      item$name
    ), call. = FALSE)
  }

  missing_vals <- setdiff(ref_vals, available_values)
  if (length(missing_vals) > 0) {
    stop(sprintf(
      "Summary '%s' references value(s) not defined in the model: %s. Available values: %s",
      item$name,
      paste0("'", missing_vals, "'", collapse = ", "),
      paste0("'", available_values, "'", collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Add Summaries to Model
#'
#' Add one or more summaries to the model.
#'
#' @param model An oq_model object
#' @param name Character string for the summary name
#' @param values Comma-separated string of value names to include
#' @param display_name Optional display name
#' @param description Optional description
#' @param type Summary type: "outcome" (default) for health outcomes or "cost" for costs.
#'   WTP can only be specified for outcome summaries.
#' @param wtp Optional willingness-to-pay value (numeric). Only valid for outcome summaries
#'   in net monetary benefit calculations. Must be NULL for cost summaries.
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_strategy("standard") |>
#'   add_state("well", initial_prob = 1) |>
#'   add_value("drug_cost", 100, state = "well", type = "cost") |>
#'   add_value("care_cost", 50, state = "well", type = "cost") |>
#'   add_value("qaly", 0.8, state = "well", type = "outcome") |>
#'   add_summary("total_cost", "drug_cost,care_cost", type = "cost") |>
#'   add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000)
#' @export
add_summary <- function(model, name, values, display_name = NULL,
                         description = NULL, type = "outcome", wtp = NULL) {
  validate_string(name, "Summary name")
  if (missing(values)) values <- NA_character_
  new_row <- fast_tibble(
    name = name,
    values = values,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type,
    wtp = if (is.null(wtp)) NA_real_ else as.numeric(wtp)
  )
  entity_add_tibble(model, .schema_summary, new_row, .callbacks_add_summary)
}

# --- Scenario ----------------------------------------------------------------

#' Add a Scenario to Model
#'
#' Define a named scenario for scenario analysis. Scenarios allow you to define
#' alternate sets of parameter values and model settings to test different
#' assumptions. Each scenario is given a name and optional description, and
#' variable/setting overrides can be added using `add_scenario_variable()` and
#' `add_scenario_setting()`.
#'
#' A "Base Case" scenario is automatically created when running scenario analysis,
#' representing the model with default parameters. User-defined scenarios are
#' compared against this base case.
#'
#' @param model An oq_model object
#' @param name Unique name for the scenario (e.g., "Optimistic", "Pessimistic")
#' @param description Optional description explaining the scenario assumptions
#'
#' @param enabled Whether the scenario is enabled (default `TRUE`). Stored with the
#'   scenario.
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_scenario("Optimistic", description = "Best case assumptions") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   add_scenario_variable("Optimistic", "cost", 4000) |>
#'   add_scenario("Pessimistic", description = "Conservative assumptions") |>
#'   add_scenario_variable("Pessimistic", "efficacy", 0.70)
#' }
#' @export
add_scenario <- function(model, name, description = NULL, enabled = TRUE) {
  validate_string(name, "Scenario name")
  new_item <- list(
    name = name,
    description = description %||% name,
    enabled = enabled,
    variable_overrides = list(),
    setting_overrides = list()
  )
  entity_add_list(model, .schema_scenario, new_item)
}

#' Edit a Scenario
#'
#' Modify an existing scenario's name and/or description. Use \code{new_name}
#' to rename the scenario.
#'
#' @param model An oq_model object
#' @param name Character string naming the scenario to edit
#' @param description Optional new description
#'
#' @param enabled New enabled flag for the scenario.
#' @param new_name Optional new name for the scenario
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_scenario("Optimistic") |>
#'   edit_scenario("Optimistic", new_name = "Best Case")
#' }
#' @export
edit_scenario <- function(model, name, description, enabled, new_name) {
  item_idx <- find_in_list(model$scenarios, "name", list(name), "Scenario")
  updates <- list()
  if (!missing(description)) updates$description <- description
  if (!missing(enabled)) updates$enabled <- enabled
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_list(model, .schema_scenario, item_idx, updates)
}

#' Remove a Scenario
#'
#' Remove an existing scenario from the model.
#'
#' @param model An oq_model object
#' @param name Character string naming the scenario to remove
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_scenario("Optimistic") |>
#'   remove_scenario("Optimistic")
#' }
#' @export
remove_scenario <- function(model, name) {
  item_idx <- find_in_list(model$scenarios, "name", list(name), "Scenario")
  entity_remove_list(model, .schema_scenario, item_idx)
}

# --- Scenario Variable Override (nested) ------------------------------------

#' Add Variable Override to Scenario
#'
#' Add a variable value override for a specific scenario. When the scenario
#' analysis runs this scenario, the specified variable will use the override
#' value instead of its default formula.
#'
#' Values can be specified as:
#' - Literal numeric values (e.g., 0.95, 5000)
#' - Expressions using NSE (e.g., base_efficacy * 1.2)
#'
#' @param model An oq_model object
#' @param scenario Name of the scenario to add the override to (must exist)
#' @param variable Name of the variable to override (must exist in model$variables)
#' @param value Value expression for the override (uses NSE)
#' @param strategy Optional strategy name to limit override to specific strategy
#' @param group Optional group name to limit override to specific group
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("efficacy", 0.8) |>
#'   add_variable("cost", 5000) |>
#'   add_scenario("Optimistic") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   add_scenario_variable("Optimistic", "cost", 4000)
#' }
#' @export
add_scenario_variable <- function(model, scenario, variable, value,
                                   strategy = "", group = "") {
  validate_string(scenario, "Scenario name")
  validate_string(variable, "Variable name")
  validate_variable_targeting(model, variable, strategy, group,
                              "Scenario", "add_scenario_variable")
  value_quo <- rlang::enquo(value)
  value_expr <- rlang::quo_get_expr(value_quo)
  stored_value <- if (is.numeric(value_expr)) {
    value_expr
  } else {
    as.oq_formula(rlang::expr_text(value_expr))
  }
  new_item <- list(
    name = variable,
    value = stored_value,
    strategy = as.character(strategy),
    group = as.character(group)
  )
  entity_add_nested(model, .schema_scenario, scenario,
                    .schema_scenario_variable_override, new_item)
}

# --- Scenario Setting Override (nested) -------------------------------------

#' Add Setting Override to Scenario
#'
#' Add a model setting override for a specific scenario. When the scenario
#' analysis runs this scenario, the specified setting will use the override
#' value instead of its default.
#'
#' Common settings that can be overridden include:
#' - `timeframe`: Model time horizon
#' - `discount_cost`: Discount rate for costs (percentage, e.g. 3 for 3%)
#' - `discount_outcomes`: Discount rate for outcomes (percentage, e.g. 3 for 3%)
#' - `cycle_length`: Length of each model cycle
#'
#' @param model An oq_model object
#' @param scenario Name of the scenario to add the override to (must exist)
#' @param setting Name of the setting to override
#' @param value Value for the setting override
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_scenario("Extended Horizon") |>
#'   add_scenario_setting("Extended Horizon", "timeframe", 30) |>
#'   add_scenario("No Discounting") |>
#'   add_scenario_setting("No Discounting", "discount_cost", 0) |>
#'   add_scenario_setting("No Discounting", "discount_outcomes", 0)
#' }
#' @export
add_scenario_setting <- function(model, scenario, setting, value) {
  validate_string(scenario, "Scenario name")
  validate_string(setting, "Setting name")
  new_item <- list(name = setting, value = value)
  entity_add_nested(model, .schema_scenario, scenario,
                    .schema_scenario_setting_override, new_item)
}

# --- Multivariate Sampling --------------------------------------------------

#' Add Multivariate Sampling Specification to Model
#'
#' Define a multivariate distribution for sampling correlated parameters.
#' This is used for Probabilistic Sensitivity Analysis (PSA) when multiple
#' parameters need to be sampled together with a specified correlation structure.
#'
#' @param model An oq_model object
#' @param name Character string naming this sampling specification
#' @param type Distribution type: `"dirichlet"`, `"mvnormal"` or `"multinomial"`.
#' @param variables Character vector of variable names to be sampled together,
#'   or a tibble with columns: variable, strategy, group (for segment-specific sampling)
#' @param strategy Strategy the sampling applies to; empty string (default) for all
#'   strategies.
#' @param group Group the sampling applies to; empty string (default) for all groups.
#' @param covariance For `"mvnormal"`: a formula giving the covariance matrix of the
#'   variables (for example the name of a model table). Required for that type.
#' @param n For `"dirichlet"`: the effective sample size. The base-case values
#'   multiplied by `n` are the Dirichlet parameters. Required for that type.
#'   For `"multinomial"`, the size comes from the base-case counts instead.
#' @param description Optional description of this sampling specification
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' # Dirichlet distribution for transition probabilities
#' model <- define_model("markov") |>
#'   add_variable("p_stable", 0.7) |>
#'   add_variable("p_progression", 0.2) |>
#'   add_variable("p_death", 0.1) |>
#'   add_multivariate_sampling(
#'     name = "transition_probs",
#'     type = "dirichlet",
#'     variables = c("p_stable", "p_progression", "p_death"),
#'     n = 100
#'   )
#'
#' # Multivariate normal with covariance from a model table
#' model <- model |>
#'   add_variable("shape", 1.5) |>
#'   add_variable("scale", 0.02) |>
#'   add_table("surv_vcov", as.data.frame(vcov(fit))) |>
#'   add_multivariate_sampling(
#'     name = "survival_params",
#'     type = "mvnormal",
#'     variables = c("shape", "scale"),
#'     covariance = "surv_vcov"
#'   )
#'
#' # Strategy-specific sampling
#' model <- model |>
#'   add_multivariate_sampling(
#'     name = "strategy_specific",
#'     type = "mvnormal",
#'     variables = c("treatment_cost", "treatment_qaly"),
#'     strategy = "intervention",
#'     covariance = "cost_qaly_cov"
#'   )
#' }
#' @export
add_multivariate_sampling <- function(model, name, type, variables,
                                       strategy = "", group = "",
                                       covariance = NULL, n = NULL,
                                       description = NULL) {
  validate_string(name, "Multivariate sampling name")
  type <- match.arg(type, c("dirichlet", "mvnormal", "multinomial"))
  if (!is.character(variables) || length(variables) == 0) {
    stop("variables must be a non-empty character vector.", call. = FALSE)
  }
  cov_formula <- if (!is.null(covariance)) as.oq_formula(covariance) else NULL
  new_item <- list(
    name = name, type = type, variables = variables,
    strategy = as.character(strategy), group = as.character(group),
    covariance = cov_formula, n = n,
    description = description %||% ""
  )
  entity_add_list(model, .schema_multivariate_sampling, new_item,
                  .callbacks_add_mv_sampling)
}

#' Edit a Multivariate Sampling Specification
#'
#' Modify an existing multivariate sampling specification. Any parameter not
#' provided will retain its current value. Use \code{new_name} to rename.
#'
#' @param model An oq_model object
#' @param name Character string identifying the specification to edit
#' @param new_name Optional new name for renaming
#' @param type Optional new type ("dirichlet", "mvnormal", "multinomial")
#' @param variables Optional new variables (character vector)
#' @param strategy Optional new strategy targeting
#' @param group Optional new group targeting
#' @param covariance Optional new covariance table name (for mvnormal)
#' @param n Optional new effective sample size (for dirichlet)
#' @param description Optional new description
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- model |>
#'   edit_multivariate_sampling(
#'     name = "transition_probs",
#'     n = 200
#'   )
#' }
#' @export
edit_multivariate_sampling <- function(model, name, new_name, type, variables,
                                        strategy, group, covariance, n,
                                        description) {
  item_idx <- find_in_list(model$multivariate_sampling, "name", list(name),
                           "Multivariate sampling specification")
  updates <- list()
  if (!missing(new_name)) updates$new_name <- new_name
  if (!missing(type)) updates$type <- match.arg(type, c("dirichlet", "mvnormal", "multinomial"))
  if (!missing(variables)) updates$variables <- variables
  if (!missing(strategy)) updates$strategy <- as.character(strategy)
  if (!missing(group)) updates$group <- as.character(group)
  if (!missing(covariance)) updates$covariance <- as.oq_formula(covariance)
  if (!missing(n)) updates$n <- n
  if (!missing(description)) updates$description <- description
  entity_edit_list(model, .schema_multivariate_sampling, item_idx, updates)
}

#' Remove a Multivariate Sampling Specification
#'
#' Remove an existing multivariate sampling specification from the model.
#'
#' @param model An oq_model object
#' @param name Character string identifying the specification to remove
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- model |>
#'   remove_multivariate_sampling("transition_probs")
#' }
#' @export
remove_multivariate_sampling <- function(model, name) {
  item_idx <- find_in_list(model$multivariate_sampling, "name", list(name),
                           "Multivariate sampling specification")
  entity_remove_list(model, .schema_multivariate_sampling, item_idx)
}

# --- DSA Variable -----------------------------------------------------------

#' Add Deterministic Sensitivity Analysis Variable to Model
#'
#' Define a variable to include in deterministic sensitivity analysis (DSA).
#' DSA will run the model with this variable set to its low and high values
#' to assess parameter sensitivity.
#'
#' The low and high bounds can be specified as:
#' - Literal numeric values (e.g., 0.01, 0.05)
#' - Expressions using the `bc` keyword to reference the base case value (e.g., bc * 0.5)
#' - Expressions referencing other model variables (e.g., bc - 2 * cost_se)
#'
#' **Display Names**: DSA parameters automatically inherit display names from their variables.
#' Since variables auto-generate unique display names for strategy/group combinations
#' (e.g., "cost, treatment_a"), DSA parameters will also have unique names. You can optionally
#' override with custom `display_name` values in either `add_variable()` or `add_dsa_variable()`.
#'
#' @param model An oq_model object
#' @param variable Character string naming the variable to vary (must exist in model$variables)
#' @param low Expression or numeric value for the low bound of the sensitivity range.
#'   Can use `bc` keyword to reference the base case value and other model variables.
#' @param high Expression or numeric value for the high bound of the sensitivity range.
#'   Can use `bc` keyword to reference the base case value and other model variables.
#' @param strategy Optional strategy name to limit DSA to specific strategy
#' @param group Optional group name to limit DSA to specific group
#' @param display_name Optional display name for plots and tables. If not provided,
#'   inherits from the variable definition. Required to be unique across all DSA parameters.
#' @param range_label Optional custom text for the input range portion of tornado labels.
#'   When provided, labels show "Parameter Name (range_label)" instead of auto-generated
#'   "Parameter Name (low - high)". For example, use "Base Case ± 20\%" or "±50\%".
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' # Example 1: Literal values (backward compatible)
#' model <- define_model("markov") |>
#'   add_variable("p_disease", 0.03) |>
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#'
#' # Example 2: Relative to base case using bc keyword
#' model <- define_model("markov") |>
#'   add_variable("p_disease", 0.03) |>
#'   add_dsa_variable("p_disease",
#'                    low = bc * 0.5,    # 50% of base case
#'                    high = bc * 1.5)   # 150% of base case
#'
#' # Example 3: Using other variables (standard error pattern)
#' model <- define_model("markov") |>
#'   add_variable("cost_tx", 1000) |>
#'   add_variable("cost_tx_se", 100) |>
#'   add_dsa_variable("cost_tx",
#'                    low = bc - 2 * cost_tx_se,  # Base - 2 SE
#'                    high = bc + 2 * cost_tx_se) # Base + 2 SE
#'
#' # Example 4: Strategy-specific DSA with auto-generated display names
#' model <- define_model("markov") |>
#'   add_variable("p_prog", strategy = "treatment_a", formula = 0.3) |>
#'   add_variable("p_prog", strategy = "treatment_b", formula = 0.2) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_a", low = 0.2, high = 0.4) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_b", low = 0.1, high = 0.3)
#' # Auto-generates: "p_prog, treatment_a" and "p_prog, treatment_b"
#'
#' # Example 5: Override auto-generated names with custom display names
#' model <- define_model("markov") |>
#'   add_variable("p_prog", display_name = "Treatment A Progression",
#'                strategy = "treatment_a", formula = 0.3) |>
#'   add_variable("p_prog", display_name = "Treatment B Progression",
#'                strategy = "treatment_b", formula = 0.2) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_a", low = 0.2, high = 0.4) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_b", low = 0.1, high = 0.3)
#' }
#' @export
add_dsa_variable <- function(model, variable, low, high,
                              strategy = "", group = "",
                              display_name = NULL, range_label = NULL) {
  validate_string(variable, "Variable name")
  validate_variable_targeting(model, variable, strategy, group,
                              "DSA", "add_dsa_variable")
  low_f <- capture_nse_formula(rlang::enquo(low))
  high_f <- capture_nse_formula(rlang::enquo(high))
  new_item <- list(
    type = "variable", name = variable,
    low = low_f, high = high_f,
    strategy = as.character(strategy), group = as.character(group),
    display_name = display_name, range_label = range_label
  )
  entity_add_list(model, .schema_dsa_param, new_item)
}

#' Add Deterministic Sensitivity Analysis Setting to Model
#'
#' Define a model setting to include in deterministic sensitivity analysis (DSA).
#' Settings include parameters like discount rates, timeframe, and other global
#' model configuration values that are not part of the variables table.
#'
#' @param model An oq_model object
#' @param setting Character string naming the setting to vary
#'   (e.g., "discount_cost", "discount_outcomes", "timeframe")
#' @param low Value for the low bound (numeric or character depending on setting)
#' @param high Value for the high bound (numeric or character depending on setting)
#' @param display_name Optional display name for plots and tables
#' @param range_label Optional custom text for the input range portion of tornado labels.
#'   When provided, labels show "Parameter Name (range_label)" instead of auto-generated
#'   "Parameter Name (low - high)". For example, use "Base Case ± 20\%" or "±50\%".
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_dsa_setting("discount_cost", low = 0, high = 5) |>
#'   add_dsa_setting("timeframe", low = 10, high = 30)
#' }
#' @export
add_dsa_setting <- function(model, setting, low, high,
                             display_name = NULL, range_label = NULL) {
  validate_string(setting, "Setting name")
  new_item <- list(
    type = "setting", name = setting,
    low = low, high = high,
    display_name = display_name %||% setting,
    range_label = range_label
  )
  entity_add_list(model, .schema_dsa_setting, new_item)
}

# --- Threshold Analysis -----------------------------------------------------

#' Add Threshold Analysis to Model
#'
#' Define a threshold analysis that finds the input parameter value producing
#' a desired output condition using iterative root-finding.
#'
#' @param model An oq_model object
#' @param name Unique name for this threshold analysis
#' @param variable Variable to solve for
#' @param lower Lower bound for search range
#' @param upper Upper bound for search range
#' @param condition A condition list created by \code{threshold_condition_*} functions
#' @param variable_strategy Strategy targeting for the variable (empty string = all)
#' @param variable_group Group targeting for the variable (empty string = all)
#' @param active Whether this analysis is active
#'
#' @return The modified model object
#' @export
add_threshold_analysis <- function(model, name, variable, lower, upper,
                                    condition, variable_strategy = "",
                                    variable_group = "", active = TRUE) {
  if (!inherits(model, "oq_model")) {
    stop("model must be an oq_model object created with define_model()", call. = FALSE)
  }
  validate_string(name, "Threshold analysis name")
  validate_string(variable, "Variable name")
  if (!is.numeric(lower) || !is.numeric(upper)) {
    stop("lower and upper must be numeric.", call. = FALSE)
  }
  if (lower >= upper) {
    stop("lower must be less than upper.", call. = FALSE)
  }
  if (!is.list(condition) || is.null(condition$output)) {
    stop("condition must be a list with an 'output' field.", call. = FALSE)
  }
  if (condition$output == "vbp") {
    stop("VBP output type is not yet supported for threshold analyses.", call. = FALSE)
  }
  validate_variable_targeting(model, variable, variable_strategy, variable_group,
                              "threshold", "add_threshold_analysis")
  validate_threshold_condition(condition)

  new_item <- list(
    name = name, variable = variable,
    variable_strategy = as.character(variable_strategy),
    variable_group = as.character(variable_group),
    lower = lower, upper = upper,
    condition = condition, active = active
  )
  entity_add_list(model, .schema_threshold_analysis, new_item)
}

#' Edit a Threshold Analysis
#'
#' Modify an existing threshold analysis. Only specified parameters are updated;
#' all others remain unchanged.
#'
#' @param model An oq_model object
#' @param name Character string naming the threshold analysis to edit
#' @param new_name Optional new name for the analysis
#' @param variable Optional new variable to solve for
#' @param variable_strategy Optional new strategy targeting for the variable
#' @param variable_group Optional new group targeting for the variable
#' @param lower Optional new lower bound for search range
#' @param upper Optional new upper bound for search range
#' @param condition Optional new condition list created by \code{threshold_condition_*} functions
#' @param active Optional new active flag
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("cost", 100) |>
#'   add_threshold_analysis("ta1", "cost", 0, 200,
#'     condition = threshold_condition_outcomes(
#'       summary = "total_qalys", type = "absolute", strategy = "A")) |>
#'   edit_threshold_analysis("ta1", new_name = "ta1_renamed", lower = 10)
#' }
#' @export
edit_threshold_analysis <- function(model, name, new_name, variable,
                                     variable_strategy, variable_group,
                                     lower, upper, condition, active) {
  item_idx <- find_in_list(model$threshold_analyses, "name", list(name),
                           "Threshold analysis")
  updates <- list()
  if (!missing(new_name)) {
    validate_string(new_name, "New name")
    check_duplicate_list(
      model$threshold_analyses, "name", list(new_name),
      list(entity_name = "Threshold analysis", duplicate_action = "error"),
      exclude_idx = item_idx
    )
    updates$new_name <- new_name
  }
  if (!missing(variable)) {
    validate_string(variable, "Variable name")
    updates$variable <- variable
  }
  if (!missing(variable_strategy)) updates$variable_strategy <- as.character(variable_strategy)
  if (!missing(variable_group)) updates$variable_group <- as.character(variable_group)

  if (!missing(lower) || !missing(upper)) {
    eff_lower <- if (!missing(lower)) lower else model$threshold_analyses[[item_idx]]$lower
    eff_upper <- if (!missing(upper)) upper else model$threshold_analyses[[item_idx]]$upper
    if (!is.numeric(eff_lower) || !is.numeric(eff_upper)) {
      stop("lower and upper must be numeric.", call. = FALSE)
    }
    if (eff_lower >= eff_upper) {
      stop("lower must be less than upper.", call. = FALSE)
    }
    if (!missing(lower)) updates$lower <- lower
    if (!missing(upper)) updates$upper <- upper
  }

  if (!missing(condition)) {
    validate_threshold_condition(condition)
    updates$condition <- condition
  }
  if (!missing(active)) updates$active <- active

  eff_variable <- updates$variable %||% model$threshold_analyses[[item_idx]]$variable
  eff_strategy <- updates$variable_strategy %||% model$threshold_analyses[[item_idx]]$variable_strategy
  eff_group <- updates$variable_group %||% model$threshold_analyses[[item_idx]]$variable_group
  if (!missing(variable) || !missing(variable_strategy) || !missing(variable_group)) {
    validate_variable_targeting(model, eff_variable, eff_strategy, eff_group,
                                "threshold", "edit_threshold_analysis")
  }

  entity_edit_list(model, .schema_threshold_analysis, item_idx, updates)
}

#' Remove a Threshold Analysis
#'
#' Remove an existing threshold analysis from the model.
#'
#' @param model An oq_model object
#' @param name Character string naming the threshold analysis to remove
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("cost", 100) |>
#'   add_threshold_analysis("ta1", "cost", 0, 200,
#'     condition = threshold_condition_outcomes(
#'       summary = "total_qalys", type = "absolute", strategy = "A")) |>
#'   remove_threshold_analysis("ta1")
#' }
#' @export
remove_threshold_analysis <- function(model, name) {
  item_idx <- find_in_list(model$threshold_analyses, "name", list(name),
                           "Threshold analysis")
  entity_remove_list(model, .schema_threshold_analysis, item_idx)
}

# --- Override Category -------------------------------------------------------

#' Add an Override Category to Model
#'
#' Create a named category for grouping related override controls.
#' Override categories organize UI controls that allow users to modify
#' model variables and settings at runtime.
#'
#' @param model An oq_model object
#' @param name Character string for the category name (must be unique, case-insensitive)
#' @param general Logical indicating if this is a system category (default: FALSE)
#'
#' @return The modified model object
#' @export
add_override_category <- function(model, name, general = FALSE) {
  validate_string(name, "Override category name")
  new_item <- list(name = name, general = general, overrides = list())
  entity_add_list(model, .schema_override_category, new_item)
}

# --- Threshold Condition Constructors (pure helpers, not CRUD) ---------------

#' Create Threshold Condition for Outcomes
#'
#' @param summary Target a summary (e.g., "total_qalys"). Mutually exclusive with \code{value}.
#' @param value Target an individual value (e.g., "qaly_sick"). Mutually exclusive with \code{summary}.
#' @param type Either "absolute" (single strategy) or "difference" (referent minus comparator)
#' @param strategy Strategy name for absolute type
#' @param referent Referent strategy for difference type
#' @param comparator Comparator strategy for difference type
#' @param discounted Whether to use discounted results
#' @param target_value Target value to find threshold for
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#'
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_outcomes <- function(summary = NULL, value = NULL,
                                          type = c("absolute", "difference"),
                                          strategy = NULL, referent = NULL,
                                          comparator = NULL, discounted = TRUE,
                                          target_value = 0, group = "") {
  type <- match.arg(type)
  list(output = "outcomes", summary = summary, value = value, type = type,
       strategy = strategy, referent = referent, comparator = comparator,
       discounted = discounted, target_value = target_value, group = group)
}

#' Create Threshold Condition for Costs
#'
#' @param summary Target a summary (e.g., "total_cost"). Mutually exclusive with \code{value}.
#' @param value Target an individual value (e.g., "cost_drug"). Mutually exclusive with \code{summary}.
#' @param type Either "absolute" (single strategy) or "difference" (referent minus comparator)
#' @param strategy Strategy name for absolute type
#' @param referent Referent strategy for difference type
#' @param comparator Comparator strategy for difference type
#' @param discounted Whether to use discounted results
#' @param target_value Target value to find threshold for
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#'
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_costs <- function(summary = NULL, value = NULL,
                                       type = c("absolute", "difference"),
                                       strategy = NULL, referent = NULL,
                                       comparator = NULL, discounted = TRUE,
                                       target_value = 0, group = "") {
  type <- match.arg(type)
  list(output = "costs", summary = summary, value = value, type = type,
       strategy = strategy, referent = referent, comparator = comparator,
       discounted = discounted, target_value = target_value, group = group)
}

#' Create Threshold Condition for NMB
#'
#' @param health_summary Health outcome summary name
#' @param cost_summary Cost outcome summary name
#' @param referent Referent strategy
#' @param comparator Comparator strategy
#' @param discounted Whether to use discounted results
#' @param target_value Target NMB value
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @param wtp Willingness-to-pay value. If NULL (default), the WTP from the health summary is used.
#'
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_nmb <- function(health_summary, cost_summary,
                                     referent, comparator,
                                     discounted = TRUE, target_value = 0,
                                     group = "", wtp = NULL) {
  list(output = "nmb", health_summary = health_summary,
       cost_summary = cost_summary, referent = referent,
       comparator = comparator, discounted = discounted,
       target_value = target_value, group = group, wtp = wtp)
}

#' Create Threshold Condition for CE
#'
#' @param health_summary Health outcome summary name
#' @param cost_summary Cost outcome summary name
#' @param referent Referent strategy
#' @param comparator Comparator strategy
#' @param discounted Whether to use discounted results
#' @param target_value Not used for CE conditions: the threshold is always where the
#'   referent and comparator are equally cost-effective at the WTP.
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @param wtp Willingness-to-pay value. If NULL (default), the WTP from the health summary is used.
#'
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_ce <- function(health_summary, cost_summary,
                                    referent, comparator,
                                    discounted = TRUE, target_value = 0,
                                    group = "", wtp = NULL) {
  list(output = "ce", health_summary = health_summary,
       cost_summary = cost_summary, referent = referent,
       comparator = comparator, discounted = discounted,
       target_value = target_value, group = group, wtp = wtp)
}

# --- State (S3 generic) -----------------------------------------------------

#' Add States to Model
#'
#' Add one or more states to the model.
#'
#' @param model An oq_model object
#' @param name Character string for the state name
#' @param ... Arguments for the model type:
#'   \describe{
#'     \item{Markov}{`display_name`, `description` (optional labels),
#'       `state_group` (optional state group for grouping related states),
#'       `share_state_time` (whether to share state time with other states in
#'       the group, default `FALSE`), `state_cycle_limit` (optional limit on the
#'       number of cycles tracked in this state), `state_cycle_limit_unit`
#'       (unit of the limit, default `"cycles"`) and `initial_prob`
#'       (required initial probability).}
#'     \item{PSM, custom PSM}{`display_name`, `description`. `initial_prob`
#'       is not used and is an error.}
#'     \item{Decision tree}{States are not supported.}
#'   }
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_state("healthy", initial_prob = 1) |>
#'   add_state("sick", initial_prob = 0)
#' @export
add_state <- function(model, name, ...) UseMethod("add_state")

#' Edit a State in the Model
#'
#' Modify an existing state's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the state are updated (transitions, values,
#' and threshold analyses).
#'
#' @param model An oq_model object
#' @param name Character string identifying the state to edit
#' @param ... Fields to update: `display_name`, `description`, and
#'   `new_name` (a new name for the state; the state and all references to it
#'   are renamed). Markov models also accept `state_group`,
#'   `share_state_time`, `state_cycle_limit`, `state_cycle_limit_unit` and
#'   `initial_prob`.
#'
#' @return The modified model object
#' @export
edit_state <- function(model, name, ...) UseMethod("edit_state")

#' Remove a State from the Model
#'
#' Remove an existing state and optionally cascade-remove all downstream
#' references (transitions, values, and threshold analyses).
#'
#' @param model An oq_model object
#' @param name Character string identifying the state to remove
#' @param ... `error_on_dependencies`: logical. If `TRUE` and the state has
#'   downstream dependencies, an error of class `"state_has_dependencies"` is
#'   thrown instead of removing anything (default `FALSE`).
#'
#' @return The modified model object
#' @export
remove_state <- function(model, name, ...) UseMethod("remove_state")

# --- Transitions (S3 generic) ------------------------------------------------

#' Add Transitions to Model
#'
#' Add one or more transitions to the model. Uses NSE to capture formula expressions.
#'
#' @param model An oq_model object
#' @param ... Arguments for the model type:
#'   \describe{
#'     \item{Markov}{`from_state`, `to_state` and `formula` (an unquoted R
#'       expression for the transition probability).}
#'     \item{PSM}{`endpoint` (`"PFS"` or `"OS"`), `time_unit` and `formula`
#'       (an unquoted R expression for the survival distribution).}
#'     \item{Custom PSM}{`state` and `formula` (an unquoted R expression for
#'       the state probability).}
#'     \item{Decision tree}{Transitions are not supported.}
#'   }
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_transition("healthy", "sick", p_disease * 0.1) |>
#'   add_transition("sick", "dead", 0.2)
#' @export
add_transition <- function(model, ...) UseMethod("add_transition")

#' Edit a Markov Transition
#'
#' Edit the formula of an existing Markov transition.
#'
#' @param model An oq_model object
#' @param ... The transition to edit and its new formula: `from_state`,
#'   `to_state` and `formula` (Markov); `endpoint`, and `formula` and/or
#'   `time_unit` (PSM); `state` and `formula` (custom PSM).
#'
#' @return The modified model object
#' @export
edit_transition <- function(model, ...) UseMethod("edit_transition")

#' Remove a Markov Transition
#'
#' Remove an existing Markov transition.
#'
#' @param model An oq_model object
#' @param ... The transition to remove: `from_state` and `to_state`
#'   (Markov), `endpoint` (PSM) or `state` (custom PSM).
#'
#' @return The modified model object
#' @export
remove_transition <- function(model, ...) UseMethod("remove_transition")

# --- Value (via engine) ------------------------------------------------------

#' Add a Value
#'
#' @param model A model object.
#' @param name Value name.
#' @param formula Value formula.
#' @param state State (residency value), origin state (transitional value),
#'   `"decision_tree"`, or `NA` (model start value).
#' @param destination Destination state for transitional values, otherwise `NA`.
#' @param display_name,description Optional labels.
#' @param type `"outcome"` or `"cost"`.
#' @param discounting_override Optional formula replacing the value's discount
#'   factors (see the discounting documentation).
#' @param half_cycle_method Optional half-cycle method for this value
#'   (`"start"`, `"end"` or `"life-table"`). If `NA`, the model's
#'   `half_cycle_method` setting applies. It sets both the occupancy the value
#'   is charged on and when it is discounted; for transitional values only when
#'   it is discounted. All rows of a value name must use the same setting.
#'
#' @return The modified model.
#' @export
add_value <- function(model, name, formula, state = NA, destination = NA,
                       display_name = NULL, description = NULL,
                       type = "outcome", discounting_override = NA,
                       half_cycle_method = NA) {
  validate_string(name, "Value name", "string")
  if (!type %in% c("outcome", "cost")) stop("Value type must be 'outcome' or 'cost'.", call. = FALSE)
  validate_value_half_cycle_method(half_cycle_method)
  formula_str <- capture_nse(rlang::enquo(formula))
  if (!nzchar(trimws(formula_str))) stop("Value formula must be a non-empty expression.", call. = FALSE)
  new_value <- fast_tibble(
    name = name, formula = formula_str,
    state = as.character(state), destination = as.character(destination),
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type, discounting_override = as.character(discounting_override),
    half_cycle_method = as.character(half_cycle_method)
  )
  entity_add_tibble(model, .schema_value, new_value, .callbacks_add_value)
}

validate_value_half_cycle_method <- function(half_cycle_method) {
  if (length(half_cycle_method) != 1 || (!is.na(half_cycle_method) &&
      !as.character(half_cycle_method) %in% c(valid_half_cycle_methods, "", "NA"))) {
    stop("Value half_cycle_method must be NA or one of: ",
         paste0("'", valid_half_cycle_methods, "'", collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Edit a Value in the Model
#'
#' Modify an existing value's properties. The value is identified by its
#' \code{name}, \code{state}, and \code{destination} combination. Only the
#' fields that are explicitly provided will be updated.
#'
#' @param model An oq_model object
#' @param name Character string identifying the value to edit
#' @param state State used for lookup (default NA)
#' @param destination Destination used for lookup (default NA)
#' @param formula An unquoted R expression for the value calculation
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param type Optional new type ("outcome" or "cost")
#' @param discounting_override Optional discounting override formula
#' @param half_cycle_method New half-cycle method for this value
#'   (`"start"`, `"end"` or `"life-table"`). If `NA`, the model's
#'   `half_cycle_method` setting applies. It sets both the occupancy the value
#'   is charged on and when it is discounted; for transitional values only when
#'   it is discounted. All rows of a value name must use the same setting.
#' @param new_name Optional new name for the value row(s)
#' @param rename_all Logical. When TRUE and \code{new_name} is provided, renames
#'   ALL rows with the old name and cascades to summaries. Default FALSE.
#' @param error_on_field_changes Logical. When TRUE and merging into an existing
#'   name, throws error class \code{"value_field_changes"} if display_name or
#'   description would change. Default FALSE.
#' @param error_on_name_sharing Logical. When TRUE and \code{new_name} is provided,
#'   throws error class \code{"value_name_shared"} if the old name has multiple
#'   rows. Default FALSE.
#'
#' @return The modified model object
#' @export
edit_value <- function(model, name, state = NA, destination = NA,
                        formula, display_name, description, type,
                        discounting_override, half_cycle_method, new_name,
                        rename_all = FALSE,
                        error_on_field_changes = FALSE,
                        error_on_name_sharing = FALSE) {
  # Custom not-found with state/destination context
  match_idx <- tryCatch(
    find_in_tibble(model$values, c("name", "state", "destination"),
      list(name, as.character(state), as.character(destination)), "value", na_safe = TRUE),
    error = function(e) {
      target_desc <- name
      if (!is.na(state) && as.character(state) != "NA") target_desc <- paste0(target_desc, ", state=\'", state, "\'")
      if (!is.na(destination) && as.character(destination) != "NA") target_desc <- paste0(target_desc, ", destination=\'", destination, "\'")
      stop(sprintf("No value found matching: %s", target_desc), call. = FALSE)
    }
  )
  updates <- list()
  if (!missing(formula)) updates$formula <- capture_nse(rlang::enquo(formula))
  if (!missing(discounting_override)) {
    disc_quo <- rlang::enquo(discounting_override)
    disc_expr <- rlang::quo_get_expr(disc_quo)
    updates$discounting_override <- if (is.character(disc_expr)) disc_expr else rlang::expr_text(disc_expr)
  }
  if (!missing(type)) {
    if (!type %in% c("outcome", "cost")) stop("Value type must be 'outcome' or 'cost'.", call. = FALSE)
    updates$type <- type
  }
  if (!missing(half_cycle_method)) {
    validate_value_half_cycle_method(half_cycle_method)
  }
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(new_name)) {
    updates$new_name <- new_name
    updates$.rename_all <- rename_all
    updates$.error_on_field_changes <- error_on_field_changes
    updates$.error_on_name_sharing <- error_on_name_sharing
  }
  model <- entity_edit_tibble(model, .schema_value, match_idx, updates, .callbacks_edit_value)
  # All rows of a value share one half-cycle method, so it is set on every row
  if (!missing(half_cycle_method)) {
    final_name <- if (!missing(new_name)) new_name else name
    if (!"half_cycle_method" %in% names(model$values)) model$values$half_cycle_method <- NA_character_
    model$values$half_cycle_method[model$values$name == final_name] <- as.character(half_cycle_method)
  }
  model
}

#' Remove a Value from the Model
#'
#' Remove one or more value rows from the model. If only \code{name} is given,
#' all rows with that value name are removed. If \code{state} and/or
#' \code{destination} are specified, only matching rows are removed.
#'
#' When the last row for a given value name is removed, references to that
#' value are also removed from summary CSV strings. If \code{error_on_dependencies}
#' is TRUE and the value name would be fully removed, an error of class
#' \code{"value_has_dependencies"} is thrown instead.
#'
#' @param model An oq_model object
#' @param name Character string identifying the value to remove
#' @param state Optional state to narrow removal. If NULL, all states are matched.
#' @param destination Optional destination to narrow removal. If NULL, all
#'   destinations are matched.
#' @param error_on_dependencies Logical. If TRUE and the value name would be
#'   fully removed while referenced in summaries, an error of class
#'   \code{"value_has_dependencies"} is thrown. Default FALSE.
#'
#' @return The modified model object
#' @export
remove_value <- function(model, name, state = NULL, destination = NULL,
                          error_on_dependencies = FALSE) {
  # Build match mask (partial removal support)
  match_mask <- model$values$name == name
  if (!is.null(state)) match_mask <- match_mask & safe_field_eq(model$values$state, as.character(state))
  if (!is.null(destination)) match_mask <- match_mask & safe_field_eq(model$values$destination, as.character(destination))
  match_idx <- which(match_mask)
  if (length(match_idx) == 0) {
    target_desc <- name
    if (!is.null(state)) target_desc <- paste0(target_desc, ", state='", state, "'")
    if (!is.null(destination)) target_desc <- paste0(target_desc, ", destination='", destination, "'")
    stop(sprintf("No value found matching: %s", target_desc), call. = FALSE)
  }
  entity_remove_tibble(model, .schema_value, match_idx, .callbacks_remove_value,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

# --- Variable (via engine) ---------------------------------------------------

#' Add Variables to Model
#'
#' Add one or more variables to the model. Uses NSE to capture formula expressions.
#'
#' **Display Names**: The `display_name` parameter is optional. If not provided, display names
#' are automatically generated based on the variable name, strategy, and group:
#' - No strategy/group: `"var_name"`
#' - Strategy only: `"var_name, strategy_name"`
#' - Group only: `"var_name, group_name"`
#' - Both: `"var_name, strategy_name, group_name"`
#'
#' This auto-generation ensures unique display names for strategy/group-specific variables,
#' which is important for DSA/PSA plots and tables.
#'
#' **Important**: When a variable has multiple definitions (e.g., one per strategy/group), if you
#' provide a custom `display_name` for at least one definition, you must provide it for ALL definitions
#' of that variable. Mixing custom and auto-generated display names for the same variable will result
#' in a validation error **immediately** when you call `add_variable()` with the inconsistent definition.
#'
#' @param model An oq_model object
#' @param name Character string for the variable name
#' @param formula An unquoted R expression for the variable calculation
#' @param display_name Optional display name. If not provided, will be auto-generated
#'   from name, strategy, and group. You can override with a custom name if desired.
#' @param description Optional description
#' @param strategy Optional strategy association
#' @param group Optional group association
#' @param source Optional source information
#' @param sampling Optional sampling information
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_variable("p_disease", look_up(table, age = cohort_age))
#'
#' # Strategy-specific variables - display names auto-generated
#' model <- model |>
#'   add_variable("cost_med", strategy = "treatment_a", formula = 5000,
#'                sampling = normal(5000, 500)) |>
#'   add_variable("cost_med", strategy = "treatment_b", formula = 3000,
#'                sampling = normal(3000, 300))
#' # Auto-generates: "cost_med, treatment_a" and "cost_med, treatment_b"
#'
#' # Or provide custom display names if preferred
#' model <- model |>
#'   add_variable("cost_admin", display_name = "Admin Cost (Treatment A)",
#'                strategy = "treatment_a", formula = 500) |>
#'   add_variable("cost_admin", display_name = "Admin Cost (Treatment B)",
#'                strategy = "treatment_b", formula = 300)
#' @export
add_variable <- function(model, name, formula, display_name = NULL,
                          description = NULL, strategy = "", group = "",
                          source = "", sampling) {
  validate_string(name, "Variable name", "string")
  formula_str <- capture_nse(rlang::enquo(formula))
  if (!nzchar(trimws(formula_str))) stop("Variable formula must be a non-empty expression.", call. = FALSE)
  sampling_str <- if (missing(sampling)) "" else {
    sq <- rlang::enquo(sampling)
    se <- rlang::quo_get_expr(sq)
    if (is.character(se)) se else rlang::expr_text(se)
  }
  if (is.null(display_name)) display_name <- ""
  new_var <- fast_tibble(
    name = name, formula = formula_str,
    display_name = display_name, description = description %||% "",
    strategy = strategy, group = group,
    source = source, sampling = sampling_str
  )
  entity_add_tibble(model, .schema_variable, new_var, .callbacks_add_variable)
}

# --- Settings (copied from original — not CRUD) -----------------------------

#' Set Model Settings
#'
#' @param model A model object.
#' @param ... Named settings, e.g. `n_cycles`, `cycle_length`,
#'   `cycle_length_unit`, `discount_cost`, `discount_outcomes`,
#'   `half_cycle_method`.
#'
#' @details
#' `half_cycle_method` is one of `"start"` (default), `"end"` or
#' `"life-table"`. It sets when values accrue in each cycle, and so both the
#' occupancy they are charged on and when they are discounted. Formulas are
#' evaluated per cycle (in cycle k, `cycle_lag` is the start and `cycle` the end
#' of the cycle), and cycle k's value applies only to cycle k:
#' \itemize{
#'   \item `"start"`: occupancy at the start of the cycle, discounted at the
#'     start;
#'   \item `"end"`: occupancy at the end of the cycle, discounted at the end;
#'   \item `"life-table"`: the average of the two, each half discounted at its
#'     own time.
#' }
#' Transitional values are charged in the cycle the transitions occur under
#' every method; the method only sets when they are discounted. Model start
#' values are charged in cycle 1 at time 0 (discounted only by a decision tree
#' phase). Individual values
#' can override the method (see [add_value()]); for example, a one-off cost
#' written as `ifelse(cycle == 1, cost, 0)` is charged in full with `"start"`,
#' and an entry cost written with `state_cycle == 1` equals the transitional
#' value with `"end"`.
#'
#' `discount_method` is `"by_cycle"` (continuous time, default) or
#' `"by_year"` (time floored to whole years).
#'
#' @return The modified model.
#' @export
set_settings <- function(model, ...) {
  dots <- list(...)
  if ("model_type" %in% names(dots)) {
    stop("model_type cannot be changed after model creation. It was set to '",
         model$settings$model_type, "' in define_model().")
  }
  setting_map <- c(
    n_cycles = "timeframe", timeframe = "timeframe",
    timeframe_unit = "timeframe_unit", cycle_length_unit = "cycle_length_unit",
    cycle_length = "cycle_length", discount_cost = "discount_cost",
    discount_outcomes = "discount_outcomes", half_cycle_method = "half_cycle_method",
    discount_method = "discount_method",
    reduce_state_cycle = "reduce_state_cycle", days_per_year = "days_per_year",
    country = "country", number_country = "number_country"
  )
  for (name in names(dots)) {
    setting_name <- if (name %in% names(setting_map)) setting_map[name] else name
    model$settings[[setting_name]] <- dots[[name]]
  }
  if ("half_cycle_method" %in% names(dots) &&
      !isTRUE(model$settings$half_cycle_method %in% valid_half_cycle_methods)) {
    stop("Invalid half_cycle_method: '", format(dots$half_cycle_method), "'. Must be one of: ",
         paste(valid_half_cycle_methods, collapse = ", "), call. = FALSE)
  }
  if ("discount_method" %in% names(dots) &&
      !isTRUE(model$settings$discount_method %in% valid_discount_methods)) {
    stop("Invalid discount_method: '", format(dots$discount_method), "'. Must be one of: ",
         paste(valid_discount_methods, collapse = ", "), call. = FALSE)
  }
  # n_cycles sets the timeframe in cycles
  if ("n_cycles" %in% names(dots)) {
    model$settings$timeframe_unit <- "cycles"
  }
  if (is.null(model$settings$days_per_year)) model$settings$days_per_year <- 365
  if (is.null(model$settings$half_cycle_method)) model$settings$half_cycle_method <- "start"
  if (is.null(model$settings$discount_cost)) model$settings$discount_cost <- 0
  if (is.null(model$settings$discount_outcomes)) model$settings$discount_outcomes <- 0
  model
}

# --- VBP / PSA (singleton setters — not CRUD) --------------------------------

#' Set VBP Configuration
#'
#' Configure value-based pricing (VBP) parameters on the model so they can be
#' serialized and used as defaults by \code{run_vbp()}, \code{run_dsa()},
#' \code{run_scenario()}, and \code{run_twsa()}.
#'
#' @param model An oq_model object
#' @param price_variable Name of the variable representing the intervention's price
#' @param intervention_strategy Name of the intervention strategy
#' @param outcome_summary Name of the outcome summary to use
#' @param cost_summary Name of the cost summary to use
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   set_vbp(
#'     price_variable = "c_drug",
#'     intervention_strategy = "treatment",
#'     outcome_summary = "total_qalys",
#'     cost_summary = "total_costs"
#'   )
#' @export
set_vbp <- function(model, price_variable, intervention_strategy,
                     outcome_summary, cost_summary) {
  for (pn in c("price_variable", "intervention_strategy", "outcome_summary", "cost_summary")) {
    val <- get(pn)
    if (!is.character(val) || length(val) != 1 || nchar(val) == 0)
      stop(pn, " must be a non-empty string", call. = FALSE)
  }
  model$vbp <- list(price_variable = price_variable, intervention_strategy = intervention_strategy,
                    outcome_summary = outcome_summary, cost_summary = cost_summary)
  model
}

#' Remove VBP Configuration
#'
#' Remove the value-based pricing (VBP) configuration from the model.
#'
#' @param model An oq_model object
#'
#' @return The modified model object
#' @export
remove_vbp <- function(model) { model$vbp <- NULL; model }

#' Set PSA Configuration
#'
#' Configure probabilistic sensitivity analysis (PSA) parameters on the model
#' so they can be serialized and used as defaults by \code{run_psa()}.
#'
#' @param model An oq_model object
#' @param n_sim Number of PSA simulations to run (positive integer)
#' @param seed Random seed for reproducibility (NULL or single numeric value)
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   set_psa(n_sim = 1000, seed = 42)
#' @export
set_psa <- function(model, n_sim, seed = NULL) {
  if (!is.numeric(n_sim) || length(n_sim) != 1 || is.na(n_sim) || n_sim < 1 || n_sim != as.integer(n_sim))
    stop("n_sim must be a positive integer", call. = FALSE)
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed)))
    stop("seed must be NULL or a single numeric value", call. = FALSE)
  model$psa <- list(n_sim = as.integer(n_sim), seed = seed)
  model
}

# --- Lockfile (singleton setter) -----------------------------------------------

#' Set renv Lockfile on Model
#'
#' Attaches a parsed renv lockfile to the model. When the model is written to
#' JSON, the lockfile fields (\code{R}, \code{Packages}) are merged at the top
#' level, making the JSON file a valid renv lockfile.
#'
#' @param model An openqaly model object.
#' @param lockfile Either a list with \code{R} and \code{Packages} fields
#'   (as returned by \code{jsonlite::fromJSON} on an renv.lock file), or a
#'   single JSON string containing a valid renv lockfile.
#'
#' @return The model with \code{lockfile} field set.
#'
#' @export
set_lockfile <- function(model, lockfile) {
  if (is.character(lockfile) && length(lockfile) == 1) {
    if (!jsonlite::validate(lockfile)) {
      stop("lockfile must be valid JSON", call. = FALSE)
    }
    lockfile <- jsonlite::fromJSON(lockfile, simplifyVector = FALSE)
  }
  if (!is.list(lockfile) || is.null(lockfile$R) || is.null(lockfile$Packages)) {
    stop("lockfile must contain 'R' and 'Packages' fields", call. = FALSE)
  }
  model$lockfile <- lockfile
  model
}

#' Capture renv Lockfile from File
#'
#' Reads an existing \code{renv.lock} file and attaches it to the model.
#'
#' @param model An openqaly model object.
#' @param lockfile_path Path to an renv lockfile. Defaults to \code{"renv.lock"}.
#'
#' @return The model with \code{lockfile} field set.
#'
#' @export
capture_lockfile <- function(model, lockfile_path = "renv.lock") {
  if (!file.exists(lockfile_path)) {
    stop("Lockfile not found: ", lockfile_path, call. = FALSE)
  }
  json_string <- paste(readLines(lockfile_path, warn = FALSE), collapse = "\n")
  set_lockfile(model, json_string)
}

#' Snapshot Current Environment as Lockfile
#'
#' Captures the currently installed package versions and R version into a
#' lockfile structure and attaches it to the model. If \code{renv} is available,
#' uses \code{renv::lockfile_create()} for a complete lockfile. Otherwise,
#' builds the lockfile from \code{installed.packages()}.
#'
#' @param model An openqaly model object.
#' @param repos Character vector of repository URLs to record. Defaults to
#'   \code{getOption("repos")}.
#'
#' @return The model with \code{lockfile} field set.
#'
#' @export
snapshot_lockfile <- function(model, repos = getOption("repos")) {
  if (requireNamespace("renv", quietly = TRUE)) {
    # Use renv for a complete lockfile with hashes
    lockfile_path <- tempfile(fileext = ".lock")
    on.exit(unlink(lockfile_path), add = TRUE)
    renv::snapshot(lockfile = lockfile_path, prompt = FALSE, force = TRUE)
    json_string <- paste(readLines(lockfile_path, warn = FALSE), collapse = "\n")
    return(set_lockfile(model, json_string))
  }

  # Fallback: build lockfile manually from installed packages
  installed <- utils::installed.packages()

  # Build R section
  repo_list <- lapply(names(repos), function(nm) {
    list(Name = nm, URL = unname(repos[nm]))
  })
  r_section <- list(
    Version = paste0(R.version$major, ".", R.version$minor),
    Repositories = repo_list
  )

  # Build Packages section
  packages_section <- list()
  for (i in seq_len(nrow(installed))) {
    pkg_name <- installed[i, "Package"]
    packages_section[[pkg_name]] <- list(
      Package = pkg_name,
      Version = installed[i, "Version"],
      Source = "Repository",
      Repository = "CRAN"
    )
  }

  set_lockfile(model, list(R = r_section, Packages = packages_section))
}

#' Remove Lockfile from Model
#'
#' @param model An openqaly model object.
#' @return The model with \code{lockfile} field removed.
#'
#' @export
remove_lockfile <- function(model) { model$lockfile <- NULL; model }

# --- Edit/Remove Summary (via engine) ----------------------------------------

#' Edit a Summary in the Model
#'
#' Modify an existing summary's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the summary are updated (threshold analyses
#' and VBP config).
#'
#' @param model An oq_model object
#' @param name Character string identifying the summary to edit
#' @param values Optional new comma-separated string of value names
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param type Optional new type: "outcome" or "cost". Changing to "cost" will
#'   clear the WTP field.
#' @param wtp Optional new willingness-to-pay value. Cannot be set on cost summaries.
#' @param new_name Optional new name for the summary. If provided, the summary
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#' @export
edit_summary <- function(model, name, values, display_name, description, type, wtp, new_name) {
  match_idx <- find_in_tibble(model$summaries, "name", list(name), "Summary",
                               not_found_message = 'Summary "%s" not found in model.')
  updates <- list()
  if (!missing(values)) updates$values <- values
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(type)) {
    if (!type %in% c("outcome", "cost")) stop("Summary type must be 'outcome' or 'cost'", call. = FALSE)
    updates$type <- type
  }
  if (!missing(wtp)) updates$wtp <- wtp %||% NA_real_
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_tibble(model, .schema_summary, match_idx, updates, .callbacks_edit_summary)
}

#' Remove a Summary from the Model
#'
#' Remove an existing summary and optionally cascade-remove all downstream
#' references (threshold analyses and VBP config).
#'
#' @param model An oq_model object
#' @param name Character string identifying the summary to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the summary has
#'   downstream dependencies, an error of class \code{"summary_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the summary and all
#'   downstream references.
#'
#' @return The modified model object
#' @export
remove_summary <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- find_in_tibble(model$summaries, "name", list(name), "Summary",
                               not_found_message = 'Summary "%s" not found in model.')
  .cbs <- list(
    pre_remove = function(model, match_info, flags) {
      if (isTRUE(flags$error_on_dependencies)) {
        deps <- list()
        nm <- match_info$name
        if (length(model$threshold_analyses) > 0) {
          dep_thresh <- character(0)
          for (i in seq_along(model$threshold_analyses)) {
            cond <- model$threshold_analyses[[i]]$condition
            if (!is.null(cond) && (identical(cond$summary, nm) || identical(cond$health_summary, nm) || identical(cond$cost_summary, nm)))
              dep_thresh <- c(dep_thresh, model$threshold_analyses[[i]]$name %||% paste0("threshold_", i))
          }
          if (length(dep_thresh) > 0) deps$threshold_analyses <- dep_thresh
        }
        if (!is.null(model$vbp)) {
          vbp_refs <- character(0)
          if (identical(model$vbp$outcome_summary, nm)) vbp_refs <- c(vbp_refs, "outcome_summary")
          if (identical(model$vbp$cost_summary, nm)) vbp_refs <- c(vbp_refs, "cost_summary")
          if (length(vbp_refs) > 0) deps$vbp <- vbp_refs
        }
        deps <- Filter(function(x) length(x) > 0, deps)
        if (length(deps) > 0) {
          cn <- structure(class = c("summary_has_dependencies", "error", "condition"),
            list(message = sprintf('Cannot remove summary "%s": it has downstream dependencies.', nm), dependencies = deps))
          stop(cn)
        }
      }
      model
    },
    post_remove = function(model, old_name) {
      if (length(model$threshold_analyses) > 0) {
        keep <- vapply(model$threshold_analyses, function(a) {
          cond <- a$condition; if (is.null(cond)) return(TRUE)
          !(identical(cond$summary, old_name) || identical(cond$health_summary, old_name) || identical(cond$cost_summary, old_name))
        }, logical(1))
        model$threshold_analyses <- model$threshold_analyses[keep]
      }
      if (!is.null(model$vbp)) {
        if (identical(model$vbp$outcome_summary, old_name)) model$vbp$outcome_summary <- ""
        if (identical(model$vbp$cost_summary, old_name)) model$vbp$cost_summary <- ""
      }
      model
    }
  )
  entity_remove_tibble(model, .schema_summary, match_idx, .cbs,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

# --- Edit/Remove Variable (via engine) ---------------------------------------

#' Edit a Variable in the Model
#'
#' Modify an existing variable's properties. The variable is identified by its
#' name, strategy, and group combination. Only the fields that are explicitly
#' provided will be updated.
#'
#' Note: You cannot change a variable's strategy or group targeting. To change
#' targeting, remove the variable and add it again with the new targeting.
#'
#' @param model An oq_model object
#' @param name Character string identifying the variable to edit
#' @param formula An unquoted R expression for the variable calculation
#' @param display_name Optional display name
#' @param description Optional description
#' @param strategy Strategy association used for lookup (default: "")
#' @param group Group association used for lookup (default: "")
#' @param source Optional source information
#' @param sampling Optional sampling information
#' @param new_name Optional new name for the variable. If provided, the
#'   variable will be renamed. Must not collide with existing decision tree
#'   names.
#'
#' @return The modified model object
#' @export
edit_variable <- function(model, name, formula, display_name, description,
                           strategy = "", group = "", source, sampling, new_name) {
  match_idx <- find_in_tibble(model$variables, c("name", "strategy", "group"),
                              list(name, strategy, group), "Variable", na_safe = TRUE)
  updates <- list()
  if (!missing(formula)) updates$formula <- capture_nse(rlang::enquo(formula))
  if (!missing(sampling)) {
    sq <- rlang::enquo(sampling); se <- rlang::quo_get_expr(sq)
    updates$sampling <- if (is.character(se)) se else rlang::expr_text(se)
  }
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(source)) updates$source <- source
  if (!missing(new_name)) updates$new_name <- new_name
  .cbs <- list(
    pre_rename = function(model, match_info, new_name, updates) {
      if (!is.null(model$values) && new_name %in% model$values$name) {
        stop(sprintf("Name collision detected: the following names are used as both variables and values: %s. Please rename the variable(s) or value(s) to avoid this conflict.", new_name), call. = FALSE)
      }
      model
    },
    post_rename = function(model, old_name, new_name) {
      vs <- model$variables[model$variables$name == new_name, ]
      if (nrow(vs) > 1) {
        err <- validate_variable_display_names_for_builder(vs, new_name)
        if (err != "") stop(err, call. = FALSE)
      }
      model
    },
    post_edit = function(model, match_info, updates) {
      if (!is.null(updates$new_name)) return(model)
      if (!is.null(updates$display_name) || !is.null(updates$description)) {
        vs <- model$variables[model$variables$name == match_info$name, ]
        if (nrow(vs) > 1) {
          err <- validate_variable_display_names_for_builder(vs, match_info$name)
          if (err != "") stop(err, call. = FALSE)
        }
      }
      model
    }
  )
  entity_edit_tibble(model, .schema_variable, match_idx, updates, .cbs)
}

#' Remove a Variable from the Model
#'
#' Remove one or more variable definitions from the model. If only `name` is
#' given, all rows with that variable name are removed (all strategy/group
#' combinations). If `strategy` and/or `group` are specified, only matching
#' rows are removed.
#'
#' @param model An oq_model object
#' @param name Character string identifying the variable to remove
#' @param strategy Optional strategy to narrow removal. If NULL, all strategies
#'   are matched.
#' @param group Optional group to narrow removal. If NULL, all groups are
#'   matched.
#'
#' @return The modified model object
#' @export
remove_variable <- function(model, name, strategy = NULL, group = NULL) {
  match_mask <- model$variables$name == name
  if (!is.null(strategy)) match_mask <- match_mask & safe_field_eq(model$variables$strategy, strategy)
  if (!is.null(group)) match_mask <- match_mask & safe_field_eq(model$variables$group, group)
  match_idx <- which(match_mask)
  if (length(match_idx) == 0) stop(sprintf('Variable "%s" not found.', name), call. = FALSE)
  entity_remove_tibble(model, .schema_variable, match_idx)
}

# --- Edit/Remove DSA (via engine) --------------------------------------------

#' Edit a DSA Variable Specification
#'
#' Modify an existing DSA variable specification. Any parameter not provided
#' will retain its current value. Use \code{new_variable}, \code{new_strategy},
#' and \code{new_group} to re-key the specification.
#'
#' @param model An oq_model object
#' @param variable Character string naming the variable to edit
#' @param strategy Strategy targeting used to identify the specification (default: "")
#' @param group Group targeting used to identify the specification (default: "")
#' @param new_variable Optional new variable name
#' @param new_strategy Optional new strategy targeting
#' @param new_group Optional new group targeting
#' @param low Optional new low bound expression or numeric value
#' @param high Optional new high bound expression or numeric value
#' @param display_name Optional new display name
#' @param range_label Optional new range label
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("p_disease", 0.03) |>
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05) |>
#'   edit_dsa_variable("p_disease", low = bc * 0.5, high = bc * 1.5)
#' }
#' @export
edit_dsa_variable <- function(model, variable, strategy = "", group = "",
                               new_variable, new_strategy, new_group,
                               low, high, display_name, range_label) {
  item_idx <- find_in_list(model$dsa_parameters, c("type", "name", "strategy", "group"),
    list("variable", variable, as.character(strategy), as.character(group)), "DSA variable parameter")
  updates <- list()
  if (!missing(low)) updates$low <- capture_nse_formula(rlang::enquo(low))
  if (!missing(high)) updates$high <- capture_nse_formula(rlang::enquo(high))
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(range_label)) updates$range_label <- range_label
  if (!missing(new_variable) || !missing(new_strategy) || !missing(new_group)) {
    eff_var <- if (!missing(new_variable)) new_variable else variable
    eff_strat <- if (!missing(new_strategy)) as.character(new_strategy) else as.character(strategy)
    eff_grp <- if (!missing(new_group)) as.character(new_group) else as.character(group)
    if (!missing(new_variable)) validate_string(new_variable, "New variable name")
    validate_variable_targeting(model, eff_var, eff_strat, eff_grp, "DSA", "edit_dsa_variable")
    check_duplicate_list(model$dsa_parameters, c("type", "name", "strategy", "group"),
      list("variable", eff_var, eff_strat, eff_grp),
      list(entity_name = "DSA variable parameter", duplicate_action = "error"), exclude_idx = item_idx)
    updates$name <- eff_var; updates$strategy <- eff_strat; updates$group <- eff_grp
  }
  entity_edit_list(model, .schema_dsa_param, item_idx, updates)
}

#' Edit a DSA Setting Specification
#'
#' Modify an existing DSA setting specification. Any parameter not provided
#' will retain its current value. Use \code{new_setting} to rename the setting.
#'
#' @param model An oq_model object
#' @param setting Character string naming the setting to edit
#' @param new_setting Optional new setting name (must be a valid setting)
#' @param low Optional new low bound value
#' @param high Optional new high bound value
#' @param display_name Optional new display name
#' @param range_label Optional new range label
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(discount_cost = 3) |>
#'   add_dsa_setting("discount_cost", low = 0, high = 5) |>
#'   edit_dsa_setting("discount_cost", low = 1, high = 4)
#' }
#' @export
edit_dsa_setting <- function(model, setting, new_setting, low, high, display_name, range_label) {
  item_idx <- find_in_list(model$dsa_parameters, c("type", "name"),
    list("setting", setting), "DSA setting parameter")
  updates <- list()
  if (!missing(low)) updates$low <- low
  if (!missing(high)) updates$high <- high
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(range_label)) updates$range_label <- range_label
  if (!missing(new_setting)) {
    validate_string(new_setting, "New setting name")
    if (!(new_setting %in% .valid_settings))
      stop(sprintf("Invalid DSA setting name: '%s'. Valid settings: %s", new_setting, paste(.valid_settings, collapse = ", ")), call. = FALSE)
    check_duplicate_list(model$dsa_parameters, c("type", "name"), list("setting", new_setting),
      list(entity_name = "DSA setting", duplicate_action = "error"), exclude_idx = item_idx)
    param <- model$dsa_parameters[[item_idx]]
    if (is.null(updates$display_name) && identical(param$display_name, setting))
      updates$display_name <- new_setting
    updates$name <- new_setting
  }
  entity_edit_list(model, .schema_dsa_param, item_idx, updates)
}

#' Remove a DSA Variable Specification
#'
#' Remove an existing DSA variable specification from the model.
#'
#' @param model An oq_model object
#' @param variable Character string naming the variable to remove
#' @param strategy Strategy targeting used to identify the specification (default: "")
#' @param group Group targeting used to identify the specification (default: "")
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("p_disease", 0.03) |>
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05) |>
#'   remove_dsa_variable("p_disease")
#' }
#' @export
remove_dsa_variable <- function(model, variable, strategy = "", group = "") {
  validate_string(variable, "Variable name")
  item_idx <- find_in_list(model$dsa_parameters, c("type", "name", "strategy", "group"),
    list("variable", variable, as.character(strategy), as.character(group)), "DSA variable parameter")
  entity_remove_list(model, .schema_dsa_param, item_idx)
}

#' Remove a DSA Setting Specification
#'
#' Remove an existing DSA setting specification from the model.
#'
#' @param model An oq_model object
#' @param setting Character string naming the setting to remove
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(discount_cost = 3) |>
#'   add_dsa_setting("discount_cost", low = 0, high = 5) |>
#'   remove_dsa_setting("discount_cost")
#' }
#' @export
remove_dsa_setting <- function(model, setting) {
  validate_string(setting, "Setting name")
  item_idx <- find_in_list(model$dsa_parameters, c("type", "name"),
    list("setting", setting), "DSA setting parameter")
  entity_remove_list(model, .schema_dsa_setting, item_idx)
}

# --- Edit/Remove Scenario Overrides (via engine) -----------------------------

#' Edit a Scenario Variable Override
#'
#' Modify an existing variable override within a scenario. Any parameter not
#' provided will retain its current value.
#'
#' @param model An oq_model object
#' @param scenario Name of the scenario containing the override
#' @param variable Name of the variable to edit
#' @param strategy Strategy targeting used to identify the override (default: "")
#' @param group Group targeting used to identify the override (default: "")
#' @param new_variable Optional new variable name
#' @param new_strategy Optional new strategy targeting
#' @param new_group Optional new group targeting
#' @param value Optional new value expression (uses NSE)
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("efficacy", 0.8) |>
#'   add_scenario("Optimistic") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   edit_scenario_variable("Optimistic", "efficacy", value = 0.99)
#' }
#' @export
edit_scenario_variable <- function(model, scenario, variable, strategy = "", group = "",
                                    new_variable, new_strategy, new_group, value) {
  validate_string(scenario, "Scenario name")
  parent_idx <- find_in_list(model$scenarios, "name", list(scenario), "Scenario")
  child_list <- model$scenarios[[parent_idx]]$variable_overrides
  child_idx <- find_in_list(child_list, c("name", "strategy", "group"),
    list(variable, as.character(strategy), as.character(group)), "Scenario variable override")
  updates <- list()
  if (!missing(value)) {
    vq <- rlang::enquo(value); ve <- rlang::quo_get_expr(vq)
    updates$value <- if (is.numeric(ve)) ve else as.oq_formula(rlang::expr_text(ve))
  }
  if (!missing(new_variable) || !missing(new_strategy) || !missing(new_group)) {
    eff_var <- if (!missing(new_variable)) new_variable else variable
    eff_strat <- if (!missing(new_strategy)) as.character(new_strategy) else as.character(strategy)
    eff_grp <- if (!missing(new_group)) as.character(new_group) else as.character(group)
    validate_variable_targeting(model, eff_var, eff_strat, eff_grp, "Scenario", "edit_scenario_variable")
    check_duplicate_list(child_list, c("name", "strategy", "group"), list(eff_var, eff_strat, eff_grp),
      list(entity_name = "Scenario variable override", duplicate_action = "error"), exclude_idx = child_idx)
    updates$name <- eff_var; updates$strategy <- eff_strat; updates$group <- eff_grp
  }
  entity_edit_nested(model, .schema_scenario, parent_idx, .schema_scenario_variable_override, child_idx, updates)
}

#' Edit a Scenario Setting Override
#'
#' Modify an existing setting override within a scenario.
#'
#' @param model An oq_model object
#' @param scenario Name of the scenario containing the override
#' @param setting Name of the setting to edit
#' @param new_setting Optional new setting name (must be a valid setting)
#' @param value Optional new value
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20) |>
#'   add_scenario("Extended") |>
#'   add_scenario_setting("Extended", "timeframe", 30) |>
#'   edit_scenario_setting("Extended", "timeframe", value = 40)
#' }
#' @export
edit_scenario_setting <- function(model, scenario, setting, new_setting, value) {
  validate_string(scenario, "Scenario name")
  parent_idx <- find_in_list(model$scenarios, "name", list(scenario), "Scenario")
  child_list <- model$scenarios[[parent_idx]]$setting_overrides
  child_idx <- find_in_list(child_list, "name", list(setting), "Scenario setting override")
  updates <- list()
  if (!missing(value)) updates$value <- value
  if (!missing(new_setting)) {
    validate_string(new_setting, "New setting name")
    if (!(new_setting %in% .valid_settings)) stop(sprintf("Invalid scenario setting name: '%s'. Valid settings: %s", new_setting, paste(.valid_settings, collapse = ", ")), call. = FALSE)
    check_duplicate_list(child_list, "name", list(new_setting),
      list(entity_name = "Scenario setting", duplicate_action = "error"), exclude_idx = child_idx)
    updates$name <- new_setting
  }
  entity_edit_nested(model, .schema_scenario, parent_idx, .schema_scenario_setting_override, child_idx, updates)
}

#' Remove a Scenario Variable Override
#'
#' Remove an existing variable override from a scenario.
#'
#' @param model An oq_model object
#' @param scenario Name of the scenario containing the override
#' @param variable Name of the variable to remove
#' @param strategy Strategy targeting used to identify the override (default: "")
#' @param group Group targeting used to identify the override (default: "")
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("efficacy", 0.8) |>
#'   add_scenario("Optimistic") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   remove_scenario_variable("Optimistic", "efficacy")
#' }
#' @export
remove_scenario_variable <- function(model, scenario, variable, strategy = "", group = "") {
  validate_string(scenario, "Scenario name")
  parent_idx <- find_in_list(model$scenarios, "name", list(scenario), "Scenario")
  child_list <- model$scenarios[[parent_idx]]$variable_overrides
  child_idx <- find_in_list(child_list, c("name", "strategy", "group"),
    list(variable, as.character(strategy), as.character(group)), "Scenario variable override")
  entity_remove_nested(model, .schema_scenario, parent_idx, .schema_scenario_variable_override, child_idx)
}

#' Remove a Scenario Setting Override
#'
#' Remove an existing setting override from a scenario.
#'
#' @param model An oq_model object
#' @param scenario Name of the scenario containing the override
#' @param setting Name of the setting to remove
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20) |>
#'   add_scenario("Extended") |>
#'   add_scenario_setting("Extended", "timeframe", 30) |>
#'   remove_scenario_setting("Extended", "timeframe")
#' }
#' @export
remove_scenario_setting <- function(model, scenario, setting) {
  validate_string(scenario, "Scenario name")
  parent_idx <- find_in_list(model$scenarios, "name", list(scenario), "Scenario")
  child_list <- model$scenarios[[parent_idx]]$setting_overrides
  child_idx <- find_in_list(child_list, "name", list(setting), "Scenario setting override")
  entity_remove_nested(model, .schema_scenario, parent_idx, .schema_scenario_setting_override, child_idx)
}

# --- TWSA (via engine) -------------------------------------------------------

#' Add a Two-Way Sensitivity Analysis to Model
#'
#' Define a named two-way sensitivity analysis (2WSA) that varies two parameters
#' simultaneously across a grid of values. Each 2WSA must have exactly two
#' variable or setting specifications added via `add_twsa_variable()` or
#' `add_twsa_setting()`.
#'
#' @param model An oq_model object
#' @param name Unique name for the 2WSA analysis (e.g., "Cost vs Efficacy")
#' @param description Optional description explaining what this analysis explores
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Cost vs Efficacy") |>
#'   add_twsa_variable("Cost vs Efficacy", "cost_tx",
#'     type = "range", min = 1000, max = 3000, steps = 5) |>
#'   add_twsa_variable("Cost vs Efficacy", "efficacy",
#'     type = "radius", radius = 0.1, steps = 5)
#' }
#' @export
add_twsa <- function(model, name, description = NULL) {
  validate_string(name, "TWSA name")
  new_item <- list(name = name, description = description %||% name, enabled = TRUE, parameters = list())
  entity_add_list(model, .schema_twsa, new_item)
}

#' Edit a TWSA Analysis
#'
#' Modify an existing TWSA analysis name and/or description.
#'
#' @param model An oq_model object
#' @param name Character string naming the TWSA to edit
#' @param new_name Optional new name
#' @param description Optional new description
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Cost vs Efficacy") |>
#'   edit_twsa("Cost vs Efficacy", new_name = "Price vs Effect")
#' }
#' @export
edit_twsa <- function(model, name, new_name, description) {
  item_idx <- find_in_list(model$twsa_analyses, "name", list(name), "TWSA analysis")
  updates <- list()
  if (!missing(description)) updates$description <- description
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_list(model, .schema_twsa, item_idx, updates)
}

#' Remove a TWSA Analysis
#'
#' Remove an existing TWSA analysis from the model.
#'
#' @param model An oq_model object
#' @param name Character string naming the TWSA to remove
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Cost vs Efficacy") |>
#'   remove_twsa("Cost vs Efficacy")
#' }
#' @export
remove_twsa <- function(model, name) {
  item_idx <- find_in_list(model$twsa_analyses, "name", list(name), "TWSA analysis")
  entity_remove_list(model, .schema_twsa, item_idx)
}

#' Add Variable to Two-Way Sensitivity Analysis
#'
#' Add a variable to vary in a two-way sensitivity analysis. Each TWSA must have
#' exactly two parameters (variables or settings). Values can be specified using
#' three different range types:
#'
#' - `type = "range"`: Vary from `min` to `max` in `steps` evenly-spaced values
#' - `type = "radius"`: Vary from `bc - radius` to `bc + radius` where `bc` is
#'   the base case value, creating `steps * 2 + 1` values centered on the base case
#' - `type = "custom"`: Use explicit values provided as a numeric vector
#'
#' @param model An oq_model object
#' @param twsa_name Name of the TWSA analysis to add the variable to (must exist)
#' @param variable Name of the variable to vary (must exist in model$variables)
#' @param type Type of range specification: "range", "radius", or "custom"
#' @param min For type="range": minimum value (can use NSE expressions)
#' @param max For type="range": maximum value (can use NSE expressions)
#' @param radius For type="radius": distance from base case (can use NSE)
#' @param steps For type="range" or "radius": number of steps
#' @param values For type="custom": numeric vector of values to use
#' @param strategy Optional strategy name to limit the variable to specific strategy
#' @param group Optional group name to limit the variable to specific group
#' @param display_name Optional display name for plots and tables
#' @param include_base_case Logical. If TRUE (default), ensures the base case
#'   value is included in the parameter grid. If the base case value is already
#'   present in the values, no duplicate is added.
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("cost_tx", 1000) |>
#'   add_variable("efficacy", 0.8) |>
#'   add_twsa("Cost vs Efficacy") |>
#'   # Using range type
#'   add_twsa_variable("Cost vs Efficacy", "cost_tx",
#'     type = "range", min = 500, max = 1500, steps = 5) |>
#'   # Using radius type (varies around base case)
#'   add_twsa_variable("Cost vs Efficacy", "efficacy",
#'     type = "radius", radius = 0.1, steps = 3)
#'
#' # Using custom values
#' model <- define_model("markov") |>
#'   add_variable("discount_rate", 0.03) |>
#'   add_twsa("Discount Analysis") |>
#'   add_twsa_variable("Discount Analysis", "discount_rate",
#'     type = "custom", values = c(0, 0.015, 0.03, 0.05))
#' }
#' @export
add_twsa_variable <- function(model, twsa_name, variable, type,
                               min = NULL, max = NULL, radius = NULL,
                               steps = NULL, values = NULL,
                               strategy = "", group = "",
                               display_name = NULL, include_base_case = TRUE) {
  validate_string(twsa_name, "TWSA name")
  validate_string(variable, "Variable name")
  validate_variable_targeting(model, variable, strategy, group, "TWSA", "add_twsa_variable")
  type <- match.arg(type, c("range", "radius", "custom"))
  parent_idx <- find_in_list(model$twsa_analyses, "name", list(twsa_name), "TWSA analysis")
  if (length(model$twsa_analyses[[parent_idx]]$parameters) >= 2)
    stop(sprintf("TWSA '%s' already has 2 parameters.", twsa_name), call. = FALSE)
  # Capture NSE BEFORE null checks (user may pass expressions like base_cost)
  min_q <- rlang::enquo(min)
  max_q <- rlang::enquo(max)
  radius_q <- rlang::enquo(radius)
  values_q <- rlang::enquo(values)
  has_min <- !rlang::quo_is_null(min_q)
  has_max <- !rlang::quo_is_null(max_q)
  has_radius <- !rlang::quo_is_null(radius_q)
  has_values <- !rlang::quo_is_null(values_q)
  if (type == "range" && (!has_min || !has_max || is.null(steps))) stop("For type='range', min, max, and steps are required", call. = FALSE)
  if (type == "radius" && (!has_radius || is.null(steps))) stop("For type='radius', radius and steps are required", call. = FALSE)
  if (type == "custom" && !has_values) stop("For type='custom', values is required.", call. = FALSE)
  min_f <- if (has_min) capture_nse_formula(min_q) else NULL
  max_f <- if (has_max) capture_nse_formula(max_q) else NULL
  radius_f <- if (has_radius) capture_nse_formula(radius_q) else NULL
  values_f <- if (has_values) capture_nse_formula(values_q) else NULL
  new_item <- list(param_type = "variable", name = variable, type = type,
    min = min_f, max = max_f, radius = radius_f, steps = steps, values = values_f,
    strategy = as.character(strategy), group = as.character(group),
    display_name = display_name, include_base_case = include_base_case)
  entity_add_nested(model, .schema_twsa, twsa_name, .schema_twsa_parameter, new_item)
}

#' Add Setting to Two-Way Sensitivity Analysis
#'
#' Add a model setting to vary in a two-way sensitivity analysis. Each TWSA must
#' have exactly two parameters (variables or settings). Settings include parameters
#' like discount rates, timeframe, and other global model configuration values.
#'
#' Values can be specified using three different range types:
#'
#' - `type = "range"`: Vary from `min` to `max` in `steps` evenly-spaced values
#' - `type = "radius"`: Vary from `base - radius` to `base + radius` where `base`
#'   is the current setting value, creating `steps * 2 + 1` values
#' - `type = "custom"`: Use explicit values provided as a numeric vector
#'
#' @param model An oq_model object
#' @param twsa_name Name of the TWSA analysis to add the setting to (must exist)
#' @param setting Name of the setting to vary (e.g., "discount_cost", "timeframe")
#' @param type Type of range specification: "range", "radius", or "custom"
#' @param min For type="range": minimum value
#' @param max For type="range": maximum value
#' @param radius For type="radius": distance from base value
#' @param steps For type="range" or "radius": number of steps
#' @param values For type="custom": numeric vector of values to use
#' @param display_name Optional display name for plots and tables
#' @param include_base_case Logical. If TRUE (default), ensures the base case
#'   value is included in the parameter grid. If the base case value is already
#'   present in the values, no duplicate is added.
#'
#' @return The modified model object
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_twsa("Time vs Discount") |>
#'   add_twsa_setting("Time vs Discount", "timeframe",
#'     type = "range", min = 10, max = 30, steps = 5) |>
#'   add_twsa_setting("Time vs Discount", "discount_cost",
#'     type = "custom", values = c(0, 1.5, 3, 5))
#' }
#' @export
add_twsa_setting <- function(model, twsa_name, setting, type,
                              min = NULL, max = NULL, radius = NULL,
                              steps = NULL, values = NULL,
                              display_name = NULL, include_base_case = TRUE) {
  validate_string(twsa_name, "TWSA name")
  validate_string(setting, "Setting name")
  type <- match.arg(type, c("range", "radius", "custom"))
  parent_idx <- find_in_list(model$twsa_analyses, "name", list(twsa_name), "TWSA analysis")
  if (length(model$twsa_analyses[[parent_idx]]$parameters) >= 2)
    stop(sprintf("TWSA '%s' already has 2 parameters.", twsa_name), call. = FALSE)
  if (type == "range" && (is.null(min) || is.null(max) || is.null(steps))) stop("For type='range', min, max, and steps are required", call. = FALSE)
  if (type == "radius" && (is.null(radius) || is.null(steps))) stop("For type='radius', radius and steps are required", call. = FALSE)
  if (type == "custom" && is.null(values)) stop("For type='custom', values is required.", call. = FALSE)
  new_item <- list(param_type = "setting", name = setting, type = type,
    min = min, max = max, radius = radius, steps = steps, values = values,
    display_name = display_name %||% setting, include_base_case = include_base_case)
  entity_add_nested(model, .schema_twsa, twsa_name, .schema_twsa_setting_parameter, new_item)
}

# --- Override (via engine) ---------------------------------------------------

#' Add an Override Control to a Category
#'
#' Define a UI override control that allows users to modify a model variable
#' or setting at runtime. When the model is rendered in a Shiny UI, these
#' controls will be displayed to the user.
#'
#' @param model An oq_model object
#' @param category Character string naming the category to add to (must exist)
#' @param title Character string for the display name
#' @param name Character string for the variable or setting name to override
#' @param type Character string: "variable" or "setting"
#' @param input_type Character string: "numeric", "slider", "dropdown", "formula", or "timeframe"
#' @param expression Default/initial override value as a string or unquoted expression (uses NSE)
#' @param description Optional description text
#' @param strategy Optional strategy name (for variable overrides only)
#' @param group Optional group name (for variable overrides only)
#' @param general Logical indicating if this is a system override (default: FALSE)
#' @param min Numeric minimum (for numeric/slider input types)
#' @param max Numeric maximum (for numeric/slider input types)
#' @param step_size Numeric step size (for slider input type)
#' @param options List of dropdown options (for dropdown input type).
#'   Each option should be created with \code{\link{override_option}}.
#'
#' @return The modified model object
#' @export
add_override <- function(model, category, title, name, type = "variable",
                          input_type = "numeric", expression, description = NULL,
                          strategy = "", group = "", general = FALSE,
                          min = NULL, max = NULL, step_size = NULL, options = NULL) {
  validate_string(title, "Override title")
  validate_string(name, "Override name")
  if (!type %in% c("variable", "setting")) stop("Override type must be 'variable' or 'setting'.", call. = FALSE)
  if (!input_type %in% .valid_input_types) stop("Override input_type must be one of: numeric, slider, dropdown, formula, timeframe", call. = FALSE)
  eq <- rlang::enquo(expression); ev <- rlang::quo_get_expr(eq)
  expression_str <- if (is.numeric(ev)) as.character(ev) else if (is.character(ev)) ev else rlang::expr_text(ev)
  if (type == "setting") {
    if (strategy != "" || group != "") stop("Strategy and group cannot be specified for setting overrides", call. = FALSE)
    if (!(name %in% .valid_settings)) stop(sprintf("Invalid override setting name: '%s'. Valid settings: %s", name, paste(.valid_settings, collapse = ", ")), call. = FALSE)
  } else {
    validate_variable_targeting(model, name, strategy, group, "override", "add_override")
  }
  if (!is.null(min) && !is.null(max) && min >= max) stop(sprintf("Override min (%s) must be less than max (%s)", min, max), call. = FALSE)
  if (!is.null(step_size) && step_size <= 0) stop("Override step_size must be greater than 0", call. = FALSE)
  input_config <- build_input_config(input_type, min, max, step_size, options)
  new_item <- list(title = title, name = name, type = type, input_type = input_type,
    overridden_expression = expression_str, description = description %||% "",
    strategy = as.character(strategy), group = as.character(group),
    general = general, input_config = input_config)
    # Find category with custom error
  parent_container <- model$override_categories
  if (is.null(parent_container) || length(parent_container) == 0) {
    stop(sprintf("Override category '%s' not found. Use add_override_category() first.", category), call. = FALSE)
  }
  found <- FALSE
  for (i in seq_along(parent_container)) {
    if (identical(parent_container[[i]]$name, category)) { found <- TRUE; break }
  }
  if (!found) {
    stop(sprintf("Override category '%s' not found. Use add_override_category() first.", category), call. = FALSE)
  }
  entity_add_nested(model, .schema_override_category, category, .schema_override, new_item)
}

#' Edit an Override Category
#'
#' Modify the name or general flag of an existing override category.
#'
#' @param model An oq_model object
#' @param name Character string identifying the category to edit
#' @param new_name Optional new name for the category
#' @param general Optional logical for the general flag
#'
#' @return The modified model object
#' @export
edit_override_category <- function(model, name, new_name, general) {
  item_idx <- find_in_list(model$override_categories, "name", list(name), "Override category")
  updates <- list()
  if (!missing(general)) updates$general <- general
  if (!missing(new_name)) {
    validate_string(new_name, "New category name")
    check_duplicate_list(model$override_categories, "name", list(new_name),
      list(entity_name = "Override category", duplicate_action = "error"), exclude_idx = item_idx)
    updates$new_name <- new_name
  }
  entity_edit_list(model, .schema_override_category, item_idx, updates)
}

#' Remove an Override Category
#'
#' Remove an existing override category and all its overrides.
#'
#' @param model An oq_model object
#' @param name Character string identifying the category to remove
#'
#' @return The modified model object
#' @export
remove_override_category <- function(model, name) {
  item_idx <- find_in_list(model$override_categories, "name", list(name), "Override category")
  entity_remove_list(model, .schema_override_category, item_idx)
}

#' Edit an Override Control
#'
#' Modify an existing override control within a category. The override is
#' identified by its composite key of category, type, name, strategy, and group.
#'
#' @param model An oq_model object
#' @param category Character string naming the category containing the override
#' @param type Character string: "variable" or "setting" (current type)
#' @param name Character string for the variable or setting name (current name)
#' @param strategy Current strategy targeting (default: "")
#' @param group Current group targeting (default: "")
#' @param title Optional new title
#' @param description Optional new description
#' @param general Optional new general flag
#'
#' @param expression Optional new expression (supports NSE)
#' @param new_type Optional new type
#' @param new_name Optional new name
#' @param new_strategy Optional new strategy
#' @param new_group Optional new group
#' @param input_type Optional new input type
#' @param min Optional new minimum value
#' @param max Optional new maximum value
#' @param step_size Optional new step size
#' @param options Optional new dropdown options
#'
#' @return The modified model object
#' @export
edit_override <- function(model, category, type, name, strategy = "", group = "",
                           title, description, general, expression,
                           new_type, new_name, new_strategy, new_group,
                           input_type, min, max, step_size, options) {
  parent_idx <- find_in_list(model$override_categories, "name", list(category), "Override category")
  child_list <- model$override_categories[[parent_idx]]$overrides
  child_idx <- find_in_list(child_list, c("type", "name", "strategy", "group"),
    list(type, name, as.character(strategy), as.character(group)), "Override")
  updates <- list()
  if (!missing(title)) updates$title <- title
  if (!missing(description)) updates$description <- description
  if (!missing(general)) updates$general <- general
  if (!missing(expression)) {
    eq <- rlang::enquo(expression); ev <- rlang::quo_get_expr(eq)
    updates$overridden_expression <- if (is.numeric(ev)) as.character(ev) else if (is.character(ev)) ev else rlang::expr_text(ev)
  }
  if (!missing(new_type) || !missing(new_name) || !missing(new_strategy) || !missing(new_group)) {
    et <- if (!missing(new_type)) new_type else type
    en <- if (!missing(new_name)) new_name else name
    es <- if (!missing(new_strategy)) as.character(new_strategy) else as.character(strategy)
    eg <- if (!missing(new_group)) as.character(new_group) else as.character(group)
    check_duplicate_list(child_list, c("type", "name", "strategy", "group"), list(et, en, es, eg),
      list(entity_name = "Override", duplicate_action = "error"), exclude_idx = child_idx)
    if (et == "setting") {
      if (es != "" || eg != "") stop("Strategy and group cannot be specified for setting overrides", call. = FALSE)
      if (!(en %in% .valid_settings)) stop(sprintf("Invalid setting name '%s'.", en), call. = FALSE)
    } else { validate_variable_targeting(model, en, es, eg, "override", "edit_override") }
    updates$type <- et; updates$name <- en; updates$strategy <- es; updates$group <- eg
  }
  ovr <- child_list[[child_idx]]
  if (!missing(input_type)) {
    if (!input_type %in% .valid_input_types) stop("Override input_type must be one of: numeric, slider, dropdown, formula, timeframe", call. = FALSE)
    if (input_type != ovr$input_type) {
      updates$input_config <- build_input_config(input_type,
        if (!missing(min)) min else NULL, if (!missing(max)) max else NULL,
        if (!missing(step_size)) step_size else NULL, if (!missing(options)) options else NULL)
      updates$input_type <- input_type
    } else {
      cfg <- ovr$input_config
      if (!missing(min)) cfg$min <- min; if (!missing(max)) cfg$max <- max
      if (!missing(step_size)) cfg$step_size <- step_size; if (!missing(options)) cfg$options <- options
      if (!missing(min) || !missing(max) || !missing(step_size) || !missing(options)) updates$input_config <- cfg
    }
  } else {
    cfg <- ovr$input_config
    if (!missing(min)) cfg$min <- min; if (!missing(max)) cfg$max <- max
    if (!missing(step_size)) cfg$step_size <- step_size; if (!missing(options)) cfg$options <- options
    if (!missing(min) || !missing(max) || !missing(step_size) || !missing(options)) updates$input_config <- cfg
  }
  eff_cfg <- updates$input_config %||% ovr$input_config
  if (!is.null(eff_cfg$min) && !is.null(eff_cfg$max) && eff_cfg$min >= eff_cfg$max) stop(sprintf("Override min (%s) must be less than max (%s)", eff_cfg$min, eff_cfg$max), call. = FALSE)
  if (!is.null(eff_cfg$step_size) && eff_cfg$step_size <= 0) stop("Override step_size must be greater than 0", call. = FALSE)
  entity_edit_nested(model, .schema_override_category, parent_idx, .schema_override, child_idx, updates)
}

#' Remove an Override Control
#'
#' Remove an existing override control from a category.
#'
#' @param model An oq_model object
#' @param category Character string naming the category containing the override
#' @param type Character string: "variable" or "setting"
#' @param name Character string for the variable or setting name
#' @param strategy Strategy targeting (default: "")
#' @param group Group targeting (default: "")
#'
#' @return The modified model object
#' @export
remove_override <- function(model, category, type, name, strategy = "", group = "") {
  parent_idx <- find_in_list(model$override_categories, "name", list(category), "Override category")
  child_list <- model$override_categories[[parent_idx]]$overrides
  child_idx <- find_in_list(child_list, c("type", "name", "strategy", "group"),
    list(type, name, as.character(strategy), as.character(group)), "Override")
  entity_remove_nested(model, .schema_override_category, parent_idx, .schema_override, child_idx)
}

# --- Table / Script (named list storage) -------------------------------------

#' Add Table to Model
#'
#' Add a data table to the model.
#'
#' @param model An oq_model object
#' @param name Character string for the table name
#' @param data A data frame containing the table data
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_table("costs", data.frame(state = c("A", "B"), cost = c(100, 200)))
#' @export
add_table <- function(model, name, data, description = NULL) {
  validate_string(name, "Table name")
  check_collision(model, name, list(container = "trees", field = "name", unique = TRUE,
    message = 'Name collision: "%s" is already used as a decision tree name.'))
  if (is.data.frame(model$values) && nrow(model$values) > 0 && name %in% model$values$name)
    stop(sprintf("Name collision: '%s' is used as both a table and a value name.", name))
  if (!is.null(model$tables[[name]])) stop(sprintf('Table "%s" already exists.', name), call. = FALSE)
  model$tables[[name]] <- list(data = data, description = description)
  model
}

#' Edit a Table in the Model
#'
#' Modify an existing table's data, description, or name. Only the fields that
#' are explicitly provided will be updated.
#'
#' @param model An oq_model object
#' @param name Character string identifying the table to edit
#' @param data A data frame to replace the table data with
#' @param description Optional new description
#' @param new_name Optional new name for the table. If provided, the table
#'   will be renamed. Must not collide with existing decision tree or value
#'   names.
#'
#' @return The modified model object
#' @export
edit_table <- function(model, name, data, description, new_name) {
  if (is.null(model$tables[[name]])) stop(sprintf('Table "%s" not found.', name), call. = FALSE)
  if (!missing(data)) model$tables[[name]]$data <- data
  if (!missing(description)) model$tables[[name]]$description <- description
  if (!missing(new_name)) {
    check_collision(model, new_name, list(container = "trees", field = "name", unique = TRUE,
      message = 'Name collision: "%s" is already used as a decision tree name.'))
    if (is.data.frame(model$values) && nrow(model$values) > 0 && new_name %in% model$values$name)
      stop(sprintf("Name collision: '%s' is used as both a table and a value.", new_name))
    idx <- which(names(model$tables) == name)
    names(model$tables)[idx] <- new_name
  }
  model
}

#' Remove a Table from the Model
#'
#' Remove an existing table from the model by name.
#'
#' @param model An oq_model object
#' @param name Character string identifying the table to remove
#'
#' @return The modified model object
#' @export
remove_table <- function(model, name) {
  if (is.null(model$tables[[name]])) stop(sprintf('Table "%s" not found.', name), call. = FALSE)
  model$tables[[name]] <- NULL; model
}

#' Add Script to Model
#'
#' Add an R script to the model.
#'
#' @param model An oq_model object
#' @param name Character string for the script name
#' @param code Character string containing the R code
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @examples
#' model <- define_model("markov") |>
#'   add_script("preprocess", "# Data preprocessing\nlibrary(dplyr)")
#' @export
add_script <- function(model, name, code, description = NULL) {
  validate_string(name, "Script name")
  model$scripts[[name]] <- list(code = code, description = description); model
}

#' Edit a Script in the Model
#'
#' Modify an existing script's code, description, or name. Only the fields that
#' are explicitly provided will be updated.
#'
#' @param model An oq_model object
#' @param name Character string identifying the script to edit
#' @param code Character string containing the new R code
#' @param description Optional new description
#' @param new_name Optional new name for the script. If provided, the script
#'   will be renamed.
#'
#' @return The modified model object
#' @export
edit_script <- function(model, name, code, description, new_name) {
  if (is.null(model$scripts[[name]])) stop(sprintf('Script "%s" not found.', name), call. = FALSE)
  if (!missing(code)) model$scripts[[name]]$code <- code
  if (!missing(description)) model$scripts[[name]]$description <- description
  if (!missing(new_name)) { idx <- which(names(model$scripts) == name); names(model$scripts)[idx] <- new_name }
  model
}

#' Remove a Script from the Model
#'
#' Remove an existing script from the model by name.
#'
#' @param model An oq_model object
#' @param name Character string identifying the script to remove
#'
#' @return The modified model object
#' @export
remove_script <- function(model, name) {
  if (is.null(model$scripts[[name]])) stop(sprintf('Script "%s" not found.', name), call. = FALSE)
  model$scripts[[name]] <- NULL; model
}

# --- Utility: override_option ------------------------------------------------

#' Create a Dropdown Option for Override Controls
#'
#' Helper function to create a dropdown option for use with
#' \code{\link{add_override}} when \code{input_type = "dropdown"}.
#'
#' @param label Display text for the option
#' @param value Actual value when selected
#' @param is_base_case Whether this is the default option (default: FALSE)
#'
#' @return A list representing a dropdown option
#' @export
override_option <- function(label, value, is_base_case = FALSE) {
  if (!is.character(label) || length(label) != 1 || nchar(trimws(label)) == 0)
    stop("Dropdown option label must be a non-empty character string", call. = FALSE)
  list(label = label, value = as.character(value), is_base_case = as.logical(is_base_case))
}


# --- Legacy functions (not yet converted to schema-based approach) -----------

#' Set Model Documentation
#'
#' Set a markdown documentation string on the model describing its purpose,
#' assumptions, and other relevant details.
#'
#' @param model An oq_model object
#' @param text A character string containing the documentation (typically markdown)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov")
#' model <- set_documentation(model, "# My Model\n\nA cost-effectiveness model.")
set_documentation <- function(model, text) {
  if (!is.character(text) || length(text) != 1) {
    stop("text must be a single character string", call. = FALSE)
  }
  model$documentation <- text
  model
}

#' Get Documentation
#'
#' Retrieve the markdown documentation string from the model.
#'
#' @param model An oq_model object
#' @return The documentation string, or NULL if not set
#' @export
get_documentation <- function(model) {
  model$documentation
}

#' Preview Documentation
#'
#' Render model documentation as HTML with math support and open in browser.
#' Uses KaTeX for rendering \\(...\\) and \\[...\\] math delimiters.
#'
#' @param model An oq_model object
#' @return Invisibly returns the path to the generated HTML file
#' @export
preview_documentation <- function(model) {
  if (!requireNamespace("commonmark", quietly = TRUE)) {
    stop("Package 'commonmark' is required. Install with: install.packages('commonmark')",
         call. = FALSE)
  }
  doc <- get_documentation(model)
  if (is.null(doc) || !nzchar(doc)) {
    stop("Model has no documentation set.", call. = FALSE)
  }
  body_html <- commonmark::markdown_html(doc)
  html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.38/dist/katex.min.css">',
    '<script src="https://cdn.jsdelivr.net/npm/katex@0.16.38/dist/katex.min.js"></script>',
    '<script src="https://cdn.jsdelivr.net/npm/katex@0.16.38/dist/contrib/auto-render.min.js"></script>',
    '<style>body { max-width: 800px; margin: 40px auto; padding: 0 20px; ',
    'font-family: sans-serif; line-height: 1.6; }</style>',
    '</head><body>', body_html,
    '<script>renderMathInElement(document.body, {',
    '  delimiters: [',
    '    {left: "\\\\[", right: "\\\\]", display: true},',
    '    {left: "\\\\(", right: "\\\\)", display: false}',
    '  ]',
    '});</script></body></html>'
  )
  tmp <- tempfile(fileext = ".html")
  writeLines(html, tmp)
  utils::browseURL(tmp)
  invisible(tmp)
}

#' Add a Tree Node
#'
#' Add a node to a decision tree in the model. Creates the trees tibble if it
#' doesn't exist yet.
#'
#' @param model An oq_model object
#' @param tree_name Character string for the tree name
#' @param node Character string for the node name
#' @param parent Character string for the parent node name (NA for root)
#' @param formula An unquoted R expression for the node probability
#' @param tags Optional character string for node tags
#'
#' @return The modified model object
#'
#' @export
add_tree_node <- function(model, tree_name, node, parent = NA, formula, tags = NA) {
  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_row <- fast_tibble(
    name = tree_name,
    node = node,
    parent = if (is.na(parent)) NA_character_ else as.character(parent),
    formula = formula_str,
    tags = if (is.na(tags)) NA_character_ else as.character(tags)
  )

  if (is.null(model$trees)) {
    model$trees <- new_row
  } else {
    model$trees <- bind_rows(model$trees, new_row)
  }

  validate_tree_name_collisions(tree_name, model)

  model
}

#' Set Decision Tree Configuration
#'
#' Configure a decision tree to run before the main model. The tree produces
#' one-time payoffs and a duration that offsets downstream discounting.
#'
#' @param model An oq_model object
#' @param tree_name Character string referencing a tree in model$trees
#' @param duration An unquoted R expression for the duration (can reference variables for PSA)
#' @param duration_unit Character string for the time unit ("days", "weeks", "months", "years")
#'
#' @return The modified model object
#'
#' @export
set_decision_tree <- function(model, tree_name, duration, duration_unit = "days") {
  # Validate duration_unit
  valid_units <- c("days", "weeks", "months", "years")
  duration_unit <- match.arg(tolower(duration_unit), valid_units)

  # Capture the duration expression using NSE
  duration_quo <- enquo(duration)
  duration_expr <- quo_get_expr(duration_quo)
  duration_str <- expr_text(duration_expr)
  if (is.character(duration_expr) && length(duration_expr) == 1) {
    duration_str <- duration_expr
  }

  model$decision_tree <- list(
    tree_name = tree_name,
    duration = duration_str,
    duration_unit = duration_unit
  )

  model
}

#' Remove Decision Tree Configuration
#'
#' Remove the decision tree configuration from the model.
#'
#' @param model An oq_model object
#'
#' @return The modified model object
#'
#' @export
remove_decision_tree <- function(model) {
  model$decision_tree <- NULL
  model
}

#' Edit a Decision Tree Node
#'
#' Edit an existing tree node identified by tree_name and node.
#'
#' @param model An oq_model object
#' @param tree_name Character string for the tree name
#' @param node Character string for the node name
#' @param formula An unquoted R expression for the node probability
#' @param tags Optional character string for node tags
#' @param parent Character string for the parent node name
#' @param new_node_name Character string to rename the node
#' @param new_tree_name Character string to rename the entire tree
#'
#' @return The modified model object
#'
#' @export
edit_tree_node <- function(model, tree_name, node, formula, tags, parent,
                           new_node_name, new_tree_name) {
  if (is.null(model$trees)) {
    stop("No trees exist in this model.", call. = FALSE)
  }

  match_idx <- which(model$trees$name == tree_name & model$trees$node == node)
  if (length(match_idx) == 0) {
    stop(sprintf('Tree node "%s" in tree "%s" not found in model.', node, tree_name),
         call. = FALSE)
  }

  if (!missing(formula)) {
    formula_quo <- enquo(formula)
    formula_expr <- quo_get_expr(formula_quo)
    formula_str <- expr_text(formula_expr)
    if (is.character(formula_expr) && length(formula_expr) == 1) {
      formula_str <- formula_expr
    }
    model$trees$formula[match_idx] <- formula_str
  }

  if (!missing(tags)) {
    model$trees$tags[match_idx] <- if (is.na(tags)) NA_character_ else as.character(tags)
  }

  if (!missing(parent)) {
    model$trees$parent[match_idx] <- if (is.na(parent)) NA_character_ else as.character(parent)
  }

  if (!missing(new_node_name)) {
    conflict <- which(model$trees$name == tree_name & model$trees$node == new_node_name)
    if (length(conflict) > 0) {
      stop(sprintf('Node "%s" already exists in tree "%s".', new_node_name, tree_name),
           call. = FALSE)
    }
    # Cascade: update parent references for children in same tree
    child_mask <- model$trees$name == tree_name & model$trees$parent %in% node
    model$trees$parent[child_mask] <- new_node_name
    model$trees$node[match_idx] <- new_node_name
  }

  if (!missing(new_tree_name)) {
    validate_tree_name_collisions(new_tree_name, model)
    tree_mask <- model$trees$name == tree_name
    model$trees$name[tree_mask] <- new_tree_name
    if (!is.null(model$decision_tree) && identical(model$decision_tree$tree_name, tree_name)) {
      model$decision_tree$tree_name <- new_tree_name
    }
  }

  model
}

#' Remove a Decision Tree Node
#'
#' Remove a tree node and all its descendants from the model.
#'
#' @param model An oq_model object
#' @param tree_name Character string for the tree name
#' @param node Character string for the node name
#'
#' @return The modified model object
#'
#' @export
remove_tree_node <- function(model, tree_name, node) {
  if (is.null(model$trees)) {
    stop("No trees exist in this model.", call. = FALSE)
  }

  match_idx <- which(model$trees$name == tree_name & model$trees$node == node)
  if (length(match_idx) == 0) {
    stop(sprintf('Tree node "%s" in tree "%s" not found in model.', node, tree_name),
         call. = FALSE)
  }

  # BFS to find all descendants
  nodes_to_remove <- node
  queue <- node
  while (length(queue) > 0) {
    children <- model$trees$node[model$trees$name == tree_name & model$trees$parent %in% queue]
    queue <- setdiff(children, nodes_to_remove)
    nodes_to_remove <- c(nodes_to_remove, queue)
  }

  model$trees <- model$trees[!(model$trees$name == tree_name & model$trees$node %in% nodes_to_remove), ]

  # If no nodes remain for this tree and it's the config tree, clear config
  remaining_in_tree <- sum(model$trees$name == tree_name)
  if (remaining_in_tree == 0 && !is.null(model$decision_tree) &&
      identical(model$decision_tree$tree_name, tree_name)) {
    model$decision_tree <- NULL
  }

  # If no trees remain at all, set to NULL
  if (nrow(model$trees) == 0) {
    model$trees <- NULL
  }

  model
}

#' Remove an Entire Decision Tree
#'
#' Remove all nodes of a tree from the model.
#'
#' @param model An oq_model object
#' @param tree_name Character string for the tree name
#'
#' @return The modified model object
#'
#' @export
remove_tree <- function(model, tree_name) {
  if (is.null(model$trees) || !any(model$trees$name == tree_name)) {
    stop(sprintf('Tree "%s" not found in model.', tree_name), call. = FALSE)
  }

  model$trees <- model$trees[model$trees$name != tree_name, ]

  if (!is.null(model$decision_tree) && identical(model$decision_tree$tree_name, tree_name)) {
    model$decision_tree <- NULL
  }

  if (nrow(model$trees) == 0) {
    model$trees <- NULL
  }

  model
}

#' Edit Decision Tree Configuration
#'
#' Edit the decision tree configuration on the model.
#'
#' @param model An oq_model object
#' @param tree_name Character string referencing a tree in model$trees
#' @param duration An unquoted R expression for the duration
#' @param duration_unit Character string for the time unit
#'
#' @return The modified model object
#'
#' @export
edit_decision_tree <- function(model, tree_name, duration, duration_unit) {
  if (is.null(model$decision_tree)) {
    stop("No decision tree configuration exists. Use set_decision_tree() first.",
         call. = FALSE)
  }

  if (!missing(tree_name)) {
    model$decision_tree$tree_name <- tree_name
  }

  if (!missing(duration)) {
    duration_quo <- enquo(duration)
    duration_expr <- quo_get_expr(duration_quo)
    duration_str <- expr_text(duration_expr)
    if (is.character(duration_expr) && length(duration_expr) == 1) {
      duration_str <- duration_expr
    }
    model$decision_tree$duration <- duration_str
  }

  if (!missing(duration_unit)) {
    valid_units <- c("days", "weeks", "months", "years")
    duration_unit <- match.arg(tolower(duration_unit), valid_units)
    model$decision_tree$duration_unit <- duration_unit
  }

  model
}

#' Update Summary Value References
#'
#' Internal helper that updates value name references in summary CSV strings.
#' Used by \code{edit_value} (rename) and \code{remove_value} (cascade removal).
#'
#' @param summaries The summaries tibble from a model
#' @param old_name The value name to find
#' @param new_name The replacement name (NULL to remove the reference)
#'
#' @return Updated summaries tibble
#' @export
update_summary_value_refs <- function(summaries, old_name, new_name = NULL) {
  if (is.null(summaries) || !is.data.frame(summaries) || nrow(summaries) == 0) {
    return(summaries)
  }
  for (i in seq_len(nrow(summaries))) {
    tokens <- trimws(strsplit(summaries$values[i], ",")[[1]])
    if (old_name %in% tokens) {
      if (is.null(new_name)) {
        tokens <- tokens[tokens != old_name]
      } else {
        if (new_name %in% tokens) {
          # new_name already present, just remove old
          tokens <- tokens[tokens != old_name]
        } else {
          tokens[tokens == old_name] <- new_name
        }
      }
      summaries$values[i] <- paste(tokens, collapse = ", ")
    }
  }
  summaries
}

#' Convert Model to JSON String
#'
#' Convert a openqaly model object to a JSON string representation.
#'
#' @param model A openqaly model object
#'
#' @return A JSON string
#'
#' @export
as_json <- function(model) {
  # Use existing function
  write_model_json(model)
}

#' Convert Model to R Code String
#'
#' Convert a openqaly model object to executable R code.
#'
#' @param model A openqaly model object
#'
#' @return A character vector of R code lines
#'
#' @export
as_r_code <- function(model) {
  # This will call the code generation function from model_codegen.R
  model_to_r_code(model)
}

#' Validate Variable Targeting for Sensitivity Analyses
#'
#' Checks if a variable requires strategy and/or group specification when used
#' in sensitivity analyses. If the variable is defined with specific strategies
#' or groups, this validation ensures the user explicitly specifies which
#' strategy/group to target.
#'
#' @param model An oq_model object
#' @param variable Character string naming the variable
#' @param strategy Strategy specification (empty string if not provided)
#' @param group Group specification (empty string if not provided)
#' @param analysis_type Character string for error message (e.g., "DSA", "TWSA")
#' @param add_function_name Character string for example in error message
#'
#' @return Invisible NULL if valid, stops with error if invalid
#' @export
validate_variable_targeting <- function(model, variable, strategy, group,
                                         analysis_type, add_function_name) {
  matching_vars <- model$variables[model$variables$name == variable, ]
  if (nrow(matching_vars) == 0) return(invisible(NULL))

  # Check group-specific
  non_na_groups <- matching_vars$group[!is.na(matching_vars$group)]
  has_group_specific <- length(non_na_groups) > 0 && any(non_na_groups != "")
  if (has_group_specific && group == "") {
    defined_groups <- unique(non_na_groups[non_na_groups != ""])
    stop(sprintf(
      paste0(
        "Variable '%s' is defined for specific group(s): %s\n",
        "You must specify which group to vary using the 'group' parameter.\n",
        "Example: %s(\"%s\", ..., group = \"%s\")"
      ),
      variable,
      paste(defined_groups, collapse = ", "),
      add_function_name,
      variable,
      defined_groups[1]
    ), call. = FALSE)
  }

  # Check strategy-specific
  non_na_strategies <- matching_vars$strategy[!is.na(matching_vars$strategy)]
  has_strategy_specific <- length(non_na_strategies) > 0 && any(non_na_strategies != "")
  if (has_strategy_specific && strategy == "") {
    defined_strategies <- unique(non_na_strategies[non_na_strategies != ""])
    stop(sprintf(
      paste0(
        "Variable '%s' is defined for specific strategy(ies): %s\n",
        "You must specify which strategy to vary using the 'strategy' parameter.\n",
        "Example: %s(\"%s\", ..., strategy = \"%s\")"
      ),
      variable,
      paste(defined_strategies, collapse = ", "),
      add_function_name,
      variable,
      defined_strategies[1]
    ), call. = FALSE)
  }

  invisible(NULL)
}

#' Print DSA Parameters
#'
#' Print method for dsa_parameters objects
#'
#' @param x A dsa_parameters object (list)
#' @param ... Additional arguments (unused)
#'
#' @export
print.dsa_parameters <- function(x, ...) {
  if (length(x) == 0) {
    cat("No DSA parameters defined\n")
    return(invisible(x))
  }

  cat("DSA Parameters (", length(x), "):\n", sep = "")
  for (i in seq_along(x)) {
    param <- x[[i]]
    display <- if (param$type == "variable") {
      # Variables use their own display_name from the model
      param$name
    } else {
      # Settings have display_name in the parameter
      param$display_name
    }
    cat(sprintf("  %d. [%s] %s\n", i, param$type, display))
  }
  invisible(x)
}

#' Print Scenarios
#'
#' Print method for displaying scenarios defined in a model
#'
#' @param model An oq_model object with scenarios
#'
#' @return Invisible model
#' @export
print_scenarios <- function(model) {
  if (is.null(model$scenarios) || length(model$scenarios) == 0) {
    cat("No scenarios defined\n")
    return(invisible(model))
  }

  cat("Scenarios (", length(model$scenarios), "):\n", sep = "")
  for (i in seq_along(model$scenarios)) {
    scenario <- model$scenarios[[i]]
    cat(sprintf("  %d. %s\n", i, scenario$name))
    if (scenario$description != scenario$name) {
      cat(sprintf("     Description: %s\n", scenario$description))
    }
    n_vars <- length(scenario$variable_overrides)
    n_settings <- length(scenario$setting_overrides)
    cat(sprintf("     Overrides: %d variable(s), %d setting(s)\n", n_vars, n_settings))
  }
  invisible(model)
}

#' Edit a TWSA Variable Parameter
#'
#' Modify an existing variable parameter within a TWSA analysis.
#'
#' @param model An oq_model object
#' @param twsa_name Name of the TWSA analysis
#' @param variable Name of the variable to edit
#' @param strategy Strategy targeting used to identify the parameter (default: "")
#' @param group Group targeting used to identify the parameter (default: "")
#' @param new_variable Optional new variable name
#' @param new_strategy Optional new strategy targeting
#' @param new_group Optional new group targeting
#' @param type Optional new range type ("range", "radius", or "custom")
#' @param min Optional new minimum (for range type, uses NSE)
#' @param max Optional new maximum (for range type, uses NSE)
#' @param radius Optional new radius (for radius type, uses NSE)
#' @param steps Optional new number of steps
#' @param values Optional new custom values (uses NSE)
#' @param display_name Optional new display name
#' @param include_base_case Optional logical for including base case in grid
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Test") |>
#'   add_twsa_variable("Test", "cost", type = "range",
#'     min = 500, max = 1500, steps = 5) |>
#'   edit_twsa_variable("Test", "cost", steps = 10)
#' }
edit_twsa_variable <- function(model, twsa_name, variable,
                                strategy = "", group = "",
                                new_variable, new_strategy, new_group,
                                type, min, max, radius, steps,
                                values, display_name, include_base_case) {

  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string", call. = FALSE)
  }

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found.", twsa_name), call. = FALSE)
  }

  # Find parameter index
  params <- model$twsa_analyses[[twsa_idx]]$parameters
  param_idx <- 0L
  if (length(params) > 0) {
    for (i in seq_along(params)) {
      p <- params[[i]]
      if (p$param_type == "variable" &&
          p$name == variable &&
          p$strategy == as.character(strategy) &&
          p$group == as.character(group)) {
        param_idx <- i
        break
      }
    }
  }
  if (param_idx == 0L) {
    stop(sprintf(
      "TWSA variable '%s'%s%s not found in TWSA '%s'.",
      variable,
      if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
      if (group != "") paste0(" (group: ", group, ")") else "",
      twsa_name
    ), call. = FALSE)
  }

  param <- params[[param_idx]]

  # Type change handling
  if (!missing(type)) {
    type <- match.arg(type, c("range", "radius", "custom"))
    if (type != param$type) {
      # Type is changing - require all new-type params
      if (type == "range") {
        if (missing(min) || missing(max) || missing(steps)) {
          stop("When changing to type='range', min, max, and steps are required",
               call. = FALSE)
        }
      } else if (type == "radius") {
        if (missing(radius) || missing(steps)) {
          stop("When changing to type='radius', radius and steps are required",
               call. = FALSE)
        }
      } else if (type == "custom") {
        if (missing(values)) {
          stop("When changing to type='custom', values is required",
               call. = FALSE)
        }
      }
      # NULL out fields not relevant to new type
      param$min <- NULL
      param$max <- NULL
      param$radius <- NULL
      param$steps <- NULL
      param$values <- NULL
      param$type <- type
    }
  }

  # Update NSE fields
  if (!missing(min)) {
    min_quo <- enquo(min)
    min_expr <- quo_get_expr(min_quo)
    if (is.numeric(min_expr)) {
      param$min <- as.oq_formula(as.character(min_expr))
    } else {
      param$min <- as.oq_formula(expr_text(min_expr))
    }
  }
  if (!missing(max)) {
    max_quo <- enquo(max)
    max_expr <- quo_get_expr(max_quo)
    if (is.numeric(max_expr)) {
      param$max <- as.oq_formula(as.character(max_expr))
    } else {
      param$max <- as.oq_formula(expr_text(max_expr))
    }
  }
  if (!missing(radius)) {
    radius_quo <- enquo(radius)
    radius_expr <- quo_get_expr(radius_quo)
    if (is.numeric(radius_expr)) {
      param$radius <- as.oq_formula(as.character(radius_expr))
    } else {
      param$radius <- as.oq_formula(expr_text(radius_expr))
    }
  }
  if (!missing(values)) {
    values_quo <- enquo(values)
    values_expr <- quo_get_expr(values_quo)
    param$values <- as.oq_formula(expr_text(values_expr))
  }

  # Update simple fields
  if (!missing(steps)) param$steps <- steps
  if (!missing(display_name)) param$display_name <- display_name
  if (!missing(include_base_case)) param$include_base_case <- include_base_case

  # Re-keying
  if (!missing(new_variable) || !missing(new_strategy) || !missing(new_group)) {
    eff_variable <- if (!missing(new_variable)) new_variable else param$name
    eff_strategy <- if (!missing(new_strategy)) as.character(new_strategy) else param$strategy
    eff_group <- if (!missing(new_group)) as.character(new_group) else param$group

    if (!missing(new_variable)) {
      if (!is.character(new_variable) || length(new_variable) != 1 || nchar(trimws(new_variable)) == 0) {
        stop("new_variable must be a non-empty character string", call. = FALSE)
      }
    }

    validate_variable_targeting(model, eff_variable, eff_strategy, eff_group,
                                "TWSA", "edit_twsa_variable")

    # Duplicate check excluding self
    if (length(params) > 1) {
      for (i in seq_along(params)) {
        if (i == param_idx) next
        other <- params[[i]]
        if (other$param_type == "variable" && other$name == eff_variable &&
            other$strategy == eff_strategy && other$group == eff_group) {
          stop(sprintf(
            "A TWSA variable parameter for '%s'%s%s already exists in TWSA '%s'",
            eff_variable,
            if (eff_strategy != "") paste0(" (strategy: ", eff_strategy, ")") else "",
            if (eff_group != "") paste0(" (group: ", eff_group, ")") else "",
            twsa_name
          ), call. = FALSE)
        }
      }
    }

    param$name <- eff_variable
    param$strategy <- eff_strategy
    param$group <- eff_group
  }

  model$twsa_analyses[[twsa_idx]]$parameters[[param_idx]] <- param
  model
}

#' Edit a TWSA Setting Parameter
#'
#' Modify an existing setting parameter within a TWSA analysis.
#'
#' @param model An oq_model object
#' @param twsa_name Name of the TWSA analysis
#' @param setting Name of the setting to edit
#' @param new_setting Optional new setting name (must be valid)
#' @param type Optional new range type ("range", "radius", or "custom")
#' @param min Optional new minimum (for range type)
#' @param max Optional new maximum (for range type)
#' @param radius Optional new radius (for radius type)
#' @param steps Optional new number of steps
#' @param values Optional new custom values
#' @param display_name Optional new display name
#' @param include_base_case Optional logical for including base case in grid
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Test") |>
#'   add_twsa_setting("Test", "discount_cost", type = "range",
#'     min = 0, max = 5, steps = 3) |>
#'   edit_twsa_setting("Test", "discount_cost", steps = 10)
#' }
edit_twsa_setting <- function(model, twsa_name, setting,
                               new_setting, type, min, max, radius,
                               steps, values, display_name,
                               include_base_case) {

  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string", call. = FALSE)
  }

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found.", twsa_name), call. = FALSE)
  }

  # Find parameter index
  params <- model$twsa_analyses[[twsa_idx]]$parameters
  param_idx <- 0L
  if (length(params) > 0) {
    for (i in seq_along(params)) {
      p <- params[[i]]
      if (p$param_type == "setting" && p$name == setting) {
        param_idx <- i
        break
      }
    }
  }
  if (param_idx == 0L) {
    stop(sprintf("TWSA setting '%s' not found in TWSA '%s'.",
                 setting, twsa_name), call. = FALSE)
  }

  param <- params[[param_idx]]

  # Type change handling
  if (!missing(type)) {
    type <- match.arg(type, c("range", "radius", "custom"))
    if (type != param$type) {
      if (type == "range") {
        if (missing(min) || missing(max) || missing(steps)) {
          stop("When changing to type='range', min, max, and steps are required",
               call. = FALSE)
        }
      } else if (type == "radius") {
        if (missing(radius) || missing(steps)) {
          stop("When changing to type='radius', radius and steps are required",
               call. = FALSE)
        }
      } else if (type == "custom") {
        if (missing(values)) {
          stop("When changing to type='custom', values is required",
               call. = FALSE)
        }
      }
      param$min <- NULL
      param$max <- NULL
      param$radius <- NULL
      param$steps <- NULL
      param$values <- NULL
      param$type <- type
    }
  }

  # Update value fields (literal for settings, no NSE)
  if (!missing(min)) param$min <- min
  if (!missing(max)) param$max <- max
  if (!missing(radius)) param$radius <- radius
  if (!missing(steps)) param$steps <- steps
  if (!missing(values)) param$values <- values
  if (!missing(display_name)) param$display_name <- display_name
  if (!missing(include_base_case)) param$include_base_case <- include_base_case

  # Re-keying
  if (!missing(new_setting)) {
    if (!is.character(new_setting) || length(new_setting) != 1) {
      stop("new_setting must be a single character string", call. = FALSE)
    }

    valid_settings <- .valid_settings
    if (!(new_setting %in% valid_settings)) {
      stop(sprintf(
        "Invalid TWSA setting name: '%s'. Valid settings: %s",
        new_setting, paste(valid_settings, collapse = ", ")
      ), call. = FALSE)
    }

    # Duplicate check excluding self
    if (length(params) > 1) {
      for (i in seq_along(params)) {
        if (i == param_idx) next
        other <- params[[i]]
        if (other$param_type == "setting" && other$name == new_setting) {
          stop(sprintf(
            "A TWSA setting parameter for '%s' already exists in TWSA '%s'",
            new_setting, twsa_name
          ), call. = FALSE)
        }
      }
    }

    # Auto-update display_name if it matched old setting name
    if (!missing(display_name)) {
      # User explicitly set display_name, don't auto-update
    } else if (!is.null(param$display_name) && param$display_name == setting) {
      param$display_name <- new_setting
    }

    param$name <- new_setting
  }

  model$twsa_analyses[[twsa_idx]]$parameters[[param_idx]] <- param
  model
}

#' Remove a TWSA Variable Parameter
#'
#' Remove an existing variable parameter from a TWSA analysis.
#'
#' @param model An oq_model object
#' @param twsa_name Name of the TWSA analysis
#' @param variable Name of the variable to remove
#' @param strategy Strategy targeting used to identify the parameter (default: "")
#' @param group Group targeting used to identify the parameter (default: "")
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Test") |>
#'   add_twsa_variable("Test", "cost", type = "range",
#'     min = 500, max = 1500, steps = 5) |>
#'   remove_twsa_variable("Test", "cost")
#' }
remove_twsa_variable <- function(model, twsa_name, variable,
                                  strategy = "", group = "") {

  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string", call. = FALSE)
  }

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found.", twsa_name), call. = FALSE)
  }

  # Find parameter index
  params <- model$twsa_analyses[[twsa_idx]]$parameters
  param_idx <- 0L
  if (length(params) > 0) {
    for (i in seq_along(params)) {
      p <- params[[i]]
      if (p$param_type == "variable" &&
          p$name == variable &&
          p$strategy == as.character(strategy) &&
          p$group == as.character(group)) {
        param_idx <- i
        break
      }
    }
  }
  if (param_idx == 0L) {
    stop(sprintf(
      "TWSA variable '%s'%s%s not found in TWSA '%s'.",
      variable,
      if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
      if (group != "") paste0(" (group: ", group, ")") else "",
      twsa_name
    ), call. = FALSE)
  }

  model$twsa_analyses[[twsa_idx]]$parameters <-
    model$twsa_analyses[[twsa_idx]]$parameters[-param_idx]
  model
}

#' Remove a TWSA Setting Parameter
#'
#' Remove an existing setting parameter from a TWSA analysis.
#'
#' @param model An oq_model object
#' @param twsa_name Name of the TWSA analysis
#' @param setting Name of the setting to remove
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Test") |>
#'   add_twsa_setting("Test", "discount_cost", type = "range",
#'     min = 0, max = 5, steps = 3) |>
#'   remove_twsa_setting("Test", "discount_cost")
#' }
remove_twsa_setting <- function(model, twsa_name, setting) {

  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string", call. = FALSE)
  }

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found.", twsa_name), call. = FALSE)
  }

  # Find parameter index
  params <- model$twsa_analyses[[twsa_idx]]$parameters
  param_idx <- 0L
  if (length(params) > 0) {
    for (i in seq_along(params)) {
      if (params[[i]]$param_type == "setting" && params[[i]]$name == setting) {
        param_idx <- i
        break
      }
    }
  }
  if (param_idx == 0L) {
    stop(sprintf("TWSA setting '%s' not found in TWSA '%s'.",
                 setting, twsa_name), call. = FALSE)
  }

  model$twsa_analyses[[twsa_idx]]$parameters <-
    model$twsa_analyses[[twsa_idx]]$parameters[-param_idx]
  model
}

#' Print TWSA Analyses
#'
#' Print method for displaying two-way sensitivity analyses defined in a model
#'
#' @param model An oq_model object with TWSA analyses
#'
#' @return Invisible model
#' @export
print_twsa <- function(model) {
  if (is.null(model$twsa_analyses) || length(model$twsa_analyses) == 0) {
    cat("No TWSA analyses defined\n")
    return(invisible(model))
  }

  cat("Two-Way Sensitivity Analyses (", length(model$twsa_analyses), "):\n", sep = "")
  for (i in seq_along(model$twsa_analyses)) {
    twsa <- model$twsa_analyses[[i]]
    cat(sprintf("  %d. %s\n", i, twsa$name))
    if (twsa$description != twsa$name) {
      cat(sprintf("     Description: %s\n", twsa$description))
    }
    n_params <- length(twsa$parameters)
    cat(sprintf("     Parameters: %d/2\n", n_params))
    for (j in seq_along(twsa$parameters)) {
      param <- twsa$parameters[[j]]
      param_display <- param$display_name %||% param$name
      cat(sprintf("       - [%s] %s (%s)\n", param$param_type, param_display, param$type))
    }
  }
  invisible(model)
}

#' Print Override Categories Summary
#'
#' Displays a formatted summary of override categories and their overrides.
#'
#' @param model A model object with override_categories
#' @return Invisible model
#' @export
print_override_categories <- function(model) {
  if (is.null(model$override_categories) || length(model$override_categories) == 0) {
    return(invisible(model))
  }

  # Count total overrides
  total_overrides <- sum(sapply(model$override_categories, function(c) length(c$overrides)))
  if (total_overrides == 0) {
    return(invisible(model))
  }

  cat(sprintf("\n  [!] Active overrides (%d):\n", total_overrides))
  for (cat_item in model$override_categories) {
    if (length(cat_item$overrides) == 0) next
    cat(sprintf("    %s:\n", cat_item$name))
    for (override in cat_item$overrides) {
      config_str <- switch(override$input_type,
        "numeric" = sprintf("[numeric: %s-%s]",
          override$input_config$min, override$input_config$max),
        "slider" = sprintf("[slider: %s-%s]",
          override$input_config$min, override$input_config$max),
        "dropdown" = "[dropdown]",
        "formula" = "[formula]",
        "timeframe" = "[timeframe]"
      )
      cat(sprintf("      - %s (%s): %s %s\n",
        override$title, override$name,
        override$overridden_expression, config_str))
    }
  }
  invisible(model)
}

#' Create Threshold Condition for Trace
#'
#' @param state State name to target in the trace
#' @param time Numeric time value at which to read the trace
#' @param time_unit Time unit for the time parameter: "cycle", "year", "month", "week", or "day"
#' @param type Whether to use absolute or difference: "absolute" or "difference"
#' @param strategy Strategy name for absolute type
#' @param referent Referent strategy for difference type
#' @param comparator Comparator strategy for difference type
#' @param target_value Target trace value to find threshold for
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_trace <- function(
  state,
  time,
  time_unit = c("cycle", "year", "month", "week", "day"),
  type = c("absolute", "difference"),
  strategy = NULL,
  referent = NULL,
  comparator = NULL,
  target_value,
  group = ""
) {
  time_unit <- match.arg(time_unit)
  type <- match.arg(type)
  list(output = "trace", state = state, time = time, time_unit = time_unit,
       type = type, strategy = strategy, referent = referent,
       comparator = comparator, target_value = target_value, group = group)
}

#' Validate Threshold Condition Fields
#' @param condition A threshold condition list
#' @export
validate_threshold_condition <- function(condition) {
  output <- condition$output

  if (output %in% c("outcomes", "costs")) {
    has_summary <- !is.null(condition$summary) && condition$summary != ""
    has_value <- !is.null(condition$value) && condition$value != ""
    if (!has_summary && !has_value) {
      stop(sprintf("Threshold condition for '%s' must specify either 'summary' or 'value'", output), call. = FALSE)
    }
    if (has_summary && has_value) {
      stop(sprintf("Threshold condition for '%s' must specify either 'summary' or 'value', not both", output), call. = FALSE)
    }

    type <- condition$type
    if (is.null(type) || !type %in% c("absolute", "difference")) {
      stop("Threshold condition 'type' must be 'absolute' or 'difference'", call. = FALSE)
    }
    if (type == "absolute") {
      if (is.null(condition$strategy) || condition$strategy == "") {
        stop("Threshold condition with type='absolute' requires 'strategy'", call. = FALSE)
      }
    } else {
      if (is.null(condition$referent) || condition$referent == "") {
        stop("Threshold condition with type='difference' requires 'referent'", call. = FALSE)
      }
      if (is.null(condition$comparator) || condition$comparator == "") {
        stop("Threshold condition with type='difference' requires 'comparator'", call. = FALSE)
      }
    }

    if (is.null(condition$target_value)) {
      stop(sprintf("Threshold condition for '%s' requires 'target_value'", output), call. = FALSE)
    }

  } else if (output == "nmb") {
    required <- c("health_summary", "cost_summary", "referent", "comparator")
    missing <- required[!sapply(required, function(f) !is.null(condition[[f]]) && condition[[f]] != "")]
    if (length(missing) > 0) {
      stop(sprintf("Threshold condition for 'nmb' requires: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }

  } else if (output == "ce") {
    required <- c("health_summary", "cost_summary", "referent", "comparator")
    missing <- required[!sapply(required, function(f) !is.null(condition[[f]]) && condition[[f]] != "")]
    if (length(missing) > 0) {
      stop(sprintf("Threshold condition for 'ce' requires: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }
  } else if (output == "trace") {
    if (is.null(condition$state) || condition$state == "") {
      stop("Threshold condition for 'trace' requires 'state'", call. = FALSE)
    }
    if (is.null(condition$time) || !is.numeric(condition$time)) {
      stop("Threshold condition for 'trace' requires numeric 'time'", call. = FALSE)
    }
    valid_time_units <- c("cycle", "year", "month", "week", "day")
    if (is.null(condition$time_unit) || !condition$time_unit %in% valid_time_units) {
      stop(sprintf("Threshold condition 'time_unit' must be one of: %s",
                   paste(valid_time_units, collapse = ", ")), call. = FALSE)
    }
    if (is.null(condition$target_value)) {
      stop("Threshold condition for 'trace' requires 'target_value'", call. = FALSE)
    }
    type <- condition$type
    if (is.null(type) || !type %in% c("absolute", "difference")) {
      stop("Threshold condition 'type' must be 'absolute' or 'difference'", call. = FALSE)
    }
    if (type == "absolute") {
      if (is.null(condition$strategy) || condition$strategy == "") {
        stop("Threshold condition with type='absolute' requires 'strategy'", call. = FALSE)
      }
    } else {
      if (is.null(condition$referent) || condition$referent == "") {
        stop("Threshold condition with type='difference' requires 'referent'", call. = FALSE)
      }
      if (is.null(condition$comparator) || condition$comparator == "") {
        stop("Threshold condition with type='difference' requires 'comparator'", call. = FALSE)
      }
    }
  }

  # Validate group field if provided
  if (!is.null(condition$group) && !identical(condition$group, "")) {
    if (!is.character(condition$group) || length(condition$group) != 1) {
      stop("Threshold condition 'group' must be a single character string", call. = FALSE)
    }
  }
}
