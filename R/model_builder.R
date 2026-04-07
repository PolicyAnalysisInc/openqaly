# ============================================================================
# Schema-Based Model Builder (v2)
# ============================================================================
# Parallel implementation of model_builder.R using the entity engine.
# All functions have "2" suffix to coexist with the original.
# ============================================================================

# --- Model Initialization (copied from original — not CRUD) -----------------

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
      discount_timing = "start", discount_method = "by_cycle",
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

#' @export
remove_strategy <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- find_in_tibble(model$strategies, "name", list(name), "Strategy")
  entity_remove_tibble(model, .schema_strategy, match_idx,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

# --- Group -------------------------------------------------------------------

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

#' @export
remove_group <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- find_in_tibble(model$groups, "name", list(name), "Group")
  entity_remove_tibble(model, .schema_group, match_idx,
                       flags = list(error_on_dependencies = error_on_dependencies))
}

# --- Summary -----------------------------------------------------------------

#' @export
add_summary <- function(model, name, values, display_name = NULL,
                         description = NULL, type = "outcome", wtp = NULL) {
  validate_string(name, "Summary name")
  validate_string(values, "Summary values")
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

#' @export
edit_scenario <- function(model, name, description, enabled, new_name) {
  item_idx <- find_in_list(model$scenarios, "name", list(name), "Scenario")
  updates <- list()
  if (!missing(description)) updates$description <- description
  if (!missing(enabled)) updates$enabled <- enabled
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_list(model, .schema_scenario, item_idx, updates)
}

#' @export
remove_scenario <- function(model, name) {
  item_idx <- find_in_list(model$scenarios, "name", list(name), "Scenario")
  entity_remove_list(model, .schema_scenario, item_idx)
}

# --- Scenario Variable Override (nested) ------------------------------------

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

#' @export
add_scenario_setting <- function(model, scenario, setting, value) {
  validate_string(scenario, "Scenario name")
  validate_string(setting, "Setting name")
  new_item <- list(name = setting, value = value)
  entity_add_nested(model, .schema_scenario, scenario,
                    .schema_scenario_setting_override, new_item)
}

# --- Multivariate Sampling --------------------------------------------------

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

#' @export
remove_multivariate_sampling <- function(model, name) {
  item_idx <- find_in_list(model$multivariate_sampling, "name", list(name),
                           "Multivariate sampling specification")
  entity_remove_list(model, .schema_multivariate_sampling, item_idx)
}

# --- DSA Variable -----------------------------------------------------------

#' @export
add_dsa_variable <- function(model, variable, low, high,
                              strategy = "", group = "",
                              display_name = NULL, range_label = NULL) {
  validate_string(variable, "variable")
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

#' @export
add_dsa_setting <- function(model, setting, low, high,
                             display_name = NULL, range_label = NULL) {
  validate_string(setting, "setting")
  new_item <- list(
    type = "setting", name = setting,
    low = low, high = high,
    display_name = display_name %||% setting,
    range_label = range_label
  )
  entity_add_list(model, .schema_dsa_setting, new_item)
}

# --- Threshold Analysis -----------------------------------------------------

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

#' @export
remove_threshold_analysis <- function(model, name) {
  item_idx <- find_in_list(model$threshold_analyses, "name", list(name),
                           "Threshold analysis")
  entity_remove_list(model, .schema_threshold_analysis, item_idx)
}

# --- Override Category -------------------------------------------------------

#' @export
add_override_category <- function(model, name, general = FALSE) {
  validate_string(name, "Override category name")
  new_item <- list(name = name, general = general, overrides = list())
  entity_add_list(model, .schema_override_category, new_item)
}

# --- Threshold Condition Constructors (pure helpers, not CRUD) ---------------

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

#' @export
add_state <- function(model, name, ...) UseMethod("add_state")

#' @export
edit_state <- function(model, name, ...) UseMethod("edit_state")

#' @export
remove_state <- function(model, name, ...) UseMethod("remove_state")

# --- Transitions (S3 generic) ------------------------------------------------

#' @export
add_transition <- function(model, ...) UseMethod("add_transition")

#' @export
edit_transition <- function(model, ...) UseMethod("edit_transition")

#' @export
remove_transition <- function(model, ...) UseMethod("remove_transition")

# --- Value (via engine) ------------------------------------------------------

#' @export
add_value <- function(model, name, formula, state = NA, destination = NA,
                       display_name = NULL, description = NULL,
                       type = "outcome", discounting_override = NA) {
  validate_string(name, "Value name", "string")
  if (!type %in% c("outcome", "cost")) stop("Value type must be 'outcome' or 'cost'.", call. = FALSE)
  formula_str <- capture_nse(rlang::enquo(formula))
  if (!nzchar(trimws(formula_str))) stop("Value formula must be a non-empty expression.", call. = FALSE)
  new_value <- fast_tibble(
    name = name, formula = formula_str,
    state = as.character(state), destination = as.character(destination),
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type, discounting_override = as.character(discounting_override)
  )
  entity_add_tibble(model, .schema_value, new_value, .callbacks_add_value)
}

#' @export
edit_value <- function(model, name, state = NA, destination = NA,
                        formula, display_name, description, type,
                        discounting_override, new_name,
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
  if (!missing(display_name)) updates$display_name <- display_name
  if (!missing(description)) updates$description <- description
  if (!missing(new_name)) {
    updates$new_name <- new_name
    updates$.rename_all <- rename_all
    updates$.error_on_field_changes <- error_on_field_changes
    updates$.error_on_name_sharing <- error_on_name_sharing
  }
  entity_edit_tibble(model, .schema_value, match_idx, updates, .callbacks_edit_value)
}

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
    discount_timing = "discount_timing", discount_method = "discount_method",
    reduce_state_cycle = "reduce_state_cycle", days_per_year = "days_per_year",
    country = "country", number_country = "number_country"
  )
  for (name in names(dots)) {
    setting_name <- if (name %in% names(setting_map)) setting_map[name] else name
    model$settings[[setting_name]] <- dots[[name]]
  }
  if ("n_cycles" %in% names(dots) && is.null(model$settings$timeframe_unit)) {
    model$settings$timeframe_unit <- "cycles"
  }
  if (is.null(model$settings$days_per_year)) model$settings$days_per_year <- 365
  if (is.null(model$settings$half_cycle_method)) model$settings$half_cycle_method <- "start"
  if (is.null(model$settings$discount_cost)) model$settings$discount_cost <- 0
  if (is.null(model$settings$discount_outcomes)) model$settings$discount_outcomes <- 0
  model
}

# --- VBP / PSA (singleton setters — not CRUD) --------------------------------

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

#' @export
remove_vbp <- function(model) { model$vbp <- NULL; model }

#' @export
set_psa <- function(model, n_sim, seed = NULL) {
  if (!is.numeric(n_sim) || length(n_sim) != 1 || is.na(n_sim) || n_sim < 1 || n_sim != as.integer(n_sim))
    stop("n_sim must be a positive integer", call. = FALSE)
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed)))
    stop("seed must be NULL or a single numeric value", call. = FALSE)
  model$psa <- list(n_sim = as.integer(n_sim), seed = seed)
  model
}

# --- Edit/Remove Summary (via engine) ----------------------------------------

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

#' @export
remove_dsa_variable <- function(model, variable, strategy = "", group = "") {
  validate_string(variable, "Variable name")
  item_idx <- find_in_list(model$dsa_parameters, c("type", "name", "strategy", "group"),
    list("variable", variable, as.character(strategy), as.character(group)), "DSA variable parameter")
  entity_remove_list(model, .schema_dsa_param, item_idx)
}

#' @export
remove_dsa_setting <- function(model, setting) {
  validate_string(setting, "Setting name")
  item_idx <- find_in_list(model$dsa_parameters, c("type", "name"),
    list("setting", setting), "DSA setting parameter")
  entity_remove_list(model, .schema_dsa_setting, item_idx)
}

# --- Edit/Remove Scenario Overrides (via engine) -----------------------------

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

#' @export
remove_scenario_variable <- function(model, scenario, variable, strategy = "", group = "") {
  validate_string(scenario, "Scenario name")
  parent_idx <- find_in_list(model$scenarios, "name", list(scenario), "Scenario")
  child_list <- model$scenarios[[parent_idx]]$variable_overrides
  child_idx <- find_in_list(child_list, c("name", "strategy", "group"),
    list(variable, as.character(strategy), as.character(group)), "Scenario variable override")
  entity_remove_nested(model, .schema_scenario, parent_idx, .schema_scenario_variable_override, child_idx)
}

#' @export
remove_scenario_setting <- function(model, scenario, setting) {
  validate_string(scenario, "Scenario name")
  parent_idx <- find_in_list(model$scenarios, "name", list(scenario), "Scenario")
  child_list <- model$scenarios[[parent_idx]]$setting_overrides
  child_idx <- find_in_list(child_list, "name", list(setting), "Scenario setting override")
  entity_remove_nested(model, .schema_scenario, parent_idx, .schema_scenario_setting_override, child_idx)
}

# --- TWSA (via engine) -------------------------------------------------------

#' @export
add_twsa <- function(model, name, description = NULL) {
  validate_string(name, "TWSA name")
  new_item <- list(name = name, description = description %||% name, enabled = TRUE, parameters = list())
  entity_add_list(model, .schema_twsa, new_item)
}

#' @export
edit_twsa <- function(model, name, new_name, description) {
  item_idx <- find_in_list(model$twsa_analyses, "name", list(name), "TWSA analysis")
  updates <- list()
  if (!missing(description)) updates$description <- description
  if (!missing(new_name)) updates$new_name <- new_name
  entity_edit_list(model, .schema_twsa, item_idx, updates)
}

#' @export
remove_twsa <- function(model, name) {
  item_idx <- find_in_list(model$twsa_analyses, "name", list(name), "TWSA analysis")
  entity_remove_list(model, .schema_twsa, item_idx)
}

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

#' @export
remove_override_category <- function(model, name) {
  item_idx <- find_in_list(model$override_categories, "name", list(name), "Override category")
  entity_remove_list(model, .schema_override_category, item_idx)
}

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
  if (!is.null(eff_cfg$min) && !is.null(eff_cfg$max) && eff_cfg$min >= eff_cfg$max) stop(sprintf("Override min (%s) must be less than max (%s)", min, max), call. = FALSE)
  if (!is.null(eff_cfg$step_size) && eff_cfg$step_size <= 0) stop("Override step_size must be greater than 0", call. = FALSE)
  entity_edit_nested(model, .schema_override_category, parent_idx, .schema_override, child_idx, updates)
}

#' @export
remove_override <- function(model, category, type, name, strategy = "", group = "") {
  parent_idx <- find_in_list(model$override_categories, "name", list(category), "Override category")
  child_list <- model$override_categories[[parent_idx]]$overrides
  child_idx <- find_in_list(child_list, c("type", "name", "strategy", "group"),
    list(type, name, as.character(strategy), as.character(group)), "Override")
  entity_remove_nested(model, .schema_override_category, parent_idx, .schema_override, child_idx)
}

# --- Table / Script (named list storage) -------------------------------------

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

#' @export
remove_table <- function(model, name) {
  if (is.null(model$tables[[name]])) stop(sprintf('Table "%s" not found.', name), call. = FALSE)
  model$tables[[name]] <- NULL; model
}

#' @export
add_script <- function(model, name, code, description = NULL) {
  validate_string(name, "Script name")
  model$scripts[[name]] <- list(code = code, description = description); model
}

#' @export
edit_script <- function(model, name, code, description, new_name) {
  if (is.null(model$scripts[[name]])) stop(sprintf('Script "%s" not found.', name), call. = FALSE)
  if (!missing(code)) model$scripts[[name]]$code <- code
  if (!missing(description)) model$scripts[[name]]$description <- description
  if (!missing(new_name)) { idx <- which(names(model$scripts) == name); names(model$scripts)[idx] <- new_name }
  model
}

#' @export
remove_script <- function(model, name) {
  if (is.null(model$scripts[[name]])) stop(sprintf('Script "%s" not found.', name), call. = FALSE)
  model$scripts[[name]] <- NULL; model
}

# --- Utility: override_option ------------------------------------------------

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

    valid_settings <- c(
      "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
      "discount_cost", "discount_outcomes", "half_cycle_method",
      "reduce_state_cycle", "days_per_year",
      "discount_rate"
    )
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

