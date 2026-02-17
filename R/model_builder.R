#' Model Builder Functions
#'
#' Functions for building openqaly models programmatically using a fluent API
#' with non-standard evaluation (NSE) for formula expressions.
#'
#' @name model_builder
#' @importFrom rlang enquo expr_text
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble tibble as_tibble
NULL

#' Define a New Model
#'
#' Initialize a new openqaly model object with the specified type.
#'
#' @param type Character string specifying the model type ("markov", "psm", or "custom_psm")
#'
#' @return An oq_model_builder object that can be piped to other builder functions
#'
#' @export
#' @examples
#' model <- define_model("markov")
#' model <- define_model("psm")
#' model <- define_model("custom_psm")
define_model <- function(type = "markov") {
  type <- match.arg(tolower(type), c("markov", "psm", "custom_psm"))

  # Type-specific state initialization
  # PSM and Custom PSM use same 3-column state structure
  states_init <- if (type %in% c("psm", "custom_psm")) {
    tibble(
      name = character(0),
      display_name = character(0),
      description = character(0)
    )
  } else {
    tibble(
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

  # Type-specific transitions initialization
  transitions_init <- if (type == "psm") {
    tibble(
      endpoint = character(0),
      time_unit = character(0),
      formula = character(0)
    )
  } else if (type == "custom_psm") {
    tibble(
      state = character(0),
      formula = character(0)
    )
  } else {
    tibble(
      from_state = character(0),
      to_state = character(0),
      formula = character(0)
    )
  }

  model <- list(
    settings = list(
      model_type = type,
      days_per_year = 365,
      half_cycle_method = "start",
      discount_cost = 0,
      discount_outcomes = 0,
      reduce_state_cycle = FALSE
    ),
    states = states_init,
    transitions = transitions_init,
    values = tibble(
      name = character(0),
      formula = character(0),
      state = character(0),
      destination = character(0),
      display_name = character(0),
      description = character(0),
      type = character(0)
    ),
    variables = tibble(
      name = character(0),
      formula = character(0),
      display_name = character(0),
      description = character(0),
      strategy = character(0),
      group = character(0),
      source = character(0),
      sampling = character(0)
    ),
    strategies = tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      enabled = numeric(0)
    ),
    groups = tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      weight = character(0),
      enabled = numeric(0)
    ),
    summaries = tibble(
      name = character(0),
      values = character(0),
      display_name = character(0),
      description = character(0),
      type = character(0),
      wtp = numeric(0)
    ),
    tables = list(),
    scripts = list(),
    trees = NULL,
    multivariate_sampling = list(),
    dsa_parameters = structure(list(), class = "dsa_parameters"),
    scenarios = list(),
    twsa_analyses = list(),
    override_categories = list(),
    threshold_analyses = list(),
    vbp = NULL
  )

  class(model) <- c("oq_model_builder", "oq_model")
  model
}

#' Set Model Settings
#'
#' Configure model settings such as number of cycles, cycle length, discount rates, etc.
#'
#' @param model An oq_model_builder object
#' @param ... Named arguments for settings (e.g., n_cycles = 100)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   set_settings(n_cycles = 100, cycle_length = "year")
set_settings <- function(model, ...) {
  dots <- list(...)

  # PREVENT model_type changes
  if ("model_type" %in% names(dots)) {
    stop("model_type cannot be changed after model creation. It was set to '",
         model$settings$model_type, "' in define_model().")
  }

  # Convert setting names to match expected format
  setting_map <- c(
    n_cycles = "timeframe",
    timeframe = "timeframe",
    timeframe_unit = "timeframe_unit",
    cycle_length_unit = "cycle_length_unit",
    cycle_length = "cycle_length",
    discount_cost = "discount_cost",
    discount_outcomes = "discount_outcomes",
    half_cycle_method = "half_cycle_method",
    reduce_state_cycle = "reduce_state_cycle",
    days_per_year = "days_per_year"
  )

  # Apply settings
  for (name in names(dots)) {
    setting_name <- if (name %in% names(setting_map)) setting_map[name] else name
    model$settings[[setting_name]] <- dots[[name]]
  }

  # If n_cycles was used and timeframe_unit is not set, default to "cycles"
  if ("n_cycles" %in% names(dots) && is.null(model$settings$timeframe_unit)) {
    model$settings$timeframe_unit <- "cycles"
  }

  # Apply defaults for settings not explicitly provided
  if (is.null(model$settings$days_per_year)) {
    model$settings$days_per_year <- 365
  }
  if (is.null(model$settings$half_cycle_method)) {
    model$settings$half_cycle_method <- "start"
  }
  if (is.null(model$settings$discount_cost)) {
    model$settings$discount_cost <- 0
  }
  if (is.null(model$settings$discount_outcomes)) {
    model$settings$discount_outcomes <- 0
  }

  model
}

#' Set VBP Configuration
#'
#' Configure value-based pricing (VBP) parameters on the model so they can be
#' serialized and used as defaults by \code{run_vbp()}, \code{run_dsa()},
#' \code{run_scenario()}, and \code{run_twsa()}.
#'
#' @param model An oq_model_builder object
#' @param price_variable Name of the variable representing the intervention's price
#' @param intervention_strategy Name of the intervention strategy
#' @param outcome_summary Name of the outcome summary to use
#' @param cost_summary Name of the cost summary to use
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   set_vbp(
#'     price_variable = "c_drug",
#'     intervention_strategy = "treatment",
#'     outcome_summary = "total_qalys",
#'     cost_summary = "total_costs"
#'   )
set_vbp <- function(model, price_variable, intervention_strategy,
                    outcome_summary, cost_summary) {
  # Validate all params are non-empty strings
  for (param_name in c("price_variable", "intervention_strategy",
                        "outcome_summary", "cost_summary")) {
    val <- get(param_name)
    if (!is.character(val) || length(val) != 1 || nchar(val) == 0) {
      stop(param_name, " must be a non-empty string", call. = FALSE)
    }
  }

  model$vbp <- list(
    price_variable = price_variable,
    intervention_strategy = intervention_strategy,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary
  )

  model
}

#' Add States to Model
#'
#' Add one or more states to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the state name
#' @param display_name Optional display name for the state
#' @param description Optional description of the state
#' @param state_group Optional state group for grouping related states
#' @param share_state_time Logical indicating whether to share state time with other states in the group
#' @param state_cycle_limit Optional limit on the number of cycles in this state
#' @param state_cycle_limit_unit Unit for the cycle limit (default: "cycles")
#' @param initial_prob Optional initial probability for this state
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_state("healthy", initial_prob = 1) |>
#'   add_state("sick", initial_prob = 0)
add_state <- function(model, name, display_name = NULL,
                     description = NULL, state_group = NULL,
                     share_state_time = FALSE, state_cycle_limit = NULL,
                     state_cycle_limit_unit = "cycles", initial_prob = NULL) {

  # Block reserved state names
  if (name %in% c("All", "All Other")) {
    stop(glue("'{name}' is a reserved state name and cannot be used as a state name. ",
              "It is used for special value targeting in add_value()."), call. = FALSE)
  }

  # Get immutable model type
  model_type <- tolower(model$settings$model_type)

  if (model_type %in% c("psm", "custom_psm")) {
    # PSM and Custom PSM: Reject Markov-specific parameters
    if (!is.null(initial_prob)) {
      stop("PSM/Custom PSM models don't use initial_prob parameter. Remove it from add_state() call.")
    }
    if (!is.null(state_group)) {
      stop("PSM/Custom PSM models don't use state_group parameter. Remove it from add_state() call.")
    }
    if (share_state_time != FALSE) {
      stop("PSM/Custom PSM models don't use share_state_time parameter. Remove it from add_state() call.")
    }
    if (!is.null(state_cycle_limit)) {
      stop("PSM/Custom PSM models don't use state_cycle_limit parameter. Remove it from add_state() call.")
    }

    # Create PSM/Custom PSM state (3 columns only)
    new_state <- tibble(
      name = name,
      display_name = display_name %||% name,
      description = description %||% display_name %||% name
    )
  } else {
    # Markov: Require initial_prob
    if (is.null(initial_prob)) {
      stop("initial_prob is required for Markov models. Specify it in add_state() call.")
    }

    # Create Markov state (8 columns)
    new_state <- tibble(
      name = name,
      initial_probability = as.character(initial_prob),
      display_name = display_name %||% name,
      description = description %||% display_name %||% name,
      state_group = state_group,
      share_state_time = share_state_time,
      state_cycle_limit = state_cycle_limit %||% Inf,
      state_cycle_limit_unit = state_cycle_limit_unit
    )
  }

  model$states <- bind_rows(model$states, new_state)

  # Incremental validation
  model <- normalize_and_validate_model(model, preserve_builder = TRUE)

  return(model)
}

#' Add Transitions to Model
#'
#' Add one or more transitions to the model. Uses NSE to capture formula expressions.
#'
#' @param model A oq_model_builder object
#' @param from_state Character string specifying the source state
#' @param to_state Character string specifying the destination state
#' @param formula An unquoted R expression for the transition probability
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_transition("healthy", "sick", p_disease * 0.1) |>
#'   add_transition("sick", "dead", 0.2)
add_transition <- function(model, from_state, to_state, formula) {
  # Check model type for appropriate structure
  model_type <- tolower(model$settings$model_type %||% "markov")

  if (model_type == "psm") {
    stop("For PSM models, use add_psm_transition() instead")
  }
  if (model_type == "custom_psm") {
    stop("For Custom PSM models, use add_custom_psm_transition() instead")
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_trans <- tibble(
    from_state = from_state,
    to_state = to_state,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)

  # Incremental validation
  model <- normalize_and_validate_model(model, preserve_builder = TRUE)

  return(model)
}

#' Add PSM Transitions to Model
#'
#' Add transitions for a PSM (Partitioned Survival Model).
#'
#' @param model A oq_model_builder object
#' @param endpoint Character string for the endpoint
#' @param time_unit Character string for the time unit
#' @param formula An unquoted R expression for the transition
#'
#' @return The modified model object
#'
#' @export
add_psm_transition <- function(model, endpoint, time_unit, formula) {
  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_trans <- tibble(
    endpoint = endpoint,
    time_unit = time_unit,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)

  # Incremental validation
  model <- normalize_and_validate_model(model, preserve_builder = TRUE)

  return(model)
}

#' Add Custom PSM Transition
#'
#' Add a state probability formula for a Custom PSM model. Each state must have
#' exactly one probability formula that defines the probability of being in that
#' state at each cycle.
#'
#' @param model An oq_model_builder object (must be type "custom_psm")
#' @param state Character string for the state name (must exist in model states)
#' @param formula An unquoted R expression for the state probability.
#'   Use `C` for complement (1 - sum of other states). Only one state can use `C`.
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("custom_psm") |>
#'   add_state("alive") |>
#'   add_state("progressed") |>
#'   add_state("dead") |>
#'   add_custom_psm_transition("alive", surv_prob(pfs_dist, month)) |>
#'   add_custom_psm_transition("progressed", surv_prob(os_dist, month) - surv_prob(pfs_dist, month)) |>
#'   add_custom_psm_transition("dead", C)
add_custom_psm_transition <- function(model, state, formula) {
  # Check model type
  is_custom_psm <- !is.null(model$settings$model_type) &&
    tolower(model$settings$model_type) == "custom_psm"

  if (!is_custom_psm) {
    stop("add_custom_psm_transition() is only for Custom PSM models. ",
         "Use add_transition() for Markov or add_psm_transition() for standard PSM.")
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_trans <- tibble(
    state = state,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)

  # Incremental validation
  model <- normalize_and_validate_model(model, preserve_builder = TRUE)

  return(model)
}

#' Add Values to Model
#'
#' Add one or more values to the model. Uses NSE to capture formula expressions.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the value name
#' @param formula An unquoted R expression for the value calculation
#' @param state Optional state association
#' @param destination Optional destination state
#' @param display_name Optional display name
#' @param description Optional description
#' @param type Value type (default: "outcome")
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_value("cost", base_cost + extra_cost, state = "sick")
add_value <- function(model, name, formula, state = NA, destination = NA,
                     display_name = NULL, description = NULL,
                     type = "outcome") {

  # Check for transitional values in custom_psm (not supported)
  model_type <- tolower(model$settings$model_type %||% "markov")
  if (model_type == "custom_psm") {
    has_state <- !is.na(state) && state != "NA"
    has_dest <- !is.na(destination) && destination != "NA"
    if (has_state && has_dest) {
      stop("Custom PSM models do not support transitional values (both state and destination). ",
           "Use residency values (state only) or model-level values (no state/destination).")
    }
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  # Validate "All" / "All Other" state targeting rules
  state_str <- as.character(state)
  dest_str <- as.character(destination)
  is_residence <- is.na(destination) || dest_str == "NA"

  if (state_str %in% c("All", "All Other")) {
    if (!is_residence) {
      stop(glue("'{state_str}' cannot be used for transition values (where destination is set). ",
                "It is only valid for residence values."), call. = FALSE)
    }

    existing <- model$values
    if (!is.null(existing) && nrow(existing) > 0) {
      existing_residence <- existing[existing$name == name &
        (is.na(existing$destination) | existing$destination == "NA"), , drop = FALSE]

      if (state_str == "All" && nrow(existing_residence) > 0) {
        stop(glue("Value '{name}' already has residence value rows. ",
                  "Cannot add 'All' when other residence rows exist."), call. = FALSE)
      }
      if (state_str == "All Other" && any(existing_residence$state == "All Other")) {
        stop(glue("Value '{name}' already has an 'All Other' residence row. ",
                  "Only one 'All Other' row is allowed per value name."), call. = FALSE)
      }
    }
  } else if (is_residence && !is.na(state) && state_str != "NA") {
    # Adding an explicit state — check if "All" already exists for this name
    existing <- model$values
    if (!is.null(existing) && nrow(existing) > 0) {
      existing_residence <- existing[existing$name == name &
        (is.na(existing$destination) | existing$destination == "NA"), , drop = FALSE]
      if (any(existing_residence$state == "All")) {
        stop(glue("Value '{name}' already uses state 'All'. ",
                  "Cannot add explicit state rows when 'All' is used."), call. = FALSE)
      }
    }
  }

  new_value <- tibble(
    name = name,
    formula = formula_str,
    state = state_str,
    destination = dest_str,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type
  )

  model$values <- bind_rows(model$values, new_value)

  # Incremental validation
  model <- normalize_and_validate_model(model, preserve_builder = TRUE)

  return(model)
}

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
#' @param model A oq_model_builder object
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
#' @export
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
add_variable <- function(model, name, formula, display_name = NULL,
                        description = NULL, strategy = "", group = "",
                        source = "", sampling) {

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  # Capture the sampling expression using NSE
  # Handle optional parameter: if missing, use empty string
  if (missing(sampling)) {
    sampling_str <- ""
  } else {
    sampling_quo <- enquo(sampling)
    sampling_expr <- quo_get_expr(sampling_quo)

    # Handle sampling: if already a string, use as-is (backward compatibility)
    # If an expression/call, convert to string using NSE
    if (is.character(sampling_expr)) {
      sampling_str <- sampling_expr
    } else {
      sampling_str <- expr_text(sampling_expr)
    }
  }

  # Store display_name as-is (don't auto-generate yet)
  # Auto-generation happens later in normalize_and_validate_model()
  # Convert NULL to empty string for consistency
  if (is.null(display_name)) {
    display_name <- ""
  }

  # If sampling is provided, validate strategy/group targeting
  # This ensures PSA sampling distributions are properly targeted
  if (sampling_str != "") {
    existing_vars <- model$variables[model$variables$name == name, ]
    if (nrow(existing_vars) > 0) {
      # Check if any existing definition has strategy-specific values
      existing_strategies <- unique(existing_vars$strategy[existing_vars$strategy != ""])
      if (length(existing_strategies) > 0 && strategy == "") {
        stop(sprintf(
          paste0(
            "Variable '%s' already has strategy-specific definitions: %s\n",
            "You must specify which strategy this sampling applies to using the 'strategy' parameter.\n",
            "Example: add_variable(\"%s\", ..., strategy = \"%s\", sampling = ...)"
          ),
          name,
          paste(existing_strategies, collapse = ", "),
          name,
          existing_strategies[1]
        ), call. = FALSE)
      }

      # Check if any existing definition has group-specific values
      existing_groups <- unique(existing_vars$group[existing_vars$group != ""])
      if (length(existing_groups) > 0 && group == "") {
        stop(sprintf(
          paste0(
            "Variable '%s' already has group-specific definitions: %s\n",
            "You must specify which group this sampling applies to using the 'group' parameter.\n",
            "Example: add_variable(\"%s\", ..., group = \"%s\", sampling = ...)"
          ),
          name,
          paste(existing_groups, collapse = ", "),
          name,
          existing_groups[1]
        ), call. = FALSE)
      }
    }
  }

  new_var <- tibble(
    name = name,
    formula = formula_str,
    display_name = display_name,
    description = description %||% "",
    strategy = strategy,
    group = group,
    source = source,
    sampling = sampling_str
  )

  model$variables <- bind_rows(model$variables, new_var)

  # Validate display name consistency for all variables with this name
  vars_with_same_name <- model$variables[model$variables$name == name, ]
  if (nrow(vars_with_same_name) > 1) {
    error_msg <- validate_variable_display_names_for_builder(vars_with_same_name, name)
    if (error_msg != "") {
      stop(error_msg, call. = FALSE)
    }
  }

  model
}

#' Add Strategies to Model
#'
#' Add one or more strategies to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the strategy name
#' @param display_name Optional display name
#' @param description Optional description
#' @param enabled Numeric indicating if strategy is enabled (default: 1)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_strategy("treatment_a", "Treatment A")
add_strategy <- function(model, name, display_name = NULL,
                        description = NULL, enabled = 1) {

  new_strat <- tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    enabled = as.numeric(enabled)
  )

  model$strategies <- bind_rows(model$strategies, new_strat)
  model
}

#' Add Groups to Model
#'
#' Add one or more groups to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the group name
#' @param display_name Optional display name
#' @param description Optional description
#' @param weight Weight expression (default: "1")
#' @param enabled Whether group is enabled (default: 1)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_group("moderate", weight = "w_moderate")
add_group <- function(model, name, display_name = NULL,
                     description = NULL, weight = "1", enabled = 1) {

  new_group <- tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    weight = as.character(weight),
    enabled = as.numeric(enabled)
  )

  model$groups <- bind_rows(model$groups, new_group)
  model
}

#' Add Summaries to Model
#'
#' Add one or more summaries to the model.
#'
#' @param model A oq_model_builder object
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
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_summary("total_cost", "cost1,cost2,cost3", type = "cost") |>
#'   add_summary("total_qalys", "qaly1,qaly2", type = "outcome", wtp = 50000)
add_summary <- function(model, name, values, display_name = NULL,
                       description = NULL, type = "outcome", wtp = NULL) {

  # Validate type
 if (!type %in% c("outcome", "cost")) {
    stop("Summary type must be 'outcome' or 'cost'")
  }

  # Validate that WTP is not specified for cost summaries
  if (type == "cost" && !is.null(wtp)) {
    stop(sprintf("WTP cannot be specified for cost summary '%s'. WTP is only valid for outcome summaries.", name))
  }

  new_summary <- tibble(
    name = name,
    values = values,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type,
    wtp = wtp %||% NA_real_
  )

  model$summaries <- bind_rows(model$summaries, new_summary)
  model
}

#' Add Table to Model
#'
#' Add a data table to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the table name
#' @param data A data frame containing the table data
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_table("costs", data.frame(state = c("A", "B"), cost = c(100, 200)))
add_table <- function(model, name, data, description = NULL) {
  # Store as structured list with data and optional description
  model$tables[[name]] <- list(
    data = data,
    description = description
  )
  model
}

#' Add Script to Model
#'
#' Add an R script to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the script name
#' @param code Character string containing the R code
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_script("preprocess", "# Data preprocessing\nlibrary(dplyr)")
add_script <- function(model, name, code, description = NULL) {
  # Store as structured list with code and optional description
  model$scripts[[name]] <- list(
    code = code,
    description = description
  )
  model
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

#' Add Multivariate Sampling Specification to Model
#'
#' Define a multivariate distribution for sampling correlated parameters.
#' This is used for Probabilistic Sensitivity Analysis (PSA) when multiple
#' parameters need to be sampled together with a specified correlation structure.
#'
#' @param model A oq_model_builder object
#' @param name Character string naming this sampling specification
#' @param distribution Expression defining the distribution function (uses NSE).
#'   The distribution should return a function(n) that generates n × k samples.
#'   Can reference any evaluated variables including base case values of sampled variables.
#'   For backward compatibility, can also be a character string.
#' @param variables Character vector of variable names to be sampled together,
#'   or a tibble with columns: variable, strategy, group (for segment-specific sampling)
#' @param description Optional description of this sampling specification
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' # Dirichlet distribution for transition probabilities
#' model <- define_model("markov") |>
#'   add_multivariate_sampling(
#'     name = "transition_probs",
#'     distribution = dirichlet(c(alpha_stable, alpha_progression, alpha_death)),
#'     variables = c("p_stable", "p_progression", "p_death"),
#'     description = "Transition probabilities from sick state"
#'   )
#'
#' # Multivariate normal for correlated cost and effectiveness
#' model <- model |>
#'   add_multivariate_sampling(
#'     name = "cost_qaly_correlation",
#'     distribution = mvnormal(mean = c(bc_cost, bc_qaly), sd = c(se_cost, se_qaly), cor = 0.6),
#'     variables = c("cost_treatment", "qaly_treatment")
#'   )
#'
#' # Segment-specific sampling with tibble specification
#' model <- model |>
#'   add_multivariate_sampling(
#'     name = "strategy_specific",
#'     distribution = mvnormal(mean = c(cost_mean, qaly_mean), sd = c(cost_sd, qaly_sd), cor = 0.5),
#'     variables = tibble(
#'       variable = c("treatment_cost", "treatment_qaly"),
#'       strategy = c("intervention", "intervention"),
#'       group = c(NA, NA)
#'     )
#'   )
#' }
add_multivariate_sampling <- function(model, name, distribution, variables, description = NULL) {

  # Capture the distribution expression using NSE
  distribution_quo <- enquo(distribution)
  distribution_expr <- quo_get_expr(distribution_quo)

  # Handle distribution: if already a string, use as-is (backward compatibility)
  # If an expression/call, convert to string using NSE
  if (is.character(distribution_expr)) {
    distribution_str <- distribution_expr
  } else {
    distribution_str <- expr_text(distribution_expr)
  }

  # Convert variables to tibble format
  if (is.character(variables)) {
    # Simple character vector -> tibble with just variable names
    variables_df <- tibble(
      variable = variables,
      strategy = NA_character_,
      group = NA_character_
    )
  } else if (is.data.frame(variables)) {
    # Already a dataframe/tibble
    variables_df <- as_tibble(variables)
    # Ensure required columns exist
    if (!"variable" %in% names(variables_df)) {
      stop("variables tibble must have a 'variable' column")
    }
    # Add missing columns with NA
    if (!"strategy" %in% names(variables_df)) {
      variables_df$strategy <- NA_character_
    }
    if (!"group" %in% names(variables_df)) {
      variables_df$group <- NA_character_
    }
    # Select only the required columns in the correct order
    variables_df <- variables_df %>% select("variable", "strategy", "group")
  } else {
    stop("variables must be either a character vector or a tibble/data.frame")
  }

  # Initialize multivariate_sampling if it doesn't exist
  if (is.null(model$multivariate_sampling)) {
    model$multivariate_sampling <- list()
  }

  # Create the sampling specification
  new_spec <- list(
    name = name,
    distribution = distribution_str,
    description = description %||% "",
    variables = variables_df
  )

  # Add to model
  model$multivariate_sampling <- c(model$multivariate_sampling, list(new_spec))

  model
}

#' Validate Variable Targeting for Sensitivity Analyses
#'
#' Checks if a variable requires strategy and/or group specification when used
#' in sensitivity analyses. If the variable is defined with specific strategies
#' or groups, this validation ensures the user explicitly specifies which
#' strategy/group to target.
#'
#' @param model A model object (oq_model_builder or parsed)
#' @param variable Character string naming the variable
#' @param strategy Strategy specification (empty string if not provided)
#' @param group Group specification (empty string if not provided)
#' @param analysis_type Character string for error message (e.g., "DSA", "TWSA")
#' @param add_function_name Character string for example in error message
#'
#' @return Invisible NULL if valid, stops with error if invalid
#' @keywords internal
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
#' @param model A oq_model_builder object
#' @param variable Character string naming the variable to vary (must exist in model$variables)
#' @param low Expression or numeric value for the low bound of the sensitivity range.
#'   Can use `bc` keyword to reference the base case value and other model variables.
#' @param high Expression or numeric value for the high bound of the sensitivity range.
#'   Can use `bc` keyword to reference the base case value and other model variables.
#' @param strategy Optional strategy name to limit DSA to specific strategy
#' @param group Optional group name to limit DSA to specific group
#' @param display_name Optional display name for plots and tables. If not provided,
#'   inherits from the variable definition. Required to be unique across all DSA parameters.
#'
#' @return The modified model object
#'
#' @export
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
add_dsa_variable <- function(model, variable, low, high,
                             strategy = "", group = "",
                             display_name = NULL) {

  # Validate variable name
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string")
  }

  # Validate strategy/group targeting for strategy/group-specific variables
  validate_variable_targeting(model, variable, strategy, group,
                              "DSA", "add_dsa_variable")

  # Check for existing DSA parameter with same name/strategy/group and replace if found
  if (length(model$dsa_parameters) > 0) {
    for (i in seq_along(model$dsa_parameters)) {
      existing <- model$dsa_parameters[[i]]
      if (existing$type == "variable" &&
          existing$name == variable &&
          existing$strategy == as.character(strategy) &&
          existing$group == as.character(group)) {
        warning(sprintf(
          "Replacing existing DSA specification for variable '%s'%s%s",
          variable,
          if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
          if (group != "") paste0(" (group: ", group, ")") else ""
        ), call. = FALSE)
        model$dsa_parameters <- model$dsa_parameters[-i]
        break
      }
    }
  }

  # Capture low expression using NSE
  low_quo <- enquo(low)
  low_expr <- quo_get_expr(low_quo)

  # Convert to oq_formula object for evaluation later
  if (is.numeric(low_expr)) {
    low_formula <- as.oq_formula(as.character(low_expr))
  } else {
    low_formula <- as.oq_formula(expr_text(low_expr))
  }

  # Capture high expression using NSE
  high_quo <- enquo(high)
  high_expr <- quo_get_expr(high_quo)

  if (is.numeric(high_expr)) {
    high_formula <- as.oq_formula(as.character(high_expr))
  } else {
    high_formula <- as.oq_formula(expr_text(high_expr))
  }

  # Create new DSA parameter specification as a list
  new_param <- list(
    type = "variable",
    name = variable,
    low = low_formula,      # Store as oq_formula object
    high = high_formula,    # Store as oq_formula object
    strategy = as.character(strategy),
    group = as.character(group),
    display_name = display_name
  )

  # Add to model's dsa_parameters list
  model$dsa_parameters <- c(model$dsa_parameters, list(new_param))
  class(model$dsa_parameters) <- "dsa_parameters"

  model
}

#' Add Deterministic Sensitivity Analysis Setting to Model
#'
#' Define a model setting to include in deterministic sensitivity analysis (DSA).
#' Settings include parameters like discount rates, timeframe, and other global
#' model configuration values that are not part of the variables table.
#'
#' @param model A oq_model_builder object
#' @param setting Character string naming the setting to vary
#'   (e.g., "discount_cost", "discount_outcomes", "timeframe")
#' @param low Value for the low bound (numeric or character depending on setting)
#' @param high Value for the high bound (numeric or character depending on setting)
#' @param display_name Optional display name for plots and tables
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 0.03) |>
#'   add_dsa_setting("discount_cost", low = 0, high = 0.05) |>
#'   add_dsa_setting("timeframe", low = 10, high = 30)
#' }
add_dsa_setting <- function(model, setting, low, high,
                            display_name = NULL) {

  # Validate inputs
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string")
  }

  # Check for existing DSA parameter with same setting name and replace if found
  if (length(model$dsa_parameters) > 0) {
    for (i in seq_along(model$dsa_parameters)) {
      existing <- model$dsa_parameters[[i]]
      if (existing$type == "setting" && existing$name == setting) {
        warning(sprintf(
          "Replacing existing DSA specification for setting '%s'",
          setting
        ), call. = FALSE)
        model$dsa_parameters <- model$dsa_parameters[-i]
        break
      }
    }
  }

  # Create new DSA parameter specification as a list
  # Store low/high as literal values (not formulas)
  new_param <- list(
    type = "setting",
    name = setting,
    low = low,           # Store as literal value
    high = high,         # Store as literal value
    display_name = display_name %||% setting
  )

  # Add to model's dsa_parameters list
  model$dsa_parameters <- c(model$dsa_parameters, list(new_param))
  class(model$dsa_parameters) <- "dsa_parameters"

  model
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
#' @param model An oq_model_builder object
#' @param name Unique name for the scenario (e.g., "Optimistic", "Pessimistic")
#' @param description Optional description explaining the scenario assumptions
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_scenario("Optimistic", description = "Best case assumptions") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   add_scenario_variable("Optimistic", "cost", 4000) |>
#'   add_scenario("Pessimistic", description = "Conservative assumptions") |>
#'   add_scenario_variable("Pessimistic", "efficacy", 0.70)
#' }
add_scenario <- function(model, name, description = NULL) {
  # Validate inputs

if (!is.character(name) || length(name) != 1 || name == "") {
    stop("Scenario name must be a non-empty character string", call. = FALSE)
  }

  # Check for reserved name "Base Case"
  if (tolower(name) == "base case") {
    stop("'Base Case' is a reserved scenario name that is automatically created",
         call. = FALSE)
  }

  # Initialize scenarios list if NULL
  if (is.null(model$scenarios)) {
    model$scenarios <- list()
  }

  # Check for duplicate names
  existing_names <- sapply(model$scenarios, function(s) s$name)
  if (name %in% existing_names) {
    stop(sprintf("Scenario '%s' already exists. Use a unique name.", name),
         call. = FALSE)
  }

  # Create new scenario
  new_scenario <- list(
    name = name,
    description = description %||% name,
    variable_overrides = list(),
    setting_overrides = list()
  )

  model$scenarios <- c(model$scenarios, list(new_scenario))
  model
}

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
#' @param model An oq_model_builder object
#' @param scenario Name of the scenario to add the override to (must exist)
#' @param variable Name of the variable to override (must exist in model$variables)
#' @param value Value expression for the override (uses NSE)
#' @param strategy Optional strategy name to limit override to specific strategy
#' @param group Optional group name to limit override to specific group
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("efficacy", 0.8) |>
#'   add_variable("cost", 5000) |>
#'   add_scenario("Optimistic") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   add_scenario_variable("Optimistic", "cost", 4000)
#' }
add_scenario_variable <- function(model, scenario, variable, value,
                                   strategy = "", group = "") {
  # Validate inputs
  if (!is.character(scenario) || length(scenario) != 1) {
    stop("scenario must be a single character string", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string", call. = FALSE)
  }

  # Validate strategy/group targeting for strategy/group-specific variables
  validate_variable_targeting(model, variable, strategy, group,
                              "Scenario", "add_scenario_variable")

  # Find scenario index
  scenario_idx <- which(sapply(model$scenarios, function(s) s$name) == scenario)
  if (length(scenario_idx) == 0) {
    stop(sprintf("Scenario '%s' not found. Use add_scenario() first.", scenario),
         call. = FALSE)
  }

  # Capture value with NSE
  value_quo <- enquo(value)
  value_expr <- quo_get_expr(value_quo)

  # Store as literal numeric or oq_formula depending on type
  if (is.numeric(value_expr)) {
    stored_value <- value_expr
  } else {
    stored_value <- as.oq_formula(expr_text(value_expr))
  }

  # Create override entry
  override <- list(
    name = variable,
    value = stored_value,
    strategy = as.character(strategy),
    group = as.character(group)
  )

  # Add to scenario's variable_overrides
  model$scenarios[[scenario_idx]]$variable_overrides <- c(
    model$scenarios[[scenario_idx]]$variable_overrides,
    list(override)
  )

  model
}

#' Add Setting Override to Scenario
#'
#' Add a model setting override for a specific scenario. When the scenario
#' analysis runs this scenario, the specified setting will use the override
#' value instead of its default.
#'
#' Common settings that can be overridden include:
#' - `timeframe`: Model time horizon
#' - `discount_cost`: Discount rate for costs (decimal, e.g. 0.03 for 3%)
#' - `discount_outcomes`: Discount rate for outcomes (decimal, e.g. 0.03 for 3%)
#' - `cycle_length`: Length of each model cycle
#'
#' @param model An oq_model_builder object
#' @param scenario Name of the scenario to add the override to (must exist)
#' @param setting Name of the setting to override
#' @param value Value for the setting override
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 0.03) |>
#'   add_scenario("Extended Horizon") |>
#'   add_scenario_setting("Extended Horizon", "timeframe", 30) |>
#'   add_scenario("No Discounting") |>
#'   add_scenario_setting("No Discounting", "discount_cost", 0) |>
#'   add_scenario_setting("No Discounting", "discount_outcomes", 0)
#' }
add_scenario_setting <- function(model, scenario, setting, value) {
  # Validate inputs
  if (!is.character(scenario) || length(scenario) != 1) {
    stop("scenario must be a single character string", call. = FALSE)
  }
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string", call. = FALSE)
  }

  # Find scenario index
  scenario_idx <- which(sapply(model$scenarios, function(s) s$name) == scenario)
  if (length(scenario_idx) == 0) {
    stop(sprintf("Scenario '%s' not found. Use add_scenario() first.", scenario),
         call. = FALSE)
  }

  # Create override entry
  override <- list(
    name = setting,
    value = value
  )

  # Add to scenario's setting_overrides
  model$scenarios[[scenario_idx]]$setting_overrides <- c(
    model$scenarios[[scenario_idx]]$setting_overrides,
    list(override)
  )

  model
}

#' Print Scenarios
#'
#' Print method for displaying scenarios defined in a model
#'
#' @param model An oq_model_builder object with scenarios
#'
#' @return Invisible model
#' @keywords internal
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

#' Add a Two-Way Sensitivity Analysis to Model
#'
#' Define a named two-way sensitivity analysis (2WSA) that varies two parameters
#' simultaneously across a grid of values. Each 2WSA must have exactly two
#' variable or setting specifications added via `add_twsa_variable()` or
#' `add_twsa_setting()`.
#'
#' @param model An oq_model_builder object
#' @param name Unique name for the 2WSA analysis (e.g., "Cost vs Efficacy")
#' @param description Optional description explaining what this analysis explores
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Cost vs Efficacy") |>
#'   add_twsa_variable("Cost vs Efficacy", "cost_tx",
#'     type = "range", min = 1000, max = 3000, steps = 5) |>
#'   add_twsa_variable("Cost vs Efficacy", "efficacy",
#'     type = "radius", radius = 0.1, steps = 5)
#' }
add_twsa <- function(model, name, description = NULL) {
  # Validate inputs
  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("TWSA name must be a non-empty character string", call. = FALSE)
  }

  # Check for reserved name "Base Case"
  if (tolower(name) == "base case") {
    stop("'Base Case' is a reserved name that cannot be used for TWSA analyses",
         call. = FALSE)
  }

  # Initialize twsa_analyses list if NULL
  if (is.null(model$twsa_analyses)) {
    model$twsa_analyses <- list()
  }

  # Check for duplicate names
  existing_names <- sapply(model$twsa_analyses, function(s) s$name)
  if (name %in% existing_names) {
    stop(sprintf("TWSA analysis '%s' already exists. Use a unique name.", name),
         call. = FALSE)
  }

  # Create new TWSA analysis
  new_twsa <- list(
    name = name,
    description = description %||% name,
    parameters = list()  # Will hold exactly 2 variable/setting specs
  )

  model$twsa_analyses <- c(model$twsa_analyses, list(new_twsa))
  model
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
#' @param model An oq_model_builder object
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
#' @export
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
add_twsa_variable <- function(model, twsa_name, variable, type,
                               min = NULL, max = NULL,
                               radius = NULL, steps = NULL,
                               values = NULL,
                               strategy = "", group = "",
                               display_name = NULL,
                               include_base_case = TRUE) {
  # Validate inputs
  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string", call. = FALSE)
  }

  # Validate strategy/group targeting for strategy/group-specific variables
  validate_variable_targeting(model, variable, strategy, group,
                              "TWSA", "add_twsa_variable")

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found. Use add_twsa() first.", twsa_name),
         call. = FALSE)
  }

  # Check if TWSA already has 2 parameters
  current_params <- length(model$twsa_analyses[[twsa_idx]]$parameters)
  if (current_params >= 2) {
    stop(sprintf(
      "TWSA analysis '%s' already has 2 parameters. Each TWSA must have exactly 2 parameters.",
      twsa_name
    ), call. = FALSE)
  }

  # Validate type
  type <- match.arg(type, c("range", "radius", "custom"))

  # Validate type-specific parameters
  if (type == "range") {
    if (is.null(substitute(min)) || is.null(substitute(max)) || is.null(steps)) {
      stop("For type='range', min, max, and steps are required", call. = FALSE)
    }
  } else if (type == "radius") {
    if (is.null(substitute(radius)) || is.null(steps)) {
      stop("For type='radius', radius and steps are required", call. = FALSE)
    }
  } else if (type == "custom") {
    if (is.null(substitute(values))) {
      stop("For type='custom', values is required", call. = FALSE)
    }
  }

  # Capture expressions with NSE
  min_formula <- NULL
  max_formula <- NULL
  radius_formula <- NULL
  values_formula <- NULL

  if (type == "range") {
    min_quo <- enquo(min)
    min_expr <- quo_get_expr(min_quo)
    if (is.numeric(min_expr)) {
      min_formula <- as.oq_formula(as.character(min_expr))
    } else {
      min_formula <- as.oq_formula(expr_text(min_expr))
    }

    max_quo <- enquo(max)
    max_expr <- quo_get_expr(max_quo)
    if (is.numeric(max_expr)) {
      max_formula <- as.oq_formula(as.character(max_expr))
    } else {
      max_formula <- as.oq_formula(expr_text(max_expr))
    }
  } else if (type == "radius") {
    radius_quo <- enquo(radius)
    radius_expr <- quo_get_expr(radius_quo)
    if (is.numeric(radius_expr)) {
      radius_formula <- as.oq_formula(as.character(radius_expr))
    } else {
      radius_formula <- as.oq_formula(expr_text(radius_expr))
    }
  } else if (type == "custom") {
    values_quo <- enquo(values)
    values_expr <- quo_get_expr(values_quo)
    # For custom, we need to handle vector expressions
    values_formula <- as.oq_formula(expr_text(values_expr))
  }

  # Create parameter specification
  param_spec <- list(
    param_type = "variable",
    name = variable,
    type = type,
    min = min_formula,
    max = max_formula,
    radius = radius_formula,
    steps = steps,
    values = values_formula,
    strategy = as.character(strategy),
    group = as.character(group),
    display_name = display_name,
    include_base_case = include_base_case
  )

  # Add to TWSA's parameters list
  model$twsa_analyses[[twsa_idx]]$parameters <- c(
    model$twsa_analyses[[twsa_idx]]$parameters,
    list(param_spec)
  )

  model
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
#' @param model An oq_model_builder object
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
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 0.03) |>
#'   add_twsa("Time vs Discount") |>
#'   add_twsa_setting("Time vs Discount", "timeframe",
#'     type = "range", min = 10, max = 30, steps = 5) |>
#'   add_twsa_setting("Time vs Discount", "discount_cost",
#'     type = "custom", values = c(0, 0.015, 0.03, 0.05))
#' }
add_twsa_setting <- function(model, twsa_name, setting, type,
                              min = NULL, max = NULL,
                              radius = NULL, steps = NULL,
                              values = NULL,
                              display_name = NULL,
                              include_base_case = TRUE) {
  # Validate inputs
  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string", call. = FALSE)
  }

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found. Use add_twsa() first.", twsa_name),
         call. = FALSE)
  }

  # Check if TWSA already has 2 parameters
  current_params <- length(model$twsa_analyses[[twsa_idx]]$parameters)
  if (current_params >= 2) {
    stop(sprintf(
      "TWSA analysis '%s' already has 2 parameters. Each TWSA must have exactly 2 parameters.",
      twsa_name
    ), call. = FALSE)
  }

  # Validate type
  type <- match.arg(type, c("range", "radius", "custom"))

  # Validate type-specific parameters
  if (type == "range") {
    if (is.null(min) || is.null(max) || is.null(steps)) {
      stop("For type='range', min, max, and steps are required", call. = FALSE)
    }
  } else if (type == "radius") {
    if (is.null(radius) || is.null(steps)) {
      stop("For type='radius', radius and steps are required", call. = FALSE)
    }
  } else if (type == "custom") {
    if (is.null(values)) {
      stop("For type='custom', values is required", call. = FALSE)
    }
  }

  # Create parameter specification (store literal values for settings)
  param_spec <- list(
    param_type = "setting",
    name = setting,
    type = type,
    min = min,
    max = max,
    radius = radius,
    steps = steps,
    values = values,
    display_name = display_name %||% setting,
    include_base_case = include_base_case
  )

  # Add to TWSA's parameters list
  model$twsa_analyses[[twsa_idx]]$parameters <- c(
    model$twsa_analyses[[twsa_idx]]$parameters,
    list(param_spec)
  )

  model
}

#' Print TWSA Analyses
#'
#' Print method for displaying two-way sensitivity analyses defined in a model
#'
#' @param model An oq_model_builder object with TWSA analyses
#'
#' @return Invisible model
#' @keywords internal
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

#' Add an Override Category to Model
#'
#' Create a named category for grouping related override controls.
#' Override categories organize UI controls that allow users to modify
#' model variables and settings at runtime.
#'
#' @param model An oq_model_builder object
#' @param name Character string for the category name (must be unique, case-insensitive)
#' @param general Logical indicating if this is a system category (default: FALSE)
#'
#' @return The modified model object
#' @export
add_override_category <- function(model, name, general = FALSE) {
  # Validate inputs
  if (!is.character(name) || length(name) != 1 || nchar(trimws(name)) == 0) {
    stop("Override category name must be a non-empty character string", call. = FALSE)
  }

  # Check for duplicate category name (case-insensitive)
  if (length(model$override_categories) > 0) {
    existing_names <- tolower(sapply(model$override_categories, function(c) c$name))
    if (tolower(name) %in% existing_names) {
      stop(sprintf("Override category '%s' already exists", name), call. = FALSE)
    }
  }

  # Create new category
  new_category <- list(
    name = name,
    general = as.logical(general),
    overrides = list()
  )

  # Add to model
  model$override_categories <- c(model$override_categories, list(new_category))

  model
}

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
  if (!is.character(label) || length(label) != 1 || nchar(trimws(label)) == 0) {
    stop("Dropdown option label must be a non-empty character string", call. = FALSE)
  }
  list(
    label = label,
    value = as.character(value),
    is_base_case = as.logical(is_base_case)
  )
}

#' Add an Override Control to a Category
#'
#' Define a UI override control that allows users to modify a model variable
#' or setting at runtime. When the model is rendered in a Shiny UI, these
#' controls will be displayed to the user.
#'
#' @param model An oq_model_builder object
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
                         input_type = "numeric", expression,
                         description = NULL, strategy = "", group = "",
                         general = FALSE,
                         min = NULL, max = NULL, step_size = NULL,
                         options = NULL) {

  # Validate category exists
  if (length(model$override_categories) == 0) {
    stop(sprintf("Override category '%s' not found. Use add_override_category() first.", category),
         call. = FALSE)
  }
  cat_idx <- which(sapply(model$override_categories, function(c) c$name) == category)
  if (length(cat_idx) == 0) {
    stop(sprintf("Override category '%s' not found. Use add_override_category() first.", category),
         call. = FALSE)
  }

  # Validate title
  if (!is.character(title) || length(title) != 1 || nchar(trimws(title)) == 0) {
    stop("Override title must be a non-empty character string", call. = FALSE)
  }

  # Validate name
  if (!is.character(name) || length(name) != 1 || nchar(trimws(name)) == 0) {
    stop("Override name must be a non-empty character string", call. = FALSE)
  }

  # Validate type
  if (!type %in% c("variable", "setting")) {
    stop("Override type must be 'variable' or 'setting'", call. = FALSE)
  }

  # Validate input_type
  valid_input_types <- c("numeric", "slider", "dropdown", "formula", "timeframe")
  if (!input_type %in% valid_input_types) {
    stop(paste0("Override input_type must be one of: ", paste(valid_input_types, collapse = ", ")),
         call. = FALSE)
  }

  # Capture expression using NSE
  expr_quo <- enquo(expression)
  expr_val <- quo_get_expr(expr_quo)

  if (is.numeric(expr_val)) {
    expression_str <- as.character(expr_val)
  } else if (is.character(expr_val) && length(expr_val) == 1) {
    expression_str <- expr_val
  } else {
    expression_str <- expr_text(expr_val)
  }

  # Validate expression non-empty
  if (nchar(trimws(expression_str)) == 0) {
    stop("Override expression must be non-empty", call. = FALSE)
  }

  # Validate type-specific rules
  if (type == "setting") {
    # Settings don't use strategy/group
    if (strategy != "" || group != "") {
      stop("Strategy and group cannot be specified for setting overrides", call. = FALSE)
    }
    # Validate setting name
    valid_settings <- c(
      "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
      "discount_cost", "discount_outcomes", "half_cycle_method",
      "reduce_state_cycle", "days_per_year"
    )
    if (!(name %in% valid_settings)) {
      stop(sprintf(
        "Invalid override setting name: '%s'. Valid settings: %s",
        name, paste(valid_settings, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (type == "variable") {
    # Validate variable exists and strategy/group targeting
    validate_variable_targeting(model, name, strategy, group,
                                "Override", "add_override")
  }

  # Validate min < max
  if (!is.null(min) && !is.null(max)) {
    if (min >= max) {
      stop(sprintf("Override min (%s) must be less than max (%s)", min, max), call. = FALSE)
    }
  }

  # Validate step_size > 0
  if (!is.null(step_size) && step_size <= 0) {
    stop("Override step_size must be greater than 0", call. = FALSE)
  }

  # Validate dropdown options
  if (input_type == "dropdown" && !is.null(options) && length(options) == 0) {
    stop("Dropdown override must have at least one option", call. = FALSE)
  }

  # Check for duplicate override in this category
  existing_overrides <- model$override_categories[[cat_idx]]$overrides
  if (length(existing_overrides) > 0) {
    for (existing in existing_overrides) {
      if (existing$type == type && existing$name == name &&
          existing$strategy == as.character(strategy) &&
          existing$group == as.character(group)) {
        stop(sprintf(
          "An override for %s '%s'%s%s already exists in category '%s'",
          type, name,
          if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
          if (group != "") paste0(" (group: ", group, ")") else "",
          category
        ), call. = FALSE)
      }
    }
  }

  # Build input_config with defaults
  input_config <- switch(input_type,
    "numeric" = list(
      min = min %||% 0,
      max = max %||% 100
    ),
    "slider" = list(
      min = min %||% 0,
      max = max %||% 1,
      step_size = step_size %||% 0.05
    ),
    "dropdown" = list(
      options = options %||% list()
    ),
    "formula" = list(),
    "timeframe" = list()
  )

  # Create override item
  override_item <- list(
    title = title,
    description = description %||% "",
    type = type,
    name = name,
    strategy = as.character(strategy),
    group = as.character(group),
    general = as.logical(general),
    input_type = input_type,
    overridden_expression = expression_str,
    input_config = input_config
  )

  # Add to category
  model$override_categories[[cat_idx]]$overrides <- c(
    model$override_categories[[cat_idx]]$overrides,
    list(override_item)
  )

  model
}

#' Print Override Categories Summary
#'
#' Displays a formatted summary of override categories and their overrides.
#'
#' @param model A model object with override_categories
#' @return Invisible model
#' @keywords internal
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

# ============================================================================
# Threshold Analysis
# ============================================================================

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
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_outcomes <- function(
  summary = NULL,
  value = NULL,
  type = c("absolute", "difference"),
  strategy = NULL,
  referent = NULL,
  comparator = NULL,
  discounted = TRUE,
  target_value = 0,
  group = ""
) {
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
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_costs <- function(
  summary = NULL,
  value = NULL,
  type = c("absolute", "difference"),
  strategy = NULL,
  referent = NULL,
  comparator = NULL,
  discounted = TRUE,
  target_value = 0,
  group = ""
) {
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
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_nmb <- function(health_summary, cost_summary, referent, comparator,
                                     discounted = TRUE, target_value = 0, group = "",
                                     wtp = NULL) {
  list(output = "nmb", health_summary = health_summary, cost_summary = cost_summary,
       referent = referent, comparator = comparator, discounted = discounted,
       target_value = target_value, group = group, wtp = wtp)
}

#' Create Threshold Condition for CE
#'
#' @param health_summary Health outcome summary name
#' @param cost_summary Cost outcome summary name
#' @param referent Referent strategy
#' @param comparator Comparator strategy
#' @param discounted Whether to use discounted results
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @param wtp Willingness-to-pay value. If NULL (default), the WTP from the health summary is used.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_ce <- function(health_summary, cost_summary, referent, comparator,
                                    discounted = TRUE, group = "",
                                    wtp = NULL) {
  list(output = "ce", health_summary = health_summary, cost_summary = cost_summary,
       referent = referent, comparator = comparator, discounted = discounted,
       group = group, wtp = wtp)
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

#' Add Threshold Analysis to Model
#'
#' Define a threshold analysis that finds the input parameter value producing
#' a desired output condition using iterative root-finding.
#'
#' @param model An oq_model_builder object
#' @param name Unique name for this threshold analysis
#' @param variable Variable to solve for
#' @param lower Lower bound for search range
#' @param upper Upper bound for search range
#' @param condition A condition list created by \code{threshold_condition_*} functions
#' @param variable_strategy Strategy targeting for the variable (empty string = all)
#' @param variable_group Group targeting for the variable (empty string = all)
#' @param active Whether this analysis is active
#' @return The modified model object
#' @export
add_threshold_analysis <- function(
  model, name, variable, lower, upper,
  condition,
  variable_strategy = "",
  variable_group = "",
  active = TRUE
) {
  if (!inherits(model, "oq_model_builder") && !inherits(model, "oq_model")) {
    stop("model must be an oq_model_builder or oq_model object", call. = FALSE)
  }

  # Validate name
  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("name must be a non-empty character string", call. = FALSE)
  }

  # Validate variable
  if (!is.character(variable) || length(variable) != 1 || variable == "") {
    stop("variable must be a non-empty character string", call. = FALSE)
  }

  # Validate bounds
  if (!is.numeric(lower) || !is.numeric(upper) || length(lower) != 1 || length(upper) != 1) {
    stop("lower and upper must be single numeric values", call. = FALSE)
  }
  if (lower >= upper) {
    stop("lower must be less than upper", call. = FALSE)
  }

  # Validate condition
  if (!is.list(condition) || is.null(condition$output)) {
    stop("condition must be a list with an 'output' field (use threshold_condition_* functions)", call. = FALSE)
  }

  valid_outputs <- c("outcomes", "costs", "nmb", "ce", "vbp", "trace")
  if (!condition$output %in% valid_outputs) {
    stop(sprintf("Invalid output type '%s'. Must be one of: %s",
                 condition$output, paste(valid_outputs, collapse = ", ")), call. = FALSE)
  }

  if (condition$output == "vbp") {
    stop("VBP output type is not yet supported for threshold analysis", call. = FALSE)
  }

  # Validate output-specific fields
  validate_threshold_condition(condition)

  # Validate variable targeting
  validate_variable_targeting(model, variable, variable_strategy, variable_group,
                               "threshold", "add_threshold_analysis")

  # Check for duplicate name - warn and replace
  existing_idx <- which(sapply(model$threshold_analyses, function(a) a$name) == name)
  if (length(existing_idx) > 0) {
    warning(sprintf("Threshold analysis '%s' already exists and will be replaced", name), call. = FALSE)
    model$threshold_analyses[[existing_idx]] <- NULL
  }

  analysis <- list(
    name = name,
    variable = variable,
    variable_strategy = variable_strategy,
    variable_group = variable_group,
    lower = lower,
    upper = upper,
    active = active,
    condition = condition
  )

  model$threshold_analyses <- c(model$threshold_analyses, list(analysis))
  model
}

#' Validate Threshold Condition Fields
#' @param condition A threshold condition list
#' @keywords internal
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