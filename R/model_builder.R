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
#' @param type Character string specifying the model type ("markov" or "psm")
#'
#' @return An oq_model_builder object that can be piped to other builder functions
#'
#' @export
#' @examples
#' model <- define_model("markov")
#' model <- define_model("psm")
define_model <- function(type = "markov") {
  type <- match.arg(tolower(type), c("markov", "psm"))

  # Type-specific state initialization
  states_init <- if (type == "psm") {
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
    dsa_parameters = structure(list(), class = "dsa_parameters")
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

  # Get immutable model type
  model_type <- tolower(model$settings$model_type)

  if (model_type == "psm") {
    # PSM: Reject Markov-specific parameters
    if (!is.null(initial_prob)) {
      stop("PSM models don't use initial_prob parameter. Remove it from add_state() call.")
    }
    if (!is.null(state_group)) {
      stop("PSM models don't use state_group parameter. Remove it from add_state() call.")
    }
    if (share_state_time != FALSE) {
      stop("PSM models don't use share_state_time parameter. Remove it from add_state() call.")
    }
    if (!is.null(state_cycle_limit)) {
      stop("PSM models don't use state_cycle_limit parameter. Remove it from add_state() call.")
    }

    # Create PSM state (3 columns only)
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
  is_psm <- !is.null(model$settings$model_type) &&
            tolower(model$settings$model_type) == "psm"

  if (is_psm) {
    stop("For PSM models, use add_psm_transition() instead")
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

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_value <- tibble(
    name = name,
    formula = formula_str,
    state = as.character(state),
    destination = as.character(destination),
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
  model$tables[[name]] <- data
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
  model$scripts[[name]] <- code
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

  # Check if variable is group-specific and require group specification
  matching_vars <- model$variables[model$variables$name == variable, ]
  if (nrow(matching_vars) > 0) {
    # Handle NA values: treat NA as not group-specific
    non_na_groups <- matching_vars$group[!is.na(matching_vars$group)]
    has_group_specific <- length(non_na_groups) > 0 && any(non_na_groups != "")
    if (has_group_specific && group == "") {
      defined_groups <- unique(non_na_groups[non_na_groups != ""])
      stop(sprintf(
        paste0(
          "Variable '%s' is defined for specific group(s): %s\n",
          "You must specify which group to vary using the 'group' parameter.\n",
          "Example: add_dsa_variable(\"%s\", low = ..., high = ..., group = \"%s\")"
        ),
        variable,
        paste(defined_groups, collapse = ", "),
        variable,
        defined_groups[1]
      ), call. = FALSE)
    }
  }

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
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_dsa_setting("discount_cost", low = 0, high = 5) |>
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