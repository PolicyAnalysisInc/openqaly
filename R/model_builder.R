#' Model Builder Functions
#'
#' Functions for building heRomod2 models programmatically using a fluent API
#' with non-standard evaluation (NSE) for formula expressions.
#'
#' @name model_builder
#' @importFrom rlang enquo expr_text
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble tibble as_tibble
NULL

#' Define a New Model
#'
#' Initialize a new heRomod2 model object with the specified type.
#'
#' @param type Character string specifying the model type ("markov" or "psm")
#'
#' @return A heRomodel_builder object that can be piped to other builder functions
#'
#' @export
#' @examples
#' model <- define_model("markov")
#' model <- define_model("psm")
define_model <- function(type = "markov") {
  type <- match.arg(tolower(type), c("markov", "psm"))

  model <- list(
    settings = list(model_type = type),
    states = tibble::tibble(),
    transitions = tibble::tibble(),
    values = tibble::tibble(),
    variables = tibble::tibble(),
    strategies = tibble::tibble(),
    groups = tibble::tibble(),
    summaries = tibble::tibble(),
    tables = list(),
    scripts = list(),
    trees = NULL
  )

  class(model) <- c("heRomodel_builder", "heRomodel")
  model
}

#' Set Model Settings
#'
#' Configure model settings such as number of cycles, cycle length, discount rates, etc.
#'
#' @param model A heRomodel_builder object
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

  # Convert setting names to match expected format
  setting_map <- c(
    n_cycles = "timeframe",
    timeframe = "timeframe",
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

  model
}

#' Add States to Model
#'
#' Add one or more states to the model.
#'
#' @param model A heRomodel_builder object
#' @param ... State specifications, either as direct arguments or using state() helper
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_state("healthy", initial_prob = 1) |>
#'   add_state("sick", initial_prob = 0)
add_state <- function(model, name, initial_prob = 0, display_name = NULL,
                     description = NULL, state_group = NULL,
                     share_state_time = FALSE, state_cycle_limit = NULL,
                     state_cycle_limit_unit = "cycles") {

  new_state <- tibble::tibble(
    name = name,
    initial_probability = as.character(initial_prob),
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    state_group = state_group,
    share_state_time = share_state_time,
    state_cycle_limit = state_cycle_limit %||% Inf,
    state_cycle_limit_unit = state_cycle_limit_unit
  )

  model$states <- bind_rows(model$states, new_state)
  model
}

#' Add Transitions to Model
#'
#' Add one or more transitions to the model. Uses NSE to capture formula expressions.
#'
#' @param model A heRomodel_builder object
#' @param from Character string specifying the source state
#' @param to Character string specifying the destination state
#' @param formula An unquoted R expression for the transition probability
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_transition("healthy", "sick", p_disease * 0.1) |>
#'   add_transition("sick", "dead", 0.2)
add_transition <- function(model, from, to, formula) {
  # Check model type for appropriate structure
  is_psm <- !is.null(model$settings$model_type) &&
            tolower(model$settings$model_type) == "psm"

  if (is_psm) {
    stop("For PSM models, use add_psm_transition() instead")
  }

  # Capture the formula expression using NSE
  formula_quo <- rlang::enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- rlang::quo_get_expr(formula_quo)
  formula_str <- rlang::expr_text(formula_expr)

  new_trans <- tibble::tibble(
    from = from,
    to = to,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)
  model
}

#' Add PSM Transitions to Model
#'
#' Add transitions for a PSM (Partitioned Survival Model).
#'
#' @param model A heRomodel_builder object
#' @param endpoint Character string for the endpoint
#' @param time_unit Character string for the time unit
#' @param formula An unquoted R expression for the transition
#'
#' @return The modified model object
#'
#' @export
add_psm_transition <- function(model, endpoint, time_unit, formula) {
  # Capture the formula expression using NSE
  formula_quo <- rlang::enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- rlang::quo_get_expr(formula_quo)
  formula_str <- rlang::expr_text(formula_expr)

  new_trans <- tibble::tibble(
    endpoint = endpoint,
    time_unit = time_unit,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)
  model
}

#' Add Values to Model
#'
#' Add one or more values to the model. Uses NSE to capture formula expressions.
#'
#' @param model A heRomodel_builder object
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
  formula_quo <- rlang::enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- rlang::quo_get_expr(formula_quo)
  formula_str <- rlang::expr_text(formula_expr)

  new_value <- tibble::tibble(
    name = name,
    formula = formula_str,
    state = as.character(state),
    destination = as.character(destination),
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type
  )

  model$values <- bind_rows(model$values, new_value)
  model
}

#' Add Variables to Model
#'
#' Add one or more variables to the model. Uses NSE to capture formula expressions.
#'
#' @param model A heRomodel_builder object
#' @param name Character string for the variable name
#' @param formula An unquoted R expression for the variable calculation
#' @param display_name Optional display name
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
add_variable <- function(model, name, formula, display_name = NULL,
                        description = NULL, strategy = "", group = "",
                        source = "", sampling = "") {

  # Capture the formula expression using NSE
  formula_quo <- rlang::enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- rlang::quo_get_expr(formula_quo)
  formula_str <- rlang::expr_text(formula_expr)

  new_var <- tibble::tibble(
    name = name,
    formula = formula_str,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    strategy = strategy,
    group = group,
    source = source,
    sampling = sampling
  )

  model$variables <- bind_rows(model$variables, new_var)
  model
}

#' Add Strategies to Model
#'
#' Add one or more strategies to the model.
#'
#' @param model A heRomodel_builder object
#' @param name Character string for the strategy name
#' @param display_name Optional display name
#' @param description Optional description
#' @param abbreviation Optional abbreviation
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_strategy("treatment_a", "Treatment A", abbreviation = "TX-A")
add_strategy <- function(model, name, display_name = NULL,
                        description = NULL, abbreviation = NULL, enabled = 1) {

  new_strat <- tibble::tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    abbreviation = abbreviation %||% toupper(substr(name, 1, 4)),
    enabled = as.numeric(enabled)
  )

  model$strategies <- bind_rows(model$strategies, new_strat)
  model
}

#' Add Groups to Model
#'
#' Add one or more groups to the model.
#'
#' @param model A heRomodel_builder object
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

  new_group <- tibble::tibble(
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
#' @param model A heRomodel_builder object
#' @param name Character string for the summary name
#' @param values Comma-separated string of value names to include
#' @param display_name Optional display name
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_summary("total_cost", "cost1,cost2,cost3")
add_summary <- function(model, name, values, display_name = NULL,
                       description = NULL) {

  new_summary <- tibble::tibble(
    name = name,
    values = values,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name
  )

  model$summaries <- bind_rows(model$summaries, new_summary)
  model
}

#' Add Table to Model
#'
#' Add a data table to the model.
#'
#' @param model A heRomodel_builder object
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
#' @param model A heRomodel_builder object
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
#' Convert a heRomodel object to a JSON string representation.
#'
#' @param model A heRomodel object
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
#' Convert a heRomodel object to executable R code.
#'
#' @param model A heRomodel object
#'
#' @return A character vector of R code lines
#'
#' @export
as_r_code <- function(model) {
  # This will call the code generation function from model_codegen.R
  model_to_r_code(model)
}