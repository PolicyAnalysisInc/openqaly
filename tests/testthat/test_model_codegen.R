context("Model codegen")

# =============================================================================
# model_to_r_code() - Main Function Tests
# =============================================================================

test_that("model_to_r_code generates valid R code with header", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0)

  code <- model_to_r_code(model)

  expect_true(is.character(code))
  expect_true(length(code) > 0)
  expect_true(any(grepl("^# Generated openqaly model code$", code)))
  expect_true(any(grepl("^# Created:", code)))
  expect_true(any(grepl("^library\\(openqaly\\)$", code)))
  expect_true(any(grepl('define_model\\("markov"\\)', code)))
})

test_that("model_to_r_code writes to file when path provided", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1)

  temp_file <- tempfile(fileext = ".R")
  on.exit(unlink(temp_file), add = TRUE)

  result <- model_to_r_code(model, file = temp_file)

  expect_true(file.exists(temp_file))
  file_contents <- readLines(temp_file)
  expect_true(any(grepl("define_model", file_contents)))
})

test_that("model_to_r_code returns invisible character vector", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1)

  result <- model_to_r_code(model)
  expect_true(is.character(result))
})

test_that("model_to_r_code removes trailing pipe from last line", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1)

  code <- model_to_r_code(model)
  last_line <- code[length(code)]

  expect_false(grepl("\\|>$", last_line))
})

test_that("model_to_r_code handles PSM model type", {
  model <- define_model("psm") |>
    add_state("pfs") |>
    add_state("dead") |>
    add_psm_transition("death", "years", exp(-0.1 * time))

  code <- model_to_r_code(model)

  expect_true(any(grepl('define_model\\("psm"\\)', code)))
  expect_true(any(grepl("add_psm_transition", code)))
})

test_that("model_to_r_code handles empty model components", {
  model <- define_model("markov")
  model$states <- NULL
  model$variables <- NULL
  model$transitions <- NULL

  code <- model_to_r_code(model)

  expect_true(is.character(code))
  expect_true(any(grepl("define_model", code)))
})

# =============================================================================
# generate_settings_code() Tests
# =============================================================================

test_that("generate_settings_code handles numeric settings", {
  settings <- list(model_type = "markov", n_cycles = 100, cycle_length = 1)
  code <- openqaly:::generate_settings_code(settings)

  expect_true(any(grepl("n_cycles = 100", code)))
  expect_true(any(grepl("cycle_length = 1", code)))
})

test_that("generate_settings_code handles string settings", {
  settings <- list(model_type = "markov", cycle_length_unit = "years")
  code <- openqaly:::generate_settings_code(settings)

  expect_true(any(grepl('cycle_length_unit = "years"', code)))
})

test_that("generate_settings_code handles boolean settings", {
  settings <- list(model_type = "markov", reduce_state_cycle = "true")
  code <- openqaly:::generate_settings_code(settings)

  expect_true(any(grepl("reduce_state_cycle = TRUE", code)))
})

test_that("generate_settings_code returns empty for empty settings", {
  code <- openqaly:::generate_settings_code(list())
  expect_equal(length(code), 0)

  code <- openqaly:::generate_settings_code(NULL)
  expect_equal(length(code), 0)
})

test_that("generate_settings_code filters out model_type", {
  settings <- list(model_type = "markov", n_cycles = 10)
  code <- openqaly:::generate_settings_code(settings)

  expect_false(any(grepl("model_type", code)))
})

test_that("generate_settings_code returns empty when only model_type present", {
  settings <- list(model_type = "markov")
  code <- openqaly:::generate_settings_code(settings)

  expect_equal(length(code), 0)
})

# =============================================================================
# generate_states_code() Tests
# =============================================================================

test_that("generate_states_code handles basic state", {
  states <- tibble::tibble(
    name = "healthy",
    initial_probability = "1"
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl('add_state\\("healthy", initial_prob = 1\\)', code)))
})

test_that("generate_states_code handles display_name different from name", {
  states <- tibble::tibble(
    name = "healthy",
    initial_probability = "1",
    display_name = "Healthy State"
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl('display_name = "Healthy State"', code)))
})

test_that("generate_states_code handles description different from name and display_name", {
  states <- tibble::tibble(
    name = "healthy",
    initial_probability = "1",
    display_name = "Healthy",
    description = "A healthy patient state"
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl('description = "A healthy patient state"', code)))
})

test_that("generate_states_code handles state_group", {
  states <- tibble::tibble(
    name = "healthy",
    initial_probability = "1",
    state_group = "alive_states"
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl('state_group = "alive_states"', code)))
})

test_that("generate_states_code handles share_state_time", {
  states <- tibble::tibble(
    name = "healthy",
    initial_probability = "1",
    share_state_time = TRUE
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl("share_state_time = TRUE", code)))
})

test_that("generate_states_code handles state_cycle_limit", {
  states <- tibble::tibble(
    name = "treatment",
    initial_probability = "0",
    state_cycle_limit = 12
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl("state_cycle_limit = 12", code)))
})

test_that("generate_states_code handles state_cycle_limit_unit non-default", {
  states <- tibble::tibble(
    name = "treatment",
    initial_probability = "0",
    state_cycle_limit = 12,
    state_cycle_limit_unit = "months"
  )
  code <- openqaly:::generate_states_code(states)

  expect_true(any(grepl('state_cycle_limit_unit = "months"', code)))
})

test_that("generate_states_code returns empty for empty states", {
  code <- openqaly:::generate_states_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_states_code(tibble::tibble())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_transitions_code() Tests
# =============================================================================

test_that("generate_transitions_code handles Markov transitions", {
  transitions <- tibble::tibble(
    from_state = "healthy",
    to_state = "sick",
    formula = "0.1"
  )
  code <- openqaly:::generate_transitions_code(transitions, is_psm = FALSE)

  expect_true(any(grepl('add_transition\\("healthy", "sick", 0.1\\)', code)))
})

test_that("generate_transitions_code handles PSM transitions", {
  transitions <- tibble::tibble(
    endpoint = "death",
    time_unit = "years",
    formula = "exp(-0.1 * time)"
  )
  code <- openqaly:::generate_transitions_code(transitions, is_psm = TRUE)

  expect_true(any(grepl('add_psm_transition\\("death", "years", exp\\(-0.1 \\* time\\)\\)', code)))
})

test_that("generate_transitions_code returns empty for empty transitions", {
  code <- openqaly:::generate_transitions_code(NULL, is_psm = FALSE)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_transitions_code(tibble::tibble(), is_psm = FALSE)
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_values_code() Tests
# =============================================================================

test_that("generate_values_code handles basic value", {
  values <- tibble::tibble(
    name = "cost",
    formula = "1000"
  )
  code <- openqaly:::generate_values_code(values)

  expect_true(any(grepl('add_value\\("cost", 1000\\)', code)))
})

test_that("generate_values_code handles state specification", {
  values <- tibble::tibble(
    name = "cost",
    formula = "1000",
    state = "healthy"
  )
  code <- openqaly:::generate_values_code(values)

  expect_true(any(grepl('state = "healthy"', code)))
})

test_that("generate_values_code handles destination specification", {
  values <- tibble::tibble(
    name = "transition_cost",
    formula = "500",
    state = "",
    destination = "sick"
  )
  code <- openqaly:::generate_values_code(values)

  expect_true(any(grepl('destination = "sick"', code)))
})

test_that("generate_values_code handles display_name different from name", {
  values <- tibble::tibble(
    name = "cost",
    formula = "1000",
    display_name = "Treatment Cost"
  )
  code <- openqaly:::generate_values_code(values)

  expect_true(any(grepl('display_name = "Treatment Cost"', code)))
})

test_that("generate_values_code handles description different from name and display_name", {
  values <- tibble::tibble(
    name = "cost",
    formula = "1000",
    display_name = "Cost",
    description = "Total treatment cost per cycle"
  )
  code <- openqaly:::generate_values_code(values)

  expect_true(any(grepl('description = "Total treatment cost per cycle"', code)))
})

test_that("generate_values_code handles type non-default", {
  values <- tibble::tibble(
    name = "cost",
    formula = "1000",
    type = "cost"
  )
  code <- openqaly:::generate_values_code(values)

  expect_true(any(grepl('type = "cost"', code)))
})

test_that("generate_values_code returns empty for empty values", {
  code <- openqaly:::generate_values_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_values_code(tibble::tibble())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_variables_code() Tests
# =============================================================================

test_that("generate_variables_code handles basic variable", {
  variables <- tibble::tibble(
    name = "p_sick",
    formula = "0.1"
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl('add_variable\\("p_sick", 0.1\\)', code)))
})

test_that("generate_variables_code handles display_name", {
  variables <- tibble::tibble(
    name = "p_sick",
    formula = "0.1",
    display_name = "Probability of Sickness"
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl('display_name = "Probability of Sickness"', code)))
})

test_that("generate_variables_code handles description", {
  variables <- tibble::tibble(
    name = "p_sick",
    formula = "0.1",
    display_name = "P(sick)",
    description = "Annual probability of becoming sick"
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl('description = "Annual probability of becoming sick"', code)))
})

test_that("generate_variables_code handles strategy specification", {
  variables <- tibble::tibble(
    name = "cost",
    formula = "1000",
    strategy = "treatment_a"
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl('strategy = "treatment_a"', code)))
})

test_that("generate_variables_code handles group specification", {
  variables <- tibble::tibble(
    name = "utility",
    formula = "0.8",
    group = "elderly"
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl('group = "elderly"', code)))
})

test_that("generate_variables_code handles source with quotes", {
  variables <- tibble::tibble(
    name = "p_sick",
    formula = "0.1",
    source = 'Smith et al. "Study" (2020)'
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl("source = ", code)))
})

test_that("generate_variables_code handles sampling specification", {
  variables <- tibble::tibble(
    name = "p_sick",
    formula = "0.1",
    sampling = "normal(0.1, 0.02)"
  )
  code <- openqaly:::generate_variables_code(variables)

  expect_true(any(grepl("sampling = normal\\(0.1, 0.02\\)", code)))
})

test_that("generate_variables_code returns empty for empty variables", {
  code <- openqaly:::generate_variables_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_variables_code(tibble::tibble())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_strategies_code() Tests
# =============================================================================

test_that("generate_strategies_code handles basic strategy", {
  strategies <- tibble::tibble(
    name = "standard"
  )
  code <- openqaly:::generate_strategies_code(strategies)

  expect_true(any(grepl('add_strategy\\("standard"\\)', code)))
})

test_that("generate_strategies_code handles display_name", {
  strategies <- tibble::tibble(
    name = "treatment_a",
    display_name = "Treatment A"
  )
  code <- openqaly:::generate_strategies_code(strategies)

  expect_true(any(grepl('display_name = "Treatment A"', code)))
})

test_that("generate_strategies_code handles description", {
  strategies <- tibble::tibble(
    name = "treatment_a",
    display_name = "Tx A",
    description = "First line treatment option"
  )
  code <- openqaly:::generate_strategies_code(strategies)

  expect_true(any(grepl('description = "First line treatment option"', code)))
})

test_that("generate_strategies_code handles enabled not equal to 1", {
  strategies <- tibble::tibble(
    name = "disabled_strategy",
    enabled = 0
  )
  code <- openqaly:::generate_strategies_code(strategies)

  expect_true(any(grepl("enabled = 0", code)))
})

test_that("generate_strategies_code returns empty for empty strategies", {
  code <- openqaly:::generate_strategies_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_strategies_code(tibble::tibble())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_groups_code() Tests
# =============================================================================

test_that("generate_groups_code handles basic group", {
  groups <- tibble::tibble(
    name = "all_patients"
  )
  code <- openqaly:::generate_groups_code(groups)

  expect_true(any(grepl('add_group\\("all_patients"\\)', code)))
})

test_that("generate_groups_code handles display_name", {
  groups <- tibble::tibble(
    name = "elderly",
    display_name = "Elderly Patients"
  )
  code <- openqaly:::generate_groups_code(groups)

  expect_true(any(grepl('display_name = "Elderly Patients"', code)))
})

test_that("generate_groups_code handles description", {
  groups <- tibble::tibble(
    name = "elderly",
    display_name = "Elderly",
    description = "Patients aged 65 and older"
  )
  code <- openqaly:::generate_groups_code(groups)

  expect_true(any(grepl('description = "Patients aged 65 and older"', code)))
})

test_that("generate_groups_code handles numeric weight", {
  groups <- tibble::tibble(
    name = "elderly",
    weight = "0.4"
  )
  code <- openqaly:::generate_groups_code(groups)

  expect_true(any(grepl("weight = 0.4", code)))
})

test_that("generate_groups_code handles expression weight", {
  groups <- tibble::tibble(
    name = "elderly",
    weight = "proportion_elderly"
  )
  code <- openqaly:::generate_groups_code(groups)

  expect_true(any(grepl('weight = "proportion_elderly"', code)))
})

test_that("generate_groups_code handles enabled not equal to 1", {
  groups <- tibble::tibble(
    name = "disabled_group",
    enabled = 0
  )
  code <- openqaly:::generate_groups_code(groups)

  expect_true(any(grepl("enabled = 0", code)))
})

test_that("generate_groups_code returns empty for empty groups", {
  code <- openqaly:::generate_groups_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_groups_code(tibble::tibble())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_summaries_code() Tests
# =============================================================================

test_that("generate_summaries_code handles basic summary", {
  summaries <- tibble::tibble(
    name = "total_cost",
    values = "cost"
  )
  code <- openqaly:::generate_summaries_code(summaries)

  expect_true(any(grepl('add_summary\\("total_cost", "cost"\\)', code)))
})

test_that("generate_summaries_code handles display_name", {
  summaries <- tibble::tibble(
    name = "total_cost",
    values = "cost",
    display_name = "Total Cost"
  )
  code <- openqaly:::generate_summaries_code(summaries)

  expect_true(any(grepl('display_name = "Total Cost"', code)))
})

test_that("generate_summaries_code handles description", {
  summaries <- tibble::tibble(
    name = "total_cost",
    values = "cost",
    display_name = "Cost",
    description = "Sum of all costs"
  )
  code <- openqaly:::generate_summaries_code(summaries)

  expect_true(any(grepl('description = "Sum of all costs"', code)))
})

test_that("generate_summaries_code returns empty for empty summaries", {
  code <- openqaly:::generate_summaries_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_summaries_code(tibble::tibble())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_multivariate_sampling_code() Tests
# =============================================================================

test_that("generate_multivariate_sampling_code handles character vector variables", {
  mv_sampling <- list(
    list(
      name = "joint_params",
      distribution = "mvnorm(mu, sigma)",
      variables = c("p_sick", "p_death")
    )
  )
  code <- openqaly:::generate_multivariate_sampling_code(mv_sampling)

  expect_true(any(grepl('name = "joint_params"', code)))
  expect_true(any(grepl('variables = c\\("p_sick", "p_death"\\)', code)))
})

test_that("generate_multivariate_sampling_code handles simple tibble variables", {
  mv_sampling <- list(
    list(
      name = "joint_params",
      distribution = "mvnorm(mu, sigma)",
      variables = tibble::tibble(
        variable = c("p_sick", "p_death"),
        strategy = c("", ""),
        group = c("", "")
      )
    )
  )
  code <- openqaly:::generate_multivariate_sampling_code(mv_sampling)

  expect_true(any(grepl('variables = c\\("p_sick", "p_death"\\)', code)))
})

test_that("generate_multivariate_sampling_code handles complex tibble with strategy/group", {
  mv_sampling <- list(
    list(
      name = "joint_params",
      distribution = "mvnorm(mu, sigma)",
      variables = tibble::tibble(
        variable = c("cost", "cost"),
        strategy = c("standard", "intervention"),
        group = c("", "")
      )
    )
  )
  code <- openqaly:::generate_multivariate_sampling_code(mv_sampling)

  expect_true(any(grepl("variables = ", code)))
  expect_true(any(grepl("tibble", code)))
})

test_that("generate_multivariate_sampling_code handles description", {
  mv_sampling <- list(
    list(
      name = "joint_params",
      distribution = "mvnorm(mu, sigma)",
      variables = c("p_sick"),
      description = "Correlated parameters"
    )
  )
  code <- openqaly:::generate_multivariate_sampling_code(mv_sampling)

  expect_true(any(grepl('description = "Correlated parameters"', code)))
})

test_that("generate_multivariate_sampling_code returns empty for empty input", {
  code <- openqaly:::generate_multivariate_sampling_code(NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_multivariate_sampling_code(list())
  expect_equal(length(code), 0)
})

# =============================================================================
# format_tribble() Tests
# =============================================================================

test_that("format_tribble handles empty dataframe", {
  df <- data.frame()
  code <- openqaly:::format_tribble(df)

  expect_equal(code, "tibble::tibble()")
})

test_that("format_tribble handles character columns", {
  df <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  code <- openqaly:::format_tribble(df)

  expect_true(any(grepl('"a"', code)))
  expect_true(any(grepl('"b"', code)))
})

test_that("format_tribble handles logical columns", {
  df <- data.frame(flag = c(TRUE, FALSE))
  code <- openqaly:::format_tribble(df)

  expect_true(any(grepl("TRUE", code)))
  expect_true(any(grepl("FALSE", code)))
})

test_that("format_tribble handles numeric columns", {
  df <- data.frame(value = c(1.5, 2.5))
  code <- openqaly:::format_tribble(df)

  expect_true(any(grepl("1.5", code)))
  expect_true(any(grepl("2.5", code)))
})

test_that("format_tribble handles column names with special characters", {
  df <- data.frame(x = 1)
  names(df) <- "my column"
  code <- openqaly:::format_tribble(df)

  expect_true(any(grepl("`my column`", code)))
})

test_that("format_tribble handles column names starting with number", {
  df <- data.frame(x = 1)
  names(df) <- "1col"
  code <- openqaly:::format_tribble(df)

  expect_true(any(grepl("`1col`", code)))
})

test_that("format_tribble handles multi-row dataframe with proper formatting", {
  df <- data.frame(
    name = c("a", "b", "c"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  code <- openqaly:::format_tribble(df)

  expect_true(any(grepl("tibble::tribble\\(", code)))
  expect_true(any(grepl("~name", code)))
  expect_true(any(grepl("~value", code)))
  expect_true(any(grepl("\\)$", code)))
})

# =============================================================================
# generate_tables_code() Tests
# =============================================================================

test_that("generate_tables_code handles single table", {
  model <- list()
  tables <- list(
    lookup = data.frame(
      age = c(50, 60),
      rate = c(0.1, 0.2),
      stringsAsFactors = FALSE
    )
  )
  code <- openqaly:::generate_tables_code(model, tables)

  expect_true(any(grepl("# Add tables", code)))
  expect_true(any(grepl("# Table: lookup", code)))
  expect_true(any(grepl("lookup_data", code)))
  expect_true(any(grepl('add_table\\(model, "lookup"', code)))
})

test_that("generate_tables_code handles multiple tables", {
  model <- list()
  tables <- list(
    rates = data.frame(age = 50, rate = 0.1),
    costs = data.frame(item = "drug", cost = 100)
  )
  code <- openqaly:::generate_tables_code(model, tables)

  expect_true(any(grepl("# Table: rates", code)))
  expect_true(any(grepl("# Table: costs", code)))
})

test_that("generate_tables_code returns empty for empty tables", {
  code <- openqaly:::generate_tables_code(list(), NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_tables_code(list(), list())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_scripts_code() Tests
# =============================================================================

test_that("generate_scripts_code handles basic script", {
  model <- list()
  scripts <- list(
    setup = "x <- 1"
  )
  code <- openqaly:::generate_scripts_code(model, scripts)

  expect_true(any(grepl("# Add scripts", code)))
  expect_true(any(grepl("# Script: setup", code)))
  expect_true(any(grepl("setup_code", code)))
  expect_true(any(grepl('add_script\\(model, "setup"', code)))
})

test_that("generate_scripts_code handles backslash escaping", {
  model <- list()
  scripts <- list(
    regex = "pattern <- '\\\\d+'"
  )
  code <- openqaly:::generate_scripts_code(model, scripts)

  expect_true(length(code) > 0)
})

test_that("generate_scripts_code handles quote escaping", {
  model <- list()
  scripts <- list(
    quoted = 'msg <- "hello"'
  )
  code <- openqaly:::generate_scripts_code(model, scripts)

  expect_true(length(code) > 0)
})

test_that("generate_scripts_code returns empty for empty scripts", {
  code <- openqaly:::generate_scripts_code(list(), NULL)
  expect_equal(length(code), 0)

  code <- openqaly:::generate_scripts_code(list(), list())
  expect_equal(length(code), 0)
})

# =============================================================================
# generate_trees_code() Tests
# =============================================================================

test_that("generate_trees_code returns empty for NULL trees", {
  code <- openqaly:::generate_trees_code(list(), NULL)
  expect_equal(length(code), 0)
})

test_that("generate_trees_code returns empty for empty data.frame trees", {
  code <- openqaly:::generate_trees_code(list(), data.frame())
  expect_equal(length(code), 0)
})

test_that("generate_trees_code returns empty for empty list trees", {
  code <- openqaly:::generate_trees_code(list(), list())
  expect_equal(length(code), 0)
})

test_that("generate_trees_code returns empty for unknown format", {
  code <- openqaly:::generate_trees_code(list(), "invalid")
  expect_equal(length(code), 0)
})

test_that("generate_trees_code returns comment for non-empty data.frame trees", {
  trees_df <- data.frame(node = "root", value = 1)
  code <- openqaly:::generate_trees_code(list(), trees_df)

  expect_true(any(grepl("Decision trees are parsed", code)))
})

test_that("generate_trees_code returns comment for non-empty list trees", {
  trees_list <- list(tree1 = list(node = "root"))
  code <- openqaly:::generate_trees_code(list(), trees_list)

  expect_true(any(grepl("Decision trees are parsed", code)))
})

# =============================================================================
# Integration Tests - Full Model Code Generation
# =============================================================================

test_that("model_to_r_code generates complete valid code for complex model", {
  model <- build_simple_psa_model()
  code <- model_to_r_code(model)

  # Verify all major sections are present
  expect_true(any(grepl("define_model", code)))
  expect_true(any(grepl("set_settings", code)))
  expect_true(any(grepl("add_strategy", code)))
  expect_true(any(grepl("add_state", code)))
  expect_true(any(grepl("add_variable", code)))
  expect_true(any(grepl("add_transition", code)))
  expect_true(any(grepl("add_value", code)))
  expect_true(any(grepl("add_summary", code)))
})

test_that("model_to_r_code generates syntactically valid R code", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("dead", "dead", 1)

  code <- model_to_r_code(model)
  code_str <- paste(code, collapse = "\n")

  # This should parse without error
  expect_no_error(parse(text = code_str))
})
