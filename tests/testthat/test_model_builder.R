context("Model builder")

library(testthat)
library(openqaly)

test_that("Model builder creates valid model structure", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 100, cycle_length = "year") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0)

  expect_s3_class(model, "oq_model")
  expect_equal(model$settings$model_type, "markov")
  expect_equal(nrow(model$states), 3)
  expect_equal(model$states$name, c("healthy", "sick", "dead"))
})

test_that("NSE works for transitions and values", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "sick", p_disease * 0.1) |>
    add_value("cost", base_cost + extra_cost, state = "sick")

  # Check that formulas are captured as strings
  expect_equal(model$transitions$formula[1], "p_disease * 0.1")
  expect_equal(model$values$formula[1], "base_cost + extra_cost")
})

test_that("Variables preserve all fields including pass-through", {
  model <- define_model("markov") |>
    add_variable("p_disease", 0.1,
                source = "Smith et al. (2020)",
                sampling = beta(10, 90))

  expect_equal(model$variables$source[1], "Smith et al. (2020)")
  expect_equal(model$variables$sampling[1], "beta(10, 90)")
})

test_that("Strategies and groups work correctly", {
  model <- define_model("markov") |>
    add_strategy("treatment_a") |>
    add_group("moderate", weight = "0.6") |>
    add_group("severe", weight = "0.4")

  expect_equal(model$groups$weight[1], "0.6")
  expect_equal(model$groups$weight[2], "0.4")
  expect_equal(nrow(model$groups), 2)
})

test_that("Round-trip conversion preserves model", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("jsonlite")

  # Create a model with various components
  original <- define_model("markov") |>
    set_settings(n_cycles = 100, cycle_length = "4", cycle_length_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.1) |>
    add_value("cost", 1000, state = "sick") |>
    add_variable("p_disease", 0.1, source = "Literature") |>
    add_strategy("treatment_a") |>
    add_group("young", weight = "0.6") |>
    add_group("elderly", weight = "0.4")

  # Test JSON round-trip
  json_str <- as_json(original)
  from_json <- read_model_json(json_str)

  expect_equal(original$settings$n_cycles, from_json$settings$n_cycles)
  expect_equal(nrow(original$states), nrow(from_json$states))
  expect_equal(original$variables$source, from_json$variables$source)

  # Test R code generation
  r_code <- as_r_code(original)
  expect_true(length(r_code) > 0)
  expect_true(any(grepl("define_model", r_code)))
  expect_true(any(grepl("add_state", r_code)))
})

test_that("Convert model function works", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("jsonlite")

  # Create a simple model
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0)

  # Test writing to different formats
  temp_dir <- tempdir()
  json_file <- file.path(temp_dir, "test_model.json")
  r_file <- file.path(temp_dir, "test_model.R")

  # Write as JSON
  write_model(model, json_file, format = "json")
  expect_true(file.exists(json_file))

  # Write as R code
  write_model(model, r_file, format = "r")
  expect_true(file.exists(r_file))

  # Clean up
  unlink(json_file)
  unlink(r_file)
})

test_that("PSM transitions work correctly", {
  model <- define_model("psm") |>
    add_psm_transition("death", "years", exp(-0.1 * time))

  expect_equal(model$transitions$endpoint[1], "death")
  expect_equal(model$transitions$time_unit[1], "years")
  expect_equal(model$transitions$formula[1], "exp(-0.1 * time)")
})

test_that("Complete PSM model can be built and executed", {
  # Build a complete PSM model programmatically
  model <- define_model("psm") |>
    set_settings(n_cycles = 24, cycle_length = 1, cycle_length_unit = "months") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_strategy("standard") |>
    add_strategy("intervention") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.1),
                strategy = "standard") |>
    add_variable("pfs_dist", apply_hr(define_surv_param("exp", rate = 0.1), 0.7),
                strategy = "intervention") |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.05),
                strategy = "standard") |>
    add_variable("os_dist", apply_hr(define_surv_param("exp", rate = 0.05), 0.8),
                strategy = "intervention") |>
    add_variable("u_pfs", 0.8) |>
    add_variable("u_prog", 0.6) |>
    add_variable("c_drug", 1000, strategy = "standard") |>
    add_variable("c_drug", 3000, strategy = "intervention") |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist) |>
    add_value("qalys", u_pfs, type = "outcome", state = "pfs") |>
    add_value("qalys", u_prog, type = "outcome", state = "progressed") |>
    add_value("cost", c_drug, type = "cost")

  # Verify model structure
  expect_s3_class(model, "oq_model")
  expect_equal(tolower(model$settings$model_type), "psm")
  expect_equal(nrow(model$states), 3)
  expect_equal(nrow(model$strategies), 2)

  # Run the model
  results <- run_model(model)

  # Verify results
  expect_equal(nrow(results$aggregated), 2)
  expect_setequal(results$aggregated$strategy, c("standard", "intervention"))

  # Verify traces exist and are valid
  # Use robust tolerance following R best practices
  tol <- 10 * sqrt(.Machine$double.eps)

  for (i in 1:2) {
    trace <- results$aggregated$collapsed_trace[[i]]
    state_trace <- get_state_columns(trace)

    expect_equal(nrow(trace), 25)  # n_cycles + 1
    expect_equal(ncol(state_trace), 3)   # 3 states

    # Verify probabilities sum to 1
    row_sums <- rowSums(state_trace)
    expect_true(all(abs(row_sums - 1.0) < tol))

    # Verify initial state (these work with or without time columns)
    expect_equal(trace[1, "pfs"], 1.0)
    expect_equal(trace[1, "progressed"], 0.0)
    expect_equal(trace[1, "dead"], 0.0)
  }

  # Verify intervention has better outcomes (lower death probability at end)
  standard_trace <- results$aggregated$collapsed_trace[[which(results$aggregated$strategy == "standard")]]
  intervention_trace <- results$aggregated$collapsed_trace[[which(results$aggregated$strategy == "intervention")]]

  expect_true(intervention_trace[25, "dead"] < standard_trace[25, "dead"],
              info = "Intervention should have lower death probability")

  # Verify intervention is more expensive
  standard_values <- results$aggregated$trace_and_values[[which(results$aggregated$strategy == "standard")]]$values
  intervention_values <- results$aggregated$trace_and_values[[which(results$aggregated$strategy == "intervention")]]$values

  standard_cost <- sum(standard_values[, "cost"])
  intervention_cost <- sum(intervention_values[, "cost"])

  expect_true(intervention_cost > standard_cost,
              info = "Intervention should be more expensive")
})

test_that("Path validation works correctly", {
  model <- define_model("markov")

  # Excel format should reject file paths
  expect_error(
    write_model(model, "test.xlsx", format = "excel"),
    "path must be a folder"
  )

  # JSON/R formats should reject folder paths
  expect_error(
    write_model(model, "test_folder/", format = "json"),
    "must include a filename"
  )

  expect_error(
    write_model(model, "test_folder/", format = "r"),
    "must include a filename"
  )
})

# Tests for variable display name validation
test_that("Variable display names are validated - R builder format", {
  library(tibble)

  # Test validation at the normalize stage (simulating what happens in run_model)
  # Valid: All display names provided for strategy-specific variable
  valid_model1 <- list(
    variables = tibble(
      name = c("cost", "cost"),
      formula = c("1000", "2000"),
      display_name = c("Drug Cost (Standard)", "Drug Cost (Intervention)"),
      description = c("Drug Cost (Standard)", "Drug Cost (Intervention)"),
      strategy = c("standard", "intervention"),
      group = c("", ""),
      source = c("", ""),
      sampling = c("", "")
    ),
    settings = list(model_type = "markov")
  )

  # Should work without error
  expect_no_error(openqaly:::normalize_and_validate_model(valid_model1))

  # Valid: No display names (all auto-generated)
  valid_model2 <- list(
    variables = tibble(
      name = c("cost", "cost"),
      formula = c("1000", "2000"),
      display_name = c("", ""),
      description = c("", ""),
      strategy = c("standard", "intervention"),
      group = c("", ""),
      source = c("", ""),
      sampling = c("", "")
    ),
    settings = list(model_type = "markov")
  )

  expect_no_error(openqaly:::normalize_and_validate_model(valid_model2))

  # Invalid: Only one strategy has display name
  invalid_model <- list(
    variables = tibble(
      name = c("cost", "cost"),
      formula = c("1000", "2000"),
      display_name = c("Drug Cost (Standard)", ""),
      description = c("", ""),
      strategy = c("standard", "intervention"),
      group = c("", ""),
      source = c("", ""),
      sampling = c("", "")
    ),
    settings = list(model_type = "markov")
  )

  expect_error(
    openqaly:::normalize_and_validate_model(invalid_model),
    "Variable 'cost'.*display_name must be provided for ALL definitions or NONE"
  )
})

test_that("R builder catches mixed auto-generated and custom display names immediately", {
  # This is the edge case from scratch.R - first variable has no display_name,
  # subsequent variables have custom display names
  expect_error({
    define_model("markov") |>
      add_variable(name = 'p_well_to_sick', strategy = 'treatment_a',
                   group = 'group_1', formula = 0.3) |>  # No display_name
      add_variable(name = 'p_well_to_sick', display_name = "Custom Name",
                   strategy = 'treatment_b', group = 'group_1', formula = 0.18)
  }, "Variable 'p_well_to_sick'.*display_name must be provided for ALL definitions or NONE")

  # Reverse order - custom first, then auto-generated
  expect_error({
    define_model("markov") |>
      add_variable(name = 'p_well_to_sick', display_name = "Custom Name",
                   strategy = 'treatment_a', group = 'group_1', formula = 0.3) |>
      add_variable(name = 'p_well_to_sick', strategy = 'treatment_b',
                   group = 'group_1', formula = 0.18)  # No display_name
  }, "Variable 'p_well_to_sick'.*display_name must be provided for ALL definitions or NONE")

  # Three variables - error thrown when second is added (before third)
  expect_error({
    define_model("markov") |>
      add_variable(name = 'cost', strategy = 's1', formula = 100) |>
      add_variable(name = 'cost', display_name = "Cost S2", strategy = 's2', formula = 200) |>
      add_variable(name = 'cost', strategy = 's3', formula = 300)
  }, "Variable 'cost'.*display_name must be provided for ALL definitions or NONE")
})

test_that("Variable display names are validated - Direct dataframe (simulating Excel/JSON)", {
  library(tibble)
  library(dplyr)

  # Valid: All display names provided
  valid_vars1 <- tibble(
    name = c("cost", "cost"),
    formula = c("1000", "2000"),
    display_name = c("Drug Cost (Standard)", "Drug Cost (Intervention)"),
    description = c("", ""),
    strategy = c("standard", "intervention"),
    group = c("", ""),
    source = c("", ""),
    sampling = c("", "")
  )

  spec <- read.csv(system.file('model_input_specs/variables.csv', package = 'openqaly'))
  expect_no_error(openqaly:::check_tbl(valid_vars1, spec, "Variables"))

  # Valid: No display names (will be auto-generated)
  valid_vars2 <- tibble(
    name = c("cost", "cost"),
    formula = c("1000", "2000"),
    display_name = c("", ""),
    description = c("", ""),
    strategy = c("standard", "intervention"),
    group = c("", ""),
    source = c("", ""),
    sampling = c("", "")
  )

  expect_no_error(openqaly:::check_tbl(valid_vars2, spec, "Variables"))

  # Invalid: Partial display names for strategy-specific variable
  invalid_vars1 <- tibble(
    name = c("cost", "cost"),
    formula = c("1000", "2000"),
    display_name = c("Drug Cost (Standard)", ""),
    description = c("", ""),
    strategy = c("standard", "intervention"),
    group = c("", ""),
    source = c("", ""),
    sampling = c("", "")
  )

  expect_error(
    openqaly:::check_tbl(invalid_vars1, spec, "Variables"),
    "Variable 'cost'.*display_name must be provided for ALL definitions or NONE"
  )

  # Invalid: Partial display names for group-specific variable
  invalid_vars2 <- tibble(
    name = c("utility", "utility"),
    formula = c("0.8", "0.7"),
    display_name = c("", "Utility (Elderly)"),
    description = c("", ""),
    strategy = c("", ""),
    group = c("young", "elderly"),
    source = c("", ""),
    sampling = c("", "")
  )

  expect_error(
    openqaly:::check_tbl(invalid_vars2, spec, "Variables"),
    "Variable 'utility'.*display_name must be provided for ALL definitions or NONE"
  )

  # Invalid: Partial display names with both strategy and group
  invalid_vars3 <- tibble(
    name = c("param", "param", "param"),
    formula = c("1", "2", "3"),
    display_name = c("Param A", "", "Param C"),
    description = c("", "", ""),
    strategy = c("s1", "s2", "s1"),
    group = c("g1", "g1", "g2"),
    source = c("", "", ""),
    sampling = c("", "", "")
  )

  expect_error(
    openqaly:::check_tbl(invalid_vars3, spec, "Variables"),
    "Variable 'param'.*display_name must be provided for ALL definitions or NONE"
  )
})

test_that("Variable display names validation - edge cases", {
  library(tibble)

  spec <- read.csv(system.file('model_input_specs/variables.csv', package = 'openqaly'))

  # Valid: Single variable (no consistency check needed)
  single_var <- tibble(
    name = "cost",
    formula = "1000",
    display_name = "",
    description = "",
    strategy = "",
    group = "",
    source = "",
    sampling = ""
  )

  expect_no_error(openqaly:::check_tbl(single_var, spec, "Variables"))

  # Valid: Multiple different variables, each with consistent display names
  multi_vars <- tibble(
    name = c("cost", "cost", "utility", "utility"),
    formula = c("1000", "2000", "0.8", "0.7"),
    display_name = c("Cost A", "Cost B", "", ""),
    description = c("", "", "", ""),
    strategy = c("s1", "s2", "s1", "s2"),
    group = c("", "", "", ""),
    source = c("", "", "", ""),
    sampling = c("", "", "", "")
  )

  expect_no_error(openqaly:::check_tbl(multi_vars, spec, "Variables"))

  # Valid: NA display names (treated as missing)
  na_vars <- tibble(
    name = c("cost", "cost"),
    formula = c("1000", "2000"),
    display_name = c(NA_character_, NA_character_),
    description = c("", ""),
    strategy = c("s1", "s2"),
    group = c("", ""),
    source = c("", ""),
    sampling = c("", "")
  )

  expect_no_error(openqaly:::check_tbl(na_vars, spec, "Variables"))
})