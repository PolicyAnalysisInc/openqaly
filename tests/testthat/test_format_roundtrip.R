context("Format round-trip tests")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Create comprehensive test model
create_test_model <- function() {
  define_model("markov") |>
    set_settings(
      timeframe = 100,
      cycle_length = 1,
      discount_cost = 3,
      discount_outcomes = 3.5
    ) |>
    add_strategy("treatment_a", display_name = "Treatment A") |>
    add_strategy("treatment_b", display_name = "Treatment B") |>
    add_group("general", weight = "1") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_disease", 0.03) |>
    add_variable("efficacy", 0.4, strategy = "treatment_a") |>
    add_variable("efficacy", 0.2, strategy = "treatment_b") |>
    add_variable("utility_healthy", 1) |>
    add_variable("utility_sick", 0.7) |>
    add_variable("cost_treat", 5000) |>
    add_transition("healthy", "sick", p_disease * (1 - efficacy)) |>
    add_transition("healthy", "dead", 0.01) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.1) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("qaly", utility_healthy, state = "healthy", type = "outcome") |>
    add_value("qaly", utility_sick, state = "sick", type = "outcome") |>
    add_value("cost", cost_treat, state = "sick", type = "cost") |>
    add_summary("total_qaly", "qaly") |>
    add_summary("total_cost", "cost")
}

#' Create test model with tables and scripts
create_model_with_tables <- function() {
  create_test_model() |>
    add_table("lookup", data.frame(
      age = c(0, 1, 2),
      rate = c(0.1, 0.2, 0.3)
    ), description = "Age-specific rates") |>
    add_script("preprocess", "# Preprocessing\nx <- 1",
               description = "Setup script")
}

#' Create test model with sensitivity analysis
create_model_with_sa <- function() {
  create_test_model() |>
    add_dsa_variable("p_disease", low = bc * 0.5, high = bc * 1.5) |>
    add_dsa_setting("discount_cost", low = 0, high = 5) |>
    add_scenario("Optimistic") |>
    add_scenario_variable("Optimistic", "p_disease", 0.02) |>
    add_scenario_setting("Optimistic", "timeframe", 120) |>
    add_twsa("Cost vs Efficacy") |>
    add_twsa_variable("Cost vs Efficacy", "p_disease",
                      type = "range", min = 0.01, max = 0.05, steps = 5) |>
    add_twsa_variable("Cost vs Efficacy", "efficacy",
                      type = "range", min = 0.2, max = 0.6, steps = 5,
                      strategy = "treatment_a")
}

# ==============================================================================
# JSON Round-Trip Tests
# ==============================================================================

test_that("basic model survives JSON round-trip", {
  model <- create_test_model()

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_s3_class(model_back, "oq_model")
  expect_equal(nrow(model$states), nrow(model_back$states))
  expect_equal(nrow(model$transitions), nrow(model_back$transitions))
  expect_equal(nrow(model$variables), nrow(model_back$variables))
  expect_equal(nrow(model$strategies), nrow(model_back$strategies))
})

test_that("model with tables/scripts survives JSON round-trip", {
  model <- create_model_with_tables()

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(names(model$tables), names(model_back$tables))
  expect_equal(names(model$scripts), names(model_back$scripts))

  # Verify table data
  tbl_orig <- model$tables[["lookup"]]$data
  tbl_back <- model_back$tables[["lookup"]]$data
  expect_equal(nrow(tbl_orig), nrow(tbl_back))

  # Verify descriptions
  expect_equal(model_back$tables[["lookup"]]$description, "Age-specific rates")
  expect_equal(model_back$scripts[["preprocess"]]$description, "Setup script")
})

test_that("model with sensitivity analysis survives JSON round-trip", {
  model <- create_model_with_sa()

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(length(model$dsa_parameters), length(model_back$dsa_parameters))
  expect_equal(length(model$scenarios), length(model_back$scenarios))
  expect_equal(length(model$twsa_analyses), length(model_back$twsa_analyses))
})

# ==============================================================================
# YAML Round-Trip Tests
# ==============================================================================

test_that("basic model survives YAML round-trip", {
  model <- create_test_model()

  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)

  expect_s3_class(model_back, "oq_model")
  expect_equal(nrow(model$states), nrow(model_back$states))
  expect_equal(nrow(model$transitions), nrow(model_back$transitions))
  expect_equal(nrow(model$variables), nrow(model_back$variables))

  unlink(yaml_path)
})

test_that("model with tables/scripts survives YAML round-trip", {
  model <- create_model_with_tables()

  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)

  expect_equal(names(model$tables), names(model_back$tables))
  expect_equal(names(model$scripts), names(model_back$scripts))

  # Verify descriptions
  expect_equal(model_back$tables[["lookup"]]$description, "Age-specific rates")
  expect_equal(model_back$scripts[["preprocess"]]$description, "Setup script")

  unlink(yaml_path)
})

test_that("model with sensitivity analysis survives YAML round-trip", {
  model <- create_model_with_sa()

  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)

  expect_equal(length(model$dsa_parameters), length(model_back$dsa_parameters))
  expect_equal(length(model$scenarios), length(model_back$scenarios))
  expect_equal(length(model$twsa_analyses), length(model_back$twsa_analyses))

  unlink(yaml_path)
})

# ==============================================================================
# R Code Round-Trip Tests
# ==============================================================================

test_that("basic model survives R code round-trip", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_death", 0.05) |>
    add_transition("alive", "dead", p_death) |>
    add_transition("alive", "alive", 1 - p_death) |>
    add_transition("dead", "dead", 1)

  r_path <- tempfile(fileext = ".R")
  write_model(model, r_path, format = "r")

  # Execute generated R code
  env <- new.env()
  source(r_path, local = env)

  # Find the model object
  model_names <- ls(env)[sapply(ls(env), function(x) inherits(env[[x]], "oq_model"))]
  expect_true(length(model_names) >= 1)

  model_back <- env[[model_names[1]]]

  expect_s3_class(model_back, "oq_model")
  expect_equal(nrow(model$states), nrow(model_back$states))
  expect_equal(nrow(model$transitions), nrow(model_back$transitions))

  unlink(r_path)
})

# ==============================================================================
# Cross-Format Round-Trip Tests
# ==============================================================================

test_that("JSON -> YAML -> JSON preserves model", {
  model <- create_test_model()

  # JSON -> YAML
  json1 <- write_model_json(model)
  yaml_path <- tempfile(fileext = ".yaml")
  convert_model(json1, yaml_path, from = "json", to = "yaml")

  # YAML -> JSON
  json_path <- tempfile(fileext = ".json")
  convert_model(yaml_path, json_path)

  model_back <- read_model_json(paste(readLines(json_path), collapse = "\n"))

  expect_equal(nrow(model$states), nrow(model_back$states))
  expect_equal(nrow(model$transitions), nrow(model_back$transitions))
  expect_equal(nrow(model$variables), nrow(model_back$variables))

  unlink(c(yaml_path, json_path))
})

test_that("YAML -> JSON -> YAML preserves model", {
  model <- create_test_model()

  # Model -> YAML
  yaml1 <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml1)

  # YAML -> JSON
  json_path <- tempfile(fileext = ".json")
  convert_model(yaml1, json_path)

  # JSON -> YAML
  yaml2 <- tempfile(fileext = ".yaml")
  convert_model(json_path, yaml2, to = "yaml")

  model_back <- read_model_yaml(yaml2)

  expect_equal(nrow(model$states), nrow(model_back$states))
  expect_equal(nrow(model$transitions), nrow(model_back$transitions))
  expect_equal(nrow(model$variables), nrow(model_back$variables))

  unlink(c(yaml1, yaml2, json_path))
})

# ==============================================================================
# Table/Script Description Cross-Format Tests
# ==============================================================================

test_that("table descriptions preserved across JSON -> YAML", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_table("test", data.frame(x = 1:3),
              description = "Test table description")

  # JSON
  json <- write_model_json(model)
  model_json <- read_model_json(json)
  expect_equal(model_json$tables[["test"]]$description, "Test table description")

  # JSON -> YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model_json, yaml_path)
  model_yaml <- read_model_yaml(yaml_path)
  expect_equal(model_yaml$tables[["test"]]$description, "Test table description")

  unlink(yaml_path)
})

test_that("script descriptions preserved across JSON -> YAML", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_script("test", "x <- 1",
               description = "Test script description")

  # JSON
  json <- write_model_json(model)
  model_json <- read_model_json(json)
  expect_equal(model_json$scripts[["test"]]$description, "Test script description")

  # JSON -> YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model_json, yaml_path)
  model_yaml <- read_model_yaml(yaml_path)
  expect_equal(model_yaml$scripts[["test"]]$description, "Test script description")

  unlink(yaml_path)
})

# ==============================================================================
# Edge Case Tests
# ==============================================================================

test_that("empty model components serialize correctly", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)
  # No strategies, groups, variables, tables, scripts, etc.

  # JSON
  json <- write_model_json(model)
  model_back <- read_model_json(json)
  expect_equal(nrow(model_back$strategies), 0)
  expect_equal(nrow(model_back$variables), 0)

  # YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)
  expect_equal(nrow(model_back$strategies), 0)
  expect_equal(nrow(model_back$variables), 0)

  unlink(yaml_path)
})

test_that("numeric precision preserved in round-trip", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("precise", 0.123456789012345)

  # JSON
  json <- write_model_json(model)
  model_back <- read_model_json(json)
  expect_equal(as.numeric(model_back$variables$formula[1]),
               0.123456789012345, tolerance = 1e-10)

  # YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)
  expect_equal(as.numeric(model_back$variables$formula[1]),
               0.123456789012345, tolerance = 1e-10)

  unlink(yaml_path)
})

test_that("unicode characters preserved in round-trip", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_strategy("treatment", display_name = "Tratamiento")

  # JSON
  json <- write_model_json(model)
  model_back <- read_model_json(json)
  expect_equal(model_back$strategies$display_name[1], "Tratamiento")

  # YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)
  expect_equal(model_back$strategies$display_name[1], "Tratamiento")

  unlink(yaml_path)
})

# ==============================================================================
# Model Type Round-Trip Tests
# ==============================================================================

test_that("markov model type round-trips correctly", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  # JSON
  json <- write_model_json(model)
  model_back <- read_model_json(json)
  expect_equal(model_back$settings$model_type, "markov")

  # YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)
  expect_equal(model_back$settings$model_type, "markov")

  unlink(yaml_path)
})

test_that("psm model type round-trips correctly", {
  model <- define_model("psm") |>
    add_state("pfs") |>
    add_state("os")

  # JSON
  json <- write_model_json(model)
  model_back <- read_model_json(json)
  expect_equal(model_back$settings$model_type, "psm")

  # YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)
  expect_equal(model_back$settings$model_type, "psm")

  unlink(yaml_path)
})

# ==============================================================================
# Comprehensive Model Component Coverage
# ==============================================================================

#' Create a model with ALL components for comprehensive testing
create_comprehensive_model <- function() {

  define_model("markov") |>
    # Settings
    set_settings(
      timeframe = 100,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5,
      half_cycle_method = "start"
    ) |>
    # Strategies
    add_strategy("control", display_name = "Control Arm", description = "Standard of care") |>
    add_strategy("treatment", display_name = "Treatment Arm", description = "New treatment") |>
    # Groups
    add_group("young", weight = "0.6", display_name = "Young patients") |>
    add_group("old", weight = "0.4", display_name = "Old patients") |>
    # States
    add_state("healthy", initial_prob = 1, display_name = "Healthy") |>
    add_state("sick", initial_prob = 0, display_name = "Sick") |>
    add_state("dead", initial_prob = 0, display_name = "Dead") |>
    # Variables - global
    add_variable("baseline_risk", 0.05, description = "Baseline risk") |>
    add_variable("utility_healthy", 1.0) |>
    add_variable("utility_sick", 0.6) |>
    add_variable("cost_sick", 10000) |>
    # Variables - strategy-specific
    add_variable("efficacy", 0.0, strategy = "control") |>
    add_variable("efficacy", 0.5, strategy = "treatment") |>
    add_variable("drug_cost", 0, strategy = "control") |>
    add_variable("drug_cost", 5000, strategy = "treatment") |>
    # Variables - group-specific
    add_variable("age_factor", 0.8, group = "young") |>
    add_variable("age_factor", 1.2, group = "old") |>
    # Transitions
    add_transition("healthy", "sick", baseline_risk * age_factor * (1 - efficacy)) |>
    add_transition("healthy", "dead", 0.01) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.15) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    # Values
    add_value("qaly_healthy", utility_healthy, state = "healthy", type = "outcome") |>
    add_value("qaly_sick", utility_sick, state = "sick", type = "outcome") |>
    add_value("cost_treatment", drug_cost, state = "healthy", type = "cost") |>
    add_value("cost_disease", cost_sick, state = "sick", type = "cost") |>
    # Summaries
    add_summary("total_qalys", "qaly_healthy, qaly_sick", type = "outcome") |>
    add_summary("total_costs", "cost_treatment, cost_disease", type = "cost") |>
    # Tables
    add_table("risk_table", data.frame(
      age = c(40, 50, 60, 70),
      risk = c(0.02, 0.05, 0.10, 0.20)
    ), description = "Age-specific risk factors") |>
    add_table("cost_table", data.frame(
      item = c("drug", "admin", "monitoring"),
      cost = c(1000, 200, 150)
    ), description = "Cost components") |>
    # Scripts
    add_script("calc_risk", "calc_risk <- function(age, base) {\n  base * (1 + age/100)\n}",
               description = "Risk calculation function") |>
    add_script("util_func", "adjust_utility <- function(u, factor) {\n  u * factor\n}",
               description = "Utility adjustment") |>
    # DSA
    add_dsa_variable("baseline_risk", low = 0.02, high = 0.10, display_name = "Baseline Risk") |>
    add_dsa_variable("efficacy", low = 0.3, high = 0.7, strategy = "treatment") |>
    add_dsa_setting("discount_cost", low = 0, high = 6) |>
    # Scenarios
    add_scenario("Optimistic", description = "Best case assumptions") |>
    add_scenario_variable("Optimistic", "baseline_risk", 0.02) |>
    add_scenario_variable("Optimistic", "efficacy", 0.7, strategy = "treatment") |>
    add_scenario_setting("Optimistic", "timeframe", 120) |>
    add_scenario("Pessimistic", description = "Worst case assumptions") |>
    add_scenario_variable("Pessimistic", "baseline_risk", 0.10) |>
    add_scenario_variable("Pessimistic", "efficacy", 0.3, strategy = "treatment") |>
    # TWSA
    add_twsa("Risk vs Efficacy", description = "Two-way on key parameters") |>
    add_twsa_variable("Risk vs Efficacy", "baseline_risk",
                      type = "range", min = 0.02, max = 0.10, steps = 5) |>
    add_twsa_variable("Risk vs Efficacy", "efficacy",
                      type = "range", min = 0.3, max = 0.7, steps = 5,
                      strategy = "treatment") |>
    # Multivariate Sampling
    add_multivariate_sampling(
      name = "correlated_utilities",
      distribution = mvnormal(mean = c(utility_healthy, utility_sick), sd = c(0.1, 0.15), cor = 0.6),
      variables = c("utility_healthy", "utility_sick"),
      description = "Correlated utility sampling"
    )
}

#' Verify all model components match between two models with full field-level verification
expect_models_equivalent <- function(original, restored, label = "") {
  prefix <- if (label != "") paste0("[", label, "] ") else ""

  # === SETTINGS (all fields) ===
  for (setting in names(original$settings)) {
    expect_equal(restored$settings[[setting]], original$settings[[setting]],
                 label = paste0(prefix, "setting: ", setting))
  }

  # === STRATEGIES (all fields) ===
  expect_equal(nrow(restored$strategies), nrow(original$strategies),
               label = paste0(prefix, "strategies count"))
  if (nrow(original$strategies) > 0) {
    orig_sorted <- original$strategies[order(original$strategies$name), ]
    rest_sorted <- restored$strategies[order(restored$strategies$name), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "strategies$", col))
    }
  }

  # === GROUPS (all fields including weight) ===
  expect_equal(nrow(restored$groups), nrow(original$groups),
               label = paste0(prefix, "groups count"))
  if (nrow(original$groups) > 0) {
    orig_sorted <- original$groups[order(original$groups$name), ]
    rest_sorted <- restored$groups[order(restored$groups$name), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "groups$", col))
    }
  }

  # === STATES (all fields including initial_prob) ===
  expect_equal(nrow(restored$states), nrow(original$states),
               label = paste0(prefix, "states count"))
  if (nrow(original$states) > 0) {
    orig_sorted <- original$states[order(original$states$name), ]
    rest_sorted <- restored$states[order(restored$states$name), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "states$", col))
    }
  }

  # === VARIABLES (all fields including formula) ===
  expect_equal(nrow(restored$variables), nrow(original$variables),
               label = paste0(prefix, "variables count"))
  if (nrow(original$variables) > 0) {
    # Sort by name + strategy + group for consistent comparison
    orig_sorted <- original$variables[order(original$variables$name,
                                            original$variables$strategy,
                                            original$variables$group), ]
    rest_sorted <- restored$variables[order(restored$variables$name,
                                            restored$variables$strategy,
                                            restored$variables$group), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "variables$", col))
    }
  }

  # === TRANSITIONS (all fields including formula) ===
  expect_equal(nrow(restored$transitions), nrow(original$transitions),
               label = paste0(prefix, "transitions count"))
  if (nrow(original$transitions) > 0) {
    orig_sorted <- original$transitions[order(original$transitions$from_state,
                                              original$transitions$to_state), ]
    rest_sorted <- restored$transitions[order(restored$transitions$from_state,
                                              restored$transitions$to_state), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "transitions$", col))
    }
  }

  # === VALUES (all fields) ===
  expect_equal(nrow(restored$values), nrow(original$values),
               label = paste0(prefix, "values count"))
  if (nrow(original$values) > 0) {
    orig_sorted <- original$values[order(original$values$name,
                                         original$values$state), ]
    rest_sorted <- restored$values[order(restored$values$name,
                                         restored$values$state), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "values$", col))
    }
  }

  # === SUMMARIES (all fields) ===
  expect_equal(nrow(restored$summaries), nrow(original$summaries),
               label = paste0(prefix, "summaries count"))
  if (nrow(original$summaries) > 0) {
    orig_sorted <- original$summaries[order(original$summaries$name), ]
    rest_sorted <- restored$summaries[order(restored$summaries$name), ]
    for (col in names(orig_sorted)) {
      expect_equal(rest_sorted[[col]], orig_sorted[[col]],
                   label = paste0(prefix, "summaries$", col))
    }
  }

  # === TABLES (including data content) ===
  expect_equal(length(restored$tables), length(original$tables),
               label = paste0(prefix, "tables count"))
  expect_setequal(names(restored$tables), names(original$tables))
  for (tbl_name in names(original$tables)) {
    expect_equal(restored$tables[[tbl_name]]$description,
                 original$tables[[tbl_name]]$description,
                 label = paste0(prefix, "table ", tbl_name, " description"))
    # Check actual data frame content
    expect_equal(restored$tables[[tbl_name]]$data,
                 original$tables[[tbl_name]]$data,
                 label = paste0(prefix, "table ", tbl_name, " data"))
  }

  # === SCRIPTS (including code content) ===
  expect_equal(length(restored$scripts), length(original$scripts),
               label = paste0(prefix, "scripts count"))
  expect_setequal(names(restored$scripts), names(original$scripts))
  for (scr_name in names(original$scripts)) {
    expect_equal(restored$scripts[[scr_name]]$description,
                 original$scripts[[scr_name]]$description,
                 label = paste0(prefix, "script ", scr_name, " description"))
    # Check code content (normalize whitespace)
    expect_equal(trimws(restored$scripts[[scr_name]]$code),
                 trimws(original$scripts[[scr_name]]$code),
                 label = paste0(prefix, "script ", scr_name, " code"))
  }

  # === MULTIVARIATE SAMPLING ===
  expect_equal(length(restored$multivariate_sampling),
               length(original$multivariate_sampling),
               label = paste0(prefix, "multivariate_sampling count"))
  if (length(original$multivariate_sampling) > 0) {
    for (i in seq_along(original$multivariate_sampling)) {
      orig_mv <- original$multivariate_sampling[[i]]
      rest_mv <- restored$multivariate_sampling[[i]]
      expect_equal(rest_mv$name, orig_mv$name,
                   label = paste0(prefix, "mv_sampling[", i, "]$name"))
      expect_equal(rest_mv$distribution, orig_mv$distribution,
                   label = paste0(prefix, "mv_sampling[", i, "]$distribution"))
      expect_equal(rest_mv$description, orig_mv$description,
                   label = paste0(prefix, "mv_sampling[", i, "]$description"))
      expect_equal(nrow(rest_mv$variables), nrow(orig_mv$variables),
                   label = paste0(prefix, "mv_sampling[", i, "]$variables count"))
    }
  }

  # === DSA PARAMETERS ===
  expect_equal(length(restored$dsa_parameters),
               length(original$dsa_parameters),
               label = paste0(prefix, "dsa_parameters count"))
  if (length(original$dsa_parameters) > 0) {
    for (i in seq_along(original$dsa_parameters)) {
      orig_p <- original$dsa_parameters[[i]]
      rest_p <- restored$dsa_parameters[[i]]
      expect_equal(rest_p$type, orig_p$type,
                   label = paste0(prefix, "dsa[", i, "]$type"))
      expect_equal(rest_p$name, orig_p$name,
                   label = paste0(prefix, "dsa[", i, "]$name"))
      # Compare low/high (may be formula or numeric)
      expect_equal(as.character(rest_p$low), as.character(orig_p$low),
                   label = paste0(prefix, "dsa[", i, "]$low"))
      expect_equal(as.character(rest_p$high), as.character(orig_p$high),
                   label = paste0(prefix, "dsa[", i, "]$high"))
    }
  }

  # === SCENARIOS ===
  expect_equal(length(restored$scenarios), length(original$scenarios),
               label = paste0(prefix, "scenarios count"))
  if (length(original$scenarios) > 0) {
    for (i in seq_along(original$scenarios)) {
      orig_s <- original$scenarios[[i]]
      rest_s <- restored$scenarios[[i]]
      expect_equal(rest_s$name, orig_s$name,
                   label = paste0(prefix, "scenario[", i, "]$name"))
      expect_equal(rest_s$description, orig_s$description,
                   label = paste0(prefix, "scenario[", i, "]$description"))
      expect_equal(length(rest_s$variable_overrides),
                   length(orig_s$variable_overrides),
                   label = paste0(prefix, "scenario[", i, "]$var_overrides count"))
      expect_equal(length(rest_s$setting_overrides),
                   length(orig_s$setting_overrides),
                   label = paste0(prefix, "scenario[", i, "]$setting_overrides count"))
    }
  }

  # === TWSA ANALYSES ===
  expect_equal(length(restored$twsa_analyses), length(original$twsa_analyses),
               label = paste0(prefix, "twsa_analyses count"))
  if (length(original$twsa_analyses) > 0) {
    for (i in seq_along(original$twsa_analyses)) {
      orig_t <- original$twsa_analyses[[i]]
      rest_t <- restored$twsa_analyses[[i]]
      expect_equal(rest_t$name, orig_t$name,
                   label = paste0(prefix, "twsa[", i, "]$name"))
      expect_equal(rest_t$description, orig_t$description,
                   label = paste0(prefix, "twsa[", i, "]$description"))
      expect_equal(length(rest_t$parameters), length(orig_t$parameters),
                   label = paste0(prefix, "twsa[", i, "]$parameters count"))
    }
  }
}

# ==============================================================================
# Excel <-> JSON Round-Trip Tests
# ==============================================================================

# NOTE: Excel format now supports full parity with JSON/YAML:
# - Table/script descriptions stored in _metadata sheet
# - DSA parameters stored in dsa_parameters sheet
# - Scenarios stored in scenarios and scenario_overrides sheets
# - TWSA analyses stored in twsa_analyses and twsa_parameters sheets
# - Script names preserve original naming (no double .R extension)

test_that("Excel -> JSON -> Excel preserves all components", {
  skip_on_cran()

  # Use comprehensive model with all components including SA
  model <- create_comprehensive_model()

  # Model -> Excel
  excel_dir <- tempfile()
  dir.create(excel_dir)
  write_model(model, excel_dir, format = "excel")

  # Excel -> JSON
  model_from_excel <- read_model(excel_dir)
  json <- write_model_json(model_from_excel)

  # JSON -> Model
  model_from_json <- read_model_json(json)

  # JSON -> Excel
  excel_dir2 <- tempfile()
  dir.create(excel_dir2)
  write_model(model_from_json, excel_dir2, format = "excel")

  # Excel -> Model (final)
  model_final <- read_model(excel_dir2)

  # Full parity check - no exceptions
  expect_models_equivalent(model_from_excel, model_final, "Excel->JSON->Excel")

  unlink(excel_dir, recursive = TRUE)
  unlink(excel_dir2, recursive = TRUE)
})

test_that("JSON -> Excel -> JSON preserves all components", {
  skip_on_cran()

  # Use comprehensive model with all components including SA
  model <- create_comprehensive_model()

  # Model -> JSON
  json1 <- write_model_json(model)
  model1 <- read_model_json(json1)

  # JSON -> Excel
  excel_dir <- tempfile()
  dir.create(excel_dir)
  write_model(model1, excel_dir, format = "excel")

  # Excel -> JSON
  model_from_excel <- read_model(excel_dir)
  json2 <- write_model_json(model_from_excel)
  model2 <- read_model_json(json2)

  # Full parity check - no exceptions
  expect_models_equivalent(model1, model2, "JSON->Excel->JSON")

  unlink(excel_dir, recursive = TRUE)
})

# ==============================================================================
# YAML <-> JSON Round-Trip Tests (Comprehensive)
# ==============================================================================

test_that("YAML -> JSON -> YAML preserves all components", {
  model <- create_comprehensive_model()

  # Model -> YAML
  yaml1 <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml1)
  model_from_yaml <- read_model_yaml(yaml1)

  # YAML -> JSON
  json <- write_model_json(model_from_yaml)

  # JSON -> YAML
  model_from_json <- read_model_json(json)
  yaml2 <- tempfile(fileext = ".yaml")
  write_model_yaml(model_from_json, yaml2)

  # YAML -> Model (final)
  model_final <- read_model_yaml(yaml2)

  expect_models_equivalent(model_from_yaml, model_final, "YAML->JSON->YAML")

  unlink(c(yaml1, yaml2))
})

test_that("JSON -> YAML -> JSON preserves all components", {
  model <- create_comprehensive_model()

  # Model -> JSON
  json1 <- write_model_json(model)
  model1 <- read_model_json(json1)

  # JSON -> YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model1, yaml_path)

  # YAML -> JSON
  model_from_yaml <- read_model_yaml(yaml_path)
  json2 <- write_model_json(model_from_yaml)
  model2 <- read_model_json(json2)

  expect_models_equivalent(model1, model2, "JSON->YAML->JSON")

  unlink(yaml_path)
})

# ==============================================================================
# R Code <-> JSON Round-Trip Tests
# ==============================================================================

test_that("R -> JSON -> R preserves core components", {
  # R code format has limitations - no SA, limited table/script support
  model <- define_model("markov") |>
    set_settings(timeframe = 50, cycle_length = 1, discount_cost = 3) |>
    add_strategy("control", display_name = "Control") |>
    add_strategy("treatment", display_name = "Treatment") |>
    add_group("general", weight = "1") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_sick", 0.05) |>
    add_variable("efficacy", 0.0, strategy = "control") |>
    add_variable("efficacy", 0.4, strategy = "treatment") |>
    add_transition("healthy", "sick", p_sick * (1 - efficacy)) |>
    add_transition("healthy", "dead", 0.01) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.1) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("qaly", 1, state = "healthy", type = "outcome") |>
    add_value("qaly", 0.7, state = "sick", type = "outcome") |>
    add_summary("total_qaly", "qaly")

  # Model -> R code
  r_path <- tempfile(fileext = ".R")
  write_model(model, r_path, format = "r")

  # R code -> Model
  env <- new.env()
  source(r_path, local = env)
  model_names <- ls(env)[sapply(ls(env), function(x) inherits(env[[x]], "oq_model"))]
  model_from_r <- env[[model_names[1]]]

  # R -> JSON
  json <- write_model_json(model_from_r)

  # JSON -> Model
  model_from_json <- read_model_json(json)

  # JSON -> R code
  r_path2 <- tempfile(fileext = ".R")
  write_model(model_from_json, r_path2, format = "r")

  # R code -> Model (final)
  env2 <- new.env()
  source(r_path2, local = env2)
  model_names2 <- ls(env2)[sapply(ls(env2), function(x) inherits(env2[[x]], "oq_model"))]
  model_final <- env2[[model_names2[1]]]

  # Check core components
  expect_equal(nrow(model_from_r$states), nrow(model_final$states),
               label = "R->JSON->R states")
  expect_equal(nrow(model_from_r$transitions), nrow(model_final$transitions),
               label = "R->JSON->R transitions")
  expect_equal(nrow(model_from_r$strategies), nrow(model_final$strategies),
               label = "R->JSON->R strategies")
  expect_equal(nrow(model_from_r$variables), nrow(model_final$variables),
               label = "R->JSON->R variables")

  unlink(c(r_path, r_path2))
})

test_that("JSON -> R -> JSON preserves core components", {
  model <- define_model("markov") |>
    add_strategy("a") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_death", 0.05) |>
    add_transition("alive", "dead", p_death) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1)

  # Model -> JSON
  json1 <- write_model_json(model)
  model1 <- read_model_json(json1)

  # JSON -> R code
  r_path <- tempfile(fileext = ".R")
  write_model(model1, r_path, format = "r")

  # R code -> Model
  env <- new.env()
  source(r_path, local = env)
  model_names <- ls(env)[sapply(ls(env), function(x) inherits(env[[x]], "oq_model"))]
  model_from_r <- env[[model_names[1]]]

  # R -> JSON
  json2 <- write_model_json(model_from_r)
  model2 <- read_model_json(json2)

  expect_equal(nrow(model1$states), nrow(model2$states),
               label = "JSON->R->JSON states")
  expect_equal(nrow(model1$transitions), nrow(model2$transitions),
               label = "JSON->R->JSON transitions")

  unlink(r_path)
})

# ==============================================================================
# All Formats -> JSON Verification
# ==============================================================================

test_that("all formats produce equivalent JSON for same model", {
  skip_on_cran()

  # Use comprehensive model to test all components including SA
  model <- create_comprehensive_model()

  # Direct to JSON
  json_direct <- write_model_json(model)
  model_direct <- read_model_json(json_direct)

  # Via YAML
  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_yaml <- read_model_yaml(yaml_path)
  json_via_yaml <- write_model_json(model_yaml)
  model_via_yaml <- read_model_json(json_via_yaml)

  # Via Excel
  excel_dir <- tempfile()
  dir.create(excel_dir)
  write_model(model, excel_dir, format = "excel")
  model_excel <- read_model(excel_dir)
  json_via_excel <- write_model_json(model_excel)
  model_via_excel <- read_model_json(json_via_excel)

  # All should have same structure - core components
  expect_equal(nrow(model_direct$states), nrow(model_via_yaml$states))
  expect_equal(nrow(model_direct$states), nrow(model_via_excel$states))
  expect_equal(nrow(model_direct$transitions), nrow(model_via_yaml$transitions))
  expect_equal(nrow(model_direct$transitions), nrow(model_via_excel$transitions))
  expect_equal(nrow(model_direct$variables), nrow(model_via_yaml$variables))
  expect_equal(nrow(model_direct$variables), nrow(model_via_excel$variables))

  # All formats should preserve tables and scripts
  expect_equal(length(model_direct$tables), length(model_via_yaml$tables))
  expect_equal(length(model_direct$tables), length(model_via_excel$tables))
  expect_equal(length(model_direct$scripts), length(model_via_yaml$scripts))
  expect_equal(length(model_direct$scripts), length(model_via_excel$scripts))

  # All formats should preserve sensitivity analysis components
  expect_equal(length(model_direct$dsa_parameters), length(model_via_yaml$dsa_parameters))
  expect_equal(length(model_direct$dsa_parameters), length(model_via_excel$dsa_parameters))
  expect_equal(length(model_direct$scenarios), length(model_via_yaml$scenarios))
  expect_equal(length(model_direct$scenarios), length(model_via_excel$scenarios))
  expect_equal(length(model_direct$twsa_analyses), length(model_via_yaml$twsa_analyses))
  expect_equal(length(model_direct$twsa_analyses), length(model_via_excel$twsa_analyses))

  unlink(yaml_path)
  unlink(excel_dir, recursive = TRUE)
})

# ==============================================================================
# Formula Handling Tests (consolidated from test_yaml_format.R)
# ==============================================================================

test_that("YAML preserves formula expressions", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_death", "0.01 + 0.005 * age") |>
    add_transition("alive", "dead", p_death) |>
    add_transition("alive", "alive", 1 - p_death) |>
    add_transition("dead", "dead", 1)

  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)

  expect_equal(model_back$variables$formula[1], "0.01 + 0.005 * age")

  unlink(yaml_path)
})

test_that("YAML preserves special characters in formulas", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("test", 'look_up(table, age = "adult", sex = "male")')

  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  model_back <- read_model_yaml(yaml_path)

  expect_equal(model_back$variables$formula[1],
               'look_up(table, age = "adult", sex = "male")')

  unlink(yaml_path)
})

# ==============================================================================
# DSA Field-Level Tests (consolidated from test_sensitivity_serialization.R)
# ==============================================================================

test_that("DSA variable parameters round-trip through JSON with field verification", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("p_disease", 0.03) |>
    add_dsa_variable("p_disease", low = bc * 0.5, high = bc * 1.5)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(length(model_back$dsa_parameters), 1)
  expect_equal(model_back$dsa_parameters[[1]]$type, "variable")
  expect_equal(model_back$dsa_parameters[[1]]$name, "p_disease")
  expect_equal(as.character(model_back$dsa_parameters[[1]]$low), "bc * 0.5")
  expect_equal(as.character(model_back$dsa_parameters[[1]]$high), "bc * 1.5")
})

test_that("DSA setting parameters round-trip through JSON with field verification", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    set_settings(discount_cost = 3) |>
    add_dsa_setting("discount_cost", low = 0, high = 5)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(length(model_back$dsa_parameters), 1)
  expect_equal(model_back$dsa_parameters[[1]]$type, "setting")
  expect_equal(model_back$dsa_parameters[[1]]$low, 0)
  expect_equal(model_back$dsa_parameters[[1]]$high, 5)
})

test_that("strategy-specific DSA parameters preserved in JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_strategy("tx_a") |>
    add_variable("cost", 1000, strategy = "tx_a") |>
    add_dsa_variable("cost", low = 500, high = 1500, strategy = "tx_a")

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$dsa_parameters[[1]]$strategy, "tx_a")
})

test_that("DSA with display_name preserved in JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("p", 0.5) |>
    add_dsa_variable("p", low = 0.3, high = 0.7, display_name = "Probability of Event")

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$dsa_parameters[[1]]$display_name, "Probability of Event")
})

test_that("oq_formula bc keyword preserved in DSA", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("p", 0.5) |>
    add_dsa_variable("p", low = bc * 0.5, high = bc + 0.1)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  low <- model_back$dsa_parameters[[1]]$low
  high <- model_back$dsa_parameters[[1]]$high

  expect_true(grepl("bc", as.character(low)))
  expect_true(grepl("bc", as.character(high)))
})

# ==============================================================================
# Scenario Field-Level Tests (consolidated from test_sensitivity_serialization.R)
# ==============================================================================

test_that("scenarios with variable overrides round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("p_disease", 0.03) |>
    add_scenario("Optimistic") |>
    add_scenario_variable("Optimistic", "p_disease", 0.02)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(length(model_back$scenarios), 1)
  expect_equal(model_back$scenarios[[1]]$name, "Optimistic")
  expect_equal(length(model_back$scenarios[[1]]$variable_overrides), 1)
  expect_equal(model_back$scenarios[[1]]$variable_overrides[[1]]$value, 0.02)
})

test_that("scenarios with formula overrides round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("p_disease", 0.03) |>
    add_variable("base_p", 0.02) |>
    add_scenario("Modified") |>
    add_scenario_variable("Modified", "p_disease", base_p * 0.8)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  override_value <- model_back$scenarios[[1]]$variable_overrides[[1]]$value
  expect_true(inherits(override_value, "oq_formula") || is.character(override_value))
})

test_that("scenarios with setting overrides round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    set_settings(timeframe = 100) |>
    add_scenario("Extended") |>
    add_scenario_setting("Extended", "timeframe", 150)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$scenarios[[1]]$setting_overrides[[1]]$value, 150)
})

test_that("scenario description preserved in JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_scenario("Optimistic", description = "Best case assumptions")

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$scenarios[[1]]$description, "Best case assumptions")
})

test_that("multiple scenarios round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("p", 0.5) |>
    add_scenario("Optimistic") |>
    add_scenario_variable("Optimistic", "p", 0.3) |>
    add_scenario("Pessimistic") |>
    add_scenario_variable("Pessimistic", "p", 0.7)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(length(model_back$scenarios), 2)
  expect_equal(model_back$scenarios[[1]]$name, "Optimistic")
  expect_equal(model_back$scenarios[[2]]$name, "Pessimistic")
})

# ==============================================================================
# TWSA Field-Level Tests (consolidated from test_sensitivity_serialization.R)
# ==============================================================================

test_that("TWSA with range parameters round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("cost", 1000) |>
    add_variable("efficacy", 0.5) |>
    add_twsa("Cost-Efficacy") |>
    add_twsa_variable("Cost-Efficacy", "cost",
                      type = "range", min = 500, max = 1500, steps = 5) |>
    add_twsa_variable("Cost-Efficacy", "efficacy",
                      type = "range", min = 0.3, max = 0.7, steps = 5)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(length(model_back$twsa_analyses), 1)
  expect_equal(length(model_back$twsa_analyses[[1]]$parameters), 2)
  expect_equal(model_back$twsa_analyses[[1]]$parameters[[1]]$type, "range")
})

test_that("TWSA with radius parameters round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("cost", 1000) |>
    add_variable("efficacy", 0.5) |>
    add_twsa("Analysis") |>
    add_twsa_variable("Analysis", "cost",
                      type = "radius", radius = 200, steps = 3) |>
    add_twsa_variable("Analysis", "efficacy",
                      type = "radius", radius = 0.1, steps = 3)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  param <- model_back$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$type, "radius")
})

test_that("TWSA with custom values round-trip through JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("discount", 0.03) |>
    set_settings(timeframe = 100) |>
    add_twsa("Discounting") |>
    add_twsa_variable("Discounting", "discount",
                      type = "custom", values = c(0, 0.015, 0.03, 0.05)) |>
    add_twsa_setting("Discounting", "timeframe",
                     type = "custom", values = c(50, 100, 150))

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$twsa_analyses[[1]]$parameters[[1]]$type, "custom")
})

test_that("TWSA description preserved in JSON", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_variable("cost", 1000) |>
    add_variable("efficacy", 0.5) |>
    add_twsa("Cost vs Efficacy", description = "Trade-off analysis") |>
    add_twsa_variable("Cost vs Efficacy", "cost",
                      type = "range", min = 500, max = 1500, steps = 3) |>
    add_twsa_variable("Cost vs Efficacy", "efficacy",
                      type = "range", min = 0.3, max = 0.7, steps = 3)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$twsa_analyses[[1]]$description, "Trade-off analysis")
})

test_that("TWSA with strategy-specific variable round-trip", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1) |>
    add_strategy("tx_a") |>
    add_variable("cost", 1000, strategy = "tx_a") |>
    add_variable("efficacy", 0.5) |>
    add_twsa("Analysis") |>
    add_twsa_variable("Analysis", "cost",
                      type = "range", min = 500, max = 1500, steps = 3,
                      strategy = "tx_a") |>
    add_twsa_variable("Analysis", "efficacy",
                      type = "range", min = 0.3, max = 0.7, steps = 3)

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_equal(model_back$twsa_analyses[[1]]$parameters[[1]]$strategy, "tx_a")
})
