context("Sampling conversions")
suppressMessages(library(dplyr))

# Helper function to create a model with both univariate and multivariate sampling
create_test_model_with_all_sampling <- function() {
  define_model("markov") |>
    set_settings(
      n_cycles = 50,
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3,
      discount_outcomes = 3
    ) |>
    add_strategy("standard") |>
    add_strategy("intervention") |>
    add_state("healthy", initial_prob = 0.8) |>
    add_state("sick", initial_prob = 0.15) |>
    add_state("dead", initial_prob = 0.05) |>

    # Univariate sampling
    add_variable("p_healthy_sick_base", 0.1,
                 sampling = "beta(mean = 0.1, sd = 0.02)") |>
    add_variable("p_sick_dead_base", 0.05,
                 sampling = "beta(shape1 = 5, shape2 = 95)") |>
    add_variable("cost_healthy", 500,
                 sampling = "normal(mean = 500, sd = 50)") |>
    add_variable("cost_sick", 2000,
                 sampling = "lognormal(meanlog = log(2000), sdlog = 0.2)") |>
    add_variable("utility_healthy", 0.9,
                 sampling = "beta(mean = 0.9, sd = 0.05)") |>
    add_variable("utility_sick", 0.6,
                 sampling = "triangular(min = 0.4, mode = 0.6, max = 0.8)") |>

    # Variables for multivariate sampling
    add_variable("alpha_h", 80) |>
    add_variable("alpha_s", 15) |>
    add_variable("alpha_d", 5) |>
    add_variable("cost_mean", 1000) |>
    add_variable("qaly_mean", 3) |>
    add_variable("cost_sd", 200) |>
    add_variable("qaly_sd", 0.5) |>
    add_variable("correlation", 0.6) |>

    # Variables to be sampled by multivariate distributions
    add_variable("p_stay_healthy", 0.8) |>
    add_variable("p_become_sick", 0.15) |>
    add_variable("p_become_dead", 0.05) |>
    add_variable("treatment_cost", 1000) |>
    add_variable("treatment_qaly", 3) |>

    # Transitions
    add_transition("healthy", "healthy", "p_stay_healthy") |>
    add_transition("healthy", "sick", "p_become_sick") |>
    add_transition("healthy", "dead", "p_become_dead") |>
    add_transition("sick", "sick", "0.7") |>
    add_transition("sick", "dead", "p_sick_dead_base") |>
    add_transition("sick", "healthy", "1 - 0.7 - p_sick_dead_base") |>
    add_transition("dead", "dead", "1") |>

    # Values
    add_value("cost", "cost_healthy + treatment_cost", state = "healthy") |>
    add_value("cost", "cost_sick + treatment_cost * 2", state = "sick") |>
    add_value("cost", "0", state = "dead") |>
    add_value("qaly", "utility_healthy * cycle_length_years", state = "healthy") |>
    add_value("qaly", "utility_sick * cycle_length_years", state = "sick") |>
    add_value("qaly", "0", state = "dead") |>

    # Summaries
    add_summary("total_cost", "cost") |>
    add_summary("total_qaly", "qaly") |>

    # Covariance table for mvnormal
    # sd = c(200, 0.5), cor = 0.6 → cov matrix:
    # c(200^2, 0.6*200*0.5, 0.6*200*0.5, 0.5^2)
    add_table("cost_qaly_cov", data.frame(
      treatment_cost = c(200^2, 0.6 * 200 * 0.5),
      treatment_qaly = c(0.6 * 200 * 0.5, 0.5^2)
    )) |>

    # Multivariate sampling
    add_multivariate_sampling(
      name = "transition_probs",
      type = "dirichlet",
      variables = c("p_stay_healthy", "p_become_sick", "p_become_dead"),
      n = 100,
      description = "Transition probabilities from healthy state"
    ) |>
    add_multivariate_sampling(
      name = "cost_qaly_corr",
      type = "mvnormal",
      variables = c("treatment_cost", "treatment_qaly"),
      covariance = "cost_qaly_cov",
      description = "Correlated cost and QALY"
    )
}

test_that("R to JSON to R preserves all sampling specifications", {
  # Create original model
  original_model <- create_test_model_with_all_sampling()

  # Convert to JSON and back
  json_string <- write_model_json(original_model)
  restored_model <- read_model_json(text = json_string)

  # Check univariate sampling preserved
  orig_univ <- original_model$variables %>%
    filter(!is.na(sampling) & sampling != "") %>%
    select(name, sampling)
  rest_univ <- restored_model$variables %>%
    filter(!is.na(sampling) & sampling != "") %>%
    select(name, sampling)

  expect_equal(nrow(orig_univ), nrow(rest_univ))
  expect_equal(orig_univ$name, rest_univ$name)
  expect_equal(orig_univ$sampling, rest_univ$sampling)

  # Check multivariate sampling preserved
  expect_equal(length(original_model$multivariate_sampling),
               length(restored_model$multivariate_sampling))

  for (i in seq_along(original_model$multivariate_sampling)) {
    orig_mv <- original_model$multivariate_sampling[[i]]
    rest_mv <- restored_model$multivariate_sampling[[i]]

    expect_equal(orig_mv$name, rest_mv$name)
    expect_equal(orig_mv$type, rest_mv$type)
    expect_equal(orig_mv$description, rest_mv$description)
    expect_equal(orig_mv$variables, rest_mv$variables)
  }

  # Test that both models produce same sampling results
  set.seed(123)
  orig_result <- run_model(original_model)
  orig_parsed <- openqaly:::parse_model(original_model)
  orig_segments <- orig_result$segments
  for (i in 1:nrow(orig_segments)) {
    orig_segments[i, ] <- openqaly:::prepare_segment_for_sampling(
      orig_parsed, orig_segments[i, ]
    )
  }
  orig_samples <- openqaly:::resample(orig_parsed, 10, orig_segments, seed = 123)

  set.seed(123)
  rest_result <- run_model(restored_model)
  rest_parsed <- openqaly:::parse_model(restored_model)
  rest_segments <- rest_result$segments
  for (i in 1:nrow(rest_segments)) {
    rest_segments[i, ] <- openqaly:::prepare_segment_for_sampling(
      rest_parsed, rest_segments[i, ]
    )
  }
  rest_samples <- openqaly:::resample(rest_parsed, 10, rest_segments, seed = 123)

  # Compare sampled values
  expect_equal(orig_samples, rest_samples, tolerance = 1e-10)
})

test_that("R to YAML to R preserves all sampling specifications", {
  # Create original model
  original_model <- create_test_model_with_all_sampling()

  # Write to YAML
  temp_path <- tempfile(pattern = "test_r_yaml_r_", fileext = ".yaml")
  write_model(original_model, temp_path, format = "yaml")

  # Read back from YAML
  restored_model <- read_model_yaml(file = temp_path)

  # Check univariate sampling preserved
  orig_univ <- original_model$variables %>%
    filter(!is.na(sampling) & sampling != "") %>%
    select(name, sampling)
  rest_univ <- restored_model$variables %>%
    filter(!is.na(sampling) & sampling != "") %>%
    select(name, sampling)

  expect_equal(nrow(orig_univ), nrow(rest_univ))
  expect_equal(orig_univ$name, rest_univ$name)
  expect_equal(orig_univ$sampling, rest_univ$sampling)

  # Check multivariate sampling preserved
  expect_equal(length(original_model$multivariate_sampling),
               length(restored_model$multivariate_sampling))

  for (i in seq_along(original_model$multivariate_sampling)) {
    orig_mv <- original_model$multivariate_sampling[[i]]
    rest_mv <- restored_model$multivariate_sampling[[i]]

    expect_equal(orig_mv$name, rest_mv$name)
    expect_equal(orig_mv$type, rest_mv$type)
    expect_equal(orig_mv$description, rest_mv$description)
    expect_equal(orig_mv$variables, rest_mv$variables)
  }

  # Clean up
  unlink(temp_path)
})

test_that("JSON to R code to JSON preserves all sampling specifications", {
  # Start with JSON
  original_model <- create_test_model_with_all_sampling()
  original_json <- write_model_json(original_model)
  json_model <- read_model_json(text = original_json)

  # Generate R code
  r_code <- model_to_r_code(json_model)

  # Execute R code to create model
  temp_file <- tempfile(fileext = ".R")
  writeLines(r_code, temp_file)

  env <- new.env()
  source(temp_file, local = env)
  r_model <- env$model

  # Convert back to JSON
  restored_json <- write_model_json(r_model)
  restored_model <- read_model_json(text = restored_json)

  # Check univariate sampling preserved
  json_univ <- json_model$variables %>%
    filter(!is.na(sampling) & sampling != "") %>%
    select(name, sampling)
  rest_univ <- restored_model$variables %>%
    filter(!is.na(sampling) & sampling != "") %>%
    select(name, sampling)

  expect_equal(nrow(json_univ), nrow(rest_univ))
  expect_equal(json_univ$name, rest_univ$name)
  expect_equal(json_univ$sampling, rest_univ$sampling)

  # Check multivariate sampling preserved
  expect_equal(length(json_model$multivariate_sampling),
               length(restored_model$multivariate_sampling))

  for (i in seq_along(json_model$multivariate_sampling)) {
    json_mv <- json_model$multivariate_sampling[[i]]
    rest_mv <- restored_model$multivariate_sampling[[i]]

    expect_equal(json_mv$name, rest_mv$name)
    expect_equal(json_mv$type, rest_mv$type)
    expect_equal(json_mv$description, rest_mv$description)
    expect_equal(json_mv$variables, rest_mv$variables)
  }

  # Clean up
  unlink(temp_file)
})

test_that("PSA runs correctly after format conversions", {
  # Create original model
  original_model <- create_test_model_with_all_sampling()

  # Test after JSON round-trip
  json_string <- write_model_json(original_model)
  json_model <- read_model_json(text = json_string)

  set.seed(789)
  json_result <- run_model(json_model)
  json_parsed <- openqaly:::parse_model(json_model)
  json_segments <- json_result$segments
  for (i in 1:nrow(json_segments)) {
    json_segments[i, ] <- openqaly:::prepare_segment_for_sampling(
      json_parsed, json_segments[i, ]
    )
  }

  # Should not error
  json_samples <- openqaly:::resample(json_parsed, 10, json_segments, seed = 789)
  expect_equal(nrow(json_samples), 10 * nrow(json_segments))

  # Test after YAML round-trip
  temp_path <- tempfile(pattern = "test_psa_yaml_", fileext = ".yaml")
  write_model(original_model, temp_path, format = "yaml")
  yaml_model <- read_model_yaml(file = temp_path)

  set.seed(789)
  yaml_result <- run_model(yaml_model)
  yaml_parsed <- openqaly:::parse_model(yaml_model)
  yaml_segments <- yaml_result$segments
  for (i in 1:nrow(yaml_segments)) {
    yaml_segments[i, ] <- openqaly:::prepare_segment_for_sampling(
      yaml_parsed, yaml_segments[i, ]
    )
  }

  # Should not error
  yaml_samples <- openqaly:::resample(yaml_parsed, 10, yaml_segments, seed = 789)
  expect_equal(nrow(yaml_samples), 10 * nrow(yaml_segments))

  # Results should be identical (same seed)
  expect_equal(json_samples, yaml_samples, tolerance = 1e-10)

  # Clean up
  unlink(temp_path)
})

test_that("Complex multivariate sampling with segment specificity converts correctly", {
  # Create model with segment-specific multivariate sampling
  model <- define_model("markov") |>
    add_strategy("A") |>
    add_strategy("B") |>
    add_group("young", weight = 0.6) |>
    add_group("old", weight = 0.4) |>
    add_state("healthy", initial_prob = 1) |>
    add_variable("base_value", 100) |>
    add_variable("param1", 100) |>
    add_variable("param2", 200) |>
    add_transition("healthy", "healthy", "1") |>
    # Covariance tables for mvnormal
    # sd = c(10, 20), cor = 0.5 → cov: c(100, 100, 100, 400)
    add_table("cov_A_young", data.frame(
      param1 = c(10^2, 0.5 * 10 * 20),
      param2 = c(0.5 * 10 * 20, 20^2)
    )) |>
    # sd = c(15, 25), cor = 0.3 → cov: c(225, 112.5, 112.5, 625)
    add_table("cov_B_old", data.frame(
      param1 = c(15^2, 0.3 * 15 * 25),
      param2 = c(0.3 * 15 * 25, 25^2)
    )) |>
    add_multivariate_sampling(
      name = "strategy_A_young",
      type = "mvnormal",
      variables = c("param1", "param2"),
      strategy = "A",
      group = "young",
      covariance = "cov_A_young",
      description = "Sampling for strategy A, young group"
    ) |>
    add_multivariate_sampling(
      name = "strategy_B_old",
      type = "mvnormal",
      variables = c("param1", "param2"),
      strategy = "B",
      group = "old",
      covariance = "cov_B_old",
      description = "Sampling for strategy B, old group"
    )

  # Test JSON conversion
  json_string <- write_model_json(model)
  json_model <- read_model_json(text = json_string)

  expect_equal(length(model$multivariate_sampling),
               length(json_model$multivariate_sampling))

  # Check variables and strategy/group preserved
  for (i in seq_along(model$multivariate_sampling)) {
    orig_mv <- model$multivariate_sampling[[i]]
    json_mv <- json_model$multivariate_sampling[[i]]

    expect_equal(orig_mv$variables, json_mv$variables)
    expect_equal(orig_mv$strategy, json_mv$strategy)
    expect_equal(orig_mv$group, json_mv$group)
  }

  # Test YAML conversion
  temp_path <- tempfile(pattern = "test_segment_specific_", fileext = ".yaml")
  write_model(model, temp_path, format = "yaml")
  yaml_model <- read_model_yaml(file = temp_path)

  expect_equal(length(model$multivariate_sampling),
               length(yaml_model$multivariate_sampling))

  # Check variables and strategy/group preserved
  for (i in seq_along(model$multivariate_sampling)) {
    orig_mv <- model$multivariate_sampling[[i]]
    yaml_mv <- yaml_model$multivariate_sampling[[i]]

    expect_equal(orig_mv$variables, yaml_mv$variables)
    expect_equal(orig_mv$strategy, yaml_mv$strategy)
    expect_equal(orig_mv$group, yaml_mv$group)
  }

  # Clean up
  unlink(temp_path)
})