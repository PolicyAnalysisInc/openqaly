context("Sampling conversions")
suppressMessages(library(dplyr))

# Helper function to create a model with both univariate and multivariate sampling
create_test_model_with_all_sampling <- function() {
  define_model("markov") |>
    set_settings(
      n_cycles = 50,
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0.03,
      discount_outcomes = 0.03
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

    # Multivariate sampling
    add_multivariate_sampling(
      name = "transition_probs",
      distribution = dirichlet(c(alpha_h, alpha_s, alpha_d)),
      variables = c("p_stay_healthy", "p_become_sick", "p_become_dead"),
      description = "Transition probabilities from healthy state"
    ) |>
    add_multivariate_sampling(
      name = "cost_qaly_corr",
      distribution = mvnormal(mean = c(cost_mean, qaly_mean), sd = c(cost_sd, qaly_sd), cor = correlation),
      variables = c("treatment_cost", "treatment_qaly"),
      description = "Correlated cost and QALY"
    )
}

test_that("R to JSON to R preserves all sampling specifications", {
  # Create original model
  original_model <- create_test_model_with_all_sampling()

  # Convert to JSON and back
  json_string <- write_model_json(original_model)
  restored_model <- read_model_json(json_string)

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
    expect_equal(orig_mv$distribution, rest_mv$distribution)
    expect_equal(orig_mv$description, rest_mv$description)
    expect_equal(orig_mv$variables$variable, rest_mv$variables$variable)
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

test_that("R to Excel to R preserves all sampling specifications", {
  # Create original model
  original_model <- create_test_model_with_all_sampling()

  # Write to Excel
  temp_path <- tempfile(pattern = "test_r_excel_r_", fileext = "")
  write_model(original_model, temp_path, format = "excel")

  # Read back from Excel
  restored_model <- read_model(temp_path)

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
    expect_equal(orig_mv$distribution, rest_mv$distribution)
    expect_equal(orig_mv$description, rest_mv$description)
    expect_equal(orig_mv$variables$variable, rest_mv$variables$variable)
  }

  # Clean up
  unlink(temp_path, recursive = TRUE)
})

test_that("Excel to JSON to Excel preserves all sampling specifications", {
  # Create model in Excel format
  temp_excel_path <- tempfile(pattern = "test_excel_json_excel_", fileext = "")
  dir.create(temp_excel_path, showWarnings = FALSE)

  # Create Excel structure with sampling
  settings <- tibble(
    setting = c("model_type", "n_cycles", "discount_cost", "discount_outcomes"),
    value = c("markov", "10", "0.03", "0.03")
  )

  states <- tibble(
    name = c("healthy", "sick", "dead"),
    initial_probability = c("0.8", "0.15", "0.05")
  )

  variables <- tibble(
    name = c("p_hs", "cost_h", "alpha1", "alpha2", "alpha3", "p1", "p2", "p3"),
    formula = c("0.1", "1000", "80", "15", "5", "0.8", "0.15", "0.05"),
    sampling = c("beta(mean = 0.1, sd = 0.02)", "normal(mean = 1000, sd = 100)",
                 rep("", 6))
  )

  transitions <- tibble(
    from_state = c("healthy", "healthy", "healthy", "sick", "sick", "dead"),
    to_state = c("healthy", "sick", "dead", "sick", "dead", "dead"),
    formula = c("p1", "p2", "p3", "0.9", "0.1", "1")
  )

  multivariate_sampling <- tibble(
    name = c("trans_probs"),
    distribution = c("dirichlet(c(alpha1, alpha2, alpha3))"),
    description = c("Transition probabilities")
  )

  multivariate_sampling_variables <- tibble(
    sampling_name = rep("trans_probs", 3),
    variable = c("p1", "p2", "p3"),
    strategy = rep("", 3),
    group = rep("", 3)
  )

  strategies <- tibble(
    name = c("standard"),
    display_name = c("Standard"),
    description = c("Standard treatment")
  )

  groups <- tibble(
    name = c("all_patients"),
    display_name = c("All Patients"),
    description = c("All patients"),
    weight = c(1)
  )

  wb_list <- list(
    settings = settings,
    strategies = strategies,
    groups = groups,
    states = states,
    variables = variables,
    transitions = transitions,
    multivariate_sampling = multivariate_sampling,
    multivariate_sampling_variables = multivariate_sampling_variables
  )

  openxlsx::write.xlsx(wb_list, file.path(temp_excel_path, "model.xlsx"))

  # Read from Excel
  excel_model <- read_model(temp_excel_path)

  # Convert to JSON
  json_string <- write_model_json(excel_model)
  json_model <- read_model_json(json_string)

  # Write back to Excel
  temp_excel_path2 <- tempfile(pattern = "test_excel_restored_", fileext = "")
  write_model(json_model, temp_excel_path2, format = "excel")

  # Read the restored Excel
  restored_model <- read_model(temp_excel_path2)

  # Check univariate sampling preserved
  expect_equal(
    excel_model$variables$sampling[!is.na(excel_model$variables$sampling)],
    restored_model$variables$sampling[!is.na(restored_model$variables$sampling)]
  )

  # Check multivariate sampling preserved
  expect_equal(length(excel_model$multivariate_sampling),
               length(restored_model$multivariate_sampling))

  if (length(excel_model$multivariate_sampling) > 0) {
    expect_equal(
      excel_model$multivariate_sampling[[1]]$name,
      restored_model$multivariate_sampling[[1]]$name
    )
    expect_equal(
      excel_model$multivariate_sampling[[1]]$distribution,
      restored_model$multivariate_sampling[[1]]$distribution
    )
  }

  # Clean up
  unlink(temp_excel_path, recursive = TRUE)
  unlink(temp_excel_path2, recursive = TRUE)
})

test_that("JSON to R code to JSON preserves all sampling specifications", {
  # Start with JSON
  original_model <- create_test_model_with_all_sampling()
  original_json <- write_model_json(original_model)
  json_model <- read_model_json(original_json)

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
  restored_model <- read_model_json(restored_json)

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
    expect_equal(json_mv$distribution, rest_mv$distribution)
    expect_equal(json_mv$description, rest_mv$description)
    expect_equal(json_mv$variables$variable, rest_mv$variables$variable)
  }

  # Clean up
  unlink(temp_file)
})

test_that("PSA runs correctly after format conversions", {
  # Create original model
  original_model <- create_test_model_with_all_sampling()

  # Test after JSON round-trip
  json_string <- write_model_json(original_model)
  json_model <- read_model_json(json_string)

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

  # Test after Excel round-trip
  temp_path <- tempfile(pattern = "test_psa_excel_", fileext = "")
  write_model(original_model, temp_path, format = "excel")
  excel_model <- read_model(temp_path)

  set.seed(789)
  excel_result <- run_model(excel_model)
  excel_parsed <- openqaly:::parse_model(excel_model)
  excel_segments <- excel_result$segments
  for (i in 1:nrow(excel_segments)) {
    excel_segments[i, ] <- openqaly:::prepare_segment_for_sampling(
      excel_parsed, excel_segments[i, ]
    )
  }

  # Should not error
  excel_samples <- openqaly:::resample(excel_parsed, 10, excel_segments, seed = 789)
  expect_equal(nrow(excel_samples), 10 * nrow(excel_segments))

  # Results should be identical (same seed)
  expect_equal(json_samples, excel_samples, tolerance = 1e-10)

  # Clean up
  unlink(temp_path, recursive = TRUE)
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
    add_multivariate_sampling(
      name = "strategy_A_young",
      distribution = mvnormal(mean = c(100, 200), sd = c(10, 20), cor = 0.5),
      variables = tibble(
        variable = c("param1", "param2"),
        strategy = c("A", "A"),
        group = c("young", "young")
      ),
      description = "Sampling for strategy A, young group"
    ) |>
    add_multivariate_sampling(
      name = "strategy_B_old",
      distribution = mvnormal(mean = c(150, 250), sd = c(15, 25), cor = 0.3),
      variables = tibble(
        variable = c("param1", "param2"),
        strategy = c("B", "B"),
        group = c("old", "old")
      ),
      description = "Sampling for strategy B, old group"
    )

  # Test JSON conversion
  json_string <- write_model_json(model)
  json_model <- read_model_json(json_string)

  expect_equal(length(model$multivariate_sampling),
               length(json_model$multivariate_sampling))

  # Check segment-specific variables preserved
  for (i in seq_along(model$multivariate_sampling)) {
    orig_vars <- model$multivariate_sampling[[i]]$variables
    json_vars <- json_model$multivariate_sampling[[i]]$variables

    expect_equal(orig_vars$variable, json_vars$variable)
    expect_equal(orig_vars$strategy, json_vars$strategy)
    expect_equal(orig_vars$group, json_vars$group)
  }

  # Test Excel conversion
  temp_path <- tempfile(pattern = "test_segment_specific_", fileext = "")
  write_model(model, temp_path, format = "excel")
  excel_model <- read_model(temp_path)

  expect_equal(length(model$multivariate_sampling),
               length(excel_model$multivariate_sampling))

  # Check segment-specific variables preserved
  for (i in seq_along(model$multivariate_sampling)) {
    orig_vars <- model$multivariate_sampling[[i]]$variables
    excel_vars <- excel_model$multivariate_sampling[[i]]$variables

    expect_equal(orig_vars$variable, excel_vars$variable)
    expect_equal(orig_vars$strategy, excel_vars$strategy)
    expect_equal(orig_vars$group, excel_vars$group)
  }

  # Clean up
  unlink(temp_path, recursive = TRUE)
})