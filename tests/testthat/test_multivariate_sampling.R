context("Multivariate sampling")
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(purrr))

test_that("Dirichlet distribution generates valid transition probabilities", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("base") |>
    add_state("healthy", initial_prob = 0.7) |>
    add_state("sick", initial_prob = 0.2) |>
    add_state("dead", initial_prob = 0.1) |>
    add_variable("p_healthy_healthy", 0.7) |>
    add_variable("p_healthy_sick", 0.2) |>
    add_variable("p_healthy_dead", 0.1) |>
    add_transition("healthy", "healthy", "p_healthy_healthy") |>
    add_transition("healthy", "sick", "p_healthy_sick") |>
    add_transition("healthy", "dead", "p_healthy_dead") |>
    add_transition("sick", "sick", "0.8") |>
    add_transition("sick", "dead", "0.2") |>
    add_transition("dead", "dead", "1") |>
    add_multivariate_sampling(
      name = "healthy_transitions",
      type = "dirichlet",
      variables = c("p_healthy_healthy", "p_healthy_sick", "p_healthy_dead"),
      n = 100
    )

  normalized_model <- openqaly:::normalize_and_validate_model(model)
  parsed_model <- openqaly:::parse_model(normalized_model)

  segments <- openqaly:::get_segments(parsed_model) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed_model, as_tibble(seg))
    }) %>%
    ungroup()

  set.seed(42)
  sampled_raw <- openqaly:::resample(parsed_model, 100, segments, seed = 42)

  sampled <- sampled_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)

  # Probabilities sum to 1
  prob_sums <- sampled$p_healthy_healthy + sampled$p_healthy_sick + sampled$p_healthy_dead
  expect_equal(prob_sums, rep(1, 100), tolerance = 1e-10)

  # All between 0 and 1
  expect_true(all(sampled$p_healthy_healthy >= 0 & sampled$p_healthy_healthy <= 1))
  expect_true(all(sampled$p_healthy_sick >= 0 & sampled$p_healthy_sick <= 1))
  expect_true(all(sampled$p_healthy_dead >= 0 & sampled$p_healthy_dead <= 1))

  # Means match expected from Dirichlet (alpha = n * base_case)
  expect_equal(mean(sampled$p_healthy_healthy), 0.7, tolerance = 0.05)
  expect_equal(mean(sampled$p_healthy_sick), 0.2, tolerance = 0.05)
  expect_equal(mean(sampled$p_healthy_dead), 0.1, tolerance = 0.05)
})

test_that("Dirichlet with explicit alpha works", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("base") |>
    add_state("healthy", initial_prob = 0.7) |>
    add_state("sick", initial_prob = 0.2) |>
    add_state("dead", initial_prob = 0.1) |>
    add_variable("p_healthy_healthy", 0.7) |>
    add_variable("p_healthy_sick", 0.2) |>
    add_variable("p_healthy_dead", 0.1) |>
    add_transition("healthy", "healthy", "p_healthy_healthy") |>
    add_transition("healthy", "sick", "p_healthy_sick") |>
    add_transition("healthy", "dead", "p_healthy_dead") |>
    add_transition("sick", "sick", "0.8") |>
    add_transition("sick", "dead", "0.2") |>
    add_transition("dead", "dead", "1") |>
    add_multivariate_sampling(
      name = "healthy_transitions",
      type = "dirichlet",
      variables = c("p_healthy_healthy", "p_healthy_sick", "p_healthy_dead"),
      n = 100
    )

  normalized_model <- openqaly:::normalize_and_validate_model(model)
  parsed_model <- openqaly:::parse_model(normalized_model)

  segments <- openqaly:::get_segments(parsed_model) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed_model, as_tibble(seg))
    }) %>%
    ungroup()

  sampled_raw <- openqaly:::resample(parsed_model, 100, segments, seed = 42)

  sampled <- sampled_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)

  prob_sums <- sampled$p_healthy_healthy + sampled$p_healthy_sick + sampled$p_healthy_dead
  expect_equal(prob_sums, rep(1, 100), tolerance = 1e-10)
  expect_equal(mean(sampled$p_healthy_healthy), 0.7, tolerance = 0.05)
})

test_that("Multivariate normal generates correlated parameters", {
  # 3x3 covariance matrix for cost, qaly, and utility
  # sd_cost=100, sd_qaly=0.5, sd_util=0.05
  # cor(cost,qaly)=0.7, cor(cost,util)=-0.3, cor(qaly,util)=0.5
  cov_mat <- data.frame(
    treatment_cost = c(100^2, 0.7 * 100 * 0.5, -0.3 * 100 * 0.05),
    treatment_qaly = c(0.7 * 100 * 0.5, 0.5^2, 0.5 * 0.5 * 0.05),
    treatment_util = c(-0.3 * 100 * 0.05, 0.5 * 0.5 * 0.05, 0.05^2)
  )

  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("base") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("treatment_cost", 1000) |>
    add_variable("treatment_qaly", 5) |>
    add_variable("treatment_util", 0.8) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "dead", "0.1") |>
    add_transition("dead", "dead", "1") |>
    add_table("cost_qaly_util_cov", cov_mat) |>
    add_multivariate_sampling(
      name = "cost_qaly_util",
      type = "mvnormal",
      variables = c("treatment_cost", "treatment_qaly", "treatment_util"),
      covariance = "cost_qaly_util_cov"
    )

  normalized_model <- openqaly:::normalize_and_validate_model(model)
  parsed_model <- openqaly:::parse_model(normalized_model)

  segments <- openqaly:::get_segments(parsed_model) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed_model, as_tibble(seg))
    }) %>%
    ungroup()

  set.seed(123)
  sampled_raw <- openqaly:::resample(parsed_model, 1000, segments, seed = 123)

  sampled <- sampled_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)

  # Check means
  expect_equal(mean(sampled$treatment_cost), 1000, tolerance = 10)
  expect_equal(mean(sampled$treatment_qaly), 5, tolerance = 0.05)
  expect_equal(mean(sampled$treatment_util), 0.8, tolerance = 0.01)

  # Check standard deviations
  expect_equal(sd(sampled$treatment_cost), 100, tolerance = 10)
  expect_equal(sd(sampled$treatment_qaly), 0.5, tolerance = 0.05)
  expect_equal(sd(sampled$treatment_util), 0.05, tolerance = 0.005)

  # Check correlations
  expect_equal(cor(sampled$treatment_cost, sampled$treatment_qaly), 0.7, tolerance = 0.05)
  expect_equal(cor(sampled$treatment_cost, sampled$treatment_util), -0.3, tolerance = 0.1)
  expect_equal(cor(sampled$treatment_qaly, sampled$treatment_util), 0.5, tolerance = 0.1)
})

test_that("Multinomial distribution generates valid count outcomes", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("base") |>
    add_state("state1", initial_prob = 0.5) |>
    add_state("state2", initial_prob = 0.3) |>
    add_state("state3", initial_prob = 0.2) |>
    # Base case values are counts (50, 30, 20 out of 100)
    add_variable("n_state1", 50) |>
    add_variable("n_state2", 30) |>
    add_variable("n_state3", 20) |>
    add_transition("state1", "state1", "1") |>
    add_transition("state2", "state2", "1") |>
    add_transition("state3", "state3", "1") |>
    add_multivariate_sampling(
      name = "initial_states",
      type = "multinomial",
      variables = c("n_state1", "n_state2", "n_state3")
    )

  normalized_model <- openqaly:::normalize_and_validate_model(model)
  parsed_model <- openqaly:::parse_model(normalized_model)

  segments <- openqaly:::get_segments(parsed_model) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed_model, as_tibble(seg))
    }) %>%
    ungroup()

  set.seed(456)
  sampled_raw <- openqaly:::resample(parsed_model, 100, segments, seed = 456)

  sampled <- sampled_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)

  # Each row sums to total base case count (50 + 30 + 20 = 100)
  row_sums <- sampled$n_state1 + sampled$n_state2 + sampled$n_state3
  expect_equal(row_sums, rep(100, 100))

  # Values are non-negative integers
  expect_true(all(sampled$n_state1 >= 0))
  expect_true(all(sampled$n_state2 >= 0))
  expect_true(all(sampled$n_state3 >= 0))

  # Approximate probabilities with larger sample
  sampled_large_raw <- openqaly:::resample(parsed_model, 1000, segments, seed = 789)
  sampled_large <- sampled_large_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)
  expect_equal(mean(sampled_large$n_state1), 50, tolerance = 3)
  expect_equal(mean(sampled_large$n_state2), 30, tolerance = 3)
  expect_equal(mean(sampled_large$n_state3), 20, tolerance = 3)
})

test_that("Strategy-specific multivariate sampling works correctly", {
  cov_mat <- data.frame(
    cost = c(100, 0.5),
    qaly = c(0.5, 0.01)
  )

  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("standard") |>
    add_strategy("intervention") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("cost_param", 100, strategy = "standard") |>
    add_variable("cost_param", 200, strategy = "intervention") |>
    add_variable("qaly_param", 1, strategy = "standard") |>
    add_variable("qaly_param", 2, strategy = "intervention") |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "dead", "0.1") |>
    add_transition("dead", "dead", "1") |>
    add_value("cost", "cost_param") |>
    add_value("qaly", "qaly_param") |>
    add_table("cov_mat", cov_mat) |>
    add_multivariate_sampling(
      name = "strategy_specific",
      type = "mvnormal",
      variables = c("cost_param", "qaly_param"),
      strategy = "intervention",
      covariance = "cov_mat"
    )

  normalized_model <- openqaly:::normalize_and_validate_model(model)
  parsed_model <- openqaly:::parse_model(normalized_model)

  segments <- openqaly:::get_segments(parsed_model) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed_model, as_tibble(seg))
    }) %>%
    ungroup()

  sampled_raw <- openqaly:::resample(parsed_model, 500, segments, seed = 321)

  standard_raw <- filter(sampled_raw, strategy == "standard")
  intervention_raw <- filter(sampled_raw, strategy == "intervention")

  # Standard strategy has no parameter overrides
  standard_override_lengths <- sapply(standard_raw$parameter_overrides, length)
  expect_true(all(standard_override_lengths == 0))

  # Intervention strategy has parameter overrides
  intervention_override_lengths <- sapply(intervention_raw$parameter_overrides, length)
  expect_true(all(intervention_override_lengths == 2))

  intervention_samples <- intervention_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)

  expect_true(length(unique(intervention_samples$cost_param)) > 1)
  expect_true(length(unique(intervention_samples$qaly_param)) > 1)

  # Means for intervention (base case: cost=200, qaly=2)
  expect_equal(mean(intervention_samples$cost_param), 200, tolerance = 3)
  expect_equal(mean(intervention_samples$qaly_param), 2, tolerance = 0.03)
})

test_that("Multivariate sampling validation catches conflicts", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_variable("p_transition", 0.1, sampling = beta(mean = 0.1, sd = 0.02)) |>
    add_variable("p_other", 0.9) |>
    add_transition("healthy", "healthy", "1") |>
    add_multivariate_sampling(
      name = "conflict",
      type = "dirichlet",
      variables = c("p_transition", "p_other"),
      n = 100
    )

  expect_error(
    openqaly:::validate_sampling_spec(model),
    "appears in both variables.sampling and multivariate_sampling"
  )
})

test_that("Multivariate sampling with missing variables is caught", {
  cov_mat <- data.frame(v1 = c(1, 0), v2 = c(0, 1))

  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_transition("healthy", "healthy", "1") |>
    add_table("cov", cov_mat) |>
    add_multivariate_sampling(
      name = "missing",
      type = "mvnormal",
      variables = c("missing_var1", "missing_var2"),
      covariance = "cov"
    )

  expect_error(
    openqaly:::validate_sampling_spec(model),
    "Variables in multivariate_sampling not found in variables table"
  )
})

test_that("Covariance is stored as oq_formula", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_variable("v1", 1) |>
    add_variable("v2", 2) |>
    add_transition("healthy", "healthy", "1") |>
    add_table("my_cov", data.frame(a = c(1, 0), b = c(0, 1))) |>
    add_multivariate_sampling(
      name = "test",
      type = "mvnormal",
      variables = c("v1", "v2"),
      covariance = "my_cov"
    )

  mv_spec <- model$multivariate_sampling[[1]]
  expect_true(inherits(mv_spec$covariance, "oq_formula"))
  expect_equal(as.character(mv_spec$covariance), "my_cov")
})

test_that("JSON round-trip preserves multivariate sampling", {
  cov_mat <- data.frame(v1 = c(100, 5, 2), v2 = c(5, 50, 3), v3 = c(2, 3, 25))

  original_model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("v1", 100) |>
    add_variable("v2", 10) |>
    add_variable("v3", 5) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "dead", "0.1") |>
    add_transition("dead", "dead", "1") |>
    add_table("test_cov", cov_mat) |>
    add_multivariate_sampling(
      name = "test_mvnormal",
      type = "mvnormal",
      variables = c("v1", "v2", "v3"),
      covariance = "test_cov",
      description = "Test multivariate normal"
    )

  json_string <- write_model_json(original_model)
  loaded_model <- read_model_json(text = json_string)

  expect_equal(length(original_model$multivariate_sampling),
               length(loaded_model$multivariate_sampling))

  if (length(loaded_model$multivariate_sampling) > 0) {
    mv_orig <- original_model$multivariate_sampling[[1]]
    mv_load <- loaded_model$multivariate_sampling[[1]]

    expect_equal(mv_orig$name, mv_load$name)
    expect_equal(mv_orig$type, mv_load$type)
    expect_equal(mv_orig$description, mv_load$description)
    expect_equal(mv_orig$variables, mv_load$variables)
    expect_equal(as.character(mv_orig$covariance), as.character(mv_load$covariance))
  }
})

test_that("R code generation includes multivariate sampling", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_variable("p1", 0.33) |>
    add_variable("p2", 0.67) |>
    add_transition("healthy", "healthy", "1") |>
    add_multivariate_sampling(
      name = "test_sampling",
      type = "dirichlet",
      variables = c("p1", "p2"),
      n = 100,
      description = "Test description"
    )

  r_code <- model_to_r_code(model)

  expect_true(any(grepl("add_multivariate_sampling", r_code)))
  expect_true(any(grepl("test_sampling", r_code)))
  expect_true(any(grepl('type = "dirichlet"', r_code)))
  expect_true(any(grepl("n = 100", r_code)))
  expect_true(any(grepl("Test description", r_code)))
})

# --- edit_multivariate_sampling / remove_multivariate_sampling tests ---

make_mv_test_model <- function() {
  define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("base") |>
    add_state("healthy", initial_prob = 0.7) |>
    add_state("sick", initial_prob = 0.2) |>
    add_state("dead", initial_prob = 0.1) |>
    add_variable("p_healthy_healthy", 0.7) |>
    add_variable("p_healthy_sick", 0.2) |>
    add_variable("p_healthy_dead", 0.1) |>
    add_transition("healthy", "healthy", "p_healthy_healthy") |>
    add_transition("healthy", "sick", "p_healthy_sick") |>
    add_transition("healthy", "dead", "p_healthy_dead") |>
    add_transition("sick", "sick", "0.8") |>
    add_transition("sick", "dead", "0.2") |>
    add_transition("dead", "dead", "1") |>
    add_multivariate_sampling(
      name = "healthy_transitions",
      type = "dirichlet",
      variables = c("p_healthy_healthy", "p_healthy_sick", "p_healthy_dead"),
      n = 100,
      description = "Original description"
    )
}

test_that("edit_multivariate_sampling updates n", {
  model <- make_mv_test_model()
  model <- edit_multivariate_sampling(
    model, "healthy_transitions",
    n = 200
  )
  expect_equal(model$multivariate_sampling[[1]]$n, 200)
})

test_that("edit_multivariate_sampling updates variables", {
  model <- make_mv_test_model()
  model <- edit_multivariate_sampling(
    model, "healthy_transitions",
    variables = c("p_healthy_sick", "p_healthy_dead")
  )
  expect_equal(length(model$multivariate_sampling[[1]]$variables), 2)
  expect_equal(model$multivariate_sampling[[1]]$variables,
               c("p_healthy_sick", "p_healthy_dead"))
})

test_that("edit_multivariate_sampling updates strategy/group", {
  model <- make_mv_test_model()
  model <- edit_multivariate_sampling(
    model, "healthy_transitions",
    strategy = "base",
    group = "subgroup1"
  )
  expect_equal(model$multivariate_sampling[[1]]$strategy, "base")
  expect_equal(model$multivariate_sampling[[1]]$group, "subgroup1")
})

test_that("edit_multivariate_sampling updates description", {
  model <- make_mv_test_model()
  model <- edit_multivariate_sampling(
    model, "healthy_transitions",
    description = "Updated description"
  )
  expect_equal(model$multivariate_sampling[[1]]$description, "Updated description")
})

test_that("edit_multivariate_sampling renames", {
  model <- make_mv_test_model()
  model <- edit_multivariate_sampling(
    model, "healthy_transitions",
    new_name = "renamed_spec"
  )
  expect_equal(model$multivariate_sampling[[1]]$name, "renamed_spec")
})

test_that("edit_multivariate_sampling retains unmodified fields", {
  model <- make_mv_test_model()
  orig_type <- model$multivariate_sampling[[1]]$type
  orig_variables <- model$multivariate_sampling[[1]]$variables
  orig_n <- model$multivariate_sampling[[1]]$n

  model <- edit_multivariate_sampling(
    model, "healthy_transitions",
    description = "Only description changed"
  )

  expect_equal(model$multivariate_sampling[[1]]$type, orig_type)
  expect_equal(model$multivariate_sampling[[1]]$variables, orig_variables)
  expect_equal(model$multivariate_sampling[[1]]$n, orig_n)
  expect_equal(model$multivariate_sampling[[1]]$description, "Only description changed")
})

test_that("edit_multivariate_sampling errors on not found", {
  model <- make_mv_test_model()
  expect_error(
    edit_multivariate_sampling(model, "nonexistent", description = "x"),
    "not found"
  )
})

test_that("edit_multivariate_sampling rename duplicate errors", {
  model <- make_mv_test_model() |>
    add_multivariate_sampling(
      name = "second_spec",
      type = "dirichlet",
      variables = c("p_healthy_healthy", "p_healthy_sick"),
      n = 50
    )
  expect_error(
    edit_multivariate_sampling(model, "healthy_transitions", new_name = "second_spec"),
    "already exists"
  )
})

test_that("remove_multivariate_sampling removes entry", {
  model <- make_mv_test_model()
  expect_equal(length(model$multivariate_sampling), 1)
  model <- remove_multivariate_sampling(model, "healthy_transitions")
  expect_equal(length(model$multivariate_sampling), 0)
})

test_that("remove_multivariate_sampling errors on not found", {
  model <- make_mv_test_model()
  expect_error(
    remove_multivariate_sampling(model, "nonexistent"),
    "not found"
  )
})

test_that("remove_multivariate_sampling removes correct entry from multiple", {
  model <- make_mv_test_model() |>
    add_multivariate_sampling(
      name = "second_spec",
      type = "dirichlet",
      variables = c("p_healthy_healthy", "p_healthy_sick"),
      n = 50
    )
  expect_equal(length(model$multivariate_sampling), 2)
  model <- remove_multivariate_sampling(model, "healthy_transitions")
  expect_equal(length(model$multivariate_sampling), 1)
  expect_equal(model$multivariate_sampling[[1]]$name, "second_spec")
})
