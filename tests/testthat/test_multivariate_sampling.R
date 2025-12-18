context("Multivariate Sampling")
suppressMessages(library(dplyr))

test_that("Dirichlet distribution generates valid transition probabilities", {
  # Create a simple model with Dirichlet sampling
  model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("healthy", initial_prob = 0.7) |>
    add_state("sick", initial_prob = 0.2) |>
    add_state("dead", initial_prob = 0.1) |>
    add_variable("alpha_healthy", 70) |>
    add_variable("alpha_sick", 20) |>
    add_variable("alpha_dead", 10) |>
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
      distribution = dirichlet(c(alpha_healthy, alpha_sick, alpha_dead)),
      variables = c("p_healthy_healthy", "p_healthy_sick", "p_healthy_dead")
    )

  # Run model to get segments
  result <- run_model(model)
  segments <- result$segments

  # Prepare segments for sampling
  for (i in 1:nrow(segments)) {
    segments[i, ] <- heRomod2:::prepare_segment_for_sampling(model, segments[i, ])
  }

  # Sample parameters
  set.seed(42)
  sampled <- heRomod2:::resample(model, 100, segments, seed = 42)

  # Check that probabilities sum to 1
  prob_sums <- sampled$p_healthy_healthy + sampled$p_healthy_sick + sampled$p_healthy_dead
  expect_equal(prob_sums, rep(1, 100), tolerance = 1e-10)

  # Check that all probabilities are between 0 and 1
  expect_true(all(sampled$p_healthy_healthy >= 0 & sampled$p_healthy_healthy <= 1))
  expect_true(all(sampled$p_healthy_sick >= 0 & sampled$p_healthy_sick <= 1))
  expect_true(all(sampled$p_healthy_dead >= 0 & sampled$p_healthy_dead <= 1))

  # Check that means are approximately as expected from Dirichlet
  expect_equal(mean(sampled$p_healthy_healthy), 0.7, tolerance = 0.05)
  expect_equal(mean(sampled$p_healthy_sick), 0.2, tolerance = 0.05)
  expect_equal(mean(sampled$p_healthy_dead), 0.1, tolerance = 0.05)
})

test_that("Multivariate normal generates correlated parameters", {
  # Create model with mvnormal sampling
  model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("mean_cost", 1000) |>
    add_variable("mean_qaly", 5) |>
    add_variable("sd_cost", 100) |>
    add_variable("sd_qaly", 0.5) |>
    add_variable("correlation", 0.7) |>
    add_variable("treatment_cost", 1000) |>
    add_variable("treatment_qaly", 5) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "dead", "0.1") |>
    add_transition("dead", "dead", "1") |>
    add_multivariate_sampling(
      name = "cost_qaly",
      distribution = mvnormal(mean = c(mean_cost, mean_qaly), sd = c(sd_cost, sd_qaly), cor = correlation),
      variables = c("treatment_cost", "treatment_qaly")
    )

  # Run model and sample
  result <- run_model(model)
  segments <- result$segments
  for (i in 1:nrow(segments)) {
    segments[i, ] <- heRomod2:::prepare_segment_for_sampling(model, segments[i, ])
  }

  set.seed(123)
  sampled <- heRomod2:::resample(model, 1000, segments, seed = 123)

  # Check means
  expect_equal(mean(sampled$treatment_cost), 1000, tolerance = 10)
  expect_equal(mean(sampled$treatment_qaly), 5, tolerance = 0.05)

  # Check standard deviations
  expect_equal(sd(sampled$treatment_cost), 100, tolerance = 10)
  expect_equal(sd(sampled$treatment_qaly), 0.5, tolerance = 0.05)

  # Check correlation
  actual_cor <- cor(sampled$treatment_cost, sampled$treatment_qaly)
  expect_equal(actual_cor, 0.7, tolerance = 0.05)
})

test_that("Multinomial distribution generates valid categorical outcomes", {
  # Create model with multinomial sampling
  model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("state1", initial_prob = 0.5) |>
    add_state("state2", initial_prob = 0.3) |>
    add_state("state3", initial_prob = 0.2) |>
    add_variable("init_state1", 0.5) |>
    add_variable("init_state2", 0.3) |>
    add_variable("init_state3", 0.2) |>
    add_transition("state1", "state1", "1") |>
    add_transition("state2", "state2", "1") |>
    add_transition("state3", "state3", "1") |>
    add_multivariate_sampling(
      name = "initial_states",
      distribution = multinomial(size = 1, prob = c(0.5, 0.3, 0.2)),
      variables = c("init_state1", "init_state2", "init_state3")
    )

  # Run model and sample
  result <- run_model(model)
  segments <- result$segments
  for (i in 1:nrow(segments)) {
    segments[i, ] <- heRomod2:::prepare_segment_for_sampling(model, segments[i, ])
  }

  set.seed(456)
  sampled <- heRomod2:::resample(model, 100, segments, seed = 456)

  # Check that each row sums to 1 (one-hot encoding)
  row_sums <- sampled$init_state1 + sampled$init_state2 + sampled$init_state3
  expect_equal(row_sums, rep(1, 100))

  # Check that values are binary (0 or 1)
  expect_true(all(sampled$init_state1 %in% c(0, 1)))
  expect_true(all(sampled$init_state2 %in% c(0, 1)))
  expect_true(all(sampled$init_state3 %in% c(0, 1)))

  # Check approximate probabilities (with larger sample for better accuracy)
  sampled_large <- heRomod2:::resample(model, 1000, segments, seed = 789)
  expect_equal(mean(sampled_large$init_state1), 0.5, tolerance = 0.05)
  expect_equal(mean(sampled_large$init_state2), 0.3, tolerance = 0.05)
  expect_equal(mean(sampled_large$init_state3), 0.2, tolerance = 0.05)
})

test_that("Segment-specific multivariate sampling works correctly", {
  # Create model with strategy-specific multivariate sampling
  model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_strategy("standard") |>
    add_strategy("intervention") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("base_cost", 100, strategy = "standard") |>
    add_variable("base_cost", 200, strategy = "intervention") |>
    add_variable("base_qaly", 1, strategy = "standard") |>
    add_variable("base_qaly", 2, strategy = "intervention") |>
    add_variable("cost_param", 100) |>
    add_variable("qaly_param", 1) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "dead", "0.1") |>
    add_transition("dead", "dead", "1") |>
    add_value("cost", "cost_param") |>
    add_value("qaly", "qaly_param") |>
    add_multivariate_sampling(
      name = "strategy_specific",
      distribution = mvnormal(mean = c(base_cost, base_qaly), sd = c(10, 0.1), cor = 0.5),
      variables = tibble(
        variable = c("cost_param", "qaly_param"),
        strategy = c("intervention", "intervention"),
        group = c("", "")
      )
    )

  # Run model and sample
  result <- run_model(model)
  segments <- result$segments
  for (i in 1:nrow(segments)) {
    segments[i, ] <- heRomod2:::prepare_segment_for_sampling(model, segments[i, ])
  }

  sampled <- heRomod2:::resample(model, 100, segments, seed = 321)

  # Separate by strategy
  standard_samples <- filter(sampled, strategy == "standard")
  intervention_samples <- filter(sampled, strategy == "intervention")

  # Check that standard strategy doesn't have multivariate sampling
  # (variables should be constant at base values)
  expect_equal(length(unique(standard_samples$cost_param)), 1)
  expect_equal(length(unique(standard_samples$qaly_param)), 1)

  # Check that intervention strategy has multivariate sampling
  expect_true(length(unique(intervention_samples$cost_param)) > 1)
  expect_true(length(unique(intervention_samples$qaly_param)) > 1)

  # Check means for intervention
  expect_equal(mean(intervention_samples$cost_param), 200, tolerance = 5)
  expect_equal(mean(intervention_samples$qaly_param), 2, tolerance = 0.05)

  # Check correlation for intervention
  cor_intervention <- cor(intervention_samples$cost_param, intervention_samples$qaly_param)
  expect_equal(cor_intervention, 0.5, tolerance = 0.1)
})

test_that("Multivariate sampling validation catches conflicts", {
  # Create model with conflicting sampling specifications
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_variable("p_transition", 0.1, sampling = beta(mean = 0.1, sd = 0.02)) |>
    add_transition("healthy", "healthy", "1") |>
    add_multivariate_sampling(
      name = "conflict",
      distribution = dirichlet(c(10, 90)),
      variables = c("p_transition", "p_other")  # p_transition already has univariate sampling
    )

  # Should detect conflict
  expect_error(
    heRomod2:::validate_sampling_spec(model),
    "Variables appear in both variables.sampling and multivariate_sampling"
  )
})

test_that("Multivariate sampling with missing variables is caught", {
  # Create model with multivariate sampling referencing non-existent variables
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_transition("healthy", "healthy", "1") |>
    add_multivariate_sampling(
      name = "missing",
      distribution = mvnormal(mean = c(0, 0), sd = c(1, 1), cor = 0),
      variables = c("missing_var1", "missing_var2")
    )

  # Should detect missing variables
  expect_error(
    heRomod2:::validate_sampling_spec(model),
    "Variables in multivariate_sampling not found in variables table"
  )
})

test_that("Excel round-trip preserves multivariate sampling", {
  # Create model with multivariate sampling
  original_model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("healthy", initial_prob = 0.7) |>
    add_state("sick", initial_prob = 0.2) |>
    add_state("dead", initial_prob = 0.1) |>
    add_variable("alpha1", 70) |>
    add_variable("alpha2", 20) |>
    add_variable("alpha3", 10) |>
    add_variable("p1", 0.7) |>
    add_variable("p2", 0.2) |>
    add_variable("p3", 0.1) |>
    add_transition("healthy", "healthy", "p1") |>
    add_transition("healthy", "sick", "p2") |>
    add_transition("healthy", "dead", "p3") |>
    add_transition("sick", "sick", "1") |>
    add_transition("dead", "dead", "1") |>
    add_multivariate_sampling(
      name = "test_dirichlet",
      distribution = dirichlet(c(alpha1, alpha2, alpha3)),
      variables = c("p1", "p2", "p3"),
      description = "Test Dirichlet distribution"
    )

  # Write to Excel
  temp_path <- tempfile(pattern = "test_excel_mv_", fileext = "")
  write_model(original_model, temp_path, format = "excel")

  # Read back from Excel
  loaded_model <- read_model(temp_path)

  # Check multivariate sampling preserved
  expect_equal(length(original_model$multivariate_sampling),
               length(loaded_model$multivariate_sampling))

  if (length(loaded_model$multivariate_sampling) > 0) {
    mv_orig <- original_model$multivariate_sampling[[1]]
    mv_load <- loaded_model$multivariate_sampling[[1]]

    expect_equal(mv_orig$name, mv_load$name)
    expect_equal(mv_orig$distribution, mv_load$distribution)
    expect_equal(mv_orig$description, mv_load$description)
    expect_equal(nrow(mv_orig$variables), nrow(mv_load$variables))
    expect_equal(mv_orig$variables$variable, mv_load$variables$variable)
  }

  # Clean up
  unlink(temp_path, recursive = TRUE)
})

test_that("JSON round-trip preserves multivariate sampling", {
  # Create model with multivariate sampling
  original_model <- define_model("markov") |>
    set_settings(n_cycles = 10) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("m1", 100) |>
    add_variable("m2", 10) |>
    add_variable("s1", 10) |>
    add_variable("s2", 1) |>
    add_variable("r", 0.6) |>
    add_variable("v1", 100) |>
    add_variable("v2", 10) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "dead", "0.1") |>
    add_transition("dead", "dead", "1") |>
    add_multivariate_sampling(
      name = "test_mvnormal",
      distribution = mvnormal(mean = c(m1, m2), sd = c(s1, s2), cor = r),
      variables = c("v1", "v2"),
      description = "Test multivariate normal"
    )

  # Convert to JSON and back
  json_string <- write_model_json(original_model)
  loaded_model <- read_model_json(json_string)

  # Check multivariate sampling preserved
  expect_equal(length(original_model$multivariate_sampling),
               length(loaded_model$multivariate_sampling))

  if (length(loaded_model$multivariate_sampling) > 0) {
    mv_orig <- original_model$multivariate_sampling[[1]]
    mv_load <- loaded_model$multivariate_sampling[[1]]

    expect_equal(mv_orig$name, mv_load$name)
    expect_equal(mv_orig$distribution, mv_load$distribution)
    expect_equal(mv_orig$description, mv_load$description)
    expect_equal(nrow(mv_orig$variables), nrow(mv_load$variables))
    expect_equal(mv_orig$variables$variable, mv_load$variables$variable)
  }
})

test_that("R code generation includes multivariate sampling", {
  # Create model with multivariate sampling
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_variable("a1", 10) |>
    add_variable("a2", 20) |>
    add_variable("p1", 0.33) |>
    add_variable("p2", 0.67) |>
    add_transition("healthy", "healthy", "1") |>
    add_multivariate_sampling(
      name = "test_sampling",
      distribution = dirichlet(c(a1, a2)),
      variables = c("p1", "p2"),
      description = "Test description"
    )

  # Generate R code
  r_code <- model_to_r_code(model)

  # Check that multivariate sampling is included
  expect_true(any(grepl("add_multivariate_sampling", r_code)))
  expect_true(any(grepl("test_sampling", r_code)))
  expect_true(any(grepl("dirichlet\\(c\\(a1, a2\\)\\)", r_code)))
  expect_true(any(grepl("Test description", r_code)))

  # Execute generated code
  temp_file <- tempfile(fileext = ".R")
  writeLines(r_code, temp_file)

  env <- new.env()
  source(temp_file, local = env)

  # Check that model was created with multivariate sampling
  expect_true(exists("model", envir = env))
  generated_model <- env$model
  expect_equal(length(generated_model$multivariate_sampling), 1)

  # Clean up
  unlink(temp_file)
})