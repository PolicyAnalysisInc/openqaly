context("Sampling")
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(purrr))

test_that("parameter sampling yields correct averages", {

  # Run a test model
  model <- system.file("models", "checkimab", package = "openqaly") %>%
    read_model()
  eval_model <- run_model(model)

  # Generate sampled parameter values
  sampled_segments_raw <- openqaly:::resample(model, 1000, eval_model$segments, seed = 10)

  # Extract scalar parameter values (exclude bootstrap data frames like eq5d_data)
  sampled_segments <- sampled_segments_raw %>%
    mutate(
      start_age = map_dbl(parameter_overrides, "start_age"),
      p_death_ae = map_dbl(parameter_overrides, "p_death_ae"),
      cost_nausea = map_dbl(parameter_overrides, "cost_nausea")
    ) %>%
    select(-parameter_overrides)

  # Separate the sampled values by strategy/group
  chemo_m_lt35 <- dplyr::filter(
    sampled_segments,
    strategy == 'chemo',
    group == 'male_age_lt_35'
  )
  chemo_m_ge35 <- dplyr::filter(
    sampled_segments,
    strategy == 'chemo',
    group == 'male_age_ge_35'
  )
  chemo_f_lt35 <- dplyr::filter(
    sampled_segments,
    strategy == 'chemo',
    group == 'female_age_lt_35'
  )
  chemo_f_ge35 <- dplyr::filter(
    sampled_segments,
    strategy == 'chemo',
    group == 'female_age_ge_35'
  )
  target_m_lt35 <- dplyr::filter(
    sampled_segments,
    strategy == 'target',
    group == 'male_age_lt_35'
  )
  target_m_ge35 <- dplyr::filter(
    sampled_segments,
    strategy == 'target',
    group == 'male_age_ge_35'
  )
  target_f_lt35 <- dplyr::filter(
    sampled_segments,
    strategy == 'target',
    group == 'female_age_lt_35'
  )
  target_f_ge35 <- dplyr::filter(
    sampled_segments,
    strategy == 'target',
    group == 'female_age_ge_35'
  )
  check_m_lt35 <- dplyr::filter(
    sampled_segments,
    strategy == 'check',
    group == 'male_age_lt_35'
  )
  check_m_ge35 <- dplyr::filter(
    sampled_segments,
    strategy == 'check',
    group == 'male_age_ge_35'
  )
  check_f_lt35 <- dplyr::filter(
    sampled_segments,
    strategy == 'check',
    group == 'female_age_lt_35'
  )
  check_f_ge35 <- dplyr::filter(
    sampled_segments,
    strategy == 'check',
    group == 'female_age_ge_35'
  )

  # Check results
  expect_equal(26, mean(chemo_m_lt35$start_age), tolerance = 1e-1)
  expect_equal(45, mean(chemo_m_ge35$start_age), tolerance = 1e-1)
  expect_equal(27, mean(chemo_f_lt35$start_age), tolerance = 1e-1)
  expect_equal(48, mean(chemo_f_ge35$start_age), tolerance = 1e-1)
  expect_equal(26, mean(target_m_lt35$start_age), tolerance = 1e-1)
  expect_equal(45, mean(target_m_ge35$start_age), tolerance = 1e-1)
  expect_equal(27, mean(target_f_lt35$start_age), tolerance = 1e-1)
  expect_equal(48, mean(target_f_ge35$start_age), tolerance = 1e-1)
  expect_equal(26, mean(check_m_lt35$start_age), tolerance = 1e-1)
  expect_equal(45, mean(check_m_ge35$start_age), tolerance = 1e-1)
  expect_equal(27, mean(check_f_lt35$start_age), tolerance = 1e-1)
  expect_equal(48, mean(check_f_ge35$start_age), tolerance = 1e-1)
  expect_equal(0.001, mean(chemo_m_lt35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.001, mean(chemo_m_ge35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.001, mean(chemo_f_lt35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.001, mean(chemo_f_ge35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0005, mean(target_m_lt35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0005, mean(target_m_ge35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0005, mean(target_f_lt35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0005, mean(target_f_ge35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0006, mean(check_m_lt35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0006, mean(check_m_ge35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0006, mean(check_f_lt35$p_death_ae), tolerance = 1e-3)
  expect_equal(0.0006, mean(check_f_ge35$p_death_ae), tolerance = 1e-3)
  expect_equal(1000, mean(chemo_m_lt35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(chemo_m_ge35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(chemo_f_lt35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(chemo_f_ge35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(target_m_lt35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(target_m_ge35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(target_f_lt35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(target_f_ge35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(check_m_lt35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(check_m_ge35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(check_f_lt35$cost_nausea), tolerance = 1e-1)
  expect_equal(1000, mean(check_f_ge35$cost_nausea), tolerance = 1e-1)
})

test_that("parameter sampling with random seed is deterministic", {
  # Setup: Run a test model
  model <- system.file("models", "checkimab", package = "openqaly") %>%
    read_model()
  eval_model <- run_model(model)

  # First run with seed = 1
  sampled_segments_1 <- openqaly:::resample(model, 1, eval_model$segments, seed = 1)

  # Second run with same seed = 1
  sampled_segments_2 <- openqaly:::resample(model, 1, eval_model$segments, seed = 1)

  # Helper to extract scalar parameters
  extract_params <- function(sampled_raw) {
    sampled_raw %>%
      mutate(
        start_age = map_dbl(parameter_overrides, "start_age"),
        p_death_ae = map_dbl(parameter_overrides, "p_death_ae"),
        cost_nausea = map_dbl(parameter_overrides, "cost_nausea")
      ) %>%
      select(-parameter_overrides)
  }

  params_1 <- extract_params(sampled_segments_1)
  params_2 <- extract_params(sampled_segments_2)

  # Verify determinism: same seed produces identical results
  expect_equal(params_1$start_age, params_2$start_age)
  expect_equal(params_1$p_death_ae, params_2$p_death_ae)
  expect_equal(params_1$cost_nausea, params_2$cost_nausea)

  # Sanity check: verify expected number of segments
  expect_equal(nrow(params_1), 12)
})

test_that("errors in distribution parsing are handled properly", {
  
  # Run a test model
  model <- system.file("models", "checkimab", package = "openqaly") %>%
    read_model()
  
  # Model with syntax error
  syntax_error <- model
  syntax_error$variables$sampling[1] <- 'normal(26, 2))'
  syntax_error_eval <- run_model(syntax_error)
  expect_error(
    sampled_segments <- openqaly:::resample(syntax_error, 10, syntax_error_eval$segments, seed = 1),
    regexp = "Failed to evaluate sampling distribution.*start_age.*Error in formula syntax"
  )
  
  # Model with no sampling distributions
  no_dist <- model
  no_dist$variables$sampling <- ''
  no_dist_eval <- run_model(no_dist)
  expect_error(
    sampled_segments <- openqaly:::resample(no_dist, 10, no_dist_eval$segments, seed = 1),
    'Error in variables specification, no sampling distributions were specified.'
  )
  
  # Model with missing sampling column
  missing_col <- model
  missing_col$variables <- dplyr::select(missing_col$variables, -sampling)
  missing_col_eval <- run_model(missing_col)
  expect_error(
    sampled_segments <- openqaly:::resample(missing_col, 10, missing_col_eval$segments, seed = 1),
    'Error in variables specification, "sampling" column was missing.'
  )
  
  # Model with missing sampling column
  not_df <- model
  not_df$variables <- dplyr::select(not_df$variables, -sampling)
  not_df_eval <- run_model(not_df)
  not_df$variables <- 'test'
  expect_error(
    sampled_segments <- openqaly:::resample(not_df, 10, missing_col_eval$segments, seed = 1),
    'Error in variables specification, specification was of class "character" rather than "data.frame".'
  )
  
})
# 
# test_that("errors in distribution evaluation are handled properly", {
#   
# })
