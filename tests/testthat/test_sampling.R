context("Sampling")
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(purrr))

test_that("parameter sampling yields correct averages", {

  # Run a test model
  model <- system.file("models", "example_markov", package = "openqaly") %>%
    read_model()
  eval_model <- run_model(model)

  # Generate sampled parameter values
  sampled_segments_raw <- openqaly:::resample(model, 1000, eval_model$segments, seed = 10)

  # Helper to safely extract parameter (returns NA if not present)
  safe_extract <- function(x, name) {
    val <- x[[name]]
    if (is.null(val)) NA_real_ else val
  }

  # Extract scalar parameter values
  sampled_segments <- sampled_segments_raw %>%
    mutate(
      p_death_healthy = map_dbl(parameter_overrides, ~safe_extract(.x, "p_death_healthy")),
      p_death_mild = map_dbl(parameter_overrides, ~safe_extract(.x, "p_death_mild")),
      u_mild = map_dbl(parameter_overrides, ~safe_extract(.x, "u_mild")),
      c_mild = map_dbl(parameter_overrides, ~safe_extract(.x, "c_mild")),
      u_severe = map_dbl(parameter_overrides, ~safe_extract(.x, "u_severe")),
      c_severe = map_dbl(parameter_overrides, ~safe_extract(.x, "c_severe")),
      c_treatment = map_dbl(parameter_overrides, ~safe_extract(.x, "c_treatment")),
      hr_progression = map_dbl(parameter_overrides, ~safe_extract(.x, "hr_progression")),
      p_mild = map_dbl(parameter_overrides, ~safe_extract(.x, "p_mild")),
      p_severe = map_dbl(parameter_overrides, ~safe_extract(.x, "p_severe")),
      p_death_severe = map_dbl(parameter_overrides, ~safe_extract(.x, "p_death_severe"))
    ) %>%
    select(-parameter_overrides)

  # Separate the sampled values by strategy/group
  seritinib_low <- dplyr::filter(sampled_segments, strategy == 'seritinib', group == 'low_risk')
  seritinib_high <- dplyr::filter(sampled_segments, strategy == 'seritinib', group == 'high_risk')
  volantor_low <- dplyr::filter(sampled_segments, strategy == 'volantor', group == 'low_risk')
  volantor_high <- dplyr::filter(sampled_segments, strategy == 'volantor', group == 'high_risk')

  # Check univariate distributions - means should match base case values
  # p_death_healthy: beta(mean = 0.01, sd = 0.002) - same across all segments
  expect_equal(0.01, mean(seritinib_low$p_death_healthy), tolerance = 1e-3)
  expect_equal(0.01, mean(volantor_high$p_death_healthy), tolerance = 1e-3)

  # p_death_mild: beta(mean = 0.03, sd = 0.005) - same across all segments
  expect_equal(0.03, mean(seritinib_low$p_death_mild), tolerance = 1e-3)
  expect_equal(0.03, mean(volantor_high$p_death_mild), tolerance = 1e-3)

  # u_severe: beta(mean = 0.5, sd = 0.1) - same across all segments
  expect_equal(0.5, mean(seritinib_low$u_severe), tolerance = 0.02)
  expect_equal(0.5, mean(volantor_high$u_severe), tolerance = 0.02)

  # c_severe: gamma(mean = 8000, sd = 1600) - same across all segments
  expect_equal(8000, mean(seritinib_low$c_severe), tolerance = 200)
  expect_equal(8000, mean(volantor_high$c_severe), tolerance = 200)

  # c_treatment by strategy: gamma distributions
  expect_equal(500, mean(seritinib_low$c_treatment), tolerance = 20)
  expect_equal(500, mean(seritinib_high$c_treatment), tolerance = 20)
  expect_equal(3000, mean(volantor_low$c_treatment), tolerance = 100)
  expect_equal(3000, mean(volantor_high$c_treatment), tolerance = 100)

  # hr_progression: lognormal(mean = 0.6, sd = 0.1) for volantor only
  # seritinib strategy doesn't have sampling for hr_progression, so it returns NA
  expect_true(all(is.na(seritinib_low$hr_progression)))
  expect_true(all(is.na(seritinib_high$hr_progression)))
  expect_equal(0.6, mean(volantor_low$hr_progression), tolerance = 0.02)
  expect_equal(0.6, mean(volantor_high$hr_progression), tolerance = 0.02)

  # Group-specific parameters
  # p_mild: 0.05 for low_risk, 0.12 for high_risk
  expect_equal(0.05, mean(seritinib_low$p_mild), tolerance = 5e-3)
  expect_equal(0.12, mean(seritinib_high$p_mild), tolerance = 1e-2)
  expect_equal(0.05, mean(volantor_low$p_mild), tolerance = 5e-3)
  expect_equal(0.12, mean(volantor_high$p_mild), tolerance = 1e-2)

  # p_severe: 0.10 for low_risk, 0.20 for high_risk
  expect_equal(0.10, mean(seritinib_low$p_severe), tolerance = 1e-2)
  expect_equal(0.20, mean(seritinib_high$p_severe), tolerance = 2e-2)
  expect_equal(0.10, mean(volantor_low$p_severe), tolerance = 1e-2)
  expect_equal(0.20, mean(volantor_high$p_severe), tolerance = 2e-2)

  # p_death_severe: 0.08 for low_risk, 0.18 for high_risk
  expect_equal(0.08, mean(seritinib_low$p_death_severe), tolerance = 1e-2)
  expect_equal(0.18, mean(seritinib_high$p_death_severe), tolerance = 2e-2)
  expect_equal(0.08, mean(volantor_low$p_death_severe), tolerance = 1e-2)
  expect_equal(0.18, mean(volantor_high$p_death_severe), tolerance = 2e-2)

  # Check multivariate distribution - u_mild and c_mild should have expected means
  # mvnormal(mean = c(0.8, 2000), sd = c(0.05, 400), cor = -0.3)
  expect_equal(0.8, mean(seritinib_low$u_mild), tolerance = 0.02)
  expect_equal(2000, mean(seritinib_low$c_mild), tolerance = 50)

  # Check negative correlation between u_mild and c_mild
  cor_u_c <- cor(seritinib_low$u_mild, seritinib_low$c_mild)
  expect_lt(cor_u_c, 0)  # Should be negative
  expect_equal(-0.3, cor_u_c, tolerance = 0.1)  # Should be approximately -0.3
})

test_that("parameter sampling with random seed is deterministic", {
  # Setup: Run a test model
  model <- system.file("models", "example_markov", package = "openqaly") %>%
    read_model()
  eval_model <- run_model(model)

  # First run with seed = 1
  sampled_segments_1 <- openqaly:::resample(model, 1, eval_model$segments, seed = 1)

  # Second run with same seed = 1
  sampled_segments_2 <- openqaly:::resample(model, 1, eval_model$segments, seed = 1)

  # Helper to safely extract parameter (returns NA if not present)
  safe_extract <- function(x, name) {
    val <- x[[name]]
    if (is.null(val)) NA_real_ else val
  }

  # Helper to extract scalar parameters
  extract_params <- function(sampled_raw) {
    sampled_raw %>%
      mutate(
        p_death_healthy = map_dbl(parameter_overrides, ~safe_extract(.x, "p_death_healthy")),
        u_mild = map_dbl(parameter_overrides, ~safe_extract(.x, "u_mild")),
        c_treatment = map_dbl(parameter_overrides, ~safe_extract(.x, "c_treatment"))
      ) %>%
      select(-parameter_overrides)
  }

  params_1 <- extract_params(sampled_segments_1)
  params_2 <- extract_params(sampled_segments_2)

  # Verify determinism: same seed produces identical results
  expect_equal(params_1$p_death_healthy, params_2$p_death_healthy)
  expect_equal(params_1$u_mild, params_2$u_mild)
  expect_equal(params_1$c_treatment, params_2$c_treatment)

  # Sanity check: verify expected number of segments (3 strategies x 2 groups = 6)
  expect_equal(nrow(params_1), 6)
})

test_that("errors in distribution parsing are handled properly", {

  # Run a test model
  model <- system.file("models", "example_markov", package = "openqaly") %>%
    read_model()

  # Model with syntax error
  syntax_error <- model
  syntax_error$variables$sampling[1] <- 'beta(mean = 0.01, sd = 0.002))'
  syntax_error_eval <- run_model(syntax_error)
  expect_error(
    sampled_segments <- openqaly:::resample(syntax_error, 10, syntax_error_eval$segments, seed = 1),
    regexp = "Failed to evaluate sampling distribution.*p_death_healthy.*Error in formula syntax"
  )

  # Model with no sampling distributions (remove both univariate and multivariate)
  no_dist <- model
  no_dist$variables$sampling <- ''
  no_dist$multivariate_sampling <- NULL
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

  # Model with variables not being a data.frame
  not_df <- model
  not_df$variables <- dplyr::select(not_df$variables, -sampling)
  not_df_eval <- run_model(not_df)
  not_df$variables <- 'test'
  expect_error(
    sampled_segments <- openqaly:::resample(not_df, 10, missing_col_eval$segments, seed = 1),
    'Error in variables specification, specification was of class "character" rather than "data.frame".'
  )

})
