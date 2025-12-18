context("Sampling")
suppressMessages(library(dplyr))

test_that("parameter sampling yields correct averages", {
  
  # Run a test model
  model <- system.file("models", "checkimab", package = "heRomod2") %>%
    read_model()
  eval_model <- run_model(model)
  
  # Generate sampled parameter values
  sampled_segments <- heRomod2:::resample(model, 1000, eval_model$segments, seed = 10)
  
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
  means <- model$tables$eq5d %>%
    group_by(treatment, state) %>%
    summarize(value = mean(value))
  chemo_m_lt35_smean <- bind_rows(chemo_m_lt35$eq5d_data) %>%
    group_by(treatment, state) %>%
    summarize(value = mean(value))
  
  # Check results
  expect_equal(means$value, chemo_m_lt35_smean$value, tolerance = 1e-2)
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
  
  # Run a test model
  model <- system.file("models", "checkimab", package = "heRomod2") %>%
    read_model()
  eval_model <- run_model(model)
  
  # Generate sampled parameter values
  sampled_segments <- heRomod2:::resample(model, 1, eval_model$segments, seed = 1)
  
  # Check results
  expect_equal(
    c(21.9280502536794, 34.5536206979549, 29.6314418669539, 37.2969463216801,
      26.4846723604542, 39.6208119860516, 34.7754043029916, 54.7289530759057,
      30.9954639302786, 32.2946652274845, 26.0883813821281, 50.907161796309),
    sampled_segments$start_age,
    tolerance = 1e-4
  )
  expect_equal(
    c(0.00091844165869545, 0.00112185726404511, 0.000713085751763911,
      0.00123595904802673, 0.000600523688642854, 0.000505840771071901,
      0.0004481256783263, 0.000468329790177951, 0.000724807067977948,
      0.000591768377139757, 0.000653809334326724, 0.000628318845687671),
    sampled_segments$p_death_ae,
    tolerance = 1e-4
  )
  expect_equal(
    c(1045.91083104622, 926.319886377034, 844.68985285505, 1108.9208250931,
      502.662092634832, 1104.48538941009, 1248.04009324853, 886.80400617843,
      971.913448706372, 1358.25592861894, 1600.40440918246, 643.725401261573),
    sampled_segments$cost_nausea,
    tolerance = 1e-4
  )
})

test_that("errors in distribution parsing are handled properly", {
  
  # Run a test model
  model <- system.file("models", "checkimab", package = "heRomod2") %>%
    read_model()
  
  # Model with syntax error
  syntax_error <- model
  syntax_error$variables$sampling[1] <- 'normal(26, 2))'
  syntax_error_eval <- run_model(syntax_error)
  expect_error(
    sampled_segments <- heRomod2:::resample(syntax_error, 10, syntax_error_eval$segments, seed = 1),
    regexp = "Failed to evaluate sampling distribution.*start_age.*Error in formula syntax"
  )
  
  # Model with no sampling distributions
  no_dist <- model
  no_dist$variables$sampling <- ''
  no_dist_eval <- run_model(no_dist)
  expect_error(
    sampled_segments <- heRomod2:::resample(no_dist, 10, no_dist_eval$segments, seed = 1),
    'Error in variables specification, no sampling distributions were specified.'
  )
  
  # Model with missing sampling column
  missing_col <- model
  missing_col$variables <- dplyr::select(missing_col$variables, -sampling)
  missing_col_eval <- run_model(missing_col)
  expect_error(
    sampled_segments <- heRomod2:::resample(missing_col, 10, missing_col_eval$segments, seed = 1),
    'Error in variables specification, "sampling" column was missing.'
  )
  
  # Model with missing sampling column
  not_df <- model
  not_df$variables <- dplyr::select(not_df$variables, -sampling)
  not_df_eval <- run_model(not_df)
  not_df$variables <- 'test'
  expect_error(
    sampled_segments <- heRomod2:::resample(not_df, 10, missing_col_eval$segments, seed = 1),
    'Error in variables specification, specification was of class "character" rather than "data.frame".'
  )
  
})
# 
# test_that("errors in distribution evaluation are handled properly", {
#   
# })
