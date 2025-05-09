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
    c(29.5766678571282, 56.8063869513948, 34.6586174642352, 35.2444921016213,
      29.5766678571282, 56.8063869513948, 34.6586174642352, 35.2444921016213,
      29.5766678571282, 56.8063869513948, 34.6586174642352, 35.2444921016213),
    sampled_segments$start_age,
    tolerance = 1e-4
  )
  expect_equal(
    c(0.00141296738191902, 0.00141296738191902, 0.00141296738191902, 0.00141296738191902,
      0.000699213199807166, 0.000699213199807166, 0.000699213199807166, 0.000699213199807166,
      0.00081557984261587, 0.00081557984261587, 0.00081557984261587, 0.00081557984261587),
    sampled_segments$p_death_ae,
    tolerance = 1e-4
  )
  expect_equal(
    c(909.07644371121, 909.07644371121, 909.07644371121, 909.07644371121,
      909.07644371121, 909.07644371121, 909.07644371121, 909.07644371121,
      909.07644371121, 909.07644371121, 909.07644371121, 909.07644371121),
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
  expect_warning(
    sampled_segments <- heRomod2:::resample(syntax_error, 10, syntax_error_eval$segments, seed = 1),
    regexp = "Error in evaluation of sampling distribution for parameter \"start_age\": Error in formula syntax",
    fixed = TRUE
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
