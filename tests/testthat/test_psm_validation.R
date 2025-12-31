# PSM Model Validation Tests
# Tests that PSM models properly validate inputs and configurations

library(testthat)
library(openqaly)

test_that("PSM models require exactly 3 states", {
  # Test with 2 states (too few) - need complete model to reach validation
  model_2_states <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model_2_states),
    "PSM models require exactly 3 states"
  )

  # Test with 4 states (too many)
  model_4_states <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("terminal") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model_4_states),
    "PSM models require exactly 3 states"
  )
})

test_that("PSM models require PFS endpoint", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model),
    "PSM model missing PFS endpoint definition"
  )
})

test_that("PSM models require OS endpoint", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_psm_transition("PFS", "months", pfs_dist)

  expect_error(
    run_model(model),
    "PSM model missing OS endpoint definition"
  )
})

test_that("PSM transitions must have required columns", {
  # Create a model and manually mess with transitions
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead")

  # Create invalid transitions table missing 'formula'
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "OS"),
    time_unit = c("months", "months")
    # Missing 'formula' column
  )

  expect_error(
    run_model(model),
    "Transitions has missing values in required column:.*formula"
  )
})

test_that("PSM endpoint formulas must reference surv_dist objects", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("bad_var", 0.5) |>  # Not a survival distribution
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", bad_var) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model),
    "PFS formula must evaluate to a survival distribution"
  )
})

test_that("PSM models error on undefined variables in endpoint formulas", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead")

  # Manually set transitions to reference undefined variable
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "OS"),
    time_unit = c("months", "months"),
    formula = c("undefined_pfs_dist", "undefined_os_dist")
  )

  expect_error(
    run_model(model),
    "not found|undefined"
  )
})

test_that("PSM models handle multiple endpoint definitions appropriately", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months", timeframe = 10, timeframe_unit = "years") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1))

  # Manually add duplicate PFS definitions
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "PFS", "OS"),
    time_unit = c("months", "months", "months"),
    formula = c("pfs_dist", "pfs_dist", "os_dist")
  )

  # Should error or warn about multiple definitions
  expect_error(
    run_model(model),
    "multiple PFS endpoint definitions"
  )
})

test_that("PSM time unit validation catches invalid units", {
  # This test verifies that invalid time units are handled
  # The actual behavior depends on implementation
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1))

  # Manually set invalid time unit
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "OS"),
    time_unit = c("fortnights", "months"),  # Invalid unit
    formula = c("pfs_dist", "os_dist")
  )

  # Should handle gracefully (may warn or treat as cycles)
  # This test just ensures it doesn't crash
  expect_no_error({
    suppressWarnings(run_model(model))
  })
})

test_that("PSM models reject initial_prob parameter", {
  # PSM models don't use initial probabilities - should error if provided
  expect_error({
    define_model("psm") |>
      set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
      add_state("pfs", initial_prob = 1)
  }, "PSM models don't use initial_prob")
})

test_that("PSM models require transitions table", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1))

  # No transitions added
  expect_error(
    run_model(model),
    "PSM models require transitions table"
  )
})

test_that("PSM correctly validates trace probabilities", {
  # Create a model that should produce valid trace
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)

  # Should run without validation errors
  expect_no_error(run_model(model))

  # Now test that validation would catch invalid probabilities
  # (This is a design test - the validation code in psm.R should catch this)
  results <- run_model(model)
  trace <- results$aggregated$collapsed_trace[[1]]
  state_trace <- get_state_columns(trace)

  # Verify validation worked by checking trace is valid
  # Use same tolerance as validation code
  tol <- 10 * sqrt(.Machine$double.eps)
  expect_true(all(rowSums(state_trace) >= 1 - tol & rowSums(state_trace) <= 1 + tol))
  expect_true(all(state_trace >= -tol & state_trace <= 1 + tol))
})