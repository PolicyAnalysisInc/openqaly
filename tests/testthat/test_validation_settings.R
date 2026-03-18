test_that("validate_settings errors on missing timeframe", {
  settings <- list(model_type = "markov", timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "must include 'timeframe'")
})

test_that("validate_settings errors on missing timeframe_unit", {
  settings <- list(model_type = "markov", timeframe = 10,
                   cycle_length = 1, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "must include 'timeframe_unit'")
})

test_that("validate_settings errors on missing cycle_length", {
  settings <- list(model_type = "markov", timeframe = 10,
                   timeframe_unit = "years", cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "must include 'cycle_length'")
})

test_that("validate_settings errors on missing cycle_length_unit", {
  settings <- list(model_type = "markov", timeframe = 10,
                   timeframe_unit = "years", cycle_length = 1)
  expect_error(validate_settings(settings, "markov"), "must include 'cycle_length_unit'")
})

test_that("validate_settings errors on zero timeframe", {
  settings <- list(model_type = "markov", timeframe = 0, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "must be positive")
})

test_that("validate_settings errors on negative timeframe", {
  settings <- list(model_type = "markov", timeframe = -5, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "must be positive")
})

test_that("validate_settings errors on zero cycle_length", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 0, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "must be positive")
})

test_that("validate_settings errors on invalid timeframe_unit", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "lightyears",
                   cycle_length = 1, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "Invalid timeframe_unit.*Valid options")
})

test_that("validate_settings accepts cycles as timeframe_unit", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "cycles",
                   cycle_length = 1, cycle_length_unit = "years")
  expect_no_error(validate_settings(settings, "markov"))
})

test_that("validate_settings errors on invalid cycle_length_unit", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "lightyears")
  expect_error(validate_settings(settings, "markov"), "Invalid cycle_length_unit.*Valid options")
})

test_that("validate_settings accepts cycles as cycle_length_unit", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "cycles")
  expect_no_error(validate_settings(settings, "markov"))
})

test_that("validate_settings errors on non-numeric timeframe", {
  settings <- list(model_type = "markov", timeframe = "abc", timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "'timeframe' must be numeric")
})

test_that("validate_settings errors on non-numeric cycle_length", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = "abc", cycle_length_unit = "years")
  expect_error(validate_settings(settings, "markov"), "'cycle_length' must be numeric")
})

test_that("validate_settings errors on negative discount_cost", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years", discount_cost = -1)
  expect_error(validate_settings(settings, "markov"), "'discount_cost' must be non-negative")
})

test_that("validate_settings errors on discount_cost over 100", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years", discount_cost = 150)
  expect_error(validate_settings(settings, "markov"), "exceeds 100.*percentages")
})

test_that("validate_settings errors on non-numeric discount_outcomes", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years", discount_outcomes = "abc")
  expect_error(validate_settings(settings, "markov"), "'discount_outcomes' must be numeric")
})

test_that("validate_settings accepts valid settings", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years",
                   discount_cost = 3, discount_outcomes = 3)
  expect_no_error(validate_settings(settings, "markov"))
})

test_that("validate_settings skips timeframe checks for decision_tree", {
  settings <- list(model_type = "decision_tree")
  expect_no_error(validate_settings(settings, "decision_tree"))
})

test_that("validate_settings validates discount rates for decision_tree", {
  settings <- list(model_type = "decision_tree", discount_cost = -1)
  expect_error(validate_settings(settings, "decision_tree"), "'discount_cost' must be non-negative")
})

test_that("validate_settings errors on discount_outcomes over 100", {
  settings <- list(model_type = "markov", timeframe = 10, timeframe_unit = "years",
                   cycle_length = 1, cycle_length_unit = "years", discount_outcomes = 101)
  expect_error(validate_settings(settings, "markov"), "exceeds 100.*percentages")
})
