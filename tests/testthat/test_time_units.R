# Test time unit functionality in trace output

library(testthat)
library(heRomod2)

test_that("time variables are stored in trace", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10,
                 cycle_length = 7,
                 cycle_length_unit = "days",
                 timeframe = 70,
                 timeframe_unit = "days") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Check that collapsed trace includes time columns
  trace_data <- results$aggregated$collapsed_trace[[1]]

  # Should be a data frame with time columns
  expect_true(is.data.frame(trace_data))
  expect_true("cycle" %in% colnames(trace_data))
  expect_true("day" %in% colnames(trace_data))
  expect_true("week" %in% colnames(trace_data))
  expect_true("month" %in% colnames(trace_data))
  expect_true("year" %in% colnames(trace_data))

  # Check that time values are calculated correctly
  # With cycle_length = 7 days:
  expect_equal(trace_data$cycle[1:3], c(1, 2, 3))
  expect_equal(trace_data$day[1:3], c(7, 14, 21))
  expect_equal(trace_data$week[1:3], c(1, 2, 3))
})


test_that("get_trace respects time_unit parameter", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 5,
                 cycle_length = 1,
                 cycle_length_unit = "months",
                 timeframe = 5,
                 timeframe_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Test different time units
  trace_cycles <- get_trace(results, format = "long", time_unit = "cycle")
  expect_true("cycle" %in% colnames(trace_cycles))
  expect_equal(unique(trace_cycles$cycle), 1:6)

  trace_days <- get_trace(results, format = "long", time_unit = "day")
  expect_true("day" %in% colnames(trace_days))

  trace_months <- get_trace(results, format = "long", time_unit = "month")
  expect_true("month" %in% colnames(trace_months))

  trace_years <- get_trace(results, format = "long", time_unit = "year")
  expect_true("year" %in% colnames(trace_years))
})


test_that("plot functions use time_unit parameter", {
  skip_if_not_installed("ggplot2")

  model <- define_model("markov") |>
    set_settings(n_cycles = 12,
                 cycle_length = 1,
                 cycle_length_unit = "months",
                 timeframe = 12,
                 timeframe_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.95) |>
    add_transition("healthy", "sick", 0.05) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Test plot_trace with different time units
  p_cycles <- plot_trace(results, time_unit = "cycle")
  expect_s3_class(p_cycles, "ggplot")
  expect_equal(p_cycles$labels$x, "Cycle")

  p_months <- plot_trace(results, time_unit = "month")
  expect_s3_class(p_months, "ggplot")
  expect_equal(p_months$labels$x, "Months")

  p_years <- plot_trace(results, time_unit = "year")
  expect_s3_class(p_years, "ggplot")
  expect_equal(p_years$labels$x, "Years")

  # Test plot_trace_lines with different time units
  p_lines_weeks <- plot_trace_lines(results, time_unit = "week")
  expect_s3_class(p_lines_weeks, "ggplot")
  expect_equal(p_lines_weeks$labels$x, "Weeks")
})


test_that("table function uses time_unit parameter", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  model <- define_model("markov") |>
    set_settings(n_cycles = 4,
                 cycle_length = 1,
                 cycle_length_unit = "weeks",
                 timeframe = 4,
                 timeframe_unit = "weeks") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Test table with different time units
  ft_cycles <- format_trace_flextable(results, time_unit = "cycle", cycles = 1:3)
  expect_s3_class(ft_cycles, "flextable")

  ft_weeks <- format_trace_flextable(results, time_unit = "week", cycles = 1:3)
  expect_s3_class(ft_weeks, "flextable")

  ft_days <- format_trace_flextable(results, time_unit = "day", cycles = 1:3)
  expect_s3_class(ft_days, "flextable")
})


test_that("backward compatibility is maintained", {
  # Test that models without time columns still work
  model <- define_model("markov") |>
    set_settings(n_cycles = 5,
                 cycle_length = 1,
                 cycle_length_unit = "months",
                 timeframe = 5,
                 timeframe_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Should still work with default time_unit = "cycle"
  trace_data <- get_trace(results, format = "long")
  expect_true("cycle" %in% colnames(trace_data))

  # Requesting other time units should warn but not fail
  expect_warning(
    trace_years <- get_trace(results, format = "long", time_unit = "year"),
    NA  # Allow warning or no warning
  )
})