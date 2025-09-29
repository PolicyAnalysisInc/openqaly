# Test Model Builder Functions

library(testthat)
library(heRomod2)

test_that("Model builder creates valid model structure", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 100, cycle_length = "year") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0)

  expect_s3_class(model, "heRomodel")
  expect_equal(model$settings$model_type, "markov")
  expect_equal(nrow(model$states), 3)
  expect_equal(model$states$name, c("healthy", "sick", "dead"))
})

test_that("NSE works for transitions and values", {
  model <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "sick", p_disease * 0.1) |>
    add_value("cost", base_cost + extra_cost, state = "sick")

  # Check that formulas are captured as strings
  expect_equal(model$transitions$formula[1], "p_disease * 0.1")
  expect_equal(model$values$formula[1], "base_cost + extra_cost")
})

test_that("Variables preserve all fields including pass-through", {
  model <- define_model("markov") |>
    add_variable("p_disease", 0.1,
                source = "Smith et al. (2020)",
                sampling = "beta(10, 90)")

  expect_equal(model$variables$source[1], "Smith et al. (2020)")
  expect_equal(model$variables$sampling[1], "beta(10, 90)")
})

test_that("Strategies and groups work correctly", {
  model <- define_model("markov") |>
    add_strategy("treatment_a", abbreviation = "TX-A") |>
    add_group("moderate", weight = "w_moderate")

  expect_equal(model$strategies$abbreviation[1], "TX-A")
  expect_equal(model$groups$weight[1], "w_moderate")
})

test_that("Round-trip conversion preserves model", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("jsonlite")

  # Create a model with various components
  original <- define_model("markov") |>
    set_settings(n_cycles = 100, cycle_length = "4", cycle_length_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.1) |>
    add_value("cost", 1000, state = "sick") |>
    add_variable("p_disease", 0.1, source = "Literature") |>
    add_strategy("treatment_a", abbreviation = "TX-A") |>
    add_group("all", weight = "1")

  # Test JSON round-trip
  json_str <- as_json(original)
  from_json <- read_model_json(json_str)

  expect_equal(original$settings$n_cycles, from_json$settings$n_cycles)
  expect_equal(nrow(original$states), nrow(from_json$states))
  expect_equal(original$variables$source, from_json$variables$source)

  # Test R code generation
  r_code <- as_r_code(original)
  expect_true(length(r_code) > 0)
  expect_true(any(grepl("define_model", r_code)))
  expect_true(any(grepl("add_state", r_code)))
})

test_that("Convert model function works", {
  skip_if_not_installed("openxlsx")
  skip_if_not_installed("jsonlite")

  # Create a simple model
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0)

  # Test writing to different formats
  temp_dir <- tempdir()
  json_file <- file.path(temp_dir, "test_model.json")
  r_file <- file.path(temp_dir, "test_model.R")

  # Write as JSON
  write_model(model, json_file, format = "json")
  expect_true(file.exists(json_file))

  # Write as R code
  write_model(model, r_file, format = "r")
  expect_true(file.exists(r_file))

  # Clean up
  unlink(json_file)
  unlink(r_file)
})

test_that("PSM transitions work correctly", {
  model <- define_model("psm") |>
    add_psm_transition("death", "years", exp(-0.1 * time))

  expect_equal(model$transitions$endpoint[1], "death")
  expect_equal(model$transitions$time_unit[1], "years")
  expect_equal(model$transitions$formula[1], "exp(-0.1 * time)")
})

test_that("Path validation works correctly", {
  model <- define_model("markov")

  # Excel format should reject file paths
  expect_error(
    write_model(model, "test.xlsx", format = "excel"),
    "path must be a folder"
  )

  # JSON/R formats should reject folder paths
  expect_error(
    write_model(model, "test_folder/", format = "json"),
    "must include a filename"
  )

  expect_error(
    write_model(model, "test_folder/", format = "r"),
    "must include a filename"
  )
})