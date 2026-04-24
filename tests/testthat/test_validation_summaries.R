# Helper to create a model with valid settings
make_model <- function(type = "markov") {
  m <- define_model(type)
  m$settings$timeframe <- 10
  m$settings$timeframe_unit <- "years"
  m$settings$cycle_length <- 1
  m$settings$cycle_length_unit <- "years"
  m
}

make_model_with_values <- function() {
  make_model() |>
    add_value("c1", 100, type = "cost") |>
    add_value("c2", 200, type = "cost") |>
    add_value("q1", 0.9, type = "outcome")
}

test_that("add_summary rejects empty name", {
  m <- make_model_with_values()
  expect_error(add_summary(m, "", "c1", type = "cost"), "Summary name must be a non-empty character string")
  expect_error(add_summary(m, "   ", "c1", type = "cost"), "Summary name must be a non-empty character string")
})

test_that("add_summary rejects NA name", {
  m <- make_model_with_values()
  expect_error(add_summary(m, NA_character_, "c1"), "Summary name must be a non-empty character string")
})

test_that("add_summary on blank model gives no-values error", {
  m <- make_model()
  expect_error(add_summary(m, "total", ""), "no values are defined in this model")
  expect_error(add_summary(m, "total", "   "), "no values are defined in this model")
  expect_error(add_summary(m, "total", NA_character_), "no values are defined in this model")
  expect_error(add_summary(m, "total", "c1"), "no values are defined in this model")
})

test_that("add_summary rejects empty values when model has values", {
  m <- make_model_with_values()
  expect_error(add_summary(m, "total", ""), "comma-separated list of value names")
  expect_error(add_summary(m, "total", "   "), "comma-separated list of value names")
  expect_error(add_summary(m, "total", NA_character_), "comma-separated list of value names")
})

test_that("add_summary rejects references to undefined values", {
  m <- make_model_with_values()
  expect_error(add_summary(m, "total", "nonexistent"), "references value\\(s\\) not defined in the model")
  expect_error(add_summary(m, "total", "c1,typo"), "references value\\(s\\) not defined in the model")
})

test_that("add_summary rejects duplicate name", {
  m <- make_model_with_values() |>
    add_summary("total_cost", "c1", type = "cost")
  expect_error(
    add_summary(m, "total_cost", "c2", type = "cost"),
    "Summary 'total_cost' already exists"
  )
})

test_that("add_summary accepts valid inputs", {
  m <- make_model_with_values() |>
    add_summary("total_cost", "c1,c2", type = "cost") |>
    add_summary("total_qalys", "q1", type = "outcome")
  expect_equal(nrow(m$summaries), 2)
})

test_that("add_table rejects empty name", {
  m <- make_model()
  expect_error(add_table(m, "", data.frame(x = 1)), "Table name must be a non-empty character string")
  expect_error(add_table(m, "   ", data.frame(x = 1)), "Table name must be a non-empty character string")
})

test_that("add_table rejects NA name", {
  m <- make_model()
  expect_error(add_table(m, NA_character_, data.frame(x = 1)), "Table name must be a non-empty character string")
})

test_that("add_summary accepts summary referencing value with different type", {
  # Legacy behavior: summaries can reference values of different types
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_strategy("a") |>
    add_value("c1", 100, state = "healthy", type = "cost") |>
    add_summary("total_outcome", "c1", type = "outcome")
  expect_equal(nrow(m$summaries), 1)
})

test_that("normalize_and_validate_model accepts matching summary/value types", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_strategy("a") |>
    add_value("c1", 100, state = "healthy", type = "cost") |>
    add_summary("total_cost", "c1", type = "cost")
  expect_no_error(normalize_and_validate_model(m))
})
