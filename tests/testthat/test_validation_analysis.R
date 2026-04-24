# Helper to create a model with valid settings
make_model <- function(type = "markov") {
  m <- define_model(type)
  m$settings$timeframe <- 10
  m$settings$timeframe_unit <- "years"
  m$settings$cycle_length <- 1
  m$settings$cycle_length_unit <- "years"
  m
}

test_that("add_dsa_variable rejects empty variable name", {
  m <- make_model() |>
    add_variable("v1", 100)
  expect_error(add_dsa_variable(m, "", low = 1, high = 2), "Variable name must be a non-empty character string")
  expect_error(add_dsa_variable(m, "   ", low = 1, high = 2), "Variable name must be a non-empty character string")
})

test_that("add_dsa_variable rejects NA variable name", {
  m <- make_model() |>
    add_variable("v1", 100)
  expect_error(add_dsa_variable(m, NA_character_, low = 1, high = 2), "Variable name must be a non-empty character string")
})

test_that("markov model with valid transitions runs successfully", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_strategy("a") |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("sick", "sick", 0.8) |>
    add_transition("sick", "healthy", 0.2) |>
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("cost", 200, state = "sick", type = "cost") |>
    add_value("qaly", 1, state = "healthy", type = "outcome") |>
    add_value("qaly", 0.5, state = "sick", type = "outcome") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qaly", "qaly", type = "outcome")

  result <- run_model(m)
  expect_true(!is.null(result))
})
