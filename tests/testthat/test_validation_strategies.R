# Helper to create a model with valid settings
make_model <- function(type = "markov") {
  m <- define_model(type)
  m$settings$timeframe <- 10
  m$settings$timeframe_unit <- "years"
  m$settings$cycle_length <- 1
  m$settings$cycle_length_unit <- "years"
  m
}

test_that("add_strategy rejects duplicate name", {
  m <- make_model() |>
    add_strategy("treatment_a")
  expect_error(
    add_strategy(m, "treatment_a"),
    'Duplicate strategy name "treatment_a"'
  )
})

test_that("add_strategy accepts unique names", {
  m <- make_model() |>
    add_strategy("treatment_a") |>
    add_strategy("treatment_b")
  expect_equal(nrow(m$strategies), 2)
})

test_that("add_group rejects duplicate name", {
  m <- make_model() |>
    add_group("moderate")
  expect_error(
    add_group(m, "moderate"),
    'Duplicate group name "moderate"'
  )
})

test_that("add_group accepts unique names", {
  m <- make_model() |>
    add_group("moderate") |>
    add_group("severe")
  expect_equal(nrow(m$groups), 2)
})

test_that("run_model errors when no strategies for markov", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1)
  m$strategies <- NULL
  expect_error(
    run_model(m),
    "at least one strategy"
  )
})

test_that("run_model errors on variable referencing undefined strategy", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_strategy("a") |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("dead", "dead", 1) |>
    add_variable("v1", 100, strategy = "nonexistent") |>
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("qaly", 1, state = "healthy", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qaly", "qaly", type = "outcome")
  expect_error(
    run_model(m),
    'reference undefined strategy.*"nonexistent"'
  )
})

test_that("run_model errors on variable referencing undefined group", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_strategy("a") |>
    add_group("grp1") |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("dead", "dead", 1) |>
    add_variable("v1", 100, group = "nonexistent") |>
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("qaly", 1, state = "healthy", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qaly", "qaly", type = "outcome")
  expect_error(
    run_model(m),
    'reference undefined group.*"nonexistent"'
  )
})
