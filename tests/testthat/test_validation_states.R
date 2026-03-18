# Helper to create a model with valid settings
make_model <- function(type = "markov") {
  m <- define_model(type)
  m$settings$timeframe <- 10
  m$settings$timeframe_unit <- "years"
  m$settings$cycle_length <- 1
  m$settings$cycle_length_unit <- "years"
  m
}

test_that("add_state rejects empty name", {
  m <- make_model()
  expect_error(add_state(m, ""), "State name must be a non-empty character string")
  expect_error(add_state(m, "   "), "State name must be a non-empty character string")
})

test_that("add_state rejects NA name", {
  m <- make_model()
  expect_error(add_state(m, NA_character_), "State name must be a non-empty character string")
})

test_that("add_state rejects non-character name", {
  m <- make_model()
  expect_error(add_state(m, 123), "State name must be a non-empty character string")
})

test_that("add_state rejects duplicate state names", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1)
  expect_error(add_state(m, "healthy", initial_prob = 0), "A state named 'healthy' already exists")
})

test_that("add_state accepts unique state names", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0)
  expect_equal(nrow(m$states), 2)
})

test_that("add_transition rejects undefined from_state", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0)
  expect_error(
    add_transition(m, "unknown", "sick", 0.1),
    "Transition references undefined from_state 'unknown'.*Available states: healthy, sick"
  )
})

test_that("add_transition rejects undefined to_state", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0)
  expect_error(
    add_transition(m, "healthy", "unknown", 0.1),
    "Transition references undefined to_state 'unknown'.*Available states: healthy, sick"
  )
})

test_that("add_transition accepts valid states", {
  m <- make_model() |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.1)
  expect_equal(nrow(m$transitions), 1)
})

test_that("run_model errors when no states for markov", {
  m <- make_model() |>
    add_strategy("a")
  m$states <- NULL
  expect_error(
    run_model(m),
    "requires at least one state"
  )
})

test_that("run_model errors when no states for psm", {
  m <- make_model("psm") |>
    add_strategy("a")
  m$states <- NULL
  expect_error(
    run_model(m),
    "requires at least one state"
  )
})
