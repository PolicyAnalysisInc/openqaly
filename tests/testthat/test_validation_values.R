test_that("add_value rejects empty name", {
  m <- define_model("markov")
  expect_error(add_value(m, "", 100), "Value name must be a non-empty string")
  expect_error(add_value(m, "   ", 100), "Value name must be a non-empty string")
})

test_that("add_value rejects NA name", {
  m <- define_model("markov")
  expect_error(add_value(m, NA_character_, 100), "Value name must be a non-empty string")
})

test_that("add_value rejects invalid type", {
  m <- define_model("markov")
  expect_error(add_value(m, "cost1", 100, type = "invalid"), "Value type must be 'outcome' or 'cost'")
})

test_that("add_value accepts valid types", {
  m <- define_model("markov")
  m1 <- add_value(m, "c1", 100, type = "cost")
  expect_equal(nrow(m1$values), 1)
  m2 <- add_value(m, "o1", 1, type = "outcome")
  expect_equal(nrow(m2$values), 1)
})

test_that("add_value rejects empty formula", {
  m <- define_model("markov")
  expect_error(add_value(m, "cost1", "", type = "cost"), "Value formula must be a non-empty expression")
  expect_error(add_value(m, "cost1", "   ", type = "cost"), "Value formula must be a non-empty expression")
})

test_that("add_value rejects duplicate name+state+destination", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_value("cost1", 100, state = "healthy", type = "cost")
  expect_error(
    add_value(m, "cost1", 200, state = "healthy", type = "cost"),
    "Duplicate value: name 'cost1', state 'healthy'"
  )
})

test_that("add_value allows same name different state", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_value("cost1", 100, state = "healthy", type = "cost") |>
    add_value("cost1", 200, state = "sick", type = "cost")
  expect_equal(nrow(m$values), 2)
})

test_that("add_variable rejects empty name", {
  m <- define_model("markov")
  expect_error(add_variable(m, "", 100), "Variable name must be a non-empty string")
  expect_error(add_variable(m, "   ", 100), "Variable name must be a non-empty string")
})

test_that("add_variable rejects NA name", {
  m <- define_model("markov")
  expect_error(add_variable(m, NA_character_, 100), "Variable name must be a non-empty string")
})

test_that("add_variable rejects empty formula", {
  m <- define_model("markov")
  expect_error(add_variable(m, "v1", ""), "Variable formula must be a non-empty expression")
  expect_error(add_variable(m, "v1", "   "), "Variable formula must be a non-empty expression")
})

test_that("add_variable accepts valid inputs", {
  m <- define_model("markov") |>
    add_variable("v1", 100)
  expect_equal(nrow(m$variables), 1)
})
