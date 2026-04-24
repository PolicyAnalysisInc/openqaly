test_that("edit_strategy rejects rename to duplicate name", {
  m <- define_model("markov") |>
    add_strategy("A") |>
    add_strategy("B")
  expect_error(edit_strategy(m, "B", new_name = "A"), "already exists")
})

test_that("edit_strategy allows rename to same name", {
  m <- define_model("markov") |>
    add_strategy("A") |>
    add_strategy("B")
  m2 <- edit_strategy(m, "A", new_name = "A")
  expect_equal(m2$strategies$name, c("A", "B"))
})

test_that("edit_variable rejects rename to duplicate key", {
  m <- define_model("markov") |>
    add_variable("x", 1) |>
    add_variable("y", 2)
  expect_error(edit_variable(m, "y", new_name = "x"), "already exists")
})

test_that("edit_variable rename respects composite key (strategy)", {
  m <- define_model("markov") |>
    add_strategy("S1") |>
    add_variable("x", 1) |>
    add_variable("x", 10, strategy = "S1")
  # Renaming the strategy-specific "x" to "y" should succeed (no conflict)
  m2 <- edit_variable(m, "x", strategy = "S1", new_name = "y")
  expect_equal(sum(m2$variables$name == "x"), 1)
  expect_equal(sum(m2$variables$name == "y"), 1)
})

test_that("edit_state rejects rename to duplicate name", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0)
  expect_error(edit_state(m, "sick", new_name = "healthy"), "already exists")
})

test_that("edit_summary rejects rename to duplicate name", {
  m <- define_model("markov") |>
    add_value("v1", 1, type = "cost") |>
    add_value("v2", 2, type = "cost") |>
    add_summary("s1", values = "v1", type = "cost") |>
    add_summary("s2", values = "v2", type = "cost")
  expect_error(edit_summary(m, "s2", new_name = "s1"), "already exists")
})

test_that("editing non-key fields does not trigger false duplicate error", {
  m <- define_model("markov") |>
    add_strategy("A") |>
    add_strategy("B")
  m2 <- edit_strategy(m, "A", display_name = "Updated A")
  expect_equal(m2$strategies$display_name[m2$strategies$name == "A"], "Updated A")
})

test_that("edit_variable non-key field update does not trigger false duplicate", {
  m <- define_model("markov") |>
    add_variable("x", 1) |>
    add_variable("y", 2)
  m2 <- edit_variable(m, "x", formula = 999)
  expect_equal(m2$variables$formula[m2$variables$name == "x"], "999")
})
