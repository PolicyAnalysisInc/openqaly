# Validation of model settings and of settings used in scenarios and TWSA.

test_that("set_settings rejects an unknown half_cycle_method", {
  # Valid values are "start", "end", "life-table"; "life_table" must error.
  expect_error(set_settings(define_model("markov"), half_cycle_method = "life_table"),
               "half_cycle_method")
})

test_that("set_settings rejects an unknown discount_method", {
  expect_error(set_settings(define_model("markov"), discount_method = "bogus"),
               "discount_method")
})

test_that("half_cycle_weights rejects an unknown method", {
  expect_error(openqaly:::half_cycle_weights("bogus"), "half_cycle_method")
})

test_that("n_cycles sets timeframe_unit to cycles even when it was years", {
  m <- set_settings(define_model("markov"), timeframe_unit = "years", n_cycles = 20)
  expect_equal(m$settings$timeframe_unit, "cycles")
})

test_that("add_scenario_setting accepts discount_method", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_strategy("s") |>
    add_variable("p", 0.1) |>
    add_scenario("sc") |>
    add_scenario_setting("sc", "discount_method", "by_year")
  expect_no_error(openqaly:::validate_scenario_spec(m))
})

test_that("add_twsa_setting accepts discount_method", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_strategy("s") |>
    add_variable("p", 0.1) |>
    add_twsa("T") |>
    add_twsa_variable("T", "p", type = "range", min = 0.1, max = 0.3, steps = 3) |>
    add_twsa_setting("T", "discount_method", type = "custom", values = c("by_cycle", "by_year"))
  expect_no_error(openqaly:::validate_twsa_spec(m))
})

test_that("edit_twsa_setting accepts discount_method as the new setting name", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_strategy("s") |>
    add_variable("p", 0.1) |>
    add_twsa("T") |>
    add_twsa_variable("T", "p", type = "range", min = 0.1, max = 0.3, steps = 3) |>
    add_twsa_setting("T", "timeframe", type = "custom", values = c(10, 20))
  expect_no_error(
    edit_twsa_setting(m, "T", "timeframe", new_setting = "discount_method",
                      type = "custom", values = c("by_cycle", "by_year"))
  )
})
