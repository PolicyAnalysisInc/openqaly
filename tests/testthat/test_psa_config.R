context("PSA configuration tests")

# ==============================================================================
# Helper
# ==============================================================================

create_psa_test_model <- function() {
  define_model("markov") |>
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3,
      discount_outcomes = 3
    ) |>
    add_strategy("control") |>
    add_strategy("treatment") |>
    add_group("general", weight = "1") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_death", 0.1, sampling = normal(0.1, 0.02)) |>
    add_variable("cost", 100) |>
    add_variable("utility", 0.8) |>
    add_transition("alive", "dead", p_death) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("qaly", utility, state = "alive", type = "outcome") |>
    add_value("cost_val", cost, state = "alive", type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome") |>
    add_summary("total_costs", "cost_val", type = "cost")
}

# ==============================================================================
# set_psa() stores values correctly
# ==============================================================================

test_that("set_psa() stores n_sim and seed", {
  model <- define_model("markov") |>
    set_psa(n_sim = 1000, seed = 42)

  expect_equal(model$psa$n_sim, 1000L)
  expect_equal(model$psa$seed, 42)
})

test_that("set_psa() stores n_sim without seed", {
  model <- define_model("markov") |>
    set_psa(n_sim = 500)

  expect_equal(model$psa$n_sim, 500L)
  expect_null(model$psa$seed)
})

test_that("set_psa() coerces n_sim to integer", {
  model <- define_model("markov") |>
    set_psa(n_sim = 100.0)

  expect_true(is.integer(model$psa$n_sim))
  expect_equal(model$psa$n_sim, 100L)
})

# ==============================================================================
# set_psa() validation
# ==============================================================================

test_that("set_psa() rejects non-positive n_sim", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = 0),
    "n_sim must be a positive integer"
  )
  expect_error(
    define_model("markov") |> set_psa(n_sim = -5),
    "n_sim must be a positive integer"
  )
})

test_that("set_psa() rejects non-integer n_sim", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = 1.5),
    "n_sim must be a positive integer"
  )
})

test_that("set_psa() rejects non-numeric n_sim", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = "abc"),
    "n_sim must be a positive integer"
  )
})

test_that("set_psa() rejects NA n_sim", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = NA),
    "n_sim must be a positive integer"
  )
})

test_that("set_psa() rejects non-numeric seed", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = 100, seed = "abc"),
    "seed must be NULL or a single numeric value"
  )
})

test_that("set_psa() rejects vector seed", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = 100, seed = c(1, 2)),
    "seed must be NULL or a single numeric value"
  )
})

test_that("set_psa() rejects NA seed", {
  expect_error(
    define_model("markov") |> set_psa(n_sim = 100, seed = NA),
    "seed must be NULL or a single numeric value"
  )
})

# ==============================================================================
# run_psa() uses model defaults
# ==============================================================================

test_that("run_psa() uses model$psa defaults when args omitted", {
  model <- create_psa_test_model() |>
    set_psa(n_sim = 5, seed = 123)

  res <- run_psa(model)
  expect_true(!is.null(res$segments))
  expect_true(!is.null(res$aggregated))
})

test_that("run_psa() runtime args override model defaults", {
  model <- create_psa_test_model() |>
    set_psa(n_sim = 5, seed = 123)

  # Override n_sim
  res <- run_psa(model, n_sim = 3)
  expect_true(!is.null(res$segments))
})

test_that("run_psa() errors when n_sim missing everywhere", {
  model <- create_psa_test_model()
  # No set_psa(), no runtime n_sim

  expect_error(
    run_psa(model),
    "n_sim must be provided"
  )
})

test_that("run_psa() backward compat: explicit n_sim works without model$psa", {
  model <- create_psa_test_model()

  res <- run_psa(model, n_sim = 3, seed = 42)
  expect_true(!is.null(res$segments))
})
