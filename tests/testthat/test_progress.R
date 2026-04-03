context("Progress Reporting")

# ============================================================================
# Model Fixtures
# ============================================================================

build_threshold_test_model <- function() {
  define_model("markov") %>%
    set_settings(
      timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3, discount_outcomes = 3
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_disease", 0.03) %>%
    add_variable("cost_base", 1000) %>%
    add_variable("cost_treatment", 5000) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("base") %>%
    add_strategy("treatment") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_value("cost_drug", cost_base, state = "healthy") %>%
    add_value("cost_drug", cost_base * 0.5, state = "sick") %>%
    add_value("cost_drug", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000) %>%
    add_summary("total_cost", "cost_drug", type = "cost")
}

# ============================================================================
# Test Helpers
# ============================================================================

# Create a mock progress recorder
make_test_progress <- function() {
  calls <- list()
  fn <- function(...) {
    calls[[length(calls) + 1]] <<- list(...)
  }
  list(fn = fn, get_calls = function() calls)
}

# Verify exact 100% progress completion
expect_exact_progress <- function(calls) {
  expect_true(length(calls) >= 2, info = "Expected at least 2 progress calls")
  total <- calls[[1]]$total
  expect_true(!is.null(total), info = "First call must declare total")
  amount_sum <- sum(vapply(
    calls[-1],
    function(c) c$amount,
    numeric(1)
  ))
  expect_equal(amount_sum, total, info = "Sum of amounts must equal total")
  # Verify no individual call exceeds remaining budget

  cumulative <- 0
  for (i in 2:length(calls)) {
    amt <- calls[[i]]$amount
    expect_true(amt > 0, info = paste("Amount at call", i, "must be > 0"))
    expect_true(
      amt <= total - cumulative + .Machine$double.eps,
      info = paste("Amount at call", i, "exceeds remaining budget")
    )
    cumulative <- cumulative + amt
  }
}

# ============================================================================
# run_model progress tests
# ============================================================================

test_that("run_model reports exact progress (markov, no groups)", {
  model <- build_simple_dsa_model()  # 2 strategies, 1 group
  rec <- make_test_progress()
  result <- run_model(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # 2 segments * 8 checkpoints + 4 non-segment = 20
  expect_equal(calls[[1]]$total, 20L)
})

test_that("run_model reports exact progress (markov, with groups)", {
  model <- build_simple_dsa_model() %>%
    add_group("young", weight = 0.6) %>%
    add_group("old", weight = 0.4)
  rec <- make_test_progress()
  result <- run_model(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # 2 strategies * 2 groups * 8 checkpoints + 4 non-segment = 36
  expect_equal(calls[[1]]$total, 36L)
})

test_that("run_model without progress works (backward compat)", {
  model <- build_simple_dsa_model()
  result <- run_model(model)
  expect_true(!is.null(result$segments))
  expect_true(!is.null(result$aggregated))
})

test_that("run_model results match with and without progress", {
  model <- build_simple_dsa_model()
  rec <- make_test_progress()
  result_with <- run_model(model, progress = rec$fn)
  result_without <- run_model(model)
  # Compare aggregated summaries
  expect_equal(
    result_with$aggregated$summaries,
    result_without$aggregated$summaries
  )
})

# ============================================================================
# run_psa progress tests
# ============================================================================

test_that("run_psa reports exact progress (markov)", {
  model <- build_simple_psa_model()  # 3 strategies, 1 group
  rec <- make_test_progress()
  set.seed(42)
  result <- run_psa(model, n_sim = 3, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # 3 sims * 3 strategies * 1 group * 8 checkpoints + 4 non-segment = 76
  expect_equal(calls[[1]]$total, 76L)
})

# ============================================================================
# run_dsa progress tests
# ============================================================================

test_that("run_dsa reports exact progress (markov, no VBP)", {
  model <- build_simple_dsa_model()  # 3 DSA params, 2 strategies, 1 group
  rec <- make_test_progress()
  result <- run_dsa(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # (1 base + 3*2 variations) * 2 strategies * 1 group * 8 checkpoints + 4 overhead = 116
  expect_equal(calls[[1]]$total, 116L)
})

# ============================================================================
# run_scenario progress tests
# ============================================================================

test_that("run_scenario reports exact progress (markov, no VBP)", {
  model <- build_simple_dsa_model() %>%
    add_scenario("Optimistic") %>%
    add_scenario_variable("Optimistic", "p_sick", 0.05) %>%
    add_scenario("Pessimistic") %>%
    add_scenario_variable("Pessimistic", "p_sick", 0.15)
  rec <- make_test_progress()
  result <- run_scenario(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # (1 base + 2 scenarios) * 2 strategies * 1 group * 8 checkpoints + 4 overhead = 52
  expect_equal(calls[[1]]$total, 52L)
})

# ============================================================================
# run_twsa progress tests
# ============================================================================

test_that("run_twsa reports exact progress (markov, no VBP)", {
  model <- build_simple_twsa_vbp_model()  # 2 strats, 1 group, 2x2 grid
  rec <- make_test_progress()
  result <- run_twsa(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # build_steps: 1 parse + 2 enrich + 1*2 value_gen + 1 grid_expand = 6
  # build_ticks: 6 * 8 = 48 (weighted same as segments)
  # steps=2 per dim + bc insertion => 3 values each => 3*3=9 grid + 1 base = 10 runs
  # 10 runs * 2 strategies * 1 group = 20 segments * 8 checkpoints = 160
  # total = 48 + 160 + 2 = 210
  expect_equal(calls[[1]]$total, 210L)
})

# ============================================================================
# run_vbp progress tests
# ============================================================================

test_that("run_vbp reports exact progress", {
  model <- build_simple_twsa_vbp_model()
  rec <- make_test_progress()
  result <- run_vbp(
    model,
    price_variable = "c_treatment",
    intervention_strategy = "volantor",
    outcome_summary = "qalys",
    cost_summary = "costs",
    progress = rec$fn
  )
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # 3 price levels * 2 strategies * 1 group * 8 checkpoints + 4 overhead = 52
  expect_equal(calls[[1]]$total, 52L)
})

# ============================================================================
# run_threshold progress tests
# ============================================================================

test_that("run_threshold reports exact progress (1 analysis)", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis(
      "QALY Target", "p_disease", 0, 0.95,
      threshold_condition_outcomes(
        type = "absolute",
        strategy = "base",
        summary = "total_qalys",
        target_value = 7.0
      )
    )
  rec <- make_test_progress()
  result <- run_threshold(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # 10 overhead + 1 * 100 per-analysis budget = 110
  expect_equal(calls[[1]]$total, 110L)
})

test_that("run_threshold reports exact progress (2 analyses)", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis(
      "QALY Target", "p_disease", 0, 0.95,
      threshold_condition_outcomes(
        type = "absolute",
        strategy = "base",
        summary = "total_qalys",
        target_value = 7.0
      )
    ) %>%
    add_threshold_analysis(
      "Cost Target", "cost_base", 0, 10000,
      threshold_condition_costs(
        type = "absolute",
        strategy = "base",
        summary = "total_cost",
        target_value = 5000
      )
    )
  rec <- make_test_progress()
  result <- run_threshold(model, progress = rec$fn)
  calls <- rec$get_calls()
  expect_exact_progress(calls)
  # 10 overhead + 2 * 100 per-analysis budget = 210
  expect_equal(calls[[1]]$total, 210L)
})

test_that("run_threshold without progress works (backward compat)", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis(
      "QALY Target", "p_disease", 0, 0.95,
      threshold_condition_outcomes(
        type = "absolute",
        strategy = "base",
        summary = "total_qalys",
        target_value = 7.0
      )
    )
  result <- run_threshold(model)
  expect_true(!is.null(result$threshold_values))
})
