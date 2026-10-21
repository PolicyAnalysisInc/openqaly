# =============================================================================
# Discounting Override - Full Test Matrix
# =============================================================================
# Tests discounting_override across all value types and model types.
# Value types: residency, transition, model start, decision tree (DT)
# Model types: Markov, PSM, Custom PSM, standalone Decision Tree

# =============================================================================
# Test 1: Markov — residency, transition, model start
# =============================================================================

test_that("Markov: discounting_override works for residency, transition, model start", {
  model <- define_model("markov") |>
    set_settings(
      timeframe = 5,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5
    ) |>
    add_strategy("s") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.2) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.3) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    # Residency cost WITH override (= "1" => no discounting)
    add_value("res_cost", 1000, state = "healthy", type = "cost",
              discounting_override = "1") |>
    # Residency outcome WITHOUT override (regular discounting)
    add_value("res_qaly", 1, state = "healthy", type = "outcome") |>
    # Transition cost WITH override (= "0.95")
    add_value("trans_cost", 500, state = "healthy", destination = "sick",
              type = "cost", discounting_override = "0.95") |>
    # Transition outcome WITHOUT override (regular discounting)
    add_value("trans_qaly", 0.5, state = "healthy", destination = "sick",
              type = "outcome") |>
    # Model start cost WITH override (= "0.9")
    add_value("start_cost", 2000, type = "cost",
              discounting_override = "0.9") |>
    # Model start outcome WITHOUT override
    add_value("start_qaly", 0.1, type = "outcome") |>
    add_summary("total_cost", type = "cost",
                values = "res_cost,trans_cost,start_cost") |>
    add_summary("total_qaly", type = "outcome",
                values = "res_qaly,trans_qaly,start_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # --- Residency cost: override = "1" => discounted == undiscounted ---
  expect_equal(disc[, "res_cost"], vals[, "res_cost"])

  # --- Residency outcome: no override => regular discounting applied ---
  expect_false(isTRUE(all.equal(disc[, "res_qaly"], vals[, "res_qaly"])))

  # --- Transition cost: override = "0.95" => discounted == undiscounted * 0.95 ---
  expect_equal(disc[, "trans_cost"], vals[, "trans_cost"] * 0.95)

  # --- Transition outcome: no override => regular discounting applied ---
  nonzero <- vals[, "trans_qaly"] != 0
  if (any(nonzero)) {
    expect_false(isTRUE(all.equal(
      disc[nonzero, "trans_qaly"],
      vals[nonzero, "trans_qaly"]
    )))
  }

  # --- Model start cost: override = "0.9" => cycle 1 value * 0.9 ---
  expect_equal(disc[1, "start_cost"], vals[1, "start_cost"] * 0.9)

  # --- Model start outcome: no override => standard discounting at cycle 1 ---
  # With start timing and no DT offset, cycle 1 discount factor = 1/(1+r)^0 = 1.
  # So model start values at cycle 1 are undiscounted with standard discounting.
  expect_equal(disc[1, "start_qaly"], vals[1, "start_qaly"])
})

# =============================================================================
# Test 2: PSM — residency, transition, model start, DT
# =============================================================================

test_that("PSM: discounting_override works for all value types", {
  model <- define_model("psm") |>
    set_settings(
      timeframe = 5,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5
    ) |>
    add_strategy("s") |>
    add_tree_node("s", "root", parent = NA, formula = 1) |>
    add_tree_node("s", "alive", parent = "root", formula = 1) |>
    set_decision_tree("s", duration = 30, duration_unit = "days") |>
    add_state("progression_free") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist",
      'define_surv_param("weibull", shape = 1.2, scale = 8)') |>
    add_variable("os_dist",
      'define_surv_param("weibull", shape = 1.1, scale = 14)') |>
    add_transition("PFS", "years", pfs_dist) |>
    add_transition("OS", "years", os_dist) |>
    # Residency cost WITH override (= "1" => no discounting)
    add_value("res_cost", 1000, state = "progression_free", type = "cost",
              discounting_override = "1") |>
    # Residency outcome WITHOUT override
    add_value("res_qaly", 1, state = "progression_free", type = "outcome") |>
    # Transition cost WITH override (PFS -> progressed)
    add_value("trans_cost", 500, state = "progression_free",
              destination = "progressed", type = "cost",
              discounting_override = "0.95") |>
    # Transition outcome WITHOUT override
    add_value("trans_qaly", 0.2, state = "progression_free",
              destination = "progressed", type = "outcome") |>
    # Model start cost WITH override
    add_value("start_cost", 2000, type = "cost",
              discounting_override = "0.9") |>
    # Model start outcome WITHOUT override
    add_value("start_qaly", 0.1, type = "outcome") |>
    # DT cost WITH override
    add_value("dt_cost", type = "cost", state = "decision_tree",
              formula = "p(alive, s) * 50000",
              discounting_override = "0.85") |>
    # DT outcome WITHOUT override
    add_value("dt_qaly", type = "outcome", state = "decision_tree",
              formula = "p(alive, s) * 0.5") |>
    add_summary("total_cost", type = "cost",
                values = "res_cost,trans_cost,start_cost,dt_cost") |>
    add_summary("total_qaly", type = "outcome",
                values = "res_qaly,trans_qaly,start_qaly,dt_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # --- Residency cost: override = "1" => discounted == undiscounted ---
  expect_equal(disc[, "res_cost"], vals[, "res_cost"])

  # --- Residency outcome: no override => regular discounting applied ---
  expect_false(isTRUE(all.equal(disc[, "res_qaly"], vals[, "res_qaly"])))

  # --- Transition cost: override = "0.95" ---
  expect_equal(disc[, "trans_cost"], vals[, "trans_cost"] * 0.95)

  # --- Transition outcome: no override => regular discounting ---
  nonzero <- vals[, "trans_qaly"] != 0
  if (any(nonzero)) {
    expect_false(isTRUE(all.equal(
      disc[nonzero, "trans_qaly"],
      vals[nonzero, "trans_qaly"]
    )))
  }

  # --- Model start cost: override = "0.9" ---
  expect_equal(disc[1, "start_cost"], vals[1, "start_cost"] * 0.9)

  # --- DT cost: override = "0.85" ---
  expect_equal(disc[, "dt_cost"], vals[, "dt_cost"] * 0.85)

  # --- DT outcome: no override => no discounting (DT default) ---
  expect_equal(disc[, "dt_qaly"], vals[, "dt_qaly"])
})

# =============================================================================
# Test 3: Custom PSM — residency, model start (no transition or DT values)
# =============================================================================
# Note: Custom PSM does not support transition values (by design).

test_that("Custom PSM: discounting_override works for residency and model start", {
  model <- define_model("custom_psm") |>
    set_settings(
      timeframe = 5,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5
    ) |>
    add_strategy("s") |>
    add_state("alive") |>
    add_state("dead") |>
    add_group("patients") |>
    add_transition("alive", 0.9^year) |>
    add_transition("dead", C) |>
    # Residency cost WITH override (= "1" => no discounting)
    add_value("res_cost", 1000, state = "alive", type = "cost",
              discounting_override = "1") |>
    # Residency outcome WITHOUT override
    add_value("res_qaly", 1, state = "alive", type = "outcome") |>
    # Model start cost WITH override
    add_value("start_cost", 2000, type = "cost",
              discounting_override = "0.9") |>
    # Model start outcome WITHOUT override
    add_value("start_qaly", 0.1, type = "outcome") |>
    add_summary("total_cost", type = "cost",
                values = "res_cost,start_cost") |>
    add_summary("total_qaly", type = "outcome",
                values = "res_qaly,start_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # --- Residency cost: override = "1" => discounted == undiscounted ---
  expect_equal(disc[, "res_cost"], vals[, "res_cost"])

  # --- Residency outcome: no override => regular discounting applied ---
  expect_false(isTRUE(all.equal(disc[, "res_qaly"], vals[, "res_qaly"])))

  # --- Model start cost: override = "0.9" ---
  expect_equal(disc[1, "start_cost"], vals[1, "start_cost"] * 0.9)

  # --- Model start outcome: no override => standard discounting at cycle 1 ---
  # With start timing and no DT offset, cycle 1 discount factor = 1/(1+r)^0 = 1.
  # So model start values at cycle 1 are undiscounted with standard discounting.
  expect_equal(disc[1, "start_qaly"], vals[1, "start_qaly"])
})

# =============================================================================
# Test 4: Standalone Decision Tree — with and without override
# =============================================================================

test_that("Standalone DT: override applies formula, blank means no discounting", {
  model <- define_model("decision_tree") |>
    add_variable("drug_a_cost", 10000) |>
    add_strategy("treatment") |>
    add_tree_node("treatment", "root", parent = NA, formula = 1) |>
    add_tree_node("treatment", "drug_a", parent = "root", formula = 0.6) |>
    add_tree_node("treatment", "drug_b", parent = "root", formula = "C") |>
    set_decision_tree("treatment", duration = 0, duration_unit = "days") |>
    # DT cost WITH override
    add_value("drug_cost", type = "cost", state = "decision_tree",
              formula = "p(drug_a, treatment) * drug_a_cost + p(drug_b, treatment) * 5000",
              discounting_override = "0.92") |>
    # DT outcome WITHOUT override (blank => no discounting in standalone DT)
    add_value("drug_qaly", type = "outcome", state = "decision_tree",
              formula = "p(drug_a, treatment) * 0.8 + p(drug_b, treatment) * 0.6") |>
    add_summary("total_cost", type = "cost", values = "drug_cost") |>
    add_summary("total_qaly", type = "outcome", values = "drug_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # drug_cost undiscounted = 0.6*10000 + 0.4*5000 = 8000
  expect_equal(vals[1, "drug_cost"], 8000, tolerance = 0.01)

  # drug_cost discounted = 8000 * 0.92 = 7360
  expect_equal(disc[1, "drug_cost"], 8000 * 0.92, tolerance = 0.01)

  # drug_qaly: no override => remains undiscounted (standalone DT default)
  expect_equal(disc[1, "drug_qaly"], vals[1, "drug_qaly"])
})

# =============================================================================
# Tests 5-10: Weighted-average trace()/discount_factors override pattern
# =============================================================================
# Tests that discounting_override with trace() uses person-time at each time point
# (occupancy x half-cycle weight), and that different half_cycle_methods produce
# different results.

# --- PSM: weighted-average override with "start" half-cycle ---
test_that("PSM: weighted-average trace() override uses person-time weights (start)", {
  model <- define_model("psm") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5,
      half_cycle_method = "start"
    ) |>
    add_strategy("s") |>
    add_state("progression_free") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", 'define_surv_param("weibull", shape = 1.2, scale = 8)') |>
    add_variable("os_dist", 'define_surv_param("weibull", shape = 1.1, scale = 14)') |>
    add_transition("PFS", "years", pfs_dist) |>
    add_transition("OS", "years", os_dist) |>
    add_value("prog_cost", 500, state = "progression_free",
              destination = "progressed", type = "cost",
              discounting_override = "sum(discount_factors * trace('progressed')) / sum(trace('progressed'))") |>
    add_value("dummy_qaly", 1, state = "progression_free", type = "outcome") |>
    add_summary("total_cost", type = "cost", values = "prog_cost") |>
    add_summary("total_qaly", type = "outcome", values = "dummy_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # trace() is person-time at time points 0..n: occupancy x half-cycle weight
  n_cycles <- 5
  occ <- tv$trace[, "progressed"]
  w <- openqaly:::point_weights(n_cycles, "start")
  discount_factors <- calculate_discount_factors(n_cycles, 3.5, 1)
  expected_factor <- sum(discount_factors * occ * w) / sum(occ * w)
  expect_equal(disc[, "prog_cost"], vals[, "prog_cost"] * expected_factor)
})

# --- PSM: weighted-average override with "life-table" half-cycle ---
test_that("PSM: weighted-average trace() override uses person-time weights (life-table)", {
  model <- define_model("psm") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5,
      half_cycle_method = "life-table"
    ) |>
    add_strategy("s") |>
    add_state("progression_free") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", 'define_surv_param("weibull", shape = 1.2, scale = 8)') |>
    add_variable("os_dist", 'define_surv_param("weibull", shape = 1.1, scale = 14)') |>
    add_transition("PFS", "years", pfs_dist) |>
    add_transition("OS", "years", os_dist) |>
    add_value("prog_cost", 500, state = "progression_free",
              destination = "progressed", type = "cost",
              discounting_override = "sum(discount_factors * trace('progressed')) / sum(trace('progressed'))") |>
    add_value("dummy_qaly", 1, state = "progression_free", type = "outcome") |>
    add_summary("total_cost", type = "cost", values = "prog_cost") |>
    add_summary("total_qaly", type = "outcome", values = "dummy_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # trace() is person-time at time points 0..n: occupancy x half-cycle weight
  n_cycles <- 5
  occ <- tv$trace[, "progressed"]
  w <- openqaly:::point_weights(n_cycles, "life-table")
  discount_factors <- calculate_discount_factors(n_cycles, 3.5, 1)
  expected_factor <- sum(discount_factors * occ * w) / sum(occ * w)
  expect_equal(disc[, "prog_cost"], vals[, "prog_cost"] * expected_factor)
})

# --- Markov: weighted-average override with "start" half-cycle ---
test_that("Markov: weighted-average trace() override uses person-time weights (start)", {
  model <- define_model("markov") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5,
      half_cycle_method = "start"
    ) |>
    add_strategy("s") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.2) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.3) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("trans_cost", 500, state = "healthy", destination = "sick",
              type = "cost",
              discounting_override = "sum(discount_factors * trace('sick')) / sum(trace('sick'))") |>
    add_value("dummy_qaly", 1, state = "healthy", type = "outcome") |>
    add_summary("total_cost", type = "cost", values = "trans_cost") |>
    add_summary("total_qaly", type = "outcome", values = "dummy_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # trace() is person-time at time points 0..n: occupancy x half-cycle weight
  n_cycles <- 5
  occ <- result$segments$collapsed_trace[[1]][, "sick"]
  w <- openqaly:::point_weights(n_cycles, "start")
  discount_factors <- calculate_discount_factors(n_cycles, 3.5, 1)
  expected_factor <- sum(discount_factors * occ * w) / sum(occ * w)
  expect_equal(disc[, "trans_cost"], vals[, "trans_cost"] * expected_factor)
})

# --- Markov: weighted-average override with "life-table" half-cycle ---
test_that("Markov: weighted-average trace() override uses person-time weights (life-table)", {
  model <- define_model("markov") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5,
      half_cycle_method = "life-table"
    ) |>
    add_strategy("s") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.2) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.3) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("trans_cost", 500, state = "healthy", destination = "sick",
              type = "cost",
              discounting_override = "sum(discount_factors * trace('sick')) / sum(trace('sick'))") |>
    add_value("dummy_qaly", 1, state = "healthy", type = "outcome") |>
    add_summary("total_cost", type = "cost", values = "trans_cost") |>
    add_summary("total_qaly", type = "outcome", values = "dummy_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # trace() is person-time at time points 0..n: occupancy x half-cycle weight
  n_cycles <- 5
  occ <- result$segments$collapsed_trace[[1]][, "sick"]
  w <- openqaly:::point_weights(n_cycles, "life-table")
  discount_factors <- calculate_discount_factors(n_cycles, 3.5, 1)
  expected_factor <- sum(discount_factors * occ * w) / sum(occ * w)
  expect_equal(disc[, "trans_cost"], vals[, "trans_cost"] * expected_factor)
})

# --- Custom PSM: weighted-average override with "start" half-cycle ---
test_that("Custom PSM: weighted-average trace() override uses person-time weights (start)", {
  model <- define_model("custom_psm") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5,
      half_cycle_method = "start"
    ) |>
    add_strategy("s") |>
    add_state("alive") |>
    add_state("dead") |>
    add_group("patients") |>
    add_transition("alive", 0.9^year) |>
    add_transition("dead", C) |>
    add_value("res_cost", 1000, state = "alive", type = "cost",
              discounting_override = "sum(discount_factors * trace('alive')) / sum(trace('alive'))") |>
    add_value("dummy_qaly", 1, state = "alive", type = "outcome") |>
    add_summary("total_cost", type = "cost", values = "res_cost") |>
    add_summary("total_qaly", type = "outcome", values = "dummy_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # trace() is person-time at time points 0..n: occupancy x half-cycle weight
  n_cycles <- 5
  occ <- tv$trace[, "alive"]
  w <- openqaly:::point_weights(n_cycles, "start")
  discount_factors <- calculate_discount_factors(n_cycles, 3.5, 1)
  expected_factor <- sum(discount_factors * occ * w) / sum(occ * w)
  expect_equal(disc[, "res_cost"], vals[, "res_cost"] * expected_factor)
})

# --- Custom PSM: weighted-average override with "life-table" half-cycle ---
test_that("Custom PSM: weighted-average trace() override uses person-time weights (life-table)", {
  model <- define_model("custom_psm") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5,
      half_cycle_method = "life-table"
    ) |>
    add_strategy("s") |>
    add_state("alive") |>
    add_state("dead") |>
    add_group("patients") |>
    add_transition("alive", 0.9^year) |>
    add_transition("dead", C) |>
    add_value("res_cost", 1000, state = "alive", type = "cost",
              discounting_override = "sum(discount_factors * trace('alive')) / sum(trace('alive'))") |>
    add_value("dummy_qaly", 1, state = "alive", type = "outcome") |>
    add_summary("total_cost", type = "cost", values = "res_cost") |>
    add_summary("total_qaly", type = "outcome", values = "dummy_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]
  vals <- tv$values
  disc <- tv$values_discounted

  # trace() is person-time at time points 0..n: occupancy x half-cycle weight
  n_cycles <- 5
  occ <- tv$trace[, "alive"]
  w <- openqaly:::point_weights(n_cycles, "life-table")
  discount_factors <- calculate_discount_factors(n_cycles, 3.5, 1)
  expected_factor <- sum(discount_factors * occ * w) / sum(occ * w)
  expect_equal(disc[, "res_cost"], vals[, "res_cost"] * expected_factor)
})
