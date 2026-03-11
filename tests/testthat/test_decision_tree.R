context("Decision tree model tests")

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Create a standalone decision tree model
create_standalone_dt_model <- function() {
  define_model("decision_tree") |>
    add_variable("drug_a_cost", 10000) |>
    add_strategy("treatment") |>
    add_tree_node("treatment", "root", parent = NA, formula = 1) |>
    add_tree_node("treatment", "drug_a", parent = "root", formula = 0.6) |>
    add_tree_node("treatment", "drug_b", parent = "root", formula = "C") |>
    set_decision_tree("treatment", duration = 0, duration_unit = "days") |>
    add_value("drug_cost", type = "cost", state = "decision_tree",
              formula = p(drug_a, treatment) * drug_a_cost + p(drug_b, treatment) * 5000) |>
    add_value("drug_qaly", type = "outcome", state = "decision_tree",
              formula = p(drug_a, treatment) * 0.8 + p(drug_b, treatment) * 0.6) |>
    add_summary("total_cost", type = "cost", values = "drug_cost") |>
    add_summary("total_qaly", type = "outcome", values = "drug_qaly")
}

#' Create a Markov model with decision tree in front
create_markov_with_dt <- function() {
  define_model("markov") |>
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5
    ) |>
    add_strategy("surgery") |>
    add_tree_node("surgery", "root", parent = NA, formula = 1) |>
    add_tree_node("surgery", "success", parent = "root", formula = 0.85) |>
    add_tree_node("surgery", "failure", parent = "root", formula = "C") |>
    set_decision_tree("surgery", duration = 30, duration_unit = "days") |>
    add_state("healthy", initial_prob = "p(success, surgery)") |>
    add_state("dead", initial_prob = "C") |>
    add_variable("c_treatment", 1000) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("surgery_cost", type = "cost", state = "decision_tree",
              formula = p(success, surgery) * 50000 + p(failure, surgery) * 75000) |>
    add_value("treatment_cost", type = "cost", state = "healthy",
              formula = c_treatment) |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "surgery_cost,treatment_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")
}

# ==============================================================================
# Builder API Tests
# ==============================================================================

test_that("define_model accepts 'decision_tree' type", {
  model <- define_model("decision_tree")
  expect_equal(model$settings$model_type, "decision_tree")
  expect_null(model$decision_tree)
  expect_s3_class(model, "oq_model_builder")
})

test_that("add_tree_node creates and appends to trees tibble", {
  model <- define_model("markov") |>
    add_tree_node("my_tree", "root", parent = NA, formula = 1)

  expect_false(is.null(model$trees))
  expect_equal(nrow(model$trees), 1)
  expect_equal(model$trees$name[1], "my_tree")
  expect_equal(model$trees$node[1], "root")
  expect_true(is.na(model$trees$parent[1]))
  expect_equal(model$trees$formula[1], "1")

  # Add more nodes
  model <- model |>
    add_tree_node("my_tree", "left", parent = "root", formula = 0.6) |>
    add_tree_node("my_tree", "right", parent = "root", formula = "C")

  expect_equal(nrow(model$trees), 3)
})

test_that("set_decision_tree sets config correctly", {
  model <- define_model("markov") |>
    add_tree_node("my_tree", "root", parent = NA, formula = 1) |>
    set_decision_tree("my_tree", duration = 30, duration_unit = "days")

  expect_false(is.null(model$decision_tree))
  expect_equal(model$decision_tree$tree_name, "my_tree")
  expect_equal(model$decision_tree$duration, "30")
  expect_equal(model$decision_tree$duration_unit, "days")
})

test_that("set_decision_tree validates duration_unit", {
  model <- define_model("markov") |>
    add_tree_node("t", "root", parent = NA, formula = 1)

  expect_error(
    set_decision_tree(model, "t", duration = 1, duration_unit = "invalid"),
    "arg"
  )
})

test_that("remove_decision_tree clears config", {
  model <- define_model("markov") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 1, duration_unit = "days") |>
    remove_decision_tree()

  expect_null(model$decision_tree)
})

test_that("add_value allows state = 'decision_tree'", {
  model <- define_model("decision_tree") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 0, duration_unit = "days") |>
    add_strategy("s") |>
    add_value("my_val", formula = 100, state = "decision_tree", type = "cost")

  expect_equal(nrow(model$values), 1)
  expect_equal(model$values$state[1], "decision_tree")
})

test_that("add_value rejects destination with state = 'decision_tree'", {
  model <- define_model("markov")
  expect_error(
    add_value(model, "v", formula = 1, state = "decision_tree", destination = "healthy"),
    "decision_tree.*cannot have a destination"
  )
})

test_that("decision_tree model rejects add_state", {
  model <- define_model("decision_tree")
  expect_error(
    add_state(model, "healthy", initial_prob = 1),
    "Decision tree models do not support states"
  )
})

test_that("decision_tree model rejects add_transition", {
  model <- define_model("decision_tree")
  expect_error(
    add_transition(model, "a", "b", 0.5),
    "Decision tree models do not support transitions"
  )
})

# ==============================================================================
# Standalone Decision Tree Execution Tests
# ==============================================================================

test_that("standalone DT model runs and produces correct summaries", {
  model <- create_standalone_dt_model()
  result <- run_model(model)

  expect_false(is.null(result))
  expect_false(is.null(result$aggregated))

  # Get summaries
  summaries <- result$aggregated$summaries_discounted[[1]]

  # Expected: drug_cost = 0.6 * 10000 + 0.4 * 5000 = 8000
  # Expected: drug_qaly = 0.6 * 0.8 + 0.4 * 0.6 = 0.72
  cost_summary <- summaries[summaries$summary == "total_cost", ]
  qaly_summary <- summaries[summaries$summary == "total_qaly", ]

  expect_equal(cost_summary$amount, 8000, tolerance = 0.01)
  expect_equal(qaly_summary$amount, 0.72, tolerance = 0.01)
})

test_that("standalone DT summaries are not discounted", {
  model <- create_standalone_dt_model()
  result <- run_model(model)

  summaries_disc <- result$aggregated$summaries_discounted[[1]]
  summaries_undisc <- result$aggregated$summaries[[1]]

  # For standalone DT, discounted and undiscounted should be identical
  expect_equal(summaries_disc$amount, summaries_undisc$amount)
})

# ==============================================================================
# Markov + Decision Tree Tests
# ==============================================================================

test_that("Markov + DT model runs successfully", {
  model <- create_markov_with_dt()
  result <- run_model(model)

  expect_false(is.null(result))
  expect_false(is.null(result$aggregated))

  summaries <- result$aggregated$summaries_discounted[[1]]
  cost_summary <- summaries[summaries$summary == "total_cost", ]

  # Surgery cost should include DT phase cost (undiscounted)
  # DT cost = 0.85 * 50000 + 0.15 * 75000 = 53750
  # Plus downstream treatment costs (discounted with offset)
  expect_true(sum(cost_summary$amount) > 53750)  # At least the DT cost
})

test_that("DT values are not discounted in Markov + DT model", {
  model <- create_markov_with_dt()
  result <- run_model(model)

  # The DT value (surgery_cost) should appear undiscounted in values_discounted
  seg <- result$segments
  tv <- seg$trace_and_values[[1]]
  values_disc <- tv$values_discounted
  values_undisc <- tv$values

  # surgery_cost should be the same in both
  expect_equal(values_disc[, "surgery_cost"], values_undisc[, "surgery_cost"])
})

# ==============================================================================
# Discounting Tests
# ==============================================================================

test_that("calculate_discount_factors respects offset_years", {
  # Without offset
  factors_no_offset <- calculate_discount_factors(5, 3.5, 1, "start", "by_cycle", offset_years = 0)
  # With 1-year offset
  factors_with_offset <- calculate_discount_factors(5, 3.5, 1, "start", "by_cycle", offset_years = 1)

  # With offset, all factors should be smaller (more discounting)
  expect_true(all(factors_with_offset < factors_no_offset))

  # Factor at cycle 1 with 1-year offset should equal factor at cycle 2 without offset
  # Because cycle 1 with offset=1 means time=0+1=1 year, same as cycle 2 with offset=0 means time=1+0=1 year
  expect_equal(factors_with_offset[1], factors_no_offset[2], tolerance = 1e-10)
})

test_that("calculate_discount_factors with offset_years = 0 matches default", {
  factors_default <- calculate_discount_factors(10, 3.5, 1, "start", "by_cycle")
  factors_explicit <- calculate_discount_factors(10, 3.5, 1, "start", "by_cycle", offset_years = 0)

  expect_equal(factors_default, factors_explicit)
})

# ==============================================================================
# Validation Tests
# ==============================================================================

test_that("validate_decision_tree catches missing tree reference", {
  model <- define_model("markov") |>
    add_tree_node("tree_a", "root", parent = NA, formula = 1)

  model$decision_tree <- list(
    tree_name = "nonexistent_tree",
    duration = "0",
    duration_unit = "days"
  )

  expect_error(
    normalize_and_validate_model(model, preserve_builder = FALSE),
    "does not exist"
  )
})

test_that("standalone DT without decision_tree config fails validation", {
  model <- define_model("decision_tree") |>
    add_strategy("s") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 0, duration_unit = "days") |>
    add_value("v", formula = 1, state = "decision_tree", type = "cost") |>
    add_summary("total", type = "cost", values = "v")

  # Remove decision_tree to trigger validation error
  model$decision_tree <- NULL

  expect_error(
    normalize_and_validate_model(model, preserve_builder = FALSE),
    "must have a decision_tree configuration"
  )
})

# ==============================================================================
# Serialization Round-Trip Tests
# ==============================================================================

test_that("decision_tree config survives JSON round-trip", {
  model <- create_standalone_dt_model()

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  expect_false(is.null(model_back$decision_tree))
  expect_equal(model_back$decision_tree$tree_name, model$decision_tree$tree_name)
  expect_equal(model_back$decision_tree$duration, model$decision_tree$duration)
  expect_equal(model_back$decision_tree$duration_unit, model$decision_tree$duration_unit)
})

test_that("decision_tree values survive JSON round-trip", {
  model <- create_standalone_dt_model()

  json <- write_model_json(model)
  model_back <- read_model_json(json)

  dt_values_orig <- model$values[model$values$state == "decision_tree", ]
  dt_values_back <- model_back$values[model_back$values$state == "decision_tree", ]

  expect_equal(nrow(dt_values_orig), nrow(dt_values_back))
  expect_equal(dt_values_orig$name, dt_values_back$name)
})

test_that("R codegen includes set_decision_tree", {
  model <- create_standalone_dt_model()
  code <- model_to_r_code(model)

  expect_true(any(grepl("set_decision_tree", code)))
  expect_true(any(grepl("treatment", code)))
})

# ==============================================================================
# Model Type Normalization Tests
# ==============================================================================

test_that("model_type 'decision_tree' is normalized correctly", {
  model <- define_model("decision_tree") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 0, duration_unit = "days")
  model <- normalize_and_validate_model(model, preserve_builder = TRUE)
  expect_equal(model$settings$model_type, "decision_tree")
})

# ==============================================================================
# Plot Tests
# ==============================================================================

test_that("plot_decision_tree returns a gg object", {
  model <- create_standalone_dt_model()
  p <- plot_decision_tree(model)
  expect_s3_class(p, "gg")
})

test_that("plot_decision_tree respects tree_name parameter", {
  model <- create_standalone_dt_model()
  p <- plot_decision_tree(model, tree_name = "treatment")
  expect_s3_class(p, "gg")
})

test_that("plot_decision_tree errors on nonexistent tree_name", {
  model <- create_standalone_dt_model()
  expect_error(
    plot_decision_tree(model, tree_name = "nonexistent"),
    "not found"
  )
})

test_that("plot_decision_tree errors on model with no trees", {
  model <- define_model("markov")
  expect_error(
    plot_decision_tree(model),
    "does not contain any decision trees"
  )
})

test_that("plot_decision_tree errors on non-model input", {
  expect_error(
    plot_decision_tree(list(a = 1)),
    "must be an oq_model"
  )
})

# ==============================================================================
# get_dt_duration_days Tests
# ==============================================================================

test_that("get_dt_duration_days returns 0 when no decision tree", {
  model <- define_model("markov")
  expect_equal(get_dt_duration_days(model), 0)
})

test_that("get_dt_duration_days converts days correctly", {
  model <- define_model("markov") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 30, duration_unit = "days")
  expect_equal(get_dt_duration_days(model), 30)
})

test_that("get_dt_duration_days converts weeks correctly", {
  model <- define_model("markov") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 4, duration_unit = "weeks")
  expect_equal(get_dt_duration_days(model), 28)
})

test_that("get_dt_duration_days converts years correctly", {
  model <- define_model("markov") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = 2, duration_unit = "years")
  expect_equal(get_dt_duration_days(model), 730)
})

test_that("get_dt_duration_days returns 0 with warning for formula duration", {
  model <- define_model("markov") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    set_decision_tree("t", duration = "some_var", duration_unit = "days")
  expect_warning(
    result <- get_dt_duration_days(model),
    "not a simple numeric value"
  )
  expect_equal(result, 0)
})

# ==============================================================================
# Cycle Count Adjustment Tests
# ==============================================================================

test_that("Markov + DT reduces cycle count by DT duration", {
  model <- define_model("markov") |>
    set_settings(
      timeframe = 5,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0,
      discount_outcomes = 0
    ) |>
    add_strategy("s") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    add_tree_node("t", "alive", parent = "root", formula = 1) |>
    set_decision_tree("t", duration = 2, duration_unit = "years") |>
    add_state("healthy", initial_prob = "p(alive, t)") |>
    add_state("dead", initial_prob = 0) |>
    add_variable("c_treatment", 1000) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("dt_cost", type = "cost", state = "decision_tree",
              formula = "p(alive, t) * 50000") |>
    add_value("treatment_cost", type = "cost", state = "healthy",
              formula = "c_treatment") |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "dt_cost,treatment_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  # 5 years total - 2 years DT = 3 years of Markov -> 3 cycles
  # Trace should have 4 rows (cycle 0 + 3 cycles)
  trace <- result$segments$trace_and_values[[1]]$trace
  expect_equal(nrow(trace), 4)
})

test_that("DT duration exceeding timeframe produces error", {
  model <- define_model("markov") |>
    set_settings(
      timeframe = 1,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0,
      discount_outcomes = 0
    ) |>
    add_strategy("s") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    add_tree_node("t", "alive", parent = "root", formula = 1) |>
    set_decision_tree("t", duration = 2, duration_unit = "years") |>
    add_state("healthy", initial_prob = "p(alive, t)") |>
    add_state("dead", initial_prob = 0) |>
    add_variable("c_treatment", 1000) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("dt_cost", type = "cost", state = "decision_tree",
              formula = "p(alive, t) * 50000") |>
    add_value("treatment_cost", type = "cost", state = "healthy",
              formula = "c_treatment") |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "dt_cost,treatment_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  expect_error(run_model(model), "exceeds or equals")
})

test_that("Markov state values are discounted with DT offset", {
  model <- create_markov_with_dt()
  result <- run_model(model)

  seg <- result$segments
  tv <- seg$trace_and_values[[1]]
  values <- tv$values
  values_disc <- tv$values_discounted

  # treatment_cost is a Markov state value (state = "healthy")
  # With start timing and by_cycle method:
  #   time_years = (cycle - 1) * cycle_length_years + offset_years
  # offset_years = 30/365 (30-day DT duration)
  # cycle 1: time = (1 - 1) * 1 + 30/365 = 30/365
  # expected factor = 1 / 1.035^(30/365)
  offset <- 30 / 365
  expected_factor_c1 <- 1 / (1.035)^(0 + offset)
  factor_without_offset <- 1 / (1.035)^0  # = 1

  actual_ratio <- values_disc[1, "treatment_cost"] / values[1, "treatment_cost"]

  expect_equal(actual_ratio, expected_factor_c1, tolerance = 1e-6)
  # Must NOT equal the factor without offset (which is 1.0)
  expect_true(abs(actual_ratio - factor_without_offset) > 1e-6)
})

test_that("PSM + DT: DT values undiscounted, state values discounted with offset", {
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
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    add_tree_node("t", "alive", parent = "root", formula = 1) |>
    set_decision_tree("t", duration = 60, duration_unit = "days") |>
    add_state("progression_free") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist",
      'define_surv_param("weibull", shape = 1.2, scale = 8)') |>
    add_variable("os_dist",
      'define_surv_param("weibull", shape = 1.1, scale = 14)') |>
    add_psm_transition("PFS", "years", pfs_dist) |>
    add_psm_transition("OS", "years", os_dist) |>
    add_value("dt_cost", type = "cost", state = "decision_tree",
              formula = "p(alive, t) * 50000") |>
    add_value("treatment_cost", type = "cost", state = "progression_free",
              formula = 1000) |>
    add_value("qaly", type = "outcome", state = "progression_free",
              formula = 1) |>
    add_summary("total_cost", type = "cost", values = "dt_cost,treatment_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)

  seg <- result$segments
  tv <- seg$trace_and_values[[1]]
  values <- tv$values
  values_disc <- tv$values_discounted

  # DT value (dt_cost) should be identical in discounted and undiscounted
  expect_equal(values_disc[, "dt_cost"], values[, "dt_cost"])

  # PSM state value (treatment_cost) should differ between discounted and undiscounted
  expect_false(isTRUE(all.equal(values_disc[, "treatment_cost"], values[, "treatment_cost"])))

  # Verify discount factor at cycle 1 uses DT offset
  # offset_years = 60/365, cycle 1 start timing: time = 0 + 60/365
  offset <- 60 / 365
  expected_factor_c1 <- 1 / (1.035)^(0 + offset)

  actual_ratio <- values_disc[1, "treatment_cost"] / values[1, "treatment_cost"]
  expect_equal(actual_ratio, expected_factor_c1, tolerance = 1e-6)
})

test_that("PSM + DT reduces cycle count by DT duration", {
  model <- define_model("psm") |>
    set_settings(
      timeframe = 5,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0,
      discount_outcomes = 0
    ) |>
    add_strategy("s") |>
    add_tree_node("t", "root", parent = NA, formula = 1) |>
    add_tree_node("t", "alive", parent = "root", formula = 1) |>
    set_decision_tree("t", duration = 2, duration_unit = "years") |>
    add_state("progression_free") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist",
      'define_surv_param("weibull", shape = 1.2, scale = 8)') |>
    add_variable("os_dist",
      'define_surv_param("weibull", shape = 1.1, scale = 14)') |>
    add_psm_transition("PFS", "years", pfs_dist) |>
    add_psm_transition("OS", "years", os_dist) |>
    add_value("dt_cost", type = "cost", state = "decision_tree",
              formula = "p(alive, t) * 50000") |>
    add_value("treatment_cost", type = "cost", state = "progression_free",
              formula = 1000) |>
    add_value("qaly", type = "outcome", state = "progression_free",
              formula = 1) |>
    add_summary("total_cost", type = "cost", values = "dt_cost,treatment_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  # 5 years total - 2 years DT = 3 years of PSM -> 3 cycles
  # Trace should have 4 rows (cycle 0 + 3 cycles)
  trace <- result$segments$trace_and_values[[1]]$trace
  expect_equal(nrow(trace), 4)
})

# ==============================================================================
# Discounting Override Tests
# ==============================================================================

test_that("discounting_override = '1' produces no discounting", {
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
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("cost", type = "cost", state = "healthy",
              formula = 1000, discounting_override = "1") |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]

  # Cost with override = "1" should be identical (no discounting applied)
  expect_equal(tv$values_discounted[, "cost"], tv$values[, "cost"])

  # QALY without override should still be discounted
  expect_false(isTRUE(all.equal(tv$values_discounted[, "qaly"], tv$values[, "qaly"])))
})

test_that("discounting_override with fixed offset formula works", {
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
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("cost", type = "cost", state = "healthy",
              formula = 1000,
              discounting_override = "1 / (1 + discount_rate/100)^(15/365)") |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]

  # The override is a scalar factor, applied uniformly to all cycles
  expected_factor <- 1 / (1 + 3.5/100)^(15/365)
  for (c in seq_len(nrow(tv$values))) {
    actual_ratio <- tv$values_discounted[c, "cost"] / tv$values[c, "cost"]
    expect_equal(actual_ratio, expected_factor, tolerance = 1e-8)
  }
})

test_that("discounting_override with trace() produces correct weighted-average factor", {
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
    add_value("progression_cost", type = "cost", state = "sick",
              formula = 5000,
              discounting_override = "sum(discount_factors * trace('sick')) / sum(trace('sick'))") |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "progression_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]

  # The override should be a scalar (weighted average discount factor)
  # All cycles should have the same ratio
  ratios <- tv$values_discounted[, "progression_cost"] / tv$values[, "progression_cost"]
  # Filter out cycles where values are 0 (avoid 0/0)
  nonzero <- tv$values[, "progression_cost"] != 0
  if (any(nonzero)) {
    ratios_nz <- ratios[nonzero]
    expect_true(all(abs(ratios_nz - ratios_nz[1]) < 1e-10))
  }
})

test_that("DT values without discounting_override still get no discounting (backward compat)", {
  model <- create_markov_with_dt()
  result <- run_model(model)

  tv <- result$segments$trace_and_values[[1]]
  # surgery_cost is a DT value without override -> should be undiscounted
  expect_equal(tv$values_discounted[, "surgery_cost"], tv$values[, "surgery_cost"])
})

test_that("DT values WITH discounting_override use the formula", {
  model <- define_model("markov") |>
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5
    ) |>
    add_strategy("surgery") |>
    add_tree_node("surgery", "root", parent = NA, formula = 1) |>
    add_tree_node("surgery", "success", parent = "root", formula = 0.85) |>
    add_tree_node("surgery", "failure", parent = "root", formula = "C") |>
    set_decision_tree("surgery", duration = 30, duration_unit = "days") |>
    add_state("healthy", initial_prob = "p(success, surgery)") |>
    add_state("dead", initial_prob = "C") |>
    add_variable("c_treatment", 1000) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("surgery_cost", type = "cost", state = "decision_tree",
              formula = p(success, surgery) * 50000 + p(failure, surgery) * 75000,
              discounting_override = "1 / (1 + discount_rate/100)^(15/365)") |>
    add_value("treatment_cost", type = "cost", state = "healthy", formula = c_treatment) |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "surgery_cost,treatment_cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]

  # DT value WITH override should NOT equal the undiscounted value
  expected_factor <- 1 / (1 + 3.5/100)^(15/365)
  expected_discounted <- tv$values[, "surgery_cost"] * expected_factor
  expect_equal(tv$values_discounted[, "surgery_cost"], expected_discounted, tolerance = 1e-8)

  # Confirm it's NOT equal to undiscounted (i.e., the override was actually applied)
  expect_false(isTRUE(all.equal(
    tv$values_discounted[, "surgery_cost"],
    tv$values[, "surgery_cost"]
  )))
})

test_that("standalone DT with fixed-offset discounting_override works", {
  model <- define_model("decision_tree") |>
    add_variable("drug_a_cost", 10000) |>
    add_strategy("treatment") |>
    add_tree_node("treatment", "root", parent = NA, formula = 1) |>
    add_tree_node("treatment", "drug_a", parent = "root", formula = 0.6) |>
    add_tree_node("treatment", "drug_b", parent = "root", formula = "C") |>
    set_decision_tree("treatment", duration = 0, duration_unit = "days") |>
    add_value("drug_cost", type = "cost", state = "decision_tree",
              formula = "p(drug_a, treatment) * drug_a_cost + p(drug_b, treatment) * 5000",
              discounting_override = "0.95") |>
    add_value("drug_qaly", type = "outcome", state = "decision_tree",
              formula = "p(drug_a, treatment) * 0.8 + p(drug_b, treatment) * 0.6") |>
    add_summary("total_cost", type = "cost", values = "drug_cost") |>
    add_summary("total_qaly", type = "outcome", values = "drug_qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]

  # drug_cost undiscounted = 0.6*10000 + 0.4*5000 = 8000
  # drug_cost discounted = 8000 * 0.95 = 7600
  expect_equal(tv$values[1, "drug_cost"], 8000, tolerance = 0.01)
  expect_equal(tv$values_discounted[1, "drug_cost"], 7600, tolerance = 0.01)

  # drug_qaly has no override -> should remain identical
  expect_equal(tv$values_discounted[1, "drug_qaly"], tv$values[1, "drug_qaly"])
})

test_that("standalone DT with trace() formula errors with clear message", {
  model <- define_model("decision_tree") |>
    add_variable("drug_a_cost", 10000) |>
    add_strategy("treatment") |>
    add_tree_node("treatment", "root", parent = NA, formula = 1) |>
    add_tree_node("treatment", "drug_a", parent = "root", formula = 0.6) |>
    add_tree_node("treatment", "drug_b", parent = "root", formula = "C") |>
    set_decision_tree("treatment", duration = 0, duration_unit = "days") |>
    add_value("drug_cost", type = "cost", state = "decision_tree",
              formula = "p(drug_a, treatment) * drug_a_cost + p(drug_b, treatment) * 5000",
              discounting_override = "sum(discount_factors * trace('healthy')) / sum(trace('healthy'))") |>
    add_value("drug_qaly", type = "outcome", state = "decision_tree",
              formula = "p(drug_a, treatment) * 0.8 + p(drug_b, treatment) * 0.6") |>
    add_summary("total_cost", type = "cost", values = "drug_cost") |>
    add_summary("total_qaly", type = "outcome", values = "drug_qaly")

  expect_error(run_model(model), "trace.*not available.*standalone decision tree")
})

test_that("discounting_override with discount_factors vector works in Markov", {
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
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("cost", type = "cost", state = "healthy",
              formula = 1000,
              discounting_override = "discount_factors") |>
    add_value("qaly", type = "outcome", state = "healthy", formula = 1) |>
    add_summary("total_cost", type = "cost", values = "cost") |>
    add_summary("total_qaly", type = "outcome", values = "qaly")

  result <- run_model(model)
  tv <- result$segments$trace_and_values[[1]]

  # Using "discount_factors" as override should produce same result as standard discounting
  # (since it just replaces with the same per-cycle factors)
  n <- nrow(tv$values)
  expected_factors <- calculate_discount_factors(n, 3.5, 1, "start", "by_cycle")
  for (c in seq_len(n)) {
    actual_ratio <- tv$values_discounted[c, "cost"] / tv$values[c, "cost"]
    expect_equal(actual_ratio, expected_factors[c], tolerance = 1e-8)
  }
})

# ==============================================================================
# plot_decision_tree on Results Tests
# ==============================================================================

test_that("plot_decision_tree on results returns gg with probability labels", {
  model <- create_standalone_dt_model()
  result <- run_model(model)
  p <- plot_decision_tree(result, strategy = "treatment")
  expect_s3_class(p, "gg")
  # Check that edge labels contain percentage signs (layer 2 = geom_label)
  edge_data <- ggplot2::layer_data(p, 2)
  expect_true(all(grepl("%", edge_data$label)))
})

test_that("plot_decision_tree on results respects tree_name parameter", {
  model <- create_standalone_dt_model()
  result <- run_model(model)
  p <- plot_decision_tree(result, strategy = "treatment",
                          tree_name = "treatment")
  expect_s3_class(p, "gg")
})

test_that("plot_decision_tree on results errors when strategy is missing", {
  model <- create_standalone_dt_model()
  result <- run_model(model)
  expect_error(
    plot_decision_tree(result),
    "strategy.*required"
  )
})

test_that("plot_decision_tree on results errors on invalid strategy", {
  model <- create_standalone_dt_model()
  result <- run_model(model)
  expect_error(
    plot_decision_tree(result, strategy = "nonexistent"),
    "not found"
  )
})

test_that("plot_decision_tree on results works with multi-strategy model", {
  model <- define_model("decision_tree") |>
    add_variable("drug_a_cost", 10000) |>
    add_strategy("treatment_a") |>
    add_strategy("treatment_b") |>
    add_tree_node("my_tree", "root", parent = NA, formula = 1) |>
    add_tree_node("my_tree", "drug_a", parent = "root", formula = 0.6) |>
    add_tree_node("my_tree", "drug_b", parent = "root", formula = "C") |>
    set_decision_tree("my_tree", duration = 0, duration_unit = "days") |>
    add_value("drug_cost", type = "cost", state = "decision_tree",
              formula = p(drug_a, my_tree) * drug_a_cost +
                p(drug_b, my_tree) * 5000) |>
    add_value("drug_qaly", type = "outcome", state = "decision_tree",
              formula = p(drug_a, my_tree) * 0.8 +
                p(drug_b, my_tree) * 0.6) |>
    add_summary("total_cost", type = "cost", values = "drug_cost") |>
    add_summary("total_qaly", type = "outcome", values = "drug_qaly")

  result <- run_model(model)
  p <- plot_decision_tree(result, strategy = "treatment_a")
  expect_s3_class(p, "gg")
  p2 <- plot_decision_tree(result, strategy = "treatment_b")
  expect_s3_class(p2, "gg")
})

test_that("plot_decision_tree on results errors without eval_vars", {
  model <- create_standalone_dt_model()
  result <- run_model(model)
  result$segments$eval_vars <- NULL
  expect_error(
    plot_decision_tree(result, strategy = "treatment"),
    "base-case"
  )
})

# ==============================================================================
# Tree Name Collision Tests
# ==============================================================================

test_that("tree name collides with variable name (tree added first)", {
  expect_error(
    define_model("markov") |>
      add_tree_node("my_var", "root", parent = NA, formula = 1) |>
      add_variable("my_var", 100),
    'Name collision.*"my_var".*decision tree'
  )
})

test_that("tree name collides with variable name (variable added first)", {
  expect_error(
    define_model("markov") |>
      add_variable("my_var", 100) |>
      add_tree_node("my_var", "root", parent = NA, formula = 1),
    "Name collision.*decision trees and variables"
  )
})

test_that("tree name collides with table name (tree added first)", {
  expect_error(
    define_model("markov") |>
      add_tree_node("my_tbl", "root", parent = NA, formula = 1) |>
      add_table("my_tbl", data.frame(x = 1)),
    'Name collision.*"my_tbl".*decision tree'
  )
})

test_that("tree name collides with table name (table added first)", {
  expect_error(
    define_model("markov") |>
      add_table("my_tbl", data.frame(x = 1)) |>
      add_tree_node("my_tbl", "root", parent = NA, formula = 1),
    "Name collision.*decision trees and tables"
  )
})

test_that("tree name collides with value name (tree added first)", {
  expect_error(
    define_model("markov") |>
      add_tree_node("my_val", "root", parent = NA, formula = 1) |>
      add_strategy("s") |>
      add_state("healthy", initial_prob = 1) |>
      add_state("dead", initial_prob = 0) |>
      add_transition("healthy", "dead", 0.1) |>
      add_transition("healthy", "healthy", C) |>
      add_transition("dead", "dead", 1) |>
      add_value("my_val", formula = 100, type = "cost", state = "healthy"),
    'Name collision.*"my_val".*decision tree'
  )
})

test_that("tree name collides with value name (value added first)", {
  model <- define_model("markov") |>
    add_strategy("s") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("my_val", formula = 100, type = "cost", state = "healthy")

  expect_error(
    model |> add_tree_node("my_val", "root", parent = NA, formula = 1),
    "Name collision.*decision trees and values"
  )
})

test_that("tree name collides with state name (tree added first)", {
  expect_error(
    define_model("markov") |>
      add_tree_node("healthy", "root", parent = NA, formula = 1) |>
      add_state("healthy", initial_prob = 1),
    'Name collision.*"healthy".*decision tree'
  )
})

test_that("tree name collides with state name (state added first)", {
  expect_error(
    define_model("markov") |>
      add_state("healthy", initial_prob = 1) |>
      add_tree_node("healthy", "root", parent = NA, formula = 1),
    "Name collision.*decision trees and states"
  )
})

test_that("tree name collides with summary name (tree added first)", {
  expect_error(
    define_model("markov") |>
      add_tree_node("total_cost", "root", parent = NA, formula = 1) |>
      add_summary("total_cost", type = "cost", values = "some_val"),
    'Name collision.*"total_cost".*decision tree'
  )
})

test_that("tree name collides with summary name (summary added first)", {
  expect_error(
    define_model("markov") |>
      add_summary("total_cost", type = "cost", values = "some_val") |>
      add_tree_node("total_cost", "root", parent = NA, formula = 1),
    "Name collision.*decision trees and summaries"
  )
})

test_that("tree name collides with reserved keyword", {
  expect_error(
    define_model("markov") |>
      add_tree_node("cycle", "root", parent = NA, formula = 1),
    "Name collision.*reserved keywords"
  )
})

test_that("validation-time detection of tree name collision", {
  # Build a model, then manually inject a collision
  model <- define_model("markov") |>
    add_tree_node("my_tree", "root", parent = NA, formula = 1)
  # Manually add a variable with the same name (bypassing builder check)
  model$variables <- tibble::tibble(
    name = "my_tree", formula = "1", display_name = "", description = "",
    strategy = "", group = "", source = "", sampling = ""
  )
  expect_error(
    normalize_and_validate_model(model, preserve_builder = FALSE),
    "Name collision.*decision trees and variables"
  )
})

test_that("non-colliding tree names work fine", {
  # This should not error
  model <- define_model("markov") |>
    add_variable("my_var", 100) |>
    add_tree_node("my_tree", "root", parent = NA, formula = 1) |>
    add_tree_node("my_tree", "left", parent = "root", formula = 0.5) |>
    add_tree_node("my_tree", "right", parent = "root", formula = "C")

  expect_false(is.null(model$trees))
  expect_equal(nrow(model$trees), 3)
})
