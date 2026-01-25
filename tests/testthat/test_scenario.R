context("Scenario Analysis")

# ============================================================================
# Model Builder Tests
# ============================================================================

test_that("scenarios list is initialized in define_model", {
  model <- define_model("markov")

  expect_true("scenarios" %in% names(model))
  expect_type(model$scenarios, "list")
  expect_equal(length(model$scenarios), 0)
})

test_that("add_scenario creates scenario slot", {
  model <- define_model("markov") %>%
    add_scenario("Optimistic", description = "Best case assumptions")

  expect_equal(length(model$scenarios), 1)
  expect_equal(model$scenarios[[1]]$name, "Optimistic")
  expect_equal(model$scenarios[[1]]$description, "Best case assumptions")
  expect_type(model$scenarios[[1]]$variable_overrides, "list")
  expect_type(model$scenarios[[1]]$setting_overrides, "list")
})

test_that("add_scenario errors on empty name", {
  model <- define_model("markov")

  expect_error(
    add_scenario(model, ""),
    "non-empty character string"
  )
})

test_that("add_scenario errors on 'Base Case' name", {
  model <- define_model("markov")

  expect_error(
    add_scenario(model, "Base Case"),
    "reserved scenario name"
  )

  # Case insensitive
  expect_error(
    add_scenario(model, "base case"),
    "reserved scenario name"
  )
})

test_that("add_scenario errors on duplicate names", {
  model <- define_model("markov") %>%
    add_scenario("Optimistic")

  expect_error(
    add_scenario(model, "Optimistic"),
    "already exists"
  )
})

test_that("add_scenario_variable adds override to scenario", {
  model <- define_model("markov") %>%
    add_scenario("Optimistic") %>%
    add_scenario_variable("Optimistic", "efficacy", 0.95)

  expect_equal(length(model$scenarios[[1]]$variable_overrides), 1)
  expect_equal(model$scenarios[[1]]$variable_overrides[[1]]$name, "efficacy")
  expect_equal(model$scenarios[[1]]$variable_overrides[[1]]$value, 0.95)
})

test_that("add_scenario_variable errors for nonexistent scenario", {
  model <- define_model("markov")

  expect_error(
    add_scenario_variable(model, "NonExistent", "var", 10),
    "not found"
  )
})

test_that("add_scenario_variable supports NSE for expressions", {
  model <- define_model("markov") %>%
    add_scenario("Optimistic") %>%
    add_scenario_variable("Optimistic", "cost", base_cost * 0.8)

  expect_s3_class(model$scenarios[[1]]$variable_overrides[[1]]$value, "oq_formula")
})

test_that("add_scenario_setting adds setting override to scenario", {
  model <- define_model("markov") %>%
    add_scenario("Extended") %>%
    add_scenario_setting("Extended", "timeframe", 30)

  expect_equal(length(model$scenarios[[1]]$setting_overrides), 1)
  expect_equal(model$scenarios[[1]]$setting_overrides[[1]]$name, "timeframe")
  expect_equal(model$scenarios[[1]]$setting_overrides[[1]]$value, 30)
})

test_that("add_scenario_setting errors for nonexistent scenario", {
  model <- define_model("markov")

  expect_error(
    add_scenario_setting(model, "NonExistent", "timeframe", 30),
    "not found"
  )
})

test_that("multiple scenarios can be defined", {
  model <- define_model("markov") %>%
    add_scenario("Optimistic", description = "Best case") %>%
    add_scenario_variable("Optimistic", "efficacy", 0.95) %>%
    add_scenario("Pessimistic", description = "Worst case") %>%
    add_scenario_variable("Pessimistic", "efficacy", 0.70)

  expect_equal(length(model$scenarios), 2)
  expect_equal(model$scenarios[[1]]$name, "Optimistic")
  expect_equal(model$scenarios[[2]]$name, "Pessimistic")
})

# ============================================================================
# Validation Tests
# ============================================================================

test_that("validate_scenario_spec catches missing scenarios", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000)

  # Finalize without adding scenarios
  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_scenario_spec(parsed),
    "No scenarios defined"
  )
})

test_that("validate_scenario_spec catches invalid variable names", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_scenario("Test") %>%
    add_scenario_variable("Test", "nonexistent_var", 500)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_scenario_spec(parsed),
    "not found in model variables"
  )
})

test_that("validate_scenario_spec catches invalid setting names", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    set_settings(timeframe = 10, timeframe_unit = "years") %>%
    add_scenario("Test") %>%
    add_scenario_setting("Test", "invalid_setting", 5)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_scenario_spec(parsed),
    "invalid setting name"
  )
})

# ============================================================================
# Segment Building Tests
# ============================================================================

test_that("build_scenario_segments includes Base Case", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years"
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "dead", 0.05) %>%
    add_strategy("control") %>%
    add_value("total_cost", cost, type = "cost") %>%
    add_summary("total_cost", "total_cost", type = "cost") %>%
    add_scenario("Test") %>%
    add_scenario_variable("Test", "cost", 2000)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  segments <- build_scenario_segments(parsed)

  # Should have scenario_id 1 (Base Case) and 2 (Test)
  expect_true("scenario_id" %in% names(segments))
  expect_true(1 %in% segments$scenario_id)
  expect_true(2 %in% segments$scenario_id)

  # Base case should have empty overrides
  base_segments <- segments %>% filter(scenario_id == 1)
  expect_true(all(sapply(base_segments$parameter_overrides, length) == 0))
  expect_true(all(sapply(base_segments$setting_overrides, length) == 0))

  # Test scenario should have parameter override
  test_segments <- segments %>% filter(scenario_id == 2)
  expect_true(any(sapply(test_segments$parameter_overrides, function(x) "cost" %in% names(x))))
})

# ============================================================================
# Metadata Generation Tests
# ============================================================================

test_that("generate_scenario_metadata creates correct structure", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years"
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "dead", 0.05) %>%
    add_strategy("control") %>%
    add_value("total_cost", cost, type = "cost") %>%
    add_summary("total_cost", "total_cost", type = "cost") %>%
    add_scenario("Optimistic", description = "Best case") %>%
    add_scenario_variable("Optimistic", "cost", 500)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  segments <- build_scenario_segments(parsed)
  metadata <- generate_scenario_metadata(parsed, segments)

  expect_true("scenario_id" %in% names(metadata))
  expect_true("scenario_name" %in% names(metadata))
  expect_true("scenario_description" %in% names(metadata))

  # Check Base Case
  base_row <- metadata %>% filter(scenario_id == 1)
  expect_equal(base_row$scenario_name, "Base Case")

  # Check user scenario
  opt_row <- metadata %>% filter(scenario_id == 2)
  expect_equal(opt_row$scenario_name, "Optimistic")
  expect_equal(opt_row$scenario_description, "Best case")
})
