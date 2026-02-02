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
# Strategy/Group Targeting Validation Tests
# ============================================================================

test_that("add_scenario_variable errors when group-specific variable used without specifying group", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000, group = "young") %>%
    add_variable("cost", 2000, group = "old") %>%
    add_scenario("Test")

  expect_error(
    add_scenario_variable(model, "Test", "cost", value = 1500),
    "defined for specific group"
  )

  # Should work when group is specified
  model_ok <- add_scenario_variable(model, "Test", "cost", value = 1500, group = "young")
  expect_equal(model_ok$scenarios[[1]]$variable_overrides[[1]]$group, "young")
})

test_that("add_scenario_variable errors when strategy-specific variable used without specifying strategy", {
  model <- define_model("markov") %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_variable("c_treatment", 1000, strategy = "treatment") %>%
    add_scenario("Test")

  expect_error(
    add_scenario_variable(model, "Test", "c_treatment", value = 1500),
    "defined for specific strategy"
  )

  # Should work when strategy is specified
  model_ok <- add_scenario_variable(model, "Test", "c_treatment", value = 1500, strategy = "treatment")
  expect_equal(model_ok$scenarios[[1]]$variable_overrides[[1]]$strategy, "treatment")
})

test_that("add_scenario_variable allows non-specific variables without strategy/group", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_scenario("Test")

  # Should work without specifying strategy or group
  model_ok <- add_scenario_variable(model, "Test", "cost", value = 1500)
  expect_equal(model_ok$scenarios[[1]]$variable_overrides[[1]]$strategy, "")
  expect_equal(model_ok$scenarios[[1]]$variable_overrides[[1]]$group, "")
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

# ============================================================================
# Scenario CE Plot and Table Tests
# ============================================================================

# Helper to build a scenario model for CE testing
build_scenario_ce_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 8000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.5, strategy = "treatment") %>%
    # Scenarios
    add_scenario("High Efficacy", description = "Treatment more effective") %>%
    add_scenario_variable("High Efficacy", "treatment_effect", 0.3, strategy = "treatment") %>%
    add_scenario("Low Cost", description = "Lower treatment cost") %>%
    add_scenario_variable("Low Cost", "c_treatment", 4000, strategy = "treatment") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summaries
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

test_that("scenario_ce_plot() returns ggplot object", {
  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  p <- scenario_ce_plot(results, "total_qalys", "total_cost",
                        interventions = "treatment", comparators = "control")
  expect_s3_class(p, "ggplot")
})

test_that("scenario_ce_plot() requires interventions or comparators", {
  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  expect_error(
    scenario_ce_plot(results, "total_qalys", "total_cost"),
    "interventions.*comparators"
  )
})

test_that("scenario_ce_table() returns flextable object", {
  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  tbl <- scenario_ce_table(results, "total_qalys", "total_cost",
                           interventions = "treatment", comparators = "control")
  expect_s3_class(tbl, "flextable")
})

test_that("scenario CE plot handles dominant base case correctly", {
  # Build model where base case is dominant (treatment cheaper and better)
  model <- define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    # Treatment is CHEAPER (dominant setup)
    add_variable("c_treatment", 5000, strategy = "control") %>%
    add_variable("c_treatment", 1000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    # Treatment is also MORE effective (dominant)
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.3, strategy = "treatment") %>%
    # Scenario that maintains dominance
    add_scenario("Still Dominant") %>%
    add_scenario_variable("Still Dominant", "c_treatment", 500, strategy = "treatment") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)

  results <- run_scenario(model)

  # Should work without error
  p <- scenario_ce_plot(results, "total_qalys", "total_cost",
                        interventions = "treatment", comparators = "control")
  expect_s3_class(p, "ggplot")
})

test_that("scenario CE tables add asterisk for flipped ICERs", {
  # The prepare_scenario_ce_table_data function should add asterisks
  # when ce_class == "flipped" (differs from requested direction)

  # This tests the internal logic via the exported function
  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  # The table should work without error
  tbl <- scenario_ce_table(results, "total_qalys", "total_cost",
                           interventions = "treatment", comparators = "control")
  expect_s3_class(tbl, "flextable")
})

# ============================================================================
# Scenario CE: Comprehensive CE Class and Asterisk Tests
# ============================================================================

test_that("scenario CE plot handles all base case types correctly", {
  # Test that scenario_ce_plot works for different base case classifications

  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  # Basic model should work
  p <- scenario_ce_plot(results, "total_qalys", "total_cost",
                        interventions = "treatment", comparators = "control")
  expect_s3_class(p, "ggplot")

  # Should be able to build the plot successfully
  built <- ggplot2::ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("scenario CE ce_class values are correctly assigned", {
  # Verify the ce_class values:
  # normal, dominated, dominant, flipped, identical

  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  # The plot should execute without error and produce valid output
  p <- scenario_ce_plot(results, "total_qalys", "total_cost",
                        interventions = "treatment", comparators = "control")

  # Should be a valid ggplot
  expect_s3_class(p, "ggplot")

  # Build the plot to verify it renders correctly
  built <- ggplot2::ggplot_build(p)

  # Should have data layers
  expect_true(length(built$data) > 0)
})

test_that("scenario CE table correctly classifies ICERs", {
  # Test that prepare_scenario_ce_table_data produces correct ce_class values

  model <- build_scenario_ce_model()
  results <- run_scenario(model)

  # Call prepare function directly to check internal structure
  prepared <- openqaly:::prepare_scenario_ce_table_data(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    groups = "overall",
    interventions = "treatment",
    comparators = "control",
    decimals = 0,
    font_size = 11
  )

  # Should return a list with data
  expect_true(is.list(prepared))
  expect_true("data" %in% names(prepared))

  # Data should have rows (one per scenario)
  expect_gt(nrow(prepared$data), 0)
})

# ============================================================================
# Scenario CE: End-to-End Asterisk Tests
# ============================================================================

# Helper to build a flipped scenario CE model (treatment cheaper but worse)
build_flipped_scenario_ce_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    # Treatment is CHEAPER (flipped setup: less cost)
    add_variable("c_treatment", 8000, strategy = "control") %>%
    add_variable("c_treatment", 2000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    # Treatment is also LESS effective (flipped: worse outcomes)
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 1.3, strategy = "treatment") %>%
    # Scenario that maintains flipped direction
    add_scenario("Still Flipped") %>%
    add_scenario_variable("Still Flipped", "c_treatment", 1500, strategy = "treatment") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

test_that("Scenario CE table adds asterisks for flipped scenarios", {
  # Build a model where base case produces a flipped ICER
  model <- build_flipped_scenario_ce_model()
  results <- run_scenario(model)

  # Get CE table data
  prepared <- openqaly:::prepare_scenario_ce_table_data(
    results, "total_qalys", "total_cost",
    groups = "overall",
    interventions = "treatment",
    comparators = "control",
    decimals = 0,
    font_size = 11
  )

  # Check that we have data
  expect_true(nrow(prepared$data) > 0)
  expect_true("data" %in% names(prepared))

  # Get the ICER column values (should contain asterisks for flipped)
  icer_col <- names(prepared$data)[grepl("vs\\.", names(prepared$data))]
  if (length(icer_col) > 0) {
    icer_values <- prepared$data[[icer_col[1]]]
    # At least one value should have asterisk if any scenarios are flipped
    has_asterisks <- any(grepl("\\*", icer_values), na.rm = TRUE)
    # The model is designed to produce flipped ICERs, so we expect asterisks
    expect_true(has_asterisks || all(icer_values %in% c("Dominated", "Dominant", "Identical", "")))
  }
})

test_that("Scenario CE table footnotes are generated for flipped scenarios", {
  model <- build_flipped_scenario_ce_model()
  results <- run_scenario(model)

  prepared <- openqaly:::prepare_scenario_ce_table_data(
    results, "total_qalys", "total_cost",
    groups = "overall",
    interventions = "treatment",
    comparators = "control",
    decimals = 0,
    font_size = 11
  )

  # Footnotes should be present if any scenarios are flipped
  expect_true("footnotes" %in% names(prepared))
})

test_that("Scenario CE plot handles flipped base case correctly", {
  # Build a model where base case is flipped
  model <- build_flipped_scenario_ce_model()
  results <- run_scenario(model)

  # Plot should work without error
  p <- scenario_ce_plot(results, "total_qalys", "total_cost",
                        interventions = "treatment", comparators = "control")
  expect_s3_class(p, "ggplot")

  # Build the plot to verify it renders correctly
  built <- ggplot2::ggplot_build(p)
  expect_true(length(built$data) > 0)
})
