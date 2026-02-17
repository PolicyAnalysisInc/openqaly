context("DSA")

test_that("DSA data structures are initialized in define_model", {
  model <- define_model("markov")

  expect_true("dsa_parameters" %in% names(model))
  expect_s3_class(model$dsa_parameters, "dsa_parameters")
  expect_equal(length(model$dsa_parameters), 0)
})

test_that("add_dsa_variable adds variable specification", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = 0.01, high = 0.05)

  expect_equal(length(model$dsa_parameters), 1)
  expect_equal(model$dsa_parameters[[1]]$type, "variable")
  expect_equal(model$dsa_parameters[[1]]$name, "p_disease")
  # Low/high are now oq_formula objects
  expect_s3_class(model$dsa_parameters[[1]]$low, "oq_formula")
  expect_s3_class(model$dsa_parameters[[1]]$high, "oq_formula")
})

test_that("add_dsa_variable validates inputs", {
  model <- define_model("markov")

  # Note: With NSE, these expressions are stored as oq_formulas and validated during evaluation
  # So these should NOT error during model building
  model_ok <- add_dsa_variable(model, "p_disease", low = 0.05, high = 0.01)
  expect_equal(length(model_ok$dsa_parameters), 1)

  # String literals are also stored as oq_formulas
  model_ok2 <- add_dsa_variable(model, "p_disease", low = "not_numeric", high = 0.05)
  expect_equal(length(model_ok2$dsa_parameters), 1)
})

test_that("add_dsa_variable errors when group-specific variable used without specifying group", {
  # Create model with group-specific variable
  model <- define_model("markov") %>%
    add_variable("cost", 1000, group = "young") %>%
    add_variable("cost", 2000, group = "old")

  # Should error when trying to add DSA for group-specific variable without group

  expect_error(
    add_dsa_variable(model, "cost", low = 500, high = 1500),
    "defined for specific group"
  )

  # Should work when group is specified
  model_ok <- add_dsa_variable(model, "cost", low = 500, high = 1500, group = "young")
  expect_equal(length(model_ok$dsa_parameters), 1)
  expect_equal(model_ok$dsa_parameters[[1]]$group, "young")
})

test_that("add_dsa_variable allows non-group-specific variables without group", {
  # Create model with non-group-specific variable
  model <- define_model("markov") %>%
    add_variable("cost", 1000)

  # Should work without specifying group
  model_ok <- add_dsa_variable(model, "cost", low = 500, high = 1500)
  expect_equal(length(model_ok$dsa_parameters), 1)
  expect_equal(model_ok$dsa_parameters[[1]]$group, "")
})

test_that("add_dsa_variable errors when strategy-specific variable used without specifying strategy", {
  # Create model with strategy-specific variable
  model <- define_model("markov") %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_variable("c_treatment", 1000, strategy = "treatment")

  # Should error when trying to add DSA for strategy-specific variable without strategy
  expect_error(
    add_dsa_variable(model, "c_treatment", low = 500, high = 1500),
    "defined for specific strategy"
  )

  # Should work when strategy is specified
  model_ok <- add_dsa_variable(model, "c_treatment", low = 500, high = 1500, strategy = "treatment")
  expect_equal(length(model_ok$dsa_parameters), 1)
  expect_equal(model_ok$dsa_parameters[[1]]$strategy, "treatment")
})

test_that("add_dsa_variable allows non-strategy-specific variables without strategy", {
  # Create model with non-strategy-specific variable
  model <- define_model("markov") %>%
    add_variable("cost", 1000)

  # Should work without specifying strategy
  model_ok <- add_dsa_variable(model, "cost", low = 500, high = 1500)
  expect_equal(length(model_ok$dsa_parameters), 1)
  expect_equal(model_ok$dsa_parameters[[1]]$strategy, "")
})

test_that("add_dsa_variable warns and replaces duplicate parameters", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_dsa_variable("cost", low = 500, high = 1500)

  expect_warning(
    model2 <- add_dsa_variable(model, "cost", low = 800, high = 1200),
    "Replacing existing DSA specification"
  )

  # Should have exactly 1 DSA parameter
  expect_equal(length(model2$dsa_parameters), 1)

  # Should have the new bounds
  expect_equal(model2$dsa_parameters[[1]]$low$text, "800")
  expect_equal(model2$dsa_parameters[[1]]$high$text, "1200")
})

test_that("add_dsa_variable warns for duplicate with strategy specified", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000, strategy = "a") %>%
    add_variable("cost", 2000, strategy = "b") %>%
    add_dsa_variable("cost", low = 500, high = 1500, strategy = "a")

  expect_warning(
    model2 <- add_dsa_variable(model, "cost", low = 800, high = 1200, strategy = "a"),
    "Replacing existing DSA specification.*strategy: a"
  )

  expect_equal(length(model2$dsa_parameters), 1)
})

test_that("add_dsa_variable does not warn for different strategy", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000, strategy = "a") %>%
    add_variable("cost", 2000, strategy = "b") %>%
    add_dsa_variable("cost", low = 500, high = 1500, strategy = "a")

  # Adding for different strategy should not warn
  expect_no_warning(
    model2 <- add_dsa_variable(model, "cost", low = 800, high = 1200, strategy = "b")
  )

  expect_equal(length(model2$dsa_parameters), 2)
})

test_that("add_dsa_setting adds setting specification", {
  model <- define_model("markov") %>%
    set_settings(discount_cost = 3) %>%
    add_dsa_setting("discount_cost", low = 0, high = 5)

  expect_equal(length(model$dsa_parameters), 1)
  expect_equal(model$dsa_parameters[[1]]$type, "setting")
  expect_equal(model$dsa_parameters[[1]]$name, "discount_cost")
  expect_equal(model$dsa_parameters[[1]]$low, 0)
  expect_equal(model$dsa_parameters[[1]]$high, 5)
  expect_equal(model$dsa_parameters[[1]]$display_name, "discount_cost")
})

test_that("add_dsa_setting warns and replaces duplicate settings", {
  model <- define_model("markov") %>%
    set_settings(discount_cost = 3.5) %>%
    add_dsa_setting("discount_cost", low = 0, high = 5)

  expect_warning(
    model2 <- add_dsa_setting(model, "discount_cost", low = 1, high = 3),
    "Replacing existing DSA specification"
  )

  expect_equal(length(model2$dsa_parameters), 1)
  expect_equal(model2$dsa_parameters[[1]]$low, 1)
  expect_equal(model2$dsa_parameters[[1]]$high, 3)
})

test_that("validate_dsa_spec catches missing specifications", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", 0.1) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - 0.1 - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  # Parse model
  parsed_model <- parse_model(model)

  expect_error(
    validate_dsa_spec(parsed_model),
    "No DSA specifications found"
  )
})

test_that("validate_dsa_spec catches missing variables", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("nonexistent_var", low = 0.01, high = 0.05) %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", 0.1) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - 0.1 - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  parsed_model <- parse_model(model)

  expect_error(
    validate_dsa_spec(parsed_model),
    "nonexistent_var"
  )
})

test_that("validate_dsa_spec catches invalid settings", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_setting("invalid_setting", low = 0, high = 1) %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", 0.1) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - 0.1 - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  parsed_model <- parse_model(model)

  expect_error(
    validate_dsa_spec(parsed_model),
    "Invalid DSA setting name"
  )
})

test_that("build_dsa_segments and metadata generation work correctly", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = 0.01, high = 0.05) %>%
    add_dsa_setting("discount_cost", low = 0, high = 5) %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  parsed_model <- parse_model(model)

  # Build DSA segments
  segments <- openqaly:::build_dsa_segments(parsed_model)

  # Generate metadata
  dsa_metadata <- openqaly:::generate_dsa_metadata_from_segments(parsed_model, segments)

  # Should have 1 base + 2 variable runs + 2 setting runs = 5 total
  expect_equal(nrow(dsa_metadata), 5)

  # Check base case
  expect_equal(dsa_metadata$parameter[1], "base")
  expect_equal(dsa_metadata$variation[1], "base")

  # Check variable runs
  var_runs <- dsa_metadata %>% filter(parameter == "p_disease")
  expect_equal(nrow(var_runs), 2)
  expect_true("low" %in% var_runs$variation)
  expect_true("high" %in% var_runs$variation)

  # Check setting runs
  setting_runs <- dsa_metadata %>% filter(parameter == "discount_cost")
  expect_equal(nrow(setting_runs), 2)
  expect_true("low" %in% setting_runs$variation)
  expect_true("high" %in% setting_runs$variation)
})

# Integration test - skip if slow
test_that("run_dsa executes full DSA analysis", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = 0.01, high = 0.05,
                    display_name = "Disease Probability") %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  results <- run_dsa(model)

  # Check results structure
  expect_true("dsa_metadata" %in% names(results))
  expect_true("run_id" %in% names(results$segments))
  expect_true("run_id" %in% names(results$aggregated))

  # Check number of runs (1 base + 2 variable runs)
  expect_equal(nrow(results$dsa_metadata), 3)

  # Check that we have results for all runs
  expect_equal(length(unique(results$aggregated$run_id)), 3)
})

# NSE and bc keyword tests

test_that("add_dsa_variable accepts expressions with NSE", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = bc * 0.5, high = bc * 1.5)

  expect_equal(length(model$dsa_parameters), 1)
  expect_equal(model$dsa_parameters[[1]]$type, "variable")
  expect_equal(model$dsa_parameters[[1]]$name, "p_disease")
  # Check that formulas contain "bc"
  expect_match(as.character(model$dsa_parameters[[1]]$low), "bc")
  expect_match(as.character(model$dsa_parameters[[1]]$high), "bc")
})

test_that("add_dsa_variable backward compatible with literals", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = 0.01, high = 0.05)

  expect_equal(length(model$dsa_parameters), 1)
  expect_equal(model$dsa_parameters[[1]]$type, "variable")
  expect_equal(model$dsa_parameters[[1]]$name, "p_disease")
  expect_s3_class(model$dsa_parameters[[1]]$low, "oq_formula")
  expect_s3_class(model$dsa_parameters[[1]]$high, "oq_formula")
})

test_that("build_dsa_segments evaluates bc correctly", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = bc * 0.5, high = bc * 1.5) %>%
    set_settings(timeframe = 10, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "healthy", 1) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard")

  parsed_model <- parse_model(model)
  segments <- openqaly:::build_dsa_segments(parsed_model)

  # Should have: base case (run_id=1) + low (run_id=2) + high (run_id=3)
  expect_true(1 %in% segments$run_id)  # base
  expect_true(2 %in% segments$run_id)  # low
  expect_true(3 %in% segments$run_id)  # high

  # Check parameter overrides for low run
  low_seg <- segments %>% filter(run_id == 2) %>% slice(1)
  expect_equal(low_seg$parameter_overrides[[1]]$p_disease, 0.015)  # 0.03 * 0.5

  # Check parameter overrides for high run
  high_seg <- segments %>% filter(run_id == 3) %>% slice(1)
  expect_equal(high_seg$parameter_overrides[[1]]$p_disease, 0.045) # 0.03 * 1.5
})

test_that("build_dsa_segments can reference other variables", {
  model <- define_model("markov") %>%
    add_variable("cost_tx", 1000) %>%
    add_variable("cost_se", 100) %>%
    add_dsa_variable("cost_tx", low = bc - 2 * cost_se, high = bc + 2 * cost_se) %>%
    set_settings(timeframe = 10, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "healthy", 1) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard")

  parsed_model <- parse_model(model)
  segments <- openqaly:::build_dsa_segments(parsed_model)

  # Check parameter overrides for low run
  low_seg <- segments %>% filter(run_id == 2) %>% slice(1)
  expect_equal(low_seg$parameter_overrides[[1]]$cost_tx, 800)   # 1000 - 2*100

  # Check parameter overrides for high run
  high_seg <- segments %>% filter(run_id == 3) %>% slice(1)
  expect_equal(high_seg$parameter_overrides[[1]]$cost_tx, 1200) # 1000 + 2*100
})

test_that("build_dsa_segments validates low < high after evaluation", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = bc * 1.5, high = bc * 0.5) %>%  # Wrong order!
    set_settings(timeframe = 10, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "healthy", 1) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard")

  parsed_model <- parse_model(model)

  expect_error(
    openqaly:::build_dsa_segments(parsed_model),
    "low.*must be less than high"
  )
})

test_that("run_dsa works with bc expressions end-to-end", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_variable("p_disease", low = bc * 0.8, high = bc * 1.2,
                     display_name = "Disease Probability") %>%
    set_settings(timeframe = 5, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 0.8) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  results <- run_dsa(model)

  # Check that runs were created for base, low (0.024), high (0.036)
  expect_equal(nrow(results$dsa_metadata), 3)

  # Check that the evaluated values are correct
  expect_true("0.024" %in% results$dsa_metadata$override_value)
  expect_true("0.036" %in% results$dsa_metadata$override_value)

  # Check that we have results for all runs
  expect_equal(length(unique(results$aggregated$run_id)), 3)
})

test_that("run_dsa with other variable references works", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- define_model("markov") %>%
    add_variable("p_base", 0.05) %>%
    add_variable("p_min", 0.02) %>%
    add_variable("p_max", 0.10) %>%
    add_dsa_variable("p_base", low = p_min, high = p_max) %>%
    set_settings(timeframe = 5, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", p_base) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_base - 0.01) %>%
    add_transition("sick", "sick", 0.8) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  results <- run_dsa(model)

  # Check that runs were created with correct values (0.02 and 0.10)
  expect_equal(nrow(results$dsa_metadata), 3)
  expect_true("0.02" %in% results$dsa_metadata$override_value)
  expect_true("0.1" %in% results$dsa_metadata$override_value)
})

# Timeframe DSA tests
test_that("DSA timeframe override works for Markov models", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_dsa_setting("timeframe", low = 5, high = 20) %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "months") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 0.8) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("standard") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly")

  results <- run_dsa(model)

  # Check that we have base + low + high = 3 runs
  expect_equal(nrow(results$dsa_metadata), 3)

  # Check that trace lengths differ based on timeframe
  # Base case: 10 years * 12 months = 120 cycles -> trace has 121 rows (0-120)
  # Low: 5 years * 12 months = 60 cycles -> trace has 61 rows (0-60)
  # High: 20 years * 12 months = 240 cycles -> trace has 241 rows (0-240)

  base_trace <- results$segments %>% filter(run_id == 1) %>% pull(collapsed_trace) %>% .[[1]]
  low_trace <- results$segments %>% filter(run_id == 2) %>% pull(collapsed_trace) %>% .[[1]]
  high_trace <- results$segments %>% filter(run_id == 3) %>% pull(collapsed_trace) %>% .[[1]]

  expect_equal(nrow(base_trace), 121)  # 10 years
  expect_equal(nrow(low_trace), 61)    # 5 years
  expect_equal(nrow(high_trace), 241)  # 20 years

  # Check that QALY totals differ (longer timeframe = more QALYs)
  base_qalys <- results$aggregated %>% filter(run_id == 1, strategy == "standard") %>%
    pull(summaries) %>% .[[1]] %>% filter(summary == "total_qalys") %>% pull(amount)
  low_qalys <- results$aggregated %>% filter(run_id == 2, strategy == "standard") %>%
    pull(summaries) %>% .[[1]] %>% filter(summary == "total_qalys") %>% pull(amount)
  high_qalys <- results$aggregated %>% filter(run_id == 3, strategy == "standard") %>%
    pull(summaries) %>% .[[1]] %>% filter(summary == "total_qalys") %>% pull(amount)

  expect_true(low_qalys < base_qalys)
  expect_true(base_qalys < high_qalys)
})

test_that("DSA timeframe override works for PSM models", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # Create a simple PSM model
  model <- define_model("psm") %>%
    set_settings(timeframe = 10, timeframe_unit = "years",
                cycle_length = 1, cycle_length_unit = "months") %>%
    add_state("pfs", display_name = "Progression Free") %>%
    add_state("progressed", display_name = "Progressed") %>%
    add_state("dead", display_name = "Dead") %>%
    add_variable("pfs_dist", define_surv_param(dist = "exp", rate = 0.1)) %>%
    add_variable("os_dist", define_surv_param(dist = "exp", rate = 0.05)) %>%
    add_psm_transition("PFS", "months", pfs_dist) %>%
    add_psm_transition("OS", "months", os_dist) %>%
    add_value("qaly_pfs", 0.9, state = "pfs") %>%
    add_value("qaly_prog", 0.6, state = "progressed") %>%
    add_value("cost_pfs", 1000, state = "pfs") %>%
    add_value("cost_prog", 2000, state = "progressed") %>%
    add_summary("total_qalys", "qaly_pfs,qaly_prog") %>%
    add_summary("total_costs", "cost_pfs,cost_prog") %>%
    add_strategy("standard") %>%
    add_dsa_setting("timeframe", low = 5, high = 20)

  results <- run_dsa(model)

  # Check that we have base + low + high = 3 runs
  expect_equal(nrow(results$dsa_metadata), 3)

  # Check that trace lengths differ
  base_trace <- results$segments %>% filter(run_id == 1) %>% pull(collapsed_trace) %>% .[[1]]
  low_trace <- results$segments %>% filter(run_id == 2) %>% pull(collapsed_trace) %>% .[[1]]
  high_trace <- results$segments %>% filter(run_id == 3) %>% pull(collapsed_trace) %>% .[[1]]

  expect_equal(nrow(base_trace), 121)  # 10 years
  expect_equal(nrow(low_trace), 61)    # 5 years
  expect_equal(nrow(high_trace), 241)  # 20 years

  # Check that QALY totals differ
  base_qalys <- results$aggregated %>% filter(run_id == 1, strategy == "standard") %>%
    pull(summaries) %>% .[[1]] %>% filter(summary == "total_qalys") %>% pull(amount) %>% sum()
  low_qalys <- results$aggregated %>% filter(run_id == 2, strategy == "standard") %>%
    pull(summaries) %>% .[[1]] %>% filter(summary == "total_qalys") %>% pull(amount) %>% sum()
  high_qalys <- results$aggregated %>% filter(run_id == 3, strategy == "standard") %>%
    pull(summaries) %>% .[[1]] %>% filter(summary == "total_qalys") %>% pull(amount) %>% sum()

  expect_true(low_qalys < base_qalys)
  expect_true(base_qalys < high_qalys)
})
