context("Threshold Analysis")

# ============================================================================
# Helper: Build a minimal model for threshold analysis tests
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
# Builder API Tests
# ============================================================================

test_that("threshold_analyses list is initialized in define_model", {
  model <- define_model("markov")
  expect_true("threshold_analyses" %in% names(model))
  expect_equal(length(model$threshold_analyses), 0)
})

test_that("add_threshold_analysis adds analysis with outcomes condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("QALY Target", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "treatment", target_value = 5))

  expect_equal(length(model$threshold_analyses), 1)
  a <- model$threshold_analyses[[1]]
  expect_equal(a$name, "QALY Target")
  expect_equal(a$variable, "p_disease")
  expect_equal(a$lower, 0)
  expect_equal(a$upper, 1)
  expect_equal(a$condition$output, "outcomes")
  expect_equal(a$condition$summary, "total_qalys")
  expect_equal(a$condition$type, "absolute")
  expect_equal(a$condition$strategy, "treatment")
  expect_equal(a$condition$target_value, 5)
})

test_that("add_threshold_analysis adds analysis with costs condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Cost Target", "cost_base", 0, 50000,
      condition = threshold_condition_costs(
        value = "cost_drug", type = "absolute",
        strategy = "treatment", discounted = TRUE, target_value = 20000))

  a <- model$threshold_analyses[[1]]
  expect_equal(a$condition$output, "costs")
  expect_equal(a$condition$value, "cost_drug")
})

test_that("add_threshold_analysis adds analysis with NMB condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("NMB Threshold", "cost_base", 0, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  a <- model$threshold_analyses[[1]]
  expect_equal(a$condition$output, "nmb")
  expect_equal(a$condition$health_summary, "total_qalys")
  expect_equal(a$condition$cost_summary, "total_cost")
  expect_equal(a$condition$referent, "treatment")
  expect_equal(a$condition$comparator, "base")
})

test_that("add_threshold_analysis adds analysis with CE condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("CE Threshold", "cost_base", 0, 50000,
      condition = threshold_condition_ce(
        "total_qalys", "total_cost", "treatment", "base"))

  a <- model$threshold_analyses[[1]]
  expect_equal(a$condition$output, "ce")
})

# ============================================================================
# Validation Tests
# ============================================================================

test_that("add_threshold_analysis validates model type", {
  expect_error(
    add_threshold_analysis(list(), "test", "x", 0, 1,
      condition = threshold_condition_outcomes(summary = "s", strategy = "s")),
    "oq_model_builder"
  )
})

test_that("add_threshold_analysis validates name", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(summary = "s", strategy = "s")),
    "non-empty"
  )
})

test_that("add_threshold_analysis validates bounds", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 1, 0,
      condition = threshold_condition_outcomes(summary = "s", strategy = "s")),
    "lower must be less than upper"
  )
})

test_that("add_threshold_analysis rejects VBP output type", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "vbp", comparator = "base", target_value = 0)),
    "VBP output type is not yet supported"
  )
})

test_that("add_threshold_analysis validates outcomes condition fields", {
  model <- build_threshold_test_model()

  # Neither summary nor value
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(type = "absolute", strategy = "base")),
    "must specify either 'summary' or 'value'"
  )

  # Missing strategy for absolute
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(summary = "total_qalys", type = "absolute")),
    "requires 'strategy'"
  )

  # Missing referent/comparator for difference
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(summary = "total_qalys", type = "difference")),
    "requires 'referent'"
  )
})

test_that("add_threshold_analysis validates NMB condition fields", {
  model <- build_threshold_test_model()

  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "nmb", health_summary = "total_qalys",
                       referent = "treatment", comparator = "base")),
    "requires: cost_summary"
  )
})

test_that("add_threshold_analysis warns and replaces duplicate names", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("test", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", strategy = "base", target_value = 5))

  expect_warning(
    model2 <- add_threshold_analysis(model, "test", "p_disease", 0, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", strategy = "base", target_value = 10)),
    "already exists"
  )

  expect_equal(length(model2$threshold_analyses), 1)
  expect_equal(model2$threshold_analyses[[1]]$upper, 0.5)
})

test_that("add_threshold_analysis validates variable targeting", {
  # Build a model with a strategy-specific variable
  model <- define_model("markov") %>%
    add_strategy("base") %>%
    add_strategy("treatment") %>%
    add_variable("c_treat", 5000, strategy = "treatment")

  # Should error when trying without strategy
  expect_error(
    add_threshold_analysis(model, "test", "c_treat", 0, 50000,
      condition = threshold_condition_costs(
        summary = "total_cost", strategy = "treatment")),
    "defined for specific strategy"
  )
})

# ============================================================================
# Format Conversion Tests
# ============================================================================

test_that("flatten_threshold_analysis works for outcomes absolute", {
  analysis <- list(
    name = "Test", variable = "p", variable_strategy = "s1",
    variable_group = "", lower = 0, upper = 1, active = TRUE,
    condition = list(output = "outcomes", summary = "total_qalys",
                     type = "absolute", strategy = "s1",
                     discounted = TRUE, target_value = 5)
  )

  flat <- openqaly:::flatten_threshold_analysis(analysis)
  expect_equal(flat$name, "Test")
  expect_equal(flat$output, "outcomes")
  expect_equal(flat$summary, "total_qalys")
  expect_equal(flat$type, "absolute")
  expect_equal(flat$strategy, "s1")
  expect_equal(flat$target_value, 5)
})

test_that("nest_threshold_analysis works for outcomes absolute", {
  flat <- list(
    name = "Test", variable = "p", variable_strategy = "s1",
    variable_group = "", lower = 0, upper = 1, active = TRUE,
    output = "outcomes", summary = "total_qalys", type = "absolute",
    strategy = "s1", discounted = TRUE, target_value = 5
  )

  nested <- openqaly:::nest_threshold_analysis(flat)
  expect_equal(nested$name, "Test")
  expect_equal(nested$condition$output, "outcomes")
  expect_equal(nested$condition$summary, "total_qalys")
  expect_equal(nested$condition$strategy, "s1")
  expect_equal(nested$condition$target_value, 5)
})

test_that("flatten/nest round-trip preserves all output types", {
  analyses <- list(
    # Outcomes absolute with summary
    list(name = "A1", variable = "p", variable_strategy = "", variable_group = "",
         lower = 0, upper = 1, active = TRUE,
         condition = list(output = "outcomes", summary = "total_qalys",
                          type = "absolute", strategy = "s1",
                          discounted = TRUE, target_value = 5)),
    # Outcomes difference with value
    list(name = "A2", variable = "p", variable_strategy = "", variable_group = "",
         lower = 0, upper = 1, active = TRUE,
         condition = list(output = "outcomes", value = "qaly_sick",
                          type = "difference", referent = "s1", comparator = "s2",
                          discounted = FALSE, target_value = 0.5)),
    # Costs absolute
    list(name = "A3", variable = "c", variable_strategy = "treatment", variable_group = "",
         lower = 0, upper = 50000, active = TRUE,
         condition = list(output = "costs", summary = "total_cost",
                          type = "absolute", strategy = "treatment",
                          discounted = TRUE, target_value = 20000)),
    # NMB
    list(name = "A4", variable = "c", variable_strategy = "", variable_group = "",
         lower = 0, upper = 50000, active = TRUE,
         condition = list(output = "nmb", health_summary = "total_qalys",
                          cost_summary = "total_cost", referent = "s1", comparator = "s2",
                          discounted = TRUE, target_value = 0)),
    # CE
    list(name = "A5", variable = "c", variable_strategy = "", variable_group = "",
         lower = 0, upper = 50000, active = FALSE,
         condition = list(output = "ce", health_summary = "total_qalys",
                          cost_summary = "total_cost", referent = "s1", comparator = "s2",
                          discounted = TRUE))
  )

  for (a in analyses) {
    flat <- openqaly:::flatten_threshold_analysis(a)
    nested <- openqaly:::nest_threshold_analysis(flat)
    expect_equal(nested$name, a$name, info = paste("Name mismatch for", a$name))
    expect_equal(nested$variable, a$variable, info = paste("Variable mismatch for", a$name))
    expect_equal(nested$lower, a$lower, info = paste("Lower mismatch for", a$name))
    expect_equal(nested$upper, a$upper, info = paste("Upper mismatch for", a$name))
    expect_equal(nested$active, a$active, info = paste("Active mismatch for", a$name))
    expect_equal(nested$condition$output, a$condition$output,
                 info = paste("Output mismatch for", a$name))
  }
})

# ============================================================================
# Serialization Round-Trip Tests
# ============================================================================

test_that("JSON round-trip preserves threshold analyses", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("QALY Target", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "treatment", target_value = 5)) %>%
    add_threshold_analysis("NMB Threshold", "cost_base", 0, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  # Write JSON (returns string)
  json_string <- write_model_json(model_norm)

  # Read JSON
  model_back <- read_model_json(json_string)

  expect_equal(length(model_back$threshold_analyses), 2)
  expect_equal(model_back$threshold_analyses[[1]]$name, "QALY Target")
  expect_equal(model_back$threshold_analyses[[1]]$condition$output, "outcomes")
  expect_equal(model_back$threshold_analyses[[1]]$condition$summary, "total_qalys")
  expect_equal(model_back$threshold_analyses[[2]]$name, "NMB Threshold")
  expect_equal(model_back$threshold_analyses[[2]]$condition$output, "nmb")
})

test_that("Excel round-trip preserves threshold analyses", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Cost Target", "cost_base", 0, 50000,
      condition = threshold_condition_costs(
        value = "cost_drug", type = "absolute",
        strategy = "treatment", target_value = 20000)) %>%
    add_threshold_analysis("CE Threshold", "cost_base", 0, 50000,
      condition = threshold_condition_ce(
        "total_qalys", "total_cost", "treatment", "base"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  # write_model_excel expects a directory path
  tmp_dir <- tempdir()
  on.exit(unlink(file.path(tmp_dir, "model.xlsx")), add = TRUE)
  write_model_excel(model_norm, tmp_dir)
  model_back <- read_model(tmp_dir)

  expect_equal(length(model_back$threshold_analyses), 2)
  expect_equal(model_back$threshold_analyses[[1]]$name, "Cost Target")
  expect_equal(model_back$threshold_analyses[[1]]$condition$output, "costs")
  expect_equal(model_back$threshold_analyses[[1]]$condition$value, "cost_drug")
  expect_equal(model_back$threshold_analyses[[2]]$name, "CE Threshold")
  expect_equal(model_back$threshold_analyses[[2]]$condition$output, "ce")
})

test_that("YAML round-trip preserves threshold analyses", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Outcomes Diff", "p_disease", 0, 1,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "difference",
        referent = "treatment", comparator = "base",
        discounted = FALSE, target_value = 0.5))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  write_model_yaml(model_norm, tmp)
  model_back <- read_model_yaml(tmp)

  expect_equal(length(model_back$threshold_analyses), 1)
  a <- model_back$threshold_analyses[[1]]
  expect_equal(a$name, "Outcomes Diff")
  expect_equal(a$condition$output, "outcomes")
  expect_equal(a$condition$type, "difference")
  expect_equal(a$condition$referent, "treatment")
  expect_equal(a$condition$comparator, "base")
})

test_that("R code generation includes threshold analyses", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("NMB Threshold", "cost_base", 0, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  code <- model_to_r_code(model_norm)

  expect_true(any(grepl("add_threshold_analysis", code)))
  expect_true(any(grepl("threshold_condition_nmb", code)))
  expect_true(any(grepl("NMB Threshold", code)))
})

# ============================================================================
# Runtime Tests
# ============================================================================

test_that("validate_threshold_spec errors when no analyses defined", {
  model <- build_threshold_test_model()
  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- openqaly:::parse_model(model_norm)
  expect_error(
    openqaly:::validate_threshold_spec(parsed),
    "No threshold analyses found"
  )
})

test_that("run_threshold finds threshold for absolute outcomes", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("P Disease", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", discounted = TRUE, target_value = 7))

  result <- run_threshold(model)

  expect_true("threshold_values" %in% names(result))
  expect_true("root_finder_history" %in% names(result))
  expect_equal(nrow(result$threshold_values), 1)
  expect_equal(result$threshold_values$name, "P Disease")
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))

  # Verify the threshold is within bounds
  expect_true(result$threshold_values$value >= 0.001)
  expect_true(result$threshold_values$value <= 0.5)
})

test_that("run_threshold finds threshold for absolute costs", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Cost Target", "cost_base", 100, 20000,
      condition = threshold_condition_costs(
        summary = "total_cost", type = "absolute",
        strategy = "treatment", discounted = TRUE, target_value = 30000))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))
})

test_that("run_threshold finds threshold for difference outcomes", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Outcome Diff", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "difference",
        referent = "treatment", comparator = "base",
        discounted = TRUE, target_value = 0))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
})

test_that("run_threshold finds threshold for NMB", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("NMB Zero", "cost_base", 100, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
  # NMB = 0 means treatment becomes cost-effective at this cost
  expect_false(is.na(result$threshold_values$value))
})

test_that("run_threshold filters inactive analyses", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Active", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 7)) %>%
    add_threshold_analysis("Inactive", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 5),
      active = FALSE)

  result <- run_threshold(model)

  # Only active analysis should run
  expect_equal(nrow(result$threshold_values), 1)
  expect_equal(result$threshold_values$name, "Active")
})

test_that("run_threshold history records iterations", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Test", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 7))

  result <- run_threshold(model)

  expect_true(nrow(result$root_finder_history) > 0)
  expect_true(all(c("name", "variable", "iteration", "input", "output", "goal", "diff")
                   %in% names(result$root_finder_history)))
})

# ============================================================================
# Mathematical Correctness Tests
# ============================================================================

# Helper: like build_threshold_test_model but with cost_drug typed as "cost"
build_threshold_cost_model <- function() {
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
    add_value("qaly", 1, state = "healthy", type = "outcome") %>%
    add_value("qaly", 0.5, state = "sick", type = "outcome") %>%
    add_value("qaly", 0, state = "dead", type = "outcome") %>%
    add_value("cost_drug", cost_base, state = "healthy", type = "cost") %>%
    add_value("cost_drug", cost_base * 0.5, state = "sick", type = "cost") %>%
    add_value("cost_drug", 0, state = "dead", type = "cost") %>%
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000) %>%
    add_summary("total_cost", "cost_drug", type = "cost")
}

# Helper: re-run model at the found threshold value and return results
verify_threshold_at_value <- function(model, threshold_result, analysis_name) {
  tv <- threshold_result$threshold_values
  row <- tv[tv$name == analysis_name, ]
  variable_name <- row$variable
  threshold_value <- row$value

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  model_norm$variables <- model_norm$variables %>%
    dplyr::mutate(formula = ifelse(name == variable_name, as.character(threshold_value), formula))

  run_model(model_norm)
}

# Helper: re-run model at the found threshold value with group-aware replacement
verify_threshold_at_value_grouped <- function(model, threshold_result, analysis_name,
                                               variable_group = NULL) {
  tv <- threshold_result$threshold_values
  row <- tv[tv$name == analysis_name, ]
  variable_name <- row$variable
  threshold_value <- row$value

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  if (!is.null(variable_group) && variable_group != "") {
    model_norm$variables <- model_norm$variables %>%
      dplyr::mutate(formula = ifelse(
        name == variable_name & group == variable_group,
        as.character(threshold_value), formula))
  } else {
    model_norm$variables <- model_norm$variables %>%
      dplyr::mutate(formula = ifelse(
        name == variable_name, as.character(threshold_value), formula))
  }
  run_model(model_norm)
}

test_that("threshold for absolute outcomes is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("P Disease", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", discounted = TRUE, target_value = 7))

  result <- run_threshold(model)
  results <- verify_threshold_at_value(model, result, "P Disease")

  total <- get_summaries(results,
    summaries = "total_qalys", strategies = "base",
    value_type = "outcome", discounted = TRUE,
    use_display_names = FALSE) %>%
    dplyr::pull(amount) %>%
    sum()

  expect_true(abs(total - 7) < 1e-3,
    info = sprintf("Expected total_qalys ~ 7, got: %f", total))
})

test_that("threshold for absolute costs is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("Cost Target", "cost_base", 100, 20000,
      condition = threshold_condition_costs(
        summary = "total_cost", type = "absolute",
        strategy = "base", discounted = TRUE, target_value = 5000))

  result <- run_threshold(model)
  results <- verify_threshold_at_value(model, result, "Cost Target")

  total <- get_summaries(results,
    summaries = "total_cost", strategies = "base",
    value_type = "cost", discounted = TRUE,
    use_display_names = FALSE) %>%
    dplyr::pull(amount) %>%
    sum()

  expect_true(abs(total - 5000) < 1e-3,
    info = sprintf("Expected total_cost ~ 5000, got: %f", total))
})

test_that("threshold for difference outcomes is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Outcome Diff", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "difference",
        referent = "treatment", comparator = "base",
        discounted = TRUE, target_value = 0))

  result <- run_threshold(model)
  results <- verify_threshold_at_value(model, result, "Outcome Diff")

  summaries <- get_summaries(results,
    summaries = "total_qalys", value_type = "outcome",
    discounted = TRUE, use_display_names = FALSE)

  treatment_total <- summaries %>%
    dplyr::filter(strategy == "treatment") %>%
    dplyr::pull(amount) %>%
    sum()
  base_total <- summaries %>%
    dplyr::filter(strategy == "base") %>%
    dplyr::pull(amount) %>%
    sum()

  diff <- treatment_total - base_total
  expect_true(abs(diff - 0) < 1e-3,
    info = sprintf("Expected outcome difference ~ 0, got: %f", diff))
})

test_that("threshold for NMB is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("NMB Zero", "cost_base", 100, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  result <- run_threshold(model)
  results <- verify_threshold_at_value(model, result, "NMB Zero")

  nmb <- calculate_nmb(results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    interventions = "treatment",
    comparators = "base",
    use_display_names = FALSE)

  total_nmb <- nmb %>%
    dplyr::pull(nmb_amount) %>%
    sum()

  expect_true(abs(total_nmb) < 1e-3,
    info = sprintf("Expected NMB ~ 0, got: %f", total_nmb))
})

test_that("threshold for CE is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("CE Threshold", "cost_base", 100, 50000,
      condition = threshold_condition_ce(
        "total_qalys", "total_cost", "treatment", "base"))

  result <- run_threshold(model)
  results <- verify_threshold_at_value(model, result, "CE Threshold")

  nmb <- calculate_nmb(results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    interventions = "treatment",
    comparators = "base",
    use_display_names = FALSE)

  total_nmb <- nmb %>%
    dplyr::pull(nmb_amount) %>%
    sum()

  expect_true(abs(total_nmb) < 1e-3,
    info = sprintf("Expected CE NMB ~ 0, got: %f", total_nmb))
})

# ============================================================================
# Group Targeting Tests
# ============================================================================

# Helper: Build a model with groups for group-targeting tests
build_threshold_group_model <- function() {
  define_model("markov") %>%
    set_settings(
      timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3, discount_outcomes = 3
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_disease", 0.03, group = "low_risk") %>%
    add_variable("p_disease", 0.10, group = "high_risk") %>%
    add_variable("cost_base", 1000) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("base") %>%
    add_strategy("treatment") %>%
    add_group("low_risk", weight = "0.6") %>%
    add_group("high_risk", weight = "0.4") %>%
    add_value("qaly", 1, state = "healthy", type = "outcome") %>%
    add_value("qaly", 0.5, state = "sick", type = "outcome") %>%
    add_value("qaly", 0, state = "dead", type = "outcome") %>%
    add_value("cost_drug", cost_base, state = "healthy", type = "cost") %>%
    add_value("cost_drug", cost_base * 0.5, state = "sick", type = "cost") %>%
    add_value("cost_drug", 0, state = "dead", type = "cost") %>%
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000) %>%
    add_summary("total_cost", "cost_drug", type = "cost")
}

test_that("threshold condition constructors store group field", {
  cond_out <- threshold_condition_outcomes(
    summary = "total_qalys", strategy = "base", group = "high_risk")
  expect_equal(cond_out$group, "high_risk")

  cond_cost <- threshold_condition_costs(
    summary = "total_cost", strategy = "base", group = "low_risk")
  expect_equal(cond_cost$group, "low_risk")

  cond_nmb <- threshold_condition_nmb(
    "total_qalys", "total_cost", "treatment", "base", group = "high_risk")
  expect_equal(cond_nmb$group, "high_risk")

  cond_ce <- threshold_condition_ce(
    "total_qalys", "total_cost", "treatment", "base", group = "low_risk")
  expect_equal(cond_ce$group, "low_risk")
})

test_that("threshold condition constructors default group to empty string", {
  cond <- threshold_condition_outcomes(summary = "s", strategy = "s")
  expect_equal(cond$group, "")

  cond2 <- threshold_condition_nmb("h", "c", "r", "comp")
  expect_equal(cond2$group, "")

  cond3 <- threshold_condition_ce("h", "c", "r", "comp")
  expect_equal(cond3$group, "")
})

test_that("validate_threshold_condition rejects non-character group", {
  expect_error(
    openqaly:::validate_threshold_condition(
      list(output = "outcomes", summary = "s", type = "absolute",
           strategy = "s", target_value = 0, group = 123)),
    "single character string"
  )
})

test_that("validate_threshold_spec rejects nonexistent group", {
  model <- build_threshold_group_model() %>%
    add_threshold_analysis("test", "cost_base", 0, 50000,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", strategy = "base", group = "nonexistent"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- openqaly:::parse_model(model_norm)

  expect_error(
    openqaly:::validate_threshold_spec(parsed),
    "group 'nonexistent' not found"
  )
})

test_that("run_threshold with group targeting finds threshold for specific group", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("High Risk Cost", "cost_base", 100, 20000,
      condition = threshold_condition_costs(
        summary = "total_cost", type = "absolute",
        strategy = "base", target_value = 5000, group = "high_risk"))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_equal(result$threshold_values$name, "High Risk Cost")
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))
  expect_true(result$threshold_values$value >= 100)
  expect_true(result$threshold_values$value <= 20000)
})

test_that("group-targeted threshold differs from aggregated threshold", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # Run with group targeting (high_risk group has higher p_disease=0.10,
  # so more people get sick and costs are different)
  model_grouped <- build_threshold_group_model() %>%
    add_threshold_analysis("Grouped", "cost_base", 100, 20000,
      condition = threshold_condition_costs(
        summary = "total_cost", type = "absolute",
        strategy = "base", target_value = 5000, group = "high_risk"))

  result_grouped <- run_threshold(model_grouped)

  # Run without group targeting (aggregated, weighted mix of groups)
  model_agg <- build_threshold_group_model() %>%
    add_threshold_analysis("Aggregated", "cost_base", 100, 20000,
      condition = threshold_condition_costs(
        summary = "total_cost", type = "absolute",
        strategy = "base", target_value = 5000))

  result_agg <- run_threshold(model_agg)

  # Results should differ because the high_risk group has different p_disease
  # which affects the proportion of healthy vs sick patients and thus total costs
  expect_false(isTRUE(all.equal(
    result_grouped$threshold_values$value,
    result_agg$threshold_values$value
  )))
})

test_that("flatten/nest round-trip preserves group field", {
  analysis <- list(
    name = "Test", variable = "p", variable_strategy = "",
    variable_group = "", lower = 0, upper = 1, active = TRUE,
    condition = list(output = "outcomes", summary = "total_qalys",
                     type = "absolute", strategy = "s1",
                     discounted = TRUE, target_value = 5, group = "high_risk")
  )

  flat <- openqaly:::flatten_threshold_analysis(analysis)
  expect_equal(flat$group, "high_risk")

  nested <- openqaly:::nest_threshold_analysis(flat)
  expect_equal(nested$condition$group, "high_risk")
})

test_that("JSON round-trip preserves group field in threshold condition", {
  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Grouped", "cost_base", 0, 50000,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 5, group = "high_risk"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  json_string <- write_model_json(model_norm)
  model_back <- read_model_json(json_string)

  expect_equal(length(model_back$threshold_analyses), 1)
  expect_equal(model_back$threshold_analyses[[1]]$condition$group, "high_risk")
})

test_that("Excel round-trip preserves group field in threshold condition", {
  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Grouped", "cost_base", 0, 50000,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 5, group = "high_risk"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  tmp_dir <- tempdir()
  on.exit(unlink(file.path(tmp_dir, "model.xlsx")), add = TRUE)
  write_model_excel(model_norm, tmp_dir)
  model_back <- read_model(tmp_dir)

  expect_equal(length(model_back$threshold_analyses), 1)
  expect_equal(model_back$threshold_analyses[[1]]$condition$group, "high_risk")
})

test_that("YAML round-trip preserves group field in threshold condition", {
  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Grouped", "cost_base", 0, 50000,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 5, group = "high_risk"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  write_model_yaml(model_norm, tmp)
  model_back <- read_model_yaml(tmp)

  expect_equal(length(model_back$threshold_analyses), 1)
  expect_equal(model_back$threshold_analyses[[1]]$condition$group, "high_risk")
})

test_that("R code generation includes group parameter", {
  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Grouped", "cost_base", 0, 50000,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", target_value = 5, group = "high_risk"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  code <- model_to_r_code(model_norm)

  expect_true(any(grepl('group = "high_risk"', code)))
})

# ============================================================================
# Group Targeting Mathematical Correctness Tests
# ============================================================================

test_that("condition group targeting is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("High Risk Cost Target", "cost_base", 100, 20000,
      condition = threshold_condition_costs(
        summary = "total_cost", type = "absolute",
        strategy = "base", discounted = TRUE,
        target_value = 5000, group = "high_risk"))

  result <- run_threshold(model)
  results <- verify_threshold_at_value(model, result, "High Risk Cost Target")

  total <- get_summaries(results,
    groups = "high_risk",
    summaries = "total_cost", strategies = "base",
    value_type = "cost", discounted = TRUE,
    use_display_names = FALSE) %>%
    dplyr::pull(amount) %>%
    sum()

  expect_true(abs(total - 5000) < 1e-3,
    info = sprintf("Expected high_risk total_cost ~ 5000, got: %f", total))
})

test_that("input variable group targeting is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Variable Group QALY", "p_disease", 0.001, 0.5,
      variable_group = "high_risk",
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", discounted = TRUE, target_value = 7))

  result <- run_threshold(model)
  results <- verify_threshold_at_value_grouped(model, result, "Variable Group QALY",
    variable_group = "high_risk")

  total <- get_summaries(results,
    summaries = "total_qalys", strategies = "base",
    value_type = "outcome", discounted = TRUE,
    use_display_names = FALSE) %>%
    dplyr::pull(amount) %>%
    sum()

  expect_true(abs(total - 7) < 1e-3,
    info = sprintf("Expected aggregated total_qalys ~ 7, got: %f", total))
})

test_that("combined variable_group and condition group targeting is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Combined Group", "p_disease", 0.001, 0.5,
      variable_group = "high_risk",
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", discounted = TRUE,
        target_value = 7, group = "high_risk"))

  result <- run_threshold(model)
  results <- verify_threshold_at_value_grouped(model, result, "Combined Group",
    variable_group = "high_risk")

  total <- get_summaries(results,
    groups = "high_risk",
    summaries = "total_qalys", strategies = "base",
    value_type = "outcome", discounted = TRUE,
    use_display_names = FALSE) %>%
    dplyr::pull(amount) %>%
    sum()

  expect_true(abs(total - 7) < 1e-3,
    info = sprintf("Expected high_risk total_qalys ~ 7, got: %f", total))
})

# ============================================================================
# Trace Threshold Condition Tests
# ============================================================================

# --- Builder Tests ---

test_that("threshold_condition_trace returns correct structure", {
  cond <- threshold_condition_trace(
    state = "healthy", time = 5, time_unit = "cycle",
    type = "absolute", strategy = "base", target_value = 0.7)

  expect_equal(cond$output, "trace")
  expect_equal(cond$state, "healthy")
  expect_equal(cond$time, 5)
  expect_equal(cond$time_unit, "cycle")
  expect_equal(cond$type, "absolute")
  expect_equal(cond$strategy, "base")
  expect_equal(cond$target_value, 0.7)
  expect_equal(cond$group, "")
})

test_that("threshold_condition_trace defaults time_unit to cycle and type to absolute", {
  cond <- threshold_condition_trace(
    state = "sick", time = 3, strategy = "base", target_value = 0.5)
  expect_equal(cond$time_unit, "cycle")
  expect_equal(cond$type, "absolute")
  expect_equal(cond$group, "")
})

test_that("threshold_condition_trace stores group field", {
  cond <- threshold_condition_trace(
    state = "healthy", time = 5, strategy = "base",
    target_value = 0.7, group = "high_risk")
  expect_equal(cond$group, "high_risk")
})

test_that("threshold_condition_trace defaults group to empty string", {
  cond <- threshold_condition_trace(
    state = "healthy", time = 5, strategy = "base", target_value = 0.7)
  expect_equal(cond$group, "")
})

# --- Validation Tests (validate_threshold_condition) ---

test_that("validate_threshold_condition rejects trace missing state", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "trace", state = "", time = 5,
                       time_unit = "cycle", type = "absolute",
                       strategy = "base", target_value = 0.7)),
    "requires 'state'"
  )
})

test_that("validate_threshold_condition rejects trace missing time", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "trace", state = "healthy", time = NULL,
                       time_unit = "cycle", type = "absolute",
                       strategy = "base", target_value = 0.7)),
    "requires numeric 'time'"
  )
})

test_that("validate_threshold_condition rejects trace missing target_value", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "trace", state = "healthy", time = 5,
                       time_unit = "cycle", type = "absolute",
                       strategy = "base", target_value = NULL)),
    "requires 'target_value'"
  )
})

test_that("validate_threshold_condition rejects trace invalid time_unit", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "trace", state = "healthy", time = 5,
                       time_unit = "fortnight", type = "absolute",
                       strategy = "base", target_value = 0.7)),
    "time_unit"
  )
})

test_that("validate_threshold_condition rejects trace absolute without strategy", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "trace", state = "healthy", time = 5,
                       time_unit = "cycle", type = "absolute",
                       target_value = 0.7)),
    "requires 'strategy'"
  )
})

test_that("validate_threshold_condition rejects trace difference without referent", {
  model <- build_threshold_test_model()
  expect_error(
    add_threshold_analysis(model, "test", "p_disease", 0, 1,
      condition = list(output = "trace", state = "healthy", time = 5,
                       time_unit = "cycle", type = "difference",
                       comparator = "base", target_value = 0)),
    "requires 'referent'"
  )
})

# --- validate_threshold_spec Tests ---

test_that("validate_threshold_spec rejects nonexistent state in trace condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("test", "p_disease", 0, 1,
      condition = threshold_condition_trace(
        state = "nonexistent", time = 5,
        strategy = "base", target_value = 0.7))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- openqaly:::parse_model(model_norm)

  expect_error(
    openqaly:::validate_threshold_spec(parsed),
    "state 'nonexistent' not found"
  )
})

test_that("validate_threshold_spec rejects out-of-range cycle in trace condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("test", "p_disease", 0, 1,
      condition = threshold_condition_trace(
        state = "healthy", time = 100,
        strategy = "base", target_value = 0.7))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- openqaly:::parse_model(model_norm)

  expect_error(
    openqaly:::validate_threshold_spec(parsed),
    "outside model timeframe"
  )
})

# --- Mathematical Correctness Tests (Overall) ---

test_that("threshold for absolute trace is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Healthy", "p_disease", 0.001, 0.5,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "cycle",
        type = "absolute", strategy = "base", target_value = 0.7))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))

  # Verify: re-run model at found value, check trace
  results <- verify_threshold_at_value(model, result, "Trace Healthy")
  trace_data <- get_trace(results, format = "wide", strategies = "base",
                          use_display_names = FALSE)
  cycle5_row <- trace_data[trace_data$cycle == 5, ]
  expect_true(abs(cycle5_row$healthy - 0.7) < 1e-3,
    info = sprintf("Expected healthy trace at cycle 5 ~ 0.7, got: %f", cycle5_row$healthy))
})

test_that("threshold for difference trace is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Diff", "p_disease", 0.001, 0.5,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "cycle",
        type = "difference", referent = "treatment",
        comparator = "base", target_value = 0))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))

  # Verify: re-run, check difference is ~ 0
  results <- verify_threshold_at_value(model, result, "Trace Diff")
  trace_base <- get_trace(results, format = "wide", strategies = "base",
                          use_display_names = FALSE)
  trace_treat <- get_trace(results, format = "wide", strategies = "treatment",
                           use_display_names = FALSE)
  base_val <- trace_base[trace_base$cycle == 5, ]$healthy
  treat_val <- trace_treat[trace_treat$cycle == 5, ]$healthy
  diff <- treat_val - base_val
  expect_true(abs(diff - 0) < 1e-3,
    info = sprintf("Expected trace difference ~ 0, got: %f", diff))
})

test_that("threshold for trace with year time_unit is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # Model has cycle_length=1 year, so time=5 year == cycle 5
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Year", "p_disease", 0.001, 0.5,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "year",
        type = "absolute", strategy = "base", target_value = 0.7))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_false(is.na(result$threshold_values$value))

  # Verify
  results <- verify_threshold_at_value(model, result, "Trace Year")
  trace_data <- get_trace(results, format = "wide", strategies = "base",
                          use_display_names = FALSE)
  cycle5_row <- trace_data[trace_data$cycle == 5, ]
  expect_true(abs(cycle5_row$healthy - 0.7) < 1e-3,
    info = sprintf("Expected healthy trace at year 5 (cycle 5) ~ 0.7, got: %f", cycle5_row$healthy))
})

# --- Mathematical Correctness Tests (With Groups) ---

test_that("condition group-targeted trace threshold is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Trace Group", "p_disease", 0.001, 0.5,
      variable_group = "high_risk",
      condition = threshold_condition_trace(
        state = "sick", time = 5, time_unit = "cycle",
        type = "absolute", strategy = "base",
        target_value = 0.15, group = "high_risk"))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_false(is.na(result$threshold_values$value))

  # Verify at found value
  results <- verify_threshold_at_value_grouped(model, result, "Trace Group",
    variable_group = "high_risk")
  trace_data <- get_trace(results, format = "wide", strategies = "base",
                          groups = "high_risk", use_display_names = FALSE)
  cycle5_row <- trace_data[trace_data$cycle == 5, ]
  expect_true(abs(cycle5_row$sick - 0.15) < 1e-3,
    info = sprintf("Expected high_risk sick trace at cycle 5 ~ 0.15, got: %f", cycle5_row$sick))
})

test_that("input variable group-targeted trace threshold is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Var Group Trace", "p_disease", 0.001, 0.5,
      variable_group = "high_risk",
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "cycle",
        type = "absolute", strategy = "base", target_value = 0.7))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_false(is.na(result$threshold_values$value))

  # Verify: re-run with group-specific override
  results <- verify_threshold_at_value_grouped(model, result, "Var Group Trace",
    variable_group = "high_risk")
  trace_data <- get_trace(results, format = "wide", strategies = "base",
                          groups = "overall", use_display_names = FALSE)
  cycle5_row <- trace_data[trace_data$cycle == 5, ]
  expect_true(abs(cycle5_row$healthy - 0.7) < 1e-3,
    info = sprintf("Expected aggregated healthy trace at cycle 5 ~ 0.7, got: %f", cycle5_row$healthy))
})

test_that("combined variable_group and condition group trace targeting is mathematically correct", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_group_model() %>%
    add_threshold_analysis("Combined Trace", "p_disease", 0.001, 0.5,
      variable_group = "high_risk",
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "cycle",
        type = "absolute", strategy = "base",
        target_value = 0.7, group = "high_risk"))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_false(is.na(result$threshold_values$value))

  # Verify
  results <- verify_threshold_at_value_grouped(model, result, "Combined Trace",
    variable_group = "high_risk")
  trace_data <- get_trace(results, format = "wide", strategies = "base",
                          groups = "high_risk", use_display_names = FALSE)
  cycle5_row <- trace_data[trace_data$cycle == 5, ]
  expect_true(abs(cycle5_row$healthy - 0.7) < 1e-3,
    info = sprintf("Expected high_risk healthy trace at cycle 5 ~ 0.7, got: %f", cycle5_row$healthy))
})

# --- Serialization Round-Trip Tests for Trace ---

test_that("JSON round-trip preserves trace condition fields", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Test", "p_disease", 0, 1,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "year",
        type = "absolute", strategy = "base",
        target_value = 0.7, group = ""))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  json_string <- write_model_json(model_norm)
  model_back <- read_model_json(json_string)

  expect_equal(length(model_back$threshold_analyses), 1)
  a <- model_back$threshold_analyses[[1]]
  expect_equal(a$name, "Trace Test")
  expect_equal(a$condition$output, "trace")
  expect_equal(a$condition$state, "healthy")
  expect_equal(a$condition$time, 5)
  expect_equal(a$condition$time_unit, "year")
  expect_equal(a$condition$type, "absolute")
  expect_equal(a$condition$strategy, "base")
  expect_equal(a$condition$target_value, 0.7)
})

test_that("Excel round-trip preserves trace condition fields", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Test", "p_disease", 0, 1,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "year",
        type = "absolute", strategy = "base", target_value = 0.7))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  tmp_dir <- tempdir()
  on.exit(unlink(file.path(tmp_dir, "model.xlsx")), add = TRUE)
  write_model_excel(model_norm, tmp_dir)
  model_back <- read_model(tmp_dir)

  expect_equal(length(model_back$threshold_analyses), 1)
  a <- model_back$threshold_analyses[[1]]
  expect_equal(a$condition$output, "trace")
  expect_equal(a$condition$state, "healthy")
  expect_equal(a$condition$time, 5)
  expect_equal(a$condition$time_unit, "year")
  expect_equal(a$condition$strategy, "base")
  expect_equal(a$condition$target_value, 0.7)
})

test_that("YAML round-trip preserves trace condition fields", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Test", "p_disease", 0, 1,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "year",
        type = "absolute", strategy = "base", target_value = 0.7))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)

  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp), add = TRUE)
  write_model_yaml(model_norm, tmp)
  model_back <- read_model_yaml(tmp)

  expect_equal(length(model_back$threshold_analyses), 1)
  a <- model_back$threshold_analyses[[1]]
  expect_equal(a$condition$output, "trace")
  expect_equal(a$condition$state, "healthy")
  expect_equal(a$condition$time, 5)
  expect_equal(a$condition$time_unit, "year")
  expect_equal(a$condition$strategy, "base")
  expect_equal(a$condition$target_value, 0.7)
})

test_that("R code generation includes trace condition", {
  model <- build_threshold_test_model() %>%
    add_threshold_analysis("Trace Test", "p_disease", 0, 1,
      condition = threshold_condition_trace(
        state = "healthy", time = 5, time_unit = "year",
        type = "absolute", strategy = "base",
        target_value = 0.7, group = "high_risk"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  code <- model_to_r_code(model_norm)

  expect_true(any(grepl("threshold_condition_trace", code)))
  expect_true(any(grepl('state = "healthy"', code)))
  expect_true(any(grepl('time = 5', code)))
  expect_true(any(grepl('time_unit = "year"', code)))
  expect_true(any(grepl('group = "high_risk"', code)))
})

# ============================================================================
# WTP Override Tests
# ============================================================================

# Helper: build model where health summary has NO wtp defined
build_threshold_no_wtp_model <- function() {
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
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("base") %>%
    add_strategy("treatment") %>%
    add_value("qaly", 1, state = "healthy", type = "outcome") %>%
    add_value("qaly", 0.5, state = "sick", type = "outcome") %>%
    add_value("qaly", 0, state = "dead", type = "outcome") %>%
    add_value("cost_drug", cost_base, state = "healthy", type = "cost") %>%
    add_value("cost_drug", cost_base * 0.5, state = "sick", type = "cost") %>%
    add_value("cost_drug", 0, state = "dead", type = "cost") %>%
    add_summary("total_qalys", "qaly", type = "outcome") %>%
    add_summary("total_cost", "cost_drug", type = "cost")
}

test_that("NMB condition with explicit wtp uses provided value", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("NMB Custom WTP", "cost_base", 100, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base", wtp = 100000))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))

  # Verify NMB at this wtp is ~ 0
  results <- verify_threshold_at_value(model, result, "NMB Custom WTP")
  nmb <- calculate_nmb(results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    interventions = "treatment",
    comparators = "base",
    wtp = 100000,
    use_display_names = FALSE)

  total_nmb <- nmb %>%
    dplyr::pull(nmb_amount) %>%
    sum()

  expect_true(abs(total_nmb) < 1e-3,
    info = sprintf("Expected NMB ~ 0 at wtp=100000, got: %f", total_nmb))
})

test_that("NMB condition without wtp falls back to summary WTP", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # build_threshold_cost_model has wtp=50000 on the health summary
  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("NMB Default WTP", "cost_base", 100, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))

  # Verify NMB at summary wtp (50000) is ~ 0
  results <- verify_threshold_at_value(model, result, "NMB Default WTP")
  nmb <- calculate_nmb(results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    interventions = "treatment",
    comparators = "base",
    wtp = 50000,
    use_display_names = FALSE)

  total_nmb <- nmb %>%
    dplyr::pull(nmb_amount) %>%
    sum()

  expect_true(abs(total_nmb) < 1e-3,
    info = sprintf("Expected NMB ~ 0 at default wtp=50000, got: %f", total_nmb))
})

test_that("NMB condition without wtp and no summary WTP errors", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_no_wtp_model() %>%
    add_threshold_analysis("NMB No WTP", "cost_base", 100, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  expect_error(
    run_threshold(model),
    "WTP"
  )
})

test_that("CE condition with explicit wtp uses provided value", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("CE Custom WTP", "cost_base", 100, 50000,
      condition = threshold_condition_ce(
        "total_qalys", "total_cost", "treatment", "base", wtp = 100000))

  result <- run_threshold(model)

  expect_equal(nrow(result$threshold_values), 1)
  expect_true(is.numeric(result$threshold_values$value))
  expect_false(is.na(result$threshold_values$value))
})

test_that("CE condition without wtp and no summary WTP errors", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_threshold_no_wtp_model() %>%
    add_threshold_analysis("CE No WTP", "cost_base", 100, 50000,
      condition = threshold_condition_ce(
        "total_qalys", "total_cost", "treatment", "base"))

  expect_error(
    run_threshold(model),
    "WTP"
  )
})

test_that("NMB condition with explicit wtp is preserved in condition list", {
  cond <- threshold_condition_nmb(
    "total_qalys", "total_cost", "treatment", "base", wtp = 75000)
  expect_equal(cond$wtp, 75000)
})

test_that("CE condition with explicit wtp is preserved in condition list", {
  cond <- threshold_condition_ce(
    "total_qalys", "total_cost", "treatment", "base", wtp = 75000)
  expect_equal(cond$wtp, 75000)
})

test_that("NMB/CE conditions default wtp to NULL", {
  cond_nmb <- threshold_condition_nmb("h", "c", "r", "comp")
  expect_null(cond_nmb$wtp)

  cond_ce <- threshold_condition_ce("h", "c", "r", "comp")
  expect_null(cond_ce$wtp)
})

test_that("R code generation includes wtp parameter for NMB condition", {
  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("NMB WTP", "cost_base", 0, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base", wtp = 75000))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  code <- model_to_r_code(model_norm)

  expect_true(any(grepl("wtp = 75000", code)))
})

test_that("R code generation includes wtp parameter for CE condition", {
  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("CE WTP", "cost_base", 0, 50000,
      condition = threshold_condition_ce(
        "total_qalys", "total_cost", "treatment", "base", wtp = 75000))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  code <- model_to_r_code(model_norm)

  expect_true(any(grepl("wtp = 75000", code)))
})

test_that("R code generation omits wtp when NULL", {
  model <- build_threshold_cost_model() %>%
    add_threshold_analysis("NMB No WTP", "cost_base", 0, 50000,
      condition = threshold_condition_nmb(
        "total_qalys", "total_cost", "treatment", "base"))

  model_norm <- openqaly:::normalize_and_validate_model(model, preserve_builder = FALSE)
  code <- model_to_r_code(model_norm)

  expect_false(any(grepl("wtp =", code)))
})
