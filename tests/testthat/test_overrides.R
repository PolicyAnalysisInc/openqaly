context("Override Controls")

# ============================================================================
# Builder API Tests
# ============================================================================

test_that("override_categories is initialized in define_model", {
  model <- define_model("markov")

  expect_true("override_categories" %in% names(model))
  expect_equal(length(model$override_categories), 0)
})

test_that("add_override_category adds a category", {
  model <- define_model("markov") %>%
    add_override_category("Clinical Parameters")

  expect_equal(length(model$override_categories), 1)
  expect_equal(model$override_categories[[1]]$name, "Clinical Parameters")
  expect_false(model$override_categories[[1]]$general)
  expect_equal(length(model$override_categories[[1]]$overrides), 0)
})

test_that("add_override_category supports general flag", {
  model <- define_model("markov") %>%
    add_override_category("Model Settings", general = TRUE)

  expect_true(model$override_categories[[1]]$general)
})

test_that("add_override_category errors on duplicate name", {
  model <- define_model("markov") %>%
    add_override_category("Clinical Parameters")

  expect_error(
    add_override_category(model, "Clinical Parameters"),
    "already exists"
  )
})

test_that("add_override_category errors on duplicate name case-insensitive", {
  model <- define_model("markov") %>%
    add_override_category("Clinical Parameters")

  expect_error(
    add_override_category(model, "clinical parameters"),
    "already exists"
  )
})

test_that("add_override_category errors on empty name", {
  model <- define_model("markov")

  expect_error(
    add_override_category(model, ""),
    "non-empty character string"
  )
})

test_that("add_override adds a numeric override", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Economic") %>%
    add_override("Economic",
      title = "Treatment Cost",
      name = "cost",
      type = "variable",
      input_type = "numeric",
      expression = 5000,
      min = 0, max = 20000
    )

  expect_equal(length(model$override_categories[[1]]$overrides), 1)
  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$title, "Treatment Cost")
  expect_equal(ovr$name, "cost")
  expect_equal(ovr$type, "variable")
  expect_equal(ovr$input_type, "numeric")
  expect_equal(ovr$overridden_expression, "5000")
  expect_equal(ovr$input_config$min, 0)
  expect_equal(ovr$input_config$max, 20000)
})

test_that("add_override adds a slider override", {
  model <- define_model("markov") %>%
    add_variable("p_disease", 0.03) %>%
    add_override_category("Clinical") %>%
    add_override("Clinical",
      title = "Disease Probability",
      name = "p_disease",
      type = "variable",
      input_type = "slider",
      expression = 0.03,
      min = 0, max = 0.1, step_size = 0.005
    )

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_type, "slider")
  expect_equal(ovr$input_config$min, 0)
  expect_equal(ovr$input_config$max, 0.1)
  expect_equal(ovr$input_config$step_size, 0.005)
})

test_that("add_override adds a dropdown override", {
  model <- define_model("markov") %>%
    add_variable("efficacy", 0.8) %>%
    add_override_category("Clinical") %>%
    add_override("Clinical",
      title = "Treatment Protocol",
      name = "efficacy",
      type = "variable",
      input_type = "dropdown",
      expression = "0.8",
      options = list(
        override_option("Standard (80%)", "0.8", is_base_case = TRUE),
        override_option("Intensive (95%)", "0.95")
      )
    )

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_type, "dropdown")
  expect_equal(length(ovr$input_config$options), 2)
  expect_equal(ovr$input_config$options[[1]]$label, "Standard (80%)")
  expect_equal(ovr$input_config$options[[1]]$value, "0.8")
  expect_true(ovr$input_config$options[[1]]$is_base_case)
  expect_false(ovr$input_config$options[[2]]$is_base_case)
})

test_that("add_override adds a formula override", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Economic") %>%
    add_override("Economic",
      title = "Cost Formula",
      name = "cost",
      type = "variable",
      input_type = "formula",
      expression = base_cost * inflation
    )

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_type, "formula")
  expect_equal(ovr$overridden_expression, "base_cost * inflation")
})

test_that("add_override adds a setting override", {
  model <- define_model("markov") %>%
    add_override_category("Model Settings") %>%
    add_override("Model Settings",
      title = "Cost Discount Rate",
      name = "discount_cost",
      type = "setting",
      input_type = "numeric",
      expression = 0.03,
      min = 0, max = 10
    )

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$type, "setting")
  expect_equal(ovr$name, "discount_cost")
})

test_that("add_override adds a timeframe override", {
  model <- define_model("markov") %>%
    add_override_category("Model Settings") %>%
    add_override("Model Settings",
      title = "Analysis Timeframe",
      name = "timeframe",
      type = "setting",
      input_type = "timeframe",
      expression = "10|years"
    )

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_type, "timeframe")
  expect_equal(ovr$overridden_expression, "10|years")
})

# ============================================================================
# Validation Tests
# ============================================================================

test_that("add_override errors when category doesn't exist", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000)

  expect_error(
    add_override(model, "NonExistent",
      title = "Cost", name = "cost", expression = 5000),
    "not found.*Use add_override_category"
  )
})

test_that("add_override errors on empty title", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "", name = "cost", expression = 5000),
    "title must be a non-empty"
  )
})

test_that("add_override errors on invalid type", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "Cost", name = "cost", type = "invalid", expression = 5000),
    "type must be 'variable' or 'setting'"
  )
})

test_that("add_override errors on invalid input_type", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "Cost", name = "cost", input_type = "checkbox", expression = 5000),
    "input_type must be one of"
  )
})

test_that("add_override errors on invalid setting name", {
  model <- define_model("markov") %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "Bad Setting", name = "invalid_setting",
      type = "setting", expression = 5),
    "Invalid override setting name"
  )
})

test_that("add_override errors when strategy/group used with setting", {
  model <- define_model("markov") %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "Discount", name = "discount_cost",
      type = "setting", strategy = "treatment_a", expression = 0.03),
    "Strategy and group cannot be specified for setting overrides"
  )
})

test_that("add_override errors on min >= max", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "Cost", name = "cost", expression = 5000,
      min = 100, max = 50),
    "min.*must be less than max"
  )
})

test_that("add_override errors on step_size <= 0", {
  model <- define_model("markov") %>%
    add_variable("p", 0.5) %>%
    add_override_category("Test")

  expect_error(
    add_override(model, "Test",
      title = "Prob", name = "p", input_type = "slider",
      expression = 0.5, step_size = 0),
    "step_size must be greater than 0"
  )
})

test_that("add_override errors on duplicate override in same category", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Cost", name = "cost", expression = 5000)

  expect_error(
    add_override(model, "Test",
      title = "Cost Again", name = "cost", expression = 6000),
    "already exists in category"
  )
})

test_that("add_override validates strategy-specific variable targeting", {
  model <- define_model("markov") %>%
    add_strategy("treatment_a") %>%
    add_strategy("treatment_b") %>%
    add_variable("cost", 5000, strategy = "treatment_a") %>%
    add_variable("cost", 3000, strategy = "treatment_b") %>%
    add_override_category("Test")

  # Should error without strategy
  expect_error(
    add_override(model, "Test",
      title = "Cost", name = "cost", expression = 5000),
    "defined for specific strategy"
  )

  # Should work with strategy
  model_ok <- add_override(model, "Test",
    title = "Cost", name = "cost", expression = 5000,
    strategy = "treatment_a")
  expect_equal(length(model_ok$override_categories[[1]]$overrides), 1)
  expect_equal(model_ok$override_categories[[1]]$overrides[[1]]$strategy, "treatment_a")
})

# ============================================================================
# Default Config Tests
# ============================================================================

test_that("add_override applies default configs for numeric", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Cost", name = "cost", input_type = "numeric",
      expression = 5000)

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_config$min, 0)
  expect_equal(ovr$input_config$max, 100)
})

test_that("add_override applies default configs for slider", {
  model <- define_model("markov") %>%
    add_variable("p", 0.5) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Prob", name = "p", input_type = "slider",
      expression = 0.5)

  ovr <- model$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_config$min, 0)
  expect_equal(ovr$input_config$max, 1)
  expect_equal(ovr$input_config$step_size, 0.05)
})

# ============================================================================
# override_option Tests
# ============================================================================

test_that("override_option creates valid option structure", {
  opt <- override_option("Standard", "0.8", is_base_case = TRUE)

  expect_equal(opt$label, "Standard")
  expect_equal(opt$value, "0.8")
  expect_true(opt$is_base_case)
})

test_that("override_option defaults is_base_case to FALSE", {
  opt <- override_option("Alternative", "0.9")

  expect_false(opt$is_base_case)
})

test_that("override_option errors on empty label", {
  expect_error(
    override_option("", "0.8"),
    "non-empty character string"
  )
})

# ============================================================================
# parse_override_expression Tests
# ============================================================================

test_that("parse_override_expression handles numeric input", {
  override <- list(
    input_type = "numeric",
    overridden_expression = "42"
  )
  result <- openqaly:::parse_override_expression(override)
  expect_equal(result, 42)
  expect_true(is.numeric(result))
})

test_that("parse_override_expression handles slider input", {
  override <- list(
    input_type = "slider",
    overridden_expression = "0.05"
  )
  result <- openqaly:::parse_override_expression(override)
  expect_equal(result, 0.05)
})

test_that("parse_override_expression handles dropdown with numeric value", {
  override <- list(
    input_type = "dropdown",
    overridden_expression = "0.8"
  )
  result <- openqaly:::parse_override_expression(override)
  expect_equal(result, 0.8)
})

test_that("parse_override_expression handles dropdown with string value", {
  override <- list(
    input_type = "dropdown",
    overridden_expression = "treatment_a"
  )
  result <- openqaly:::parse_override_expression(override)
  expect_equal(result, "treatment_a")
})

test_that("parse_override_expression handles formula input", {
  override <- list(
    input_type = "formula",
    overridden_expression = "base_cost * 1.1"
  )
  result <- openqaly:::parse_override_expression(override)
  expect_s3_class(result, "oq_formula")
})

test_that("parse_override_expression handles timeframe input", {
  override <- list(
    input_type = "timeframe",
    overridden_expression = "10|years"
  )
  result <- openqaly:::parse_override_expression(override)
  expect_equal(result$timeframe, 10)
  expect_equal(result$timeframe_unit, "years")
})

# ============================================================================
# apply_override_categories Tests
# ============================================================================

test_that("apply_override_categories returns segments unchanged when no overrides", {
  model <- define_model("markov")
  segments <- tibble(strategy = "default", group = "all")
  result <- openqaly:::apply_override_categories(model, segments)
  expect_equal(nrow(result), 1)
})

test_that("apply_override_categories injects variable overrides", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Cost", name = "cost",
      input_type = "numeric", expression = 7000)

  segments <- tibble(strategy = "default", group = "all")
  result <- openqaly:::apply_override_categories(model, segments)

  expect_true("parameter_overrides" %in% names(result))
  expect_equal(result$parameter_overrides[[1]][["cost"]], 7000)
})

test_that("apply_override_categories injects setting overrides", {
  model <- define_model("markov") %>%
    add_override_category("Settings") %>%
    add_override("Settings",
      title = "Discount", name = "discount_cost",
      type = "setting", input_type = "numeric", expression = 0.03)

  segments <- tibble(strategy = "default", group = "all")
  result <- openqaly:::apply_override_categories(model, segments)

  expect_true("setting_overrides" %in% names(result))
  expect_equal(result$setting_overrides[[1]][["discount_cost"]], 0.03)
})

test_that("apply_override_categories applies strategy-specific overrides correctly", {
  model <- define_model("markov") %>%
    add_strategy("treatment_a") %>%
    add_strategy("treatment_b") %>%
    add_variable("cost", 5000, strategy = "treatment_a") %>%
    add_variable("cost", 3000, strategy = "treatment_b") %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Cost A", name = "cost",
      strategy = "treatment_a",
      input_type = "numeric", expression = 7000)

  segments <- tibble(
    strategy = c("treatment_a", "treatment_b"),
    group = c("all", "all")
  )
  result <- openqaly:::apply_override_categories(model, segments)

  # treatment_a should have override
  expect_equal(result$parameter_overrides[[1]][["cost"]], 7000)
  # treatment_b should NOT have override
  expect_null(result$parameter_overrides[[2]][["cost"]])
})

test_that("apply_override_categories handles timeframe overrides", {
  model <- define_model("markov") %>%
    add_override_category("Settings") %>%
    add_override("Settings",
      title = "Timeframe", name = "timeframe",
      type = "setting", input_type = "timeframe",
      expression = "20|years")

  segments <- tibble(strategy = "default", group = "all")
  result <- openqaly:::apply_override_categories(model, segments)

  expect_equal(result$setting_overrides[[1]][["timeframe"]], 20)
  expect_equal(result$setting_overrides[[1]][["timeframe_unit"]], "years")
})

# ============================================================================
# Serialization Round-Trip Tests
# ============================================================================

# Helper to create a model with overrides for serialization tests
create_test_override_model <- function() {
  define_model("markov") %>%
    set_settings(timeframe = 20, discount_cost = 0.03, discount_outcomes = 0.03) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_disease", 0.03) %>%
    add_variable("cost_treatment", 5000) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("sick", "dead", 0.1) %>%
    add_transition("healthy", "healthy", C) %>%
    add_transition("sick", "sick", C) %>%
    add_value("cost", cost_treatment, state = "sick", type = "cost") %>%
    add_summary("total_cost", "cost", type = "cost") %>%
    add_override_category("Clinical Parameters") %>%
    add_override_category("Economic Parameters") %>%
    add_override("Clinical Parameters",
      title = "Disease Probability",
      name = "p_disease",
      type = "variable",
      input_type = "slider",
      expression = 0.03,
      description = "Annual prob of progression",
      min = 0, max = 0.1, step_size = 0.005
    ) %>%
    add_override("Economic Parameters",
      title = "Treatment Cost",
      name = "cost_treatment",
      type = "variable",
      input_type = "numeric",
      expression = 5000,
      min = 0, max = 20000
    )
}

test_that("JSON serialization round-trip preserves overrides", {
  model <- create_test_override_model()
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  # Write to JSON
  json_str <- write_model_json(model)

  # Read back
  model2 <- read_model_json(json_str)

  # Verify structure
  expect_equal(length(model2$override_categories), 2)
  expect_equal(model2$override_categories[[1]]$name, "Clinical Parameters")
  expect_equal(model2$override_categories[[2]]$name, "Economic Parameters")

  # Verify overrides
  ovr1 <- model2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr1$title, "Disease Probability")
  expect_equal(ovr1$name, "p_disease")
  expect_equal(ovr1$input_type, "slider")
  expect_equal(ovr1$overridden_expression, "0.03")
  expect_equal(ovr1$input_config$min, 0)
  expect_equal(ovr1$input_config$max, 0.1)
  expect_equal(ovr1$input_config$step_size, 0.005)

  ovr2 <- model2$override_categories[[2]]$overrides[[1]]
  expect_equal(ovr2$title, "Treatment Cost")
  expect_equal(ovr2$name, "cost_treatment")
  expect_equal(ovr2$input_type, "numeric")
})

test_that("JSON serialization round-trip preserves dropdown overrides", {
  model <- define_model("markov") %>%
    add_variable("efficacy", 0.8) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Protocol",
      name = "efficacy",
      input_type = "dropdown",
      expression = "0.8",
      options = list(
        override_option("Standard", "0.8", is_base_case = TRUE),
        override_option("Intensive", "0.95")
      )
    )
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  json_str <- write_model_json(model)
  model2 <- read_model_json(json_str)

  ovr <- model2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_type, "dropdown")
  expect_equal(length(ovr$input_config$options), 2)
  expect_equal(ovr$input_config$options[[1]]$label, "Standard")
  expect_equal(ovr$input_config$options[[1]]$value, "0.8")
  expect_true(ovr$input_config$options[[1]]$is_base_case)
})

test_that("YAML serialization round-trip preserves overrides", {
  model <- create_test_override_model()
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  # Write to YAML temp file
  tmp_file <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp_file))
  write_model_yaml(model, tmp_file)

  # Read back
  model2 <- read_model_yaml(tmp_file)

  # Verify structure
  expect_equal(length(model2$override_categories), 2)
  expect_equal(model2$override_categories[[1]]$name, "Clinical Parameters")

  ovr1 <- model2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr1$title, "Disease Probability")
  expect_equal(ovr1$input_type, "slider")
  expect_equal(ovr1$overridden_expression, "0.03")
})

test_that("Excel serialization round-trip preserves overrides", {
  model <- create_test_override_model()
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  # Write to Excel temp dir
  tmp_dir <- tempdir()
  model_dir <- file.path(tmp_dir, "test_override_model")
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(model_dir, recursive = TRUE))

  write_model_excel(model, model_dir)

  # Read back
  model2 <- read_model(model_dir)

  # Verify structure
  expect_equal(length(model2$override_categories), 2)
  expect_equal(model2$override_categories[[1]]$name, "Clinical Parameters")

  ovr1 <- model2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr1$title, "Disease Probability")
  expect_equal(ovr1$name, "p_disease")
  expect_equal(ovr1$input_type, "slider")
  expect_equal(ovr1$overridden_expression, "0.03")
  expect_equal(ovr1$input_config$min, 0)
  expect_equal(ovr1$input_config$max, 0.1)
  expect_equal(ovr1$input_config$step_size, 0.005)
})

test_that("R code generation includes overrides", {
  model <- create_test_override_model()
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  code <- model_to_r_code(model)
  code_str <- paste(code, collapse = "\n")

  expect_true(grepl("add_override_category", code_str))
  expect_true(grepl("add_override", code_str))
  expect_true(grepl("Clinical Parameters", code_str))
  expect_true(grepl("Disease Probability", code_str))
  expect_true(grepl("p_disease", code_str))
})

# ============================================================================
# CLI Notification Tests
# ============================================================================

test_that("notify_active_overrides emits message with active overrides", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Cost", name = "cost", expression = 5000)

  expect_message(
    openqaly:::notify_active_overrides(model),
    "active override"
  )
})

test_that("notify_active_overrides is silent with no overrides", {
  model <- define_model("markov")

  expect_silent(
    openqaly:::notify_active_overrides(model)
  )
})

test_that("print_override_categories formats output correctly", {
  model <- define_model("markov") %>%
    add_variable("cost", 5000) %>%
    add_override_category("Test") %>%
    add_override("Test",
      title = "Cost", name = "cost",
      input_type = "numeric", expression = 5000,
      min = 0, max = 10000)

  output <- capture.output(print_override_categories(model))
  output_str <- paste(output, collapse = "\n")

  expect_true(grepl("Active overrides", output_str))
  expect_true(grepl("Test:", output_str))
  expect_true(grepl("Cost", output_str))
})

# ============================================================================
# Model with no overrides behaves normally
# ============================================================================

test_that("model without overrides works identically to original", {
  model <- define_model("markov") %>%
    set_settings(timeframe = 5, timeframe_unit = "years", cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0.03, discount_outcomes = 0.03) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_strategy("base") %>%
    add_variable("p_disease", 0.1) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", C) %>%
    add_transition("sick", "sick", C) %>%
    add_transition("dead", "dead", 1) %>%
    add_value("cost", 1000, state = "healthy", type = "cost") %>%
    add_value("cost", 2000, state = "sick", type = "cost") %>%
    add_value("cost", 0, state = "dead", type = "cost") %>%
    add_value("qalys", 1, state = "healthy", type = "outcome") %>%
    add_value("qalys", 0.5, state = "sick", type = "outcome") %>%
    add_value("qalys", 0, state = "dead", type = "outcome") %>%
    add_summary("total_cost", "cost", type = "cost") %>%
    add_summary("total_qalys", "qalys", type = "outcome")

  # This should work exactly as before
  expect_no_error(result <- run_model(model))
  expect_true(!is.null(result$aggregated))
})
