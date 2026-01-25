context("Two-Way Sensitivity Analysis")

# ============================================================================
# Model Builder Tests
# ============================================================================

test_that("twsa_analyses list is initialized in define_model", {
  model <- define_model("markov")

  expect_true("twsa_analyses" %in% names(model))
  expect_type(model$twsa_analyses, "list")
  expect_equal(length(model$twsa_analyses), 0)
})

test_that("add_twsa creates analysis slot", {
  model <- define_model("markov") %>%
    add_twsa("Cost vs Efficacy", description = "Vary cost and efficacy together")

  expect_equal(length(model$twsa_analyses), 1)
  expect_equal(model$twsa_analyses[[1]]$name, "Cost vs Efficacy")
  expect_equal(model$twsa_analyses[[1]]$description, "Vary cost and efficacy together")
  expect_type(model$twsa_analyses[[1]]$parameters, "list")
  expect_equal(length(model$twsa_analyses[[1]]$parameters), 0)
})

test_that("add_twsa errors on empty name", {
  model <- define_model("markov")

  expect_error(
    add_twsa(model, ""),
    "non-empty character string"
  )
})

test_that("add_twsa errors on 'Base Case' name", {
  model <- define_model("markov")

  expect_error(
    add_twsa(model, "Base Case"),
    "reserved name"
  )

  # Case insensitive
  expect_error(
    add_twsa(model, "base case"),
    "reserved name"
  )
})

test_that("add_twsa errors on duplicate names", {
  model <- define_model("markov") %>%
    add_twsa("Cost vs Efficacy")

  expect_error(
    add_twsa(model, "Cost vs Efficacy"),
    "already exists"
  )
})

test_that("add_twsa_variable adds variable with range type", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5)

  expect_equal(length(model$twsa_analyses[[1]]$parameters), 1)
  param <- model$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$param_type, "variable")
  expect_equal(param$name, "cost")
  expect_equal(param$type, "range")
  expect_equal(param$steps, 5)
  expect_s3_class(param$min, "oq_formula")
  expect_s3_class(param$max, "oq_formula")
})

test_that("add_twsa_variable adds variable with radius type", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "efficacy", type = "radius", radius = 0.1, steps = 3)

  param <- model$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$type, "radius")
  expect_equal(param$steps, 3)
  expect_s3_class(param$radius, "oq_formula")
})

test_that("add_twsa_variable adds variable with custom type", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "discount", type = "custom", values = c(0, 3, 5))

  param <- model$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$type, "custom")
  expect_s3_class(param$values, "oq_formula")
})

test_that("add_twsa_variable errors for nonexistent TWSA", {
  model <- define_model("markov")

  expect_error(
    add_twsa_variable(model, "NonExistent", "var", type = "range",
                       min = 0, max = 1, steps = 5),
    "not found"
  )
})

test_that("add_twsa_variable enforces 2-parameter limit", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5) %>%
    add_twsa_variable("Test", "efficacy", type = "radius", radius = 0.1, steps = 3)

  expect_error(
    add_twsa_variable(model, "Test", "discount", type = "custom", values = c(0, 3, 5)),
    "already has 2 parameters"
  )
})

test_that("add_twsa_variable supports NSE for range expressions", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range",
                       min = base_cost * 0.5, max = base_cost * 1.5, steps = 5)

  param <- model$twsa_analyses[[1]]$parameters[[1]]
  expect_s3_class(param$min, "oq_formula")
  expect_s3_class(param$max, "oq_formula")
})

test_that("add_twsa_setting adds setting with range type", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3)

  expect_equal(length(model$twsa_analyses[[1]]$parameters), 1)
  param <- model$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$param_type, "setting")
  expect_equal(param$name, "discount_cost")
  expect_equal(param$type, "range")
  expect_equal(param$min, 0)
  expect_equal(param$max, 5)
  expect_equal(param$steps, 3)
})

test_that("add_twsa_setting adds setting with custom type", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "timeframe", type = "custom", values = c(10, 20, 30))

  param <- model$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$type, "custom")
  expect_equal(param$values, c(10, 20, 30))
})

test_that("add_twsa_setting errors for nonexistent TWSA", {
  model <- define_model("markov")

  expect_error(
    add_twsa_setting(model, "NonExistent", "timeframe", type = "range",
                      min = 10, max = 30, steps = 3),
    "not found"
  )
})

test_that("add_twsa_setting enforces 2-parameter limit", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3) %>%
    add_twsa_setting("Test", "timeframe", type = "range", min = 10, max = 30, steps = 3)

  expect_error(
    add_twsa_setting(model, "Test", "discount_outcomes", type = "range",
                      min = 0, max = 5, steps = 3),
    "already has 2 parameters"
  )
})

test_that("multiple TWSA analyses can be defined", {
  model <- define_model("markov") %>%
    add_twsa("Cost vs Efficacy") %>%
    add_twsa_variable("Cost vs Efficacy", "cost", type = "range",
                       min = 500, max = 1500, steps = 5) %>%
    add_twsa_variable("Cost vs Efficacy", "efficacy", type = "radius",
                       radius = 0.1, steps = 3) %>%
    add_twsa("Cost vs Discount") %>%
    add_twsa_variable("Cost vs Discount", "cost", type = "range",
                       min = 500, max = 1500, steps = 5) %>%
    add_twsa_setting("Cost vs Discount", "discount_cost", type = "custom",
                      values = c(0, 3, 5))

  expect_equal(length(model$twsa_analyses), 2)
  expect_equal(model$twsa_analyses[[1]]$name, "Cost vs Efficacy")
  expect_equal(model$twsa_analyses[[2]]$name, "Cost vs Discount")
  expect_equal(length(model$twsa_analyses[[1]]$parameters), 2)
  expect_equal(length(model$twsa_analyses[[2]]$parameters), 2)
})

# ============================================================================
# Range Type Parameter Validation Tests
# ============================================================================

test_that("add_twsa_variable errors when range type missing required params", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    add_twsa_variable(model, "Test", "cost", type = "range", min = 500, max = 1500),
    "min, max, and steps are required"
  )

  expect_error(
    add_twsa_variable(model, "Test", "cost", type = "range", min = 500, steps = 5),
    "min, max, and steps are required"
  )
})

test_that("add_twsa_variable errors when radius type missing required params", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    add_twsa_variable(model, "Test", "efficacy", type = "radius", radius = 0.1),
    "radius and steps are required"
  )

  expect_error(
    add_twsa_variable(model, "Test", "efficacy", type = "radius", steps = 5),
    "radius and steps are required"
  )
})

test_that("add_twsa_variable errors when custom type missing values", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    add_twsa_variable(model, "Test", "discount", type = "custom"),
    "values is required"
  )
})

# ============================================================================
# Validation Tests
# ============================================================================

test_that("validate_twsa_spec catches missing TWSA analyses", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_twsa_spec(parsed),
    "No TWSA analyses defined"
  )
})

test_that("validate_twsa_spec catches TWSA with wrong number of parameters", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5)
  # Only 1 parameter added, need 2

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_twsa_spec(parsed),
    "must have exactly 2 parameters"
  )
})

test_that("validate_twsa_spec catches invalid variable names", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "nonexistent_var", type = "range",
                       min = 500, max = 1500, steps = 5) %>%
    add_twsa_variable("Test", "cost", type = "range",
                       min = 500, max = 1500, steps = 5)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_twsa_spec(parsed),
    "not found in model variables"
  )
})

test_that("validate_twsa_spec catches invalid setting names", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range",
                       min = 500, max = 1500, steps = 5) %>%
    add_twsa_setting("Test", "invalid_setting", type = "range",
                      min = 0, max = 5, steps = 3)

  finalized <- normalize_and_validate_model(model, preserve_builder = FALSE)
  parsed <- parse_model(finalized)

  expect_error(
    validate_twsa_spec(parsed),
    "invalid setting name"
  )
})

# ============================================================================
# Value Generation Tests
# ============================================================================

test_that("generate_twsa_values creates correct range sequence", {
  param <- list(
    param_type = "variable",
    type = "range",
    name = "cost",
    min = as.oq_formula("500"),
    max = as.oq_formula("1500"),
    steps = 5
  )

  values <- generate_twsa_values(param, namespace = NULL, settings = NULL)

  expect_equal(length(values), 5)
  expect_equal(values[1], 500)
  expect_equal(values[5], 1500)
})

test_that("generate_twsa_values creates correct custom values", {
  param <- list(
    param_type = "setting",
    type = "custom",
    name = "discount_cost",
    values = c(0, 3, 5, 7)
  )

  values <- generate_twsa_values(param, namespace = NULL, settings = list(discount_cost = 3))

  expect_equal(values, c(0, 3, 5, 7))
})

# ============================================================================
# Print Function Tests
# ============================================================================

test_that("print_twsa displays TWSA analyses correctly", {
  model <- define_model("markov") %>%
    add_twsa("Cost vs Efficacy", description = "Test analysis") %>%
    add_twsa_variable("Cost vs Efficacy", "cost", type = "range",
                       min = 500, max = 1500, steps = 5) %>%
    add_twsa_variable("Cost vs Efficacy", "efficacy", type = "radius",
                       radius = 0.1, steps = 3)

  output <- capture.output(print_twsa(model))

  expect_true(any(grepl("Two-Way Sensitivity Analyses", output)))
  expect_true(any(grepl("Cost vs Efficacy", output)))
  expect_true(any(grepl("Parameters: 2/2", output)))
})

test_that("print_twsa handles empty model", {
  model <- define_model("markov")

  output <- capture.output(print_twsa(model))

  expect_true(any(grepl("No TWSA analyses defined", output)))
})
