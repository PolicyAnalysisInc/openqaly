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
# Strategy/Group Targeting Validation Tests
# ============================================================================

test_that("add_twsa_variable errors when group-specific variable used without specifying group", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000, group = "young") %>%
    add_variable("cost", 2000, group = "old") %>%
    add_twsa("Test")

  expect_error(
    add_twsa_variable(model, "Test", "cost", type = "range", min = 500, max = 1500, steps = 3),
    "defined for specific group"
  )

  # Should work when group is specified
  model_ok <- add_twsa_variable(model, "Test", "cost", type = "range",
                                 min = 500, max = 1500, steps = 3, group = "young")
  expect_equal(model_ok$twsa_analyses[[1]]$parameters[[1]]$group, "young")
})

test_that("add_twsa_variable errors when strategy-specific variable used without specifying strategy", {
  model <- define_model("markov") %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_variable("c_treatment", 1000, strategy = "treatment") %>%
    add_twsa("Test")

  expect_error(
    add_twsa_variable(model, "Test", "c_treatment", type = "range", min = 500, max = 1500, steps = 3),
    "defined for specific strategy"
  )

  # Should work when strategy is specified
  model_ok <- add_twsa_variable(model, "Test", "c_treatment", type = "range",
                                 min = 500, max = 1500, steps = 3, strategy = "treatment")
  expect_equal(model_ok$twsa_analyses[[1]]$parameters[[1]]$strategy, "treatment")
})

test_that("add_twsa_variable allows non-specific variables without strategy/group", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_twsa("Test")

  # Should work without specifying strategy or group
  model_ok <- add_twsa_variable(model, "Test", "cost", type = "range",
                                 min = 500, max = 1500, steps = 3)
  expect_equal(model_ok$twsa_analyses[[1]]$parameters[[1]]$strategy, "")
  expect_equal(model_ok$twsa_analyses[[1]]$parameters[[1]]$group, "")
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

# ============================================================================
# Strategy-Specific Parameter Tests
# ============================================================================

test_that("build_twsa_segments uses correct bc values for strategy-specific parameters", {
  # Use example model which has strategy-specific c_treatment
  # c_treatment is 500 for seritinib, 3000 for volantor, 5000 for cendralimab
  model_path <- system.file("models", "example_markov", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_markov"
  }
  model <- read_model(model_path)

  # Add TWSA with strategy-specific parameter using bc-relative bounds
  model <- model |>
    add_twsa("Cost Sensitivity") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment",
                      type = "range", min = bc * 0.5, max = bc * 1.5, steps = 3,
                      strategy = "volantor") |>
    add_twsa_variable("Cost Sensitivity", "u_mild",
                      type = "range", min = 0.7, max = 0.9, steps = 3)

  parsed_model <- parse_model(model)
  segments <- build_twsa_segments(parsed_model)

  # Filter to non-base-case segments
  twsa_segs <- segments %>% filter(!is.na(twsa_name))

  # Check volantor segments have correct c_treatment range (bc=3000)
  volantor_segs <- twsa_segs %>% filter(strategy == "volantor")
  volantor_x_values <- unique(volantor_segs$x_value)

  # Expected: 1500, 3000, 4500 (bc * 0.5, bc, bc * 1.5 where bc = 3000)
  expect_true(1500 %in% volantor_x_values)
  expect_true(3000 %in% volantor_x_values)
  expect_true(4500 %in% volantor_x_values)

  # Check x_bc_value is correct for volantor
  expect_equal(unique(volantor_segs$x_bc_value), 3000)

  # Check that volantor segments have c_treatment in parameter_overrides
  volantor_first <- volantor_segs[1, ]
  expect_true("c_treatment" %in% names(volantor_first$parameter_overrides[[1]]))

  # Check that seritinib segments do NOT have c_treatment in parameter_overrides
  # (since c_treatment with strategy="volantor" doesn't apply to seritinib)
  seritinib_segs <- twsa_segs %>% filter(strategy == "seritinib")
  seritinib_first <- seritinib_segs[1, ]
  expect_false("c_treatment" %in% names(seritinib_first$parameter_overrides[[1]]))

  # seritinib should only have u_mild overrides
  expect_true("u_mild" %in% names(seritinib_first$parameter_overrides[[1]]))
})
