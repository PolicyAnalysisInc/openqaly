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
# Edit/Remove TWSA Tests
# ============================================================================

test_that("edit_twsa renames analysis", {
  model <- define_model("markov") %>%
    add_twsa("Cost vs Efficacy")

  model2 <- edit_twsa(model, "Cost vs Efficacy", new_name = "Price vs Effect")

  expect_equal(model2$twsa_analyses[[1]]$name, "Price vs Effect")
})

test_that("edit_twsa auto-updates description when it matched old name", {
  model <- define_model("markov") %>%
    add_twsa("Cost vs Efficacy")

  expect_equal(model$twsa_analyses[[1]]$description, "Cost vs Efficacy")

  model2 <- edit_twsa(model, "Cost vs Efficacy", new_name = "Price vs Effect")
  expect_equal(model2$twsa_analyses[[1]]$description, "Price vs Effect")
})

test_that("edit_twsa does not auto-update description when explicitly set", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  model2 <- edit_twsa(model, "Test", new_name = "New Name",
                       description = "Custom desc")
  expect_equal(model2$twsa_analyses[[1]]$description, "Custom desc")
})

test_that("edit_twsa errors on Base Case name", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    edit_twsa(model, "Test", new_name = "Base Case"),
    "reserved"
  )
})

test_that("edit_twsa errors on duplicate name", {
  model <- define_model("markov") %>%
    add_twsa("A") %>%
    add_twsa("B")

  expect_error(
    edit_twsa(model, "A", new_name = "B"),
    "already exists"
  )
})

test_that("edit_twsa errors for nonexistent analysis", {
  model <- define_model("markov")

  expect_error(
    edit_twsa(model, "NonExistent", new_name = "X"),
    "not found"
  )
})

test_that("edit_twsa updates description only", {
  model <- define_model("markov") %>%
    add_twsa("Test", description = "Old desc")

  model2 <- edit_twsa(model, "Test", description = "New desc")
  expect_equal(model2$twsa_analyses[[1]]$description, "New desc")
  expect_equal(model2$twsa_analyses[[1]]$name, "Test")
})

test_that("edit_twsa_variable updates steps", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5)

  model2 <- edit_twsa_variable(model, "Test", "cost", steps = 10)

  expect_equal(model2$twsa_analyses[[1]]$parameters[[1]]$steps, 10)
})

test_that("edit_twsa_variable changes type with required params", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5)

  model2 <- edit_twsa_variable(model, "Test", "cost", type = "custom",
                                values = c(500, 1000, 1500))

  param <- model2$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$type, "custom")
  expect_s3_class(param$values, "oq_formula")
  expect_null(param$min)
  expect_null(param$max)
})

test_that("edit_twsa_variable errors when changing type without required params", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5)

  expect_error(
    edit_twsa_variable(model, "Test", "cost", type = "radius"),
    "radius and steps are required"
  )
})

test_that("edit_twsa_variable re-keys variable", {
  model <- define_model("markov") %>%
    add_variable("cost", 1000) %>%
    add_variable("price", 2000) %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5)

  model2 <- edit_twsa_variable(model, "Test", "cost", new_variable = "price")

  expect_equal(model2$twsa_analyses[[1]]$parameters[[1]]$name, "price")
})

test_that("edit_twsa_variable errors for nonexistent parameter", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    edit_twsa_variable(model, "Test", "nonexistent", steps = 10),
    "not found"
  )
})

test_that("edit_twsa_variable errors for nonexistent TWSA", {
  model <- define_model("markov")

  expect_error(
    edit_twsa_variable(model, "NonExistent", "var", steps = 10),
    "not found"
  )
})

test_that("edit_twsa_setting updates value fields", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3)

  model2 <- edit_twsa_setting(model, "Test", "discount_cost", min = 1, max = 4)

  param <- model2$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$min, 1)
  expect_equal(param$max, 4)
  expect_equal(param$steps, 3)  # Unchanged
})

test_that("edit_twsa_setting re-keys setting", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3)

  model2 <- edit_twsa_setting(model, "Test", "discount_cost",
                               new_setting = "discount_outcomes")

  expect_equal(model2$twsa_analyses[[1]]$parameters[[1]]$name, "discount_outcomes")
})

test_that("edit_twsa_setting auto-updates display_name on re-key", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3)

  # display_name defaults to setting name
  expect_equal(model$twsa_analyses[[1]]$parameters[[1]]$display_name, "discount_cost")

  model2 <- edit_twsa_setting(model, "Test", "discount_cost",
                               new_setting = "discount_outcomes")
  expect_equal(model2$twsa_analyses[[1]]$parameters[[1]]$display_name, "discount_outcomes")
})

test_that("edit_twsa_setting errors on invalid setting name", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3)

  expect_error(
    edit_twsa_setting(model, "Test", "discount_cost",
                      new_setting = "invalid_setting"),
    "Invalid TWSA setting"
  )
})

test_that("edit_twsa_setting errors on duplicate setting", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3) %>%
    add_twsa_setting("Test", "discount_outcomes", type = "range", min = 0, max = 5, steps = 3)

  expect_error(
    edit_twsa_setting(model, "Test", "discount_cost",
                      new_setting = "discount_outcomes"),
    "already exists"
  )
})

test_that("edit_twsa_setting changes type with required params", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3)

  model2 <- edit_twsa_setting(model, "Test", "discount_cost",
                               type = "custom", values = c(0, 3, 5))

  param <- model2$twsa_analyses[[1]]$parameters[[1]]
  expect_equal(param$type, "custom")
  expect_equal(param$values, c(0, 3, 5))
  expect_null(param$min)
})

test_that("edit_twsa_setting errors for nonexistent parameter", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    edit_twsa_setting(model, "Test", "nonexistent", steps = 10),
    "not found"
  )
})

test_that("remove_twsa removes analysis", {
  model <- define_model("markov") %>%
    add_twsa("A") %>%
    add_twsa("B")

  model2 <- remove_twsa(model, "A")

  expect_equal(length(model2$twsa_analyses), 1)
  expect_equal(model2$twsa_analyses[[1]]$name, "B")
})

test_that("remove_twsa errors for nonexistent analysis", {
  model <- define_model("markov")

  expect_error(
    remove_twsa(model, "NonExistent"),
    "not found"
  )
})

test_that("remove_twsa_variable removes parameter", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_variable("Test", "cost", type = "range", min = 500, max = 1500, steps = 5) %>%
    add_twsa_variable("Test", "efficacy", type = "radius", radius = 0.1, steps = 3)

  model2 <- remove_twsa_variable(model, "Test", "cost")

  expect_equal(length(model2$twsa_analyses[[1]]$parameters), 1)
  expect_equal(model2$twsa_analyses[[1]]$parameters[[1]]$name, "efficacy")
})

test_that("remove_twsa_variable errors for nonexistent parameter", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    remove_twsa_variable(model, "Test", "nonexistent"),
    "not found"
  )
})

test_that("remove_twsa_variable errors for nonexistent TWSA", {
  model <- define_model("markov")

  expect_error(
    remove_twsa_variable(model, "NonExistent", "var"),
    "not found"
  )
})

test_that("remove_twsa_setting removes parameter", {
  model <- define_model("markov") %>%
    add_twsa("Test") %>%
    add_twsa_setting("Test", "discount_cost", type = "range", min = 0, max = 5, steps = 3) %>%
    add_twsa_setting("Test", "discount_outcomes", type = "range", min = 0, max = 5, steps = 3)

  model2 <- remove_twsa_setting(model, "Test", "discount_cost")

  expect_equal(length(model2$twsa_analyses[[1]]$parameters), 1)
  expect_equal(model2$twsa_analyses[[1]]$parameters[[1]]$name, "discount_outcomes")
})

test_that("remove_twsa_setting errors for nonexistent parameter", {
  model <- define_model("markov") %>%
    add_twsa("Test")

  expect_error(
    remove_twsa_setting(model, "Test", "nonexistent"),
    "not found"
  )
})

test_that("remove_twsa_setting errors for nonexistent TWSA", {
  model <- define_model("markov")

  expect_error(
    remove_twsa_setting(model, "NonExistent", "timeframe"),
    "not found"
  )
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

# ============================================================================
# Tests for twsa_costs_table()
# ============================================================================

test_that("twsa_costs_table() returns a rendered table", {
  results <- get_cached_twsa_vbp_results()
  tbl <- twsa_costs_table(results, "costs", backend = "kable")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

# ============================================================================
# Tests for twsa_costs_plot()
# ============================================================================

test_that("twsa_costs_plot() returns ggplot object", {
  results <- get_cached_twsa_vbp_results()
  p <- twsa_costs_plot(results, "costs")
  expect_s3_class(p, "ggplot")
})

test_that("render_twsa_heatmap uses unique auto-formatted axis labels", {
  heatmap_data <- tibble::tibble(
    x_value = c(0.775, 0.8, 0.825),
    y_value = c(0.5, 0.5, 0.5),
    value = c(1, 2, 3),
    strategy = "Strategy A",
    group = "Overall",
    x_param_display_name = "Utility (Mild)",
    y_param_display_name = "Utility (Severe)"
  )

  p <- openqaly:::render_twsa_heatmap(heatmap_data, show_base_case = FALSE)

  expect_equal(length(unique(levels(p$data$x_factor))), 3)
})

test_that("render_twsa_heatmap includes legend ticks at both scale endpoints", {
  heatmap_data <- tibble::tibble(
    x_value = rep(c(0.775, 0.8, 0.825), each = 2),
    y_value = rep(c(0.5, 0.55), times = 3),
    value = c(0.207, 0.24, 0.272, 0.304, 0.337, 0.456),
    strategy = "Strategy A",
    group = "Overall",
    x_param_display_name = "Utility (Mild)",
    y_param_display_name = "Utility (Severe)"
  )

  p <- openqaly:::render_twsa_heatmap(heatmap_data, show_base_case = FALSE)
  built <- ggplot2::ggplot_build(p)
  breaks <- built$plot$scales$get_scales("fill")$get_breaks()

  expect_equal(breaks[1], min(heatmap_data$value))
  expect_equal(breaks[length(breaks)], max(heatmap_data$value))
})

test_that("render_twsa_heatmap keeps endpoint ticks for narrow ranges", {
  heatmap_data <- tibble::tibble(
    x_value = rep(c(0.775, 0.8, 0.825), each = 2),
    y_value = rep(c(0.5, 0.55), times = 3),
    value = c(10.51, 10.56, 10.62, 10.68, 10.74, 10.79),
    strategy = "Strategy A",
    group = "Overall",
    x_param_display_name = "Utility (Mild)",
    y_param_display_name = "Utility (Severe)"
  )

  p <- openqaly:::render_twsa_heatmap(heatmap_data, show_base_case = FALSE)
  built <- ggplot2::ggplot_build(p)
  breaks <- built$plot$scales$get_scales("fill")$get_breaks()

  expect_equal(breaks[1], min(heatmap_data$value))
  expect_equal(breaks[length(breaks)], max(heatmap_data$value))
})

test_that("render_twsa_ce_heatmap uses auto-precision for numeric legend labels", {
  ce_data <- tibble::tibble(
    x_value = c(0.775, 0.8, 0.825),
    y_value = c(0.5, 0.5, 0.5),
    icer = c(1000.01, 1000.02, 2000),
    display_value = c(1000.01, 1000.02, 2000),
    ce_class = c("regular", "regular", "regular"),
    strategy = "Strategy A",
    group = "Overall",
    x_param_display_name = "Utility (Mild)",
    y_param_display_name = "Utility (Severe)"
  )

  p <- openqaly:::render_twsa_ce_heatmap(ce_data, show_base_case = FALSE)
  labels <- p$scales$get_scales("fill")$labels(c(1000.01, 1000.02, 2000))

  expect_equal(length(unique(labels)), 3)
  expect_true(any(grepl("\\.", labels)))
})

test_that("render_twsa_ce_heatmap includes endpoint legend ticks with labels", {
  ce_data <- tibble::tibble(
    x_value = rep(c(0.775, 0.8, 0.825), each = 2),
    y_value = rep(c(0.5, 0.55), times = 3),
    icer = c(1200, 1600, 2200, 2800, 3400, 4000),
    display_value = c(1200, 1600, 2200, 2800, 3400, 4000),
    ce_class = rep("regular", 6),
    strategy = "Strategy A",
    group = "Overall",
    x_param_display_name = "Utility (Mild)",
    y_param_display_name = "Utility (Severe)"
  )

  p <- openqaly:::render_twsa_ce_heatmap(ce_data, show_base_case = FALSE)
  built <- ggplot2::ggplot_build(p)
  fill_scale <- built$plot$scales$get_scales("fill")
  breaks <- fill_scale$get_breaks()
  labels <- fill_scale$get_labels(breaks)

  expect_equal(breaks[1], min(ce_data$display_value))
  expect_equal(breaks[length(breaks)], max(ce_data$display_value))
  expect_false(any(is.na(labels)))
  expect_true(all(nzchar(labels)))
})

test_that("cost-sensitive TWSA cost plots vary across the grid", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("seritinib") |>
    add_strategy("volantor") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_sick", 0.1) |>
    add_variable("p_death_healthy", 0.02) |>
    add_variable("p_death_sick", 0.15) |>
    add_variable("c_healthy", 1000) |>
    add_variable("c_sick", 5000) |>
    add_variable("c_treatment", 500, strategy = "seritinib") |>
    add_variable("c_treatment", 3000, strategy = "volantor") |>
    add_variable("u_mild", 0.85) |>
    add_variable("u_severe", 0.55) |>
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death_healthy") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death_healthy") |>
    add_transition("sick", "dead", "p_death_sick") |>
    add_transition("sick", "sick", "1 - p_death_sick") |>
    add_transition("dead", "dead", "1") |>
    add_value("cost", "c_healthy + c_treatment", state = "healthy", type = "cost") |>
    add_value("cost", "c_sick + c_treatment", state = "sick", type = "cost") |>
    add_value("cost", "0", state = "dead", type = "cost") |>
    add_value("qalys", "u_mild", state = "healthy", type = "outcome") |>
    add_value("qalys", "u_severe", state = "sick", type = "outcome") |>
    add_value("qalys", "0", state = "dead", type = "outcome") |>
    add_summary("qalys", "qalys", type = "outcome") |>
    add_summary("costs", "cost", type = "cost") |>
    add_twsa("Cost Sensitivity") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment",
      type = "custom", values = c(1500, 3000, 4500),
      display_name = "Treatment Cost", strategy = "volantor") |>
    add_twsa_variable("Cost Sensitivity", "c_sick",
      type = "custom", values = c(3500, 5000, 6500),
      display_name = "Cost (Sick)")

  results <- run_twsa(model)
  plot_data <- openqaly:::prepare_twsa_outcomes_data(
    results = results,
    summary_name = "costs",
    twsa_name = "Cost Sensitivity",
    groups = "overall",
    discounted = TRUE
  )

  expect_gt(dplyr::n_distinct(plot_data$value), 1)
})
