context("Validation functions")

# ==============================================================================
# Helper Functions
# ==============================================================================

# Create realistic valid cpp inputs matching markov.r:420-435 structure
create_valid_cpp_inputs <- function() {
  list(
    init = c(healthy = 1, sick = 0, dead = 0),
    transitions = matrix(
      c(0.9, 0.08, 0.02,
        0,   0.85, 0.15,
        0,   0,    1),
      nrow = 3, byrow = TRUE,
      dimnames = list(c("healthy", "sick", "dead"), NULL)
    ),
    values = data.frame(
      state = c("healthy", "sick", "dead"),
      destination = NA_character_,
      cost = c(100, 500, 0)
    ),
    value_names = "cost",
    state_names = c("healthy", "sick", "dead"),
    expanded_state_map = data.frame(
      state = c("healthy", "sick", "dead"),
      original_state = c("healthy", "sick", "dead")
    ),
    half_cycle_method = "start"
  )
}

# Create realistic metadata matching actual model output structure
create_mock_metadata <- function() {
  list(
    summaries = data.frame(
      name = c("total_cost", "total_qaly"),
      display_name = c("Total Cost", "Total QALYs"),
      description = c("Sum of all costs", "Sum of all QALYs"),
      type = c("cost", "outcome"),
      wtp = c(NA, 50000),
      values = c("cost", "qaly"),
      stringsAsFactors = FALSE
    ),
    strategies = data.frame(
      name = c("standard", "intervention"),
      display_name = c("Standard Care", "Intervention"),
      description = c("No treatment", "New treatment"),
      stringsAsFactors = FALSE
    ),
    groups = data.frame(
      name = c("male", "female"),
      display_name = c("Male", "Female"),
      weight = c(0.5, 0.5),
      stringsAsFactors = FALSE
    )
  )
}

create_mock_results <- function() {
  list(
    metadata = create_mock_metadata(),
    segments = data.frame(
      group = c("male", "female"),
      strategy = c("standard", "standard"),
      stringsAsFactors = FALSE
    )
  )
}

# ==============================================================================
# validate_numeric_result() tests
# ==============================================================================

test_that("validate_numeric_result returns numeric values unchanged", {
  expect_equal(validate_numeric_result(42, "test context"), 42)
  expect_equal(validate_numeric_result(3.14, "test context"), 3.14)
  expect_equal(validate_numeric_result(c(1, 2, 3), "test context"), c(1, 2, 3))
})

test_that("validate_numeric_result returns integer values unchanged", {
  expect_equal(validate_numeric_result(42L, "test context"), 42L)
  expect_equal(validate_numeric_result(c(1L, 2L), "test context"), c(1L, 2L))
})

test_that("validate_numeric_result rejects character strings with clear message", {
  err <- expect_error(
    validate_numeric_result("hello", "test context", "my_formula")
  )
  expect_match(err$message, "character string 'hello'")
  expect_match(err$message, "test context")
  expect_match(err$message, "expected numeric")
})

test_that("validate_numeric_result detects undefined variable when value matches formula", {
  err <- expect_error(
    validate_numeric_result("undefined_var", "test context", "undefined_var")
  )
  expect_match(err$message, "undefined variable 'undefined_var'")
  expect_match(err$message, "Check that variable 'undefined_var' is defined")
})

test_that("validate_numeric_result rejects logical values", {
  err <- expect_error(
    validate_numeric_result(TRUE, "test context", "my_formula")
  )
  expect_match(err$message, "logical value 'TRUE'")
  expect_match(err$message, "expected numeric")

  err2 <- expect_error(
    validate_numeric_result(FALSE, "test context", "my_formula")
  )
  expect_match(err2$message, "logical value 'FALSE'")
})

test_that("validate_numeric_result rejects list objects", {
  err <- expect_error(
    validate_numeric_result(list(a = 1), "test context", "my_formula")
  )
  expect_match(err$message, "list object")
  expect_match(err$message, "expected numeric")
})

test_that("validate_numeric_result provides quoted string hint", {
  err <- expect_error(
    validate_numeric_result("100", "test context", "'100'")
  )
  expect_match(err$message, "quoted string")
  expect_match(err$message, "Remove quotes")
})

test_that("validate_numeric_result suggests numeric conversion for string numbers", {
  err <- expect_error(
    validate_numeric_result("100", "test context", "some_var")
  )
  expect_match(err$message, "evaluated to string '100'")
  expect_match(err$message, "Use numeric 100 instead")
})

test_that("validate_numeric_result handles empty character vector", {
  err <- expect_error(
    validate_numeric_result(character(0), "test context", "my_formula")
  )
  expect_match(err$message, "empty character vector")
})

# ==============================================================================
# validate_transition_result() tests
# ==============================================================================

test_that("validate_transition_result uses trans_name in context when provided", {
  err <- expect_error(
    validate_transition_result("bad", "A", "B", trans_name = "my_transition", formula_text = "x")
  )
  expect_match(err$message, "Transition 'my_transition'")
})

test_that("validate_transition_result falls back to state names when no trans_name", {
  err <- expect_error(
    validate_transition_result("bad", "healthy", "sick", trans_name = NULL, formula_text = "x")
  )
  expect_match(err$message, "Transition from 'healthy' to 'sick'")
})

test_that("validate_transition_result returns valid numeric unchanged", {
  expect_equal(
    validate_transition_result(0.5, "A", "B", trans_name = "test"),
    0.5
  )
})

# ==============================================================================
# validate_value_result() tests
# ==============================================================================

test_that("validate_value_result includes value_name in context", {
  err <- expect_error(
    validate_value_result("bad", "my_cost", formula_text = "x")
  )
  expect_match(err$message, "Value 'my_cost'")
})

test_that("validate_value_result appends state info when provided", {
  err <- expect_error(
    validate_value_result("bad", "my_cost", state = "healthy", formula_text = "x")
  )
  expect_match(err$message, "Value 'my_cost'")
  expect_match(err$message, "in state 'healthy'")
})

test_that("validate_value_result appends destination info when provided", {
  err <- expect_error(
    validate_value_result("bad", "my_cost", destination = "sick", formula_text = "x")
  )
  expect_match(err$message, "Value 'my_cost'")
  expect_match(err$message, "to destination 'sick'")
})

test_that("validate_value_result returns valid numeric unchanged", {
  expect_equal(
    validate_value_result(100, "cost", state = "healthy"),
    100
  )
})

# ==============================================================================
# validate_cpp_inputs() tests - init validation
# ==============================================================================

test_that("validate_cpp_inputs accepts valid inputs", {
  inputs <- create_valid_cpp_inputs()
  result <- do.call(validate_cpp_inputs, inputs)
  expect_true(result)
})

test_that("validate_cpp_inputs rejects non-numeric init", {
  inputs <- create_valid_cpp_inputs()
  inputs$init <- c("a", "b", "c")
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "must be numeric")
})

test_that("validate_cpp_inputs shows state names for NA positions", {
  inputs <- create_valid_cpp_inputs()
  inputs$init <- c(healthy = 0.5, sick = NA, dead = 0.5)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "NA values")
  expect_match(err$message, "sick")
})

test_that("validate_cpp_inputs falls back to positions when no state names", {

  inputs <- create_valid_cpp_inputs()
  inputs$init <- c(0.5, NA, 0.5)  # No names
  inputs$state_names <- c("A", "B")  # Mismatched length to trigger position fallback
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "NA values at positions")
  expect_match(err$message, "2")
})

test_that("validate_cpp_inputs shows state names for Inf values", {
  inputs <- create_valid_cpp_inputs()
  inputs$init <- c(healthy = 0.5, sick = Inf, dead = 0.5)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "non-finite values")
  expect_match(err$message, "sick")
})

test_that("validate_cpp_inputs shows state and value for negative probabilities", {
  inputs <- create_valid_cpp_inputs()
  inputs$init <- c(healthy = -0.1, sick = 0.6, dead = 0.5)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "non-negative")
  expect_match(err$message, "healthy")
  expect_match(err$message, "-0.1")
})

test_that("validate_cpp_inputs requires init sum to 1", {
  inputs <- create_valid_cpp_inputs()
  inputs$init <- c(healthy = 0.3, sick = 0.3, dead = 0.3)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "must sum to 1")
  expect_match(err$message, "0.9")
})

# ==============================================================================
# validate_cpp_inputs() tests - transitions validation
# ==============================================================================

test_that("validate_cpp_inputs rejects non-matrix transitions", {
  inputs <- create_valid_cpp_inputs()
  inputs$transitions <- list(a = 1, b = 2)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "must be a matrix or data frame")
})

test_that("validate_cpp_inputs rejects non-numeric transition values", {
  inputs <- create_valid_cpp_inputs()
  inputs$transitions <- matrix(c("a", "b", "c", "d"), nrow = 2)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "non-numeric values")
})

test_that("validate_cpp_inputs shows row numbers for NA in transitions", {
  inputs <- create_valid_cpp_inputs()
  inputs$transitions[2, 1] <- NA
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "NA values")
  expect_match(err$message, "row")
  expect_match(err$message, "2")
})

test_that("validate_cpp_inputs rejects Inf in transitions", {
  inputs <- create_valid_cpp_inputs()
  inputs$transitions[1, 1] <- Inf
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "non-finite values")
})

# ==============================================================================
# validate_cpp_inputs() tests - values validation
# ==============================================================================

test_that("validate_cpp_inputs rejects non-data.frame values", {
  inputs <- create_valid_cpp_inputs()
  inputs$values <- list(a = 1, b = 2)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "must be a data frame")
})

test_that("validate_cpp_inputs requires state and destination columns", {
  inputs <- create_valid_cpp_inputs()
  inputs$values <- data.frame(cost = c(100, 200))
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "missing required columns")
  expect_match(err$message, "state")
  expect_match(err$message, "destination")
})

# ==============================================================================
# validate_cpp_inputs() tests - character vectors
# ==============================================================================

test_that("validate_cpp_inputs rejects non-character value_names", {
  inputs <- create_valid_cpp_inputs()
  inputs$value_names <- 123
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "value_names must be a character vector")
})

test_that("validate_cpp_inputs rejects NA in value_names", {
  inputs <- create_valid_cpp_inputs()
  inputs$value_names <- c("cost", NA)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "value_names contains NA")
})

test_that("validate_cpp_inputs rejects non-character state_names", {
  inputs <- create_valid_cpp_inputs()
  inputs$state_names <- 123
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "state_names must be a character vector")
})

test_that("validate_cpp_inputs rejects NA in state_names", {
  inputs <- create_valid_cpp_inputs()
  inputs$state_names <- c("healthy", NA, "dead")
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "state_names contains NA")
})

# ==============================================================================
# validate_cpp_inputs() tests - other validations
# ==============================================================================

test_that("validate_cpp_inputs rejects non-data.frame expanded_state_map", {
  inputs <- create_valid_cpp_inputs()
  inputs$expanded_state_map <- list(a = 1)
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "expanded_state_map must be a data frame")
})

test_that("validate_cpp_inputs rejects invalid half_cycle_method", {
  inputs <- create_valid_cpp_inputs()
  inputs$half_cycle_method <- "invalid_method"
  err <- expect_error(do.call(validate_cpp_inputs, inputs))
  expect_match(err$message, "half_cycle_method must be one of")
  expect_match(err$message, "start")
  expect_match(err$message, "end")
  expect_match(err$message, "life-table")
})

# ==============================================================================
# check_summary_exists() tests
# ==============================================================================

test_that("check_summary_exists returns TRUE for valid summary", {
  metadata <- create_mock_metadata()
  result <- openqaly:::check_summary_exists("total_cost", metadata)
  expect_true(result)
})

test_that("check_summary_exists throws error for missing summary", {
  metadata <- create_mock_metadata()
  err <- expect_error(openqaly:::check_summary_exists("nonexistent", metadata))
  expect_match(err$message, "Summary 'nonexistent' was not found")
})

test_that("check_summary_exists shows markdown table with available summaries", {
  metadata <- create_mock_metadata()
  err <- expect_error(openqaly:::check_summary_exists("nonexistent", metadata))
  expect_match(err$message, "Available summaries")
  expect_match(err$message, "\\|")  # Markdown table
  expect_match(err$message, "total_cost")
  expect_match(err$message, "total_qaly")
})

test_that("check_summary_exists handles NULL metadata", {
  err <- expect_error(openqaly:::check_summary_exists("any", NULL))
  expect_match(err$message, "No summary metadata available")
})

# ==============================================================================
# check_strategies_exist() tests
# ==============================================================================

test_that("check_strategies_exist returns TRUE for valid strategies", {
  metadata <- create_mock_metadata()
  result <- openqaly:::check_strategies_exist(c("standard", "intervention"), metadata)
  expect_true(result)
})

test_that("check_strategies_exist lists missing strategies in error", {
  metadata <- create_mock_metadata()
  err <- expect_error(
    openqaly:::check_strategies_exist(c("standard", "nonexistent", "also_missing"), metadata)
  )
  expect_match(err$message, "Strategies not found")
  expect_match(err$message, "nonexistent")
  expect_match(err$message, "also_missing")
})

test_that("check_strategies_exist shows markdown table with available strategies", {
  metadata <- create_mock_metadata()
  err <- expect_error(openqaly:::check_strategies_exist("nonexistent", metadata))
  expect_match(err$message, "Available strategies")
  expect_match(err$message, "\\|")  # Markdown table
  expect_match(err$message, "standard")
  expect_match(err$message, "intervention")
})

test_that("check_strategies_exist handles NULL metadata", {
  err <- expect_error(openqaly:::check_strategies_exist("any", NULL))
  expect_match(err$message, "No strategy metadata available")
})

# ==============================================================================
# check_group_exists() tests
# ==============================================================================

test_that("check_group_exists accepts keyword 'overall'", {
  results <- create_mock_results()
  result <- openqaly:::check_group_exists("overall", results)
  expect_true(result)
})

test_that("check_group_exists accepts keyword 'all'", {
  results <- create_mock_results()
  result <- openqaly:::check_group_exists("all", results)
  expect_true(result)
})

test_that("check_group_exists accepts keyword 'all_groups'", {
  results <- create_mock_results()
  result <- openqaly:::check_group_exists("all_groups", results)
  expect_true(result)
})

test_that("check_group_exists validates against segments data", {
  results <- create_mock_results()
  result <- openqaly:::check_group_exists("male", results)
  expect_true(result)
})

test_that("check_group_exists throws informative error for missing group", {
  results <- create_mock_results()
  err <- expect_error(openqaly:::check_group_exists("nonexistent", results))
  expect_match(err$message, "Group 'nonexistent' not found")
  expect_match(err$message, "Available groups")
  expect_match(err$message, "male")
  expect_match(err$message, "female")
})

# ==============================================================================
# check_groups_exist() tests
# ==============================================================================

test_that("check_groups_exist returns TRUE for valid groups", {
  results <- create_mock_results()
  result <- openqaly:::check_groups_exist(c("male", "female"), results)
  expect_true(result)
})

test_that("check_groups_exist lists missing groups and shows keywords help", {
  results <- create_mock_results()
  err <- expect_error(openqaly:::check_groups_exist(c("male", "nonexistent"), results))
  expect_match(err$message, "Groups not found")
  expect_match(err$message, "nonexistent")
  expect_match(err$message, "overall")  # Keywords
  expect_match(err$message, "all_groups")  # Keywords
})

test_that("check_groups_exist shows table with display_name and weight", {
  results <- create_mock_results()
  err <- expect_error(openqaly:::check_groups_exist("nonexistent", results))
  expect_match(err$message, "\\|")  # Markdown table
  expect_match(err$message, "male")
  expect_match(err$message, "female")
})

# ==============================================================================
# error.R additional tests
# ==============================================================================

test_that("is_oq_dependency_error identifies dependency errors", {
  dep_err <- openqaly:::define_dependency_error("test message")
  regular_err <- openqaly:::define_error("test message")

  expect_true(is_oq_dependency_error(dep_err))
  expect_false(is_oq_dependency_error(regular_err))
  expect_false(is_oq_dependency_error(list(message = "test")))
  expect_false(is_oq_dependency_error("string"))
})

test_that("define_dependency_error creates dual-class object", {
  dep_err <- openqaly:::define_dependency_error("test message")

  expect_true(inherits(dep_err, "oq_dependency_error"))
  expect_true(inherits(dep_err, "oq_error"))
  expect_equal(dep_err$message, "test message")
})

test_that("format_and_throw_errors filters out dependency errors", {
  clear_oq_errors()

  root_err <- openqaly:::define_error("Root cause error")
  dep_err <- openqaly:::define_dependency_error("Dependency error")

  error_list <- list(
    list(context = "Context 1", error = root_err),
    list(context = "Context 2", error = dep_err)
  )

  err <- expect_error(openqaly:::format_and_throw_errors(error_list))

  # Should contain root error
  expect_match(err$message, "Root cause error")
  expect_match(err$message, "Context 1")

  # Should NOT contain dependency error
  expect_false(grepl("Dependency error", err$message))
  expect_false(grepl("Context 2", err$message))
})

test_that("format_and_throw_errors is silent when only dependency errors exist", {
  clear_oq_errors()

  dep_err1 <- openqaly:::define_dependency_error("Dep 1")
  dep_err2 <- openqaly:::define_dependency_error("Dep 2")

  error_list <- list(
    list(context = "Context 1", error = dep_err1),
    list(context = "Context 2", error = dep_err2)
  )

  # Should NOT throw an error
  result <- openqaly:::format_and_throw_errors(error_list)
  expect_null(result)
})
