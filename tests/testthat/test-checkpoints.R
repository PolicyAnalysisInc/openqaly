context("Error checkpoints")

library(tibble)

# Define a minimal segment for namespace creation
mock_segment <- tibble::tibble(strategy = "S1", group = "G1")

# Define a minimal valid model structure helper function
create_minimal_model <- function() {
  model <- list(
      settings = list(
          timeframe = 1, 
          timeframe_unit = "Years", 
          cycle_length = 1, 
          cycle_length_unit = "Months",
          days_per_year = 365 
      ),
      states = tibble::tibble( 
          name = "StateA", 
          state_cycle_limit = Inf, 
          state_cycle_limit_unit = "cycles",
          initial_probability = 1 # Need initial probability
      ),
      env = new.env(parent = baseenv())
  )
  # Pre-calculate and add cycle_length_days as expected by downstream functions
  model$settings$cycle_length_days <- openqaly:::get_cycle_length_days(model$settings)
  # Add other elements expected by create_namespace's callees if necessary
  # For now, cycle_length_days seems the most critical missing piece
  model
}

 

test_that("eval_variables collects and throws multiple errors", {
  # Clear errors before test
  clear_oq_errors()

  # Define variables with errors using tribble
  bad_vars_tbl <- tibble::tribble(
    ~name,   ~display_name, ~description, ~formula,
    "var_a", "Var A",       "Desc A",     "undefined_variable * 2",
    "var_b", "Var B",       "Desc B",     "another_missing + 5"
  )
  bad_vars <- openqaly:::parse_variables(bad_vars_tbl)

  # Expect an error when calling eval_variables directly
  err <- expect_error({
    # Use the helper to create a valid model structure
    minimal_model <- create_minimal_model()
    test_ns <- openqaly:::create_namespace(model=minimal_model, segment=mock_segment)
    openqaly:::eval_variables(bad_vars, test_ns, context = "variables") # oq_error_checkpoint is called inside this
  })
  
  # Check that the error message contains the key components using grepl
  expect_true(grepl("Multiple errors found", err$message), info = "Message should start with prefix")
  expect_true(grepl("Context", err$message), info = "Message should contain 'Context' header")
  expect_true(grepl("Error", err$message), info = "Message should contain 'Error' header")
  
  expect_true(grepl("Evaluation of variables 'var_a'", err$message), 
              info = "Message should mention var_a context")
  expect_true(grepl('Variable "undefined_variable" not found\\.', err$message), 
              info = "Message should mention var_a error")
              
  expect_true(grepl("Evaluation of variables 'var_b'", err$message), 
              info = "Message should mention var_b context")
  expect_true(grepl('Variable "another_missing" not found\\.', err$message), 
              info = "Message should mention var_b error")
})


 

test_that("checkpoint filters out dependency errors", {
  # Clear errors before test
  clear_oq_errors()

  # var_root causes a root error, var_dep depends on it
  vars_tbl <- tibble::tribble(
    ~name,            ~display_name, ~description, ~formula,
    "var_root_error", "Root",        "Desc Root",  "non_existent_var + 1",
    "var_dep_error",  "Dep",         "Desc Dep",   "var_root_error * 2" 
  )
  vars <- openqaly:::parse_variables(vars_tbl)

  # Expect an error, then check its message content
  err <- expect_error({
      # Use the helper to create a valid model structure
      minimal_model <- create_minimal_model()
      test_ns <- openqaly:::create_namespace(model=minimal_model, segment=mock_segment)
      openqaly:::eval_variables(vars, test_ns, context = "variables") # Checkpoint inside
  })

  # Check that the final message ONLY contains the root error
  # (Assertions remain the same)
  expect_true(grepl("Multiple errors found", err$message), info = "Message should start with prefix")
  
  expect_true(grepl("Evaluation of variables 'var_root_error'", err$message), 
              info = "Message should mention root error context")
  expect_true(grepl('Variable \"non_existent_var\" not found\\.', err$message), 
              info = "Message should mention root error message")
              
  expect_false(grepl("Evaluation of variables 'var_dep_error'", err$message), 
              info = "Message should NOT mention dependency error context")
  expect_false(grepl("Error in dependency \"var_root_error\"\\.", err$message), 
              info = "Message should NOT mention dependency error message")
})

test_that("modify_error_msg correctly formats messages", {
  # Test old lazy_eval format (for backwards compatibility)
  expect_equal(openqaly:::modify_error_msg("Error in eval(x$expr, data, x$env): object 'myVar' not found"), 'Variable "myVar" not found.')

  # Test new eval_tidy format
  expect_equal(openqaly:::modify_error_msg("Error: object 'myVar' not found"), 'Variable "myVar" not found.')

  expect_equal(openqaly:::modify_error_msg("Error: Some other error occurred"), "Some other error occurred")
  expect_equal(openqaly:::modify_error_msg("Just a string"), "Just a string")
})

test_that("is_oq_error identifies errors correctly", {
  err <- openqaly:::define_error("test error")
  not_err <- list(message = "hello")
  expect_true(openqaly:::is_oq_error(err))
  expect_false(openqaly:::is_oq_error(not_err))
  expect_false(openqaly:::is_oq_error("a string"))
})

test_that("define_error uses modify_error_msg", {
  err <- openqaly:::define_error("Error: object 'temp' not found")
  expect_equal(err$message, 'Variable "temp" not found.')
}) 

# Add a test for get_accumulated_errors and clear_oq_errors
test_that("Error accumulation and clearing works", {
  clear_oq_errors() # Start clean
  expect_length(get_accumulated_errors(), 0)
  
  # Accumulate some errors manually (using internal function for testing)
  err1 <- openqaly:::define_error("Error 1")
  err2 <- openqaly:::define_dependency_error("Dep Error 2")
  
  openqaly:::accumulate_oq_error(err1, "Context 1")
  openqaly:::accumulate_oq_error(err2, "Context 2")
  
  # Check accumulation
  accumulated <- get_accumulated_errors()
  expect_length(accumulated, 2)
  expect_equal(accumulated[[1]]$context, "Context 1")
  expect_true(openqaly:::is_oq_error(accumulated[[1]]$error))
  expect_equal(accumulated[[2]]$context, "Context 2")
  expect_true(openqaly:::is_oq_dependency_error(accumulated[[2]]$error))
  
  # Check clearing
  clear_oq_errors()
  expect_length(get_accumulated_errors(), 0)
}) 