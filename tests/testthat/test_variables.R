context("Variables")

# Prep data for test cases
var_tests <- system.file("test_cases", "test_variables.xlsx", package = "openqaly") %>%
  read_workbook()
segment <- tibble::tibble(strategy = "S1", group = "G1")
test_ns <- openqaly:::create_test_ns(segment)

# Formula Parsing
test_that('missing column names in variables spec are caught', {
  
  # Parse the variables specification
  expect_error(
    openqaly:::parse_seg_variables(var_tests$missing_col, segment),
    'Variables definition was missing columns: "name".'
  )
  
})
test_that('duplicate variable names are detected', {
  
  # Parse the variables specification
  expect_error(
    openqaly:::parse_seg_variables(var_tests$dupe, segment),
    'Variables definition contained duplicate names for variables: "g", "f".'
  )
})
test_that('invalid variable names are detected', {
  # Parse the variables specification
  expect_error(
    openqaly:::parse_seg_variables(var_tests$invalid_name, segment),
    'Variables definition contained invalid names for variables: "1e", ".fun". Names must start with a letter and contain only letters, numbers, and underscores.'
  )
})
test_that('reserved variable names are detected', {
  # Parse the variables specification
  expect_error(
    openqaly:::parse_seg_variables(var_tests$keyword, segment),
    'Variables definition contained names reserved for keywords for variables: "cycle".'
  )
})
test_that('circular references are detected', {
  
  # Parse the variables specification
  expect_error(
    openqaly:::parse_seg_variables(var_tests$circular, segment),
    'Variables definition contained circular references in variables: "f", "g", "c", "e", "a".'
  )
})

# Formula Evaluation
test_that('formula syntax errors are handled properly', {
  
  # Parse the variables specification
  parsed_vars <- openqaly:::parse_seg_variables(var_tests$invalid, segment)
  
  # Clear any existing errors before test
  openqaly:::clear_oq_errors()
  
  # Check that an error is produced naming all the variables generating
  # errors when checkpoint is triggered
  expect_error(
    openqaly:::eval_variables(parsed_vars, test_ns),
    regexp = "Multiple errors found during evaluation",
    fixed = FALSE
  )
  
  # Since eval_variables throws an error, we need to evaluate it again
  # without triggering checkpoint to check the values
  openqaly:::clear_oq_errors()
  
  # Test without checkpoint to verify values
  test_ns_copy <- openqaly:::create_test_ns(segment)
  
  # Manually evaluate without checkpoint to inspect results
  suppressWarnings({
    # Re-evaluate each variable formula manually to check results
    for (i in seq_len(nrow(parsed_vars))) {
      name <- parsed_vars$name[i]
      formula <- parsed_vars$formula[[i]]
      res <- openqaly:::eval_formula(formula, test_ns_copy)
      
      if (is_oq_error(res)) {
        # For error objects, assign to environment only
        assign(name, res, envir = test_ns_copy$env)
      } else {
        # Check if result can be assigned to dataframe
        vector_type <- is.vector(res) && !is.list(res)
        if (vector_type && (length(res) == nrow(test_ns_copy$df))) {
          test_ns_copy$df[name] <- res
        } else {
          assign(name, res, envir = test_ns_copy$env)
        }
      }
    }
  })
  
  # Now check the values
  # Check that the blank formula evaluated to NA
  expect_true(all(is.na(test_ns_copy$df[['d']])))
  
  # Check that the value of the variables are oq_error objects.
  expect_true(is_oq_error(test_ns_copy$env$a), info = "'a' should be oq_error")
  expect_true(is_oq_error(test_ns_copy$env$c), info = "'c' should be oq_error (dependency)")
  expect_true(is_oq_error(test_ns_copy$env$e), info = "'e' should be oq_error (dependency)")
  expect_true(is_oq_error(test_ns_copy$env$g), info = "'g' should be oq_error (dependency)")
  
  # Check that the error messages print correctly
  expect_output(
    print(test_ns_copy$env$a),
    'Error: Error in formula syntax.'
  )
  expect_output(
    print(test_ns_copy$env$c),
    'Error: Error in dependency "a".'
  )
  expect_output(
    print(test_ns_copy$env$e),
    'Error: Error in dependency "c".' # Note: This dependency is indirect via 'c'
  )
  expect_output(
    print(test_ns_copy$env$g),
    'Error: Error in dependency "c".' # Note: This dependency is indirect via 'c'
  )
})
test_that('formula evaluation errors are handled properly', {
  
  # Parse the variables specification
  parsed_vars <- openqaly:::parse_seg_variables(var_tests$err, segment)
  
  # Clear any existing errors before test
  openqaly:::clear_oq_errors()
  
  # Check that an error is produced naming all the parameters generating
  # errors when checkpoint is triggered
  expect_error(
    openqaly:::eval_variables(parsed_vars, test_ns),
    regexp = "Multiple errors found during evaluation",
    fixed = FALSE
  )
  
  # Since eval_variables throws an error, we need to evaluate it again
  # without triggering checkpoint to check the values
  openqaly:::clear_oq_errors()
  
  # Create a fresh namespace for manual evaluation
  test_ns_copy <- openqaly:::create_test_ns(segment)
  
  # Manually evaluate without checkpoint to inspect results
  suppressWarnings({
    # Re-evaluate each variable formula manually to check results
    for (i in seq_len(nrow(parsed_vars))) {
      name <- parsed_vars$name[i]
      formula <- parsed_vars$formula[[i]]
      res <- openqaly:::eval_formula(formula, test_ns_copy)
      
      if (is_oq_error(res)) {
        # For error objects, assign to environment only
        assign(name, res, envir = test_ns_copy$env)
      } else {
        # Check if result can be assigned to dataframe
        vector_type <- is.vector(res) && !is.list(res)
        if (vector_type && (length(res) == nrow(test_ns_copy$df))) {
          test_ns_copy$df[name] <- res
        } else {
          assign(name, res, envir = test_ns_copy$env)
        }
      }
    }
  })
  
  # Check that the value of the parameters are oq_error objects.
  expect_true(is_oq_error(test_ns_copy$env$a), info = "'a' should be oq_error")
  expect_true(is_oq_error(test_ns_copy$env$c), info = "'c' should be oq_error (dependency)")
  expect_true(is_oq_error(test_ns_copy$env$e), info = "'e' should be oq_error (dependency)")
  expect_true(is_oq_error(test_ns_copy$env$g), info = "'g' should be oq_error (dependency)")
  
  # Check that the error messages print correctly
  expect_output(
    print(test_ns_copy$env$a),
    'Error: Variable "z" not found.'
  )
  expect_output(
    print(test_ns_copy$env$c),
    'Error: Error in dependency "a".'
  )
  expect_output(
    print(test_ns_copy$env$e),
    'Error: Error in dependency "c".' # Note: Indirect dependency
  )
  expect_output(
    print(test_ns_copy$env$g),
    'Error: Error in dependency "c".' # Note: Indirect dependency
  )
})
test_that('variables are evaluated properly when sorted', {
  
  # Parse the variables specification
  parsed_vars <- openqaly:::parse_seg_variables(var_tests$sorted, segment)
  
  # Check that parsed spec has right number of rows
  expect_equal(nrow(parsed_vars), 7)
  
  # Check that parsed spec has right variable names
  expect_equal(
    parsed_vars$name %in% var_tests$sorted$name,
    rep(TRUE, 7)
  )
  
  # Check that parsed variables are correctly formatted
  expect_equal(
    class(parsed_vars),
    c("variables", "tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    colnames(parsed_vars),
    c("name", "display_name", "description", "formula")
  )
  
  expect_equal(
    unname(lapply(parsed_vars, class)),
    list("character", "character", "character", "list")
  )
  
  expect_equal(
    unname(lapply(parsed_vars$formula, class)),
    rep(list(c("oq_formula", "list")), 7)
  )
  
  # Check that dependencies have been propagated
  expect_equal(
    parsed_vars$formula[[5]]$depends,
    c("+", "a", "c", "b")
  )
  expect_equal(
    parsed_vars$formula[[6]]$depends,
    c("cars", "lm", "~", "speed", "dist", "d")  
  )
  
  # Clear any existing errors before test
  openqaly:::clear_oq_errors()
  
  # Check that no warnings, errors, or outputs are produced
  # during evaluation.
  expect_silent(
    result_ns <- openqaly:::eval_variables(parsed_vars, test_ns)
  )
  
  # Check that parameter values are correct.
  # These are all scalar values, so they should be in env
  expect_equal(unname(result_ns$env$g), 25.00623, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$b), 200, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$a), 100, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$c), 101, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$e), 301, tolerance = 1e-7)
  
})
test_that('variables are evaluated properly when unsorted', {
  
  # Parse the variables specification
  parsed_vars <- openqaly:::parse_seg_variables(var_tests$unsorted, segment)
  
  # Check that parsed spec has right number of rows
  expect_equal(nrow(parsed_vars), 7)
  
  # Check that parsed spec has right variable names
  expect_equal(
    parsed_vars$name %in% var_tests$sorted$name,
    rep(TRUE, 7)
  )
  
  # Check that parsed variables are correctly formatted
  expect_equal(
    class(parsed_vars),
    c("variables", "tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    colnames(parsed_vars),
    c("name", "display_name", "description", "formula")
  )
  
  expect_equal(
    unname(lapply(parsed_vars, class)),
    list("character", "character", "character", "list")
  )
  
  expect_equal(
    unname(lapply(parsed_vars$formula, class)),
    rep(list(c("oq_formula", "list")), 7)
  )
  
  # Check that dependencies have been propagated
  expect_equal(
    parsed_vars$formula[[6]]$depends,
    c("+", "a", "c", "b")
  )
  expect_equal(
    parsed_vars$formula[[4]]$depends,
    c("cars", "lm", "~", "speed", "dist", "d")  
  )
  
  # Clear any existing errors before test
  openqaly:::clear_oq_errors()
  
  # Check that no warnings, errors, or outputs are produced
  # during evaluation.
  expect_silent(
    result_ns <- openqaly:::eval_variables(parsed_vars, test_ns)
  )
  
  # Check that parameter values are correct.
  # These are all scalar values, so they should be in env
  expect_equal(unname(result_ns$env$g), 25.00623, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$b), 200, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$a), 100, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$c), 101, tolerance = 1e-7)
  expect_equal(unname(result_ns$env$e), 301, tolerance = 1e-7)
  
})
