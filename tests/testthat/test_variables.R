context("Variables")

# Prep data for test cases
var_tests <- system.file("test_cases", "test_variables.xlsx", package = "heRomod2") %>%
  read_workbook()
segment <- tibble::tibble(strategy = "S1", group = "G1")
test_ns <- heRomod2:::create_test_ns(segment)

# Formula Parsing
test_that('missing column names in variables spec are caught', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(var_tests$missing_col, segment),
    'Variables definition was missing columns: "name".'
  )
  
})
test_that('duplicate variable names are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(var_tests$dupe, segment),
    'Variables definition contained duplicate names for variables: "g", "f".'
  )
})
test_that('invalid variable names are detected', {
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(var_tests$invalid_name, segment),
    'Variables definition contained invalid names for variables: "1e", ".fun". Names must start with a letter and contain only letters, numbers, and underscores.'
  )
})
test_that('reserved variable names are detected', {
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(var_tests$keyword, segment),
    'Variables definition contained names reserved for keywords for variables: "cycle".'
  )
})
test_that('circular references are detected', {
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(var_tests$circular, segment),
    'Variables definition contained circular references in variables: "f", "g", "c", "e", "a".'
  )
})

# Formula Evaluation
test_that('formula syntax errors are handled properly', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_seg_variables(var_tests$invalid, segment)
  
  # Check that a warning is produced naming all the variables generating
  # errors.
  expect_warning(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns),
    regexp = "Error in evaluation of variables \"a\": Error in formula syntax",
    fixed = TRUE
  )
  
  # Check that the blank formula evaluated to NA
  expect_equal(eval_vars['d'], NA)
  
  # Check that the value of the variables are heRo_error objects.
  expect_true(is_hero_error(eval_vars['a']), info = "'a' should be heRo_error")
  expect_true(is_hero_error(eval_vars['c']), info = "'c' should be heRo_error (dependency)")
  expect_true(is_hero_error(eval_vars['e']), info = "'e' should be heRo_error (dependency)")
  expect_true(is_hero_error(eval_vars['g']), info = "'g' should be heRo_error (dependency)")
  
  # Check that the error messages print correctly
  expect_output(
    print(eval_vars['a'], 'Error: Error in formula syntax.')
  )
  expect_output(
    print(eval_vars['c'], 'Error: Error in dependency "a".')
  )
  expect_output(
    print(eval_vars['e'], 'Error: Error in dependency "c".') # Note: This dependency is indirect via 'c'
  )
  expect_output(
    print(eval_vars['g'], 'Error: Error in dependency "c".') # Note: This dependency is indirect via 'c'
  )
})
test_that('formula evaluation errors are handled properly', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_seg_variables(var_tests$err, segment)
  
  # Check that a warning is produced naming all the parameters generating
  # errors.
  expect_warning(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns),
    'Error in evaluation of variables "a": Variable "z" not found.'
  )
  
  # Check that the value of the parameters are heRo_error objects.
  expect_true(is_hero_error(eval_vars['a']), info = "'a' should be heRo_error")
  expect_true(is_hero_error(eval_vars['c']), info = "'c' should be heRo_error (dependency)")
  expect_true(is_hero_error(eval_vars['e']), info = "'e' should be heRo_error (dependency)")
  expect_true(is_hero_error(eval_vars['g']), info = "'g' should be heRo_error (dependency)")
  
  # Check that the error messages print correctly
  expect_output(
    print(eval_vars['a'], 'Error: Variable "z" not found.')
  )
  expect_output(
    print(eval_vars['c'], 'Error: Error in dependency "a".')
  )
  expect_output(
    print(eval_vars['e'], 'Error: Error in dependency "c".') # Note: Indirect dependency
  )
  expect_output(
    print(eval_vars['g'], 'Error: Error in dependency "c".') # Note: Indirect dependency
  )
})
test_that('variables are evaluated properly when sorted', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_seg_variables(var_tests$sorted, segment)
  
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
    rep(list(c("heRoFormula", "list")), 7)
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
  
  # Check that no warnings, errors, or outputs are produced
  # during evaluation.
  expect_silent(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns)
  )
  
  # Check that parameter values are correct.
  expect_equal(unname(eval_vars['g']), 25.00623, tolerance = 1e-7)
  expect_equal(unname(eval_vars['b']), 200, tolerance = 1e-7)
  expect_equal(unname(eval_vars['a']), 100, tolerance = 1e-7)
  expect_equal(unname(eval_vars['c']), 101, tolerance = 1e-7)
  expect_equal(unname(eval_vars['e']), 301, tolerance = 1e-7)
  
})
test_that('variables are evaluated properly when unsorted', {
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_seg_variables(var_tests$unsorted, segment)
  
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
    rep(list(c("heRoFormula", "list")), 7)
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
  
  # Check that no warnings, errors, or outputs are produced
  # during evaluation.
  expect_silent(
    eval_vars <- heRomod2:::eval_variables(parsed_vars, test_ns)
  )
  
  # Check that parameter values are correct.
  expect_equal(unname(eval_vars['g']), 25.00623, tolerance = 1e-7)
  expect_equal(unname(eval_vars['b']), 200, tolerance = 1e-7)
  expect_equal(unname(eval_vars['a']), 100, tolerance = 1e-7)
  expect_equal(unname(eval_vars['c']), 101, tolerance = 1e-7)
  expect_equal(unname(eval_vars['e']), 301, tolerance = 1e-7)
  
})
