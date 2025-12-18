context("Decision Trees")

# Prep data for test cases
tree_tests <- system.file("test_cases", "test_trees.xlsx", package = "heRomod2") %>%
  read_workbook()
segment <- tibble::tibble(strategy = "S1", group = "G1")

# Tree Parsing
test_that('incorrect column names are detected', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_misnamed_col
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_misnamed_col),
    'Error in decision tree specification, missing columns: "name".',
  )
  
})
test_that('duplicated node names are detected', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_dupe_node_name
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_dupe_node_name),
    'Error in decision tree specification, tree "tree" contained duplicate node names: "no_event".',
  )
  
})
test_that('invalid tag names detected', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_bad_csl
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_bad_csl),
    paste0(
      'Error in decision tree specification, tree "tree" contained invalid tag names for nodes:',
      ' "survived_surgery". Tag names must be provided in a comma-separated list, start with a letter and',
      ' contain only letters, numbers, and underscores.'
    )
  )
  
})
test_that('tag names that are duplicates of node names detected', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_dupe_tag_node_name
  
  # Parse the variables specification
  expect_error(
    heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_dupe_tag_node_name),
    'Error in decision tree "tree", tag names were duplicates of node names: "had_event".',
  )
  
})

# Tree Evaluation
test_that('evaluated decision tree is of correct class', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_valid
  
  expect_silent({
    
    # Parse the variables specification
    parsed_vars <- heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_valid)
    
    # Evaluate the variables
    var_res <- heRomod2:::eval_variables(parsed_vars, test_ns)
    
    # Plot the tree
    expect_equal(class(var_res['tree']), 'eval_decision_tree')
    
  })
  
})
test_that('duplicate uses of "C" within a level are detected.', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_double_C
  
  # Parse the variables specification
  expect_silent(
    parsed_vars <- heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_double_C)
  )
  
  # Clear any existing errors before test
  heRomod2:::clear_hero_errors()
  
  # Evaluate the variables - expect error due to checkpoint
  expect_error(
    heRomod2:::eval_variables(parsed_vars, test_ns),
    regexp = "Multiple errors found during evaluation",
    fixed = FALSE
  )
  
  # To check the individual error objects, evaluate again without checkpoint
  heRomod2:::clear_hero_errors()
  test_ns_copy <- heRomod2:::create_test_ns(segment)
  test_ns_copy$env$.trees <- tree_tests$trees_double_C
  
  # Manually evaluate to inspect error objects
  suppressWarnings({
    for (i in seq_len(nrow(parsed_vars))) {
      name <- parsed_vars$name[i]
      formula <- parsed_vars$formula[[i]]
      res <- heRomod2:::eval_formula(formula, test_ns_copy)
      
      if (is_hero_error(res)) {
        assign(name, res, envir = test_ns_copy$env)
      } else {
        vector_type <- is.vector(res) && !is.list(res)
        if (vector_type && (length(res) == nrow(test_ns_copy$df))) {
          test_ns_copy$df[name] <- res
        } else {
          assign(name, res, envir = test_ns_copy$env)
        }
      }
    }
  })
  
  # Check that the value of the parameters are heRo_error objects.
  # Use is_hero_error which checks class inheritance
  expect_true(is_hero_error(test_ns_copy$env$p_event), info = "p_event should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died), info = "p_died should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_survived), info = "p_survived should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_surgery), info = "p_surgery should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_given_event), info = "p_died_given_event should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_event_given_died), info = "p_event_given_died should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_or_surgery), info = "p_died_or_surgery should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_or_not_surgery), info = "p_died_or_not_surgery should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_and_surgery), info = "p_died_and_surgery should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_or_survived_and_had_event_given_surgery), info = "p_died_or_survived_and_had_event_given_surgery should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_or_survived_and_event_given_surgery), info = "p_died_or_survived_and_event_given_surgery should be a heRo_error")
  expect_true(is_hero_error(test_ns_copy$env$p_died_or_survived_and_surgery_given_event), info = "p_died_or_survived_and_surgery_given_event should be a heRo_error")
  
  # Check that the error messages print correctly
  expect_output(
    print(test_ns_copy$env$tree),
    'Error: Error in calculating complementary probabilities, "C" may be used only once per level.'
  )
  
  expect_output(
    print(test_ns_copy$env$p_event),
    'Error: Error in dependency "tree".'
  )
  
})
test_that('decision tree based probability calculations yield correct results', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_valid
  
  # Parse the variables specification
  parsed_vars <- heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_valid)
  
  # Evaluate the variables
  var_res <- heRomod2:::eval_variables(parsed_vars, test_ns)
  
  # Check the results
  
  expect_equal(var_res['p_event'], 0.15 + 0.005 * cycles)
  
  expect_equal(
    var_res['p_died'],
    (0.15 + 0.005 * cycles) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * cycles)) * 0.01
  )
  
  expect_equal(
    1 - var_res['p_died'],
    var_res['p_survived']
  )
  
  expect_equal(var_res['p_died_given_event'], rep((0.34 * 0.05) + ((1 - 0.34) * 0.15), rows))
  
  expect_equal(
    var_res['p_event_given_died'],
    ((0.15 + 0.005 * cycles) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15))) / ((0.15 + 0.005 * cycles) * ((0.34 * 0.05) + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * cycles)) * 0.01)
  )
  
  expect_equal(
    var_res['p_died_or_surgery'],
    (0.15 + 0.005 * cycles) * (0.34 + ((1 - 0.34) * 0.15)) + (1 - (0.15 + 0.005 * cycles)) * 0.01
  )
  
  expect_equal(
    var_res['p_died_or_not_surgery'],
    1 - var_res['p_surgery'] + var_res['p_died_and_surgery']
  )
  
  expect_equal(
    var_res['p_died_and_surgery'],
    (0.15 + 0.005 * cycles) * 0.34 * 0.05
  )
  
  expect_equal(
    var_res['p_died_or_survived_and_had_event_given_surgery'],
    rep(1, rows)
  )
  
  expect_equal(
    var_res['p_died_or_survived_and_event_given_surgery'],
    rep(1, rows)
  )
  
  expect_equal(
    var_res['p_died_or_survived_and_surgery_given_event'],
    rep(0.34, rows)
  )
  
})
test_that('evaluated decision tress can be plotted', {
  
  test_ns <- heRomod2:::create_test_ns(segment)
  cycles <- rep(seq_len(12), 12)
  rows <- length(cycles)
  test_ns$env$.trees <- tree_tests$trees_valid
  
  expect_silent({
    
    # Parse the variables specification
    parsed_vars <- heRomod2:::parse_seg_variables(tree_tests$vars, segment, tree_tests$trees_valid)
  
    # Evaluate the variables
    var_res <- heRomod2:::eval_variables(parsed_vars, test_ns)
    
    # Plot the tree
    plot_decision_tree(var_res['tree'])

  })
  
})

# Internals
test_that('checking subtrees works', {
  expect_error(
    heRomod2:::check_subtree(NULL),
    'Error, argument must be of type "subtree"'
  )
})
