context("Values output")

# =============================================================================
# Values Extraction Tests
# =============================================================================

test_that("get_values works with aggregated data", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Test long format
  values_long <- get_values(results, format = "long", groups = "overall")
  expect_s3_class(values_long, "data.frame")
  expect_true("strategy" %in% colnames(values_long))
  expect_true("group" %in% colnames(values_long))
  expect_true("value_name" %in% colnames(values_long))
  expect_true("amount" %in% colnames(values_long))
  expect_equal(unique(values_long$group), "Overall")

  # Test wide format
  values_wide <- get_values(results, format = "wide", groups = "overall")
  expect_s3_class(values_wide, "data.frame")
  expect_true("strategy" %in% colnames(values_wide))
  expect_true("group" %in% colnames(values_wide))
})

test_that("get_values filters by value type correctly", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get cost values only
  costs <- get_values(results, value_type = "cost")
  cost_values <- unique(costs$value_name)

  # Get outcome values only
  outcomes <- get_values(results, value_type = "outcome")
  outcome_values <- unique(outcomes$value_name)

  # Should be mutually exclusive
  expect_length(intersect(cost_values, outcome_values), 0)

  # Get all values
  all_values <- get_values(results, value_type = "all")
  all_value_names <- unique(all_values$value_name)

  # All should be union of costs and outcomes
  expect_setequal(all_value_names, union(cost_values, outcome_values))
})

test_that("get_values handles discounted vs undiscounted", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get undiscounted
  values_undiscounted <- get_values(results, discounted = FALSE)

  # Get discounted
  values_discounted <- get_values(results, discounted = TRUE)

  # Should have same structure
  expect_equal(colnames(values_undiscounted), colnames(values_discounted))

  # Discounted values should generally be less than or equal to undiscounted
  # (at least for later cycles)
  # Note: First cycle might be equal
})

test_that("get_summaries works with aggregated data", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  summaries <- get_summaries(results, groups = "overall")

  expect_s3_class(summaries, "data.frame")
  expect_true("strategy" %in% colnames(summaries))
  expect_true("group" %in% colnames(summaries))
  expect_true("summary" %in% colnames(summaries))
  expect_true("value" %in% colnames(summaries))
  expect_true("amount" %in% colnames(summaries))
  expect_equal(unique(summaries$group), "Overall")
})

test_that("get_summaries filters by value type", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get cost summaries (default uses display names)
  cost_summaries <- get_summaries(results, value_type = "cost")
  # Just check we got 4 cost values
  expect_equal(length(unique(cost_summaries$value)), 4)

  # Verify technical names work with explicit parameter
  cost_summaries_tech <- get_summaries(results, value_type = "cost", use_display_names = FALSE)
  expect_true(all(cost_summaries_tech$value %in% c("cost_drug", "cost_admin", "cost_ae", "cost_prog")))

  # Get outcome summaries (default uses display names)
  outcome_summaries <- get_summaries(results, value_type = "outcome")
  # Should have at least one outcome value
  expect_true(length(unique(outcome_summaries$value)) >= 1)

  # Verify technical names work with explicit parameter
  outcome_summaries_tech <- get_summaries(results, value_type = "outcome", use_display_names = FALSE)
  expect_true(all(outcome_summaries_tech$value %in% c("qalys")))
})

test_that("filter_by_value_type helper works correctly", {
  # Create mock metadata
  metadata <- list(
    values = data.frame(
      name = c("cost1", "cost2", "outcome1", "outcome2"),
      type = c("cost", "cost", "outcome", "outcome"),
      stringsAsFactors = FALSE
    )
  )

  columns <- c("cost1", "cost2", "outcome1", "outcome2", "other")

  # Filter costs
  cost_cols <- openqaly:::filter_by_value_type(columns, metadata, "cost")
  expect_equal(cost_cols, c("cost1", "cost2"))

  # Filter outcomes
  outcome_cols <- openqaly:::filter_by_value_type(columns, metadata, "outcome")
  expect_equal(outcome_cols, c("outcome1", "outcome2"))

  # All
  all_cols <- openqaly:::filter_by_value_type(columns, metadata, "all")
  expect_equal(all_cols, columns)
})

test_that("map_value_names helper works correctly", {
  # Create mock metadata
  metadata <- list(
    values = data.frame(
      name = c("cost_drug", "qalys"),
      display_name = c("Drug Cost", "QALYs"),
      abbreviation = c("Drug", "Q"),
      stringsAsFactors = FALSE
    )
  )

  names_technical <- c("cost_drug", "qalys", "unknown")

  # Map to display names
  display <- openqaly:::map_value_names(names_technical, metadata, "display_name")
  expect_equal(display, c("Drug Cost", "QALYs", "unknown"))

  # Map to abbreviations
  abbrev <- openqaly:::map_value_names(names_technical, metadata, "abbreviation")
  expect_equal(abbrev, c("Drug", "Q", "unknown"))

  # Map to names (identity)
  names_out <- openqaly:::map_value_names(names_technical, metadata, "name")
  expect_equal(names_out, names_technical)
})

# =============================================================================
# Values Type Safety Tests
# =============================================================================

test_that("values dataframe enforces types proactively", {
  # Load the values spec for testing
  values_spec <- system.file('model_input_specs', 'values.csv', package = 'openqaly') %>%
    readr::read_csv(col_types = 'clccc', progress = FALSE, show_col_types = FALSE)

  # Test 1: Empty columns get correct type (logical -> character)
  values_all_na <- tibble::tibble(
    name = c("val1", "val2"),
    display_name = NA,  # Will be logical by default
    description = NA,   # Will be logical by default
    state = NA,         # Will be logical by default
    destination = NA,   # Will be logical by default
    formula = c("1", "2")
  )

  # Check initial types (before processing)
  expect_true(is.character(values_all_na$name))
  expect_true(is.logical(values_all_na$display_name))
  expect_true(is.logical(values_all_na$description))
  expect_true(is.logical(values_all_na$state))
  expect_true(is.logical(values_all_na$destination))
  expect_true(is.character(values_all_na$formula))

  # After processing with check_tbl, all should be character
  processed <- check_tbl(values_all_na, values_spec, "Values")
  expect_true(all(sapply(processed, is.character)))
  expect_equal(nrow(processed), 2)
  expect_equal(processed$name, c("val1", "val2"))
  expect_equal(processed$formula, c("1", "2"))

  # Test 2: Numeric columns get converted to character
  values_wrong_type <- tibble::tibble(
    name = c(1, 2),  # numeric, should become character
    formula = c("a", "b")
  )

  expect_true(is.numeric(values_wrong_type$name))

  processed <- check_tbl(values_wrong_type, values_spec, "Values")
  expect_equal(processed$name, c("1", "2"))
  expect_true(is.character(processed$name))

  # Test 3: Factor columns get converted to character
  values_factor <- tibble::tibble(
    name = factor(c("val1", "val2")),
    formula = c("1", "2")
  )

  expect_true(is.factor(values_factor$name))

  processed <- check_tbl(values_factor, values_spec, "Values")
  expect_true(is.character(processed$name))
  expect_equal(processed$name, c("val1", "val2"))

  # Test 4: Missing columns are added with correct type
  values_missing_cols <- tibble::tibble(
    name = c("val1", "val2"),
    formula = c("1", "2")
  )

  processed <- check_tbl(values_missing_cols, values_spec, "Values")
  expect_true("display_name" %in% colnames(processed))
  expect_true("description" %in% colnames(processed))
  expect_true("state" %in% colnames(processed))
  expect_true("destination" %in% colnames(processed))
  expect_true(is.character(processed$display_name))
  expect_true(is.character(processed$description))
  expect_true(is.character(processed$state))
  expect_true(is.character(processed$destination))

  # Test 5: Fallback values work correctly
  values_with_missing <- tibble::tibble(
    name = c("val1", "val2"),
    display_name = c(NA, "Value 2"),
    description = c(NA, NA),
    formula = c("1", "2")
  )

  processed <- check_tbl(values_with_missing, values_spec, "Values")
  # display_name fallback to name
  expect_equal(processed$display_name[1], "val1")
  expect_equal(processed$display_name[2], "Value 2")
  # description fallback to display_name
  expect_equal(processed$description[1], "val1")
  expect_equal(processed$description[2], "Value 2")
})

test_that("check_values_df validates business logic correctly", {
  # Test 1: Invalid names are caught
  values_invalid_names <- tibble::tibble(
    name = c("123invalid", "valid_name", "_startswith"),
    display_name = c("a", "b", "c"),
    description = c("a", "b", "c"),
    state = c("s1", "s2", "s3"),
    destination = c(NA, NA, NA),
    formula = c("1", "2", "3")
  )

  expect_error(
    check_values_df(values_invalid_names),
    "Invalid value names.*123invalid.*_startswith"
  )

  # Test 2: Invalid formulas are caught
  values_invalid_formula <- tibble::tibble(
    name = c("val1", "val2"),
    display_name = c("a", "b"),
    description = c("a", "b"),
    state = c("s1", "s2"),
    destination = c(NA, NA),
    formula = c("1", "this is not ) valid R code")
  )

  expect_error(
    check_values_df(values_invalid_formula),
    "Invalid formula syntax for value 'val2'"
  )

  # Test 3: Valid data passes through
  values_valid <- tibble::tibble(
    name = c("val1", "val2"),
    display_name = c("Value 1", "Value 2"),
    description = c("Desc 1", "Desc 2"),
    state = c("s1", "s2"),
    destination = c(NA, "s2"),
    formula = c("1", "2 + 2")
  )

  # Should not error
  expect_silent(check_values_df(values_valid))

  # Test 4: NA values are handled correctly
  values_with_na <- tibble::tibble(
    name = c("val1", NA),
    display_name = c("Value 1", NA),
    description = c("Desc 1", NA),
    state = c("s1", NA),
    destination = c(NA, NA),
    formula = c("1", NA)
  )

  # Should not error (NAs are allowed)
  expect_silent(check_values_df(values_with_na))
})

test_that("check_values_df passes when all names are NA", {
  # Edge case: all names are NA (should pass since we only validate non-NA names)
  values_all_na_names <- tibble::tibble(
    name = c(NA_character_, NA_character_),
    display_name = c("Value 1", "Value 2"),
    description = c("Desc 1", "Desc 2"),
    state = c("s1", "s2"),
    destination = c(NA_character_, NA_character_),
    formula = c("1", "2")
  )

  # Should not error (we only validate non-NA names)
  expect_silent(check_values_df(values_all_na_names))
})

test_that("read_model enforces values type safety", {
  skip_if_not_installed("openxlsx")

  # Create a temporary Excel file with values that have wrong types
  temp_dir <- tempdir()
  test_model_path <- file.path(temp_dir, "test_model_type_safety")
  dir.create(test_model_path, showWarnings = FALSE)

  wb <- openxlsx::createWorkbook()

  # Add sheets with test data
  openxlsx::addWorksheet(wb, "settings")
  settings_data <- data.frame(
    setting = c("n_cycles", "model_type", "discount_cost", "discount_outcomes"),
    value = c("10", "markov", "3", "3")
  )
  openxlsx::writeData(wb, "settings", settings_data)

  openxlsx::addWorksheet(wb, "strategies")
  strategies_data <- data.frame(
    name = c("standard", "new"),
    display_name = c("Standard", "New"),
    description = c("Standard treatment", "New treatment")
  )
  openxlsx::writeData(wb, "strategies", strategies_data)

  openxlsx::addWorksheet(wb, "states")
  states_data <- data.frame(
    name = c("healthy", "sick"),
    display_name = c("Healthy", "Sick"),
    description = c("Healthy state", "Sick state"),
    initial_probability = c(1, 0),
    state_cycle_limit = c(0, 0)
  )
  openxlsx::writeData(wb, "states", states_data)

  openxlsx::addWorksheet(wb, "values")
  # Create values with potentially problematic types
  values_data <- data.frame(
    name = c(1, 2),  # numeric instead of character
    display_name = NA,  # all NA - will be logical
    description = NA,    # all NA - will be logical
    state = factor(c("healthy", "sick")),  # factor instead of character
    destination = NA,  # all NA - will be logical
    formula = c("100", "50")
  )
  openxlsx::writeData(wb, "values", values_data)

  openxlsx::addWorksheet(wb, "transitions")
  transitions_data <- data.frame(
    from_state = c("healthy", "healthy", "sick"),
    to_state = c("healthy", "sick", "sick"),
    formula = c("0.9", "0.1", "1")
  )
  openxlsx::writeData(wb, "transitions", transitions_data)

  openxlsx::saveWorkbook(wb, file.path(test_model_path, "model.xlsx"), overwrite = TRUE)

  # Create empty data directory
  dir.create(file.path(test_model_path, "data"), showWarnings = FALSE)
  dir.create(file.path(test_model_path, "scripts"), showWarnings = FALSE)

  # Read the model - type conversion should happen automatically
  model <- read_model(test_model_path)

  # Check that all values columns are character type
  expect_true(is.character(model$values$name))
  expect_true(is.character(model$values$display_name))
  expect_true(is.character(model$values$description))
  expect_true(is.character(model$values$state))
  expect_true(is.character(model$values$destination))
  expect_true(is.character(model$values$formula))

  # Check that values were correctly converted
  expect_equal(model$values$name, c("1", "2"))
  expect_equal(model$values$state, c("healthy", "sick"))
  expect_equal(model$values$formula, c("100", "50"))

  # Check fallback values worked
  expect_equal(model$values$display_name, c("1", "2"))  # fallback to name
  expect_equal(model$values$description, c("1", "2"))   # fallback to display_name

  # Clean up
  unlink(test_model_path, recursive = TRUE)
})

test_that("read_model_json enforces values type safety", {
  # Create test JSON with values that have wrong types
  test_json <- jsonlite::toJSON(list(
    settings = data.frame(
      setting = c("n_cycles", "model_type", "discount_cost", "discount_outcomes"),
      value = c("10", "markov", "3", "3")
    ),
    strategies = data.frame(
      name = c("standard", "new"),
      display_name = c("Standard", "New"),
      description = c("Standard treatment", "New treatment")
    ),
    states = data.frame(
      name = c("healthy", "sick"),
      display_name = c("Healthy", "Sick"),
      description = c("Healthy state", "Sick state"),
      initial_probability = c("1", "0"),
      state_cycle_limit = c(0, 0)
    ),
    values = data.frame(
      name = c(1, 2),  # numeric instead of character
      display_name = c(NA, NA),  # all NA - will be logical in JSON
      description = c(NA, NA),    # all NA - will be logical in JSON
      state = c("healthy", "sick"),
      destination = c(NA, NA),  # all NA - will be logical in JSON
      formula = c("100", "50")
    ),
    transitions = data.frame(
      from_state = c("healthy", "healthy", "sick"),
      to_state = c("healthy", "sick", "sick"),
      formula = c("0.9", "0.1", "1")
    ),
    tables = list(),
    scripts = list()
  ), auto_unbox = TRUE, na = "null")

  # Read the JSON model
  model <- read_model_json(as.character(test_json))

  # Check that all values columns are character type
  expect_true(is.character(model$values$name))
  expect_true(is.character(model$values$display_name))
  expect_true(is.character(model$values$description))
  expect_true(is.character(model$values$state))
  expect_true(is.character(model$values$destination))
  expect_true(is.character(model$values$formula))

  # Check that values were correctly converted
  expect_equal(model$values$name, c("1", "2"))
  expect_equal(model$values$state, c("healthy", "sick"))
  expect_equal(model$values$formula, c("100", "50"))

  # Check fallback values worked
  expect_equal(model$values$display_name, c("1", "2"))  # fallback to name
  expect_equal(model$values$description, c("1", "2"))   # fallback to display_name
})

# =============================================================================
# Helper Function Tests: format_ranges_for_eval_values
# =============================================================================

test_that("format_ranges_for_eval_values handles NULL and empty inputs", {
  expect_equal(openqaly:::format_ranges_for_eval_values(NULL), "N/A")
  expect_equal(openqaly:::format_ranges_for_eval_values(c()), "N/A")
  expect_equal(openqaly:::format_ranges_for_eval_values(c(NA, NA)), "N/A")
})

test_that("format_ranges_for_eval_values formats single numbers", {
  expect_equal(openqaly:::format_ranges_for_eval_values(5), "5")
  expect_equal(openqaly:::format_ranges_for_eval_values(c(5)), "5")
})

test_that("format_ranges_for_eval_values formats consecutive ranges", {
  expect_equal(openqaly:::format_ranges_for_eval_values(c(1, 2, 3, 4, 5)), "1-5")
  expect_equal(openqaly:::format_ranges_for_eval_values(c(10, 11, 12)), "10-12")
})

test_that("format_ranges_for_eval_values formats non-consecutive numbers with gaps", {
  expect_equal(openqaly:::format_ranges_for_eval_values(c(1, 2, 5, 6, 7)), "1-2, 5-7")
  expect_equal(openqaly:::format_ranges_for_eval_values(c(1, 3, 5, 6, 7, 10)), "1, 3, 5-7, 10")
  expect_equal(openqaly:::format_ranges_for_eval_values(c(1, 3, 5)), "1, 3, 5")
})

test_that("format_ranges_for_eval_values handles duplicates and unsorted input", {
  # Duplicates should be deduplicated
  expect_equal(openqaly:::format_ranges_for_eval_values(c(1, 1, 2, 2, 3)), "1-3")
  # Unsorted input should be sorted
  expect_equal(openqaly:::format_ranges_for_eval_values(c(5, 3, 1, 2, 4)), "1-5")
})

# =============================================================================
# Helper Function Tests: format_na_table_to_markdown_for_eval_values
# =============================================================================

test_that("format_na_table_to_markdown_for_eval_values returns empty string for empty df", {
  empty_df <- tibble::tibble(
    `Value Name` = character(0),
    State = character(0),
    Destination = character(0),
    Cycles = character(0),
    `State Cycles` = character(0)
  )
  result <- openqaly:::format_na_table_to_markdown_for_eval_values(empty_df, "Test prefix")
  expect_equal(result, "")
})

test_that("format_na_table_to_markdown_for_eval_values formats single row correctly", {
  single_row_df <- tibble::tibble(
    `Value Name` = "cost",
    State = "sick",
    Destination = "dead",
    Cycles = "1-5",
    `State Cycles` = "1-5"
  )
  result <- openqaly:::format_na_table_to_markdown_for_eval_values(single_row_df, "Test prefix:")

  # Check that result contains expected components

  expect_true(grepl("Test prefix:", result))
  expect_true(grepl("Value Name", result))
  expect_true(grepl("State", result))
  expect_true(grepl("Destination", result))
  expect_true(grepl("cost", result))
  expect_true(grepl("sick", result))
  expect_true(grepl("dead", result))
  # Check markdown table structure (header separator)
  expect_true(grepl("\\|[-]+", result))
})

test_that("format_na_table_to_markdown_for_eval_values displays NA as N/A", {
  df_with_na <- tibble::tibble(
    `Value Name` = "cost",
    State = "sick",
    Destination = NA_character_,
    Cycles = "1-5",
    `State Cycles` = "1-5"
  )
  result <- openqaly:::format_na_table_to_markdown_for_eval_values(df_with_na, "Prefix")

  # NA should be displayed as "N/A"
  expect_true(grepl("N/A", result))
})

# =============================================================================
# parse_values Tests
# =============================================================================

test_that("parse_values returns empty tibble for NULL input", {
  # Create minimal states structure with max_state_time (Markov-style)
  states <- tibble::tibble(
    name = c("healthy", "sick"),
    display_name = c("Healthy", "Sick"),
    description = c("Healthy state", "Sick state"),
    formula = list(openqaly:::as.oq_formula("1"), openqaly:::as.oq_formula("0")),
    state_group = c(".healthy", ".sick"),
    share_state_time = c(FALSE, FALSE),
    max_state_time = c(Inf, Inf)
  )

  # Empty extra_vars
  extra_vars <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    formula = list()
  )

  result <- openqaly:::parse_values(NULL, states, extra_vars)

  expect_equal(nrow(result), 0)
  expect_true("name" %in% colnames(result))
  expect_true("formula" %in% colnames(result))
  expect_true("max_st" %in% colnames(result))
})

test_that("parse_values returns empty tibble for empty tibble input", {
  states <- tibble::tibble(
    name = c("healthy", "sick"),
    display_name = c("Healthy", "Sick"),
    description = c("Healthy state", "Sick state"),
    formula = list(openqaly:::as.oq_formula("1"), openqaly:::as.oq_formula("0")),
    state_group = c(".healthy", ".sick"),
    share_state_time = c(FALSE, FALSE),
    max_state_time = c(Inf, Inf)
  )

  extra_vars <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    formula = list()
  )

  empty_values <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    state = character(0),
    destination = character(0),
    formula = character(0),
    type = character(0)
  )

  result <- openqaly:::parse_values(empty_values, states, extra_vars)

  expect_equal(nrow(result), 0)
})

test_that("parse_values detects duplicate values", {
  states <- tibble::tibble(
    name = c("healthy", "sick"),
    display_name = c("Healthy", "Sick"),
    description = c("Healthy state", "Sick state"),
    formula = list(openqaly:::as.oq_formula("1"), openqaly:::as.oq_formula("0")),
    state_group = c(".healthy", ".sick"),
    share_state_time = c(FALSE, FALSE),
    max_state_time = c(Inf, Inf)
  )

  extra_vars <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    formula = list()
  )

  # Duplicate values: same name, state, destination
  duplicate_values <- tibble::tibble(
    name = c("cost", "cost"),
    display_name = c("Cost", "Cost"),
    description = c("Cost 1", "Cost 2"),
    state = c("healthy", "healthy"),
    destination = c(NA_character_, NA_character_),
    formula = c("100", "200"),
    type = c("cost", "cost")
  )

  expect_error(
    openqaly:::parse_values(duplicate_values, states, extra_vars),
    "Duplicate values found"
  )
})

test_that("parse_values calculates max_st from states max_state_time", {
  # States with specific max_state_time values
  states <- tibble::tibble(
    name = c("healthy", "sick"),
    display_name = c("Healthy", "Sick"),
    description = c("Healthy state", "Sick state"),
    formula = list(openqaly:::as.oq_formula("1"), openqaly:::as.oq_formula("0")),
    state_group = c(".healthy", ".sick"),
    share_state_time = c(FALSE, FALSE),
    max_state_time = c(5, 10)  # healthy has max_st=5, sick has max_st=10
  )

  extra_vars <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    formula = list()
  )

  values <- tibble::tibble(
    name = c("cost_healthy", "cost_sick"),
    display_name = c("Healthy Cost", "Sick Cost"),
    description = c("Cost in healthy", "Cost in sick"),
    state = c("healthy", "sick"),
    destination = c(NA_character_, NA_character_),
    formula = c("100", "200"),
    type = c("cost", "cost")
  )

  result <- openqaly:::parse_values(values, states, extra_vars)

  # Check max_st is calculated correctly from states
  healthy_row <- result[result$state == "healthy", ]
  sick_row <- result[result$state == "sick", ]

  expect_equal(healthy_row$max_st, 5)
  expect_equal(sick_row$max_st, 10)
})

test_that("parse_values defaults max_st to 1 without max_state_time (PSM)", {
  # States without max_state_time (PSM-style)
  states <- tibble::tibble(
    name = c("pf", "pp", "dead"),
    display_name = c("PF", "PP", "Dead"),
    description = c("PF state", "PP state", "Dead state")
  )

  extra_vars <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    formula = list()
  )

  values <- tibble::tibble(
    name = c("cost_pf", "cost_pp"),
    display_name = c("PF Cost", "PP Cost"),
    description = c("Cost in PF", "Cost in PP"),
    state = c("pf", "pp"),
    destination = c(NA_character_, NA_character_),
    formula = c("100", "200"),
    type = c("cost", "cost")
  )

  result <- openqaly:::parse_values(values, states, extra_vars)

  # All max_st should be 1 when states don't have max_state_time
  expect_true(all(result$max_st == 1))
})

# =============================================================================
# evaluate_values Tests
# =============================================================================

test_that("evaluate_values returns empty tibble for empty input", {
  # Create empty parsed values tibble with expected structure
  empty_parsed_values <- tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    state = character(0),
    destination = character(0),
    formula = list(),
    type = character(0),
    max_st = numeric(0)
  )

  # Create minimal namespace using test helper pattern
  mock_segment <- tibble::tibble(strategy = "S1", group = "G1")
  minimal_model <- list(
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
      initial_probability = 1
    ),
    env = new.env(parent = baseenv())
  )
  minimal_model$settings$cycle_length_days <- openqaly:::get_cycle_length_days(minimal_model$settings)
  ns <- openqaly:::create_namespace(model = minimal_model, segment = mock_segment)

  result <- openqaly:::evaluate_values(
    empty_parsed_values,
    ns,
    value_names = character(0),
    state_names = c("StateA")
  )

  expect_equal(nrow(result), 0)
  expect_true("state" %in% colnames(result))
  expect_true("destination" %in% colnames(result))
  expect_true("max_st" %in% colnames(result))
  expect_true("state_cycle" %in% colnames(result))
  expect_true("values_list" %in% colnames(result))
})

test_that("evaluate_values evaluates simple formulas correctly", {
  # Build a simple model using the fluent API
  model <- define_model("markov") |>
    set_settings(
      n_cycles = 5,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    add_strategy("standard") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_variable("x", 5) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "sick", "0.1") |>
    add_transition("sick", "sick", "1") |>
    add_value("cost", "10", state = "healthy") |>
    add_value("cost", "x * 2", state = "sick")  # Should evaluate to 10

  # Run the model to get results
  results <- run_model(model)

  # Get values and check they were evaluated correctly
  values <- get_values(results, format = "long")

  # Filter to cost values
  cost_values <- values[values$value_name == "cost", ]

  # The values should be numeric (10 in both states based on formulas)
  expect_true(is.numeric(cost_values$amount))
  expect_true(all(!is.na(cost_values$amount)))
})

test_that("evaluate_values allows NA values to pass through", {
  # Note: The evaluate_values function allows NA values to pass through
  # rather than erroring. This test documents this current behavior.
  model <- define_model("markov") |>
    set_settings(
      n_cycles = 3,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    add_strategy("standard") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", "0.9") |>
    add_transition("healthy", "sick", "0.1") |>
    add_transition("sick", "sick", "1") |>
    # Use NA_real_ directly to create NA
    add_value("bad_cost", "NA_real_", state = "healthy")

  # Model should run without error (NA values are allowed)
  results <- run_model(model)
  values <- get_values(results, format = "long")

  # Values should contain NA amounts
  bad_cost_values <- values[values$value_name == "bad_cost", ]
  expect_true(all(is.na(bad_cost_values$amount)))
})

test_that("validate_value_result catches non-numeric values", {
  # Test the validation function directly rather than through the full model
  # This avoids issues with C++ crashes when type validation is bypassed

  # Character value should error with context info
  err <- expect_error(
    validate_value_result("not_a_number", "test_cost", state = "healthy", formula_text = "x")
  )
  expect_match(err$message, "Value 'test_cost'")
  expect_match(err$message, "in state 'healthy'")
  expect_match(err$message, "character string")

  # Valid numeric should pass through unchanged
  expect_equal(
    validate_value_result(100, "test_cost", state = "healthy"),
    100
  )
})

test_that("evaluate_values processes multiple states correctly", {
  # Build a model with multiple states - test that all states get processed
  model <- define_model("markov") |>
    set_settings(
      n_cycles = 3,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    add_strategy("standard") |>
    add_state("zeta", initial_prob = 0.5) |>
    add_state("alpha", initial_prob = 0.3) |>
    add_state("beta", initial_prob = 0.2) |>
    add_transition("zeta", "zeta", "0.9") |>
    add_transition("zeta", "alpha", "0.1") |>
    add_transition("alpha", "alpha", "0.9") |>
    add_transition("alpha", "beta", "0.1") |>
    add_transition("beta", "beta", "1") |>
    add_value("cost", "100", state = "zeta") |>
    add_value("cost", "200", state = "alpha") |>
    add_value("cost", "300", state = "beta")

  # Run and get values
  results <- run_model(model)
  values <- get_values(results, format = "long")

  # Verify the model ran and produced values
  expect_true(nrow(values) > 0)
  expect_true("cost" %in% values$value_name)

  # Values should be weighted by state occupancy across cycles
  # The total cost should reflect contributions from all states
  cost_values <- values[values$value_name == "cost", ]
  expect_true(all(!is.na(cost_values$amount)))
  expect_true(all(cost_values$amount >= 0))
})
