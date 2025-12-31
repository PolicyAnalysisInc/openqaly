context("Values DataFrame Type Safety")

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
    setting = c("n_cycles", "model_type"),
    value = c("10", "markov")
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
      setting = c("n_cycles", "model_type"),
      value = c("10", "markov")
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