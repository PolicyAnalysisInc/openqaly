context("Miscellaneous utility functions")

# ==============================================================================
# vswitch() Tests - EXPORTED
# ==============================================================================

test_that("vswitch handles basic string matching with scalar values", {
  # Input: vector of conditions
  # Options: scalar values for each case
  x <- c("a", "b", "c", "a")
  result <- vswitch(x, a = 1, b = 2, c = 3)

  # Expected: each "a" -> 1, "b" -> 2, "c" -> 3
  expect_equal(result, c(1, 2, 3, 1))
})

test_that("vswitch handles character output type", {
  x <- c("red", "green", "blue")
  result <- vswitch(x, red = "R", green = "G", blue = "B")

  expect_equal(result, c("R", "G", "B"))
  expect_type(result, "character")
})

test_that("vswitch returns NA of correct type for unmatched values", {
  # Numeric case
  x <- c("a", "b", "unknown")
  result <- vswitch(x, a = 1, b = 2)

  # "unknown" has no match, should return NA (numeric)
  expect_equal(result, c(1, 2, NA_real_))

  # Character case
  result_chr <- vswitch(x, a = "A", b = "B")
  expect_equal(result_chr, c("A", "B", NA_character_))
})

test_that("vswitch handles NA in input", {
  x <- c("a", NA, "b")
  result <- vswitch(x, a = 10, b = 20)

  # NA input should produce NA output
  expect_equal(result, c(10, NA_real_, 20))
})

test_that("vswitch handles vector option values (positional matching)", {
  x <- c("a", "b", "a", "b")
  # When option value is a vector, it uses positional matching
  result <- vswitch(x, a = c(1, 2, 3, 4), b = c(10, 20, 30, 40))

  # At position 1: x[1]="a", uses a[1]=1

  # At position 2: x[2]="b", uses b[2]=20
  # At position 3: x[3]="a", uses a[3]=3
  # At position 4: x[4]="b", uses b[4]=40
  expect_equal(result, c(1, 20, 3, 40))
})

test_that("vswitch handles empty input",
{
  x <- character(0)
  result <- vswitch(x, a = 1, b = 2)

  expect_length(result, 0)
})

test_that("vswitch handles all NAs in input", {
  x <- c(NA_character_, NA_character_)
  result <- vswitch(x, a = 1, b = 2)

  expect_equal(result, c(NA_real_, NA_real_))
})

test_that("vswitch handles logical output type", {
  x <- c("yes", "no", "yes")
  result <- vswitch(x, yes = TRUE, no = FALSE)

  expect_equal(result, c(TRUE, FALSE, TRUE))
  expect_type(result, "logical")
})

# ==============================================================================
# read_workbook() Tests - EXPORTED
# ==============================================================================

test_that("read_workbook reads Excel file with multiple sheets", {
  # Use existing test fixture
  test_file <- system.file("test_cases", "test_variables.xlsx", package = "openqaly")
  skip_if(test_file == "", "Test file not found")

  result <- read_workbook(test_file)

  # Verify it's a named list
  expect_type(result, "list")

  # Verify sheet names are preserved
  expect_true(length(names(result)) > 0)

  # Verify each element is a tibble/data.frame
  for (sheet_name in names(result)) {
    expect_true(is.data.frame(result[[sheet_name]]),
                info = paste("Sheet", sheet_name, "should be a data.frame"))
  }
})

test_that("read_workbook sheets have expected content structure", {
  test_file <- system.file("test_cases", "test_variables.xlsx", package = "openqaly")
  skip_if(test_file == "", "Test file not found")

  result <- read_workbook(test_file)

  # The sorted sheet should have the standard variable columns
  if ("sorted" %in% names(result)) {
    expect_true("name" %in% colnames(result$sorted))
    expect_true("formula" %in% colnames(result$sorted))
  }
})

# ==============================================================================
# read_model() Tests - EXPORTED
# ==============================================================================

test_that("read_model loads checkimab_simple model correctly", {
  model_path <- system.file("models", "checkimab_simple", package = "openqaly")
  skip_if(model_path == "", "Model fixture not found")

  model <- read_model(model_path)

  # Verify class
  expect_s3_class(model, "oq_model")

  # Verify states exist and have expected structure
  expect_true("states" %in% names(model))
  expect_true(is.data.frame(model$states))
  expect_true("name" %in% colnames(model$states))

  # checkimab_simple should have 3 states
  expect_equal(nrow(model$states), 3)
  # Verify state names exist and are non-empty
  expect_true(all(!is.na(model$states$name)))
  expect_true(all(nchar(model$states$name) > 0))
})

test_that("read_model loads markov_medium model with strategies and groups", {
  model_path <- system.file("models", "markov_medium", package = "openqaly")
  skip_if(model_path == "", "Model fixture not found")

  model <- read_model(model_path)

  # Verify class
  expect_s3_class(model, "oq_model")

  # Should have strategies defined
  expect_true("strategies" %in% names(model))
  expect_true(is.data.frame(model$strategies))
  expect_true(nrow(model$strategies) > 0)

  # Settings should be converted to list
  expect_true(is.list(model$settings))
  expect_false(is.data.frame(model$settings))
})

test_that("read_model sets default empty tables and scripts", {
  model_path <- system.file("models", "checkimab_simple", package = "openqaly")
  skip_if(model_path == "", "Model fixture not found")

  model <- read_model(model_path)

  # tables and scripts should exist even if empty
  expect_true("tables" %in% names(model))
  expect_true("scripts" %in% names(model))
  expect_type(model$tables, "list")
  expect_type(model$scripts, "list")
})

# ==============================================================================
# write_model_json() Tests - EXPORTED
# ==============================================================================

test_that("write_model_json errors on non-oq_model input", {
  fake_model <- list(a = 1, b = 2)

  expect_error(
    write_model_json(fake_model),
    "Input must be a oq_model object"
  )
})

test_that("write_model_json errors on NULL settings", {
  fake_model <- list(strategies = data.frame(name = "test"))
  class(fake_model) <- "oq_model"
  fake_model$settings <- NULL

  expect_error(
    write_model_json(fake_model),
    "Model settings must be a list"
  )
})

test_that("write_model_json produces valid JSON that can be parsed back", {
  model_path <- system.file("models", "checkimab_simple", package = "openqaly")
  skip_if(model_path == "", "Model fixture not found")

  model <- read_model(model_path)
  json_string <- write_model_json(model)

  # Verify it's a non-empty character string
  expect_type(json_string, "character")
  expect_true(nchar(json_string) > 0)

  # Verify it can be parsed back
  parsed_model <- read_model_json(json_string)
  expect_s3_class(parsed_model, "oq_model")

  # Key structure should be preserved
  expect_equal(nrow(model$states), nrow(parsed_model$states))
  expect_equal(model$states$name, parsed_model$states$name)
})

# ==============================================================================
# read_model_json() Tests - EXPORTED
# ==============================================================================

test_that("read_model_json parses minimal valid JSON model", {
  # Create minimal valid JSON model structure
  json_string <- '{
    "settings": [
      {"setting": "model_type", "value": "markov"},
      {"setting": "n_cycles", "value": "10"},
      {"setting": "cycle_length", "value": "1"},
      {"setting": "cycle_length_unit", "value": "year"},
      {"setting": "discount_cost", "value": "3.5"},
      {"setting": "discount_outcomes", "value": "3.5"}
    ],
    "states": [
      {"name": "alive", "initial_probability": "1", "absorbing": "0"},
      {"name": "dead", "initial_probability": "0", "absorbing": "1"}
    ],
    "strategies": [
      {"name": "control", "display_name": "Control", "enabled": "1"}
    ],
    "transitions": [
      {"from_state": "alive", "to_state": "dead", "formula": "0.1"},
      {"from_state": "alive", "to_state": "alive", "formula": "C"}
    ]
  }'

  model <- read_model_json(json_string)

  expect_s3_class(model, "oq_model")
  expect_equal(nrow(model$states), 2)
  expect_equal(model$states$name, c("alive", "dead"))
  expect_equal(model$settings$model_type, "markov")
})

test_that("read_model_json handles tables array-of-objects format", {
  json_string <- '{
    "settings": [
      {"setting": "model_type", "value": "markov"},
      {"setting": "n_cycles", "value": "5"},
      {"setting": "cycle_length", "value": "1"},
      {"setting": "cycle_length_unit", "value": "year"},
      {"setting": "discount_cost", "value": "3.5"},
      {"setting": "discount_outcomes", "value": "3.5"}
    ],
    "states": [
      {"name": "alive", "initial_probability": "1", "absorbing": "0"},
      {"name": "dead", "initial_probability": "0", "absorbing": "1"}
    ],
    "strategies": [
      {"name": "control", "display_name": "Control", "enabled": "1"}
    ],
    "transitions": [
      {"from_state": "alive", "to_state": "dead", "formula": "0.1"},
      {"from_state": "alive", "to_state": "alive", "formula": "C"}
    ],
    "tables": [
      {
        "name": "my_table",
        "data": {
          "rows": [
            {"age": 50, "rate": 0.01},
            {"age": 60, "rate": 0.02}
          ]
        }
      }
    ]
  }'

  model <- read_model_json(json_string)

  # Tables should be converted to named list with data + description structure
  expect_type(model$tables, "list")
  expect_true("my_table" %in% names(model$tables))
  # Access table data from the new structure
  table_data <- model$tables$my_table$data
  expect_equal(nrow(table_data), 2)
  expect_equal(table_data$age, c(50, 60))
})

# ==============================================================================
# normalize_and_validate_model() Tests - EXPORTED
# ==============================================================================

test_that("normalize_and_validate_model handles markov model type", {
  model_path <- system.file("models", "checkimab_simple", package = "openqaly")
  skip_if(model_path == "", "Model fixture not found")

  model <- read_model(model_path)

  # Model type should be normalized to lowercase "markov"
  expect_equal(tolower(model$settings$model_type), "markov")
  expect_s3_class(model, "oq_model")
})

test_that("normalize_and_validate_model handles PSM model type", {
  model_path <- system.file("models", "example_psm", package = "openqaly")
  skip_if(model_path == "", "PSM model fixture not found")

  model <- read_model(model_path)

  expect_equal(tolower(model$settings$model_type), "psm")
  expect_s3_class(model, "oq_model")
})

test_that("normalize_and_validate_model warns on invalid model_type", {
  # Create a raw model structure with invalid type
  raw_model <- list(
    settings = list(model_type = "invalid_type"),
    states = tibble::tibble(
      name = c("alive", "dead"),
      initial_probability = c(1, 0),
      absorbing = c(0, 1)
    ),
    strategies = tibble::tibble(name = "control"),
    transitions = tibble::tibble(
      from_state = c("alive", "alive"),
      to_state = c("alive", "dead"),
      formula = c("C", "0.1")
    )
  )

  expect_warning(
    normalize_and_validate_model(raw_model),
    "Invalid model_type"
  )
})

# ==============================================================================
# validate_group_names() Tests - Defensive code path
# ==============================================================================

test_that("validate_group_names passes for non-reserved names", {
  result <- openqaly:::validate_group_names(c("group_a", "group_b", "patients"))
  expect_true(result)
})
test_that("validate_group_names errors on 'overall' keyword", {
  expect_error(
    openqaly:::validate_group_names(c("group_a", "overall")),
    "Reserved group names detected.*overall"
  )
})

test_that("validate_group_names errors on 'all' keyword", {
  expect_error(
    openqaly:::validate_group_names(c("all", "group_b")),
    "Reserved group names detected.*all"
  )
})

test_that("validate_group_names errors on 'all_groups' keyword", {
  expect_error(
    openqaly:::validate_group_names(c("all_groups")),
    "Reserved group names detected.*all_groups"
  )
})

# ==============================================================================
# format_dataframe_as_markdown_table() Tests - Defensive code path
# ==============================================================================

test_that("format_dataframe_as_markdown_table handles empty dataframe", {
  empty_df <- data.frame()
  result <- openqaly:::format_dataframe_as_markdown_table(empty_df)

  expect_equal(result, "")
})

test_that("format_dataframe_as_markdown_table handles NULL input", {
  result <- openqaly:::format_dataframe_as_markdown_table(NULL)
  expect_equal(result, "")
})

test_that("format_dataframe_as_markdown_table formats simple dataframe", {
  df <- data.frame(
    Name = c("Alice", "Bob"),
    Age = c(30, 25),
    stringsAsFactors = FALSE
  )

  result <- openqaly:::format_dataframe_as_markdown_table(df)

  # Should contain header, separator, and data rows
  expect_true(grepl("Name", result))
  expect_true(grepl("Age", result))
  expect_true(grepl("Alice", result))
  expect_true(grepl("Bob", result))
  expect_true(grepl("\\|", result))  # Should have pipe characters
  expect_true(grepl("-", result))     # Should have separator dashes
})

test_that("format_dataframe_as_markdown_table handles NA values", {
  df <- data.frame(
    Col1 = c("a", NA),
    Col2 = c(NA, "b"),
    stringsAsFactors = FALSE
  )

  result <- openqaly:::format_dataframe_as_markdown_table(df)

  # NA values should be represented as "NA" in the output
  expect_true(grepl("NA", result))
})

# ==============================================================================
# apply_setting_overrides() Tests - Defensive code path
# ==============================================================================

test_that("apply_setting_overrides returns unchanged model when no overrides column", {
  segment <- tibble::tibble(strategy = "A", group = "G1")
  model <- list(settings = list(n_cycles = 10))

  result <- openqaly:::apply_setting_overrides(segment, model)

  expect_equal(result$settings$n_cycles, 10)
})

test_that("apply_setting_overrides returns unchanged model when overrides empty", {
  segment <- tibble::tibble(
    strategy = "A",
    group = "G1",
    setting_overrides = list(list())
  )
  model <- list(settings = list(n_cycles = 10))

  result <- openqaly:::apply_setting_overrides(segment, model)

  expect_equal(result$settings$n_cycles, 10)
})

test_that("apply_setting_overrides applies override values", {
  segment <- tibble::tibble(
    strategy = "A",
    group = "G1",
    setting_overrides = list(list(n_cycles = 20, discount_cost = 5))
  )
  model <- list(settings = list(n_cycles = 10, discount_cost = 3.5))

  result <- openqaly:::apply_setting_overrides(segment, model)

  expect_equal(result$settings$n_cycles, 20)
  expect_equal(result$settings$discount_cost, 5)
})

# ==============================================================================
# apply_parameter_overrides() Tests - Defensive code path
# ==============================================================================

test_that("apply_parameter_overrides returns unchanged when no overrides column", {
  segment <- tibble::tibble(strategy = "A", group = "G1")
  ns <- list(env = new.env())
  uneval_vars <- tibble::tibble(name = c("var1", "var2"), formula = list(NULL, NULL))

  result <- openqaly:::apply_parameter_overrides(segment, ns, uneval_vars)

  expect_equal(nrow(result$uneval_vars), 2)
})

test_that("apply_parameter_overrides applies overrides and filters vars", {
  segment <- tibble::tibble(
    strategy = "A",
    group = "G1",
    parameter_overrides = list(list(var1 = 100))
  )
  ns <- list(env = new.env())
  uneval_vars <- tibble::tibble(name = c("var1", "var2"), formula = list(NULL, NULL))

  result <- openqaly:::apply_parameter_overrides(segment, ns, uneval_vars)

  # var1 should be assigned in environment
  expect_equal(get("var1", envir = result$ns$env), 100)

  # var1 should be removed from uneval_vars
  expect_equal(result$uneval_vars$name, "var2")
})
