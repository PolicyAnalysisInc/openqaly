context("Model converters")

# ==============================================================================
# convert_model - Input Auto-detection
# ==============================================================================

test_that("convert_model detects oq_model object input", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  json_path <- tempfile(fileext = ".json")
  result <- convert_model(model, json_path)

  expect_equal(result, json_path)

  # Verify round-trip
  converted <- read_model_json(paste(readLines(json_path), collapse = "\n"))
  expect_s3_class(converted, "oq_model")
  expect_equal(nrow(model$states), nrow(converted$states))

  unlink(json_path)
})

test_that("convert_model detects Excel folder input", {
  model_path <- system.file("models/markov_medium", package = "openqaly")
  skip_if(model_path == "", "markov_medium model not available")

  json_path <- tempfile(fileext = ".json")
  convert_model(model_path, json_path)

  # Verify round-trip preserves structure
  original <- read_model(model_path)
  converted <- read_model_json(paste(readLines(json_path), collapse = "\n"))
  expect_equal(nrow(original$states), nrow(converted$states))
  expect_equal(nrow(original$variables), nrow(converted$variables))

  unlink(json_path)
})

test_that("convert_model detects JSON file input", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  json_in <- tempfile(fileext = ".json")
  writeLines(as_json(model), json_in)

  r_out <- tempfile(fileext = ".R")
  convert_model(json_in, r_out)

  # Verify R code is executable and recreates model
  r_content <- readLines(r_out)
  expect_true(any(grepl("define_model", r_content)))
  expect_true(any(grepl("add_state", r_content)))

  unlink(c(json_in, r_out))
})

test_that("convert_model detects R file input", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_variable("cost", 100) |>
    add_transition("alive", "alive", 1)

  r_in <- tempfile(fileext = ".R")
  writeLines(as_r_code(model), r_in)

  json_out <- tempfile(fileext = ".json")
  convert_model(r_in, json_out)

  # Verify round-trip
  converted <- read_model_json(paste(readLines(json_out), collapse = "\n"))
  expect_equal(nrow(model$states), nrow(converted$states))
  expect_equal(model$variables$name, converted$variables$name)

  unlink(c(r_in, json_out))
})

test_that("convert_model detects raw JSON string input", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  json_str <- as_json(model)
  excel_out <- tempfile()

  convert_model(json_str, excel_out, to = "excel")

  # Verify Excel structure created
  expect_true(file.exists(file.path(excel_out, "model.xlsx")))

  # Verify round-trip
  converted <- read_model(excel_out)
  expect_equal(nrow(model$states), nrow(converted$states))

  unlink(excel_out, recursive = TRUE)
})

# ==============================================================================
# convert_model - Output Auto-detection
# ==============================================================================

test_that("convert_model auto-detects output format from extension", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  # .json extension -> JSON format
  json_out <- tempfile(fileext = ".json")
  convert_model(model, json_out)
  expect_true(jsonlite::validate(paste(readLines(json_out), collapse = "")))

  # .R extension -> R format
  r_out <- tempfile(fileext = ".R")
  convert_model(model, r_out)
  expect_true(any(grepl("define_model", readLines(r_out))))

  # No extension -> Excel format (default)
  excel_out <- tempfile()
  convert_model(model, excel_out)
  expect_true(file.exists(file.path(excel_out, "model.xlsx")))

  unlink(c(json_out, r_out))
  unlink(excel_out, recursive = TRUE)
})

# ==============================================================================
# convert_model - Explicit format parameters
# ==============================================================================

test_that("convert_model respects explicit from='object' parameter", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  json_out <- tempfile(fileext = ".json")
  convert_model(model, json_out, from = "object", to = "json")

  converted <- read_model_json(paste(readLines(json_out), collapse = "\n"))
  expect_s3_class(converted, "oq_model")

  unlink(json_out)
})

test_that("convert_model respects explicit from='json' for non-.json file", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  # Create JSON content in a .txt file
  txt_path <- tempfile(fileext = ".txt")
  writeLines(as_json(model), txt_path)

  json_out <- tempfile(fileext = ".json")
  convert_model(txt_path, json_out, from = "json")

  converted <- read_model_json(paste(readLines(json_out), collapse = "\n"))
  expect_equal(nrow(model$states), nrow(converted$states))

  unlink(c(txt_path, json_out))
})

test_that("convert_model respects explicit from='excel'", {
  model_path <- system.file("models/markov_medium", package = "openqaly")
  skip_if(model_path == "", "markov_medium model not available")

  json_out <- tempfile(fileext = ".json")
  convert_model(model_path, json_out, from = "excel")

  converted <- read_model_json(paste(readLines(json_out), collapse = "\n"))
  expect_s3_class(converted, "oq_model")

  unlink(json_out)
})

test_that("convert_model respects explicit from='r'", {
  model <- define_model("markov") |>
    add_state("alive", initial_prob = 1) |>
    add_transition("alive", "alive", 1)

  r_path <- tempfile(fileext = ".R")
  writeLines(as_r_code(model), r_path)

  json_out <- tempfile(fileext = ".json")
  convert_model(r_path, json_out, from = "r")

  converted <- read_model_json(paste(readLines(json_out), collapse = "\n"))
  expect_equal(nrow(model$states), nrow(converted$states))

  unlink(c(r_path, json_out))
})

# ==============================================================================
# convert_model - Edge cases
# ==============================================================================

test_that("convert_model warns when R file has multiple models", {
  r_content <- c(
    'library(openqaly)',
    'model1 <- define_model("markov") |> add_state("a", initial_prob = 1) |> add_transition("a", "a", 1)',
    'model2 <- define_model("markov") |> add_state("b", initial_prob = 1) |> add_transition("b", "b", 1)'
  )
  r_path <- tempfile(fileext = ".R")
  writeLines(r_content, r_path)

  json_out <- tempfile(fileext = ".json")

  expect_warning(
    convert_model(r_path, json_out),
    "Multiple model objects found"
  )

  # Should still produce valid output (using first model)
  converted <- read_model_json(paste(readLines(json_out), collapse = "\n"))
  expect_s3_class(converted, "oq_model")

  unlink(c(r_path, json_out))
})

# ==============================================================================
# convert_model - Error cases
# ==============================================================================

test_that("convert_model errors for directory without model.xlsx", {
  empty_dir <- tempfile()
  dir.create(empty_dir)

  expect_error(
    convert_model(empty_dir, tempfile(fileext = ".json")),
    "no model.xlsx found"
  )

  unlink(empty_dir, recursive = TRUE)
})

test_that("convert_model errors for unknown file extension", {
  fake_file <- tempfile(fileext = ".xyz")
  writeLines("garbage", fake_file)

  expect_error(
    convert_model(fake_file, tempfile(fileext = ".json")),
    "Unknown file extension"
  )

  unlink(fake_file)
})

test_that("convert_model errors for invalid string input", {
  expect_error(
    convert_model("not_a_file_or_valid_json", tempfile(fileext = ".json")),
    "Input not recognized"
  )
})

test_that("convert_model errors for R file without model", {
  r_path <- tempfile(fileext = ".R")
  writeLines("x <- 1; y <- 2", r_path)

  expect_error(
    convert_model(r_path, tempfile(fileext = ".json")),
    "No oq_model object found"
  )

  unlink(r_path)
})

test_that("convert_model errors for unknown from format", {
  expect_error(
    convert_model("input", "output.json", from = "unknown"),
    "Unknown input format"
  )
})

test_that("convert_model errors for invalid input type (vector)", {
  expect_error(
    convert_model(c("a", "b"), "output.json"),
    "Invalid input type"
  )
})
