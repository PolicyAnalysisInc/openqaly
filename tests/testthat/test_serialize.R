context("Serialization")

library(jsonlite)
library(tibble)

# =============================================================================
# Helper Functions
# =============================================================================

# Create oq_formula fixture from expression string
create_formula <- function(expr) {
  as.oq_formula(expr)
}

# Get real namespace from running a model
# This ensures we test with realistic data structures
get_real_results <- function() {
  model <- build_simple_psa_model()
  run_model(model)
}

# =============================================================================
# Tests for convert_oq_formulas()
# =============================================================================

test_that("convert_oq_formulas converts single oq_formula to string", {
  formula <- create_formula("x + y * z")
  result <- openqaly:::convert_oq_formulas(formula)

  expect_type(result, "character")
  expect_equal(result, "x + y * z")
})

test_that("convert_oq_formulas handles list of formulas", {
  formulas <- list(
    a = create_formula("x + 1"),
    b = create_formula("y * 2"),
    c = create_formula("sqrt(z)")
  )

  result <- openqaly:::convert_oq_formulas(formulas)

  expect_type(result, "list")
  expect_equal(result$a, "x + 1")
  expect_equal(result$b, "y * 2")
  expect_equal(result$c, "sqrt(z)")
})

test_that("convert_oq_formulas handles deeply nested structures", {
  nested <- list(
    level1 = list(
      level2 = list(
        formula = create_formula("deep_expression")
      )
    ),
    shallow = create_formula("top_level")
  )

  result <- openqaly:::convert_oq_formulas(nested)

  expect_equal(result$level1$level2$formula, "deep_expression")
  expect_equal(result$shallow, "top_level")
})

test_that("convert_oq_formulas handles dataframes with list columns containing formulas", {
  # Note: Due to R's is.list() returning TRUE for dataframes, the function
  # converts dataframes to lists. This test verifies current behavior.
  df <- tibble(
    name = c("var1", "var2"),
    formula_col = list(
      create_formula("a + b"),
      create_formula("c * d")
    )
  )

  result <- openqaly:::convert_oq_formulas(df)

  # Dataframes become lists due to is.list() check order in implementation
  expect_type(result, "list")
  expect_equal(result$name, c("var1", "var2"))
  expect_equal(result$formula_col[[1]], "a + b")
  expect_equal(result$formula_col[[2]], "c * d")
})

test_that("convert_oq_formulas preserves non-formula values", {
  input <- list(
    number = 42,
    string = "hello",
    vector = c(1, 2, 3),
    formula = create_formula("x")
  )

  result <- openqaly:::convert_oq_formulas(input)

  expect_equal(result$number, 42)
  expect_equal(result$string, "hello")
  expect_equal(result$vector, c(1, 2, 3))
  expect_equal(result$formula, "x")
})

test_that("convert_oq_formulas handles empty list", {
  result <- openqaly:::convert_oq_formulas(list())
  expect_equal(result, list())
})

test_that("convert_oq_formulas handles NULL input", {
  result <- openqaly:::convert_oq_formulas(NULL)
  expect_null(result)
})

# =============================================================================
# Tests for prepare_segments_for_json()
# =============================================================================

test_that("prepare_segments_for_json converts formulas in uneval_vars", {
  segments <- tibble(
    strategy = "A",
    uneval_vars = list(
      list(
        var1 = create_formula("x + 1"),
        var2 = create_formula("y * 2")
      )
    )
  )

  result <- openqaly:::prepare_segments_for_json(segments)

  expect_equal(result$uneval_vars[[1]]$var1, "x + 1")
  expect_equal(result$uneval_vars[[1]]$var2, "y * 2")
})

test_that("prepare_segments_for_json converts namespace to summary dataframe", {
  # Get real results with actual namespace objects
  results <- get_real_results()
  segments <- results$segments

  # Verify we have namespace objects in eval_vars
  expect_true("eval_vars" %in% names(segments))
  expect_s3_class(segments$eval_vars[[1]], "namespace")

  # Convert
  result <- openqaly:::prepare_segments_for_json(segments)

  # Should now be a dataframe (from summary())
  expect_s3_class(result$eval_vars[[1]], "data.frame")
  # summary.namespace returns columns: name, cycle, state_cycle, value, print, summary
  expect_true("name" %in% names(result$eval_vars[[1]]))
  expect_true("value" %in% names(result$eval_vars[[1]]))
})

test_that("prepare_segments_for_json handles segments without uneval_vars", {
  segments <- tibble(
    strategy = "A",
    other_col = "value"
  )

  result <- openqaly:::prepare_segments_for_json(segments)

  expect_equal(result$strategy, "A")
  expect_equal(result$other_col, "value")
})

test_that("prepare_segments_for_json handles segments without eval_vars", {
  segments <- tibble(
    strategy = "A",
    uneval_vars = list(list(var1 = create_formula("x")))
  )

  result <- openqaly:::prepare_segments_for_json(segments)

  expect_equal(result$uneval_vars[[1]]$var1, "x")
})

test_that("prepare_segments_for_json returns non-dataframe input unchanged", {
  result <- openqaly:::prepare_segments_for_json(list(a = 1, b = 2))
  expect_equal(result, list(a = 1, b = 2))

  result2 <- openqaly:::prepare_segments_for_json(NULL)
  expect_null(result2)
})

# =============================================================================
# Tests for prepare_model_results_for_json()
# =============================================================================

test_that("prepare_model_results_for_json processes real model results", {
  results <- get_real_results()

  serializable <- prepare_model_results_for_json(results)

  # Segments should still exist
  expect_true("segments" %in% names(serializable))
  # eval_vars should now be dataframes, not namespaces
  expect_s3_class(serializable$segments$eval_vars[[1]], "data.frame")
})

test_that("prepare_model_results_for_json handles missing segments", {
  model_results <- list(
    aggregated = list(a = 1),
    metadata = list(b = 2)
  )

  result <- prepare_model_results_for_json(model_results)

  expect_equal(result$aggregated, list(a = 1))
  expect_equal(result$metadata, list(b = 2))
  expect_null(result$segments)
})

test_that("prepare_model_results_for_json does not modify original", {
  formula <- create_formula("original")

  model_results <- list(
    segments = tibble(
      strategy = "A",
      uneval_vars = list(list(f = formula))
    )
  )

  # Store original class
  original_class <- class(model_results$segments$uneval_vars[[1]]$f)

  result <- prepare_model_results_for_json(model_results)

  # Original formula should still be oq_formula
  expect_equal(class(model_results$segments$uneval_vars[[1]]$f), original_class)
  # Result should be character
  expect_type(result$segments$uneval_vars[[1]]$f, "character")
})

test_that("prepare_model_results_for_json output is JSON-serializable", {
  results <- get_real_results()

  serializable <- prepare_model_results_for_json(results)

  # Should be able to convert to JSON without error
  json_str <- toJSON(serializable, auto_unbox = TRUE, null = "null", na = "null")

  # JSON should be valid
  expect_true(validate(json_str))
})

# =============================================================================
# Tests for export_model_json()
# =============================================================================

test_that("export_model_json writes valid JSON file", {
  results <- get_real_results()

  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)

  export_model_json(results, tmp_file)

  # File should exist
  expect_true(file.exists(tmp_file))

  # Content should be valid JSON
  content <- paste(readLines(tmp_file, warn = FALSE), collapse = "\n")
  expect_true(validate(content))
})

test_that("export_model_json respects pretty parameter", {
  model_results <- list(data = list(a = 1, b = 2, c = 3))

  tmp_file_pretty <- tempfile(fileext = ".json")
  tmp_file_compact <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_file_pretty)
    unlink(tmp_file_compact)
  }, add = TRUE)

  export_model_json(model_results, tmp_file_pretty, pretty = TRUE)
  export_model_json(model_results, tmp_file_compact, pretty = FALSE)

  # Pretty should have more lines (newlines for formatting)
  lines_pretty <- length(readLines(tmp_file_pretty, warn = FALSE))
  lines_compact <- length(readLines(tmp_file_compact, warn = FALSE))

  expect_gt(lines_pretty, lines_compact)
})

test_that("export_model_json respects auto_unbox parameter", {
  model_results <- list(single_value = 42)

  tmp_file_unbox <- tempfile(fileext = ".json")
  tmp_file_box <- tempfile(fileext = ".json")
  on.exit({
    unlink(tmp_file_unbox)
    unlink(tmp_file_box)
  }, add = TRUE)

  export_model_json(model_results, tmp_file_unbox, auto_unbox = TRUE)
  export_model_json(model_results, tmp_file_box, auto_unbox = FALSE)

  content_unbox <- paste(readLines(tmp_file_unbox, warn = FALSE), collapse = "")
  content_box <- paste(readLines(tmp_file_box, warn = FALSE), collapse = "")

  # Unboxed: 42 (not in array), Boxed: [42] (in array)
  expect_false(grepl("\\[\\s*42\\s*\\]", content_unbox))
  expect_true(grepl("\\[\\s*42\\s*\\]", content_box))
})

test_that("export_model_json returns serializable invisibly", {
  model_results <- list(data = 123)

  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)

  result <- export_model_json(model_results, tmp_file)

  expect_equal(result$data, 123)
})

test_that("export_model_json roundtrip works with real model", {
  results <- get_real_results()

  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)

  export_model_json(results, tmp_file)

  # Should be parseable back to R
  content <- paste(readLines(tmp_file, warn = FALSE), collapse = "\n")
  parsed <- fromJSON(content, simplifyVector = FALSE)

  expect_type(parsed, "list")
  expect_true("segments" %in% names(parsed))
})
