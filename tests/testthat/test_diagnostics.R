# Diagnostics Tests
context("Diagnostics")

# =============================================================================
# Helper: load and run the example PSM model
# =============================================================================
load_example_psm_results <- function() {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }
  model <- read_model(model_path)
  run_model(model)
}

# =============================================================================
# export.default tests
# =============================================================================

test_that("export.default adds type field for scalars", {
  res <- export(42)
  expect_equal(res$type, "scalar")
  expect_equal(res$value, 42)
  expect_equal(res$class, "numeric")
})

test_that("export.default adds type field for vectors", {
  res <- export(c(1, 2, 3))
  expect_equal(res$type, "vector")
  expect_equal(res$values, c(1, 2, 3))
})

test_that("export.default marks non-numeric as object", {
  res <- export(list(a = 1))
  expect_equal(res$type, "object")
})

test_that("export.default handles character", {
  res <- export("hello")
  expect_equal(res$type, "object")
  expect_true(!is.null(res$print))
})

# =============================================================================
# export.data.frame tests
# =============================================================================

test_that("export.data.frame returns correct structure", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  res <- export(df)
  expect_equal(res$type, "data.frame")
  expect_equal(res$columns, c("x", "y"))
  expect_equal(res$nrow, 3)
  expect_equal(res$ncol, 2)
  expect_equal(length(res$data), 3)
})

# =============================================================================
# diagnose_variable tests (using example PSM model)
# =============================================================================

test_that("diagnose_variable detects shared variables", {
  results <- load_example_psm_results()

  # Find a variable that is NOT strategy-specific (shared across all)
  shared_vars <- results$metadata$variables
  shared <- shared_vars[is.na(shared_vars$strategy) | shared_vars$strategy == "", ]
  if (nrow(shared) > 0) {
    var_name <- shared$name[1]
    diag <- diagnose_variable(results, var_name)

    expect_equal(diag$name, var_name)
    expect_true(!is.null(diag$type))
    expect_true(is.character(diag$text))
    expect_true(inherits(diag$table, "flextable") || is.null(diag$table))
    expect_true(inherits(diag$plot, "gg") || is.null(diag$plot))
  }
})

test_that("diagnose_variable detects strategy-specific variables", {
  results <- load_example_psm_results()

  # c_drug should vary by strategy
  diag <- diagnose_variable(results, "c_drug")

  expect_equal(diag$name, "c_drug")
  expect_true(!is.null(diag$type))
  expect_true(is.character(diag$text))
  expect_true(inherits(diag$table, "flextable") || is.null(diag$table))
  expect_true(inherits(diag$plot, "gg") || is.null(diag$plot))


  # For varying scalars, text is empty and a Values table is produced instead
  if (diag$type == "scalar") {
    expect_true(!is.null(diag$table))
  } else {
    # Strategy names should appear in the text for non-scalar types
    expect_true(grepl("chemo", diag$text, ignore.case = TRUE) ||
                grepl("targeted", diag$text, ignore.case = TRUE) ||
                grepl("immuno", diag$text, ignore.case = TRUE))
  }
})

test_that("diagnose_variable returns correct output structure", {
  results <- load_example_psm_results()

  diag <- diagnose_variable(results, "c_drug")

  expect_true("name" %in% names(diag))
  expect_true("display_name" %in% names(diag))
  expect_true("formula_text" %in% names(diag))
  expect_true("type" %in% names(diag))
  expect_true("text" %in% names(diag))
  expect_true("table" %in% names(diag))
  expect_true("plot" %in% names(diag))
})

test_that("diagnose_variable scalar type has Value table and empty plots", {
  results <- load_example_psm_results()

  # Find a shared scalar variable
  shared_vars <- results$metadata$variables
  shared <- shared_vars[is.na(shared_vars$strategy) | shared_vars$strategy == "", ]
  if (nrow(shared) > 0) {
    var_name <- shared$name[1]
    diag <- diagnose_variable(results, var_name)

    if (diag$type == "scalar") {
      # Non-varying scalars produce text only, no table
      expect_null(diag$table)
      expect_true(nchar(diag$text) > 0)
      expect_null(diag$plot)
    }
  }
})

test_that("diagnose_variable vector type has Values table and Trend plot", {
  results <- load_example_psm_results()

  # Find a variable that is in ns$df and varies by cycle
  all_diag <- diagnose_all_variables(results)
  vector_diags <- all_diag[sapply(all_diag, function(d) {
    !is.null(d$type) && d$type == "vector"
  })]

  if (length(vector_diags) > 0) {
    diag <- vector_diags[[1]]
    expect_true(!is.null(diag$table))
    expect_true(!is.null(diag$plot))
  }
})

# =============================================================================
# diagnose_all_variables tests
# =============================================================================

test_that("diagnose_all_variables returns named list for all variables", {
  results <- load_example_psm_results()

  all_diag <- diagnose_all_variables(results)
  var_names <- unique(results$metadata$variables$name)

  expect_true(is.list(all_diag))
  expect_equal(length(all_diag), length(var_names))
  expect_setequal(names(all_diag), var_names)
})

test_that("diagnose_all_variables all have text/tables/plots", {
  results <- load_example_psm_results()
  all_diag <- diagnose_all_variables(results)

  for (diag in all_diag) {
    if (is.null(diag$error)) {
      expect_true(is.character(diag$text), info = paste("Variable:", diag$name))
      expect_true(inherits(diag$table, "flextable") || is.null(diag$table),
                  info = paste("Variable:", diag$name))
      expect_true(inherits(diag$plot, "gg") || is.null(diag$plot),
                  info = paste("Variable:", diag$name))
    }
  }
})

test_that("diagnose_all_variables handles empty variables", {
  results <- list(
    segments = data.frame(strategy = character(0), group = character(0)),
    metadata = list(variables = data.frame(name = character(0)))
  )
  all_diag <- diagnose_all_variables(results)
  expect_equal(length(all_diag), 0)
})

# =============================================================================
# Internal helper tests
# =============================================================================

test_that("values_are_equal handles numeric with tolerance", {
  expect_true(openqaly:::values_are_equal(1.0, 1.0 + 1e-16))
  expect_false(openqaly:::values_are_equal(1.0, 2.0))
  expect_true(openqaly:::values_are_equal(c(1, 2, 3), c(1, 2, 3)))
  expect_false(openqaly:::values_are_equal(c(1, 2, 3), c(1, 2, 4)))
})

test_that("values_are_equal handles NULL", {
  expect_true(openqaly:::values_are_equal(NULL, NULL))
  expect_false(openqaly:::values_are_equal(NULL, 1))
  expect_false(openqaly:::values_are_equal(1, NULL))
})

test_that("values_are_equal handles non-numeric objects", {
  expect_true(openqaly:::values_are_equal("abc", "abc"))
  expect_false(openqaly:::values_are_equal("abc", "def"))
})
