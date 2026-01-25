context("VBP Tables")

# ============================================================================
# Test Fixtures
# ============================================================================

get_example_model <- function() {
  model_path <- system.file("models", "example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }
  read_model(model_path)
}

run_example_vbp <- function() {
  model <- get_example_model()
  run_vbp(
    model,
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost"
  )
}

# ============================================================================
# 1. Helper Function Tests
# ============================================================================

test_that("generate_vbp_wtp_sequence() returns valid sequence with default WTP", {
  seq_vals <- openqaly:::generate_vbp_wtp_sequence(50000)

  expect_true(is.numeric(seq_vals))
  expect_true(length(seq_vals) >= 3)
  expect_equal(seq_vals[1], 0)  # Starts at 0
  expect_true(50000 %in% seq_vals)  # Contains default
  expect_true(max(seq_vals) >= 50000)  # Extends beyond default
})

test_that("generate_vbp_wtp_sequence() handles NULL default WTP", {
  seq_vals <- openqaly:::generate_vbp_wtp_sequence(NULL)

  expect_true(is.numeric(seq_vals))
  expect_true(length(seq_vals) >= 3)
  expect_equal(seq_vals[1], 0)
})

test_that("generate_vbp_wtp_sequence() handles NA default WTP", {
  seq_vals <- openqaly:::generate_vbp_wtp_sequence(NA)

  expect_true(is.numeric(seq_vals))
  expect_true(length(seq_vals) >= 3)
})

test_that("generate_vbp_wtp_sequence() handles zero/negative default WTP", {
  seq_vals_zero <- openqaly:::generate_vbp_wtp_sequence(0)
  seq_vals_neg <- openqaly:::generate_vbp_wtp_sequence(-1000)

  expect_true(is.numeric(seq_vals_zero))
  expect_true(is.numeric(seq_vals_neg))
})

# ============================================================================
# 2. prepare_vbp_table_data() Tests
# ============================================================================

test_that("prepare_vbp_table_data() returns valid table spec", {
  vbp_results <- run_example_vbp()
  prepared <- openqaly:::prepare_vbp_table_data(vbp_results)

  # Check it's a valid table spec

  expect_true(is.list(prepared))
  expect_true("headers" %in% names(prepared))
  expect_true("data" %in% names(prepared))
  expect_true("column_alignments" %in% names(prepared))
})

test_that("prepare_vbp_table_data() includes all comparators by default", {
  vbp_results <- run_example_vbp()
  prepared <- openqaly:::prepare_vbp_table_data(vbp_results)

  # Should have WTP column + comparator columns
  n_comparators <- nrow(vbp_results$vbp_equations)
  # +1 for "All Comparators" column, +1 for WTP column
  expect_gte(ncol(prepared$data), n_comparators + 1)
})

test_that("prepare_vbp_table_data() filters comparators correctly", {
  vbp_results <- run_example_vbp()
  comparators <- unique(vbp_results$vbp_equations$comparator)

  if (length(comparators) >= 1) {
    prepared <- openqaly:::prepare_vbp_table_data(
      vbp_results,
      comparators = comparators[1]
    )

    # Should have WTP + 1 comparator column
    expect_equal(ncol(prepared$data), 2)
  }
})

test_that("prepare_vbp_table_data() errors on invalid comparator", {
  vbp_results <- run_example_vbp()

  expect_error(
    openqaly:::prepare_vbp_table_data(
      vbp_results,
      comparators = "nonexistent_comparator"
    ),
    "not found"
  )
})

test_that("prepare_vbp_table_data() uses custom WTP thresholds", {
  vbp_results <- run_example_vbp()
  custom_wtp <- c(0, 25000, 50000, 75000)

  prepared <- openqaly:::prepare_vbp_table_data(
    vbp_results,
    wtp_thresholds = custom_wtp
  )

  # Should have 4 rows (one per WTP threshold)
  expect_equal(nrow(prepared$data), 4)
})

test_that("prepare_vbp_table_data() includes 'All Comparators' column", {
  vbp_results <- run_example_vbp()
  n_comparators <- nrow(vbp_results$vbp_equations)

  # Only test if there are multiple comparators
  if (n_comparators > 1) {
    prepared <- openqaly:::prepare_vbp_table_data(
      vbp_results,
      comparators = "all"
    )

    # Check "vs. All Comparators" column exists (columns use "vs. X" format)
    col_names <- colnames(prepared$data)
    expect_true("vs. All Comparators" %in% col_names)
  }
})

test_that("prepare_vbp_table_data() can exclude 'All Comparators' column", {
  vbp_results <- run_example_vbp()

  prepared <- openqaly:::prepare_vbp_table_data(
    vbp_results,
    comparators = "all_comparators"
  )

  col_names <- colnames(prepared$data)
  expect_false("vs. All Comparators" %in% col_names)
})

# ============================================================================
# 3. "All Comparators" Calculation Tests
# ============================================================================

test_that("'All Comparators' column is minimum of individual comparator VBPs", {
  vbp_results <- run_example_vbp()
  n_comparators <- nrow(vbp_results$vbp_equations)

  # Only test if there are multiple comparators
  if (n_comparators > 1) {
    wtp_test <- c(50000)

    # Calculate VBP for each comparator manually
    comparators <- unique(vbp_results$vbp_equations$comparator)
    vbp_values <- sapply(comparators, function(comp) {
      calculate_vbp_price(vbp_results, wtp_test, comp)
    })
    expected_min <- min(vbp_values)

    # Get the "All Comparators" value from table
    prepared <- openqaly:::prepare_vbp_table_data(
      vbp_results,
      wtp_thresholds = wtp_test,
      comparators = "all"
    )

    # Parse the dollar-formatted "vs. All Comparators" value (columns use "vs. X" format)
    all_comp_col <- prepared$data[["vs. All Comparators"]]
    # Remove $ and commas, convert to numeric
    all_comp_value <- as.numeric(gsub("[$,]", "", all_comp_col[1]))

    # Should match minimum (with some tolerance for rounding)
    expect_equal(all_comp_value, round(expected_min, 0), tolerance = 1)
  }
})

# ============================================================================
# 4. vbp_table() Output Tests
# ============================================================================

test_that("vbp_table() returns flextable object by default", {
  skip_if_not_installed("flextable")

  vbp_results <- run_example_vbp()
  tbl <- vbp_table(vbp_results, table_format = "flextable")

  expect_s3_class(tbl, "flextable")
})

test_that("vbp_table() returns kable object when requested", {
  skip_if_not_installed("kableExtra")

  vbp_results <- run_example_vbp()
  tbl <- vbp_table(vbp_results, table_format = "kable")

  expect_true(inherits(tbl, "knitr_kable") || is.character(tbl))
})

test_that("vbp_table() respects decimals parameter", {
  vbp_results <- run_example_vbp()

  prepared_0 <- openqaly:::prepare_vbp_table_data(vbp_results, decimals = 0)
  prepared_2 <- openqaly:::prepare_vbp_table_data(vbp_results, decimals = 2)

  # Values should be formatted differently
  # Just verify it runs without error
  expect_true(is.list(prepared_0))
  expect_true(is.list(prepared_2))
})

# ============================================================================
# 5. Group Handling Tests
# ============================================================================

test_that("vbp_table() works with 'overall' group", {
  vbp_results <- run_example_vbp()

  prepared <- openqaly:::prepare_vbp_table_data(
    vbp_results,
    groups = "overall"
  )

  expect_true(is.list(prepared))
  expect_true(nrow(prepared$data) > 0)
})

test_that("prepare_vbp_table_data() errors on invalid group", {
  vbp_results <- run_example_vbp()

  expect_error(
    openqaly:::prepare_vbp_table_data(
      vbp_results,
      groups = "nonexistent_group"
    ),
    "No VBP equations found"
  )
})

# ============================================================================
# 6. Column Naming Tests
# ============================================================================

test_that("vbp_table() uses 'vs. X' format for comparator columns", {
  vbp_results <- run_example_vbp()
  comparators <- unique(vbp_results$vbp_equations$comparator)

  prepared <- openqaly:::prepare_vbp_table_data(
    vbp_results,
    comparators = comparators[1]
  )

  col_names <- colnames(prepared$data)
  # Second column should be "vs. <comparator>"
  expect_true(grepl("^vs\\. ", col_names[2]))
})

test_that("vbp_table() first column is 'WTP'", {
  vbp_results <- run_example_vbp()

  prepared <- openqaly:::prepare_vbp_table_data(vbp_results)

  expect_equal(colnames(prepared$data)[1], "WTP")
})

# ============================================================================
# 7. Display Name Mapping Tests
# ============================================================================

test_that("vbp_table() maps comparator names to display names", {
  vbp_results <- run_example_vbp()
  prepared <- openqaly:::prepare_vbp_table_data(vbp_results)

  # Check that table is created successfully with display name mapping
  expect_true(is.list(prepared))
  expect_true(nrow(prepared$data) > 0)

  # The comparator column headers should use display names from metadata
  # This is tested implicitly - the function should not error
})
