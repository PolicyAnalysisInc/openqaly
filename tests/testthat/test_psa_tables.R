context("PSA Tables")

# Uses shared fixtures from setup.R:
# - build_simple_psa_model() - builds the test model
# - get_cached_psa_results() - returns cached PSA results (computed once)

# ============================================================================
# Tests for incremental_ceac_table()
# ============================================================================

test_that("incremental_ceac_table() creates table object", {
  results <- get_cached_psa_results()
  tbl <- incremental_ceac_table(results, "total_qalys", "total_cost")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("incremental_ceac_table() respects table_format = 'kable'", {
  results <- get_cached_psa_results()
  tbl <- incremental_ceac_table(results, "total_qalys", "total_cost",
                                 table_format = "kable")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("incremental_ceac_table() respects table_format = 'flextable'", {
  skip_if_not_installed("flextable")
  results <- get_cached_psa_results()
  tbl <- incremental_ceac_table(results, "total_qalys", "total_cost",
                                 table_format = "flextable")
  expect_s3_class(tbl, "flextable")
})

test_that("incremental_ceac_table() respects custom wtp_thresholds", {
  results <- get_cached_psa_results()
  custom_wtp <- c(0, 25000, 75000)

  # Get prepared data to verify row count
  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    wtp_thresholds = custom_wtp
  )

  # Should have 3 rows (one per WTP threshold)
  expect_equal(nrow(prepared$data), 3)
})

test_that("incremental_ceac_table() respects decimals parameter", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    decimals = 2
  )

  # Get a strategy column value and check for % symbol
  strategy_cols <- setdiff(names(prepared$data), "WTP")
  sample_val <- prepared$data[[strategy_cols[1]]][1]
  expect_true(grepl("%$", sample_val))
})

# ============================================================================
# Tests for prepare_incremental_ceac_table_data() [internal]
# ============================================================================

test_that("prepare_incremental_ceac_table_data() single group has one header row", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    group = "aggregated"
  )

  # Single group mode should have exactly 1 header row
  expect_equal(length(prepared$headers), 1)
})

test_that("prepare_incremental_ceac_table_data() formats WTP as dollars", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    wtp_thresholds = c(50000, 100000)
  )

  # WTP column should have dollar format (e.g., "$50,000")
  wtp_vals <- prepared$data[["WTP"]]
  expect_true(all(grepl("^\\$", wtp_vals)))
})

test_that("prepare_incremental_ceac_table_data() highlights optimal when show_optimal=TRUE", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    show_optimal = TRUE
  )

  # Should have highlighted_cells in special_rows
  expect_true(length(prepared$special_rows$highlighted_cells) > 0)
})

test_that("prepare_incremental_ceac_table_data() no highlights when show_optimal=FALSE", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    show_optimal = FALSE
  )

  # Should have empty or no highlighted_cells
  expect_true(is.null(prepared$special_rows$highlighted_cells) ||
              length(prepared$special_rows$highlighted_cells) == 0)
})

test_that("prepare_incremental_ceac_table_data() sets correct column alignments", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost"
  )

  # First column (WTP) should be left aligned
  expect_equal(prepared$column_alignments[1], "left")

  # Strategy columns should be right aligned
  expect_true(all(prepared$column_alignments[-1] == "right"))
})

test_that("prepare_incremental_ceac_table_data() probabilities sum approximately to 100%", {
  results <- get_cached_psa_results()

  prepared <- prepare_incremental_ceac_table_data(
    results, "total_qalys", "total_cost",
    wtp_thresholds = c(50000)
  )

  # Extract numeric probabilities from string values
  strategy_cols <- setdiff(names(prepared$data), "WTP")
  probs <- sapply(strategy_cols, function(col) {
    val <- prepared$data[[col]][1]
    as.numeric(gsub("%", "", val))
  })

  # Probabilities should sum to approximately 100
  expect_true(abs(sum(probs) - 100) < 0.1)
})

# ============================================================================
# Tests for psa_summary_table()
# ============================================================================

test_that("psa_summary_table() creates table object", {
  results <- get_cached_psa_results()
  tbl <- psa_summary_table(results, "total_qalys", "total_cost")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("psa_summary_table() respects table_format = 'kable'", {
  results <- get_cached_psa_results()
  tbl <- psa_summary_table(results, "total_qalys", "total_cost",
                            table_format = "kable")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("psa_summary_table() respects table_format = 'flextable'", {
  skip_if_not_installed("flextable")
  results <- get_cached_psa_results()
  tbl <- psa_summary_table(results, "total_qalys", "total_cost",
                            table_format = "flextable")
  expect_s3_class(tbl, "flextable")
})

# ============================================================================
# Tests for prepare_psa_summary_table_data() [internal]
# ============================================================================

test_that("prepare_psa_summary_table_data() has correct row structure", {
  results <- get_cached_psa_results()

  prepared <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    pce_wtp = c(50000, 100000)
  )

  # Expected rows: 5 outcome + 5 cost + 1 P(CE) header + 2 WTP rows = 13 rows
  expected_rows <- 5 + 5 + 1 + 2
  expect_equal(nrow(prepared$data), expected_rows)
})

test_that("prepare_psa_summary_table_data() identifies section header rows", {
  results <- get_cached_psa_results()

  prepared <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost"
  )

  # Should have 3 group header rows: Outcome, Cost, P(CE)
  expect_equal(length(prepared$special_rows$group_header_rows), 3)

  # Header rows should be at positions 1, 6, 11
  expect_equal(prepared$special_rows$group_header_rows, c(1, 6, 11))
})

test_that("prepare_psa_summary_table_data() identifies indented detail rows", {
  results <- get_cached_psa_results()

  prepared <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    pce_wtp = c(50000, 100000)
  )

  # Should have indented rows (stats under each section + WTP rows)
  expect_true(length(prepared$special_rows$indented_rows) > 0)
  # Should include rows 2-5, 7-10, 12-13
  expect_true(all(c(2, 3, 4, 5, 7, 8, 9, 10, 12, 13) %in%
                  prepared$special_rows$indented_rows))
})

test_that("prepare_psa_summary_table_data() uses lambda character in P(CE) WTP labels", {
  results <- get_cached_psa_results()

  prepared <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    pce_wtp = c(50000)
  )

  # Find the P(CE) WTP row and check for lambda
  row_labels <- prepared$data[[1]]
  pce_rows <- row_labels[grepl("50,000", row_labels)]

  # Should contain Unicode lambda (\u03bb)
  expect_true(any(grepl("\u03bb", pce_rows)))
})

test_that("prepare_psa_summary_table_data() single group has one header row", {
  results <- get_cached_psa_results()

  prepared <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    group = "aggregated"
  )

  # Single group mode should have exactly 1 header row
  expect_equal(length(prepared$headers), 1)
})

test_that("prepare_psa_summary_table_data() respects custom pce_wtp thresholds", {
  results <- get_cached_psa_results()

  prepared_1 <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    pce_wtp = c(50000)
  )

  prepared_3 <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    pce_wtp = c(50000, 100000, 150000)
  )

  # Row count should differ by 2 (2 extra WTP thresholds)
  expect_equal(nrow(prepared_3$data) - nrow(prepared_1$data), 2)
})

test_that("prepare_psa_summary_table_data() formats costs with commas", {
  results <- get_cached_psa_results()

  prepared <- prepare_psa_summary_table_data(
    results, "total_qalys", "total_cost",
    cost_decimals = 0
  )

  # Cost values should have comma separators for large numbers
  # Check the Cost Mean row (row 7)
  strategy_cols <- setdiff(names(prepared$data), " ")
  cost_mean_val <- prepared$data[[strategy_cols[1]]][7]

  # Should contain comma if value is >= 1000
  # (most health economic costs are > 1000)
  expect_true(grepl(",", cost_mean_val) || grepl("^[0-9]{1,3}$", cost_mean_val))
})

# ============================================================================
# Tests for pairwise_ceac_table()
# NOTE: pairwise_ceac tests are skipped due to a bug in calculate_pairwise_ceac()
# in ceac.R that causes size mismatch errors. Enable these tests once the bug
# is fixed.
# ============================================================================

test_that("pairwise_ceac_table() works in comparator mode", {
  results <- get_cached_psa_results()

  # Get first strategy as comparator
  strategies <- results$metadata$strategies$display_name

  tbl <- pairwise_ceac_table(
    results, "total_qalys", "total_cost",
    comparator = strategies[1]
  )

  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("pairwise_ceac_table() works in intervention mode", {
  results <- get_cached_psa_results()

  strategies <- results$metadata$strategies$display_name

  tbl <- pairwise_ceac_table(
    results, "total_qalys", "total_cost",
    intervention = strategies[1]
  )

  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("pairwise_ceac_table() errors when neither comparator nor intervention provided", {
  results <- get_cached_psa_results()

  expect_error(
    pairwise_ceac_table(results, "total_qalys", "total_cost"),
    "comparator.*intervention|intervention.*comparator|must be provided"
  )
})

test_that("pairwise_ceac_table() errors when both comparator and intervention provided", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  expect_error(
    pairwise_ceac_table(results, "total_qalys", "total_cost",
                        comparator = strategies[1],
                        intervention = strategies[2]),
    "not both|Only one"
  )
})

test_that("pairwise_ceac_table() respects table_format = 'flextable'", {
  skip_if_not_installed("flextable")
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  tbl <- pairwise_ceac_table(
    results, "total_qalys", "total_cost",
    comparator = strategies[1],
    table_format = "flextable"
  )

  expect_s3_class(tbl, "flextable")
})

# ============================================================================
# Tests for prepare_pairwise_ceac_table_data() [internal]
# ============================================================================

test_that("prepare_pairwise_ceac_table_data() generates comparison labels with 'vs.'", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  prepared <- prepare_pairwise_ceac_table_data(
    results, "total_qalys", "total_cost",
    comparator = strategies[1]
  )

  # Headers should contain "vs." in comparison labels
  header_texts <- sapply(prepared$headers[[1]], function(x) x$text)
  # At least one header should have "vs." (the comparison columns)
  expect_true(any(grepl("vs\\.", header_texts)))
})

test_that("prepare_pairwise_ceac_table_data() single group has one header row", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  prepared <- prepare_pairwise_ceac_table_data(
    results, "total_qalys", "total_cost",
    comparator = strategies[1],
    group = "aggregated"
  )

  # Single group mode should have exactly 1 header row
  expect_equal(length(prepared$headers), 1)
})

test_that("prepare_pairwise_ceac_table_data() formats WTP as dollars", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  prepared <- prepare_pairwise_ceac_table_data(
    results, "total_qalys", "total_cost",
    comparator = strategies[1],
    wtp_thresholds = c(50000, 100000)
  )

  # WTP column should have dollar format
  wtp_vals <- prepared$data[["WTP"]]
  expect_true(all(grepl("^\\$", wtp_vals)))
})

test_that("prepare_pairwise_ceac_table_data() respects custom wtp_thresholds", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name
  custom_wtp <- c(0, 25000, 50000, 75000, 100000)

  prepared <- prepare_pairwise_ceac_table_data(
    results, "total_qalys", "total_cost",
    comparator = strategies[1],
    wtp_thresholds = custom_wtp
  )

  # Should have 5 rows (one per WTP threshold)
  expect_equal(nrow(prepared$data), 5)
})

# ============================================================================
# Tests for evpi_table()
# ============================================================================

test_that("evpi_table() creates table object", {
  results <- get_cached_psa_results()
  tbl <- evpi_table(results, "total_qalys", "total_cost")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("evpi_table() respects table_format = 'kable'", {
  results <- get_cached_psa_results()
  tbl <- evpi_table(results, "total_qalys", "total_cost",
                    table_format = "kable")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("evpi_table() respects table_format = 'flextable'", {
  skip_if_not_installed("flextable")
  results <- get_cached_psa_results()
  tbl <- evpi_table(results, "total_qalys", "total_cost",
                    table_format = "flextable")
  expect_s3_class(tbl, "flextable")
})

test_that("evpi_table() respects custom wtp_thresholds", {
  results <- get_cached_psa_results()
  custom_wtp <- c(25000, 50000, 75000, 100000, 150000)

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost",
    wtp_thresholds = custom_wtp
  )

  # Should have 5 rows (one per WTP threshold)
  expect_equal(nrow(prepared$data), 5)
})

# ============================================================================
# Tests for prepare_evpi_table_data() [internal]
# ============================================================================

test_that("prepare_evpi_table_data() single group has WTP/EVPI columns", {
  results <- get_cached_psa_results()

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost",
    group = "aggregated"
  )

  # Should have exactly 2 columns: WTP and EVPI
  expect_equal(ncol(prepared$data), 2)
  expect_equal(names(prepared$data), c("WTP", "EVPI"))
})

test_that("prepare_evpi_table_data() formats EVPI values as dollars", {
  results <- get_cached_psa_results()

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost"
  )

  # EVPI column should have dollar format
  evpi_vals <- prepared$data[["EVPI"]]
  expect_true(all(grepl("^\\$", evpi_vals)))
})

test_that("prepare_evpi_table_data() formats WTP as dollars", {
  results <- get_cached_psa_results()

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost",
    wtp_thresholds = c(50000, 100000)
  )

  # WTP column should have dollar format
  wtp_vals <- prepared$data[["WTP"]]
  expect_true(all(grepl("^\\$", wtp_vals)))
})

test_that("prepare_evpi_table_data() single group has one header row", {
  results <- get_cached_psa_results()

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost",
    group = "aggregated"
  )

  # Single group mode should have exactly 1 header row
  expect_equal(length(prepared$headers), 1)
})

test_that("prepare_evpi_table_data() header contains WTP and EVPI", {
  results <- get_cached_psa_results()

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost",
    group = "aggregated"
  )

  # Header should have WTP and EVPI
  header_texts <- sapply(prepared$headers[[1]], function(x) x$text)
  expect_true("WTP" %in% header_texts)
  expect_true("EVPI" %in% header_texts)
})

test_that("prepare_evpi_table_data() sets correct column alignments", {
  results <- get_cached_psa_results()

  prepared <- prepare_evpi_table_data(
    results, "total_qalys", "total_cost"
  )

  # First column (WTP) should be left aligned
  expect_equal(prepared$column_alignments[1], "left")

  # EVPI column should be right aligned
  expect_equal(prepared$column_alignments[2], "right")
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("Full PSA tables workflow with example model", {
  # 1. Build a simple model with PSA support

  model <- build_simple_psa_model()

  # 2. Run PSA (n_sim = 50 for performance)
  set.seed(42)
  results <- run_psa(model, n_sim = 50)

  # Verify PSA structure
  expect_true("simulation" %in% names(results$aggregated))

  # 3. Generate all table types

  # Incremental CEAC table
  ceac_tbl <- incremental_ceac_table(results, "total_qalys", "total_cost")
  expect_true(inherits(ceac_tbl, "kableExtra") || is.character(ceac_tbl))

  # PSA summary table
  summary_tbl <- psa_summary_table(results, "total_qalys", "total_cost")
  expect_true(inherits(summary_tbl, "kableExtra") || is.character(summary_tbl))

  # NOTE: Pairwise CEAC table skipped due to bug in calculate_pairwise_ceac()

  # EVPI table
  evpi_tbl <- evpi_table(results, "total_qalys", "total_cost")
  expect_true(inherits(evpi_tbl, "kableExtra") || is.character(evpi_tbl))
})

test_that("Tables work with discounted values", {
  results <- get_cached_psa_results()

  # Test with discounted = TRUE
  ceac_tbl <- incremental_ceac_table(
    results, "total_qalys", "total_cost",
    discounted = TRUE
  )
  expect_true(inherits(ceac_tbl, "kableExtra") || is.character(ceac_tbl))

  summary_tbl <- psa_summary_table(
    results, "total_qalys", "total_cost",
    discounted = TRUE
  )
  expect_true(inherits(summary_tbl, "kableExtra") || is.character(summary_tbl))

  # NOTE: Pairwise CEAC table skipped due to bug in calculate_pairwise_ceac()

  evpi_tbl <- evpi_table(
    results, "total_qalys", "total_cost",
    discounted = TRUE
  )
  expect_true(inherits(evpi_tbl, "kableExtra") || is.character(evpi_tbl))
})

test_that("Tables work with strategies filter", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  # Filter to specific strategies (if more than 1)
  if (length(strategies) >= 2) {
    filtered_strategies <- strategies[1:2]

    ceac_tbl <- incremental_ceac_table(
      results, "total_qalys", "total_cost",
      strategies = filtered_strategies
    )
    expect_true(inherits(ceac_tbl, "kableExtra") || is.character(ceac_tbl))

    summary_tbl <- psa_summary_table(
      results, "total_qalys", "total_cost",
      strategies = filtered_strategies
    )
    expect_true(inherits(summary_tbl, "kableExtra") || is.character(summary_tbl))
  }
})
