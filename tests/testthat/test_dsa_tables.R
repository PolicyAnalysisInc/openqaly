context("DSA Tables")

# ============================================================================
# Test Fixtures
# ============================================================================

build_dsa_test_model <- function() {
  define_model("markov") |>
    set_settings(
      timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) |>
    add_strategy("standard", display_name = "Standard") |>
    add_strategy("new_treatment", display_name = "New Treatment") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_sick", 0.1) |>
    add_variable("p_death", 0.05) |>
    add_variable("c_healthy", 1000, strategy = "standard") |>
    add_variable("c_healthy", 3000, strategy = "new_treatment") |>
    add_variable("c_sick", 5000) |>
    add_variable("u_healthy", 0.9) |>
    add_variable("u_sick", 0.5) |>
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Probability of Sickness") |>
    add_dsa_variable("c_sick", low = 3000, high = 7000,
                     display_name = "Cost of Sick State") |>
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death") |>
    add_transition("sick", "dead", "0.15") |>
    add_transition("sick", "sick", "0.85") |>
    add_transition("dead", "dead", "1") |>
    add_value("cost", "c_healthy", state = "healthy") |>
    add_value("cost", "c_sick", state = "sick") |>
    add_value("qaly", "u_healthy", state = "healthy") |>
    add_value("qaly", "u_sick", state = "sick") |>
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qaly")
}

get_dsa_test_results <- function() {
  model <- build_dsa_test_model()
  run_dsa(model)
}

# ============================================================================
# Mathematical Correctness Tests
# ============================================================================

test_that("Base case values in table match extract_dsa_summaries()", {

  results <- get_dsa_test_results()
  # Note: using "aggregated" because the DSA tables implementation
  # doesn't map "overall" to "aggregated" like other table functions
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated"
  )

  # Get ground truth from extract_dsa_summaries
  base_data <- openqaly:::extract_dsa_summaries(
    results, "total_qalys", group = "aggregated"
  ) %>%
    filter(.data$variation == "base")

  # Map strategy names to display names
  strategy_map <- setNames(
    results$metadata$strategies$display_name,
    results$metadata$strategies$name
  )

  # For each strategy, verify base column value matches ground truth
  for (strat in unique(base_data$strategy)) {
    expected <- base_data %>% filter(.data$strategy == strat) %>% pull(amount) %>% unique()
    display_name <- strategy_map[strat]
    base_col <- paste0(display_name, "_base")
    actual <- as.numeric(prepared$data[[base_col]][1])
    expect_equal(actual, expected, tolerance = 0.01,
                 info = paste("Base case mismatch for strategy:", strat))
  }
})

test_that("Low/High values match extract_dsa_summaries() for each parameter", {

  results <- get_dsa_test_results()
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated"
  )

  summaries <- openqaly:::extract_dsa_summaries(
    results, "total_qalys", group = "aggregated"
  )
  strategy_map <- setNames(
    results$metadata$strategies$display_name,
    results$metadata$strategies$name
  )

  # For each DSA parameter (excluding base)
  params <- unique(summaries$parameter_display_name[summaries$variation != "base"])

  for (param in params) {
    # Find row in table
    row_idx <- which(prepared$data[[1]] == param)
    expect_equal(length(row_idx), 1,
                 info = paste("Parameter not found in table:", param))

    for (strat in unique(summaries$strategy)) {
      display_name <- strategy_map[strat]

      # Get expected low/high from summaries
      expected_low <- summaries %>%
        filter(.data$strategy == strat,
               .data$parameter_display_name == param,
               .data$variation == "low") %>%
        pull(amount)
      expected_high <- summaries %>%
        filter(.data$strategy == strat,
               .data$parameter_display_name == param,
               .data$variation == "high") %>%
        pull(amount)

      # Get actual from table
      actual_low <- as.numeric(prepared$data[[paste0(display_name, "_low")]][row_idx])
      actual_high <- as.numeric(prepared$data[[paste0(display_name, "_high")]][row_idx])

      expect_equal(actual_low, expected_low, tolerance = 0.01,
                   info = paste("Low mismatch for", param, "strategy:", strat))
      expect_equal(actual_high, expected_high, tolerance = 0.01,
                   info = paste("High mismatch for", param, "strategy:", strat))
    }
  }
})

test_that("Differences equal intervention minus comparator for base case", {

  results <- get_dsa_test_results()

  # Get absolute values
  summaries <- openqaly:::extract_dsa_summaries(
    results, "total_qalys", group = "aggregated"
  )

  # Calculate expected base case difference manually
  base_standard <- summaries %>%
    filter(.data$variation == "base", .data$strategy == "standard") %>%
    pull(amount) %>% unique()
  base_new <- summaries %>%
    filter(.data$variation == "base", .data$strategy == "new_treatment") %>%
    pull(amount) %>% unique()
  expected_base_diff <- base_new - base_standard

  # Get difference via comparators parameter
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated",
    comparators = "standard"
  )

  # Find the comparison column and extract base value
  col_names <- names(prepared$data)
  base_col <- col_names[grepl("vs\\.", col_names) & grepl("_base$", col_names)]
  expect_equal(length(base_col), 1,
               info = "Should find exactly one base comparison column")

  actual_diff <- as.numeric(prepared$data[[base_col]][1])

  expect_equal(actual_diff, expected_base_diff, tolerance = 0.01)
})

test_that("Differences are correct for low and high variations", {

  results <- get_dsa_test_results()
  summaries <- openqaly:::extract_dsa_summaries(
    results, "total_qalys", group = "aggregated"
  )

  # Get difference via comparators parameter
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated",
    comparators = "standard"
  )

  col_names <- names(prepared$data)
  low_col <- col_names[grepl("vs\\.", col_names) & grepl("_low$", col_names)]
  high_col <- col_names[grepl("vs\\.", col_names) & grepl("_high$", col_names)]

  # For each DSA parameter, verify low/high differences
  params <- unique(summaries$parameter_display_name[summaries$variation != "base"])

  for (param in params) {
    row_idx <- which(prepared$data[[1]] == param)

    # Calculate expected differences manually
    low_standard <- summaries %>%
      filter(.data$strategy == "standard",
             .data$parameter_display_name == param,
             .data$variation == "low") %>%
      pull(amount)
    low_new <- summaries %>%
      filter(.data$strategy == "new_treatment",
             .data$parameter_display_name == param,
             .data$variation == "low") %>%
      pull(amount)
    expected_low_diff <- low_new - low_standard

    high_standard <- summaries %>%
      filter(.data$strategy == "standard",
             .data$parameter_display_name == param,
             .data$variation == "high") %>%
      pull(amount)
    high_new <- summaries %>%
      filter(.data$strategy == "new_treatment",
             .data$parameter_display_name == param,
             .data$variation == "high") %>%
      pull(amount)
    expected_high_diff <- high_new - high_standard

    actual_low_diff <- as.numeric(prepared$data[[low_col]][row_idx])
    actual_high_diff <- as.numeric(prepared$data[[high_col]][row_idx])

    expect_equal(actual_low_diff, expected_low_diff, tolerance = 0.01,
                 info = paste("Low diff mismatch for:", param))
    expect_equal(actual_high_diff, expected_high_diff, tolerance = 0.01,
                 info = paste("High diff mismatch for:", param))
  }
})

# ============================================================================
# Structure Tests
# ============================================================================

test_that("Table has one row per DSA parameter", {

  results <- get_dsa_test_results()
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated"
  )

  n_params <- length(unique(
    results$dsa_metadata$parameter[results$dsa_metadata$parameter != "base"]
  ))
  expect_equal(nrow(prepared$data), n_params)
})

test_that("Table has Low/Base/High columns per strategy", {

  results <- get_dsa_test_results()
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated"
  )

  n_strategies <- nrow(results$metadata$strategies)
  expected_cols <- 1 + (n_strategies * 3)  # label + 3 per strategy
  expect_equal(ncol(prepared$data), expected_cols)
})

test_that("Headers have strategy names and Low/Base/High labels", {

  results <- get_dsa_test_results()
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated"
  )

  # Two header rows
  expect_equal(length(prepared$headers), 2)

  # Row 2 should have Low/Base/High repeated
  row2_texts <- sapply(prepared$headers[[2]], function(x) x$text)
  expect_true(all(c("Low", "Base", "High") %in% row2_texts))
})

test_that("Column alignments are correct", {

  results <- get_dsa_test_results()
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated"
  )

  # First column (parameter names) should be left aligned
  expect_equal(prepared$column_alignments[1], "left")

  # All numeric columns should be right aligned
  expect_true(all(prepared$column_alignments[-1] == "right"))
})

# ============================================================================
# API Contract Tests
# ============================================================================

test_that("dsa_outcomes_table() returns kable by default", {

  results <- get_dsa_test_results()
  tbl <- dsa_outcomes_table(results, "total_qalys", groups = "aggregated")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

test_that("dsa_outcomes_table() returns flextable when requested", {
  skip_if_not_installed("flextable")

  results <- get_dsa_test_results()
  tbl <- dsa_outcomes_table(
    results, "total_qalys", groups = "aggregated", table_format = "flextable"
  )
  expect_s3_class(tbl, "flextable")
})

test_that("dsa_outcomes_table() errors with both interventions and comparators", {

  results <- get_dsa_test_results()
  expect_error(
    dsa_outcomes_table(results, "total_qalys", groups = "aggregated",
                       interventions = "new_treatment",
                       comparators = "standard"),
    "not both"
  )
})

test_that("dsa_outcomes_table() works with discounted = TRUE", {

  results <- get_dsa_test_results()
  expect_no_error(
    dsa_outcomes_table(results, "total_qalys",
                       groups = "aggregated", discounted = TRUE)
  )
})

test_that("dsa_outcomes_table() respects decimals parameter", {

  results <- get_dsa_test_results()
  prepared_2 <- openqaly:::prepare_dsa_outcomes_table_data(
    results, "total_qalys", groups = "aggregated", decimals = 2
  )
  prepared_4 <- openqaly:::prepare_dsa_outcomes_table_data(
    results, "total_qalys", groups = "aggregated", decimals = 4
  )

  # Get a numeric column value
  col <- names(prepared_2$data)[2]
  val_2 <- prepared_2$data[[col]][1]
  val_4 <- prepared_4$data[[col]][1]

  # decimals = 4 should have more decimal places
  expect_true(nchar(val_4) >= nchar(val_2))
})

test_that("dsa_outcomes_table() works with cost outcomes", {

  results <- get_dsa_test_results()
  tbl <- dsa_outcomes_table(results, "total_cost", groups = "aggregated")
  expect_true(inherits(tbl, "kableExtra") || is.character(tbl))
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("Full DSA table workflow produces valid output", {

  model <- build_dsa_test_model()
  results <- run_dsa(model)

  # Generate tables for both outcomes
  qaly_tbl <- dsa_outcomes_table(results, "total_qalys", groups = "aggregated")
  cost_tbl <- dsa_outcomes_table(results, "total_cost", groups = "aggregated")

  expect_true(inherits(qaly_tbl, "kableExtra") || is.character(qaly_tbl))
  expect_true(inherits(cost_tbl, "kableExtra") || is.character(cost_tbl))
})

test_that("DSA comparison table workflow produces valid output", {

  model <- build_dsa_test_model()
  results <- run_dsa(model)

  # Generate comparison tables with comparators
  diff_tbl <- dsa_outcomes_table(
    results, "total_qalys", groups = "aggregated", comparators = "standard"
  )
  expect_true(inherits(diff_tbl, "kableExtra") || is.character(diff_tbl))

  # Generate comparison tables with interventions
  int_tbl <- dsa_outcomes_table(
    results, "total_qalys", groups = "aggregated",
    interventions = "new_treatment"
  )
  expect_true(inherits(int_tbl, "kableExtra") || is.character(int_tbl))
})

test_that("DSA table with strategy filter works", {

  results <- get_dsa_test_results()

  # Filter to single strategy
  prepared <- openqaly:::prepare_dsa_outcomes_table_data(
    results, outcome = "total_qalys", groups = "aggregated",
    strategies = "standard"
  )

  # Should have 1 label + 3 columns for single strategy
  expect_equal(ncol(prepared$data), 4)
})
