context("Outcome Tables")

# ============================================================================
# Test Fixtures
# ============================================================================

# Build a minimal Markov model with known, deterministic values for testing
build_test_model <- function() {
  define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    add_strategy("standard") |>
    add_strategy("new_treatment") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>

    # Fixed probabilities (no sampling) for deterministic results
    add_variable("p_sick", 0.1) |>
    add_variable("p_death_healthy", 0.02) |>
    add_variable("p_death_sick", 0.15) |>

    # Costs: standard=1000/cycle, new_treatment=3000/cycle in healthy state
    add_variable("c_healthy", 1000, strategy = "standard") |>
    add_variable("c_healthy", 3000, strategy = "new_treatment") |>
    add_variable("c_sick", 5000) |>

    # Utilities: healthy=0.9, sick=0.5
    add_variable("u_healthy", 0.9) |>
    add_variable("u_sick", 0.5) |>

    # Transitions
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death_healthy") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death_healthy") |>
    add_transition("sick", "dead", "p_death_sick") |>
    add_transition("sick", "sick", "1 - p_death_sick") |>
    add_transition("dead", "dead", "1") |>

    # Values (multiple components for testing breakdowns)
    add_value("drug_cost", "c_healthy", state = "healthy", type = "cost") |>
    add_value("care_cost", "c_sick", state = "sick", type = "cost") |>
    add_value("healthy_qalys", "u_healthy", state = "healthy", type = "outcome") |>
    add_value("sick_qalys", "u_sick", state = "sick", type = "outcome") |>

    # Summaries with WTP for NMB calculations
    add_summary("total_cost", "drug_cost,care_cost", type = "cost") |>
    add_summary("total_qalys", "healthy_qalys,sick_qalys", type = "outcome", wtp = 50000)
}

get_test_results <- function() {
  model <- build_test_model()
  run_model(model)
}

# Build a model with multiple groups for testing three-level mode
build_multi_group_test_model <- function() {
  define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    add_strategy("standard") |>
    add_strategy("new_treatment") |>
    add_group("young", weight = 0.6) |>
    add_group("old", weight = 0.4) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>

    # Fixed probabilities (no sampling) for deterministic results
    add_variable("p_sick", 0.1) |>
    add_variable("p_death_healthy", 0.02) |>
    add_variable("p_death_sick", 0.15) |>

    # Costs: standard=1000/cycle, new_treatment=3000/cycle in healthy state
    add_variable("c_healthy", 1000, strategy = "standard") |>
    add_variable("c_healthy", 3000, strategy = "new_treatment") |>
    add_variable("c_sick", 5000) |>

    # Utilities: healthy=0.9, sick=0.5
    add_variable("u_healthy", 0.9) |>
    add_variable("u_sick", 0.5) |>

    # Transitions
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death_healthy") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death_healthy") |>
    add_transition("sick", "dead", "p_death_sick") |>
    add_transition("sick", "sick", "1 - p_death_sick") |>
    add_transition("dead", "dead", "1") |>

    # Values (multiple components for testing breakdowns)
    add_value("drug_cost", "c_healthy", state = "healthy", type = "cost") |>
    add_value("care_cost", "c_sick", state = "sick", type = "cost") |>
    add_value("healthy_qalys", "u_healthy", state = "healthy", type = "outcome") |>
    add_value("sick_qalys", "u_sick", state = "sick", type = "outcome") |>

    # Summaries with WTP for NMB calculations
    add_summary("total_cost", "drug_cost,care_cost", type = "cost") |>
    add_summary("total_qalys", "healthy_qalys,sick_qalys", type = "outcome", wtp = 50000)
}

get_multi_group_test_results <- function() {
  model <- build_multi_group_test_model()
  run_model(model)
}

# ============================================================================
# Tests for outcomes_table()
# ============================================================================

test_that("outcomes_table() Total row equals sum of component values", {
  results <- get_test_results()

  prepared <- prepare_outcomes_table_data(
    results, outcome = "total_qalys",
    groups = "overall", show_total = TRUE, decimals = 2
  )

  # Get strategy columns (exclude the row label column)
  strategy_cols <- setdiff(names(prepared$data), " ")
  # Also exclude spacer columns
  strategy_cols <- strategy_cols[!grepl("^spacer", strategy_cols)]

  for (col in strategy_cols) {
    values <- as.numeric(prepared$data[[col]])
    n <- length(values)
    component_sum <- sum(values[1:(n - 1)])
    total_row <- values[n]

    expect_equal(total_row, component_sum, tolerance = 0.01,
      info = paste("Total should equal sum of components for", col))
  }
})

test_that("outcomes_table() with comparators calculates differences correctly", {
  results <- get_test_results()

  # Get absolute values first
  abs_data <- get_summaries(results, summaries = "total_qalys", groups = "overall")
  standard_qalys <- sum(abs_data$amount[abs_data$strategy == "Standard"])
  new_qalys <- sum(abs_data$amount[abs_data$strategy == "New Treatment"])
  expected_diff <- new_qalys - standard_qalys

  # Now get difference via comparators
  prepared <- prepare_outcomes_table_data(
    results, outcome = "total_qalys",
    groups = "overall", comparators = "standard",
    show_total = TRUE, decimals = 2
  )

  # Find comparison column and Total row
  comparison_col <- names(prepared$data)[grepl("vs\\.", names(prepared$data))]
  total_idx <- which(prepared$data[[" "]] == "Total")
  actual_diff <- as.numeric(prepared$data[[comparison_col]][total_idx])

  expect_equal(actual_diff, expected_diff, tolerance = 0.01)
})

test_that("outcomes_table() returns flextable by default", {
  results <- get_test_results()
  tbl <- outcomes_table(results, "total_qalys")
  expect_true(inherits(tbl, "flextable"))
})

test_that("outcomes_table() returns flextable when requested", {
  skip_if_not_installed("flextable")
  results <- get_test_results()
  tbl <- outcomes_table(results, "total_qalys", table_format = "flextable")
  expect_s3_class(tbl, "flextable")
})

# ============================================================================
# Tests for nmb_table()
# ============================================================================

test_that("nmb_table() calculates NMB = (dOutcome * WTP) - dCost", {
  results <- get_test_results()

  # Calculate expected NMB manually
  cost_data <- get_summaries(results, summaries = "total_cost", groups = "overall")
  qaly_data <- get_summaries(results, summaries = "total_qalys", groups = "overall")

  std_cost <- sum(cost_data$amount[cost_data$strategy == "Standard"])
  new_cost <- sum(cost_data$amount[cost_data$strategy == "New Treatment"])
  std_qaly <- sum(qaly_data$amount[qaly_data$strategy == "Standard"])
  new_qaly <- sum(qaly_data$amount[qaly_data$strategy == "New Treatment"])

  dCost <- new_cost - std_cost
  dQALY <- new_qaly - std_qaly
  wtp <- 50000
  expected_nmb <- (dQALY * wtp) - dCost

  # Get NMB from table
  prepared <- prepare_nmb_table_data(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "overall",
    comparators = "standard",
    show_total = TRUE, decimals = 0
  )

  comparison_col <- names(prepared$data)[grepl("vs\\.", names(prepared$data))]
  total_idx <- which(prepared$data[[" "]] == "Total")
  actual_nmb <- as.numeric(prepared$data[[comparison_col]][total_idx])

  expect_equal(actual_nmb, expected_nmb, tolerance = 1)
})

test_that("nmb_table() component rows sum to Total row", {
  results <- get_test_results()

  prepared <- prepare_nmb_table_data(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "overall",
    comparators = "standard",
    show_total = TRUE, decimals = 0
  )

  comparison_col <- names(prepared$data)[grepl("vs\\.", names(prepared$data))]
  row_labels <- prepared$data[[" "]]
  values <- as.numeric(prepared$data[[comparison_col]])

  total_idx <- which(row_labels == "Total")
  non_total_idx <- which(row_labels != "Total")

  component_sum <- sum(values[non_total_idx])
  total_value <- values[total_idx]

  expect_equal(component_sum, total_value, tolerance = 1)
})

test_that("nmb_table() extracts WTP from metadata when not provided", {
  results <- get_test_results()

  # Call without explicit wtp - should use 50000 from metadata
  expect_no_error(
    prepare_nmb_table_data(
      results,
      outcome_summary = "total_qalys",
      cost_summary = "total_cost",
      groups = "overall",
      comparators = "standard",
      wtp = NULL
    )
  )
})

test_that("nmb_table() errors without interventions or comparators", {
  results <- get_test_results()

  expect_error(
    nmb_table(results, "total_qalys", "total_cost"),
    "must be provided"
  )
})

# ============================================================================
# Tests for pairwise_ce_table()
# ============================================================================

test_that("pairwise_ce_table() handles special ICER cases", {
  results <- get_test_results()

  ce <- calculate_pairwise_ce(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    comparators = "standard"
  )

  # Verify ICER is calculated (positive, negative, or special)
  expect_s3_class(ce$icer, "icer")

  # Format should handle special cases
  formatted <- format(ce$icer)
  expect_type(formatted, "character")
})

test_that("pairwise_ce_table() returns table object", {
  results <- get_test_results()

  tbl <- pairwise_ce_table(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    comparators = "standard"
  )

  expect_true(inherits(tbl, "flextable"))
})

# ============================================================================
# Tests for parameter validation
# ============================================================================

test_that("outcomes_table() errors when strategies used with comparators", {
  results <- get_test_results()

  expect_error(
    outcomes_table(results, "total_qalys",
                   strategies = "standard", comparators = "standard"),
    "Cannot specify 'strategies'"
  )
})

# ============================================================================
# Tests for three-level mode (multi-group outcomes tables)
# ============================================================================

test_that("Three-level mode has spacer columns between groups", {
  results <- get_multi_group_test_results()

  prepared <- prepare_outcomes_table_data(
    results, outcome = "total_qalys",
    groups = "all", show_total = TRUE, decimals = 2
  )

  # Should have spacer columns
  spacer_cols <- names(prepared$data)[grepl("^spacer_", names(prepared$data))]
  expect_true(length(spacer_cols) > 0,
              info = "Three-level mode should have spacer columns")

  # Number of spacers should match number of groups displayed
  # (includes Overall + user-defined groups)
  n_groups_displayed <- length(unique(results$metadata$groups$name)) + 1  # +1 for Overall
  expect_equal(length(spacer_cols), n_groups_displayed,
               info = "Should have one spacer per group including Overall")
})

test_that("Three-level mode has two-level header structure", {
  results <- get_multi_group_test_results()

  prepared <- prepare_outcomes_table_data(
    results, outcome = "total_qalys",
    groups = "all", show_total = TRUE, decimals = 2
  )

  # Should have header structure with level1 and level2

  expect_true("header_structure" %in% names(prepared) ||
              length(prepared$headers) >= 2,
              info = "Three-level mode should have multi-level headers")

  # If using header_structure, check level1 contains group names
  if ("header_structure" %in% names(prepared)) {
    level1_values <- prepared$header_structure$level1$values
    group_names <- results$metadata$groups$display_name
    for (grp in group_names) {
      expect_true(grp %in% level1_values,
                  info = paste("Level 1 headers should contain group:", grp))
    }
  }
})

test_that("Three-level mode values match get_summaries() for each group-strategy", {
  results <- get_multi_group_test_results()

  prepared <- prepare_outcomes_table_data(
    results, outcome = "total_qalys",
    groups = "all", show_total = TRUE, decimals = 2
  )

  # Get ground truth from get_summaries
  summaries <- get_summaries(results, summaries = "total_qalys", groups = "all")

  # Get group and strategy display names
  group_map <- setNames(
    results$metadata$groups$display_name,
    results$metadata$groups$name
  )
  strategy_map <- setNames(
    results$metadata$strategies$display_name,
    results$metadata$strategies$name
  )

  # For each group-strategy combination, verify values match
  for (grp_name in names(group_map)) {
    grp_display <- group_map[grp_name]
    for (strat_name in names(strategy_map)) {
      strat_display <- strategy_map[strat_name]

      # Get expected total from summaries
      expected_total <- summaries %>%
        filter(.data$group == grp_display, .data$strategy == strat_display) %>%
        pull(amount) %>%
        sum()

      # Find the corresponding column in prepared data (Group_Strategy format)
      col_name <- paste0(grp_display, "_", strat_display)
      if (col_name %in% names(prepared$data)) {
        # Get Total row value
        total_idx <- which(prepared$data[[" "]] == "Total")
        if (length(total_idx) > 0) {
          actual_total <- as.numeric(prepared$data[[col_name]][total_idx])
          expect_equal(actual_total, expected_total, tolerance = 0.01,
                       info = paste("Value mismatch for", grp_display, strat_display))
        }
      }
    }
  }
})

test_that("outcomes_table() renders correctly with multiple groups", {
  results <- get_multi_group_test_results()

  # Should not error and return valid table
  tbl <- outcomes_table(results, "total_qalys", groups = "all")
  expect_true(inherits(tbl, "flextable"))
})

test_that("outcomes_table() with flextable renders correctly with multiple groups", {
  skip_if_not_installed("flextable")
  results <- get_multi_group_test_results()

  tbl <- outcomes_table(results, "total_qalys", groups = "all", table_format = "flextable")
  expect_s3_class(tbl, "flextable")
})

# ============================================================================
# Tests for multi-group pairwise CE tables
# ============================================================================

test_that("Multi-group pairwise CE table has group header rows", {
  results <- get_multi_group_test_results()

  prepared <- prepare_pairwise_ce_table_data(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "all",
    comparators = "standard",
    decimals = 2
  )

  # Should have group_header_rows in special_rows
  expect_true("special_rows" %in% names(prepared),
              info = "Should have special_rows")
  expect_true("group_header_rows" %in% names(prepared$special_rows),
              info = "Should have group_header_rows in special_rows")

  # Number of group headers should match number of groups displayed
  # (includes Overall + user-defined groups)
  n_groups_displayed <- length(unique(results$metadata$groups$name)) + 1  # +1 for Overall
  expect_equal(length(prepared$special_rows$group_header_rows), n_groups_displayed,
               info = "Should have one group header per group including Overall")
})

test_that("Multi-group pairwise CE table has correct columns", {
  results <- get_multi_group_test_results()

  prepared <- prepare_pairwise_ce_table_data(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "all",
    comparators = "standard",
    decimals = 2
  )

  # Should have expected columns (note: uses " ", "Cost", "Outcome", "ICER")
  expected_cols <- c(" ", "Cost", "Outcome", "ICER")
  for (col in expected_cols) {
    expect_true(col %in% names(prepared$data),
                info = paste("Should have column:", col))
  }
})

test_that("Multi-group pairwise CE table group headers contain group names", {
  results <- get_multi_group_test_results()

  prepared <- prepare_pairwise_ce_table_data(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "all",
    comparators = "standard",
    decimals = 2
  )

  # Get group display names (plus Overall)
  group_names <- c("Overall", results$metadata$groups$display_name)

  # Group header rows should contain the group names
  row_labels <- prepared$data[[" "]]
  group_header_indices <- prepared$special_rows$group_header_rows

  header_labels <- row_labels[group_header_indices]
  for (grp in group_names) {
    expect_true(grp %in% header_labels,
                info = paste("Group header should contain:", grp))
  }
})

test_that("pairwise_ce_table() renders correctly with multiple groups", {
  results <- get_multi_group_test_results()

  tbl <- pairwise_ce_table(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "all",
    comparators = "standard"
  )

  expect_true(inherits(tbl, "flextable"))
})

test_that("pairwise_ce_table() with flextable renders correctly with multiple groups", {
  skip_if_not_installed("flextable")
  results <- get_multi_group_test_results()

  tbl <- pairwise_ce_table(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "all",
    comparators = "standard",
    table_format = "flextable"
  )

  expect_s3_class(tbl, "flextable")
})
