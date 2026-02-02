context("Base Case CE Tables")

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
    add_summary("total_cost", "drug_cost,care_cost", type = "cost", display_name = "Total Cost") |>
    add_summary("total_qalys", "healthy_qalys,sick_qalys", type = "outcome", wtp = 50000, display_name = "QALYs")
}

get_multi_group_test_results <- function() {
  model <- build_multi_group_test_model()
  run_model(model)
}

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

  # Should have expected columns matching display names from test model
  expected_cols <- c(" ", "Total Cost", "QALYs", "ICER")
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
