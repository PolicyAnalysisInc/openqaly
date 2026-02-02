context("Base Case NMB Tables")

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

# ============================================================================
# Tests for nmb_table()
# ============================================================================

test_that("nmb_table() calculates NMB = (dOutcome * WTP) - dCost", {
  results <- get_test_results()

  # Get expected NMB from calculate_nmb (raw values)
  expected <- calculate_nmb(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    groups = "overall",
    comparators = "standard"
  )
  expected_nmb <- expected$nmb_amount[1]

  # Get NMB from plot data (raw values, same source as table)
  p <- nmb_plot_bar(results,
                    health_outcome = "total_qalys",
                    cost_outcome = "total_cost",
                    comparators = "standard")
  total_row <- p$data[p$data$value == "Total", ]
  actual_nmb <- total_row$amount[1]

  expect_equal(actual_nmb, expected_nmb, tolerance = 1)
})

test_that("nmb_table() component rows sum to Total row", {
  results <- get_test_results()

  # Get raw data from plot (unformatted)
  p <- nmb_plot_bar(results,
                    health_outcome = "total_qalys",
                    cost_outcome = "total_cost",
                    comparators = "standard")

  total_value <- p$data$amount[p$data$value == "Total"]
  component_values <- p$data$amount[p$data$value != "Total"]
  component_sum <- sum(component_values)

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
