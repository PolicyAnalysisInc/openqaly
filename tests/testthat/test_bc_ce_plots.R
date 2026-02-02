context("Base Case CE Plots")

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
# Tests for pairwise_ce_plot()
# ============================================================================

test_that("pairwise_ce_plot() returns ggplot", {
  results <- get_test_results()

  p <- pairwise_ce_plot(results,
                        outcome_summary = "total_qalys",
                        cost_summary = "total_cost",
                        comparators = "standard")

  expect_s3_class(p, "ggplot")
})

test_that("pairwise_ce_plot() includes x=0 and y=0 reference lines", {
  results <- get_test_results()

  p <- pairwise_ce_plot(results,
                        outcome_summary = "total_qalys",
                        cost_summary = "total_cost",
                        comparators = "standard")

  # Check for hline and vline
  has_hline <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomHline")))
  has_vline <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomVline")))

  expect_true(has_hline && has_vline)
})

test_that("pairwise_ce_plot() errors without interventions or comparators", {
  results <- get_test_results()

  expect_error(
    pairwise_ce_plot(results,
                     outcome_summary = "total_qalys",
                     cost_summary = "total_cost"),
    "must be provided"
  )
})
