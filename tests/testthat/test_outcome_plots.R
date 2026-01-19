context("Outcome Plots")

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
# Tests for outcomes_plot_line()
# ============================================================================

test_that("outcomes_plot_line() returns ggplot with line geom", {
  results <- get_test_results()

  p <- outcomes_plot_line(results, outcome = "total_qalys")

  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

test_that("outcomes_plot_line() cumulative values increase monotonically", {
  results <- get_test_results()

  p <- outcomes_plot_line(results, outcome = "total_qalys", cumulative = TRUE)

  # Extract Total line data for one strategy
  total_data <- p$data[p$data$value_name == "Total", ]
  std_data <- total_data[grepl("Standard", total_data$strategy), ]
  std_data <- std_data[order(std_data$cycle), ]

  # Cumulative should be monotonically increasing (for positive utilities)
  diffs <- diff(std_data$amount)
  expect_true(all(diffs >= 0), info = "Cumulative should not decrease")
})

test_that("outcomes_plot_line() non-cumulative mode shows per-cycle values", {
  results <- get_test_results()

  p_cum <- outcomes_plot_line(results, outcome = "total_qalys", cumulative = TRUE)
  p_noncum <- outcomes_plot_line(results, outcome = "total_qalys", cumulative = FALSE)

  # Non-cumulative final value should be less than cumulative final value
  cum_data <- p_cum$data[p_cum$data$value_name == "Total", ]
  noncum_data <- p_noncum$data[p_noncum$data$value_name == "Total", ]

  max_cycle <- max(cum_data$cycle)
  cum_final <- cum_data$amount[cum_data$cycle == max_cycle][1]
  noncum_final <- noncum_data$amount[noncum_data$cycle == max_cycle][1]

  expect_true(noncum_final < cum_final)
})

test_that("outcomes_plot_line() with comparator has 'Difference' in label", {
  results <- get_test_results()

  p <- outcomes_plot_line(results, outcome = "total_qalys",
                          comparators = "standard")

  expect_true(grepl("Difference", p$labels$y))
})

test_that("outcomes_plot_line() errors when strategies used with comparators", {
  results <- get_test_results()

  expect_error(
    outcomes_plot_line(results, outcome = "total_qalys",
                       strategies = "standard", comparators = "standard"),
    "'strategies' cannot be used"
  )
})

# ============================================================================
# Tests for nmb_plot_bar()
# ============================================================================

test_that("nmb_plot_bar() returns ggplot with bar geom", {
  results <- get_test_results()

  p <- nmb_plot_bar(results,
                    health_outcome = "total_qalys",
                    cost_outcome = "total_cost",
                    comparators = "standard")

  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomBar"))))
})

test_that("nmb_plot_bar() includes Total component", {
  results <- get_test_results()

  p <- nmb_plot_bar(results,
                    health_outcome = "total_qalys",
                    cost_outcome = "total_cost",
                    comparators = "standard")

  values_in_plot <- unique(p$data$value)
  expect_true("Total" %in% values_in_plot)
})

test_that("nmb_plot_bar() errors without interventions or comparators", {
  results <- get_test_results()

  expect_error(
    nmb_plot_bar(results,
                 health_outcome = "total_qalys",
                 cost_outcome = "total_cost"),
    "must be provided"
  )
})

test_that("nmb_plot_bar() orders values correctly: outcomes, costs, Total", {
  results <- get_test_results()

  p <- nmb_plot_bar(results,
                    health_outcome = "total_qalys",
                    cost_outcome = "total_cost",
                    comparators = "standard")

  # Get the factor levels from the plot data
  # Note: ggplot displays y-axis factor levels from bottom to top
  # So factor levels are reversed: Total first (bottom), then costs, then outcomes (top)
  value_levels <- levels(p$data$value)

  # Total should be first in factor levels (displays at bottom of chart)
  expect_equal(value_levels[1], "Total")

  # In factor level order: Total, costs, outcomes
  # In visual order (top to bottom): outcomes, costs, Total
  total_pos <- which(value_levels == "Total")
  outcome_positions <- which(value_levels %in% c("healthy_qalys", "sick_qalys"))
  cost_positions <- which(value_levels %in% c("drug_cost", "care_cost"))

  # Total should be first (position 1) in factor levels
  expect_equal(total_pos, 1)

  # Costs should come after Total in factor levels
  expect_true(all(cost_positions > total_pos))

  # Outcomes should come after costs in factor levels (appear at top visually)
  expect_true(all(outcome_positions > max(cost_positions)),
              info = "Outcomes should appear at top (last in factor levels)")
})

# ============================================================================
# Tests for nmb_plot_line()
# ============================================================================

test_that("nmb_plot_line() returns ggplot with line geom", {
  results <- get_test_results()

  p <- nmb_plot_line(results,
                     health_outcome = "total_qalys",
                     cost_outcome = "total_cost",
                     comparators = "standard")

  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

test_that("nmb_plot_line() cumulative NMB produces finite values", {
  results <- get_test_results()

  p <- nmb_plot_line(results,
                     health_outcome = "total_qalys",
                     cost_outcome = "total_cost",
                     comparators = "standard",
                     cumulative = TRUE)

  # NMB should have finite values
  total_data <- p$data[p$data$value_name == "Total", ]
  final_nmb <- total_data$amount[total_data$cycle == max(total_data$cycle)][1]

  expect_true(is.finite(final_nmb))
})

test_that("nmb_plot_line() only shows values from specified summaries", {
  results <- get_test_results()

  p <- nmb_plot_line(results,
                     health_outcome = "total_qalys",
                     cost_outcome = "total_cost",
                     comparators = "standard")

  # Get unique value names in the plot
  value_names <- unique(p$data$value_name)

  # Should only contain values from total_qalys and total_cost summaries, plus Total
  # total_qalys contains: healthy_qalys, sick_qalys
  # total_cost contains: drug_cost, care_cost
  expected_values <- c("healthy_qalys", "sick_qalys", "drug_cost", "care_cost", "Total")

  # All values should be in expected set
  expect_true(all(value_names %in% expected_values))
})

test_that("nmb_plot_line() errors without interventions or comparators", {
  results <- get_test_results()

  expect_error(
    nmb_plot_line(results,
                  health_outcome = "total_qalys",
                  cost_outcome = "total_cost"),
    "must be provided"
  )
})

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

# ============================================================================
# Tests for consistency between table and plot
# ============================================================================

test_that("nmb_table() and nmb_plot_bar() produce consistent NMB values", {
  results <- get_test_results()

  # Get NMB from table
  prepared <- prepare_nmb_table_data(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    comparators = "standard",
    show_total = TRUE, decimals = 0
  )
  comparison_col <- names(prepared$data)[grepl("vs\\.", names(prepared$data))]
  total_idx <- which(prepared$data[[" "]] == "Total")
  table_nmb <- as.numeric(prepared$data[[comparison_col]][total_idx])

  # Get NMB from plot
  p <- nmb_plot_bar(results,
                    health_outcome = "total_qalys",
                    cost_outcome = "total_cost",
                    comparators = "standard")
  total_row <- p$data[p$data$value == "Total", ]
  plot_nmb <- total_row$amount[1]

  expect_equal(table_nmb, plot_nmb, tolerance = 1)
})
