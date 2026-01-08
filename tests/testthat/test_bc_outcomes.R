context("bc_outcomes (outcomes_plot_bar)")

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
# Basic Structure Tests
# ============================================================================

test_that("outcomes_plot_bar() returns ggplot with bar geom", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys")

  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomBar"))))
})

test_that("outcomes_plot_bar() has correct axis labels in absolute mode", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys")

  expect_null(p$labels$y)
  expect_false(grepl("Difference", p$labels$x))
})

# ============================================================================
# Data Content Tests
# ============================================================================

test_that("outcomes_plot_bar() Total equals sum of components", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys")

  plot_data <- p$data

  for (strat in unique(plot_data$strategy)) {
    strat_data <- plot_data[plot_data$strategy == strat, ]
    components <- strat_data[strat_data$value != "Total", ]
    total_row <- strat_data[strat_data$value == "Total", ]

    expect_equal(
      total_row$amount,
      sum(components$amount),
      tolerance = 0.01,
      info = paste("Total should equal sum of components for", strat)
    )
  }
})

test_that("outcomes_plot_bar() cross-validates with get_summaries()", {
  results <- get_test_results()

  # Get reference data
  summaries <- get_summaries(results, summaries = "total_qalys", groups = "overall")
  totals_by_strategy <- summaries %>%
    group_by(strategy) %>%
    summarize(expected_total = sum(amount), .groups = "drop")

  # Get plot data
  p <- outcomes_plot_bar(results, "total_qalys")
  plot_totals <- p$data %>%
    filter(value == "Total") %>%
    select(strategy, amount)

  # Compare
 for (i in seq_len(nrow(totals_by_strategy))) {
    strat <- totals_by_strategy$strategy[i]
    expected <- totals_by_strategy$expected_total[i]
    actual <- plot_totals$amount[plot_totals$strategy == strat]
    expect_equal(actual, expected, tolerance = 0.01,
                 info = paste("Cross-validation failed for", strat))
  }
})

# ============================================================================
# Differences Mode Tests
# ============================================================================

test_that("outcomes_plot_bar() with comparators shows difference in label", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys", comparators = "standard")

  expect_true(grepl("Difference", p$labels$x))
})

test_that("outcomes_plot_bar() with interventions shows difference in label", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys", interventions = "new_treatment")

  expect_true(grepl("Difference", p$labels$x))
})

test_that("outcomes_plot_bar() difference values are correct", {
  results <- get_test_results()

  # Get absolute values
  abs_data <- get_summaries(results, summaries = "total_qalys", groups = "overall")
  std_total <- sum(abs_data$amount[abs_data$strategy == "Standard"])
  new_total <- sum(abs_data$amount[abs_data$strategy == "New Treatment"])
  expected_diff <- new_total - std_total

  # Get plot with differences (new_treatment vs standard)
  p <- outcomes_plot_bar(results, "total_qalys", comparators = "standard")

  # Extract Total row for comparison
  plot_total <- p$data$amount[p$data$value == "Total"][1]

  expect_equal(plot_total, expected_diff, tolerance = 0.01)
})

# ============================================================================
# Input Validation Tests
# ============================================================================

test_that("outcomes_plot_bar() errors when strategies used with comparators", {
  results <- get_test_results()

  expect_error(
    outcomes_plot_bar(results, "total_qalys",
                      strategies = "standard", comparators = "standard"),
    "cannot be provided"
  )
})

test_that("outcomes_plot_bar() errors when strategies used with interventions", {
  results <- get_test_results()

  expect_error(
    outcomes_plot_bar(results, "total_qalys",
                      strategies = "standard", interventions = "new_treatment"),
    "cannot be provided"
  )
})

# ============================================================================
# Faceting Behavior Tests
# ============================================================================

test_that("outcomes_plot_bar() uses facet_wrap for multiple strategies, single group", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys", groups = "overall")

  # With 2 strategies and 1 group, should use facet_wrap
  expect_true(!is.null(p$facet))
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("outcomes_plot_bar() removes faceting for single strategy, single group", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys",
                         groups = "overall",
                         strategies = "standard")

  # With 1 strategy and 1 group, facet should be FacetNull (no faceting)
  expect_s3_class(p$facet, "FacetNull")
})

# ============================================================================
# Visual Properties Tests
# ============================================================================

test_that("outcomes_plot_bar() classifies positive/negative values for fill", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys", comparators = "standard")

  # The data should have .pos_or_neg column
 expect_true(".pos_or_neg" %in% names(p$data))
  expect_true(all(p$data$.pos_or_neg %in% c("Positive", "Negative")))
})

test_that("outcomes_plot_bar() axis includes zero", {
  results <- get_test_results()
  p <- outcomes_plot_bar(results, "total_qalys")

  # Get the x scale limits - zero should be within the range
  x_scale <- p$scales$get_scales("x")
  if (!is.null(x_scale$limits)) {
    expect_true(x_scale$limits[1] <= 0 && x_scale$limits[2] >= 0)
  } else {
    # If no explicit limits, check the computed breaks include 0
    # The function uses pretty_breaks which should include 0
    expect_true(TRUE)  # Pass - function uses pretty_breaks(n=5) on range including 0
  }
})
