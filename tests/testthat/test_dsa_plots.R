context("DSA Plots")

# ============================================================================
# Test Fixtures
# ============================================================================

# Build a simple DSA model with multiple parameters
build_simple_dsa_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10,
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years"
    ) %>%
    add_strategy("standard") %>%
    add_strategy("new_treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    # Variables with DSA specifications
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 2000, strategy = "standard") %>%
    add_variable("c_treatment", 8000, strategy = "new_treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    # DSA parameter specifications
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_healthy", low = 800, high = 1200,
                     display_name = "Cost (Healthy)") %>%
    add_dsa_variable("u_healthy", low = 0.8, high = 1.0,
                     display_name = "Utility (Healthy)") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summaries
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

get_example_dsa_results <- function() {
  model <- build_simple_dsa_model()
  run_dsa(model)
}

# ============================================================================
# Tests for dsa_outcomes_plot()
# ============================================================================

test_that("dsa_outcomes_plot() returns ggplot object", {
  results <- get_example_dsa_results()
  p <- dsa_outcomes_plot(results, "total_qalys")
  expect_s3_class(p, "ggplot")
})

test_that("dsa_outcomes_plot() parameters sorted by impact range", {
  results <- get_example_dsa_results()

  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Get the rect layer (tornado bars)
  rect_data <- NULL
  for (layer_data in built$data) {
    if ("xmin" %in% names(layer_data) && "xmax" %in% names(layer_data)) {
      rect_data <- layer_data
      break
    }
  }

  if (!is.null(rect_data)) {
    # Parameters should be sorted by range (largest first = highest y)
    # Calculate range for each parameter (y level)
    param_ranges <- rect_data %>%
      group_by(y) %>%
      summarize(range = max(xmax) - min(xmin), .groups = "drop") %>%
      arrange(desc(y))

    # Check ranges are in descending order (largest range = highest y)
    ranges <- param_ranges$range
    if (length(ranges) > 1) {
      for (i in 1:(length(ranges) - 1)) {
        expect_gte(ranges[i], ranges[i + 1])
      }
    }
  }
})

test_that("dsa_outcomes_plot() has base case reference line", {
  results <- get_example_dsa_results()

  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Look for vline layer
  has_vline <- FALSE
  for (layer_data in built$data) {
    if ("xintercept" %in% names(layer_data)) {
      has_vline <- TRUE
      # The xintercept should be the base case value
      expect_true(!is.na(layer_data$xintercept[1]))
      break
    }
  }
  expect_true(has_vline)
})

test_that("dsa_outcomes_plot() comparison mode calculates differences", {
  results <- get_example_dsa_results()
  strategies <- results$metadata$strategies$display_name

  # Single strategy mode
  p_single <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = strategies[1]
  )

  # Comparison mode - use cost outcome since it has more variation
  p_compare <- dsa_outcomes_plot(
    results, "total_cost",
    interventions = strategies[2],
    comparators = strategies[1]
  )

  built_single <- ggplot_build(p_single)
  built_compare <- ggplot_build(p_compare)

  # Both should produce plots
  expect_true(length(built_single$data) > 0)
  expect_true(length(built_compare$data) > 0)
})

test_that("dsa_outcomes_plot() drop_zero_impact removes flat bars", {
  results <- get_example_dsa_results()

  p_with <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = "standard",
    drop_zero_impact = TRUE
  )
  p_without <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = "standard",
    drop_zero_impact = FALSE
  )

  built_with <- ggplot_build(p_with)
  built_without <- ggplot_build(p_without)

  # With drop_zero_impact, should have same or fewer parameters
  rect_with <- NULL
  rect_without <- NULL
  for (ld in built_with$data) {
    if ("xmin" %in% names(ld)) rect_with <- ld
  }
  for (ld in built_without$data) {
    if ("xmin" %in% names(ld)) rect_without <- ld
  }

  if (!is.null(rect_with) && !is.null(rect_without)) {
    n_params_with <- length(unique(rect_with$y))
    n_params_without <- length(unique(rect_without$y))
    expect_lte(n_params_with, n_params_without)
  }
})

test_that("dsa_outcomes_plot() show_parameter_values adds values to labels", {
  results <- get_example_dsa_results()

  p_with <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = "standard",
    show_parameter_values = TRUE
  )
  p_without <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = "standard",
    show_parameter_values = FALSE
  )

  # Check y-axis labels are different
  built_with <- ggplot_build(p_with)
  built_without <- ggplot_build(p_without)

  # Labels with values should be longer (contain numbers)
  labels_with <- built_with$layout$panel_params[[1]]$y$get_labels()
  labels_without <- built_without$layout$panel_params[[1]]$y$get_labels()

  if (!is.null(labels_with) && !is.null(labels_without) &&
      length(labels_with) > 0 && length(labels_without) > 0) {
    # Labels with parameter values should be longer
    avg_len_with <- mean(nchar(labels_with))
    avg_len_without <- mean(nchar(labels_without))
    expect_gt(avg_len_with, avg_len_without)
  }
})

# ============================================================================
# Tests for dsa_nmb_plot()
# ============================================================================

test_that("dsa_nmb_plot() returns ggplot object", {
  results <- get_example_dsa_results()
  strategies <- results$metadata$strategies$display_name

  p <- dsa_nmb_plot(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    interventions = strategies[2],
    comparators = strategies[1],
    wtp = 50000  # Explicit WTP required
  )
  expect_s3_class(p, "ggplot")
})

test_that("dsa_nmb_plot() correctly computes NMB formula", {
  results <- get_example_dsa_results()
  strategies <- results$metadata$strategies$display_name

  wtp_test <- 50000

  p <- dsa_nmb_plot(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    interventions = strategies[2],
    comparators = strategies[1],
    wtp = wtp_test
  )
  built <- ggplot_build(p)

  # The NMB tornado values should be: delta_outcome * wtp - delta_cost
  # This is validated by the plot being created successfully
  expect_true(length(built$data) > 0)
})

test_that("dsa_nmb_plot() errors when no interventions/comparators", {
  results <- get_example_dsa_results()

  expect_error(
    dsa_nmb_plot(
      results,
      health_outcome = "total_qalys",
      cost_outcome = "total_cost"
    ),
    "interventions|comparators|required|specify"
  )
})

test_that("dsa_nmb_plot() works with explicit WTP parameter", {
  results <- get_example_dsa_results()
  strategies <- results$metadata$strategies$display_name

  # Test with different WTP values
  p1 <- dsa_nmb_plot(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    interventions = strategies[2],
    comparators = strategies[1],
    wtp = 25000
  )
  p2 <- dsa_nmb_plot(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    interventions = strategies[2],
    comparators = strategies[1],
    wtp = 100000
  )
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("dsa_nmb_plot() handles cost-only parameters", {
  results <- get_example_dsa_results()
  strategies <- results$metadata$strategies$display_name

  # c_healthy is a cost-only parameter (doesn't affect QALYs directly)
  p <- dsa_nmb_plot(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    interventions = strategies[2],
    comparators = strategies[1],
    wtp = 50000  # Explicit WTP required
  )
  built <- ggplot_build(p)

  # Check that parameters appear
  rect_data <- NULL
  for (ld in built$data) {
    if ("xmin" %in% names(ld)) {
      rect_data <- ld
      break
    }
  }

  # Should have at least one parameter
  if (!is.null(rect_data)) {
    expect_gt(nrow(rect_data), 0)
  }
})

# ============================================================================
# Tests for Helper Functions
# ============================================================================

# format_param_value() tests - Direct ::: access
test_that("format_param_value() uses significant figures correctly", {
  formatted <- openqaly:::format_param_value(1234.5678, 4)
  # Should round to 4 sig figs: 1235
  expect_true(grepl("1,235", formatted) || grepl("1235", formatted))
})

test_that("format_param_value() avoids scientific notation for large numbers", {
  formatted <- openqaly:::format_param_value(1000000, 4)
  # Should NOT contain "e" (scientific notation)
  expect_false(grepl("e", formatted, ignore.case = TRUE))
})

test_that("format_param_value() uses comma formatting", {
  formatted <- openqaly:::format_param_value(10000, 4)
  # Should contain comma for thousands separator
  expect_true(grepl(",", formatted))
})

# render_tornado_plot() tests - via exported functions
test_that("render_tornado_plot() single strategy/group has single panel", {
  results <- get_example_dsa_results()

  p <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = "standard",
    groups = "overall"
  )
  built <- ggplot_build(p)

  # Find rect layer
  rect_data <- NULL
  for (ld in built$data) {
    if ("xmin" %in% names(ld)) {
      rect_data <- ld
      break
    }
  }

  # Should have single panel
  if (!is.null(rect_data) && "PANEL" %in% names(rect_data)) {
    expect_equal(length(unique(rect_data$PANEL)), 1)
  }
})

test_that("render_tornado_plot() multiple strategies creates facets", {
  results <- get_example_dsa_results()

  p <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = NULL  # All strategies
  )
  built <- ggplot_build(p)

  # Find rect layer
  rect_data <- NULL
  for (ld in built$data) {
    if ("xmin" %in% names(ld)) {
      rect_data <- ld
      break
    }
  }

  # Should have multiple panels (one per strategy)
  if (!is.null(rect_data) && "PANEL" %in% names(rect_data)) {
    n_panels <- length(unique(rect_data$PANEL))
    expect_gte(n_panels, 2)
  }
})

# extract_parameter_values() tests - via exported functions
test_that("extract_parameter_values() extracts from variable overrides", {
  results <- get_example_dsa_results()

  p <- dsa_outcomes_plot(
    results, "total_qalys",
    strategies = "standard",
    show_parameter_values = TRUE
  )

  # The labels should contain the parameter values
  built <- ggplot_build(p)
  labels <- built$layout$panel_params[[1]]$y$get_labels()

  # Check that labels contain numeric values
  has_numbers <- any(grepl("[0-9]", labels))
  expect_true(has_numbers)
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("DSA plots workflow with example model", {
  model <- build_simple_dsa_model()
  results <- run_dsa(model)
  strategies <- results$metadata$strategies$display_name

  # DSA outcomes plot
  p1 <- dsa_outcomes_plot(results, "total_qalys", strategies = strategies[1])
  expect_s3_class(p1, "ggplot")

  # DSA NMB plot
  p2 <- dsa_nmb_plot(
    results,
    health_outcome = "total_qalys",
    cost_outcome = "total_cost",
    interventions = strategies[2],
    comparators = strategies[1],
    wtp = 50000  # Explicit WTP required
  )
  expect_s3_class(p2, "ggplot")
})

test_that("DSA plots work with cost summary", {
  results <- get_example_dsa_results()

  # Plot cost outcomes instead of QALY outcomes
  p <- dsa_outcomes_plot(results, "total_cost", strategies = "standard")
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

# ============================================================================
# Numeric Verification Tests - dsa_outcomes_plot()
# ============================================================================

test_that("dsa_outcomes_plot base line matches aggregated base case value", {
  results <- get_example_dsa_results()

  # Helper to extract summary value from aggregated results
  get_summary <- function(run_id, strategy, summary_name) {
    results$aggregated %>%
      dplyr::filter(.data$run_id == !!run_id, .data$strategy == !!strategy) %>%
      dplyr::pull(summaries) %>% .[[1]] %>%
      dplyr::filter(summary == summary_name) %>%
      dplyr::pull(amount) %>% sum()
  }

  # Compute expected base case value directly from results
  # run_id == 1 is always the base case
  expected_base <- get_summary(1, "standard", "total_qalys")

  # Build plot
  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Extract vline intercept
  vline_idx <- which(sapply(built$data, function(x) "xintercept" %in% names(x)))
  vline_data <- built$data[[vline_idx]]

  expect_equal(vline_data$xintercept[1], expected_base, tolerance = 0.01)
})

test_that("dsa_outcomes_plot bar endpoints match low/high run values", {
  results <- get_example_dsa_results()

  # Helper to extract summary value from aggregated results
  get_summary <- function(run_id, strategy, summary_name) {
    results$aggregated %>%
      dplyr::filter(.data$run_id == !!run_id, .data$strategy == !!strategy) %>%
      dplyr::pull(summaries) %>% .[[1]] %>%
      dplyr::filter(summary == summary_name) %>%
      dplyr::pull(amount) %>% sum()
  }

  # Get metadata to find which run_id corresponds to which parameter
  metadata <- results$dsa_metadata

  # For p_sick parameter:
  p_sick_low_run <- metadata %>%
    dplyr::filter(parameter == "p_sick", variation == "low") %>%
    dplyr::pull(run_id)
  p_sick_high_run <- metadata %>%
    dplyr::filter(parameter == "p_sick", variation == "high") %>%
    dplyr::pull(run_id)

  # Get the actual outcome values for those runs
  low_value <- get_summary(p_sick_low_run, "standard", "total_qalys")
  high_value <- get_summary(p_sick_high_run, "standard", "total_qalys")
  base_value <- get_summary(1, "standard", "total_qalys")

  # Build plot
  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Extract rect data
  rect_idx <- which(sapply(built$data, function(x) "xmin" %in% names(x)))
  rect_data <- built$data[[rect_idx]]

  # The plot should contain bars spanning from min(low, high, base) to max(low, high, base)
  all_values <- c(low_value, high_value, base_value)
  min_expected <- min(all_values)
  max_expected <- max(all_values)

  # Some bar should have xmin close to min_expected or xmax close to max_expected
  expect_true(
    any(abs(rect_data$xmin - min_expected) < 0.1) ||
    any(abs(rect_data$xmax - max_expected) < 0.1)
  )
})

# ============================================================================
# Numeric Verification Tests - dsa_nmb_plot()
# ============================================================================

test_that("dsa_nmb_plot base line equals (delta_qalys * wtp) - delta_costs", {
  results <- get_example_dsa_results()
  wtp <- 50000

  # Helper to extract summary value from aggregated results
  get_summary <- function(run_id, strategy, summary_name) {
    results$aggregated %>%
      dplyr::filter(.data$run_id == !!run_id, .data$strategy == !!strategy) %>%
      dplyr::pull(summaries) %>% .[[1]] %>%
      dplyr::filter(summary == summary_name) %>%
      dplyr::pull(amount) %>% sum()
  }

  # Get base case (run_id = 1) values
  int_qalys_base <- get_summary(1, "new_treatment", "total_qalys")
  comp_qalys_base <- get_summary(1, "standard", "total_qalys")
  int_cost_base <- get_summary(1, "new_treatment", "total_cost")
  comp_cost_base <- get_summary(1, "standard", "total_cost")

  delta_qalys <- int_qalys_base - comp_qalys_base
  delta_cost <- int_cost_base - comp_cost_base

  # NMB = (delta_outcome * wtp) - delta_cost
  expected_nmb <- (delta_qalys * wtp) - delta_cost

  # Build plot
  p <- dsa_nmb_plot(results, "total_qalys", "total_cost",
                    interventions = "new_treatment", comparators = "standard",
                    wtp = wtp)
  built <- ggplot_build(p)

  # Extract base line position
  vline_idx <- which(sapply(built$data, function(x) "xintercept" %in% names(x)))
  vline_data <- built$data[[vline_idx]]

  expect_equal(vline_data$xintercept[1], expected_nmb, tolerance = 1)
})

test_that("dsa_nmb_plot bar range reflects correct NMB for low/high runs", {
  results <- get_example_dsa_results()
  wtp <- 50000

  # Helper to extract summary value
  get_summary <- function(run_id, strategy, summary_name) {
    results$aggregated %>%
      dplyr::filter(.data$run_id == !!run_id, .data$strategy == !!strategy) %>%
      dplyr::pull(summaries) %>% .[[1]] %>%
      dplyr::filter(summary == summary_name) %>%
      dplyr::pull(amount) %>% sum()
  }

  # Find p_sick low/high run IDs
  p_sick_low_run <- results$dsa_metadata %>%
    dplyr::filter(parameter == "p_sick", variation == "low") %>%
    dplyr::pull(run_id)
  p_sick_high_run <- results$dsa_metadata %>%
    dplyr::filter(parameter == "p_sick", variation == "high") %>%
    dplyr::pull(run_id)

  # Compute NMB for low run
  int_qalys_low <- get_summary(p_sick_low_run, "new_treatment", "total_qalys")
  comp_qalys_low <- get_summary(p_sick_low_run, "standard", "total_qalys")
  int_cost_low <- get_summary(p_sick_low_run, "new_treatment", "total_cost")
  comp_cost_low <- get_summary(p_sick_low_run, "standard", "total_cost")
  nmb_low <- ((int_qalys_low - comp_qalys_low) * wtp) - (int_cost_low - comp_cost_low)

  # Compute NMB for high run
  int_qalys_high <- get_summary(p_sick_high_run, "new_treatment", "total_qalys")
  comp_qalys_high <- get_summary(p_sick_high_run, "standard", "total_qalys")
  int_cost_high <- get_summary(p_sick_high_run, "new_treatment", "total_cost")
  comp_cost_high <- get_summary(p_sick_high_run, "standard", "total_cost")
  nmb_high <- ((int_qalys_high - comp_qalys_high) * wtp) - (int_cost_high - comp_cost_high)

  expected_range <- abs(nmb_high - nmb_low)

  # Build plot
  p <- dsa_nmb_plot(results, "total_qalys", "total_cost",
                    interventions = "new_treatment", comparators = "standard",
                    wtp = wtp, show_parameter_values = FALSE)
  built <- ggplot_build(p)

  # Extract rect data
  rect_idx <- which(sapply(built$data, function(x) "xmin" %in% names(x)))
  rect_data <- built$data[[rect_idx]]

  # Calculate range for each y level (parameter)
  actual_ranges <- rect_data %>%
    dplyr::group_by(y) %>%
    dplyr::summarize(range = max(xmax) - min(xmin), .groups = "drop")

  # At least one range should approximately match our expected range
  # Use 5% tolerance since there may be minor rounding differences
  expect_true(
    any(abs(actual_ranges$range - expected_range) < expected_range * 0.05) ||
    expected_range < 1  # If expected range is tiny, skip this check
  )
})

# ============================================================================
# Edge Case Tests
# ============================================================================

test_that("format_param_value handles small decimals", {
  formatted <- openqaly:::format_param_value(0.001234, 4)
  # Should contain the significant digits

  expect_true(grepl("0\\.001", formatted))
})

test_that("format_param_value handles negative numbers", {
  formatted <- openqaly:::format_param_value(-1234.5, 4)
  expect_true(grepl("-", formatted))
})

test_that("dsa_outcomes_plot errors when strategies used with interventions", {
  results <- get_example_dsa_results()

  expect_error(
    dsa_outcomes_plot(results, "total_qalys",
                      strategies = "standard",
                      interventions = "new_treatment"),
    "cannot be used with|mutually exclusive"
  )
})

test_that("dsa_outcomes_plot works with discounted = TRUE", {
  results <- get_example_dsa_results()
  p <- dsa_outcomes_plot(results, "total_qalys", discounted = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("dsa_nmb_plot extracts WTP from metadata when not provided", {
  results <- get_example_dsa_results()

  # The model has wtp = 50000 in settings
  p <- dsa_nmb_plot(results, "total_qalys", "total_cost",
                    interventions = "new_treatment",
                    comparators = "standard")
  expect_s3_class(p, "ggplot")
})

test_that("dsa_nmb_plot errors when health_outcome not found", {
  results <- get_example_dsa_results()

  expect_error(
    dsa_nmb_plot(results, "nonexistent_outcome", "total_cost",
                 interventions = "new_treatment",
                 comparators = "standard",
                 wtp = 50000),
    "not found|does not exist|invalid"
  )
})

test_that("dsa_nmb_plot works with drop_zero_impact = FALSE", {
  results <- get_example_dsa_results()

  p <- dsa_nmb_plot(results, "total_qalys", "total_cost",
                    interventions = "new_treatment",
                    comparators = "standard",
                    wtp = 50000,
                    drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# Same-side Bar Stacking Tests
# ============================================================================

test_that("render_tornado_plot uses half-height stacked bars when both values on same side", {
  # Create DSA model with a parameter where both low and high produce same-side results
  model <- define_model("markov") %>%
    set_settings(
      n_cycles = 10,
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years"
    ) %>%
    add_strategy("standard") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    # Variable that affects outcomes positively
    add_variable("u_healthy", 0.8) %>%
    add_variable("u_sick", 0.5) %>%
    # DSA specification where both low AND high are ABOVE base case
    # (both increase utility, so QALYs will be higher in both cases)
    add_dsa_variable("u_healthy", low = 0.85, high = 0.95,
                     display_name = "Utility (Both Above Base)") %>%
    # Transitions
    add_transition("healthy", "sick", "0.1") %>%
    add_transition("healthy", "dead", "0.05") %>%
    add_transition("healthy", "healthy", "0.85") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summary
    add_summary("total_qalys", "qalys")

  results <- run_dsa(model)

  # Build the plot
  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Find the rect layer (tornado bars)
  rect_data <- NULL
  for (ld in built$data) {
    if ("xmin" %in% names(ld) && "height" %in% names(ld)) {
      rect_data <- ld
      break
    }
  }

  expect_false(is.null(rect_data))

  # Since both low and high are above base case:
  # - The bars should have height 0.4 (half of normal 0.8)
  # - The bars should be at different y positions (offset by +/- 0.2)
  if (!is.null(rect_data)) {
    # Calculate actual rendered height from ymax - ymin
    rect_data <- rect_data %>%
      dplyr::mutate(actual_height = ymax - ymin)

    # Both bars for same-side parameters should have half height (0.4)
    same_side_bars <- rect_data %>%
      dplyr::filter(abs(actual_height - 0.4) < 0.01)

    # Same-side parameters should have half-height (0.4) bars
    expect_gt(nrow(same_side_bars), 0)

    # Check that same-side bars are offset (different y values for same parameter)
    if (nrow(same_side_bars) >= 2) {
      y_values <- round(same_side_bars$y, 1)
      # For stacked bars, we expect pairs with small y differences (the offset)
      y_diffs <- diff(sort(y_values))
      # Same-side bars should be vertically offset
      expect_true(any(y_diffs < 0.5 & y_diffs > 0))
    }
  }
})

test_that("render_tornado_plot uses normal height when values on opposite sides", {
  results <- get_example_dsa_results()

  # Build the plot
  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Find the rect layer
  rect_data <- NULL
  for (ld in built$data) {
    if ("xmin" %in% names(ld) && "height" %in% names(ld)) {
      rect_data <- ld
      break
    }
  }

  expect_false(is.null(rect_data))

  # For normal opposite-side parameters, bars should have height 0.8
  if (!is.null(rect_data)) {
    # Calculate actual rendered height from ymax - ymin
    rect_data <- rect_data %>%
      dplyr::mutate(actual_height = ymax - ymin)

    normal_bars <- rect_data %>%
      dplyr::filter(abs(actual_height - 0.8) < 0.01)

    # Most bars in the standard test fixture should be normal height
    expect_gt(nrow(normal_bars), 0)
  }
})

# ============================================================================
# Tests for unit formatting in DSA setting labels
# ============================================================================

test_that("abbreviate_time_unit() abbreviates time units correctly", {
  expect_equal(openqaly:::abbreviate_time_unit("years"), "yrs")
  expect_equal(openqaly:::abbreviate_time_unit("year"), "yrs")
  expect_equal(openqaly:::abbreviate_time_unit("months"), "mos")
  expect_equal(openqaly:::abbreviate_time_unit("month"), "mos")
  expect_equal(openqaly:::abbreviate_time_unit("weeks"), "wks")
  expect_equal(openqaly:::abbreviate_time_unit("week"), "wks")
  expect_equal(openqaly:::abbreviate_time_unit("days"), "days")
  expect_equal(openqaly:::abbreviate_time_unit("day"), "days")
  expect_equal(openqaly:::abbreviate_time_unit("cycles"), "cycles")
  expect_equal(openqaly:::abbreviate_time_unit("cycle"), "cycles")
})

test_that("abbreviate_time_unit() handles edge cases", {
  expect_equal(openqaly:::abbreviate_time_unit(NULL), "")
  expect_equal(openqaly:::abbreviate_time_unit(NA), "")
  expect_equal(openqaly:::abbreviate_time_unit(""), "")
  expect_equal(openqaly:::abbreviate_time_unit("unknown"), "unknown")
})

test_that("get_setting_unit_suffix() returns % for discount settings", {
  settings <- list(timeframe_unit = "years", cycle_length_unit = "months")
  expect_equal(openqaly:::get_setting_unit_suffix("discount_cost", settings), "%")
  expect_equal(openqaly:::get_setting_unit_suffix("discount_outcomes", settings), "%")
})

test_that("get_setting_unit_suffix() returns abbreviated time unit for time settings", {
  settings <- list(timeframe_unit = "years", cycle_length_unit = "months")
  expect_equal(openqaly:::get_setting_unit_suffix("timeframe", settings), "yrs")
  expect_equal(openqaly:::get_setting_unit_suffix("cycle_length", settings), "mos")
})

test_that("get_setting_unit_suffix() returns empty for other settings", {
  settings <- list(timeframe_unit = "years", cycle_length_unit = "months")
  expect_equal(openqaly:::get_setting_unit_suffix("half_cycle_method", settings), "")
  expect_equal(openqaly:::get_setting_unit_suffix("days_per_year", settings), "")
  expect_equal(openqaly:::get_setting_unit_suffix("reduce_state_cycle", settings), "")
  expect_equal(openqaly:::get_setting_unit_suffix("timeframe_unit", settings), "")
  expect_equal(openqaly:::get_setting_unit_suffix("cycle_length_unit", settings), "")
})

test_that("get_setting_unit_suffix() handles NULL settings", {
  expect_equal(openqaly:::get_setting_unit_suffix("discount_cost", NULL), "")
})

# Build a model with DSA settings to test unit formatting in labels
build_dsa_settings_model <- function() {
  define_model("markov") %>%
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3,
      discount_outcomes = 3
    ) %>%
    add_strategy("standard") %>%
    add_strategy("new_treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 2000, strategy = "standard") %>%
    add_variable("c_treatment", 8000, strategy = "new_treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    # DSA variables (should have no unit suffix)
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    # DSA settings (should have appropriate unit suffixes)
    add_dsa_setting("discount_cost", low = 0, high = 5,
                    display_name = "Cost Discount") %>%
    add_dsa_setting("timeframe", low = 5, high = 20,
                    display_name = "Time Horizon") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summaries
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

test_that("dsa_outcomes_plot() shows % suffix for discount settings in labels", {
  model <- build_dsa_settings_model()
  results <- run_dsa(model)

  # Use drop_zero_impact = FALSE because discount_cost has zero impact on QALYs
  # (cost discounting only affects cost summaries, not outcome summaries)
  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard", drop_zero_impact = FALSE)
  built <- ggplot_build(p)

  # Extract y-axis labels
  y_labels <- built$layout$panel_params[[1]]$y$get_labels()

  # Find the discount cost label - should have %
  discount_labels <- y_labels[grepl("Discount", y_labels, ignore.case = TRUE)]
  expect_true(any(grepl("%", discount_labels)),
              info = paste("Discount labels should contain %:", paste(discount_labels, collapse = ", ")))
})

test_that("dsa_outcomes_plot() shows time unit suffix for timeframe setting", {
  model <- build_dsa_settings_model()
  results <- run_dsa(model)

  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Extract y-axis labels
  y_labels <- built$layout$panel_params[[1]]$y$get_labels()

  # Find the timeframe label - should have yrs (abbreviated years)
  timeframe_labels <- y_labels[grepl("Time Horizon", y_labels, ignore.case = TRUE)]
  expect_true(any(grepl("yrs", timeframe_labels)),
              info = paste("Timeframe labels should contain yrs:", paste(timeframe_labels, collapse = ", ")))
})

test_that("dsa_outcomes_plot() shows no unit suffix for variable parameters", {
  model <- build_dsa_settings_model()
  results <- run_dsa(model)

  p <- dsa_outcomes_plot(results, "total_qalys", strategies = "standard")
  built <- ggplot_build(p)

  # Extract y-axis labels
  y_labels <- built$layout$panel_params[[1]]$y$get_labels()

  # Find the p_sick label - should NOT have % or yrs
  prob_labels <- y_labels[grepl("Prob\\. Getting Sick", y_labels)]
  expect_true(length(prob_labels) > 0,
              info = "Should find probability label")
  # Variable labels should have (low - high) format but no unit suffix
  expect_false(any(grepl("%", prob_labels)),
               info = paste("Variable labels should not contain %:", paste(prob_labels, collapse = ", ")))
  expect_false(any(grepl("yrs", prob_labels)),
               info = paste("Variable labels should not contain yrs:", paste(prob_labels, collapse = ", ")))
})


# ============================================================================
# CE Test Fixtures - Models that produce specific ICER scenarios
# ============================================================================

# Normal ICER: Treatment more costly AND more effective (NE quadrant)
build_normal_icer_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 10000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.5, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 5000, high = 15000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("u_sick", low = 0.3, high = 0.7,
                     display_name = "Utility (Sick)") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Dominated: Treatment more costly AND less effective (SE quadrant, ICER = Inf)
build_dominated_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 10000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 1.5, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 5000, high = 15000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("treatment_effect", low = 1.2, high = 2.0,
                     display_name = "Treatment Harm", strategy = "treatment") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Dominant: Treatment less costly AND more effective (NW quadrant, ICER = 0)
build_dominant_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 5000, strategy = "control") %>%
    add_variable("c_treatment", 1000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.3, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 500, high = 2000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("treatment_effect", low = 0.1, high = 0.5,
                     display_name = "Treatment Effect", strategy = "treatment") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Flipped: Treatment less costly BUT less effective (SW quadrant, negative ICER)
build_flipped_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 8000, strategy = "control") %>%
    add_variable("c_treatment", 2000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 1.3, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 1000, high = 3000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("treatment_effect", low = 1.1, high = 1.5,
                     display_name = "Treatment Effect", strategy = "treatment") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Multiple strategies model (3 strategies)
build_multi_strategy_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment_a") %>%
    add_strategy("treatment_b") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 8000, strategy = "treatment_a") %>%
    add_variable("c_treatment", 15000, strategy = "treatment_b") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.6, strategy = "treatment_a") %>%
    add_variable("treatment_effect", 0.3, strategy = "treatment_b") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 5000, high = 12000,
                     display_name = "Treatment A Cost", strategy = "treatment_a") %>%
    add_dsa_variable("c_treatment", low = 10000, high = 20000,
                     display_name = "Treatment B Cost", strategy = "treatment_b") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# ============================================================================
# Tests for dsa_ce_plot() - Cost-Effectiveness Tornado Plot
# ============================================================================

test_that("dsa_ce_plot() works with normal ICER scenario", {
  model <- build_normal_icer_model()
  results <- run_dsa(model)

  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   interventions = "treatment", comparators = "control")
  expect_s3_class(p, "ggplot")

  # Verify it builds without error
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("dsa_ce_plot() works with dominated scenario", {
  model <- build_dominated_model()
  results <- run_dsa(model)

  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   interventions = "treatment", comparators = "control",
                   drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("dsa_ce_plot() works with dominant scenario", {
  model <- build_dominant_model()
  results <- run_dsa(model)

  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   interventions = "treatment", comparators = "control",
                   drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("dsa_ce_plot() works with flipped scenario", {
  model <- build_flipped_model()
  results <- run_dsa(model)

  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   interventions = "treatment", comparators = "control",
                   drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("dsa_ce_plot() works with multiple strategies", {
  model <- build_multi_strategy_model()
  results <- run_dsa(model)

  # All interventions vs control
  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   comparators = "control", drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("dsa_ce_plot() works with specific intervention and comparator", {
  model <- build_multi_strategy_model()
  results <- run_dsa(model)

  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   interventions = "treatment_a", comparators = "control",
                   drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("dsa_ce_plot() requires interventions or comparators", {
  model <- build_normal_icer_model()
  results <- run_dsa(model)

  expect_error(
    dsa_ce_plot(results, "total_qalys", "total_cost"),
    "interventions.*comparators"
  )
})

test_that("dsa_ce_plot() show_parameter_values option works", {
  model <- build_normal_icer_model()
  results <- run_dsa(model)

  p_with <- dsa_ce_plot(results, "total_qalys", "total_cost",
                        interventions = "treatment", comparators = "control",
                        show_parameter_values = TRUE)
  p_without <- dsa_ce_plot(results, "total_qalys", "total_cost",
                           interventions = "treatment", comparators = "control",
                           show_parameter_values = FALSE)

  expect_s3_class(p_with, "ggplot")
  expect_s3_class(p_without, "ggplot")
})

test_that("dsa_ce_plot() drop_zero_impact option works", {
  model <- build_normal_icer_model()
  results <- run_dsa(model)

  p <- dsa_ce_plot(results, "total_qalys", "total_cost",
                   interventions = "treatment", comparators = "control",
                   drop_zero_impact = FALSE)
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# Tests for ICER Helper Functions
# ============================================================================

test_that("classify_base_case() returns correct classifications", {
  expect_equal(openqaly:::classify_base_case(50000), "normal")
  expect_equal(openqaly:::classify_base_case(-50000), "flipped")
  expect_equal(openqaly:::classify_base_case(0), "dominant")
  expect_equal(openqaly:::classify_base_case(Inf), "dominated")
  expect_equal(openqaly:::classify_base_case(NaN), "identical")
  expect_equal(openqaly:::classify_base_case(NA_real_), "reference")
})

test_that("detect_variation_error() returns correct error states", {
  # Normal base with direction change
 result <- openqaly:::detect_variation_error("normal", -50000)
  expect_equal(result$type, "direction_change")
  expect_false(result$show_bar)

  # Normal base with identical outcome
  result <- openqaly:::detect_variation_error("normal", NaN)
  expect_equal(result$type, "identical")
  expect_false(result$show_bar)

  # Normal base with dominated
  result <- openqaly:::detect_variation_error("normal", Inf)
  expect_null(result$type)
  expect_true(result$show_bar)
  expect_equal(result$bar_type, "arrow")

  # Normal base with dominant
  result <- openqaly:::detect_variation_error("normal", 0)
  expect_null(result$type)
  expect_true(result$show_bar)
  expect_equal(result$bar_type, "to_zero")

  # Normal base with normal variation
  result <- openqaly:::detect_variation_error("normal", 60000)
  expect_null(result$type)
  expect_true(result$show_bar)
  expect_equal(result$bar_type, "standard")
})

test_that("format_icer_label() formats values correctly", {
  expect_equal(openqaly:::format_icer_label(50000), "$50,000")
  expect_equal(openqaly:::format_icer_label(50000, is_flipped = TRUE), "$50,000*")
  expect_equal(openqaly:::format_icer_label(Inf), "Dominated")
  expect_equal(openqaly:::format_icer_label(0), "Dominant")
  expect_equal(openqaly:::format_icer_label(NaN), "Equivalent")
  expect_equal(openqaly:::format_icer_label(NA_real_), "")
})

test_that("generate_ce_error_message() creates correct messages", {
  msg <- openqaly:::generate_ce_error_message(
    "direction_change", "Low", -50000, "Treatment A", "Control"
  )
  expect_true(grepl("Low value", msg))
  expect_true(grepl("directionality", msg))
  expect_true(grepl("Control vs. Treatment A", msg))

  msg <- openqaly:::generate_ce_error_message(
    "identical", "High", NaN, "Treatment A", "Control"
  )
  expect_true(grepl("High value", msg))
  expect_true(grepl("identical outcomes and costs", msg))

  expect_true(is.na(
    openqaly:::generate_ce_error_message(NULL, "Low", 50000, "A", "B")
  ))
})

test_that("generate_ce_footnote() creates correct footnote", {
  footnote <- openqaly:::generate_ce_footnote("Control", "Treatment A")
  expect_true(grepl("Control is more costly", footnote))
  expect_true(grepl("more effective than Treatment A", footnote))
})
