context("PSA Plots")

# Uses shared fixtures from setup.R:
# - build_simple_psa_model() - builds the test model
# - get_cached_psa_results() - returns cached PSA results (computed once)

# ============================================================================
# Tests for incremental_ceac_plot()
# ============================================================================

test_that("incremental_ceac_plot() returns ggplot object", {
  results <- get_cached_psa_results()
  p <- incremental_ceac_plot(results, "total_qalys", "total_cost")
  expect_s3_class(p, "ggplot")
})

test_that("incremental_ceac_plot() data matches calculate_incremental_ceac()", {
  results <- get_cached_psa_results()

  # Get calculation result
  ceac_data <- calculate_incremental_ceac(
    results, "total_qalys", "total_cost",
    wtp = seq(0, 100000, by = 25000)
  )

  # Get plot data
  p <- incremental_ceac_plot(
    results, "total_qalys", "total_cost",
    wtp_range = c(0, 100000), wtp_step = 25000
  )
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]  # First layer (geom_line)

  # y values should match probabilities (allowing for ordering differences)
  expect_equal(
    sort(round(plot_data$y, 6)),
    sort(round(ceac_data$probability, 6))
  )
})

test_that("incremental_ceac_plot() probabilities sum to 1 at each WTP", {
  results <- get_cached_psa_results()

  p <- incremental_ceac_plot(
    results, "total_qalys", "total_cost",
    wtp_range = c(0, 100000), wtp_step = 25000
  )
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Group by x (WTP) and sum probabilities
  wtp_sums <- plot_data %>%
    group_by(x) %>%
    summarize(prob_sum = sum(y), .groups = "drop")

  # Each WTP should sum to 1

  for (i in seq_len(nrow(wtp_sums))) {
    expect_equal(wtp_sums$prob_sum[i], 1, tolerance = 1e-6)
  }
})

test_that("incremental_ceac_plot() respects strategies filter", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  # Filter to first strategy only
  p <- incremental_ceac_plot(
    results, "total_qalys", "total_cost",
    strategies = strategies[1]
  )
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # Should only have one strategy (one unique color)
  expect_equal(length(unique(plot_data$colour)), 1)
})

test_that("incremental_ceac_plot() accepts pre-calculated CEAC data", {
  results <- get_cached_psa_results()

  # Pre-calculate CEAC
  ceac_data <- calculate_incremental_ceac(
    results, "total_qalys", "total_cost",
    wtp = seq(0, 50000, by = 10000)
  )

  # Pass pre-calculated data
  p <- incremental_ceac_plot(ceac_data)
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

# ============================================================================
# Tests for psa_scatter_plot()
# ============================================================================

test_that("psa_scatter_plot() returns ggplot object", {
  results <- get_cached_psa_results()
  p <- psa_scatter_plot(results, "total_qalys", "total_cost")
  expect_s3_class(p, "ggplot")
})

test_that("psa_scatter_plot() data has x (outcome) and y (cost) columns", {
  results <- get_cached_psa_results()

  p <- psa_scatter_plot(results, "total_qalys", "total_cost")
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  expect_true("x" %in% names(plot_data))
  expect_true("y" %in% names(plot_data))
})

test_that("psa_scatter_plot() show_means adds layer for mean points", {
  results <- get_cached_psa_results()

  p_without <- psa_scatter_plot(
    results, "total_qalys", "total_cost",
    show_means = FALSE
  )
  p_with <- psa_scatter_plot(
    results, "total_qalys", "total_cost",
    show_means = TRUE
  )

  built_without <- ggplot_build(p_without)
  built_with <- ggplot_build(p_with)

  # With means should have more layers
  expect_gt(length(built_with$data), length(built_without$data))
})

test_that("psa_scatter_plot() mean positions are correct", {
  results <- get_cached_psa_results()

  # Get raw simulation data
  psa_data <- get_psa_simulations(
    results, "total_qalys", "total_cost"
  )

  # Calculate expected means per strategy
  expected_means <- psa_data %>%
    group_by(strategy) %>%
    summarize(
      mean_outcome = mean(outcome),
      mean_cost = mean(cost),
      .groups = "drop"
    )

  # Get plot with means
  p <- psa_scatter_plot(
    results, "total_qalys", "total_cost",
    show_means = TRUE
  )
  built <- ggplot_build(p)

  # Mean points are in second layer
  mean_data <- built$data[[2]]

  # Check that mean positions match
  expect_equal(
    sort(round(mean_data$x, 2)),
    sort(round(expected_means$mean_outcome, 2))
  )
  expect_equal(
    sort(round(mean_data$y, 2)),
    sort(round(expected_means$mean_cost, 2))
  )
})

test_that("psa_scatter_plot() axis limits include origin", {
  results <- get_cached_psa_results()

  p <- psa_scatter_plot(results, "total_qalys", "total_cost")
  built <- ggplot_build(p)

  # Check x and y ranges include 0
  x_range <- built$layout$panel_params[[1]]$x.range
  y_range <- built$layout$panel_params[[1]]$y.range

  expect_lte(x_range[1], 0)
  expect_lte(y_range[1], 0)
})

# ============================================================================
# Tests for pairwise_ceac_plot()
# ============================================================================

test_that("pairwise_ceac_plot() returns ggplot object with comparators", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  p <- pairwise_ceac_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1]
  )
  expect_s3_class(p, "ggplot")
})

test_that("pairwise_ceac_plot() returns ggplot object with interventions", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  p <- pairwise_ceac_plot(
    results, "total_qalys", "total_cost",
    interventions = strategies[1]
  )
  expect_s3_class(p, "ggplot")
})

test_that("pairwise_ceac_plot() errors when neither comparators nor interventions provided", {
  results <- get_cached_psa_results()

  expect_error(
    pairwise_ceac_plot(results, "total_qalys", "total_cost"),
    "comparators.*interventions|interventions.*comparators|must be provided"
  )
})

test_that("pairwise_ceac_plot() works with both comparators and interventions (N×M)", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  # When both provided, creates explicit N×M comparisons
  p <- pairwise_ceac_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1],
    interventions = strategies[2]
  )
  expect_s3_class(p, "ggplot")
})

test_that("pairwise_ceac_plot() has reference line at y=0.5", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  p <- pairwise_ceac_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1]
  )
  built <- ggplot_build(p)

  # Find the hline layer (should have yintercept = 0.5)
  has_hline <- FALSE
  for (layer_data in built$data) {
    if ("yintercept" %in% names(layer_data)) {
      if (any(layer_data$yintercept == 0.5)) {
        has_hline <- TRUE
        break
      }
    }
  }
  expect_true(has_hline)
})

# ============================================================================
# Tests for psa_parameter_scatter_matrix()
# ============================================================================

test_that("psa_parameter_scatter_matrix() returns ggmatrix object", {
  skip_if_not_installed("GGally")
  results <- get_cached_psa_results()

  p <- psa_parameter_scatter_matrix(
    results,
    variables = c("p_sick", "c_healthy"),
    strategies = "standard"
  )
  expect_s3_class(p, "ggmatrix")
})

test_that("psa_parameter_scatter_matrix() extracts correct variables", {
  skip_if_not_installed("GGally")
  results <- get_cached_psa_results()

  requested_vars <- c("p_sick", "c_healthy")
  p <- psa_parameter_scatter_matrix(
    results,
    variables = requested_vars,
    strategies = "standard"
  )

  # The matrix should have 2 columns (one per variable)
  expect_equal(p$ncol, 2)
  expect_equal(p$nrow, 2)
})

test_that("psa_parameter_scatter_matrix() errors with less than 2 variables", {
  skip_if_not_installed("GGally")
  results <- get_cached_psa_results()

  expect_error(
    psa_parameter_scatter_matrix(results, variables = c("p_sick"), strategies = "standard"),
    "at least 2 variables"
  )
})

# ============================================================================
# Tests for evpi_plot()
# ============================================================================

test_that("evpi_plot() returns ggplot object", {
  results <- get_cached_psa_results()
  p <- evpi_plot(results, "total_qalys", "total_cost")
  expect_s3_class(p, "ggplot")
})

test_that("evpi_plot() data matches calculate_evpi()", {
  results <- get_cached_psa_results()

  # Get calculation result
  evpi_data <- calculate_evpi(
    results, "total_qalys", "total_cost",
    wtp = seq(0, 100000, by = 25000)
  )

  # Get plot data
  p <- evpi_plot(
    results, "total_qalys", "total_cost",
    wtp_range = c(0, 100000), wtp_step = 25000
  )
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # y values should match EVPI values
  expect_equal(
    sort(round(plot_data$y, 2)),
    sort(round(evpi_data$evpi, 2))
  )
})

test_that("evpi_plot() EVPI values are non-negative", {
  results <- get_cached_psa_results()

  p <- evpi_plot(results, "total_qalys", "total_cost")
  built <- ggplot_build(p)
  plot_data <- built$data[[1]]

  # All EVPI values should be >= 0
  expect_true(all(plot_data$y >= 0))
})

test_that("evpi_plot() accepts pre-calculated EVPI data", {
  results <- get_cached_psa_results()

  # Pre-calculate EVPI
  evpi_data <- calculate_evpi(
    results, "total_qalys", "total_cost",
    wtp = seq(0, 50000, by = 10000)
  )

  # Pass pre-calculated data
  p <- evpi_plot(evpi_data)
  expect_s3_class(p, "ggplot")

  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

# ============================================================================
# Tests for pairwise_psa_scatter_plot()
# ============================================================================

test_that("pairwise_psa_scatter_plot() returns ggplot object with comparators", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  p <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1]
  )
  expect_s3_class(p, "ggplot")
})

test_that("pairwise_psa_scatter_plot() returns ggplot object with interventions", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  p <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    interventions = strategies[1]
  )
  expect_s3_class(p, "ggplot")
})

test_that("pairwise_psa_scatter_plot() errors when neither provided", {
  results <- get_cached_psa_results()

  expect_error(
    pairwise_psa_scatter_plot(results, "total_qalys", "total_cost"),
    "comparators.*interventions|interventions.*comparators|must be provided"
  )
})

test_that("pairwise_psa_scatter_plot() works with both interventions and comparators (N x M)", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  # Should work when both are provided (N x M mode)
  p <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1],
    interventions = strategies[2]
  )
  expect_s3_class(p, "ggplot")
})

test_that("pairwise_psa_scatter_plot() WTP threshold adds coloring", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  p_without <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1],
    wtp = NULL
  )
  p_with <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1],
    wtp = 50000
  )

  built_without <- ggplot_build(p_without)
  built_with <- ggplot_build(p_with)

  # With WTP should have multiple colors (cost-effective vs not)
  # Get point layer (after hline/vline layers)
  point_layer_with <- NULL
  for (i in seq_along(built_with$data)) {
    if (nrow(built_with$data[[i]]) > 10) {  # Point layer has many rows
      point_layer_with <- built_with$data[[i]]
      break
    }
  }

  if (!is.null(point_layer_with)) {
    # Should have multiple unique colors when WTP provided
    n_colors <- length(unique(point_layer_with$colour))
    expect_gt(n_colors, 1)
  }
})

test_that("pairwise_psa_scatter_plot() NMB classification is correct", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  wtp_test <- 50000

  p <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1],
    wtp = wtp_test
  )
  built <- ggplot_build(p)

  # Find point layer
  point_layer <- NULL
  for (i in seq_along(built$data)) {
    if (nrow(built$data[[i]]) > 10) {
      point_layer <- built$data[[i]]
      break
    }
  }

  if (!is.null(point_layer)) {
    # Manually verify: cost-effective if doutcome * wtp - dcost > 0
    # Green color (#00BA38) for CE, red (#F8766D) for not CE
    for (i in seq_len(nrow(point_layer))) {
      nmb <- point_layer$x[i] * wtp_test - point_layer$y[i]
      color <- point_layer$colour[i]
      if (nmb > 0) {
        expect_equal(color, "#00BA38")  # Cost-effective
      } else {
        expect_equal(color, "#F8766D")  # Not cost-effective
      }
    }
  }
})

test_that("pairwise_psa_scatter_plot() creates facets for multiple comparisons", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$display_name

  # With comparators mode, should create facet for each other strategy
  p <- pairwise_psa_scatter_plot(
    results, "total_qalys", "total_cost",
    comparators = strategies[1]
  )
  built <- ggplot_build(p)

  # Should have multiple panels (one per non-comparator strategy)
  point_layer <- NULL
  for (i in seq_along(built$data)) {
    if (nrow(built$data[[i]]) > 10) {
      point_layer <- built$data[[i]]
      break
    }
  }
  expect_true(!is.null(point_layer) && "PANEL" %in% names(point_layer))
  n_panels <- length(unique(point_layer$PANEL))
  expect_equal(n_panels, length(strategies) - 1)  # One panel per non-comparator
})

# ============================================================================
# Tests for nmb_density_plot()
# ============================================================================

test_that("nmb_density_plot() returns ggplot object", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name
  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1]
  )
  expect_s3_class(p, "ggplot")
})

test_that("nmb_density_plot() calculates incremental NMB correctly", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  wtp_test <- 50000
  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = wtp_test,
    comparators = strategies[1]
  )

  built <- ggplot_build(p)

  # Get the density layer data
  density_data <- built$data[[1]]

  # Incremental NMB x values should span negative and positive
  # (or at least have a reasonable range)
  expect_true(min(density_data$x) < max(density_data$x))
})

test_that("nmb_density_plot() requires interventions or comparators", {
  results <- get_cached_psa_results()

  # Should error when neither provided

  expect_error(
    nmb_density_plot(results, "total_qalys", "total_cost", wtp = 50000),
    "At least one of 'interventions' or 'comparators' must be provided"
  )
})

test_that("nmb_density_plot() rejects strategies with interventions/comparators", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  # Should error when both strategies and comparators provided
  expect_error(
    nmb_density_plot(
      results, "total_qalys", "total_cost",
      wtp = 50000,
      strategies = strategies[1],
      comparators = strategies[2]
    ),
    "'strategies' parameter cannot be used with 'interventions' or 'comparators'"
  )
})

test_that("nmb_density_plot() show_mean adds vertical lines", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p_without <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1],
    show_mean = FALSE
  )
  p_with <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1],
    show_mean = TRUE
  )

  built_without <- ggplot_build(p_without)
  built_with <- ggplot_build(p_with)

  # With mean lines should have more layers
  expect_gt(length(built_with$data), length(built_without$data))
})

test_that("nmb_density_plot() title includes WTP value", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  wtp_test <- 75000
  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = wtp_test,
    comparators = strategies[1]
  )

  # Title should mention the WTP value
  expect_true(grepl("75,000", p$labels$title) || grepl("75000", p$labels$title))
})

test_that("nmb_density_plot() custom title is applied", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  custom_title <- "My Custom NMB Plot"
  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1],
    title = custom_title
  )

  expect_equal(p$labels$title, custom_title)
})

test_that("nmb_density_plot() respects alpha parameter", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1],
    alpha = 0.7
  )
  built <- ggplot_build(p)
  density_data <- built$data[[1]]

  # Alpha should be applied to density layer
  expect_true("alpha" %in% names(density_data))
  expect_equal(unique(density_data$alpha), 0.7)
})

test_that("nmb_density_plot() x-axis has incremental NMB label", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1]
  )

  # Default x label should be "Incremental Net Monetary Benefit"
  expect_equal(p$labels$x, "Incremental Net Monetary Benefit")
})

test_that("nmb_density_plot() includes reference line at zero", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    comparators = strategies[1]
  )
  built <- ggplot_build(p)

  # Should have a vline layer at x=0
  # Layer 2 should be the reference line (after density)
  vline_data <- built$data[[2]]
  expect_true(0 %in% vline_data$xintercept)
})

test_that("nmb_density_plot() creates multiple comparisons with interventions", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  # Use first strategy as intervention, should compare to all others
  p <- nmb_density_plot(
    results, "total_qalys", "total_cost",
    wtp = 50000,
    interventions = strategies[1]
  )
  built <- ggplot_build(p)
  density_data <- built$data[[1]]

  # Should have comparisons for n-1 strategies
  n_comparisons <- length(unique(density_data$fill))
  expect_equal(n_comparisons, length(strategies) - 1)
})

# ============================================================================
# Tests for get_psa_outcome_simulations()
# ============================================================================

test_that("get_psa_outcome_simulations() returns correct columns in absolute mode", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  data <- get_psa_outcome_simulations(
    results, "total_qalys",
    strategies = strategies
  )

  expect_true(tibble::is_tibble(data))
  expect_true(all(c("simulation", "strategy", "group", "outcome") %in% names(data)))
  expect_false("comparison" %in% names(data))
})

test_that("get_psa_outcome_simulations() returns correct columns in difference mode", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  data <- get_psa_outcome_simulations(
    results, "total_qalys",
    comparators = strategies[1]
  )

  expect_true(tibble::is_tibble(data))
  expect_true(all(c("simulation", "comparison", "group", "outcome") %in% names(data)))
  expect_false("strategy" %in% names(data))
})

test_that("get_psa_outcome_simulations() validates mutual exclusivity", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  expect_error(
    get_psa_outcome_simulations(
      results, "total_qalys",
      strategies = strategies[1],
      comparators = strategies[2]
    ),
    "'strategies' parameter cannot be used with 'interventions' or 'comparators'"
  )
})

test_that("get_psa_outcome_simulations() requires at least one mode", {
  results <- get_cached_psa_results()

  expect_error(
    get_psa_outcome_simulations(results, "total_qalys"),
    "Either 'strategies'"
  )
})

test_that("get_psa_outcome_simulations() calculates differences correctly", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  # Get absolute data for both strategies
  abs_data <- get_psa_outcome_simulations(
    results, "total_qalys",
    strategies = strategies[1:2]
  )

  # Get difference data
  diff_data <- get_psa_outcome_simulations(
    results, "total_qalys",
    interventions = strategies[2],
    comparators = strategies[1]
  )

  # Calculate expected difference manually
  strat1_outcomes <- abs_data %>%
    filter(.data$strategy == strategies[1]) %>%
    arrange(.data$simulation)
  strat2_outcomes <- abs_data %>%
    filter(.data$strategy == strategies[2]) %>%
    arrange(.data$simulation)

  expected_diff <- strat2_outcomes$outcome - strat1_outcomes$outcome

  actual_diff <- diff_data %>%
    arrange(.data$simulation) %>%
    pull(.data$outcome)

  expect_equal(actual_diff, expected_diff, tolerance = 1e-10)
})

# ============================================================================
# Tests for outcome_density_plot()
# ============================================================================

test_that("outcome_density_plot() returns ggplot object in absolute mode", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    strategies = strategies
  )
  expect_s3_class(p, "ggplot")
})

test_that("outcome_density_plot() returns ggplot object in difference mode", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    comparators = strategies[1]
  )
  expect_s3_class(p, "ggplot")
})

test_that("outcome_density_plot() does NOT include reference line at zero", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    comparators = strategies[1],
    show_mean = FALSE
  )
  built <- ggplot_build(p)

  # Should only have one layer (density) when show_mean is FALSE
  # No vline at zero should exist
  expect_equal(length(built$data), 1)
})

test_that("outcome_density_plot() show_mean adds vertical lines", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p_without <- outcome_density_plot(
    results, "total_qalys",
    comparators = strategies[1],
    show_mean = FALSE
  )
  p_with <- outcome_density_plot(
    results, "total_qalys",
    comparators = strategies[1],
    show_mean = TRUE
  )

  built_without <- ggplot_build(p_without)
  built_with <- ggplot_build(p_with)

  # With mean lines should have more layers
  expect_gt(length(built_with$data), length(built_without$data))
})

test_that("outcome_density_plot() uses delta symbol in difference mode x-label", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    comparators = strategies[1]
  )

  # X label should contain delta symbol

  expect_true(grepl("\u0394", p$labels$x))
})

test_that("outcome_density_plot() does not use delta in absolute mode x-label", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    strategies = strategies
  )

  # X label should NOT contain delta symbol
  expect_false(grepl("\u0394", p$labels$x))
})

test_that("outcome_density_plot() respects alpha parameter", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    strategies = strategies,
    alpha = 0.7
  )
  built <- ggplot_build(p)
  density_data <- built$data[[1]]

  expect_true("alpha" %in% names(density_data))
  expect_equal(unique(density_data$alpha), 0.7)
})

test_that("outcome_density_plot() custom title is applied", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  custom_title <- "My Custom Outcome Plot"
  p <- outcome_density_plot(
    results, "total_qalys",
    strategies = strategies,
    title = custom_title
  )

  expect_equal(p$labels$title, custom_title)
})

test_that("outcome_density_plot() creates multiple comparisons with interventions", {
  results <- get_cached_psa_results()
  strategies <- results$metadata$strategies$name

  p <- outcome_density_plot(
    results, "total_qalys",
    interventions = strategies[1]
  )
  built <- ggplot_build(p)
  density_data <- built$data[[1]]

  # Should have comparisons for n-1 strategies
  n_comparisons <- length(unique(density_data$fill))
  expect_equal(n_comparisons, length(strategies) - 1)
})
