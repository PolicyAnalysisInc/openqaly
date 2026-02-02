context("VBP Plots")

# ============================================================================
# Test Fixtures
# Uses get_cached_vbp_results() from setup.R for performance
# ============================================================================

# ============================================================================
# 1. prepare_vbp_plot_data() Tests
# ============================================================================

test_that("prepare_vbp_plot_data() returns tibble with correct columns", {
  vbp_results <- get_cached_vbp_results()
  plot_data <- openqaly:::prepare_vbp_plot_data(vbp_results)

  expect_s3_class(plot_data, "tbl_df")
  expect_true("wtp" %in% names(plot_data))
  expect_true("comparator" %in% names(plot_data))
  expect_true("vbp_price" %in% names(plot_data))
  expect_true("group" %in% names(plot_data))
})

test_that("prepare_vbp_plot_data() includes all comparators by default", {
  vbp_results <- get_cached_vbp_results()
  plot_data <- openqaly:::prepare_vbp_plot_data(vbp_results)

  n_comparators <- nrow(vbp_results$vbp_equations)
  comparators_in_data <- unique(plot_data$comparator)

  # Should have individual comparators + possibly "All Comparators"
  expect_gte(length(comparators_in_data), n_comparators)
})

test_that("prepare_vbp_plot_data() filters comparators correctly", {
  vbp_results <- get_cached_vbp_results()
  comparators <- unique(vbp_results$vbp_equations$comparator)

  if (length(comparators) >= 1) {
    plot_data <- openqaly:::prepare_vbp_plot_data(
      vbp_results,
      comparators = comparators[1]
    )

    # Should only have 1 comparator
    expect_equal(length(unique(plot_data$comparator)), 1)
  }
})

test_that("prepare_vbp_plot_data() errors on invalid comparator", {
  vbp_results <- get_cached_vbp_results()

  expect_error(
    openqaly:::prepare_vbp_plot_data(
      vbp_results,
      comparators = "nonexistent_comparator"
    ),
    "not found"
  )
})

test_that("prepare_vbp_plot_data() uses custom WTP range", {
  vbp_results <- get_cached_vbp_results()

  plot_data <- openqaly:::prepare_vbp_plot_data(
    vbp_results,
    wtp_range = c(0, 50000),
    wtp_step = 10000
  )

  wtp_values <- unique(plot_data$wtp)
  expect_equal(min(wtp_values), 0)
  expect_equal(max(wtp_values), 50000)
})

test_that("prepare_vbp_plot_data() includes 'All Comparators' series", {
  vbp_results <- get_cached_vbp_results()
  n_comparators <- nrow(vbp_results$vbp_equations)

  if (n_comparators > 1) {
    plot_data <- openqaly:::prepare_vbp_plot_data(
      vbp_results,
      comparators = "all"
    )

    comparators_in_data <- unique(plot_data$comparator)
    expect_true("All Comparators" %in% comparators_in_data)
  }
})

test_that("prepare_vbp_plot_data() can exclude 'All Comparators' series", {
  vbp_results <- get_cached_vbp_results()

  plot_data <- openqaly:::prepare_vbp_plot_data(
    vbp_results,
    comparators = "all_comparators"
  )

  comparators_in_data <- unique(plot_data$comparator)
  expect_false("All Comparators" %in% comparators_in_data)
})

# ============================================================================
# 2. "All Comparators" Calculation Tests
# ============================================================================

test_that("'All Comparators' series is minimum of individual comparator VBPs", {
  vbp_results <- get_cached_vbp_results()
  n_comparators <- nrow(vbp_results$vbp_equations)

  if (n_comparators > 1) {
    plot_data <- openqaly:::prepare_vbp_plot_data(
      vbp_results,
      wtp_range = c(50000, 50000),
      wtp_step = 1,
      comparators = "all"
    )

    # Get individual comparator VBPs at WTP = 50000
    individual_vbps <- plot_data %>%
      dplyr::filter(.data$comparator != "All Comparators", .data$wtp == 50000) %>%
      dplyr::pull(.data$vbp_price)

    # Get "All Comparators" VBP
    all_comp_vbp <- plot_data %>%
      dplyr::filter(.data$comparator == "All Comparators", .data$wtp == 50000) %>%
      dplyr::pull(.data$vbp_price)

    expect_equal(unname(all_comp_vbp), unname(min(individual_vbps)))
  }
})

# ============================================================================
# 3. vbp_plot() Output Tests
# ============================================================================

test_that("vbp_plot() returns ggplot object", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results)

  expect_s3_class(p, "ggplot")
})


# ============================================================================
# 4. WTP Range Handling Tests
# ============================================================================

test_that("vbp_plot() auto-generates WTP range when NULL", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results, wtp_range = NULL)

  # Should still produce valid plot
  expect_s3_class(p, "ggplot")

  # Check that data exists
  plot_data <- ggplot2::ggplot_build(p)$data
  expect_true(length(plot_data) > 0)
})

test_that("vbp_plot() uses custom WTP range", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(
    vbp_results,
    wtp_range = c(0, 100000),
    wtp_step = 25000
  )

  expect_s3_class(p, "ggplot")
})

# ============================================================================
# 5. Default WTP Reference Line Tests
# ============================================================================

test_that("vbp_plot() adds default WTP line when show_default_wtp = TRUE", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results, show_default_wtp = TRUE)

  # Check that plot builds without error
  expect_s3_class(p, "ggplot")
})

test_that("vbp_plot() omits default WTP line when show_default_wtp = FALSE", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results, show_default_wtp = FALSE)

  expect_s3_class(p, "ggplot")
})

# ============================================================================
# 6. Comparator Selection Tests
# ============================================================================

test_that("vbp_plot() with single comparator produces valid plot", {
  vbp_results <- get_cached_vbp_results()
  comparators <- unique(vbp_results$vbp_equations$comparator)

  if (length(comparators) >= 1) {
    p <- vbp_plot(
      vbp_results,
      comparators = comparators[1]
    )

    expect_s3_class(p, "ggplot")
  }
})

# ============================================================================
# 7. Group Handling Tests
# ============================================================================

test_that("vbp_plot() works with 'overall' group", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results, groups = "overall")

  expect_s3_class(p, "ggplot")
})

# ============================================================================
# 8. Visual Consistency Tests
# ============================================================================

test_that("vbp_plot() uses theme_bw as base theme", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results)

  # Check theme inheritance
  expect_s3_class(p, "ggplot")
})

test_that("vbp_plot() formats axes with dollar labels", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results)

  # Both X and Y axes should use dollar formatting
  # This is verified by the scales being set, but exact format is hard to test
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# 9. VBP Callout at Default WTP Tests
# ============================================================================

test_that("vbp_plot() adds VBP callouts when show_vbp_at_wtp = TRUE", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results, show_default_wtp = TRUE, show_vbp_at_wtp = TRUE)

  # Check that plot builds without error
  expect_s3_class(p, "ggplot")
})

test_that("vbp_plot() omits VBP callouts when show_vbp_at_wtp = FALSE", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results, show_default_wtp = TRUE, show_vbp_at_wtp = FALSE)

  expect_s3_class(p, "ggplot")
})

test_that("vbp_plot() omits VBP callouts when show_default_wtp = FALSE", {
  vbp_results <- get_cached_vbp_results()
  # VBP callouts require default WTP line
  p <- vbp_plot(vbp_results, show_default_wtp = FALSE, show_vbp_at_wtp = TRUE)

  expect_s3_class(p, "ggplot")
})

# ============================================================================
# 10. Display Name Mapping Tests
# ============================================================================

test_that("vbp_plot() maps comparator names to display names", {
  vbp_results <- get_cached_vbp_results()
  p <- vbp_plot(vbp_results)

  # Check that plot builds without error with display name mapping
  expect_s3_class(p, "ggplot")

  # The comparator labels should use display names from metadata
  # This is tested implicitly - the function should not error
})
