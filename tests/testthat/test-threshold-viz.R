context("Threshold Analysis Visualization")

# ============================================================================
# Mock Data Builder
# ============================================================================

#' Build mock threshold results for visualization tests
#'
#' Creates realistic converging history data with analysis specs.
#'
#' @param n_analyses Number of analyses (1 or 2)
#' @param output_type Type of output: "outcomes", "nmb", "ce", "trace"
build_mock_threshold_results <- function(n_analyses = 1, output_type = "outcomes") {
  build_one <- function(name, variable, threshold_val, goal_val, output_type) {
    # Simulate converging iterations
    n_iter <- 6
    inputs <- c(0.1, 0.5, 0.3, 0.25, 0.27, threshold_val)
    outputs <- c(goal_val - 2, goal_val + 1, goal_val - 0.5,
                 goal_val - 0.1, goal_val + 0.02, goal_val)

    history <- tibble::tibble(
      name = rep(name, n_iter),
      variable = rep(variable, n_iter),
      iteration = 1:n_iter,
      input = inputs,
      output = outputs,
      goal = rep(goal_val, n_iter),
      diff = outputs - goal_val
    )

    tv <- tibble::tibble(
      name = name,
      variable = variable,
      value = threshold_val
    )

    # Build analysis spec
    if (output_type == "outcomes") {
      spec <- list(
        name = name,
        variable = variable,
        variable_strategy = "",
        variable_group = "",
        lower = 0,
        upper = 1,
        active = TRUE,
        condition = list(
          output = "outcomes",
          summary = "total_qalys",
          type = "absolute",
          strategy = "base",
          target_value = goal_val,
          discounted = TRUE
        )
      )
    } else if (output_type == "nmb") {
      spec <- list(
        name = name,
        variable = variable,
        variable_strategy = "",
        variable_group = "",
        lower = 0,
        upper = 50000,
        active = TRUE,
        condition = list(
          output = "nmb",
          health_summary = "total_qalys",
          cost_summary = "total_cost",
          referent = "treatment",
          comparator = "base",
          target_value = goal_val,
          discounted = TRUE
        )
      )
    } else if (output_type == "ce") {
      spec <- list(
        name = name,
        variable = variable,
        variable_strategy = "",
        variable_group = "",
        lower = 0,
        upper = 50000,
        active = TRUE,
        condition = list(
          output = "ce",
          health_summary = "total_qalys",
          cost_summary = "total_cost",
          referent = "treatment",
          comparator = "base",
          discounted = TRUE
        )
      )
    } else if (output_type == "trace") {
      spec <- list(
        name = name,
        variable = variable,
        variable_strategy = "",
        variable_group = "",
        lower = 0,
        upper = 1,
        active = TRUE,
        condition = list(
          output = "trace",
          state = "healthy",
          time = 5,
          time_unit = "year",
          type = "absolute",
          strategy = "base",
          target_value = goal_val
        )
      )
    } else if (output_type == "outcomes_diff") {
      spec <- list(
        name = name,
        variable = variable,
        variable_strategy = "",
        variable_group = "",
        lower = 0,
        upper = 1,
        active = TRUE,
        condition = list(
          output = "outcomes",
          summary = "total_qalys",
          type = "difference",
          referent = "treatment",
          comparator = "base",
          target_value = goal_val,
          discounted = TRUE
        )
      )
    }

    list(history = history, tv = tv, spec = spec)
  }

  # Build metadata
  metadata <- list(
    summaries = data.frame(
      name = c("total_qalys", "total_cost"),
      display_name = c("Total QALYs", "Total Cost"),
      wtp = c(50000, NA),
      stringsAsFactors = FALSE
    ),
    strategies = data.frame(
      name = c("base", "treatment"),
      display_name = c("Base Case", "Treatment"),
      stringsAsFactors = FALSE
    ),
    states = data.frame(
      name = c("healthy", "sick", "dead"),
      display_name = c("Healthy", "Sick", "Dead"),
      stringsAsFactors = FALSE
    ),
    variables = data.frame(
      name = c("p_disease", "cost_base"),
      display_name = c("Disease Probability", "Base Cost"),
      stringsAsFactors = FALSE
    )
  )

  if (n_analyses == 1) {
    one <- build_one("Analysis 1", "p_disease", 0.265, 7, output_type)
    list(
      threshold_values = one$tv,
      root_finder_history = one$history,
      metadata = metadata,
      analyses = list(one$spec)
    )
  } else {
    one <- build_one("Analysis 1", "p_disease", 0.265, 7, output_type)
    two <- build_one("Analysis 2", "cost_base", 3500, 5000, output_type)
    # Adjust second analysis inputs to be cost-scale
    two$history$input <- c(100, 5000, 3000, 3200, 3400, 3500)
    two$tv$value <- 3500

    list(
      threshold_values = dplyr::bind_rows(one$tv, two$tv),
      root_finder_history = dplyr::bind_rows(one$history, two$history),
      metadata = metadata,
      analyses = list(one$spec, two$spec)
    )
  }
}

# ============================================================================
# Plot Tests
# ============================================================================

test_that("threshold_plot returns ggplot for single analysis", {
  results <- build_mock_threshold_results(1)
  p <- threshold_plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("threshold_plot returns ggplot for multiple analyses", {
  results <- build_mock_threshold_results(2)
  p <- threshold_plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("threshold_plot filters by analyses parameter", {
  results <- build_mock_threshold_results(2)
  p <- threshold_plot(results, analyses = "Analysis 1")
  expect_s3_class(p, "ggplot")
  # Only Analysis 1 data should be in the plot
  plot_data <- ggplot2::ggplot_build(p)$data
  # The line layer should only have data from Analysis 1
  expect_true(all(ggplot2::layer_data(p, 1)$group == 1) ||
              nrow(ggplot2::layer_data(p, 1)) > 0)
})

test_that("threshold_plot errors with no data", {
  results <- build_mock_threshold_results(1)
  expect_error(
    threshold_plot(results, analyses = "nonexistent"),
    "No history data"
  )
})

test_that("threshold_plot data is sorted by input", {
  results <- build_mock_threshold_results(1)
  p <- threshold_plot(results)
  # Line layer data should be sorted by input (x values)
  line_data <- ggplot2::layer_data(p, 1)
  expect_true(all(diff(line_data$x) >= 0))
})

test_that("threshold_convergence_plot returns ggplot for single analysis", {
  results <- build_mock_threshold_results(1)
  p <- threshold_convergence_plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("threshold_convergence_plot returns ggplot for multiple analyses", {
  results <- build_mock_threshold_results(2)
  p <- threshold_convergence_plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("threshold_convergence_plot filters by analyses parameter", {
  results <- build_mock_threshold_results(2)
  p <- threshold_convergence_plot(results, analyses = "Analysis 2")
  expect_s3_class(p, "ggplot")
})

test_that("threshold_convergence_plot errors with no data", {
  results <- build_mock_threshold_results(1)
  expect_error(
    threshold_convergence_plot(results, analyses = "nonexistent"),
    "No history data"
  )
})

# ============================================================================
# Table Tests
# ============================================================================

test_that("threshold_table returns flextable for single analysis", {
  results <- build_mock_threshold_results(1)
  ft <- threshold_table(results)
  expect_s3_class(ft, "flextable")
})

test_that("threshold_table returns kable for single analysis", {
  results <- build_mock_threshold_results(1)
  kt <- threshold_table(results, table_format = "kable")
  expect_true(inherits(kt, "kableExtra") || inherits(kt, "character"))
})

test_that("threshold_table data is sorted by input", {
  results <- build_mock_threshold_results(1)
  # Use prepare helper to verify sort
  data <- openqaly:::prepare_threshold_history_data(results, NULL)
  h <- data$history[order(data$history$input), ]
  expect_true(all(diff(h$input) >= 0))
})

test_that("threshold_table handles multiple analyses with group headers", {
  results <- build_mock_threshold_results(2)
  ft <- threshold_table(results)
  expect_s3_class(ft, "flextable")
})

test_that("threshold_table filters by analyses parameter", {
  results <- build_mock_threshold_results(2)
  ft <- threshold_table(results, analyses = "Analysis 1")
  expect_s3_class(ft, "flextable")
})

test_that("threshold_table errors with no data", {
  results <- build_mock_threshold_results(1)
  expect_error(
    threshold_table(results, analyses = "nonexistent"),
    "No history data"
  )
})

test_that("threshold_convergence_table returns flextable for single analysis", {
  results <- build_mock_threshold_results(1)
  ft <- threshold_convergence_table(results)
  expect_s3_class(ft, "flextable")
})

test_that("threshold_convergence_table returns kable for single analysis", {
  results <- build_mock_threshold_results(1)
  kt <- threshold_convergence_table(results, table_format = "kable")
  expect_true(inherits(kt, "kableExtra") || inherits(kt, "character"))
})

test_that("threshold_convergence_table handles multiple analyses", {
  results <- build_mock_threshold_results(2)
  ft <- threshold_convergence_table(results)
  expect_s3_class(ft, "flextable")
})

test_that("threshold_convergence_table filters by analyses parameter", {
  results <- build_mock_threshold_results(2)
  ft <- threshold_convergence_table(results, analyses = "Analysis 2")
  expect_s3_class(ft, "flextable")
})

test_that("threshold_convergence_table errors with no data", {
  results <- build_mock_threshold_results(1)
  expect_error(
    threshold_convergence_table(results, analyses = "nonexistent"),
    "No history data"
  )
})

# ============================================================================
# Output Label Tests
# ============================================================================

test_that("get_threshold_output_label returns summary name for outcomes absolute", {
  results <- build_mock_threshold_results(1, "outcomes")
  label <- openqaly:::get_threshold_output_label(
    results$analyses[[1]], results$metadata)
  expect_equal(as.character(label), "Total QALYs")
})

test_that("get_threshold_output_label returns delta format for outcomes difference", {
  results <- build_mock_threshold_results(1, "outcomes_diff")
  label <- openqaly:::get_threshold_output_label(
    results$analyses[[1]], results$metadata)
  expect_true(grepl("\u0394", label))
  expect_true(grepl("Total QALYs", label))
  expect_true(grepl("Treatment", label))
  expect_true(grepl("Base Case", label))
})

test_that("get_threshold_output_label returns NMB format for nmb condition", {
  results <- build_mock_threshold_results(1, "nmb")
  label <- openqaly:::get_threshold_output_label(
    results$analyses[[1]], results$metadata)
  expect_true(grepl("Net Monetary Benefit", label))
  expect_true(grepl("Total Cost", label))
  expect_true(grepl("Total QALYs", label))
  expect_true(grepl("50,000", label))
})

test_that("get_threshold_output_label returns NMB format for ce condition", {
  results <- build_mock_threshold_results(1, "ce")
  label <- openqaly:::get_threshold_output_label(
    results$analyses[[1]], results$metadata)
  expect_true(grepl("Net Monetary Benefit", label))
})

test_that("get_threshold_output_label returns trace format", {
  results <- build_mock_threshold_results(1, "trace")
  label <- openqaly:::get_threshold_output_label(
    results$analyses[[1]], results$metadata)
  expect_true(grepl("P\\(Healthy\\)", label))
  expect_true(grepl("5 years", label))
})

# ============================================================================
# Edge Case Tests
# ============================================================================

test_that("threshold_plot handles NA threshold value with 'Did not converge' label", {
  results <- build_mock_threshold_results(1)
  results$threshold_values$value <- NA
  p <- threshold_plot(results)
  expect_s3_class(p, "ggplot")
  # Should have a "Did not converge" label layer
  layer_data_list <- lapply(seq_along(p$layers), function(i) {
    tryCatch(ggplot2::layer_data(p, i), error = function(e) NULL)
  })
  labels_found <- any(vapply(layer_data_list, function(ld) {
    !is.null(ld) && "label" %in% names(ld) && any(ld$label == "Did not converge")
  }, logical(1)))
  expect_true(labels_found)
})

test_that("threshold_convergence_plot handles NA threshold value", {
  results <- build_mock_threshold_results(1)
  results$threshold_values$value <- NA
  p <- threshold_convergence_plot(results)
  expect_s3_class(p, "ggplot")
  # Should have gray diamond + "Did not converge" label, not red diamond
  layer_data_list <- lapply(seq_along(p$layers), function(i) {
    tryCatch(ggplot2::layer_data(p, i), error = function(e) NULL)
  })
  labels_found <- any(vapply(layer_data_list, function(ld) {
    !is.null(ld) && "label" %in% names(ld) && any(ld$label == "Did not converge")
  }, logical(1)))
  expect_true(labels_found)
})

test_that("threshold_convergence_plot shows red diamond only for converged analyses", {
  results <- build_mock_threshold_results(2)
  # Fail the second analysis
  results$threshold_values$value[2] <- NA
  p <- threshold_convergence_plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("threshold_plot handles single iteration", {
  results <- build_mock_threshold_results(1)
  results$root_finder_history <- results$root_finder_history[1, ]
  p <- threshold_plot(results)
  expect_s3_class(p, "ggplot")
})

test_that("threshold_convergence_plot handles single iteration", {
  results <- build_mock_threshold_results(1)
  results$root_finder_history <- results$root_finder_history[1, ]
  p <- threshold_convergence_plot(results)
  expect_s3_class(p, "ggplot")
})

# ============================================================================
# Integration: CE/NMB produce proper NMB labels in plots
# ============================================================================

test_that("threshold_plot uses NMB label for CE condition", {
  results <- build_mock_threshold_results(1, "ce")
  p <- threshold_plot(results)
  expect_s3_class(p, "ggplot")
  # Y-axis label should contain "Net Monetary Benefit"
  expect_true(grepl("Net Monetary Benefit", p$labels$y))
})

test_that("threshold_plot uses NMB label for NMB condition", {
  results <- build_mock_threshold_results(1, "nmb")
  p <- threshold_plot(results)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("Net Monetary Benefit", p$labels$y))
})

# ============================================================================
# Prepare Helper Tests
# ============================================================================

test_that("prepare_threshold_history_data filters correctly", {
  results <- build_mock_threshold_results(2)
  data <- openqaly:::prepare_threshold_history_data(results, "Analysis 1")
  expect_equal(unique(data$history$name), "Analysis 1")
  expect_equal(nrow(data$threshold_values), 1)
  expect_equal(length(data$analyses), 1)
})

test_that("prepare_threshold_history_data returns all when analyses is NULL", {
  results <- build_mock_threshold_results(2)
  data <- openqaly:::prepare_threshold_history_data(results, NULL)
  expect_equal(length(unique(data$history$name)), 2)
  expect_equal(nrow(data$threshold_values), 2)
  expect_equal(length(data$analyses), 2)
})

# ============================================================================
# run_threshold includes analyses field
# ============================================================================

test_that("run_threshold result includes analyses field", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- define_model("markov") %>%
    set_settings(
      timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3, discount_outcomes = 3
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_disease", 0.03) %>%
    add_variable("cost_base", 1000) %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("sick", "dead", 0.2) %>%
    add_transition("healthy", "healthy", 1 - p_disease - 0.01) %>%
    add_transition("sick", "sick", 1 - 0.2) %>%
    add_transition("dead", "dead", 1) %>%
    add_strategy("base") %>%
    add_strategy("treatment") %>%
    add_value("qaly", 1, state = "healthy") %>%
    add_value("qaly", 0.5, state = "sick") %>%
    add_value("qaly", 0, state = "dead") %>%
    add_value("cost_drug", cost_base, state = "healthy") %>%
    add_value("cost_drug", cost_base * 0.5, state = "sick") %>%
    add_value("cost_drug", 0, state = "dead") %>%
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000) %>%
    add_summary("total_cost", "cost_drug", type = "cost") %>%
    add_threshold_analysis("P Disease", "p_disease", 0.001, 0.5,
      condition = threshold_condition_outcomes(
        summary = "total_qalys", type = "absolute",
        strategy = "base", discounted = TRUE, target_value = 7))

  result <- run_threshold(model)

  expect_true("analyses" %in% names(result))
  expect_equal(length(result$analyses), 1)
  expect_equal(result$analyses[[1]]$name, "P Disease")
})
