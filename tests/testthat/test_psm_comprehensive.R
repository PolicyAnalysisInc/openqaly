# Comprehensive PSM Model Tests
# Tests the example_psm model for mathematical correctness

library(testthat)
library(heRomod2)

test_that("example_psm model loads correctly", {
  # Load the model
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)

  # Verify basic structure
  expect_s3_class(model, "heRomodel")
  expect_equal(tolower(model$settings$model_type), "psm")

  # Verify exactly 3 states
  expect_equal(nrow(model$states), 3)
  expect_setequal(model$states$name, c("progression_free", "progressed", "dead"))

  # Verify exactly 2 strategies
  expect_equal(nrow(model$strategies), 2)
  expect_setequal(model$strategies$name, c("standard", "new_drug"))

  # Verify transitions have PFS and OS endpoints
  expect_true("PFS" %in% toupper(model$transitions$endpoint))
  expect_true("OS" %in% toupper(model$transitions$endpoint))
})

test_that("Strategy-specific variables evaluate correctly", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)

  # Check that variables exist with strategy column populated
  expect_true("c_drug" %in% model$variables$name)
  expect_true("c_admin" %in% model$variables$name)
  expect_true("c_ae" %in% model$variables$name)

  # Verify c_drug has two rows (one per strategy)
  c_drug_rows <- model$variables[model$variables$name == "c_drug", ]
  expect_equal(nrow(c_drug_rows), 2)
  expect_setequal(c_drug_rows$strategy, c("standard", "new_drug"))

  # Verify formulas are simple values (not vswitch)
  expect_false(any(grepl("vswitch", model$variables$formula)))

  # Verify expected values
  standard_drug <- c_drug_rows[c_drug_rows$strategy == "standard", ]$formula
  new_drug_drug <- c_drug_rows[c_drug_rows$strategy == "new_drug", ]$formula
  expect_equal(standard_drug, "5000")
  expect_equal(new_drug_drug, "12000")
})

test_that("example_psm model executes successfully", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)

  # Run the model
  results <- run_model(model)

  # Verify results structure
  expect_true("segments" %in% names(results))
  expect_true("aggregated" %in% names(results))

  # Verify both strategies ran
  expect_equal(nrow(results$aggregated), 2)
  expect_setequal(results$aggregated$strategy, c("standard", "new_drug"))

  # Verify traces exist
  expect_true(!is.null(results$aggregated$collapsed_trace[[1]]))
  expect_true(!is.null(results$aggregated$collapsed_trace[[2]]))
})

test_that("PSM trace has correct structure and dimensions", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get trace for standard strategy
  standard_trace <- results$aggregated$collapsed_trace[[which(results$aggregated$strategy == "standard")]]
  state_trace <- get_state_columns(standard_trace)

  # Verify dimensions
  # Calculate expected n_cycles from settings
  # For example_psm: 10 years / (1 month cycle) = 120 cycles
  n_cycles <- ceiling((model$settings$timeframe * 365) / (model$settings$cycle_length * 365/12))
  expect_equal(nrow(standard_trace), n_cycles + 1)  # Includes cycle 0
  expect_equal(ncol(state_trace), 3)  # 3 states

  # Verify column names (check state columns only)
  state_cols <- get_state_column_names(standard_trace)
  expect_true(all(state_cols %in% c("progression_free", "progressed", "dead")))
})

test_that("PSM trace probabilities sum to 1 at all cycles", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Use robust tolerance following R best practices
  tol <- 10 * sqrt(.Machine$double.eps)

  for (i in 1:nrow(results$aggregated)) {
    trace <- results$aggregated$collapsed_trace[[i]]
    state_trace <- get_state_columns(trace)
    row_sums <- rowSums(state_trace)

    # All row sums should be 1 (within tolerance)
    expect_true(all(abs(row_sums - 1.0) < tol),
                info = paste("Strategy:", results$aggregated$strategy[i]))
  }
})

test_that("PSM trace probabilities are valid", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  for (i in 1:nrow(results$aggregated)) {
    trace <- results$aggregated$collapsed_trace[[i]]
    state_trace <- get_state_columns(trace)

    # All probabilities should be in [0, 1]
    expect_true(all(state_trace >= 0 & state_trace <= 1),
                info = paste("Strategy:", results$aggregated$strategy[i]))
  }
})

test_that("PSM initial state is correct", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  for (i in 1:nrow(results$aggregated)) {
    trace <- results$aggregated$collapsed_trace[[i]]

    # Initial state (cycle 0) should be [1, 0, 0]
    expect_equal(trace[1, "progression_free"], 1.0)
    expect_equal(trace[1, "progressed"], 0.0)
    expect_equal(trace[1, "dead"], 0.0)
  }
})

test_that("PSM trace shows expected monotonic behavior", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  for (i in 1:nrow(results$aggregated)) {
    trace <- results$aggregated$collapsed_trace[[i]]

    # progression_free should be monotonically decreasing
    pfs_diffs <- diff(trace[, "progression_free"])
    expect_true(all(pfs_diffs <= 0),
                info = paste("PFS should decrease for strategy:", results$aggregated$strategy[i]))

    # dead should be monotonically increasing
    dead_diffs <- diff(trace[, "dead"])
    expect_true(all(dead_diffs >= 0),
                info = paste("Dead should increase for strategy:", results$aggregated$strategy[i]))
  }
})

test_that("PSM strategies produce different costs", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get traces and values for both strategies
  standard_idx <- which(results$aggregated$strategy == "standard")
  new_drug_idx <- which(results$aggregated$strategy == "new_drug")

  standard_values <- results$aggregated$trace_and_values[[standard_idx]]$values
  new_drug_values <- results$aggregated$trace_and_values[[new_drug_idx]]$values

  # Verify cost columns exist
  expect_true("cost_drug" %in% colnames(standard_values))
  expect_true("cost_drug" %in% colnames(new_drug_values))

  # Verify costs differ between strategies
  standard_total_cost <- sum(standard_values[, "cost_drug"])
  new_drug_total_cost <- sum(new_drug_values[, "cost_drug"])

  expect_true(new_drug_total_cost > standard_total_cost,
              info = "New drug should be more expensive than standard")
})

test_that("PSM values are calculated correctly with half-cycle methods", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)

  # Test with "start" method
  model$settings$half_cycle_method <- "start"
  results_start <- run_model(model)

  # Test with "end" method
  model$settings$half_cycle_method <- "end"
  results_end <- run_model(model)

  # Test with "life-table" method
  model$settings$half_cycle_method <- "life-table"
  results_lifetable <- run_model(model)

  # Get standard strategy results
  start_values <- results_start$aggregated$trace_and_values[[1]]$values
  end_values <- results_end$aggregated$trace_and_values[[1]]$values
  lifetable_values <- results_lifetable$aggregated$trace_and_values[[1]]$values

  # Methods should produce different results
  expect_false(all(start_values == end_values))
  expect_false(all(start_values == lifetable_values))
  expect_false(all(end_values == lifetable_values))

  # Life-table should generally be between start and end
  # (for first cycle, test this explicitly)
  if ("qalys" %in% colnames(start_values)) {
    qaly_start_cycle1 <- start_values[1, "qalys"]
    qaly_end_cycle1 <- end_values[1, "qalys"]
    qaly_lifetable_cycle1 <- lifetable_values[1, "qalys"]

    # Life-table should be approximately the average
    expected_lifetable <- (qaly_start_cycle1 + qaly_end_cycle1) / 2
    expect_equal(qaly_lifetable_cycle1, expected_lifetable)
  }
})

test_that("PSM discounting is applied correctly", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get undiscounted and discounted values
  standard_idx <- which(results$aggregated$strategy == "standard")
  values_undiscounted <- results$aggregated$trace_and_values[[standard_idx]]$values
  values_discounted <- results$aggregated$trace_and_values[[standard_idx]]$values_discounted

  # Discounted values should be less than or equal to undiscounted
  # (with tolerance for numerical precision)
  expect_true(all(values_discounted <= values_undiscounted + 1e-10))

  # Sum of discounted should be less than sum of undiscounted
  for (col in colnames(values_undiscounted)) {
    sum_undiscounted <- sum(values_undiscounted[, col])
    sum_discounted <- sum(values_discounted[, col])
    expect_true(sum_discounted <= sum_undiscounted + 1e-10,
                info = paste("Column:", col))
  }
})

test_that("PSM model can be converted to JSON and back", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  # Load original model
  original_model <- read_model(model_path)

  # Convert to JSON
  json_string <- write_model_json(original_model)

  # Convert back
  json_model <- read_model_json(json_string)

  # Verify model type preserved
  expect_equal(tolower(original_model$settings$model_type),
               tolower(json_model$settings$model_type))

  # Verify states preserved
  expect_equal(nrow(original_model$states), nrow(json_model$states))
  expect_setequal(original_model$states$name, json_model$states$name)

  # Verify strategies preserved
  expect_equal(nrow(original_model$strategies), nrow(json_model$strategies))
  expect_setequal(original_model$strategies$name, json_model$strategies$name)

  # Verify variables preserved
  expect_equal(nrow(original_model$variables), nrow(json_model$variables))
})

test_that("PSM JSON conversion produces identical results", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  # Load and run original model
  original_model <- read_model(model_path)
  set.seed(123)
  original_results <- run_model(original_model)

  # Convert to JSON and back
  json_string <- write_model_json(original_model)
  json_model <- read_model_json(json_string)

  # Run converted model
  set.seed(123)
  json_results <- run_model(json_model)

  # Compare traces (use testthat default tolerance)
  for (i in 1:nrow(original_results$aggregated)) {
    original_trace <- original_results$aggregated$collapsed_trace[[i]]
    json_trace <- json_results$aggregated$collapsed_trace[[i]]

    expect_equal(original_trace, json_trace,
                 info = paste("Strategy:", original_results$aggregated$strategy[i]))
  }

  # Compare values (use testthat default tolerance)
  for (i in 1:nrow(original_results$aggregated)) {
    original_values <- original_results$aggregated$trace_and_values[[i]]$values
    json_values <- json_results$aggregated$trace_and_values[[i]]$values

    expect_equal(original_values, json_values,
                 info = paste("Strategy:", original_results$aggregated$strategy[i]))
  }
})