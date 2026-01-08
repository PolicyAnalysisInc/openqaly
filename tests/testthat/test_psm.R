# PSM Model Tests
context("PSM")

# =============================================================================
# Functional/Integration Tests
# =============================================================================

test_that("example_psm model loads correctly", {
  # Load the model
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)

  # Verify basic structure
  expect_s3_class(model, "oq_model")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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
  model_path <- system.file("models/example_psm", package = "openqaly")
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

# =============================================================================
# Validation/Error Handling Tests
# =============================================================================

test_that("PSM models require exactly 3 states", {
  # Test with 2 states (too few) - need complete model to reach validation
  model_2_states <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model_2_states),
    "PSM models require exactly 3 states"
  )

  # Test with 4 states (too many)
  model_4_states <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("terminal") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model_4_states),
    "PSM models require exactly 3 states"
  )
})

test_that("PSM models require PFS endpoint", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model),
    "PSM model missing PFS endpoint definition"
  )
})

test_that("PSM models require OS endpoint", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_psm_transition("PFS", "months", pfs_dist)

  expect_error(
    run_model(model),
    "PSM model missing OS endpoint definition"
  )
})

test_that("PSM transitions must have required columns", {
  # Create a model and manually mess with transitions
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead")

  # Create invalid transitions table missing 'formula'
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "OS"),
    time_unit = c("months", "months")
    # Missing 'formula' column
  )

  expect_error(
    run_model(model),
    "Transitions has missing values in required column:.*formula"
  )
})

test_that("PSM endpoint formulas must reference surv_dist objects", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("bad_var", 0.5) |>  # Not a survival distribution
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", bad_var) |>
    add_psm_transition("OS", "months", os_dist)

  expect_error(
    run_model(model),
    "PFS formula must evaluate to a survival distribution"
  )
})

test_that("PSM models error on undefined variables in endpoint formulas", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead")

  # Manually set transitions to reference undefined variable
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "OS"),
    time_unit = c("months", "months"),
    formula = c("undefined_pfs_dist", "undefined_os_dist")
  )

  expect_error(
    run_model(model),
    "not found|undefined"
  )
})

test_that("PSM models handle multiple endpoint definitions appropriately", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months", timeframe = 10, timeframe_unit = "years") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1))

  # Manually add duplicate PFS definitions
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "PFS", "OS"),
    time_unit = c("months", "months", "months"),
    formula = c("pfs_dist", "pfs_dist", "os_dist")
  )

  # Should error or warn about multiple definitions
  expect_error(
    run_model(model),
    "multiple PFS endpoint definitions"
  )
})

test_that("PSM time unit validation catches invalid units", {
  # This test verifies that invalid time units are handled
  # The actual behavior depends on implementation
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1))

  # Manually set invalid time unit
  model$transitions <- tibble::tibble(
    endpoint = c("PFS", "OS"),
    time_unit = c("fortnights", "months"),  # Invalid unit
    formula = c("pfs_dist", "os_dist")
  )

  # Should handle gracefully (may warn or treat as cycles)
  # This test just ensures it doesn't crash
  expect_no_error({
    suppressWarnings(run_model(model))
  })
})

test_that("PSM models reject initial_prob parameter", {
  # PSM models don't use initial probabilities - should error if provided
  expect_error({
    define_model("psm") |>
      set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
      add_state("pfs", initial_prob = 1)
  }, "PSM models don't use initial_prob")
})

test_that("PSM models require transitions table", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1))

  # No transitions added
  expect_error(
    run_model(model),
    "PSM models require transitions table"
  )
})

test_that("PSM correctly validates trace probabilities", {
  # Create a model that should produce valid trace
  model <- define_model("psm") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "months") |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)

  # Should run without validation errors
  expect_no_error(run_model(model))

  # Now test that validation would catch invalid probabilities
  # (This is a design test - the validation code in psm.R should catch this)
  results <- run_model(model)
  trace <- results$aggregated$collapsed_trace[[1]]
  state_trace <- get_state_columns(trace)

  # Verify validation worked by checking trace is valid
  # Use same tolerance as validation code
  tol <- 10 * sqrt(.Machine$double.eps)
  expect_true(all(rowSums(state_trace) >= 1 - tol & rowSums(state_trace) <= 1 + tol))
  expect_true(all(state_trace >= -tol & state_trace <= 1 + tol))
})

# =============================================================================
# Unit Tests: convert_cycles_to_time_unit
# =============================================================================

test_that("convert_cycles_to_time_unit returns 0 for cycle 0", {
  # Create a minimal namespace with time conversion data
  days_per_year <- 365.25
  cycle_length_days <- 30  # 1-month cycles
  n_cycles <- 5

  # Build namespace dataframe with time columns for each cycle
  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    day = (0:n_cycles) * cycle_length_days,
    week = (0:n_cycles) * cycle_length_days / 7,
    month = (0:n_cycles) * cycle_length_days / (days_per_year / 12),
    year = (0:n_cycles) * cycle_length_days / days_per_year
  )

  ns_env <- new.env()
  assign("cycle_length_days", cycle_length_days, envir = ns_env)
  assign("cycle_length_weeks", cycle_length_days / 7, envir = ns_env)
  assign("cycle_length_months", cycle_length_days / (days_per_year / 12), envir = ns_env)
  assign("cycle_length_years", cycle_length_days / days_per_year, envir = ns_env)

  ns <- list(df = ns_df, env = ns_env)
  class(ns) <- "namespace"

  # Cycle 0 should always return 0 regardless of time unit
  expect_equal(openqaly:::convert_cycles_to_time_unit(0, "days", ns), 0)
  expect_equal(openqaly:::convert_cycles_to_time_unit(0, "weeks", ns), 0)
  expect_equal(openqaly:::convert_cycles_to_time_unit(0, "months", ns), 0)
  expect_equal(openqaly:::convert_cycles_to_time_unit(0, "years", ns), 0)
})

test_that("convert_cycles_to_time_unit correctly converts to different units", {
  days_per_year <- 365.25
  cycle_length_days <- 30
  n_cycles <- 3

  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    day = (0:n_cycles) * cycle_length_days,
    week = (0:n_cycles) * cycle_length_days / 7,
    month = (0:n_cycles) * cycle_length_days / (days_per_year / 12),
    year = (0:n_cycles) * cycle_length_days / days_per_year
  )

  ns_env <- new.env()
  assign("cycle_length_days", cycle_length_days, envir = ns_env)
  assign("cycle_length_weeks", cycle_length_days / 7, envir = ns_env)
  assign("cycle_length_months", cycle_length_days / (days_per_year / 12), envir = ns_env)
  assign("cycle_length_years", cycle_length_days / days_per_year, envir = ns_env)

  ns <- list(df = ns_df, env = ns_env)
  class(ns) <- "namespace"

  cycles <- c(0, 1, 2, 3)

  # Days: 0, 30, 60, 90
  result_days <- openqaly:::convert_cycles_to_time_unit(cycles, "days", ns)
  expect_equal(result_days, c(0, 30, 60, 90))

  # Weeks: 0, 30/7, 60/7, 90/7
  result_weeks <- openqaly:::convert_cycles_to_time_unit(cycles, "weeks", ns)
  expect_equal(result_weeks, cycles * cycle_length_days / 7)

  # Months: 0, 30/(365.25/12), ...
  days_per_month <- days_per_year / 12
  result_months <- openqaly:::convert_cycles_to_time_unit(cycles, "months", ns)
  expect_equal(result_months, cycles * cycle_length_days / days_per_month)

  # Years: 0, 30/365.25, ...
  result_years <- openqaly:::convert_cycles_to_time_unit(cycles, "years", ns)
  expect_equal(result_years, cycles * cycle_length_days / days_per_year)
})

test_that("convert_cycles_to_time_unit warns on NULL time_unit", {
  ns_df <- data.frame(cycle = 0:3, state_cycle = rep(1, 4))
  ns_env <- new.env()
  ns <- list(df = ns_df, env = ns_env)
  class(ns) <- "namespace"

  expect_warning(
    result <- openqaly:::convert_cycles_to_time_unit(c(0, 1, 2, 3), NULL, ns),
    "No time unit specified"
  )

  # Should return cycles unchanged when no time unit specified
  expect_equal(result, c(0, 1, 2, 3))
})

test_that("convert_cycles_to_time_unit uses fallback when namespace lacks time data", {
  # Create namespace without pre-computed time columns
  ns_df <- data.frame(
    cycle = numeric(0),  # Empty - no matching rows
    state_cycle = numeric(0)
  )

  ns_env <- new.env()
  cycle_length_days <- 30
  assign("cycle_length_days", cycle_length_days, envir = ns_env)
  assign("cycle_length_weeks", cycle_length_days / 7, envir = ns_env)
  assign("cycle_length_months", cycle_length_days / (365.25 / 12), envir = ns_env)
  assign("cycle_length_years", cycle_length_days / 365.25, envir = ns_env)

  ns <- list(df = ns_df, env = ns_env)
  class(ns) <- "namespace"

  cycles <- c(0, 1, 2)

  # Should use fallback calculation from cycle_length_* variables
  result_days <- openqaly:::convert_cycles_to_time_unit(cycles, "days", ns)
  expect_equal(result_days, c(0, 30, 60))
})

# =============================================================================
# Unit Tests: calculate_psm_values
# =============================================================================

test_that("calculate_psm_values applies residency values correctly", {
  # Fixed trace where state probabilities are known at each cycle
  n_cycles <- 3
  trace <- matrix(c(
    1.0, 0.0, 0.0,  # cycle 0: 100% in PFS
    0.8, 0.1, 0.1,  # cycle 1: 80% PFS, 10% progressed, 10% dead
    0.6, 0.2, 0.2   # cycle 2: 60% PFS, 20% progressed, 20% dead
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Utility of 1.0 in PFS state, 0.5 in progressed
  uneval_values <- data.frame(
    name = c("utility_pfs", "utility_prog"),
    state = c("pfs", "progressed"),
    destination = c(NA, NA),
    type = c("outcome", "outcome"),
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(
    openqaly:::as.oq_formula("1.0"),
    openqaly:::as.oq_formula("0.5")
  )

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()
  )

  trans_pfs_to_pp <- rep(0, n_cycles)
  trans_pp_to_dead <- rep(0, n_cycles)
  value_names <- c("utility_pfs", "utility_prog")
  state_names <- c("pfs", "progressed", "dead")

  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "start"
  )

  # With start method: cycle 1 uses trace[1,] = [1.0, 0.0, 0.0]
  # PFS utility = 1.0 * 1.0 = 1.0
  expect_equal(result[1, "utility_pfs"], 1.0)
  expect_equal(result[1, "utility_prog"], 0.0)  # 0.5 * 0.0 = 0

  # cycle 2 uses trace[2,] = [0.8, 0.1, 0.1]
  # PFS utility = 1.0 * 0.8 = 0.8
  # Progressed utility = 0.5 * 0.1 = 0.05
  expect_equal(result[2, "utility_pfs"], 0.8)
  expect_equal(result[2, "utility_prog"], 0.05)
})

test_that("calculate_psm_values applies transitional values correctly", {
  n_cycles <- 3
  trace <- matrix(c(
    1.0, 0.0, 0.0,
    0.7, 0.2, 0.1,
    0.5, 0.3, 0.2
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Transitional cost: 1000 when transitioning from PFS to progressed
  uneval_values <- data.frame(
    name = "prog_cost",
    state = "pfs",
    destination = "progressed",
    type = "cost",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1000"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()
  )

  # Transition probabilities: PFS to progressed
  # At cycle 1: 0.2 transition (1.0 - 0.7 - 0.1 = 0.2 but we track decrease in PFS)
  trans_pfs_to_pp <- c(0, 0.3, 0.2)  # Calculated as diff in PFS
  trans_pp_to_dead <- c(0, 0.1, 0.1)
  value_names <- c("prog_cost")
  state_names <- c("pfs", "progressed", "dead")

  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "start"
  )

  # Transitional cost = formula_value * transition_probability
  # Cycle 1: 1000 * 0 = 0 (no transition at cycle 0)
  expect_equal(result[1, "prog_cost"], 0)
  # Cycle 2: 1000 * 0.3 = 300
  expect_equal(result[2, "prog_cost"], 300)
  # Cycle 3: 1000 * 0.2 = 200
  expect_equal(result[3, "prog_cost"], 200)
})

test_that("calculate_psm_values applies model-level values correctly", {
  n_cycles <- 2
  trace <- matrix(c(
    1.0, 0.0, 0.0,
    0.8, 0.1, 0.1,
    0.6, 0.2, 0.2
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Model-level value (no state association): fixed cost per cycle
  uneval_values <- data.frame(
    name = "admin_cost",
    state = NA,
    destination = NA,
    type = "cost",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("100"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()
  )

  trans_pfs_to_pp <- rep(0, n_cycles)
  trans_pp_to_dead <- rep(0, n_cycles)
  value_names <- c("admin_cost")
  state_names <- c("pfs", "progressed", "dead")

  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "start"
  )

  # Model-level value: applied directly without state weighting
  expect_equal(result[1, "admin_cost"], 100)
  expect_equal(result[2, "admin_cost"], 100)
})

test_that("calculate_psm_values handles half-cycle method 'end' correctly", {
  n_cycles <- 2
  trace <- matrix(c(
    1.0, 0.0, 0.0,  # cycle 0
    0.8, 0.2, 0.0,  # cycle 1
    0.6, 0.3, 0.1   # cycle 2
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  uneval_values <- data.frame(
    name = "utility",
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1.0"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()
  )

  trans_pfs_to_pp <- rep(0, n_cycles)
  trans_pp_to_dead <- rep(0, n_cycles)
  value_names <- c("utility")
  state_names <- c("pfs", "progressed", "dead")

  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "end"
  )

  # With end method: cycle 1 uses trace[2,] = [0.8, 0.2, 0.0]
  expect_equal(result[1, "utility"], 0.8)
  # cycle 2 uses trace[3,] = [0.6, 0.3, 0.1]
  expect_equal(result[2, "utility"], 0.6)
})

test_that("calculate_psm_values handles half-cycle method 'life-table' correctly", {
  n_cycles <- 2
  trace <- matrix(c(
    1.0, 0.0, 0.0,  # cycle 0
    0.8, 0.2, 0.0,  # cycle 1
    0.6, 0.3, 0.1   # cycle 2
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  uneval_values <- data.frame(
    name = "utility",
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1.0"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()
  )

  trans_pfs_to_pp <- rep(0, n_cycles)
  trans_pp_to_dead <- rep(0, n_cycles)
  value_names <- c("utility")
  state_names <- c("pfs", "progressed", "dead")

  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "life-table"
  )

  # Life-table method for cycle 1: average of trace[1,] and trace[2,]
  # (1.0 + 0.8) / 2 = 0.9
  expect_equal(result[1, "utility"], 0.9)
  # Cycle 2 (last cycle): uses end value only = trace[3,] = 0.6
  expect_equal(result[2, "utility"], 0.6)
})

test_that("calculate_psm_values returns empty matrix when no values defined", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.0, 0.8, 0.1, 0.1, 0.6, 0.2, 0.2),
                  nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  uneval_values <- data.frame(
    name = character(0),
    state = character(0),
    destination = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()
  )

  value_names <- character(0)
  state_names <- c("pfs", "progressed", "dead")

  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    rep(0, n_cycles), rep(0, n_cycles),
    value_names, state_names, n_cycles,
    "start"
  )

  expect_equal(nrow(result), n_cycles)
  expect_equal(ncol(result), 0)
})

# =============================================================================
# Unit Tests: calculate_psm_trace_and_values (Mathematical Verification)
# =============================================================================

test_that("calculate_psm_trace_and_values produces mathematically correct trace for exponential distributions", {
  # Use exponential distributions where S(t) = exp(-rate * t)
  # This allows us to calculate expected values analytically
  rate_pfs <- 0.2
  rate_os <- 0.1
  n_cycles <- 5

  # Create real survival distributions from openqalysurv
  pfs_dist <- define_surv_param("exp", rate = rate_pfs)
  os_dist <- define_surv_param("exp", rate = rate_os)

  # Set time units (distributions expect time in the same unit as cycles)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  # Create namespace with 1-month cycles (so cycle number = months)
  days_per_year <- 365.25
  cycle_length_days <- days_per_year / 12  # ~30.44 days per month

  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    day = (0:n_cycles) * cycle_length_days,
    week = (0:n_cycles) * cycle_length_days / 7,
    month = 0:n_cycles,  # 1 cycle = 1 month
    year = (0:n_cycles) / 12
  )

  ns_env <- new.env()
  assign("cycle_length_days", cycle_length_days, envir = ns_env)
  assign("cycle_length_months", 1, envir = ns_env)

  namespace <- list(df = ns_df, env = ns_env)
  class(namespace) <- "namespace"

  # Empty values for this test (we're just testing trace calculation)
  uneval_values <- data.frame(
    name = character(0),
    state = character(0),
    destination = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  survival_distributions <- list(pfs = pfs_dist, os = os_dist)

  result <- openqaly:::calculate_psm_trace_and_values(
    survival_distributions,
    uneval_values,
    namespace,
    value_names = character(0),
    state_names = c("pfs", "progressed", "dead"),
    n_cycles = n_cycles,
    half_cycle_method = "start"
  )

  # Verify trace against analytical solution
  tol <- 1e-6
  for (t in 0:n_cycles) {
    # Analytical survival probabilities
    s_pfs <- exp(-rate_pfs * t)
    s_os <- exp(-rate_os * t)

    # PSM state probabilities
    expected_pfs <- min(s_pfs, s_os)
    expected_dead <- 1 - s_os
    expected_prog <- s_os - expected_pfs

    expect_equal(result$trace[t + 1, "pfs"], expected_pfs, tolerance = tol,
                 info = paste("PFS at cycle", t))
    expect_equal(result$trace[t + 1, "dead"], expected_dead, tolerance = tol,
                 info = paste("Dead at cycle", t))
    expect_equal(result$trace[t + 1, "progressed"], expected_prog, tolerance = tol,
                 info = paste("Progressed at cycle", t))
  }
})

test_that("calculate_psm_trace_and_values handles PFS > OS crossover correctly", {
  # Test case where PFS survival exceeds OS survival (shouldn't happen clinically,
  # but the model should handle it gracefully by using min(PFS, OS))
  # Create distributions where PFS is "better" than OS initially
  # Use rates such that PFS decays slower than OS
  rate_pfs <- 0.05  # Slower decay
  rate_os <- 0.2    # Faster decay

  pfs_dist <- define_surv_param("exp", rate = rate_pfs)
  os_dist <- define_surv_param("exp", rate = rate_os)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  n_cycles <- 3
  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    month = 0:n_cycles
  )

  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = pfs_dist, os = os_dist),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # At each cycle, PFS state should be min(S_PFS, S_OS)
  # Since OS decays faster, S_OS < S_PFS after a while
  for (t in 0:n_cycles) {
    s_pfs <- exp(-rate_pfs * t)
    s_os <- exp(-rate_os * t)

    # Even when S_PFS > S_OS, the model should use the lower value
    expected_pfs_state <- min(s_pfs, s_os)
    expect_equal(result$trace[t + 1, "pfs"], expected_pfs_state, tolerance = 1e-6)

    # Dead state should always be 1 - S_OS
    expect_equal(result$trace[t + 1, "dead"], 1 - s_os, tolerance = 1e-6)
  }
})

test_that("calculate_psm_trace_and_values trace sums to 1 at all cycles", {
  rate_pfs <- 0.15
  rate_os <- 0.08
  n_cycles <- 10

  pfs_dist <- define_surv_param("exp", rate = rate_pfs)
  os_dist <- define_surv_param("exp", rate = rate_os)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    month = 0:n_cycles
  )

  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = pfs_dist, os = os_dist),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # All rows should sum to 1
  tol <- 10 * sqrt(.Machine$double.eps)
  row_sums <- rowSums(result$trace)
  expect_true(all(abs(row_sums - 1) < tol))
})

test_that("calculate_psm_trace_and_values initial state is correct", {
  rate_pfs <- 0.1
  rate_os <- 0.05
  n_cycles <- 3

  pfs_dist <- define_surv_param("exp", rate = rate_pfs)
  os_dist <- define_surv_param("exp", rate = rate_os)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    month = 0:n_cycles
  )

  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = pfs_dist, os = os_dist),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # At t=0, S(0) = exp(0) = 1 for both distributions
  # So initial state should be [1, 0, 0]
  expect_equal(result$trace[1, "pfs"], 1.0)
  expect_equal(result$trace[1, "progressed"], 0.0)
  expect_equal(result$trace[1, "dead"], 0.0)
})

test_that("calculate_psm_trace_and_values handles NULL distributions gracefully", {
  n_cycles <- 3

  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    month = 0:n_cycles
  )

  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  # NULL distributions should return empty trace
  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = NULL, os = NULL),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # Should return zero trace
  expect_equal(dim(result$trace), c(n_cycles + 1, 3))
  expect_true(all(result$trace == 0))
})

# =============================================================================
# Unit Tests: parse_psm_custom (Validation)
# =============================================================================

# Note: parse_psm_custom validation tests require creating model structures
# manually since define_model() doesn't support "psm_custom" type directly.
# These tests call the internal parse_psm_custom function directly.

test_that("parse_psm_custom requires at least 2 states", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = "alive", display_name = "Alive", description = ""),
    transitions = tibble::tibble(state = "alive", formula = "1.0"),
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "at least 2 states"
  )
})

test_that("parse_psm_custom requires transitions table", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("alive", "dead"), display_name = c("Alive", "Dead"), description = c("", "")),
    transitions = tibble::tibble(),  # Empty
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "require transitions table"
  )
})

test_that("parse_psm_custom requires state and formula columns", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("alive", "dead"), display_name = c("Alive", "Dead"), description = c("", "")),
    transitions = tibble::tibble(state = c("alive", "dead")),  # Missing formula column
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "missing required columns"
  )
})

test_that("parse_psm_custom requires formula for each state", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("alive", "dead"), display_name = c("Alive", "Dead"), description = c("", "")),
    transitions = tibble::tibble(state = "dead", formula = "0.1"),  # Missing "alive"
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "Missing probability formulas for states"
  )
})

test_that("parse_psm_custom rejects duplicate state formulas", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("alive", "dead"), display_name = c("Alive", "Dead"), description = c("", "")),
    transitions = tibble::tibble(state = c("alive", "alive", "dead"), formula = c("0.9", "0.8", "C")),
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "Duplicate probability formulas"
  )
})

test_that("parse_psm_custom rejects formulas for undefined states", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("alive", "dead"), display_name = c("Alive", "Dead"), description = c("", "")),
    transitions = tibble::tibble(state = c("alive", "dead", "unknown"), formula = c("0.9", "C", "0.1")),
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "undefined states"
  )
})

test_that("parse_psm_custom allows only one complement operator", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("state1", "state2", "state3"), display_name = c("S1", "S2", "S3"), description = c("", "", "")),
    transitions = tibble::tibble(state = c("state1", "state2", "state3"), formula = c("0.5", "C", "C")),
    values = tibble::tibble()
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "Only one state can use the complement"
  )
})

test_that("parse_psm_custom rejects transitional values", {
  model <- list(
    settings = list(model_type = "psm_custom", n_cycles = 10),
    states = tibble::tibble(name = c("alive", "dead"), display_name = c("Alive", "Dead"), description = c("", "")),
    transitions = tibble::tibble(state = c("alive", "dead"), formula = c("0.9", "C")),
    values = tibble::tibble(name = "cost", formula = "100", state = "alive", destination = "dead", type = "cost")
  )

  expect_error(
    openqaly:::parse_psm_custom(model),
    "transitional values"
  )
})

# =============================================================================
# Unit Tests: calculate_psm_custom_trace_and_values
# =============================================================================

test_that("calculate_psm_custom_trace_and_values computes complement correctly", {
  n_cycles <- 3

  # Formulas: state A = 0.6, state B = 0.3, state C = complement (should be 0.1)
  formulas <- list(
    A = openqaly:::as.oq_formula("0.6"),
    B = openqaly:::as.oq_formula("0.3"),
    C = openqaly:::as.oq_formula("C")
  )

  # Create namespace with single row - the function sets cycle for each iteration
  ns_df <- data.frame(cycle = 0, state_cycle = 1)
  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_custom_trace_and_values(
    formulas,
    uneval_values,
    namespace,
    character(0),
    c("A", "B", "C"),
    n_cycles,
    "start"
  )

  # All cycles should have the same probabilities (constant formulas)
  tol <- 1e-10
  for (i in 1:(n_cycles + 1)) {
    expect_equal(result$trace[i, "A"], 0.6, tolerance = tol)
    expect_equal(result$trace[i, "B"], 0.3, tolerance = tol)
    expect_equal(result$trace[i, "C"], 0.1, tolerance = tol)  # 1 - 0.6 - 0.3
  }
})

test_that("calculate_psm_custom_trace_and_values trace sums to 1", {
  n_cycles <- 5

  # Time-varying formula using cycle variable
  formulas <- list(
    alive = openqaly:::as.oq_formula("1 - 0.1 * cycle"),
    dead = openqaly:::as.oq_formula("C")  # Complement
  )

  # Single row namespace - function sets cycle for each iteration
  ns_df <- data.frame(cycle = 0, state_cycle = 1)
  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_custom_trace_and_values(
    formulas,
    uneval_values,
    namespace,
    character(0),
    c("alive", "dead"),
    n_cycles,
    "start"
  )

  # All rows should sum to 1
  tol <- 10 * sqrt(.Machine$double.eps)
  row_sums <- rowSums(result$trace)
  expect_true(all(abs(row_sums - 1) < tol))

  # Verify specific values
  for (t in 0:n_cycles) {
    expected_alive <- 1 - 0.1 * t
    expect_equal(result$trace[t + 1, "alive"], expected_alive, tolerance = 1e-10)
    expect_equal(result$trace[t + 1, "dead"], 0.1 * t, tolerance = 1e-10)
  }
})

test_that("calculate_psm_custom_trace_and_values handles variables in namespace", {
  n_cycles <- 2

  # Formula referencing variable in namespace
  ns_env <- new.env()
  assign("mortality_rate", 0.15, envir = ns_env)

  formulas <- list(
    alive = openqaly:::as.oq_formula("1 - mortality_rate * cycle"),
    dead = openqaly:::as.oq_formula("C")
  )

  # Single row namespace
  ns_df <- data.frame(cycle = 0, state_cycle = 1)
  namespace <- list(df = ns_df, env = ns_env)
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_custom_trace_and_values(
    formulas,
    uneval_values,
    namespace,
    character(0),
    c("alive", "dead"),
    n_cycles,
    "start"
  )

  # At cycle 1: alive = 1 - 0.15 * 1 = 0.85
  expect_equal(result$trace[2, "alive"], 0.85, tolerance = 1e-10)
  expect_equal(result$trace[2, "dead"], 0.15, tolerance = 1e-10)
})

# =============================================================================
# Unit Tests: calculate_psm_custom_values
# =============================================================================

test_that("calculate_psm_custom_values applies residency values correctly", {
  n_cycles <- 2
  trace <- matrix(c(
    1.0, 0.0,  # cycle 0
    0.8, 0.2,  # cycle 1
    0.6, 0.4   # cycle 2
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("alive", "dead")
  rownames(trace) <- 0:2

  uneval_values <- data.frame(
    name = "utility",
    state = "alive",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1.0"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  result <- openqaly:::calculate_psm_custom_values(
    uneval_values, namespace, trace,
    c("utility"), c("alive", "dead"),
    n_cycles, "start"
  )

  # Start method: cycle 1 uses trace[1,] = [1.0, 0.0]
  expect_equal(result[1, "utility"], 1.0)
  # cycle 2 uses trace[2,] = [0.8, 0.2]
  expect_equal(result[2, "utility"], 0.8)
})

test_that("calculate_psm_custom_values applies model-level values correctly", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.8, 0.2, 0.6, 0.4), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("alive", "dead")
  rownames(trace) <- 0:2

  uneval_values <- data.frame(
    name = "admin_cost",
    state = NA,  # Model-level
    destination = NA,
    type = "cost",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("50"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  result <- openqaly:::calculate_psm_custom_values(
    uneval_values, namespace, trace,
    c("admin_cost"), c("alive", "dead"),
    n_cycles, "start"
  )

  # Model-level: applied without state weighting
  expect_equal(result[1, "admin_cost"], 50)
  expect_equal(result[2, "admin_cost"], 50)
})

test_that("calculate_psm_custom_values handles half-cycle methods", {
  n_cycles <- 2
  trace <- matrix(c(
    1.0, 0.0,
    0.8, 0.2,
    0.6, 0.4
  ), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("alive", "dead")
  rownames(trace) <- 0:2

  uneval_values <- data.frame(
    name = "utility",
    state = "alive",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1.0"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  # Test "end" method
  result_end <- openqaly:::calculate_psm_custom_values(
    uneval_values, namespace, trace,
    c("utility"), c("alive", "dead"),
    n_cycles, "end"
  )

  # End method: cycle 1 uses trace[2,] = [0.8, 0.2]
  expect_equal(result_end[1, "utility"], 0.8)
  expect_equal(result_end[2, "utility"], 0.6)

  # Test "life-table" method
  result_lt <- openqaly:::calculate_psm_custom_values(
    uneval_values, namespace, trace,
    c("utility"), c("alive", "dead"),
    n_cycles, "life-table"
  )

  # Life-table: cycle 1 averages trace[1,] and trace[2,]
  expect_equal(result_lt[1, "utility"], 0.9)  # (1.0 + 0.8) / 2
})

# =============================================================================
# Custom PSM Integration Tests
# =============================================================================

# Note: The define_model() builder doesn't support "psm_custom" type.
# Custom PSM models must be created via file import or manual construction.
# These tests verify the internal calculation functions work correctly,
# which is covered by the unit tests above.

# =============================================================================
# Edge Case Tests
# =============================================================================

test_that("PSM handles single cycle model correctly", {
  # Test with n_cycles = 1 (minimum viable)
  pfs_dist <- define_surv_param("exp", rate = 0.1)
  os_dist <- define_surv_param("exp", rate = 0.05)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  n_cycles <- 1
  ns_df <- data.frame(
    cycle = 0:n_cycles,
    state_cycle = rep(1, n_cycles + 1),
    month = 0:n_cycles
  )

  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = pfs_dist, os = os_dist),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # Should have 2 rows (cycle 0 and cycle 1)
  expect_equal(nrow(result$trace), 2)
  expect_equal(result$trace[1, "pfs"], 1.0)  # At t=0, S(0) = 1
})

test_that("evaluate_psm_endpoint accumulates error for undefined distribution", {
  # Clear any previous errors
  openqaly:::clear_oq_errors()

  namespace <- list(df = data.frame(cycle = 0:3, month = 0:3), env = new.env())
  class(namespace) <- "namespace"

  # Missing distribution definition
  endpoint_def <- list(
    formula = openqaly:::as.oq_formula("some_undefined_dist"),
    time_unit = "months"
  )

  # The function accumulates errors rather than throwing immediately
  result <- openqaly:::evaluate_psm_endpoint(endpoint_def, "PFS", namespace)

  # Should return NULL when there's an error
  expect_null(result)

  # Clean up
  openqaly:::clear_oq_errors()
})

test_that("PSM trace is monotonically decreasing in PFS state", {
  # Clear any previous errors
  openqaly:::clear_oq_errors()

  # For realistic PSM, PFS should monotonically decrease
  rate_pfs <- 0.15
  rate_os <- 0.08
  n_cycles <- 10

  pfs_dist <- define_surv_param("exp", rate = rate_pfs)
  os_dist <- define_surv_param("exp", rate = rate_os)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  ns_df <- data.frame(cycle = 0:n_cycles, state_cycle = rep(1, n_cycles + 1), month = 0:n_cycles)
  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = pfs_dist, os = os_dist),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # PFS should be monotonically non-increasing
  pfs_vals <- result$trace[, "pfs"]
  for (i in 2:length(pfs_vals)) {
    expect_lte(pfs_vals[i], pfs_vals[i - 1] + 1e-10)
  }
})

test_that("PSM trace dead state is monotonically increasing", {
  # Clear any previous errors
  openqaly:::clear_oq_errors()

  rate_pfs <- 0.15
  rate_os <- 0.08
  n_cycles <- 10

  pfs_dist <- define_surv_param("exp", rate = rate_pfs)
  os_dist <- define_surv_param("exp", rate = rate_os)
  attr(pfs_dist, "time_unit") <- "months"
  attr(os_dist, "time_unit") <- "months"

  ns_df <- data.frame(cycle = 0:n_cycles, state_cycle = rep(1, n_cycles + 1), month = 0:n_cycles)
  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  result <- openqaly:::calculate_psm_trace_and_values(
    list(pfs = pfs_dist, os = os_dist),
    uneval_values,
    namespace,
    character(0),
    c("pfs", "progressed", "dead"),
    n_cycles,
    "start"
  )

  # Dead should be monotonically non-decreasing
  dead_vals <- result$trace[, "dead"]
  for (i in 2:length(dead_vals)) {
    expect_gte(dead_vals[i], dead_vals[i - 1] - 1e-10)
  }
})

test_that("Custom PSM handles negative complement gracefully", {
  # Test case where complement would be negative (invalid probabilities)
  n_cycles <- 3

  # Formulas that sum to more than 1
  formulas <- list(
    A = openqaly:::as.oq_formula("0.7"),
    B = openqaly:::as.oq_formula("0.5"),  # 0.7 + 0.5 = 1.2 > 1
    C = openqaly:::as.oq_formula("C")     # Complement would be -0.2
  )

  # Single row namespace
  ns_df <- data.frame(cycle = 0, state_cycle = 1)
  namespace <- list(df = ns_df, env = new.env())
  class(namespace) <- "namespace"

  uneval_values <- data.frame(
    name = character(0), state = character(0), destination = character(0),
    type = character(0), stringsAsFactors = FALSE
  )
  uneval_values$formula <- list()

  # The function accumulates errors and throws at checkpoint
  # Negative complement should trigger an error
  expect_error(
    openqaly:::calculate_psm_custom_trace_and_values(
      formulas,
      uneval_values,
      namespace,
      character(0),
      c("A", "B", "C"),
      n_cycles,
      "start"
    ),
    "out of bounds|do not sum to 1"
  )
})

test_that("PSM values formula error accumulates properly", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.0, 0.8, 0.1, 0.1, 0.6, 0.2, 0.2), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Formula referencing undefined variable
  uneval_values <- data.frame(
    name = "bad_value",
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("undefined_xyz_var"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  # Should error due to undefined variable
  expect_error(
    openqaly:::calculate_psm_values(
      uneval_values, namespace, trace,
      rep(0, n_cycles), rep(0, n_cycles),
      c("bad_value"), c("pfs", "progressed", "dead"),
      n_cycles, "start"
    ),
    "undefined_xyz_var|not found|error"
  )
})

# =============================================================================
# Coverage Tests: Empty Summaries (Lines 208-210)
# =============================================================================

test_that("PSM handles missing summaries gracefully (empty summary tibble)", {
  # Create a PSM model without any summaries
  model <- define_model("psm") |>
    set_settings(
      n_cycles = 5,
      cycle_length = 1,
      cycle_length_unit = "months",
      timeframe = 1,
      timeframe_unit = "years"
    ) |>
    add_strategy("default") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.2)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.1)) |>
    add_psm_transition("PFS", "months", pfs_dist) |>
    add_psm_transition("OS", "months", os_dist)
  # NO summaries added - this triggers the else branch at lines 208-210

  results <- run_model(model)

  # Verify the empty summary structure is created correctly
  summary <- results$aggregated$summaries[[1]]
  expect_equal(nrow(summary), 0)
  expect_equal(colnames(summary), c("summary", "value", "amount"))

  # Also check discounted summaries
  summary_disc <- results$aggregated$summaries_discounted[[1]]
  expect_equal(nrow(summary_disc), 0)
  expect_equal(colnames(summary_disc), c("summary", "value", "amount"))
})

# =============================================================================
# Coverage Tests: Transitions Parsing Errors (Lines 238-242)
# =============================================================================

test_that("parse_and_eval_psm_transitions handles null transitions", {
  namespace <- list(df = data.frame(cycle = 0:3, month = 0:3), env = new.env())
  class(namespace) <- "namespace"

  expect_error(
    openqaly:::parse_and_eval_psm_transitions(NULL, list(), namespace),
    "require.*transitions|PFS and OS"
  )
})

test_that("parse_and_eval_psm_transitions handles empty transitions table", {
  namespace <- list(df = data.frame(cycle = 0:3, month = 0:3), env = new.env())
  class(namespace) <- "namespace"

  empty_transitions <- tibble::tibble(
    endpoint = character(0),
    time_unit = character(0),
    formula = character(0)
  )

  expect_error(
    openqaly:::parse_and_eval_psm_transitions(empty_transitions, list(), namespace),
    "require.*transitions|PFS and OS"
  )
})

# =============================================================================
# Coverage Tests: Transitions Validation - Missing/Multiple PFS/OS (Lines 252-272)
# =============================================================================

test_that("parse_and_eval_psm_transitions detects missing PFS endpoint", {
  # Create namespace with survival distribution for OS only
  ns_env <- new.env()
  os_dist <- define_surv_param("exp", rate = 0.1)
  attr(os_dist, "time_unit") <- "months"
  assign("os_dist", os_dist, envir = ns_env)

  namespace <- list(
    df = data.frame(cycle = 0:3, month = 0:3),
    env = ns_env
  )
  class(namespace) <- "namespace"

  # Only OS defined, no PFS
  transitions <- tibble::tibble(
    endpoint = "OS",
    time_unit = "months",
    formula = "os_dist"
  )

  expect_error(
    openqaly:::parse_and_eval_psm_transitions(transitions, list(), namespace),
    "missing PFS"
  )
})

test_that("parse_and_eval_psm_transitions detects missing OS endpoint", {
  # Create namespace with survival distribution for PFS only
  ns_env <- new.env()
  pfs_dist <- define_surv_param("exp", rate = 0.2)
  attr(pfs_dist, "time_unit") <- "months"
  assign("pfs_dist", pfs_dist, envir = ns_env)

  namespace <- list(
    df = data.frame(cycle = 0:3, month = 0:3),
    env = ns_env
  )
  class(namespace) <- "namespace"

  # Only PFS defined, no OS
  transitions <- tibble::tibble(
    endpoint = "PFS",
    time_unit = "months",
    formula = "pfs_dist"
  )

  expect_error(
    openqaly:::parse_and_eval_psm_transitions(transitions, list(), namespace),
    "missing OS"
  )
})

test_that("parse_and_eval_psm_transitions detects multiple PFS definitions", {
  # Create namespace with multiple PFS distributions
  ns_env <- new.env()
  pfs_dist1 <- define_surv_param("exp", rate = 0.2)
  attr(pfs_dist1, "time_unit") <- "months"
  assign("pfs_dist1", pfs_dist1, envir = ns_env)

  pfs_dist2 <- define_surv_param("exp", rate = 0.3)
  attr(pfs_dist2, "time_unit") <- "months"
  assign("pfs_dist2", pfs_dist2, envir = ns_env)

  os_dist <- define_surv_param("exp", rate = 0.1)
  attr(os_dist, "time_unit") <- "months"
  assign("os_dist", os_dist, envir = ns_env)

  namespace <- list(
    df = data.frame(cycle = 0:3, month = 0:3),
    env = ns_env
  )
  class(namespace) <- "namespace"

  # Multiple PFS definitions
  transitions <- tibble::tibble(
    endpoint = c("PFS", "PFS", "OS"),
    time_unit = c("months", "months", "months"),
    formula = c("pfs_dist1", "pfs_dist2", "os_dist")
  )

  expect_error(
    openqaly:::parse_and_eval_psm_transitions(transitions, list(), namespace),
    "multiple PFS"
  )
})

test_that("parse_and_eval_psm_transitions detects multiple OS definitions", {
  # Create namespace with multiple OS distributions
  ns_env <- new.env()
  pfs_dist <- define_surv_param("exp", rate = 0.2)
  attr(pfs_dist, "time_unit") <- "months"
  assign("pfs_dist", pfs_dist, envir = ns_env)

  os_dist1 <- define_surv_param("exp", rate = 0.1)
  attr(os_dist1, "time_unit") <- "months"
  assign("os_dist1", os_dist1, envir = ns_env)

  os_dist2 <- define_surv_param("exp", rate = 0.08)
  attr(os_dist2, "time_unit") <- "months"
  assign("os_dist2", os_dist2, envir = ns_env)

  namespace <- list(
    df = data.frame(cycle = 0:3, month = 0:3),
    env = ns_env
  )
  class(namespace) <- "namespace"

  # Multiple OS definitions
  transitions <- tibble::tibble(
    endpoint = c("PFS", "OS", "OS"),
    time_unit = c("months", "months", "months"),
    formula = c("pfs_dist", "os_dist1", "os_dist2")
  )

  expect_error(
    openqaly:::parse_and_eval_psm_transitions(transitions, list(), namespace),
    "multiple OS"
  )
})

# =============================================================================
# Coverage Tests: Value Evaluation Errors (Lines 519-534)
# =============================================================================

test_that("calculate_psm_values skips NA value_name", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.0, 0.8, 0.1, 0.1, 0.6, 0.2, 0.2), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Value with NA name - should be skipped
  uneval_values <- data.frame(
    name = NA_character_,
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1.0"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  # Should not error, should just skip the NA-named value
  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    rep(0, n_cycles), rep(0, n_cycles),
    character(0),  # Empty value_names - NA won't match
    c("pfs", "progressed", "dead"),
    n_cycles, "start"
  )

  # Result should have 0 columns (no values calculated)
  expect_equal(ncol(result), 0)
  expect_equal(nrow(result), n_cycles)
})

test_that("calculate_psm_values skips value_name not in value_names list", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.0, 0.8, 0.1, 0.1, 0.6, 0.2, 0.2), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Value with name that's not in value_names - should be skipped
  uneval_values <- data.frame(
    name = "unlisted_value",
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("1.0"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  # value_names doesn't include "unlisted_value"
  result <- openqaly:::calculate_psm_values(
    uneval_values, namespace, trace,
    rep(0, n_cycles), rep(0, n_cycles),
    c("other_value"),  # Doesn't include "unlisted_value"
    c("pfs", "progressed", "dead"),
    n_cycles, "start"
  )

  # Result should have 1 column for "other_value" but values should be 0
  expect_equal(ncol(result), 1)
  expect_equal(colnames(result), "other_value")
  expect_true(all(result[, "other_value"] == 0))
})

test_that("calculate_psm_values handles non-numeric evaluation result", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.0, 0.8, 0.1, 0.1, 0.6, 0.2, 0.2), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Value that evaluates to character (non-numeric)
  uneval_values <- data.frame(
    name = "bad_type",
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("'not_numeric'"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  # Should error due to non-numeric result
  expect_error(
    openqaly:::calculate_psm_values(
      uneval_values, namespace, trace,
      rep(0, n_cycles), rep(0, n_cycles),
      c("bad_type"),
      c("pfs", "progressed", "dead"),
      n_cycles, "start"
    ),
    "single numeric value|error"
  )
})

test_that("calculate_psm_values handles vector evaluation result", {
  n_cycles <- 2
  trace <- matrix(c(1.0, 0.0, 0.0, 0.8, 0.1, 0.1, 0.6, 0.2, 0.2), nrow = 3, byrow = TRUE)
  colnames(trace) <- c("pfs", "progressed", "dead")
  rownames(trace) <- 0:2

  # Value that evaluates to vector instead of scalar
  uneval_values <- data.frame(
    name = "bad_length",
    state = "pfs",
    destination = NA,
    type = "outcome",
    stringsAsFactors = FALSE
  )
  uneval_values$formula <- list(openqaly:::as.oq_formula("c(1, 2, 3)"))

  namespace <- list(
    df = data.frame(cycle = 1:n_cycles, state_cycle = rep(1, n_cycles)),
    env = new.env()
  )

  # Should error due to vector result (not single value)
  expect_error(
    openqaly:::calculate_psm_values(
      uneval_values, namespace, trace,
      rep(0, n_cycles), rep(0, n_cycles),
      c("bad_length"),
      c("pfs", "progressed", "dead"),
      n_cycles, "start"
    ),
    "single numeric value|error"
  )
})

# =============================================================================
# Coverage Tests: Custom PSM Integration (Lines 689-700)
# =============================================================================

test_that("run_segment.psm_custom executes correctly via run_model", {
  # Build a complete psm_custom model structure
  # Note: define_model() doesn't support psm_custom, so we build manually
  model <- list(
    settings = list(
      model_type = "psm_custom",
      timeframe = 1,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "months",
      days_per_year = 365.25,
      half_cycle_method = "start",
      discount_cost = 0.035,
      discount_outcomes = 0.035
    ),
    states = tibble::tibble(
      name = c("alive", "dead"),
      display_name = c("Alive", "Dead"),
      description = c("", "")
    ),
    # psm_custom uses state/formula format (not endpoint/time_unit/formula like standard PSM)
    # Formulas must evaluate to scalar values for each cycle
    transitions = tibble::tibble(
      state = c("alive", "dead"),
      formula = c("0.8", "C")  # C = complement (1 - 0.8 = 0.2)
    ),
    strategies = tibble::tibble(
      name = "default",
      display_name = "Default",
      description = ""
    ),
    groups = tibble::tibble(
      name = "all",
      display_name = "All",
      description = "",
      weight = "1"
    ),
    variables = tibble::tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      formula = character(0),
      strategy = character(0),
      group = character(0)
    ),
    values = tibble::tibble(
      name = "qalys",
      display_name = "QALYs",
      description = "",
      state = "alive",
      destination = NA_character_,
      type = "outcome",
      formula = "0.8"
    ),
    summaries = tibble::tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      values = character(0),
      type = character(0),
      wtp = numeric(0)
    ),
    tables = list(),
    scripts = list(),
    trees = NULL
  )
  class(model) <- "oq_model"

  # run_model handles normalization and sets up model$env
  results <- run_model(model)

  # Verify custom PSM executed
  expect_true(!is.null(results$aggregated))
  expect_true(!is.null(results$aggregated$collapsed_trace))

  # Verify trace has expected states
  trace <- results$aggregated$collapsed_trace[[1]]
  expect_true("alive" %in% colnames(trace))
  expect_true("dead" %in% colnames(trace))

  # Verify probabilities sum to 1
  state_cols <- trace[, c("alive", "dead")]
  row_sums <- rowSums(state_cols)
  expect_true(all(abs(row_sums - 1) < 1e-10))

  # Verify values were calculated
  values <- results$aggregated$trace_and_values[[1]]$values
  expect_true("qalys" %in% colnames(values))
})
