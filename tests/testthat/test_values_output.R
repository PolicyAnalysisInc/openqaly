# Test Values Output Functions

library(testthat)
library(heRomod2)

test_that("get_values works with aggregated data", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Test long format
  values_long <- get_values(results, format = "long", group = "aggregated")
  expect_s3_class(values_long, "data.frame")
  expect_true("strategy" %in% colnames(values_long))
  expect_true("group" %in% colnames(values_long))
  expect_true("value_name" %in% colnames(values_long))
  expect_true("amount" %in% colnames(values_long))
  expect_equal(unique(values_long$group), "Aggregated")

  # Test wide format
  values_wide <- get_values(results, format = "wide", group = "aggregated")
  expect_s3_class(values_wide, "data.frame")
  expect_true("strategy" %in% colnames(values_wide))
  expect_true("group" %in% colnames(values_wide))
})

test_that("get_values filters by value type correctly", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get cost values only
  costs <- get_values(results, value_type = "cost")
  cost_values <- unique(costs$value_name)

  # Get outcome values only
  outcomes <- get_values(results, value_type = "outcome")
  outcome_values <- unique(outcomes$value_name)

  # Should be mutually exclusive
  expect_length(intersect(cost_values, outcome_values), 0)

  # Get all values
  all_values <- get_values(results, value_type = "all")
  all_value_names <- unique(all_values$value_name)

  # All should be union of costs and outcomes
  expect_setequal(all_value_names, union(cost_values, outcome_values))
})

test_that("get_values handles discounted vs undiscounted", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get undiscounted
  values_undiscounted <- get_values(results, discounted = FALSE)

  # Get discounted
  values_discounted <- get_values(results, discounted = TRUE)

  # Should have same structure
  expect_equal(colnames(values_undiscounted), colnames(values_discounted))

  # Discounted values should generally be less than or equal to undiscounted
  # (at least for later cycles)
  # Note: First cycle might be equal
})

test_that("get_summaries works with aggregated data", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  summaries <- get_summaries(results, group = "aggregated")

  expect_s3_class(summaries, "data.frame")
  expect_true("strategy" %in% colnames(summaries))
  expect_true("group" %in% colnames(summaries))
  expect_true("summary" %in% colnames(summaries))
  expect_true("value" %in% colnames(summaries))
  expect_true("amount" %in% colnames(summaries))
  expect_equal(unique(summaries$group), "Aggregated")
})

test_that("get_summaries filters by value type", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get cost summaries (default uses display names)
  cost_summaries <- get_summaries(results, value_type = "cost")
  # Just check we got 4 cost values
  expect_equal(length(unique(cost_summaries$value)), 4)

  # Verify technical names work with explicit parameter
  cost_summaries_tech <- get_summaries(results, value_type = "cost", value_name_field = "name")
  expect_true(all(cost_summaries_tech$value %in% c("cost_drug", "cost_admin", "cost_ae", "cost_prog")))

  # Get outcome summaries (default uses display names)
  outcome_summaries <- get_summaries(results, value_type = "outcome")
  # Should have at least one outcome value
  expect_true(length(unique(outcome_summaries$value)) >= 1)

  # Verify technical names work with explicit parameter
  outcome_summaries_tech <- get_summaries(results, value_type = "outcome", value_name_field = "name")
  expect_true(all(outcome_summaries_tech$value %in% c("qalys")))
})

test_that("filter_by_value_type helper works correctly", {
  # Create mock metadata
  metadata <- list(
    values = data.frame(
      name = c("cost1", "cost2", "outcome1", "outcome2"),
      type = c("cost", "cost", "outcome", "outcome"),
      stringsAsFactors = FALSE
    )
  )

  columns <- c("cost1", "cost2", "outcome1", "outcome2", "other")

  # Filter costs
  cost_cols <- heRomod2:::filter_by_value_type(columns, metadata, "cost")
  expect_equal(cost_cols, c("cost1", "cost2"))

  # Filter outcomes
  outcome_cols <- heRomod2:::filter_by_value_type(columns, metadata, "outcome")
  expect_equal(outcome_cols, c("outcome1", "outcome2"))

  # All
  all_cols <- heRomod2:::filter_by_value_type(columns, metadata, "all")
  expect_equal(all_cols, columns)
})

test_that("map_value_names helper works correctly", {
  # Create mock metadata
  metadata <- list(
    values = data.frame(
      name = c("cost_drug", "qalys"),
      display_name = c("Drug Cost", "QALYs"),
      abbreviation = c("Drug", "Q"),
      stringsAsFactors = FALSE
    )
  )

  names_technical <- c("cost_drug", "qalys", "unknown")

  # Map to display names
  display <- heRomod2:::map_value_names(names_technical, metadata, "display_name")
  expect_equal(display, c("Drug Cost", "QALYs", "unknown"))

  # Map to abbreviations
  abbrev <- heRomod2:::map_value_names(names_technical, metadata, "abbreviation")
  expect_equal(abbrev, c("Drug", "Q", "unknown"))

  # Map to names (identity)
  names_out <- heRomod2:::map_value_names(names_technical, metadata, "name")
  expect_equal(names_out, names_technical)
})
