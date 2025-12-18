# Comprehensive Integration Tests for Discounting
# Uses pure JSON models defined as strings to verify EXACT mathematical correctness

library(testthat)
library(heRomod2)
library(jsonlite)

test_that("Discounting produces mathematically exact results", {
  # Pure JSON model with predictable values
  json_model <- '
{
  "settings": [
    {"setting": "model_type", "value": "markov"},
    {"setting": "timeframe", "value": "5"},
    {"setting": "timeframe_unit", "value": "years"},
    {"setting": "discount_cost", "value": "0.03"},
    {"setting": "discount_outcomes", "value": "0.015"},
    {"setting": "cycle_length", "value": "1"},
    {"setting": "cycle_length_unit", "value": "years"},
    {"setting": "half_cycle_method", "value": "start"},
    {"setting": "days_per_year", "value": "365"},
    {"setting": "reduce_state_cycle", "value": "false"}
  ],
  "strategies": [{"name": "base", "display_name": "Base", "description": "", "abbreviation": ""}],
  "groups": [],
  "states": [
    {"name": "Alive", "display_name": "Alive", "description": "", "initial_probability": "1", "state_group": "", "share_state_time": false, "state_cycle_limit": null, "state_cycle_limit_unit": ""},
    {"name": "Dead", "display_name": "Dead", "description": "", "initial_probability": "0", "state_group": "", "share_state_time": false, "state_cycle_limit": null, "state_cycle_limit_unit": ""}
  ],
  "transitions": [
    {"from_state": "Alive", "to_state": "Alive", "formula": "0.9"},
    {"from_state": "Alive", "to_state": "Dead", "formula": "0.1"},
    {"from_state": "Dead", "to_state": "Dead", "formula": "1"},
    {"from_state": "Dead", "to_state": "Alive", "formula": "0"}
  ],
  "values": [
    {"name": "cost", "display_name": "Cost", "description": "", "state": "Alive", "destination": "", "formula": "1000", "type": "cost"},
    {"name": "cost", "display_name": "Cost", "description": "", "state": "Dead", "destination": "", "formula": "0", "type": "cost"},
    {"name": "utility", "display_name": "Utility", "description": "", "state": "Alive", "destination": "", "formula": "0.8", "type": "outcome"},
    {"name": "utility", "display_name": "Utility", "description": "", "state": "Dead", "destination": "", "formula": "0", "type": "outcome"}
  ],
  "summaries": [
    {"name": "total_cost", "display_name": "Total Cost", "description": "", "values": "cost"},
    {"name": "total_utility", "display_name": "Total Utility", "description": "", "values": "utility"}
  ],
  "variables": [
    {"name": "discount_weight", "display_name": "Discount Weight", "description": "Weight for discounting", "formula": "1", "strategy": "", "group": ""}
  ],
  "trees": [],
  "tables": [],
  "scripts": []
}
'

  # Write JSON to temp file and read
  temp_file <- tempfile(fileext = ".json")
  writeLines(json_model, temp_file)
  model <- read_model_json(readLines(temp_file))
  unlink(temp_file)

  results <- run_model(model)

  # Expected state probabilities with 0.1 death per cycle
  alive_probs <- c(1.0, 0.9, 0.81, 0.729, 0.6561)

  # Discount factors
  cost_factors <- (1.03)^(-(0:4))
  utility_factors <- (1.015)^(-(0:4))

  # Expected discounted values
  expected_costs <- alive_probs * 1000 * cost_factors
  expected_utilities <- alive_probs * 0.8 * utility_factors

  # Get actual values
  values_disc <- results$aggregated$trace_and_values[[1]]$values_discounted

  # Test EXACT correctness for each cycle
  for(i in 1:5) {
    expect_equal(values_disc[i, "cost"], expected_costs[i],
                tolerance = 1e-10,
                label = paste("Cycle", i, "cost"))
    expect_equal(values_disc[i, "utility"], expected_utilities[i],
                tolerance = 1e-10,
                label = paste("Cycle", i, "utility"))
  }
})

test_that("Zero discount rate produces identical values", {
  json_model <- '
{
  "settings": [
    {"setting": "model_type", "value": "markov"},
    {"setting": "timeframe", "value": "5"},
    {"setting": "timeframe_unit", "value": "years"},
    {"setting": "discount_cost", "value": "0"},
    {"setting": "discount_outcomes", "value": "0"},
    {"setting": "cycle_length", "value": "1"},
    {"setting": "cycle_length_unit", "value": "years"},
    {"setting": "half_cycle_method", "value": "start"},
    {"setting": "days_per_year", "value": "365"},
    {"setting": "reduce_state_cycle", "value": "false"}
  ],
  "strategies": [{"name": "base", "display_name": "Base", "description": "", "abbreviation": ""}],
  "groups": [],
  "states": [
    {"name": "Alive", "display_name": "Alive", "description": "", "initial_probability": "1", "state_group": "", "share_state_time": false, "state_cycle_limit": null, "state_cycle_limit_unit": ""},
    {"name": "Dead", "display_name": "Dead", "description": "", "initial_probability": "0", "state_group": "", "share_state_time": false, "state_cycle_limit": null, "state_cycle_limit_unit": ""}
  ],
  "transitions": [
    {"from_state": "Alive", "to_state": "Alive", "formula": "0.9"},
    {"from_state": "Alive", "to_state": "Dead", "formula": "0.1"},
    {"from_state": "Dead", "to_state": "Dead", "formula": "1"},
    {"from_state": "Dead", "to_state": "Alive", "formula": "0"}
  ],
  "values": [
    {"name": "cost", "display_name": "Cost", "description": "", "state": "Alive", "destination": "", "formula": "1000", "type": "cost"},
    {"name": "cost", "display_name": "Cost", "description": "", "state": "Dead", "destination": "", "formula": "0", "type": "cost"},
    {"name": "utility", "display_name": "Utility", "description": "", "state": "Alive", "destination": "", "formula": "0.8", "type": "outcome"},
    {"name": "utility", "display_name": "Utility", "description": "", "state": "Dead", "destination": "", "formula": "0", "type": "outcome"}
  ],
  "summaries": [
    {"name": "total_cost", "display_name": "Total Cost", "description": "", "values": "cost"},
    {"name": "total_utility", "display_name": "Total Utility", "description": "", "values": "utility"}
  ],
  "variables": [
    {"name": "discount_weight", "display_name": "Discount Weight", "description": "Weight for discounting", "formula": "1", "strategy": "", "group": ""}
  ],
  "trees": [],
  "tables": [],
  "scripts": []
}
'

  temp_file <- tempfile(fileext = ".json")
  writeLines(json_model, temp_file)
  model <- read_model_json(readLines(temp_file))
  unlink(temp_file)

  results <- run_model(model)

  values <- results$aggregated$trace_and_values[[1]]$values
  values_disc <- results$aggregated$trace_and_values[[1]]$values_discounted

  # With 0% discount, values should be identical
  expect_equal(values, values_disc, tolerance = 1e-10)
})

test_that("Different discount rates for costs vs outcomes work correctly", {
  json_model <- '
{
  "settings": [
    {"setting": "model_type", "value": "markov"},
    {"setting": "timeframe", "value": "10"},
    {"setting": "timeframe_unit", "value": "years"},
    {"setting": "discount_cost", "value": "0.05"},
    {"setting": "discount_outcomes", "value": "0.01"},
    {"setting": "cycle_length", "value": "1"},
    {"setting": "cycle_length_unit", "value": "years"},
    {"setting": "half_cycle_method", "value": "start"},
    {"setting": "days_per_year", "value": "365"},
    {"setting": "reduce_state_cycle", "value": "false"}
  ],
  "strategies": [{"name": "base", "display_name": "Base", "description": "", "abbreviation": ""}],
  "groups": [],
  "states": [
    {"name": "Healthy", "display_name": "Healthy", "description": "", "initial_probability": "1", "state_group": "", "share_state_time": false, "state_cycle_limit": null, "state_cycle_limit_unit": ""}
  ],
  "transitions": [
    {"from_state": "Healthy", "to_state": "Healthy", "formula": "1"}
  ],
  "values": [
    {"name": "cost", "display_name": "Cost", "description": "", "state": "Healthy", "destination": "", "formula": "1000", "type": "cost"},
    {"name": "qaly", "display_name": "QALY", "description": "", "state": "Healthy", "destination": "", "formula": "1", "type": "outcome"}
  ],
  "summaries": [],
  "variables": [
    {"name": "discount_weight", "display_name": "Discount Weight", "description": "Weight for discounting", "formula": "1", "strategy": "", "group": ""}
  ],
  "trees": [],
  "tables": [],
  "scripts": []
}
'

  temp_file <- tempfile(fileext = ".json")
  writeLines(json_model, temp_file)
  model <- read_model_json(readLines(temp_file))
  unlink(temp_file)

  results <- run_model(model)
  values_disc <- results$aggregated$trace_and_values[[1]]$values_discounted

  # With constant values and no deaths, verify discount factors directly
  cost_factors <- (1.05)^(-(0:9))
  qaly_factors <- (1.01)^(-(0:9))

  for(i in 1:10) {
    expect_equal(values_disc[i, "cost"], 1000 * cost_factors[i],
                tolerance = 1e-10,
                label = paste("Cycle", i, "cost with 5% discount"))
    expect_equal(values_disc[i, "qaly"], 1 * qaly_factors[i],
                tolerance = 1e-10,
                label = paste("Cycle", i, "qaly with 1% discount"))
  }

  # Cost should be discounted more than QALYs
  total_cost_disc <- sum(values_disc[, "cost"])
  total_qaly_disc <- sum(values_disc[, "qaly"])

  cost_reduction <- 1 - (total_cost_disc / 10000)
  qaly_reduction <- 1 - (total_qaly_disc / 10)

  expect_gt(cost_reduction, qaly_reduction)
})

test_that("High discount rate (50%) works correctly", {
  json_model <- '
{
  "settings": [
    {"setting": "model_type", "value": "markov"},
    {"setting": "timeframe", "value": "5"},
    {"setting": "timeframe_unit", "value": "years"},
    {"setting": "discount_cost", "value": "0.5"},
    {"setting": "discount_outcomes", "value": "0.5"},
    {"setting": "cycle_length", "value": "1"},
    {"setting": "cycle_length_unit", "value": "years"},
    {"setting": "half_cycle_method", "value": "start"},
    {"setting": "days_per_year", "value": "365"},
    {"setting": "reduce_state_cycle", "value": "false"}
  ],
  "strategies": [{"name": "base", "display_name": "Base", "description": "", "abbreviation": ""}],
  "groups": [],
  "states": [
    {"name": "Alive", "display_name": "Alive", "description": "", "initial_probability": "1", "state_group": "", "share_state_time": false, "state_cycle_limit": null, "state_cycle_limit_unit": ""}
  ],
  "transitions": [
    {"from_state": "Alive", "to_state": "Alive", "formula": "1"}
  ],
  "values": [
    {"name": "cost", "display_name": "Cost", "description": "", "state": "Alive", "destination": "", "formula": "1000", "type": "cost"},
    {"name": "utility", "display_name": "Utility", "description": "", "state": "Alive", "destination": "", "formula": "1", "type": "outcome"}
  ],
  "summaries": [],
  "variables": [
    {"name": "discount_weight", "display_name": "Discount Weight", "description": "Weight for discounting", "formula": "1", "strategy": "", "group": ""}
  ],
  "trees": [],
  "tables": [],
  "scripts": []
}
'

  temp_file <- tempfile(fileext = ".json")
  writeLines(json_model, temp_file)
  model <- read_model_json(readLines(temp_file))
  unlink(temp_file)

  results <- run_model(model)
  values_disc <- results$aggregated$trace_and_values[[1]]$values_discounted

  # With 50% discount rate
  expected_factors <- (1.5)^(-(0:4))

  for(i in 1:5) {
    expect_equal(values_disc[i, "cost"], 1000 * expected_factors[i],
                tolerance = 1e-10,
                label = paste("Cycle", i, "cost with 50% discount"))
    expect_equal(values_disc[i, "utility"], 1 * expected_factors[i],
                tolerance = 1e-10,
                label = paste("Cycle", i, "utility with 50% discount"))
  }

  # By cycle 5, values should be heavily discounted
  expect_lt(values_disc[5, "cost"], 200)
})