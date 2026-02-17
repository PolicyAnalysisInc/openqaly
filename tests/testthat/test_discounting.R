context("Discounting")

library(jsonlite)

# =============================================================================
# Unit Tests: calculate_discount_factors
# =============================================================================

test_that("calculate_discount_factors generates correct factors", {
  # Test with 5% discount rate for 5 annual cycles
  factors <- calculate_discount_factors(5, 0.05, cycle_length_years = 1)
  expected <- c(1, 1/1.05, 1/1.05^2, 1/1.05^3, 1/1.05^4)
  expect_equal(factors, expected, tolerance = 1e-10)

  # Test with 0% discount rate (no discounting)
  factors_zero <- calculate_discount_factors(5, 0, cycle_length_years = 1)
  expect_equal(factors_zero, rep(1, 5))

  # Test with 3.5% discount rate (standard health economics rate)
  factors_35 <- calculate_discount_factors(3, 0.035, cycle_length_years = 1)
  expected_35 <- c(1, 1/1.035, 1/1.035^2)
  expect_equal(factors_35, expected_35, tolerance = 1e-10)

  # Test edge case: 0 cycles
  factors_empty <- calculate_discount_factors(0, 0.05, cycle_length_years = 1)
  expect_equal(length(factors_empty), 0)

  # Test edge case: 1 cycle (should be 1)
  factors_one <- calculate_discount_factors(1, 0.05, cycle_length_years = 1)
  expect_equal(factors_one, 1)

  # Test with monthly cycles (cycle_length_years = 1/12)
  factors_monthly <- calculate_discount_factors(12, 0.05, cycle_length_years = 1 / 12)
  # Month 1 is at time 0, Month 12 is at time 11/12
  expected_monthly <- c(1, 1/1.05^(1/12), 1/1.05^(2/12), 1/1.05^(3/12),
                       1/1.05^(4/12), 1/1.05^(5/12), 1/1.05^(6/12),
                       1/1.05^(7/12), 1/1.05^(8/12), 1/1.05^(9/12),
                       1/1.05^(10/12), 1/1.05^(11/12))
  expect_equal(factors_monthly, expected_monthly, tolerance = 1e-10)

  # Test with weekly cycles (cycle_length_years = 1/52)
  factors_weekly <- calculate_discount_factors(4, 0.05, cycle_length_years = 1/52)
  expected_weekly <- c(1, 1/1.05^(1/52), 1/1.05^(2/52), 1/1.05^(3/52))
  expect_equal(factors_weekly, expected_weekly, tolerance = 1e-10)
})

# =============================================================================
# Unit Tests: apply_discounting
# =============================================================================

test_that("apply_discounting correctly applies discount factors", {
  # Create a simple values matrix
  values_matrix <- matrix(
    c(100, 100, 100,  # Cost column
      50, 50, 50),     # Outcome column
    nrow = 3,
    ncol = 2
  )
  colnames(values_matrix) <- c("cost1", "qaly1")

  # Define value types
  types <- c(cost1 = "cost", qaly1 = "outcome")

  # Calculate discount factors (annual cycles)
  discount_factors_cost <- calculate_discount_factors(3, 0.03, cycle_length_years = 1)
  discount_factors_outcomes <- calculate_discount_factors(
    3, 0.015, cycle_length_years = 1
  )

  # Apply discounting
  discounted <- apply_discounting(
    values_matrix,
    discount_factors_cost,
    discount_factors_outcomes,
    types
  )

  # Check dimensions preserved
  expect_equal(dim(discounted), dim(values_matrix))
  expect_equal(colnames(discounted), colnames(values_matrix))

  # Check first cycle not discounted
  expect_equal(as.numeric(discounted[1, 1]), 100)
  expect_equal(as.numeric(discounted[1, 2]), 50)

  # Check discounting applied correctly
  expect_equal(as.numeric(discounted[2, 1]), 100 / 1.03, tolerance = 1e-10)
  expect_equal(as.numeric(discounted[2, 2]), 50 / 1.015, tolerance = 1e-10)
  expect_equal(as.numeric(discounted[3, 1]), 100 / 1.03^2, tolerance = 1e-10)
  expect_equal(as.numeric(discounted[3, 2]), 50 / 1.015^2, tolerance = 1e-10)
})

test_that("apply_discounting handles data frames", {
  # Create a data frame
  values_df <- data.frame(
    cost1 = c(100, 100, 100),
    qaly1 = c(50, 50, 50)
  )

  types <- c(cost1 = "cost", qaly1 = "outcome")
  discount_factors_cost <- calculate_discount_factors(3, 0.03)
  discount_factors_outcomes <- calculate_discount_factors(3, 0.015)

  discounted <- apply_discounting(
    values_df,
    discount_factors_cost,
    discount_factors_outcomes,
    types
  )

  # Should return a data frame
  expect_true(is.data.frame(discounted))
  expect_equal(dim(discounted), dim(values_df))
  expect_equal(colnames(discounted), colnames(values_df))
})

test_that("apply_discounting uses default 'outcome' type when not specified", {
  values_matrix <- matrix(
    c(100, 100, 100),
    nrow = 3,
    ncol = 1
  )
  colnames(values_matrix) <- c("unknown_value")

  # No value types specified
  discount_factors_cost <- calculate_discount_factors(3, 0.03)
  discount_factors_outcomes <- calculate_discount_factors(3, 0.015)

  discounted <- apply_discounting(
    values_matrix,
    discount_factors_cost,
    discount_factors_outcomes,
    NULL  # No value types
  )

  # Should use outcomes discount rate by default
  expect_equal(as.numeric(discounted[2, 1]), 100 / 1.015, tolerance = 1e-10)
})

test_that("apply_discounting handles mismatched discount factor lengths", {
  values_matrix <- matrix(
    c(100, 100, 100, 100, 100),
    nrow = 5,
    ncol = 1
  )
  colnames(values_matrix) <- c("cost1")

  types <- c(cost1 = "cost")

  # Only 3 discount factors for 5 cycles
  discount_factors_cost <- calculate_discount_factors(3, 0.03)
  discount_factors_outcomes <- calculate_discount_factors(3, 0.015)

  expect_warning(
    discounted <- apply_discounting(
      values_matrix,
      discount_factors_cost,
      discount_factors_outcomes,
      types
    ),
    "Discount factors length"
  )

  # Should apply available factors and pad with 1s
  expect_equal(as.numeric(discounted[1, 1]), 100)
  expect_equal(as.numeric(discounted[2, 1]), 100 / 1.03, tolerance = 1e-10)
  expect_equal(as.numeric(discounted[3, 1]), 100 / 1.03^2, tolerance = 1e-10)
  expect_equal(as.numeric(discounted[4, 1]), 100)  # No discounting (padded with 1)
  expect_equal(as.numeric(discounted[5, 1]), 100)  # No discounting (padded with 1)
})

test_that("discounting produces same results with 0% rate", {
  values_matrix <- matrix(
    c(100, 200, 300,
      50, 60, 70),
    nrow = 3,
    ncol = 2
  )
  colnames(values_matrix) <- c("cost1", "qaly1")

  types <- c(cost1 = "cost", qaly1 = "outcome")

  # 0% discount rate
  discount_factors_zero <- calculate_discount_factors(3, 0, cycle_length_years = 1)

  discounted <- apply_discounting(
    values_matrix,
    discount_factors_zero,
    discount_factors_zero,
    types
  )

  # Should be identical to original
  expect_equal(discounted, values_matrix)
})

# =============================================================================
# Cycle Length Tests
# =============================================================================

test_that("Discounting correctly accounts for cycle length", {
  # Annual discount rate
  annual_rate <- 0.05

  # Test 1: Annual cycles - after 1 year, discount factor should be 1/1.05
  annual_factors <- calculate_discount_factors(2, annual_rate, cycle_length_years = 1)
  expect_equal(annual_factors[1], 1)  # Year 0
  expect_equal(annual_factors[2], 1/1.05, tolerance = 1e-10)  # Year 1

  # Test 2: Monthly cycles - after 12 months (1 year), cumulative discounting should match annual
  monthly_factors <- calculate_discount_factors(13, annual_rate, cycle_length_years = 1/12)
  expect_equal(monthly_factors[1], 1)  # Month 1 (time 0)
  expect_equal(monthly_factors[13], 1/1.05, tolerance = 1e-10)  # Month 13 (time 1 year)

  # Test 3: Weekly cycles - after 52 weeks (1 year), cumulative discounting should match annual
  weekly_factors <- calculate_discount_factors(53, annual_rate, cycle_length_years = 1/52)
  expect_equal(weekly_factors[1], 1)  # Week 1 (time 0)
  expect_equal(weekly_factors[53], 1/1.05, tolerance = 1e-10)  # Week 53 (time 1 year)

  # Test 4: Quarterly cycles
  quarterly_factors <- calculate_discount_factors(5, annual_rate, cycle_length_years = 0.25)
  expect_equal(quarterly_factors[1], 1)  # Q1 (time 0)
  expect_equal(quarterly_factors[2], 1/1.05^0.25, tolerance = 1e-10)  # Q2 (time 0.25 years)
  expect_equal(quarterly_factors[5], 1/1.05, tolerance = 1e-10)  # Q5 (time 1 year)
})

test_that("Monthly discounting produces correct progression", {
  annual_rate <- 0.035  # Standard health economics rate
  monthly_factors <- calculate_discount_factors(24, annual_rate, cycle_length_years = 1/12)

  # Check specific months
  expect_equal(monthly_factors[1], 1)  # Month 1 (time 0)

  # After 6 months (0.5 years)
  expect_equal(monthly_factors[7], 1/1.035^0.5, tolerance = 1e-10)

  # After 1 year
  expect_equal(monthly_factors[13], 1/1.035, tolerance = 1e-10)

  # After 1.5 years
  expect_equal(monthly_factors[19], 1/1.035^1.5, tolerance = 1e-10)

  # After 2 years (minus 1 month, since we start at month 1)
  expect_equal(monthly_factors[24], 1/1.035^(23/12), tolerance = 1e-10)
})

test_that("Discounting with different cycle lengths produces consistent present values", {
  # Set up a simple cashflow: $100 per year, paid in different frequencies
  annual_total <- 100
  annual_rate <- 0.05

  # Annual: $100 at start of each year for 2 years
  annual_factors <- calculate_discount_factors(2, annual_rate, cycle_length_years = 1)
  annual_pv <- sum(annual_total * annual_factors)

  # Monthly: $100/12 at start of each month for 24 months
  # This gives the same total annual cashflow
  monthly_payment <- annual_total / 12
  monthly_factors <- calculate_discount_factors(24, annual_rate, cycle_length_years = 1/12)
  # Sum up monthly payments for each year
  monthly_pv_year1 <- sum(monthly_payment * monthly_factors[1:12])
  monthly_pv_year2 <- sum(monthly_payment * monthly_factors[13:24])
  monthly_pv_total <- monthly_pv_year1 + monthly_pv_year2

  # The total PVs should be close but not identical
  # Monthly payments throughout the year vs lump sum at start of year
  expect_gt(monthly_pv_total, annual_pv * 0.97)  # Within reasonable range
  expect_lt(monthly_pv_total, annual_pv * 1.00)  # Monthly should be slightly less (payments spread out)
})

test_that("Very short cycle lengths work correctly", {
  # Daily cycles (366 days to get to day 366 which is at time = 1 year)
  annual_rate <- 0.05
  daily_factors <- calculate_discount_factors(366, annual_rate, cycle_length_years = 1/365.25)

  # Day 1 is at time 0, Day 366 is at time 365/365.25 ≈ 1 year
  expect_equal(daily_factors[1], 1)  # Day 1 (time 0)
  expect_equal(daily_factors[366], 1/1.05^(365/365.25), tolerance = 0.001)  # After 365 days

  # Check that discounting increases smoothly
  for (i in 2:365) {
    expect_lt(daily_factors[i], daily_factors[i-1])
  }
})

# =============================================================================
# Integration Tests
# =============================================================================

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
    {"setting": "discount_cost", "value": "0.50"},
    {"setting": "discount_outcomes", "value": "0.50"},
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
