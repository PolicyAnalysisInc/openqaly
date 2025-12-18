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