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
  factors_monthly <- calculate_discount_factors(12, 0.05, cycle_length_years = 1/12)
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
  discount_factors_outcomes <- calculate_discount_factors(3, 0.015, cycle_length_years = 1)

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