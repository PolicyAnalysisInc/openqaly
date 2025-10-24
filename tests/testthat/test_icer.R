context("ICER Calculation and Printing")

# ============================================================================
# Tests for icer() function
# ============================================================================

test_that("icer() validates input lengths match", {
  expect_error(
    icer(c(100, 200), c(1)),
    "dcost and deffect must have the same length."
  )

  expect_error(
    icer(c(100), c(1, 2, 3)),
    "dcost and deffect must have the same length."
  )
})

test_that("icer() validates inputs are numeric", {
  expect_error(
    icer(c("100", "200"), c(1, 2)),
    "dcost and deffect must be numeric."
  )

  expect_error(
    icer(c(100, 200), c("1", "2")),
    "dcost and deffect must be numeric."
  )

  expect_error(
    icer(c(TRUE, FALSE), c(1, 2)),
    "dcost and deffect must be numeric."
  )
})

test_that("icer() identifies equivalent scenarios (ΔC=0, ΔE=0)", {
  result <- icer(dcost = 0, deffect = 0)
  expect_true(is.nan(result))
  expect_s3_class(result, "icer")

  # Multiple equivalent scenarios
  result <- icer(dcost = c(0, 0, 0), deffect = c(0, 0, 0))
  expect_true(all(is.nan(result)))
})

test_that("icer() identifies dominant scenarios", {
  # Less costly, more effective (ΔC<0, ΔE>0)
  result <- icer(dcost = -100, deffect = 1)
  expect_equal(as.numeric(result), 0)
  expect_s3_class(result, "icer")

  # Same cost, more effective (ΔC=0, ΔE>0)
  result <- icer(dcost = 0, deffect = 1)
  expect_equal(as.numeric(result), 0)

  # Less costly, same effect (ΔC<0, ΔE=0)
  result <- icer(dcost = -100, deffect = 0)
  expect_equal(as.numeric(result), 0)

  # Multiple dominant scenarios
  result <- icer(
    dcost = c(-100, 0, -50),
    deffect = c(1, 2, 0)
  )
  expect_equal(as.numeric(result), c(0, 0, 0))
})

test_that("icer() identifies dominated scenarios", {
  # More costly, less effective (ΔC>0, ΔE<0)
  result <- icer(dcost = 100, deffect = -1)
  expect_equal(as.numeric(result), Inf)
  expect_s3_class(result, "icer")

  # Same cost, less effective (ΔC=0, ΔE<0)
  result <- icer(dcost = 0, deffect = -1)
  expect_equal(as.numeric(result), Inf)

  # More costly, same effect (ΔC>0, ΔE=0)
  result <- icer(dcost = 100, deffect = 0)
  expect_equal(as.numeric(result), Inf)

  # Multiple dominated scenarios
  result <- icer(
    dcost = c(100, 0, 50),
    deffect = c(-1, -2, 0)
  )
  expect_equal(as.numeric(result), c(Inf, Inf, Inf))
})

test_that("icer() calculates positive ICER for NE quadrant (ΔC>0, ΔE>0)", {
  # Simple positive ICER
  result <- icer(dcost = 1000, deffect = 2)
  expect_equal(as.numeric(result), 500)
  expect_true(result > 0)
  expect_s3_class(result, "icer")

  # Multiple NE quadrant scenarios
  result <- icer(
    dcost = c(100, 200, 1000),
    deffect = c(1, 4, 10)
  )
  expect_equal(as.numeric(result), c(100, 50, 100))
  expect_true(all(result > 0))
})

test_that("icer() calculates negative values for SW quadrant (ΔC<0, ΔE<0)", {
  # Simple SW quadrant: less costly, less effective
  # ΔC = -1000, ΔE = -2 → ratio = 500, but stored as -500
  result <- icer(dcost = -1000, deffect = -2)
  expect_equal(as.numeric(result), -500)
  expect_true(result < 0)
  expect_s3_class(result, "icer")

  # Multiple SW quadrant scenarios
  result <- icer(
    dcost = c(-100, -200, -1000),
    deffect = c(-1, -4, -10)
  )
  expect_equal(as.numeric(result), c(-100, -50, -100))
  expect_true(all(result < 0))
})

test_that("icer() handles mixed scenarios in single vector", {
  result <- icer(
    dcost = c(0, -100, 100, 0, 100, -100, 1000, -1000),
    deffect = c(0, 1, -1, 1, 0, -1, 2, -2)
  )

  expect_true(is.nan(result[1]))      # Equivalent (ΔC=0, ΔE=0)
  expect_equal(as.numeric(result[2]), 0)       # Dominant (ΔC=-100, ΔE=1)
  expect_equal(as.numeric(result[3]), Inf)     # Dominated (ΔC=100, ΔE=-1)
  expect_equal(as.numeric(result[4]), 0)       # Dominant (ΔC=0, ΔE=1)
  expect_equal(as.numeric(result[5]), Inf)     # Dominated (ΔC=100, ΔE=0)
  expect_equal(as.numeric(result[6]), -100)    # SW quadrant (ΔC=-100, ΔE=-1)
  expect_equal(as.numeric(result[7]), 500)     # NE quadrant (ΔC=1000, ΔE=2)
  expect_equal(as.numeric(result[8]), -500)    # SW quadrant (ΔC=-1000, ΔE=-2)
})

test_that("icer() does not preserve names from input vectors", {
  # Names are not preserved by the icer() function
  dcost <- c(a = 100, b = -100, c = 0)
  deffect <- c(a = 1, b = 1, c = 0)

  result <- icer(dcost, deffect)
  expect_null(names(result))
})

test_that("icer() creates correct class structure", {
  result <- icer(dcost = 100, deffect = 1)
  expect_s3_class(result, "icer")
  expect_s3_class(result, "numeric")
  expect_equal(class(result), c("icer", "numeric"))
})

test_that("icer() handles edge cases with very small/large values", {
  # Very large ICER
  result <- icer(dcost = 1e10, deffect = 1)
  expect_equal(as.numeric(result), 1e10)

  # Very small ICER
  result <- icer(dcost = 1, deffect = 1e10)
  expect_equal(as.numeric(result), 1e-10)

  # Very small differences
  result <- icer(dcost = 1e-10, deffect = 1e-10)
  expect_equal(as.numeric(result), 1)
})

test_that("icer() handles NA inputs correctly", {
  # Single NA (reference strategy case)
  result <- icer(NA_real_, NA_real_)
  expect_true(is.na(result))
  expect_false(is.nan(result))
  expect_s3_class(result, "icer")

  # Mixed with valid values
  result <- icer(c(NA, 100, 50), c(NA, 1, -1))
  expect_true(is.na(result[1]))
  expect_equal(as.numeric(result[2]), 100)
  expect_equal(as.numeric(result[3]), Inf)

  # NA in dcost only
  result <- icer(c(NA, 100), c(1, 1))
  expect_true(is.na(result[1]))
  expect_equal(as.numeric(result[2]), 100)

  # NA in deffect only
  result <- icer(c(100, 100), c(NA, 1))
  expect_true(is.na(result[1]))
  expect_equal(as.numeric(result[2]), 100)

  # Vector of all NAs
  result <- icer(c(NA_real_, NA_real_, NA_real_), c(NA_real_, NA_real_, NA_real_))
  expect_true(all(is.na(result)))
  expect_s3_class(result, "icer")
})


# ============================================================================
# Tests for print.icer() function
# ============================================================================

test_that("print.icer() formats NaN as 'Equivalent'", {
  x <- icer(dcost = 0, deffect = 0)
  output <- capture.output(print(x))
  expect_match(output, "Equivalent")
})

test_that("print.icer() formats Inf as 'Dominated'", {
  x <- icer(dcost = 100, deffect = -1)
  output <- capture.output(print(x))
  expect_match(output, "Dominated")
})

test_that("print.icer() formats 0 as 'Dominant'", {
  x <- icer(dcost = -100, deffect = 1)
  output <- capture.output(print(x))
  expect_match(output, "Dominant")
})

test_that("print.icer() formats positive values correctly", {
  x <- icer(dcost = 1000, deffect = 2)  # ICER = 500
  output <- capture.output(print(x))
  expect_match(output, "500")
  expect_false(grepl("\\*", output))  # No asterisk for positive
})

test_that("print.icer() formats negative values with asterisk", {
  x <- icer(dcost = -1000, deffect = -2)  # Stored as -500
  output <- capture.output(print(x))
  expect_match(output, "500\\*")  # Should print as "500*"
})

test_that("print.icer() respects digits parameter", {
  x <- icer(dcost = 123.456789, deffect = 1)

  # Default digits = 3
  output <- capture.output(print(x, digits = 3))
  expect_match(output, "123")

  # Higher precision (rounds to 6 decimal places)
  # Note: Due to floating point precision, 123.456789 is stored as 123.4568
  output <- capture.output(print(x, digits = 6))
  expect_match(output, "123\\.4568")
})

test_that("print.icer() respects big.mark parameter", {
  x <- icer(dcost = 10000, deffect = 1)  # ICER = 10000

  # With comma separator
  output <- capture.output(print(x, big.mark = ","))
  expect_match(output, "10,000")

  # With space separator
  output <- capture.output(print(x, big.mark = " "))
  expect_match(output, "10 000")
})

test_that("print.icer() handles vectors even when input had names", {
  # Names are not preserved by icer(), so output is space-separated
  x <- icer(
    dcost = c(intervention1 = 100, intervention2 = -100),
    deffect = c(intervention1 = 1, intervention2 = 1)
  )
  output <- capture.output(print(x))

  # Should be on one line, space-separated (no names preserved)
  expect_length(output, 1)
  expect_match(output, "100")
  expect_match(output, "Dominant")
})

test_that("print.icer() handles named icer objects with name: value format", {
  # If we manually add names to an icer object, they should print properly
  x <- icer(
    dcost = c(100, -100),
    deffect = c(1, 1)
  )
  names(x) <- c("intervention1", "intervention2")
  output <- capture.output(print(x))

  # Should be on separate lines with name: value format
  expect_match(output[1], "intervention1: 100")
  expect_match(output[2], "intervention2: Dominant")
})

test_that("print.icer() handles unnamed vectors with space-separated format", {
  x <- icer(
    dcost = c(100, -100, 0),
    deffect = c(1, 1, 0)
  )
  output <- capture.output(print(x))

  # Should be on one line, space-separated
  expect_length(output, 1)
  expect_match(output, "100")
  expect_match(output, "Dominant")
  expect_match(output, "Equivalent")
})

test_that("print.icer() returns original object invisibly", {
  x <- icer(dcost = 100, deffect = 1)

  # Capture return value (invisible returns can be captured with withVisible)
  result <- withVisible(print(x))

  expect_false(result$visible)
  expect_identical(result$value, x)
})

test_that("print.icer() handles mixed scenarios formatting", {
  x <- icer(
    dcost = c(0, -100, 100, 1000, -1000),
    deffect = c(0, 1, -1, 2, -2)
  )
  output <- capture.output(print(x))

  # Should be on one line, space-separated
  expect_length(output, 1)
  expect_match(output, "Equivalent")
  expect_match(output, "Dominant")
  expect_match(output, "Dominated")
  expect_match(output, "500")
  expect_match(output, "500\\*")
})

test_that("print.icer() handles large numbers with big.mark", {
  x <- icer(dcost = 1234567.89, deffect = 1)
  output <- capture.output(print(x, digits = 2, big.mark = ","))

  # Should format large number with commas
  # Note: Due to floating point precision, 1234567.89 rounds to 1234568
  expect_match(output, "1,234,568")
})

test_that("print.icer() handles very small positive and negative ICERs", {
  x <- icer(
    dcost = c(0.001, -0.001),
    deffect = c(1, -1)
  )
  output <- capture.output(print(x, digits = 3))

  expect_match(output, "0\\.001")
  expect_match(output, "0\\.001\\*")
})

test_that("print.icer() displays NA as blank", {
  # Single NA
  x <- icer(NA_real_, NA_real_)
  output <- capture.output(print(x))
  # Should be essentially blank (just whitespace/newline)
  expect_true(nchar(trimws(output)) == 0)

  # Mixed with values
  x <- icer(c(NA, 100, -50, 0), c(NA, 1, -1, 0))
  output <- capture.output(print(x))
  # Should have blank for first, then values
  # Format is space-separated, so we check it contains the values but starts with spaces
  expect_match(output, "100")
  expect_match(output, "50\\*")
  expect_match(output, "Equivalent")
})
