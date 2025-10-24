context("Incremental Cost-Effectiveness Analysis")

# ============================================================================
# Tests for calculate_incremental_ce() function
# ============================================================================

test_that("calculate_incremental_ce() requires valid results object", {
  expect_error(
    calculate_incremental_ce(NULL, "qalys", "costs"),
    "object"
  )
})

test_that("calculate_incremental_ce() sorts strategies by cost", {
  # Create mock results with strategies in random cost order
  # This would need actual model results - placeholder for integration test
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() calculates incremental values correctly", {
  # Test with known cost/outcome values
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() identifies strictly dominated strategies", {
  # More costly, less effective strategies should be marked as strictly_dominated
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() identifies extendedly dominated strategies", {
  # Strategies with ICER > next strategy's ICER should be marked as extendedly_dominated
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() handles single strategy", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() handles two strategies", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() recalculates after removing extended dominance", {
  # Final ICERs should be vs. previous frontier strategy after removals
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() handles group parameter correctly", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() filters strategies correctly", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() uses name fields correctly", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() handles discounted parameter", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() returns correct column structure", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() handles ties in cost", {
  # When multiple strategies have same cost, should sort by outcome
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() first strategy has NA deltas", {
  skip("Requires actual model results for integration testing")
})

test_that("calculate_incremental_ce() ICER uses icer class", {
  skip("Requires actual model results for integration testing")
})


# ============================================================================
# Tests for incremental_ce_plot() function
# ============================================================================

test_that("incremental_ce_plot() creates ggplot object", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() handles group=aggregated", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() handles group=NULL with faceting", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() handles specific group", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() shows frontier line segments", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() colors strategies by status", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() uses name fields for labels", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_plot() filters strategies", {
  skip("Requires actual model results for integration testing")
})


# ============================================================================
# Tests for incremental_ce_table() function
# ============================================================================

test_that("incremental_ce_table() creates table object", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() handles kable backend", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() handles flextable backend", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() handles group=aggregated", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() handles group=NULL (three-level)", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() handles specific group", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() formats ICER column correctly", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() formats numeric columns with decimals", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() shows first strategy with — for ICER", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() shows dominated status correctly", {
  # Should show dominated column as TRUE for strictly or extendedly dominated strategies
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() uses name fields", {
  skip("Requires actual model results for integration testing")
})

test_that("incremental_ce_table() filters strategies", {
  skip("Requires actual model results for integration testing")
})


# ============================================================================
# Integration tests with actual model results
# ============================================================================

# Note: These tests require actual model results to be meaningful.
# They should be filled in once the basic functionality is confirmed working.

test_that("Full incremental CE workflow with example model", {
  skip_if_not_installed("heRomod2")

  # This test should:
  # 1. Load an example model
  # 2. Run the model
  # 3. Calculate incremental CE
  # 4. Create plot
  # 5. Create table
  # 6. Verify all results are consistent

  skip("Integration test - to be implemented with actual model")
})
