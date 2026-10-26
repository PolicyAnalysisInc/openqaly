# Efficiency frontier of calculate_incremental_ce(): strict and extended
# dominance, including equal-cost strategies.

# Run the package's own calculate_incremental_ce on chosen costs/outcomes.
# Only the summary lookup is mocked; the frontier logic runs unchanged.
run_frontier <- function(labels, costs, outs) {
  mock <- function(results, groups, strategies = NULL, summaries, value_type, discounted, ...) {
    vals <- if (value_type == "cost") costs else outs
    data.frame(strategy = labels, group = "overall", amount = vals, stringsAsFactors = FALSE)
  }
  f <- get("calculate_incremental_ce", envir = asNamespace("openqaly"))
  environment(f) <- list2env(list(get_summaries = mock), parent = asNamespace("openqaly"))
  r <- as.data.frame(f(NULL, "outcome", "cost"))
  r$icer <- as.numeric(r$icer)
  r
}

# an extendedly dominated strategy must not stay on the frontier.
test_that("frontier for A-D is A -> D with D's ICER = 22/3", {
  fr <- run_frontier(c("A", "B", "C", "D"), c(0, 10, 21, 22), c(0, 1, 2, 3))
  expect_setequal(fr$strategy[fr$on_frontier], c("A", "D"))
  expect_equal(fr$icer[fr$strategy == "D"], 22 / 3)
  expect_true(all(fr$extendedly_dominated[fr$strategy %in% c("B", "C")]))
})

# equal cost, X less effective than Y, so X is strictly dominated.
test_that("equal-cost case 1 - less effective X is strictly dominated", {
  fr <- run_frontier(c("X", "Y"), c(5, 5), c(1, 2))
  expect_true(fr$strictly_dominated[fr$strategy == "X"])
  expect_true(fr$on_frontier[fr$strategy == "Y"])
})

# A and B both cost 0, B more effective, so A is strictly dominated.
test_that("equal-cost case 2 - less effective A is strictly dominated", {
  fr <- run_frontier(c("A", "B", "C"), c(0, 0, 10), c(1, 2, 3))
  expect_true(fr$strictly_dominated[fr$strategy == "A"])
  expect_false(fr$on_frontier[fr$strategy == "A"])
})

# B and C both cost 10, C more effective, so B is strictly (not
# extendedly) dominated.
test_that("equal-cost case 3 - B is strictly, not extendedly, dominated", {
  fr <- run_frontier(c("A", "B", "C"), c(0, 10, 10), c(0, 1, 2))
  expect_true(fr$strictly_dominated[fr$strategy == "B"])
  expect_false(fr$extendedly_dominated[fr$strategy == "B"])
})
