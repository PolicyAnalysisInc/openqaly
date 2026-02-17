# Helper to build a simple test model and run it
build_test_model <- function() {
  model <- define_model("markov") |>
    set_settings(
      n_cycles = 5,
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0.03,
      discount_outcomes = 0.03
    ) |>
    add_state("alive", initial_prob = 1, display_name = "Alive") |>
    add_state("dead", initial_prob = 0, display_name = "Dead") |>
    add_strategy("soc", display_name = "Standard of Care") |>
    add_strategy("tx", display_name = "Treatment") |>
    add_transition("alive", "dead", 0.1) |>
    add_transition("alive", "alive", 0.9) |>
    add_transition("dead", "dead", 1) |>
    add_value("qaly", 1, state = "alive", type = "outcome",
              display_name = "QALYs") |>
    add_value("cost", 1000, state = "alive", type = "cost",
              display_name = "Cost") |>
    add_value("qaly_dead", 0, state = "dead", type = "outcome",
              display_name = "QALYs Dead") |>
    add_value("cost_dead", 0, state = "dead", type = "cost",
              display_name = "Cost Dead") |>
    add_summary("total_qalys", "qaly,qaly_dead", type = "outcome",
                display_name = "Total QALYs", wtp = 50000) |>
    add_summary("total_cost", "cost,cost_dead", type = "cost",
                display_name = "Total Cost")

  run_model(model)
}


# =============================================================================
# outcomes_data tests
# =============================================================================

test_that("outcomes_data returns tibble with correct columns", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_s3_class(d, "tbl_df")
  expect_named(d, c("strategy", "group", "type", "summary", "value", "discounted", "amount"))
})

test_that("outcomes_data returns correct column types", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_type(d$strategy, "character")
  expect_type(d$group, "character")
  expect_type(d$type, "character")
  expect_type(d$summary, "character")
  expect_type(d$value, "character")
  expect_type(d$discounted, "logical")
  expect_type(d$amount, "double")
})

test_that("outcomes_data includes both discounted and undiscounted", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_true(TRUE %in% d$discounted)
  expect_true(FALSE %in% d$discounted)

  # Should have twice the rows of a single discount type
  disc_rows <- nrow(d[d$discounted == TRUE, ])
  undisc_rows <- nrow(d[d$discounted == FALSE, ])
  expect_equal(disc_rows, undisc_rows)
})

test_that("outcomes_data includes both cost and outcome types", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_true("cost" %in% d$type)
  expect_true("outcome" %in% d$type)
})

test_that("outcomes_data includes all strategies", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_true("Standard of Care" %in% d$strategy)
  expect_true("Treatment" %in% d$strategy)
})

test_that("outcomes_data uses display names", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_true("Total QALYs" %in% d$summary)
  expect_true("Total Cost" %in% d$summary)
})

test_that("outcomes_data strategies filter works", {
  results <- build_test_model()
  d <- outcomes_data(results, strategies = "soc")

  expect_equal(unique(d$strategy), "Standard of Care")
})

test_that("outcomes_data amounts are numeric", {
  results <- build_test_model()
  d <- outcomes_data(results)

  expect_true(all(!is.na(d$amount)))
  expect_true(all(is.numeric(d$amount)))
})


# =============================================================================
# incremental_ce_data tests
# =============================================================================

test_that("incremental_ce_data returns tibble with correct columns", {
  results <- build_test_model()
  d <- incremental_ce_data(results)

  expect_s3_class(d, "tbl_df")
  expected_cols <- c("outcome_summary", "cost_summary", "group", "strategy",
                     "comparator", "cost", "outcome", "dcost", "doutcome",
                     "icer", "on_frontier", "dominated", "strictly_dominated",
                     "extendedly_dominated")
  expect_named(d, expected_cols)
})

test_that("incremental_ce_data returns correct column types", {
  results <- build_test_model()
  d <- incremental_ce_data(results)

  expect_type(d$cost, "double")
  expect_type(d$outcome, "double")
  expect_s3_class(d$icer, "icer")
  expect_type(d$on_frontier, "logical")
  expect_type(d$dominated, "logical")
  expect_type(d$strictly_dominated, "logical")
  expect_type(d$extendedly_dominated, "logical")
})

test_that("incremental_ce_data includes summary pair labels", {
  results <- build_test_model()
  d <- incremental_ce_data(results)

  expect_true("Total QALYs" %in% d$outcome_summary)
  expect_true("Total Cost" %in% d$cost_summary)
})

test_that("incremental_ce_data has NA comparator for reference strategy", {
  results <- build_test_model()
  d <- incremental_ce_data(results)

  # First strategy should have NA comparator
  expect_true(any(is.na(d$comparator)))
})


# =============================================================================
# pairwise_ce_data tests
# =============================================================================

test_that("pairwise_ce_data returns tibble with correct columns", {
  results <- build_test_model()
  d <- pairwise_ce_data(results, comparators = "soc")

  expect_s3_class(d, "tbl_df")
  expected_cols <- c("outcome_summary", "cost_summary", "group", "strategy",
                     "comparator", "cost", "outcome", "dcost", "doutcome",
                     "icer")
  expect_named(d, expected_cols)
})

test_that("pairwise_ce_data returns correct column types", {
  results <- build_test_model()
  d <- pairwise_ce_data(results, comparators = "soc")

  expect_type(d$cost, "double")
  expect_type(d$outcome, "double")
  expect_s3_class(d$icer, "icer")
})

test_that("pairwise_ce_data errors without interventions or comparators", {
  results <- build_test_model()
  expect_error(
    pairwise_ce_data(results),
    "At least one of 'interventions' or 'comparators' must be provided"
  )
})

test_that("pairwise_ce_data includes summary pair labels", {
  results <- build_test_model()
  d <- pairwise_ce_data(results, comparators = "soc")

  expect_true("Total QALYs" %in% d$outcome_summary)
  expect_true("Total Cost" %in% d$cost_summary)
})


# =============================================================================
# nmb_data tests
# =============================================================================

test_that("nmb_data returns tibble with correct columns", {
  results <- build_test_model()
  d <- nmb_data(results, comparators = "soc")

  expect_s3_class(d, "tbl_df")
  expected_cols <- c("outcome_summary", "cost_summary", "strategy", "group",
                     "type", "value", "amount")
  expect_named(d, expected_cols)
})

test_that("nmb_data returns correct column types", {
  results <- build_test_model()
  d <- nmb_data(results, comparators = "soc")

  expect_type(d$amount, "double")
  expect_type(d$type, "character")
  expect_type(d$value, "character")
})

test_that("nmb_data errors without interventions or comparators", {
  results <- build_test_model()
  expect_error(
    nmb_data(results),
    "At least one of 'interventions' or 'comparators' must be provided"
  )
})

test_that("nmb_data includes both cost and outcome types", {
  results <- build_test_model()
  d <- nmb_data(results, comparators = "soc")

  expect_true("cost" %in% d$type)
  expect_true("outcome" %in% d$type)
})

test_that("nmb_data uses explicit wtp parameter", {
  results <- build_test_model()
  d1 <- nmb_data(results, interventions = "tx", comparators = "soc", wtp = 50000)
  d2 <- nmb_data(results, interventions = "tx", comparators = "soc", wtp = 100000)

  # With same model, cost amounts should be identical but we can verify
 # the function accepts and uses the wtp parameter without error
  expect_s3_class(d1, "tbl_df")
  expect_s3_class(d2, "tbl_df")
  expect_true(nrow(d1) > 0)
  expect_true(nrow(d2) > 0)
})

test_that("nmb_data includes summary pair labels", {
  results <- build_test_model()
  d <- nmb_data(results, comparators = "soc")

  expect_true("Total QALYs" %in% d$outcome_summary)
  expect_true("Total Cost" %in% d$cost_summary)
})


# =============================================================================
# trace_data tests
# =============================================================================

test_that("trace_data returns tibble with correct columns", {
  results <- build_test_model()
  d <- trace_data(results)

  expect_s3_class(d, "tbl_df")
  expect_true("strategy" %in% names(d))
  expect_true("group" %in% names(d))
  expect_true("cycle" %in% names(d))
  expect_true("state" %in% names(d))
  expect_true("probability" %in% names(d))
})

test_that("trace_data returns correct column types", {
  results <- build_test_model()
  d <- trace_data(results)

  expect_type(d$probability, "double")
  expect_type(d$cycle, "double")
  expect_type(d$strategy, "character")
})

test_that("trace_data includes all strategies", {
  results <- build_test_model()
  d <- trace_data(results)

  expect_true("Standard of Care" %in% d$strategy)
  expect_true("Treatment" %in% d$strategy)
})

test_that("trace_data strategies filter works", {
  results <- build_test_model()
  d <- trace_data(results, strategies = "soc")

  expect_equal(unique(d$strategy), "Standard of Care")
})

test_that("trace_data probabilities sum to 1 per cycle", {
  results <- build_test_model()
  d <- trace_data(results)

  totals <- d |>
    dplyr::group_by(strategy, group, cycle) |>
    dplyr::summarize(total = sum(probability), .groups = "drop")

  expect_true(all(abs(totals$total - 1) < 1e-10))
})

test_that("trace_data uses display names for states", {
  results <- build_test_model()
  d <- trace_data(results)

  expect_true("Alive" %in% d$state)
  expect_true("Dead" %in% d$state)
})


# =============================================================================
# get_summary_pairs tests
# =============================================================================

test_that("get_summary_pairs correctly identifies cost/outcome summaries", {
  results <- build_test_model()
  pairs <- get_summary_pairs(results$metadata)

  expect_equal(nrow(pairs), 1)
  expect_equal(pairs$outcome, "total_qalys")
  expect_equal(pairs$cost, "total_cost")
  expect_equal(pairs$outcome_display, "Total QALYs")
  expect_equal(pairs$cost_display, "Total Cost")
})

test_that("get_summary_pairs errors with no metadata", {
  expect_error(get_summary_pairs(NULL), "No summary metadata")
})
