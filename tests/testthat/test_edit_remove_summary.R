context("edit_summary and remove_summary")

# Helper to build a minimal model with summaries
make_test_model <- function() {
  define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("qaly", 1, state = "healthy", type = "outcome") |>
    add_summary("total_cost", "cost", type = "cost", display_name = "Total Cost", description = "Cost desc") |>
    add_summary("total_qaly", "qaly", type = "outcome", wtp = 50000, display_name = "Total QALY", description = "QALY desc")
}

# =============================================================================
# edit_summary — basic field updates
# =============================================================================

test_that("edit_summary updates values", {
  m <- make_test_model()
  m2 <- edit_summary(m, "total_cost", values = "cost,qaly")
  expect_equal(m2$summaries$values[m2$summaries$name == "total_cost"], "cost,qaly")
})

test_that("edit_summary updates display_name", {
  m <- make_test_model()
  m2 <- edit_summary(m, "total_cost", display_name = "New Display")
  expect_equal(m2$summaries$display_name[m2$summaries$name == "total_cost"], "New Display")
})

test_that("edit_summary updates description", {
  m <- make_test_model()
  m2 <- edit_summary(m, "total_cost", description = "New desc")
  expect_equal(m2$summaries$description[m2$summaries$name == "total_cost"], "New desc")
})

test_that("edit_summary updates type", {
  m <- make_test_model()
  m2 <- edit_summary(m, "total_qaly", type = "cost")
  expect_equal(m2$summaries$type[m2$summaries$name == "total_qaly"], "cost")
})

test_that("edit_summary rejects invalid type", {
  m <- make_test_model()
  expect_error(edit_summary(m, "total_cost", type = "invalid"),
               "must be 'outcome' or 'cost'")
})

test_that("edit_summary changing type to cost clears WTP", {
  m <- make_test_model()
  # total_qaly has wtp = 50000
  expect_equal(m$summaries$wtp[m$summaries$name == "total_qaly"], 50000)
  m2 <- edit_summary(m, "total_qaly", type = "cost")
  expect_true(is.na(m2$summaries$wtp[m2$summaries$name == "total_qaly"]))
})

test_that("edit_summary updates wtp on outcome summary", {
  m <- make_test_model()
  m2 <- edit_summary(m, "total_qaly", wtp = 100000)
  expect_equal(m2$summaries$wtp[m2$summaries$name == "total_qaly"], 100000)
})

test_that("edit_summary rejects wtp on cost summary", {
  m <- make_test_model()
  expect_error(edit_summary(m, "total_cost", wtp = 50000),
               "WTP cannot be specified for cost summary")
})

test_that("edit_summary rejects wtp when type is being changed to cost in same call", {
  m <- make_test_model()
  # Type changes to cost first (clearing wtp), then wtp assignment checks current type

  expect_error(edit_summary(m, "total_qaly", type = "cost", wtp = 50000),
               "WTP cannot be specified for cost summary")
})

test_that("edit_summary errors for nonexistent summary", {
  m <- make_test_model()
  expect_error(edit_summary(m, "nonexistent", display_name = "foo"),
               'Summary "nonexistent" not found')
})

# =============================================================================
# edit_summary — rename
# =============================================================================

test_that("edit_summary renames summary", {
  m <- make_test_model()
  m2 <- edit_summary(m, "total_cost", new_name = "all_costs")
  expect_true("all_costs" %in% m2$summaries$name)
  expect_false("total_cost" %in% m2$summaries$name)
})

test_that("edit_summary rename cascades to threshold analyses", {
  m <- make_test_model()
  # Add threshold analyses with different condition fields
  m$threshold_analyses <- list(
    list(name = "thresh1", condition = list(summary = "total_cost")),
    list(name = "thresh2", condition = list(health_summary = "total_qaly", cost_summary = "total_cost")),
    list(name = "thresh3", condition = list(summary = "other_summary"))
  )
  m2 <- edit_summary(m, "total_cost", new_name = "all_costs")
  expect_equal(m2$threshold_analyses[[1]]$condition$summary, "all_costs")
  expect_equal(m2$threshold_analyses[[2]]$condition$cost_summary, "all_costs")
  expect_equal(m2$threshold_analyses[[2]]$condition$health_summary, "total_qaly")
  expect_equal(m2$threshold_analyses[[3]]$condition$summary, "other_summary")
})

test_that("edit_summary rename cascades to VBP config", {
  m <- make_test_model()
  m$vbp <- list(outcome_summary = "total_qaly", cost_summary = "total_cost")
  m2 <- edit_summary(m, "total_qaly", new_name = "qalys_renamed")
  expect_equal(m2$vbp$outcome_summary, "qalys_renamed")
  expect_equal(m2$vbp$cost_summary, "total_cost")
})

test_that("edit_summary rename detects tree name collision", {
  m <- make_test_model()
  m$trees <- fast_tibble(name = "my_tree", type = "decision")
  expect_error(edit_summary(m, "total_cost", new_name = "my_tree"),
               "already used as a decision tree name")
})

# =============================================================================
# remove_summary — basic
# =============================================================================

test_that("remove_summary removes the summary", {
  m <- make_test_model()
  m2 <- remove_summary(m, "total_cost")
  expect_false("total_cost" %in% m2$summaries$name)
  expect_true("total_qaly" %in% m2$summaries$name)
})

test_that("remove_summary errors for nonexistent summary", {
  m <- make_test_model()
  expect_error(remove_summary(m, "nonexistent"),
               'Summary "nonexistent" not found')
})

# =============================================================================
# remove_summary — error_on_dependencies
# =============================================================================

test_that("remove_summary error_on_dependencies detects threshold analysis deps", {
  m <- make_test_model()
  m$threshold_analyses <- list(
    list(name = "thresh1", condition = list(summary = "total_cost"))
  )
  err <- tryCatch(
    remove_summary(m, "total_cost", error_on_dependencies = TRUE),
    summary_has_dependencies = function(e) e
  )
  expect_s3_class(err, "summary_has_dependencies")
  expect_true("threshold_analyses" %in% names(err$dependencies))
  expect_equal(err$dependencies$threshold_analyses, "thresh1")
})

test_that("remove_summary error_on_dependencies detects VBP deps", {
  m <- make_test_model()
  m$vbp <- list(outcome_summary = "total_qaly", cost_summary = "total_cost")
  err <- tryCatch(
    remove_summary(m, "total_qaly", error_on_dependencies = TRUE),
    summary_has_dependencies = function(e) e
  )
  expect_s3_class(err, "summary_has_dependencies")
  expect_true("vbp" %in% names(err$dependencies))
  expect_equal(err$dependencies$vbp, "outcome_summary")
})

test_that("remove_summary error_on_dependencies passes when no deps", {
  m <- make_test_model()
  m2 <- remove_summary(m, "total_cost", error_on_dependencies = TRUE)
  expect_false("total_cost" %in% m2$summaries$name)
})

# =============================================================================
# remove_summary — cascade
# =============================================================================

test_that("remove_summary cascades removal of threshold analyses", {
  m <- make_test_model()
  m$threshold_analyses <- list(
    list(name = "thresh1", condition = list(summary = "total_cost")),
    list(name = "thresh2", condition = list(health_summary = "total_qaly")),
    list(name = "thresh3", condition = list(summary = "other"))
  )
  m2 <- remove_summary(m, "total_cost")
  expect_length(m2$threshold_analyses, 2)
  expect_equal(m2$threshold_analyses[[1]]$name, "thresh2")
  expect_equal(m2$threshold_analyses[[2]]$name, "thresh3")
})

test_that("remove_summary clears VBP config field", {
  m <- make_test_model()
  m$vbp <- list(outcome_summary = "total_qaly", cost_summary = "total_cost")
  m2 <- remove_summary(m, "total_qaly")
  expect_equal(m2$vbp$outcome_summary, "")
  expect_equal(m2$vbp$cost_summary, "total_cost")
})
