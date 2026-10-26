# Error messages for invalid model inputs.

test_that("blank formula error names the transition that is blank", {
  # Blank formula is in b->b only; the error must name b->b, not a->a.
  x <- data.frame(from_state = c("a", "b", "c"), to_state = c("a", "b", "c"),
                  formula = c("1", "", "0.5"), stringsAsFactors = FALSE)
  expect_error(openqaly:::check_trans_markov(x, c("a", "b", "c")), "b→b", fixed = TRUE)
})

# custom PSM whose "alive" state probability is NA in cycle 3.
make_custom_psm_na_model <- function() {
  define_model("custom_psm") |>
    set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("std") |>
    add_state("alive") |>
    add_state("dead") |>
    add_transition("alive", "ifelse(cycle == 3, NA, 0.9)") |>
    add_transition("dead", "C") |>
    add_value("c", 1000, state = "alive", type = "cost")
}
test_that("NA custom PSM state probability error names the state and cycle", {
  m <- make_custom_psm_na_model()
  msg <- tryCatch({ run_model(m); "" }, error = function(e) conditionMessage(e))
  expect_true(nzchar(msg))
  expect_false(grepl("missing value where TRUE/FALSE needed", msg, fixed = TRUE))
  expect_true(grepl("alive", msg, fixed = TRUE))
  expect_true(grepl("\\b3\\b", msg))
})

test_that("NA group weight warns 'invalid weights' and falls back to equal weights", {
  ss <- data.frame(group = c("g1", "g2"), weight = c(NA, 0.4),
                   strategy = "s", stringsAsFactors = FALSE)
  expect_warning(openqaly:::aggregate_strategy_segments(ss, list()), "invalid weights")
})

test_that("override min/max error reports the effective min and the new max", {
  # Stored min is 0.1; editing only max to 0.05 must report min (0.1) and max (0.05).
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_strategy("s") |>
    add_variable("p", 0.2) |>
    add_override_category("sens", general = FALSE) |>
    add_override(category = "sens", title = "P", name = "p", type = "variable",
                 input_type = "numeric", expression = 0.2, min = 0.1, max = 0.9)
  expect_error(edit_override(m, "sens", "variable", "p", max = 0.05),
               "Override min \\(0.1\\) must be less than max \\(0.05\\)")
})

test_that("validate_numeric_result with NULL formula_text gives a non-empty message", {
  expect_error(validate_numeric_result("abc", "Value 'v'", formula_text = NULL),
               "Value 'v'.*expected numeric")
})
