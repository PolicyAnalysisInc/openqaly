context("edit/remove override categories and overrides")

# Helper to build a model with override categories and overrides
make_test_model <- function() {
  define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_strategy("treatment") |>
    add_strategy("control") |>
    add_group("male") |>
    add_group("female") |>
    add_variable("cost_drug", 100) |>
    add_variable("util_sick", 0.5) |>
    add_override_category("Costs", general = FALSE) |>
    add_override_category("Utilities", general = TRUE) |>
    add_override(
      category = "Costs", title = "Drug Cost", name = "cost_drug",
      type = "variable", input_type = "numeric", expression = 100,
      min = 0, max = 500
    ) |>
    add_override(
      category = "Costs", title = "Discount Rate", name = "discount_cost",
      type = "setting", input_type = "slider", expression = 0.03,
      min = 0, max = 0.1, step_size = 0.01
    ) |>
    add_override(
      category = "Utilities", title = "Sick Utility", name = "util_sick",
      type = "variable", input_type = "dropdown", expression = 0.5,
      options = list(
        override_option("Low", "0.3"),
        override_option("Mid", "0.5", is_base_case = TRUE),
        override_option("High", "0.7")
      )
    )
}

# =============================================================================
# edit_override_category
# =============================================================================

test_that("edit_override_category renames category", {
  m <- make_test_model()
  m2 <- edit_override_category(m, "Costs", new_name = "Drug Costs")
  expect_equal(m2$override_categories[[1]]$name, "Drug Costs")
})

test_that("edit_override_category updates general flag", {
  m <- make_test_model()
  m2 <- edit_override_category(m, "Costs", general = TRUE)
  expect_true(m2$override_categories[[1]]$general)
})

test_that("edit_override_category errors on duplicate name (case-insensitive)", {
  m <- make_test_model()
  expect_error(
    edit_override_category(m, "Costs", new_name = "utilities"),
    "already exists"
  )
})

test_that("edit_override_category errors when not found", {
  m <- make_test_model()
  expect_error(
    edit_override_category(m, "Nonexistent", new_name = "Foo"),
    "not found"
  )
})

test_that("edit_override_category errors on empty new_name", {
  m <- make_test_model()
  expect_error(
    edit_override_category(m, "Costs", new_name = ""),
    "non-empty"
  )
})

# =============================================================================
# remove_override_category
# =============================================================================

test_that("remove_override_category removes category with overrides", {
  m <- make_test_model()
  m2 <- remove_override_category(m, "Costs")
  expect_equal(length(m2$override_categories), 1)
  expect_equal(m2$override_categories[[1]]$name, "Utilities")
})

test_that("remove_override_category errors when not found", {
  m <- make_test_model()
  expect_error(
    remove_override_category(m, "Nonexistent"),
    "not found"
  )
})

# =============================================================================
# edit_override — simple field updates
# =============================================================================

test_that("edit_override updates title", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", title = "New Drug Cost")
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$title, "New Drug Cost")
})

test_that("edit_override updates description", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", description = "A description")
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$description, "A description")
})

test_that("edit_override updates general flag", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", general = TRUE)
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_true(ovr$general)
})

test_that("edit_override updates expression with numeric literal", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", expression = 200)
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$overridden_expression, "200")
})

test_that("edit_override updates expression with NSE formula", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", expression = base * 1.1)
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$overridden_expression, "base * 1.1")
})

# =============================================================================
# edit_override — input_type changes
# =============================================================================

test_that("edit_override changes input_type numeric to slider", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug",
                      input_type = "slider", step_size = 10)
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$input_type, "slider")
  expect_equal(ovr$input_config$step_size, 10)
  # Defaults for min/max when rebuilding slider
  expect_equal(ovr$input_config$min, 0)
  expect_equal(ovr$input_config$max, 1)
})

test_that("edit_override changes input_type slider to dropdown", {
  m <- make_test_model()
  opts <- list(override_option("A", "1"), override_option("B", "2"))
  m2 <- edit_override(m, "Costs", "setting", "discount_cost",
                      input_type = "dropdown", options = opts)
  ovr <- m2$override_categories[[1]]$overrides[[2]]
  expect_equal(ovr$input_type, "dropdown")
  expect_equal(length(ovr$input_config$options), 2)
})

test_that("edit_override changes input_type dropdown to formula", {
  m <- make_test_model()
  m2 <- edit_override(m, "Utilities", "variable", "util_sick",
                      input_type = "formula")
  ovr <- m2$override_categories[[2]]$overrides[[1]]
  expect_equal(ovr$input_type, "formula")
  expect_equal(ovr$input_config, list())
})

test_that("edit_override updates min/max/step_size without changing input_type", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "setting", "discount_cost",
                      min = 0.01, max = 0.2, step_size = 0.005)
  ovr <- m2$override_categories[[1]]$overrides[[2]]
  expect_equal(ovr$input_type, "slider")
  expect_equal(ovr$input_config$min, 0.01)
  expect_equal(ovr$input_config$max, 0.2)
  expect_equal(ovr$input_config$step_size, 0.005)
})

# =============================================================================
# edit_override — re-keying
# =============================================================================

test_that("edit_override re-keys name", {
  m <- make_test_model()
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", new_name = "util_sick")
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$name, "util_sick")
})

test_that("edit_override re-keys type variable to setting errors with strategy", {
  m <- make_test_model()
  # First add an override with strategy
  m2 <- add_override(m, "Costs", title = "Strat Drug", name = "cost_drug",
                     type = "variable", input_type = "numeric", expression = 50,
                     strategy = "treatment")
  expect_error(
    edit_override(m2, "Costs", "variable", "cost_drug", strategy = "treatment",
                  new_type = "setting", new_name = "discount_cost"),
    "Strategy and group cannot be specified for setting overrides"
  )
})

test_that("edit_override errors on duplicate composite key", {
  m <- make_test_model()
  # Add a second variable override
  m2 <- add_override(m, "Costs", title = "Sick Utility", name = "util_sick",
                     type = "variable", input_type = "numeric", expression = 0.5)
  expect_error(
    edit_override(m2, "Costs", "variable", "util_sick", new_name = "cost_drug"),
    "already exists"
  )
})

test_that("edit_override re-keys to different variable name", {
  m <- make_test_model()
  # validate_variable_targeting allows unknown variables (no error)
  m2 <- edit_override(m, "Costs", "variable", "cost_drug", new_name = "util_sick")
  ovr <- m2$override_categories[[1]]$overrides[[1]]
  expect_equal(ovr$name, "util_sick")
  expect_equal(ovr$type, "variable")
})

test_that("edit_override errors on setting with non-empty strategy", {
  m <- make_test_model()
  expect_error(
    edit_override(m, "Costs", "setting", "discount_cost",
                  new_strategy = "treatment"),
    "Strategy and group cannot be specified"
  )
})

# =============================================================================
# edit_override — error cases
# =============================================================================

test_that("edit_override errors when override not found", {
  m <- make_test_model()
  expect_error(
    edit_override(m, "Costs", "variable", "nonexistent"),
    "not found"
  )
})

test_that("edit_override errors when category not found", {
  m <- make_test_model()
  expect_error(
    edit_override(m, "Nonexistent", "variable", "cost_drug"),
    "not found"
  )
})

test_that("edit_override errors when min >= max", {
  m <- make_test_model()
  expect_error(
    edit_override(m, "Costs", "variable", "cost_drug", min = 500, max = 100),
    "min.*must be less than max"
  )
})

test_that("edit_override errors when step_size <= 0", {
  m <- make_test_model()
  expect_error(
    edit_override(m, "Costs", "setting", "discount_cost", step_size = -1),
    "step_size must be greater than 0"
  )
})

# =============================================================================
# remove_override
# =============================================================================

test_that("remove_override removes override", {
  m <- make_test_model()
  m2 <- remove_override(m, "Costs", "variable", "cost_drug")
  expect_equal(length(m2$override_categories[[1]]$overrides), 1)
  # Setting override remains
  expect_equal(m2$override_categories[[1]]$overrides[[1]]$name, "discount_cost")
})

test_that("remove_override isolates by strategy/group", {
  m <- make_test_model()
  # Add strategy-specific override
  m2 <- add_override(m, "Costs", title = "Treatment Drug", name = "cost_drug",
                     type = "variable", input_type = "numeric", expression = 150,
                     strategy = "treatment")
  # Remove only the strategy-specific one
  m3 <- remove_override(m2, "Costs", "variable", "cost_drug", strategy = "treatment")
  # The non-strategy one should remain
  remaining <- m3$override_categories[[1]]$overrides
  has_cost_drug <- any(sapply(remaining, function(o) o$name == "cost_drug" && o$strategy == ""))
  expect_true(has_cost_drug)
})

test_that("remove_override leaves empty overrides list in category", {
  m <- make_test_model()
  m2 <- remove_override(m, "Utilities", "variable", "util_sick")
  expect_equal(length(m2$override_categories[[2]]$overrides), 0)
  # Category still exists
  expect_equal(m2$override_categories[[2]]$name, "Utilities")
})

test_that("remove_override errors when override not found", {
  m <- make_test_model()
  expect_error(
    remove_override(m, "Costs", "variable", "nonexistent"),
    "not found"
  )
})

test_that("remove_override errors when category not found", {
  m <- make_test_model()
  expect_error(
    remove_override(m, "Nonexistent", "variable", "cost_drug"),
    "not found"
  )
})
