context("Model API")

# ============================================================================
# Helper: build a populated test model
# ============================================================================

build_test_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3,
      discount_outcomes = 3
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_strategy("standard", "Standard Care") %>%
    add_strategy("intervention", "Intervention") %>%
    add_group("young", "Young Adults", weight = "0.6") %>%
    add_group("elderly", "Elderly", weight = "0.4") %>%
    add_variable("cost", 5000) %>%
    add_variable("rr", 1, strategy = "standard") %>%
    add_variable("rr", 0.5, strategy = "intervention") %>%
    add_variable("p_disease", 0.03, group = "young") %>%
    add_variable("p_disease", 0.08, group = "elderly") %>%
    add_transition("healthy", "sick", p_disease) %>%
    add_transition("healthy", "dead", 0.01) %>%
    add_transition("healthy", "healthy", C) %>%
    add_transition("sick", "dead", 0.05) %>%
    add_transition("sick", "sick", C) %>%
    add_transition("dead", "dead", 1) %>%
    add_value("total_cost", cost, state = "healthy", type = "cost") %>%
    add_value("qalys", 0.9, state = "healthy", type = "outcome") %>%
    add_summary("costs", "total_cost", type = "cost") %>%
    add_summary("qalys_sum", "qalys", type = "outcome") %>%
    add_override_category("Clinical") %>%
    add_override_category("Economic") %>%
    add_override("Clinical",
      title = "Disease Risk (Young)",
      name = "p_disease",
      input_type = "slider",
      expression = 0.03,
      group = "young",
      min = 0, max = 0.2, step_size = 0.01
    ) %>%
    add_override("Clinical",
      title = "Disease Risk (Elderly)",
      name = "p_disease",
      input_type = "slider",
      expression = 0.08,
      group = "elderly",
      min = 0, max = 0.3, step_size = 0.01
    ) %>%
    add_override("Economic",
      title = "Treatment Cost",
      name = "cost",
      input_type = "numeric",
      expression = 5000,
      min = 0, max = 20000
    )
}

# ============================================================================
# Validation Tests
# ============================================================================

test_that(".validate_model_arg errors on non-model objects", {
  expect_error(get_variables(list()), "must be an oq_model")
  expect_error(get_variables(NULL), "must be an oq_model")
  expect_error(get_variables(42), "must be an oq_model")
  expect_error(get_variables("not a model"), "must be an oq_model")
})

test_that(".validate_model_arg accepts oq_model and oq_model_builder", {
  builder <- define_model("markov")
  expect_no_error(get_variables(builder))
})

# ============================================================================
# Getter Tests - Populated Model
# ============================================================================

test_that("get_override_categories returns categories", {
  model <- build_test_model()
  cats <- get_override_categories(model)
  expect_true(is.list(cats))
  expect_equal(length(cats), 2)
  expect_equal(cats[[1]]$name, "Clinical")
  expect_equal(cats[[2]]$name, "Economic")
})

test_that("get_variables returns tibble", {
  model <- build_test_model()
  vars <- get_variables(model)
  expect_true(is.data.frame(vars))
  expect_true(nrow(vars) > 0)
  expect_true("name" %in% names(vars))
})

test_that("get_variable_names returns unique names", {
  model <- build_test_model()
  vnames <- get_variable_names(model)
  expect_true(is.character(vnames))
  expect_true("cost" %in% vnames)
  expect_true("rr" %in% vnames)
  expect_true("p_disease" %in% vnames)
  expect_equal(length(vnames), length(unique(vnames)))
})

test_that("get_global_variables returns only unsegmented variables", {
  model <- build_test_model()
  globals <- get_global_variables(model)
  expect_true(is.data.frame(globals))
  expect_true("cost" %in% globals$name)
  # rr has strategy targeting, p_disease has group targeting
  expect_false("rr" %in% globals$name)
  expect_false("p_disease" %in% globals$name)
})

test_that("get_variable_targeting returns strategy/group info", {
  model <- build_test_model()
  targeting <- get_variable_targeting(model)
  expect_true(is.list(targeting))
  expect_true("rr" %in% names(targeting))
  expect_true("standard" %in% targeting$rr$strategies)
  expect_true("intervention" %in% targeting$rr$strategies)
  expect_null(targeting$rr$groups)
  expect_true("p_disease" %in% names(targeting))
  expect_null(targeting$p_disease$strategies)
  expect_true("young" %in% targeting$p_disease$groups)
  expect_true("elderly" %in% targeting$p_disease$groups)
  # Global variable
  expect_null(targeting$cost$strategies)
  expect_null(targeting$cost$groups)
})

test_that("get_strategies returns strategies tibble", {
  model <- build_test_model()
  strats <- get_strategies(model)
  expect_true(is.data.frame(strats))
  expect_equal(nrow(strats), 2)
  expect_true("standard" %in% strats$name)
  expect_true("intervention" %in% strats$name)
})

test_that("get_groups returns groups tibble", {
  model <- build_test_model()
  grps <- get_groups(model)
  expect_true(is.data.frame(grps))
  expect_equal(nrow(grps), 2)
  expect_true("young" %in% grps$name)
  expect_true("elderly" %in% grps$name)
})

test_that("get_settings returns settings list", {
  model <- build_test_model()
  settings <- get_settings(model)
  expect_true(is.list(settings))
  expect_equal(settings$model_type, "markov")
  expect_equal(settings$discount_cost, 3)
})

test_that("get_model_type returns model type string", {
  model <- build_test_model()
  expect_equal(get_model_type(model), "markov")
})

test_that("get_dsa_parameters returns dsa_parameters class", {
  model <- build_test_model()
  dsa <- get_dsa_parameters(model)
  expect_true(inherits(dsa, "dsa_parameters"))
})

test_that("get_tables returns tables list", {
  model <- build_test_model()
  tables <- get_tables(model)
  expect_true(is.list(tables))
})

test_that("get_table_names returns character vector", {
  model <- build_test_model()
  tnames <- get_table_names(model)
  expect_true(is.character(tnames))
})

test_that("get_values returns values tibble", {
  model <- build_test_model()
  vals <- get_model_values(model)
  expect_true(is.data.frame(vals))
  expect_true(nrow(vals) > 0)
  expect_true("total_cost" %in% vals$name)
})

test_that("get_value_names returns unique value names", {
  model <- build_test_model()
  vnames <- get_model_value_names(model)
  expect_true(is.character(vnames))
  expect_true("total_cost" %in% vnames)
  expect_true("qalys" %in% vnames)
})

test_that("get_trees returns NULL when no trees defined", {
  model <- build_test_model()
  expect_null(get_trees(model))
})

test_that("get_tree_names returns empty character when no trees", {
  model <- build_test_model()
  expect_equal(get_tree_names(model), character(0))
})

# ============================================================================
# Getter Tests - Empty Model
# ============================================================================

test_that("getters return appropriate empty values for empty model", {
  model <- define_model("markov")
  expect_equal(get_override_categories(model), list())
  expect_equal(nrow(get_variables(model)), 0)
  expect_equal(get_variable_names(model), character(0))
  expect_equal(nrow(get_global_variables(model)), 0)
  expect_equal(get_variable_targeting(model), list())
  expect_equal(nrow(get_strategies(model)), 0)
  expect_equal(nrow(get_groups(model)), 0)
  expect_true(is.list(get_settings(model)))
  expect_equal(get_model_type(model), "markov")
  expect_true(inherits(get_dsa_parameters(model), "dsa_parameters"))
  expect_equal(get_tables(model), list())
  expect_equal(get_table_names(model), character(0))
  expect_equal(nrow(get_model_values(model)), 0)
  expect_equal(get_model_value_names(model), character(0))
  expect_null(get_trees(model))
  expect_equal(get_tree_names(model), character(0))
})

# ============================================================================
# Setter Tests - set_override_categories
# ============================================================================

test_that("set_override_categories replaces categories", {
  model <- build_test_model()
  new_cats <- list(
    list(
      name = "New Category",
      general = FALSE,
      overrides = list()
    )
  )
  updated <- set_override_categories(model, new_cats)
  expect_equal(length(get_override_categories(updated)), 1)
  expect_equal(get_override_categories(updated)[[1]]$name, "New Category")
})

test_that("set_override_categories accepts empty list", {
  model <- build_test_model()
  updated <- set_override_categories(model, list())
  expect_equal(length(get_override_categories(updated)), 0)
})

test_that("set_override_categories validates categories is a list", {
  model <- build_test_model()
  expect_error(set_override_categories(model, "not a list"), "must be a list")
})

test_that("set_override_categories validates category name", {
  model <- build_test_model()
  expect_error(
    set_override_categories(model, list(list(name = "", general = FALSE))),
    "non-empty 'name'"
  )
  expect_error(
    set_override_categories(model, list(list(general = FALSE))),
    "non-empty 'name'"
  )
})

test_that("set_override_categories validates override type", {
  model <- build_test_model()
  bad_cats <- list(list(
    name = "Test", general = FALSE,
    overrides = list(list(name = "x", type = "invalid"))
  ))
  expect_error(
    set_override_categories(model, bad_cats),
    "'type' must be"
  )
})

test_that("set_override_categories validates setting override has no strategy/group", {
  model <- build_test_model()
  bad_cats <- list(list(
    name = "Test", general = FALSE,
    overrides = list(list(
      name = "discount_cost", type = "setting",
      input_type = "numeric", strategy = "standard"
    ))
  ))
  expect_error(
    set_override_categories(model, bad_cats),
    "setting overrides cannot have strategy/group"
  )
})

test_that("set_override_categories validates on non-model", {
  expect_error(set_override_categories(list(), list()), "must be an oq_model")
})

# ============================================================================
# Setter Tests - set_override_expression
# ============================================================================

test_that("set_override_expression updates matching override by (name, strategy, group)", {
  model <- build_test_model()

  # Update the young group's p_disease override
  updated <- set_override_expression(model, "p_disease", "0.05", group = "young")
  cats <- get_override_categories(updated)
  young_ovr <- cats[[1]]$overrides[[1]]
  expect_equal(young_ovr$overridden_expression, "0.05")

  # The elderly override should be unchanged
  elderly_ovr <- cats[[1]]$overrides[[2]]
  expect_equal(elderly_ovr$overridden_expression, "0.08")
})

test_that("set_override_expression errors when no match", {
  model <- build_test_model()
  expect_error(
    set_override_expression(model, "nonexistent", "42"),
    "No override found"
  )
})

test_that("set_override_expression errors on wrong strategy/group targeting", {
  model <- build_test_model()
  # p_disease has group targeting, not strategy targeting
  expect_error(
    set_override_expression(model, "p_disease", "0.05", strategy = "standard"),
    "No override found"
  )
})

test_that("set_override_expression validates model", {
  expect_error(
    set_override_expression(list(), "x", "1"),
    "must be an oq_model"
  )
})

# ============================================================================
# Setter Tests - set_override_expressions (batch)
# ============================================================================

test_that("set_override_expressions batch updates multiple overrides", {
  model <- build_test_model()
  overrides <- list(
    list(name = "p_disease", expression = "0.05", group = "young"),
    list(name = "p_disease", expression = "0.12", group = "elderly"),
    list(name = "cost", expression = "7000")
  )
  updated <- set_override_expressions(model, overrides)
  cats <- get_override_categories(updated)

  expect_equal(cats[[1]]$overrides[[1]]$overridden_expression, "0.05")
  expect_equal(cats[[1]]$overrides[[2]]$overridden_expression, "0.12")
  expect_equal(cats[[2]]$overrides[[1]]$overridden_expression, "7000")
})

test_that("set_override_expressions errors if any entry doesn't match", {
  model <- build_test_model()
  overrides <- list(
    list(name = "cost", expression = "7000"),
    list(name = "nonexistent", expression = "42")
  )
  expect_error(set_override_expressions(model, overrides), "No override found")
})

test_that("set_override_expressions validates model", {
  expect_error(
    set_override_expressions(list(), list()),
    "must be an oq_model"
  )
})
