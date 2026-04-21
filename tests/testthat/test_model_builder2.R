test_that("define_model creates valid model structure", {
  m <- define_model("markov")
  expect_s3_class(m, "oq_markov")
  expect_s3_class(m, "oq_model")
  expect_equal(m$settings$model_type, "markov")
  expect_true(is.data.frame(m$states))
  expect_true(is.data.frame(m$strategies))
  expect_true(is.data.frame(m$groups))
  expect_true(is.data.frame(m$summaries))
  expect_equal(nrow(m$states), 0)
})

test_that("define_model matches define_model output", {
  m1 <- define_model("markov")
  m2 <- define_model("markov")
  expect_identical(m1, m2)

  m1_psm <- define_model("psm")
  m2_psm <- define_model("psm")
  expect_identical(m1_psm, m2_psm)

  m1_cpsm <- define_model("custom_psm")
  m2_cpsm <- define_model("custom_psm")
  expect_identical(m1_cpsm, m2_cpsm)

  m1_dt <- define_model("decision_tree")
  m2_dt <- define_model("decision_tree")
  expect_identical(m1_dt, m2_dt)
})

test_that("add_strategy adds a strategy", {
  m <- define_model() |> add_strategy("A", "Strategy A")
  expect_equal(nrow(m$strategies), 1)
  expect_equal(m$strategies$name, "A")
  expect_equal(m$strategies$display_name, "Strategy A")
})

test_that("add_strategy matches add_strategy output", {
  m1 <- define_model() |> add_strategy("A", "Strategy A", "desc A")
  m2 <- define_model() |> add_strategy("A", "Strategy A", "desc A")
  expect_identical(m1$strategies, m2$strategies)
})

test_that("add_strategy errors on duplicate name", {
  m <- define_model() |> add_strategy("A")
  expect_error(add_strategy(m, "A"), "Duplicate strategy name")
})

test_that("add_strategy applies defaults", {
  m <- define_model() |> add_strategy("A")
  expect_equal(m$strategies$display_name, "A")
  expect_equal(m$strategies$description, "A")
  expect_equal(m$strategies$enabled, 1)
})

test_that("edit_strategy updates fields", {
  m <- define_model() |>
    add_strategy("A", "Strategy A") |>
    edit_strategy("A", display_name = "New A")
  expect_equal(m$strategies$display_name, "New A")
})

test_that("edit_strategy renames with cascade", {
  m <- define_model() |>
    add_strategy("A") |>
    add_variable("cost", 100, strategy = "A") |>
    edit_strategy("A", new_name = "B")
  expect_equal(m$strategies$name, "B")
  expect_equal(m$variables$strategy, "B")
})

test_that("edit_strategy matches edit_strategy cascade behavior", {
  build <- function(builder_add_strategy, builder_add_variable, builder_edit_strategy) {
    define_model("markov") |>
      builder_add_strategy("A") |>
      set_settings(timeframe = 10, cycle_length = 1,
                   timeframe_unit = "years", cycle_length_unit = "years") |>
      builder_add_variable("cost", 100, strategy = "A") |>
      builder_edit_strategy("A", new_name = "B")
  }
  m1 <- build(add_strategy, add_variable, edit_strategy)
  m2 <- build(add_strategy, add_variable, edit_strategy)
  expect_equal(m1$strategies$name, m2$strategies$name)
  expect_equal(m1$variables$strategy, m2$variables$strategy)
})

test_that("remove_strategy removes strategy", {
  m <- define_model() |>
    add_strategy("A") |>
    add_strategy("B") |>
    remove_strategy("A")
  expect_equal(nrow(m$strategies), 1)
  expect_equal(m$strategies$name, "B")
})

test_that("remove_strategy cascades to variables", {
  m <- define_model() |>
    add_strategy("A") |>
    add_variable("cost", 100, strategy = "A") |>
    remove_strategy("A")
  expect_equal(nrow(m$variables), 0)
})

test_that("remove_strategy errors on dependencies when flagged", {
  m <- define_model() |>
    add_strategy("A") |>
    add_variable("cost", 100, strategy = "A")
  expect_error(
    remove_strategy(m, "A", error_on_dependencies = TRUE),
    class = "strategy_has_dependencies"
  )
})

test_that("add_group adds a group", {
  m <- define_model() |> add_group("G1", "Group 1")
  expect_equal(nrow(m$groups), 1)
  expect_equal(m$groups$name, "G1")
  expect_equal(m$groups$weight, "1")
})

test_that("add_group matches add_group output", {
  m1 <- define_model() |> add_group("G1", "Group 1", weight = "0.5")
  m2 <- define_model() |> add_group("G1", "Group 1", weight = "0.5")
  expect_identical(m1$groups, m2$groups)
})

test_that("edit_group renames with cascade", {
  m <- define_model() |>
    add_group("G1") |>
    add_variable("cost", 100, group = "G1") |>
    edit_group("G1", new_name = "G2")
  expect_equal(m$groups$name, "G2")
  expect_equal(m$variables$group, "G2")
})

test_that("remove_group cascades to variables", {
  m <- define_model() |>
    add_group("G1") |>
    add_variable("cost", 100, group = "G1") |>
    remove_group("G1")
  expect_equal(nrow(m$variables), 0)
})

test_that("add_summary adds a summary", {
  m <- define_model() |> add_summary("total_cost", "cost", type = "cost")
  expect_equal(nrow(m$summaries), 1)
  expect_equal(m$summaries$name, "total_cost")
  expect_equal(m$summaries$type, "cost")
})

test_that("add_summary rejects WTP for cost summaries", {
  m <- define_model()
  expect_error(
    add_summary(m, "total_cost", "cost", type = "cost", wtp = 50000),
    "WTP cannot be specified"
  )
})

test_that("add_scenario adds a scenario", {
  m <- define_model() |> add_scenario("Optimistic")
  expect_equal(length(m$scenarios), 1)
  expect_equal(m$scenarios[[1]]$name, "Optimistic")
  expect_equal(m$scenarios[[1]]$description, "Optimistic")
  expect_true(is.list(m$scenarios[[1]]$variable_overrides))
})

test_that("add_scenario rejects Base Case name", {
  m <- define_model()
  expect_error(add_scenario(m, "Base Case"), "reserved")
})

test_that("add_scenario_variable adds override to scenario", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_scenario("Opt") |>
    add_scenario_variable("Opt", "cost", 200)
  expect_equal(length(m$scenarios[[1]]$variable_overrides), 1)
  expect_equal(m$scenarios[[1]]$variable_overrides[[1]]$name, "cost")
})

test_that("add_scenario_setting adds setting override", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_scenario("Opt") |>
    add_scenario_setting("Opt", "discount_cost", 0.05)
  expect_equal(length(m$scenarios[[1]]$setting_overrides), 1)
  expect_equal(m$scenarios[[1]]$setting_overrides[[1]]$name, "discount_cost")
})

test_that("add_multivariate_sampling adds spec", {
  m <- define_model() |>
    add_variable("p1", 0.3) |>
    add_variable("p2", 0.3) |>
    add_multivariate_sampling("mv1", "dirichlet", c("p1", "p2"), n = 100)
  expect_equal(length(m$multivariate_sampling), 1)
  expect_equal(m$multivariate_sampling[[1]]$name, "mv1")
  expect_equal(m$multivariate_sampling[[1]]$type, "dirichlet")
})

test_that("edit_multivariate_sampling updates fields", {
  m <- define_model() |>
    add_variable("p1", 0.3) |>
    add_variable("p2", 0.3) |>
    add_multivariate_sampling("mv1", "dirichlet", c("p1", "p2"), n = 100) |>
    edit_multivariate_sampling("mv1", n = 200)
  expect_equal(m$multivariate_sampling[[1]]$n, 200)
})

test_that("edit_multivariate_sampling renames", {
  m <- define_model() |>
    add_variable("p1", 0.3) |>
    add_variable("p2", 0.3) |>
    add_multivariate_sampling("mv1", "dirichlet", c("p1", "p2"), n = 100) |>
    edit_multivariate_sampling("mv1", new_name = "mv2")
  expect_equal(m$multivariate_sampling[[1]]$name, "mv2")
})

test_that("remove_multivariate_sampling removes entry", {
  m <- define_model() |>
    add_variable("p1", 0.3) |>
    add_variable("p2", 0.3) |>
    add_multivariate_sampling("mv1", "dirichlet", c("p1", "p2"), n = 100) |>
    remove_multivariate_sampling("mv1")
  expect_equal(length(m$multivariate_sampling), 0)
})

test_that("add_dsa_variable adds DSA parameter", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_dsa_variable("cost", 50, 200)
  expect_equal(length(m$dsa_parameters), 1)
  expect_equal(m$dsa_parameters[[1]]$name, "cost")
  expect_equal(m$dsa_parameters[[1]]$type, "variable")
})

test_that("add_dsa_variable warns and replaces duplicate", {
  m <- define_model() |> add_variable("cost", 100)
  m <- add_dsa_variable(m, "cost", 50, 200)
  expect_warning(
    m2 <- add_dsa_variable(m, "cost", 80, 120),
    "Replacing"
  )
  expect_equal(length(m2$dsa_parameters), 1)
})

test_that("add_dsa_setting adds DSA setting parameter", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_dsa_setting("discount_cost", 0, 0.1)
  expect_equal(length(m$dsa_parameters), 1)
  expect_equal(m$dsa_parameters[[1]]$type, "setting")
})

test_that("add_threshold_analysis adds analysis", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_strategy("A") |>
    add_variable("cost", 100) |>
    add_threshold_analysis(
      "ta1", "cost", 0, 500,
      threshold_condition_costs(value = "cost", type = "absolute",
                                 strategy = "A", target_value = 1000)
    )
  expect_equal(length(m$threshold_analyses), 1)
  expect_equal(m$threshold_analyses[[1]]$name, "ta1")
})

test_that("add_override_category adds category", {
  m <- define_model() |> add_override_category("UI Controls")
  expect_equal(length(m$override_categories), 1)
  expect_equal(m$override_categories[[1]]$name, "UI Controls")
})

test_that("add_override_category errors on duplicate", {
  m <- define_model() |> add_override_category("UI Controls")
  expect_error(add_override_category(m, "UI Controls"), "already exists")
})

# --- State tests via engine ---------------------------------------------------

test_that("add_state works for markov", {
  m <- define_model("markov") |> add_state("healthy", initial_prob = 1)
  expect_equal(nrow(m$states), 1)
  expect_equal(m$states$name, "healthy")
  expect_equal(m$states$initial_probability, "1")
})

test_that("add_state works for custom_psm", {
  m <- define_model("custom_psm") |> add_state("alive")
  expect_equal(nrow(m$states), 1)
  expect_equal(ncol(m$states), 3) # name, display_name, description only
})

test_that("add_state rejects reserved names", {
  expect_error(define_model() |> add_state("All", initial_prob = 1), "reserved")
})

test_that("add_state rejects Markov params for PSM", {
  expect_error(define_model("psm") |> add_state("A", initial_prob = 0.5), "initial_prob")
})

test_that("add_state blocks decision_tree", {
  expect_error(define_model("decision_tree") |> add_state("A"), "Decision tree")
})

test_that("edit_state updates fields via engine", {
  m <- define_model() |> add_state("A", initial_prob = 1) |>
    edit_state("A", display_name = "State A")
  expect_equal(m$states$display_name, "State A")
})

test_that("edit_state renames with cascade via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_transition("A", "B", 0.1) |>
    add_value("cost", 100, state = "A") |>
    edit_state("A", new_name = "C")
  expect_equal(m$states$name[1], "C")
  expect_equal(m$transitions$from_state, "C")
  expect_equal(m$values$state, "C")
})

test_that("remove_state cascades via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_transition("A", "B", 0.1) |>
    add_value("cost", 100, state = "A") |>
    remove_state("A")
  expect_equal(nrow(m$states), 1)
  expect_equal(nrow(m$transitions), 0)
  expect_equal(nrow(m$values), 0)
})

test_that("remove_state errors on dependencies when flagged", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_transition("A", "B", 0.1)
  expect_error(remove_state(m, "A", error_on_dependencies = TRUE),
               class = "state_has_dependencies")
})

# --- Transition tests via engine ----------------------------------------------

test_that("add_transition validates states via engine", {
  m <- define_model() |> add_state("A", initial_prob = 1)
  expect_error(add_transition(m, "A", "Z", 0.1), "undefined to_state")
})

test_that("edit_transition works via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_transition("A", "B", 0.1) |>
    edit_transition("A", "B", 0.2)
  expect_equal(m$transitions$formula, "0.2")
})

test_that("remove_transition works via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_transition("A", "B", 0.1) |>
    remove_transition("A", "B")
  expect_equal(nrow(m$transitions), 0)
})

test_that("PSM transitions work via engine", {
  m <- define_model("psm") |>
    add_transition("pfs", "months", 1 - exp(-0.01 * month))
  expect_equal(nrow(m$transitions), 1)
  expect_equal(m$transitions$endpoint, "pfs")
})

test_that("Custom PSM transitions work via engine", {
  m <- define_model("custom_psm") |>
    add_state("alive") |>
    add_transition("alive", 0.9)
  expect_equal(nrow(m$transitions), 1)
})

# --- Value tests via engine ---------------------------------------------------

test_that("add_value works via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 1) |>
    add_value("cost", 100, state = "A", type = "cost")
  expect_equal(nrow(m$values), 1)
  expect_equal(m$values$name, "cost")
})

test_that("add_value rejects custom_psm transitional values", {
  m <- define_model("custom_psm") |> add_state("A") |> add_state("B")
  expect_error(add_value(m, "cost", 100, state = "A", destination = "B"), "transitional")
})

test_that("add_value enforces All/AllOther rules", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_value("cost", 100, state = "A")
  expect_error(add_value(m, "cost", 200, state = "All"), "already has residence")
})

test_that("add_value table collision check", {
  m <- define_model() |> add_table("cost", data.frame(x = 1))
  expect_error(add_value(m, "cost", 100), "collision")
})

test_that("edit_value works via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 1) |>
    add_value("cost", 100, state = "A") |>
    edit_value("cost", state = "A", formula = 200)
  expect_equal(m$values$formula, "200")
})

test_that("remove_value works via engine", {
  m <- define_model() |>
    add_state("A", initial_prob = 1) |>
    add_value("cost", 100, state = "A") |>
    remove_value("cost")
  expect_equal(nrow(m$values), 0)
})

# --- Variable tests via engine ------------------------------------------------

test_that("add_variable works via engine", {
  m <- define_model() |> add_variable("cost", 100)
  expect_equal(nrow(m$variables), 1)
  expect_equal(m$variables$formula, "100")
})

test_that("edit_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    edit_variable("cost", formula = 200)
  expect_equal(m$variables$formula, "200")
})

test_that("edit_variable renames via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    edit_variable("cost", new_name = "price")
  expect_equal(m$variables$name, "price")
})

test_that("remove_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    remove_variable("cost")
  expect_equal(nrow(m$variables), 0)
})

# --- DSA edit/remove tests ---------------------------------------------------

test_that("edit_dsa_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_dsa_variable("cost", 50, 200) |>
    edit_dsa_variable("cost", low = 80)
  expect_equal(as.character(m$dsa_parameters[[1]]$low), "80")
})

test_that("remove_dsa_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_dsa_variable("cost", 50, 200) |>
    remove_dsa_variable("cost")
  expect_equal(length(m$dsa_parameters), 0)
})

test_that("edit_dsa_setting works via engine", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_dsa_setting("discount_cost", 0, 0.1) |>
    edit_dsa_setting("discount_cost", low = 0.01)
  expect_equal(m$dsa_parameters[[1]]$low, 0.01)
})

test_that("remove_dsa_setting works via engine", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_dsa_setting("discount_cost", 0, 0.1) |>
    remove_dsa_setting("discount_cost")
  expect_equal(length(m$dsa_parameters), 0)
})

# --- Scenario edit/remove tests ----------------------------------------------

test_that("edit_scenario_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_scenario("Opt") |>
    add_scenario_variable("Opt", "cost", 200) |>
    edit_scenario_variable("Opt", "cost", value = 300)
  expect_equal(m$scenarios[[1]]$variable_overrides[[1]]$value, 300)
})

test_that("remove_scenario_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_scenario("Opt") |>
    add_scenario_variable("Opt", "cost", 200) |>
    remove_scenario_variable("Opt", "cost")
  expect_equal(length(m$scenarios[[1]]$variable_overrides), 0)
})

test_that("remove_scenario_setting works via engine", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_scenario("Opt") |>
    add_scenario_setting("Opt", "discount_cost", 0.05) |>
    remove_scenario_setting("Opt", "discount_cost")
  expect_equal(length(m$scenarios[[1]]$setting_overrides), 0)
})

# --- TWSA tests --------------------------------------------------------------

test_that("add_twsa works via engine", {
  m <- define_model() |> add_twsa("my_twsa")
  expect_equal(length(m$twsa_analyses), 1)
  expect_equal(m$twsa_analyses[[1]]$name, "my_twsa")
})

test_that("edit_twsa renames via engine", {
  m <- define_model() |>
    add_twsa("tw1") |>
    edit_twsa("tw1", new_name = "tw2")
  expect_equal(m$twsa_analyses[[1]]$name, "tw2")
})

test_that("remove_twsa works via engine", {
  m <- define_model() |>
    add_twsa("tw1") |>
    remove_twsa("tw1")
  expect_equal(length(m$twsa_analyses), 0)
})

test_that("add_twsa_variable works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_twsa("tw1") |>
    add_twsa_variable("tw1", "cost", "range", min = 50, max = 200, steps = 10)
  expect_equal(length(m$twsa_analyses[[1]]$parameters), 1)
  expect_equal(m$twsa_analyses[[1]]$parameters[[1]]$param_type, "variable")
})

test_that("add_twsa_variable enforces 2-param limit", {
  m <- define_model() |>
    add_variable("a", 1) |> add_variable("b", 2) |> add_variable("c", 3) |>
    add_twsa("tw1") |>
    add_twsa_variable("tw1", "a", "range", min = 0, max = 10, steps = 5) |>
    add_twsa_variable("tw1", "b", "range", min = 0, max = 10, steps = 5)
  expect_error(add_twsa_variable(m, "tw1", "c", "range", min = 0, max = 10, steps = 5),
               "already has 2 parameters")
})

test_that("add_twsa_setting works via engine", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_twsa("tw1") |>
    add_twsa_setting("tw1", "discount_cost", "range", min = 0, max = 0.1, steps = 5)
  expect_equal(m$twsa_analyses[[1]]$parameters[[1]]$param_type, "setting")
})

# --- Override tests ----------------------------------------------------------

test_that("add_override works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_override_category("UI") |>
    add_override("UI", "Cost Override", "cost", expression = 200)
  expect_equal(length(m$override_categories[[1]]$overrides), 1)
  expect_equal(m$override_categories[[1]]$overrides[[1]]$title, "Cost Override")
})

test_that("add_override builds slider config", {
  m <- define_model() |>
    add_variable("p", 0.5) |>
    add_override_category("UI") |>
    add_override("UI", "Prob", "p", input_type = "slider", expression = 0.5)
  cfg <- m$override_categories[[1]]$overrides[[1]]$input_config
  expect_equal(cfg$step_size, 0.05)
})

test_that("edit_override_category works via engine", {
  m <- define_model() |>
    add_override_category("UI") |>
    edit_override_category("UI", new_name = "Controls")
  expect_equal(m$override_categories[[1]]$name, "Controls")
})

test_that("remove_override_category works via engine", {
  m <- define_model() |>
    add_override_category("UI") |>
    remove_override_category("UI")
  expect_equal(length(m$override_categories), 0)
})

test_that("remove_override works via engine", {
  m <- define_model() |>
    add_variable("cost", 100) |>
    add_override_category("UI") |>
    add_override("UI", "Cost", "cost", expression = 200) |>
    remove_override("UI", "variable", "cost")
  expect_equal(length(m$override_categories[[1]]$overrides), 0)
})

# --- Table / Script tests ----------------------------------------------------

test_that("add_table and edit/remove work", {
  m <- define_model() |>
    add_table("costs", data.frame(x = 1:3)) |>
    edit_table("costs", new_name = "prices") |>
    remove_table("prices")
  expect_equal(length(m$tables), 0)
})

test_that("add_script and edit/remove work", {
  m <- define_model() |>
    add_script("init", "x <- 1") |>
    edit_script("init", code = "x <- 2") |>
    remove_script("init")
  expect_equal(length(m$scripts), 0)
})

# --- VBP / PSA tests ---------------------------------------------------------

test_that("set_vbp and remove_vbp work", {
  m <- define_model() |>
    set_vbp("price", "A", "health", "cost")
  expect_equal(m$vbp$price_variable, "price")
  m2 <- remove_vbp(m)
  expect_null(m2$vbp)
})

test_that("set_psa works", {
  m <- define_model() |> set_psa(1000, seed = 42)
  expect_equal(m$psa$n_sim, 1000L)
  expect_equal(m$psa$seed, 42)
})

# --- Cascade edge case tests -------------------------------------------------

test_that("remove_strategy removes TWSA if < 2 params remain", {
  m <- define_model() |>
    add_strategy("A") |>
    add_strategy("B") |>
    add_variable("cost", 100, strategy = "A") |>
    add_variable("price", 200, strategy = "B") |>
    add_twsa("tw1") |>
    add_twsa_variable("tw1", "cost", "range", min = 50, max = 200, steps = 10, strategy = "A") |>
    add_twsa_variable("tw1", "price", "range", min = 100, max = 300, steps = 10, strategy = "B")
  expect_equal(length(m$twsa_analyses), 1)
  expect_equal(length(m$twsa_analyses[[1]]$parameters), 2)
  m2 <- remove_strategy(m, "A")
  # After removing strategy A, TWSA has only 1 param → entire TWSA removed
  expect_equal(length(m2$twsa_analyses), 0)
})

test_that("remove_group removes TWSA if < 2 params remain", {
  m <- define_model() |>
    add_group("G1") |>
    add_group("G2") |>
    add_variable("cost", 100, group = "G1") |>
    add_variable("price", 200, group = "G2") |>
    add_twsa("tw1") |>
    add_twsa_variable("tw1", "cost", "range", min = 50, max = 200, steps = 10, group = "G1") |>
    add_twsa_variable("tw1", "price", "range", min = 100, max = 300, steps = 10, group = "G2")
  m2 <- remove_group(m, "G1")
  expect_equal(length(m2$twsa_analyses), 0)
})

# --- Value merge-on-rename tests ---------------------------------------------

test_that("edit_value error_on_name_sharing fires correctly", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_value("cost", 100, state = "A") |>
    add_value("cost", 200, state = "B")
  expect_error(
    edit_value(m, "cost", state = "A", new_name = "price", error_on_name_sharing = TRUE),
    class = "value_name_shared"
  )
})

test_that("edit_value error_on_field_changes fires correctly", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_value("cost", 100, state = "A", display_name = "Cost A") |>
    add_value("price", 200, state = "B", display_name = "Price B")
  expect_error(
    edit_value(m, "cost", state = "A", new_name = "price", error_on_field_changes = TRUE),
    class = "value_field_changes"
  )
})

test_that("edit_value auto-adopts target display_name on merge", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_value("cost", 100, state = "A", display_name = "Old Cost") |>
    add_value("price", 200, state = "B", display_name = "Price") |>
    edit_value("cost", state = "A", new_name = "price")
  # Should have adopted target's display_name
  merged_row <- m$values[m$values$state == "A", ]
  expect_equal(merged_row$display_name, "Price")
})

test_that("edit_value explicit display_name overrides auto-adopt (no conflict)", {
  # When there's only one row per name, no display_name conflict
  m <- define_model() |>
    add_state("A", initial_prob = 1) |>
    add_value("cost", 100, state = "A", display_name = "Old Cost") |>
    edit_value("cost", state = "A", new_name = "price", display_name = "Custom Name")
  expect_equal(m$values$display_name, "Custom Name")
})

test_that("edit_value rename_all renames all rows and cascades to summaries", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_value("cost", 100, state = "A") |>
    add_value("cost", 200, state = "B") |>
    add_summary("total", "cost", type = "cost") |>
    edit_value("cost", state = "A", new_name = "price", rename_all = TRUE)
  expect_equal(sum(m$values$name == "price"), 2)
  expect_equal(sum(m$values$name == "cost"), 0)
  expect_true(grepl("price", m$summaries$values[1]))
})

test_that("edit_value partial rename cascades to summaries only when fully orphaned", {
  m <- define_model() |>
    add_state("A", initial_prob = 0.5) |>
    add_state("B", initial_prob = 0.5) |>
    add_value("cost", 100, state = "A") |>
    add_value("cost", 200, state = "B") |>
    add_summary("total", "cost", type = "cost") |>
    edit_value("cost", state = "A", new_name = "price")
  # "cost" still exists for state B, so summary should NOT be updated
  expect_true(grepl("cost", m$summaries$values[1]))
  expect_false(grepl("price", m$summaries$values[1]))
})

test_that("edit_value partial rename cascades when last row orphaned", {
  m <- define_model() |>
    add_state("A", initial_prob = 1) |>
    add_value("cost", 100, state = "A") |>
    add_summary("total", "cost", type = "cost") |>
    edit_value("cost", state = "A", new_name = "price")
  # "cost" is fully gone → summary should be updated
  expect_true(grepl("price", m$summaries$values[1]))
})

# --- error_on_dependencies tests for all entity types ------------------------

test_that("remove_value errors on dependencies when flagged", {
  m <- define_model() |>
    add_state("A", initial_prob = 1) |>
    add_value("cost", 100, state = "A", type = "cost") |>
    add_summary("total", "cost", type = "cost")
  expect_error(
    remove_value(m, "cost", error_on_dependencies = TRUE),
    class = "value_has_dependencies"
  )
  # Verify the error contains a dependencies field
  err <- tryCatch(
    remove_value(m, "cost", error_on_dependencies = TRUE),
    value_has_dependencies = function(e) e
  )
  expect_true("summaries" %in% names(err$dependencies))
})

test_that("remove_summary errors on dependencies when flagged", {
  m <- define_model() |>
    set_settings(timeframe = 10, cycle_length = 1,
                  timeframe_unit = "years", cycle_length_unit = "years") |>
    add_strategy("A") |>
    add_variable("cost", 100) |>
    add_summary("total", "cost", type = "cost") |>
    add_threshold_analysis("ta1", "cost", 0, 500,
      threshold_condition_costs(summary = "total", type = "absolute",
                                  strategy = "A", target_value = 1000))
  expect_error(
    remove_summary(m, "total", error_on_dependencies = TRUE),
    class = "summary_has_dependencies"
  )
})

test_that("remove_strategy dependency error includes all affected entities", {
  m <- define_model() |>
    add_strategy("A") |>
    add_variable("cost", 100, strategy = "A") |>
    add_dsa_variable("cost", 50, 200, strategy = "A")
  err <- tryCatch(
    remove_strategy(m, "A", error_on_dependencies = TRUE),
    strategy_has_dependencies = function(e) e
  )
  expect_true("variables" %in% names(err$dependencies))
  expect_true("dsa_parameters" %in% names(err$dependencies))
})

test_that("remove_group dependency error includes all affected entities", {
  m <- define_model() |>
    add_group("G1") |>
    add_variable("cost", 100, group = "G1")
  err <- tryCatch(
    remove_group(m, "G1", error_on_dependencies = TRUE),
    group_has_dependencies = function(e) e
  )
  expect_true("variables" %in% names(err$dependencies))
})

test_that("edit_strategy cascades rename to all 7 downstream entities", {
  m <- define_model() |>
    add_strategy("A") |>
    add_variable("cost", 100, strategy = "A") |>
    add_dsa_variable("cost", 50, 200, strategy = "A") |>
    add_scenario("Opt") |>
    add_scenario_variable("Opt", "cost", 300, strategy = "A") |>
    add_twsa("tw1") |>
    add_twsa_variable("tw1", "cost", "range", min = 50, max = 200, steps = 10, strategy = "A") |>
    add_override_category("UI") |>
    add_override("UI", "Cost", "cost", expression = 200, strategy = "A") |>
    add_multivariate_sampling("mv1", "dirichlet", c("cost"), strategy = "A", n = 100) |>
    edit_strategy("A", new_name = "B")
  expect_equal(m$strategies$name, "B")
  expect_equal(m$variables$strategy, "B")
  expect_equal(m$dsa_parameters[[1]]$strategy, "B")
  expect_equal(m$scenarios[[1]]$variable_overrides[[1]]$strategy, "B")
  expect_equal(m$twsa_analyses[[1]]$parameters[[1]]$strategy, "B")
  expect_equal(m$override_categories[[1]]$overrides[[1]]$strategy, "B")
  expect_equal(m$multivariate_sampling[[1]]$strategy, "B")
})
