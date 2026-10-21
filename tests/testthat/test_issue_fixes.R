# Regression tests for model-building and engine defects

two_state_markov <- function(n_cycles = 3) {
  define_model("markov") |>
    set_settings(n_cycles = n_cycles, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0) |>
    add_strategy("s") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("alive", "dead", "p_death") |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1)
}

# --- Value / variable name clash (previously a segfault) ----------------------

test_that("a value cannot share a name with a variable", {
  base <- two_state_markov() |> add_variable("p_death", 0.5)
  expect_error(
    base |> add_variable("c_death", 100) |> add_value("c_death", "c_death", state = "alive"),
    "used as both variables and values: c_death"
  )
  expect_error(
    base |> add_value("c_death", 100, state = "alive") |> add_variable("c_death", 100),
    "used as both variables and values: c_death"
  )
  expect_error(
    base |> add_variable("c_death", 100) |> add_value("cost", "c_death", state = "alive") |>
      edit_value("cost", state = "alive", new_name = "c_death"),
    "used as both variables and values: c_death"
  )
  expect_error(
    base |> add_variable("c_death", 100) |> add_value("cost", "c_death", state = "alive") |>
      edit_variable("c_death", new_name = "cost"),
    "used as both variables and values: cost"
  )
})

test_that("a loaded model with a value named like a variable errors instead of crashing", {
  model <- two_state_markov() |>
    add_variable("p_death", 0.5) |>
    add_variable("c_death", 100) |>
    add_value("cost", "c_death", state = "alive", type = "cost")
  model$values$name[model$values$name == "cost"] <- "c_death"
  expect_error(run_model(model), "used as both variables and values: c_death")
})

test_that("the dependency sort rejects a duplicated name instead of crashing", {
  expect_error(
    openqaly:::cppSortVariables(
      c("a", "a"), list(character(0), character(0)), list(character(0), character(0)),
      character(0), list()
    ),
    "Duplicate name 'a'"
  )
  expect_error(
    openqaly:::cppSortVariables(
      "a", list(character(0)), list("a"), c("b", "a"), list(character(0), character(0))
    ),
    "used both by a variable and by an item being sorted"
  )
})

# --- state_cycle_limit ----------------------------------------------------------

sick_tunnel_model <- function(limit, n_cycles = 4) {
  define_model("markov") |>
    set_settings(n_cycles = n_cycles, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0,
                 half_cycle_method = "start") |>
    add_strategy("s") |>
    add_state("well", initial_prob = 1) |>
    add_state("sick", initial_prob = 0, state_cycle_limit = limit, state_cycle_limit_unit = "cycles") |>
    add_state("dead", initial_prob = 0) |>
    add_transition("well", "sick", 0.5) |>
    add_transition("well", "well", C) |>
    add_transition("sick", "dead", 0.3) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("x", "1000 * (state_cycle <= 2)", state = "sick", type = "cost")
}

test_that("a state_cycle_limit longer than the model runs", {
  v4 <- get_values(run_model(sick_tunnel_model(4)), format = "wide", discounted = FALSE)$x
  v10 <- get_values(run_model(sick_tunnel_model(10)), format = "wide", discounted = FALSE)$x
  expect_equal(v10, v4)
})

test_that("states that never use state time do not deepen the namespace", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 50, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("s") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("alive", "dead", 0.1) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("c", 1, state = "alive", type = "cost")
  uneval_states <- openqaly:::parse_states(model$states, 365.25, 365.25)
  state_time_use <- tibble::tibble(state = c("alive", "dead"), uses_st = c(FALSE, FALSE))
  depth <- openqaly:::namespace_state_cycle_depth(uneval_states, state_time_use, 50)
  expect_equal(depth, 1)
  tv <- openqaly:::time_variables(model$settings, model$states, n_cycles = 50, st_cycles = depth)
  expect_equal(nrow(tv), 50)

  # Blank limits and limit 1 give identical results
  blank <- get_values(run_model(model), format = "wide", discounted = FALSE)$c
  model1 <- model
  model1$states$state_cycle_limit <- 1
  model1$states$state_cycle_limit_unit <- "cycles"
  expect_equal(get_values(run_model(model1), format = "wide", discounted = FALSE)$c, blank)
})

test_that("a state_cycle_limit counts state cycles 1..L exactly, then 'more than L'", {
  model_for <- function(limit) {
    define_model("markov") |>
      set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years",
                   discount_cost = 0, discount_outcomes = 0, half_cycle_method = "end") |>
      add_strategy("s") |>
      add_state("well", initial_prob = 1) |>
      add_state("sick", initial_prob = 0, state_cycle_limit = limit, state_cycle_limit_unit = "cycles") |>
      add_transition("well", "sick", 0.5) |>
      add_transition("well", "well", C) |>
      add_transition("sick", "sick", 1) |>
      add_value("first_two", "1000 * (state_cycle <= 2)", state = "sick", type = "cost") |>
      add_value("first", "1000 * (state_cycle == 1)", state = "sick", type = "cost") |>
      add_value("entry", 1000, state = "well", destination = "sick", type = "cost")
  }
  # Arrivals into sick at time points 1..5: 0.5, 0.25, 0.125, 0.0625, 0.03125
  v2 <- get_values(run_model(model_for(2)), format = "wide", discounted = FALSE)
  expect_equal(v2$first_two, c(500, 750, 375, 187.5, 93.75))
  v1 <- get_values(run_model(model_for(1)), format = "wide", discounted = FALSE)
  expect_equal(v1$first, v1$entry)
})

test_that("a state that uses state time keeps its tunnels", {
  v <- get_values(run_model(sick_tunnel_model(3)), format = "wide", discounted = FALSE)$x
  # start: arrivals in their first two cycles in sick, priced at time points 0..3
  expect_equal(v, c(0, 500, 600, 300))
})

# --- PSM -------------------------------------------------------------------------

test_that("PSM transitional values on unsupported state pairs are an error", {
  model <- define_model("psm") |>
    set_settings(n_cycles = 3, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("s") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.5)) |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.2)) |>
    add_transition("PFS", "years", pfs_dist) |>
    add_transition("OS", "years", os_dist)
  # Rejected when added
  expect_error(
    add_value(model, "bad", 100, state = "pfs", destination = "dead", type = "cost"),
    "only supports pfs -> progressed"
  )
  # And at run time for a model edited by hand
  model <- add_value(model, "bad", 100, state = "pfs", destination = "progressed", type = "cost")
  model$values$destination[model$values$name == "bad"] <- "dead"
  expect_error(run_model(model), "only supports pfs -> progressed")
})

test_that("a PSM survival curve that is missing before the horizon gives a clear error", {
  os_table <- data.frame(time = c(0, 1, 2), survival = c(1, 0.8, 0.6))
  model <- define_model("psm") |>
    set_settings(n_cycles = 4, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("s") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_table("os_table", os_table) |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.5)) |>
    add_variable("os_dist", define_surv_table(os_table)) |>
    add_transition("PFS", "years", pfs_dist) |>
    add_transition("OS", "years", os_dist)
  expect_error(run_model(model), "OS survival is missing \\(NA\\) from cycle 3")
})

test_that("a raw flexsurvreg fit is accepted as a PSM endpoint", {
  skip_if_not_installed("flexsurv")
  fit <- flexsurv::flexsurvreg(survival::Surv(rectime, censrec) ~ 1, data = flexsurv::bc, dist = "weibull")
  model <- define_model("psm") |>
    set_settings(n_cycles = 3, cycle_length = 365, cycle_length_unit = "days") |>
    add_strategy("s") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", flexsurv::flexsurvreg(survival::Surv(rectime, censrec) ~ 1,
                                                   data = flexsurv::bc, dist = "weibull")) |>
    add_variable("os_dist", define_surv_param("exp", rate = 1e-6)) |>
    add_transition("PFS", "days", pfs_dist) |>
    add_transition("OS", "days", os_dist)
  trace <- get_trace(run_model(model), format = "wide")
  expect_equal(trace$pfs[2], openqalysurv::surv_prob(fit, 365), tolerance = 1e-8)
})

# --- Decision tree + Markov without variables ------------------------------------

test_that("a Markov model with a decision tree and no variables runs", {
  model <- define_model("markov") |>
    set_settings(timeframe = 3, timeframe_unit = "years", cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("surgery") |>
    add_tree_node("surgery", "root", parent = NA, formula = 1) |>
    add_tree_node("surgery", "success", parent = "root", formula = 0.8) |>
    add_tree_node("surgery", "failure", parent = "root", formula = "C") |>
    set_decision_tree("surgery", duration = 0, duration_unit = "days") |>
    add_state("healthy", initial_prob = "p(success, surgery)") |>
    add_state("dead", initial_prob = "C") |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("treatment_cost", 1000, state = "healthy", type = "cost")
  expect_no_error(run_model(model))
})
