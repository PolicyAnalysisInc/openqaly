# Half-cycle correction applied to value x trace at time points 0..n.
# Values are evaluated per cycle; time point t is priced with cycle t's value
# (the cycle ending at t) and time point 0 with cycle 1's. Point values X(t)
# are allocated to cycle k as start X(k-1), end X(k), life-table
# (X(k-1) + X(k)) / 2, for residency and transitional values alike.

build_issue10_markov <- function(hcm, n_cycles = 3) {
  define_model("markov") |>
    set_settings(n_cycles = n_cycles, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0, half_cycle_method = hcm) |>
    add_strategy("std") |>
    add_state("well", initial_prob = 1, state_cycle_limit = 1, state_cycle_limit_unit = "cycles") |>
    add_state("sick", initial_prob = 0, state_cycle_limit = 3, state_cycle_limit_unit = "cycles") |>
    add_state("dead", initial_prob = 0, state_cycle_limit = 1, state_cycle_limit_unit = "cycles") |>
    add_variable("p_sick", 0.5) |>
    add_variable("p_die", 0.3) |>
    add_variable("c", 1000) |>
    add_transition("well", "sick", "p_sick") |>
    add_transition("well", "well", "C") |>
    add_transition("sick", "dead", "p_die") |>
    add_transition("sick", "sick", "C") |>
    add_transition("dead", "dead", "1") |>
    add_value("explicit", "c", state = "well", destination = "sick", type = "cost") |>
    add_value("implicit", "c * (state_cycle == 1)", state = "sick", type = "cost") |>
    add_value("spike", "c * (cycle == 1)", state = "well", type = "cost") |>
    add_value("const", "c", state = "sick", type = "cost") |>
    add_value("start_val", "c", type = "cost")
}

values_wide <- function(model) {
  as.data.frame(get_values(run_model(model), format = "wide", discounted = FALSE))
}

test_that("explicit transitional values equal implicit state_cycle == 1 values under every method", {
  # Arrivals into sick observed at t = 1, 2, 3: 0.5, 0.25, 0.125
  expected <- list(
    "start" = c(0, 500, 250),
    "end" = c(500, 250, 125),
    "life-table" = c(250, 375, 187.5)
  )
  for (hcm in names(expected)) {
    v <- values_wide(build_issue10_markov(hcm))
    expect_equal(v$explicit, expected[[hcm]], info = hcm)
    expect_equal(v$implicit, v$explicit, info = hcm)
  }
})

test_that("constant residency values are unchanged by the value-stream correction", {
  # Sick occupancy at t = 0..3: 0, 0.5, 0.6, 0.545
  expected <- list(
    "start" = c(0, 500, 600),
    "end" = c(500, 600, 545),
    "life-table" = c(250, 550, 572.5)
  )
  for (hcm in names(expected)) {
    v <- values_wide(build_issue10_markov(hcm))
    expect_equal(v$const, expected[[hcm]], info = hcm)
  }
})

test_that("a value that varies with model time is priced at time points", {
  # 1000 * (cycle == 1) on well: cycle 1's value prices time points 0 and 1
  # (well = 1 and 0.5); later time points are priced 0
  expected <- list(
    "start" = c(1000, 500, 0),
    "end" = c(500, 0, 0),
    "life-table" = c(750, 250, 0)
  )
  for (hcm in names(expected)) {
    v <- values_wide(build_issue10_markov(hcm))
    expect_equal(v$spike, expected[[hcm]], info = hcm)
  }
})

test_that("model start values stay in cycle 1 without correction", {
  for (hcm in c("start", "end", "life-table")) {
    v <- values_wide(build_issue10_markov(hcm))
    expect_equal(v$start_val, c(1000, 0, 0), info = hcm)
  }
})

test_that("custom PSM allocates value x occupancy at time points", {
  # alive(t) = 1 - 0.1 t, value(t) = 100 - 10 t, t = 0..4
  # Point values: 100, 81, 64, 49, 36
  model_for <- function(hcm) {
    define_model("custom_psm") |>
      set_settings(n_cycles = 4, cycle_length = 1, cycle_length_unit = "years",
                   discount_cost = 0, discount_outcomes = 0, half_cycle_method = hcm) |>
      add_strategy("std") |>
      add_state("alive") |>
      add_state("dead") |>
      add_transition("alive", 1 - 0.1 * cycle) |>
      add_transition("dead", C) |>
      add_value("smooth", "100 - 10 * cycle", state = "alive", type = "cost")
  }
  # Cycle values 90, 80, 70, 60 price time points 1..4; time point 0 takes 90.
  # Point values: 90, 81, 64, 49, 36
  expect_equal(values_wide(model_for("start"))$smooth, c(90, 81, 64, 49))
  expect_equal(values_wide(model_for("end"))$smooth, c(81, 64, 49, 36))
  expect_equal(values_wide(model_for("life-table"))$smooth, c(85.5, 72.5, 56.5, 42.5))
})

test_that("PSM transitional values are priced on flows observed at each time point", {
  skip_if_not_installed("openqalysurv")
  # PFS(t) = 0.25^t, OS(t) = 0.5^t; flows at t = 1..3:
  # PFS decline 0.75, 0.1875, 0.046875; OS decline 0.5, 0.25, 0.125
  model_for <- function(hcm) {
    define_model("psm") |>
      set_settings(n_cycles = 3, cycle_length = 1, cycle_length_unit = "years",
                   discount_cost = 0, discount_outcomes = 0, half_cycle_method = hcm) |>
      add_strategy("std") |>
      add_state("pfs") |>
      add_state("progressed") |>
      add_state("dead") |>
      add_variable("pfs_dist", define_surv_param("exp", rate = 2 * log(2))) |>
      add_variable("os_dist", define_surv_param("exp", rate = log(2))) |>
      add_transition("PFS", "years", pfs_dist) |>
      add_transition("OS", "years", os_dist) |>
      add_value("prog_cost", 100, state = "pfs", destination = "progressed", type = "cost") |>
      add_value("death_cost", 100, state = "progressed", destination = "dead", type = "cost")
  }
  v_end <- values_wide(model_for("end"))
  expect_equal(v_end$prog_cost, c(75, 18.75, 4.6875))
  expect_equal(v_end$death_cost, c(50, 25, 12.5))
  v_start <- values_wide(model_for("start"))
  expect_equal(v_start$prog_cost, c(0, 75, 18.75))
  v_lt <- values_wide(model_for("life-table"))
  expect_equal(v_lt$death_cost, c(25, 37.5, 18.75))
})

test_that("variables built on lag time variables work (cycle 1 starts at 0, ends at 1)", {
  skip_if_not_installed("openqalysurv")
  model <- define_model("markov") |>
    set_settings(n_cycles = 3, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0) |>
    add_strategy("std") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("os", define_surv_param("exp", rate = 0.2)) |>
    add_variable("p_death", event_prob(os, cycle_lag, cycle)) |>
    add_variable("sched", c(100, 200, 300)) |>
    add_transition("alive", "dead", p_death) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("sched_cost", "sched[cycle]", state = "alive", type = "cost", half_cycle_method = "end")
  v <- values_wide(model)
  alive <- exp(-0.2 * (1:3))
  expect_equal(v$sched_cost, c(100, 200, 300) * alive, tolerance = 1e-10)
})

test_that("time variables with cycle 0 keep each block's state_cycle", {
  settings <- list(cycle_length = 1, cycle_length_unit = "years", n_cycles = 3,
                   days_per_year = 365)
  # A limit of 2 needs 3 state-cycle blocks (the last holds "more than 2")
  states <- data.frame(state_cycle_limit = 2, state_cycle_limit_unit = "cycles")
  tv <- openqaly:::time_variables(settings, states, include_cycle_zero = TRUE, n_cycles = 3)
  expect_equal(tv$cycle, rep(0:3, times = 3))
  expect_equal(tv$state_cycle, rep(1:3, each = 4))
})

test_that("the default half-cycle method is life-table", {
  m <- build_issue10_markov("life-table")
  m$settings$half_cycle_method <- NULL
  expect_equal(values_wide(m), values_wide(build_issue10_markov("life-table")))
  expect_equal(define_model("markov")$settings$half_cycle_method, "life-table")
})
