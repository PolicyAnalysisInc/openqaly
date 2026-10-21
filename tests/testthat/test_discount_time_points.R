# Discounting at time points: each time point's value is discounted at its own
# time and then allocated to cycles with the half-cycle method.

custom_psm_discount_model <- function(hcm, override = NA, value_hcm = NA) {
  define_model("custom_psm") |>
    set_settings(n_cycles = 3, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 3.5, discount_outcomes = 3.5, half_cycle_method = hcm) |>
    add_strategy("std") |>
    add_state("alive") |>
    add_state("dead") |>
    add_transition("alive", 0.9^cycle) |>
    add_transition("dead", C) |>
    add_value("c", 1000, state = "alive", type = "cost",
              discounting_override = override, half_cycle_method = value_hcm) |>
    add_value("c_other", 1000, state = "alive", type = "cost")
}

segment_values <- function(model) {
  run_model(model)$segments$trace_and_values[[1]]
}

test_that("life-table discounts each half at its own time point", {
  # Point values 1000 x 0.9^t discounted at 1.035^-t, t = 0..3
  tv <- segment_values(custom_psm_discount_model("life-table"))
  expect_equal(unname(tv$values_discounted[, "c"]), c(934.783, 812.854, 706.830), tolerance = 1e-6)
  expect_equal(unname(tv$values[, "c"]), c(950, 855, 769.5))
})

test_that("start and end discount at the time point they allocate", {
  points <- 1000 * 0.9^(0:3) * 1.035^-(0:3)
  expect_equal(unname(segment_values(custom_psm_discount_model("start"))$values_discounted[, "c"]), points[1:3])
  expect_equal(unname(segment_values(custom_psm_discount_model("end"))$values_discounted[, "c"]), points[2:4])
})

test_that("trace() average equals the discount of a residency value of 1 in that state", {
  avg <- "sum(discount_factors * trace('alive')) / sum(trace('alive'))"
  for (hcm in c("start", "end", "life-table")) {
    tv <- segment_values(custom_psm_discount_model(hcm, override = avg))
    default_ratio <- sum(tv$values_discounted[, "c_other"]) / sum(tv$values[, "c_other"])
    override_ratio <- tv$values_discounted[, "c"] / tv$values[, "c"]
    expect_equal(unname(override_ratio), rep(default_ratio, 3), tolerance = 1e-10, info = hcm)
  }
})

test_that("trace() average matches residency discounting in Markov and PSM", {
  avg_sick <- "sum(discount_factors * trace('sick')) / sum(trace('sick'))"
  markov <- define_model("markov") |>
    set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 3.5, discount_outcomes = 3.5, half_cycle_method = "life-table") |>
    add_strategy("s") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.2) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("sick", "dead", 0.3) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("lump", 500, state = "healthy", destination = "sick", type = "cost",
              discounting_override = avg_sick) |>
    add_value("sick_cost", 1, state = "sick", type = "cost")
  tv <- segment_values(markov)
  ratio <- sum(tv$values_discounted[, "sick_cost"]) / sum(tv$values[, "sick_cost"])
  expect_equal(unname(tv$values_discounted[, "lump"]), unname(tv$values[, "lump"]) * ratio, tolerance = 1e-10)

  skip_if_not_installed("openqalysurv")
  avg_pp <- "sum(discount_factors * trace('progressed')) / sum(trace('progressed'))"
  psm <- define_model("psm") |>
    set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 3.5, discount_outcomes = 3.5, half_cycle_method = "life-table") |>
    add_strategy("s") |>
    add_state("pfs") |>
    add_state("progressed") |>
    add_state("dead") |>
    add_variable("pfs_dist", define_surv_param("weibull", shape = 1.2, scale = 3)) |>
    add_variable("os_dist", define_surv_param("weibull", shape = 1.1, scale = 6)) |>
    add_transition("PFS", "years", pfs_dist) |>
    add_transition("OS", "years", os_dist) |>
    add_value("psact", 5000, state = "pfs", destination = "progressed", type = "cost",
              discounting_override = avg_pp) |>
    add_value("pp_cost", 1, state = "progressed", type = "cost")
  tv <- segment_values(psm)
  ratio <- sum(tv$values_discounted[, "pp_cost"]) / sum(tv$values[, "pp_cost"])
  expect_equal(unname(tv$values_discounted[, "psact"]), unname(tv$values[, "psact"]) * ratio, tolerance = 1e-10)
})

test_that("an override with one factor per time point replaces d(t): declining rate", {
  # 2.5% to year 30, then 1.5% (applied from year 30 on)
  declining <- "ifelse(discount_time <= 30, 1.025^-discount_time, 1.025^-30 * 1.015^-(discount_time - 30))"
  model <- define_model("markov") |>
    set_settings(n_cycles = 40, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 3.5, discount_outcomes = 3.5, half_cycle_method = "life-table") |>
    add_strategy("s") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("alive", "dead", 0.05) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("c", 100, state = "alive", type = "cost", discounting_override = declining)
  tv <- segment_values(model)
  t <- 0:40
  d <- ifelse(t <= 30, 1.025^-t, 1.025^-30 * 1.015^-(t - 30))
  points <- 100 * 0.95^t * d
  expect_equal(unname(tv$values_discounted[, "c"]), (points[1:40] + points[2:41]) / 2, tolerance = 1e-10)
})

test_that("an override with the wrong length errors", {
  expect_error(
    run_model(custom_psm_discount_model("life-table", override = "c(1, 2)")),
    "must evaluate to length 1 or 4"
  )
  # Model variables have one value per cycle; per-time-point factors use discount_time
  expect_error(
    run_model(custom_psm_discount_model("life-table", override = "1.015^-year")),
    "use discount_time"
  )
})

test_that("per-time-point overrides work in models with tunnel states", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 4, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 3.5, discount_outcomes = 3.5, half_cycle_method = "start") |>
    add_strategy("s") |>
    add_state("well", initial_prob = 1, state_cycle_limit = 1, state_cycle_limit_unit = "cycles") |>
    add_state("sick", initial_prob = 0, state_cycle_limit = 3, state_cycle_limit_unit = "cycles") |>
    add_state("dead", initial_prob = 0, state_cycle_limit = 1, state_cycle_limit_unit = "cycles") |>
    add_transition("well", "sick", 0.5) |>
    add_transition("well", "well", C) |>
    add_transition("sick", "dead", 0.3) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("first_year", "1000 * (state_cycle == 1)", state = "sick", type = "cost",
              discounting_override = "1.015^-discount_time")
  tv <- segment_values(model)
  # Point values: arrivals at t (0, .5, .25, .125, .0625) x 1000, discounted at 1.015^-t
  arrivals <- c(0, 0.5^(1:4))
  points <- arrivals * 1000 * 1.015^-(0:4)
  expect_equal(unname(tv$values_discounted[, "first_year"]), points[1:4], tolerance = 1e-10)
})

test_that("decision tree + Markov: offset discounting, model start and decision tree parts", {
  dt_offset <- 30 / 365
  model <- define_model("markov") |>
    set_settings(timeframe = 5, timeframe_unit = "years", cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 3.5, discount_outcomes = 3.5, half_cycle_method = "start") |>
    add_strategy("surgery") |>
    add_tree_node("surgery", "root", parent = NA, formula = 1) |>
    add_tree_node("surgery", "success", parent = "root", formula = 0.8) |>
    add_tree_node("surgery", "failure", parent = "root", formula = "C") |>
    set_decision_tree("surgery", duration = 30, duration_unit = "days") |>
    add_state("healthy", initial_prob = "p(success, surgery)") |>
    add_state("dead", initial_prob = "C") |>
    add_transition("healthy", "dead", 0.1) |>
    add_transition("healthy", "healthy", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("surgery_cost", type = "cost", state = "decision_tree", formula = 50000) |>
    add_value("start_cost", 2000, type = "cost") |>
    add_value("treatment_cost", 1000, state = "healthy", type = "cost") |>
    add_value("custom_cost", 1000, state = "healthy", type = "cost",
              discounting_override = "1.015^-discount_time")
  tv <- segment_values(model)
  n <- nrow(tv$values)
  t <- 0:n
  healthy <- 0.8 * 0.9^t

  # Decision tree values are not discounted
  expect_equal(unname(tv$values_discounted[, "surgery_cost"]), unname(tv$values[, "surgery_cost"]))
  # Model start values are discounted at time point 0 (after the decision tree offset)
  expect_equal(unname(tv$values_discounted[1, "start_cost"]), 2000 * 1.035^-dt_offset, tolerance = 1e-10)
  # Residency values: time point t is at t + offset years
  expected <- (healthy * 1000 * 1.035^-(t + dt_offset))[1:n]
  expect_equal(unname(tv$values_discounted[, "treatment_cost"]), expected, tolerance = 1e-10)
  # discount_time includes the offset
  expected_custom <- (healthy * 1000 * 1.015^-(t + dt_offset))[1:n]
  expect_equal(unname(tv$values_discounted[, "custom_cost"]), expected_custom, tolerance = 1e-10)
})

test_that("by_year floors each time point to whole years", {
  model <- define_model("custom_psm") |>
    set_settings(n_cycles = 24, cycle_length = 1, cycle_length_unit = "months",
                 discount_cost = 5, discount_outcomes = 5, half_cycle_method = "end",
                 discount_method = "by_year") |>
    add_strategy("std") |>
    add_state("alive") |>
    add_state("dead") |>
    add_transition("alive", 1) |>
    add_transition("dead", C) |>
    add_value("c", 1, state = "alive", type = "cost")
  tv <- segment_values(model)
  # Uses the model's days per year for the cycle length in years
  cl_years <- openqaly:::get_cycle_length_days(model$settings) / openqaly:::get_days_per_year(model$settings)
  expect_equal(unname(tv$values_discounted[, "c"]), 1.05^-floor((1:24) * cl_years), tolerance = 1e-10)
})

test_that("segments no longer store corrected traces", {
  result <- run_model(custom_psm_discount_model("life-table"))
  expect_false(any(c("corrected_collapsed_trace", "corrected_expanded_trace") %in% names(result$segments)))
  expect_null(result$segments$trace_and_values[[1]]$correctedTrace)
})

# --- Per-value half-cycle method ---------------------------------------------

test_that("a value's own half_cycle_method overrides the model setting for that value only", {
  tv <- segment_values(custom_psm_discount_model("life-table", value_hcm = "end"))
  points <- 1000 * 0.9^(0:3)
  expect_equal(unname(tv$values[, "c"]), points[2:4])
  expect_equal(unname(tv$values[, "c_other"]), (points[1:3] + points[2:4]) / 2)
  disc_points <- points * 1.035^-(0:3)
  expect_equal(unname(tv$values_discounted[, "c"]), disc_points[2:4])
})

test_that("trace() weights follow the value's own half-cycle method", {
  avg <- "sum(discount_factors * trace('alive')) / sum(trace('alive'))"
  tv <- segment_values(custom_psm_discount_model("life-table", override = avg, value_hcm = "end"))
  occ <- 0.9^(0:3)
  w <- c(0, 1, 1, 1)
  factor <- sum(1.035^-(0:3) * occ * w) / sum(occ * w)
  expect_equal(unname(tv$values_discounted[, "c"]), unname(tv$values[, "c"]) * factor, tolerance = 1e-10)
})

test_that("conflicting half_cycle_method settings across rows of one value error", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 2, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("s") |>
    add_state("a", initial_prob = 1) |>
    add_state("b", initial_prob = 0) |>
    add_transition("a", "b", 0.5) |>
    add_transition("a", "a", C) |>
    add_transition("b", "b", 1) |>
    add_value("cost", 1, state = "a", type = "cost", half_cycle_method = "end") |>
    add_value("cost", 2, state = "b", type = "cost")
  # The builder keeps rows consistent; a loaded model edited by hand may not be
  model$values$half_cycle_method[2] <- "start"
  expect_error(run_model(model), "different half_cycle_method settings")
})

test_that("invalid per-value half_cycle_method is rejected", {
  expect_error(
    define_model("markov") |> add_state("a", initial_prob = 1) |>
      add_value("cost", 1, state = "a", half_cycle_method = "midpoint"),
    "half_cycle_method must be NA or one of"
  )
})

test_that("per-value half_cycle_method survives YAML, JSON and R code round-trips", {
  model <- custom_psm_discount_model("life-table", value_hcm = "end")
  json_back <- read_model_json(text = write_model_json(model))
  expect_equal(json_back$values$half_cycle_method[json_back$values$name == "c"], "end")

  yaml_path <- tempfile(fileext = ".yaml")
  write_model_yaml(model, yaml_path)
  yaml_back <- read_model_yaml(yaml_path)
  unlink(yaml_path)
  expect_equal(yaml_back$values$half_cycle_method[yaml_back$values$name == "c"], "end")

  # Evaluate the package's own generated R code to reconstruct the model
  # (same pattern as the other code generation round-trip tests)
  code <- model_to_r_code(model)
  code_back <- eval(parse(text = code))
  expect_equal(code_back$values$half_cycle_method[code_back$values$name == "c"], "end")
})

# --- Bundled models ------------------------------------------------------------

test_that("every bundled model runs", {
  model_dirs <- list.dirs(system.file("models", package = "openqaly"), recursive = FALSE)
  for (d in model_dirs) {
    if (file.exists(file.path(d, "model.yaml"))) {
      model <- suppressWarnings(read_model(d))
    } else {
      model <- read_model_json(file = file.path(d, "model.json"))
    }
    expect_no_error(suppressWarnings(run_model(model)), message = basename(d))
  }
})

test_that("a value's half_cycle_method stays the same on all its rows", {
  base <- define_model("markov") |>
    add_state("a", initial_prob = 1) |>
    add_state("b", initial_prob = 0) |>
    add_value("cost", 1, state = "a", half_cycle_method = "end")

  # A new row without a setting takes the value's setting
  m <- base |> add_value("cost", 2, state = "b")
  expect_equal(m$values$half_cycle_method, c("end", "end"))

  # A new row with a different setting is rejected
  expect_error(base |> add_value("cost", 2, state = "b", half_cycle_method = "start"),
               "already uses half_cycle_method 'end'")

  # Editing the setting on one row changes every row
  m2 <- m |> edit_value("cost", state = "b", half_cycle_method = "life-table")
  expect_equal(m2$values$half_cycle_method, c("life-table", "life-table"))

  # Renaming into an existing value adopts its setting
  m3 <- base |> add_value("other", 2, state = "b") |> edit_value("other", state = "b", new_name = "cost")
  expect_equal(m3$values$half_cycle_method, c("end", "end"))
})
