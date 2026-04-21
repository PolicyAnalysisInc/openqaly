context("Override diagnostics regressions")

build_override_diag_base_model <- function() {
  define_model("markov") %>%
    set_settings(
      timeframe = 2,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0,
      discount_outcomes = 0
    ) %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_strategy("base") %>%
    add_variable("p_die", 0.1) %>%
    add_variable("cost_x", 10) %>%
    add_transition("healthy", "dead", p_die) %>%
    add_transition("healthy", "healthy", C) %>%
    add_transition("dead", "dead", 1) %>%
    add_value("cost", cost_x, state = "healthy", type = "cost") %>%
    add_summary("cost_total", "cost", type = "cost")
}

build_variable_override_model <- function() {
  build_override_diag_base_model() %>%
    add_override_category("Clinical") %>%
    add_override("Clinical",
      title = "Death risk",
      name = "p_die",
      expression = 0.2
    )
}

build_setting_override_model <- function() {
  build_override_diag_base_model() %>%
    add_override_category("Settings") %>%
    add_override("Settings",
      title = "Horizon",
      name = "timeframe",
      type = "setting",
      input_type = "numeric",
      expression = 4
    )
}

extract_eval_var <- function(segment_row, name) {
  openqaly:::extract_var_from_ns(segment_row$eval_vars[[1]], name)
}

test_that("run_model without overrides still includes eval_vars", {
  res <- run_model(build_override_diag_base_model())

  expect_true("eval_vars" %in% names(res$segments))
  expect_equal(extract_eval_var(res$segments[1, ], "p_die"), 0.1)
})

test_that("run_model with active variable overrides includes final eval_vars", {
  res <- suppressMessages(run_model(build_variable_override_model()))

  expect_true("eval_vars" %in% names(res$segments))
  expect_equal(extract_eval_var(res$segments[1, ], "p_die"), 0.2)
})

test_that("run_model with active setting overrides includes eval_vars", {
  res <- suppressMessages(run_model(build_setting_override_model()))

  expect_true("eval_vars" %in% names(res$segments))
})

test_that("diagnose_variable works on run_model results with active overrides", {
  res <- suppressMessages(run_model(build_variable_override_model()))

  expect_no_error(diagnose_variable(res, "p_die"))
})

test_that("default run_dsa keeps only final base-case diagnostics", {
  model <- build_variable_override_model() %>%
    add_dsa_variable("p_die", low = 0.05, high = 0.15)

  results <- suppressMessages(run_dsa(model))
  base_seg <- results$segments %>% filter(.data$run_id == 1) %>% slice(1)
  low_seg <- results$segments %>% filter(.data$run_id == 2) %>% slice(1)

  expect_true("eval_vars" %in% names(results$segments))
  expect_equal(extract_eval_var(base_seg, "p_die"), 0.2)
  expect_null(low_seg$eval_vars[[1]])
})

test_that("default run_scenario keeps only final base-case diagnostics", {
  model <- build_variable_override_model() %>%
    add_scenario("Alt") %>%
    add_scenario_variable("Alt", "p_die", 0.3)

  results <- suppressMessages(run_scenario(model))
  base_seg <- results$segments %>% filter(.data$scenario_id == 1) %>% slice(1)
  alt_seg <- results$segments %>% filter(.data$scenario_id == 2) %>% slice(1)

  expect_true("eval_vars" %in% names(results$segments))
  expect_equal(extract_eval_var(base_seg, "p_die"), 0.2)
  expect_null(alt_seg$eval_vars[[1]])
})

test_that("default run_twsa keeps only final base-case diagnostics", {
  model <- build_variable_override_model() %>%
    add_twsa("Clinical vs Cost") %>%
    add_twsa_variable("Clinical vs Cost", "p_die",
      type = "range",
      min = 0.05,
      max = 0.15,
      steps = 2
    ) %>%
    add_twsa_variable("Clinical vs Cost", "cost_x",
      type = "range",
      min = 5,
      max = 15,
      steps = 2
    )

  results <- suppressMessages(run_twsa(model))
  base_seg <- results$segments %>% filter(.data$run_id == 1) %>% slice(1)
  varied_seg <- results$segments %>% filter(.data$run_id == 2) %>% slice(1)

  expect_true("eval_vars" %in% names(results$segments))
  expect_equal(extract_eval_var(base_seg, "p_die"), 0.2)
  expect_null(varied_seg$eval_vars[[1]])
})

test_that("run_dsa keep_diagnostics stores final diagnostics for all rows", {
  model <- build_variable_override_model() %>%
    add_dsa_variable("p_die", low = 0.05, high = 0.15)

  results <- suppressMessages(run_dsa(model, keep_diagnostics = TRUE))
  base_seg <- results$segments %>% filter(.data$run_id == 1) %>% slice(1)
  low_seg <- results$segments %>% filter(.data$run_id == 2) %>% slice(1)
  high_seg <- results$segments %>% filter(.data$run_id == 3) %>% slice(1)

  expect_equal(extract_eval_var(base_seg, "p_die"), 0.2)
  expect_equal(extract_eval_var(low_seg, "p_die"), 0.05)
  expect_equal(extract_eval_var(high_seg, "p_die"), 0.15)
})

test_that("run_scenario keep_diagnostics stores final diagnostics for all rows", {
  model <- build_variable_override_model() %>%
    add_scenario("Alt") %>%
    add_scenario_variable("Alt", "p_die", 0.3)

  results <- suppressMessages(run_scenario(model, keep_diagnostics = TRUE))
  base_seg <- results$segments %>% filter(.data$scenario_id == 1) %>% slice(1)
  alt_seg <- results$segments %>% filter(.data$scenario_id == 2) %>% slice(1)

  expect_equal(extract_eval_var(base_seg, "p_die"), 0.2)
  expect_equal(extract_eval_var(alt_seg, "p_die"), 0.3)
})

test_that("run_twsa keep_diagnostics stores final diagnostics for varied rows", {
  model <- build_variable_override_model() %>%
    add_twsa("Clinical vs Cost") %>%
    add_twsa_variable("Clinical vs Cost", "p_die",
      type = "range",
      min = 0.05,
      max = 0.15,
      steps = 2
    ) %>%
    add_twsa_variable("Clinical vs Cost", "cost_x",
      type = "range",
      min = 5,
      max = 15,
      steps = 2
    )

  results <- suppressMessages(run_twsa(model, keep_diagnostics = TRUE))
  varied_seg <- results$segments %>% filter(.data$run_id == 2) %>% slice(1)

  expect_equal(extract_eval_var(varied_seg, "p_die"), varied_seg$x_value[[1]])
})

test_that("run_psa keep_diagnostics stores diagnostics when requested", {
  model <- build_override_diag_base_model() %>%
    add_variable("p_die_se", 0.01) %>%
    edit_variable("p_die", sampling = normal(mean = bc, sd = p_die_se)) %>%
    set_psa(n_sim = 2, seed = 42)

  res_default <- suppressMessages(run_psa(model, n_sim = 2, seed = 42))
  res_debug <- suppressMessages(run_psa(model, n_sim = 2, seed = 42, keep_diagnostics = TRUE))

  expect_false("eval_vars" %in% names(res_default$segments))
  expect_true("eval_vars" %in% names(res_debug$segments))
  expect_s3_class(res_debug$segments$eval_vars[[1]], "namespace")
})

test_that("build_dsa_segments uses override-adjusted bc", {
  model <- build_variable_override_model() %>%
    add_dsa_variable("p_die", low = bc * 0.5, high = bc * 1.5)

  parsed <- openqaly:::parse_model(model)
  segs <- openqaly:::build_dsa_segments(parsed)
  low_seg <- segs %>% filter(.data$run_id == 2) %>% slice(1)
  high_seg <- segs %>% filter(.data$run_id == 3) %>% slice(1)

  expect_equal(low_seg$parameter_overrides[[1]][["p_die"]], 0.1)
  expect_equal(high_seg$parameter_overrides[[1]][["p_die"]], 0.3)
})

test_that("build_scenario_segments uses override-adjusted bc", {
  model <- build_variable_override_model() %>%
    add_scenario("Alt") %>%
    add_scenario_variable("Alt", "p_die", bc * 0.8)

  parsed <- openqaly:::parse_model(model)
  segs <- openqaly:::build_scenario_segments(parsed)
  alt_seg <- segs %>% filter(.data$scenario_id == 2) %>% slice(1)

  expect_equal(alt_seg$parameter_overrides[[1]][["p_die"]], 0.16)
})

test_that("build_twsa_segments uses override-adjusted variable bc", {
  model <- build_variable_override_model() %>%
    add_twsa("Clinical vs Cost") %>%
    add_twsa_variable("Clinical vs Cost", "p_die",
      type = "radius",
      radius = bc * 0.5,
      steps = 1
    ) %>%
    add_twsa_variable("Clinical vs Cost", "cost_x",
      type = "range",
      min = 5,
      max = 15,
      steps = 2
    )

  parsed <- openqaly:::parse_model(model)
  segs <- openqaly:::build_twsa_segments(parsed)
  x_vals <- segs$x_value[!is.na(segs$x_value)]

  expect_equal(sort(unique(x_vals)), c(0.1, 0.2, 0.3))
})

test_that("build_twsa_segments uses override-adjusted setting bc", {
  model <- build_setting_override_model() %>%
    add_twsa("Time vs Cost") %>%
    add_twsa_setting("Time vs Cost", "timeframe",
      type = "radius",
      radius = 1,
      steps = 1
    ) %>%
    add_twsa_variable("Time vs Cost", "cost_x",
      type = "range",
      min = 5,
      max = 15,
      steps = 2
    )

  parsed <- openqaly:::parse_model(model)
  segs <- openqaly:::build_twsa_segments(parsed)
  x_vals <- segs$x_value[!is.na(segs$x_value)]

  expect_equal(sort(unique(x_vals)), c(3, 4, 5))
})
