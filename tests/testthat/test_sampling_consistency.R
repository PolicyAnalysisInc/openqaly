context("Sampling consistency across segments")

suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(tibble))

# Helper: build a model, parse it, prepare segments, and resample
run_sampling <- function(model, n = 100, seed = 42) {
  normalized <- openqaly:::normalize_and_validate_model(model)
  parsed <- openqaly:::parse_model(normalized)
  segments <- openqaly:::get_segments(parsed) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed, as_tibble(seg))
    }) %>%
    ungroup()
  sampled_raw <- openqaly:::resample(parsed, n, segments, seed = seed)
  sampled_raw %>%
    mutate(params = map(parameter_overrides, as_tibble)) %>%
    select(-parameter_overrides) %>%
    unnest(params)
}

# Helper: build a minimal 2-strategy, 2-group markov skeleton
make_skeleton <- function(strategies = c("A", "B"), groups = NULL) {
  m <- define_model("markov") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5
    )
  for (s in strategies) m <- m |> add_strategy(s)
  if (!is.null(groups)) {
    for (g in groups) m <- m |> add_group(g, weight = "1")
  }
  m |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("alive", "dead", 0.1) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("qaly", 1, state = "alive", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>
    add_value("cost_v", 100, state = "alive", type = "cost") |>
    add_value("cost_v", 0, state = "dead", type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome") |>
    add_summary("total_costs", "cost_v", type = "cost")
}

# ============================================================================
# Validation tests (T1-T5)
# ============================================================================

test_that("T1: Global var with bc differing by strategy errors", {
  model <- make_skeleton() |>
    add_variable("markup", 1.5, strategy = "A") |>
    add_variable("markup", 2.0, strategy = "B") |>
    add_variable("base_cost", 100) |>
    add_variable("total", "base_cost * markup",
                 sampling = gamma(mean = bc, sd = 10))

  expect_error(run_sampling(model, n = 5), "differs across strategies")
})

test_that("T2: Global var with bc differing by group errors", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("age_factor", 1.0, group = "young") |>
    add_variable("age_factor", 1.5, group = "old") |>
    add_variable("base_rate", 0.01) |>
    add_variable("p_death", "base_rate * age_factor",
                 sampling = beta(mean = bc, sd = 0.002))

  expect_error(run_sampling(model, n = 5), "differs across groups")
})

test_that("T3: Global var with sampling formula dep differing by strategy errors", {
  model <- make_skeleton() |>
    add_variable("cost_se", 200, strategy = "A") |>
    add_variable("cost_se", 500, strategy = "B") |>
    add_variable("cost", 5000,
                 sampling = normal(mean = bc, sd = cost_se))

  expect_error(run_sampling(model, n = 5), "cost_se.*varies")
})

test_that("T4: Global var with sampling formula dep differing by group errors", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("se_param", 0.01, group = "young") |>
    add_variable("se_param", 0.05, group = "old") |>
    add_variable("p_event", 0.1,
                 sampling = beta(mean = bc, sd = se_param))

  expect_error(run_sampling(model, n = 5), "se_param.*varies")
})

test_that("T5: Var in both univariate and multivariate errors", {
  model <- make_skeleton() |>
    add_variable("p_transition", 0.8, sampling = beta(mean = bc, sd = 0.02)) |>
    add_variable("p_other", 0.2) |>
    add_multivariate_sampling(
      name = "conflict", type = "dirichlet",
      variables = c("p_transition", "p_other"), n = 100
    )
  normalized <- openqaly:::normalize_and_validate_model(model)
  parsed <- openqaly:::parse_model(normalized)
  expect_error(
    openqaly:::validate_sampling_spec(parsed),
    "appears in both variables.sampling and multivariate_sampling"
  )
})

# ============================================================================
# Univariate consistency tests — one per distribution type (T6-T12)
# ============================================================================

test_that("T6: normal - global variable identical across all segments", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v", 100, sampling = normal(mean = bc, sd = 10))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$v, paste(sampled$strategy, sampled$group))
  vals <- unname(seg_vals)
  for (i in 2:length(vals)) {
    expect_identical(vals[[1]], vals[[i]])
  }
})

test_that("T7: lognormal - global variable identical across all segments", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v", 50, sampling = lognormal(meanlog = log(50), sdlog = 0.2))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$v, paste(sampled$strategy, sampled$group))
  vals <- unname(seg_vals)
  for (i in 2:length(vals)) {
    expect_identical(vals[[1]], vals[[i]])
  }
})

test_that("T8: gamma - global variable identical across all segments", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v", 1500, sampling = gamma(mean = bc, sd = 300))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$v, paste(sampled$strategy, sampled$group))
  vals <- unname(seg_vals)
  for (i in 2:length(vals)) {
    expect_identical(vals[[1]], vals[[i]])
  }
})

test_that("T9: beta - global variable identical across all segments", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v", 0.05, sampling = beta(mean = bc, sd = 0.01))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$v, paste(sampled$strategy, sampled$group))
  vals <- unname(seg_vals)
  for (i in 2:length(vals)) {
    expect_identical(vals[[1]], vals[[i]])
  }
})

test_that("T10: uniform - global variable identical across all segments", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v", 0.78, sampling = uniform(min = 0.70, max = 0.86))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$v, paste(sampled$strategy, sampled$group))
  vals <- unname(seg_vals)
  for (i in 2:length(vals)) {
    expect_identical(vals[[1]], vals[[i]])
  }
})

test_that("T11: triangular - global variable identical across all segments", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v", 0.45, sampling = triangular(min = 0.30, mode = 0.45, max = 0.60))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$v, paste(sampled$strategy, sampled$group))
  vals <- unname(seg_vals)
  for (i in 2:length(vals)) {
    expect_identical(vals[[1]], vals[[i]])
  }
})

test_that("T12: bootstrap - global variable produces identical resampled data across segments", {
  ipd <- data.frame(cost = c(100, 200, 300, 400, 500),
                    effect = c(0.5, 0.6, 0.7, 0.8, 0.9))
  model <- make_skeleton() |>
    add_table("ipd_table", ipd) |>
    add_variable("ipd_data", "bootstrap(ipd_table)",
                 sampling = bootstrap(ipd_table))

  normalized <- openqaly:::normalize_and_validate_model(model)
  parsed <- openqaly:::parse_model(normalized)
  segments <- openqaly:::get_segments(parsed) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed, as_tibble(seg))
    }) %>%
    ungroup()
  sampled_raw <- openqaly:::resample(parsed, 10, segments, seed = 42)

  # Bootstrap produces list overrides — extract and compare across segments
  seg_a <- sampled_raw$parameter_overrides[sampled_raw$strategy == "A"]
  seg_b <- sampled_raw$parameter_overrides[sampled_raw$strategy == "B"]
  # Each simulation's overrides should be identical across strategies
  for (i in seq_along(seg_a)) {
    expect_identical(seg_a[[i]][["ipd_data"]], seg_b[[i]][["ipd_data"]])
  }
})

# ============================================================================
# Multivariate consistency tests (T13-T15)
# ============================================================================

test_that("T13: mvnormal - global spec identical across all segments", {
  cov_mat <- data.frame(
    v1 = c(100, 30),
    v2 = c(30, 50)
  )
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("v1", 10) |>
    add_variable("v2", 20) |>
    add_table("cov_table", cov_mat) |>
    add_multivariate_sampling(
      name = "mv_test", type = "mvnormal",
      variables = c("v1", "v2"), covariance = "cov_table"
    )
  sampled <- run_sampling(model)
  for (vname in c("v1", "v2")) {
    seg_vals <- split(sampled[[vname]], paste(sampled$strategy, sampled$group))
    vals <- unname(seg_vals)
    for (i in 2:length(vals)) {
      expect_identical(vals[[1]], vals[[i]])
    }
  }
})

test_that("T14: dirichlet - global spec identical across segments, rows sum to 1", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("p1", 0.6) |>
    add_variable("p2", 0.25) |>
    add_variable("p3", 0.15) |>
    add_multivariate_sampling(
      name = "dir_test", type = "dirichlet",
      variables = c("p1", "p2", "p3"), n = 100
    )
  sampled <- run_sampling(model)
  # Rows sum to 1
  row_sums <- sampled$p1 + sampled$p2 + sampled$p3
  expect_equal(row_sums, rep(1, length(row_sums)), tolerance = 1e-10)
  # Identical across segments
  for (vname in c("p1", "p2", "p3")) {
    seg_vals <- split(sampled[[vname]], paste(sampled$strategy, sampled$group))
    vals <- unname(seg_vals)
    for (i in 2:length(vals)) {
      expect_identical(vals[[1]], vals[[i]])
    }
  }
})

test_that("T15: multinomial - global spec identical across segments, rows sum to total count", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("n1", 45) |>
    add_variable("n2", 30) |>
    add_variable("n3", 25) |>
    add_multivariate_sampling(
      name = "multi_test", type = "multinomial",
      variables = c("n1", "n2", "n3")
    )
  sampled <- run_sampling(model)
  # Rows sum to total base case count (45 + 30 + 25 = 100)
  row_sums <- sampled$n1 + sampled$n2 + sampled$n3
  expect_equal(row_sums, rep(100, length(row_sums)))
  # Identical across segments
  for (vname in c("n1", "n2", "n3")) {
    seg_vals <- split(sampled[[vname]], paste(sampled$strategy, sampled$group))
    vals <- unname(seg_vals)
    for (i in 2:length(vals)) {
      expect_identical(vals[[1]], vals[[i]])
    }
  }
})

# ============================================================================
# Segment-scoping tests (T16-T18)
# ============================================================================

test_that("T16: Group-specific variable identical across strategies, different across groups", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("p_prog", 0.05, group = "young",
                 sampling = beta(mean = bc, sd = 0.01)) |>
    add_variable("p_prog", 0.12, group = "old",
                 sampling = beta(mean = bc, sd = 0.02))
  sampled <- run_sampling(model)

  # Same within group, across strategies
  a_young <- sampled$p_prog[sampled$strategy == "A" & sampled$group == "young"]
  b_young <- sampled$p_prog[sampled$strategy == "B" & sampled$group == "young"]
  expect_identical(a_young, b_young)

  # Different across groups
  a_old <- sampled$p_prog[sampled$strategy == "A" & sampled$group == "old"]
  expect_false(identical(a_young, a_old))
})

test_that("T17: Strategy-specific variable identical across groups, different across strategies", {
  model <- make_skeleton(groups = c("young", "old")) |>
    add_variable("c_drug", 800, strategy = "A",
                 sampling = gamma(mean = bc, sd = 100)) |>
    add_variable("c_drug", 4500, strategy = "B",
                 sampling = gamma(mean = bc, sd = 500))
  sampled <- run_sampling(model)

  # Same within strategy, across groups
  a_young <- sampled$c_drug[sampled$strategy == "A" & sampled$group == "young"]
  a_old <- sampled$c_drug[sampled$strategy == "A" & sampled$group == "old"]
  expect_identical(a_young, a_old)

  # Different across strategies
  b_young <- sampled$c_drug[sampled$strategy == "B" & sampled$group == "young"]
  expect_false(identical(a_young, b_young))
})

test_that("T18: Strategy-specific multivariate only fires in matching segments", {
  cov_mat <- data.frame(v1 = c(100, 30), v2 = c(30, 50))
  model <- make_skeleton() |>
    add_variable("v1", 10) |>
    add_variable("v2", 20) |>
    add_table("cov_table", cov_mat) |>
    add_multivariate_sampling(
      name = "mv_b_only", type = "mvnormal",
      variables = c("v1", "v2"), strategy = "B",
      covariance = "cov_table"
    )
  normalized <- openqaly:::normalize_and_validate_model(model)
  parsed <- openqaly:::parse_model(normalized)
  segments <- openqaly:::get_segments(parsed) %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      openqaly:::prepare_segment_for_sampling(parsed, as_tibble(seg))
    }) %>%
    ungroup()
  sampled_raw <- openqaly:::resample(parsed, 50, segments, seed = 42)

  # Strategy A should have empty overrides
  a_overrides <- sampled_raw$parameter_overrides[sampled_raw$strategy == "A"]
  a_has_v1 <- any(sapply(a_overrides, function(x) "v1" %in% names(x)))
  expect_false(a_has_v1)

  # Strategy B should have overrides
  b_overrides <- sampled_raw$parameter_overrides[sampled_raw$strategy == "B"]
  b_has_v1 <- any(sapply(b_overrides, function(x) "v1" %in% names(x)))
  expect_true(b_has_v1)
})

# ============================================================================
# Infrastructure tests (T19-T22)
# ============================================================================

test_that("T19: Seed reproducibility", {
  model <- make_skeleton() |>
    add_variable("v", 100, sampling = normal(mean = bc, sd = 10))
  sampled1 <- run_sampling(model, seed = 42)
  sampled2 <- run_sampling(model, seed = 42)
  expect_identical(sampled1$v, sampled2$v)
})

test_that("T20: Single-segment model works correctly", {
  m <- define_model("markov") |>
    set_settings(
      timeframe = 5, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years",
      discount_cost = 3.5, discount_outcomes = 3.5
    ) |>
    add_strategy("mono") |>
    add_state("alive", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("alive", "dead", 0.1) |>
    add_transition("alive", "alive", C) |>
    add_transition("dead", "dead", 1) |>
    add_variable("p", 0.1, sampling = beta(mean = bc, sd = 0.02)) |>
    add_value("qaly", 1, state = "alive", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>
    add_value("cost_v", 100, state = "alive", type = "cost") |>
    add_value("cost_v", 0, state = "dead", type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome") |>
    add_summary("total_costs", "cost_v", type = "cost")

  sampled <- run_sampling(m, n = 1000)
  expect_equal(mean(sampled$p), 0.1, tolerance = 0.01)
})

test_that("T21: No sampled variables errors", {
  model <- make_skeleton() |>
    add_variable("p", 0.1)  # no sampling

  expect_error(run_sampling(model, n = 5), "no sampling distributions")
})

test_that("T22: Pure global constant variable identical across segments", {
  model <- make_skeleton() |>
    add_variable("rate", 0.03, sampling = beta(mean = 0.03, sd = 0.005))
  sampled <- run_sampling(model)
  seg_vals <- split(sampled$rate, sampled$strategy)
  expect_identical(seg_vals[["A"]], seg_vals[["B"]])
})
