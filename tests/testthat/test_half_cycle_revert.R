# Half-cycle correction on occupancy (as in 0.1.3 / 7962847), with discounting
# set by the same method. Cycle k is priced with cycle k's value:
#   start: occ(k-1) x v(k) at d(k-1)    end: occ(k) x v(k) at d(k)
#   life-table: the average of the two, each discounted at its own end
# Transitional values are charged in the cycle they occur under every method.
#
# Toy model (fixtures/half_cycle_toy): A -> B 0.2, A -> D 0.1, B -> D 0.3,
# everyone starts in A, 5 annual cycles, 3% discounting. Expected values in
# expected_7962847.csv and example_markov_7962847.csv were produced by
# openqaly 7962847.

toy_dir <- function() test_path("fixtures", "half_cycle_toy")
toy_model <- function(name) read_model_json(file = file.path(toy_dir(), paste0("toy_", name, ".json")))
toy_expected <- function() read.csv(file.path(toy_dir(), "expected_7962847.csv"))

long_values <- function(res, discounted) {
  as.data.frame(get_values(res, discounted = discounted, use_display_names = FALSE))
}

value_of <- function(res, value, discounted = FALSE) {
  v <- long_values(res, discounted)
  v <- v[v$value_name == value, ]
  v$amount[order(v$cycle)]
}

A <- 0.7^(0:5)
d3 <- 1.03^-(0:5)

test_that("undiscounted values equal 7962847 under every method", {
  exp <- toy_expected()
  for (m in unique(exp$model)) {
    res <- run_model(read_model_json(file = file.path(toy_dir(), m)))
    e <- exp[exp$model == m & !exp$discounted, ]
    got <- merge(e, long_values(res, FALSE), by = c("value_name", "cycle"))
    expect_equal(nrow(got), nrow(e), info = m)
    expect_equal(got$amount.y, got$amount.x, tolerance = 1e-10, info = m)
  }
})

test_that("start and end discounting equal 7962847 with matching discount_timing", {
  exp <- toy_expected()
  for (m in c("toy_start.json", "toy_end.json", "toy_by_year_start.json")) {
    res <- run_model(read_model_json(file = file.path(toy_dir(), m)))
    e <- exp[exp$model == m & exp$discounted, ]
    got <- merge(e, long_values(res, TRUE), by = c("value_name", "cycle"))
    expect_equal(nrow(got), nrow(e), info = m)
    # Model start values are now discounted at time 0 under every method
    # (7962847 used d(1) for them under discount_timing = "end")
    keep <- !(m == "toy_end.json" & got$value_name == "ms")
    expect_equal(got$amount.y[keep], got$amount.x[keep], tolerance = 1e-10, info = m)
  }
  expect_equal(value_of(run_model(toy_model("end")), "ms", TRUE), c(777, 0, 0, 0, 0))
})

test_that("spot values per method", {
  res <- list(
    start = run_model(toy_model("start")),
    end = run_model(toy_model("end")),
    lt = run_model(toy_model("life_table"))
  )
  expect_equal(value_of(res$start, "r_c1"), c(1000, 0, 0, 0, 0))
  expect_equal(value_of(res$lt, "r_c1"), c(850, 0, 0, 0, 0))
  expect_equal(value_of(res$lt, "r_first3"), c(425, 297.5, 208.25, 0, 0))
  expect_equal(value_of(res$start, "r_cyc"), c(10, 14, 14.7, 13.72, 12.005))
  for (r in res) {
    expect_equal(value_of(r, "t_AB"), c(200, 140, 98, 68.6, 48.02))
  }
})

test_that("life-table discounts each half at its own end", {
  res <- run_model(toy_model("life_table"))
  k <- 1:5
  expect_equal(value_of(res, "r_const", TRUE), 100 * (A[k] * d3[k] + A[k + 1] * d3[k + 1]) / 2)
  expect_equal(value_of(res, "r_const", TRUE)[1], 83.9806, tolerance = 1e-6)
  expect_equal(value_of(res, "t_AB", TRUE), 200 * 0.7^(k - 1) * (d3[k] + d3[k + 1]) / 2)
  expect_equal(value_of(res, "t_AB", TRUE)[1], 197.0874, tolerance = 1e-6)
})

test_that("life-table equals the average of start and end, discounted and undiscounted", {
  res <- lapply(c(start = "start", end = "end", lt = "life_table"), function(n) run_model(toy_model(n)))
  for (disc in c(FALSE, TRUE)) {
    s <- long_values(res$start, disc)
    e <- long_values(res$end, disc)
    l <- long_values(res$lt, disc)
    key <- c("value_name", "cycle")
    se <- merge(s, e, by = key)
    sel <- merge(se, l, by = key)
    expect_equal(sel$amount, (sel$amount.x + sel$amount.y) / 2, tolerance = 1e-10, info = disc)
  }
})

test_that("life-table with by_year discounting floors each end to whole years", {
  res <- run_model(toy_model("by_year_life_table"))
  # 6 half-year cycles; time points at 0, 0.5, ..., 3 years
  occ <- 0.7^(0:6)
  d <- 1.03^-floor((0:6) * 0.5)
  k <- 1:6
  expect_equal(value_of(res, "r_const", TRUE), 100 * (occ[k] * d[k] + occ[k + 1] * d[k + 1]) / 2)
  expect_equal(value_of(res, "t_AB", TRUE), 200 * occ[k] * (d[k] + d[k + 1]) / 2)
})

test_that("per-value overrides are the escape route for one-offs", {
  m <- toy_model("life_table")
  m$values$half_cycle_method <- NA_character_
  m$values$half_cycle_method[m$values$name == "r_c1"] <- "start"
  m$values$half_cycle_method[m$values$name == "t_AB"] <- "end"
  m <- add_value(m, "entry_sc1", "1000 * (state_cycle == 1)", state = "B", type = "cost",
                 half_cycle_method = "end")
  res <- run_model(m)
  # One-off for everyone in A at the start: full amount, at time 0
  expect_equal(value_of(res, "r_c1"), c(1000, 0, 0, 0, 0))
  expect_equal(value_of(res, "r_c1", TRUE), c(1000, 0, 0, 0, 0))
  # Entry cost written with state_cycle == 1 equals the transitional value under end
  expect_equal(value_of(res, "entry_sc1"), value_of(res, "t_AB"))
  expect_equal(value_of(res, "entry_sc1", TRUE), value_of(res, "t_AB", TRUE))
  # Values without an override keep the model's life-table method
  expect_equal(value_of(res, "r_first3"), c(425, 297.5, 208.25, 0, 0))
})

test_that("a per-value method on a transitional value only changes its discounting", {
  m <- toy_model("life_table")
  m$values$half_cycle_method <- NA_character_
  m$values$half_cycle_method[m$values$name == "t_AB"] <- "start"
  res <- run_model(m)
  start_res <- run_model(toy_model("start"))
  expect_equal(value_of(res, "t_AB"), c(200, 140, 98, 68.6, 48.02))
  expect_equal(value_of(res, "t_AB", TRUE), value_of(start_res, "t_AB", TRUE))
})

test_that("discounting overrides keep their identities under every method", {
  avg <- "sum(discount_factors * trace('A')) / sum(trace('A'))"
  for (n in c("start", "end", "life_table")) {
    base <- run_model(toy_model(n))
    m <- toy_model(n)
    m$values$discounting_override <- NA_character_
    m$values$discounting_override[m$values$name == "t_AB"] <- "discount_factors"
    m$values$discounting_override[m$values$name == "r_const"] <- avg
    res <- run_model(m)
    expect_equal(value_of(res, "t_AB", TRUE), value_of(base, "t_AB", TRUE), info = n)
    # A constant residency value in A: the trace() average discount equals the
    # ratio of its discounted to undiscounted total
    ratio <- sum(value_of(base, "r_const", TRUE)) / sum(value_of(base, "r_const"))
    expect_equal(value_of(res, "r_const", TRUE) / value_of(res, "r_const"), rep(ratio, 5), info = n)
  }
})

test_that("corrected traces are stored for the model's method", {
  expected <- list(start = A[1:5], end = A[2:6], life_table = (A[1:5] + A[2:6]) / 2)
  for (n in names(expected)) {
    res <- run_model(toy_model(n))
    seg <- res$segments
    expect_true(all(c("corrected_collapsed_trace", "corrected_expanded_trace") %in% names(seg)), info = n)
    expect_equal(as.numeric(seg$corrected_collapsed_trace[[1]][["A"]]), expected[[n]], info = n)
    ct <- seg$trace_and_values[[1]]$correctedTrace
    expect_equal(nrow(ct), 5, info = n)
    # Tunnel states of B collapse to B
    b_cols <- grep("^B", colnames(ct))
    expect_equal(unname(rowSums(ct[, b_cols, drop = FALSE])),
                 as.numeric(seg$corrected_collapsed_trace[[1]][["B"]]), info = n)
  }
})

test_that("PSM and custom PSM store corrected traces", {
  cpsm <- define_model("custom_psm") |>
    set_settings(n_cycles = 3, cycle_length = 1, cycle_length_unit = "years",
                 half_cycle_method = "life-table") |>
    add_strategy("std") |>
    add_state("alive") |>
    add_state("dead") |>
    add_transition("alive", 0.9^cycle) |>
    add_transition("dead", C) |>
    add_value("c", 1000, state = "alive", type = "cost")
  seg <- run_model(cpsm)$segments
  expect_equal(as.numeric(seg$corrected_collapsed_trace[[1]][["alive"]]),
               (0.9^(0:2) + 0.9^(1:3)) / 2)
})

test_that("PSM transitional values are charged in the cycle the events occur", {
  skip_if_not_installed("openqalysurv")
  # PFS(t) = 0.25^t, OS(t) = 0.5^t
  for (hcm in c("start", "end", "life-table")) {
    m <- define_model("psm") |>
      set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years",
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
    res <- run_model(m)
    k <- 1:5
    expect_equal(value_of(res, "prog_cost"), 100 * (0.25^(k - 1) - 0.25^k), info = hcm)
    expect_equal(value_of(res, "death_cost"), c(50, 25, 12.5, 6.25, 3.125), info = hcm)
    expect_true(all(c("corrected_collapsed_trace", "corrected_expanded_trace") %in% names(res$segments)))
  }
})

test_that("bundled example_markov equals 7962847", {
  exp <- read.csv(file.path(toy_dir(), "example_markov_7962847.csv"))
  path <- system.file("models", "example_markov", package = "openqaly")
  for (m in unique(exp$method)) {
    model <- read_model(path)
    model$settings$half_cycle_method <- m
    res <- run_model(model)
    for (disc in unique(exp$discounted[exp$method == m])) {
      e <- exp[exp$method == m & exp$discounted == disc, ]
      got <- merge(e, long_values(res, disc), by = c("strategy", "group", "value_name", "cycle"))
      expect_equal(nrow(got), nrow(e), info = paste(m, disc))
      expect_equal(got$amount.y, got$amount.x, tolerance = 1e-10, info = paste(m, disc))
    }
  }
})

test_that("the default half-cycle method is start", {
  expect_equal(define_model("markov")$settings$half_cycle_method, "start")
  m <- define_model("markov")
  m$settings$half_cycle_method <- NULL
  expect_equal(set_settings(m, n_cycles = 5)$settings$half_cycle_method, "start")
  base <- data.frame(setting = c("discount_cost", "discount_outcomes"), value = c("3", "3"))
  missing <- openqaly:::convert_settings_from_df(base)
  expect_equal(missing$half_cycle_method, "start")
  expect_warning(
    bad <- openqaly:::convert_settings_from_df(
      rbind(base, data.frame(setting = "half_cycle_method", value = "middle"))
    ),
    "Defaulting to 'start'"
  )
  expect_equal(bad$half_cycle_method, "start")
})
