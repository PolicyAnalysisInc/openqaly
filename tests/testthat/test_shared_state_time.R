# Tests for shared state-time (state_group + share_state_time) tunnel-depth handling.
#
# Covers:
#   - group-max depth equalization (members of a shared group expand to one depth)
#   - the conflicting-limit guard + the 0 = "unlimited" sentinel
#   - parse-time share_state_time spec validation
#   - the lf_to_lf_mat unresolved-tunnel backstop
#   - the actionable transition-matrix NA message
#   - explicit state_cycle_limit remaining a ceiling (reduce may trim below it)

# Helper: number of tunnel copies a state expanded to, from the expanded trace.
expanded_state_depth <- function(result, state) {
  et <- result$aggregated$expanded_trace[[1]]
  sum(grepl(paste0("^", state, "\\."), names(et)))
}

# Builds a 4-state model (well -> {A,B} -> dead) where A and B form a shared group.
# Knobs let each test vary the depth dynamics, group membership, and limits.
build_shared_model <- function(reduce = TRUE,
                               a_dead = quote(0.01 + 0.01 * state_cycle),
                               b_dead = quote(0.04),
                               b_to_a = TRUE,
                               a_group = "g", b_group = "g",
                               a_share = TRUE, b_share = TRUE,
                               a_limit = NULL, b_limit = NULL,
                               n_cycles = 10) {
  m <- define_model("markov") |>
    set_settings(n_cycles = n_cycles, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0, reduce_state_cycle = reduce) |>
    add_state("well", initial_prob = 1) |>
    add_state("A", state_group = a_group, share_state_time = a_share,
              state_cycle_limit = a_limit, initial_prob = 0) |>
    add_state("B", state_group = b_group, share_state_time = b_share,
              state_cycle_limit = b_limit, initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("well", "A", 0.3) |>
    add_transition("well", "well", C) |>
    add_transition("A", "B", 0.05) |>
    add_transition_("A", "dead", a_dead) |>
    add_transition("A", "A", C) |>
    add_transition_("B", "dead", b_dead) |>
    add_transition("B", "B", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("cost", 1000, state = "well", type = "cost") |>
    add_value("cost", 3000, state = "A", type = "cost") |>
    add_value("cost", 4000, state = "B", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("q", 0.8, state = "well", type = "outcome") |>
    add_value("q", 0.5, state = "A", type = "outcome") |>
    add_value("q", 0.4, state = "B", type = "outcome") |>
    add_value("q", 0, state = "dead", type = "outcome") |>
    add_strategy("sc", "SC") |>
    add_summary("tc", "cost", type = "cost") |>
    add_summary("tq", "q", type = "outcome")
  if (b_to_a) {
    m <- add_transition(m, "B", "A", 0.1)
  }
  m
}

# add_transition uses NSE; this variant injects an unevaluated formula expression.
add_transition_ <- function(model, from, to, formula_expr) {
  add_transition(model, from, to, !!formula_expr)
}


# --- Fix #1: group-max depth equalization -----------------------------------

test_that("shared group with differing derived depths builds instead of NA-erroring", {
  # A keeps changing through every state_cycle (deep); B is constant (shallow).
  # A within-group transition into B would otherwise target a tunnel B doesn't have.
  model <- build_shared_model(reduce = TRUE)
  expect_no_error(run_model(model))
})

test_that("shared group member that never references state time still expands to group depth", {
  # B's formulas are all constant, so on its own it would collapse to depth 1.
  model <- build_shared_model(reduce = TRUE, b_dead = quote(0.04))
  result <- run_model(model)
  # A is deep (n_cycles = 10); B must be equalized to the same depth.
  expect_equal(expanded_state_depth(result, "B"), expanded_state_depth(result, "A"))
  expect_gt(expanded_state_depth(result, "B"), 1)
})

test_that("shared group deep tunnels carry the constant tail value", {
  # reduce=TRUE trims B then bumps it back up; reduce=FALSE expands B fully.
  # Since B is constant, the bumped tail must equal the fully-evaluated values,
  # so the two models must produce identical summaries.
  reduced <- run_model(build_shared_model(reduce = TRUE))
  full    <- run_model(build_shared_model(reduce = FALSE))
  expect_equal(reduced$aggregated$summaries[[1]], full$aggregated$summaries[[1]])
})

test_that("shared group with acyclic shallow->deep flow advances the shared clock", {
  # A->B only (no B->A). After equalization the shared clock advances B with A.
  model <- build_shared_model(reduce = TRUE, b_to_a = FALSE)
  result <- run_model(model)
  expect_no_error(result)
  expect_equal(expanded_state_depth(result, "B"), expanded_state_depth(result, "A"))
})


# --- Fix #2: conflict guard + 0 = unlimited sentinel ------------------------

test_that("shared group with conflicting explicit limits errors", {
  expect_error(
    run_model(build_shared_model(a_limit = 5, b_limit = 9)),
    "conflicting state_cycle_limit"
  )
})

test_that("shared group with one unlimited (0) limit and one finite limit does not falsely conflict", {
  # 0 means unlimited; it must not be read as a finite ceiling that conflicts with 5.
  expect_no_error(run_model(build_shared_model(a_limit = 0, b_limit = 5)))
})

test_that("all-zero-limit shared group behaves as unlimited", {
  expect_no_error(run_model(build_shared_model(a_limit = 0, b_limit = 0)))
})


# --- Fix #3: parse-time share_state_time spec validation ---------------------

test_that("share_state_time = TRUE on a lone explicit group errors", {
  expect_error(
    run_model(build_shared_model(b_group = "other", b_share = FALSE,
                                 a_group = "solo", a_share = TRUE)),
    "at least 2 members"
  )
})

test_that("share_state_time = TRUE with no state_group errors", {
  expect_error(
    run_model(build_shared_model(a_group = NULL, a_share = TRUE,
                                 b_group = "g2", b_share = FALSE)),
    "no state_group"
  )
})

test_that("mixed share_state_time within one group errors", {
  expect_error(
    run_model(build_shared_model(a_share = TRUE, b_share = FALSE)),
    "disagree on share_state_time"
  )
})

test_that("a group with share_state_time = FALSE on all members builds (permitted no-op)", {
  expect_no_error(run_model(build_shared_model(a_share = FALSE, b_share = FALSE)))
})


# --- Fix #4: lf_to_lf_mat unresolved-tunnel backstop ------------------------

test_that("lf_to_lf_mat throws on an unresolved tunnel name", {
  df <- data.frame(
    cycle = 1L, state_cycle = 2L,
    from_state = "A", to_state = "B",
    .from_e = "A.2", .to_e = "B.3",   # B.3 is not in state_names
    value = 0.5, stringsAsFactors = FALSE
  )
  expect_error(
    lf_to_lf_mat(df, state_names = c("A.1", "A.2", "B.1", "B.2")),
    "B.3"
  )
})


# --- Fix #5: actionable transition-matrix NA message ------------------------

test_that("transition matrix NA error names from->to states, not raw row integers", {
  # Longform (cycle, from, to, value) matrix with an NA value.
  tm <- matrix(c(1, 1, 2, NA), nrow = 1, dimnames = list(NULL, c("cycle", "from", "to", "value")))
  expect_error(
    validate_cpp_inputs(
      c(1, 0), tm,
      data.frame(state = NA_character_, destination = NA_character_),
      value_names = "cost", state_names = c("well.1", "sick.1"),
      expanded_state_map = data.frame(from_state = character(0), .from_e = character(0)),
      half_cycle_method = "start"
    ),
    "well.1→sick.1"
  )
})


# --- Ceiling-not-floor: explicit limit is a maximum, reduce may trim below ---

test_that("explicit state_cycle_limit is a ceiling that reduce can trim below", {
  # A non-grouped state with limit 8 whose transitions AND values stop varying after
  # a few state cycles must trim below 8 under reduce_state_cycle = TRUE. (Trimming
  # requires every state-time-varying transition/value to settle; constant residency
  # values are stored as scalars and don't participate, so they are made to settle
  # explicitly here.)
  model <- define_model("markov") |>
    set_settings(n_cycles = 20, cycle_length = 1, cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0, reduce_state_cycle = TRUE) |>
    add_state("well", initial_prob = 1) |>
    add_state("sick", state_cycle_limit = 8, initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("well", "sick", 0.3) |>
    add_transition("well", "well", C) |>
    add_transition("sick", "dead", 0.05 + 0.02 * pmin(state_cycle, 3)) |>
    add_transition("sick", "sick", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("cost", 3000 + 100 * pmin(state_cycle, 4), state = "sick", type = "cost") |>
    add_value("cost", 1000, state = "well", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("q", 0.5 - 0.01 * pmin(state_cycle, 2), state = "sick", type = "outcome") |>
    add_value("q", 0.8, state = "well", type = "outcome") |>
    add_value("q", 0, state = "dead", type = "outcome") |>
    add_strategy("sc", "SC") |>
    add_summary("tc", "cost", type = "cost") |>
    add_summary("tq", "q", type = "outcome")
  result <- run_model(model)
  # everything settles by state_cycle 4 -> trimmed to 4, below the explicit ceiling of 8
  expect_lt(expanded_state_depth(result, "sick"), 8)
})
