context("cell_get and cell_set")

# ============================================================================
# Test Data
# ============================================================================

tbl <- data.frame(
  drug  = c("A", "B", "C"),
  age   = c(30, 50, 70),
  cost  = c(100, 200, 300),
  label = c("low", "mid", "high"),
  stringsAsFactors = FALSE
)

# ============================================================================
# cell_get — basic behavior
# ============================================================================

test_that("cell_get retrieves value with a single key", {
  expect_equal(cell_get(tbl, drug = "A", col = "cost"), 100)
  expect_equal(cell_get(tbl, drug = "B", col = "cost"), 200)
  expect_equal(cell_get(tbl, drug = "C", col = "label"), "high")
})

test_that("cell_get retrieves value with multiple keys", {
  expect_equal(cell_get(tbl, drug = "B", age = 50, col = "cost"), 200)
})

test_that("cell_get works with numeric keys", {
  expect_equal(cell_get(tbl, age = 70, col = "drug"), "C")
})

# ============================================================================
# cell_set — basic behavior
# ============================================================================

test_that("cell_set replaces a numeric cell", {
  result <- cell_set(tbl, drug = "A", col = "cost", value = 999)
  expect_equal(result$cost, c(999, 200, 300))
})

test_that("cell_set replaces a character cell", {
  result <- cell_set(tbl, drug = "B", col = "label", value = "replaced")
  expect_equal(result$label, c("low", "replaced", "high"))
})

test_that("cell_set returns a copy (original unchanged)", {
  original <- data.frame(key = c("a", "b"), val = c(1, 2), stringsAsFactors = FALSE)
  modified <- cell_set(original, key = "a", col = "val", value = 99)
  expect_equal(original$val, c(1, 2))
  expect_equal(modified$val, c(99, 2))
})

test_that("cell_set only changes the targeted cell", {
  result <- cell_set(tbl, drug = "B", col = "cost", value = 0)
  expect_equal(result$drug, tbl$drug)
  expect_equal(result$age, tbl$age)
  expect_equal(result$label, tbl$label)
  expect_equal(result$cost, c(100, 0, 300))
})

# ============================================================================
# Round-trip tests
# ============================================================================

test_that("cell_get then cell_set round-trip preserves data frame", {
  val <- cell_get(tbl, drug = "A", col = "cost")
  result <- cell_set(tbl, drug = "A", col = "cost", value = val)
  expect_equal(result, tbl)
})

test_that("get, modify, set changes only target cell", {
  val <- cell_get(tbl, drug = "A", col = "cost")
  result <- cell_set(tbl, drug = "A", col = "cost", value = val * 10)
  expect_equal(result$cost, c(1000, 200, 300))
})

# ============================================================================
# Error cases — cell_get
# ============================================================================

test_that("cell_get errors with no keys", {
  expect_error(cell_get(tbl, col = "cost"), "At least one key")
})

test_that("cell_get errors with unnamed keys", {
  expect_error(cell_get(tbl, "A", col = "cost"), "All keys.*must be named")
})

test_that("cell_get catches == misuse", {
  expect_error(
    cell_get(tbl, drug == "A", col = "cost"),
    "Use '=' instead of '=='"
  )
})

test_that("cell_get errors when col is not a single string", {
  expect_error(cell_get(tbl, drug = "A", col = 123), "`col` must be a single string")
  expect_error(cell_get(tbl, drug = "A", col = c("cost", "label")), "`col` must be a single string")
})

test_that("cell_get errors on unknown columns", {
  expect_error(cell_get(tbl, drug = "A", col = "missing"), "Unknown column")
  expect_error(cell_get(tbl, foo = "A", col = "cost"), "Unknown column")
})

test_that("cell_get errors when zero rows match", {
  expect_error(cell_get(tbl, drug = "Z", col = "cost"), "Expected exactly 1 matching row, found 0")
})

test_that("cell_get errors when multiple rows match", {
  dup <- rbind(tbl, tbl[1, ])
  expect_error(cell_get(dup, drug = "A", col = "cost"), "Expected exactly 1 matching row, found 2")
})

test_that("cell_get errors when key value length != 1", {
  expect_error(cell_get(tbl, drug = c("A", "B"), col = "cost"), "must be length 1")
})

# ============================================================================
# Error cases — cell_set
# ============================================================================

test_that("cell_set errors with no keys", {
  expect_error(cell_set(tbl, col = "cost", value = 0), "At least one key")
})

test_that("cell_set errors with unnamed keys", {
  expect_error(cell_set(tbl, "A", col = "cost", value = 0), "All keys.*must be named")
})

test_that("cell_set catches == misuse", {
  expect_error(
    cell_set(tbl, drug == "A", col = "cost", value = 999),
    "Use '=' instead of '=='"
  )
})

test_that("cell_set errors when col is not a single string", {
  expect_error(cell_set(tbl, drug = "A", col = 123, value = 0), "`col` must be a single string")
})

test_that("cell_set errors on unknown columns", {
  expect_error(cell_set(tbl, drug = "A", col = "missing", value = 0), "Unknown column")
  expect_error(cell_set(tbl, foo = "A", col = "cost", value = 0), "Unknown column")
})

test_that("cell_set errors when zero rows match", {
  expect_error(cell_set(tbl, drug = "Z", col = "cost", value = 0), "Expected exactly 1 matching row, found 0")
})

test_that("cell_set errors when multiple rows match", {
  dup <- rbind(tbl, tbl[1, ])
  expect_error(cell_set(dup, drug = "A", col = "cost", value = 0), "Expected exactly 1 matching row, found 2")
})

test_that("cell_set errors when key value length != 1", {
  expect_error(cell_set(tbl, drug = c("A", "B"), col = "cost", value = 0), "must be length 1")
})

# ============================================================================
# Integration tests — variable promotion in model
# ============================================================================

cost_tbl <- data.frame(
  state = c("healthy", "sick"),
  cost  = c(100, 500),
  stringsAsFactors = FALSE
)

build_cell_promotion_model <- function() {
  define_model("markov") |>
    set_settings(timeframe = 2, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0) |>
    add_table("cost_tbl", cost_tbl) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.5) |>
    add_transition("healthy", "dead", 0) |>
    add_transition("healthy", "healthy", 0.5) |>
    add_transition("sick", "dead", 0) |>
    add_transition("sick", "sick", 1) |>
    add_transition("dead", "dead", 1) |>
    add_strategy("standard") |>
    # Promote the "sick" cost cell
    add_variable("sick_cost",
      cell_get(cost_tbl, state = "sick", col = "cost")) |>
    # Use promoted variable directly in state values
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("cost", sick_cost, state = "sick", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("qaly", 1, state = "healthy") |>
    add_value("qaly", 0.5, state = "sick") |>
    add_value("qaly", 0, state = "dead") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qaly", "qaly", type = "outcome")
}

test_that("DSA on promoted cell variable produces different results", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_cell_promotion_model() |>
    add_dsa_variable("sick_cost", low = 100, high = 1000)

  results <- run_dsa(model)

  # Should have 3 runs: base, low, high
  expect_equal(nrow(results$dsa_metadata), 3)

  # Extract total_cost for each run
  agg <- results$aggregated
  costs <- agg %>%
    dplyr::left_join(
      dplyr::select(results$dsa_metadata, run_id, variation),
      by = "run_id"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_cost = summaries %>%
        dplyr::filter(summary == "total_cost") %>%
        dplyr::pull(amount)
    ) %>%
    dplyr::ungroup()

  base_cost <- costs$total_cost[costs$variation == "base"]
  low_cost  <- costs$total_cost[costs$variation == "low"]
  high_cost <- costs$total_cost[costs$variation == "high"]

  # Low sick_cost (100) should give lower total cost than high (1000)
  expect_true(low_cost < high_cost)
  # Base case (500) should be between low and high
  expect_true(low_cost < base_cost)
  expect_true(base_cost < high_cost)
})

test_that("PSA on promoted cell variable produces variation", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # Build model with sampling on the promoted variable directly
  model <- define_model("markov") |>
    set_settings(timeframe = 2, cycle_length = 1,
                 timeframe_unit = "years", cycle_length_unit = "years",
                 discount_cost = 0, discount_outcomes = 0) |>
    add_table("cost_tbl", cost_tbl) |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_transition("healthy", "sick", 0.5) |>
    add_transition("healthy", "dead", 0) |>
    add_transition("healthy", "healthy", 0.5) |>
    add_transition("sick", "dead", 0) |>
    add_transition("sick", "sick", 1) |>
    add_transition("dead", "dead", 1) |>
    add_strategy("standard") |>
    add_variable("sick_cost",
      cell_get(cost_tbl, state = "sick", col = "cost"),
      sampling = normal(bc, bc * 0.1)) |>
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("cost", sick_cost, state = "sick", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("qaly", 1, state = "healthy") |>
    add_value("qaly", 0.5, state = "sick") |>
    add_value("qaly", 0, state = "dead") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qaly", "qaly", type = "outcome")

  results <- run_psa(model, n_sim = 10, seed = 42)

  # Extract total_cost across simulations
  agg <- results$aggregated
  sim_costs <- agg %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_cost = summaries %>%
        dplyr::filter(summary == "total_cost") %>%
        dplyr::pull(amount)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::pull(total_cost)

  # Costs should vary across iterations (not all identical)
  expect_true(sd(sim_costs) > 0)
})

test_that("Scenario override on promoted cell variable changes results", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- build_cell_promotion_model() |>
    add_scenario("High Cost") |>
    add_scenario_variable("High Cost", "sick_cost", 1000)

  results <- run_scenario(model)

  agg <- results$aggregated
  base_cost <- agg %>%
    dplyr::filter(scenario_id == 1) %>%
    dplyr::pull(summaries) %>%
    .[[1]] %>%
    dplyr::filter(summary == "total_cost") %>%
    dplyr::pull(amount)

  scenario_cost <- agg %>%
    dplyr::filter(scenario_id == 2) %>%
    dplyr::pull(summaries) %>%
    .[[1]] %>%
    dplyr::filter(summary == "total_cost") %>%
    dplyr::pull(amount)

  # Scenario with sick_cost=1000 (vs base 500) should have higher total cost
  expect_true(scenario_cost > base_cost)
})
