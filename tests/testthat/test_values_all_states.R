context("Values All/All Other state targeting")

# =============================================================================
# Helper: build a minimal model up to the point where values can be added
# =============================================================================
build_base_model <- function() {
  define_model("markov") |>
    set_settings(
      n_cycles = 5,
      timeframe = 5,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 0,
      discount_outcomes = 0
    ) |>
    add_strategy("standard") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_sick", 0.1) |>
    add_variable("p_death", 0.05) |>
    add_variable("c_healthy", 1000) |>
    add_variable("c_sick", 5000) |>
    add_variable("u_healthy", 0.9) |>
    add_variable("u_sick", 0.5) |>
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death") |>
    add_transition("sick", "dead", "0.2") |>
    add_transition("sick", "sick", "0.8") |>
    add_transition("dead", "dead", "1")
}

# =============================================================================
# Reserved State Name Tests
# =============================================================================

test_that("add_state rejects 'All' as a state name", {
  expect_error(
    define_model("markov") |> add_state("All", initial_prob = 1),
    "reserved state name"
  )
})

test_that("add_state rejects 'All Other' as a state name", {
  expect_error(
    define_model("markov") |> add_state("All Other", initial_prob = 1),
    "reserved state name"
  )
})

test_that("check_states rejects reserved names in raw state tibbles", {
  bad_states <- tibble::tibble(
    name = c("healthy", "All"),
    initial_probability = c("1", "0"),
    display_name = c("healthy", "All"),
    description = c("healthy", "All")
  )
  expect_error(
    openqaly:::check_states(bad_states),
    "Reserved state name"
  )
})

# =============================================================================
# Builder Validation Tests (add_value)
# =============================================================================

test_that("add_value accepts 'All' for residence values", {
  model <- build_base_model() |>
    add_value("cost", "c_healthy", state = "All", type = "cost")
  expect_true(any(model$values$state == "All"))
})

test_that("add_value rejects 'All' for transition values", {
  expect_error(
    build_base_model() |>
      add_value("cost", "c_healthy", state = "All", destination = "sick", type = "cost"),
    "cannot be used for transition values"
  )
})

test_that("add_value rejects 'All' when other residence rows exist for same name", {
  expect_error(
    build_base_model() |>
      add_value("cost", "c_healthy", state = "healthy", type = "cost") |>
      add_value("cost", "c_sick", state = "All", type = "cost"),
    "already has residence value rows"
  )
})

test_that("add_value rejects adding explicit state when 'All' exists for same name", {
  expect_error(
    build_base_model() |>
      add_value("cost", "c_healthy", state = "All", type = "cost") |>
      add_value("cost", "c_sick", state = "sick", type = "cost"),
    "already uses state 'All'"
  )
})

test_that("add_value accepts 'All Other' for residence values", {
  model <- build_base_model() |>
    add_value("cost", "c_healthy", state = "healthy", type = "cost") |>
    add_value("cost", "0", state = "All Other", type = "cost")
  expect_true(any(model$values$state == "All Other"))
})

test_that("add_value rejects 'All Other' for transition values", {
  expect_error(
    build_base_model() |>
      add_value("cost", "c_healthy", state = "All Other", destination = "sick", type = "cost"),
    "cannot be used for transition values"
  )
})

test_that("add_value rejects duplicate 'All Other' for same name", {
  expect_error(
    build_base_model() |>
      add_value("cost", "c_healthy", state = "All Other", type = "cost") |>
      add_value("cost", "c_sick", state = "All Other", type = "cost"),
    "already has an 'All Other'"
  )
})

test_that("check_values_df catches All/All Other rules on raw tibbles", {
  # "All" coexists with explicit state
  bad_values <- tibble::tibble(
    name = c("cost", "cost"),
    formula = c("100", "200"),
    state = c("All", "healthy"),
    destination = c(NA_character_, NA_character_),
    display_name = c("cost", "cost"),
    description = c("cost", "cost"),
    type = c("cost", "cost")
  )
  expect_error(
    openqaly:::check_values_df(bad_values),
    "uses state 'All' but also has other"
  )
})

# =============================================================================
# Expansion Tests
# =============================================================================

test_that("expand_all_states_in_values expands 'All' to one row per state", {
  state_names <- c("healthy", "sick", "dead")
  values_df <- tibble::tibble(
    name = "cost",
    formula = "100",
    state = "All",
    destination = NA_character_,
    display_name = "cost",
    description = "cost",
    type = "cost"
  )
  result <- openqaly:::expand_all_states_in_values(values_df, state_names)
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$state), sort(state_names))
  expect_true(all(result$formula == "100"))
})

test_that("expand_all_states_in_values expands 'All Other' to uncovered states only", {
  state_names <- c("healthy", "sick", "dead")
  values_df <- tibble::tibble(
    name = c("cost", "cost"),
    formula = c("1000", "0"),
    state = c("healthy", "All Other"),
    destination = c(NA_character_, NA_character_),
    display_name = c("cost", "cost"),
    description = c("cost", "cost"),
    type = c("cost", "cost")
  )
  result <- openqaly:::expand_all_states_in_values(values_df, state_names)
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$state), sort(state_names))
  # healthy should keep its original formula
  expect_equal(result$formula[result$state == "healthy"], "1000")
  # sick and dead should get "All Other" formula
  expect_equal(result$formula[result$state == "sick"], "0")
  expect_equal(result$formula[result$state == "dead"], "0")
})

test_that("expand_all_states_in_values preserves non-residence rows unchanged", {
  state_names <- c("healthy", "sick", "dead")
  values_df <- tibble::tibble(
    name = c("cost", "trans_cost"),
    formula = c("100", "500"),
    state = c("All", "healthy"),
    destination = c(NA_character_, "sick"),
    display_name = c("cost", "trans_cost"),
    description = c("cost", "trans_cost"),
    type = c("cost", "cost")
  )
  result <- openqaly:::expand_all_states_in_values(values_df, state_names)
  # 3 expanded + 1 non-residence
  expect_equal(nrow(result), 4)
  non_res <- result[!is.na(result$destination) & result$destination != "NA", ]
  expect_equal(nrow(non_res), 1)
  expect_equal(non_res$state, "healthy")
  expect_equal(non_res$destination, "sick")
})

test_that("expand_all_states_in_values errors when 'All Other' has no room", {
  state_names <- c("healthy", "sick")
  values_df <- tibble::tibble(
    name = c("cost", "cost", "cost"),
    formula = c("100", "200", "0"),
    state = c("healthy", "sick", "All Other"),
    destination = c(NA_character_, NA_character_, NA_character_),
    display_name = c("cost", "cost", "cost"),
    description = c("cost", "cost", "cost"),
    type = c("cost", "cost", "cost")
  )
  expect_error(
    openqaly:::expand_all_states_in_values(values_df, state_names),
    "no remaining states"
  )
})

test_that("expand_all_states_in_values preserves display_name, description, type", {
  state_names <- c("healthy", "sick", "dead")
  values_df <- tibble::tibble(
    name = "cost",
    formula = "100",
    state = "All",
    destination = NA_character_,
    display_name = "Total Cost",
    description = "All state cost",
    type = "cost"
  )
  result <- openqaly:::expand_all_states_in_values(values_df, state_names)
  expect_true(all(result$display_name == "Total Cost"))
  expect_true(all(result$description == "All state cost"))
  expect_true(all(result$type == "cost"))
})

test_that("expand_all_states_in_values handles multiple value names independently", {
  state_names <- c("healthy", "sick", "dead")
  values_df <- tibble::tibble(
    name = c("cost", "qalys"),
    formula = c("100", "0.9"),
    state = c("All", "All"),
    destination = c(NA_character_, NA_character_),
    display_name = c("cost", "qalys"),
    description = c("cost", "qalys"),
    type = c("cost", "outcome")
  )
  result <- openqaly:::expand_all_states_in_values(values_df, state_names)
  expect_equal(nrow(result), 6)
  expect_equal(nrow(result[result$name == "cost", ]), 3)
  expect_equal(nrow(result[result$name == "qalys", ]), 3)
})

# =============================================================================
# Integration Tests
# =============================================================================

test_that("full model with 'All' state runs and produces correct values", {
  model <- build_base_model() |>
    add_value("cost", "c_healthy", state = "All", type = "cost") |>
    add_value("qalys", "u_healthy", state = "All", type = "outcome") |>
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qalys")

  results <- run_model(model)
  expect_true(!is.null(results))

  vals <- get_values(results, format = "long")
  expect_true(nrow(vals) > 0)
})

test_that("full model with 'All Other' state runs and produces correct values", {
  model <- build_base_model() |>
    add_value("cost", "c_healthy", state = "healthy", type = "cost") |>
    add_value("cost", "c_sick", state = "sick", type = "cost") |>
    add_value("cost", "0", state = "All Other", type = "cost") |>
    add_value("qalys", "u_healthy", state = "healthy", type = "outcome") |>
    add_value("qalys", "0", state = "All Other", type = "outcome") |>
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qalys")

  results <- run_model(model)
  expect_true(!is.null(results))

  vals <- get_values(results, format = "long")
  expect_true(nrow(vals) > 0)
})

test_that("JSON serialization round-trip preserves 'All' and 'All Other'", {
  model <- build_base_model() |>
    add_value("cost", "c_healthy", state = "All", type = "cost") |>
    add_value("qalys", "u_healthy", state = "healthy", type = "outcome") |>
    add_value("qalys", "0", state = "All Other", type = "outcome") |>
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qalys")

  json_str <- write_model_json(model)
  restored <- read_model_json(json_str)

  # Check "All" is preserved
  cost_vals <- restored$values[restored$values$name == "cost", ]
  expect_true("All" %in% cost_vals$state)

  # Check "All Other" is preserved
  qaly_vals <- restored$values[restored$values$name == "qalys", ]
  expect_true("All Other" %in% qaly_vals$state)
})

test_that("code generation outputs 'All' and 'All Other' correctly", {
  model <- build_base_model() |>
    add_value("cost", "c_healthy", state = "All", type = "cost") |>
    add_value("qalys", "u_healthy", state = "healthy", type = "outcome") |>
    add_value("qalys", "0", state = "All Other", type = "outcome") |>
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qalys")

  code <- model_to_r_code(model)
  code_str <- paste(code, collapse = "\n")

  expect_true(grepl('state = "All"', code_str, fixed = TRUE))
  expect_true(grepl('state = "All Other"', code_str, fixed = TRUE))
})
