test_that("Half-cycle correction methods produce different results", {
  # Create a simple 2-state Markov model for testing
  # State 1 (Alive) -> State 2 (Dead) with fixed transition probability

  # Set up basic transition matrix
  n_cycles <- 10
  n_states <- 2
  trans_prob <- 0.1  # 10% transition probability per cycle

  # Create transition matrix for cppMarkovTransitionsAndTrace
  # Format: cycle, from, to, value
  transitions <- matrix(0, nrow = n_cycles * n_states * n_states, ncol = 4)
  row <- 1
  for (cycle in 1:n_cycles) {
    for (from_state in 1:n_states) {
      for (to_state in 1:n_states) {
        transitions[row, 1] <- cycle
        transitions[row, 2] <- from_state
        transitions[row, 3] <- to_state
        if (from_state == 1 && to_state == 2) {
          transitions[row, 4] <- trans_prob
        } else if (from_state == 1 && to_state == 1) {
          transitions[row, 4] <- 1 - trans_prob
        } else if (from_state == 2 && to_state == 2) {
          transitions[row, 4] <- 1
        } else {
          transitions[row, 4] <- 0
        }
        row <- row + 1
      }
    }
  }

  # Initial state: all in state 1
  init <- c(1, 0)
  state_names <- c("Alive", "Dead")

  # Create simple residency values (1 for alive, 0 for dead)
  residency_values <- data.frame(
    state = c("Alive", "Dead"),
    values_list = I(list(
      list(value = 1),
      list(value = 0)
    ))
  )
  names(residency_values$values_list[[1]]) <- "value"
  names(residency_values$values_list[[2]]) <- "value"

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  value_names <- c("value")

  # Test with "start" method
  result_start <- cppMarkovTransitionsAndTrace(
    transitions,
    empty_transitional,
    residency_values,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"
  )

  # Test with "end" method
  result_end <- cppMarkovTransitionsAndTrace(
    transitions,
    empty_transitional,
    residency_values,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "end"
  )

  # Test with "life-table" method
  result_lifetable <- cppMarkovTransitionsAndTrace(
    transitions,
    empty_transitional,
    residency_values,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "life-table"
  )

  # Extract values
  values_start <- result_start$values[, "value"]
  values_end <- result_end$values[, "value"]
  values_lifetable <- result_lifetable$values[, "value"]

  # All three methods should produce different results
  expect_false(all(values_start == values_end))
  expect_false(all(values_start == values_lifetable))
  expect_false(all(values_end == values_lifetable))

  # Life-table should be between start and end for most cycles
  # (except first and last where special rules apply)
  for (i in 2:(n_cycles-1)) {
    min_val <- min(values_start[i], values_end[i])
    max_val <- max(values_start[i], values_end[i])
    # Life-table should be approximately in the middle
    expect_true(values_lifetable[i] >= min_val && values_lifetable[i] <= max_val)
  }

  # The sum of values should follow the pattern: start > lifetable > end
  # (because we're measuring time alive, which decreases over time)
  expect_true(sum(values_start) > sum(values_lifetable))
  expect_true(sum(values_lifetable) > sum(values_end))
})

test_that("Life-table method implements half-cycle correction correctly", {
  # Create a simple test case where we can calculate the expected values
  n_cycles <- 5
  n_states <- 2

  # Create a deterministic transition where everyone moves from state 1 to 2
  # in cycle 3
  transitions <- matrix(0, nrow = n_cycles * n_states * n_states, ncol = 4)
  row <- 1
  for (cycle in 1:n_cycles) {
    for (from_state in 1:n_states) {
      for (to_state in 1:n_states) {
        transitions[row, 1] <- cycle
        transitions[row, 2] <- from_state
        transitions[row, 3] <- to_state
        if (cycle < 3) {
          # Stay in current state
          if (from_state == to_state) {
            transitions[row, 4] <- 1
          } else {
            transitions[row, 4] <- 0
          }
        } else if (cycle == 3) {
          # Transition from state 1 to state 2
          if (from_state == 1 && to_state == 2) {
            transitions[row, 4] <- 1
          } else if (from_state == 2 && to_state == 2) {
            transitions[row, 4] <- 1
          } else {
            transitions[row, 4] <- 0
          }
        } else {
          # After cycle 3, everyone stays in state 2
          if (from_state == 2 && to_state == 2) {
            transitions[row, 4] <- 1
          } else {
            transitions[row, 4] <- 0
          }
        }
        row <- row + 1
      }
    }
  }

  init <- c(1, 0)  # Start all in state 1
  state_names <- c("State1", "State2")

  # Create residency values
  residency_values <- data.frame(
    state = c("State1", "State2"),
    values_list = I(list(
      list(value1 = 1, value2 = 0),
      list(value1 = 0, value2 = 1)
    ))
  )
  names(residency_values$values_list[[1]]) <- c("value1", "value2")
  names(residency_values$values_list[[2]]) <- c("value1", "value2")

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  value_names <- c("value1", "value2")

  result_lifetable <- cppMarkovTransitionsAndTrace(
    transitions,
    empty_transitional,
    residency_values,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "life-table"
  )

  # Check the trace first to understand the state occupancy
  trace <- result_lifetable$trace

  # Expected trace:
  # Cycle 0: [1, 0]
  # Cycle 1: [1, 0]
  # Cycle 2: [1, 0]
  # Cycle 3: [0, 1]
  # Cycle 4: [0, 1]
  # Cycle 5: [0, 1]

  expect_equal(as.numeric(trace[1, ]), c(1, 0))  # Cycle 0
  expect_equal(as.numeric(trace[2, ]), c(1, 0))  # Cycle 1
  expect_equal(as.numeric(trace[3, ]), c(1, 0))  # Cycle 2
  expect_equal(as.numeric(trace[4, ]), c(0, 1))  # Cycle 3
  expect_equal(as.numeric(trace[5, ]), c(0, 1))  # Cycle 4
  expect_equal(as.numeric(trace[6, ]), c(0, 1))  # Cycle 5

  # With life-table method:
  # Cycle 1: average of trace[0] and trace[1] = (1+1)/2 = 1 in state 1
  # Cycle 2: average of trace[1] and trace[2] = (1+1)/2 = 1 in state 1
  # Cycle 3: average of trace[2] and trace[3] = (1+0)/2 = 0.5 in state 1, 0.5 in state 2
  # Cycle 4: average of trace[3] and trace[4] = (0+0)/2 = 0 in state 1, 1 in state 2
  # Cycle 5: just trace[5] = 0 in state 1, 1 in state 2 (last cycle)

  values <- result_lifetable$values
  expect_equal(values[1, "value1"], 1)
  expect_equal(values[2, "value1"], 1)
  expect_equal(values[3, "value1"], 0.5)
  expect_equal(values[4, "value1"], 0)
  expect_equal(values[5, "value1"], 0)

  expect_equal(values[1, "value2"], 0)
  expect_equal(values[2, "value2"], 0)
  expect_equal(values[3, "value2"], 0.5)
  expect_equal(values[4, "value2"], 1)
  expect_equal(values[5, "value2"], 1)
})

test_that("Half-cycle method setting defaults to 'start' and validates input", {
  # Test that convert_settings_from_df handles half_cycle_method correctly

  # Test default behavior (not specified)
  settings_df <- data.frame(
    setting = c("n_cycles", "model_type"),
    value = c("10", "markov")
  )
  settings <- convert_settings_from_df(settings_df)
  expect_equal(settings$half_cycle_method, "start")

  # Test valid values
  settings_df <- data.frame(
    setting = c("n_cycles", "half_cycle_method"),
    value = c("10", "life-table")
  )
  settings <- convert_settings_from_df(settings_df)
  expect_equal(settings$half_cycle_method, "life-table")

  settings_df$value[2] <- "end"
  settings <- convert_settings_from_df(settings_df)
  expect_equal(settings$half_cycle_method, "end")

  settings_df$value[2] <- "START"  # Test case insensitivity
  settings <- convert_settings_from_df(settings_df)
  expect_equal(settings$half_cycle_method, "start")

  # Test invalid value (should warn and default to "start")
  settings_df$value[2] <- "invalid"
  expect_warning(
    settings <- convert_settings_from_df(settings_df),
    "Invalid half_cycle_method"
  )
  expect_equal(settings$half_cycle_method, "start")
})

test_that("PSM model respects half-cycle method", {
  # Create a simple PSM model
  # We'll mock the necessary components since we're just testing the calculation logic

  # Create mock trace (3 states: PFS, Progressed, Dead)
  n_cycles <- 5
  trace <- matrix(
    c(1.0, 0.0, 0.0,  # Cycle 0
      0.8, 0.2, 0.0,  # Cycle 1
      0.6, 0.3, 0.1,  # Cycle 2
      0.4, 0.4, 0.2,  # Cycle 3
      0.2, 0.5, 0.3,  # Cycle 4
      0.1, 0.5, 0.4), # Cycle 5
    nrow = 6, ncol = 3, byrow = TRUE
  )
  colnames(trace) <- c("PFS", "Progressed", "Dead")

  # Create mock uneval_values
  uneval_values <- data.frame(
    name = c("utility_pfs", "utility_prog"),
    state = c("PFS", "Progressed"),
    destination = c(NA, NA),
    formula = I(list(
      as.heRoFormula("0.8"),
      as.heRoFormula("0.6")
    )),
    type = c("outcome", "outcome")
  )

  # Create mock namespace with env
  namespace <- list(
    df = data.frame(cycle = 1:n_cycles),
    env = new.env()  # Add the required environment
  )

  # Mock transitions (not used for residency values)
  trans_pfs_to_pp <- rep(0, n_cycles)
  trans_pp_to_dead <- rep(0, n_cycles)

  value_names <- c("utility_pfs", "utility_prog")
  state_names <- c("PFS", "Progressed", "Dead")

  # Test with "start" method
  values_start <- calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "start"
  )

  # Test with "end" method
  values_end <- calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "end"
  )

  # Test with "life-table" method
  values_lifetable <- calculate_psm_values(
    uneval_values, namespace, trace,
    trans_pfs_to_pp, trans_pp_to_dead,
    value_names, state_names, n_cycles,
    "life-table"
  )

  # Check that methods produce different results
  expect_false(all(values_start == values_end))
  expect_false(all(values_start == values_lifetable))
  expect_false(all(values_end == values_lifetable))

  # PSM trace is 0-indexed: trace[cycle,] for start method gives state probs at start of cycle
  # For "start" method, cycle 1 uses trace[1,] = [1.0, 0.0, 0.0]
  # PFS utility = 1.0 * 0.8 = 0.8
  expect_equal(values_start[1, "utility_pfs"], 0.8)

  # For "end" method, cycle 1 uses trace[2,] = [0.8, 0.2, 0.0]
  # PFS utility = 0.8 * 0.8 = 0.64
  expect_equal(values_end[1, "utility_pfs"], 0.64)

  # For "life-table" method, cycle 1 averages trace[1,] and trace[2,]
  # ((1.0 + 0.8) / 2) * 0.8 = 0.9 * 0.8 = 0.72
  expect_equal(values_lifetable[1, "utility_pfs"], 0.72)
})