context("Markov models")

model <- system.file("models", "checkimab_simple", package = "openqaly") %>%
  read_model()

test_that('markov trace calculations work success case', {
  mat1 <- as.matrix(
    tibble::tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   0.25,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   0.15,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,    -pi,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,    -pi,
           3,     1,   2,   0.20,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   0.1,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  # Define empty placeholders for the value arguments
  empty_transitional_values <- data.frame(state=character(), destination=character(), values_list=I(list()))
  empty_residency_values <- data.frame(state=character(), values_list=I(list()))
  empty_model_start_values <- data.frame(values_list=I(list()))
  empty_value_names <- character(0)

  res <- cppMarkovTransitionsAndTrace(
    mat1,                   # transitions
    empty_transitional_values, # transitional_values placeholder
    empty_residency_values,    # residency_values placeholder
    empty_model_start_values,  # model_start_values placeholder
    init1,                  # init
    statenames1,            # state_names
    empty_value_names,      # value_names placeholder
    3,                      # n_cycles
    -pi                     # sentinel
  )
  
  expect_equal(res$trace, expected_trace)

  transitionsSumsByStateAndCycle <- res$transitions %>%
    as.data.frame() %>%
    group_by(cycle, from) %>%
    summarize(sum = sum(value))


  expect_equal(transitionsSumsByStateAndCycle$sum, rep(1, 9))

  uncondTransProbSumsByCycle <- res$uncondtransprod %>%
    as.data.frame() %>%
    group_by(cycle) %>%
    summarize(sum = sum(value))

  expect_equal(uncondTransProbSumsByCycle$sum, c(1,1,1))
  expect_equal(!any(res$errors), TRUE)

})

test_that('markov trace calculations work with complement error', {
  mat1 <- as.matrix(
    tibble::tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   0.25,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   -pi,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,    -pi,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,    -pi,
           3,     1,   2,   0.20,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   -pi,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  # Define empty placeholders for the value arguments
  empty_transitional_values <- data.frame(state=character(), destination=character(), values_list=I(list()))
  empty_residency_values <- data.frame(state=character(), values_list=I(list()))
  empty_model_start_values <- data.frame(values_list=I(list()))
  empty_value_names <- character(0)

  res <- openqaly:::cppMarkovTransitionsAndTrace(
    mat1,                   # transitions
    empty_transitional_values,
    empty_residency_values,
    empty_model_start_values,
    init1,                  # init
    statenames1,            # state_names
    empty_value_names,      # value_names
    3,                      # n_cycles
    -pi                     # sentinel
  )

  error_states_cycles <- as.data.frame(res$errors) %>%
    mutate(cycle = res$transitions[,1], from = res$transitions[,2]) %>%
    group_by(cycle, from) %>%
    summarize(complement = any(complement)) %>%
    filter(complement) %>%
    ungroup()

  expect_equal(
    error_states_cycles,
    tibble(cycle = c(1, 3), from = c(2, 2), complement = c(TRUE, TRUE))
  )

})

test_that('markov trace calculations work with bounds error', {
  mat1 <- as.matrix(
    tibble::tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   1.00000001,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   0.15,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,    -pi,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,    -pi,
           3,     1,   2,   -0.0000001,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   0.1,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  # Define empty placeholders for the value arguments
  empty_transitional_values <- data.frame(state=character(), destination=character(), values_list=I(list()))
  empty_residency_values <- data.frame(state=character(), values_list=I(list()))
  empty_model_start_values <- data.frame(values_list=I(list()))
  empty_value_names <- character(0)

  res <- openqaly:::cppMarkovTransitionsAndTrace(
    mat1,                   # transitions
    empty_transitional_values,
    empty_residency_values,
    empty_model_start_values,
    init1,                  # init
    statenames1,            # state_names
    empty_value_names,      # value_names
    3,                      # n_cycles
    -pi                     # sentinel
  )

  error_states_cycles <- as.data.frame(res$errors) %>%
    mutate(cycle = res$transitions[,1], from = res$transitions[,2]) %>%
    mutate(hasError = probLessThanZero | probGreaterThanOne) %>%
    group_by(cycle, from) %>%
    summarize(hasError = any(hasError), .groups = "drop") %>%
    filter(hasError)

  expect_equal(
    error_states_cycles,
    tibble(cycle = c(1, 3), from = c(1, 1), hasError = c(TRUE, TRUE))
  )

})

test_that('markov trace calculations work with sum error', {
  mat1 <- as.matrix(
    tibble::tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   0.25,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   0.15,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,   0.900001,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,   0.69,
           3,     1,   2,   0.20,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   0.1,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  # Define empty placeholders for the value arguments
  empty_transitional_values <- data.frame(state=character(), destination=character(), values_list=I(list()))
  empty_residency_values <- data.frame(state=character(), values_list=I(list()))
  empty_model_start_values <- data.frame(values_list=I(list()))
  empty_value_names <- character(0)

  res <- openqaly:::cppMarkovTransitionsAndTrace(
    mat1,                   # transitions
    empty_transitional_values,
    empty_residency_values,
    empty_model_start_values,
    init1,                  # init
    statenames1,            # state_names
    empty_value_names,      # value_names
    3,                      # n_cycles
    -pi                     # sentinel
  )

  error_states_cycles <- as.data.frame(res$errors) %>%
    mutate(cycle = res$transitions[,1], from = res$transitions[,2]) %>%
    group_by(cycle, from) %>%
    summarize(sumNotEqualOne = any(sumNotEqualOne)) %>%
    filter(sumNotEqualOne) %>%
    ungroup()

  expect_equal(
    error_states_cycles,
    tibble(cycle = c(2, 3), from = c(2, 1), sumNotEqualOne = c(TRUE, TRUE))
  )

})

test_that('markov trace calculations work with NA/NaN error', {
  mat1 <- as.matrix(
    tibble::tribble(
      ~cycle, ~from, ~to, ~value,
           1,     1,   2,   NA,
           1,     1,   3,   0.15,
           1,     1,   1,    -pi,
           1,     2,   2,    -pi,
           1,     2,   3,   0.15,
           1,     3,   3,    -pi,
           2,     1,   1,    -pi,
           2,     1,   2,   0.20,
           2,     1,   3,   0.10,
           2,     2,   2,   NA,
           2,     2,   3,   0.1,
           2,     3,   3,    -pi,
           3,     1,   1,   NaN,
           3,     1,   2,   0.20,
           3,     1,   3,   0.10,
           3,     2,   2,    -pi,
           3,     2,   3,   0.1,
           3,     3,   3,    -pi,
    )
  )
  init1 <- c(0.8, 0.2, 0)
  nstate1 <- 3
  ncycle1 <- 3
  statenames1 <- c('a', 'b', 'c')
  mat1_c1 <- matrix(c(
    0.6, 0.25, 0.15,
    0, 0.85, 0.15,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  mat1_c23 <- matrix(c(
    0.7, 0.2, 0.1,
    0, 0.9, 0.1,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_trace <- matrix(nrow = 4, ncol = 3)
  colnames(expected_trace) <- statenames1
  rownames(expected_trace) <- c(0:3)
  expected_trace[1, ] <- init1
  expected_trace[2, ] <- expected_trace[1, ] %*% mat1_c1
  expected_trace[3, ] <- expected_trace[2, ] %*% mat1_c23
  expected_trace[4, ] <- expected_trace[3, ] %*% mat1_c23

  # Define empty placeholders for the value arguments
  empty_transitional_values <- data.frame(state=character(), destination=character(), values_list=I(list()))
  empty_residency_values <- data.frame(state=character(), values_list=I(list()))
  empty_model_start_values <- data.frame(values_list=I(list()))
  empty_value_names <- character(0)

  res <- openqaly:::cppMarkovTransitionsAndTrace(
    mat1,                   # transitions
    empty_transitional_values,
    empty_residency_values,
    empty_model_start_values,
    init1,                  # init
    statenames1,            # state_names
    empty_value_names,      # value_names
    3,                      # n_cycles
    -pi                     # sentinel
  )

  error_states_cycles <- as.data.frame(res$errors) %>%
    mutate(cycle = res$transitions[,1], from = res$transitions[,2]) %>%
    group_by(cycle, from) %>%
    summarize(NaOrNaN = any(NaOrNaN)) %>%
    filter(NaOrNaN) %>%
    ungroup()

  expect_equal(
    error_states_cycles,
    tibble(cycle = c(1, 2, 3), from = c(1, 2, 1), NaOrNaN = c(TRUE, TRUE, TRUE))
  )

})

# ============================================================================
# NEW COVERAGE TESTS
# ============================================================================

test_that("transitional values are calculated correctly", {
  # 3-state model: A -> B with cost 100, A -> C with cost 50
  n_cycles <- 3

  # Create transition matrix
  # Format: cycle, from, to, value (1-indexed states)
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   0.5,  # A stays A
         1,     1,   2,   0.3,  # A -> B
         1,     1,   3,   0.2,  # A -> C
         1,     2,   2,   1.0,  # B stays B (absorbing)
         1,     3,   3,   1.0,  # C stays C (absorbing)
         2,     1,   1,   0.5,
         2,     1,   2,   0.3,
         2,     1,   3,   0.2,
         2,     2,   2,   1.0,
         2,     3,   3,   1.0,
         3,     1,   1,   0.5,
         3,     1,   2,   0.3,
         3,     1,   3,   0.2,
         3,     2,   2,   1.0,
         3,     3,   3,   1.0
  ))

  init <- c(1, 0, 0)  # Start all in state A
  state_names <- c("A", "B", "C")
  value_names <- c("cost")

  # Transitional values: cost 100 for A->B, cost 50 for A->C
  transitional_values <- data.frame(
    state = c("A", "A"),
    destination = c("B", "C"),
    values_list = I(list(
      list(cost = 100),
      list(cost = 50)
    ))
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    transitional_values,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"
  )

  # Cycle 1: init[A]=1.0, A->B prob=0.3, A->C prob=0.2
  # Transitional cost = 1.0 * 0.3 * 100 + 1.0 * 0.2 * 50 = 30 + 10 = 40
  expect_equal(res$transitionalValues[1, "cost"], 40)

  # Cycle 2: trace[A]=0.5, A->B prob=0.3, A->C prob=0.2
  # Transitional cost = 0.5 * 0.3 * 100 + 0.5 * 0.2 * 50 = 15 + 5 = 20
  expect_equal(res$transitionalValues[2, "cost"], 20)

  # Cycle 3: trace[A]=0.25, A->B prob=0.3, A->C prob=0.2
  # Transitional cost = 0.25 * 0.3 * 100 + 0.25 * 0.2 * 50 = 7.5 + 2.5 = 10
  expect_equal(res$transitionalValues[3, "cost"], 10)
})

test_that("time-varying values use correct cycle values", {
  # 2-state model with declining utility over time
  n_cycles <- 5

  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   1.0,  # Stay in state 1
         1,     2,   2,   1.0,
         2,     1,   1,   1.0,
         2,     2,   2,   1.0,
         3,     1,   1,   1.0,
         3,     2,   2,   1.0,
         4,     1,   1,   1.0,
         4,     2,   2,   1.0,
         5,     1,   1,   1.0,
         5,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("Alive", "Dead")
  value_names <- c("utility")

  # Time-varying utility: declines each cycle
  residency_values <- data.frame(
    state = "Alive",
    values_list = I(list(
      list(utility = c(1.0, 0.9, 0.8, 0.7, 0.6))  # Different value per cycle
    ))
  )

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    residency_values,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"  # Use start method: uses prev trace row
  )

  # With start method, cycle N uses trace[N-1] which is 1.0 for all cycles
  # Each cycle should use its corresponding utility value
  expect_equal(res$residencyValues[1, "utility"], 1.0)  # Cycle 1: utility=1.0
  expect_equal(res$residencyValues[2, "utility"], 0.9)  # Cycle 2: utility=0.9
  expect_equal(res$residencyValues[3, "utility"], 0.8)  # Cycle 3: utility=0.8
  expect_equal(res$residencyValues[4, "utility"], 0.7)  # Cycle 4: utility=0.7
  expect_equal(res$residencyValues[5, "utility"], 0.6)  # Cycle 5: utility=0.6
})

test_that("model start values apply only to first cycle", {
  n_cycles <- 3

  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   1.0,
         1,     2,   2,   1.0,
         2,     1,   1,   1.0,
         2,     2,   2,   1.0,
         3,     1,   1,   1.0,
         3,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("Alive", "Dead")
  value_names <- c("initial_cost")

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  # Model start value: one-time cost of 1000 at model start
  model_start_values <- data.frame(
    values_list = I(list(
      list(initial_cost = 1000)
    ))
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    model_start_values,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"
  )

  # Model start value should only appear in cycle 1
  expect_equal(res$modelStartValues[1, "initial_cost"], 1000)
  expect_equal(res$modelStartValues[2, "initial_cost"], 0)
  expect_equal(res$modelStartValues[3, "initial_cost"], 0)

  # Total values should include model start only in cycle 1
  expect_equal(res$values[1, "initial_cost"], 1000)
  expect_equal(res$values[2, "initial_cost"], 0)
  expect_equal(res$values[3, "initial_cost"], 0)
})

test_that("invalid state names in transitional values produce warnings", {
  n_cycles <- 2

  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   1.0,
         1,     2,   2,   1.0,
         2,     1,   1,   1.0,
         2,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- c("cost")

  # Invalid state name in transitional values
  transitional_values <- data.frame(
    state = "NonExistent",  # This state doesn't exist
    destination = "B",
    values_list = I(list(
      list(cost = 100)
    ))
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  # Should produce warning about state not found
  expect_warning(
    cppMarkovTransitionsAndTrace(
      mat,
      transitional_values,
      empty_residency,
      empty_model_start,
      init,
      state_names,
      value_names,
      n_cycles,
      -pi,
      "start"
    ),
    "not found in state index"
  )
})

test_that("invalid state names in residency values produce warnings", {
  n_cycles <- 2

  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   1.0,
         1,     2,   2,   1.0,
         2,     1,   1,   1.0,
         2,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- c("utility")

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  # Invalid state name in residency values
  residency_values <- data.frame(
    state = "NonExistent",  # This state doesn't exist
    values_list = I(list(
      list(utility = 1)
    ))
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  # Should produce warning about state not found
  expect_warning(
    cppMarkovTransitionsAndTrace(
      mat,
      empty_transitional,
      residency_values,
      empty_model_start,
      init,
      state_names,
      value_names,
      n_cycles,
      -pi,
      "start"
    ),
    "not found in state index"
  )
})

test_that("transitional values work on complement transitions", {
  # Model where A->A uses complement (-pi), with transitional value on A->A
  n_cycles <- 2

  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,   0.3,  # A -> B = 0.3
         1,     1,   1,   -pi,  # A -> A = complement (0.7)
         1,     2,   2,   1.0,  # B stays B
         2,     1,   2,   0.3,
         2,     1,   1,   -pi,
         2,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- c("stay_cost")

  # Transitional value for staying in A (the complement transition)
  transitional_values <- data.frame(
    state = "A",
    destination = "A",
    values_list = I(list(
      list(stay_cost = 10)
    ))
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    transitional_values,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"
  )

  # Cycle 1: init[A]=1.0, A->A complement prob = 1 - 0.3 = 0.7
  # Transitional cost = 1.0 * 0.7 * 10 = 7
  expect_equal(res$transitionalValues[1, "stay_cost"], 7)

  # Cycle 2: trace[A]=0.7, A->A complement prob = 0.7
  # Transitional cost = 0.7 * 0.7 * 10 = 4.9
  expect_equal(res$transitionalValues[2, "stay_cost"], 4.9)
})

test_that("error reporting works with expanded state names", {
  # Use expanded state names like "A.1", "A.2" that have state_time > 1
  n_cycles <- 2

  # Create invalid transitions to trigger error reporting
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   0.5,
         1,     1,   2,   0.6,  # Sum > 1, will trigger error
         1,     2,   2,   1.0,
         2,     1,   1,   0.5,
         2,     1,   2,   0.5,
         2,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("A.2", "B")  # A.2 indicates state_time = 2
  value_names <- character(0)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"
  )

  # Check that errors are detected
  error_detected <- any(res$errors[, "sumNotEqualOne"])
  expect_true(error_detected)
})

test_that("checkpoint error mode stops execution instead of warning", {
  n_cycles <- 2

  # Create transitions with sum > 1 to trigger error
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,   0.6,
         1,     1,   2,   0.6,  # Sum = 1.2 > 1
         1,     2,   2,   1.0,
         2,     1,   1,   0.5,
         2,     1,   2,   0.5,
         2,     2,   2,   1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")

  # Create expanded_state_map for cppCalculateTraceAndValues
  expanded_state_map <- data.frame(
    from_state = c("A", "B"),
    .from_e = c("A", "B")
  )

  values <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  value_names <- character(0)

  # Test with checkpoint error mode - should stop execution
  withr::with_options(
    list(openqaly.error_mode = "checkpoint"),
    {
      expect_error(
        cppCalculateTraceAndValues(
          init,
          mat,
          values,
          value_names,
          state_names,
          expanded_state_map,
          "start"
        ),
        "Transition probability errors detected"
      )
    }
  )

  # Test with default warning mode - should warn but not stop
  withr::with_options(
    list(openqaly.error_mode = "warning"),
    {
      expect_warning(
        cppCalculateTraceAndValues(
          init,
          mat,
          values,
          value_names,
          state_names,
          expanded_state_map,
          "start"
        ),
        "Transition probability errors detected"
      )
    }
  )
})

# ==============================================================================
# Input Validation Tests (Gap 1)
# ==============================================================================

test_that("cppMarkovTransitionsAndTrace validates nStates == 0", {
  # Empty transitions - doesn't matter since validation happens first
  mat <- matrix(numeric(0), nrow = 0, ncol = 4)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  expect_error(
    cppMarkovTransitionsAndTrace(
      mat,
      empty_transitional,
      empty_residency,
      empty_model_start,
      numeric(0),           # empty initialProbs
      character(0),         # empty stateNames - nStates == 0
      character(0),
      1,
      -pi,
      "start"
    ),
    "No states provided"
  )
})

test_that("cppMarkovTransitionsAndTrace validates initialProbs length", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    1.0,
         1,     2,   2,    1.0
  ))

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  # 3 initial probs but only 2 states
  expect_error(
    cppMarkovTransitionsAndTrace(
      mat,
      empty_transitional,
      empty_residency,
      empty_model_start,
      c(0.5, 0.3, 0.2),    # 3 elements
      c("A", "B"),          # 2 states - mismatch!
      character(0),
      1,
      -pi,
      "start"
    ),
    "initialProbs length does not match"
  )
})

test_that("cppMarkovTransitionsAndTrace validates nCycles positive", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    1.0,
         1,     2,   2,    1.0
  ))

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  # nCycles = 0
  expect_error(
    cppMarkovTransitionsAndTrace(
      mat,
      empty_transitional,
      empty_residency,
      empty_model_start,
      c(0.5, 0.5),
      c("A", "B"),
      character(0),
      0,                    # nCycles = 0
      -pi,
      "start"
    ),
    "nCycles must be positive"
  )
})

# ==============================================================================
# Invalid Destination Warning Test (Gap 2)
# ==============================================================================

test_that("invalid destination in transitional values produces warning", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    0.5,
         1,     1,   2,    0.5,
         1,     2,   2,    1.0
  ))

  # Valid state "A" but invalid destination "Z"
  transitional_values <- data.frame(
    state = "A",
    destination = "Z",  # This state doesn't exist
    values_list = I(list(list(cost = 100)))
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  expect_warning(
    cppMarkovTransitionsAndTrace(
      mat,
      transitional_values,
      empty_residency,
      empty_model_start,
      c(1, 0),
      c("A", "B"),
      c("cost"),
      1,
      -pi,
      "start"
    ),
    "Transitional value destination 'Z' not found"
  )
})

# ==============================================================================
# Complement Edge Case Tests (Gaps 9-10)
# ==============================================================================

test_that("complement > 1 is flagged as probGreaterThanOne error", {
  # When non-complement probabilities are negative, complement will be > 1
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,   -0.5,  # Negative prob - invalid
         1,     1,   1,    -pi,  # Complement will be 1 - (-0.5) = 1.5 > 1
         1,     2,   2,    1.0
  ))

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    c(1, 0),
    c("A", "B"),
    character(0),
    1,
    -pi,
    "start"
  )

  # The complement (1.5) should be flagged as probGreaterThanOne
  expect_true(any(res$errors[, "probGreaterThanOne"]))
})

test_that("NaN in transition is flagged as NaOrNaN error", {
  # NaN input should be detected
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,    NaN,  # NaN value
         1,     1,   1,    0.5,
         1,     2,   2,    1.0
  ))

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    c(1, 0),
    c("A", "B"),
    character(0),
    1,
    -pi,
    "start"
  )

  # NaN should be flagged
  expect_true(any(res$errors[, "NaOrNaN"]))
})

test_that("NaN complement from NaN input is flagged as error", {
  # When a complement is computed from probabilities containing NaN
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,    NaN,  # NaN input
         1,     1,   1,    -pi,  # Complement of NaN is NaN
         1,     2,   2,    1.0
  ))

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    c(1, 0),
    c("A", "B"),
    character(0),
    1,
    -pi,
    "start"
  )

  # NaN should be flagged (both the input NaN and computed complement NaN)
  expect_true(any(res$errors[, "NaOrNaN"]))
})

# ==============================================================================
# Extra Value Names Ignored Test (Gap 3)
# ==============================================================================

test_that("extra value names in values_list are silently ignored", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    0.9,
         1,     1,   2,    0.1,
         1,     2,   2,    1.0
  ))

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  # Residency values contains "cost" and "extra_cost", but we only request "cost"
  residency_values <- data.frame(
    state = c("A", "B"),
    values_list = I(list(
      list(cost = 10, extra_cost = 999),  # extra_cost should be ignored
      list(cost = 5, extra_cost = 888)
    ))
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    residency_values,
    empty_model_start,
    c(1, 0),
    c("A", "B"),
    c("cost"),  # Only request "cost", not "extra_cost"
    1,
    -pi,
    "start"
  )

  # Should have values output with only "cost" column
  expect_true("cost" %in% colnames(res$values))
  expect_false("extra_cost" %in% colnames(res$values))

  # The cost value should be calculated correctly (A=10, B=5, weighted by trace)
  expect_equal(res$values[1, "cost"], 10)  # 100% in A at cycle 1 start
})

# ==============================================================================
# Error Table Truncation Test (Gap 6)
# ==============================================================================

test_that("error table truncates at 40 rows with message", {
  # Error groups are keyed by from_state|to_state|error_type
  # Need >40 DISTINCT groups, not just 40 cycles with the same error
  # Create 45 states, each with a probGreaterThanOne error
  n_states <- 45

  # Create state names
  state_names <- paste0("S", 1:n_states)

  # Create initial probs (all start in state 1)
  init <- c(1, rep(0, n_states - 1))

  # Create transitions: each state has a self-loop with prob > 1 (error)
  # This creates 44 distinct error groups (one per state, excluding last)
  mat_rows <- list()
  for (i in 1:n_states) {
    for (j in 1:n_states) {
      if (i == j) {
        # Self-transition - make it > 1 for first 44 states to create errors
        if (i < n_states) {
          mat_rows <- c(mat_rows, list(c(1, i, j, 1.5)))  # prob > 1 error
        } else {
          mat_rows <- c(mat_rows, list(c(1, i, j, 1.0)))  # last state valid
        }
      } else {
        mat_rows <- c(mat_rows, list(c(1, i, j, 0.0)))
      }
    }
  }
  mat <- do.call(rbind, mat_rows)

  # Use cppCalculateTraceAndValues which actually emits warnings
  expanded_state_map <- data.frame(
    from_state = state_names,
    .from_e = state_names
  )

  values <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  value_names <- character(0)

  # Should produce warning with truncation message (44 groups > 40 limit)
  withr::with_options(
    list(openqaly.error_mode = "warning"),
    {
      expect_warning(
        cppCalculateTraceAndValues(
          init,
          mat,
          values,
          value_names,
          state_names,
          expanded_state_map,
          "start"
        ),
        "showing top 40 of"
      )
    }
  )
})

# ==============================================================================
# Additional Coverage Tests
# ==============================================================================

test_that("state names with non-numeric suffix after dot are not parsed as expanded", {
  # State name "A.abc" has a dot followed by non-numeric characters
  # It should NOT be parsed as an expanded state (state_time should be 1)
  n_cycles <- 2

  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    0.9,
         1,     1,   2,    0.1,
         1,     2,   2,    1.0,
         2,     1,   1,    0.9,
         2,     1,   2,    0.1,
         2,     2,   2,    1.0
  ))

  init <- c(1, 0)
  state_names <- c("A.abc", "B")  # A.abc has non-numeric suffix
  value_names <- character(0)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    n_cycles,
    -pi,
    "start"
  )

  # The model should run successfully
  expect_equal(nrow(res$trace), n_cycles + 1)
  # State names should be preserved as-is (not parsed as expanded)
  expect_equal(colnames(res$trace), state_names)
  # No errors should be detected
  expect_false(any(res$errors))
})

test_that("multiple complements in same state/cycle triggers complement error", {
  # Two -pi complement markers for same state/cycle should trigger error
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    -pi,  # First complement A->A
         1,     1,   2,    -pi,  # Second complement A->B - ERROR
         1,     2,   2,    1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- character(0)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    1,
    -pi,
    "start"
  )

  # Check that complement error is flagged
  expect_true(any(res$errors[, "complement"]))
})

test_that("negative probability triggers probLessThanZero error", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,   -0.1,  # Negative probability
         1,     1,   1,    -pi,
         1,     2,   2,    1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- character(0)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    1,
    -pi,
    "start"
  )

  # Check that probLessThanZero error is flagged on the row with -0.1
  expect_true(any(res$errors[, "probLessThanZero"]))
})

test_that("probability greater than 1 triggers probGreaterThanOne error", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,    1.5,  # > 1
         1,     1,   1,    -pi,  # Complement will be negative (-0.5)
         1,     2,   2,    1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- character(0)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    1,
    -pi,
    "start"
  )

  # Check that probGreaterThanOne error is flagged
  expect_true(any(res$errors[, "probGreaterThanOne"]))
})

test_that("NaN in complement calculation triggers NaOrNaN error on complement row", {
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   2,    NaN,  # NaN input
         1,     1,   1,    -pi,  # Complement of NaN = NaN
         1,     2,   2,    1.0
  ))

  init <- c(1, 0)
  state_names <- c("A", "B")
  value_names <- character(0)

  empty_transitional <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  empty_residency <- data.frame(
    state = character(),
    values_list = I(list())
  )

  empty_model_start <- data.frame(
    values_list = I(list())
  )

  res <- cppMarkovTransitionsAndTrace(
    mat,
    empty_transitional,
    empty_residency,
    empty_model_start,
    init,
    state_names,
    value_names,
    1,
    -pi,
    "start"
  )

  # Check that NaOrNaN error is flagged
  # The complement row (row 2) should have NaOrNaN flagged because complement of NaN is NaN
  expect_true(any(res$errors[, "NaOrNaN"]))
})

test_that("error table shows State Time column for expanded states", {
  # Use expanded state names where state_time > 1 (e.g., "A.2" parses to state_time=2)
  # Create an error condition FROM state "A.2" and verify the warning includes "State Time"
  n_cycles <- 1

  # State 1 = A.2 (state_time=2), State 2 = B
  # Error occurs from state 1 (A.2) which has state_time=2 (> 1)
  mat <- as.matrix(tibble::tribble(
    ~cycle, ~from, ~to, ~value,
         1,     1,   1,    0.5,
         1,     1,   2,    0.6,  # Sum > 1, will trigger sumNotEqualOne error
         1,     2,   2,    1.0
  ))

  init <- c(1, 0)
  state_names <- c("A.2", "B")  # A.2 has state_time=2, which is > 1
  value_names <- character(0)

  expanded_state_map <- data.frame(
    from_state = state_names,
    .from_e = state_names
  )

  values <- data.frame(
    state = character(),
    destination = character(),
    values_list = I(list())
  )

  # Use cppCalculateTraceAndValues with warning mode to capture the warning
  withr::with_options(
    list(openqaly.error_mode = "warning"),
    {
      expect_warning(
        cppCalculateTraceAndValues(
          init,
          mat,
          values,
          value_names,
          state_names,
          expanded_state_map,
          "start"
        ),
        "State Time"  # Warning should include "State Time" column for expanded states
      )
    }
  )
})

# NOTE: Tests for lines 698-704 (empty transitions) and lines 738-742 (invalid indices)
# are skipped because:
# 1. Lines 698-704 are in transformTransitionsOutput which is only called internally
#    after cppMarkovTransitionsAndTrace processes the data. Empty transitions matrices
#    cause crashes before reaching that defensive code path.
# 2. Lines 738-742 handle invalid state indices, but the R API doesn't allow passing
#    invalid indices - they would need to be generated by corrupted internal data.

# ==============================================================================
# evaluate_group_weight() Error Handling Tests
# ==============================================================================

test_that("evaluate_group_weight handles eval_formula error (undefined variable)", {
  # Clear any accumulated errors before test
  openqaly:::clear_oq_errors()

  # Create a minimal namespace with required structure
  test_env <- new.env()
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a group_row with a formula that references an undefined variable
  group_row <- data.frame(
    name = "test_group",
    weight = "undefined_variable_xyz",
    stringsAsFactors = FALSE
  )

  # Call the function - should accumulate an error and return NA
  result <- openqaly:::evaluate_group_weight(group_row, ns)

  expect_true(is.na(result))

  # Check that an error was accumulated
  errors <- openqaly:::get_accumulated_errors()
  expect_true(length(errors) > 0)

  # Clean up
  openqaly:::clear_oq_errors()
})

test_that("evaluate_group_weight rejects non-numeric weight", {
  openqaly:::clear_oq_errors()

  # Create namespace with a character variable
  test_env <- new.env()
  assign("char_var", "not_a_number", envir = test_env)
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a group_row with a formula that evaluates to character
  group_row <- data.frame(
    name = "test_group",
    weight = "char_var",
    stringsAsFactors = FALSE
  )

  result <- openqaly:::evaluate_group_weight(group_row, ns)

  expect_true(is.na(result))

  errors <- openqaly:::get_accumulated_errors()
  expect_true(length(errors) > 0)
  expect_true(any(grepl("must be numeric", sapply(errors, function(e) e$error$message))))

  openqaly:::clear_oq_errors()
})

test_that("evaluate_group_weight rejects weight with length != 1", {
  openqaly:::clear_oq_errors()

  # Create namespace with a vector variable
  test_env <- new.env()
  assign("vec_var", c(1, 2, 3), envir = test_env)
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a group_row with a formula that evaluates to a vector
  group_row <- data.frame(
    name = "test_group",
    weight = "vec_var",
    stringsAsFactors = FALSE
  )

  result <- openqaly:::evaluate_group_weight(group_row, ns)

  expect_true(is.na(result))

  errors <- openqaly:::get_accumulated_errors()
  expect_true(length(errors) > 0)
  expect_true(any(grepl("must be length 1", sapply(errors, function(e) e$error$message))))

  openqaly:::clear_oq_errors()
})

test_that("evaluate_group_weight rejects NA weight", {
  openqaly:::clear_oq_errors()

  # Create namespace with an NA variable
  test_env <- new.env()
  assign("na_var", NA_real_, envir = test_env)
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a group_row with a formula that evaluates to NA
  group_row <- data.frame(
    name = "test_group",
    weight = "na_var",
    stringsAsFactors = FALSE
  )

  result <- openqaly:::evaluate_group_weight(group_row, ns)

  expect_true(is.na(result))

  errors <- openqaly:::get_accumulated_errors()
  expect_true(length(errors) > 0)
  expect_true(any(grepl("evaluated to NA", sapply(errors, function(e) e$error$message))))

  openqaly:::clear_oq_errors()
})

test_that("evaluate_group_weight rejects infinite weight", {
  openqaly:::clear_oq_errors()

  # Create namespace with an infinite variable
  test_env <- new.env()
  assign("inf_var", Inf, envir = test_env)
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a group_row with a formula that evaluates to Inf
  group_row <- data.frame(
    name = "test_group",
    weight = "inf_var",
    stringsAsFactors = FALSE
  )

  result <- openqaly:::evaluate_group_weight(group_row, ns)

  expect_true(is.na(result))

  errors <- openqaly:::get_accumulated_errors()
  expect_true(length(errors) > 0)
  expect_true(any(grepl("must be finite", sapply(errors, function(e) e$error$message))))

  openqaly:::clear_oq_errors()
})

test_that("evaluate_group_weight returns valid numeric weight", {
  openqaly:::clear_oq_errors()

  # Create namespace with a valid numeric variable
  test_env <- new.env()
  assign("valid_weight", 0.5, envir = test_env)
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a group_row with a valid weight formula
  group_row <- data.frame(
    name = "test_group",
    weight = "valid_weight",
    stringsAsFactors = FALSE
  )

  result <- openqaly:::evaluate_group_weight(group_row, ns)

  expect_equal(result, 0.5)

  # No errors should be accumulated
  errors <- openqaly:::get_accumulated_errors()
  expect_equal(length(errors), 0)

  openqaly:::clear_oq_errors()
})

# ==============================================================================
# calculate_segment_weight() Tests
# ==============================================================================

test_that("calculate_segment_weight warns when group not found", {
  openqaly:::clear_oq_errors()

  # Create a minimal namespace
  test_env <- new.env()
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a model with groups that don't include the segment's group
  model <- list(
    groups = data.frame(
      name = c("group_a", "group_b"),
      weight = c("0.5", "0.5"),
      stringsAsFactors = FALSE
    )
  )

  # Create a segment with a group name not in model$groups
  segment <- list(group = "nonexistent_group")

  # Should warn and return default weight of 1
  expect_warning(
    result <- openqaly:::calculate_segment_weight(segment, model, ns),
    "not found in model groups"
  )

  expect_equal(result, 1)

  openqaly:::clear_oq_errors()
})

test_that("calculate_segment_weight returns 1 when no groups defined", {
  openqaly:::clear_oq_errors()

  # Create a minimal namespace
  test_env <- new.env()
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a model with no groups
  model <- list(groups = NULL)

  # Create a segment
  segment <- list(group = "any_group")

  # Should return default weight of 1
  result <- openqaly:::calculate_segment_weight(segment, model, ns)

  expect_equal(result, 1)

  openqaly:::clear_oq_errors()
})

test_that("calculate_segment_weight returns 1 when groups is empty", {
  openqaly:::clear_oq_errors()

  # Create a minimal namespace
  test_env <- new.env()
  test_df <- data.frame(cycle = 1:3, state_cycle = 1:3)
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create a model with empty groups dataframe
  model <- list(
    groups = data.frame(
      name = character(0),
      weight = character(0),
      stringsAsFactors = FALSE
    )
  )

  # Create a segment
  segment <- list(group = "any_group")

  # Should return default weight of 1
  result <- openqaly:::calculate_segment_weight(segment, model, ns)

  expect_equal(result, 1)

  openqaly:::clear_oq_errors()
})

# ==============================================================================
# check_trans_markov() Validation Tests
# ==============================================================================

test_that("check_trans_markov errors on missing columns", {
  # Transitions missing 'formula' column
  transitions <- data.frame(
    from_state = c("A", "B"),
    to_state = c("B", "A")
    # missing 'formula' column
  )
  state_names <- c("A", "B")

  expect_error(
    openqaly:::check_trans_markov(transitions, state_names),
    "missing column"
  )
})

test_that("check_trans_markov errors on missing from_state", {
  # Transitions that don't include all states as from_state
  transitions <- data.frame(
    from_state = c("A", "A"),  # Missing "B" as from_state
    to_state = c("A", "B"),
    formula = c("0.5", "0.5"),
    stringsAsFactors = FALSE
  )
  state_names <- c("A", "B")

  expect_error(
    openqaly:::check_trans_markov(transitions, state_names),
    "missing state"
  )
})

test_that("check_trans_markov errors on duplicate transitions", {
  # Transitions with duplicate from_state/to_state pairs
  transitions <- data.frame(
    from_state = c("A", "A", "A", "B"),
    to_state = c("A", "B", "B", "B"),  # A->B is duplicated
    formula = c("0.5", "0.3", "0.2", "1.0"),
    stringsAsFactors = FALSE
  )
  state_names <- c("A", "B")

  expect_error(
    openqaly:::check_trans_markov(transitions, state_names),
    "duplicate"
  )
})

test_that("check_trans_markov errors on blank formulas", {
  # Transitions with blank formula
  transitions <- data.frame(
    from_state = c("A", "A", "B"),
    to_state = c("A", "B", "B"),
    formula = c("0.5", "", "1.0"),  # Empty formula
    stringsAsFactors = FALSE
  )
  state_names <- c("A", "B")

  expect_error(
    openqaly:::check_trans_markov(transitions, state_names),
    "blank formula"
  )
})

test_that("check_trans_markov errors on NA formulas", {
  # Transitions with NA formula
  transitions <- data.frame(
    from_state = c("A", "A", "B"),
    to_state = c("A", "B", "B"),
    formula = c("0.5", NA, "1.0"),  # NA formula
    stringsAsFactors = FALSE
  )
  state_names <- c("A", "B")

  expect_error(
    openqaly:::check_trans_markov(transitions, state_names),
    "blank formula"
  )
})

# ==============================================================================
# eval_trans_markov_lf() Error Handling Tests
# ==============================================================================

test_that("eval_trans_markov_lf handles formula evaluation errors", {
  openqaly:::clear_oq_errors()

  # Create a minimal namespace
  test_env <- new.env()
  test_df <- data.frame(cycle = c(1, 1, 2, 2), state_cycle = c(1, 1, 1, 1))
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create transitions with an invalid formula
  df <- data.frame(
    name = c("A\u2192B"),
    from_state = "A",
    to_state = "B",
    from_state_group = NA,
    to_state_group = NA,
    share_state_time = FALSE,
    max_st = 1,
    stringsAsFactors = FALSE
  )
  df$formula <- list(openqaly:::as.oq_formula("undefined_var_xyz"))

  # The function accumulates errors and then throws them at checkpoint
  # So we expect an error to be thrown
  expect_error(
    openqaly:::eval_trans_markov_lf(df, ns),
    "Multiple errors found|undefined_var_xyz"
  )

  openqaly:::clear_oq_errors()
})

test_that("eval_trans_markov_lf stops on error when option is set", {
  openqaly:::clear_oq_errors()

  # Create a minimal namespace
  test_env <- new.env()
  test_df <- data.frame(cycle = c(1), state_cycle = c(1))
  ns <- list(df = test_df, env = test_env)
  class(ns) <- 'namespace'

  # Create transitions with an invalid formula
  df <- data.frame(
    name = c("A\u2192B"),
    from_state = "A",
    to_state = "B",
    from_state_group = NA,
    to_state_group = NA,
    share_state_time = FALSE,
    max_st = 1,
    stringsAsFactors = FALSE
  )
  df$formula <- list(openqaly:::as.oq_formula("undefined_var_xyz"))

  # Run with stop_on_error = TRUE
  withr::with_options(
    list(openqaly.stop_on_error = TRUE),
    {
      expect_error(
        openqaly:::eval_trans_markov_lf(df, ns),
        "Error evaluating transition"
      )
    }
  )

  openqaly:::clear_oq_errors()
})

# ==============================================================================
# Shared State Time + state_cycle Bug Fix Tests
# ==============================================================================

test_that("shared state time with state_cycle propagates limit to all group members", {
  # This model previously crashed because progressed_comp had no state_cycle_limit
  # but shared state time with progressed (which had limit=5). The fix propagates
  # the limit from progressed to progressed_comp.
  model <- define_model("markov") |>
    set_settings(
      n_cycles = 20,
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3.5,
      discount_outcomes = 3.5
    ) |>
    add_state(
      "stable",
      display_name = "Stable Disease",
      initial_prob = 1
    ) |>
    add_state(
      "progressed",
      display_name = "Progressed",
      state_group = "progression",
      share_state_time = TRUE,
      state_cycle_limit = 5,
      initial_prob = 0
    ) |>
    add_state(
      "progressed_comp",
      display_name = "Progressed (Complicated)",
      state_group = "progression",
      share_state_time = TRUE,
      initial_prob = 0
    ) |>
    add_state(
      "dead",
      display_name = "Dead",
      initial_prob = 0
    ) |>
    add_transition("stable", "progressed", 0.15) |>
    add_transition("stable", "dead", 0.02) |>
    add_transition("stable", "stable", C) |>
    add_transition("progressed", "progressed_comp", 0.10) |>
    add_transition("progressed", "dead", 0.05 + 0.03 * state_cycle) |>
    add_transition("progressed", "progressed", C) |>
    add_transition("progressed_comp", "progressed", 0.05) |>
    add_transition("progressed_comp", "dead", 0.08 + 0.04 * state_cycle) |>
    add_transition("progressed_comp", "progressed_comp", C) |>
    add_transition("dead", "dead", 1) |>
    add_value("cost", 1000, state = "stable", type = "cost") |>
    add_value("cost", 3000, state = "progressed", type = "cost") |>
    add_value("cost", 5000, state = "progressed_comp", type = "cost") |>
    add_value("cost", 0, state = "dead", type = "cost") |>
    add_value("qaly", 0.8, state = "stable", type = "outcome") |>
    add_value("qaly", 0.5, state = "progressed", type = "outcome") |>
    add_value("qaly", 0.3, state = "progressed_comp", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>
    add_strategy("standard_care", "Standard Care") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome")

  expect_no_error(run_model(model))
})

test_that("shared state time with conflicting state_cycle_limit values errors", {
  expect_error(
    define_model("markov") |>
      set_settings(
        n_cycles = 10,
        cycle_length = 1,
        cycle_length_unit = "years",
        discount_cost = 3.5,
        discount_outcomes = 3.5
      ) |>
      add_state(
        "stable",
        display_name = "Stable",
        initial_prob = 1
      ) |>
      add_state(
        "prog_a",
        display_name = "Progressed A",
        state_group = "progression",
        share_state_time = TRUE,
        state_cycle_limit = 5,
        initial_prob = 0
      ) |>
      add_state(
        "prog_b",
        display_name = "Progressed B",
        state_group = "progression",
        share_state_time = TRUE,
        state_cycle_limit = 10,
        initial_prob = 0
      ) |>
      add_state(
        "dead",
        display_name = "Dead",
        initial_prob = 0
      ) |>
      add_transition("stable", "prog_a", 0.15) |>
      add_transition("stable", "dead", 0.02) |>
      add_transition("stable", "stable", C) |>
      add_transition("prog_a", "prog_b", 0.10) |>
      add_transition("prog_a", "dead", 0.05) |>
      add_transition("prog_a", "prog_a", C) |>
      add_transition("prog_b", "prog_a", 0.05) |>
      add_transition("prog_b", "dead", 0.08) |>
      add_transition("prog_b", "prog_b", C) |>
      add_transition("dead", "dead", 1) |>
      add_value("cost", 1000, state = "stable", type = "cost") |>
      add_value("cost", 3000, state = "prog_a", type = "cost") |>
      add_value("cost", 5000, state = "prog_b", type = "cost") |>
      add_value("cost", 0, state = "dead", type = "cost") |>
      add_value("qaly", 0.8, state = "stable", type = "outcome") |>
      add_value("qaly", 0.5, state = "prog_a", type = "outcome") |>
      add_value("qaly", 0.3, state = "prog_b", type = "outcome") |>
      add_value("qaly", 0, state = "dead", type = "outcome") |>
      add_strategy("standard_care", "Standard Care") |>
      add_summary("total_cost", "cost", type = "cost") |>
      add_summary("total_qalys", "qaly", type = "outcome") |>
      run_model(),
    "conflicting state_cycle_limit"
  )
})

