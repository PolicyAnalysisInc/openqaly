context("Markov Models")

model <- system.file("models", "checkimab_simple", package = "heRomod2") %>%
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

  res <- heRomod2:::cppMarkovTransitionsAndTrace(
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

  res <- heRomod2:::cppMarkovTransitionsAndTrace(
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

  res <- heRomod2:::cppMarkovTransitionsAndTrace(
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

  res <- heRomod2:::cppMarkovTransitionsAndTrace(
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

# test_that('markov models are properly evaluated', {
#   expect_silent(results <- run_model(model))
#   tmat <- matrix(c(0.5, 0.3, 0.2, 0, 0.65, 0.35, 0, 0, 1), ncol = 3, byrow = T)
#   init_vec <- matrix(c(1, 0, 0), nrow = 1)
#   trace <- init_vec
#   cycle_tmat <- tmat
#   for (i in 1:12) {
#     trace <- rbind(trace, init_vec %*% cycle_tmat)
#     cycle_tmat <- cycle_tmat %*% tmat
#   }
#   trace
#   expect_equal(results$segments$trace_and_values[[1]][[1]], trace)

#   corrected_trace <- (trace[-1, ] + trace[-nrow(trace), ]) / 2
#   lys <- rowSums(corrected_trace[,1:2]) * 1/12
#   qalys <- rowSums(corrected_trace * cbind(rep(0.8*1/12, nrow(corrected_trace)), rep(0.65*1/12, nrow(corrected_trace)), rep(0, nrow(corrected_trace))))
#   values <- cbind(lys, qalys)

#   res_values <- cbind(
#     lys = rowSums(results$segments$trace_and_values[[1]][[3]][,,1]),
#     qalys = rowSums(results$segments$trace_and_values[[1]][[3]][,,2])
#   )

#   values
#   expect_equal(res_values, values)

# })
