#!/usr/bin/env Rscript

# Test script for the optimized cpp_process_state_cycles_fast function

library(heRomod2)
library(dplyr)
library(purrr)
library(microbenchmark)

# Access the C++ function directly
cpp_process_state_cycles_fast <- function(state_res, value_names_in_df, state_ns_env,
                                         value_names_in_env, state, destination, max_st) {
  .Call("_heRomod2_cpp_process_state_cycles_fast",
        state_res, value_names_in_df, state_ns_env, value_names_in_env,
        state, destination, max_st, PACKAGE = "heRomod2")
}

# Create test data with various sizes
create_test_data <- function(n_cycles = 100, n_state_cycles = 10, n_values = 20) {
  total_rows <- n_cycles * n_state_cycles

  # Simulate state_res dataframe with many value columns
  state_res <- data.frame(
    cycle = rep(1:n_cycles, each = n_state_cycles),
    state_cycle = rep(1:n_state_cycles, n_cycles)
  )

  # Add value columns
  value_names_in_df <- paste0("value", 1:n_values)
  for (col_name in value_names_in_df) {
    state_res[[col_name]] <- rnorm(total_rows)
  }

  # Simulated environment values
  state_ns_env <- list(
    env_val1 = 42,
    env_val2 = "test_string",
    env_val3 = c(1, 2, 3),
    env_val4 = 100.5,
    env_val5 = TRUE
  )

  # Value names from environment
  value_names_in_env <- names(state_ns_env)

  # Metadata
  state <- "state_A"
  destination <- "state_B"
  max_st <- 10.5

  return(list(
    state_res = state_res,
    value_names_in_df = value_names_in_df,
    state_ns_env = state_ns_env,
    value_names_in_env = value_names_in_env,
    state = state,
    destination = destination,
    max_st = max_st
  ))
}

# Original R implementation for comparison
original_r_implementation <- function(state_res, value_names_in_df, state_ns_env,
                                     value_names_in_env, state, destination, max_st) {
  expanded_state_res_list <- state_res %>%
    group_by(state_cycle) %>%
    group_split()

  inner_mapped_rows <- map(expanded_state_res_list, function(state_cycle_df) {
    expanded_state_values_list <- append(
      as.list(state_cycle_df[, value_names_in_df, drop = FALSE]),
      state_ns_env[value_names_in_env]
    )

    tibble(
      state = state,
      destination = destination,
      max_st = max_st,
      state_cycle = state_cycle_df$state_cycle[1],
      values_list = list(expanded_state_values_list)
    )
  })

  return(bind_rows(inner_mapped_rows))
}

# Test with different data sizes
test_performance <- function() {
  cat("=== Performance Testing ===\n\n")

  # Test different sizes
  test_configs <- list(
    list(name = "Small", n_cycles = 10, n_state_cycles = 5, n_values = 10),
    list(name = "Medium", n_cycles = 50, n_state_cycles = 10, n_values = 20),
    list(name = "Large", n_cycles = 100, n_state_cycles = 20, n_values = 30)
  )

  for (config in test_configs) {
    cat("Testing", config$name, "dataset:\n")
    cat(sprintf("  - %d cycles x %d state_cycles x %d values\n",
                config$n_cycles, config$n_state_cycles, config$n_values))

    test_data <- create_test_data(config$n_cycles, config$n_state_cycles, config$n_values)

    # Benchmark
    results <- microbenchmark(
      R_original = original_r_implementation(
        test_data$state_res, test_data$value_names_in_df,
        test_data$state_ns_env, test_data$value_names_in_env,
        test_data$state, test_data$destination, test_data$max_st
      ),
      Cpp_fast = cpp_process_state_cycles_fast(
        test_data$state_res, test_data$value_names_in_df,
        test_data$state_ns_env, test_data$value_names_in_env,
        test_data$state, test_data$destination, test_data$max_st
      ),
      times = 10
    )

    summary_stats <- summary(results)
    r_mean <- summary_stats$mean[summary_stats$expr == "R_original"]
    cpp_mean <- summary_stats$mean[summary_stats$expr == "Cpp_fast"]
    speedup <- r_mean / cpp_mean

    cat(sprintf("  R implementation:   %.2f ms\n", r_mean / 1e6))
    cat(sprintf("  C++ implementation: %.2f ms\n", cpp_mean / 1e6))
    cat(sprintf("  Speedup:            %.1fx\n\n", speedup))
  }

  # Verify correctness with a smaller dataset
  cat("=== Correctness Verification ===\n")
  test_data <- create_test_data(10, 3, 5)

  r_result <- original_r_implementation(
    test_data$state_res, test_data$value_names_in_df,
    test_data$state_ns_env, test_data$value_names_in_env,
    test_data$state, test_data$destination, test_data$max_st
  )

  cpp_result <- cpp_process_state_cycles_fast(
    test_data$state_res, test_data$value_names_in_df,
    test_data$state_ns_env, test_data$value_names_in_env,
    test_data$state, test_data$destination, test_data$max_st
  )

  # Compare structure
  cat("Structure comparison:\n")
  cat("  Dimensions match:",
      nrow(r_result) == nrow(cpp_result) && ncol(r_result) == ncol(cpp_result), "\n")
  cat("  Column names match:", identical(names(r_result), names(cpp_result)), "\n")

  # Compare non-list columns
  non_list_cols <- c("state", "destination", "max_st", "state_cycle")
  for (col in non_list_cols) {
    match <- all.equal(r_result[[col]], cpp_result[[col]])
    cat(sprintf("  %s values match: %s\n", col, isTRUE(match)))
  }

  # Test with NA destination
  cat("\nTesting NA destination:\n")
  r_na <- original_r_implementation(
    test_data$state_res, test_data$value_names_in_df,
    test_data$state_ns_env, test_data$value_names_in_env,
    test_data$state, NA, test_data$max_st
  )

  cpp_na <- cpp_process_state_cycles_fast(
    test_data$state_res, test_data$value_names_in_df,
    test_data$state_ns_env, test_data$value_names_in_env,
    test_data$state, NA, test_data$max_st
  )

  cat("  NA destinations handled correctly:",
      all(is.na(r_na$destination)) && all(is.na(cpp_na$destination)), "\n")

  cat("\n✓ All tests completed\n")
}

# Run the tests
test_performance()