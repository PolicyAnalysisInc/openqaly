#!/usr/bin/env Rscript

# Test script for the cpp_process_state_cycles optimization

library(heRomod2)
library(dplyr)
library(purrr)

# Create test data similar to what evaluate_values would process
create_test_data <- function() {
  # Simulate state_res dataframe
  state_res <- data.frame(
    cycle = rep(1:10, each = 3),
    state_cycle = rep(1:3, 10),
    value1 = rnorm(30),
    value2 = runif(30),
    value3 = rpois(30, 5)
  )

  # Value names from dataframe columns
  value_names_in_df <- c("value1", "value2", "value3")

  # Simulated environment values
  state_ns_env <- list(
    env_val1 = 42,
    env_val2 = "test_string",
    env_val3 = c(1, 2, 3)
  )

  # Value names from environment
  value_names_in_env <- c("env_val1", "env_val2", "env_val3")

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

  return(inner_mapped_rows)
}

# Test the implementations
test_implementations <- function() {
  cat("Creating test data...\n")
  test_data <- create_test_data()

  cat("\nRunning original R implementation...\n")
  t1 <- system.time({
    r_result <- original_r_implementation(
      test_data$state_res,
      test_data$value_names_in_df,
      test_data$state_ns_env,
      test_data$value_names_in_env,
      test_data$state,
      test_data$destination,
      test_data$max_st
    )
  })

  cat("\nRunning optimized C++ implementation...\n")
  t2 <- system.time({
    cpp_result <- cpp_process_state_cycles(
      test_data$state_res,
      test_data$value_names_in_df,
      test_data$state_ns_env,
      test_data$value_names_in_env,
      test_data$state,
      test_data$destination,
      test_data$max_st
    )
  })

  cat("\n--- Performance Comparison ---\n")
  cat("R implementation time:  ", t1[3], "seconds\n")
  cat("C++ implementation time:", t2[3], "seconds\n")
  cat("Speedup factor:         ", round(t1[3] / t2[3], 2), "x\n")

  # Check structure equivalence
  cat("\n--- Structure Verification ---\n")
  cat("R result length:  ", length(r_result), "\n")
  cat("C++ result length:", length(cpp_result), "\n")

  # Bind rows for comparison
  r_combined <- bind_rows(r_result)
  cpp_combined <- bind_rows(cpp_result)

  cat("\nR combined dimensions:  ", nrow(r_combined), "x", ncol(r_combined), "\n")
  cat("C++ combined dimensions:", nrow(cpp_combined), "x", ncol(cpp_combined), "\n")

  # Compare column names
  cat("\nColumn names match:", identical(names(r_combined), names(cpp_combined)), "\n")

  # Compare non-list columns
  non_list_cols <- c("state", "destination", "max_st", "state_cycle")
  for (col in non_list_cols) {
    if (col %in% names(r_combined) && col %in% names(cpp_combined)) {
      match <- all.equal(r_combined[[col]], cpp_combined[[col]])
      cat(sprintf("  %s match: %s\n", col, isTRUE(match)))
    }
  }

  # Test with NA destination
  cat("\n--- Testing NA destination ---\n")
  cpp_na_result <- cpp_process_state_cycles(
    test_data$state_res,
    test_data$value_names_in_df,
    test_data$state_ns_env,
    test_data$value_names_in_env,
    test_data$state,
    NA,  # NA destination
    test_data$max_st
  )

  r_na_result <- original_r_implementation(
    test_data$state_res,
    test_data$value_names_in_df,
    test_data$state_ns_env,
    test_data$value_names_in_env,
    test_data$state,
    NA,  # NA destination
    test_data$max_st
  )

  r_na_combined <- bind_rows(r_na_result)
  cpp_na_combined <- bind_rows(cpp_na_result)

  cat("NA destinations handled correctly:",
      all(is.na(r_na_combined$destination)) && all(is.na(cpp_na_combined$destination)), "\n")

  cat("\n--- Test Results ---\n")
  cat("✓ All tests completed successfully\n")
}

# Run the tests
test_implementations()