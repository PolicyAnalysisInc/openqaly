#!/usr/bin/env Rscript

library(heRomod2)
library(dplyr)

# Direct access to C++ function
cpp_process_state_cycles_fast <- function(state_res, value_names_in_df, state_ns_env,
                                         value_names_in_env, state, destination, max_st) {
  .Call("_heRomod2_cpp_process_state_cycles_fast",
        state_res, value_names_in_df, state_ns_env, value_names_in_env,
        state, destination, max_st, PACKAGE = "heRomod2")
}

# Create simple test data
state_res <- data.frame(
  cycle = c(1, 1, 2, 2, 3, 3),
  state_cycle = c(1, 2, 1, 2, 1, 2),
  value1 = c(1.1, 1.2, 2.1, 2.2, 3.1, 3.2),
  value2 = c(10, 20, 30, 40, 50, 60)
)

value_names_in_df <- c("value1", "value2")
state_ns_env <- list(env_val = 999)
value_names_in_env <- c("env_val")

cat("Input data:\n")
print(state_res)
cat("\n")

# Test the function
cat("Testing cpp_process_state_cycles_fast...\n")
tryCatch({
  result <- cpp_process_state_cycles_fast(
    state_res,
    value_names_in_df,
    state_ns_env,
    value_names_in_env,
    "state_A",
    "state_B",
    10.5
  )

  cat("Success! Result structure:\n")
  str(result)
  cat("\nResult data:\n")
  print(result)

}, error = function(e) {
  cat("Error occurred:\n")
  print(e)

  # Try debugging step by step
  cat("\nDebugging info:\n")
  cat("  state_res dimensions:", dim(state_res), "\n")
  cat("  value_names_in_df:", value_names_in_df, "\n")
  cat("  value_names_in_env:", value_names_in_env, "\n")
  cat("  unique state_cycles:", unique(state_res$state_cycle), "\n")
})