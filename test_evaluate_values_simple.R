library(heRomod2)
library(microbenchmark)
library(dplyr)
library(tibble)
library(purrr)

# Create a simple test to verify the optimized function works
cat("Testing evaluate_values optimization...\n\n")

# Create test data
set.seed(123)
n_states <- 3
n_cycles <- 50
n_values <- 5

# Create a simple uneval_values dataframe
test_df <- expand.grid(
  state = c("Healthy", "Sick", "Dead"),
  destination = c("Healthy", "Sick", "Dead"),
  state_cycle = 1:5
) %>%
  mutate(
    max_st = 5,
    formula = list(quote(1 + cycle))  # Simple formula
  ) %>%
  as_tibble()

# Add parsed formula objects (simplified)
for (i in 1:nrow(test_df)) {
  test_df$formula[[i]] <- list(
    quo = rlang::quo(1 + cycle),
    depends = character(0),
    after = character(0),
    fo_depends = character(0)
  )
  class(test_df$formula[[i]]) <- "heRoFormula"
}

# Add some value columns directly
test_df$cost <- 100
test_df$qaly <- 0.8
test_df$name <- "test_value"
test_df$display_name <- "Test Value"
test_df$description <- "A test value"

# Create namespace
test_ns <- list(
  df = data.frame(
    cycle = rep(1:n_cycles, each = 5),
    state_cycle = rep(1:5, n_cycles)
  ),
  env = new.env()
)
class(test_ns) <- "namespace"

value_names <- c("cost", "qaly")
state_names <- c("Healthy", "Sick", "Dead")

# Function to save original implementation
evaluate_values_original <- function(df, ns, value_names, state_names, simplify = FALSE) {
  # Just do the grouping and return a simple result for testing
  if (nrow(df) == 0) {
    return(tibble(
      state = character(0),
      destination = character(0),
      max_st = numeric(0),
      state_cycle = numeric(0),
      values_list = list()
    ))
  }

  # Group by state and destination
  grouped_df_list <- df %>%
    group_by(.data$state, .data$destination) %>%
    group_split()

  # For testing, just evaluate and create simple output
  results <- list()
  for (g in 1:length(grouped_df_list)) {
    x_group <- grouped_df_list[[g]]

    # Create a simple result
    result <- tibble(
      state = x_group$state[1],
      destination = x_group$destination[1],
      max_st = x_group$max_st[1],
      state_cycle = x_group$state_cycle,
      values_list = lapply(1:nrow(x_group), function(i) {
        list(cost = 100, qaly = 0.8)
      })
    )
    results[[g]] <- result
  }

  bind_rows(results) %>%
    arrange(factor(state, levels = state_names))
}

# Test the optimized version exists
if (exists("evaluate_values_cpp")) {
  cat("✓ C++ function is available\n\n")

  # Try running the optimized version
  tryCatch({
    cat("Running optimized version...\n")
    result <- evaluate_values(test_df, test_ns, value_names, state_names, FALSE)
    cat("✓ Optimized version ran successfully\n")
    cat("  Result dimensions:", nrow(result), "rows x", ncol(result), "columns\n\n")

    # Compare with a simple baseline
    cat("Running baseline version...\n")
    result_baseline <- evaluate_values_original(test_df, test_ns, value_names, state_names, FALSE)
    cat("✓ Baseline version ran successfully\n")
    cat("  Result dimensions:", nrow(result_baseline), "rows x", ncol(result_baseline), "columns\n\n")

    # Simple benchmark
    cat("Benchmarking (small dataset)...\n")
    bench_small <- microbenchmark(
      Baseline = evaluate_values_original(test_df, test_ns, value_names, state_names, FALSE),
      Optimized = evaluate_values(test_df, test_ns, value_names, state_names, FALSE),
      times = 50
    )
    print(summary(bench_small)[, c("expr", "min", "mean", "median", "max")])

    # Create larger dataset for better benchmark
    large_df <- expand.grid(
      state = paste0("S", 1:10),
      destination = paste0("S", 1:10),
      state_cycle = 1:20
    ) %>%
      mutate(
        max_st = 20,
        cost = 100,
        qaly = 0.8
      ) %>%
      as_tibble()

    # Add formula column
    for (i in 1:nrow(large_df)) {
      large_df$formula[[i]] <- list(
        quo = rlang::quo(1),
        depends = character(0),
        after = character(0),
        fo_depends = character(0)
      )
      class(large_df$formula[[i]]) <- "heRoFormula"
    }

    large_ns <- list(
      df = data.frame(
        cycle = rep(1:100, each = 20),
        state_cycle = rep(1:20, 100)
      ),
      env = new.env()
    )
    class(large_ns) <- "namespace"

    cat("\nBenchmarking (large dataset: ", nrow(large_df), " rows)...\n", sep = "")
    bench_large <- microbenchmark(
      Optimized = evaluate_values(large_df, large_ns, c("cost", "qaly"), paste0("S", 1:10), FALSE),
      times = 10
    )
    print(summary(bench_large)[, c("expr", "min", "mean", "median", "max")])

  }, error = function(e) {
    cat("✗ Error running optimized version:\n")
    cat("  ", e$message, "\n")
  })

} else {
  cat("✗ C++ function not found\n")
  cat("  Make sure the package was compiled with the C++ code\n")
}