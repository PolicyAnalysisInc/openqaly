library(heRomod2)
library(microbenchmark)
library(dplyr)
library(tibble)
library(purrr)

# Load a test model
model_path <- system.file("bookdown/models/example", package = "heRomod2")
if (model_path == "") {
  # If no example model, create a minimal test case
  set.seed(123)

  # Create test data similar to what evaluate_values expects
  n_states <- 5
  n_cycles <- 100
  n_values <- 10

  # Create uneval_values dataframe
  test_df <- expand.grid(
    state = paste0("S", 1:n_states),
    destination = paste0("S", 1:n_states),
    state_cycle = 1:10
  ) %>%
    mutate(
      max_st = 10,
      formula = "1"
    )

  # Add some value columns
  for (i in 1:n_values) {
    test_df[[paste0("value", i)]] <- runif(nrow(test_df))
  }

  # Create namespace
  test_ns <- list(
    df = data.frame(
      cycle = rep(1:n_cycles, each = 10),
      state_cycle = rep(1:10, n_cycles)
    ),
    env = new.env()
  )
  class(test_ns) <- "namespace"

  value_names <- paste0("value", 1:n_values)
  state_names <- paste0("S", 1:n_states)

} else {
  # Load actual model
  model <- read_model(model_path)

  # Run through normal processing to get the inputs for evaluate_values
  # This would normally be done in run_markov
  # For now, create simplified test data

  test_df <- data.frame(
    state = rep(c("Healthy", "Sick", "Dead"), each = 10),
    destination = rep(c("Healthy", "Sick", "Dead"), each = 10),
    max_st = rep(10, 30),
    state_cycle = rep(1:10, 3),
    formula = rep("1", 30),
    stringsAsFactors = FALSE
  )

  # Add value columns
  test_df$cost <- runif(30, 100, 1000)
  test_df$qaly <- runif(30, 0.5, 1.0)

  test_ns <- list(
    df = data.frame(
      cycle = 1:100,
      state_cycle = rep(1:10, 10)
    ),
    env = new.env()
  )
  class(test_ns) <- "namespace"

  value_names <- c("cost", "qaly")
  state_names <- c("Healthy", "Sick", "Dead")
}

# Create a version that uses the old R implementation
evaluate_values_original <- function(df, ns, value_names, state_names, simplify = FALSE) {

  if (nrow(df) == 0) {
    empty_evaluated_values <- tibble(
      state = character(0),
      destination = character(0),
      max_st = numeric(0),
      state_cycle = numeric(0),
      values_list = list(),
      .rows = 0
    )
    return(arrange(empty_evaluated_values, factor(state, levels = state_names)))
  }

  # Original R implementation
  grouped_df_list <- df %>%
    group_by(.data$state, .data$destination) %>%
    group_split()

  processed_groups_and_na_info <- map(grouped_df_list, function(x_group) {
      # Simplified eval_variables for testing - just returns the namespace
      state_ns <- clone_namespace(ns)
      state_res <- state_ns$df

      state_res$state <- x_group$state[1]

      value_names_in_df <- intersect(colnames(state_res), value_names)
      value_names_in_env <- intersect(names(state_ns$env), value_names)

      current_max_st <- x_group$max_st[1]

      expanded_state_res_list <- state_res %>%
        group_by(state_cycle) %>%
        group_split()

      inner_mapped_rows <- map(expanded_state_res_list, function(state_cycle_df) {
          expanded_state_values_list <- append(
            as.list(state_cycle_df[ ,value_names_in_df, drop = FALSE]),
            as.list(state_ns$env)[value_names_in_env]
          )

          tibble(
            state = x_group$state[1],
            destination = x_group$destination[1],
            max_st = current_max_st,
            state_cycle = state_cycle_df$state_cycle[1],
            values_list = list(expanded_state_values_list)
          )
      })

      list(
          data_output = bind_rows(inner_mapped_rows),
          na_report = tibble()
      )
  })

  mapped_results_list <- map(processed_groups_and_na_info, "data_output")
  all_results <- bind_rows(mapped_results_list)

  return(arrange(all_results, factor(state, levels = state_names)))
}

# Test that results are identical
cat("Testing result equivalence...\n")
result_original <- evaluate_values_original(test_df, test_ns, value_names, state_names, simplify = FALSE)
result_optimized <- evaluate_values(test_df, test_ns, value_names, state_names, simplify = FALSE)

# Check if results are identical (structure might differ slightly)
cat("Dimensions match:",
    identical(dim(result_original), dim(result_optimized)), "\n")
cat("Column names match:",
    identical(names(result_original), names(result_optimized)), "\n")
cat("States match:",
    identical(result_original$state, result_optimized$state), "\n")
cat("Destinations match:",
    identical(result_original$destination, result_optimized$destination), "\n")

# Benchmark performance
cat("\nBenchmarking performance...\n")
benchmark_results <- microbenchmark(
  Original_R = evaluate_values_original(test_df, test_ns, value_names, state_names, FALSE),
  Optimized_Cpp = evaluate_values(test_df, test_ns, value_names, state_names, FALSE),
  times = 100
)

print(benchmark_results)

# Calculate speedup
mean_times <- summary(benchmark_results)$mean
speedup <- mean_times[1] / mean_times[2]
cat("\nSpeedup factor:", round(speedup, 2), "x\n")
cat("Original mean time:", round(mean_times[1]/1000, 2), "ms\n")
cat("Optimized mean time:", round(mean_times[2]/1000, 2), "ms\n")