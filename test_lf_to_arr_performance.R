library(heRomod2)
library(microbenchmark)
library(tidyr)
library(dplyr)

# Create test data similar to what evaluate_values produces
n_cycles <- 100
n_state_cycles <- 5
n_values <- 10

# Create a wide format dataframe
wide_df <- expand.grid(
  cycle = 1:n_cycles,
  state_cycle = 1:n_state_cycles
) %>%
  as_tibble()

# Add value columns
for (i in 1:n_values) {
  wide_df[[paste0("value", i)]] <- runif(nrow(wide_df))
}

# Add state column
wide_df$state <- "State1"

cat("Testing lf_to_arr performance\n")
cat("Data size: ", n_cycles, "cycles x", n_state_cycles, "state_cycles x", n_values, "values\n\n")

# Convert to long format using tidyr
long_df <- wide_df %>%
  pivot_longer(
    cols = starts_with("value"),
    names_to = "variable",
    values_to = "value"
  )

# Test the original R version
cat("Testing original R lf_to_arr...\n")
result_r <- heRomod2:::lf_to_arr(long_df, c('cycle', 'state_cycle', 'variable'), 'value')
cat("  Result dimensions:", paste(dim(result_r), collapse=" x "), "\n")

# Test if C++ version is available
cpp_available <- exists("cpp_lf_to_array_direct", mode = "function")
cat("C++ version available:", cpp_available, "\n\n")

if (cpp_available) {
  # Test C++ version
  cat("Testing C++ lf_to_arr...\n")
  result_cpp <- cpp_lf_to_array_direct(long_df, c('cycle', 'state_cycle', 'variable'), 'value')
  cat("  Result dimensions:", paste(dim(result_cpp), collapse=" x "), "\n")

  # Check if results match
  cat("\nResults match:", all.equal(result_r, result_cpp), "\n")
}

# Benchmark
cat("\nBenchmarking...\n")
bench <- microbenchmark(
  R_version = heRomod2:::lf_to_arr(long_df, c('cycle', 'state_cycle', 'variable'), 'value'),
  times = 100
)

summary_bench <- summary(bench)
cat("R version median time:", sprintf("%.2f ms", summary_bench$median/1000), "\n")

if (cpp_available) {
  bench2 <- microbenchmark(
    R_version = heRomod2:::lf_to_arr(long_df, c('cycle', 'state_cycle', 'variable'), 'value'),
    Cpp_version = cpp_lf_to_array_direct(long_df, c('cycle', 'state_cycle', 'variable'), 'value'),
    times = 100
  )

  summary_bench2 <- summary(bench2)
  cat("C++ version median time:", sprintf("%.2f ms", summary_bench2$median[2]/1000), "\n")
  cat("Speedup:", sprintf("%.2fx", summary_bench2$median[1]/summary_bench2$median[2]), "\n")
}