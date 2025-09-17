# Working test for PSM model
library(openxlsx)  # Load this first
library(heRomod2)
library(herosurv)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(glue)
library(rlang)

# Source all R files to ensure everything is loaded
r_files <- list.files("/Users/jrdnmdhl/Code/heRomod2/R", pattern = "\\.R$|\\.r$", full.names = TRUE)
for (f in r_files) {
  tryCatch(source(f), error = function(e) {})
}

# Read the model
cat("Reading PSM model...\n")
model_path <- "/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm"
model <- read_model(model_path)
cat("Model loaded successfully\n")

# Print basic info
cat("\nModel info:\n")
cat("- Type:", model$settings$model_type, "\n")
cat("- Strategies:", paste(model$strategies$name, collapse = ", "), "\n")
cat("- Cycles:", model$settings$n_cycles, "\n")

# Run the model
cat("\nRunning model...\n")
results <- run_model(model)
cat("Model run successfully!\n")

# Basic results check
cat("\nBasic results:\n")
cat("- Number of segments:", nrow(results$segments), "\n")
cat("- Number of aggregated results:", nrow(results$aggregated), "\n")

# Show trace for first few cycles
cat("\nSample trace (first segment, first 5 cycles):\n")
trace <- results$segments$collapsed_trace[[1]]
print(round(head(trace, 5), 3))

# Show summaries for each strategy
cat("\nSummary results by strategy:\n")
for (i in 1:nrow(results$aggregated)) {
  strat <- results$aggregated$strategy[i]
  summaries <- results$aggregated$summaries[[i]]
  
  cat("\nStrategy:", strat, "\n")
  summary_totals <- summaries %>%
    group_by(summary) %>%
    summarise(total = round(sum(amount), 2), .groups = "drop")
  print(summary_totals, n = Inf)
}

cat("\nPSM model test completed successfully!\n")