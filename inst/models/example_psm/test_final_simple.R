# Simple final test
suppressPackageStartupMessages({
  library(openxlsx)
  library(lazyeval)
  library(purrr)
  library(heRomod2)
  library(herosurv)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(glue)
  library(rlang)
})

# Source all files
r_files <- list.files("/Users/jrdnmdhl/Code/heRomod2/R", pattern = "\\.R$|\\.r$", full.names = TRUE)
for (f in r_files) {
  tryCatch(source(f), error = function(e) {})
}

# Read and run model
model_path <- "/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm"
cat("Loading PSM model...\n")
model <- read_model(model_path)

cat("\nModel settings:\n")
cat("- Type:", model$settings$model_type, "\n")
cat("- Strategies:", paste(model$strategies$name, collapse = ", "), "\n")
cat("- N cycles:", model$settings$n_cycles, "\n")
cat("- Cycle length:", model$settings$cycle_length, model$settings$cycle_length_unit, "\n")

cat("\nRunning model...\n")
results <- run_model(model)

cat("\nModel run complete!\n")
cat("- Segments:", nrow(results$segments), "\n")
cat("- Aggregated:", nrow(results$aggregated), "\n")

# Show trace
cat("\nSample trace (strategy: standard, first 5 cycles):\n")
trace <- results$segments$collapsed_trace[[1]]
print(round(head(trace, 5), 3))

# Show final summaries
cat("\nFinal results:\n")
for (i in 1:nrow(results$aggregated)) {
  strat <- results$aggregated$strategy[i]
  summaries <- results$aggregated$summaries[[i]]
  totals <- summaries %>%
    group_by(summary) %>%
    summarise(total = round(sum(amount), 2), .groups = "drop")
  
  cat("\nStrategy:", strat, "\n")
  print(totals)
}

cat("\nTest completed successfully!\n")