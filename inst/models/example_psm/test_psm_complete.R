# Test PSM model with all dependencies
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(glue)
library(lazyeval)
library(herosurv)
library(Rcpp)
library(rlang)

# Load heRomod2 package to get base functions
library(heRomod2)

# Source additional files in correct order
source_files <- c(
  "heromod2.R",
  "error.R",
  "misc.R",
  "namespace.R",
  "formula.R",
  "time.R",
  "states.R",
  "variables.R",
  "values.R",
  "markov.r",
  "psm.R",
  "model.R"
)

for (f in source_files) {
  source(file.path("/Users/jrdnmdhl/Code/heRomod2/R", f))
}

# Read the model
model_path <- "/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm"
model <- read_model(model_path)

# Check model structure
cat("Model type:", model$settings$model_type, "\n")
cat("Strategies:", paste(model$strategies$name, collapse = ", "), "\n")
cat("States:", paste(model$states$name, collapse = ", "), "\n")
cat("\nTransitions structure:\n")
print(model$transitions)

# Run the model
cat("\nRunning PSM model...\n")
tryCatch({
  results <- run_model(model)
  
  # Check results structure
  cat("\nResults structure:\n")
  cat("- Segments:", nrow(results$segments), "\n")
  cat("- Aggregated:", nrow(results$aggregated), "\n")
  
  # Display segment results
  cat("\nSegment results:\n")
  print(results$segments %>% select(strategy, group, weight))
  
  # Check trace for first segment
  cat("\nTrace for first segment (first 5 cycles):\n")
  trace1 <- results$segments$collapsed_trace[[1]]
  print(head(trace1, 5))
  
  # Check values for first segment
  cat("\nValues for first segment (first 5 cycles):\n")
  values1 <- results$segments$trace_and_values[[1]]$values
  print(head(values1, 5))
  
  # Check summaries
  cat("\nSummaries for each strategy:\n")
  for (i in 1:nrow(results$segments)) {
    seg <- results$segments[i,]
    summaries <- seg$summaries[[1]]
    cat("\nStrategy:", seg$strategy, "\n")
    print(summaries %>% 
      group_by(summary) %>% 
      summarise(total = sum(amount), .groups = "drop"))
  }
  
  cat("\nPSM model test completed successfully!\n")
  
}, error = function(e) {
  cat("\nError occurred:\n")
  print(e)
  cat("\nTraceback:\n")
  traceback()
})