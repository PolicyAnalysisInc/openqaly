# Test PSM model with manual source
library(heRomod2)
library(herosurv)
library(dplyr)

# Manually source PSM file to ensure it's loaded
source("/Users/jrdnmdhl/Code/heRomod2/R/psm.R")

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

# Check aggregated results
cat("\nAggregated results:\n")
for (i in 1:nrow(results$aggregated)) {
  agg <- results$aggregated[i,]
  summaries <- agg$summaries[[1]]
  cat("\nStrategy:", agg$strategy, "\n")
  print(summaries %>% 
    group_by(summary) %>% 
    summarise(total = sum(amount), .groups = "drop"))
}

cat("\nPSM model test completed successfully!\n")