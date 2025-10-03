library(heRomod2)

# Create simple model
model <- define_model("markov") |>
  set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "weeks") |>
  add_state("healthy", initial_prob = 1) |>
  add_state("sick", initial_prob = 0) |>
  add_transition("healthy", "healthy", 0.9) |>
  add_transition("healthy", "sick", 0.1) |>
  add_transition("sick", "sick", 1) |>
  add_strategy("standard")

# Run model
res <- run_model(model)

# Check trace structure
trace <- res$aggregated$collapsed_trace[[1]]
cat("Trace class:", class(trace), "\n")
cat("Trace dimensions:", dim(trace), "\n")
cat("Column names:", colnames(trace), "\n")

# Test get_trace with different time units
trace_cycle <- get_trace(res, format = "long", time_unit = "cycle")
cat("\nCycle column exists:", "cycle" %in% colnames(trace_cycle), "\n")

trace_week <- get_trace(res, format = "long", time_unit = "week")
cat("Week column exists:", "week" %in% colnames(trace_week), "\n")

# Test plot function
p <- plot_trace(res, time_unit = "week")
cat("Plot created successfully\n")

# Test table function
if (requireNamespace("flextable", quietly = TRUE)) {
  ft <- format_trace_flextable(res, time_unit = "week", cycles = 1:3)
  cat("Table created successfully\n")
}
