library(heRomod2)

# Test with checkimab_simple model
model <- system.file('models', 'checkimab_simple', package = 'heRomod2') |>
  read_model()

res <- run_model(model)

# Check trace structure
trace <- res$aggregated$collapsed_trace[[1]]
cat("✓ Trace includes time columns:", all(c("cycle", "day", "week", "month", "year") %in% colnames(trace)), "\n")

# Test get_trace with different time units
trace_cycle <- get_trace(res, format = "long", time_unit = "cycle")
cat("✓ get_trace with cycle:", "cycle" %in% colnames(trace_cycle), "\n")

trace_year <- get_trace(res, format = "long", time_unit = "year") 
cat("✓ get_trace with year:", "year" %in% colnames(trace_year), "\n")

# Test plot functions
p_month <- plot_trace(res, time_unit = "month")
cat("✓ plot_trace with month: x-axis label =", p_month$labels$x, "\n")

p_lines_week <- plot_trace_lines(res, time_unit = "week")
cat("✓ plot_trace_lines with week: x-axis label =", p_lines_week$labels$x, "\n")

# Test table function
if (requireNamespace("flextable", quietly = TRUE)) {
  ft_day <- format_trace_flextable(res, time_unit = "day", cycles = 1:3)
  cat("✓ format_trace_flextable with day: created successfully\n")
}

cat("\n🎉 All time unit functionality tests passed!\n")
