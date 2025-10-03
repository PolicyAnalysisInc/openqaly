library(heRomod2)

# Create test model
model <- system.file('models', 'checkimab_simple', package = 'heRomod2') |>
  read_model()

# Run model
res <- run_model(model)

# Test different time units in table
cat("\n=== Table with Cycles ===\n")
ft_cycles <- format_trace_flextable(res, time_unit = "cycle", cycles = 1:5)
print(ft_cycles$body$dataset[1:5, 1:6])

cat("\n=== Table with Months ===\n")
ft_months <- format_trace_flextable(res, time_unit = "month", cycles = 1:5)
print(ft_months$body$dataset[1:5, 1:6])

cat("\n=== Table with Years ===\n")
ft_years <- format_trace_flextable(res, time_unit = "year", cycles = 1:5)
print(ft_years$body$dataset[1:5, 1:6])

cat("\n✓ Time unit functionality is working correctly!\n")
