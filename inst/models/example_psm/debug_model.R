# Debug PSM model loading
suppressPackageStartupMessages({
  library(openxlsx)
  library(lazyeval)
  library(heRomod2)
  library(dplyr)
})

# Source all files
r_files <- list.files("/Users/jrdnmdhl/Code/heRomod2/R", pattern = "\\.R$|\\.r$", full.names = TRUE)
for (f in r_files) {
  tryCatch(source(f), error = function(e) {})
}

# Read raw Excel file
cat("Reading Excel file...\n")
model_path <- "/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm"
wb_data <- read_workbook(file.path(model_path, "model.xlsx"))

# Check settings
cat("\nSettings sheet:\n")
print(wb_data$settings)

# Check if settings conversion works
cat("\nConverted settings:\n")
settings <- convert_settings_from_df(wb_data$settings)
print(str(settings))

# Check n_cycles calculation
cat("\nChecking n_cycles calculation...\n")
cat("- days_per_year:", settings$days_per_year, "\n")
cat("- cycle_length:", settings$cycle_length, "\n") 
cat("- cycle_length_unit:", settings$cycle_length_unit, "\n")
cat("- timeframe:", settings$timeframe, "\n")
cat("- timeframe_unit:", settings$timeframe_unit, "\n")

# Try to calculate n_cycles
tryCatch({
  settings$cycle_length_days <- get_cycle_length_days(settings)
  cat("- cycle_length_days:", settings$cycle_length_days, "\n")
  
  settings$n_cycles <- get_n_cycles(settings)
  cat("- n_cycles:", settings$n_cycles, "\n")
}, error = function(e) {
  cat("Error calculating n_cycles:", e$message, "\n")
})