# Test script for write_model_json function

library(heRomod2)

# Test with the checkimab_simple model
cat("Testing Excel to JSON conversion...\n")

# Read an Excel model
excel_model_path <- system.file("models/checkimab_simple", package = "heRomod2")
if (excel_model_path == "") {
  excel_model_path <- "inst/models/checkimab_simple"
}

cat("Reading Excel model from:", excel_model_path, "\n")
model <- read_model(excel_model_path)

# Convert to JSON
cat("Converting to JSON...\n")
json_output <- write_model_json(model)

# Write to file for inspection
output_file <- tempfile(fileext = ".json")
writeLines(json_output, output_file)
cat("JSON output written to:", output_file, "\n")

# Test round-trip conversion
cat("\nTesting round-trip conversion...\n")
model_from_json <- read_model_json(json_output)

# Compare key elements
cat("Original model class:", class(model), "\n")
cat("Round-trip model class:", class(model_from_json), "\n")

# Check if main components are preserved
components <- c("settings", "strategies", "groups", "states", "transitions", "values", "summaries")
for (comp in components) {
  orig_rows <- if (!is.null(model[[comp]]) && is.data.frame(model[[comp]])) nrow(model[[comp]]) else 0
  json_rows <- if (!is.null(model_from_json[[comp]]) && is.data.frame(model_from_json[[comp]])) nrow(model_from_json[[comp]]) else 0
  cat(glue("{format(comp, width=15, justify='left')}: original = {format(orig_rows, width=3)} rows, json = {format(json_rows, width=3)} rows\n"))
}

cat("\nConversion test completed!\n")