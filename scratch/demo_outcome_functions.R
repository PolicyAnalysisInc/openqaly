# Demonstration of Outcome and Cost Reporting Functions
# This script shows how to use the new outcome/cost reporting functions

library(heRomod2)

# Load example PSM model
model_path <- system.file("models/example_psm", package = "heRomod2")
if (model_path == "") {
  model_path <- "inst/models/example_psm"
}

model <- read_model(model_path)
results <- run_model(model)

# ============================================================================
# 1. DATA EXTRACTION
# ============================================================================

cat("=== Data Extraction ===\n\n")

# Get outcome values (aggregated)
cat("Outcome values (aggregated, long format):\n")
outcomes_long <- get_values(results, value_type = "outcome", group = "aggregated")
print(head(outcomes_long, 10))

cat("\n\nOutcome summaries:\n")
outcome_summaries <- get_summaries(results, value_type = "outcome", group = "aggregated")
print(outcome_summaries)

# Get cost values
cat("\n\nCost values (aggregated, wide format):\n")
costs_wide <- get_values(results, format = "wide", value_type = "cost", group = "aggregated")
print(head(costs_wide, 5))

cat("\n\nCost summaries:\n")
cost_summaries <- get_summaries(results, value_type = "cost", group = "aggregated")
print(cost_summaries)

# ============================================================================
# 2. OUTCOME PLOTS
# ============================================================================

cat("\n\n=== Outcome Plots ===\n\n")

# Stacked bar chart for outcomes
cat("Creating stacked bar chart for outcomes...\n")
p1 <- plot_outcomes_stacked_bar(results, summary_name = "total_qalys")
print(p1)

# Waterfall chart for outcomes
cat("\nCreating waterfall chart for outcomes...\n")
p2 <- plot_outcomes_waterfall(results, strategy = "standard", summary_name = "total_qalys")
print(p2)

# Line chart for cumulative outcomes
cat("\nCreating line chart for cumulative outcomes...\n")
p3 <- plot_outcomes_line(results, cumulative = TRUE)
print(p3)

# Line chart for per-cycle outcomes
cat("\nCreating line chart for per-cycle outcomes...\n")
p4 <- plot_outcomes_line(results, cumulative = FALSE)
print(p4)

# ============================================================================
# 3. COST PLOTS
# ============================================================================

cat("\n\n=== Cost Plots ===\n\n")

# Stacked bar chart for costs
cat("Creating stacked bar chart for costs...\n")
p5 <- plot_costs_stacked_bar(results, summary_name = "total_cost")
print(p5)

# Waterfall chart for costs
cat("\nCreating waterfall chart for costs...\n")
p6 <- plot_costs_waterfall(results, strategy = "standard", summary_name = "total_cost")
print(p6)

# Line chart for cumulative costs
cat("\nCreating line chart for cumulative costs...\n")
p7 <- plot_costs_line(results, cumulative = TRUE)
print(p7)

# ============================================================================
# 4. OUTCOME TABLES
# ============================================================================

cat("\n\n=== Outcome Tables ===\n\n")

# Cycle-by-cycle outcome table (cumulative, first 10 cycles only)
cat("Creating cycle-by-cycle outcome table (cumulative)...\n")
ft1 <- format_outcomes_cycle_table(results, cycles = 1:10, cumulative = TRUE)
print(ft1)

# Summary breakdown table
cat("\nCreating outcome summary breakdown table...\n")
ft2 <- format_outcomes_summary_table(results, summary_name = "total_qalys")
print(ft2)

# ============================================================================
# 5. COST TABLES
# ============================================================================

cat("\n\n=== Cost Tables ===\n\n")

# Cycle-by-cycle cost table (cumulative, first 10 cycles only)
cat("Creating cycle-by-cycle cost table (cumulative)...\n")
ft3 <- format_costs_cycle_table(results, cycles = 1:10, cumulative = TRUE)
print(ft3)

# Summary breakdown table
cat("\nCreating cost summary breakdown table...\n")
ft4 <- format_costs_summary_table(results, summary_name = "total_cost")
print(ft4)

# ============================================================================
# 6. EXPORT EXAMPLES
# ============================================================================

cat("\n\n=== Export Examples ===\n\n")

# Export outcomes to CSV
cat("Exporting outcomes to CSV...\n")
export_outcomes(results, file = tempfile(fileext = ".csv"))

# Export costs to Excel (if openxlsx is available)
if (requireNamespace("openxlsx", quietly = TRUE)) {
  cat("Exporting costs to Excel...\n")
  export_costs(results, file = tempfile(fileext = ".xlsx"), separate_sheets = TRUE)
}

cat("\n\nDemonstration complete!\n")
cat("\nKey features demonstrated:\n")
cat("- Data extraction with get_values() and get_summaries()\n")
cat("- Filtering by value type (cost vs outcome)\n")
cat("- Stacked bar charts for component breakdown\n")
cat("- Waterfall charts for buildup visualization\n")
cat("- Line charts for time series (cumulative and per-cycle)\n")
cat("- Formatted tables for cycle-by-cycle and summary views\n")
cat("- Export to various file formats\n")
