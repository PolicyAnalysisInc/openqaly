# Trace Output Functions Demo
# Demonstrates various ways to visualize and export Markov trace data

library(heRomod2)

# Load example PSM model
model <- read_model(system.file("models/example_psm", package = "heRomod2"))
results <- run_model(model)

# ============================================================================
# 1. DATA EXTRACTION
# ============================================================================

# Get trace in long format (ggplot2-ready)
trace_long <- get_trace(results, format = "long")
head(trace_long)

# Get trace in wide format (Excel-style)
trace_wide <- get_trace(results, format = "wide")
head(trace_wide)

# Get trace as matrices
trace_matrices <- get_trace(results, format = "matrix")
print(trace_matrices[[1]][1:10, ])

# Filter by strategy
trace_standard <- get_trace(results, format = "long", strategies = "standard")

# Filter by states
trace_alive <- get_trace(results, format = "long", states = c("progression_free", "progressed"))

# Filter by cycles (first 24 months)
trace_24m <- get_trace(results, format = "wide", cycles = 0:24)


# ============================================================================
# 2. VISUALIZATIONS (ggplot2)
# ============================================================================

# Stacked area chart (primary visualization)
plot_trace(results)

# Faceted by strategy for comparison
plot_trace(results, facet_by = "strategy")

# Show as percentages
plot_trace(results, proportional = TRUE)

# Custom colors
custom_colors <- c(
  "progression_free" = "#4CAF50",
  "progressed" = "#FFC107",
  "dead" = "#F44336"
)
plot_trace(results, color_palette = custom_colors)

# Line chart showing individual state trajectories
plot_trace_lines(results)

# Faceted line chart
plot_trace_lines(results, facet_by = "strategy")


# ============================================================================
# 3. EXPORT TO FILES
# ============================================================================

# Export to CSV
export_trace(results, "trace.csv")

# Export to Excel with separate sheets per strategy
export_trace(results, "trace.xlsx", separate_sheets = TRUE)

# Export to JSON
export_trace(results, "trace.json")

# Export to RDS (R data format)
export_trace(results, "trace.rds")


# ============================================================================
# 4. FORMATTED PUBLICATION-QUALITY TABLE
# ============================================================================

# Publication-quality table with flextable (for Word/PowerPoint/HTML)
if (requireNamespace("flextable", quietly = TRUE)) {
  # Basic table with hierarchical column headers
  ft <- format_trace_flextable(results)
  print(ft)

  # Filter to specific strategies and cycles
  ft_filtered <- format_trace_flextable(
    results,
    strategies = c("standard", "new_drug"),
    cycles = 0:20,
    decimals = 3
  )

  # Save to Word
  # flextable::save_as_docx(ft, path = "trace.docx")

  # Save to PowerPoint
  # flextable::save_as_pptx(ft, path = "trace.pptx")

  # Save to HTML
  # flextable::save_as_html(ft, path = "trace.html")
}

# ============================================================================
# 5. ADVANCED USAGE
# ============================================================================

# Compare strategies with faceted stacked area
p <- plot_trace(results, facet_by = "strategy", proportional = TRUE)
p + ggplot2::labs(
  title = "State Occupancy by Strategy",
  subtitle = "Partitioned Survival Model (PSM)",
  caption = "Data: Example PSM Model"
)

# Create multi-panel figure
library(ggplot2)
library(patchwork)

p1 <- plot_trace(results, strategies = "standard") +
  ggtitle("Standard Treatment")
p2 <- plot_trace(results, strategies = "new_drug") +
  ggtitle("New Drug")
p3 <- plot_trace_lines(results) +
  ggtitle("Individual State Trajectories")

# Combine plots
p1 | p2 | p3

# Export multiple formats programmatically
for (fmt in c("csv", "json", "rds")) {
  export_trace(results, paste0("trace_export.", fmt))
}

# Clean up demo files
file.remove(c("trace.csv", "trace.xlsx", "trace.json", "trace.rds",
              "trace_export.csv", "trace_export.json", "trace_export.rds"))

message("Demo completed successfully!")
