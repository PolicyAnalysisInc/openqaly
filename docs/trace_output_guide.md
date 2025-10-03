# Trace Output Functions Guide

This guide describes the comprehensive suite of functions for visualizing, formatting, and exporting Markov trace data from heRomod2 models.

## Overview

The trace output functions provide multiple ways to work with model trace data:

1. **Data Extraction**: Convert trace matrices to data frames
2. **Visualizations**: Create publication-quality plots with ggplot2
3. **Tables**: Generate formatted tables for reports and publications
4. **Export**: Save trace data in multiple file formats

## Core Functions

### 1. Data Extraction: `get_trace()`

Converts trace matrices to tidy data frames in three formats:

```r
# Long format (ggplot2-ready)
trace_long <- get_trace(results, format = "long")
# Columns: strategy, group, cycle, state, probability

# Wide format (Excel-style, traditional Markov trace)
trace_wide <- get_trace(results, format = "wide")
# Columns: strategy, group, cycle, state1, state2, state3, ...

# Matrix format (keep original structure)
trace_matrices <- get_trace(results, format = "matrix")
# Returns: Named list of matrices
```

**Filtering options:**
```r
# Filter by strategy
get_trace(results, strategies = c("standard"))

# Filter by state
get_trace(results, states = c("progression_free", "progressed"))

# Filter by cycle
get_trace(results, cycles = 0:24)

# Use segment-level data (not aggregated)
get_trace(results, collapsed = FALSE)
```

---

## Visualization Functions (ggplot2)

### 2. Stacked Area Chart: `plot_trace()`

The **primary** visualization for Markov traces. Shows state occupancy over time as a stacked area.

```r
# Basic plot
plot_trace(results)

# Compare strategies side-by-side
plot_trace(results, facet_by = "strategy")

# Show as percentages (0-100%)
plot_trace(results, proportional = TRUE)

# Custom colors
custom_colors <- c(
  "progression_free" = "#4CAF50",
  "progressed" = "#FFC107",
  "dead" = "#F44336"
)
plot_trace(results, color_palette = custom_colors)
```

**Best for:** Standard trace presentations, showing how cohort distributes across states over time

---

### 3. Line Chart: `plot_trace_lines()`

Shows individual state trajectories as separate lines.

```r
# Basic line plot
plot_trace_lines(results)

# Faceted comparison
plot_trace_lines(results, facet_by = "strategy")

# As percentages
plot_trace_lines(results, proportional = TRUE)
```

**Best for:** Tracking specific state trajectories, comparing state trends across strategies

---

## Table Formatting Function

### 4. Publication-Quality Table: `format_trace_flextable()`

Creates professional tables with clean black and white styling, hierarchical column headers,
and visual separation between strategy groups using spacer columns. Ideal for publications,
presentations, and Microsoft Office integration.

```r
# Basic table with hierarchical column headers
ft <- format_trace_flextable(results)

# Filter to specific strategies and cycles
ft <- format_trace_flextable(
  results,
  strategies = c("standard", "new_drug"),
  cycles = 0:20,
  decimals = 3
)

# Save to Word
flextable::save_as_docx(ft, path = "trace.docx")

# Save to PowerPoint
flextable::save_as_pptx(ft, path = "trace.pptx")

# Save to HTML
flextable::save_as_html(ft, path = "trace.html")
```

**Package required:** `flextable`, `officer`
**Best for:** Publications, presentations, Microsoft Office documents
**Outputs:** Word (.docx), PowerPoint (.pptx), HTML
**Key features:**
- Hierarchical column headers (strategies as column groups)
- Clean black and white borders only
- Spacer columns for visual separation
- Professional publication-ready styling

---

## Export Functions

### 5. Multi-Format Export: `export_trace()`

One function to export to multiple file formats.

```r
# Export to CSV
export_trace(results, "trace.csv")

# Export to Excel (separate sheets per strategy)
export_trace(results, "trace.xlsx", separate_sheets = TRUE)

# Export to JSON
export_trace(results, "trace.json")

# Export to RDS (R data format)
export_trace(results, "trace.rds")

# Export to HTML
export_trace(results, "trace.html")

# Format auto-detected from extension
export_trace(results, "my_trace.csv")  # Automatically uses CSV format
```

**Supported formats:**
- **CSV**: Universal text format
- **Excel**: `.xlsx` with optional separate sheets per strategy
- **JSON**: Web-friendly structured data
- **RDS**: R native format (preserves exact structure)
- **HTML**: Simple HTML table

---

## Usage Patterns

### Pattern 1: Quick Exploration
```r
# Load and run model
results <- run_model(model)

# Quick visualization
plot_trace(results, facet_by = "strategy")

# Export to CSV for exploration
export_trace(results, "trace.csv")
```

### Pattern 2: Academic Publication
```r
# Create publication-quality plot
p <- plot_trace(results, facet_by = "strategy") +
  labs(title = "State Occupancy Over Time",
       subtitle = "Comparison of Treatment Strategies")
ggsave("figure1.png", p, width = 10, height = 6, dpi = 300)

# Create publication table
ft <- format_trace_flextable(results,
                              cycles = seq(0, 120, 12))  # Annual timepoints
flextable::save_as_docx(ft, "table2.docx")
```

### Pattern 3: HTML Report (R Markdown)
````markdown
# Markov Trace Results

## Visualization
```{r}
plot_trace(results, facet_by = "strategy", proportional = TRUE)
```

## Summary Table
```{r}
ft <- format_trace_flextable(results, cycles = c(0, 12, 24, 36, 48, 60))
print(ft)
```
````

### Pattern 4: PowerPoint Presentation
```r
# Create plots
p1 <- plot_trace(results, strategies = "standard")
p2 <- plot_trace(results, strategies = "new_drug")

# Save plots
ggsave("slide_standard.png", p1, width = 8, height = 5)
ggsave("slide_newdrug.png", p2, width = 8, height = 5)

# Create table for slide
ft <- format_trace_flextable(results,
                             cycles = seq(0, 60, 12))
flextable::save_as_pptx(ft, path = "trace_table.pptx")
```

### Pattern 5: Multi-Format Export
```r
# Export data in multiple formats for different audiences
export_trace(results, "trace_for_excel.xlsx", separate_sheets = TRUE)
export_trace(results, "trace_for_analysis.csv")
export_trace(results, "trace_for_web.json")
export_trace(results, "trace_backup.rds")
```

---

## Tips and Best Practices

### Choosing the Right Format

**For visualizations:**
- **Stacked area** (`plot_trace`): Default choice, best for showing state occupancy
- **Line plot** (`plot_trace_lines`): When tracking individual states is important

**For tables:**
- **flextable** (`format_trace_flextable`): Publication-quality tables for Office documents and reports

**For exports:**
- **CSV**: Universal compatibility, data analysis
- **Excel**: Business users, stakeholder reports
- **JSON**: Web applications, APIs
- **RDS**: R-to-R workflows, preserve exact structure

### Performance

For large traces (many cycles/strategies):
```r
# Filter before visualization
trace_subset <- get_trace(results, cycles = 0:50)
plot_trace(results, cycles = 0:50)  # Or filter directly

# Filter cycles for tables
format_trace_flextable(results, cycles = 0:50)
```

### Customization

All plot functions return ggplot2 objects that can be further customized:
```r
p <- plot_trace(results) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "My Custom Title",
       caption = "Source: heRomod2 model")
print(p)
```

---

## Name Field Selection

As of version 2.0, all trace output functions support flexible name field selection for displaying strategies and states. Models can define multiple name fields:
- `name`: Technical identifier used internally
- `display_name`: Human-friendly name for reports
- `abbreviation`: Short form for space-constrained outputs

### Using Name Fields

```r
# In table functions
format_trace_flextable(results,
  strategy_name_field = "display_name",  # Use display names for strategies
  state_name_field = "abbreviation"      # Use abbreviations for states
)

# In plot functions
plot_trace(results,
  strategy_name_field = "display_name",
  state_name_field = "display_name"
)

# Available options: "name", "display_name", "abbreviation"
```

## Function Reference Summary

| Function | Purpose | Output Type | Key Use Case | Name Fields |
|----------|---------|-------------|--------------|-------------|
| `get_trace()` | Extract data | Data frame / Matrix | Data manipulation | No |
| `plot_trace()` | Stacked area | ggplot2 | Standard visualization | Yes |
| `plot_trace_lines()` | Line chart | ggplot2 | Individual trajectories | Yes |
| `format_trace_flextable()` | Publication table | flextable | Publications, Office docs | Yes |
| `export_trace()` | File export | File | Data sharing | No |

---

## Dependencies

**Core (always available):**
- `ggplot2` - Visualization
- `dplyr`, `tidyr` - Data manipulation
- `knitr` - Basic tables

**Optional (install as needed):**
```r
install.packages("flextable")     # Publication tables and Office integration
install.packages("officer")       # Required for flextable borders
install.packages("openxlsx")      # Excel export
install.packages("jsonlite")      # JSON export
```

---

## See Also

- Run `?get_trace` for detailed function documentation
- See `examples/trace_output_demo.R` for complete working examples
- Check `tests/testthat/test_trace_output.R` for usage patterns
