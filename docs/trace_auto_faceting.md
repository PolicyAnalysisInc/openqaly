# Auto-Faceting in Trace Plots

## Overview

All trace plotting functions (`plot_trace()`, `plot_trace_lines()`, and `plot_trace_heatmap()`) now automatically facet by strategy/group when multiple series are present. This prevents overlapping traces in the same plot area, which would create visual confusion.

## Behavior

### Default Behavior (`facet_by = NULL`)

When `facet_by = NULL` (the default), the functions automatically determine appropriate faceting:

1. **Multiple strategies AND multiple groups**: Facet by `strategy + group`
2. **Multiple strategies only**: Facet by `strategy`
3. **Multiple groups (non-collapsed data)**: Facet by `group`
4. **Single strategy/group**: No faceting

### Example with 2 Strategies

```r
results <- run_model(model)  # Model has 2 strategies

# Auto-facets by strategy (2 panels)
plot_trace(results)

# Equivalent to:
plot_trace(results, facet_by = "strategy")
```

### Example with Multiple Groups

```r
# With segment-level data (multiple groups)
plot_trace(results, collapsed = FALSE)  # Auto-facets by group

# With aggregated data (single "_aggregated" group)
plot_trace(results, collapsed = TRUE)   # Facets by strategy only
```

## Manual Control

You can always override the auto-detection:

```r
# Force specific faceting
plot_trace(results, facet_by = "strategy")
plot_trace(results, facet_by = "group")
plot_trace(results, facet_by = "both")

# Disable faceting (NOT RECOMMENDED with multiple series)
# This will overlay multiple traces, causing visual confusion
plot_trace(results, facet_by = NULL)  # Only works if auto-detection finds 1 series
```

## Rationale

**Why auto-facet?**

Stacked area charts, line plots, and heatmaps cannot meaningfully display multiple strategy/group combinations in the same plot area because:

1. **Stacked areas**: Each series must sum to 1.0 (or 100%), so overlaying two strategies would sum to 2.0
2. **Lines**: Multiple overlapping series become visually cluttered and hard to distinguish
3. **Heatmaps**: Each tile represents a specific state×cycle, so multiple groups would require overplotting

**Solution**: Automatically create separate facet panels for each strategy/group combination.

## API Changes

### Old Behavior (Before Fix)
- `facet_by = NULL` → No faceting, even with multiple strategies (caused overlapping plots)
- User had to manually specify `facet_by = "strategy"` every time

### New Behavior (After Fix)
- `facet_by = NULL` → Intelligent auto-detection and faceting
- Prevents visual errors by default
- Still allows manual override if needed

## Examples

```r
library(heRomod2)

model <- read_model(system.file("models/example_psm", package = "heRomod2"))
results <- run_model(model)

# Default: Auto-facets by strategy (2 panels for 2 strategies)
plot_trace(results)

# All plot types support auto-faceting
plot_trace_lines(results)      # Auto-facets line plot
plot_trace_heatmap(results)    # Auto-facets heatmap

# Works with proportional option
plot_trace(results, proportional = TRUE)  # Still auto-facets

# Works with state filtering
plot_trace(results, states = c("progression_free", "dead"))

# Manual override examples
plot_trace(results, facet_by = "strategy")  # Explicit (same as auto)
plot_trace(results, facet_by = NULL)        # Auto-detection still applies

# Single strategy: No auto-faceting
plot_trace(results, strategies = "standard")  # Single panel
```

## Technical Implementation

The auto-detection logic:

```r
# Count unique strategies and groups
n_strategies <- length(unique(trace_data$strategy))
n_groups <- length(unique(trace_data$group))

# Determine faceting
if (is.null(facet_by)) {
  if (n_strategies > 1 && n_groups > 1) {
    facet_by <- "both"          # facet_wrap(~ strategy + group)
  } else if (n_strategies > 1) {
    facet_by <- "strategy"      # facet_wrap(~ strategy)
  } else if (n_groups > 1 && !collapsed) {
    facet_by <- "group"         # facet_wrap(~ group)
  }
  # else: facet_by remains NULL (no faceting needed)
}
```

## Backward Compatibility

This change is **mostly backward compatible**:

- Existing code with `facet_by = "strategy"` works identically
- Existing code with `facet_by = NULL` now works *correctly* (was broken before)
- No breaking changes to function signatures or return types

The only "breaking" change is that plots with multiple strategies now correctly facet by default instead of incorrectly overlaying traces.
