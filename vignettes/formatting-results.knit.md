---
title: "Formatting and Presenting Base Case Results"
author: "heRomod2"
date: "2025-10-19"
output:
  rmarkdown::html_vignette:
    toc: true
    toc_depth: 3
vignette: >
  %\VignetteIndexEntry{Formatting and Presenting Base Case Results}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



## Introduction

After running a health economic model, the next critical step is presenting the results in clear, publication-quality formats. heRomod2 provides a comprehensive suite of functions for visualizing and tabulating model results, including:

- **Trace outputs**: State occupancy over time
- **Outcome summaries**: QALYs, life years, and other health outcomes
- **Cost summaries**: Treatment costs, disease costs, and total costs
- **Net Monetary Benefit**: Economic value of interventions

This vignette demonstrates these capabilities using the **Checkimab** model, an oncology cost-effectiveness model comparing immunotherapy strategies across different patient risk groups.

### Loading the Model and Running Base Case

Let's start by loading the Checkimab model and running the base case analysis:


``` r
library(heRomod2)
library(ggplot2)
library(dplyr)

# Load the Checkimab model
model_path <- system.file("models/checkimab", package = "heRomod2")
model <- read_model(model_path)

# Run the base case
results <- run_model(model)

# View available strategies
unique(results$aggregated$strategy)
#> [1] "chemo"  "target" "check"
```

The Checkimab model compares three strategies:
- **chemo** (Chemotherapy - standard of care)
- **target** (Targeted therapy)
- **check** (Checkimab - immunotherapy)

Results are available both at the **population-aggregated** level and for individual risk **groups**.

## Trace Visualizations

Markov trace plots show how the patient cohort moves through different health states over time. These are essential for understanding model dynamics.

### Basic Stacked Area Plot

The most common trace visualization is a stacked area chart showing state occupancy:


``` r
# Basic stacked area plot
trace_plot_area(results)
```

![](formatting-results_files/figure-html/trace-area-basic-1.png)<!-- -->

By default, this shows:
- All strategies in separate facets
- Probability scale (0-1)
- All health states with default colors

### Showing as Percentages

For easier interpretation, you can display occupancy as percentages:


``` r
trace_plot_area(results, proportional = TRUE)
```

![](formatting-results_files/figure-html/trace-area-percent-1.png)<!-- -->

### Customizing Colors

Define custom colors for specific states:


``` r
# Define color palette (using state technical names)
state_colors <- c(
  "relapse_free_on_tx" = "#2E7D32",
  "relapse_free_off_tx" = "#81C784",
  "relapse" = "#FFA726",
  "dead" = "#424242"
)

trace_plot_area(
  results,
  color_palette = state_colors,
  proportional = TRUE
)
```

![](formatting-results_files/figure-html/trace-area-colors-1.png)<!-- -->

### Controlling Faceting

You can explicitly control how strategies are displayed:


``` r
# Explicit faceting by strategy
trace_plot_area(
  results,
  facet_by = "strategy",
  proportional = TRUE
)
```

![](formatting-results_files/figure-html/trace-area-facet-1.png)<!-- -->

``` r

# To show a single strategy, filter the results first using get_trace
trace_data_chemo <- get_trace(results, format = "long", strategies = "chemo")
# Then create custom plot as needed
```

### Line Plots for Individual States

Line plots make it easier to compare individual state trajectories:


``` r
trace_plot_line(results, proportional = TRUE)
```

![](formatting-results_files/figure-html/trace-line-1.png)<!-- -->

Line plots are particularly useful when:
- Comparing specific states across strategies
- States have similar occupancies (hard to see in stacked areas)
- You want to emphasize trends rather than composition

### Using Different Time Units

If your model defines time conversions, you can plot in different units:


``` r
# Plot by years instead of cycles
trace_plot_area(
  results,
  time_unit = "year",
  proportional = TRUE
)
```

### Customizing Display Names

Control how strategies and states are labeled using name fields:


``` r
# Use abbreviations for compact display
trace_plot_area(
  results,
  strategy_name_field = "abbreviation",
  state_name_field = "abbreviation",
  proportional = TRUE
)
```

![](formatting-results_files/figure-html/trace-names-1.png)<!-- -->

``` r

# Use display names (default)
trace_plot_area(
  results,
  strategy_name_field = "display_name",
  state_name_field = "display_name"
)
```

![](formatting-results_files/figure-html/trace-names-2.png)<!-- -->

The three name field options are:
- **"name"**: Technical identifier (e.g., "relapse_free_on_tx")
- **"display_name"**: User-friendly label (e.g., "Relapse-Free (On Treatment)")
- **"abbreviation"**: Short form (e.g., "RF-On")

## Trace Tables

For publications and reports, you'll often need tabular presentations of trace data.

### Publication-Quality Tables

The `trace_table()` function creates professional tables with hierarchical headers:


``` r
library(flextable)

# Create trace table
ft <- trace_table(
  results,
  cycles = 0:10,  # Show first 10 cycles only
  decimals = 3
)

ft
```


```{=html}
<div class="tabwid"><style>.cl-fe1756b6{}.cl-fe14c112{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fe14c11c{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fe15ad70{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe15ad71{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe15ad72{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe15b90a{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b90b{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b90c{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b914{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b915{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b916{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b917{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b91e{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b91f{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b920{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b921{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b928{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b929{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b92a{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b92b{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b932{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b933{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b934{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b93c{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b93d{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b93e{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b93f{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b946{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b947{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b948{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b949{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b94a{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b950{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b951{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b952{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b953{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b95a{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b95b{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b95c{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b95d{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b964{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b965{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b966{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b96e{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b96f{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b970{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b971{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b978{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b979{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b97a{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b982{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b983{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b984{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b985{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b98c{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b98d{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b98e{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe15b98f{width:0.568in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-fe1756b6'><thead><tr style="overflow-wrap:break-word;"><th  rowspan="2"class="cl-fe15b90a"><p class="cl-fe15ad70"><span class="cl-fe14c112">Cycle</span></p></th><th class="cl-fe15b90b"><p class="cl-fe15ad70"><span class="cl-fe14c11c"></span></p></th><th  colspan="4"class="cl-fe15b90c"><p class="cl-fe15ad70"><span class="cl-fe14c112">Chemotherapy</span></p></th><th class="cl-fe15b90b"><p class="cl-fe15ad70"><span class="cl-fe14c11c"></span></p></th><th  colspan="4"class="cl-fe15b90c"><p class="cl-fe15ad70"><span class="cl-fe14c112">Targeted Therapy</span></p></th><th class="cl-fe15b90b"><p class="cl-fe15ad70"><span class="cl-fe14c11c"></span></p></th><th  colspan="4"class="cl-fe15b90c"><p class="cl-fe15ad70"><span class="cl-fe14c112">Checkimab</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-fe15b91e"><p class="cl-fe15ad70"><span class="cl-fe14c11c"></span></p></th><th class="cl-fe15b91f"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe15b920"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe15b921"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse</span></p></th><th class="cl-fe15b928"><p class="cl-fe15ad70"><span class="cl-fe14c112">Dead</span></p></th><th class="cl-fe15b91e"><p class="cl-fe15ad70"><span class="cl-fe14c11c"></span></p></th><th class="cl-fe15b91f"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe15b920"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe15b921"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse</span></p></th><th class="cl-fe15b928"><p class="cl-fe15ad70"><span class="cl-fe14c112">Dead</span></p></th><th class="cl-fe15b91e"><p class="cl-fe15ad70"><span class="cl-fe14c11c"></span></p></th><th class="cl-fe15b91f"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe15b920"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe15b921"><p class="cl-fe15ad70"><span class="cl-fe14c112">Relapse</span></p></th><th class="cl-fe15b928"><p class="cl-fe15ad70"><span class="cl-fe14c112">Dead</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-fe15b929"><p class="cl-fe15ad71"><span class="cl-fe14c11c">1</span></p></td><td class="cl-fe15b92a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b92b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">1.000</span></p></td><td class="cl-fe15b932"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b933"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b934"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b92a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b92b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">1.000</span></p></td><td class="cl-fe15b932"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b933"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b934"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b92a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b92b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">1.000</span></p></td><td class="cl-fe15b932"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b933"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b934"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b93c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">2</span></p></td><td class="cl-fe15b93d"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b93e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b93f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b946"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b947"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.100</span></p></td><td class="cl-fe15b93d"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b93e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b93f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b946"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b947"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.100</span></p></td><td class="cl-fe15b93d"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b93e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b93f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b946"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.300</span></p></td><td class="cl-fe15b947"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.100</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b948"><p class="cl-fe15ad71"><span class="cl-fe14c11c">3</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.090</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.378</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.387</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.145</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.090</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.378</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.387</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.145</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.090</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.378</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.387</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.145</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b953"><p class="cl-fe15ad71"><span class="cl-fe14c11c">4</span></p></td><td class="cl-fe15b95a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b95b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.027</span></p></td><td class="cl-fe15b95c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.390</span></p></td><td class="cl-fe15b95d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.410</span></p></td><td class="cl-fe15b964"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.173</span></p></td><td class="cl-fe15b95a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b95b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.027</span></p></td><td class="cl-fe15b95c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.390</span></p></td><td class="cl-fe15b95d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.410</span></p></td><td class="cl-fe15b964"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.173</span></p></td><td class="cl-fe15b95a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b95b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.027</span></p></td><td class="cl-fe15b95c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.390</span></p></td><td class="cl-fe15b95d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.410</span></p></td><td class="cl-fe15b964"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.173</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b948"><p class="cl-fe15ad71"><span class="cl-fe14c11c">5</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.005</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.380</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.411</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.204</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.005</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.380</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.411</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.204</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.005</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.380</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.411</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.204</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b948"><p class="cl-fe15ad71"><span class="cl-fe14c11c">6</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.001</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.366</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.407</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.227</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.001</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.366</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.407</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.227</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b94a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.001</span></p></td><td class="cl-fe15b950"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.366</span></p></td><td class="cl-fe15b951"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.407</span></p></td><td class="cl-fe15b952"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.227</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b953"><p class="cl-fe15ad71"><span class="cl-fe14c11c">7</span></p></td><td class="cl-fe15b95a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b95b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b95c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.351</span></p></td><td class="cl-fe15b95d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.402</span></p></td><td class="cl-fe15b964"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.247</span></p></td><td class="cl-fe15b95a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b95b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b95c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.351</span></p></td><td class="cl-fe15b95d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.402</span></p></td><td class="cl-fe15b964"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.247</span></p></td><td class="cl-fe15b95a"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b95b"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b95c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.351</span></p></td><td class="cl-fe15b95d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.402</span></p></td><td class="cl-fe15b964"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.247</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b965"><p class="cl-fe15ad71"><span class="cl-fe14c11c">8</span></p></td><td class="cl-fe15b966"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b96e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b96f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.337</span></p></td><td class="cl-fe15b970"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.396</span></p></td><td class="cl-fe15b971"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.267</span></p></td><td class="cl-fe15b966"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b96e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b96f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.337</span></p></td><td class="cl-fe15b970"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.396</span></p></td><td class="cl-fe15b971"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.267</span></p></td><td class="cl-fe15b966"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b96e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b96f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.337</span></p></td><td class="cl-fe15b970"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.396</span></p></td><td class="cl-fe15b971"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.267</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b978"><p class="cl-fe15ad71"><span class="cl-fe14c11c">9</span></p></td><td class="cl-fe15b979"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b97a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b982"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.324</span></p></td><td class="cl-fe15b983"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.390</span></p></td><td class="cl-fe15b984"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.286</span></p></td><td class="cl-fe15b979"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b97a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b982"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.324</span></p></td><td class="cl-fe15b983"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.390</span></p></td><td class="cl-fe15b984"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.286</span></p></td><td class="cl-fe15b979"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b97a"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b982"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.324</span></p></td><td class="cl-fe15b983"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.390</span></p></td><td class="cl-fe15b984"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.286</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe15b985"><p class="cl-fe15ad71"><span class="cl-fe14c11c">10</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b98c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b98d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.311</span></p></td><td class="cl-fe15b98e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.384</span></p></td><td class="cl-fe15b98f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.305</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b98c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b98d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.311</span></p></td><td class="cl-fe15b98e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.384</span></p></td><td class="cl-fe15b98f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.305</span></p></td><td class="cl-fe15b949"><p class="cl-fe15ad72"><span class="cl-fe14c11c"></span></p></td><td class="cl-fe15b98c"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.000</span></p></td><td class="cl-fe15b98d"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.311</span></p></td><td class="cl-fe15b98e"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.384</span></p></td><td class="cl-fe15b98f"><p class="cl-fe15ad71"><span class="cl-fe14c11c">0.305</span></p></td></tr></tbody></table></div>
```


Key features of trace tables:
- **Hierarchical headers**: Strategy names span their state columns
- **Visual separation**: Spacer columns between strategies
- **Clean borders**: Black and white styling suitable for publications
- **Flexible precision**: Control decimal places

### Controlling Display


``` r
# Specific strategies and states
trace_table(
  results,
  strategies = c("chemo", "check"),
  states = c("relapse_free_on_tx", "relapse_free_off_tx"),
  cycles = 0:5,
  decimals = 4
)
```


```{=html}
<div class="tabwid"><style>.cl-fe23d1f2{}.cl-fe21e752{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fe21e753{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fe22a7be{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe22a7bf{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe22a7c0{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe22b042{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b043{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b044{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b04c{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b04d{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b04e{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b04f{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b050{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b051{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b056{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b057{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b058{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b059{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b05a{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b060{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b061{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b062{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b063{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b064{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b06a{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b06b{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b06c{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b06d{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe22b06e{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-fe23d1f2'><thead><tr style="overflow-wrap:break-word;"><th  rowspan="2"class="cl-fe22b042"><p class="cl-fe22a7be"><span class="cl-fe21e752">Cycle</span></p></th><th class="cl-fe22b043"><p class="cl-fe22a7be"><span class="cl-fe21e753"></span></p></th><th  colspan="2"class="cl-fe22b044"><p class="cl-fe22a7be"><span class="cl-fe21e752">Chemotherapy</span></p></th><th class="cl-fe22b043"><p class="cl-fe22a7be"><span class="cl-fe21e753"></span></p></th><th  colspan="2"class="cl-fe22b044"><p class="cl-fe22a7be"><span class="cl-fe21e752">Checkimab</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-fe22b04e"><p class="cl-fe22a7be"><span class="cl-fe21e753"></span></p></th><th class="cl-fe22b04f"><p class="cl-fe22a7be"><span class="cl-fe21e752">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe22b050"><p class="cl-fe22a7be"><span class="cl-fe21e752">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe22b04e"><p class="cl-fe22a7be"><span class="cl-fe21e753"></span></p></th><th class="cl-fe22b04f"><p class="cl-fe22a7be"><span class="cl-fe21e752">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe22b050"><p class="cl-fe22a7be"><span class="cl-fe21e752">Relapse-Free (Off-Tx)</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-fe22b051"><p class="cl-fe22a7bf"><span class="cl-fe21e753">1</span></p></td><td class="cl-fe22b056"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b057"><p class="cl-fe22a7bf"><span class="cl-fe21e753">1.0000</span></p></td><td class="cl-fe22b058"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0000</span></p></td><td class="cl-fe22b056"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b057"><p class="cl-fe22a7bf"><span class="cl-fe21e753">1.0000</span></p></td><td class="cl-fe22b058"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0000</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe22b059"><p class="cl-fe22a7bf"><span class="cl-fe21e753">2</span></p></td><td class="cl-fe22b05a"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b060"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3000</span></p></td><td class="cl-fe22b061"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3000</span></p></td><td class="cl-fe22b05a"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b060"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3000</span></p></td><td class="cl-fe22b061"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3000</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe22b062"><p class="cl-fe22a7bf"><span class="cl-fe21e753">3</span></p></td><td class="cl-fe22b063"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b064"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0900</span></p></td><td class="cl-fe22b06a"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3780</span></p></td><td class="cl-fe22b063"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b064"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0900</span></p></td><td class="cl-fe22b06a"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3780</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe22b062"><p class="cl-fe22a7bf"><span class="cl-fe21e753">4</span></p></td><td class="cl-fe22b063"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b064"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0270</span></p></td><td class="cl-fe22b06a"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3899</span></p></td><td class="cl-fe22b063"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b064"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0270</span></p></td><td class="cl-fe22b06a"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3899</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe22b06b"><p class="cl-fe22a7bf"><span class="cl-fe21e753">5</span></p></td><td class="cl-fe22b06c"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b06d"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0054</span></p></td><td class="cl-fe22b06e"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3797</span></p></td><td class="cl-fe22b06c"><p class="cl-fe22a7c0"><span class="cl-fe21e753"></span></p></td><td class="cl-fe22b06d"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.0054</span></p></td><td class="cl-fe22b06e"><p class="cl-fe22a7bf"><span class="cl-fe21e753">0.3797</span></p></td></tr></tbody></table></div>
```


### Exporting to Word or PowerPoint


``` r
library(officer)

# Save to Word
ft <- trace_table(results, cycles = 0:20)
save_as_docx(ft, path = "trace_table.docx")

# Save to PowerPoint
save_as_pptx(ft, path = "trace_table.pptx")
```

## Outcome Visualizations

Outcome plots show the accumulation of health benefits (QALYs, life years, etc.) and help understand which strategies provide better health outcomes.

### Bar Charts for Outcome Summaries

Bar charts show total outcomes broken down by component:


``` r
# Show total QALYs by component (requires model with defined summaries)
outcomes_plot_bar(results, outcome = "total_qalys")
```

**Note**: The Checkimab model uses individual values rather than defined summaries. The outcome plot functions work best with models that define summaries (aggregations of multiple values). For this model, we can examine individual value components using the values output functions or define summaries programmatically.

When summaries are defined, this shows:
- Each strategy's total outcomes
- Breakdown by value component
- A "Total" bar showing the sum

### Comparing Strategies: Differences

Often we want to see **differences** between strategies rather than absolute values. heRomod2 supports two perspectives:

#### Comparator Perspective

Show how interventions compare to a reference (typically standard of care):


``` r
# Show differences relative to chemo (standard of care)
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  comparator = "chemo"
)
```

This creates N-1 comparisons: "target vs. chemo" and "check vs. chemo"

#### Referent/Intervention Perspective

Show how a new intervention compares to alternatives:


``` r
# Show how check (immunotherapy) compares to alternatives
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  referent = "check"
)
```

This creates N-1 comparisons: "check vs. chemo" and "check vs. target"

### Line Charts Over Time

See how outcomes accumulate over the model time horizon:


``` r
# Cumulative QALYs over time
outcomes_plot_line(
  results,
  outcome = "total_qalys",
  cumulative = TRUE
)
```

#### Per-Cycle Outcomes

Switch to per-cycle view to see the rate of outcome accrual:


``` r
# Per-cycle QALY accrual
outcomes_plot_line(
  results,
  outcome = "total_qalys",
  cumulative = FALSE
)
```

#### Differences Over Time

Combine time-series with comparisons:


``` r
# Cumulative QALY difference vs. chemo over time
outcomes_plot_line(
  results,
  outcome = "total_qalys",
  comparator = "chemo",
  cumulative = TRUE
)
```

### Using Different Time Units


``` r
# Plot outcomes by year
outcomes_plot_line(
  results,
  outcome = "total_qalys",
  time_unit = "year",
  cumulative = TRUE
)
```

## Outcome Tables

Tabular outcome presentations are essential for manuscripts and reports.

### Summary Tables

Show total outcomes broken down by component:


``` r
# Total QALYs by component
outcomes_table(
  results,
  summary_name = "total_qalys",
  decimals = 2
)
```

### Cycle-by-Cycle Tables

Show outcome accumulation over time:


``` r
# Cumulative QALYs by cycle
format_outcomes_cycle_table(
  results,
  values = c("qaly_pf", "qaly_prog"),  # Specific outcome components
  cycles = 0:10,
  cumulative = TRUE,
  decimals = 3
)
```

### Per-Cycle Tables

Show the rate of outcome accrual:


``` r
format_outcomes_cycle_table(
  results,
  values = c("qaly_pf", "qaly_prog"),
  cycles = 0:10,
  cumulative = FALSE,
  decimals = 4
)
```

## Net Monetary Benefit

Net Monetary Benefit (NMB) combines costs and outcomes into a single economic metric:

**NMB = (Outcomes × WTP) - Costs**

Where WTP is the willingness-to-pay threshold per unit outcome (e.g., $100,000 per QALY).

### NMB Bar Charts

Show NMB broken down by component:


``` r
# NMB comparing to chemo at WTP = $100,000/QALY
nmb_plot_bar(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo",
  wtp = 100000
)
```

The plot shows:
- **Outcome components** (multiplied by WTP): positive contribution to NMB
- **Cost components** (negated): negative contribution to NMB
- **Total NMB**: net economic benefit

**Interpretation**: Positive NMB means the intervention is cost-effective at the given WTP threshold.

### Auto-Extracting WTP

If WTP is defined in the model metadata, you can omit it:


``` r
# WTP extracted from outcome summary metadata
nmb_plot_bar(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo"
  # wtp parameter omitted - extracted automatically
)
```

### NMB Over Time

See how economic value accumulates:


``` r
# Cumulative NMB over time
nmb_plot_line(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo",
  wtp = 100000,
  cumulative = TRUE
)
```

### Per-Cycle NMB

See the rate of economic value generation:


``` r
# Per-cycle NMB
nmb_plot_line(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo",
  wtp = 100000,
  cumulative = FALSE
)
```

### NMB Tables

#### Summary Table


``` r
# NMB summary by component
nmb_table(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo",
  wtp = 100000,
  decimals = 0
)
```

#### Cycle-by-Cycle NMB Table


``` r
# NMB accumulation over time
format_nmb_cycle_table(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo",
  wtp = 100000,
  cycles = 0:10,
  cumulative = TRUE,
  decimals = 0
)
```

## Working with Groups

Many models stratify results by patient subgroups (e.g., risk levels, age groups). heRomod2 provides flexible group handling.

### Understanding Group Parameters

The `group` parameter appears in most output functions and accepts three types of values:

1. **`group = "aggregated"`** (default): Population-weighted average across all groups
2. **`group = "specific_group"`**: Results for a single group (e.g., "high_risk")
3. **`group = NULL`**: All groups plus aggregated

### Aggregated Results (Default)

By default, all output functions show aggregated (population-weighted) results:


``` r
# These are equivalent
outcomes_plot_bar(results, outcome = "total_qalys")
outcomes_plot_bar(results, outcome = "total_qalys", group = "aggregated")
```

Aggregated results represent the **average** patient in the population, weighted by group proportions defined in the model.

### Single Group Results

Analyze a specific subgroup:


``` r
# Results for high-risk patients only
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  group = "high_risk"
)

# Compare costs across strategies for moderate-risk patients
outcomes_plot_bar(
  results,
  outcome = "total_cost",
  group = "moderate_risk",
  comparator = "chemo"
)
```

### Multi-Group Comparisons

Show all groups simultaneously:


``` r
# All groups + aggregated
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  group = NULL
)
```

When `group = NULL`:
- Plots create additional facets for each group
- Tables add group columns with hierarchical headers
- Aggregated results appear alongside individual groups

### Group-Specific Tables


``` r
# Summary table showing all groups
outcomes_table(
  results,
  summary_name = "total_qalys",
  group = NULL,
  decimals = 2
)
```

This creates a table with hierarchical headers:
- **Level 1**: Strategy names
- **Level 2**: Group names (including "Aggregated")
- **Rows**: Outcome components

### Interpreting Aggregated vs. Disaggregated

**When to use aggregated results:**
- Reporting population-level cost-effectiveness
- Making overall policy recommendations
- Comparing to other published analyses

**When to use group-specific results:**
- Identifying subgroups that benefit most
- Supporting stratified treatment recommendations
- Understanding heterogeneity in treatment effects

**When to use multi-group displays:**
- Showing robustness across subgroups
- Highlighting differential effects
- Supporting equity considerations

### Group-Specific Traces

Trace visualizations also support group filtering:


``` r
# Trace for high-risk group only
trace_plot_area(
  results,
  collapsed = FALSE,  # Use group-specific traces
  groups = "high_risk",
  proportional = TRUE
)
```

**Note**: The `collapsed` parameter determines trace source:
- `collapsed = TRUE` (default): Uses aggregated traces
- `collapsed = FALSE`: Uses group-specific traces (allows `groups` parameter)

## Customization & Styling

### Name Field Parameters

Almost all output functions support three name field parameters:


``` r
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  strategy_name_field = "display_name",  # "name", "display_name", "abbreviation"
  group_name_field = "display_name",
  value_name_field = "display_name"
)
```

For trace functions, there's also `state_name_field`:


``` r
trace_plot_area(
  results,
  strategy_name_field = "abbreviation",
  state_name_field = "display_name"
)
```

And for outcome/NMB functions, there's `summary_name_field`:


``` r
outcomes_plot_line(
  results,
  outcome = "total_qalys",
  summary_name_field = "display_name"
)
```

**Use cases:**
- **"name"**: Technical documentation, debugging
- **"display_name"**: Publications, presentations (most common)
- **"abbreviation"**: Compact displays, figures with limited space

### Color Palettes

Customize plot colors:


``` r
# Custom colors for states (using display names)
my_colors <- c(
  "Relapse-Free (On Treatment)" = "#4CAF50",
  "Relapse-Free (Off Treatment)" = "#81C784",
  "Relapse" = "#FF9800",
  "Dead" = "#757575"
)

trace_plot_area(
  results,
  color_palette = my_colors,
  state_name_field = "display_name"
)
```

![](formatting-results_files/figure-html/color-palettes-1.png)<!-- -->

**Important**: Color palette names must match the state/value names **after** applying the name field transformation.

### Decimal Precision

Control decimal places in tables:


``` r
# High precision for probabilities
trace_table(results, cycles = 0:5, decimals = 5)
```


```{=html}
<div class="tabwid"><style>.cl-fe6d0c0a{}.cl-fe6ade80{font-family:'Helvetica';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fe6ade8a{font-family:'Helvetica';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-fe6bae3c{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe6bae3d{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe6bae3e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-fe6bb97c{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb97d{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb986{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb987{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb988{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb990{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb991{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb992{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb99a{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb99b{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb99c{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb99d{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9a4{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9a5{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9a6{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9a7{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9a8{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9ae{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9af{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9b8{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9b9{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9ba{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9bb{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9c2{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9c3{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9c4{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9c5{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9cc{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9cd{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9ce{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9cf{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9d0{width:0.185in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9d6{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9d7{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9e0{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9e1{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9e2{width:0.593in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9ea{width:1.73in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9eb{width:1.739in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9ec{width:0.771in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-fe6bb9f4{width:0.737in;background-color:rgba(255, 255, 255, 1.00);vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table data-quarto-disable-processing='true' class='cl-fe6d0c0a'><thead><tr style="overflow-wrap:break-word;"><th  rowspan="2"class="cl-fe6bb97c"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Cycle</span></p></th><th class="cl-fe6bb97d"><p class="cl-fe6bae3c"><span class="cl-fe6ade8a"></span></p></th><th  colspan="4"class="cl-fe6bb986"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Chemotherapy</span></p></th><th class="cl-fe6bb97d"><p class="cl-fe6bae3c"><span class="cl-fe6ade8a"></span></p></th><th  colspan="4"class="cl-fe6bb986"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Targeted Therapy</span></p></th><th class="cl-fe6bb97d"><p class="cl-fe6bae3c"><span class="cl-fe6ade8a"></span></p></th><th  colspan="4"class="cl-fe6bb986"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Checkimab</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-fe6bb992"><p class="cl-fe6bae3c"><span class="cl-fe6ade8a"></span></p></th><th class="cl-fe6bb99a"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe6bb99b"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe6bb99c"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse</span></p></th><th class="cl-fe6bb99d"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Dead</span></p></th><th class="cl-fe6bb992"><p class="cl-fe6bae3c"><span class="cl-fe6ade8a"></span></p></th><th class="cl-fe6bb99a"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe6bb99b"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe6bb99c"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse</span></p></th><th class="cl-fe6bb99d"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Dead</span></p></th><th class="cl-fe6bb992"><p class="cl-fe6bae3c"><span class="cl-fe6ade8a"></span></p></th><th class="cl-fe6bb99a"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse-Free (On-Tx)</span></p></th><th class="cl-fe6bb99b"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse-Free (Off-Tx)</span></p></th><th class="cl-fe6bb99c"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Relapse</span></p></th><th class="cl-fe6bb99d"><p class="cl-fe6bae3c"><span class="cl-fe6ade80">Dead</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-fe6bb9a4"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">1</span></p></td><td class="cl-fe6bb9a5"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9a6"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">1.00000</span></p></td><td class="cl-fe6bb9a7"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9a8"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9ae"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9a5"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9a6"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">1.00000</span></p></td><td class="cl-fe6bb9a7"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9a8"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9ae"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9a5"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9a6"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">1.00000</span></p></td><td class="cl-fe6bb9a7"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9a8"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td><td class="cl-fe6bb9ae"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00000</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe6bb9af"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">2</span></p></td><td class="cl-fe6bb9b8"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9b9"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9ba"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9bb"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9c2"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.10000</span></p></td><td class="cl-fe6bb9b8"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9b9"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9ba"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9bb"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9c2"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.10000</span></p></td><td class="cl-fe6bb9b8"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9b9"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9ba"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9bb"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.30000</span></p></td><td class="cl-fe6bb9c2"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.10000</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe6bb9c3"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">3</span></p></td><td class="cl-fe6bb9c4"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9c5"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.09000</span></p></td><td class="cl-fe6bb9cc"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.37800</span></p></td><td class="cl-fe6bb9cd"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.38700</span></p></td><td class="cl-fe6bb9ce"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.14500</span></p></td><td class="cl-fe6bb9c4"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9c5"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.09000</span></p></td><td class="cl-fe6bb9cc"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.37800</span></p></td><td class="cl-fe6bb9cd"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.38700</span></p></td><td class="cl-fe6bb9ce"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.14500</span></p></td><td class="cl-fe6bb9c4"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9c5"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.09000</span></p></td><td class="cl-fe6bb9cc"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.37800</span></p></td><td class="cl-fe6bb9cd"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.38700</span></p></td><td class="cl-fe6bb9ce"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.14500</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe6bb9cf"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">4</span></p></td><td class="cl-fe6bb9d0"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9d6"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.02700</span></p></td><td class="cl-fe6bb9d7"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.38988</span></p></td><td class="cl-fe6bb9e0"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.40986</span></p></td><td class="cl-fe6bb9e1"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.17326</span></p></td><td class="cl-fe6bb9d0"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9d6"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.02700</span></p></td><td class="cl-fe6bb9d7"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.38988</span></p></td><td class="cl-fe6bb9e0"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.40986</span></p></td><td class="cl-fe6bb9e1"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.17326</span></p></td><td class="cl-fe6bb9d0"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9d6"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.02700</span></p></td><td class="cl-fe6bb9d7"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.38988</span></p></td><td class="cl-fe6bb9e0"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.40986</span></p></td><td class="cl-fe6bb9e1"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.17326</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-fe6bb9e2"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">5</span></p></td><td class="cl-fe6bb9d0"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9ea"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00540</span></p></td><td class="cl-fe6bb9eb"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.37968</span></p></td><td class="cl-fe6bb9ec"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.41056</span></p></td><td class="cl-fe6bb9f4"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.20435</span></p></td><td class="cl-fe6bb9d0"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9ea"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00540</span></p></td><td class="cl-fe6bb9eb"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.37968</span></p></td><td class="cl-fe6bb9ec"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.41056</span></p></td><td class="cl-fe6bb9f4"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.20435</span></p></td><td class="cl-fe6bb9d0"><p class="cl-fe6bae3e"><span class="cl-fe6ade8a"></span></p></td><td class="cl-fe6bb9ea"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.00540</span></p></td><td class="cl-fe6bb9eb"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.37968</span></p></td><td class="cl-fe6bb9ec"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.41056</span></p></td><td class="cl-fe6bb9f4"><p class="cl-fe6bae3d"><span class="cl-fe6ade8a">0.20435</span></p></td></tr></tbody></table></div>
```



``` r
# Low precision for large cost values (requires model with summaries)
outcomes_table(
  results,
  summary_name = "total_cost",
  decimals = 0
)
```

### Time Units

Most functions support multiple time units:


``` r
# Available time units: "cycle", "day", "week", "month", "year"

outcomes_plot_line(
  results,
  outcome = "total_qalys",
  time_unit = "year"
)

trace_table(
  results,
  time_unit = "month",
  cycles = 0:60  # First 60 cycles (converted to months in display)
)
```

**Note**: Time unit conversion requires the model to define time mappings.

### Discounting

Toggle between discounted and undiscounted values:


``` r
# Undiscounted values (default)
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  discounted = FALSE
)

# Discounted values
outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  discounted = TRUE
)
```

### Legend Control


``` r
# Hide legend
trace_plot_area(
  results,
  show_legend = FALSE
)
```

![](formatting-results_files/figure-html/legend-1.png)<!-- -->

``` r

# Show legend (default)
trace_plot_line(
  results,
  show_legend = TRUE
)
```

![](formatting-results_files/figure-html/legend-2.png)<!-- -->

### Further ggplot2 Customization

All plot functions return ggplot2 objects, so you can customize further:


``` r
p <- outcomes_plot_line(
  results,
  outcome = "total_qalys",
  cumulative = TRUE
)

# Add custom theme and labels
p +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold")
  ) +
  labs(
    title = "QALY Accumulation Over Time",
    subtitle = "Immunotherapy vs. Standard of Care",
    caption = "Source: Checkimab Model"
  )
```

## Complete Workflow Example

Here's a complete workflow for creating a comprehensive results report:


``` r
# 1. Load and run model
model <- read_model(system.file("models/checkimab", package = "heRomod2"))
results <- run_model(model)

# 2. Trace visualization - understand model dynamics
p_trace <- trace_plot_area(
  results,
  proportional = TRUE,
  strategy_name_field = "display_name",
  state_name_field = "display_name"
) +
  labs(title = "State Occupancy Over Time")

print(p_trace)

# 3. Outcome comparison - identify which strategy provides more QALYs
p_qalys <- outcomes_plot_bar(
  results,
  outcome = "total_qalys",
  comparator = "chemo",
  strategy_name_field = "display_name"
) +
  labs(title = "Incremental QALYs vs. Standard of Care")

print(p_qalys)

# 4. Cost comparison - understand cost implications
p_costs <- outcomes_plot_bar(
  results,
  outcome = "total_cost",
  comparator = "chemo",
  strategy_name_field = "display_name"
) +
  labs(title = "Incremental Costs vs. Standard of Care")

print(p_costs)

# 5. Economic evaluation - assess cost-effectiveness
p_nmb <- nmb_plot_bar(
  results,
  outcome_summary = "total_qalys",
  cost_summary = "total_cost",
  comparator = "chemo",
  wtp = 100000,
  strategy_name_field = "display_name"
) +
  labs(title = "Net Monetary Benefit at $100,000/QALY")

print(p_nmb)

# 6. Create summary tables for manuscript
tab_qalys <- outcomes_table(
  results,
  summary_name = "total_qalys",
  strategy_name_field = "display_name",
  decimals = 2
)

tab_costs <- outcomes_table(
  results,
  summary_name = "total_cost",
  strategy_name_field = "display_name",
  decimals = 0
)

# Display tables
print(tab_qalys)
print(tab_costs)
```

### Exporting Results


``` r
# Save plots
ggsave("trace.png", p_trace, width = 10, height = 6, dpi = 300)
ggsave("qalys.png", p_qalys, width = 8, height = 6, dpi = 300)
ggsave("costs.png", p_costs, width = 8, height = 6, dpi = 300)
ggsave("nmb.png", p_nmb, width = 8, height = 6, dpi = 300)

# Save tables to Word
library(officer)
save_as_docx(tab_qalys, path = "qalys_table.docx")
save_as_docx(tab_costs, path = "costs_table.docx")

# Or combine multiple tables in one document
doc <- read_docx()
doc <- doc %>%
  body_add_par("Table 1. Total QALYs by Strategy", style = "heading 2") %>%
  body_add_flextable(tab_qalys) %>%
  body_add_par("") %>%
  body_add_par("Table 2. Total Costs by Strategy", style = "heading 2") %>%
  body_add_flextable(tab_costs)
print(doc, target = "results_tables.docx")
```

## Best Practices

### For Publications

1. **Use display names**: `strategy_name_field = "display_name"` for reader-friendly labels
2. **Control precision**: Match journal requirements (typically 2-3 decimals)
3. **Clear comparisons**: Always specify comparator or referent for difference plots
4. **Include uncertainty**: Combine base case results with PSA results (see PSA vignette)
5. **Export high-resolution**: Use `dpi = 300` or higher for publication-quality figures

### For Reports

1. **Multi-group displays**: Use `group = NULL` to show robustness
2. **Time-series plots**: Show accumulation patterns with `outcomes_plot_line()`
3. **Component breakdowns**: Use bar charts to show where outcomes/costs come from
4. **Consistent styling**: Use the same color palettes and name fields throughout

### For Presentations

1. **Use abbreviations**: `strategy_name_field = "abbreviation"` for compact slides
2. **Proportional scales**: `proportional = TRUE` for easier interpretation
3. **Focus on key comparisons**: Filter to 2-3 strategies for clarity
4. **Large fonts**: Customize ggplot2 themes with larger text

### For Decision-Makers

1. **NMB plots**: Focus on economic value, not separate costs and outcomes
2. **Group-specific results**: Show differential effects across populations
3. **Clear thresholds**: Visualize decision criteria (e.g., WTP thresholds)
4. **Cumulative plots**: Show long-term value accumulation

## Summary

heRomod2 provides a comprehensive toolkit for presenting model results:

**Traces**: Understand model dynamics through state occupancy
- `trace_plot_area()` and `trace_plot_line()` for visualizations
- `trace_table()` for publication-quality tables

**Outcomes**: Evaluate health benefits
- `outcomes_plot_bar()` and `outcomes_plot_line()` for comparisons
- `outcomes_table()` for summary tables
- Support for both absolute values and differences

**Net Monetary Benefit**: Assess economic value
- `nmb_plot_bar()` and `nmb_plot_line()` for visualizations
- `nmb_table()` for summaries
- Automatic WTP extraction from metadata

**Groups**: Analyze heterogeneity
- `group = "aggregated"` for population-level results
- `group = "specific"` for subgroup analyses
- `group = NULL` for comprehensive comparisons

**Customization**: Control presentation
- Name fields for display preferences
- Color palettes for brand consistency
- Time units for appropriate scales
- Export formats for different audiences

All functions follow a consistent API, making it easy to create comprehensive reports that tell the complete story of your cost-effectiveness analysis.
