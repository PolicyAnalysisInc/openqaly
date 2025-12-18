#' Convert use_display_names to field name
#'
#' Internal helper to convert boolean use_display_names parameter to field name.
#'
#' @param use_display_names Logical. If TRUE, use "display_name", else use "name"
#'
#' @return Character string: "display_name" or "name"
#' @keywords internal
field_from_display_names <- function(use_display_names = TRUE) {
  if (use_display_names) "display_name" else "name"
}

#' Map Names Using Specified Field
#'
#' Internal helper to map technical names to display names based on metadata.
#'
#' @param names Character vector of technical names to map
#' @param metadata Data frame with name fields (name, display_name)
#' @param field Which field to use: "name" or "display_name"
#'
#' @return Character vector of mapped names
#' @keywords internal
map_names <- function(names, metadata, field = "name") {
  # If no metadata or empty, return original names
  if (is.null(metadata) || nrow(metadata) == 0 || is.null(field)) {
    return(names)
  }

  # Check if field exists in metadata
  if (!field %in% colnames(metadata)) {
    # Fallback to name if requested field doesn't exist
    warning(sprintf("Field '%s' not found in metadata, using 'name' instead", field))
    field <- "name"
  }

  # Create mapping
  mapped_names <- names
  for (i in seq_along(names)) {
    row_idx <- which(metadata$name == names[i])
    if (length(row_idx) > 0) {
      # Use the specified field, fallback to name if NA
      mapped_value <- metadata[[field]][row_idx[1]]
      if (!is.na(mapped_value) && mapped_value != "") {
        mapped_names[i] <- mapped_value
      }
    }
  }

  mapped_names
}


#' Extract Trace Data from Model Results
#'
#' Converts trace matrices from model results into various data frame formats
#' suitable for visualization and export.
#'
#' @param results A heRomod2 model results object (output from run_model)
#' @param format Output format: "long" (one row per cycle×state×strategy),
#'   "wide" (one row per cycle, states as columns), or "matrix" (keep as matrix)
#' @param collapsed Logical. If TRUE (default), use aggregated traces from
#'   results$aggregated. If FALSE, use segment-level traces from results$segments
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param states Character vector of state names to include (NULL for all)
#' @param cycles Integer vector of cycles to include (NULL for all)
#'
#' @return A data frame (format = "long" or "wide") or list of matrices (format = "matrix")
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Get long format (ggplot2-ready) for overall population
#' trace_long <- get_trace(results, format = "long")
#'
#' # Get wide format (Excel-style)
#' trace_wide <- get_trace(results, format = "wide")
#'
#' # Get specific strategies and groups
#' trace_subset <- get_trace(results, strategies = c("standard"),
#'                           groups = c("overall", "moderate"))
#' }
#'
#' @export
get_trace <- function(results,
                      format = c("long", "wide", "matrix"),
                      collapsed = TRUE,
                      strategies = NULL,
                      groups = "overall",
                      states = NULL,
                      cycles = NULL,
                      time_unit = c("cycle", "day", "week", "month", "year"),
                      use_display_names = TRUE) {

  format <- match.arg(format)
  time_unit <- match.arg(time_unit)

  # Convert use_display_names to field names
  name_field <- field_from_display_names(use_display_names)

  # Select source data based on groups parameter
  source_data <- select_source_data(groups, results)

  # Determine which trace column to use
  trace_column <- if (collapsed) "collapsed_trace" else "expanded_trace"

  # Check if expanded_trace exists when collapsed=FALSE
  if (!collapsed && !(trace_column %in% colnames(source_data))) {
    warning("Expanded traces not available. Using collapsed traces instead.")
    trace_column <- "collapsed_trace"
  }

  # Filter by strategy
  if (!is.null(strategies)) {
    source_data <- source_data[source_data$strategy %in% strategies, ]
  }

  if (nrow(source_data) == 0) {
    stop("No data remaining after filtering")
  }

  # Extract traces based on format
  if (format == "matrix") {
    return(extract_trace_matrix(source_data, states, cycles, time_unit, trace_column,
                                results$metadata, name_field))
  } else if (format == "wide") {
    return(extract_trace_wide(source_data, states, cycles, time_unit, trace_column,
                              results$metadata, name_field))
  } else {
    return(extract_trace_long(source_data, states, cycles, time_unit, trace_column,
                              results$metadata, name_field))
  }
}


#' Extract Trace as Matrix
#' @keywords internal
extract_trace_matrix <- function(source_data, states = NULL, cycles = NULL, time_unit = "cycle", trace_column = "collapsed_trace",
                                 metadata = NULL, name_field = "display_name") {
  traces <- lapply(seq_len(nrow(source_data)), function(i) {
    trace <- source_data[[trace_column]][[i]]

    # Extract only state columns for matrix format (remove time columns)
    if (is.data.frame(trace)) {
      time_cols <- c("cycle", "day", "week", "month", "year")
      state_cols <- setdiff(colnames(trace), time_cols)
      trace <- as.matrix(trace[, state_cols, drop = FALSE])
    }

    # Filter states
    if (!is.null(states)) {
      trace <- trace[, colnames(trace) %in% states, drop = FALSE]
    }

    # Filter cycles
    if (!is.null(cycles)) {
      trace <- trace[rownames(trace) %in% as.character(cycles), , drop = FALSE]
    }

    trace
  })

  names(traces) <- paste0(source_data$strategy, "_", source_data$group)

  # Map state column names if metadata available
  if (!is.null(metadata) && !is.null(metadata$states) && name_field != "name") {
    traces <- lapply(traces, function(trace) {
      old_colnames <- colnames(trace)
      new_colnames <- map_names(old_colnames, metadata$states, name_field)
      colnames(trace) <- new_colnames
      trace
    })
  }

  traces
}


#' Extract Trace as Wide Format
#' @keywords internal
extract_trace_wide <- function(source_data, states = NULL, cycles = NULL, time_unit = "cycle", trace_column = "collapsed_trace",
                               metadata = NULL, name_field = "display_name") {
  dfs <- lapply(seq_len(nrow(source_data)), function(i) {
    trace <- source_data[[trace_column]][[i]]

    # Filter states
    if (!is.null(states)) {
      trace <- trace[, colnames(trace) %in% states, drop = FALSE]
    }

    # Filter cycles
    if (!is.null(cycles)) {
      trace <- trace[rownames(trace) %in% as.character(cycles), , drop = FALSE]
    }

    # Convert to data frame
    df <- as.data.frame(trace)

    # Only add cycle column if it doesn't already exist
    if (!"cycle" %in% colnames(df)) {
      df$cycle <- as.integer(rownames(trace))
    }

    df$strategy <- source_data$strategy[i]
    df$group <- source_data$group[i]

    # Reorder columns - avoid duplicating cycle
    time_cols <- intersect(c("cycle", "day", "week", "month", "year"), colnames(df))
    state_cols <- setdiff(colnames(trace), time_cols)
    df <- df[, c("strategy", "group", time_cols, state_cols)]
    df
  })

  result <- do.call(rbind, dfs)

  # Map names for display
  if (!is.null(metadata)) {
    if (!is.null(metadata$strategies) && name_field != "name") {
      result$strategy <- map_names(result$strategy, metadata$strategies, name_field)
    }
    if (!is.null(metadata$groups) && name_field != "name") {
      result$group <- map_names(result$group, metadata$groups, name_field)
    }
    # Map state column names
    if (!is.null(metadata$states) && name_field != "name") {
      time_cols <- c("strategy", "group", "cycle", "day", "week", "month", "year")
      state_cols <- setdiff(colnames(result), time_cols)
      colnames(result)[colnames(result) %in% state_cols] <-
        map_names(state_cols, metadata$states, name_field)
    }
  }

  result
}


#' Extract Trace as Long Format
#' @keywords internal
extract_trace_long <- function(source_data, states = NULL, cycles = NULL, time_unit = "cycle", trace_column = "collapsed_trace",
                               metadata = NULL, name_field = "display_name") {
  dfs <- lapply(seq_len(nrow(source_data)), function(i) {
    trace <- source_data[[trace_column]][[i]]

    # Check if trace is a matrix (old format) or data.frame with time columns (new format)
    if (is.matrix(trace)) {
      # Old format - convert to data frame with cycle column
      df <- as.data.frame(trace)
      df$cycle <- as.integer(rownames(trace))
      # Create placeholder time columns (will not be accurate)
      df$time <- df$cycle
      time_col <- "cycle"
    } else {
      # New format - trace is already a data frame with time columns
      df <- as.data.frame(trace)
      # Determine which time column to use
      time_col <- switch(time_unit,
        "cycle" = "cycle",
        "day" = "day",
        "week" = "week",
        "month" = "month",
        "year" = "year",
        "cycle"  # default
      )

      # Check if requested time column exists
      if (!time_col %in% colnames(df)) {
        warning(paste("Time unit", time_unit, "not available, using cycle instead"))
        time_col <- "cycle"
      }

      # Rename selected time column to "time" for consistency
      df$time <- df[[time_col]]
    }

    # Filter states (exclude time columns)
    state_cols <- setdiff(colnames(df), c("cycle", "day", "week", "month", "year", "time"))
    if (!is.null(states)) {
      state_cols <- state_cols[state_cols %in% states]
    }
    df_states <- df[, c("time", state_cols), drop = FALSE]

    # Filter cycles/time
    if (!is.null(cycles)) {
      # cycles parameter still refers to cycle numbers for backward compatibility
      if ("cycle" %in% colnames(df)) {
        df_states <- df_states[df$cycle %in% cycles, , drop = FALSE]
      } else {
        # Fallback for old format
        df_states <- df_states[df_states$time %in% cycles, , drop = FALSE]
      }
    }

    # Add strategy and group
    df_states$strategy <- source_data$strategy[i]
    df_states$group <- source_data$group[i]

    # Reshape to long
    df_long <- pivot_longer(
      df_states,
      cols = -c(strategy, group, time),
      names_to = "state",
      values_to = "probability"
    )

    # For backward compatibility, keep both specific time column and cycle
    # Rename "time" to the specific time unit
    time_col_name <- switch(time_unit,
      "cycle" = "cycle",
      "day" = "day",
      "week" = "week",
      "month" = "month",
      "year" = "year",
      "cycle"  # default
    )

    names(df_long)[names(df_long) == "time"] <- time_col_name

    # Ensure cycle column exists for backward compatibility
    if (time_col_name != "cycle" && !"cycle" %in% names(df_long)) {
      if ("cycle" %in% colnames(df)) {
        # Map time values back to cycles
        cycle_mapping <- df[, c(time_col_name, "cycle"), drop = FALSE]
        cycle_mapping <- unique(cycle_mapping)
        df_long$cycle <- cycle_mapping$cycle[match(df_long[[time_col_name]], cycle_mapping[[time_col_name]])]
      } else {
        # Fallback: use time as cycle
        df_long$cycle <- df_long[[time_col_name]]
      }
    }

    df_long
  })

  result <- do.call(rbind, dfs)

  # Map names for display
  if (!is.null(metadata)) {
    if (!is.null(metadata$strategies) && name_field != "name") {
      result$strategy <- map_names(result$strategy, metadata$strategies, name_field)
    }
    if (!is.null(metadata$groups) && name_field != "name") {
      result$group <- map_names(result$group, metadata$groups, name_field)
    }
    # For long format, map state column
    if (!is.null(metadata$states) && name_field != "name") {
      result$state <- map_names(result$state, metadata$states, name_field)
    }
  }

  result
}


#' Plot Trace as Stacked Area Chart
#'
#' Creates a stacked area chart showing state occupancy over time.
#' This is the primary visualization for Markov trace data.
#' When multiple strategies or groups are present, they are automatically
#' faceted into separate panels.
#'
#' @param results A heRomod2 model results object
#' @param facet_by Faceting variable: NULL (auto-detect), "strategy", "group",
#'   or "both". When NULL and multiple strategies/groups exist, automatically
#'   creates facets to separate them.
#' @param proportional Logical. If TRUE, show as percentages (0-100%). If FALSE
#'   (default), show as probabilities (0-1)
#' @param states Character vector of states to include (NULL for all)
#' @param color_palette Named character vector of colors (names = state names),
#'   or NULL to use default colors
#' @param show_legend Logical. Show legend?
#' @param collapsed Logical. Use aggregated traces (TRUE, default) or segment
#'   traces (FALSE)
#' @param strategy_name_field Which strategy name field to use for facet labels:
#'   "name", "display_name", or "abbreviation". Default is "name".
#' @param state_name_field Which state name field to use for legend labels:
#'   "name", "display_name", or "abbreviation". Default is "name".
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Basic stacked area plot (auto-facets by strategy if >1)
#' trace_plot_area(results)
#'
#' # Explicitly facet by strategy
#' trace_plot_area(results, facet_by = "strategy")
#'
#' # Show as percentages
#' trace_plot_area(results, proportional = TRUE)
#' }
#'
#' @export
trace_plot_area <- function(results,
                       facet_by = NULL,
                       proportional = FALSE,
                       states = NULL,
                       color_palette = NULL,
                       show_legend = TRUE,
                       collapsed = TRUE,
                       use_display_names = FALSE,
                       time_unit = "cycle") {

  # Get trace data in long format (names already mapped by get_trace)
  trace_data <- get_trace(results, format = "long", collapsed = collapsed, states = states,
                          time_unit = time_unit,
                          use_display_names = use_display_names)

  # Update color palette keys if provided (plot-specific mapping)
  name_field <- field_from_display_names(use_display_names)
  if (!is.null(color_palette) && !is.null(names(color_palette))) {
    if (!is.null(results$metadata) && !is.null(results$metadata$states) && name_field != "name") {
      old_names <- names(color_palette)
      new_names <- map_names(old_names, results$metadata$states, name_field)
      names(color_palette) <- new_names
    }
  }

  # Convert to percentage if requested
  if (proportional) {
    trace_data <- trace_data %>%
      group_by(strategy, group, cycle) %>%
      mutate(probability = probability * 100) %>%
      ungroup()
  }

  # Determine faceting - auto-facet if multiple series and no faceting specified
  n_strategies <- length(unique(trace_data$strategy))
  n_groups <- length(unique(trace_data$group))

  if (is.null(facet_by)) {
    if (n_strategies > 1 && n_groups > 1) {
      facet_by <- "both"
    } else if (n_strategies > 1) {
      facet_by <- "strategy"
    } else if (n_groups > 1 && !collapsed) {
      facet_by <- "group"
    }
  }

  # Determine time column and label
  time_col_name <- switch(time_unit,
    "cycle" = "cycle",
    "day" = "day",
    "week" = "week",
    "month" = "month",
    "year" = "year",
    "cycle"  # default
  )

  time_label <- switch(time_unit,
    "cycle" = "Cycle",
    "day" = "Days",
    "week" = "Weeks",
    "month" = "Months",
    "year" = "Years",
    "Cycle"  # default
  )

  # Check if time column exists, fallback to cycle if not
  if (!time_col_name %in% colnames(trace_data)) {
    warning(paste("Time unit", time_unit, "not available, using cycle instead"))
    time_col_name <- "cycle"
    time_label <- "Cycle"
  }

  # Create base plot using the appropriate time column
  p <- ggplot(trace_data, aes(x = .data[[time_col_name]], y = probability, fill = state)) +
    geom_area(position = "stack") +
    scale_x_continuous(expand = c(0, 0, 0, 0)) +
    scale_y_continuous(expand = c(0, 0, 0, 0)) +
    theme_bw() +
    labs(
      x = time_label,
      y = if (proportional) "State Occupancy (%)" else "State Occupancy (Probability)",
      fill = "State"
    )

  # Apply color palette if provided
  if (!is.null(color_palette)) {
    p <- p + scale_fill_manual(values = color_palette)
  }

  # Add faceting
  if (!is.null(facet_by)) {
    if (facet_by == "strategy") {
      p <- p + facet_wrap(~ strategy)
    } else if (facet_by == "group") {
      p <- p + facet_wrap(~ group)
    } else if (facet_by == "both") {
      p <- p + facet_wrap(~ strategy + group)
    }
  }

  # Position legend at bottom with horizontal layout (or hide if requested)
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    )
  }

  p
}


#' Plot Trace as Line Chart
#'
#' Creates line plots showing individual state trajectories over time.
#' Alternative to stacked area charts for easier comparison of individual states.
#'
#' @inheritParams trace_plot_area
#' @param strategy_name_field Which strategy name field to use for facet labels:
#'   "name", "display_name", or "abbreviation". Default is "name".
#' @param state_name_field Which state name field to use for legend labels:
#'   "name", "display_name", or "abbreviation". Default is "name".
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Line plot showing each state's trajectory
#' trace_plot_line(results)
#'
#' # Compare strategies
#' trace_plot_line(results, facet_by = "strategy")
#' }
#'
#' @export
trace_plot_line <- function(results,
                              facet_by = NULL,
                              proportional = FALSE,
                              states = NULL,
                              color_palette = NULL,
                              show_legend = TRUE,
                              collapsed = TRUE,
                              use_display_names = FALSE,
                              time_unit = "cycle") {

  # Get trace data in long format (names already mapped by get_trace)
  trace_data <- get_trace(results, format = "long", collapsed = collapsed, states = states,
                          time_unit = time_unit,
                          use_display_names = use_display_names)

  # Update color palette keys if provided (plot-specific mapping)
  name_field <- field_from_display_names(use_display_names)
  if (!is.null(color_palette) && !is.null(names(color_palette))) {
    if (!is.null(results$metadata) && !is.null(results$metadata$states) && name_field != "name") {
      old_names <- names(color_palette)
      new_names <- map_names(old_names, results$metadata$states, name_field)
      names(color_palette) <- new_names
    }
  }

  # Convert to percentage if requested
  if (proportional) {
    trace_data <- trace_data %>%
      group_by(strategy, group, cycle) %>%
      mutate(probability = probability * 100) %>%
      ungroup()
  }

  # Determine faceting - auto-facet if multiple series and no faceting specified
  n_strategies <- length(unique(trace_data$strategy))
  n_groups <- length(unique(trace_data$group))

  if (is.null(facet_by)) {
    if (n_strategies > 1 && n_groups > 1) {
      facet_by <- "both"
    } else if (n_strategies > 1) {
      facet_by <- "strategy"
    } else if (n_groups > 1 && !collapsed) {
      facet_by <- "group"
    }
  }

  # Determine time column and label
  time_col_name <- switch(time_unit,
    "cycle" = "cycle",
    "day" = "day",
    "week" = "week",
    "month" = "month",
    "year" = "year",
    "cycle"  # default
  )

  time_label <- switch(time_unit,
    "cycle" = "Cycle",
    "day" = "Days",
    "week" = "Weeks",
    "month" = "Months",
    "year" = "Years",
    "Cycle"  # default
  )

  # Check if time column exists, fallback to cycle if not
  if (!time_col_name %in% colnames(trace_data)) {
    warning(paste("Time unit", time_unit, "not available, using cycle instead"))
    time_col_name <- "cycle"
    time_label <- "Cycle"
  }

  # Create base plot using the appropriate time column
  p <- ggplot(trace_data, aes(x = .data[[time_col_name]], y = probability, color = state)) +
    geom_line(linewidth = 1) +
    theme_bw() +
    labs(
      x = time_label,
      y = if (proportional) "State Occupancy (%)" else "State Occupancy (Probability)",
      color = "State"
    )

  # Apply color palette if provided
  if (!is.null(color_palette)) {
    p <- p + scale_color_manual(values = color_palette)
  }

  # Add faceting
  if (!is.null(facet_by)) {
    if (facet_by == "strategy") {
      p <- p + facet_wrap(~ strategy)
    } else if (facet_by == "group") {
      p <- p + facet_wrap(~ group)
    } else if (facet_by == "both") {
      p <- p + facet_wrap(~ strategy + group)
    }
  }

  # Position legend at bottom with horizontal layout (or hide if requested)
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  p
}


#' Export Trace Data to File
#'
#' Exports trace data to various file formats including CSV, Excel, HTML, JSON, and RDS.
#'
#' @param results A heRomod2 model results object
#' @param file Output file path (extension determines format if format is NULL)
#' @param format Output format: "csv", "excel", "html", "json", or "rds".
#'   If NULL, inferred from file extension
#' @param collapsed Logical. Use aggregated traces (TRUE, default) or segment traces (FALSE)
#' @param separate_sheets Logical. For Excel format, create separate sheets
#'   for each strategy (TRUE, default) or combine into one sheet (FALSE)
#' @param ... Additional arguments passed to format-specific functions
#'
#' @return Invisibly returns the file path
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Export to CSV
#' export_trace(results, "trace.csv")
#'
#' # Export to Excel with separate sheets per strategy
#' export_trace(results, "trace.xlsx", separate_sheets = TRUE)
#'
#' # Export to JSON
#' export_trace(results, "trace.json")
#' }
#'
#' @export
export_trace <- function(results,
                         file,
                         format = NULL,
                         collapsed = TRUE,
                         separate_sheets = TRUE,
                         ...) {

  # Infer format from file extension if not provided
  if (is.null(format)) {
    ext <- tolower(file_ext(file))
    format <- switch(ext,
      csv = "csv",
      xlsx = "excel",
      xls = "excel",
      html = "html",
      json = "json",
      rds = "rds",
      stop("Cannot infer format from file extension. Please specify format argument.")
    )
  }

  # Export based on format
  if (format == "csv") {
    trace_data <- get_trace(results, format = "wide", collapsed = collapsed)
    write.csv(trace_data, file, row.names = FALSE, ...)
  } else if (format == "excel") {
    export_trace_excel(results, file, collapsed, separate_sheets, ...)
  } else if (format == "html") {
    trace_data <- get_trace(results, format = "wide", collapsed = collapsed)
    # Simple HTML table
    html_content <- kable(trace_data, format = "html", ...)
    writeLines(as.character(html_content), file)
  } else if (format == "json") {
    trace_data <- get_trace(results, format = "long", collapsed = collapsed)
    write_json(trace_data, file, pretty = TRUE, ...)
  } else if (format == "rds") {
    trace_data <- get_trace(results, format = "matrix", collapsed = collapsed)
    saveRDS(trace_data, file, ...)
  } else {
    stop("Unsupported format: ", format)
  }

  message("Trace exported to: ", file)
  invisible(file)
}


#' Export Trace to Excel
#' @keywords internal
export_trace_excel <- function(results, file, collapsed, separate_sheets, ...) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required for Excel export. Please install it.")
  }

  wb <- createWorkbook()

  if (separate_sheets) {
    # Get trace data
    source_data <- if (collapsed) results$aggregated else results$segments
    strategies <- unique(source_data$strategy)

    for (strat in strategies) {
      trace_data <- get_trace(results, format = "wide", collapsed = collapsed, strategies = strat)
      # Remove strategy column if all same
      if (length(unique(trace_data$strategy)) == 1) {
        trace_data$strategy <- NULL
      }

      sheet_name <- substr(strat, 1, 31)  # Excel sheet name limit
      addWorksheet(wb, sheet_name)
      writeData(wb, sheet_name, trace_data)
    }
  } else {
    trace_data <- get_trace(results, format = "wide", collapsed = collapsed)
    addWorksheet(wb, "Trace")
    writeData(wb, "Trace", trace_data)
  }

  saveWorkbook(wb, file, overwrite = TRUE)
}
