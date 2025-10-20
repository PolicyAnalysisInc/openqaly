#' Format Outcomes as Cycle-by-Cycle Table
#'
#' Creates a flextable showing outcome values for each cycle.
#' Supports three display modes with hierarchical headers.
#'
#' @param results A heRomod2 model results object
#' @param group Group selection: "aggregated", specific group, or NULL (all groups + aggregated)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param values Character vector of value names to include (NULL for all outcomes)
#' @param cycles Integer vector or range of cycles to display (NULL for all)
#' @param cumulative Logical. Show cumulative totals (TRUE, default) or per-cycle amounts (FALSE)
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values?
#' @param value_name_field Which value name field to use
#' @param strategy_name_field Which strategy name field to use
#' @param group_name_field Which group name field to use
#' @param time_unit Time unit: "cycle", "day", "week", "month", "year"
#'
#' @return A flextable object
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Aggregated cumulative outcomes
#' ft <- format_outcomes_cycle_table(results)
#'
#' # All groups with faceting
#' ft <- format_outcomes_cycle_table(results, group = NULL)
#'
#' # Save to Word
#' save_as_docx(ft, path = "outcomes.docx")
#' }
#'
#' @keywords internal
format_outcomes_cycle_table <- function(results,
                                       group = "aggregated",
                                       strategies = NULL,
                                       values = NULL,
                                       cycles = NULL,
                                       cumulative = TRUE,
                                       decimals = 2,
                                       discounted = FALSE,
                                       value_name_field = "display_name",
                                       strategy_name_field = "display_name",
                                       group_name_field = "display_name",
                                       time_unit = "cycle") {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' required. Install with: install.packages('flextable')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' required. Install with: install.packages('officer')")
  }

  # Get values data in wide format (names already mapped by get_values)
  values_data <- get_values(
    results,
    format = "wide",
    group = group,
    strategies = strategies,
    values = values,
    value_type = "outcome",
    discounted = discounted,
    cycles = cycles,
    time_unit = time_unit,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = value_name_field
  )

  # Get unique strategies, groups, and values (already have display names)
  strategies_display <- unique(values_data$strategy)
  groups_display <- unique(values_data$group)

  # Get value column names (already have display names)
  time_cols <- c("cycle", "day", "week", "month", "year")
  value_cols <- setdiff(colnames(values_data), c("strategy", "group", time_cols))
  values_display <- value_cols

  n_values <- length(value_cols)
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Calculate cumulative if requested
  if (cumulative) {
    time_col <- intersect(time_cols, colnames(values_data))[1]
    values_data <- values_data %>%
      group_by(strategy, group) %>%
      arrange(!!sym(time_col)) %>%
      mutate(across(all_of(value_cols), cumsum)) %>%
      ungroup()
  }

  # Determine time column and label
  time_col <- intersect(c("cycle", "day", "week", "month", "year"),
                       colnames(values_data))[1]
  time_label <- switch(time_col,
                      "cycle" = "Cycle",
                      "day" = "Day",
                      "week" = "Week",
                      "month" = "Month",
                      "year" = "Year",
                      "Time")

  # Pivot wider to organize by strategy and group
  if (n_groups > 1 || is.null(group)) {
    # Mode 3: Show groups
    pivot_data <- values_data %>%
      pivot_wider(
        names_from = c(strategy, group),
        values_from = all_of(value_cols),
        names_sep = "_",
        names_glue = if (n_values > 1) "{.value}_{strategy}_{group}" else "{strategy}_{group}"
      )
  } else {
    # Mode 1 or 2: Single group
    pivot_data <- values_data %>%
      select(-group) %>%
      pivot_wider(
        names_from = strategy,
        values_from = all_of(value_cols),
        names_sep = "_",
        names_glue = if (n_values > 1) "{.value}_{strategy}" else "{strategy}"
      )
  }

  # Round time column intelligently
  time_values <- pivot_data[[time_col]]
  rounded_values <- round(time_values, 1)
  if (all(abs(rounded_values - round(rounded_values)) < 0.01)) {
    pivot_data[[time_col]] <- as.integer(round(rounded_values))
  } else {
    pivot_data[[time_col]] <- rounded_values
  }

  # Rename time column for display
  names(pivot_data)[names(pivot_data) == time_col] <- time_label

  # Round value columns before creating flextable
  value_col_names <- setdiff(colnames(pivot_data), time_label)
  for (col in value_col_names) {
    pivot_data[[col]] <- round(pivot_data[[col]], decimals)
  }

  # Add spacer columns between strategy groups
  result_cols <- pivot_data[, time_label, drop = FALSE]

  for (i in seq_along(strategies_display)) {
    # Add spacer before each strategy
    spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
    names(spacer_col) <- paste0("spacer_", i)
    result_cols <- cbind(result_cols, spacer_col)

    # Get columns for this strategy
    strat <- strategies_display[i]
    if (n_groups > 1 || is.null(group)) {
      # Mode 3: columns are like "strategy_group" or "value_strategy_group"
      strat_cols <- pivot_data[, grepl(paste0("^", strat, "_"), names(pivot_data)), drop = FALSE]
    } else {
      # Mode 1/2: columns are like "strategy" (1 value) or "value_strategy" (>1 value)
      if (n_values == 1) {
        # Direct match: column name IS the strategy name
        strat_cols <- pivot_data[, names(pivot_data) == strat, drop = FALSE]
      } else {
        # Pattern match: "value_strategy"
        strat_cols <- pivot_data[, grepl(paste0("_", strat, "$"), names(pivot_data)), drop = FALSE]
      }
    }
    result_cols <- cbind(result_cols, strat_cols)
  }

  # Create flextable
  ft <- flextable(result_cols)

  # Format value columns
  value_col_names <- setdiff(colnames(result_cols),
                             c(time_label, grep("^spacer_", colnames(result_cols), value = TRUE)))
  for (col in value_col_names) {
    ft <- colformat_double(ft, j = col, digits = decimals)
  }

  # Build hierarchical headers
  all_cols <- colnames(result_cols)
  spacer_col_indices <- which(grepl("^spacer_", all_cols))
  non_spacer_col_indices <- setdiff(seq_along(all_cols), spacer_col_indices)

  # Determine header structure based on mode
  if (n_groups > 1 || is.null(group)) {
    # Mode 3: Strategy > Group > Values hierarchy
    # Row 1: Time | Strategy names
    # Row 2: Time | Group names
    # Row 3: Time | Value names

    header_row1_values <- c(time_label)
    header_row1_widths <- c(1)
    header_row2_values <- c(time_label)
    header_row2_widths <- c(1)

    for (i in seq_along(strategies_display)) {
      # Spacer in row 1
      header_row1_values <- c(header_row1_values, "")
      header_row1_widths <- c(header_row1_widths, 1)
      header_row2_values <- c(header_row2_values, "")
      header_row2_widths <- c(header_row2_widths, 1)

      # Strategy name spanning all groups × values
      header_row1_values <- c(header_row1_values, strategies_display[i])
      header_row1_widths <- c(header_row1_widths, n_groups * n_values)

      # Group names each spanning values
      for (j in seq_along(groups_display)) {
        header_row2_values <- c(header_row2_values, groups_display[j])
        header_row2_widths <- c(header_row2_widths, n_values)
      }
    }

    ft <- ft %>%
      add_header_row(values = header_row2_values, colwidths = header_row2_widths, top = TRUE) %>%
      add_header_row(values = header_row1_values, colwidths = header_row1_widths, top = TRUE)

    # Merge time column across all 3 header rows
    ft <- merge_at(ft, i = 1:3, j = 1, part = "header")

    # Set value names in bottom row
    current_col <- 2
    for (i in seq_along(strategies_display)) {
      current_col <- current_col + 1  # Skip spacer
      for (j in seq_along(groups_display)) {
        for (k in seq_along(value_cols)) {
          ft <- compose(ft, i = 3, j = current_col,
                                  value = as_paragraph(values_display[k]),
                                  part = "header")
          current_col <- current_col + 1
        }
      }
    }

  } else {
    # Mode 1 or 2: Strategy > Values hierarchy (2 rows)
    header_row1_values <- c(time_label)
    header_row1_widths <- c(1)

    for (i in seq_along(strategies_display)) {
      header_row1_values <- c(header_row1_values, "")  # Spacer
      header_row1_widths <- c(header_row1_widths, 1)
      header_row1_values <- c(header_row1_values, strategies_display[i])
      header_row1_widths <- c(header_row1_widths, n_values)
    }

    ft <- ft %>%
      add_header_row(values = header_row1_values, colwidths = header_row1_widths, top = TRUE)

    # Merge time column
    ft <- merge_at(ft, i = 1:2, j = 1, part = "header")

    # Set value names in row 2
    current_col <- 2
    for (i in seq_along(strategies_display)) {
      ft <- compose(ft, i = 2, j = current_col,
                              value = as_paragraph(""),
                              part = "header")
      current_col <- current_col + 1
      for (k in seq_along(value_cols)) {
        ft <- compose(ft, i = 2, j = current_col,
                                value = as_paragraph(values_display[k]),
                                part = "header")
        current_col <- current_col + 1
      }
    }
  }

  # Apply styling
  ft <- ft %>%
    bg(bg = "white", part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header")

  # Apply borders to non-spacer columns only
  black_border <- fp_border(color = "black", width = 1)
  no_border <- fp_border(width = 0)

  ft <- border_remove(ft)

  for (col_idx in non_spacer_col_indices) {
    ft <- hline_top(ft, j = col_idx, border = black_border, part = "header")
    ft <- hline_bottom(ft, j = col_idx, border = black_border, part = "header")
    ft <- hline_bottom(ft, j = col_idx, border = black_border, part = "body")
  }

  # Style spacer columns
  if (length(spacer_col_indices) > 0) {
    ft <- width(ft, j = spacer_col_indices, width = 0.01)
    ft <- void(ft, j = spacer_col_indices, part = "all")
    for (spacer_idx in spacer_col_indices) {
      ft <- border(ft, i = NULL, j = spacer_idx, border = no_border, part = "all")
    }
  }

  # Autofit
  ft <- autofit(ft, add_w = 0, add_h = 0)

  ft
}


#' Prepare Outcomes Summary Table Data
#'
#' Internal helper function that prepares outcomes summary data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A heRomod2 model results object
#' @param summary_name Name of summary to display (e.g., "total_qalys")
#' @param group Group selection: "aggregated", specific group, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param show_total Logical. Show TOTAL row?
#' @param decimals Number of decimal places
#' @param discounted Logical. Use discounted values?
#' @param value_name_field Which value name field to use
#' @param strategy_name_field Which strategy name field to use
#' @param group_name_field Which group name field to use
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_outcomes_table_data <- function(results,
                                        summary_name,
                                        group = "aggregated",
                                        strategies = NULL,
                                        show_total = TRUE,
                                        decimals = 2,
                                        discounted = FALSE,
                                        value_name_field = "display_name",
                                        strategy_name_field = "display_name",
                                        group_name_field = "display_name") {

  # Get summary data (names already mapped by get_summaries)
  summary_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = summary_name,
    value_type = "outcome",
    discounted = discounted,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = value_name_field
  )

  # Get unique strategies, groups, values (already have display names)
  strategies_display <- unique(summary_data$strategy)
  groups_display <- unique(summary_data$group)
  values_display <- unique(summary_data$value)
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Value column is already mapped, just rename for consistency
  summary_data$value_display <- summary_data$value

  # Determine mode and pivoting strategy
  mode <- if (n_groups > 1 || is.null(group)) "three_level" else "single"

  # Pivot to table format
  if (n_groups > 1 || is.null(group)) {
    # Mode 3: Strategy > Group columns
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = c(strategy, group),
        values_from = amount,
        names_sep = "_",
        id_cols = value_display
      )
  } else {
    # Mode 1 or 2: Strategy columns only
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = strategy,
        values_from = amount,
        id_cols = value_display
      )
  }

  # Round values
  value_cols <- setdiff(colnames(pivot_data), "value_display")
  for (col in value_cols) {
    pivot_data[[col]] <- round(pivot_data[[col]], decimals)
  }

  # Track total row index if requested
  total_row_index <- NULL
  if (show_total) {
    total_row <- pivot_data %>%
      summarize(across(all_of(value_cols), ~sum(., na.rm = TRUE))) %>%
      mutate(value_display = "Total")
    pivot_data <- bind_rows(pivot_data, total_row)
    total_row_index <- nrow(pivot_data)
  }

  # Rename value_display column to space (blank header)
  names(pivot_data)[names(pivot_data) == "value_display"] <- " "

  # Add spacer columns and build result_cols based on mode
  spacer_indices <- integer()
  if (mode == "three_level") {
    result_cols <- pivot_data[, " ", drop = FALSE]
    col_counter <- 1

    for (i in seq_along(strategies_display)) {
      col_counter <- col_counter + 1  # Next position is spacer

      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)
      spacer_indices <- c(spacer_indices, col_counter)

      col_counter <- col_counter + 1

      # Strategy columns
      strat <- strategies_display[i]
      strat_cols <- pivot_data[, grepl(paste0("^", strat, "_"), names(pivot_data)), drop = FALSE]
      result_cols <- cbind(result_cols, strat_cols)
      col_counter <- col_counter + ncol(strat_cols) - 1
    }
  } else {
    # Mode 1/2: No spacer columns
    result_cols <- pivot_data
  }

  # Keep original column names (Strategy_Group format) to avoid duplicates
  # The hierarchical headers will be handled by the renderer using compose()

  # Get all column names and indices
  all_cols <- colnames(result_cols)
  non_spacer_indices <- which(!grepl("^spacer_", all_cols))

  # Build column_names vector for kable in three-level mode
  column_names_vec <- NULL
  if (mode == "three_level") {
    column_names_vec <- c(" ")  # First column
    for (i in seq_along(strategies_display)) {
      column_names_vec <- c(column_names_vec, "")  # Spacer column

      # For each strategy's columns, extract just the group names
      for (grp in groups_display) {
        column_names_vec <- c(column_names_vec, grp)
      }
    }
  }

  # Build header structure
  if (mode == "three_level") {
    header_row1_values <- c("")
    header_row1_widths <- c(1)

    # Build level 2 values (group names) for proper display
    header_row2_values <- c(" ")
    header_row2_widths <- c(1)

    for (i in seq_along(strategies_display)) {
      header_row1_values <- c(header_row1_values, "")
      header_row1_widths <- c(header_row1_widths, 1)
      header_row1_values <- c(header_row1_values, strategies_display[i])
      header_row1_widths <- c(header_row1_widths, n_groups)

      # Add spacer and group names to level 2
      header_row2_values <- c(header_row2_values, "")
      header_row2_widths <- c(header_row2_widths, 1)
      for (j in seq_along(groups_display)) {
        header_row2_values <- c(header_row2_values, groups_display[j])
        header_row2_widths <- c(header_row2_widths, 1)
      }
    }

    # Create header vector for kableExtra
    hdr_vec <- c(" " = 1)
    for (i in seq_along(strategies_display)) {
      hdr_vec <- c(hdr_vec, setNames(1, paste0("spacer_", i)), setNames(n_groups, strategies_display[i]))
    }

    header_structure <- list(
      level1 = list(values = header_row1_values, widths = header_row1_widths, header_vector = hdr_vec),
      level2 = list(values = header_row2_values, widths = header_row2_widths),
      level3 = list(values = NA, widths = NA)
    )
  } else {
    # Single header level for Mode 1/2
    header_row1_values <- c("", strategies_display)
    header_row1_widths <- rep(1, length(header_row1_values))

    # Create header vector for kableExtra
    hdr_vec <- setNames(c(1, rep(1, n_strategies)), c("", strategies_display))

    header_structure <- list(
      level1 = list(values = header_row1_values, widths = header_row1_widths, header_vector = hdr_vec),
      level2 = list(values = NA, widths = NA),
      level3 = list(values = NA, widths = NA)
    )
  }

  # Create metadata list
  create_table_metadata(
    data = result_cols,
    mode = mode,
    n_strategies = n_strategies,
    n_groups = n_groups,
    n_values = length(values_display),
    n_states = NULL,
    strategy_names = strategies_display,
    group_names = groups_display,
    value_names = values_display,
    state_names = NULL,
    spacer_indices = spacer_indices,
    first_col_name = " ",
    first_col_type = "label",
    header_levels = if (mode == "three_level") 1 else 0,
    header_structure = header_structure,
    first_col_merge = if (mode == "three_level") TRUE else FALSE,
    column_names = column_names_vec,  # Use display names for kable
    replace_column_names = (mode == "three_level"),
    first_column_content = "",
    special_rows = list(total_row_index = total_row_index, bold_rows = integer()),
    decimals = decimals,
    value_range = "currency"
  )
}


#' Format Outcomes as Summary Breakdown Table
#'
#' Creates a table showing outcome summary totals broken down by component values.
#' Supports both flextable and kableExtra backends for flexible output formatting.
#'
#' @param results A heRomod2 model results object
#' @param summary_name Name of summary to display (e.g., "total_qalys")
#' @param group Group selection: "aggregated" (default), specific group, or NULL (all groups + aggregated)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values?
#' @param value_name_field Which value name field to use
#' @param strategy_name_field Which strategy name field to use
#' @param group_name_field Which group name field to use
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Summary table for aggregated
#' ft <- outcomes_table(results, "total_qalys")
#'
#' # Compare across groups
#' ft <- outcomes_table(results, "total_qalys", group = NULL)
#' }
#'
#' @export
outcomes_table <- function(results,
                           summary_name,
                           group = "aggregated",
                           strategies = NULL,
                           show_total = TRUE,
                           decimals = 2,
                           discounted = FALSE,
                           value_name_field = "display_name",
                           strategy_name_field = "display_name",
                           group_name_field = "display_name",
                           table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_outcomes_table_data(
    results = results,
    summary_name = summary_name,
    group = group,
    strategies = strategies,
    show_total = show_total,
    decimals = decimals,
    discounted = discounted,
    value_name_field = value_name_field,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' OLD FLEXTABLE IMPLEMENTATION - KEPT FOR REFERENCE, DELETE AFTER TESTING
#'
#' @keywords internal
outcomes_table_flextable_old <- function(results,
                                         summary_name,
                                         group = "aggregated",
                                         strategies = NULL,
                                         show_total = TRUE,
                                         decimals = 2,
                                         discounted = FALSE,
                                         value_name_field = "display_name",
                                         strategy_name_field = "display_name",
                                         group_name_field = "display_name") {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' required. Install with: install.packages('flextable')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' required. Install with: install.packages('officer')")
  }

  # Get summary data (names already mapped by get_summaries)
  summary_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = summary_name,
    value_type = "outcome",
    discounted = discounted,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = value_name_field
  )

  # Get unique strategies, groups, values (already have display names)
  strategies_display <- unique(summary_data$strategy)
  groups_display <- unique(summary_data$group)
  values_display <- unique(summary_data$value)
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Value column is already mapped, just rename for consistency
  summary_data$value_display <- summary_data$value

  # Pivot to table format
  if (n_groups > 1 || is.null(group)) {
    # Mode 3: Strategy > Group columns
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = c(strategy, group),
        values_from = amount,
        names_sep = "_",
        id_cols = value_display
      )
  } else {
    # Mode 1 or 2: Strategy columns only
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = strategy,
        values_from = amount,
        id_cols = value_display
      )
  }

  # Round values
  value_cols <- setdiff(colnames(pivot_data), "value_display")
  for (col in value_cols) {
    pivot_data[[col]] <- round(pivot_data[[col]], decimals)
  }

  # Add total row if requested
  if (show_total) {
    total_row <- pivot_data %>%
      summarize(across(all_of(value_cols), ~sum(., na.rm = TRUE))) %>%
      mutate(value_display = "Total")
    pivot_data <- bind_rows(pivot_data, total_row)
  }

  # Rename value_display column to space (blank header)
  names(pivot_data)[names(pivot_data) == "value_display"] <- " "

  # Add spacer columns between strategy groups (only in multi-group mode)
  result_cols <- pivot_data[, " ", drop = FALSE]

  for (i in seq_along(strategies_display)) {
    # Spacer - only in multi-group mode
    if (n_groups > 1 || is.null(group)) {
      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)
    }

    # Strategy columns
    strat <- strategies_display[i]
    if (n_groups > 1 || is.null(group)) {
      strat_cols <- pivot_data[, grepl(paste0("^", strat, "_"), names(pivot_data)), drop = FALSE]
    } else {
      strat_cols <- pivot_data[, strat, drop = FALSE]
    }
    result_cols <- cbind(result_cols, strat_cols)
  }

  # Create flextable
  ft <- flextable(result_cols)

  # Format value columns
  value_col_names <- setdiff(colnames(result_cols),
                             c(" ", grep("^spacer_", colnames(result_cols), value = TRUE)))
  for (col in value_col_names) {
    ft <- colformat_double(ft, j = col, digits = decimals)
  }

  # Build headers
  all_cols <- colnames(result_cols)
  spacer_col_indices <- which(grepl("^spacer_", all_cols))
  non_spacer_col_indices <- setdiff(seq_along(all_cols), spacer_col_indices)

  if (n_groups > 1 || is.null(group)) {
    # Add strategy header row
    header_row1_values <- c("")
    header_row1_widths <- c(1)

    for (i in seq_along(strategies_display)) {
      header_row1_values <- c(header_row1_values, "")  # Spacer
      header_row1_widths <- c(header_row1_widths, 1)
      header_row1_values <- c(header_row1_values, strategies_display[i])
      header_row1_widths <- c(header_row1_widths, n_groups)
    }

    ft <- ft %>%
      add_header_row(values = header_row1_values, colwidths = header_row1_widths, top = TRUE)

    # Merge first column
    ft <- merge_at(ft, i = 1:2, j = 1, part = "header")

    # Set group names in row 2
    current_col <- 2
    for (i in seq_along(strategies_display)) {
      ft <- compose(ft, i = 2, j = current_col,
                              value = as_paragraph(""),
                              part = "header")
      current_col <- current_col + 1
      for (j in seq_along(groups_display)) {
        ft <- compose(ft, i = 2, j = current_col,
                                value = as_paragraph(groups_display[j]),
                                part = "header")
        current_col <- current_col + 1
      }
    }

    # Blank first column header in multi-group mode too
    ft <- compose(ft, i = 1:2, j = 1,
                  value = as_paragraph(""),
                  part = "header")
  } else {
    # Simple header with strategy names
    # Set first column header to blank
    ft <- compose(ft, i = 1, j = 1,
                  value = as_paragraph(""),
                  part = "header")
  }

  # Apply styling
  ft <- ft %>%
    bg(bg = "white", part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header")

  # Bold TOTAL row if present
  if (show_total) {
    ft <- bold(ft, i = nrow(result_cols), part = "body")
  }

  # Apply borders
  black_border <- fp_border(color = "black", width = 1)
  no_border <- fp_border(width = 0)

  ft <- border_remove(ft)

  for (col_idx in non_spacer_col_indices) {
    ft <- hline_top(ft, j = col_idx, border = black_border, part = "header")
    ft <- hline_bottom(ft, j = col_idx, border = black_border, part = "header")
    ft <- hline_bottom(ft, j = col_idx, border = black_border, part = "body")
  }

  # Style spacers
  if (length(spacer_col_indices) > 0) {
    ft <- width(ft, j = spacer_col_indices, width = 0.01)
    ft <- void(ft, j = spacer_col_indices, part = "all")
    for (spacer_idx in spacer_col_indices) {
      ft <- border(ft, i = NULL, j = spacer_idx, border = no_border, part = "all")
    }
  }

  # Add border above Total row if present (applies to both modes)
  if (show_total) {
    ft <- hline(ft, i = nrow(result_cols) - 1, j = non_spacer_col_indices,
                border = black_border, part = "body")
  }

  # Autofit
  ft <- autofit(ft, add_w = 0, add_h = 0)

  ft
}


#' Format Net Monetary Benefit as Cycle-by-Cycle Table
#'
#' Creates a flextable showing NMB values for each cycle.
#' NMB = (Difference in Outcomes × WTP) - Difference in Costs
#'
#' @param results A heRomod2 model results object
#' @param outcome_summary Name of the outcome summary (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary (e.g., "total_cost")
#' @param group Group selection: "aggregated", specific group, or NULL
#' @param wtp Optional override for willingness-to-pay
#' @param referent Single reference strategy for intervention perspective.
#' @param comparator Single reference strategy for comparator perspective.
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param cycles Integer vector or range of cycles to display (NULL for all)
#' @param cumulative Logical. Show cumulative totals (TRUE, default)
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values?
#' @param value_name_field Which value name field to use
#' @param strategy_name_field Which strategy name field to use
#' @param group_name_field Which group name field to use
#' @param summary_name_field Which summary name field to use
#' @param time_unit Time unit: "cycle", "day", "week", "month", "year"
#'
#' @return A flextable object
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' ft <- format_nmb_cycle_table(results, "total_qalys", "total_cost",
#'                              comparator = "control")
#' }
#'
#' @keywords internal
format_nmb_cycle_table <- function(results,
                                   outcome_summary,
                                   cost_summary,
                                   group = "aggregated",
                                   wtp = NULL,
                                   referent = NULL,
                                   comparator = NULL,
                                   strategies = NULL,
                                   cycles = NULL,
                                   cumulative = TRUE,
                                   decimals = 2,
                                   discounted = FALSE,
                                   value_name_field = "display_name",
                                   strategy_name_field = "display_name",
                                   group_name_field = "display_name",
                                   summary_name_field = "display_name",
                                   time_unit = "cycle") {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' required. Install with: install.packages('flextable')")
  }

  # Validate referent/comparator
  if (is.null(referent) && is.null(comparator)) {
    stop("One of 'referent' or 'comparator' must be provided")
  }

  # Get WTP
  if (is.null(wtp)) {
    if (is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata.")
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(name == outcome_summary)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Outcome summary '%s' not found", outcome_summary))
    }
    wtp <- outcome_meta$wtp[1]
    if (is.na(wtp)) {
      stop("WTP not found. Provide explicit wtp parameter.")
    }
  }

  # Get time-series data with difference calculations via referent/comparator
  outcome_data <- get_values(
    results,
    format = "long",
    group = group,
    strategies = strategies,
    value_type = "outcome",
    discounted = discounted,
    cycles = cycles,
    time_unit = time_unit,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    referent = referent,
    comparator = comparator
  )

  cost_data <- get_values(
    results,
    format = "long",
    group = group,
    strategies = strategies,
    value_type = "cost",
    discounted = discounted,
    cycles = cycles,
    time_unit = time_unit,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    referent = referent,
    comparator = comparator
  )

  # Get time column
  time_cols <- c("cycle", "day", "week", "month", "year")
  time_col <- intersect(time_cols, colnames(outcome_data))[1]

  # Aggregate by time, strategy, group
  outcome_agg <- outcome_data %>%
    group_by(!!sym(time_col), strategy, group) %>%
    summarize(outcome_amount = sum(amount, na.rm = TRUE),
              .groups = "drop")

  cost_agg <- cost_data %>%
    group_by(!!sym(time_col), strategy, group) %>%
    summarize(cost_amount = sum(amount, na.rm = TRUE),
              .groups = "drop")

  # Calculate cumulative if requested
  if (cumulative) {
    outcome_agg <- outcome_agg %>%
      group_by(strategy, group) %>%
      arrange(!!sym(time_col)) %>%
      mutate(outcome_amount = cumsum(outcome_amount)) %>%
      ungroup()

    cost_agg <- cost_agg %>%
      group_by(strategy, group) %>%
      arrange(!!sym(time_col)) %>%
      mutate(cost_amount = cumsum(cost_amount)) %>%
      ungroup()
  }

  # Combine outcome and cost data
  nmb_data <- outcome_agg %>%
    full_join(cost_agg, by = c(time_col, "strategy", "group")) %>%
    mutate(nmb = outcome_amount * wtp - cost_amount) %>%
    select(all_of(c(time_col, "group", "strategy", "nmb")))

  # Pivot to wide format
  table_data <- nmb_data %>%
    pivot_wider(
      names_from = "strategy",
      values_from = "nmb"
    )

  # Round values
  value_cols <- setdiff(colnames(table_data), c(time_col, "group"))
  for (col in value_cols) {
    table_data[[col]] <- round(table_data[[col]], decimals)
  }

  # Create flextable
  ft <- flextable(table_data)

  # Format value columns
  for (col in value_cols) {
    ft <- colformat_double(ft, j = col, digits = decimals)
  }

  # Apply styling
  ft <- ft %>%
    bg(bg = "white", part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header")

  # Apply borders
  black_border <- fp_border(color = "black", width = 1)
  ft <- border_remove(ft)
  ft <- hline_top(ft, border = black_border, part = "header")
  ft <- hline_bottom(ft, border = black_border, part = "header")
  ft <- hline_bottom(ft, border = black_border, part = "body")

  ft <- autofit(ft, add_w = 0, add_h = 0)
  ft
}


#' Prepare Net Monetary Benefit Table Data
#'
#' Internal helper function that prepares NMB data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A heRomod2 model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param group Group selection: "aggregated", specific group, or NULL
#' @param wtp Optional override for willingness-to-pay
#' @param referent Single reference strategy for intervention perspective.
#' @param comparator Single reference strategy for comparator perspective.
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values?
#' @param value_name_field Which value name field to use
#' @param strategy_name_field Which strategy name field to use
#' @param group_name_field Which group name field to use
#' @param summary_name_field Which summary name field to use
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_nmb_table_data <- function(results,
                                  outcome_summary,
                                  cost_summary,
                                  group = "aggregated",
                                  wtp = NULL,
                                  referent = NULL,
                                  comparator = NULL,
                                  strategies = NULL,
                                  show_total = TRUE,
                                  decimals = 2,
                                  discounted = FALSE,
                                  value_name_field = "display_name",
                                  strategy_name_field = "display_name",
                                  group_name_field = "display_name",
                                  summary_name_field = "display_name") {

  # Validate referent/comparator
  if (is.null(referent) && is.null(comparator)) {
    stop("One of 'referent' or 'comparator' must be provided")
  }

  # Get WTP
  if (is.null(wtp)) {
    if (is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata.")
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(name == outcome_summary)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Outcome summary '%s' not found", outcome_summary))
    }
    wtp <- outcome_meta$wtp[1]
    if (is.na(wtp)) {
      stop("WTP not found. Provide explicit wtp parameter.")
    }
  }

  # Get outcome and cost summaries with differences
  outcome_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = outcome_summary,
    value_type = "outcome",
    discounted = discounted,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = value_name_field,
    referent = referent,
    comparator = comparator
  )

  cost_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = cost_summary,
    value_type = "cost",
    discounted = discounted,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = value_name_field,
    referent = referent,
    comparator = comparator
  )

  # Transform outcome data: multiply by WTP
  outcome_data <- outcome_data %>%
    mutate(amount = amount * wtp)

  # Transform cost data: negate
  cost_data <- cost_data %>%
    mutate(amount = -amount)

  # Combine outcome and cost data
  combined_data <- bind_rows(outcome_data, cost_data) %>%
    select(strategy, group, value, amount)

  # Get unique strategies, groups, values
  strategies_display <- unique(combined_data$strategy)
  groups_display <- unique(combined_data$group)
  values_display <- unique(combined_data$value)
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(group)) "three_level" else "single"

  # Pivot to table format
  if (mode == "three_level") {
    # Mode 3: Strategy > Group columns
    pivot_data <- combined_data %>%
      pivot_wider(
        names_from = c(strategy, group),
        values_from = amount,
        names_sep = "_",
        id_cols = value
      )
  } else {
    # Mode 1 or 2: Strategy columns only
    pivot_data <- combined_data %>%
      pivot_wider(
        names_from = strategy,
        values_from = amount,
        id_cols = value
      )
  }

  # Round values
  value_cols <- setdiff(colnames(pivot_data), "value")
  for (col in value_cols) {
    pivot_data[[col]] <- round(pivot_data[[col]], decimals)
  }

  # Track total row index if requested
  total_row_index <- NULL
  if (show_total) {
    total_row <- pivot_data %>%
      summarize(across(all_of(value_cols), ~sum(., na.rm = TRUE))) %>%
      mutate(value = "Total")
    pivot_data <- bind_rows(pivot_data, total_row)
    total_row_index <- nrow(pivot_data)
  }

  # Rename value column to space (blank header) for NMB tables
  names(pivot_data)[names(pivot_data) == "value"] <- " "

  # Build result columns based on mode
  spacer_indices <- integer()
  if (mode == "three_level") {
    result_cols <- pivot_data[, " ", drop = FALSE]
    col_counter <- 1

    for (i in seq_along(strategies_display)) {
      col_counter <- col_counter + 1  # Next position is spacer

      # Add spacer before each strategy
      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)
      spacer_indices <- c(spacer_indices, col_counter)

      col_counter <- col_counter + 1

      # Get columns for this strategy
      strat <- strategies_display[i]
      strat_cols <- pivot_data[, grepl(paste0("^", strat, "_"), names(pivot_data)), drop = FALSE]
      result_cols <- cbind(result_cols, strat_cols)
      col_counter <- col_counter + ncol(strat_cols) - 1
    }
  } else {
    # Mode 1/2: No spacer columns, just rename columns to strategy names
    result_cols <- pivot_data
  }

  # Get all column names and indices
  all_cols <- colnames(result_cols)
  non_spacer_indices <- which(!grepl("^spacer_", all_cols))

  # Build column_names vector for kable in three-level mode
  column_names_vec <- NULL
  if (mode == "three_level") {
    column_names_vec <- c(" ")  # First column
    for (i in seq_along(strategies_display)) {
      column_names_vec <- c(column_names_vec, "")  # Spacer column

      # For each strategy's columns, extract just the group names
      for (grp in groups_display) {
        column_names_vec <- c(column_names_vec, grp)
      }
    }
  }

  # Build header structure
  if (mode == "three_level") {
    header_row1_values <- c("")
    header_row1_widths <- c(1)

    # Build level 2 values (group names) for proper display
    header_row2_values <- c(" ")
    header_row2_widths <- c(1)

    for (i in seq_along(strategies_display)) {
      header_row1_values <- c(header_row1_values, "")
      header_row1_widths <- c(header_row1_widths, 1)
      header_row1_values <- c(header_row1_values, strategies_display[i])
      header_row1_widths <- c(header_row1_widths, n_groups)

      # Add spacer and group names to level 2
      header_row2_values <- c(header_row2_values, "")
      header_row2_widths <- c(header_row2_widths, 1)
      for (j in seq_along(groups_display)) {
        header_row2_values <- c(header_row2_values, groups_display[j])
        header_row2_widths <- c(header_row2_widths, 1)
      }
    }

    # Create header vector for kableExtra
    hdr_vec <- c(" " = 1)
    for (i in seq_along(strategies_display)) {
      hdr_vec <- c(hdr_vec, setNames(1, paste0("spacer_", i)), setNames(n_groups, strategies_display[i]))
    }

    header_structure <- list(
      level1 = list(values = header_row1_values, widths = header_row1_widths, header_vector = hdr_vec),
      level2 = list(values = header_row2_values, widths = header_row2_widths),
      level3 = list(values = NA, widths = NA)
    )
  } else {
    # Single header level for Mode 1/2 - just strategy names
    header_row1_values <- c("", strategies_display)
    header_row1_widths <- rep(1, length(header_row1_values))

    # Create header vector for kableExtra
    hdr_vec <- setNames(rep(1, length(header_row1_values)), header_row1_values)

    header_structure <- list(
      level1 = list(values = header_row1_values, widths = header_row1_widths, header_vector = hdr_vec),
      level2 = list(values = NA, widths = NA),
      level3 = list(values = NA, widths = NA)
    )
  }

  # Create metadata list
  create_table_metadata(
    data = result_cols,
    mode = mode,
    n_strategies = n_strategies,
    n_groups = n_groups,
    n_values = length(values_display),
    n_states = NULL,
    strategy_names = strategies_display,
    group_names = groups_display,
    value_names = values_display,
    state_names = NULL,
    spacer_indices = spacer_indices,
    first_col_name = " ",
    first_col_type = "label",
    header_levels = if (mode == "three_level") 1 else 0,
    header_structure = header_structure,
    first_col_merge = if (mode == "three_level") TRUE else FALSE,
    column_names = column_names_vec,  # Use display names for kable
    replace_column_names = (mode == "three_level"),
    first_column_content = "",
    special_rows = list(total_row_index = total_row_index, bold_rows = integer()),
    decimals = decimals,
    value_range = "currency"
  )
}


#' Format Net Monetary Benefit as Summary Table
#'
#' Creates a table showing NMB breakdown by component values.
#' Supports both flextable and kableExtra backends for flexible output formatting.
#'
#' @param results A heRomod2 model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param group Group selection: "aggregated", specific group, or NULL
#' @param wtp Optional override for willingness-to-pay
#' @param referent Single reference strategy for intervention perspective.
#' @param comparator Single reference strategy for comparator perspective.
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values?
#' @param value_name_field Which value name field to use
#' @param strategy_name_field Which strategy name field to use
#' @param group_name_field Which group name field to use
#' @param summary_name_field Which summary name field to use
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Create table using flextable (default)
#' ft <- nmb_table(results, "total_qalys", "total_cost",
#'                 comparator = "control")
#'
#' # Create table using kableExtra
#' kt <- nmb_table(results, "total_qalys", "total_cost",
#'                 comparator = "control", table_format = "kable")
#' }
#'
#' @export
nmb_table <- function(results,
                     outcome_summary,
                     cost_summary,
                     group = "aggregated",
                     wtp = NULL,
                     referent = NULL,
                     comparator = NULL,
                     strategies = NULL,
                     show_total = TRUE,
                     decimals = 2,
                     discounted = FALSE,
                     value_name_field = "display_name",
                     strategy_name_field = "display_name",
                     group_name_field = "display_name",
                     summary_name_field = "display_name",
                     table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_nmb_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    wtp = wtp,
    referent = referent,
    comparator = comparator,
    strategies = strategies,
    show_total = show_total,
    decimals = decimals,
    discounted = discounted,
    value_name_field = value_name_field,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    summary_name_field = summary_name_field
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
