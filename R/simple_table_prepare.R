#' Simple Table Preparation Functions
#'
#' Functions to prepare table data and generate clean table specifications
#' without any domain-specific knowledge.
#'
#' @keywords internal

#' Prepare Simple Trace Table Specification
#'
#' @param results openqaly results object
#' @param strategies Character vector of strategies to include
#' @param group Group selection
#' @param states Character vector of states to include
#' @param cycles Integer vector of cycles to display
#' @param decimals Number of decimal places
#' @param state_name_field Field to use for state names
#' @param time_unit Time unit to display
#' @param font_size Font size in points
#'
#' @return A simple table specification
#' @keywords internal
prepare_simple_trace_table <- function(results,
                                      strategies = NULL,
                                      group = "aggregated",
                                      states = NULL,
                                      cycles = NULL,
                                      decimals = 4,
                                      state_name_field = "display_name",
                                      time_unit = "cycle",
                                      font_size = 11) {

  # Get data using existing function
  trace_long <- get_trace(
    results,
    format = "long",
    collapsed = TRUE,
    strategies = strategies,
    groups = if(is.null(group)) "all" else if(group == "aggregated") NULL else group,
    states = states,
    cycles = cycles,
    time_unit = time_unit,
    strategy_name_field = "display_name",
    group_name_field = "display_name",
    state_name_field = state_name_field
  )

  # Extract unique values
  strategies_display <- unique(trace_long$strategy)
  groups_display <- if("group" %in% names(trace_long)) unique(trace_long$group) else NULL
  states_display <- unique(trace_long$state)
  n_states <- length(states_display)
  n_strategies <- length(strategies_display)
  has_groups <- !is.null(groups_display) && length(groups_display) > 1

  # Determine time column
  time_col_name <- switch(time_unit,
    "cycle" = "cycle", "day" = "day", "week" = "week",
    "month" = "month", "year" = "year", "cycle"
  )
  time_label <- switch(time_unit,
    "cycle" = "Cycle", "day" = "Day", "week" = "Week",
    "month" = "Month", "year" = "Year", "Cycle"
  )

  # Pivot data
  if (has_groups) {
    trace_data <- trace_long %>%
      select(all_of(c("group", time_col_name, "strategy", "state", "probability"))) %>%
      arrange(group, !!sym(time_col_name)) %>%
      pivot_wider(
        names_from = c(strategy, state),
        values_from = probability,
        names_sep = "_",
        id_cols = c("group", all_of(time_col_name))
      )
  } else {
    trace_data <- trace_long %>%
      select(all_of(c(time_col_name, "strategy", "state", "probability"))) %>%
      pivot_wider(
        names_from = c(strategy, state),
        values_from = probability,
        names_sep = "_",
        id_cols = all_of(time_col_name)
      )
  }

  # Build data frame with spacer columns
  result_data <- data.frame()
  column_alignments <- character()
  column_widths <- numeric()

  # First columns (Group and/or Time)
  if (has_groups) {
    result_data <- trace_data[, c("group", time_col_name), drop = FALSE]
    names(result_data) <- c("Group", time_label)
    column_alignments <- c("left", "left")
    column_widths <- c(NA, NA)
  } else {
    result_data <- trace_data[, time_col_name, drop = FALSE]
    names(result_data) <- time_label
    column_alignments <- "left"
    column_widths <- NA
  }

  # Format time column
  time_values <- result_data[[time_label]]
  rounded_values <- round(time_values, 1)
  if (all(abs(rounded_values - round(rounded_values)) < 0.01)) {
    result_data[[time_label]] <- as.character(as.integer(round(rounded_values)))
  } else {
    result_data[[time_label]] <- format(rounded_values, nsmall = 1, scientific = FALSE, trim = TRUE)
  }

  # Add strategy columns with spacers
  for (i in seq_along(strategies_display)) {
    # Add spacer column
    spacer_name <- paste0("spacer_", i)
    result_data[[spacer_name]] <- ""
    column_alignments <- c(column_alignments, "center")
    column_widths <- c(column_widths, 0.01)

    # Add state columns for this strategy
    strat <- strategies_display[i]
    for (state in states_display) {
      col_name <- paste0(strat, "_", state)
      if (col_name %in% names(trace_data)) {
        # Format probability values
        vals <- trace_data[[col_name]]
        rounded_vals <- round(vals, decimals)
        rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
        result_data[[col_name]] <- format(rounded_vals,
                                         nsmall = decimals,
                                         scientific = FALSE,
                                         trim = TRUE)
      } else {
        result_data[[col_name]] <- ""
      }
      column_alignments <- c(column_alignments, "right")
      column_widths <- c(column_widths, NA)
    }
  }

  # Build headers structure
  headers <- list()

  # First header row
  row1 <- list()

  # First column(s) - merged vertically
  if (has_groups) {
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    row1[[2]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    first_cols <- 2
  } else {
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    first_cols <- 1
  }

  # Strategy headers
  for (i in seq_along(strategies_display)) {
    # Spacer
    row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))
    # Strategy name spanning states
    row1[[length(row1) + 1]] <- list(
      span = n_states,
      text = strategies_display[i],
      borders = c(1, 0, 1, 0)
    )
  }

  headers[[1]] <- row1

  # Second header row
  row2 <- list()

  # Column labels
  if (has_groups) {
    row2[[1]] <- list(span = 1, text = "Group", borders = c(0, 0, 1, 0))
    row2[[2]] <- list(span = 1, text = time_label, borders = c(0, 0, 1, 0))
  } else {
    row2[[1]] <- list(span = 1, text = time_label, borders = c(0, 0, 1, 0))
  }

  # State names
  for (i in seq_along(strategies_display)) {
    # Spacer
    row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))
    # State names
    for (state in states_display) {
      row2[[length(row2) + 1]] <- list(
        span = 1,
        text = state,
        borders = c(0, 0, 1, 0)
      )
    }
  }

  headers[[2]] <- row2

  # Create the specification
  create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Prepare Simple Outcomes Table Specification
#'
#' @param results openqaly results object
#' @param summary_name Name of summary to display
#' @param group Group selection
#' @param strategies Character vector of strategies to include
#' @param show_total Show TOTAL row
#' @param decimals Number of decimal places
#' @param discounted Use discounted values
#' @param value_name_field Field to use for value names
#' @param font_size Font size in points
#'
#' @return A simple table specification
#' @keywords internal
prepare_simple_outcomes_table <- function(results,
                                         summary_name,
                                         group = "aggregated",
                                         strategies = NULL,
                                         show_total = TRUE,
                                         decimals = 2,
                                         discounted = FALSE,
                                         value_name_field = "display_name",
                                         font_size = 11) {

  # Get summary data
  summary_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = summary_name,
    value_type = "outcome",
    discounted = discounted,
    strategy_name_field = "display_name",
    group_name_field = "display_name",
    value_name_field = value_name_field
  )

  # Extract unique values
  strategies_display <- unique(summary_data$strategy)
  groups_display <- unique(summary_data$group)
  values_display <- unique(summary_data$value)
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1 || is.null(group)

  # Pivot data
  if (has_multiple_groups) {
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = c(strategy, group),
        values_from = amount,
        names_sep = "_",
        id_cols = value
      )
  } else {
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = strategy,
        values_from = amount,
        id_cols = value
      )
  }

  # Round values
  value_cols <- setdiff(colnames(pivot_data), "value")
  for (col in value_cols) {
    if (is.numeric(pivot_data[[col]])) {
      pivot_data[[col]] <- round(pivot_data[[col]], decimals)
    }
  }

  # Add total row if requested
  total_row_index <- NULL
  if (show_total) {
    total_row <- pivot_data %>%
      summarize(across(all_of(value_cols), ~sum(., na.rm = TRUE))) %>%
      mutate(value = "Total")
    pivot_data <- bind_rows(pivot_data, total_row)
    total_row_index <- nrow(pivot_data)
  }

  # Format numeric columns
  for (col in value_cols) {
    if (is.numeric(pivot_data[[col]])) {
      rounded_vals <- pivot_data[[col]]
      rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
      pivot_data[[col]] <- format(rounded_vals,
                                  nsmall = decimals,
                                  scientific = FALSE,
                                  trim = TRUE)
    }
  }

  # Rename first column
  names(pivot_data)[1] <- " "

  # Build result data with spacers if multi-group
  if (has_multiple_groups) {
    result_data <- pivot_data[, " ", drop = FALSE]
    column_alignments <- "left"
    column_widths <- NA

    for (i in seq_along(strategies_display)) {
      # Add spacer
      spacer_name <- paste0("spacer_", i)
      result_data[[spacer_name]] <- ""
      column_alignments <- c(column_alignments, "center")
      column_widths <- c(column_widths, 0.01)

      # Add group columns for this strategy
      strat <- strategies_display[i]
      strat_cols <- pivot_data[, grepl(paste0("^", strat, "_"), names(pivot_data)), drop = FALSE]
      result_data <- cbind(result_data, strat_cols)

      for (j in seq_len(ncol(strat_cols))) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }
  } else {
    result_data <- pivot_data
    column_alignments <- c("left", rep("right", n_strategies))
    column_widths <- rep(NA, ncol(result_data))
  }

  # Build headers
  headers <- list()

  if (has_multiple_groups) {
    # First row: Strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))

    for (i in seq_along(strategies_display)) {
      # Spacer
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))
      # Strategy name
      row1[[length(row1) + 1]] <- list(
        span = n_groups,
        text = strategies_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Second row: Group names
    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))

    for (i in seq_along(strategies_display)) {
      # Spacer
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))
      # Group names
      for (grp in groups_display) {
        row2[[length(row2) + 1]] <- list(
          span = 1,
          text = grp,
          borders = c(0, 0, 1, 0)
        )
      }
    }
    headers[[2]] <- row2
  }
  # If no multiple groups, no special headers needed (use column names)

  # Special rows
  special_rows <- list()
  if (!is.null(total_row_index)) {
    special_rows$total_row <- total_row_index
  }

  # Create the specification
  create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size,
    font_family = "Helvetica"
  )
}