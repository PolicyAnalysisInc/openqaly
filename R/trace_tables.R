#' Prepare Trace Table Data
#'
#' Internal helper function that prepares trace data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A heRomod2 model results object
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param states Character vector of states to include (NULL for all)
#' @param cycles Integer vector or range of cycles to display (NULL for all)
#' @param decimals Number of decimal places for probabilities (default: 4)
#' @param strategy_name_field Which strategy name field to use
#' @param state_name_field Which state name field to use
#' @param time_unit Which time unit to display
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_trace_table_data <- function(results,
                                     strategies = NULL,
                                     states = NULL,
                                     cycles = NULL,
                                     decimals = 4,
                                     strategy_name_field = "display_name",
                                     state_name_field = "display_name",
                                     time_unit = "cycle",
                                     font_size = 11) {

  # Get long format trace data (names already mapped by get_trace)
  trace_long <- get_trace(
    results,
    format = "long",
    collapsed = TRUE,
    strategies = strategies,
    states = states,
    cycles = cycles,
    time_unit = time_unit,
    strategy_name_field = strategy_name_field,
    state_name_field = state_name_field
  )

  # Get unique strategies and states (already have display names)
  strategies_display <- unique(trace_long$strategy)
  states_display <- unique(trace_long$state)
  n_states <- length(states_display)
  n_strategies <- length(strategies_display)

  # Determine time column name first
  time_col_name <- switch(time_unit,
    "cycle" = "cycle",
    "day" = "day",
    "week" = "week",
    "month" = "month",
    "year" = "year",
    "cycle"  # default
  )

  # Select only the relevant columns before pivoting
  trace_for_pivot <- trace_long %>%
    select(all_of(c(time_col_name, "strategy", "state", "probability")))

  # Pivot wider: strategies and states become columns
  trace_data <- trace_for_pivot %>%
    pivot_wider(
      names_from = c(strategy, state),
      values_from = probability,
      names_sep = "_",
      id_cols = all_of(time_col_name)
    )

  # Get time label for header
  time_label <- switch(time_unit,
    "cycle" = "Cycle",
    "day" = "Day",
    "week" = "Week",
    "month" = "Month",
    "year" = "Year",
    "Cycle"  # default
  )

  # Add spacer columns between strategy groups
  spacer_indices <- integer()
  if (n_strategies >= 1) {
    # Get the time column
    if (time_col_name %in% colnames(trace_data)) {
      time_col <- trace_data[, time_col_name, drop = FALSE]
    } else {
      # Fallback to cycle if specific time unit not available
      time_col <- trace_data[, "cycle", drop = FALSE]
      time_col_name <- "cycle"
      time_label <- "Cycle"
    }

    # Rename time column for display
    names(time_col) <- time_label
    result_cols <- time_col

    col_counter <- 1  # Time column is column 1
    for (i in seq_along(strategies_display)) {
      col_counter <- col_counter + 1  # Next position is spacer

      # Add spacer column BEFORE each strategy group
      spacer_col <- data.frame(rep("", nrow(trace_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)
      spacer_indices <- c(spacer_indices, col_counter)

      col_counter <- col_counter + 1  # Move past spacer

      # Get columns for this strategy
      strat <- strategies_display[i]
      strat_cols <- trace_data[, grepl(paste0("^", strat, "_"), names(trace_data)), drop = FALSE]
      result_cols <- cbind(result_cols, strat_cols)

      col_counter <- col_counter + ncol(strat_cols) - 1
    }

    trace_data <- result_cols

    # Keep original column names (Strategy_State format) to avoid duplicates
    # The hierarchical headers will be handled by the renderer using compose()
  }

  # Intelligent rounding for time column
  time_values <- trace_data[[time_label]]
  rounded_values <- round(time_values, 1)

  if (all(abs(rounded_values - round(rounded_values)) < 0.01)) {
    trace_data[[time_label]] <- as.integer(round(rounded_values))
  } else {
    trace_data[[time_label]] <- rounded_values
  }

  # Round probability columns BEFORE creating table to prevent negative zeros
  prob_cols <- setdiff(
    colnames(trace_data),
    c(time_label, grep("^spacer_", colnames(trace_data), value = TRUE))
  )
  for (col in prob_cols) {
    trace_data[[col]] <- round(trace_data[[col]], decimals)
  }

  # Get column indices
  all_cols <- colnames(trace_data)
  non_spacer_indices <- which(!grepl("^spacer_", all_cols))

  # Build header structure for flextable
  header_row1_values <- c(" ")  # Empty for merged cell
  header_row1_widths <- c(1)

  # Build level 2 values (state names) for proper display
  header_row2_values <- c(time_label)  # Show actual time unit (Cycle/Day/Week/etc.)
  header_row2_widths <- c(1)

  for (i in seq_along(strategies_display)) {
    header_row1_values <- c(header_row1_values, "")
    header_row1_widths <- c(header_row1_widths, 1)
    header_row1_values <- c(header_row1_values, strategies_display[i])
    header_row1_widths <- c(header_row1_widths, n_states)

    # Add spacer and state names to level 2
    header_row2_values <- c(header_row2_values, "")
    header_row2_widths <- c(header_row2_widths, 1)
    for (j in seq_along(states_display)) {
      header_row2_values <- c(header_row2_values, states_display[j])
      header_row2_widths <- c(header_row2_widths, 1)
    }
  }

  header_structure <- list(
    level1 = list(values = header_row1_values, widths = header_row1_widths),
    level2 = list(values = header_row2_values, widths = header_row2_widths),
    level3 = list(values = NA, widths = NA)
  )

  # Create header vector for kableExtra (add_header_above format)
  # Note: Named vector where empty strings are spacer columns
  level1_vector <- c(" " = 1)  # Empty for merged cell appearance
  for (i in seq_along(strategies_display)) {
    # Add spacer with special marker (kableExtra will handle empty string names)
    spacer_name <- paste0("spacer_", i)
    level1_vector <- c(level1_vector, setNames(1, spacer_name), setNames(n_states, strategies_display[i]))
  }
  header_structure$level1$header_vector <- level1_vector

  # Build column_names vector for kable (time label, then spacers and states)
  column_names_vec <- c(time_label)  # Start with time label
  for (i in seq_along(strategies_display)) {
    column_names_vec <- c(column_names_vec, "")  # Spacer column
    for (j in seq_along(states_display)) {
      column_names_vec <- c(column_names_vec, states_display[j])
    }
  }

  # Create metadata list
  create_table_metadata(
    data = trace_data,
    mode = "two_level",
    n_strategies = n_strategies,
    n_groups = NULL,
    n_values = NULL,
    n_states = n_states,
    strategy_names = strategies_display,
    group_names = NULL,
    value_names = NULL,
    state_names = states_display,
    spacer_indices = spacer_indices,
    first_col_name = time_label,
    first_col_type = "time",
    header_levels = 1,
    header_structure = header_structure,
    first_col_merge = TRUE,
    column_names = column_names_vec,
    replace_column_names = TRUE,
    first_column_content = "",
    special_rows = list(total_row_index = NULL, bold_rows = integer()),
    decimals = decimals,
    font_size = font_size,
    value_range = "probability"
  )
}


#' Format Trace as Table
#'
#' Creates a publication-quality table with clean black and white styling showing
#' state probabilities over time. Strategies appear as column groups with hierarchical
#' headers separated by spacer columns for easy visual comparison.
#'
#' Supports both flextable and kableExtra backends for flexible output to Word,
#' PowerPoint, HTML, and PDF documents.
#'
#' @param results A heRomod2 model results object
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param states Character vector of states to include (NULL for all)
#' @param cycles Integer vector or range of cycles to display (NULL for all)
#' @param decimals Number of decimal places for probabilities (default: 4)
#' @param strategy_name_field Which strategy name field to use: "name" (technical name),
#'   "display_name", or "abbreviation". Default is "display_name" if available.
#' @param state_name_field Which state name field to use: "name" (technical name),
#'   "display_name", or "abbreviation". Default is "display_name" if available.
#' @param time_unit Which time unit to display: "cycle" (default), "day", "week",
#'   "month", or "year".
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
#' ft <- trace_table(results)
#'
#' # Create table using kableExtra
#' kt <- trace_table(results, table_format = "kable")
#'
#' # Show specific strategies and cycles
#' trace_table(results, strategies = c("Strategy1", "Strategy2"), cycles = 0:20)
#' }
#'
#' @export
trace_table <- function(results,
                        strategies = NULL,
                        states = NULL,
                        cycles = NULL,
                        decimals = 4,
                        strategy_name_field = "display_name",
                        state_name_field = "display_name",
                        time_unit = "cycle",
                        font_size = 11,
                        table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_trace_table_data(
    results = results,
    strategies = strategies,
    states = states,
    cycles = cycles,
    decimals = decimals,
    strategy_name_field = strategy_name_field,
    state_name_field = state_name_field,
    time_unit = time_unit,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
