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
#' @param time_unit Which time unit to display
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_trace_table_data <- function(results,
                                     strategies = NULL,
                                     groups = "overall",
                                     states = NULL,
                                     cycles = NULL,
                                     decimals = 4,
                                     time_unit = "cycle",
                                     font_size = 11) {

  # Get long format trace data (names already mapped by get_trace)
  trace_long <- get_trace(
    results,
    format = "long",
    collapsed = TRUE,
    strategies = strategies,
    groups = groups,
    states = states,
    cycles = cycles,
    time_unit = time_unit,
    use_display_names = TRUE
  )

  # Get unique strategies, groups, and states (already have display names)
  strategies_display <- unique(trace_long$strategy)
  groups_display <- if("group" %in% names(trace_long)) unique(trace_long$group) else NULL
  states_display <- unique(trace_long$state)
  n_states <- length(states_display)
  n_strategies <- length(strategies_display)
  n_groups <- if(!is.null(groups_display)) length(groups_display) else 0

  # Determine if we have multiple groups to display
  has_multiple_groups <- n_groups > 1

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
  if (has_multiple_groups) {
    # Multiple groups: keep group column and use it as an id column
    trace_for_pivot <- trace_long %>%
      select(all_of(c("group", time_col_name, "strategy", "state", "probability"))) %>%
      arrange(group, !!sym(time_col_name))  # Ensure group-first ordering

    # Pivot wider: strategies and states become columns, group and time are row identifiers
    trace_data <- trace_for_pivot %>%
      pivot_wider(
        names_from = c(strategy, state),
        values_from = probability,
        names_sep = "_",
        id_cols = c("group", all_of(time_col_name))
      )
  } else {
    # Single group: no group column needed
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
  }

  # Get time label for header
  time_label <- switch(time_unit,
    "cycle" = "Cycle",
    "day" = "Day",
    "week" = "Week",
    "month" = "Month",
    "year" = "Year",
    "Cycle"  # default
  )

  # Add spacer columns between strategies
  spacer_indices <- integer()

  # Build the column structure
  if (has_multiple_groups) {
    # Start with group column
    group_col <- trace_data[, "group", drop = FALSE]
    names(group_col) <- "Group"
    result_cols <- group_col
    col_counter <- 1
  } else {
    result_cols <- NULL
    col_counter <- 0
  }

  # Add time column
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
  if (is.null(result_cols)) {
    result_cols <- time_col
  } else {
    result_cols <- cbind(result_cols, time_col)
  }
  col_counter <- col_counter + 1  # Time column

  # Add spacers between strategies
  for (i in seq_along(strategies_display)) {
    col_counter <- col_counter + 1  # Next position is spacer

    # Add spacer column BEFORE each strategy
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

  # Intelligent rounding and formatting for time column
  time_values <- trace_data[[time_label]]
  rounded_values <- round(time_values, 1)

  if (all(abs(rounded_values - round(rounded_values)) < 0.01)) {
    trace_data[[time_label]] <- as.character(as.integer(round(rounded_values)))
  } else {
    trace_data[[time_label]] <- format(rounded_values, nsmall = 1, scientific = FALSE, trim = TRUE)
  }

  # Format probability columns as character strings to prevent renderer reformatting
  prob_cols <- setdiff(
    colnames(trace_data),
    c("Group", time_label, grep("^spacer_", colnames(trace_data), value = TRUE))
  )
  for (col in prob_cols) {
    if (is.numeric(trace_data[[col]])) {
      rounded_vals <- round(trace_data[[col]], decimals)
      # Fix negative zero display
      rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
      trace_data[[col]] <- format(rounded_vals,
                                  nsmall = decimals,
                                  scientific = FALSE,
                                  trim = TRUE)
    }
  }

  # Get column indices
  all_cols <- colnames(trace_data)
  non_spacer_indices <- which(!grepl("^spacer_", all_cols))

  # Build header structure for two levels: Strategies > States
  # First row spans merged cells for group/time columns
  if (has_multiple_groups) {
    # Group and time columns are merged
    header_row1_values <- c(" ", " ")  # Empty for group and time
    header_row1_widths <- c(1, 1)
    first_col_count <- 2
  } else {
    # Just time column is merged
    header_row1_values <- c(" ")  # Empty for time
    header_row1_widths <- c(1)
    first_col_count <- 1
  }

  # Second row has actual column names
  if (has_multiple_groups) {
    header_row2_values <- c("Group", time_label)
    header_row2_widths <- c(1, 1)
  } else {
    header_row2_values <- c(time_label)
    header_row2_widths <- c(1)
  }

  # Add strategies and states
  for (i in seq_along(strategies_display)) {
    # Add spacer to both rows
    header_row1_values <- c(header_row1_values, "")
    header_row1_widths <- c(header_row1_widths, 1)
    header_row2_values <- c(header_row2_values, "")
    header_row2_widths <- c(header_row2_widths, 1)

    # Add strategy name spanning its states
    header_row1_values <- c(header_row1_values, strategies_display[i])
    header_row1_widths <- c(header_row1_widths, n_states)

    # Add state names
    for (j in seq_along(states_display)) {
      header_row2_values <- c(header_row2_values, states_display[j])
      header_row2_widths <- c(header_row2_widths, 1)
    }
  }

  header_structure <- list(
    level1 = list(values = header_row1_values, widths = header_row1_widths),
    level2 = list(values = header_row2_values, widths = header_row2_widths)
  )

  # Create header vector for kableExtra
  level1_vector <- setNames(header_row1_widths, header_row1_values)
  # Replace spacer names with single space for kableExtra
  names(level1_vector)[grepl("^$", names(level1_vector))] <- " "
  header_structure$level1$header_vector <- level1_vector

  # Build column_names vector
  column_names_vec <- header_row2_values

  # Build clean headers structure
  headers <- list()

  # First header row - merged columns and strategy names
  row1 <- list()
  if (has_multiple_groups) {
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
    row1[[2]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
  } else {
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
  }

  # Add strategy headers with spacers
  for (i in seq_along(strategies_display)) {
    row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
    row1[[length(row1) + 1]] <- list(
      span = n_states,
      text = strategies_display[i],
      borders = c(1, 0, 1, 0)
    )
  }
  headers[[1]] <- row1

  # Second header row - column labels and state names
  row2 <- list()
  if (has_multiple_groups) {
    row2[[1]] <- list(span = 1, text = "Group", borders = c(0, 0, 1, 0))
    row2[[2]] <- list(span = 1, text = time_label, borders = c(0, 0, 1, 0))
  } else {
    row2[[1]] <- list(span = 1, text = time_label, borders = c(0, 0, 1, 0))
  }

  # Add state names with spacers
  for (i in seq_along(strategies_display)) {
    row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
    for (state in states_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = state, borders = c(0, 0, 1, 0))
    }
  }
  headers[[2]] <- row2

  # Build column alignments and widths
  column_alignments <- character()
  column_widths <- numeric()

  # First columns
  if (has_multiple_groups) {
    column_alignments <- c("left", "left")
    column_widths <- c(NA, NA)
  } else {
    column_alignments <- "left"
    column_widths <- NA
  }

  # Add alignments/widths for strategy columns with spacers
  for (i in seq_along(strategies_display)) {
    column_alignments <- c(column_alignments, "center")  # spacer
    column_widths <- c(column_widths, 0.2)
    for (j in seq_len(n_states)) {
      column_alignments <- c(column_alignments, "right")
      column_widths <- c(column_widths, NA)
    }
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = trace_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format Trace as Table
#'
#' Creates a publication-quality table with clean black and white styling showing
#' state probabilities over time. When groups are present, they appear as the top-level
#' hierarchy above strategies, which are above states. Hierarchical headers are
#' separated by spacer columns for easy visual comparison.
#'
#' Supports both flextable and kableExtra backends for flexible output to Word,
#' PowerPoint, HTML, and PDF documents.
#'
#' @param results A heRomod2 model results object
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param states Character vector of states to include (NULL for all)
#' @param cycles Integer vector or range of cycles to display (NULL for all)
#' @param decimals Number of decimal places for probabilities (default: 4)
#' @param time_unit Which time unit to display: "cycle" (default), "day", "week",
#'   "month", or "year".
#' @param font_size Font size for the table (default: 11)
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
#' # Show all groups
#' trace_table(results, groups = NULL)
#'
#' # Show specific strategies and cycles
#' trace_table(results, strategies = c("Strategy1", "Strategy2"), cycles = 0:20)
#' }
#'
#' @export
trace_table <- function(results,
                        strategies = NULL,
                        groups = "overall",
                        states = NULL,
                        cycles = NULL,
                        decimals = 4,
                        time_unit = "cycle",
                        font_size = 11,
                        table_format = c("kable", "flextable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_trace_table_data(
    results = results,
    strategies = strategies,
    groups = groups,
    states = states,
    cycles = cycles,
    decimals = decimals,
    time_unit = time_unit,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
