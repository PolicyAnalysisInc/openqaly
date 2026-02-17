#' Threshold Analysis Visualization - Tables
#'
#' Table functions for threshold analysis results.
#'
#' @name threshold_tables
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
NULL

# ============================================================================
# Table Functions
# ============================================================================

#' Threshold Table
#'
#' Tabular display of threshold analysis data showing input parameter values,
#' output metric values, goal, and difference. Data is sorted by input value.
#'
#' @param results Threshold results from run_threshold()
#' @param analyses Optional character vector of analysis names to include
#' @param decimals Number of decimal places for formatting (default: 4)
#' @param font_size Font size in points (default: 11)
#' @param table_format Output format: "flextable" or "kable" (default: "flextable")
#' @return A flextable or kable object
#' @export
threshold_table <- function(results, analyses = NULL, decimals = 4,
                            font_size = 11,
                            table_format = c("flextable", "kable")) {
  table_format <- match.arg(table_format)
  data <- prepare_threshold_history_data(results, analyses)
  history <- data$history
  specs <- data$analyses

  if (nrow(history) == 0) {
    stop("No history data available for the selected analyses", call. = FALSE)
  }

  analysis_names <- unique(history$name)
  multi_analysis <- length(analysis_names) > 1

  # Build labels
  if (!multi_analysis && length(specs) == 1) {
    input_label <- get_threshold_input_label(specs[[1]], results$metadata)
    output_label <- as.character(get_threshold_output_label(specs[[1]], results$metadata))
  } else {
    input_label <- "Variable"
    output_label <- "Output"
  }

  fmt <- function(x) formatC(x, format = "f", digits = decimals, big.mark = ",")

  # Determine convergence status per analysis
  tv <- data$threshold_values
  failed_names <- tv$name[is.na(tv$value)]

  if (!multi_analysis) {
    # Single analysis — simple table sorted by input
    h <- history %>% arrange(.data$input)

    table_data <- data.frame(
      col1 = fmt(h$input),
      col2 = fmt(h$output),
      col3 = fmt(h$goal),
      col4 = fmt(h$diff),
      stringsAsFactors = FALSE
    )
    names(table_data) <- c("col1", "col2", "col3", "col4")

    # Add "Did not converge" footer row for failed analyses
    if (analysis_names[1] %in% failed_names) {
      footer_row <- data.frame(
        col1 = "Did not converge", col2 = "", col3 = "", col4 = "",
        stringsAsFactors = FALSE
      )
      table_data <- rbind(table_data, footer_row)
    }

    headers <- list(
      list(
        list(text = input_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = output_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Goal", span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Difference", span = 1, borders = c(1, 0, 1, 0))
      )
    )

    column_alignments <- rep("right", 4)
    special_rows <- list()

  } else {
    # Multi-analysis — group header rows
    table_data <- data.frame(
      col1 = character(), col2 = character(),
      col3 = character(), col4 = character(),
      stringsAsFactors = FALSE
    )

    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (analysis_name in analysis_names) {
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Add convergence status to group header for failed analyses
      header_text <- analysis_name
      if (analysis_name %in% failed_names) {
        header_text <- paste0(analysis_name, " (Did not converge)")
      }

      group_row <- data.frame(
        col1 = header_text, col2 = "", col3 = "", col4 = "",
        stringsAsFactors = FALSE
      )
      table_data <- rbind(table_data, group_row)

      h <- history[history$name == analysis_name, ] %>% arrange(.data$input)
      n_rows <- nrow(h)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_rows))

      data_rows <- data.frame(
        col1 = fmt(h$input),
        col2 = fmt(h$output),
        col3 = fmt(h$goal),
        col4 = fmt(h$diff),
        stringsAsFactors = FALSE
      )
      table_data <- rbind(table_data, data_rows)
      current_row <- current_row + n_rows
    }

    headers <- list(
      list(
        list(text = input_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = output_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Goal", span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Difference", span = 1, borders = c(1, 0, 1, 0))
      )
    )

    column_alignments <- rep("right", 4)
    group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
  }

  spec <- create_simple_table_spec(
    headers = headers,
    data = table_data,
    column_alignments = column_alignments,
    column_widths = rep(NA, 4),
    special_rows = special_rows,
    font_size = font_size
  )

  render_table(spec, format = table_format)
}


#' Threshold Convergence Table
#'
#' Tabular display of solver convergence data showing iteration number,
#' input value, output value, goal, and difference. Data is sorted by iteration.
#'
#' @param results Threshold results from run_threshold()
#' @param analyses Optional character vector of analysis names to include
#' @param decimals Number of decimal places for formatting (default: 4)
#' @param font_size Font size in points (default: 11)
#' @param table_format Output format: "flextable" or "kable" (default: "flextable")
#' @return A flextable or kable object
#' @export
threshold_convergence_table <- function(results, analyses = NULL, decimals = 4,
                                        font_size = 11,
                                        table_format = c("flextable", "kable")) {
  table_format <- match.arg(table_format)
  data <- prepare_threshold_history_data(results, analyses)
  history <- data$history
  specs <- data$analyses

  if (nrow(history) == 0) {
    stop("No history data available for the selected analyses", call. = FALSE)
  }

  analysis_names <- unique(history$name)
  multi_analysis <- length(analysis_names) > 1

  # Build labels
  if (!multi_analysis && length(specs) == 1) {
    input_label <- get_threshold_input_label(specs[[1]], results$metadata)
    output_label <- as.character(get_threshold_output_label(specs[[1]], results$metadata))
  } else {
    input_label <- "Variable"
    output_label <- "Output"
  }

  fmt <- function(x) formatC(x, format = "f", digits = decimals, big.mark = ",")

  # Determine convergence status per analysis
  tv <- data$threshold_values
  failed_names <- tv$name[is.na(tv$value)]

  if (!multi_analysis) {
    # Single analysis — simple table sorted by iteration
    h <- history %>% arrange(.data$iteration)

    # Determine convergence per iteration: only the final iteration is marked
    # as converged (if the analysis actually converged)
    converged <- !(analysis_names[1] %in% failed_names)
    converged_col <- rep("FALSE", nrow(h))
    if (converged && nrow(h) > 0) {
      converged_col[nrow(h)] <- "TRUE"
    }

    table_data <- data.frame(
      col1 = as.character(h$iteration),
      col2 = fmt(h$input),
      col3 = fmt(h$output),
      col4 = fmt(h$goal),
      col5 = fmt(h$diff),
      col6 = converged_col,
      stringsAsFactors = FALSE
    )

    headers <- list(
      list(
        list(text = "Iteration", span = 1, borders = c(1, 0, 1, 0)),
        list(text = input_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = output_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Goal", span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Difference", span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Converged", span = 1, borders = c(1, 0, 1, 0))
      )
    )

    column_alignments <- c("right", "right", "right", "right", "right", "center")
    special_rows <- list()

  } else {
    # Multi-analysis — group header rows
    table_data <- data.frame(
      col1 = character(), col2 = character(),
      col3 = character(), col4 = character(), col5 = character(),
      col6 = character(),
      stringsAsFactors = FALSE
    )

    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (analysis_name in analysis_names) {
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Determine convergence per iteration for this analysis
      converged <- !(analysis_name %in% failed_names)

      group_row <- data.frame(
        col1 = analysis_name, col2 = "", col3 = "", col4 = "", col5 = "",
        col6 = "",
        stringsAsFactors = FALSE
      )
      table_data <- rbind(table_data, group_row)

      h <- history[history$name == analysis_name, ] %>% arrange(.data$iteration)
      n_rows <- nrow(h)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_rows))

      converged_col <- rep("FALSE", n_rows)
      if (converged && n_rows > 0) {
        converged_col[n_rows] <- "TRUE"
      }

      data_rows <- data.frame(
        col1 = as.character(h$iteration),
        col2 = fmt(h$input),
        col3 = fmt(h$output),
        col4 = fmt(h$goal),
        col5 = fmt(h$diff),
        col6 = converged_col,
        stringsAsFactors = FALSE
      )
      table_data <- rbind(table_data, data_rows)
      current_row <- current_row + n_rows
    }

    headers <- list(
      list(
        list(text = "Iteration", span = 1, borders = c(1, 0, 1, 0)),
        list(text = input_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = output_label, span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Goal", span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Difference", span = 1, borders = c(1, 0, 1, 0)),
        list(text = "Converged", span = 1, borders = c(1, 0, 1, 0))
      )
    )

    column_alignments <- c(rep("right", 5), "center")
    group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
  }

  n_cols <- ncol(table_data)
  spec <- create_simple_table_spec(
    headers = headers,
    data = table_data,
    column_alignments = column_alignments,
    column_widths = rep(NA, n_cols),
    special_rows = special_rows,
    font_size = font_size
  )

  render_table(spec, format = table_format)
}
