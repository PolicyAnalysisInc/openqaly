# Helper functions for working with traces that include time columns

#' Extract only state columns from a trace
#'
#' Traces now include time columns (cycle, day, week, month, year) in addition
#' to state probability columns. This helper extracts only the state columns.
#'
#' @param trace A trace data frame or matrix
#' @return A data frame or matrix with only state columns
get_state_columns <- function(trace) {
  if (is.null(trace)) {
    return(NULL)
  }

  # If it's a matrix, it doesn't have time columns (legacy format)
  if (is.matrix(trace)) {
    return(trace)
  }

  # If it's a data frame, remove time columns
  if (is.data.frame(trace)) {
    time_cols <- c("cycle", "day", "week", "month", "year")
    state_cols <- setdiff(colnames(trace), time_cols)
    return(trace[, state_cols, drop = FALSE])
  }

  # Otherwise return as-is
  return(trace)
}

#' Get names of state columns from a trace
#'
#' @param trace A trace data frame or matrix
#' @return Character vector of state column names
get_state_column_names <- function(trace) {
  if (is.null(trace)) {
    return(character(0))
  }

  if (is.matrix(trace)) {
    return(colnames(trace))
  }

  if (is.data.frame(trace)) {
    time_cols <- c("cycle", "day", "week", "month", "year")
    return(setdiff(colnames(trace), time_cols))
  }

  return(colnames(trace))
}