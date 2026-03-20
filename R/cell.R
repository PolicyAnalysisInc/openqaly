#' Get or Set a Single Cell in a Data Frame by Key
#'
#' \code{cell_get()} retrieves the value of a single cell identified by one or
#' more key columns. \code{cell_set()} returns a modified copy of the data
#' frame with that cell replaced.
#'
#' @section Variable Promotion for Sensitivity Analyses:
#'
#' Model inputs often live inside tables (data frames). For example, a table
#' might store unit costs for several treatments, or mortality rates by age
#' band. Sensitivity analyses (PSA, DSA, scenarios, VBP) work by overriding
#' \emph{variables}, not individual table cells. To vary a single cell in a
#' table you need to \strong{promote} it into its own variable so the
#' analysis engine can reach it.
#'
#' The promotion pattern has three steps:
#'
#' \enumerate{
#'   \item \strong{Extract.}
#'     Create a variable whose formula uses \code{cell_get()} to pull the
#'     target cell out of the original table. For example, a variable called
#'     \code{drug_a_cost} with formula
#'     \code{cell_get(cost_table, drug = "A", col = "cost")} evaluates to the
#'     scalar value in that cell.
#'   \item \strong{Rebuild.}
#'     Create a second variable whose formula uses \code{cell_set()} to
#'     produce a copy of the table with the same cell replaced by the
#'     variable from step 1. For example, a variable called
#'     \code{cost_table_v} with formula
#'     \code{cell_set(cost_table, drug = "A", col = "cost",
#'     value = drug_a_cost)}.
#'     Because the rebuilt table references \code{drug_a_cost} rather than a
#'     hard-coded number, any change to that variable is automatically
#'     reflected in the table.
#'   \item \strong{Reference.}
#'     Use the rebuilt table (\code{cost_table_v}) — not the original — in
#'     all downstream formulas that need the value (e.g.\sspace{}inside
#'     \code{\link{look_up}()} calls or column references).
#' }
#'
#' With this wiring in place, overriding \code{drug_a_cost} — whether through
#' PSA sampling, a DSA range, a scenario override, or VBP — automatically
#' propagates into the rebuilt table and every calculation that depends on it.
#'
#' This pattern works identically whether the model is defined in R, JSON, or
#' YAML. In JSON/YAML the same formulas appear as strings in the variable
#' \code{formula} field:
#'
#' \preformatted{
#' variables:
#'   - name: drug_a_cost
#'     formula: 'cell_get(cost_table, drug = "A", col = "cost")'
#'   - name: cost_table_v
#'     formula: 'cell_set(cost_table, drug = "A", col = "cost",
#'               value = drug_a_cost)'
#'   - name: treatment_cost
#'     formula: 'look_up(cost_table_v, drug = current_drug,
#'               value = "cost")'
#' }
#'
#' @param x A data frame.
#' @param ... Named key-value pairs identifying a single row. Each name must
#'   match a column in \code{x} and each value must be length 1.
#' @param col A single character string naming the target column.
#' @param value (\code{cell_set} only) The replacement value for the matched
#'   cell.
#'
#' @return \code{cell_get()} returns the scalar value of the matched cell.
#'   \code{cell_set()} returns a modified copy of \code{x} with the matched
#'   cell replaced by \code{value}.
#'
#' @examples
#' # A small cost table
#' tbl <- data.frame(
#'   drug = c("A", "B"),
#'   cost = c(100, 200),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Extract a single cell
#' cell_get(tbl, drug = "A", col = "cost")
#' # 100
#'
#' # Replace a single cell (returns a modified copy)
#' cell_set(tbl, drug = "A", col = "cost", value = 999)
#' #   drug cost
#' # 1    A  999
#' # 2    B  200
#'
#' @name cell_access
NULL

#' @rdname cell_access
#' @export
cell_get <- function(x, ..., col) {
  # Catch == misuse (same pattern as look_up())
  call <- match.call(expand.dots = FALSE)
  dots <- call$...
  if (length(dots) > 0) {
    dot_names <- names(dots)
    if (is.null(dot_names)) dot_names <- rep("", length(dots))
    unnamed_indices <- which(dot_names == "")
    if (length(unnamed_indices) > 0) {
      is_equality_check <- vapply(dots[unnamed_indices], function(arg) {
        is.call(arg) && length(arg) == 3 && deparse(arg[[1]]) == "=="
      }, logical(1))
      if (any(is_equality_check)) {
        offending <- deparse(dots[unnamed_indices][is_equality_check][[1]])
        corrected <- gsub("==", "=", offending, fixed = TRUE)
        stop(
          "Use '=' instead of '==' to specify keys. Found ",
          sQuote(offending), ". Did you mean ", sQuote(corrected), "?",
          call. = FALSE
        )
      }
      stop("All keys in `...` must be named.", call. = FALSE)
    }
  }

  if (!is.character(col) || length(col) != 1L) {
    stop("`col` must be a single string.", call. = FALSE)
  }

  keys <- list(...)
  key_names <- names(keys)

  if (length(keys) == 0L) {
    stop("At least one key must be supplied in `...`.", call. = FALSE)
  }
  if (is.null(key_names) || any(key_names == "")) {
    stop("All keys in `...` must be named.", call. = FALSE)
  }

  cols <- c(key_names, col)
  ok <- cols %in% names(x)
  if (!all(ok)) {
    stop(
      sprintf("Unknown column(s): %s", paste(cols[!ok], collapse = ", ")),
      call. = FALSE
    )
  }

  hit <- rep_len(TRUE, nrow(x))

  for (i in seq_along(keys)) {
    val <- keys[[i]]
    if (length(val) != 1L) {
      stop(sprintf("Key `%s` must be length 1.", key_names[[i]]), call. = FALSE)
    }

    xi <- x[[key_names[[i]]]]
    hit <- hit & !is.na(xi) & (xi == val)
  }

  idx <- which(hit)

  if (length(idx) != 1L) {
    stop(sprintf("Expected exactly 1 matching row, found %d.", length(idx)), call. = FALSE)
  }

  x[[col]][[idx]]
}

#' @rdname cell_access
#' @export
cell_set <- function(x, ..., col, value) {
  # Catch == misuse (same pattern as look_up())
  call <- match.call(expand.dots = FALSE)
  dots <- call$...
  if (length(dots) > 0) {
    dot_names <- names(dots)
    if (is.null(dot_names)) dot_names <- rep("", length(dots))
    unnamed_indices <- which(dot_names == "")
    if (length(unnamed_indices) > 0) {
      is_equality_check <- vapply(dots[unnamed_indices], function(arg) {
        is.call(arg) && length(arg) == 3 && deparse(arg[[1]]) == "=="
      }, logical(1))
      if (any(is_equality_check)) {
        offending <- deparse(dots[unnamed_indices][is_equality_check][[1]])
        corrected <- gsub("==", "=", offending, fixed = TRUE)
        stop(
          "Use '=' instead of '==' to specify keys. Found ",
          sQuote(offending), ". Did you mean ", sQuote(corrected), "?",
          call. = FALSE
        )
      }
      stop("All keys in `...` must be named.", call. = FALSE)
    }
  }

  if (!is.character(col) || length(col) != 1L) {
    stop("`col` must be a single string.", call. = FALSE)
  }

  keys <- list(...)
  key_names <- names(keys)

  if (length(keys) == 0L) {
    stop("At least one key must be supplied in `...`.", call. = FALSE)
  }
  if (is.null(key_names) || any(key_names == "")) {
    stop("All keys in `...` must be named.", call. = FALSE)
  }

  cols <- c(key_names, col)
  ok <- cols %in% names(x)
  if (!all(ok)) {
    stop(
      sprintf("Unknown column(s): %s", paste(cols[!ok], collapse = ", ")),
      call. = FALSE
    )
  }

  hit <- rep_len(TRUE, nrow(x))

  for (i in seq_along(keys)) {
    val <- keys[[i]]
    if (length(val) != 1L) {
      stop(sprintf("Key `%s` must be length 1.", key_names[[i]]), call. = FALSE)
    }

    xi <- x[[key_names[[i]]]]
    hit <- hit & !is.na(xi) & (xi == val)
  }

  idx <- which(hit)

  if (length(idx) != 1L) {
    stop(sprintf("Expected exactly 1 matching row, found %d.", length(idx)), call. = FALSE)
  }

  x[[col]][idx] <- value
  x
}
