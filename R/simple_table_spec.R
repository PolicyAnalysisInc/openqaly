#' Simple Table Specification System
#'
#' A clean, generic table formatting system with no domain-specific knowledge.
#' Tables are defined by their header structure and data, nothing more.
#'
#' @keywords internal

#' Create a Simple Table Specification
#'
#' @param headers List of header rows, each containing cells with span/text/borders
#' @param data data.frame with the actual table data
#' @param column_alignments Character vector of alignments ("left", "center", "right")
#' @param column_widths Numeric vector of widths (NA for auto, 0.01 for spacer columns)
#' @param special_rows List with optional total_row and bold_rows indices
#' @param font_size Font size in points
#' @param font_family Font family name
#'
#' @return A simple table specification list
#' @keywords internal
create_simple_table_spec <- function(headers,
                                    data,
                                    column_alignments = NULL,
                                    column_widths = NULL,
                                    special_rows = list(),
                                    font_size = 11,
                                    font_family = "Helvetica") {

  # Default alignments if not provided
  if (is.null(column_alignments)) {
    column_alignments <- rep("left", ncol(data))
  }

  # Default widths if not provided
  if (is.null(column_widths)) {
    column_widths <- rep(NA, ncol(data))
  }

  list(
    headers = headers,
    data = data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size,
    font_family = font_family
  )
}
