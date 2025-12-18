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


#' Render Simple Table Spec as Flextable
#'
#' @param spec A simple table specification
#' @return A flextable object
#' @keywords internal
render_flextable_simple <- function(spec) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' required. Install with: install.packages('flextable')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' required. Install with: install.packages('officer')")
  }

  # Create base flextable
  ft <- flextable::flextable(spec$data)

  # Apply basic formatting
  ft <- flextable::font(ft, fontname = spec$font_family, part = "all")
  ft <- flextable::fontsize(ft, size = spec$font_size, part = "all")
  ft <- flextable::bg(ft, bg = "white", part = "all")

  # Add header rows if specified
  if (length(spec$headers) > 0) {
    # Process headers from bottom to top for add_header_row
    for (i in rev(seq_along(spec$headers))) {
      header_row <- spec$headers[[i]]

      # Extract values and spans
      values <- sapply(header_row, function(cell) cell$text)
      spans <- sapply(header_row, function(cell) cell$span)

      ft <- flextable::add_header_row(ft, values = values, colwidths = spans, top = TRUE)
    }

    # Bold headers
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::align(ft, align = "center", part = "header")
  }

  # Apply column alignments
  for (j in seq_along(spec$column_alignments)) {
    ft <- flextable::align(ft, j = j, align = spec$column_alignments[j], part = "body")
  }

  # Apply column widths
  for (j in seq_along(spec$column_widths)) {
    if (!is.na(spec$column_widths[j])) {
      ft <- flextable::width(ft, j = j, width = spec$column_widths[j])
    }
  }

  # Handle spacer columns (width = 0.01)
  spacer_cols <- which(spec$column_widths == 0.01)
  if (length(spacer_cols) > 0) {
    ft <- flextable::void(ft, j = spacer_cols, part = "all")
  }

  # Apply borders
  ft <- flextable::border_remove(ft)

  black_border <- officer::fp_border(color = "black", width = 1)
  no_border <- officer::fp_border(width = 0)

  # Apply header borders based on specification
  if (length(spec$headers) > 0) {
    for (row_idx in seq_along(spec$headers)) {
      header_row <- spec$headers[[row_idx]]
      col_pos <- 0

      for (cell in header_row) {
        col_range <- (col_pos + 1):(col_pos + cell$span)

        # Skip if spacer column (no borders)
        if (all(cell$borders == 0)) {
          col_pos <- col_pos + cell$span
          next
        }

        # Apply borders: c(top, right, bottom, left)
        if (cell$borders[1] == 1) {  # Top
          for (col in col_range) {
            if (row_idx == 1) {
              ft <- flextable::hline_top(ft, j = col, border = black_border, part = "header")
            } else {
              ft <- flextable::hline(ft, i = row_idx - 1, j = col, border = black_border, part = "header")
            }
          }
        }

        if (cell$borders[3] == 1) {  # Bottom
          for (col in col_range) {
            if (row_idx == length(spec$headers)) {
              ft <- flextable::hline_bottom(ft, j = col, border = black_border, part = "header")
            } else {
              ft <- flextable::hline(ft, i = row_idx, j = col, border = black_border, part = "header")
            }
          }
        }

        col_pos <- col_pos + cell$span
      }
    }
  }

  # Body bottom border (non-spacer columns only)
  non_spacer_cols <- which(spec$column_widths != 0.01)
  if (nrow(spec$data) > 0 && length(non_spacer_cols) > 0) {
    ft <- flextable::hline_bottom(ft, j = non_spacer_cols, border = black_border, part = "body")
  }

  # Special rows
  if (!is.null(spec$special_rows$total_row)) {
    total_idx <- spec$special_rows$total_row
    ft <- flextable::bold(ft, i = total_idx, part = "body")

    # Add border above total row
    ft <- flextable::hline(ft, i = total_idx - 1, j = non_spacer_cols,
                          border = black_border, part = "body")
  }

  if (!is.null(spec$special_rows$bold_rows)) {
    for (row_idx in spec$special_rows$bold_rows) {
      ft <- flextable::bold(ft, i = row_idx, part = "body")
    }
  }

  # Group header rows (bold + italic for section headers)
  if (!is.null(spec$special_rows$group_header_rows)) {
    for (row_idx in spec$special_rows$group_header_rows) {
      ft <- flextable::bold(ft, i = row_idx, part = "body")
      ft <- flextable::italic(ft, i = row_idx, part = "body")
    }
  }

  # Indented rows (add left padding)
  if (!is.null(spec$special_rows$indented_rows)) {
    for (row_idx in spec$special_rows$indented_rows) {
      ft <- flextable::padding(ft, i = row_idx, j = 1, padding.left = 20, part = "body")
    }
  }

  # Ensure spacer columns have no borders
  if (length(spacer_cols) > 0) {
    for (col in spacer_cols) {
      ft <- flextable::border(ft, i = NULL, j = col, border = no_border, part = "all")
    }
  }

  ft <- flextable::autofit(ft, add_w = 0, add_h = 0)
  ft
}


#' Render Simple Table Spec as Kable
#'
#' @param spec A simple table specification
#' @return A kableExtra HTML table
#' @keywords internal
render_kable_simple <- function(spec) {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' required. Install with: install.packages('kableExtra')")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' required. Install with: install.packages('knitr')")
  }

  # Convert alignments to kable format
  align_vec <- substr(spec$column_alignments, 1, 1)  # "left" -> "l"

  # Create base kable
  kt <- knitr::kable(spec$data, format = "html", align = align_vec, escape = FALSE)
  kt <- kableExtra::kable_styling(kt, bootstrap_options = c("condensed"),
                                  full_width = FALSE, position = "left")

  # Add header rows
  if (length(spec$headers) > 0) {
    for (i in rev(seq_along(spec$headers))) {
      header_row <- spec$headers[[i]]

      # Build header vector for kableExtra
      header_vec <- integer()
      header_names <- character()

      for (cell in header_row) {
        # Use space for empty cells
        text <- if (cell$text == "") " " else cell$text
        header_names <- c(header_names, text)
        header_vec <- c(header_vec, cell$span)
      }
      names(header_vec) <- header_names

      kt <- kableExtra::add_header_above(kt, header = header_vec, escape = FALSE, line = FALSE)
    }
  }

  # Build CSS for styling
  css_lines <- c(
    "table { border-collapse: collapse; background-color: white;",
    sprintf("  font-family: %s, Arial, sans-serif; font-size: %dpt; }",
            spec$font_family, spec$font_size),
    "th, td { border: none !important; }",
    "thead th { text-align: center; font-weight: bold; }"
  )

  # Add border CSS based on header specification
  if (length(spec$headers) > 0) {
    # Top border on first header row (for non-spacer columns)
    non_spacer_cols <- which(spec$column_widths != 0.01)

    css_lines <- c(css_lines, "thead tr:first-child th {")
    css_lines <- c(css_lines, "  border-top: 1px solid #000000 !important;")
    css_lines <- c(css_lines, "}")

    # Bottom border on last header row
    css_lines <- c(css_lines, "thead tr:last-child th {")
    css_lines <- c(css_lines, "  border-bottom: 1px solid #000000 !important;")
    css_lines <- c(css_lines, "}")
  }

  # Body bottom border
  non_spacer_cols <- which(spec$column_widths != 0.01)
  if (length(non_spacer_cols) > 0) {
    for (col_idx in non_spacer_cols) {
      css_lines <- c(css_lines,
        sprintf("tbody tr:last-child td:nth-child(%d) {", col_idx),
        "  border-bottom: 1px solid #000000 !important;",
        "}")
    }
  }

  # Spacer column CSS
  spacer_cols <- which(spec$column_widths == 0.01)
  for (col_idx in spacer_cols) {
    css_lines <- c(css_lines,
      sprintf("th:nth-child(%d), td:nth-child(%d) {", col_idx, col_idx),
      "  width: 0.01in !important;",
      "  border: none !important;",
      "  background: transparent !important;",
      "  padding: 0 !important;",
      "}")

    # Apply column_spec for spacers
    kt <- kableExtra::column_spec(kt, column = col_idx, width = "0.01in",
                                  border_left = FALSE, border_right = FALSE,
                                  include_thead = TRUE,
                                  extra_css = "border: none !important;")
  }

  # Special rows CSS
  if (!is.null(spec$special_rows$total_row)) {
    total_idx <- spec$special_rows$total_row

    # Bold total row
    kt <- kableExtra::row_spec(kt, row = total_idx, bold = TRUE)

    # Border above total row
    for (col_idx in non_spacer_cols) {
      css_lines <- c(css_lines,
        sprintf("tbody tr:nth-child(%d) td:nth-child(%d) {", total_idx - 1, col_idx),
        "  border-bottom: 1px solid #000000 !important;",
        "}")
    }
  }

  # Group header rows (bold + italic for section headers)
  if (!is.null(spec$special_rows$group_header_rows)) {
    for (row_idx in spec$special_rows$group_header_rows) {
      kt <- kableExtra::row_spec(kt, row = row_idx, bold = TRUE, italic = TRUE)
    }
  }

  # Indented rows (add left padding via CSS)
  if (!is.null(spec$special_rows$indented_rows)) {
    for (row_idx in spec$special_rows$indented_rows) {
      # Add CSS for left padding in first column
      css_lines <- c(css_lines,
        sprintf("tbody tr:nth-child(%d) td:nth-child(1) {", row_idx),
        "  padding-left: 20px !important;",
        "}")
    }
  }

  # Apply column widths
  for (j in seq_along(spec$column_widths)) {
    if (!is.na(spec$column_widths[j]) && spec$column_widths[j] != 0.01) {
      kt <- kableExtra::column_spec(kt, column = j,
                                    width = paste0(spec$column_widths[j], "in"))
    }
  }

  # Inject CSS
  css_block <- paste0("<style>\n", paste(css_lines, collapse = "\n"), "\n</style>\n")
  html_str <- paste0(css_block, as.character(kt))
  class(html_str) <- c("kableExtra", "character")

  html_str
}