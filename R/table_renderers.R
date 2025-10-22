#' Table Rendering Infrastructure
#'
#' Backend-agnostic table rendering system supporting flextable and kableExtra.
#' This module handles all styling, borders, and formatting after data preparation.
#'
#' @keywords internal
#'

#' Render Prepared Table Data Using Specified Backend
#'
#' Dispatcher function that routes prepared table data to the appropriate
#' backend renderer (flextable or kableExtra).
#'
#' @param prepared_data Simple table specification with headers, data, alignments, widths
#' @param format Character. One of "flextable" or "kable".
#'
#' @return Rendered table object (flextable or kable depending on format)
#'
#' @keywords internal
render_table <- function(prepared_data, format = c("flextable", "kable")) {
  format <- match.arg(format)

  if (format == "flextable") {
    render_flextable_simple(prepared_data)
  } else {
    render_kable_simple(prepared_data)
  }
}


#' Render Simple Table Spec as Flextable
#'
#' Clean renderer that iterates through spec and applies formatting.
#' No domain knowledge, no conditional logic for special cases.
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

  # Delete the auto-generated header from column names
  ft <- flextable::delete_part(ft, part = "header")

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

  # Handle spacer columns (width = 0.2)
  spacer_cols <- which(spec$column_widths == 0.2)
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
              # Use border() instead of hline() for intermediate header rows
              ft <- flextable::border(ft, i = row_idx, j = col,
                                      border.bottom = black_border, part = "header")
            }
          }
        }

        col_pos <- col_pos + cell$span
      }
    }
  }

  # Body bottom border (non-spacer columns only)
  non_spacer_cols <- which(is.na(spec$column_widths) | spec$column_widths != 0.2)
  last_row <- nrow(spec$data)
  if (last_row > 0 && length(non_spacer_cols) > 0) {
    # Apply bottom border to last row using border() function
    ft <- flextable::border(ft, i = last_row, j = non_spacer_cols,
                            border.bottom = black_border, part = "body")
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

  # Ensure spacer columns have no borders
  if (length(spacer_cols) > 0) {
    for (col in spacer_cols) {
      ft <- flextable::border(ft, i = NULL, j = col, border = no_border, part = "all")
    }
  }

  # Check if we have spacer columns
  spacer_cols <- which(spec$column_widths == 0.2)

  if (length(spacer_cols) == 0) {
    # No spacers - use autofit normally
    ft <- flextable::autofit(ft, add_w = 0, add_h = 0)
  } else {
    # Have spacers - skip autofit to preserve explicit widths
    # The widths were already set above in lines 82-86
    # autofit would override them, so we skip it when spacers are present
    NULL
  }

  ft
}


#' Render Simple Table Spec as Kable
#'
#' Clean renderer that converts spec to kableExtra HTML table.
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

  # Create base kable without column names
  kt <- knitr::kable(spec$data, format = "html", align = align_vec,
                     escape = FALSE, col.names = NULL)
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
        # Use Unicode non-breaking space for empty cells to prevent collapse
        # \u00A0 is the Unicode character for non-breaking space
        text <- if (cell$text == "") "\u00A0" else cell$text
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
    # Top border on first header row (for non-spacer columns only)
    non_spacer_cols <- which(is.na(spec$column_widths) | spec$column_widths != 0.2)

    # Build mapping from column index to TH element index for first row
    # (needed because first row has cells with different colspan values)
    col_to_th <- integer(length(spec$column_widths))
    th_idx <- 0
    col_idx <- 0

    for (cell in spec$headers[[1]]) {
      th_idx <- th_idx + 1  # TH element index
      for (span_i in seq_len(cell$span)) {
        col_idx <- col_idx + 1
        col_to_th[col_idx] <- th_idx
      }
    }

    # Apply top border only to non-spacer TH elements
    # Use unique() to avoid duplicate CSS rules for multi-column cells
    th_elements_to_border <- unique(col_to_th[non_spacer_cols])

    for (th_idx in th_elements_to_border) {
      css_lines <- c(css_lines,
        sprintf("thead tr:first-child th:nth-child(%d) {", th_idx),
        "  border-top: 1px solid #000000 !important;",
        "}")
    }

    # Add bottom border to strategy headers in first row
    # Strategy headers have colspan > 1 and non-empty text
    if (length(spec$headers) >= 1) {
      first_row <- spec$headers[[1]]
      col_pos <- 0
      for (cell_idx in seq_along(first_row)) {
        cell <- first_row[[cell_idx]]
        # Calculate which th element this is (accounting for colspan)
        th_index <- col_pos + 1

        # If this cell has a bottom border and non-empty text
        if (length(cell$borders) >= 3 && cell$borders[3] == 1 && cell$text != "") {
          # Add CSS to force bottom border on this specific th
          css_lines <- c(css_lines,
            sprintf("thead tr:first-child th:nth-child(%d) {", th_index),
            "  border-bottom: 1px solid #000000 !important;",
            "}")
        }

        col_pos <- col_pos + 1  # Move to next header position
      }
    }

    # Bottom border on last header row (for non-spacer columns only)
    for (col_idx in non_spacer_cols) {
      css_lines <- c(css_lines,
        sprintf("thead tr:last-child th:nth-child(%d) {", col_idx),
        "  border-bottom: 1px solid #000000 !important;",
        "}")
    }
  }

  # Body bottom border
  non_spacer_cols <- which(is.na(spec$column_widths) | spec$column_widths != 0.2)
  if (length(non_spacer_cols) > 0) {
    for (col_idx in non_spacer_cols) {
      css_lines <- c(css_lines,
        sprintf("tbody tr:last-child td:nth-child(%d) {", col_idx),
        "  border-bottom: 1px solid #000000 !important;",
        "}")
    }
  }

  # Spacer column CSS
  spacer_cols <- which(spec$column_widths == 0.2)
  for (col_idx in spacer_cols) {
    css_lines <- c(css_lines,
      sprintf("th:nth-child(%d), td:nth-child(%d) {", col_idx, col_idx),
      "  width: 0.2in !important;",
      "  min-width: 0.2in !important;",
      "  border-top: none !important;",
      "  border-right: none !important;",
      "  border-bottom: none !important;",
      "  border-left: none !important;",
      "  background: transparent !important;",
      "  padding: 0 !important;",
      "  empty-cells: show !important;",
      "}")

    # Apply column_spec for spacers
    kt <- kableExtra::column_spec(kt, column = col_idx, width = "0.2in",
                                  border_left = FALSE, border_right = FALSE,
                                  include_thead = TRUE,
                                  extra_css = "border: none !important; empty-cells: show !important; min-width: 0.2in !important;")
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

  # Apply column widths
  for (j in seq_along(spec$column_widths)) {
    if (!is.na(spec$column_widths[j]) && spec$column_widths[j] != 0.2) {
      kt <- kableExtra::column_spec(kt, column = j,
                                    width = paste0(spec$column_widths[j], "in"))
    }
  }

  # Inject CSS
  css_block <- paste0("<style>\n", paste(css_lines, collapse = "\n"), "\n</style>\n")
  html_str <- paste0(css_block, as.character(kt))

  # Post-process HTML to remove inline styles that override our CSS
  # Remove "empty-cells: hide;" from inline styles (causes spacer collapse)
  html_str <- gsub("empty-cells: hide;", "", html_str)

  # Remove "border-bottom:hidden;" from inline styles (prevents strategy header borders)
  html_str <- gsub("border-bottom:hidden;", "", html_str)

  class(html_str) <- c("kableExtra", "character")

  html_str
}


# ===== SIMPLE TABLE SPEC CREATION =====

#' Create Simple Table Specification
#'
#' Creates a clean table specification with just headers, data, and formatting.
#' No domain-specific knowledge required.
#'
#' @param headers List of header rows, each containing cells with span/text/borders
#' @param data data.frame with the actual table data
#' @param column_alignments Character vector of alignments
#' @param column_widths Numeric vector of widths (NA for auto, 0.2 for spacers)
#' @param special_rows List with optional total_row and bold_rows
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

  if (is.null(column_alignments)) {
    column_alignments <- rep("left", ncol(data))
  }

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

