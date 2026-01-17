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

  # Helper to decode common HTML entities for flextable
  decode_html_entities <- function(text) {
    # Decode numeric entities like &#916; (Delta)
    # Find all numeric entities
    matches <- gregexpr("&#(\\d+);", text, perl = TRUE)
    if (matches[[1]][1] != -1) {
      # Extract the numeric codes
      match_data <- regmatches(text, matches)[[1]]
      for (entity in match_data) {
        code <- as.integer(sub("&#(\\d+);", "\\1", entity, perl = TRUE))
        char <- intToUtf8(code)
        text <- sub(entity, char, text, fixed = TRUE)
      }
    }
    text
  }

  # Add header rows if specified
  if (length(spec$headers) > 0) {
    # Process headers from bottom to top for add_header_row
    for (i in rev(seq_along(spec$headers))) {
      header_row <- spec$headers[[i]]

      # Extract values and spans, decode HTML entities
      values <- sapply(header_row, function(cell) decode_html_entities(cell$text))
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

  # Group header rows
  if (!is.null(spec$special_rows$group_header_rows)) {
    for (row_idx in spec$special_rows$group_header_rows) {
      ft <- flextable::bold(ft, i = row_idx, part = "body")
      ft <- flextable::italic(ft, i = row_idx, part = "body")
    }
  }

  # Group boundary rows - add border above
  if (!is.null(spec$special_rows$group_boundary_rows)) {
    for (row_idx in spec$special_rows$group_boundary_rows) {
      ft <- flextable::hline(ft, i = row_idx - 1, j = non_spacer_cols,
                            border = black_border, part = "body")
    }
  }

  # Indented rows (add left padding)
  if (!is.null(spec$special_rows$indented_rows) && length(spec$special_rows$indented_rows) > 0) {
    ft <- flextable::padding(ft, i = spec$special_rows$indented_rows, j = 1,
                             padding.left = 20, part = "body")
    # Left-align first column header to match indented content
    ft <- flextable::align(ft, j = 1, align = "left", part = "header")
  }

  # Footnote
  if (!is.null(spec$special_rows$footnote)) {
    # Add footnote below the table
    ft <- flextable::add_footer_lines(ft, values = spec$special_rows$footnote)
    ft <- flextable::align(ft, align = "left", part = "footer")
    ft <- flextable::fontsize(ft, size = spec$font_size - 1, part = "footer")
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

  # Fix double borders in html_vignette output
  ft <- flextable::set_table_properties(
    ft,
    opts_html = list(extra_css = "table { border-collapse: collapse; }")
  )

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
                                  full_width = TRUE, position = "left")

  # Identify spacer columns
  spacer_cols <- which(spec$column_widths == 0.2)

  # Add header rows with appropriate line settings
  if (length(spec$headers) > 0) {
    for (i in rev(seq_along(spec$headers))) {
      header_row <- spec$headers[[i]]

      # Build header vector for kableExtra
      header_vec <- integer()
      header_names <- character()

      for (cell in header_row) {
        # Use Unicode non-breaking space for empty cells to prevent collapse
        text <- if (cell$text == "") "\u00A0" else cell$text
        header_names <- c(header_names, text)
        header_vec <- c(header_vec, cell$span)
      }
      names(header_vec) <- header_names

      # No lines on headers
      kt <- kableExtra::add_header_above(kt, header = header_vec,
                                         escape = FALSE, line = FALSE)
    }
  }

  # Apply spacer column specs - no borders, double width
  for (col_idx in spacer_cols) {
    kt <- kableExtra::column_spec(kt, column = col_idx,
                                  width = "0.4in",
                                  border_left = FALSE,
                                  border_right = FALSE,
                                  include_thead = TRUE)
  }

  # Apply non-spacer column widths
  for (j in seq_along(spec$column_widths)) {
    if (!is.na(spec$column_widths[j]) && !(j %in% spacer_cols)) {
      kt <- kableExtra::column_spec(kt, column = j,
                                    width = paste0(spec$column_widths[j], "in"),
                                    include_thead = TRUE)
    }
  }

  # Set minimum width on non-spacer columns
  non_spacer_cols <- setdiff(seq_along(spec$column_widths), spacer_cols)
  for (col_idx in non_spacer_cols) {
    kt <- kableExtra::column_spec(kt, column = col_idx,
                                  width = "0.4in",
                                  include_thead = TRUE)
  }

  # Special rows
  if (!is.null(spec$special_rows$total_row)) {
    total_idx <- spec$special_rows$total_row

    # Bold total row
    kt <- kableExtra::row_spec(kt, row = total_idx, bold = TRUE)
  }

  # Group header rows
  if (!is.null(spec$special_rows$group_header_rows)) {
    for (row_idx in spec$special_rows$group_header_rows) {
      kt <- kableExtra::row_spec(kt, row = row_idx, bold = TRUE, italic = TRUE)
    }
  }

  # Group boundary rows - add border above
  if (!is.null(spec$special_rows$group_boundary_rows)) {
    for (row_idx in spec$special_rows$group_boundary_rows) {
      kt <- kableExtra::row_spec(kt, row = row_idx, extra_css = "border-top: 1px solid black;")
    }
  }

  # Indented rows - add left padding to first column
  if (!is.null(spec$special_rows$indented_rows)) {
    for (row_idx in spec$special_rows$indented_rows) {
      kt <- kableExtra::row_spec(kt, row = row_idx, extra_css = "td:first-child { padding-left: 20px; }")
    }
  }

  # Footnote
  if (!is.null(spec$special_rows$footnote)) {
    kt <- kableExtra::footnote(kt, general = spec$special_rows$footnote,
                               general_title = "",
                               footnote_as_chunk = FALSE)
  }

  # Build spacer CSS with min/max width
  spacer_css <- ""
  for (col_idx in spacer_cols) {
    spacer_css <- paste0(spacer_css,
      "th:nth-child(", col_idx, "), td:nth-child(", col_idx, ") { ",
      "min-width: 0.2in !important; max-width: 0.2in !important; }")
  }

  # Add CSS for header styling
  css_rule <- paste0(
    "<style>",
    "thead th { vertical-align: bottom; } ",
    "thead tr:last-child th { border-bottom: 1px solid black !important; }",
    spacer_css,
    "</style>"
  )
  html_str <- paste0(css_rule, as.character(kt))
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

