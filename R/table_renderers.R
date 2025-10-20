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
#' @param prepared_data List with structure from prepare_*_table_data() functions.
#'   Must contain: data, table_type, mode, header_levels, spacer_indices,
#'   non_spacer_indices, decimals, and header_structure.
#'
#' @param format Character. One of "flextable" or "kable".
#'
#' @return Rendered table object (flextable or kable depending on format)
#'
#' @keywords internal
render_table <- function(prepared_data, format = c("flextable", "kable")) {
  format <- match.arg(format)

  if (format == "flextable") {
    render_table_flextable(prepared_data)
  } else if (format == "kable") {
    render_table_kable(prepared_data)
  } else {
    stop("Unknown format: ", format)
  }
}


#' Render Table Using flextable Backend
#'
#' Converts prepared table data into a publication-quality flextable with
#' hierarchical headers, borders, and proper formatting. Explicitly removes
#' all borders from spacer columns.
#'
#' @param prepared_data List from prepare_*_table_data()
#'
#' @return flextable object
#'
#' @details
#' This function handles:
#' - Creating flextable from data with spacer columns
#' - Formatting numeric columns with proper decimal places
#' - Building multi-level hierarchical headers
#' - Merging cells where appropriate (first column, strategy headers)
#' - Applying black borders to non-spacer columns only
#' - Explicitly removing ALL borders from spacer columns
#' - Styling special rows (e.g., TOTAL rows)
#' - Setting appropriate column widths and alignment
#'
#' CRITICAL: Spacer columns must have width=0.01in and NO borders on any side.
#'
#' @keywords internal
render_table_flextable <- function(prepared_data) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' required. Install with: install.packages('flextable')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' required. Install with: install.packages('officer')")
  }

  data <- prepared_data$data
  decimals <- prepared_data$decimals
  spacer_indices <- prepared_data$spacer_indices
  non_spacer_indices <- prepared_data$non_spacer_indices
  header_levels <- prepared_data$header_levels
  header_structure <- prepared_data$header_structure
  first_col_name <- prepared_data$first_col_name
  first_col_merge <- prepared_data$first_col_merge
  special_rows <- prepared_data$special_rows

  # Create base flextable
  ft <- flextable::flextable(data)

  # Set font to Helvetica for consistency
  ft <- flextable::font(ft, fontname = "Helvetica", part = "all")

  # Set font size
  ft <- flextable::fontsize(ft, size = prepared_data$font_size, part = "all")

  # Format numeric columns (value columns, not first column or spacers)
  all_cols <- colnames(data)
  value_col_indices <- setdiff(non_spacer_indices, 1)  # Exclude first column

  for (col_idx in value_col_indices) {
    col_name <- all_cols[col_idx]
    ft <- flextable::colformat_double(ft, j = col_idx, digits = decimals)
  }

  # ===== BUILD HEADERS =====
  # Add header rows based on number of levels (skip if header_levels = 0, use column names as-is)
  if (header_levels >= 3) {
    # Add level 3 (bottom) first when using add_header_row with top=TRUE
    ft <- flextable::add_header_row(
      ft,
      values = header_structure$level3$values,
      colwidths = header_structure$level3$widths,
      top = TRUE
    )
  }

  if (header_levels >= 2) {
    ft <- flextable::add_header_row(
      ft,
      values = header_structure$level2$values,
      colwidths = header_structure$level2$widths,
      top = TRUE
    )
  }

  if (header_levels >= 1) {
    ft <- flextable::add_header_row(
      ft,
      values = header_structure$level1$values,
      colwidths = header_structure$level1$widths,
      top = TRUE
    )
  }
  # If header_levels = 0, use default column name headers (no add_header_row calls)

  # For two-level headers (trace and outcomes with groups), use compose to set the bottom row
  # This shows group/state names instead of the full Strategy_Group column names
  if (header_levels == 1 && !is.na(header_structure$level2$values[1])) {
    # The bottom header row (row 2) should show the level2 values
    current_col <- 1
    for (val_idx in seq_along(header_structure$level2$values)) {
      if (header_structure$level2$widths[val_idx] > 0) {
        ft <- flextable::compose(
          ft,
          i = 2,
          j = current_col,
          value = flextable::as_paragraph(header_structure$level2$values[val_idx]),
          part = "header"
        )
        current_col <- current_col + header_structure$level2$widths[val_idx]
      }
    }
  }

  # Merge first column vertically if needed (only if header_levels > 0)
  if (first_col_merge && header_levels > 0) {
    ft <- flextable::merge_at(
      ft,
      i = 1:header_levels,
      j = 1,
      part = "header"
    )
  }

  # ===== APPLY BASIC STYLING =====
  ft <- ft %>%
    flextable::bg(bg = "white", part = "all") %>%
    flextable::bold(part = "header") %>%
    flextable::align(align = "center", part = "header")

  # ===== APPLY BORDERS =====
  # Start with clean slate - remove all borders
  ft <- flextable::border_remove(ft)

  # Define border objects
  black_border <- officer::fp_border(color = "black", width = 1)
  no_border <- officer::fp_border(width = 0)

  # Apply borders ONLY to non-spacer columns
  for (col_idx in non_spacer_indices) {
    # Top of header (row 1)
    ft <- flextable::hline_top(ft, j = col_idx, border = black_border, part = "header")

    # Internal header borders (between header rows)
    if (header_levels > 1) {
      for (level_row in 1:(header_levels - 1)) {
        ft <- flextable::hline(
          ft,
          i = level_row,
          j = col_idx,
          border = black_border,
          part = "header"
        )
      }
    }

    # Bottom of header
    ft <- flextable::hline_bottom(ft, j = col_idx, border = black_border, part = "header")

    # Bottom of body (last data row)
    ft <- flextable::hline_bottom(ft, j = col_idx, border = black_border, part = "body")
  }

  # ===== CRITICAL: REMOVE BORDERS FROM SPACER COLUMNS =====
  if (length(spacer_indices) > 0) {
    # Set spacer column width to minimal
    ft <- flextable::width(ft, j = spacer_indices, width = 0.01)

    # Void content (remove all text/formatting)
    ft <- flextable::void(ft, j = spacer_indices, part = "all")

    # EXPLICITLY set NO borders on all sides of spacer columns
    for (spacer_idx in spacer_indices) {
      ft <- flextable::border(
        ft,
        i = NULL,              # All rows (header and body)
        j = spacer_idx,        # This specific spacer column
        border = no_border,    # Zero-width border (transparent)
        part = "all"           # Apply to both header and body
      )
    }
  }

  # ===== SPECIAL ROW FORMATTING =====
  if (!is.null(special_rows$total_row_index)) {
    total_idx <- special_rows$total_row_index

    # Bold the total row
    ft <- flextable::bold(ft, i = total_idx, part = "body")

    # Add border above total row (on non-spacer columns only)
    ft <- flextable::hline(
      ft,
      i = total_idx - 1,
      j = non_spacer_indices,
      border = black_border,
      part = "body"
    )
  }

  # ===== AUTOFIT =====
  ft <- flextable::autofit(ft, add_w = 0, add_h = 0)

  ft
}


#' Render Table Using kableExtra Backend
#'
#' Converts prepared table data into an HTML table using kable/kableExtra with
#' hierarchical headers, borders, and proper formatting. Explicitly removes
#' borders from spacer columns using CSS.
#'
#' @param prepared_data List from prepare_*_table_data()
#'
#' @return kableExtra object (HTML table)
#'
#' @details
#' This function handles:
#' - Creating kable table with proper alignment and formatting
#' - Building multi-level hierarchical headers using add_header_above()
#' - Styling spacer columns with minimal width and no borders
#' - Applying CSS border rules to match flextable output
#' - Formatting total rows when present
#' - Using CSS injection to ensure spacer columns have no borders
#'
#' CRITICAL: Uses CSS selectors to ensure spacer columns have border:none
#' in the HTML output to match flextable's visual appearance.
#'
#' @keywords internal
render_table_kable <- function(prepared_data) {
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    stop("Package 'kableExtra' required. Install with: install.packages('kableExtra')")
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' required. Install with: install.packages('knitr')")
  }

  data <- prepared_data$data
  decimals <- prepared_data$decimals
  spacer_indices <- prepared_data$spacer_indices
  non_spacer_indices <- prepared_data$non_spacer_indices
  header_levels <- prepared_data$header_levels
  header_structure <- prepared_data$header_structure
  first_col_name <- prepared_data$first_col_name
  first_col_merge <- prepared_data$first_col_merge
  special_rows <- prepared_data$special_rows

  # Build alignment vector (left for first col, right for numbers, center for spacers)
  all_cols <- colnames(data)
  align_vec <- rep("c", length(all_cols))  # Default center

  # Identify numeric columns (all except first col and spacers)
  numeric_col_indices <- setdiff(non_spacer_indices, 1)
  align_vec[numeric_col_indices] <- "r"    # Right align for numeric columns
  align_vec[1] <- "l"                       # Left align first column
  align_vec[spacer_indices] <- "c"          # Center spacers (though they're empty)

  # Format numeric columns to specified decimal places BEFORE creating kable
  # This prevents kable from applying decimals to the first column
  data_formatted <- data
  for (col_idx in numeric_col_indices) {
    if (is.numeric(data_formatted[[col_idx]])) {
      # Round and fix -0.00 display issue
      rounded_vals <- round(data_formatted[[col_idx]], decimals)
      rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
      data_formatted[[col_idx]] <- format(rounded_vals,
                                           nsmall = decimals,
                                           scientific = FALSE)
    }
  }

  # Create base kable WITHOUT digits parameter (data is pre-formatted)
  kt <- knitr::kable(
    data_formatted,
    format = "html",
    align = align_vec,
    escape = FALSE
  )

  # Apply base styling (NO bootstrap borders - will use custom CSS)
  kt <- kableExtra::kable_styling(
    kt,
    bootstrap_options = c("condensed"),
    full_width = FALSE,
    position = "left"
  )

  # ===== BUILD HIERARCHICAL HEADERS =====
  # Explicit column name replacement based on metadata
  replace_column_names <- prepared_data$replace_column_names
  column_names <- prepared_data$column_names

  if (replace_column_names && !is.null(column_names)) {
    # Explicitly replace column names as instructed
    if (length(column_names) == ncol(data_formatted)) {
      colnames(data_formatted) <- column_names

      # Recreate kable with new column names (using pre-formatted data)
      kt <- knitr::kable(
        data_formatted,
        format = "html",
        align = align_vec,
        escape = FALSE
      )

      kt <- kableExtra::kable_styling(
        kt,
        bootstrap_options = c("condensed"),
        full_width = FALSE,
        position = "left"
      )
    }
  }

  # Only add headers if header_levels > 0 (skip for header_levels = 0, use column names)
  if (header_levels >= 1 && !is.na(header_structure$level1$header_vector[1])) {
    # Convert any "spacer_X" names to single space " " for kableExtra
    hdr <- header_structure$level1$header_vector
    names(hdr)[grepl("^spacer_", names(hdr))] <- " "
    kt <- kableExtra::add_header_above(kt, header = hdr, escape = FALSE)
  }

  if (header_levels >= 2 && !is.na(header_structure$level2$header_vector[1])) {
    hdr <- header_structure$level2$header_vector
    names(hdr)[grepl("^spacer_", names(hdr))] <- " "
    kt <- kableExtra::add_header_above(kt, header = hdr, escape = FALSE)
  }

  if (header_levels >= 3 && !is.na(header_structure$level3$header_vector[1])) {
    hdr <- header_structure$level3$header_vector
    names(hdr)[grepl("^spacer_", names(hdr))] <- " "
    kt <- kableExtra::add_header_above(kt, header = hdr, escape = FALSE)
  }

  # ===== ADD CUSTOM CSS FOR BORDERS (matches flextable) =====
  # CSS rules: borders on top/bottom, no spacer borders, bold/centered headers
  css_lines <- c(
    "table {",
    "  border-collapse: collapse;",
    "  background-color: white;",
    sprintf("  font-family: Helvetica, Arial, sans-serif; font-size: %dpt;", prepared_data$font_size),
    "}",
    "/* Remove ALL default borders first to prevent doubling */",
    ".table-bordered th,",
    ".table-bordered td,",
    ".table th,",
    ".table td {",
    "  border: none !important;",
    "}",
    "/* Top border on first header row */",
    "thead tr:first-child th {",
    "  border-top: 1px solid #000000 !important;",
    "}",
    "/* BLACK border between header rows (fixes gray border) */",
    "/* But NOT on first column which is merged */",
    "thead tr:nth-child(1) th:not(:first-child) {",
    "  border-bottom: 1px solid #000000 !important;",
    "}",
    "/* Bottom border on last header row */",
    "thead tr:last-child th {",
    "  border-bottom: 1px solid #000000 !important;",
    "}",
    "/* General header styling */",
    "thead th {",
    "  border-color: #000000 !important;",
    "  text-align: center;",
    "  font-weight: bold;",
    "}",
    "/* Remove other intermediate borders */",
    "thead tr:not(:first-child) th {",
    "  border-top: none !important;",
    "}",
    "/* No side borders */",
    "th, td {",
    "  border-left: none !important;",
    "  border-right: none !important;",
    "  font-family: Helvetica, Arial, sans-serif;",
    "}",
    "/* Body cells have no borders */",
    "tbody td {",
    "  border: none !important;",
    "}",
    "/* Last body row gets black bottom border */",
    "tbody tr:last-child td {",
    "  border-bottom: 1px solid #000000 !important;",
    "}"
  )

  # Add CSS for specific spacer column borders (remove all borders and background)
  if (length(spacer_indices) > 0) {
    for (idx in spacer_indices) {
      css_lines <- c(css_lines,
        sprintf("/* Remove ALL borders from spacer column %d */", idx),
        sprintf("thead tr th:nth-child(%d), tbody tr td:nth-child(%d) {", idx, idx),
        "  border: none !important;",
        "  border-top: none !important;",
        "  border-bottom: none !important;",
        "  border-left: none !important;",
        "  border-right: none !important;",
        "  background: transparent !important;",
        "}",
        sprintf("/* Ensure no borders at header-body boundary for spacer %d */", idx),
        sprintf("thead tr:last-child th:nth-child(%d) {", idx),
        "  border-bottom: none !important;",
        "}",
        sprintf("/* Ensure no borders at any header row for spacer %d */", idx),
        sprintf("thead tr:nth-child(1) th:nth-child(%d) {", idx),
        "  border-bottom: none !important;",
        "}",
        sprintf("/* Ensure no borders below total row for spacer %d */", idx),
        sprintf("tbody tr:nth-last-child(2) td:nth-child(%d) {", idx),
        "  border-bottom: none !important;",
        "}",
        sprintf("/* Ensure last row spacer has no border */"),
        sprintf("tbody tr:last-child td:nth-child(%d) {", idx),
        "  border-bottom: none !important;",
        "}"
      )
    }
  }

  # ===== STYLE SPECIAL ROWS (before any character conversion) =====
  if (!is.null(special_rows$total_row_index)) {
    total_idx <- special_rows$total_row_index

    # Bold total row
    kt <- kableExtra::row_spec(kt, row = total_idx, bold = TRUE)

    # Add CSS for border above total row (only on non-spacer columns)
    # We need to add this to the CSS instead of using row_spec
    # so that spacer columns don't get borders
    if (length(non_spacer_indices) > 0) {
      # Create CSS selector for each non-spacer column in the row above total
      total_border_css <- character()
      for (col_idx in non_spacer_indices) {
        total_border_css <- c(total_border_css,
          sprintf("tbody tr:nth-child(%d) td:nth-child(%d) {", total_idx - 1, col_idx),
          "  border-bottom: 1px solid #000000 !important;",
          "}"
        )
      }
      css_lines <- c(css_lines, total_border_css)
    }
  }

  # ===== APPLY SPACER COLUMN STYLING =====
  if (length(spacer_indices) > 0) {
    for (idx in spacer_indices) {
      kt <- kableExtra::column_spec(
        kt,
        column = idx,
        width = "0.01in",
        border_left = FALSE,
        border_right = FALSE,
        include_thead = TRUE
      )
    }
  }

  # ===== INJECT CUSTOM CSS =====
  # Combine all CSS rules and inject into HTML
  css_block <- paste0(
    "<style>\n",
    paste(css_lines, collapse = "\n"),
    "\n</style>\n"
  )
  html_str <- as.character(kt)
  html_str <- paste0(css_block, html_str)
  class(html_str) <- c("kableExtra", "character")
  kt <- html_str

  kt
}


# ===== HELPER FUNCTIONS FOR METADATA CREATION =====

#' Create Standard Metadata List for Table Rendering
#'
#' Helper function that creates the standard metadata structure returned by
#' prepare_*_table_data() functions.
#'
#' @param data DataFrame with pivoted data and spacer columns
#' @param table_type Character. One of "trace", "outcomes_cycle", "outcomes_summary",
#'   "nmb_cycle", "nmb_summary"
#' @param mode Character. One of "single", "two_level", "three_level"
#' @param n_strategies Integer
#' @param n_groups Integer (NULL if single group)
#' @param n_values Integer (NULL for trace tables)
#' @param n_states Integer (NULL for outcome tables)
#' @param strategy_names Character vector
#' @param group_names Character vector (NULL if single group)
#' @param value_names Character vector (NULL for trace tables)
#' @param state_names Character vector (NULL for outcome tables)
#' @param spacer_indices Integer vector of spacer column positions
#' @param first_col_name Character. Name of first column (e.g., "Cycle", "Component")
#' @param first_col_type Character. One of "time" or "label"
#' @param header_levels Integer. Number of header rows (1, 2, or 3)
#' @param header_structure List with level1, level2, level3 header specs
#' @param first_col_merge Logical. Should first column merge vertically?
#' @param special_rows List with total_row_index and bold_rows
#' @param decimals Integer. Number of decimal places
#' @param value_range Character. One of "probability", "currency", "general"
#'
#' @return List with all table metadata and data
#'
#' @keywords internal
create_table_metadata <- function(
    data,
    mode,
    n_strategies,
    n_groups = NULL,
    n_values = NULL,
    n_states = NULL,
    strategy_names,
    group_names = NULL,
    value_names = NULL,
    state_names = NULL,
    spacer_indices,
    first_col_name,
    first_col_type = "label",
    header_levels,
    header_structure,
    first_col_merge = FALSE,
    column_names = NULL,
    replace_column_names = FALSE,
    first_column_content = "",
    special_rows = list(total_row_index = NULL, bold_rows = integer()),
    decimals,
    font_size = 11,
    value_range = "general") {

  all_cols <- seq_along(colnames(data))
  non_spacer_indices <- setdiff(all_cols, spacer_indices)

  list(
    data = data,
    mode = mode,
    n_strategies = n_strategies,
    n_groups = n_groups,
    n_values = n_values,
    n_states = n_states,
    strategy_names = strategy_names,
    group_names = group_names,
    value_names = value_names,
    state_names = state_names,
    spacer_indices = spacer_indices,
    non_spacer_indices = non_spacer_indices,
    first_col_name = first_col_name,
    first_col_type = first_col_type,
    header_levels = header_levels,
    header_structure = header_structure,
    first_col_merge = first_col_merge,
    column_names = column_names,
    replace_column_names = replace_column_names,
    first_column_content = first_column_content,
    special_rows = special_rows,
    decimals = decimals,
    font_size = font_size,
    value_range = value_range
  )
}
