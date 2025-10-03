#' Format Trace as flextable
#'
#' Creates a publication-quality table using flextable with clean black and white
#' styling. Strategies appear as column groups with hierarchical headers separated
#' by spacer columns for easy visual comparison. Ideal for export to Word, PowerPoint,
#' or HTML documents.
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
#'
#' @return A flextable object with hierarchical column headers and clean borders
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Create flextable with hierarchical headers
#' ft <- format_trace_flextable(results)
#'
#' # Save to Word
#' flextable::save_as_docx(ft, path = "trace.docx")
#'
#' # Save to PowerPoint
#' flextable::save_as_pptx(ft, path = "trace.pptx")
#'
#' # Show specific strategies and cycles
#' format_trace_flextable(results, strategies = c("Strategy1", "Strategy2"), cycles = 0:20)
#' }
#'
#' @export
format_trace_flextable <- function(results,
                                    strategies = NULL,
                                    states = NULL,
                                    cycles = NULL,
                                    decimals = 4,
                                    strategy_name_field = "display_name",
                                    state_name_field = "display_name",
                                    time_unit = "cycle") {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' is required. Please install it with: install.packages('flextable')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' is required. Please install it with: install.packages('officer')")
  }

  # Get long format trace data with specified time unit (always use collapsed traces)
  trace_long <- get_trace(results, format = "long", collapsed = TRUE,
                          strategies = strategies, states = states, cycles = cycles,
                          time_unit = time_unit)

  # Get unique strategies and states
  strategies_unique <- unique(trace_long$strategy)
  states_unique <- unique(trace_long$state)
  n_states <- length(states_unique)
  n_strategies <- length(strategies_unique)

  # Map names for display using metadata if available
  strategies_display <- if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
    map_names(strategies_unique, results$metadata$strategies, strategy_name_field)
  } else {
    strategies_unique
  }

  states_display <- if (!is.null(results$metadata) && !is.null(results$metadata$states)) {
    map_names(states_unique, results$metadata$states, state_name_field)
  } else {
    states_unique
  }

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
  # Keep only: strategy, state, probability, and the selected time column
  trace_for_pivot <- trace_long %>%
    dplyr::select(all_of(c(time_col_name, "strategy", "state", "probability")))

  # Pivot wider: strategies and states become columns
  trace_data <- trace_for_pivot %>%
    tidyr::pivot_wider(
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

  # Add dummy spacer columns between strategy groups (and before first group)
  if (n_strategies >= 1) {
    # Get the time column (could be cycle, day, week, month, or year)
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

    for (i in seq_along(strategies_unique)) {
      # Add spacer column BEFORE each strategy group
      spacer_col <- data.frame(rep("", nrow(trace_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)

      # Get columns for this strategy
      strat <- strategies_unique[i]
      strat_cols <- trace_data[, grepl(paste0("^", strat, "_"), names(trace_data)), drop = FALSE]
      result_cols <- cbind(result_cols, strat_cols)
    }

    trace_data <- result_cols
  }

  # Rename time column to proper label BEFORE creating flextable
  names(trace_data)[names(trace_data) == time_col_name] <- time_label

  # Intelligent rounding for time column
  # Round to tenths place, but if all values are effectively integers, show as integers
  time_values <- trace_data[[time_label]]
  rounded_values <- round(time_values, 1)  # Round to tenths

  # Check if all rounded values are effectively integers (diff from nearest int < 0.01)
  if (all(abs(rounded_values - round(rounded_values)) < 0.01)) {
    # All values are effectively integers, so display as integers
    trace_data[[time_label]] <- as.integer(round(rounded_values))
  } else {
    # Some values have decimals, so keep the tenths place
    trace_data[[time_label]] <- rounded_values
  }

  # Create flextable
  ft <- flextable::flextable(trace_data)

  # Format probability columns (exclude time column and spacers)
  prob_cols <- setdiff(colnames(trace_data), c(time_label, grep("^spacer_", colnames(trace_data), value = TRUE)))
  for (col in prob_cols) {
    ft <- flextable::colformat_double(ft, j = col, digits = decimals)
  }

  # Get column indices BEFORE using them
  all_cols <- colnames(trace_data)
  cycle_col_idx <- which(all_cols == time_label)
  spacer_col_indices <- which(grepl("^spacer_", all_cols))
  non_spacer_col_indices <- setdiff(seq_along(all_cols), spacer_col_indices)

  # Build header row for strategies with spacers
  # Row 1 (top): empty cell over time column | empty over spacers | Strategy names spanning states
  header_row1_values <- c(time_label)  # Put time label in the first cell so it appears correctly
  header_row1_widths <- c(1)

  for (i in seq_along(strategies_unique)) {
    # Add empty cell over spacer BEFORE each strategy
    header_row1_values <- c(header_row1_values, "")
    header_row1_widths <- c(header_row1_widths, 1)

    # Add strategy name spanning states (use display name)
    header_row1_values <- c(header_row1_values, strategies_display[i])
    header_row1_widths <- c(header_row1_widths, n_states)
  }

  # Add the top header row
  ft <- ft %>%
    flextable::add_header_row(
      values = header_row1_values,
      colwidths = header_row1_widths,
      top = TRUE
    )

  # Now we need to merge "Cycle" vertically to span both header rows
  ft <- flextable::merge_at(
    ft,
    i = 1:2,  # Merge both header rows
    j = 1,    # Column 1 (Cycle)
    part = "header"
  )

  # Set the proper state names in row 2 header (without strategy prefix)
  current_col <- 2  # Start after Cycle column
  for (i in seq_along(strategies_unique)) {
    # Skip spacer column - set it to empty
    ft <- flextable::compose(ft,
                              i = 2,  # Row 2 of header
                              j = current_col,
                              value = flextable::as_paragraph(""),
                              part = "header")
    current_col <- current_col + 1

    # Set state names for this strategy (use display names)
    for (j in seq_along(states_unique)) {
      ft <- flextable::compose(ft,
                                i = 2,  # Row 2 of header
                                j = current_col,
                                value = flextable::as_paragraph(states_display[j]),
                                part = "header")
      current_col <- current_col + 1
    }
  }

  # Now we have:
  # Header row 1: "" | Strategy1 | "" | Strategy2 | ...
  # Header row 2: "Cycle" | state1 | state2 | ... | "" | state1 | state2 | ...

  # Set white background first
  ft <- ft %>%
    flextable::bg(bg = "white", part = "all")

  # Bold the header text
  ft <- ft %>%
    flextable::bold(part = "header")

  # Center-align all column headers
  ft <- ft %>%
    flextable::align(align = "center", part = "header")

  # Remove ALL borders to start with a clean slate
  ft <- flextable::border_remove(ft)

  # Define borders
  black_border <- officer::fp_border(color = "black", width = 1)
  no_border <- officer::fp_border(width = 0)


  # Apply borders ONLY to non-spacer columns, one by one to ensure precision
  for (col_idx in non_spacer_col_indices) {
    # 1. Top border of header (top of row 1)
    ft <- flextable::hline_top(ft, j = col_idx, border = black_border, part = "header")

    # 2. Border between header row 1 and header row 2 (bottom of row 1)
    ft <- flextable::hline(ft, i = 1, j = col_idx, border = black_border, part = "header")

    # 3. Border between header and body (bottom of header row 2)
    ft <- flextable::hline_bottom(ft, j = col_idx, border = black_border, part = "header")

    # 4. Bottom border of table
    ft <- flextable::hline_bottom(ft, j = col_idx, border = black_border, part = "body")
  }

  # Style spacer columns AFTER applying other borders
  if (length(spacer_col_indices) > 0) {
    # Set width first
    ft <- flextable::width(ft, j = spacer_col_indices, width = 0.01)

    # Use delete() to completely remove spacer columns, then add them back empty
    # Actually, let's use a different approach - set them to empty and remove all formatting

    # First void the columns to remove content
    ft <- flextable::void(ft, j = spacer_col_indices, part = "all")

    # Now explicitly set borders to none using border_remove on specific columns
    # We need to use the border() function with all borders set to no_border
    for (spacer_idx in spacer_col_indices) {
      ft <- flextable::border(ft,
                              i = NULL,  # All rows
                              j = spacer_idx,
                              border = no_border,
                              part = "all")
    }
  }

  # Auto-fit NON-SPACER columns only
  non_spacer_col_names <- all_cols[non_spacer_col_indices]
  for (col_name in non_spacer_col_names) {
    ft <- flextable::autofit(ft, add_w = 0, add_h = 0)  # Autofit but don't add extra width
    break  # Only need to call autofit once
  }

  ft
}
