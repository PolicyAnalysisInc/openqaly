#' Prepare Transition Matrix Table Data
#'
#' Internal helper function that prepares transition matrix data for rendering.
#' Follows the same pattern as \code{prepare_trace_table_data()}.
#'
#' @param results A openqaly model results object
#' @param cycle Integer. Which cycle's transition matrix to display.
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param groups Group selection
#' @param decimals Number of decimal places for probabilities (default: 4)
#' @param font_size Font size for the table (default: 11)
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_transition_matrix_table_data <- function(results,
                                                  cycle = 1,
                                                  strategies = NULL,
                                                  groups = "overall",
                                                  decimals = 4,
                                                  font_size = 11) {

  # Get long format transition data
  trans_long <- get_transitions(
    results,
    format = "long",
    collapsed = TRUE,
    strategies = strategies,
    groups = groups,
    cycles = cycle,
    use_display_names = TRUE
  )

  # Get unique strategies, groups, and states
  strategies_display <- unique(trans_long$strategy)
  groups_display <- if ("group" %in% names(trans_long)) unique(trans_long$group) else NULL
  from_states_display <- unique(trans_long$from_state)
  to_states_display <- unique(trans_long$to_state)

  # Reorder groups
  if (!is.null(groups_display)) {
    groups_display <- get_group_order(groups_display, results$metadata)
  }

  # Preserve state order from metadata
  if (!is.null(results$metadata) && !is.null(results$metadata$states)) {
    state_levels <- results$metadata$states$display_name
    from_states_display <- state_levels[state_levels %in% from_states_display]
    to_states_display <- state_levels[state_levels %in% to_states_display]
  }

  n_to_states <- length(to_states_display)
  n_strategies <- length(strategies_display)
  n_groups <- if (!is.null(groups_display)) length(groups_display) else 0
  has_multiple_groups <- n_groups > 1

  # Build the data: pivot from_state x to_state into matrix form per strategy/group
  # Start by building result columns
  if (has_multiple_groups) {
    # Initialize with group column placeholder
    all_rows <- list()

    for (grp in groups_display) {
      grp_data <- trans_long[trans_long$group == grp, ]

      for (strat in strategies_display) {
        strat_data <- grp_data[grp_data$strategy == strat, ]

        # Create matrix: rows = from_state, cols = to_state
        for (fs in from_states_display) {
          row_data <- list()
          for (ts in to_states_display) {
            match_rows <- strat_data[strat_data$from_state == fs & strat_data$to_state == ts, ]
            if (nrow(match_rows) > 0) {
              row_data[[ts]] <- match_rows$probability[1]
            } else {
              row_data[[ts]] <- 0
            }
          }
          all_rows[[length(all_rows) + 1]] <- list(
            group = grp, strategy = strat, from_state = fs, values = row_data
          )
        }
      }
    }
  } else {
    all_rows <- list()

    for (strat in strategies_display) {
      strat_data <- trans_long[trans_long$strategy == strat, ]

      for (fs in from_states_display) {
        row_data <- list()
        for (ts in to_states_display) {
          match_rows <- strat_data[strat_data$from_state == fs & strat_data$to_state == ts, ]
          if (nrow(match_rows) > 0) {
            row_data[[ts]] <- match_rows$probability[1]
          } else {
            row_data[[ts]] <- 0
          }
        }
        all_rows[[length(all_rows) + 1]] <- list(
          strategy = strat, from_state = fs, values = row_data
        )
      }
    }
  }

  # Build the actual data frame with spacer columns between strategies
  result_cols <- NULL
  spacer_indices <- integer()
  col_counter <- 0

  # Add group column if needed
  if (has_multiple_groups) {
    group_vals <- sapply(all_rows, function(r) r$group)
    result_cols <- data.frame(Group = group_vals, stringsAsFactors = FALSE)
    col_counter <- 1
  }

  # Add from_state column
  from_vals <- sapply(all_rows, function(r) r$from_state)
  from_col <- data.frame(From = from_vals, stringsAsFactors = FALSE)
  if (is.null(result_cols)) {
    result_cols <- from_col
  } else {
    result_cols <- cbind(result_cols, from_col)
  }
  col_counter <- col_counter + 1

  # Add spacers and strategy value columns
  for (si in seq_along(strategies_display)) {
    strat <- strategies_display[si]
    col_counter <- col_counter + 1

    # Add spacer column
    spacer_col <- data.frame(rep("", length(all_rows)), stringsAsFactors = FALSE)
    names(spacer_col) <- paste0("spacer_", si)
    result_cols <- cbind(result_cols, spacer_col)
    spacer_indices <- c(spacer_indices, col_counter)

    # Add value columns for each to_state
    for (ts in to_states_display) {
      col_counter <- col_counter + 1
      vals <- sapply(all_rows, function(r) {
        if (r$strategy == strat) {
          r$values[[ts]]
        } else {
          NA
        }
      })

      val_col <- data.frame(x = vals, stringsAsFactors = FALSE)
      names(val_col) <- paste0(strat, "_", ts)
      result_cols <- cbind(result_cols, val_col)
    }
  }

  trace_data <- result_cols

  # Now we need to filter rows: each row should only contain data for its own strategy
  # Since we built rows per strategy, NAs appear in other strategy columns
  # We need to restructure: each from_state row should have values for ALL strategies

  # Restructure: one row per (group x from_state), all strategy columns filled
  if (has_multiple_groups) {
    id_cols <- c("Group", "From")
  } else {
    id_cols <- "From"
  }

  # Aggregate: for each unique (group, from_state), fill in all strategy columns
  unique_ids <- unique(trace_data[, id_cols, drop = FALSE])
  rebuilt_rows <- list()

  for (r in seq_len(nrow(unique_ids))) {
    row_match <- trace_data
    for (col in id_cols) {
      row_match <- row_match[row_match[[col]] == unique_ids[[col]][r], ]
    }

    # Combine non-NA values from matching rows
    combined <- unique_ids[r, , drop = FALSE]
    rownames(combined) <- NULL

    for (si in seq_along(strategies_display)) {
      strat <- strategies_display[si]
      spacer_name <- paste0("spacer_", si)
      combined[[spacer_name]] <- ""

      for (ts in to_states_display) {
        col_name <- paste0(strat, "_", ts)
        strat_rows <- row_match[!is.na(row_match[[col_name]]), ]
        if (nrow(strat_rows) > 0) {
          combined[[col_name]] <- strat_rows[[col_name]][1]
        } else {
          combined[[col_name]] <- 0
        }
      }
    }

    rebuilt_rows[[r]] <- combined
  }

  trace_data <- do.call(rbind, rebuilt_rows)
  rownames(trace_data) <- NULL

  # Format probability columns
  prob_cols <- setdiff(
    colnames(trace_data),
    c("Group", "From", grep("^spacer_", colnames(trace_data), value = TRUE))
  )
  for (col in prob_cols) {
    if (is.numeric(trace_data[[col]])) {
      rounded_vals <- round(trace_data[[col]], decimals)
      rounded_vals[abs(rounded_vals) < 10^(-decimals - 1)] <- 0
      trace_data[[col]] <- format(rounded_vals,
                                  nsmall = decimals,
                                  scientific = FALSE,
                                  trim = TRUE)
    }
  }

  # Build header structure
  headers <- list()

  # First header row - strategy names spanning to_state columns
  row1 <- list()
  if (has_multiple_groups) {
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
    row1[[2]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
  } else {
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
  }

  for (si in seq_along(strategies_display)) {
    row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))
    row1[[length(row1) + 1]] <- list(
      span = n_to_states,
      text = strategies_display[si],
      borders = c(1, 0, 1, 0)
    )
  }
  headers[[1]] <- row1

  # Second header row - "From" label and to_state names
  row2 <- list()
  if (has_multiple_groups) {
    row2[[1]] <- list(span = 1, text = "Group", borders = c(0, 0, 1, 0))
    row2[[2]] <- list(span = 1, text = "From", borders = c(0, 0, 1, 0))
  } else {
    row2[[1]] <- list(span = 1, text = "From", borders = c(0, 0, 1, 0))
  }

  for (si in seq_along(strategies_display)) {
    row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))
    for (ts in to_states_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = ts, borders = c(0, 0, 1, 0))
    }
  }
  headers[[2]] <- row2

  # Build column alignments and widths
  column_alignments <- character()
  column_widths <- numeric()

  if (has_multiple_groups) {
    column_alignments <- c("left", "left")
    column_widths <- c(NA, NA)
  } else {
    column_alignments <- "left"
    column_widths <- NA
  }

  for (si in seq_along(strategies_display)) {
    column_alignments <- c(column_alignments, "center")
    column_widths <- c(column_widths, 0.2)
    for (j in seq_len(n_to_states)) {
      column_alignments <- c(column_alignments, "right")
      column_widths <- c(column_widths, NA)
    }
  }

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


#' Format Transition Matrix as Table
#'
#' Creates a publication-quality table showing the transition probability matrix
#' for a specific cycle. Rows represent from-states and columns represent
#' to-states. When multiple strategies are present, they are separated by
#' spacer columns with hierarchical headers.
#'
#' @param results A openqaly model results object
#' @param cycle Integer. Which cycle's transition matrix to display (default: 1).
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param decimals Number of decimal places for probabilities (default: 4)
#' @param font_size Font size for the table (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Create transition matrix table for cycle 1
#' transition_matrix_table(results)
#'
#' # Show cycle 5 with kable backend
#' transition_matrix_table(results, cycle = 5, table_format = "kable")
#' }
#'
#' @export
transition_matrix_table <- function(results,
                                    cycle = 1,
                                    strategies = NULL,
                                    groups = "overall",
                                    decimals = 4,
                                    font_size = 11,
                                    table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  prepared <- prepare_transition_matrix_table_data(
    results = results,
    cycle = cycle,
    strategies = strategies,
    groups = groups,
    decimals = decimals,
    font_size = font_size
  )

  render_table(prepared, format = table_format)
}
