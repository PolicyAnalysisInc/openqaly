#' Prepare Net Monetary Benefit Table Data
#'
#' Internal helper function that prepares NMB data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param wtp Optional override for willingness-to-pay
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_nmb_table_data <- function(results,
                                  outcome_summary,
                                  cost_summary,
                                  groups = "overall",
                                  wtp = NULL,
                                  interventions = NULL,
                                  comparators = NULL,
                                  show_total = TRUE,
                                  decimals = 2,
                                  font_size = 11) {

  # Validate interventions/comparators
  if (is.null(interventions) && is.null(comparators)) {
    stop("One of 'interventions' or 'comparators' must be provided")
  }

  # Get WTP
  if (is.null(wtp)) {
    if (is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata.")
    }
    # Check if outcome summary exists using validation helper
    check_summary_exists(outcome_summary, results$metadata)
    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == outcome_summary)
    wtp <- outcome_meta$wtp[1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for outcome summary '%s'. Provide explicit wtp parameter.", outcome_summary))
    }
  }

  # Get outcome and cost summaries with differences (always use discounted for NMB)
  outcome_data <- get_summaries(
    results,
    groups = groups,
    summaries = outcome_summary,
    value_type = "outcome",
    discounted = TRUE,
    use_display_names = TRUE,
    interventions = interventions,
    comparators = comparators
  )

  cost_data <- get_summaries(
    results,
    groups = groups,
    summaries = cost_summary,
    value_type = "cost",
    discounted = TRUE,
    use_display_names = TRUE,
    interventions = interventions,
    comparators = comparators
  )

  # Transform outcome data: multiply by WTP
  outcome_data <- outcome_data %>%
    mutate(amount = .data$amount * wtp)

  # Transform cost data: negate
  cost_data <- cost_data %>%
    mutate(amount = -.data$amount)

  # Combine outcome and cost data
  combined_data <- bind_rows(outcome_data, cost_data) %>%
    select("strategy", "group", "value", "amount")

  # Get unique strategies, groups, values
  strategies_display <- unique(combined_data$strategy)
  groups_display <- unique(combined_data$group)
  values_display <- unique(combined_data$value)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "three_level" else "single"

  # Pivot to table format
  if (mode == "three_level") {
    # Mode 3: Group > Strategy columns
    pivot_data <- combined_data %>%
      pivot_wider(
        names_from = c("group", "strategy"),
        values_from = "amount",
        names_sep = "_",
        id_cols = "value"
      )
  } else{
    # Mode 1 or 2: Strategy columns only
    pivot_data <- combined_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = "amount",
        id_cols = "value"
      )
  }

  # Round values first (keep as numeric for now)
  value_cols <- setdiff(colnames(pivot_data), "value")
  for (col in value_cols) {
    if (is.numeric(pivot_data[[col]])) {
      pivot_data[[col]] <- round(pivot_data[[col]], decimals)
    }
  }

  # Track total row index if requested
  total_row_index <- NULL
  if (show_total) {
    total_row <- pivot_data %>%
      summarize(across(all_of(value_cols), ~sum(., na.rm = TRUE))) %>%
      mutate(value = "Total")
    pivot_data <- bind_rows(pivot_data, total_row)
    total_row_index <- nrow(pivot_data)
  }

  # NOW format values as character strings to prevent renderer reformatting
  for (col in value_cols) {
    if (is.numeric(pivot_data[[col]])) {
      rounded_vals <- pivot_data[[col]]
      # Fix negative zero display
      rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
      pivot_data[[col]] <- scales::comma(rounded_vals, accuracy = 10^(-decimals))
    }
  }

  # Rename value column to space (blank header) for NMB tables
  names(pivot_data)[names(pivot_data) == "value"] <- " "

  # Build result columns based on mode
  spacer_indices <- integer()
  if (mode == "three_level") {
    result_cols <- pivot_data[, " ", drop = FALSE]
    col_counter <- 1

    for (i in seq_along(groups_display)) {
      col_counter <- col_counter + 1  # Next position is spacer

      # Add spacer before each group
      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)
      spacer_indices <- c(spacer_indices, col_counter)

      col_counter <- col_counter + 1

      # Get columns for this group (use startsWith for literal matching to avoid regex issues)
      grp <- groups_display[i]
      grp_cols <- pivot_data[, startsWith(names(pivot_data), paste0(grp, "_")), drop = FALSE]
      result_cols <- cbind(result_cols, grp_cols)
      col_counter <- col_counter + ncol(grp_cols) - 1
    }
  } else {
    # Mode 1/2: No spacer columns, just rename columns to strategy names
    result_cols <- pivot_data
  }

  # Get all column names and indices
  all_cols <- colnames(result_cols)
  non_spacer_indices <- which(!grepl("^spacer_", all_cols))

  # Build column_names vector for kable in three-level mode
  column_names_vec <- NULL
  if (mode == "three_level") {
    column_names_vec <- c(" ")  # First column
    for (i in seq_along(groups_display)) {
      column_names_vec <- c(column_names_vec, "")  # Spacer column

      # For each group's columns, extract just the strategy names
      for (strat in strategies_display) {
        column_names_vec <- c(column_names_vec, strat)
      }
    }
  }

  # Build header structure
  if (mode == "three_level") {
    header_row1_values <- c("")
    header_row1_widths <- c(1)

    # Build level 2 values (strategy names) for proper display
    header_row2_values <- c(" ")
    header_row2_widths <- c(1)

    for (i in seq_along(groups_display)) {
      header_row1_values <- c(header_row1_values, "")
      header_row1_widths <- c(header_row1_widths, 1)
      header_row1_values <- c(header_row1_values, groups_display[i])
      header_row1_widths <- c(header_row1_widths, n_strategies)

      # Add spacer and strategy names to level 2
      header_row2_values <- c(header_row2_values, "")
      header_row2_widths <- c(header_row2_widths, 1)
      for (j in seq_along(strategies_display)) {
        header_row2_values <- c(header_row2_values, strategies_display[j])
        header_row2_widths <- c(header_row2_widths, 1)
      }
    }

    # Create header vector for kableExtra
    hdr_vec <- c(" " = 1)
    for (i in seq_along(groups_display)) {
      hdr_vec <- c(hdr_vec, setNames(1, paste0("spacer_", i)), setNames(n_strategies, groups_display[i]))
    }

    header_structure <- list(
      level1 = list(values = header_row1_values, widths = header_row1_widths, header_vector = hdr_vec),
      level2 = list(values = header_row2_values, widths = header_row2_widths),
      level3 = list(values = NA, widths = NA)
    )
  } else {
    # Single header level for Mode 1/2 - just strategy names
    header_row1_values <- c("", strategies_display)
    header_row1_widths <- rep(1, length(header_row1_values))

    # Create header vector for kableExtra
    hdr_vec <- setNames(rep(1, length(header_row1_values)), header_row1_values)

    header_structure <- list(
      level1 = list(values = header_row1_values, widths = header_row1_widths, header_vector = hdr_vec),
      level2 = list(values = NA, widths = NA),
      level3 = list(values = NA, widths = NA)
    )
  }

  # Build clean headers
  headers <- list()

  if (mode == "three_level") {
    # First row: Group names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))

    for (i in seq_along(groups_display)) {
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      row1[[length(row1) + 1]] <- list(
        span = n_strategies,
        text = groups_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Second row: Strategy names
    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))

    for (i in seq_along(groups_display)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      for (strat in strategies_display) {
        row2[[length(row2) + 1]] <- list(span = 1, text = strat, borders = c(0, 0, 1, 0))
      }
    }
    headers[[2]] <- row2
  } else {
    # Single mode: One header row with strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))

    for (i in seq_along(strategies_display)) {
      row1[[length(row1) + 1]] <- list(
        span = 1,
        text = strategies_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1
  }

  # Build column alignments and widths
  column_alignments <- "left"  # First column
  column_widths <- NA

  if (mode == "three_level") {
    for (i in seq_along(groups_display)) {
      column_alignments <- c(column_alignments, "center")  # spacer
      column_widths <- c(column_widths, 0.2)
      for (j in seq_len(n_strategies)) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }
  } else{
    # Simple mode - just strategy columns
    for (i in seq_len(n_strategies)) {
      column_alignments <- c(column_alignments, "right")
      column_widths <- c(column_widths, NA)
    }
  }

  # Special rows
  special_rows <- list()
  if (!is.null(total_row_index)) {
    special_rows$total_row <- total_row_index
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = result_cols,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format Net Monetary Benefit as Summary Table
#'
#' Creates a table showing NMB breakdown by component values.
#' Supports both flextable and kableExtra backends for flexible output formatting.
#'
#' @param results A openqaly model results object
#' @param health_outcome Name of the health outcome summary
#' @param cost_outcome Name of the cost summary
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param wtp Optional override for willingness-to-pay
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Create table using flextable (default)
#' ft <- nmb_table(results, "total_qalys", "total_cost",
#'                 comparator = "control")
#'
#' # Create table using kableExtra
#' kt <- nmb_table(results, "total_qalys", "total_cost",
#'                 comparator = "control", table_format = "kable")
#' }
#'
#' @export
nmb_table <- function(results,
                     health_outcome,
                     cost_outcome,
                     groups = "overall",
                     wtp = NULL,
                     interventions = NULL,
                     comparators = NULL,
                     show_total = TRUE,
                     decimals = 2,
                     font_size = 11,
                     table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_nmb_table_data(
    results = results,
    outcome_summary = health_outcome,
    cost_summary = cost_outcome,
    groups = groups,
    wtp = wtp,
    interventions = interventions,
    comparators = comparators,
    show_total = show_total,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
