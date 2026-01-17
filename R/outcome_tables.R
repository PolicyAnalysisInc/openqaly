#' Prepare Outcomes Summary Table Data
#'
#' Internal helper function that prepares outcomes summary data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly model results object
#' @param outcome Name of outcome to display (e.g., "total_qalys")
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param show_total Logical. Show TOTAL row?
#' @param decimals Number of decimal places
#' @param discounted Logical. Use discounted values?
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_outcomes_table_data <- function(results,
                                        outcome,
                                        groups = "overall",
                                        strategies = NULL,
                                        interventions = NULL,
                                        comparators = NULL,
                                        show_total = TRUE,
                                        decimals = 2,
                                        discounted = FALSE,
                                        font_size = 11) {

  # Get summary data (names already mapped by get_summaries)
  summary_data <- get_summaries(
    results,
    groups = groups,
    strategies = strategies,
    summaries = outcome,
    value_type = "outcome",
    discounted = discounted,
    use_display_names = TRUE,
    interventions = interventions,
    comparators = comparators
  )

  # Get unique strategies, groups, values (already have display names)
  strategies_display <- unique(summary_data$strategy)
  groups_display <- unique(summary_data$group)
  values_display <- unique(summary_data$value)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Value column is already mapped, just rename for consistency
  summary_data$value_display <- summary_data$value

  # Determine mode and pivoting strategy
  mode <- if (n_groups > 1 || is.null(groups)) "three_level" else "single"

  # Pivot to table format
  if (n_groups > 1 || is.null(groups)) {
    # Mode 3: Group > Strategy columns
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = c("group", "strategy"),
        values_from = "amount",
        names_sep = "_",
        id_cols = "value_display"
      )
  } else {
    # Mode 1 or 2: Strategy columns only
    pivot_data <- summary_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = "amount",
        id_cols = "value_display"
      )
  }

  # Round values first (keep as numeric for now)
  value_cols <- setdiff(colnames(pivot_data), "value_display")
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
      mutate(value_display = "Total")
    pivot_data <- bind_rows(pivot_data, total_row)
    total_row_index <- nrow(pivot_data)
  }

  # NOW format values as character strings to prevent renderer reformatting
  for (col in value_cols) {
    if (is.numeric(pivot_data[[col]])) {
      rounded_vals <- pivot_data[[col]]
      # Fix negative zero display
      rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
      pivot_data[[col]] <- format(rounded_vals,
                                  nsmall = decimals,
                                  scientific = FALSE,
                                  trim = TRUE)
    }
  }

  # Rename value_display column to space (blank header)
  names(pivot_data)[names(pivot_data) == "value_display"] <- " "

  # Add spacer columns and build result_cols based on mode
  spacer_indices <- integer()
  if (mode == "three_level") {
    result_cols <- pivot_data[, " ", drop = FALSE]
    col_counter <- 1

    for (i in seq_along(groups_display)) {
      col_counter <- col_counter + 1  # Next position is spacer

      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)
      spacer_indices <- c(spacer_indices, col_counter)

      col_counter <- col_counter + 1

      # Group columns (use startsWith for literal matching to avoid regex issues with special chars)
      grp <- groups_display[i]
      grp_cols <- pivot_data[, startsWith(names(pivot_data), paste0(grp, "_")), drop = FALSE]
      result_cols <- cbind(result_cols, grp_cols)
      col_counter <- col_counter + ncol(grp_cols) - 1
    }
  } else {
    # Mode 1/2: No spacer columns
    result_cols <- pivot_data
  }

  # Keep original column names (Group_Strategy format) to avoid duplicates
  # The hierarchical headers will be handled by the renderer using compose()

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
    # Single header level for Mode 1/2
    header_row1_values <- c("", strategies_display)
    header_row1_widths <- rep(1, length(header_row1_values))

    # Create header vector for kableExtra
    hdr_vec <- setNames(c(1, rep(1, n_strategies)), c("", strategies_display))

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


#' Format Outcomes as Summary Breakdown Table
#'
#' Creates a table showing outcome summary totals broken down by component values.
#' Supports both flextable and kableExtra backends for flexible output formatting.
#'
#' @param results A openqaly model results object
#' @param outcome Name of outcome to display (e.g., "total_qalys")
#' @param groups Group selection: "overall" (default), specific group, or NULL (all groups)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param show_total Logical. Show TOTAL row? (default: TRUE)
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values?
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
#' # Summary table for overall
#' ft <- outcomes_table(results, "total_qalys")
#'
#' # Compare across groups
#' ft <- outcomes_table(results, "total_qalys", groups = NULL)
#' }
#'
#' @export
outcomes_table <- function(results,
                           outcome,
                           groups = "overall",
                           strategies = NULL,
                           interventions = NULL,
                           comparators = NULL,
                           show_total = TRUE,
                           decimals = 2,
                           discounted = FALSE,
                           font_size = 11,
                           table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Validate that strategies cannot be used with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("Cannot specify 'strategies' together with 'interventions' or 'comparators'. Use either 'strategies' for absolute values, or 'interventions'/'comparators' for differences.")
  }

  # Prepare data
  prepared <- prepare_outcomes_table_data(
    results = results,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    show_total = show_total,
    decimals = decimals,
    discounted = discounted,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}

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
      pivot_data[[col]] <- format(rounded_vals,
                                  nsmall = decimals,
                                  scientific = FALSE,
                                  trim = TRUE)
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


#' Prepare Incremental Cost-Effectiveness Table Data
#'
#' Internal helper function that prepares incremental CE data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_incremental_ce_table_data <- function(results,
                                              outcome_summary,
                                              cost_summary,
                                              groups = "overall",
                                              strategies = NULL,
                                              decimals = 2,
                                              font_size = 11) {

  # Calculate incremental CE (always uses discounted values)
  ce_data <- calculate_incremental_ce(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Format ICER column using print.icer() logic
  format_icer <- function(icer_values, digits = decimals) {
    fmt_num <- function(v) {
      prettyNum(round(v, digits = digits),
                big.mark = ",",
                preserve.width = "none",
                scientific = FALSE)
    }

    out <- character(length(icer_values))
    out[is.na(icer_values)] <- ""  # Blank for NA (reference strategy)
    out[is.nan(icer_values)] <- "Equivalent"  # Changed from "—" to match print.icer
    out[is.infinite(icer_values)] <- "Dominated"
    out[!is.nan(icer_values) & !is.infinite(icer_values) & !is.na(icer_values) & icer_values == 0] <- "Dominant"

    pos <- !is.nan(icer_values) & !is.infinite(icer_values) & !is.na(icer_values) & icer_values > 0
    out[pos] <- fmt_num(icer_values[pos])

    neg <- !is.nan(icer_values) & !is.infinite(icer_values) & !is.na(icer_values) & icer_values < 0
    out[neg] <- paste0(fmt_num(-icer_values[neg]), "*")

    out
  }

  # Get unique strategies and groups
  strategies_display <- unique(ce_data$strategy)
  groups_display <- unique(ce_data$group)
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  # Create data for table - each strategy is already a row (no pivot needed!)
  table_data <- ce_data %>%
    arrange(.data$group, .data$cost) %>%
    select("strategy", "group", "cost", "outcome", "dcost", "doutcome", "icer", "comparator")

  # Format columns to character
  # Format numeric columns with proper handling for NA
  format_numeric_col <- function(values, digits) {
    sapply(values, function(v) {
      if (is.na(v)) {
        ""
      } else {
        format(round(v, digits), nsmall = digits, scientific = FALSE, trim = TRUE)
      }
    })
  }

  # Format the ICER column using the format_icer helper (vectorized)
  formatted_data <- table_data %>%
    mutate(
      cost = format_numeric_col(.data$cost, decimals),
      outcome = format_numeric_col(.data$outcome, decimals),
      dcost = format_numeric_col(.data$dcost, decimals),
      doutcome = format_numeric_col(.data$doutcome, decimals),
      icer = format_icer(.data$icer, decimals),
      comparator = ifelse(is.na(.data$comparator), "", .data$comparator)
    )

  # Prepare final table structure based on mode
  if (mode == "single_group") {
    # Single group: simple table with strategy column + metric columns
    result_cols <- formatted_data %>%
      select("strategy", "comparator", "cost", "outcome", "dcost", "doutcome", "icer")

    # Rename columns (use HTML entity &#916; for Delta symbol)
    names(result_cols) <- c("Strategy", "Comparator", "Cost", "Outcome", "&#916; Cost", "&#916; Outcome", "ICER")

    # Build header structure - simple single row
    headers <- list()
    row1 <- list()
    for (i in seq_along(names(result_cols))) {
      row1[[i]] <- list(
        span = 1,
        text = names(result_cols)[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Column alignments: left for strategy and comparator, right for metrics
    column_alignments <- c("left", "left", rep("right", 5))
    column_widths <- rep(NA, 7)

  } else {
    # Multi-group mode: group headers + indented rows (like pairwise)
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      # Group header row (bold + italic, empty metric cells)
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)
      result_data <- bind_rows(
        result_data,
        tibble(
          row_label = grp,
          comparator = "",
          cost = "",
          outcome = "",
          dcost = "",
          doutcome = "",
          icer = ""
        )
      )

      # Strategy rows for this group (will be indented via CSS)
      grp_data <- formatted_data %>%
        filter(.data$group == grp) %>%
        mutate(row_label = .data$strategy) %>%
        select("row_label", "comparator", "cost", "outcome", "dcost", "doutcome", "icer")

      # Track indented row indices
      n_grp_rows <- nrow(grp_data)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_grp_rows))

      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + n_grp_rows
    }

    result_cols <- result_data

    # Rename columns (use HTML entity &#916; for Delta symbol)
    names(result_cols) <- c("Strategy", "Comparator", "Cost", "Outcome", "&#916; Cost", "&#916; Outcome", "ICER")

    # Build header structure - simple single row (7 columns, no Group column)
    headers <- list()
    row1 <- list()
    for (i in seq_along(names(result_cols))) {
      row1[[i]] <- list(
        span = 1,
        text = names(result_cols)[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Column alignments: left for strategy/comparator, right for metrics
    column_alignments <- c("left", "left", rep("right", 5))
    column_widths <- rep(NA, 7)

    # Calculate group boundary rows (all group headers except the first)
    group_boundary_rows <- group_header_rows[-1]
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
  }

  # Build special_rows for single_group mode
  if (mode == "single_group") {
    special_rows <- list()
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


#' Format Incremental Cost-Effectiveness as Summary Table
#'
#' Creates a table showing incremental cost-effectiveness analysis with strategies
#' sorted by cost, including incremental costs, outcomes, and ICERs.
#'
#' @param results A openqaly model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param decimals Number of decimal places (default: 2)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Create incremental CE table
#' incremental_ce_table(results, "total_qalys", "total_cost")
#'
#' # For all groups
#' incremental_ce_table(results, "total_qalys", "total_cost", groups = NULL)
#' }
#'
#' @export
incremental_ce_table <- function(results,
                                outcome_summary,
                                cost_summary,
                                groups = "overall",
                                strategies = NULL,
                                decimals = 2,
                                font_size = 11,
                                table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_incremental_ce_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare Pairwise Cost-Effectiveness Table Data
#'
#' Internal helper function that prepares pairwise CE data for rendering.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_pairwise_ce_table_data <- function(results,
                                          outcome_summary,
                                          cost_summary,
                                          groups = "overall",
                                          strategies = NULL,
                                          interventions = NULL,
                                          comparators = NULL,
                                          decimals = 2,
                                          font_size = 11) {

  # Calculate pairwise CE (always uses discounted values)
  ce_data <- calculate_pairwise_ce(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators
  )

  # Get absolute values for all strategies (always use discounted for CE)
  cost_data <- get_summaries(
    results,
    groups = groups,
    strategies = strategies,
    summaries = cost_summary,
    value_type = "cost",
    discounted = TRUE,
    use_display_names = TRUE
  ) %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(cost = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  outcome_data <- get_summaries(
    results,
    groups = groups,
    strategies = strategies,
    summaries = outcome_summary,
    value_type = "outcome",
    discounted = TRUE,
    use_display_names = TRUE
  ) %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(outcome = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  absolute_data <- cost_data %>%
    inner_join(outcome_data, by = c("strategy", "group"))

  # Format ICER column using print.icer() logic
  format_icer <- function(icer_values, digits = decimals) {
    fmt_num <- function(v) {
      prettyNum(round(v, digits = digits),
                big.mark = ",",
                preserve.width = "none",
                scientific = FALSE)
    }

    out <- character(length(icer_values))
    out[is.na(icer_values)] <- ""  # Blank for NA
    out[is.nan(icer_values)] <- "Equivalent"
    out[is.infinite(icer_values)] <- "Dominated"
    out[!is.nan(icer_values) & !is.infinite(icer_values) & !is.na(icer_values) & icer_values == 0] <- "Dominant"

    pos <- !is.nan(icer_values) & !is.infinite(icer_values) & !is.na(icer_values) & icer_values > 0
    out[pos] <- fmt_num(icer_values[pos])

    neg <- !is.nan(icer_values) & !is.infinite(icer_values) & !is.na(icer_values) & icer_values < 0
    out[neg] <- paste0(fmt_num(-icer_values[neg]), "*")

    out
  }

  # Format numeric columns
  format_numeric_col <- function(values, digits) {
    sapply(values, function(v) {
      if (is.na(v)) {
        ""
      } else {
        format(round(v, digits), nsmall = digits, scientific = FALSE, trim = TRUE)
      }
    })
  }

  # Check if footnote is needed (any negative ICER)
  needs_footnote <- any(!is.na(ce_data$icer) & !is.nan(ce_data$icer) &
                        !is.infinite(ce_data$icer) & ce_data$icer < 0)

  # Get unique groups
  groups_display <- unique(absolute_data$group)
  n_groups <- length(groups_display)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  # Build table data
  if (mode == "single_group") {
    # Single group: simple table
    # Strategy rows
    strategy_rows <- absolute_data %>%
      arrange(.data$cost) %>%
      mutate(
        row_label = .data$strategy,
        cost_fmt = format_numeric_col(.data$cost, decimals),
        outcome_fmt = format_numeric_col(.data$outcome, decimals),
        icer_fmt = ""
      ) %>%
      select("row_label", "cost_fmt", "outcome_fmt", "icer_fmt")

    # Comparison rows
    comparison_rows <- ce_data %>%
      mutate(
        row_label = paste(.data$strategy, "vs.", .data$comparator),
        cost_fmt = format_numeric_col(.data$dcost, decimals),
        outcome_fmt = format_numeric_col(.data$doutcome, decimals),
        icer_fmt = format_icer(.data$icer, decimals)
      ) %>%
      select("row_label", "cost_fmt", "outcome_fmt", "icer_fmt")

    # Combine
    result_data <- bind_rows(strategy_rows, comparison_rows)
    colnames(result_data) <- c(" ", "Cost", "Outcome", "ICER")

    # Build header
    headers <- list()
    row1 <- list(
      list(span = 1, text = "", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Cost", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Outcome", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "ICER", borders = c(1, 0, 1, 0))
    )
    headers[[1]] <- row1

    column_alignments <- c("left", "right", "right", "right")
    column_widths <- rep(NA, 4)
    special_rows <- list()

  } else {
    # Multi-group mode: group headers + indented rows
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      # Group header row
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)
      result_data <- bind_rows(
        result_data,
        tibble(
          row_label = grp,
          cost_fmt = "",
          outcome_fmt = "",
          icer_fmt = ""
        )
      )

      # Strategy rows for this group (will be indented via CSS)
      grp_strategies <- absolute_data %>%
        filter(.data$group == grp) %>%
        arrange(.data$cost) %>%
        mutate(
          row_label = .data$strategy,
          cost_fmt = format_numeric_col(.data$cost, decimals),
          outcome_fmt = format_numeric_col(.data$outcome, decimals),
          icer_fmt = ""
        ) %>%
        select("row_label", "cost_fmt", "outcome_fmt", "icer_fmt")

      # Track indented row indices for strategy rows
      n_strategy_rows <- nrow(grp_strategies)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_strategy_rows))

      result_data <- bind_rows(result_data, grp_strategies)
      current_row <- current_row + n_strategy_rows

      # Comparison rows for this group (will be indented via CSS)
      grp_comparisons <- ce_data %>%
        filter(.data$group == grp) %>%
        mutate(
          row_label = paste0(.data$strategy, " vs. ", .data$comparator),
          cost_fmt = format_numeric_col(.data$dcost, decimals),
          outcome_fmt = format_numeric_col(.data$doutcome, decimals),
          icer_fmt = format_icer(.data$icer, decimals)
        ) %>%
        select("row_label", "cost_fmt", "outcome_fmt", "icer_fmt")

      # Track indented row indices for comparison rows
      n_comparison_rows <- nrow(grp_comparisons)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_comparison_rows))

      result_data <- bind_rows(result_data, grp_comparisons)
      current_row <- current_row + n_comparison_rows
    }

    colnames(result_data) <- c(" ", "Cost", "Outcome", "ICER")

    # Build header
    headers <- list()
    row1 <- list(
      list(span = 1, text = "", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Cost", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Outcome", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "ICER", borders = c(1, 0, 1, 0))
    )
    headers[[1]] <- row1

    column_alignments <- c("left", "right", "right", "right")
    column_widths <- rep(NA, 4)

    # Calculate group boundary rows (all group headers except the first)
    group_boundary_rows <- group_header_rows[-1]
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
  }

  # Add footnote if needed
  if (needs_footnote) {
    special_rows$footnote <- "* Intervention is less effective and less costly than comparator. ICER reflects cost-effectiveness of comparator vs. intervention"
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format Pairwise Cost-Effectiveness as Summary Table
#'
#' Creates a table showing pairwise cost-effectiveness comparisons against a single
#' reference strategy. Shows absolute values for all strategies plus incremental
#' comparisons.
#'
#' @param results A openqaly model results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param groups Group selection: "overall" (default), specific group, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param decimals Number of decimal places (default: 2)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Pairwise CE table vs control
#' pairwise_ce_table(results, "total_qalys", "total_cost", comparator = "control")
#'
#' # For all groups
#' pairwise_ce_table(results, "total_qalys", "total_cost", groups = NULL,
#'                   comparator = "control")
#' }
#'
#' @export
pairwise_ce_table <- function(results,
                             outcome_summary,
                             cost_summary,
                             groups = "overall",
                             strategies = NULL,
                             interventions = NULL,
                             comparators = NULL,
                             decimals = 2,
                             font_size = 11,
                             table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_pairwise_ce_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
