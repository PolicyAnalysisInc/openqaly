#' Prepare Incremental CEAC Table Data
#'
#' Internal helper function that prepares incremental CEAC data for rendering as
#' a table. Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns
#' @param group Group selection: "aggregated" uses aggregated results, specific
#'   group name filters to that group, NULL or "all" includes all groups
#'   (creates hierarchical table with group column)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param show_optimal Logical. Highlight optimal strategy at each WTP?
#' @param decimals Number of decimal places for percentages
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_incremental_ceac_table_data <- function(results,
                                                outcome_summary,
                                                cost_summary,
                                                wtp_thresholds = c(0, 20000, 50000, 100000),
                                                group = "aggregated",
                                                strategies = NULL,
                                                discounted = FALSE,
                                                show_optimal = TRUE,
                                                decimals = 1,
                                                font_size = 11) {

  # Calculate incremental CEAC at specified WTP thresholds
  ceac_data <- calculate_incremental_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp_thresholds,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Get unique strategies and groups
  strategies_display <- unique(ceac_data$strategy)
  groups_display <- unique(ceac_data$group)

  # Reorder according to original model order from metadata
  if (!is.null(results$metadata$strategies)) {
    strategy_order <- results$metadata$strategies$display_name
    strategies_display <- strategy_order[strategy_order %in% strategies_display]
  }
  if (!is.null(results$metadata$groups)) {
    group_order <- results$metadata$groups$display_name
    groups_display <- group_order[group_order %in% groups_display]
  }

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine if we have multiple groups
  has_multiple_groups <- n_groups > 1

  # Pivot to wide format: WTP in rows, strategies in columns
  if (has_multiple_groups) {
    # Multiple groups: pivot with group_strategy columns
    pivot_data <- ceac_data %>%
      mutate(probability_pct = .data$probability * 100) %>%
      pivot_wider(
        id_cols = "wtp",
        names_from = c("group", "strategy"),
        names_sep = "_",
        values_from = "probability_pct"
      ) %>%
      arrange(.data$wtp)
  } else {
    # Single group: simple structure
    pivot_data <- ceac_data %>%
      mutate(probability_pct = .data$probability * 100) %>%
      pivot_wider(
        id_cols = "wtp",
        names_from = "strategy",
        values_from = "probability_pct"
      ) %>%
      arrange(.data$wtp)
  }

  # Identify optimal strategies if requested
  optimal_strategies <- NULL
  if (show_optimal) {
    optimal_strategies <- ceac_data %>%
      group_by(.data$group, .data$wtp) %>%
      slice_max(.data$probability, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select("group", "wtp", "strategy")
  }

  # Format WTP column as dollar amounts
  pivot_data <- pivot_data %>%
    mutate(wtp_display = scales::dollar(.data$wtp))

  # Format probability columns as percentage strings
  strategy_cols <- setdiff(colnames(pivot_data), c("wtp", "wtp_display"))
  for (col in strategy_cols) {
    if (is.numeric(pivot_data[[col]])) {
      pivot_data[[col]] <- format(
        round(pivot_data[[col]], decimals),
        nsmall = decimals,
        scientific = FALSE,
        trim = TRUE
      )
      # Add % symbol
      pivot_data[[col]] <- paste0(pivot_data[[col]], "%")
    }
  }

  # Build result columns based on mode
  if (has_multiple_groups) {
    # Build with spacers: [WTP] [spacer] [Group1 strategies] [spacer] [Group2 strategies] ...
    result_cols <- pivot_data[, "wtp_display", drop = FALSE]
    colnames(result_cols) <- "WTP"

    for (i in seq_along(groups_display)) {
      # Add spacer column
      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)

      # Add group's strategy columns
      grp <- groups_display[i]
      grp_cols <- pivot_data[, grepl(paste0("^", grp, "_"), colnames(pivot_data)), drop = FALSE]
      result_cols <- cbind(result_cols, grp_cols)
    }
  } else {
    # Simple mode: [WTP] [Strategy columns]
    result_cols <- pivot_data %>%
      select("wtp_display", all_of(strategies_display))
    colnames(result_cols)[1] <- "WTP"
  }

  # Build headers structure
  headers <- list()

  if (has_multiple_groups) {
    # Two header rows
    # Row 1: Empty over WTP, spacers (no borders), group names spanning strategies
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))

    for (i in seq_along(groups_display)) {
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      row1[[length(row1) + 1]] <- list(
        span = n_strategies,
        text = groups_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Row 2: WTP, spacers (no borders), strategy names
    row2 <- list()
    row2[[1]] <- list(span = 1, text = "WTP", borders = c(0, 0, 1, 0))

    for (i in seq_along(groups_display)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      for (strat in strategies_display) {
        row2[[length(row2) + 1]] <- list(span = 1, text = strat, borders = c(0, 0, 1, 0))
      }
    }
    headers[[2]] <- row2

    # Column alignments and widths
    column_alignments <- "left"  # WTP column
    column_widths <- NA

    for (i in seq_along(groups_display)) {
      column_alignments <- c(column_alignments, "center")  # spacer
      column_widths <- c(column_widths, 0.2)
      for (j in seq_len(n_strategies)) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }
  } else {
    # Single header row: WTP + strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "WTP", borders = c(1, 0, 1, 0))

    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(
        span = 1,
        text = strat,
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Column alignments
    column_alignments <- c("left", rep("right", n_strategies))
    column_widths <- c(NA, rep(NA, n_strategies))
  }

  # Identify cells to highlight (optimal strategies)
  highlighted_cells <- list()
  if (show_optimal && !is.null(optimal_strategies)) {
    for (i in seq_len(nrow(optimal_strategies))) {
      opt <- optimal_strategies[i, ]

      # Find row index (by WTP)
      row_idx <- which(pivot_data$wtp == opt$wtp)

      if (length(row_idx) > 0) {
        # Find column index (by group_strategy combination)
        if (has_multiple_groups) {
          # Find the strategy column within the group's columns
          # Column structure: WTP, spacer1, Group1_Strat1, Group1_Strat2, ..., spacer2, Group2_Strat1, ...
          # Calculate position: 1 (WTP) + group_index * (1 spacer + n_strategies) + strategy_position
          group_idx <- which(groups_display == opt$group)
          strat_idx <- which(strategies_display == opt$strategy)
          if (length(group_idx) > 0 && length(strat_idx) > 0) {
            col_idx <- 1 + group_idx * (1 + n_strategies) - n_strategies + strat_idx
            highlighted_cells[[length(highlighted_cells) + 1]] <- c(row_idx, col_idx)
          }
        } else {
          # Simple mode: column position is 1 (WTP) + strategy position
          strat_idx <- which(strategies_display == opt$strategy)
          if (length(strat_idx) > 0) {
            col_idx <- 1 + strat_idx
            highlighted_cells[[length(highlighted_cells) + 1]] <- c(row_idx, col_idx)
          }
        }
      }
    }
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = result_cols,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(highlighted_cells = highlighted_cells),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format Incremental CEAC as Probability Table
#'
#' Creates a table showing the probability that each strategy is most
#' cost-effective at selected willingness-to-pay thresholds in a multi-way
#' (incremental) comparison.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns.
#'   Default is c(0, 20000, 50000, 100000)
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (creates hierarchical table with group column)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param show_optimal Logical. Highlight optimal strategy at each WTP?
#' @param decimals Number of decimal places for percentages (default: 1)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The incremental CEAC table displays probabilities from a multi-way comparison
#' where all strategies are compared simultaneously. Probabilities sum to 1 across
#' all strategies at each WTP threshold.
#'
#' When multiple groups are present, the table includes a group column showing
#' results in a hierarchical structure (group -> strategy -> probabilities).
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Incremental CEAC table at default thresholds
#' incremental_ceac_table(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP thresholds
#' incremental_ceac_table(psa_results, "total_qalys", "total_cost",
#'                        wtp_thresholds = c(0, 30000, 50000, 75000, 100000))
#' }
#'
#' @export
incremental_ceac_table <- function(results,
                                   outcome_summary,
                                   cost_summary,
                                   wtp_thresholds = c(0, 20000, 50000, 100000),
                                   group = "aggregated",
                                   strategies = NULL,
                                   discounted = FALSE,
                                   show_optimal = TRUE,
                                   decimals = 1,
                                   font_size = 11,
                                   table_format = c("kable", "flextable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_incremental_ceac_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp_thresholds = wtp_thresholds,
    group = group,
    strategies = strategies,
    discounted = discounted,
    show_optimal = show_optimal,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare PSA Summary Table Data
#'
#' Internal helper function that prepares PSA summary statistics for rendering.
#' Creates a transposed table with strategies as columns and statistics as rows.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param pce_wtp Numeric vector of WTP thresholds for P(CE) rows
#' @param group Group selection
#' @param strategies Character vector of strategies to include
#' @param discounted Logical. Use discounted values?
#' @param outcome_decimals Number of decimal places for outcomes
#' @param cost_decimals Number of decimal places for costs
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_psa_summary_table_data <- function(results,
                                          outcome_summary,
                                          cost_summary,
                                          pce_wtp = c(50000, 100000, 150000),
                                          group = "aggregated",
                                          strategies = NULL,
                                          discounted = FALSE,
                                          outcome_decimals = 2,
                                          cost_decimals = 0,
                                          font_size = 11) {

  # Get PSA simulation data
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Calculate summary statistics (expanded to include median, IQR, min, max)
  summary_stats <- psa_data %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(
      # Cost statistics
      mean_cost = mean(.data$cost, na.rm = TRUE),
      sd_cost = stats::sd(.data$cost, na.rm = TRUE),
      ci_lower_cost = stats::quantile(.data$cost, 0.025, na.rm = TRUE),
      ci_upper_cost = stats::quantile(.data$cost, 0.975, na.rm = TRUE),
      median_cost = stats::median(.data$cost, na.rm = TRUE),
      q25_cost = stats::quantile(.data$cost, 0.25, na.rm = TRUE),
      q75_cost = stats::quantile(.data$cost, 0.75, na.rm = TRUE),
      min_cost = min(.data$cost, na.rm = TRUE),
      max_cost = max(.data$cost, na.rm = TRUE),

      # Outcome statistics
      mean_outcome = mean(.data$outcome, na.rm = TRUE),
      sd_outcome = stats::sd(.data$outcome, na.rm = TRUE),
      ci_lower_outcome = stats::quantile(.data$outcome, 0.025, na.rm = TRUE),
      ci_upper_outcome = stats::quantile(.data$outcome, 0.975, na.rm = TRUE),
      median_outcome = stats::median(.data$outcome, na.rm = TRUE),
      q25_outcome = stats::quantile(.data$outcome, 0.25, na.rm = TRUE),
      q75_outcome = stats::quantile(.data$outcome, 0.75, na.rm = TRUE),
      min_outcome = min(.data$outcome, na.rm = TRUE),
      max_outcome = max(.data$outcome, na.rm = TRUE),

      .groups = "drop"
    )

  # Calculate P(CE) for each WTP threshold
  pce_results <- list()
  for (wtp_val in pce_wtp) {
    ceac_at_wtp <- calculate_incremental_ceac(
      results = results,
      outcome_summary = outcome_summary,
      cost_summary = cost_summary,
      wtp = wtp_val,
      group = group,
      strategies = strategies,
      discounted = discounted
    ) %>%
      mutate(wtp = wtp_val) %>%
      select("strategy", "group", "wtp", "probability")

    pce_results[[length(pce_results) + 1]] <- ceac_at_wtp
  }
  pce_data <- bind_rows(pce_results)

  # Formatting helper functions (uses decimal places, not sigfigs)
  format_with_ci <- function(mean_val, lower, upper, decimals, use_comma = FALSE) {
    big_mark <- if (use_comma) "," else ""
    paste0(
      format(round(mean_val, decimals), nsmall = decimals, big.mark = big_mark, trim = TRUE),
      " (",
      format(round(lower, decimals), nsmall = decimals, big.mark = big_mark, trim = TRUE),
      " - ",
      format(round(upper, decimals), nsmall = decimals, big.mark = big_mark, trim = TRUE),
      ")"
    )
  }

  format_range <- function(min_val, max_val, decimals, use_comma = FALSE) {
    big_mark <- if (use_comma) "," else ""
    paste0(
      format(round(min_val, decimals), nsmall = decimals, big.mark = big_mark, trim = TRUE),
      " - ",
      format(round(max_val, decimals), nsmall = decimals, big.mark = big_mark, trim = TRUE)
    )
  }

  format_numeric <- function(val, decimals, use_comma = FALSE) {
    big_mark <- if (use_comma) "," else ""
    format(round(val, decimals), nsmall = decimals, big.mark = big_mark, trim = TRUE)
  }

  # Get unique strategies and groups (preserve order from metadata)
  strategies_display <- unique(summary_stats$strategy)
  groups_display <- unique(summary_stats$group)

  if (!is.null(results$metadata$strategies)) {
    strategy_order <- results$metadata$strategies$display_name
    filtered_strategies <- strategy_order[strategy_order %in% strategies_display]
    # Only use filtered version if it's non-empty
    if (length(filtered_strategies) > 0) {
      strategies_display <- filtered_strategies
    }
  }
  if (!is.null(results$metadata$groups)) {
    group_order <- results$metadata$groups$display_name
    filtered_groups <- group_order[group_order %in% groups_display]
    # Only use filtered version if it's non-empty
    if (length(filtered_groups) > 0) {
      groups_display <- filtered_groups
    }
  }

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1

  # Build row labels first (these are fixed regardless of strategies)
  # Note: We need unique identifiers for duplicate label names (Mean, SD, etc. appear twice)
  row_labels_ordered <- c(
    "Outcome",
    "outcome_mean",
    "outcome_sd",
    "outcome_median",
    "outcome_range",
    "Cost",
    "cost_mean",
    "cost_sd",
    "cost_median",
    "cost_range",
    "Probability of Cost-Effectiveness"
  )

  # Display labels (what will actually be shown in the table)
  row_labels_display <- c(
    "Outcome",
    "Mean (95% CI)",
    "SD",
    "Median (IQR)",
    "Range",
    "Cost",
    "Mean (95% CI)",
    "SD",
    "Median (IQR)",
    "Range",
    "Probability of Cost-Effectiveness"
  )

  # Add WTP rows
  for (wtp_val in pce_wtp) {
    wtp_id <- paste0("pce_", wtp_val)
    wtp_label <- paste0("\u03bb = $", format(wtp_val, big.mark = ",", scientific = FALSE))
    row_labels_ordered <- c(row_labels_ordered, wtp_id)
    row_labels_display <- c(row_labels_display, wtp_label)
  }

  # Build transposed data structure
  # For each strategy-group combination, create a column of values
  row_data_list <- list()

  for (strat in strategies_display) {
    for (grp in groups_display) {
      stat_row <- summary_stats %>%
        filter(.data$strategy == strat, .data$group == grp)

      if (nrow(stat_row) == 0) next
      stat_row <- stat_row[1, ]

      # Build values for this strategy-group in the same order as row_labels_ordered
      values_col <- c(
        # Outcome section
        "",  # "Outcome" header
        format_with_ci(stat_row$mean_outcome, stat_row$ci_lower_outcome,
                       stat_row$ci_upper_outcome, outcome_decimals, use_comma = FALSE),
        format_numeric(stat_row$sd_outcome, outcome_decimals, use_comma = FALSE),
        format_with_ci(stat_row$median_outcome, stat_row$q25_outcome,
                       stat_row$q75_outcome, outcome_decimals, use_comma = FALSE),
        format_range(stat_row$min_outcome, stat_row$max_outcome,
                     outcome_decimals, use_comma = FALSE),

        # Cost section
        "",  # "Cost" header
        format_with_ci(stat_row$mean_cost, stat_row$ci_lower_cost,
                       stat_row$ci_upper_cost, cost_decimals, use_comma = TRUE),
        format_numeric(stat_row$sd_cost, cost_decimals, use_comma = TRUE),
        format_with_ci(stat_row$median_cost, stat_row$q25_cost,
                       stat_row$q75_cost, cost_decimals, use_comma = TRUE),
        format_range(stat_row$min_cost, stat_row$max_cost,
                     cost_decimals, use_comma = TRUE),

        # P(CE) section
        ""  # "Probability of Cost-Effectiveness" header
      )

      # Add P(CE) values for each WTP
      for (wtp_val in pce_wtp) {
        prob_val <- pce_data %>%
          filter(.data$strategy == strat, .data$group == grp, .data$wtp == wtp_val) %>%
          pull(.data$probability)

        if (length(prob_val) == 0) prob_val <- NA

        prob_formatted <- if (is.na(prob_val)) {
          "NA"
        } else {
          paste0(format(round(prob_val * 100, 1), nsmall = 1), "%")
        }

        values_col <- c(values_col, prob_formatted)
      }

      # Add to list
      row_data_list[[paste0(strat, "_", grp)]] <- tibble(
        row_label = row_labels_ordered,
        strategy = strat,
        group = grp,
        value = values_col
      )
    }
  }

  # Combine all data
  row_data_long <- bind_rows(row_data_list)

  # Handle single group vs. multiple groups
  if (!has_multiple_groups) {
    # Single group mode: [Row Label, Strategy 1, Strategy 2, ...]

    # Pivot to wide format
    pivot_data <- row_data_long %>%
      pivot_wider(
        names_from = "strategy",
        values_from = "value",
        id_cols = "row_label"
      ) %>%
      mutate(row_label = factor(.data$row_label, levels = row_labels_ordered)) %>%
      arrange(.data$row_label) %>%
      mutate(row_label = as.character(.data$row_label))

    # Map row labels to display labels
    label_mapping <- tibble(
      row_label = row_labels_ordered,
      display_label = row_labels_display
    )
    pivot_data <- pivot_data %>%
      left_join(label_mapping, by = "row_label") %>%
      select("display_label", all_of(strategies_display))

    colnames(pivot_data)[1] <- " "

    # Update row labels for finding section headers
    row_label_col <- pivot_data[[" "]]

    # Build headers
    headers <- list()
    row1 <- list(list(span = 1, text = "", borders = c(1, 0, 1, 0)))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_strategies))
    column_widths <- rep(NA, n_strategies + 1)

    # Track section header rows
    section_headers <- c("Outcome", "Cost", "Probability of Cost-Effectiveness")
    group_header_rows <- which(row_label_col %in% section_headers)

    # Track indented rows (detail rows under section headers)
    # Rows 2-5 (outcome details), 7-10 (cost details), 12+ (P(CE) WTP rows)
    indented_rows <- c(2:5, 7:10, (12:(11 + length(pce_wtp))))

  } else {
    # Multiple groups mode: [Row Label, spacer, Group1-Strat1, Group1-Strat2, spacer, Group2-Strat1, ...]

    # Create initial dataframe with display labels
    result_cols <- data.frame(row_label = row_labels_display, stringsAsFactors = FALSE)
    colnames(result_cols) <- " "

    for (i in seq_along(groups_display)) {
      # Add spacer column
      spacer_col <- data.frame(rep("", length(row_labels_ordered)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)

      # Add group's strategy columns
      grp <- groups_display[i]
      for (strat in strategies_display) {
        strat_data <- row_data_long %>%
          filter(.data$strategy == strat, .data$group == grp) %>%
          mutate(row_label = factor(.data$row_label, levels = row_labels_ordered)) %>%
          arrange(.data$row_label) %>%
          select("value") %>%
          pull(.data$value)

        if (length(strat_data) == 0) {
          strat_data <- rep("", length(row_labels_ordered))
        }

        col_df <- data.frame(strat_data, stringsAsFactors = FALSE)
        names(col_df) <- paste0(grp, "_", strat)
        result_cols <- cbind(result_cols, col_df)
      }
    }

    # Add row_label column (already has it as " ")
    pivot_data <- as_tibble(result_cols)

    # Build two-level headers
    headers <- list()

    # Row 1: Group names spanning strategies
    row1 <- list(list(span = 1, text = "", borders = c(1, 0, 1, 0)))
    for (i in seq_along(groups_display)) {
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      row1[[length(row1) + 1]] <- list(
        span = n_strategies,
        text = groups_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Row 2: Strategy names
    row2 <- list(list(span = 1, text = "", borders = c(0, 0, 1, 0)))
    for (i in seq_along(groups_display)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      for (strat in strategies_display) {
        row2[[length(row2) + 1]] <- list(span = 1, text = strat, borders = c(0, 0, 1, 0))
      }
    }
    headers[[2]] <- row2

    # Column alignments and widths
    column_alignments <- "left"  # Row label column
    column_widths <- NA

    for (i in seq_along(groups_display)) {
      column_alignments <- c(column_alignments, "center")  # spacer
      column_widths <- c(column_widths, 0.2)
      for (j in seq_len(n_strategies)) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }

    # Track section header rows
    section_headers <- c("Outcome", "Cost", "Probability of Cost-Effectiveness")
    # Find row indices in pivot_data
    group_header_rows <- which(pivot_data[[" "]] %in% section_headers)

    # Track indented rows (detail rows under section headers)
    # Rows 2-5 (outcome details), 7-10 (cost details), 12+ (P(CE) WTP rows)
    indented_rows <- c(2:5, 7:10, (12:(11 + length(pce_wtp))))
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = pivot_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(
      group_header_rows = group_header_rows,
      indented_rows = indented_rows
    ),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format PSA Summary Statistics Table
#'
#' Creates a table showing summary statistics from PSA simulations including
#' mean, SD, median (IQR), range, and probability of being cost-effective at
#' multiple WTP thresholds. Table is transposed with strategies as columns and
#' statistics as rows.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param pce_wtp Numeric vector of WTP thresholds for P(CE) rows
#'   (default: c(50000, 100000, 150000))
#' @param group Group selection: "aggregated" (default), specific group, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param outcome_decimals Number of decimal places for outcomes (default: 2)
#' @param cost_decimals Number of decimal places for costs (default: 0, nearest unit)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The table is transposed with strategies as columns. Rows are organized into
#' three sections:
#' \itemize{
#'   \item Outcome: Mean (95% CI), SD, Median (IQR), Range
#'   \item Cost: Mean (95% CI), SD, Median (IQR), Range
#'   \item Probability of Cost-Effectiveness: One row per WTP threshold
#' }
#'
#' All rounding uses decimal places (not significant figures). Costs are rounded
#' to the nearest unit by default (cost_decimals = 0).
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # PSA summary table with default WTP thresholds
#' psa_summary_table(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP thresholds
#' psa_summary_table(psa_results, "total_qalys", "total_cost",
#'                   pce_wtp = c(30000, 50000, 100000, 150000))
#'
#' # Different decimal places
#' psa_summary_table(psa_results, "total_qalys", "total_cost",
#'                   outcome_decimals = 3, cost_decimals = 2)
#' }
#'
#' @export
psa_summary_table <- function(results,
                             outcome_summary,
                             cost_summary,
                             pce_wtp = c(50000, 100000, 150000),
                             group = "aggregated",
                             strategies = NULL,
                             discounted = FALSE,
                             outcome_decimals = 2,
                             cost_decimals = 0,
                             font_size = 11,
                             table_format = c("kable", "flextable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_psa_summary_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    pce_wtp = pce_wtp,
    group = group,
    strategies = strategies,
    discounted = discounted,
    outcome_decimals = outcome_decimals,
    cost_decimals = cost_decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare Pairwise CEAC Table Data
#'
#' Internal helper function that prepares pairwise CEAC data for rendering as
#' a table. Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param intervention Reference strategy for comparison (intervention perspective).
#'   Mutually exclusive with comparator.
#' @param comparator Reference strategy for comparison (comparator perspective).
#'   Mutually exclusive with intervention.
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns
#' @param group Group selection: "aggregated" uses aggregated results, specific
#'   group name filters to that group, NULL or "all" includes all groups
#'   (creates hierarchical table with group column)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param decimals Number of decimal places for percentages
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_pairwise_ceac_table_data <- function(results,
                                             outcome_summary,
                                             cost_summary,
                                             intervention = NULL,
                                             comparator = NULL,
                                             wtp_thresholds = c(0, 20000, 50000, 100000),
                                             group = "aggregated",
                                             strategies = NULL,
                                             discounted = FALSE,
                                             decimals = 1,
                                             font_size = 11) {

  # Calculate pairwise CEAC at specified WTP thresholds
  ceac_data <- calculate_pairwise_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    intervention = intervention,
    comparator = comparator,
    wtp = wtp_thresholds,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Determine reference name and label direction
  reference_name <- unique(ceac_data$comparator)[1]
  comparison_label <- if (!is.null(comparator)) {
    paste0("P > ", reference_name)
  } else {
    paste0("P(", reference_name, " >)")
  }

  # Get unique comparisons and groups
  comparisons_display <- unique(ceac_data$strategy)
  groups_display <- unique(ceac_data$group)

  # Reorder groups according to original model order from metadata
  if (!is.null(results$metadata$groups)) {
    group_order <- results$metadata$groups$display_name
    groups_display <- group_order[group_order %in% groups_display]
  }

  # Reorder comparisons according to original strategy order from metadata
  # Comparisons may have "vs. " prefix in intervention mode
  if (!is.null(results$metadata$strategies)) {
    strategy_order <- results$metadata$strategies$display_name
    # Check if comparisons have "vs. " prefix
    has_vs_prefix <- any(grepl("^vs\\. ", comparisons_display))
    if (has_vs_prefix) {
      # Strip prefix, reorder, add prefix back
      comparison_strategies <- sub("^vs\\. ", "", comparisons_display)
      ordered_strategies <- strategy_order[strategy_order %in% comparison_strategies]
      comparisons_display <- paste0("vs. ", ordered_strategies)
    } else {
      # Direct ordering
      comparisons_display <- strategy_order[strategy_order %in% comparisons_display]
    }
  }

  n_comparisons <- length(comparisons_display)
  n_groups <- length(groups_display)

  # Build full "X vs. Y" comparison labels for column headers
  comparison_labels <- character(length(comparisons_display))
  for (i in seq_along(comparisons_display)) {
    comp <- comparisons_display[i]
    if (!is.null(comparator)) {
      # Comparator mode: strategy vs. comparator
      comparison_labels[i] <- paste(comp, "vs.", reference_name)
    } else {
      # Referent mode: comparator vs. strategy (strip "vs. " prefix)
      strategy_name <- sub("^vs\\. ", "", comp)
      comparison_labels[i] <- paste(reference_name, "vs.", strategy_name)
    }
  }

  # Determine if we have multiple groups
  has_multiple_groups <- n_groups > 1

  # Pivot to wide format: WTP in rows, comparisons in columns
  if (has_multiple_groups) {
    # Multiple groups: pivot with group_comparison columns
    pivot_data <- ceac_data %>%
      mutate(probability_pct = .data$probability * 100) %>%
      pivot_wider(
        id_cols = "wtp",
        names_from = c("group", "strategy"),
        names_sep = "_",
        values_from = "probability_pct"
      ) %>%
      arrange(.data$wtp)
  } else {
    # Single group: simple structure
    pivot_data <- ceac_data %>%
      mutate(probability_pct = .data$probability * 100) %>%
      pivot_wider(
        id_cols = "wtp",
        names_from = "strategy",
        values_from = "probability_pct"
      ) %>%
      arrange(.data$wtp)
  }

  # Format WTP column as dollar amounts
  pivot_data <- pivot_data %>%
    mutate(wtp_display = scales::dollar(.data$wtp))

  # Format probability columns as percentage strings
  comparison_cols <- setdiff(colnames(pivot_data), c("wtp", "wtp_display"))
  for (col in comparison_cols) {
    if (is.numeric(pivot_data[[col]])) {
      pivot_data[[col]] <- format(
        round(pivot_data[[col]], decimals),
        nsmall = decimals,
        scientific = FALSE,
        trim = TRUE
      )
      # Add % symbol
      pivot_data[[col]] <- paste0(pivot_data[[col]], "%")
    }
  }

  # Build result columns based on mode
  if (has_multiple_groups) {
    # Build with spacers: [WTP] [spacer] [Group1 comparisons] [spacer] [Group2 comparisons] ...
    result_cols <- pivot_data[, "wtp_display", drop = FALSE]
    colnames(result_cols) <- "WTP"

    for (i in seq_along(groups_display)) {
      # Add spacer column
      spacer_col <- data.frame(rep("", nrow(pivot_data)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)

      # Add group's comparison columns
      grp <- groups_display[i]
      grp_cols <- pivot_data[, grepl(paste0("^", grp, "_"), colnames(pivot_data)), drop = FALSE]
      result_cols <- cbind(result_cols, grp_cols)
    }
  } else {
    # Simple mode: [WTP] [Comparison columns]
    result_cols <- pivot_data %>%
      select("wtp_display", all_of(comparisons_display))
    colnames(result_cols)[1] <- "WTP"
  }

  # Build headers structure
  headers <- list()

  if (has_multiple_groups) {
    # Two header rows
    # Row 1: Empty over WTP, spacers (no borders), group names spanning comparisons
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))

    for (i in seq_along(groups_display)) {
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      row1[[length(row1) + 1]] <- list(
        span = n_comparisons,
        text = groups_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Row 2: WTP, spacers (no borders), comparison labels
    row2 <- list()
    row2[[1]] <- list(span = 1, text = "WTP", borders = c(0, 0, 1, 0))

    for (i in seq_along(groups_display)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      for (j in seq_along(comparison_labels)) {
        row2[[length(row2) + 1]] <- list(span = 1, text = comparison_labels[j], borders = c(0, 0, 1, 0))
      }
    }
    headers[[2]] <- row2

    # Column alignments and widths
    column_alignments <- "left"  # WTP column
    column_widths <- NA

    for (i in seq_along(groups_display)) {
      column_alignments <- c(column_alignments, "center")  # spacer
      column_widths <- c(column_widths, 0.2)
      for (j in seq_len(n_comparisons)) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }
  } else {
    # Single header row: WTP + comparison labels
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "WTP", borders = c(1, 0, 1, 0))

    for (comp_label in comparison_labels) {
      row1[[length(row1) + 1]] <- list(
        span = 1,
        text = comp_label,
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Column alignments
    column_alignments <- c("left", rep("right", n_comparisons))
    column_widths <- c(NA, rep(NA, n_comparisons))
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = result_cols,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format Pairwise CEAC as Probability Table
#'
#' Creates a table showing probabilities from pairwise comparisons at selected
#' willingness-to-pay thresholds.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param intervention Reference strategy for comparison (intervention perspective: A vs. B, A vs. C).
#'   Mutually exclusive with comparator.
#' @param comparator Reference strategy for comparison (comparator perspective: B vs. A, C vs. A).
#'   Mutually exclusive with intervention.
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns.
#'   Default is c(0, 20000, 50000, 100000)
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (creates hierarchical table with group column)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param decimals Number of decimal places for percentages (default: 1)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' Pairwise comparisons calculate P(intervention > comparator) for each pair.
#' The table displays these probabilities at selected WTP thresholds.
#'
#' \strong{Intervention mode}: Shows P(intervention > each other strategy)
#'
#' \strong{Comparator mode}: Shows P(each other strategy > comparator)
#'
#' A probability > 50% indicates higher likelihood of being cost-effective
#' in the comparison.
#'
#' When multiple groups are present, the table includes a group column showing
#' results in a hierarchical structure (group -> strategy -> probabilities).
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Comparator perspective: Compare strategies to control
#' pairwise_ceac_table(psa_results, "total_qalys", "total_cost",
#'                     comparator = "control")
#'
#' # Intervention perspective: Compare new treatment to others
#' pairwise_ceac_table(psa_results, "total_qalys", "total_cost",
#'                     intervention = "new_treatment")
#'
#' # Custom WTP thresholds
#' pairwise_ceac_table(psa_results, "total_qalys", "total_cost",
#'                     comparator = "control",
#'                     wtp_thresholds = c(0, 30000, 50000, 75000, 100000))
#' }
#'
#' @export
pairwise_ceac_table <- function(results,
                                outcome_summary,
                                cost_summary,
                                intervention = NULL,
                                comparator = NULL,
                                wtp_thresholds = c(0, 20000, 50000, 100000),
                                group = "aggregated",
                                strategies = NULL,
                                discounted = FALSE,
                                decimals = 1,
                                font_size = 11,
                                table_format = c("kable", "flextable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_pairwise_ceac_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    intervention = intervention,
    comparator = comparator,
    wtp_thresholds = wtp_thresholds,
    group = group,
    strategies = strategies,
    discounted = discounted,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare EVPI Table Data
#'
#' Internal helper function that prepares EVPI data for rendering as a table.
#' Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as rows
#' @param group Group selection: "aggregated" uses aggregated results, specific
#'   group name filters to that group, NULL or "all" includes all groups
#'   (creates columns for each group)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param decimals Number of decimal places for values
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_evpi_table_data <- function(results,
                                    outcome_summary,
                                    cost_summary,
                                    wtp_thresholds = c(20000, 50000, 100000, 150000),
                                    group = "aggregated",
                                    strategies = NULL,
                                    discounted = FALSE,
                                    decimals = 0,
                                    font_size = 11) {

  # Calculate EVPI at specified WTP thresholds
  evpi_data <- calculate_evpi(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp_thresholds,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Get unique groups
  groups_display <- unique(evpi_data$group)

  # Reorder according to original model order from metadata
  if (!is.null(results$metadata$groups)) {
    group_order <- results$metadata$groups$display_name
    groups_display <- group_order[group_order %in% groups_display]
  }

  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1

  # Pivot to wide format: WTP in rows, groups in columns (if multiple groups)
  if (has_multiple_groups) {
    # Multiple groups: pivot with group columns
    pivot_data <- evpi_data %>%
      pivot_wider(
        id_cols = "wtp",
        names_from = "group",
        values_from = "evpi"
      ) %>%
      arrange(.data$wtp)
  } else {
    # Single group: simple structure
    pivot_data <- evpi_data %>%
      select("wtp", "evpi") %>%
      arrange(.data$wtp)
  }

  # Format WTP column as dollar amounts
  pivot_data <- pivot_data %>%
    mutate(wtp_display = scales::dollar(.data$wtp))

  # Format EVPI columns with proper decimals and dollar formatting
  evpi_cols <- setdiff(colnames(pivot_data), c("wtp", "wtp_display"))
  for (col in evpi_cols) {
    if (is.numeric(pivot_data[[col]])) {
      pivot_data[[col]] <- scales::dollar(
        round(pivot_data[[col]], decimals),
        accuracy = 10^(-decimals)
      )
    }
  }

  # Build result columns based on mode
  if (has_multiple_groups) {
    # Build with group columns: [WTP] [Group1] [Group2] ...
    result_cols <- pivot_data %>%
      select("wtp_display", all_of(groups_display))
    colnames(result_cols)[1] <- "WTP"
  } else {
    # Simple mode: [WTP] [EVPI]
    result_cols <- pivot_data %>%
      select("wtp_display", "evpi")
    colnames(result_cols) <- c("WTP", "EVPI")
  }

  # Build headers structure
  headers <- list()

  if (has_multiple_groups) {
    # Header row with group names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "WTP", borders = c(1, 0, 1, 0))

    for (grp in groups_display) {
      row1[[length(row1) + 1]] <- list(
        span = 1,
        text = grp,
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Column alignments
    column_alignments <- c("left", rep("right", n_groups))
    column_widths <- c(NA, rep(NA, n_groups))
  } else {
    # Single header row: WTP + EVPI
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "WTP", borders = c(1, 0, 1, 0))
    row1[[2]] <- list(span = 1, text = "EVPI", borders = c(1, 0, 1, 0))
    headers[[1]] <- row1

    # Column alignments
    column_alignments <- c("left", "right")
    column_widths <- c(NA, NA)
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = result_cols,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Format EVPI as Table
#'
#' Creates a table showing the Expected Value of Perfect Information (EVPI)
#' at selected willingness-to-pay thresholds.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as rows.
#'   Default is c(20000, 50000, 100000, 150000)
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (creates columns for each group)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param discounted Logical. Use discounted values?
#' @param decimals Number of decimal places for EVPI values (default: 0)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The EVPI table displays the Expected Value of Perfect Information at different
#' WTP thresholds. EVPI represents the maximum amount a decision-maker should be
#' willing to pay for perfect information about all uncertain parameters.
#'
#' EVPI provides an upper bound on the value of conducting further research to
#' reduce uncertainty. When EVPI = 0, the same strategy is optimal in all
#' simulations (no uncertainty affects the decision). Higher EVPI indicates
#' greater decision uncertainty.
#'
#' When multiple groups are present, the table includes a column for each group
#' showing EVPI values.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # EVPI table at default thresholds
#' evpi_table(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP thresholds
#' evpi_table(psa_results, "total_qalys", "total_cost",
#'            wtp_thresholds = c(0, 30000, 50000, 75000, 100000, 150000))
#' }
#'
#' @export
evpi_table <- function(results,
                       outcome_summary,
                       cost_summary,
                       wtp_thresholds = c(20000, 50000, 100000, 150000),
                       group = "aggregated",
                       strategies = NULL,
                       discounted = FALSE,
                       decimals = 0,
                       font_size = 11,
                       table_format = c("kable", "flextable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_evpi_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp_thresholds = wtp_thresholds,
    group = group,
    strategies = strategies,
    discounted = discounted,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}