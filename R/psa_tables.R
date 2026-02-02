#' Prepare Incremental CEAC Table Data
#'
#' Internal helper function that prepares incremental CEAC data for rendering as
#' a table. Extracts data preparation logic to enable multi-backend support.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param decimals Number of decimal places for percentages
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_incremental_ceac_table_data <- function(results,
                                                outcome_summary,
                                                cost_summary,
                                                wtp_thresholds = c(0, 20000, 50000, 100000),
                                                groups = "overall",
                                                strategies = NULL,
                                                decimals = 1,
                                                font_size = 11) {

  # Calculate incremental CEAC at specified WTP thresholds
  # Always uses discounted values (cost-effectiveness measure)
  ceac_data <- calculate_incremental_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp_thresholds,
    groups = groups,
    strategies = strategies
  )

  # Get unique strategies and groups
  strategies_display <- unique(ceac_data$strategy)
  groups_display <- unique(ceac_data$group)

  # Reorder according to original model order from metadata
  if (!is.null(results$metadata$strategies)) {
    strategy_order <- results$metadata$strategies$display_name
    strategies_display <- strategy_order[strategy_order %in% strategies_display]
  }
  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine if we have multiple groups
  has_multiple_groups <- n_groups > 1 || is.null(groups)

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

  # Format WTP column as dollar amounts
  pivot_data <- pivot_data %>%
    mutate(wtp_display = dollar(.data$wtp))

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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
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
#' CEAC calculations always use discounted values as this is a cost-effectiveness measure.
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
                                   groups = "overall",
                                   strategies = NULL,
                                   decimals = 1,
                                   font_size = 11,
                                   table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_incremental_ceac_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp_thresholds = wtp_thresholds,
    groups = groups,
    strategies = strategies,
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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include
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
                                          groups = "overall",
                                          strategies = NULL,
                                          outcome_decimals = 2,
                                          cost_decimals = 0,
                                          font_size = 11) {

  # Get PSA simulation data (always uses discounted values for CE-related summary)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Calculate summary statistics (expanded to include median, IQR, min, max)
  summary_stats <- psa_data %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(
      # Cost statistics
      mean_cost = mean(.data$cost, na.rm = TRUE),
      sd_cost = sd(.data$cost, na.rm = TRUE),
      ci_lower_cost = quantile(.data$cost, 0.025, na.rm = TRUE),
      ci_upper_cost = quantile(.data$cost, 0.975, na.rm = TRUE),
      median_cost = median(.data$cost, na.rm = TRUE),
      q25_cost = quantile(.data$cost, 0.25, na.rm = TRUE),
      q75_cost = quantile(.data$cost, 0.75, na.rm = TRUE),
      min_cost = min(.data$cost, na.rm = TRUE),
      max_cost = max(.data$cost, na.rm = TRUE),

      # Outcome statistics
      mean_outcome = mean(.data$outcome, na.rm = TRUE),
      sd_outcome = sd(.data$outcome, na.rm = TRUE),
      ci_lower_outcome = quantile(.data$outcome, 0.025, na.rm = TRUE),
      ci_upper_outcome = quantile(.data$outcome, 0.975, na.rm = TRUE),
      median_outcome = median(.data$outcome, na.rm = TRUE),
      q25_outcome = quantile(.data$outcome, 0.25, na.rm = TRUE),
      q75_outcome = quantile(.data$outcome, 0.75, na.rm = TRUE),
      min_outcome = min(.data$outcome, na.rm = TRUE),
      max_outcome = max(.data$outcome, na.rm = TRUE),

      .groups = "drop"
    )

  # Calculate P(CE) for each WTP threshold
  # Always uses discounted values (cost-effectiveness measure)
  pce_results <- list()
  for (wtp_val in pce_wtp) {
    ceac_at_wtp <- calculate_incremental_ceac(
      results = results,
      outcome_summary = outcome_summary,
      cost_summary = cost_summary,
      wtp = wtp_val,
      groups = groups,
      strategies = strategies
    ) %>%
      mutate(wtp = wtp_val) %>%
      select("strategy", "group", "wtp", "probability")

    pce_results[[length(pce_results) + 1]] <- ceac_at_wtp
  }
  pce_data <- bind_rows(pce_results)

  # Formatting helper functions (uses decimal places, not sigfigs)
  format_with_ci <- function(mean_val, lower, upper, decimals, use_comma = FALSE) {
    fmt <- function(v) {
      if (use_comma) {
        scales::comma(round(v, decimals), accuracy = 10^(-decimals))
      } else {
        format(round(v, decimals), nsmall = decimals, trim = TRUE)
      }
    }
    paste0(fmt(mean_val), " (", fmt(lower), " - ", fmt(upper), ")")
  }

  format_range <- function(min_val, max_val, decimals, use_comma = FALSE) {
    fmt <- function(v) {
      if (use_comma) {
        scales::comma(round(v, decimals), accuracy = 10^(-decimals))
      } else {
        format(round(v, decimals), nsmall = decimals, trim = TRUE)
      }
    }
    paste0(fmt(min_val), " - ", fmt(max_val))
  }

  format_numeric <- function(val, decimals, use_comma = FALSE) {
    if (use_comma) {
      scales::comma(round(val, decimals), accuracy = 10^(-decimals))
    } else {
      format(round(val, decimals), nsmall = decimals, trim = TRUE)
    }
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
  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1 || is.null(groups)

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
    wtp_label <- paste0("\u03bb = $", scales::comma(wtp_val))
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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
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
#' PSA summary calculations always use discounted values as this is a
#' cost-effectiveness measure.
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
                             groups = "overall",
                             strategies = NULL,
                             outcome_decimals = 2,
                             cost_decimals = 0,
                             font_size = 11,
                             table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_psa_summary_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    pce_wtp = pce_wtp,
    groups = groups,
    strategies = strategies,
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
#' @param interventions Reference strategies for comparison (intervention perspective).
#' @param comparators Reference strategies for comparison (comparator perspective).
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param decimals Number of decimal places for percentages
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_pairwise_ceac_table_data <- function(results,
                                             outcome_summary,
                                             cost_summary,
                                             interventions = NULL,
                                             comparators = NULL,
                                             wtp_thresholds = c(0, 20000, 50000, 100000),
                                             groups = "overall",
                                             decimals = 1,
                                             font_size = 11) {

  # Calculate pairwise CEAC at specified WTP thresholds
  # Always uses discounted values (cost-effectiveness measure)
  ceac_data <- calculate_pairwise_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    interventions = interventions,
    comparators = comparators,
    wtp = wtp_thresholds,
    groups = groups
  )

  # Get unique comparisons and groups
  comparisons_display <- unique(ceac_data$comparison)
  groups_display <- unique(ceac_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_comparisons <- length(comparisons_display)
  n_groups <- length(groups_display)

  # Determine if we have multiple groups
  has_multiple_groups <- n_groups > 1 || is.null(groups)

  # Use comparison labels directly (already in "X vs. Y" format)
  comparison_labels <- comparisons_display

  # Pivot to wide format: WTP in rows, comparisons in columns
  if (has_multiple_groups) {
    # Multiple groups: pivot with group_comparison columns
    pivot_data <- ceac_data %>%
      mutate(probability_pct = .data$probability * 100) %>%
      pivot_wider(
        id_cols = "wtp",
        names_from = c("group", "comparison"),
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
        names_from = "comparison",
        values_from = "probability_pct"
      ) %>%
      arrange(.data$wtp)
  }

  # Format WTP column as dollar amounts
  pivot_data <- pivot_data %>%
    mutate(wtp_display = dollar(.data$wtp))

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
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))

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
#' @param interventions Reference strategies for comparison (intervention perspective).
#'   Can be a single strategy or vector of strategies.
#' @param comparators Reference strategies for comparison (comparator perspective).
#'   Can be a single strategy or vector of strategies.
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as columns.
#'   Default is c(0, 20000, 50000, 100000)
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
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
#' At least one of interventions or comparators must be provided. If both are
#' provided, N×M explicit comparisons are generated.
#'
#' A probability > 50% indicates higher likelihood of being cost-effective
#' in the comparison.
#'
#' CEAC calculations always use discounted values as this is a cost-effectiveness measure.
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
#'                     comparators = "control")
#'
#' # Intervention perspective: Compare new treatment to others
#' pairwise_ceac_table(psa_results, "total_qalys", "total_cost",
#'                     interventions = "new_treatment")
#'
#' # Custom WTP thresholds
#' pairwise_ceac_table(psa_results, "total_qalys", "total_cost",
#'                     comparators = "control",
#'                     wtp_thresholds = c(0, 30000, 50000, 75000, 100000))
#' }
#'
#' @export
pairwise_ceac_table <- function(results,
                                outcome_summary,
                                cost_summary,
                                interventions = NULL,
                                comparators = NULL,
                                wtp_thresholds = c(0, 20000, 50000, 100000),
                                groups = "overall",
                                decimals = 1,
                                font_size = 11,
                                table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_pairwise_ceac_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    interventions = interventions,
    comparators = comparators,
    wtp_thresholds = wtp_thresholds,
    groups = groups,
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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param decimals Number of decimal places for values
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_evpi_table_data <- function(results,
                                    outcome_summary,
                                    cost_summary,
                                    wtp_thresholds = c(20000, 50000, 100000, 150000),
                                    groups = "overall",
                                    strategies = NULL,
                                    decimals = 0,
                                    font_size = 11) {

  # Calculate EVPI at specified WTP thresholds
  # Always uses discounted values (cost-effectiveness measure)
  evpi_data <- calculate_evpi(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp_thresholds,
    groups = groups,
    strategies = strategies
  )

  # Get unique groups
  groups_display <- unique(evpi_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1 || is.null(groups)

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
    mutate(wtp_display = dollar(.data$wtp))

  # Format EVPI columns with proper decimals and dollar formatting
  evpi_cols <- setdiff(colnames(pivot_data), c("wtp", "wtp_display"))
  for (col in evpi_cols) {
    if (is.numeric(pivot_data[[col]])) {
      pivot_data[[col]] <- dollar(
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
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
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
#' EVPI calculations always use discounted values as this is a cost-effectiveness measure.
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
                       groups = "overall",
                       strategies = NULL,
                       decimals = 0,
                       font_size = 11,
                       table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_evpi_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp_thresholds = wtp_thresholds,
    groups = groups,
    strategies = strategies,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare CE Quadrant Table Data
#'
#' Internal helper function that prepares cost-effectiveness plane quadrant
#' probabilities for rendering as a table. Shows the percentage of PSA simulations
#' falling into each quadrant relative to a comparator strategy.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param comparator Name of the comparator strategy
#' @param groups Group selection
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param decimals Number of decimal places for percentages
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_ce_quadrant_table_data <- function(results,
                                           outcome_summary,
                                           cost_summary,
                                           comparator,
                                           groups = "overall",
                                           strategies = NULL,
                                           decimals = 1,
                                           font_size = 11) {

  # Get PSA simulation data (always uses discounted values for CE-related analysis)
  psa_data <- get_psa_simulations(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Validate comparator exists
  all_strategies <- unique(psa_data$strategy)
  if (!comparator %in% all_strategies) {
    stop(sprintf("Comparator '%s' not found in results. Available: %s",
                 comparator, paste(all_strategies, collapse = ", ")))
  }

  # Get comparator data
  comparator_data <- psa_data %>%
    filter(.data$strategy == comparator) %>%
    select("simulation", "group", cost_comp = "cost", outcome_comp = "outcome")

  # Calculate incremental values for other strategies
  other_strategies <- setdiff(all_strategies, comparator)

  if (length(other_strategies) == 0) {
    stop("No other strategies to compare against comparator")
  }

  incremental_data <- psa_data %>%
    filter(.data$strategy != comparator) %>%
    left_join(comparator_data, by = c("simulation", "group")) %>%
    mutate(
      incr_cost = .data$cost - .data$cost_comp,
      incr_outcome = .data$outcome - .data$outcome_comp,
      quadrant = case_when(
        .data$incr_cost > 0 & .data$incr_outcome > 0 ~ "NE",
        .data$incr_cost <= 0 & .data$incr_outcome > 0 ~ "SE",
        .data$incr_cost <= 0 & .data$incr_outcome <= 0 ~ "SW",
        .data$incr_cost > 0 & .data$incr_outcome <= 0 ~ "NW"
      )
    )

  # Calculate quadrant percentages per strategy and group
  quadrant_counts <- incremental_data %>%
    group_by(.data$strategy, .data$group, .data$quadrant) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(.data$strategy, .data$group) %>%
    mutate(pct = .data$n / sum(.data$n) * 100) %>%
    ungroup()

  # Get unique strategies and groups
  strategies_display <- unique(quadrant_counts$strategy)
  groups_display <- unique(quadrant_counts$group)

  # Reorder according to original model order from metadata
  if (!is.null(results$metadata$strategies)) {
    strategy_order <- results$metadata$strategies$display_name
    strategies_display <- strategy_order[strategy_order %in% strategies_display]
  }
  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1

  # Pivot to wide format with quadrants as columns
  pivot_data <- quadrant_counts %>%
    select("strategy", "group", "quadrant", "pct") %>%
    pivot_wider(
      id_cols = c("strategy", "group"),
      names_from = "quadrant",
      values_from = "pct",
      values_fill = 0
    )

  # Ensure all quadrant columns exist
  for (q in c("NE", "SE", "SW", "NW")) {
    if (!q %in% names(pivot_data)) {
      pivot_data[[q]] <- 0
    }
  }

  # Format percentages
  format_pct <- function(x) {
    paste0(format(round(x, decimals), nsmall = decimals, trim = TRUE), "%")
  }

  # Build result table
  if (has_multiple_groups) {
    # Multiple groups: include group column
    result_data <- pivot_data %>%
      arrange(factor(.data$group, levels = groups_display),
              factor(.data$strategy, levels = strategies_display)) %>%
      transmute(
        Group = .data$group,
        Strategy = .data$strategy,
        `NE (%)` = format_pct(.data$NE),
        `SE (%)` = format_pct(.data$SE),
        `SW (%)` = format_pct(.data$SW),
        `NW (%)` = format_pct(.data$NW)
      )

    # Build headers
    headers <- list(list(
      list(span = 1, text = "Group", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Strategy", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "NE (%)", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "SE (%)", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "SW (%)", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "NW (%)", borders = c(1, 0, 1, 0))
    ))

    column_alignments <- c("left", "left", "right", "right", "right", "right")
    column_widths <- rep(NA, 6)

  } else {
    # Single group: no group column
    result_data <- pivot_data %>%
      arrange(factor(.data$strategy, levels = strategies_display)) %>%
      transmute(
        Strategy = .data$strategy,
        `NE (%)` = format_pct(.data$NE),
        `SE (%)` = format_pct(.data$SE),
        `SW (%)` = format_pct(.data$SW),
        `NW (%)` = format_pct(.data$NW)
      )

    # Build headers
    headers <- list(list(
      list(span = 1, text = "Strategy", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "NE (%)", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "SE (%)", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "SW (%)", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "NW (%)", borders = c(1, 0, 1, 0))
    ))

    column_alignments <- c("left", "right", "right", "right", "right")
    column_widths <- rep(NA, 5)
  }

  # Add footnote explaining quadrants
  footnote <- paste0(
    "Quadrants relative to ", comparator, ": ",
    "NE = costlier & more effective; ",
    "SE = cheaper & more effective (dominant); ",
    "SW = cheaper & less effective; ",
    "NW = costlier & less effective (dominated)"
  )

  create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(footnote = footnote),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' Cost-Effectiveness Quadrant Probabilities Table
#'
#' Creates a table showing the percentage of PSA simulations falling into each
#' quadrant of the cost-effectiveness plane relative to a comparator strategy.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param comparator Name of the comparator strategy
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param decimals Number of decimal places for percentages (default: 1)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The cost-effectiveness plane is divided into four quadrants:
#' \itemize{
#'   \item \strong{NE (Northeast)}: Costlier and more effective - trade-off region
#'   \item \strong{SE (Southeast)}: Cheaper and more effective - dominant
#'   \item \strong{SW (Southwest)}: Cheaper and less effective - trade-off region
#'   \item \strong{NW (Northwest)}: Costlier and less effective - dominated
#' }
#'
#' CE quadrant calculations always use discounted values as this is a
#' cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # CE quadrant probabilities relative to standard care
#' ce_quadrant_table(psa_results, "total_qalys", "total_cost",
#'                   comparator = "Standard Care")
#' }
#'
#' @export
ce_quadrant_table <- function(results,
                              outcome_summary,
                              cost_summary,
                              comparator,
                              groups = "overall",
                              strategies = NULL,
                              decimals = 1,
                              font_size = 11,
                              table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_ce_quadrant_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    comparator = comparator,
    groups = groups,
    strategies = strategies,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare PSA Parameters Table Data
#'
#' Internal helper function that prepares PSA sampled parameter statistics
#' for rendering as a table. Shows summary statistics for each sampled parameter.
#'
#' @param results A openqaly PSA results object
#' @param variables Character vector of variables to include (NULL for all)
#' @param group Single group name to filter parameters (NULL for first segment)
#' @param strategies Single strategy to filter parameters (NULL for first strategy)
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_psa_parameters_table_data <- function(results,
                                               variables = NULL,
                                               group = NULL,
                                               strategies = NULL,
                                               decimals = 3,
                                               font_size = 11) {

  # Get sampled parameters - use first group/strategy if not specified
  if (is.null(group)) {
    group <- results$segments$group[1]
  }
  if (is.null(strategies)) {
    strategies <- results$segments$strategy[1]
  }

  # Try to get sampled parameters
  param_data <- tryCatch({
    get_sampled_parameters(
      results = results,
      variables = variables,
      group = group,
      strategies = strategies
    )
  }, error = function(e) {
    # If no parameters sampled, return empty table
    NULL
  })

  if (is.null(param_data) || ncol(param_data) <= 1) {
    # No sampled parameters - create empty result
    result_data <- data.frame(
      Parameter = "No sampled parameters",
      Type = "",
      Mean = "",
      SD = "",
      Median = "",
      `2.5%` = "",
      `97.5%` = "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    headers <- list(list(
      list(span = 1, text = "Parameter", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Type", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Mean", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "SD", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "Median", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "2.5%", borders = c(1, 0, 1, 0)),
      list(span = 1, text = "97.5%", borders = c(1, 0, 1, 0))
    ))

    return(create_simple_table_spec(
      headers = headers,
      data = result_data,
      column_alignments = c("left", "left", rep("right", 5)),
      column_widths = rep(NA, 7),
      special_rows = list(),
      font_size = font_size,
      font_family = "Helvetica"
    ))
  }

  # Calculate summary statistics for each parameter
  param_names <- setdiff(names(param_data), "simulation")
  stats_list <- lapply(param_names, function(param) {
    vals <- param_data[[param]]
    vals <- vals[!is.na(vals)]

    if (length(vals) == 0) {
      return(list(
        Parameter = param,
        Type = "Scalar",
        Mean = NA,
        SD = NA,
        Median = NA,
        `2.5%` = NA,
        `97.5%` = NA
      ))
    }

    list(
      Parameter = param,
      Type = "Scalar",
      Mean = mean(vals),
      SD = sd(vals),
      Median = median(vals),
      `2.5%` = quantile(vals, 0.025),
      `97.5%` = quantile(vals, 0.975)
    )
  })

  stats_df <- bind_rows(stats_list)

  # Check for bootstrap tables in model (multivariate sampling with bootstrap)
  # These are stored differently - they're tables, not scalars
  # Add rows indicating bootstrap tables if present
  if (!is.null(results$metadata) && "tables" %in% names(results$metadata)) {
    # Check if any tables have bootstrap sampling configured
    # This is a placeholder - actual bootstrap detection depends on model structure
    bootstrap_tables <- character(0)

    if (length(bootstrap_tables) > 0) {
      bootstrap_rows <- data.frame(
        Parameter = bootstrap_tables,
        Type = rep("Bootstrap", length(bootstrap_tables)),
        Mean = rep(NA, length(bootstrap_tables)),
        SD = rep(NA, length(bootstrap_tables)),
        Median = rep(NA, length(bootstrap_tables)),
        `2.5%` = rep(NA, length(bootstrap_tables)),
        `97.5%` = rep(NA, length(bootstrap_tables)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      stats_df <- bind_rows(stats_df, bootstrap_rows)
    }
  }

  # Format numeric values
  format_num <- function(x) {
    if (is.na(x)) return("\u2014")  # em-dash
    format(round(x, decimals), nsmall = decimals, trim = TRUE, scientific = FALSE)
  }

  result_data <- stats_df %>%
    mutate(
      Mean = sapply(.data$Mean, format_num),
      SD = sapply(.data$SD, format_num),
      Median = sapply(.data$Median, format_num),
      `2.5%` = sapply(.data$`2.5%`, format_num),
      `97.5%` = sapply(.data$`97.5%`, format_num)
    )

  # Build headers
  headers <- list(list(
    list(span = 1, text = "Parameter", borders = c(1, 0, 1, 0)),
    list(span = 1, text = "Type", borders = c(1, 0, 1, 0)),
    list(span = 1, text = "Mean", borders = c(1, 0, 1, 0)),
    list(span = 1, text = "SD", borders = c(1, 0, 1, 0)),
    list(span = 1, text = "Median", borders = c(1, 0, 1, 0)),
    list(span = 1, text = "2.5%", borders = c(1, 0, 1, 0)),
    list(span = 1, text = "97.5%", borders = c(1, 0, 1, 0))
  ))

  create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = c("left", "left", rep("right", 5)),
    column_widths = rep(NA, 7),
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' PSA Sampled Parameters Table
#'
#' Creates a table showing summary statistics for sampled PSA parameters.
#' Displays mean, SD, median, and 95% credible interval for each parameter.
#'
#' @param results A openqaly PSA results object
#' @param variables Character vector of variables to include (NULL for all)
#' @param group Single group name to filter parameters (NULL for first segment)
#' @param strategies Single strategy to filter parameters (NULL for first strategy)
#' @param decimals Number of decimal places (default: 3)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' This table summarizes the distributions of sampled parameters used in
#' probabilistic sensitivity analysis. For each parameter, it shows:
#' \itemize{
#'   \item Mean: Average sampled value
#'   \item SD: Standard deviation of sampled values
#'   \item Median: Middle sampled value
#'   \item 2.5%: Lower bound of 95% credible interval
#'   \item 97.5%: Upper bound of 95% credible interval
#' }
#'
#' Bootstrap tables (patient-level data resampling) are indicated with
#' "Bootstrap" type and dashes for statistics.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # All sampled parameters
#' psa_parameters_table(psa_results)
#'
#' # Specific parameters only
#' psa_parameters_table(psa_results, variables = c("p_response", "c_treatment"))
#' }
#'
#' @export
psa_parameters_table <- function(results,
                                  variables = NULL,
                                  group = NULL,
                                  strategies = NULL,
                                  decimals = 3,
                                  font_size = 11,
                                  table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_psa_parameters_table_data(
    results = results,
    variables = variables,
    group = group,
    strategies = strategies,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare PSA Outcomes Table Data
#'
#' Internal helper function that prepares PSA outcome simulation statistics
#' for rendering. Creates a transposed table with strategies/comparisons as
#' columns and statistics as rows.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param discounted Logical. Use discounted outcome values? Default TRUE.
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_psa_outcomes_table_data <- function(results,
                                            outcome_summary,
                                            groups = "overall",
                                            strategies = NULL,
                                            interventions = NULL,
                                            comparators = NULL,
                                            discounted = TRUE,
                                            decimals = 2,
                                            font_size = 11) {

  # Validate mutual exclusivity
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("Cannot specify 'strategies' together with 'interventions' or 'comparators'. ",
         "Use either 'strategies' for absolute values, or 'interventions'/'comparators' for differences.")
  }

  # Default to all strategies if nothing specified
  if (is.null(strategies) && is.null(interventions) && is.null(comparators)) {
    # Get all available strategies
    all_strats <- unique(results$aggregated$strategy)
    if (!is.null(results$metadata$strategies)) {
      strategies <- results$metadata$strategies$display_name
    } else {
      strategies <- all_strats
    }
  }

  # Get PSA outcome simulations using existing helper
  outcome_data <- get_psa_outcome_simulations(
    results = results,
    outcome_summary = outcome_summary,
    interventions = interventions,
    comparators = comparators,
    groups = groups,
    strategies = strategies,
    discounted = discounted
  )

  # Determine if absolute mode (has 'strategy') or difference mode (has 'comparison')
  is_difference_mode <- "comparison" %in% names(outcome_data)
  col_var <- if (is_difference_mode) "comparison" else "strategy"

  # Calculate summary statistics
  summary_stats <- outcome_data %>%
    group_by(across(all_of(c(col_var, "group")))) %>%
    summarize(
      mean_val = mean(.data$outcome, na.rm = TRUE),
      sd_val = sd(.data$outcome, na.rm = TRUE),
      ci_lower = quantile(.data$outcome, 0.025, na.rm = TRUE),
      ci_upper = quantile(.data$outcome, 0.975, na.rm = TRUE),
      median_val = median(.data$outcome, na.rm = TRUE),
      q25 = quantile(.data$outcome, 0.25, na.rm = TRUE),
      q75 = quantile(.data$outcome, 0.75, na.rm = TRUE),
      min_val = min(.data$outcome, na.rm = TRUE),
      max_val = max(.data$outcome, na.rm = TRUE),
      .groups = "drop"
    )

  # Get unique column values and groups
  col_values <- unique(summary_stats[[col_var]])
  groups_display <- unique(summary_stats$group)

  # Preserve factor levels if present
  if (is.factor(summary_stats[[col_var]])) {
    col_values <- levels(summary_stats[[col_var]])
  }
  if (is.factor(summary_stats$group)) {
    groups_display <- levels(summary_stats$group)
  } else {
    groups_display <- get_group_order(groups_display, results$metadata)
  }

  n_cols <- length(col_values)
  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1 || is.null(groups)

  # Formatting helper functions
  format_with_ci <- function(mean_val, lower, upper, decs) {
    fmt <- function(v) {
      format(round(v, decs), nsmall = decs, trim = TRUE)
    }
    paste0(fmt(mean_val), " (", fmt(lower), " - ", fmt(upper), ")")
  }

  format_range <- function(min_val, max_val, decs) {
    fmt <- function(v) {
      format(round(v, decs), nsmall = decs, trim = TRUE)
    }
    paste0(fmt(min_val), " - ", fmt(max_val))
  }

  format_numeric <- function(val, decs) {
    format(round(val, decs), nsmall = decs, trim = TRUE)
  }

  # Row labels
  row_labels <- c("Mean (95% CI)", "SD", "Median (IQR)", "Range")

  # Build data for each column value and group
  row_data_list <- list()

  for (col_val in col_values) {
    for (grp in groups_display) {
      stat_row <- summary_stats %>%
        filter(.data[[col_var]] == col_val, .data$group == grp)

      if (nrow(stat_row) == 0) next
      stat_row <- stat_row[1, ]

      values_col <- c(
        format_with_ci(stat_row$mean_val, stat_row$ci_lower,
                       stat_row$ci_upper, decimals),
        format_numeric(stat_row$sd_val, decimals),
        format_with_ci(stat_row$median_val, stat_row$q25,
                       stat_row$q75, decimals),
        format_range(stat_row$min_val, stat_row$max_val, decimals)
      )

      row_data_list[[paste0(col_val, "_", grp)]] <- tibble(
        row_label = row_labels,
        col_value = col_val,
        group = grp,
        value = values_col
      )
    }
  }

  row_data_long <- bind_rows(row_data_list)

  # Build table structure based on mode
  if (!has_multiple_groups) {
    # Single group mode: [Row Label, ColVal1, ColVal2, ...]
    pivot_data <- row_data_long %>%
      pivot_wider(
        names_from = "col_value",
        values_from = "value",
        id_cols = "row_label"
      ) %>%
      mutate(row_label = factor(.data$row_label, levels = row_labels)) %>%
      arrange(.data$row_label) %>%
      mutate(row_label = as.character(.data$row_label))

    # Reorder columns to match col_values order
    pivot_data <- pivot_data %>%
      select(" " := "row_label", all_of(as.character(col_values)))

    # Build headers
    headers <- list()
    row1 <- list(list(span = 1, text = "", borders = c(1, 0, 1, 0)))
    for (col_val in col_values) {
      row1[[length(row1) + 1]] <- list(span = 1, text = as.character(col_val), borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_cols))
    column_widths <- rep(NA, n_cols + 1)

  } else {
    # Multiple groups mode: [Row Label, spacer, Group1-Col1, Group1-Col2, spacer, Group2-Col1, ...]
    result_cols <- data.frame(row_label = row_labels, stringsAsFactors = FALSE)
    colnames(result_cols) <- " "

    for (i in seq_along(groups_display)) {
      # Add spacer column
      spacer_col <- data.frame(rep("", length(row_labels)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)

      # Add group's column values
      grp <- groups_display[i]
      for (col_val in col_values) {
        col_data <- row_data_long %>%
          filter(.data$col_value == col_val, .data$group == grp) %>%
          mutate(row_label = factor(.data$row_label, levels = row_labels)) %>%
          arrange(.data$row_label) %>%
          pull(.data$value)

        if (length(col_data) == 0) {
          col_data <- rep("", length(row_labels))
        }

        col_df <- data.frame(col_data, stringsAsFactors = FALSE)
        names(col_df) <- paste0(grp, "_", col_val)
        result_cols <- cbind(result_cols, col_df)
      }
    }

    pivot_data <- as_tibble(result_cols)

    # Build two-level headers
    headers <- list()

    # Row 1: Group names spanning columns
    row1 <- list(list(span = 1, text = "", borders = c(1, 0, 1, 0)))
    for (i in seq_along(groups_display)) {
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      row1[[length(row1) + 1]] <- list(
        span = n_cols,
        text = groups_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Row 2: Column value names
    row2 <- list(list(span = 1, text = "", borders = c(0, 0, 1, 0)))
    for (i in seq_along(groups_display)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      for (col_val in col_values) {
        row2[[length(row2) + 1]] <- list(span = 1, text = as.character(col_val), borders = c(0, 0, 1, 0))
      }
    }
    headers[[2]] <- row2

    # Column alignments and widths
    column_alignments <- "left"  # Row label column
    column_widths <- NA

    for (i in seq_along(groups_display)) {
      column_alignments <- c(column_alignments, "center")  # spacer
      column_widths <- c(column_widths, 0.2)
      for (j in seq_len(n_cols)) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = pivot_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' PSA Outcomes Table
#'
#' Creates a table showing summary statistics for outcome values from PSA
#' simulations. Displays mean, SD, median (IQR), and range for each strategy
#' or comparison.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary (e.g., "qalys")
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all).
#'   Cannot be used with interventions/comparators.
#' @param interventions Character vector of reference strategies for intervention
#'   perspective. Use for outcome differences. Cannot be used with strategies.
#' @param comparators Character vector of reference strategies for comparator
#'   perspective. Use for outcome differences. Cannot be used with strategies.
#' @param discounted Logical. Use discounted outcome values? Default TRUE.
#' @param decimals Number of decimal places (default: 2)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The table displays summary statistics for each strategy (absolute mode) or
#' comparison (difference mode). Statistics include:
#' \itemize{
#'   \item Mean (95% CI): Mean outcome with 2.5th and 97.5th percentiles
#'   \item SD: Standard deviation
#'   \item Median (IQR): Median with interquartile range
#'   \item Range: Minimum to maximum values
#' }
#'
#' Use \code{strategies} for absolute outcome values per strategy, or
#' \code{interventions}/\code{comparators} for outcome differences between pairs.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_markov", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Outcomes table for all strategies
#' psa_outcomes_table(psa_results, "qalys")
#'
#' # Outcome differences relative to comparator
#' psa_outcomes_table(psa_results, "qalys", comparators = "seritinib")
#'
#' # Undiscounted outcomes by group
#' psa_outcomes_table(psa_results, "qalys", discounted = FALSE, groups = "all_groups")
#' }
#'
#' @export
psa_outcomes_table <- function(results,
                               outcome_summary,
                               groups = "overall",
                               strategies = NULL,
                               interventions = NULL,
                               comparators = NULL,
                               discounted = TRUE,
                               decimals = 2,
                               font_size = 11,
                               table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_psa_outcomes_table_data(
    results = results,
    outcome_summary = outcome_summary,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare PSA NMB Table Data
#'
#' Internal helper function that prepares PSA incremental NMB statistics
#' for rendering. Creates a transposed table with comparisons as columns
#' and statistics as rows.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param wtp Willingness-to-pay threshold
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param interventions Character vector of reference strategies for intervention perspective
#' @param comparators Character vector of reference strategies for comparator perspective
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_psa_nmb_table_data <- function(results,
                                       outcome_summary,
                                       cost_summary,
                                       wtp,
                                       groups = "overall",
                                       interventions = NULL,
                                       comparators = NULL,
                                       decimals = 0,
                                       font_size = 11) {

  # Validate interventions/comparators - at least one required
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided. ",
         "NMB is calculated as differences between strategies.")
  }

  # Get PSA simulation data (always uses discounted for NMB)
  psa_data <- get_psa_simulations(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = NULL  # Get all strategies for comparison
  )

  # Get all strategies
  all_strategies <- unique(psa_data$strategy)

  # Helper function to resolve strategy names
  resolve_strategy <- function(strat_name) {
    if (strat_name %in% all_strategies) {
      return(strat_name)
    }
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == strat_name]
      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        return(matched_display[1])
      }
    }
    stop(sprintf("Strategy '%s' not found in results", strat_name))
  }

  # Resolve user-provided interventions/comparators to display names
  if (!is.null(interventions)) {
    interventions <- sapply(interventions, resolve_strategy, USE.NAMES = FALSE)
  }
  if (!is.null(comparators)) {
    comparators <- sapply(comparators, resolve_strategy, USE.NAMES = FALSE)
  }

  # Determine comparison pairs
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N x M explicit comparisons
    for (int_strat in interventions) {
      for (comp_strat in comparators) {
        if (int_strat != comp_strat) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = int_strat,
            comparator = comp_strat
          )
        }
      }
    }
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }
  } else if (!is.null(interventions)) {
    # Intervention only: each intervention vs all others
    for (int_strat in interventions) {
      other_strategies <- setdiff(all_strategies, int_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = other
        )
      }
    }
  } else {
    # Comparator only: all others vs each comparator
    for (comp_strat in comparators) {
      other_strategies <- setdiff(all_strategies, comp_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = other,
          comparator = comp_strat
        )
      }
    }
  }

  # Calculate incremental NMB for each comparison pair
  nmb_data_list <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator
    comp_label <- paste0(int_strat, " vs. ", comp_strat)

    # Get data for intervention and comparator
    int_data <- psa_data %>%
      filter(.data$strategy == int_strat) %>%
      select("simulation", "group", int_cost = "cost", int_outcome = "outcome")

    comp_data <- psa_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("simulation", "group", comp_cost = "cost", comp_outcome = "outcome")

    # Join and calculate incremental NMB
    pair_data <- int_data %>%
      inner_join(comp_data, by = c("simulation", "group")) %>%
      mutate(
        d_outcome = .data$int_outcome - .data$comp_outcome,
        d_cost = .data$int_cost - .data$comp_cost,
        nmb = .data$d_outcome * wtp - .data$d_cost,
        comparison = comp_label
      ) %>%
      select("simulation", "group", "comparison", "nmb")

    nmb_data_list[[length(nmb_data_list) + 1]] <- pair_data
  }

  nmb_data <- bind_rows(nmb_data_list)

  # Calculate summary statistics
  summary_stats <- nmb_data %>%
    group_by(.data$comparison, .data$group) %>%
    summarize(
      mean_val = mean(.data$nmb, na.rm = TRUE),
      sd_val = sd(.data$nmb, na.rm = TRUE),
      ci_lower = quantile(.data$nmb, 0.025, na.rm = TRUE),
      ci_upper = quantile(.data$nmb, 0.975, na.rm = TRUE),
      median_val = median(.data$nmb, na.rm = TRUE),
      q25 = quantile(.data$nmb, 0.25, na.rm = TRUE),
      q75 = quantile(.data$nmb, 0.75, na.rm = TRUE),
      min_val = min(.data$nmb, na.rm = TRUE),
      max_val = max(.data$nmb, na.rm = TRUE),
      p_positive = mean(.data$nmb > 0, na.rm = TRUE),
      .groups = "drop"
    )

  # Get unique comparisons and groups
  comparisons_display <- unique(summary_stats$comparison)
  groups_display <- unique(summary_stats$group)

  # Preserve order from comparison_pairs
  comparisons_display <- sapply(comparison_pairs, function(p) {
    paste0(p$intervention, " vs. ", p$comparator)
  })

  # Apply group ordering
  groups_display <- get_group_order(groups_display, results$metadata)

  n_comparisons <- length(comparisons_display)
  n_groups <- length(groups_display)
  has_multiple_groups <- n_groups > 1 || is.null(groups)

  # Formatting helper functions (with comma formatting for monetary values)
  format_with_ci <- function(mean_val, lower, upper, decs) {
    fmt <- function(v) {
      scales::comma(round(v, decs), accuracy = 10^(-decs))
    }
    paste0(fmt(mean_val), " (", fmt(lower), " - ", fmt(upper), ")")
  }

  format_range <- function(min_val, max_val, decs) {
    fmt <- function(v) {
      scales::comma(round(v, decs), accuracy = 10^(-decs))
    }
    paste0(fmt(min_val), " - ", fmt(max_val))
  }

  format_numeric <- function(val, decs) {
    scales::comma(round(val, decs), accuracy = 10^(-decs))
  }

  format_pct <- function(val) {
    paste0(format(round(val * 100, 1), nsmall = 1), "%")
  }

  # Row labels
  row_labels <- c("Mean (95% CI)", "SD", "Median (IQR)", "Range", "P(NMB > 0)")

  # Build data for each comparison and group
  row_data_list <- list()

  for (comp in comparisons_display) {
    for (grp in groups_display) {
      stat_row <- summary_stats %>%
        filter(.data$comparison == comp, .data$group == grp)

      if (nrow(stat_row) == 0) next
      stat_row <- stat_row[1, ]

      values_col <- c(
        format_with_ci(stat_row$mean_val, stat_row$ci_lower,
                       stat_row$ci_upper, decimals),
        format_numeric(stat_row$sd_val, decimals),
        format_with_ci(stat_row$median_val, stat_row$q25,
                       stat_row$q75, decimals),
        format_range(stat_row$min_val, stat_row$max_val, decimals),
        format_pct(stat_row$p_positive)
      )

      row_data_list[[paste0(comp, "_", grp)]] <- tibble(
        row_label = row_labels,
        comparison = comp,
        group = grp,
        value = values_col
      )
    }
  }

  row_data_long <- bind_rows(row_data_list)

  # Build table structure based on mode
  if (!has_multiple_groups) {
    # Single group mode: [Row Label, Comparison1, Comparison2, ...]
    pivot_data <- row_data_long %>%
      pivot_wider(
        names_from = "comparison",
        values_from = "value",
        id_cols = "row_label"
      ) %>%
      mutate(row_label = factor(.data$row_label, levels = row_labels)) %>%
      arrange(.data$row_label) %>%
      mutate(row_label = as.character(.data$row_label))

    # Reorder columns to match comparisons order
    pivot_data <- pivot_data %>%
      select(" " := "row_label", all_of(comparisons_display))

    # Build headers
    headers <- list()
    row1 <- list(list(span = 1, text = "", borders = c(1, 0, 1, 0)))
    for (comp in comparisons_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = comp, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_comparisons))
    column_widths <- rep(NA, n_comparisons + 1)

  } else {
    # Multiple groups mode
    result_cols <- data.frame(row_label = row_labels, stringsAsFactors = FALSE)
    colnames(result_cols) <- " "

    for (i in seq_along(groups_display)) {
      # Add spacer column
      spacer_col <- data.frame(rep("", length(row_labels)), stringsAsFactors = FALSE)
      names(spacer_col) <- paste0("spacer_", i)
      result_cols <- cbind(result_cols, spacer_col)

      # Add group's comparison columns
      grp <- groups_display[i]
      for (comp in comparisons_display) {
        comp_data <- row_data_long %>%
          filter(.data$comparison == comp, .data$group == grp) %>%
          mutate(row_label = factor(.data$row_label, levels = row_labels)) %>%
          arrange(.data$row_label) %>%
          pull(.data$value)

        if (length(comp_data) == 0) {
          comp_data <- rep("", length(row_labels))
        }

        col_df <- data.frame(comp_data, stringsAsFactors = FALSE)
        names(col_df) <- paste0(grp, "_", comp)
        result_cols <- cbind(result_cols, col_df)
      }
    }

    pivot_data <- as_tibble(result_cols)

    # Build two-level headers
    headers <- list()

    # Row 1: Group names spanning columns
    row1 <- list(list(span = 1, text = "", borders = c(1, 0, 1, 0)))
    for (i in seq_along(groups_display)) {
      row1[[length(row1) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      row1[[length(row1) + 1]] <- list(
        span = n_comparisons,
        text = groups_display[i],
        borders = c(1, 0, 1, 0)
      )
    }
    headers[[1]] <- row1

    # Row 2: Comparison names
    row2 <- list(list(span = 1, text = "", borders = c(0, 0, 1, 0)))
    for (i in seq_along(groups_display)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "", borders = c(0, 0, 0, 0))  # spacer
      for (comp in comparisons_display) {
        row2[[length(row2) + 1]] <- list(span = 1, text = comp, borders = c(0, 0, 1, 0))
      }
    }
    headers[[2]] <- row2

    # Column alignments and widths
    column_alignments <- "left"  # Row label column
    column_widths <- NA

    for (i in seq_along(groups_display)) {
      column_alignments <- c(column_alignments, "center")  # spacer
      column_widths <- c(column_widths, 0.2)
      for (j in seq_len(n_comparisons)) {
        column_alignments <- c(column_alignments, "right")
        column_widths <- c(column_widths, NA)
      }
    }
  }

  # Return clean spec
  create_simple_table_spec(
    headers = headers,
    data = pivot_data,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = list(),
    font_size = font_size,
    font_family = "Helvetica"
  )
}


#' PSA Incremental NMB Table
#'
#' Creates a table showing summary statistics for incremental net monetary
#' benefit (NMB) from PSA simulations. Displays mean, SD, median (IQR), range,
#' and probability of positive NMB for each comparison.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary (e.g., "qalys")
#' @param cost_summary Name of the cost summary (e.g., "costs")
#' @param wtp Willingness-to-pay threshold for NMB calculation
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param interventions Character vector of reference strategies for intervention
#'   perspective. At least one of interventions or comparators required.
#' @param comparators Character vector of reference strategies for comparator
#'   perspective. At least one of interventions or comparators required.
#' @param decimals Number of decimal places (default: 0, appropriate for monetary values)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The table displays summary statistics for incremental NMB for each pairwise
#' comparison. Statistics include:
#' \itemize{
#'   \item Mean (95% CI): Mean incremental NMB with 2.5th and 97.5th percentiles
#'   \item SD: Standard deviation
#'   \item Median (IQR): Median with interquartile range
#'   \item Range: Minimum to maximum values
#'   \item P(NMB > 0): Probability that incremental NMB is positive (i.e., the
#'     intervention is cost-effective at the given WTP)
#' }
#'
#' Incremental NMB is calculated as: (outcome_intervention - outcome_comparator) * WTP -
#' (cost_intervention - cost_comparator). A positive NMB indicates the intervention
#' is cost-effective at the given WTP threshold.
#'
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_markov", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # NMB relative to comparator
#' psa_nmb_table(psa_results, "qalys", "costs", wtp = 50000, comparators = "seritinib")
#'
#' # NMB for specific intervention
#' psa_nmb_table(psa_results, "qalys", "costs", wtp = 100000, interventions = "volantor")
#'
#' # NMB by group
#' psa_nmb_table(psa_results, "qalys", "costs", wtp = 50000,
#'               comparators = "seritinib", groups = "all_groups")
#' }
#'
#' @export
psa_nmb_table <- function(results,
                          outcome_summary,
                          cost_summary,
                          wtp,
                          groups = "overall",
                          interventions = NULL,
                          comparators = NULL,
                          decimals = 0,
                          font_size = 11,
                          table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_psa_nmb_table_data(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}