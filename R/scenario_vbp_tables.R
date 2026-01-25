#' Scenario VBP Table Functions
#'
#' Functions for creating VBP tables from scenario analysis results.
#'
#' @name scenario_vbp_tables
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows distinct
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
NULL

#' Prepare Scenario VBP Table Data
#'
#' Internal helper function that prepares scenario VBP data for table rendering.
#'
#' @param results A openqaly scenario+VBP results object
#' @param wtp Willingness-to-pay threshold
#' @param groups Group selection
#' @param comparators Comparator strategies
#' @param decimals Number of decimal places
#' @param font_size Font size
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_scenario_vbp_table_data <- function(results,
                                             wtp,
                                             groups = "overall",
                                             comparators = "all",
                                             decimals = 0,
                                             font_size = 11) {

  # Prepare bar data (reuse logic)
  vbp_data <- prepare_scenario_vbp_bar_data(results, wtp, groups, comparators)

  if (nrow(vbp_data) == 0) {
    stop("No VBP data available for the specified parameters", call. = FALSE)
  }

  # Get unique comparators and groups
  comparators_display <- unique(vbp_data$comparator)
  groups_display <- unique(vbp_data$group)
  groups_display <- get_group_order(groups_display, results$metadata)

  n_comparators <- length(comparators_display)
  n_groups <- length(groups_display)

  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  if (mode == "single_group") {
    # Pivot to wide format with comparator columns
    result_data <- vbp_data %>%
      mutate(
        sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id))
      ) %>%
      arrange(.data$sort_order) %>%
      select("scenario_name", "comparator", "value") %>%
      pivot_wider(
        names_from = "comparator",
        values_from = "value"
      )

    # Format numeric columns
    for (col in comparators_display) {
      if (is.numeric(result_data[[col]])) {
        rounded_vals <- round(result_data[[col]], decimals)
        result_data[[col]] <- format(rounded_vals, big.mark = ",", nsmall = decimals, scientific = FALSE)
      }
    }

    names(result_data)[1] <- "Scenario"

    # Headers
    headers <- list()
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (comp in comparators_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = paste0("vs. ", comp), borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_comparators))
    column_widths <- rep(NA, ncol(result_data))
    special_rows <- list()

  } else {
    # Multi-group mode
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Group header row
      group_row <- tibble(scenario_name = grp)
      for (comp in comparators_display) {
        group_row[[comp]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      # Scenario rows
      grp_data <- vbp_data %>%
        filter(.data$group == grp) %>%
        mutate(
          sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id))
        ) %>%
        arrange(.data$sort_order) %>%
        select("scenario_name", "comparator", "value") %>%
        pivot_wider(
          names_from = "comparator",
          values_from = "value"
        )

      # Format numeric columns
      for (col in comparators_display) {
        if (is.numeric(grp_data[[col]])) {
          rounded_vals <- round(grp_data[[col]], decimals)
          grp_data[[col]] <- format(rounded_vals, big.mark = ",", nsmall = decimals, scientific = FALSE)
        }
      }

      n_scenario_rows <- nrow(grp_data)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_scenario_rows))

      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + n_scenario_rows
    }

    names(result_data)[1] <- "Scenario"

    # Headers
    headers <- list()
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (comp in comparators_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = paste0("vs. ", comp), borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_comparators))
    column_widths <- rep(NA, ncol(result_data))

    special_rows <- list(
      group_header_rows = group_header_rows,
      indented_rows = indented_rows
    )
  }

  create_simple_table_spec(
    data = result_data,
    headers = headers,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size
  )
}


#' Scenario VBP Table
#'
#' Creates a table showing value-based prices for each scenario at a given
#' willingness-to-pay threshold.
#'
#' @param results A openqaly scenario+VBP results object (output from run_scenario with VBP enabled)
#' @param wtp Willingness-to-pay threshold. If NULL, uses default WTP from model metadata.
#' @param groups Group selection: "overall" (default), "all", "all_groups", or specific group names
#' @param comparators Comparator strategies: "all" (default) or specific comparator names
#' @param decimals Number of decimal places (default: 0)
#' @param font_size Font size for rendering (default: 11)
#'
#' @return A gt table object
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model,
#'   vbp_price_variable = "cost_tx",
#'   vbp_intervention = "treatment"
#' )
#'
#' # VBP table
#' scenario_vbp_table(results, wtp = 50000)
#' }
#'
#' @export
scenario_vbp_table <- function(results,
                                wtp = NULL,
                                groups = "overall",
                                comparators = "all",
                                decimals = 0,
                                font_size = 11) {

  # Get WTP if not provided
  if (is.null(wtp)) {
    if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
      outcome_summary <- results$vbp_spec$outcome_summary
      outcome_meta <- results$metadata$summaries %>%
        filter(.data$name == outcome_summary)
      if (nrow(outcome_meta) > 0 && !is.na(outcome_meta$wtp[1])) {
        wtp <- outcome_meta$wtp[1]
      }
    }
    if (is.null(wtp)) {
      stop("WTP not found in model metadata. Provide explicit wtp parameter.",
           call. = FALSE)
    }
  }

  table_spec <- prepare_scenario_vbp_table_data(
    results,
    wtp = wtp,
    groups = groups,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  render_table(table_spec)
}
