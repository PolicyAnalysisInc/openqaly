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

  # Map summary names for column headers
  outcome_label <- outcome_summary
  cost_label <- cost_summary
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    outcome_label <- map_names(outcome_summary, results$metadata$summaries, "display_name")
    cost_label <- map_names(cost_summary, results$metadata$summaries, "display_name")
  }

  # Format ICER column using print.icer() logic
  format_icer <- function(icer_values, digits = decimals) {
    fmt_num <- function(v) {
      scales::comma(round(v, digits = digits))
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
        scales::comma(round(v, digits), accuracy = 10^(-digits))
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
    names(result_cols) <- c("Strategy", "Comparator", cost_label, outcome_label,
                            paste0("&#916; ", cost_label), paste0("&#916; ", outcome_label), "ICER")

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
    names(result_cols) <- c("Strategy", "Comparator", cost_label, outcome_label,
                            paste0("&#916; ", cost_label), paste0("&#916; ", outcome_label), "ICER")

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
    interventions = interventions,
    comparators = comparators
  )

  # Map summary names for column headers
  outcome_label <- outcome_summary
  cost_label <- cost_summary
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    outcome_label <- map_names(outcome_summary, results$metadata$summaries, "display_name")
    cost_label <- map_names(cost_summary, results$metadata$summaries, "display_name")
  }

  # Derive strategies filter:
  # - If both interventions AND comparators provided: filter to just those
  # - If only one provided: use all strategies (NULL) so we can compare against "all others"
  strategies <- if (!is.null(interventions) && !is.null(comparators)) {
    unique(c(interventions, comparators))
  } else {
    NULL
  }

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
      scales::comma(round(v, digits = digits))
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
        scales::comma(round(v, digits), accuracy = 10^(-digits))
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
    colnames(result_data) <- c(" ", cost_label, outcome_label, "ICER")

    # Build header
    headers <- list()
    row1 <- list(
      list(span = 1, text = "", borders = c(1, 0, 1, 0)),
      list(span = 1, text = cost_label, borders = c(1, 0, 1, 0)),
      list(span = 1, text = outcome_label, borders = c(1, 0, 1, 0)),
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

    colnames(result_data) <- c(" ", cost_label, outcome_label, "ICER")

    # Build header
    headers <- list()
    row1 <- list(
      list(span = 1, text = "", borders = c(1, 0, 1, 0)),
      list(span = 1, text = cost_label, borders = c(1, 0, 1, 0)),
      list(span = 1, text = outcome_label, borders = c(1, 0, 1, 0)),
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
#' pairwise_ce_table(results, "total_qalys", "total_cost", comparators = "control")
#'
#' # For all groups
#' pairwise_ce_table(results, "total_qalys", "total_cost", groups = NULL,
#'                   comparators = "control")
#' }
#'
#' @export
pairwise_ce_table <- function(results,
                             outcome_summary,
                             cost_summary,
                             groups = "overall",
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
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
