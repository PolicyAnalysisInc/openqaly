#' Scenario Analysis Table Functions
#'
#' Functions for creating tables from scenario analysis results.
#'
#' @name scenario_tables
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup row_number slice n all_of
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
NULL

#' Prepare Scenario Outcomes Table Data
#'
#' Internal helper function that prepares scenario outcomes data for table rendering.
#' Creates a table showing the outcome value for each scenario across strategies.
#'
#' @param results A openqaly scenario results object
#' @param outcome Name of outcome to display
#' @param groups Group selection: "overall", specific group, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Intervention strategy name
#' @param comparators Comparator strategy name
#' @param decimals Number of decimal places
#' @param discounted Logical. Use discounted values?
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_scenario_outcomes_table_data <- function(results,
                                                  outcome,
                                                  groups = "overall",
                                                  strategies = NULL,
                                                  interventions = NULL,
                                                  comparators = NULL,
                                                  decimals = 2,
                                                  discounted = FALSE,
                                                  font_size = 11) {

  # Use prepare_scenario_bar_data which handles all the extraction and comparison logic
  scenario_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Get unique strategies and groups
  strategies_display <- unique(scenario_data$strategy)
  groups_display <- unique(scenario_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  if (mode == "single_group") {
    # Single group: pivot to wide format with strategy columns
    result_data <- scenario_data %>%
      mutate(
        # Order: Base Case first, then by scenario_id
        sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id))
      ) %>%
      arrange(.data$sort_order) %>%
      select("scenario_name", "strategy", "value") %>%
      pivot_wider(
        names_from = "strategy",
        values_from = "value"
      )

    # Reorder columns
    col_order <- c("scenario_name", strategies_display)
    result_data <- result_data[, col_order]

    # Format numeric columns
    for (col in strategies_display) {
      if (is.numeric(result_data[[col]])) {
        rounded_vals <- round(result_data[[col]], decimals)
        rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
        result_data[[col]] <- format(rounded_vals, nsmall = decimals, scientific = FALSE, trim = TRUE)
      }
    }

    # Rename first column
    names(result_data)[1] <- "Scenario"

    # Simple two-level headers
    headers <- list()

    # Level 1: empty + strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Column alignments
    column_alignments <- c("left", rep("right", n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    special_rows <- list()

  } else {
    # Multi-group mode: group header rows with indented scenarios
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      # Group header row
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Create group header row (all columns empty except first)
      group_row <- tibble(scenario_name = grp)
      for (strat in strategies_display) {
        group_row[[strat]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      # Scenario rows for this group
      grp_data <- scenario_data %>%
        filter(.data$group == grp) %>%
        mutate(
          sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id))
        ) %>%
        arrange(.data$sort_order) %>%
        select("scenario_name", "strategy", "value") %>%
        pivot_wider(
          names_from = "strategy",
          values_from = "value"
        )

      # Reorder columns
      col_order <- c("scenario_name", strategies_display)
      grp_data <- grp_data[, col_order]

      # Format numeric columns
      for (col in strategies_display) {
        if (is.numeric(grp_data[[col]])) {
          rounded_vals <- round(grp_data[[col]], decimals)
          rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
          grp_data[[col]] <- format(rounded_vals, nsmall = decimals, scientific = FALSE, trim = TRUE)
        }
      }

      # Track indented row indices
      n_scenario_rows <- nrow(grp_data)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_scenario_rows))

      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + n_scenario_rows
    }

    # Rename first column
    names(result_data)[1] <- "Scenario"

    # Simple headers
    headers <- list()

    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Column alignments
    column_alignments <- c("left", rep("right", n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    # Special rows
    special_rows <- list(
      group_header_rows = group_header_rows,
      indented_rows = indented_rows
    )
  }

  # Build table spec
  create_simple_table_spec(
    data = result_data,
    headers = headers,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size
  )
}


#' Scenario Outcomes Table
#'
#' Creates a table showing scenario outcome values. Each row represents a scenario,
#' with columns for each strategy showing the outcome value.
#'
#' @param results A openqaly scenario results object (output from run_scenario)
#' @param outcome Name of outcome to display (e.g., "total_qalys", "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, or NULL
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Character vector of intervention strategy name(s).
#'   When specified, shows differences (intervention - comparator).
#' @param comparators Character vector of comparator strategy name(s).
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param font_size Font size for rendering (default: 11)
#'
#' @return A gt table object
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model)
#'
#' # Basic outcome table
#' scenario_outcomes_table(results, "total_qalys")
#'
#' # Show differences vs comparator
#' scenario_outcomes_table(results, "total_qalys", comparators = "control")
#' }
#'
#' @export
scenario_outcomes_table <- function(results,
                                     outcome,
                                     groups = "overall",
                                     strategies = NULL,
                                     interventions = NULL,
                                     comparators = NULL,
                                     decimals = 2,
                                     discounted = FALSE,
                                     font_size = 11) {

  table_spec <- prepare_scenario_outcomes_table_data(
    results,
    outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    discounted = discounted,
    font_size = font_size
  )

  render_table(table_spec)
}


#' Prepare Scenario CE Table Data
#'
#' Internal helper function that prepares scenario CE data for table rendering.
#'
#' @param results A openqaly scenario results object
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param interventions Intervention strategies
#' @param comparators Comparator strategies
#' @param decimals Number of decimal places
#' @param font_size Font size
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_scenario_ce_table_data <- function(results,
                                            health_outcome,
                                            cost_outcome,
                                            groups = "overall",
                                            interventions = NULL,
                                            comparators = NULL,
                                            decimals = 0,
                                            font_size = 11) {

  # Get outcome data (always discounted for CE)
  outcome_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Get cost data (always discounted for CE)
  cost_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Calculate ICER
  ce_data <- outcome_data %>%
    inner_join(
      cost_data %>% select("scenario_id", "strategy", "group", delta_cost = "value"),
      by = c("scenario_id", "strategy", "group")
    ) %>%
    rename(delta_outcome = "value") %>%
    mutate(
      icer = case_when(
        abs(.data$delta_outcome) < .Machine$double.eps ~ NaN,
        .data$delta_outcome < 0 & .data$delta_cost > 0 ~ Inf,
        .data$delta_outcome > 0 & .data$delta_cost <= 0 ~ 0,
        TRUE ~ .data$delta_cost / .data$delta_outcome
      ),
      # Classify for display
      # SW quadrant: intervention cheaper but worse (delta_cost < 0 AND delta_outcome < 0)
      ce_class = case_when(
        is.nan(.data$icer) ~ "equivalent",
        is.infinite(.data$icer) & .data$icer > 0 ~ "dominated",
        .data$icer == 0 ~ "dominant",
        .data$delta_outcome < 0 & .data$delta_cost < 0 ~ "sw_quadrant",
        TRUE ~ "normal"
      )
    )

  # Determine base case classification for direction change detection
  base_case_class <- ce_data %>%
    filter(.data$scenario_name == "Base Case") %>%
    pull(.data$ce_class) %>%
    first()

  # Detect direction changes relative to base case
  ce_data <- ce_data %>%
    mutate(
      has_direction_change = case_when(
        .data$scenario_name == "Base Case" ~ FALSE,
        # Base is normal/dominated (positive direction) and scenario is sw_quadrant (flipped)
        base_case_class %in% c("normal", "dominated") & .data$ce_class == "sw_quadrant" ~ TRUE,
        # Base is sw_quadrant and scenario is normal/dominated
        base_case_class == "sw_quadrant" & .data$ce_class %in% c("normal", "dominated") ~ TRUE,
        TRUE ~ FALSE
      ),
      icer_display = case_when(
        is.nan(.data$icer) ~ "Equivalent",
        is.infinite(.data$icer) & .data$icer > 0 ~ "Dominated",
        .data$icer == 0 ~ "Dominant",
        .data$has_direction_change ~ paste0(format(round(abs(.data$icer), decimals), big.mark = ",", scientific = FALSE), "*"),
        TRUE ~ format(round(.data$icer, decimals), big.mark = ",", scientific = FALSE)
      )
    )

  # Extract intervention/comparator names from strategy label and collect footnotes
  direction_change_footnotes <- ce_data %>%
    filter(.data$has_direction_change) %>%
    mutate(
      parts = strsplit(as.character(.data$strategy), " vs\\. "),
      intervention_name = sapply(.data$parts, `[`, 1),
      comparator_name = sapply(.data$parts, `[`, 2),
      footnote_text = sprintf(
        "* %s is more costly & more effective than %s. ICER represents cost-effectiveness of %s vs. %s.",
        .data$comparator_name, .data$intervention_name,
        .data$comparator_name, .data$intervention_name
      )
    ) %>%
    distinct(.data$footnote_text) %>%
    pull(.data$footnote_text)

  footnotes <- unique(direction_change_footnotes)

  # Get unique strategies and groups
  strategies_display <- unique(ce_data$strategy)
  groups_display <- unique(ce_data$group)
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  if (mode == "single_group") {
    # Format table with columns: Scenario, Delta Cost, Delta Outcome, ICER (per strategy)
    result_data <- ce_data %>%
      mutate(
        sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id)),
        delta_cost_fmt = format(round(.data$delta_cost, 2), big.mark = ",", scientific = FALSE),
        delta_outcome_fmt = format(round(.data$delta_outcome, 4), big.mark = ",", scientific = FALSE)
      ) %>%
      arrange(.data$sort_order) %>%
      select("scenario_name", "strategy", "delta_cost_fmt", "delta_outcome_fmt", "icer_display")

    # For single comparison, create simple table
    if (n_strategies == 1) {
      result_data <- result_data %>%
        select("scenario_name",
               `Incremental Cost` = "delta_cost_fmt",
               `Incremental Outcome` = "delta_outcome_fmt",
               `ICER` = "icer_display")

      names(result_data)[1] <- "Scenario"

      headers <- list()
      row1 <- list()
      row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
      row1[[2]] <- list(span = 1, text = "Incremental Cost", borders = c(1, 0, 1, 0))
      row1[[3]] <- list(span = 1, text = "Incremental Outcome", borders = c(1, 0, 1, 0))
      row1[[4]] <- list(span = 1, text = "ICER", borders = c(1, 0, 1, 0))
      headers[[1]] <- row1

      column_alignments <- c("left", "right", "right", "right")
      column_widths <- rep(NA, 4)
      special_rows <- list()

    } else {
      # Multiple comparisons: pivot wider
      result_data <- result_data %>%
        pivot_wider(
          names_from = "strategy",
          values_from = c("delta_cost_fmt", "delta_outcome_fmt", "icer_display"),
          names_glue = "{strategy}_{.value}"
        )

      # Reorder columns
      col_order <- "scenario_name"
      for (strat in strategies_display) {
        col_order <- c(col_order,
                       paste0(strat, "_delta_cost_fmt"),
                       paste0(strat, "_delta_outcome_fmt"),
                       paste0(strat, "_icer_display"))
      }
      result_data <- result_data[, col_order]

      names(result_data)[1] <- "Scenario"

      # Build headers
      headers <- list()

      # Level 1: Strategy names spanning 3 columns each
      row1 <- list()
      row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
      for (strat in strategies_display) {
        row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
      }
      headers[[1]] <- row1

      # Level 2: Cost/Outcome/ICER
      row2 <- list()
      row2[[1]] <- list(span = 1, text = "", borders = c(0, 0, 1, 0))
      for (strat in strategies_display) {
        row2[[length(row2) + 1]] <- list(span = 1, text = "\u0394 Cost", borders = c(0, 0, 1, 0))
        row2[[length(row2) + 1]] <- list(span = 1, text = "\u0394 Outcome", borders = c(0, 0, 1, 0))
        row2[[length(row2) + 1]] <- list(span = 1, text = "ICER", borders = c(0, 0, 1, 0))
      }
      headers[[2]] <- row2

      column_alignments <- c("left", rep(c("right", "right", "right"), n_strategies))
      column_widths <- rep(NA, ncol(result_data))
      special_rows <- list()
    }

  } else {
    # Multi-group mode - similar structure with group headers
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Group header row
      group_row <- tibble(scenario_name = grp)
      for (strat in strategies_display) {
        group_row[[paste0(strat, "_delta_cost_fmt")]] <- ""
        group_row[[paste0(strat, "_delta_outcome_fmt")]] <- ""
        group_row[[paste0(strat, "_icer_display")]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      # Scenario rows
      grp_data <- ce_data %>%
        filter(.data$group == grp) %>%
        mutate(
          sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id)),
          delta_cost_fmt = format(round(.data$delta_cost, 2), big.mark = ",", scientific = FALSE),
          delta_outcome_fmt = format(round(.data$delta_outcome, 4), big.mark = ",", scientific = FALSE)
        ) %>%
        arrange(.data$sort_order) %>%
        select("scenario_name", "strategy", "delta_cost_fmt", "delta_outcome_fmt", "icer_display") %>%
        pivot_wider(
          names_from = "strategy",
          values_from = c("delta_cost_fmt", "delta_outcome_fmt", "icer_display"),
          names_glue = "{strategy}_{.value}"
        )

      # Reorder columns
      col_order <- "scenario_name"
      for (strat in strategies_display) {
        col_order <- c(col_order,
                       paste0(strat, "_delta_cost_fmt"),
                       paste0(strat, "_delta_outcome_fmt"),
                       paste0(strat, "_icer_display"))
      }
      grp_data <- grp_data[, col_order]

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
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    row2 <- list()
    row2[[1]] <- list(span = 1, text = "", borders = c(0, 0, 1, 0))
    for (strat in strategies_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "\u0394 Cost", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "\u0394 Outcome", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "ICER", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    column_alignments <- c("left", rep(c("right", "right", "right"), n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    special_rows <- list(
      group_header_rows = group_header_rows,
      indented_rows = indented_rows
    )
  }

  # Create table spec
  spec <- create_simple_table_spec(
    data = result_data,
    headers = headers,
    column_alignments = column_alignments,
    column_widths = column_widths,
    special_rows = special_rows,
    font_size = font_size
  )

  # Add footnotes to spec
  spec$footnotes <- footnotes

  spec
}


#' Scenario Cost-Effectiveness Table
#'
#' Creates a table showing incremental cost-effectiveness results for each scenario.
#' Shows incremental costs, incremental outcomes, and ICER.
#'
#' @param results A openqaly scenario results object (output from run_scenario)
#' @param health_outcome Name of health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, or NULL
#' @param interventions Character vector of intervention strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s).
#' @param decimals Number of decimal places for ICER (default: 0)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The table shows each scenario as a row with columns for incremental cost,
#' incremental outcome, and ICER.
#'
#' **ICER Cell Formatting:**
#' - Positive finite: Formatted ICER value
#' - Negative finite: Formatted value with asterisk (direction change)
#' - Dominated (+Inf): "Dominated"
#' - Dominant (0): "Dominant"
#' - Equivalent (NaN): "Equivalent"
#'
#' **Direction Change Handling:**
#' - Cells showing direction change (SW quadrant) get asterisk
#' - Footnote explains the asterisk
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model)
#'
#' # CE table
#' scenario_ce_table(results, "total_qalys", "total_cost", comparators = "control")
#' }
#'
#' @export
scenario_ce_table <- function(results,
                               health_outcome,
                               cost_outcome,
                               groups = "overall",
                               interventions = NULL,
                               comparators = NULL,
                               decimals = 0,
                               font_size = 11,
                               table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided",
         call. = FALSE)
  }

  table_spec <- prepare_scenario_ce_table_data(
    results,
    health_outcome,
    cost_outcome,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  tbl <- render_table(table_spec, format = table_format)

  # Add footnotes if any
  if (length(table_spec$footnotes) > 0) {
    if (table_format == "flextable") {
      tbl <- flextable::add_footer_lines(tbl, values = table_spec$footnotes)
      tbl <- flextable::align(tbl, align = "left", part = "footer")
      tbl <- flextable::fontsize(tbl, size = font_size - 1, part = "footer")
    } else {
      # For kable, footnotes are handled differently
      tbl <- kableExtra::footnote(tbl,
                                  general = table_spec$footnotes,
                                  general_title = "",
                                  footnote_as_chunk = FALSE)
    }
  }

  tbl
}


#' Prepare Scenario NMB Table Data
#'
#' Internal helper function that prepares scenario NMB data for table rendering.
#'
#' @param results A openqaly scenario results object
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param wtp Willingness to pay
#' @param groups Group selection
#' @param interventions Intervention strategies
#' @param comparators Comparator strategies
#' @param decimals Number of decimal places
#' @param font_size Font size
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_scenario_nmb_table_data <- function(results,
                                             health_outcome,
                                             cost_outcome,
                                             wtp,
                                             groups = "overall",
                                             interventions = NULL,
                                             comparators = NULL,
                                             decimals = 0,
                                             font_size = 11) {

  # Get outcome data
  outcome_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Get cost data
  cost_data <- prepare_scenario_bar_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Calculate NMB
  nmb_data <- outcome_data %>%
    inner_join(
      cost_data %>% select("scenario_id", "strategy", "group", delta_cost = "value"),
      by = c("scenario_id", "strategy", "group")
    ) %>%
    rename(delta_outcome = "value") %>%
    mutate(
      nmb = (.data$delta_outcome * wtp) - .data$delta_cost
    )

  # Similar structure to outcomes table
  strategies_display <- unique(nmb_data$strategy)
  groups_display <- unique(nmb_data$group)
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  if (mode == "single_group") {
    result_data <- nmb_data %>%
      mutate(
        sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id))
      ) %>%
      arrange(.data$sort_order) %>%
      select("scenario_name", "strategy", "nmb") %>%
      pivot_wider(
        names_from = "strategy",
        values_from = "nmb"
      )

    # Format numeric columns
    for (col in strategies_display) {
      if (is.numeric(result_data[[col]])) {
        rounded_vals <- round(result_data[[col]], decimals)
        result_data[[col]] <- format(rounded_vals, big.mark = ",", nsmall = decimals, scientific = FALSE)
      }
    }

    names(result_data)[1] <- "Scenario"

    headers <- list()
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_strategies))
    column_widths <- rep(NA, ncol(result_data))
    special_rows <- list()

  } else {
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      group_row <- tibble(scenario_name = grp)
      for (strat in strategies_display) {
        group_row[[strat]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      grp_data <- nmb_data %>%
        filter(.data$group == grp) %>%
        mutate(
          sort_order = if_else(.data$is_base_case, 0L, as.integer(.data$scenario_id))
        ) %>%
        arrange(.data$sort_order) %>%
        select("scenario_name", "strategy", "nmb") %>%
        pivot_wider(
          names_from = "strategy",
          values_from = "nmb"
        )

      for (col in strategies_display) {
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

    headers <- list()
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 1, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    column_alignments <- c("left", rep("right", n_strategies))
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


#' Scenario NMB Table
#'
#' Creates a table showing incremental Net Monetary Benefit for each scenario.
#'
#' @param results A openqaly scenario results object (output from run_scenario)
#' @param health_outcome Name of health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of cost summary (e.g., "total_cost")
#' @param wtp Willingness to pay threshold. If NULL, extracted from outcome metadata.
#' @param groups Group selection: "overall" (default), specific group name, or NULL
#' @param interventions Character vector of intervention strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s).
#' @param decimals Number of decimal places (default: 0)
#' @param font_size Font size for rendering (default: 11)
#'
#' @return A gt table object
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model)
#'
#' # NMB table
#' scenario_nmb_table(results, "total_qalys", "total_cost",
#'                    wtp = 50000, comparators = "control")
#' }
#'
#' @export
scenario_nmb_table <- function(results,
                                health_outcome,
                                cost_outcome,
                                wtp = NULL,
                                groups = "overall",
                                interventions = NULL,
                                comparators = NULL,
                                decimals = 0,
                                font_size = 11) {

  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided",
         call. = FALSE)
  }

  # Get WTP if needed
  if (is.null(wtp)) {
    if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Provide explicit wtp parameter.",
           call. = FALSE)
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome),
           call. = FALSE)
    }
    wtp <- outcome_meta[["wtp"]][1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.",
                   health_outcome), call. = FALSE)
    }
  }

  table_spec <- prepare_scenario_nmb_table_data(
    results,
    health_outcome,
    cost_outcome,
    wtp = wtp,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  render_table(table_spec)
}
