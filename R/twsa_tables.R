#' Two-Way Sensitivity Analysis Tables
#'
#' Functions for creating table outputs of two-way sensitivity analysis
#' (TWSA) results in grid format.
#'
#' @name twsa_tables
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup summarize slice n all_of
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom glue glue
NULL

#' Prepare TWSA Outcomes Table Data
#'
#' Internal helper function that prepares TWSA data for table rendering.
#' Creates a grid format with X parameter values as columns and Y parameter
#' values as rows.
#'
#' @param results TWSA results object from run_twsa()
#' @param summary_name Name of summary to display
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection: "overall", specific group, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param discounted Logical. Use discounted values?
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_twsa_outcomes_table_data <- function(results,
                                     summary_name,
                                     twsa_name = NULL,
                                     groups = "overall",
                                     strategies = NULL,
                                     interventions = NULL,
                                     comparators = NULL,
                                     discounted = TRUE,
                                     decimals = 2,
                                     font_size = 11) {

  # Extract TWSA summaries
  twsa_data <- extract_twsa_summaries(
    results,
    summary_name = summary_name,
    value_type = "all",
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Filter to specific TWSA analysis if multiple exist
  if (!is.null(twsa_name)) {
    twsa_data <- twsa_data %>%
      filter(.data$twsa_name == !!twsa_name | is.na(.data$twsa_name))
  } else {
    # Use first non-base-case TWSA if multiple exist
    available_twsa <- unique(twsa_data$twsa_name[!is.na(twsa_data$twsa_name)])
    if (length(available_twsa) > 1) {
      warning(glue("Multiple TWSA analyses found: {paste(available_twsa, collapse=', ')}. ",
                   "Using '{available_twsa[1]}'. Specify twsa_name to select a different one."))
      twsa_data <- twsa_data %>%
        filter(.data$twsa_name == available_twsa[1] | is.na(.data$twsa_name))
    }
  }

  # Filter out base case for grid data
  grid_data <- twsa_data %>%
    filter(!is.na(.data$twsa_name))

  if (nrow(grid_data) == 0) {
    stop("No TWSA grid data found for table", call. = FALSE)
  }

  # Get parameter display names
  x_param_name <- unique(grid_data$x_param_display_name)[1]
  y_param_name <- unique(grid_data$y_param_display_name)[1]

  # Calculate differences if interventions/comparators provided
  if (!is.null(interventions) || !is.null(comparators)) {
    intervention_strategy <- if (!is.null(interventions)) interventions else comparators

    # Get all strategies present
    all_strats <- unique(grid_data$strategy)
    other_strategies <- setdiff(all_strats, intervention_strategy)

    if (length(other_strategies) == 0) {
      stop("No comparator strategies found for incremental calculation", call. = FALSE)
    }

    # Calculate incremental values
    diff_data <- list()
    for (other in other_strategies) {
      int_data <- grid_data %>%
        filter(.data$strategy == intervention_strategy) %>%
        select("x_value", "y_value", "group", int_amount = "amount")

      other_data <- grid_data %>%
        filter(.data$strategy == other) %>%
        select("x_value", "y_value", "group", other_amount = "amount")

      int_mapped <- map_names_if_available(intervention_strategy, results$metadata$strategies)
      other_mapped <- map_names_if_available(other, results$metadata$strategies)

      if (!is.null(interventions)) {
        comparison_label <- paste0(int_mapped, " vs. ", other_mapped)
      } else {
        comparison_label <- paste0(other_mapped, " vs. ", int_mapped)
      }

      joined <- int_data %>%
        inner_join(other_data, by = c("x_value", "y_value", "group")) %>%
        mutate(
          strategy = comparison_label,
          value = if (!is.null(interventions)) {
            .data$int_amount - .data$other_amount
          } else {
            .data$other_amount - .data$int_amount
          }
        ) %>%
        select("x_value", "y_value", "group", "strategy", "value")

      diff_data[[length(diff_data) + 1]] <- joined
    }

    table_data <- bind_rows(diff_data)
  } else {
    # No incremental - use raw values
    table_data <- grid_data %>%
      mutate(value = .data$amount) %>%
      select("x_value", "y_value", "group", "strategy", "value")

    # Map strategy display names
    if (!is.null(results$metadata$strategies)) {
      table_data$strategy <- map_names(table_data$strategy,
                                        results$metadata$strategies,
                                        "display_name")
    }
  }

  # Map group display names
  if (!is.null(results$metadata$groups)) {
    table_data$group <- map_names(table_data$group,
                                   results$metadata$groups,
                                   "display_name")
  }

  # Get unique values
  strategies_display <- unique(table_data$strategy)
  groups_display <- unique(table_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Build table for each strategy/group combination
  tables <- list()

  for (strat in strategies_display) {
    for (grp in groups_display) {
      subset_data <- table_data %>%
        filter(.data$strategy == strat, .data$group == grp)

      if (nrow(subset_data) == 0) next

      # Pivot to wide format: X values as columns, Y values as rows
      wide_data <- subset_data %>%
        select("x_value", "y_value", "value") %>%
        pivot_wider(
          names_from = "x_value",
          values_from = "value",
          names_prefix = "x_"
        ) %>%
        arrange(.data$y_value)

      # Format numeric values
      x_cols <- setdiff(names(wide_data), "y_value")
      for (col in x_cols) {
        if (is.numeric(wide_data[[col]])) {
          rounded_vals <- round(wide_data[[col]], decimals)
          rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
          wide_data[[col]] <- format(rounded_vals, nsmall = decimals,
                                      scientific = FALSE, trim = TRUE)
        }
      }

      # Rename y_value column
      names(wide_data)[1] <- y_param_name

      # Get x values for column headers
      x_values <- unique(subset_data$x_value)
      x_values <- sort(x_values)

      tables[[length(tables) + 1]] <- list(
        strategy = strat,
        group = grp,
        data = wide_data,
        x_param_name = x_param_name,
        y_param_name = y_param_name,
        x_values = x_values
      )
    }
  }

  list(
    tables = tables,
    n_strategies = n_strategies,
    n_groups = n_groups,
    decimals = decimals,
    font_size = font_size
  )
}

#' TWSA Outcomes Table
#'
#' Creates a grid table showing two-way sensitivity analysis results.
#' The table displays values across a grid with X parameter values as
#' columns and Y parameter values as rows.
#'
#' @param results TWSA results object from run_twsa()
#' @param summary_name Name of the summary to display (e.g., "total_qalys")
#' @param twsa_name Name of specific TWSA analysis to show (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Intervention strategy name(s) for incremental calculation
#' @param comparators Comparator strategy name(s) for incremental calculation
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param decimals Number of decimal places (default: 2)
#' @param font_size Font size for table (default: 11)
#' @param backend Table rendering backend: "flextable" (default) or "kable"
#'
#' @return A rendered table object (flextable or kable)
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model)
#'
#' # Basic table
#' twsa_outcomes_table(results, "total_qalys")
#'
#' # Incremental table
#' twsa_outcomes_table(results, "total_qalys",
#'   interventions = "treatment",
#'   comparators = "standard_care")
#' }
twsa_outcomes_table <- function(results,
                        summary_name,
                        twsa_name = NULL,
                        groups = "overall",
                        strategies = NULL,
                        interventions = NULL,
                        comparators = NULL,
                        discounted = TRUE,
                        decimals = 2,
                        font_size = 11,
                        backend = c("flextable", "kable")) {

  backend <- match.arg(backend)

  # Prepare table data
  prepared <- prepare_twsa_outcomes_table_data(
    results = results,
    summary_name = summary_name,
    twsa_name = twsa_name,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    decimals = decimals,
    font_size = font_size
  )

  if (length(prepared$tables) == 0) {
    stop("No data available for table", call. = FALSE)
  }

  # For simplicity, render first table (single strategy/group)
  # Could be extended to combine multiple tables
  table_info <- prepared$tables[[1]]

  # Build headers
  x_param_name <- table_info$x_param_name
  x_values <- table_info$x_values

  # Create header structure
  # Row 1: X parameter name spanning all X columns
  # Row 2: X values as column headers
  headers <- list()

  # Level 1: X parameter name
  row1 <- list()
  row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
  row1[[2]] <- list(span = length(x_values), text = x_param_name, borders = c(1, 0, 1, 0))
  headers[[1]] <- row1

  # Level 2: X values
  row2 <- list()
  row2[[1]] <- list(span = 1, text = table_info$y_param_name, borders = c(0, 0, 1, 1))
  for (i in seq_along(x_values)) {
    row2[[i + 1]] <- list(
      span = 1,
      text = scales::comma(x_values[i]),
      borders = c(0, 0, 1, 1)
    )
  }
  headers[[2]] <- row2

  # Add strategy/group header if multiple
  if (prepared$n_strategies > 1 || prepared$n_groups > 1) {
    title_text <- table_info$strategy
    if (prepared$n_groups > 1) {
      title_text <- paste0(title_text, " - ", table_info$group)
    }
    # Prepend title row
    title_row <- list()
    title_row[[1]] <- list(
      span = length(x_values) + 1,
      text = title_text,
      borders = c(1, 0, 0, 0)
    )
    headers <- c(list(title_row), headers)
  }

  # Create column alignments
  n_cols <- ncol(table_info$data)
  column_alignments <- c("left", rep("right", n_cols - 1))

  # Create table spec
  spec <- create_simple_table_spec(
    headers = headers,
    data = table_info$data,
    column_alignments = column_alignments,
    font_size = font_size
  )

  # Render table
  render_table(spec, format = backend)
}

#' Render TWSA Grid Table
#'
#' Internal helper to render a grid table from prepared data.
#' Shared logic for all TWSA table functions.
#'
#' @param prepared Prepared table data with tables, n_strategies, n_groups
#' @param font_size Font size for table
#' @param backend Table rendering backend
#'
#' @return A rendered table object
#' @keywords internal
render_twsa_grid_table <- function(prepared, font_size, backend) {

  if (length(prepared$tables) == 0) {
    stop("No data available for table", call. = FALSE)
  }

  # For simplicity, render first table (single strategy/group)
  table_info <- prepared$tables[[1]]

  # Build headers
  x_param_name <- table_info$x_param_name
  x_values <- table_info$x_values

  # Create header structure
  headers <- list()

  # Level 1: X parameter name
  row1 <- list()
  row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
  row1[[2]] <- list(span = length(x_values), text = x_param_name, borders = c(1, 0, 1, 0))
  headers[[1]] <- row1

  # Level 2: X values
  row2 <- list()
  row2[[1]] <- list(span = 1, text = table_info$y_param_name, borders = c(0, 0, 1, 1))
  for (i in seq_along(x_values)) {
    row2[[i + 1]] <- list(
      span = 1,
      text = scales::comma(x_values[i]),
      borders = c(0, 0, 1, 1)
    )
  }
  headers[[2]] <- row2

  # Add strategy/group header if multiple
  if (prepared$n_strategies > 1 || prepared$n_groups > 1) {
    title_text <- table_info$strategy
    if (prepared$n_groups > 1) {
      title_text <- paste0(title_text, " - ", table_info$group)
    }
    title_row <- list()
    title_row[[1]] <- list(
      span = length(x_values) + 1,
      text = title_text,
      borders = c(1, 0, 0, 0)
    )
    headers <- c(list(title_row), headers)
  }

  # Create column alignments
  n_cols <- ncol(table_info$data)
  column_alignments <- c("left", rep("right", n_cols - 1))

  # Create table spec
  spec <- create_simple_table_spec(
    headers = headers,
    data = table_info$data,
    column_alignments = column_alignments,
    font_size = font_size
  )

  # Render table
  render_table(spec, format = backend)
}

#' Prepare TWSA NMB Table Data
#'
#' Internal helper that prepares TWSA NMB data for table rendering.
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary
#' @param cost_outcome Name of the cost summary
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param wtp Willingness-to-pay threshold
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_twsa_nmb_table_data <- function(results,
                                         health_outcome,
                                         cost_outcome,
                                         twsa_name = NULL,
                                         groups = "overall",
                                         interventions = NULL,
                                         comparators = NULL,
                                         wtp,
                                         decimals = 0,
                                         font_size = 11) {

  # Use the NMB data preparation from plots
  nmb_data <- prepare_twsa_nmb_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    wtp = wtp
  )

  if (nrow(nmb_data) == 0) {
    stop("No NMB data found for table", call. = FALSE)
  }

  # Get parameter display names
  x_param_name <- unique(nmb_data$x_param_display_name)[1]
  y_param_name <- unique(nmb_data$y_param_display_name)[1]

  # Get unique values
  strategies_display <- unique(nmb_data$strategy)
  groups_display <- unique(nmb_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Build table for each strategy/group combination
  tables <- list()

  for (strat in strategies_display) {
    for (grp in groups_display) {
      subset_data <- nmb_data %>%
        filter(.data$strategy == strat, .data$group == grp)

      if (nrow(subset_data) == 0) next

      # Pivot to wide format
      wide_data <- subset_data %>%
        select("x_value", "y_value", "value") %>%
        pivot_wider(
          names_from = "x_value",
          values_from = "value",
          names_prefix = "x_"
        ) %>%
        arrange(.data$y_value)

      # Format numeric values
      x_cols <- setdiff(names(wide_data), "y_value")
      for (col in x_cols) {
        if (is.numeric(wide_data[[col]])) {
          rounded_vals <- round(wide_data[[col]], decimals)
          wide_data[[col]] <- scales::comma(rounded_vals, accuracy = 10^(-decimals))
        }
      }

      # Rename y_value column
      names(wide_data)[1] <- y_param_name

      # Get x values for column headers
      x_values <- unique(subset_data$x_value)
      x_values <- sort(x_values)

      tables[[length(tables) + 1]] <- list(
        strategy = strat,
        group = grp,
        data = wide_data,
        x_param_name = x_param_name,
        y_param_name = y_param_name,
        x_values = x_values
      )
    }
  }

  list(
    tables = tables,
    n_strategies = n_strategies,
    n_groups = n_groups,
    decimals = decimals,
    font_size = font_size
  )
}

#' TWSA Net Monetary Benefit Table
#'
#' Creates a grid table showing Net Monetary Benefit across the TWSA parameter grid.
#' NMB = (delta_outcomes x WTP) - delta_costs.
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param twsa_name Name of specific TWSA analysis to show (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param interventions Intervention strategy name(s). At least one of interventions
#'   or comparators must be specified.
#' @param comparators Comparator strategy name(s). At least one of interventions
#'   or comparators must be specified.
#' @param wtp Willingness-to-pay threshold. If NULL, extracts from outcome metadata.
#' @param decimals Number of decimal places (default: 0)
#' @param font_size Font size for table (default: 11)
#' @param backend Table rendering backend: "flextable" (default) or "kable"
#'
#' @return A rendered table object (flextable or kable)
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model)
#'
#' # NMB table with explicit WTP
#' twsa_nmb_table(results, "total_qalys", "total_cost",
#'   interventions = "treatment", wtp = 50000)
#' }
twsa_nmb_table <- function(results,
                            health_outcome,
                            cost_outcome,
                            twsa_name = NULL,
                            groups = "overall",
                            interventions = NULL,
                            comparators = NULL,
                            wtp = NULL,
                            decimals = 0,
                            font_size = 11,
                            backend = c("flextable", "kable")) {

  backend <- match.arg(backend)

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation",
         call. = FALSE)
  }

  # Get WTP if not provided
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

  # Prepare table data
  prepared <- prepare_twsa_nmb_table_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    wtp = wtp,
    decimals = decimals,
    font_size = font_size
  )

  # Render grid table
  render_twsa_grid_table(prepared, font_size, backend)
}

#' Prepare TWSA CE Table Data
#'
#' Internal helper that prepares TWSA CE/ICER data for table rendering.
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary
#' @param cost_outcome Name of the cost summary
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_twsa_ce_table_data <- function(results,
                                        health_outcome,
                                        cost_outcome,
                                        twsa_name = NULL,
                                        groups = "overall",
                                        interventions,
                                        comparators,
                                        font_size = 11) {

  # Use the CE data preparation from plots
  ce_data <- prepare_twsa_ce_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    interventions = interventions,
    comparators = comparators
  )

  if (nrow(ce_data) == 0) {
    stop("No CE data found for table", call. = FALSE)
  }

  # Get parameter display names
  x_param_name <- unique(ce_data$x_param_display_name)[1]
  y_param_name <- unique(ce_data$y_param_display_name)[1]

  # Get unique values
  strategies_display <- unique(ce_data$strategy)
  groups_display <- unique(ce_data$group)

  # Reorder groups
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Build table for each strategy/group combination
  tables <- list()

  for (strat in strategies_display) {
    for (grp in groups_display) {
      subset_data <- ce_data %>%
        filter(.data$strategy == strat, .data$group == grp)

      if (nrow(subset_data) == 0) next

      # Format ICER values with special case handling
      subset_data <- subset_data %>%
        mutate(
          display_text = case_when(
            .data$ce_class == "dominated" ~ "Dominated",
            .data$ce_class == "dominant" ~ "Dominant",
            .data$ce_class == "equivalent" ~ "Equivalent",
            .data$ce_class == "sw_quadrant" ~ paste0(scales::comma(abs(.data$icer), accuracy = 1), "*"),
            TRUE ~ scales::comma(.data$icer, accuracy = 1)
          )
        )

      # Pivot to wide format
      wide_data <- subset_data %>%
        select("x_value", "y_value", "display_text") %>%
        pivot_wider(
          names_from = "x_value",
          values_from = "display_text",
          names_prefix = "x_"
        ) %>%
        arrange(.data$y_value)

      # Rename y_value column
      names(wide_data)[1] <- y_param_name

      # Get x values for column headers
      x_values <- unique(subset_data$x_value)
      x_values <- sort(x_values)

      tables[[length(tables) + 1]] <- list(
        strategy = strat,
        group = grp,
        data = wide_data,
        x_param_name = x_param_name,
        y_param_name = y_param_name,
        x_values = x_values
      )
    }
  }

  list(
    tables = tables,
    n_strategies = n_strategies,
    n_groups = n_groups,
    decimals = 0,
    font_size = font_size
  )
}

#' TWSA Cost-Effectiveness (ICER) Table
#'
#' Creates a grid table showing ICER values across the TWSA parameter grid.
#' Handles edge cases: Dominated, Dominant, and Equivalent scenarios.
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param twsa_name Name of specific TWSA analysis to show (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param interventions Intervention strategy name(s). Required.
#' @param comparators Comparator strategy name(s). Required.
#' @param font_size Font size for table (default: 11)
#' @param backend Table rendering backend: "flextable" (default) or "kable"
#'
#' @return A rendered table object (flextable or kable)
#'
#' @details
#' ICER = (Cost_intervention - Cost_comparator) / (Outcome_intervention - Outcome_comparator)
#'
#' Special cases displayed in the table:
#' \itemize{
#'   \item "Dominated": Intervention is more costly and less effective
#'   \item "Dominant": Intervention is less costly and more effective
#'   \item "Equivalent": Identical outcomes result in undefined ICER
#'   \item Values with "*" suffix: SW quadrant (comparator is cheaper but less effective)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model)
#'
#' # CE table
#' twsa_ce_table(results, "total_qalys", "total_cost",
#'   interventions = "treatment",
#'   comparators = "standard_care")
#' }
twsa_ce_table <- function(results,
                           health_outcome,
                           cost_outcome,
                           twsa_name = NULL,
                           groups = "overall",
                           interventions,
                           comparators,
                           font_size = 11,
                           backend = c("flextable", "kable")) {

  backend <- match.arg(backend)

  # Prepare table data
  prepared <- prepare_twsa_ce_table_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    font_size = font_size
  )

  # Render grid table
  render_twsa_grid_table(prepared, font_size, backend)
}
