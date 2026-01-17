#' Prepare DSA Outcomes Table Data
#'
#' Internal helper function that prepares DSA outcomes data for table rendering.
#' Creates a table showing low, base case, and high values for each parameter
#' across strategies.
#'
#' @param results A openqaly DSA results object
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
prepare_dsa_outcomes_table_data <- function(results,
                                            outcome,
                                            groups = "overall",
                                            strategies = NULL,
                                            interventions = NULL,
                                            comparators = NULL,
                                            decimals = 2,
                                            discounted = FALSE,
                                            font_size = 11) {

  # Extract DSA summaries
  dsa_data <- extract_dsa_summaries(
    results,
    summary_name = outcome,
    value_type = "all",
    groups = groups,
    strategies = strategies,
    discounted = discounted
  )

  # Separate base case from variations
  # Note: base case has parameter="base" for all rows, so we don't join on parameter
  base_data <- dsa_data %>%
    filter(.data$run_id == 1) %>%
    select("strategy", "group", base = "amount")

  low_data <- dsa_data %>%
    filter(.data$variation == "low") %>%
    select("strategy", "group", "parameter", "parameter_display_name", low = "amount")

  high_data <- dsa_data %>%
    filter(.data$variation == "high") %>%
    select("strategy", "group", "parameter", "parameter_display_name", high = "amount")

  # Combine data (join base only on strategy/group, not parameter)
  combined_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group"))

  # Calculate differences if interventions/comparators provided
  differences_created <- FALSE
  if (!is.null(interventions) || !is.null(comparators)) {
    intervention_strategy <- if (!is.null(interventions)) interventions else comparators

    # Pivot to get strategies as columns
    data_wide <- combined_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = c("low", "base", "high"),
        id_cols = c("group", "parameter", "parameter_display_name")
      )

    # Get all strategies
    all_strategies <- unique(dsa_data$strategy)
    other_strategies <- setdiff(all_strategies, intervention_strategy)

    # Create comparison labels using display names
    # intervention_strategy and other_strategies are technical names
    intervention_mapped <- map_names(intervention_strategy, results$metadata$strategies, "display_name")
    other_mapped <- map_names(other_strategies, results$metadata$strategies, "display_name")

    # Create comparison labels
    if (!is.null(interventions)) {
      comparison_labels <- paste0(intervention_mapped, " vs. ", other_mapped)
    } else {
      comparison_labels <- paste0(other_mapped, " vs. ", intervention_mapped)
    }

    # Calculate differences
    diff_data <- list()
    for (i in seq_along(other_strategies)) {
      other <- other_strategies[i]
      comp_label <- comparison_labels[i]

      low_int_col <- paste0("low_", intervention_strategy)
      low_other_col <- paste0("low_", other)
      base_int_col <- paste0("base_", intervention_strategy)
      base_other_col <- paste0("base_", other)
      high_int_col <- paste0("high_", intervention_strategy)
      high_other_col <- paste0("high_", other)

      if (!is.null(interventions)) {
        diff_df <- data_wide %>%
          mutate(
            strategy = comp_label,
            low = !!sym(low_int_col) - !!sym(low_other_col),
            base = !!sym(base_int_col) - !!sym(base_other_col),
            high = !!sym(high_int_col) - !!sym(high_other_col)
          ) %>%
          select("group", "parameter", "parameter_display_name", "strategy", "low", "base", "high")
      } else {
        diff_df <- data_wide %>%
          mutate(
            strategy = comp_label,
            low = !!sym(low_other_col) - !!sym(low_int_col),
            base = !!sym(base_other_col) - !!sym(base_int_col),
            high = !!sym(high_other_col) - !!sym(high_int_col)
          ) %>%
          select("group", "parameter", "parameter_display_name", "strategy", "low", "base", "high")
      }

      diff_data[[i]] <- diff_df
    }

    combined_data <- bind_rows(diff_data)
    differences_created <- TRUE
  }

  # Map display names ONLY if differences were NOT created
  # (When differences are created, comparison labels are already in display format)
  if (!differences_created && !is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      combined_data$strategy <- map_names(combined_data$strategy, results$metadata$strategies, "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      combined_data$group <- map_names(combined_data$group, results$metadata$groups, "display_name")
    }
  } else if (differences_created && !is.null(results$metadata)) {
    # For differences, only map group names (strategy names are already comparison labels)
    if (!is.null(results$metadata$groups)) {
      combined_data$group <- map_names(combined_data$group, results$metadata$groups, "display_name")
    }
  }

  # Get unique strategies and groups
  strategies_display <- unique(combined_data$strategy)
  groups_display <- unique(combined_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  # Build table data based on mode
  if (mode == "single_group") {
    # Single group: pivot to wide format with strategy columns
    # Each strategy gets 3 columns: Low, Base, High
    result_data <- combined_data %>%
      arrange(.data$parameter_display_name) %>%
      select("parameter_display_name", "strategy", "low", "base", "high")

    # Pivot wider to get strategy columns
    result_data <- result_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = c("low", "base", "high"),
        names_glue = "{strategy}_{.value}"
      )

    # Reorder columns to group by strategy
    col_order <- c("parameter_display_name")
    for (strat in strategies_display) {
      col_order <- c(col_order, paste0(strat, "_low"), paste0(strat, "_base"), paste0(strat, "_high"))
    }
    result_data <- result_data[, col_order]

    # Format numeric columns
    for (col in setdiff(colnames(result_data), "parameter_display_name")) {
      if (is.numeric(result_data[[col]])) {
        rounded_vals <- round(result_data[[col]], decimals)
        rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
        result_data[[col]] <- format(rounded_vals, nsmall = decimals, scientific = FALSE, trim = TRUE)
      }
    }

    # Rename first column
    names(result_data)[1] <- " "

    # Build three-level headers
    headers <- list()

    # Level 1: Strategy names spanning 3 columns each
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Level 2: Low/Base/High labels
    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))
    for (strat in strategies_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    # Column alignments
    column_alignments <- c("left", rep(c("right", "right", "right"), n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    special_rows <- list()

  } else {
    # Multi-group mode: group header rows with indented parameters
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      # Group header row
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Create group header row (all columns empty except first)
      group_row <- tibble(parameter_display_name = grp)
      for (strat in strategies_display) {
        group_row[[paste0(strat, "_low")]] <- ""
        group_row[[paste0(strat, "_base")]] <- ""
        group_row[[paste0(strat, "_high")]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      # Parameter rows for this group
      grp_data <- combined_data %>%
        filter(.data$group == grp) %>%
        arrange(.data$parameter_display_name) %>%
        select("parameter_display_name", "strategy", "low", "base", "high") %>%
        pivot_wider(
          names_from = "strategy",
          values_from = c("low", "base", "high"),
          names_glue = "{strategy}_{.value}"
        )

      # Reorder columns
      col_order <- c("parameter_display_name")
      for (strat in strategies_display) {
        col_order <- c(col_order, paste0(strat, "_low"), paste0(strat, "_base"), paste0(strat, "_high"))
      }
      grp_data <- grp_data[, col_order]

      # Format numeric columns
      for (col in setdiff(colnames(grp_data), "parameter_display_name")) {
        if (is.numeric(grp_data[[col]])) {
          rounded_vals <- round(grp_data[[col]], decimals)
          rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
          grp_data[[col]] <- format(rounded_vals, nsmall = decimals, scientific = FALSE, trim = TRUE)
        }
      }

      # Track indented row indices
      n_param_rows <- nrow(grp_data)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_param_rows))

      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + n_param_rows
    }

    # Rename first column
    names(result_data)[1] <- " "

    # Build three-level headers (same as single group)
    headers <- list()

    # Level 1: Strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Level 2: Low/Base/High
    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))
    for (strat in strategies_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    # Column alignments
    column_alignments <- c("left", rep(c("right", "right", "right"), n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    # Calculate group boundary rows (all group headers except the first)
    group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
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


#' Format DSA Outcomes as Summary Table
#'
#' Creates a table showing DSA outcomes with low, base case, and high values
#' for each parameter across strategies. Supports both outcome and cost summaries.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param outcome Name of outcome to display (e.g., "total_qalys", "total_cost")
#' @param groups Group selection: "overall" (default), specific group, or NULL
#'   (all groups)
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Single reference strategy for intervention perspective.
#'   If provided, shows interventions - comparators comparisons. Mutually exclusive with comparators.
#' @param comparators Single reference strategy for comparator perspective.
#'   If provided, shows interventions - comparators comparisons. Mutually exclusive with interventions.
#' @param decimals Number of decimal places (default: 2)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "kable" (default) or "flextable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The table shows each DSA parameter as a row with three columns per strategy:
#' Low, Base, and High values.
#'
#' When multiple groups need to be displayed (groups = NULL), uses group label rows
#' (in bold) followed by indented parameter rows for each group.
#'
#' When interventions or comparators is specified, shows differences between strategies
#' instead of absolute values.
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#' dsa_results <- run_dsa(model)
#'
#' # Basic DSA outcomes table
#' dsa_outcomes_table(dsa_results, "total_qalys")
#'
#' # Show differences vs control
#' dsa_outcomes_table(dsa_results, "total_qalys", comparators = "control")
#'
#' # Cost table
#' dsa_outcomes_table(dsa_results, "total_cost")
#'
#' # All groups with flextable format
#' dsa_outcomes_table(dsa_results, "total_qalys", groups = NULL,
#'                    table_format = "flextable")
#' }
#'
#' @export
dsa_outcomes_table <- function(results,
                               outcome,
                               groups = "overall",
                               strategies = NULL,
                               interventions = NULL,
                               comparators = NULL,
                               decimals = 2,
                               discounted = FALSE,
                               font_size = 11,
                               table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Validate interventions/comparators
  if (!is.null(interventions) && !is.null(comparators)) {
    stop("Only one of 'interventions' or 'comparators' should be provided, not both")
  }

  # Prepare data
  prepared <- prepare_dsa_outcomes_table_data(
    results = results,
    outcome = outcome,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    discounted = discounted,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}


#' Prepare DSA NMB Table Data
#'
#' Internal helper function that prepares DSA Net Monetary Benefit data for table
#' rendering. Creates a table showing low, base case, and high NMB values for each
#' parameter across strategy comparisons.
#'
#' @param results A openqaly DSA results object
#' @param health_outcome Name of health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall", specific group, or NULL
#' @param wtp Override willingness-to-pay (extracts from metadata if NULL)
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_dsa_nmb_table_data <- function(results,
                                       health_outcome,
                                       cost_outcome,
                                       groups = "overall",
                                       wtp = NULL,
                                       interventions = NULL,
                                       comparators = NULL,
                                       decimals = 0,
                                       font_size = 11) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation")
  }

  # Get WTP if needed
  if (is.null(wtp)) {
    if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Metadata not available. Provide explicit wtp parameter.")
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome))
    }
    wtp <- outcome_meta[["wtp"]][1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.", health_outcome))
    }
  }

  # Use prepare_dsa_tornado_data to get outcome differences (same as dsa_nmb_plot)
  # Always use discounted values for NMB (cost-effectiveness measure)
  outcome_tornado <- prepare_dsa_tornado_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  )

  # Multiply outcome by WTP
  outcome_tornado <- outcome_tornado %>%
    mutate(
      low = .data$low * wtp,
      base = .data$base * wtp,
      high = .data$high * wtp
    )

  # Use prepare_dsa_tornado_data to get cost differences
  # Always use discounted values for NMB (cost-effectiveness measure)
  cost_tornado <- prepare_dsa_tornado_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  )

  # Negate costs (NMB = outcome*wtp - cost)
  cost_tornado <- cost_tornado %>%
    mutate(
      low = -.data$low,
      base = -.data$base,
      high = -.data$high
    )

  # Combine outcomes and costs to get NMB
  # Use full_join to include both cost-only and outcome-only parameters
  nmb_data <- outcome_tornado %>%
    full_join(
      cost_tornado,
      by = c("strategy", "group", "parameter", "parameter_display_name"),
      suffix = c("_outcome", "_cost")
    ) %>%
    mutate(
      # Replace NA with 0 for missing outcome or cost components
      low_outcome = replace_na(.data$low_outcome, 0),
      base_outcome = replace_na(.data$base_outcome, 0),
      high_outcome = replace_na(.data$high_outcome, 0),
      low_cost = replace_na(.data$low_cost, 0),
      base_cost = replace_na(.data$base_cost, 0),
      high_cost = replace_na(.data$high_cost, 0),
      # Calculate NMB
      low = .data$low_outcome + .data$low_cost,
      base = .data$base_outcome + .data$base_cost,
      high = .data$high_outcome + .data$high_cost
    ) %>%
    select("strategy", "group", "parameter", "parameter_display_name", "low", "base", "high")

  # Get unique strategies and groups
  strategies_display <- unique(nmb_data$strategy)
  groups_display <- unique(nmb_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  # Build table data based on mode (same structure as dsa_outcomes_table)
  if (mode == "single_group") {
    # Single group: pivot to wide format with strategy columns
    # Each strategy gets 3 columns: Low, Base, High
    result_data <- nmb_data %>%
      arrange(.data$parameter_display_name) %>%
      select("parameter_display_name", "strategy", "low", "base", "high")

    # Pivot wider to get strategy columns
    result_data <- result_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = c("low", "base", "high"),
        names_glue = "{strategy}_{.value}"
      )

    # Reorder columns to group by strategy
    col_order <- c("parameter_display_name")
    for (strat in strategies_display) {
      col_order <- c(col_order, paste0(strat, "_low"), paste0(strat, "_base"), paste0(strat, "_high"))
    }
    result_data <- result_data[, col_order]

    # Format numeric columns
    for (col in setdiff(colnames(result_data), "parameter_display_name")) {
      if (is.numeric(result_data[[col]])) {
        rounded_vals <- round(result_data[[col]], decimals)
        rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
        result_data[[col]] <- format(rounded_vals, nsmall = decimals, scientific = FALSE, trim = TRUE, big.mark = ",")
      }
    }

    # Rename first column
    names(result_data)[1] <- " "

    # Build three-level headers
    headers <- list()

    # Level 1: Strategy names spanning 3 columns each
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Level 2: Low/Base/High labels
    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))
    for (strat in strategies_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    # Column alignments
    column_alignments <- c("left", rep(c("right", "right", "right"), n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    special_rows <- list()

  } else {
    # Multi-group mode: group header rows with indented parameters
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      # Group header row
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Create group header row (all columns empty except first)
      group_row <- tibble(parameter_display_name = grp)
      for (strat in strategies_display) {
        group_row[[paste0(strat, "_low")]] <- ""
        group_row[[paste0(strat, "_base")]] <- ""
        group_row[[paste0(strat, "_high")]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      # Parameter rows for this group
      grp_data <- nmb_data %>%
        filter(.data$group == grp) %>%
        arrange(.data$parameter_display_name) %>%
        select("parameter_display_name", "strategy", "low", "base", "high") %>%
        pivot_wider(
          names_from = "strategy",
          values_from = c("low", "base", "high"),
          names_glue = "{strategy}_{.value}"
        )

      # Reorder columns
      col_order <- c("parameter_display_name")
      for (strat in strategies_display) {
        col_order <- c(col_order, paste0(strat, "_low"), paste0(strat, "_base"), paste0(strat, "_high"))
      }
      grp_data <- grp_data[, col_order]

      # Format numeric columns
      for (col in setdiff(colnames(grp_data), "parameter_display_name")) {
        if (is.numeric(grp_data[[col]])) {
          rounded_vals <- round(grp_data[[col]], decimals)
          rounded_vals[abs(rounded_vals) < 10^(-decimals-1)] <- 0
          grp_data[[col]] <- format(rounded_vals, nsmall = decimals, scientific = FALSE, trim = TRUE, big.mark = ",")
        }
      }

      # Track indented row indices
      n_param_rows <- nrow(grp_data)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_param_rows))

      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + n_param_rows
    }

    # Rename first column
    names(result_data)[1] <- " "

    # Build three-level headers (same as single group)
    headers <- list()

    # Level 1: Strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Level 2: Low/Base/High
    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))
    for (strat in strategies_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    # Column alignments
    column_alignments <- c("left", rep(c("right", "right", "right"), n_strategies))
    column_widths <- rep(NA, ncol(result_data))

    # Calculate group boundary rows (all group headers except the first)
    group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
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


#' Format DSA Net Monetary Benefit as Summary Table
#'
#' Creates a table showing DSA Net Monetary Benefit with low, base case, and high
#' values for each parameter. NMB = (Difference in Outcomes x WTP) - Difference in Costs.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group, or NULL
#'   (all groups)
#' @param wtp Optional override for willingness-to-pay. If NULL, extracts from
#'   outcome summary metadata.
#' @param interventions Character vector of intervention strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param comparators Character vector of comparator strategy name(s).
#'   At least one of interventions or comparators must be specified.
#' @param decimals Number of decimal places (default: 0 for monetary values)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" (default) or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The table shows each DSA parameter as a row with three columns per comparison:
#' Low, Base, and High NMB values.
#'
#' When multiple groups need to be displayed (groups = NULL), uses group label rows
#' (in bold) followed by indented parameter rows for each group.
#'
#' Unlike dsa_outcomes_table, this function always requires at least one of
#' interventions or comparators since NMB is inherently a comparison metric.
#'
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- define_model("markov") %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#' dsa_results <- run_dsa(model)
#'
#' # Basic DSA NMB table (comparator perspective)
#' dsa_nmb_table(dsa_results, "total_qalys", "total_cost",
#'               comparators = "control")
#'
#' # With explicit WTP
#' dsa_nmb_table(dsa_results, "total_qalys", "total_cost",
#'               interventions = "treatment", wtp = 50000)
#'
#' # All groups with flextable format
#' dsa_nmb_table(dsa_results, "total_qalys", "total_cost",
#'               groups = NULL, comparators = "control",
#'               table_format = "flextable")
#' }
#'
#' @export
dsa_nmb_table <- function(results,
                          health_outcome,
                          cost_outcome,
                          groups = "overall",
                          wtp = NULL,
                          interventions = NULL,
                          comparators = NULL,
                          decimals = 0,
                          font_size = 11,
                          table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_dsa_nmb_table_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    groups = groups,
    wtp = wtp,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
