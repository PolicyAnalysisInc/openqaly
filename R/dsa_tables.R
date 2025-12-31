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
    group = groups,
    strategies = strategies,
    discounted = discounted
  )

  # Separate base case from variations
  # Note: base case has parameter="base" for all rows, so we don't join on parameter
  base_data <- dsa_data %>%
    filter(run_id == 1) %>%
    select(strategy, group, base = amount)

  low_data <- dsa_data %>%
    filter(variation == "low") %>%
    select(strategy, group, parameter, parameter_display_name, low = amount)

  high_data <- dsa_data %>%
    filter(variation == "high") %>%
    select(strategy, group, parameter, parameter_display_name, high = amount)

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
        names_from = strategy,
        values_from = c(low, base, high),
        id_cols = c(group, parameter, parameter_display_name)
      )

    # Get all strategies
    all_strategies <- unique(dsa_data$strategy)
    other_strategies <- setdiff(all_strategies, intervention_strategy)

    # Create comparison labels using display names
    # intervention_strategy and other_strategies are technical names
    intervention_mapped <- map_names(intervention_strategy, results$metadata$strategies, "display_name")
    other_mapped <- map_names(other_strategies, results$metadata$strategies, "display_name")

    # Create comparison labels
    if (!is.null(intervention)) {
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

      if (!is.null(intervention)) {
        diff_df <- data_wide %>%
          mutate(
            strategy = comp_label,
            low = !!sym(low_int_col) - !!sym(low_other_col),
            base = !!sym(base_int_col) - !!sym(base_other_col),
            high = !!sym(high_int_col) - !!sym(high_other_col)
          ) %>%
          select(group, parameter, parameter_display_name, strategy, low, base, high)
      } else {
        diff_df <- data_wide %>%
          mutate(
            strategy = comp_label,
            low = !!sym(low_other_col) - !!sym(low_int_col),
            base = !!sym(base_other_col) - !!sym(base_int_col),
            high = !!sym(high_other_col) - !!sym(high_int_col)
          ) %>%
          select(group, parameter, parameter_display_name, strategy, low, base, high)
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
  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Determine mode
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  # Build table data based on mode
  if (mode == "single_group") {
    # Single group: pivot to wide format with strategy columns
    # Each strategy gets 3 columns: Low, Base, High
    result_data <- combined_data %>%
      arrange(parameter_display_name) %>%
      select(parameter_display_name, strategy, low, base, high)

    # Pivot wider to get strategy columns
    result_data <- result_data %>%
      pivot_wider(
        names_from = strategy,
        values_from = c(low, base, high),
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
        filter(group == grp) %>%
        arrange(parameter_display_name) %>%
        mutate(parameter_display_name = paste0("  ", parameter_display_name)) %>%
        select(parameter_display_name, strategy, low, base, high) %>%
        pivot_wider(
          names_from = strategy,
          values_from = c(low, base, high),
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

      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + nrow(grp_data)
    }

    # Rename first column
    names(result_data)[1] <- " "

    # Build three-level headers (same as single group)
    headers <- list()

    # Level 1: Strategy names
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
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

    special_rows <- list(group_header_rows = group_header_rows)
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
                               table_format = c("kable", "flextable")) {

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
