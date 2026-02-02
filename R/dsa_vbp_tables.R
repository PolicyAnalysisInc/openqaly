#' DSA+VBP Table Functions
#'
#' Functions for creating tables showing value-based prices across DSA
#' parameter variations at specified WTP thresholds.
#'
#' @name dsa_vbp_tables
#' @importFrom dplyr filter mutate arrange select bind_rows group_by summarize pull left_join
#' @importFrom dplyr inner_join distinct ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
NULL

#' DSA+VBP Table
#'
#' Creates a table showing value-based prices at a specified WTP threshold
#' for each DSA parameter variation (low, base, high).
#'
#' @param results DSA+VBP results object from run_dsa() with VBP enabled
#' @param wtp Willingness-to-pay threshold for calculating VBP. If NULL (default),
#'   uses the WTP from the outcome summary specified in the VBP analysis.
#' @param comparators Comparator selection: "all" (default, shows each comparator + aggregate),
#'   "overall" (aggregate only), "all_comparators" (individuals only), or specific comparator name(s)
#' @param groups Group selection: "overall" (default), "all" (overall + all groups),
#'   "all_groups" (all groups without overall), or specific group name(s)
#' @param decimals Number of decimal places for VBP values (default: 0)
#' @param font_size Font size for rendering (default: 11)
#' @param ... Additional arguments (reserved for future use)
#'
#' @return A rendered table object (gt or equivalent)
#'
#' @examples
#' \dontrun{
#' # Create table using default WTP from model
#' dsa_vbp_table(results)
#'
#' # Create table at specific WTP
#' dsa_vbp_table(results, wtp = 50000)
#'
#' # Create table for specific comparator
#' dsa_vbp_table(results, wtp = 50000, comparators = "chemo")
#'
#' # Create table for all groups with 2 decimal places
#' dsa_vbp_table(results, groups = "all", decimals = 2)
#' }
#'
#' @export
dsa_vbp_table <- function(results,
                           wtp = NULL,
                           comparators = "all",
                           groups = "overall",
                           decimals = 0,
                           font_size = 11,
                           ...) {

  # Check that VBP equations exist
  if (is.null(results$dsa_vbp_equations) || nrow(results$dsa_vbp_equations) == 0) {
    stop("No VBP equations found. Ensure run_dsa() was called with VBP parameters.")
  }

  # Get WTP from model metadata if not provided
  if (is.null(wtp)) {
    wtp <- extract_vbp_wtp(results)
  }

  # Prepare table data
  table_info <- prepare_dsa_vbp_table_data(
    results,
    wtp = wtp,
    comparators = comparators,
    groups = groups,
    decimals = decimals,
    font_size = font_size
  )

  # Render using the table renderer
  render_table(table_info)
}

#' Prepare DSA+VBP Table Data
#'
#' Internal helper function that prepares DSA+VBP data for table rendering.
#' Creates a table showing low, base case, and high VBP values for each
#' DSA parameter across comparators.
#'
#' @param results DSA+VBP results object
#' @param wtp Willingness-to-pay threshold
#' @param comparators Comparator selection
#' @param groups Group selection
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with result_data tibble and headers list
#' @keywords internal
prepare_dsa_vbp_table_data <- function(results,
                                        wtp,
                                        comparators,
                                        groups,
                                        decimals,
                                        font_size) {

  equations <- results$dsa_vbp_equations

  # Filter by groups
  if (identical(groups, "overall")) {
    equations <- equations %>% filter(.data$group == "overall")
  } else if (identical(groups, "all")) {
    # Keep all groups including "overall"
  } else if (identical(groups, "all_groups")) {
    equations <- equations %>% filter(.data$group != "overall")
  } else {
    # Specific group names
    equations <- equations %>% filter(.data$group %in% groups)
  }

  # Calculate VBP at specified WTP
  equations <- equations %>%
    mutate(vbp_price = .data$vbp_slope * wtp + .data$vbp_intercept)

  # Get unique comparators for processing
  unique_comparators <- unique(equations$comparator)

  # Process comparators based on selection
  if (identical(comparators, "overall")) {
    # Just the aggregate "All Comparators"
    if (length(unique_comparators) > 1) {
      equations <- equations %>%
        group_by(.data$run_id, .data$parameter, .data$parameter_display_name,
                 .data$parameter_type, .data$variation, .data$override_value, .data$group) %>%
        summarize(
          vbp_price = min(.data$vbp_price),
          comparator = "All Comparators",
          .groups = "drop"
        )
    }
  } else if (identical(comparators, "all")) {
    # All individuals + aggregate
    if (length(unique_comparators) > 1) {
      all_comparators_data <- equations %>%
        group_by(.data$run_id, .data$parameter, .data$parameter_display_name,
                 .data$parameter_type, .data$variation, .data$override_value, .data$group) %>%
        summarize(
          vbp_price = min(.data$vbp_price),
          comparator = "All Comparators",
          .groups = "drop"
        )
      equations <- bind_rows(equations, all_comparators_data)
    }
  } else if (identical(comparators, "all_comparators")) {
    # Just individuals, no aggregate - keep equations as-is
  } else {
    # Specific comparator names
    equations <- equations %>% filter(.data$comparator %in% comparators)
  }

  # Create comparison label with display names
  equations <- equations %>%
    mutate(comparison_label = ifelse(
      .data$comparator == "All Comparators",
      "vs. All Comparators",
      paste0("vs. ", map_names_if_available(.data$comparator, results$metadata$strategies))
    ))

  # Get unique dimensions
  unique_comparisons <- unique(equations$comparison_label)
  unique_groups <- unique(equations$group)
  n_comparisons <- length(unique_comparisons)
  n_groups <- length(unique_groups)

  # Separate base case from variations
  base_data <- equations %>%
    filter(.data$variation == "base") %>%
    select("comparison_label", "group", base = "vbp_price")

  low_data <- equations %>%
    filter(.data$variation == "low") %>%
    select("comparison_label", "group", "parameter", "parameter_display_name", low = "vbp_price")

  high_data <- equations %>%
    filter(.data$variation == "high") %>%
    select("comparison_label", "group", "parameter", "parameter_display_name", high = "vbp_price")

  # Combine data
  combined_data <- low_data %>%
    inner_join(high_data, by = c("comparison_label", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("comparison_label", "group"))

  # Map group display names
  if (!is.null(results$metadata$groups)) {
    combined_data$group <- map_names(combined_data$group, results$metadata$groups, "display_name")
    # Update unique_groups to use display names for iteration
    unique_groups <- unique(combined_data$group)
  }

  # Determine table mode based on dimensions
  mode <- if (n_groups > 1) "multi_group" else "single_group"

  # Build table based on mode
  if (mode == "single_group") {
    # Single group: pivot to wide format with comparison columns
    # Each comparison gets 3 columns: Low, Base, High
    result_data <- combined_data %>%
      arrange(.data$parameter_display_name) %>%
      select("parameter_display_name", "comparison_label", "low", "base", "high")

    # Pivot wider to get comparison columns
    result_data <- result_data %>%
      pivot_wider(
        names_from = "comparison_label",
        values_from = c("low", "base", "high"),
        names_glue = "{comparison_label}_{.value}"
      )

    # Reorder columns to group by comparison
    col_order <- c("parameter_display_name")
    for (comp in unique_comparisons) {
      col_order <- c(col_order, paste0(comp, "_low"), paste0(comp, "_base"), paste0(comp, "_high"))
    }
    result_data <- result_data[, col_order]

    # Format numeric columns
    for (col in setdiff(colnames(result_data), "parameter_display_name")) {
      if (is.numeric(result_data[[col]])) {
        rounded_vals <- round(result_data[[col]], decimals)
        result_data[[col]] <- scales::comma(rounded_vals, accuracy = 10^(-decimals))
      }
    }

    # Rename first column
    names(result_data)[1] <- " "

    # Build three-level headers
    headers <- list()

    # Level 1: Comparison names spanning 3 columns each
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 1, 0))
    for (comp in unique_comparisons) {
      row1[[length(row1) + 1]] <- list(span = 3, text = comp, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    # Level 2: Low/Base/High for each comparison
    row2 <- list()
    row2[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
    for (i in seq_along(unique_comparisons)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(1, 0, 0, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 0, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 0, 1))
    }
    headers[[2]] <- row2

  } else {
    # Multi-group mode: group header rows with indented parameters
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in unique_groups) {
      # Group header row
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      # Create group header row (all columns empty except first)
      group_row <- tibble(parameter_display_name = grp)
      for (comp in unique_comparisons) {
        group_row[[paste0(comp, "_low")]] <- ""
        group_row[[paste0(comp, "_base")]] <- ""
        group_row[[paste0(comp, "_high")]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      # Parameter rows for this group
      grp_data <- combined_data %>%
        filter(.data$group == grp) %>%
        arrange(.data$parameter_display_name) %>%
        select("parameter_display_name", "comparison_label", "low", "base", "high") %>%
        pivot_wider(
          names_from = "comparison_label",
          values_from = c("low", "base", "high"),
          names_glue = "{comparison_label}_{.value}"
        )

      # Reorder columns
      col_order <- c("parameter_display_name")
      for (comp in unique_comparisons) {
        col_order <- c(col_order, paste0(comp, "_low"), paste0(comp, "_base"), paste0(comp, "_high"))
      }
      grp_data <- grp_data[, col_order]

      # Format numeric columns
      for (col in setdiff(colnames(grp_data), "parameter_display_name")) {
        if (is.numeric(grp_data[[col]])) {
          rounded_vals <- round(grp_data[[col]], decimals)
          grp_data[[col]] <- scales::comma(rounded_vals, accuracy = 10^(-decimals))
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

    # Build headers (same as single_group mode)
    headers <- list()
    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, 0, 0))
    for (comp in unique_comparisons) {
      row1[[length(row1) + 1]] <- list(span = 3, text = comp, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    row2 <- list()
    row2[[1]] <- list(span = 1, text = "", borders = c(0, 0, 1, 0))
    for (i in seq_along(unique_comparisons)) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    # Column alignments
    n_cols <- ncol(result_data)
    column_alignments <- c("left", rep("right", n_cols - 1))

    # Calculate group boundary rows (all group headers except the first)
    group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()
  }

  # Column alignments and special_rows for single_group mode
  if (mode == "single_group") {
    n_cols <- ncol(result_data)
    column_alignments <- c("left", rep("right", n_cols - 1))
    special_rows <- list()
  } else {
    # Multi-group mode special_rows already defined above
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
  }

  # Create the table spec using the common helper
  create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = column_alignments,
    column_widths = rep(NA, n_cols),
    special_rows = special_rows,
    font_size = font_size,
    font_family = "Helvetica"
  )
}
