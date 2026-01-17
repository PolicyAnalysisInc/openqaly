#' VBP Table Output Functions
#'
#' Functions for generating value-based pricing tables showing VBP at
#' different willingness-to-pay thresholds.
#'
#' @name vbp_tables
#' @keywords internal
NULL

#' Generate Default WTP Sequence for VBP
#'
#' Creates a sensible WTP sequence centered around the default WTP
#' from the outcome summary metadata.
#'
#' @param default_wtp The default WTP from metadata
#' @param n_points Number of points in the sequence (default: 5)
#' @return Numeric vector of WTP thresholds
#' @keywords internal
generate_vbp_wtp_sequence <- function(default_wtp, n_points = 5) {
  if (is.null(default_wtp) || is.na(default_wtp) || default_wtp <= 0) {
    # Fallback to standard range if no valid default

    return(c(0, 50000, 100000, 150000, 200000))
  }


  # Generate sequence from 0 to 2x default, ensuring default is included
  max_wtp <- default_wtp * 2
  step <- max_wtp / (n_points - 1)

  # Round step to a nice number
  magnitude <- 10^floor(log10(step))
  step <- ceiling(step / magnitude) * magnitude

  # Generate sequence
  seq_vals <- seq(0, max_wtp, by = step)

  # Ensure default_wtp is in the sequence

  if (!default_wtp %in% seq_vals) {
    seq_vals <- sort(unique(c(seq_vals, default_wtp)))
  }

  seq_vals
}

#' Extract Default WTP from VBP Results
#'
#' Extracts the default WTP threshold from the outcome summary metadata
#' in the VBP results.
#'
#' @param vbp_results Results from run_vbp()
#' @return Numeric WTP value or NULL if not found
#' @keywords internal
extract_vbp_default_wtp <- function(vbp_results) {
  # VBP results contain aggregated results which have metadata
  if (is.null(vbp_results$aggregated) ||
      is.null(attr(vbp_results$aggregated, "metadata"))) {
    return(NULL)
  }

  metadata <- attr(vbp_results$aggregated, "metadata")
  if (is.null(metadata$summaries)) {
    return(NULL)
  }

  outcome_name <- vbp_results$spec$outcome_summary
  outcome_meta <- metadata$summaries %>%
    filter(.data$name == outcome_name)

  if (nrow(outcome_meta) == 0 || is.null(outcome_meta$wtp)) {
    return(NULL)
  }

  outcome_meta$wtp[1]
}

#' Prepare VBP Table Data
#'
#' Internal helper function that prepares VBP data for rendering as a table.
#' Calculates VBP prices at each WTP threshold for each comparator.
#'
#' @param vbp_results Results from run_vbp()
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as rows.
#'   If NULL, generates a default sequence based on outcome metadata.
#' @param comparators Character vector of comparators to include.
#'   NULL for all comparators.
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (default)
#'     \item \code{"group_name"} - Specific group by name
#'   }
#' @param include_all_comparators Logical. Include a column showing the
#'   minimum VBP across all comparators (price to be cost-effective vs all)?
#' @param decimals Number of decimal places for VBP values
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_vbp_table_data <- function(vbp_results,
                                   wtp_thresholds = NULL,
                                   comparators = NULL,
                                   groups = "overall",
                                   include_all_comparators = TRUE,
                                   decimals = 0,
                                   font_size = 11) {

  # Handle group selection for tables
  if (is.null(groups) || identical(groups, "all")) {
    # Use all groups from metadata + overall (overall last, matching CE table pattern)
    groups_to_process <- c(vbp_results$vbp_metadata$groups, "overall")
    groups_to_process <- unique(groups_to_process)
  } else if (identical(groups, "all_groups")) {
    # All groups except overall
    groups_to_process <- setdiff(vbp_results$vbp_metadata$groups, "overall")
  } else {
    groups_to_process <- groups
  }

  # Reorder groups: Overall first, then model definition order
  metadata <- attr(vbp_results$aggregated, "metadata")
  # Map "overall" to "Overall" for consistency with get_group_order
  groups_to_process[groups_to_process == "overall"] <- "Overall"
  groups_to_process <- get_group_order(groups_to_process, metadata)
  # Map back to internal name for data lookup
  groups_to_process[groups_to_process == "Overall"] <- "overall"

  # Check if any valid groups remain after filtering

  if (length(groups_to_process) == 0) {
    stop(sprintf("No VBP equations found for the specified group(s): %s", paste(groups, collapse = ", ")))
  }

  # If multiple groups, process each and combine
  if (length(groups_to_process) > 1) {
    return(prepare_vbp_table_data_multi_group(
      vbp_results = vbp_results,
      wtp_thresholds = wtp_thresholds,
      comparators = comparators,
      groups_to_process = groups_to_process,
      include_all_comparators = include_all_comparators,
      decimals = decimals,
      font_size = font_size
    ))
  }

  # Single group processing
  grp <- groups_to_process[1]

  # Get available comparators from VBP equations
  if (grp == "overall") {
    equations <- vbp_results$vbp_equations
  } else {
    equations <- vbp_results$vbp_equations_by_group %>%
      filter(.data$group == grp)
  }

  if (nrow(equations) == 0) {
    stop(sprintf("No VBP equations found for group: %s", grp))
  }

  available_comparators <- unique(equations$comparator)

  # Filter comparators if specified
  if (!is.null(comparators)) {
    invalid <- setdiff(comparators, available_comparators)
    if (length(invalid) > 0) {
      stop(sprintf("Comparator(s) not found: %s. Available: %s",
                   paste(invalid, collapse = ", "),
                   paste(available_comparators, collapse = ", ")))
    }
    comparators_display <- comparators
  } else {
    comparators_display <- available_comparators
  }

  # Generate WTP thresholds if not provided
  if (is.null(wtp_thresholds)) {
    default_wtp <- extract_vbp_default_wtp(vbp_results)
    wtp_thresholds <- generate_vbp_wtp_sequence(default_wtp)
  }

  # Map comparator names to display names
  metadata <- attr(vbp_results$aggregated, "metadata")
  comparators_for_calc <- comparators_display  # Keep original names for calculation
  if (!is.null(metadata) && !is.null(metadata$strategies)) {
    comparators_display <- map_names(comparators_display,
                                      metadata$strategies,
                                      "display_name")
  }

  # Calculate VBP at each WTP for each comparator
  vbp_data <- expand_grid(
    wtp = wtp_thresholds,
    comparator = comparators_for_calc
  ) %>%
    rowwise() %>%
    mutate(
      vbp_price = calculate_vbp_price(
        vbp_results,
        wtp = .data$wtp,
        comparator = .data$comparator,
        group = grp
      )
    ) %>%
    ungroup()

  # Map comparator names to display names in the data
  if (!is.null(metadata) && !is.null(metadata$strategies)) {
    vbp_data$comparator <- map_names(vbp_data$comparator,
                                      metadata$strategies,
                                      "display_name")
  }

  # Calculate "All Comparators" column (minimum VBP at each WTP)
  if (include_all_comparators && length(comparators_display) > 1) {
    all_comp_data <- vbp_data %>%
      group_by(.data$wtp) %>%
      summarize(vbp_price = min(.data$vbp_price), .groups = "drop") %>%
      mutate(comparator = "All Comparators")

    vbp_data <- bind_rows(vbp_data, all_comp_data)
    comparators_display <- c(comparators_display, "All Comparators")
  }

  # Pivot to wide format: WTP as rows, comparators as columns
  pivot_data <- vbp_data %>%
    pivot_wider(
      id_cols = "wtp",
      names_from = "comparator",
      values_from = "vbp_price"
    ) %>%
    arrange(.data$wtp)

  # Format WTP column with thousand separators
  pivot_data <- pivot_data %>%
    mutate(wtp_display = comma(.data$wtp))

  # Format VBP columns with thousand separators
  for (comp in comparators_display) {
    if (comp %in% colnames(pivot_data) && is.numeric(pivot_data[[comp]])) {
      pivot_data[[comp]] <- comma(
        round(pivot_data[[comp]], decimals),
        accuracy = if (decimals == 0) 1 else 10^(-decimals)
      )
    }
  }

  # Build result columns
  result_cols <- pivot_data %>%
    select("wtp_display", all_of(comparators_display))
  colnames(result_cols)[1] <- "WTP"

  # Rename comparator columns to "vs. X" format
  for (i in seq_along(comparators_display)) {
    comp <- comparators_display[i]
    if (comp != "All Comparators") {
      colnames(result_cols)[i + 1] <- paste0("vs. ", comp)
    }
  }

  # Build headers
  headers <- list()
  header_labels <- colnames(result_cols)

  row1 <- list()
  for (i in seq_along(header_labels)) {
    row1[[i]] <- list(
      span = 1,
      text = header_labels[i],
      borders = c(1, 0, 1, 0)
    )
  }
  headers[[1]] <- row1

  # Column alignments: left for WTP, right for prices
  column_alignments <- c("left", rep("right", length(comparators_display)))
  column_widths <- rep(NA, length(column_alignments))

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

#' Prepare VBP Table Data for Multiple Groups
#'
#' Internal helper function that prepares VBP data for rendering as a table
#' when multiple groups are selected. Creates a table with group header rows
#' and indented WTP rows, matching the CE table pattern.
#'
#' @param vbp_results Results from run_vbp()
#' @param wtp_thresholds Numeric vector of WTP thresholds
#' @param comparators Character vector of comparators
#' @param groups_to_process Character vector of groups to include
#' @param include_all_comparators Logical. Include all comparators column?
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_vbp_table_data_multi_group <- function(vbp_results,
                                                wtp_thresholds = NULL,
                                                comparators = NULL,
                                                groups_to_process,
                                                include_all_comparators = TRUE,
                                                decimals = 0,
                                                font_size = 11) {

  # Generate WTP thresholds if not provided
  if (is.null(wtp_thresholds)) {
    default_wtp <- extract_vbp_default_wtp(vbp_results)
    wtp_thresholds <- generate_vbp_wtp_sequence(default_wtp)
  }

  # Get metadata for name mapping
  metadata <- attr(vbp_results$aggregated, "metadata")

  # Determine comparators to use (check first group that has data)
  comparators_for_calc <- NULL
  for (grp in groups_to_process) {
    if (grp == "overall") {
      equations <- vbp_results$vbp_equations
    } else {
      equations <- vbp_results$vbp_equations_by_group %>%
        filter(.data$group == grp)
    }
    if (nrow(equations) > 0) {
      available_comparators <- unique(equations$comparator)
      if (!is.null(comparators)) {
        invalid <- setdiff(comparators, available_comparators)
        if (length(invalid) > 0) {
          stop(sprintf("Comparator(s) not found: %s. Available: %s",
                       paste(invalid, collapse = ", "),
                       paste(available_comparators, collapse = ", ")))
        }
        comparators_for_calc <- comparators
      } else {
        comparators_for_calc <- available_comparators
      }
      break
    }
  }

  if (is.null(comparators_for_calc)) {
    stop("No VBP data found for any of the specified groups")
  }

  # Map comparator names to display names
  comparators_display <- comparators_for_calc
  if (!is.null(metadata) && !is.null(metadata$strategies)) {
    comparators_display <- map_names(comparators_for_calc,
                                      metadata$strategies,
                                      "display_name")
  }

  # Build column names (WTP + comparators + optionally All Comparators)
  col_names <- c("WTP")
  for (comp in comparators_display) {
    col_names <- c(col_names, paste0("vs. ", comp))
  }
  if (include_all_comparators && length(comparators_display) > 1) {
    col_names <- c(col_names, "All Comparators")
  }

  # Build result data with group headers (like CE tables)
  result_data <- tibble()
  group_header_rows <- integer()
  indented_rows <- integer()
  current_row <- 0

  for (grp in groups_to_process) {
    # Get equations for this group
    if (grp == "overall") {
      equations <- vbp_results$vbp_equations
    } else {
      equations <- vbp_results$vbp_equations_by_group %>%
        filter(.data$group == grp)
    }

    if (nrow(equations) == 0) {
      next
    }

    # Get display name for group
    grp_display <- grp
    if (grp == "overall") {
      grp_display <- "Overall"
    } else if (!is.null(metadata) && !is.null(metadata$groups)) {
      grp_display <- map_names(grp, metadata$groups, "display_name")
    }

    # Add group header row (bold + italic, empty price cells)
    current_row <- current_row + 1
    group_header_rows <- c(group_header_rows, current_row)

    header_row <- tibble(WTP = grp_display)
    for (comp in comparators_display) {
      header_row[[paste0("vs. ", comp)]] <- ""
    }
    if (include_all_comparators && length(comparators_display) > 1) {
      header_row[["All Comparators"]] <- ""
    }
    result_data <- bind_rows(result_data, header_row)

    # Calculate VBP at each WTP for this group
    grp_rows <- tibble()
    for (wtp_val in wtp_thresholds) {
      row_data <- tibble(WTP = comma(wtp_val))

      for (i in seq_along(comparators_for_calc)) {
        comp <- comparators_for_calc[i]
        comp_display <- comparators_display[i]
        vbp_price <- calculate_vbp_price(
          vbp_results,
          wtp = wtp_val,
          comparator = comp,
          group = grp
        )
        row_data[[paste0("vs. ", comp_display)]] <- comma(
          round(vbp_price, decimals),
          accuracy = if (decimals == 0) 1 else 10^(-decimals)
        )
      }

      # Calculate "All Comparators" (minimum VBP)
      if (include_all_comparators && length(comparators_for_calc) > 1) {
        all_vbps <- sapply(comparators_for_calc, function(comp) {
          calculate_vbp_price(vbp_results, wtp = wtp_val, comparator = comp, group = grp)
        })
        min_vbp <- min(all_vbps)
        row_data[["All Comparators"]] <- comma(
          round(min_vbp, decimals),
          accuracy = if (decimals == 0) 1 else 10^(-decimals)
        )
      }

      grp_rows <- bind_rows(grp_rows, row_data)
    }

    # Track indented row indices
    n_wtp_rows <- nrow(grp_rows)
    indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_wtp_rows))

    result_data <- bind_rows(result_data, grp_rows)
    current_row <- current_row + n_wtp_rows
  }

  # Calculate group boundaries (all group headers except the first)
  group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()

  # Build headers
  headers <- list()
  row1 <- list()
  for (i in seq_along(col_names)) {
    row1[[i]] <- list(
      span = 1,
      text = col_names[i],
      borders = c(1, 0, 1, 0)
    )
  }
  headers[[1]] <- row1

  # Column alignments: left for WTP, right for prices
  n_price_cols <- length(col_names) - 1
  column_alignments <- c("left", rep("right", n_price_cols))
  column_widths <- rep(NA, length(column_alignments))

  # Special rows for group formatting
  special_rows <- list(
    group_header_rows = group_header_rows,
    group_boundary_rows = group_boundary_rows,
    indented_rows = indented_rows
  )

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

#' Format VBP as Table
#'
#' Creates a table showing the value-based price at different willingness-to-pay
#' thresholds for each comparator strategy.
#'
#' @param vbp_results Results from run_vbp()
#' @param wtp_thresholds Numeric vector of WTP thresholds to show as rows.
#'   If NULL, generates a default sequence based on outcome summary metadata.
#' @param comparators Character vector of comparators to include.
#'   NULL for all comparators.
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{"all_groups"} - All groups (excluding overall)
#'     \item \code{NULL} or \code{"all"} - All groups plus overall
#'   }
#' @param include_all_comparators Logical. Include a column showing the
#'   minimum VBP across all comparators? This represents the maximum price
#'   that is cost-effective versus ALL alternatives. Default TRUE.
#' @param decimals Number of decimal places for VBP values (default: 0)
#' @param font_size Font size for rendering (default: 11)
#' @param table_format Character. Backend to use: "flextable" or "kable"
#'
#' @return A table object (flextable or kable depending on table_format)
#'
#' @details
#' The VBP table displays the maximum price the intervention can charge while
#' remaining cost-effective at each WTP threshold. The "All Comparators" column
#' shows the minimum VBP across all comparators - this is the price that ensures
#' cost-effectiveness versus ALL alternatives simultaneously.
#'
#' VBP is calculated using the linear equation:
#' \deqn{VBP = slope \times WTP + intercept}
#'
#' where the slope and intercept are derived from the incremental outcomes and
#' costs between the intervention and comparator strategies.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#'
#' vbp_results <- run_vbp(
#'   model,
#'   price_variable = "c_drug",
#'   intervention_strategy = "targeted"
#' )
#'
#' # VBP table with auto-generated WTP thresholds
#' vbp_table(vbp_results)
#'
#' # VBP table with custom WTP thresholds
#' vbp_table(vbp_results, wtp_thresholds = c(0, 50000, 100000, 150000))
#'
#' # VBP table for specific comparator
#' vbp_table(vbp_results, comparators = "chemo")
#' }
#'
#' @export
vbp_table <- function(vbp_results,
                      wtp_thresholds = NULL,
                      comparators = NULL,
                      groups = "overall",
                      include_all_comparators = TRUE,
                      decimals = 0,
                      font_size = 11,
                      table_format = c("flextable", "kable")) {

  table_format <- match.arg(table_format)

  # Prepare data
  prepared <- prepare_vbp_table_data(
    vbp_results = vbp_results,
    wtp_thresholds = wtp_thresholds,
    comparators = comparators,
    groups = groups,
    include_all_comparators = include_all_comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render using specified backend
  render_table(prepared, format = table_format)
}
