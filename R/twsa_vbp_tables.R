#' TWSA Value-Based Pricing Tables
#'
#' Functions for creating table outputs of TWSA+VBP analysis results
#' in grid format.
#'
#' @name twsa_vbp_tables
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup summarize
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#' @importFrom glue glue
NULL

#' Prepare TWSA VBP Table Data
#'
#' Internal helper function that prepares TWSA VBP data for table rendering.
#' Creates a grid format with X parameter values as columns and Y parameter
#' values as rows.
#'
#' @param results TWSA results object with VBP equations
#' @param wtp Willingness-to-pay threshold
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection
#' @param comparators Comparator selection
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#'
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_twsa_vbp_table_data <- function(results,
                                         wtp,
                                         twsa_name = NULL,
                                         groups = "overall",
                                         comparators = "all",
                                         decimals = 0,
                                         font_size = 11) {

  # Get VBP heatmap data using the shared preparation function
  vbp_data <- prepare_twsa_vbp_heatmap_data(
    results = results,
    wtp = wtp,
    twsa_name = twsa_name,
    groups = groups,
    comparators = comparators
  )

  if (nrow(vbp_data) == 0) {
    stop("No VBP data found for table", call. = FALSE)
  }

  # Get parameter display names
  x_param_name <- unique(vbp_data$x_param_display_name)[1]
  y_param_name <- unique(vbp_data$y_param_display_name)[1]

  # Get unique values
  strategies_display <- unique(vbp_data$strategy)
  groups_display <- unique(vbp_data$group)

  # Reorder groups: Overall first, then model definition order
  groups_display <- get_group_order(groups_display, results$metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)

  # Build table for each strategy/group combination
  tables <- list()

  for (strat in strategies_display) {
    for (grp in groups_display) {
      subset_data <- vbp_data %>%
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

      # Format numeric values as currency
      x_cols <- setdiff(names(wide_data), "y_value")
      for (col in x_cols) {
        if (is.numeric(wide_data[[col]])) {
          rounded_vals <- round(wide_data[[col]], decimals)
          wide_data[[col]] <- format(rounded_vals, nsmall = decimals,
                                      big.mark = ",", scientific = FALSE, trim = TRUE)
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

#' TWSA VBP Table
#'
#' Creates a grid table showing Value-Based Prices across the TWSA parameter grid.
#' VBP = slope x WTP + intercept.
#'
#' @param results TWSA results object from run_twsa() with VBP enabled
#' @param twsa_name Name of specific TWSA analysis to show (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param comparators Comparator selection: "all" (default, shows each comparator + aggregate),
#'   "overall" (aggregate only), "all_comparators" (individuals only), or specific names
#' @param wtp Willingness-to-pay threshold. If NULL, extracts from VBP metadata.
#' @param decimals Number of decimal places (default: 0)
#' @param font_size Font size for table (default: 11)
#' @param backend Table rendering backend: "flextable" (default) or "kable"
#'
#' @return A rendered table object (flextable or kable)
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model, vbp_price_variable = "price",
#'                     vbp_intervention = "treatment")
#'
#' # VBP table at specific WTP
#' twsa_vbp_table(results, wtp = 50000)
#'
#' # VBP table for specific comparator
#' twsa_vbp_table(results, wtp = 50000, comparators = "standard_care")
#' }
twsa_vbp_table <- function(results,
                            twsa_name = NULL,
                            groups = "overall",
                            comparators = "all",
                            wtp = NULL,
                            decimals = 0,
                            font_size = 11,
                            backend = c("flextable", "kable")) {

  backend <- match.arg(backend)

  # Check VBP equations exist
  if (is.null(results$twsa_vbp_equations) || nrow(results$twsa_vbp_equations) == 0) {
    stop("No VBP equations found. Run run_twsa() with vbp_price_variable specified.",
         call. = FALSE)
  }

  # Get WTP if not provided
  if (is.null(wtp)) {
    wtp <- extract_vbp_wtp(results)
  }

  # Prepare table data
  prepared <- prepare_twsa_vbp_table_data(
    results = results,
    wtp = wtp,
    twsa_name = twsa_name,
    groups = groups,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size
  )

  # Render grid table using shared helper
  render_twsa_grid_table(prepared, font_size, backend)
}
