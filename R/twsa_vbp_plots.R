#' TWSA Value-Based Pricing Visualizations
#'
#' Functions for creating heatmap visualizations of TWSA+VBP analysis results.
#'
#' @name twsa_vbp_plots
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup summarize
#' @importFrom ggplot2 ggplot aes geom_tile geom_point scale_fill_viridis_c
#' @importFrom ggplot2 facet_wrap vars labs theme_bw theme element_text
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous
#' @importFrom scales comma
#' @importFrom tibble tibble
#' @importFrom glue glue
NULL

#' Prepare TWSA VBP Heatmap Data
#'
#' Internal helper function that prepares TWSA VBP data for heatmap rendering.
#' Calculates VBP at the specified WTP for each grid point.
#'
#' @param results TWSA results object with VBP equations
#' @param wtp Willingness-to-pay threshold
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection
#' @param comparators Comparator selection
#'
#' @return Tibble with x_value, y_value, value, comparator, group, and parameter info
#' @keywords internal
prepare_twsa_vbp_heatmap_data <- function(results,
                                           wtp,
                                           twsa_name = NULL,
                                           groups = "overall",
                                           comparators = "all") {

  equations <- results$twsa_vbp_equations

  if (is.null(equations) || nrow(equations) == 0) {
    stop("No VBP equations found. Run run_twsa() with VBP parameters.",
         call. = FALSE)
  }

  # Filter by groups
  if (identical(groups, "overall")) {
    equations <- equations %>% filter(.data$group == "_aggregated")
  } else if (identical(groups, "all")) {
    # Keep all groups
  } else if (identical(groups, "all_groups")) {
    equations <- equations %>% filter(.data$group != "_aggregated")
  } else {
    equations <- equations %>% filter(.data$group %in% groups)
  }

  # Filter to specific TWSA if requested
  if (!is.null(twsa_name)) {
    equations <- equations %>%
      filter(.data$twsa_name == !!twsa_name | is.na(.data$twsa_name))
  } else {
    available_twsa <- unique(equations$twsa_name[!is.na(equations$twsa_name)])
    if (length(available_twsa) > 1) {
      warning(glue("Multiple TWSA analyses found: {paste(available_twsa, collapse=', ')}. ",
                   "Using '{available_twsa[1]}'. Specify twsa_name to select a different one."))
      equations <- equations %>%
        filter(.data$twsa_name == available_twsa[1] | is.na(.data$twsa_name))
    }
  }

  # Filter out base case (run_id = 1)
  equations <- equations %>%
    filter(!is.na(.data$twsa_name))

  if (nrow(equations) == 0) {
    stop("No TWSA VBP grid data found for plotting", call. = FALSE)
  }

  # Calculate VBP at specified WTP
  equations <- equations %>%
    mutate(vbp_price = .data$vbp_slope * wtp + .data$vbp_intercept)

  # Get unique comparators for processing
  unique_comparators <- unique(equations$comparator)

  # Process comparators based on selection
  if (identical(comparators, "overall")) {
    # Just the aggregate (minimum across comparators)
    if (length(unique_comparators) > 1) {
      equations <- equations %>%
        group_by(.data$run_id, .data$twsa_name, .data$x_value, .data$y_value, .data$group) %>%
        summarize(
          vbp_price = min(.data$vbp_price, na.rm = TRUE),
          comparator = "All Comparators",
          .groups = "drop"
        )
    }
  } else if (identical(comparators, "all")) {
    # All individuals + aggregate
    if (length(unique_comparators) > 1) {
      all_comparators_data <- equations %>%
        group_by(.data$run_id, .data$twsa_name, .data$x_value, .data$y_value, .data$group) %>%
        summarize(
          vbp_price = min(.data$vbp_price, na.rm = TRUE),
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

  # Get TWSA metadata for base case and parameter names
  if (!is.null(results$twsa_metadata)) {
    meta_cols <- results$twsa_metadata %>%
      filter(!is.na(.data$twsa_name)) %>%
      select("run_id", "x_param_display_name", "y_param_display_name",
             "x_bc_value", "y_bc_value") %>%
      distinct()

    equations <- equations %>%
      left_join(meta_cols, by = "run_id")
  }

  # Create strategy label for comparator
  equations <- equations %>%
    mutate(strategy = ifelse(
      .data$comparator == "All Comparators",
      "vs. All Comparators",
      paste0("vs. ", map_names_if_available(.data$comparator, results$metadata$strategies))
    ))

  # Map group display names
  if (!is.null(results$metadata$groups)) {
    equations$group <- map_names(equations$group, results$metadata$groups, "display_name")
  }

  # Rename for render_twsa_heatmap compatibility
  equations %>%
    mutate(value = .data$vbp_price) %>%
    select("run_id", "x_value", "y_value", "group", "strategy", "value",
           "x_param_display_name", "y_param_display_name",
           "x_bc_value", "y_bc_value")
}

#' TWSA VBP Heatmap Plot
#'
#' Creates a heatmap visualization showing the Value-Based Price (VBP)
#' across a 2D grid of parameter values. VBP = slope x WTP + intercept.
#'
#' @param results TWSA results object from run_twsa() with VBP enabled
#' @param twsa_name Name of specific TWSA analysis to plot (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param comparators Comparator selection: "all" (default, shows each comparator + aggregate),
#'   "overall" (aggregate only), "all_comparators" (individuals only), or specific names
#' @param wtp Willingness-to-pay threshold. If NULL, extracts from VBP metadata.
#' @param title Optional plot title
#' @param xlab Optional x-axis label (defaults to X parameter name)
#' @param ylab Optional y-axis label (defaults to Y parameter name)
#' @param viridis_option Viridis color palette: "viridis" (default), "magma",
#'   "plasma", "inferno", or "cividis"
#' @param show_base_case Logical. Show base case point marker? (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model, vbp_price_variable = "price",
#'                     vbp_intervention = "treatment")
#'
#' # VBP heatmap at specific WTP
#' twsa_vbp_plot(results, wtp = 50000)
#'
#' # VBP heatmap using WTP from metadata
#' twsa_vbp_plot(results, comparators = "standard_care")
#' }
twsa_vbp_plot <- function(results,
                           twsa_name = NULL,
                           groups = "overall",
                           comparators = "all",
                           wtp = NULL,
                           title = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           viridis_option = "viridis",
                           show_base_case = TRUE) {

  # Check VBP equations exist
  if (is.null(results$twsa_vbp_equations) || nrow(results$twsa_vbp_equations) == 0) {
    stop("No VBP equations found. Run run_twsa() with vbp_price_variable specified.",
         call. = FALSE)
  }

  # Validate viridis option
  viridis_option <- match.arg(viridis_option,
                               c("viridis", "magma", "plasma", "inferno", "cividis"))

  # Get WTP if not provided
  if (is.null(wtp)) {
    wtp <- extract_vbp_wtp(results)
  }

  # Prepare VBP heatmap data
  vbp_data <- prepare_twsa_vbp_heatmap_data(
    results = results,
    wtp = wtp,
    twsa_name = twsa_name,
    groups = groups,
    comparators = comparators
  )

  # Check if data is valid
  if (nrow(vbp_data) == 0) {
    stop("No data available for VBP heatmap with specified parameters",
         call. = FALSE)
  }

  # Create legend title with WTP
  wtp_formatted <- scales::comma(wtp)
  legend_title <- glue("VBP (\u03bb = {wtp_formatted})")

  # Render heatmap
  render_twsa_heatmap(
    heatmap_data = vbp_data,
    title = title,
    xlab = xlab,
    ylab = ylab,
    legend_title = legend_title,
    viridis_option = viridis_option,
    show_base_case = show_base_case
  )
}
