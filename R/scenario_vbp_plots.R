#' Scenario VBP Bar Chart Visualizations
#'
#' Functions for creating VBP bar chart visualizations of scenario analysis results.
#'
#' @name scenario_vbp_plots
#' @importFrom ggplot2 ggplot aes geom_col geom_vline geom_text scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous facet_wrap vars labs theme_bw theme element_text
#' @importFrom ggplot2 expansion
#' @importFrom scales comma pretty_breaks
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows distinct
#' @importFrom forcats fct_reorder
#' @importFrom glue glue
NULL

#' Prepare Scenario VBP Bar Data
#'
#' Internal helper to prepare VBP data for bar charts from scenario+VBP results.
#'
#' @param results Scenario+VBP results object
#' @param wtp Willingness-to-pay threshold
#' @param groups Group selection
#' @param comparators Comparator strategies
#'
#' @return Tibble with scenario_name, value (VBP price), comparator, group, is_base_case
#' @keywords internal
prepare_scenario_vbp_bar_data <- function(results, wtp, groups, comparators) {

  # Check that VBP equations exist
  if (is.null(results$scenario_vbp_equations) || nrow(results$scenario_vbp_equations) == 0) {
    stop("No VBP equations found. Ensure run_scenario() was called with VBP parameters.",
         call. = FALSE)
  }

  # Filter to specified groups
  vbp_data <- results$scenario_vbp_equations

  if (!is.null(groups)) {
    if (groups == "overall") {
      vbp_data <- vbp_data %>% filter(.data$group == "overall")
    } else if (groups == "all") {
      # Keep all including overall
    } else if (groups == "all_groups") {
      vbp_data <- vbp_data %>% filter(.data$group != "overall")
    } else {
      vbp_data <- vbp_data %>% filter(.data$group %in% groups)
    }
  }

  # Filter to specified comparators
  if (!is.null(comparators) && comparators != "all") {
    vbp_data <- vbp_data %>% filter(.data$comparator %in% comparators)
  }

  # Calculate VBP price at given WTP
  bar_data <- vbp_data %>%
    mutate(
      value = .data$vbp_slope * wtp + .data$vbp_intercept,
      is_base_case = .data$scenario_name == "Base Case"
    )

  # Map comparator names to display names
  if (!is.null(results$metadata$strategies)) {
    bar_data$comparator <- map_names(bar_data$comparator, results$metadata$strategies, "display_name")
  }

  # Map group names to display names
  if (!is.null(results$metadata$groups)) {
    bar_data$group <- map_names(bar_data$group, results$metadata$groups, "display_name")
  }

  bar_data %>%
    select("scenario_id", "scenario_name", "scenario_description",
           "comparator", "group", "value", "is_base_case")
}


#' Plot Scenario VBP as Bar Chart
#'
#' Creates a horizontal bar chart showing the value-based price for each scenario
#' at a given willingness-to-pay threshold.
#'
#' @param results A openqaly scenario+VBP results object (output from run_scenario with VBP enabled)
#' @param wtp Willingness-to-pay threshold. If NULL, uses default WTP from model metadata.
#' @param groups Group selection: "overall" (default), "all", "all_groups", or specific group names
#' @param comparators Comparator strategies: "all" (default) or specific comparator names
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' results <- run_scenario(model,
#'   vbp_price_variable = "cost_tx",
#'   vbp_intervention = "treatment"
#' )
#'
#' # VBP bar chart at WTP = 50,000
#' scenario_vbp_plot(results, wtp = 50000)
#'
#' # VBP for specific comparator
#' scenario_vbp_plot(results, wtp = 50000, comparators = "control")
#' }
#'
#' @export
scenario_vbp_plot <- function(results,
                               wtp = NULL,
                               groups = "overall",
                               comparators = "all") {

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

  # Prepare bar data
  bar_data <- prepare_scenario_vbp_bar_data(results, wtp, groups, comparators)

  if (nrow(bar_data) == 0) {
    stop("No VBP data available for the specified parameters", call. = FALSE)
  }

  # Determine faceting
  n_groups <- length(unique(bar_data$group))
  n_comparators <- length(unique(bar_data$comparator))

  n_facets <- if (n_groups > 1 && n_comparators > 1) {
    n_groups * n_comparators
  } else if (n_groups > 1) {
    n_groups
  } else if (n_comparators > 1) {
    n_comparators
  } else {
    1
  }
  ncol <- min(2, ceiling(n_facets / 3))

  facet_component <- NULL
  if ((n_groups > 1) && (n_comparators > 1)) {
    facet_component <- facet_wrap(vars(.data$comparator, .data$group), scales = "free_x", ncol = n_groups)
  } else if ((n_groups > 1) && (n_comparators == 1)) {
    facet_component <- facet_wrap(vars(.data$group), scales = "free_x", ncol = ncol)
  } else if ((n_comparators > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$comparator), scales = "free_x", ncol = ncol)
  }

  # Order scenarios: Base Case first, then by value
  bar_data <- bar_data %>%
    mutate(
      sort_order = if_else(.data$is_base_case, -Inf, .data$value),
      scenario_name = fct_reorder(.data$scenario_name, .data$sort_order, .desc = TRUE)
    )

  # Calculate axis limits
  x_range <- range(c(0, bar_data$value), na.rm = TRUE)
  x_breaks <- pretty_breaks(n = 5)(x_range)
  x_offset <- diff(range(x_breaks)) * 0.02

  # Determine expansion based on where values exist
  has_negative <- any(bar_data$value < 0, na.rm = TRUE)
  has_positive <- any(bar_data$value > 0, na.rm = TRUE)

  # Higher expansion (0.15) on sides with values for label room, lower (0.05) otherwise
  left_expand <- if (has_negative) 0.15 else 0.05
  right_expand <- if (has_positive) 0.15 else 0.05

  # Create plot with uniform color
  p <- ggplot(bar_data, aes(
    y = .data$scenario_name,
    x = .data$value
  )) +
    geom_col(width = 0.7, fill = "#4A90D9", color = "black", linewidth = 0.2) +
    geom_vline(xintercept = 0, linewidth = 0.5, color = "gray30") +
    geom_text(
      aes(
        x = .data$value + x_offset * sign(if_else(.data$value == 0, 1, .data$value)),
        label = scales::comma(.data$value, accuracy = 1)
      ),
      hjust = if_else(bar_data$value >= 0, 0, 1),
      size = 2.8,
      color = "black"
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      labels = comma,
      expand = expansion(mult = c(left_expand, right_expand))
    ) +
    labs(
      y = "Scenario",
      x = glue("Value-Based Price (\u03bb = {scales::comma(wtp)})")
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 9)
    )

  # Add faceting
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}
