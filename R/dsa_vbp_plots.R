#' DSA+VBP Visualization Functions
#'
#' Functions for creating tornado plots and other visualizations for
#' combined DSA + VBP analysis results.
#'
#' @name dsa_vbp_plots
#' @importFrom dplyr filter mutate arrange select bind_rows group_by summarize pull left_join
#' @importFrom dplyr inner_join distinct ungroup dense_rank desc
#' @importFrom purrr map
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggplot2 ggplot aes geom_tile geom_vline geom_text scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal theme element_text element_blank
#' @importFrom ggplot2 facet_wrap vars
#' @importFrom scales comma dollar pretty_breaks
#' @importFrom tidytext reorder_within scale_y_reordered
#' @importFrom tibble tibble
NULL

#' DSA+VBP Plot
#'
#' Creates a tornado plot showing value-based prices across DSA parameter variations
#' at a specified WTP threshold. This visualizes how each DSA parameter affects
#' the VBP, allowing identification of which parameters have the largest impact
#' on the value-based price.
#'
#' This function follows the same visual style and faceting logic as other DSA
#' plots (dsa_outcomes_plot, dsa_nmb_plot, dsa_ce_plot).
#'
#' @param results DSA+VBP results object from run_dsa() with VBP enabled
#' @param wtp Willingness-to-pay threshold for calculating VBP. If NULL (default),
#'   uses the WTP from the outcome summary specified in the VBP analysis.
#' @param comparators Comparator selection: "all" (default, shows each comparator + aggregate),
#'   "overall" (aggregate only), "all_comparators" (individuals only), or specific comparator name(s)
#' @param groups Group selection: "overall" (default), "all" (overall + all groups),
#'   "all_groups" (all groups without overall), or specific group name(s)
#' @param show_parameter_values Logical. Show parameter values in y-axis labels?
#'   (default: TRUE)
#' @param ... Additional arguments (reserved for future use)
#'
#' @return A ggplot2 tornado plot object
#'
#' @examples
#' \dontrun{
#' # Create tornado plot using default WTP from model
#' dsa_vbp_plot(results)
#'
#' # Create tornado plot at specific WTP
#' dsa_vbp_plot(results, wtp = 50000)
#'
#' # Create tornado plot for specific comparator
#' dsa_vbp_plot(results, wtp = 50000, comparators = "chemo")
#'
#' # Create tornado plot for all groups
#' dsa_vbp_plot(results, groups = "all")
#' }
#'
#' @export
dsa_vbp_plot <- function(results,
                                  wtp = NULL,
                                  comparators = "all",
                                  groups = "overall",
                                  show_parameter_values = TRUE,
                                  ...) {

  # Check that VBP equations exist
  if (is.null(results$dsa_vbp_equations) || nrow(results$dsa_vbp_equations) == 0) {
    stop("No VBP equations found. Ensure run_dsa() was called with VBP parameters.")
  }

  # Get WTP from model metadata if not provided
  if (is.null(wtp)) {
    wtp <- extract_vbp_wtp(results)
  }

  # Prepare tornado data
  tornado_data <- prepare_dsa_vbp_tornado_data(
    results,
    wtp = wtp,
    comparators = comparators,
    groups = groups,
    show_parameter_values = show_parameter_values
  )

  # If no valid data after preparation
  if (nrow(tornado_data) == 0) {
    stop("No valid data for tornado plot after filtering")
  }

  # Create x-axis label
  summary_label <- paste0("Value-Based Price at WTP = ", scales::comma(wtp))

  # Render the tornado plot using the shared helper from dsa_plots.R
  render_tornado_plot(tornado_data, summary_label)
}

#' Prepare DSA+VBP Tornado Data
#'
#' Internal helper to prepare tornado plot data from DSA+VBP results.
#' Calculates VBP at the specified WTP for each DSA run and structures
#' the data for tornado plot visualization.
#'
#' @param results DSA+VBP results object
#' @param wtp Willingness-to-pay threshold
#' @param comparators Comparator selection
#' @param groups Group selection
#' @param show_parameter_values Whether to show parameter values in labels
#'
#' @return Tibble with columns: parameter_display_name, comparator, group,
#'   low, base, high, range
#' @keywords internal
prepare_dsa_vbp_tornado_data <- function(results,
                                          wtp,
                                          comparators,
                                          groups,
                                          show_parameter_values = TRUE) {

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

  # Create strategy label for faceting (named 'strategy' to match render_tornado_plot interface)
  equations <- equations %>%
    mutate(strategy = ifelse(
      .data$comparator == "All Comparators",
      "vs. All Comparators",
      paste0("vs. ", map_names_if_available(.data$comparator, results$metadata$strategies))
    ))

  # Separate base case (variation = "base") from variations
  base_data <- equations %>%
    filter(.data$variation == "base") %>%
    select("strategy", "group", base_vbp = "vbp_price")

  # Separate low and high variations
  low_data <- equations %>%
    filter(.data$variation == "low") %>%
    select("strategy", "group", "parameter", "parameter_display_name",
           "override_value", low = "vbp_price")

  high_data <- equations %>%
    filter(.data$variation == "high") %>%
    select("strategy", "group", "parameter", "parameter_display_name",
           "override_value", high = "vbp_price")

  # Combine into tornado format
  tornado_data <- low_data %>%
    inner_join(
      high_data %>% select("strategy", "group", "parameter", "override_value", "high"),
      by = c("strategy", "group", "parameter"),
      suffix = c("_low", "_high")
    ) %>%
    inner_join(base_data, by = c("strategy", "group")) %>%
    rename(
      low_override = override_value_low,
      high_override = override_value_high,
      base = base_vbp
    ) %>%
    mutate(range = pmax(.data$low, .data$base, .data$high) -
             pmin(.data$low, .data$base, .data$high))

  # Add parameter value labels if requested (render_tornado_plot uses parameter_display_name)
  if (show_parameter_values) {
    tornado_data <- tornado_data %>%
      mutate(
        parameter_display_name = paste0(
          .data$parameter_display_name, " (",
          .data$low_override, " - ", .data$high_override, ")"
        )
      )
  }

  tornado_data
}

#' Map Names If Available
#'
#' Helper to map technical names to display names if metadata is available.
#'
#' @param names Vector of names to map
#' @param strategies_metadata Strategies metadata with name and display_name columns
#' @return Vector of mapped names
#' @keywords internal
map_names_if_available <- function(names, strategies_metadata) {
  if (is.null(strategies_metadata) || nrow(strategies_metadata) == 0) {
    return(names)
  }

  sapply(names, function(n) {
    row <- strategies_metadata %>% filter(.data$name == n)
    if (nrow(row) > 0 && !is.null(row$display_name) && !is.na(row$display_name[1])) {
      row$display_name[1]
    } else {
      n
    }
  }, USE.NAMES = FALSE)
}

#' Extract WTP from VBP Results
#'
#' Internal helper to extract the WTP threshold from the outcome summary
#' metadata used in the VBP analysis.
#'
#' @param results DSA+VBP results object
#' @return Numeric WTP value
#' @keywords internal
extract_vbp_wtp <- function(results) {
  # Get outcome summary name from vbp_spec
  if (is.null(results$vbp_spec) || is.null(results$vbp_spec$outcome_summary)) {
    stop("Cannot determine outcome summary from VBP results. Provide explicit wtp parameter.")
  }

  outcome_summary <- results$vbp_spec$outcome_summary

  # Extract WTP from outcome summary metadata
  if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
    stop("Cannot extract WTP from metadata. Metadata not available. Provide explicit wtp parameter.")
  }

  outcome_meta <- results$metadata$summaries %>%
    filter(.data$name == outcome_summary)

  if (nrow(outcome_meta) == 0) {
    stop(sprintf("Outcome summary '%s' not found in metadata. Provide explicit wtp parameter.", outcome_summary))
  }

  wtp <- outcome_meta$wtp[1]

  if (is.na(wtp)) {
    stop(sprintf("WTP not found for outcome summary '%s'. Provide explicit wtp parameter.", outcome_summary))
  }

  wtp
}

