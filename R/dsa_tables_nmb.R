#' Prepare DSA NMB Table Data
#'
#' Internal helper function that prepares DSA Net Monetary Benefit data for table rendering.
#'
#' @param results A openqaly DSA results object
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param wtp Override willingness-to-pay
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#' @param top_n Integer or NULL
#' @param show_parameter_values Logical. Include input parameter values in row labels?
#' @param abbreviate Logical. Use abbreviated number format?
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_dsa_nmb_table_data <- function(results,
                                       health_outcome,
                                       cost_outcome,
                                       groups = "overall",
                                       wtp = NULL,
                                       interventions = NULL,
                                       comparators = NULL,
                                       decimals = NULL,
                                       font_size = 11,
                                       top_n = NULL,
                                       show_parameter_values = FALSE,
                                       abbreviate = FALSE) {
  locale <- get_results_locale(results)

  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation")
  }

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

  outcome_tornado <- prepare_dsa_triplet_data(
    results = results,
    summary_name = health_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  ) %>%
    mutate(
      low = .data$low * wtp,
      base = .data$base * wtp,
      high = .data$high * wtp
    )

  cost_tornado <- prepare_dsa_triplet_data(
    results = results,
    summary_name = cost_outcome,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE,
    show_parameter_values = FALSE
  ) %>%
    mutate(
      low = -.data$low,
      base = -.data$base,
      high = -.data$high
    )

  nmb_data <- outcome_tornado %>%
    full_join(
      cost_tornado,
      by = c("strategy", "group", "parameter", "parameter_display_name"),
      suffix = c("_outcome", "_cost")
    ) %>%
    mutate(
      low_outcome = replace_na(.data$low_outcome, 0),
      base_outcome = replace_na(.data$base_outcome, 0),
      high_outcome = replace_na(.data$high_outcome, 0),
      low_cost = replace_na(.data$low_cost, 0),
      base_cost = replace_na(.data$base_cost, 0),
      high_cost = replace_na(.data$high_cost, 0),
      low = .data$low_outcome + .data$low_cost,
      base = .data$base_outcome + .data$base_cost,
      high = .data$high_outcome + .data$high_cost
    ) %>%
    select("strategy", "group", "parameter", "parameter_display_name", "low", "base", "high")

  if (!is.null(top_n)) {
    nmb_data <- nmb_data %>%
      mutate(.range = abs(.data$high - .data$low)) %>%
      group_by(.data$strategy, .data$group) %>%
      slice_max(order_by = .data$.range, n = top_n, with_ties = FALSE) %>%
      ungroup() %>%
      select(-".range")
  }

  if (show_parameter_values) {
    nmb_data <- enhance_dsa_parameter_labels(nmb_data, results, locale = locale)
  }

  formatted_data <- nmb_data %>%
    mutate(
      low = oq_format(.data$low, decimals = decimals, locale = locale, abbreviate = abbreviate, currency = TRUE),
      base = oq_format(.data$base, decimals = decimals, locale = locale, abbreviate = abbreviate, currency = TRUE),
      high = oq_format(.data$high, decimals = decimals, locale = locale, abbreviate = abbreviate, currency = TRUE)
    )

  build_dsa_triplet_table_spec(
    data = formatted_data,
    groups = groups,
    metadata = results$metadata,
    font_size = font_size
  )
}

#' Format DSA Net Monetary Benefit as Summary Table
#'
#' Creates a table showing DSA Net Monetary Benefit with low, base case, and high values.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param wtp Optional override for willingness-to-pay
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param decimals Number of decimal places
#' @param font_size Font size for rendering
#' @param table_format Character. Backend to use
#' @param top_n Integer or NULL. Show only the top N parameters by impact range.
#' @param show_parameter_values Logical. Include input parameter values in row labels?
#' @param abbreviate Logical. Use abbreviated number format?
#' @return A table object
#' @export
dsa_nmb_table <- function(results,
                          health_outcome,
                          cost_outcome,
                          groups = "overall",
                          wtp = NULL,
                          interventions = NULL,
                          comparators = NULL,
                          decimals = NULL,
                          font_size = 11,
                          table_format = c("flextable", "kable"),
                          top_n = NULL,
                          show_parameter_values = FALSE,
                          abbreviate = FALSE) {
  table_format <- match.arg(table_format)

  prepared <- prepare_dsa_nmb_table_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    groups = groups,
    wtp = wtp,
    interventions = interventions,
    comparators = comparators,
    decimals = decimals,
    font_size = font_size,
    top_n = top_n,
    show_parameter_values = show_parameter_values,
    abbreviate = abbreviate
  )

  render_table(prepared, format = table_format)
}
