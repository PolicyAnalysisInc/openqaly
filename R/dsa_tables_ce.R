#' Prepare DSA CE Table Data
#'
#' Internal helper function that prepares DSA cost-effectiveness data for table rendering.
#'
#' @param results A openqaly DSA results object
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param cost_decimals Number of decimal places for costs
#' @param outcome_decimals Number of decimal places for outcomes
#' @param icer_decimals Number of decimal places for ICERs
#' @param font_size Font size for rendering
#' @param top_n Integer or NULL
#' @param show_parameter_values Logical. Include input parameter values in row labels?
#' @param abbreviate Logical. Use abbreviated number format?
#' @return List with prepared data and metadata for render_table()
#' @keywords internal
prepare_dsa_ce_table_data <- function(results,
                                      health_outcome,
                                      cost_outcome,
                                      groups = "overall",
                                      interventions = NULL,
                                      comparators = NULL,
                                      cost_decimals = NULL,
                                      outcome_decimals = NULL,
                                      icer_decimals = NULL,
                                      font_size = 11,
                                      top_n = NULL,
                                      show_parameter_values = FALSE,
                                      abbreviate = FALSE) {
  locale <- get_results_locale(results)

  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for CE table")
  }

  combined_icer <- prepare_dsa_ce_pairwise_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    groups = groups,
    interventions = interventions,
    comparators = comparators
  )

  base_data <- combined_icer %>%
    filter(.data$variation == "base") %>%
    select("strategy", "group", "intervention_name", "comparator_name",
           base_icer = "icer_value")

  low_data <- combined_icer %>%
    filter(.data$variation == "low") %>%
    select("strategy", "group", "parameter", "parameter_display_name",
           low_icer = "icer_value")

  high_data <- combined_icer %>%
    filter(.data$variation == "high") %>%
    select("strategy", "group", "parameter", "parameter_display_name",
           high_icer = "icer_value")

  ce_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group"))

  if (!is.null(top_n)) {
    ce_data <- ce_data %>%
      mutate(.range = abs(as.numeric(.data$high_icer) - as.numeric(.data$low_icer))) %>%
      group_by(.data$strategy, .data$group) %>%
      slice_max(order_by = .data$.range, n = top_n, with_ties = FALSE) %>%
      ungroup() %>%
      select(-".range")
  }

  if (show_parameter_values) {
    ce_data <- enhance_dsa_parameter_labels(ce_data, results, locale = locale)
  }

  ce_data <- ce_data %>%
    rowwise() %>%
    mutate(
      low = oq_format_icer(.data$low_icer, decimals = icer_decimals,
                           locale = locale, abbreviate = abbreviate),
      base = oq_format_icer(abs(.data$base_icer), decimals = icer_decimals,
                            locale = locale, abbreviate = abbreviate),
      high = oq_format_icer(.data$high_icer, decimals = icer_decimals,
                            locale = locale, abbreviate = abbreviate),
      has_flipped_icer = is_flipped_icer(.data$low_icer) || is_flipped_icer(.data$high_icer)
    ) %>%
    ungroup()

  footnote_data <- ce_data %>%
    filter(.data$has_flipped_icer) %>%
    distinct(.data$intervention_name, .data$comparator_name) %>%
    mutate(
      footnote_text = sprintf(
        "* %s is more costly & more effective than %s. ICER represents cost-effectiveness of %s vs. %s.",
        .data$comparator_name,
        .data$intervention_name,
        .data$comparator_name,
        .data$intervention_name
      )
    )

  formatted_data <- ce_data %>%
    select("strategy", "group", "parameter", "parameter_display_name", "low", "base", "high")

  build_dsa_triplet_table_spec(
    data = formatted_data,
    groups = groups,
    metadata = results$metadata,
    font_size = font_size,
    footnotes = unique(footnote_data$footnote_text)
  )
}

#' Format DSA Cost-Effectiveness as Summary Table
#'
#' Creates a table showing DSA cost-effectiveness results with low, base case, and high ICER values.
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param cost_decimals Number of decimal places for costs
#' @param outcome_decimals Number of decimal places for outcomes
#' @param icer_decimals Number of decimal places for ICERs
#' @param font_size Font size for rendering
#' @param table_format Character. Backend to use
#' @param top_n Integer or NULL. Show only the top N parameters by impact range.
#' @param show_parameter_values Logical. Include input parameter values in row labels?
#' @param abbreviate Logical. Use abbreviated number format?
#' @return A table object
#' @export
dsa_ce_table <- function(results,
                         health_outcome,
                         cost_outcome,
                         groups = "overall",
                         interventions = NULL,
                         comparators = NULL,
                         cost_decimals = NULL,
                         outcome_decimals = NULL,
                         icer_decimals = NULL,
                         font_size = 11,
                         table_format = c("flextable", "kable"),
                         top_n = NULL,
                         show_parameter_values = FALSE,
                         abbreviate = FALSE) {
  table_format <- match.arg(table_format)

  prepared <- prepare_dsa_ce_table_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    cost_decimals = cost_decimals,
    outcome_decimals = outcome_decimals,
    icer_decimals = icer_decimals,
    font_size = font_size,
    top_n = top_n,
    show_parameter_values = show_parameter_values,
    abbreviate = abbreviate
  )

  tbl <- render_table(prepared, format = table_format)

  if (length(prepared$footnotes) > 0) {
    if (table_format == "flextable") {
      tbl <- flextable::add_footer_lines(tbl, values = prepared$footnotes)
      tbl <- flextable::align(tbl, align = "left", part = "footer")
      tbl <- flextable::fontsize(tbl, size = font_size - 1, part = "footer")
    } else {
      tbl <- kableExtra::footnote(tbl,
                                  general = prepared$footnotes,
                                  general_title = "",
                                  footnote_as_chunk = FALSE)
    }
  }

  tbl
}
