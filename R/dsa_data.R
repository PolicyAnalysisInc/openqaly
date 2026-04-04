#' Build DSA Comparison Pairs
#'
#' Internal helper to build pairwise comparisons for DSA outputs.
#'
#' @param all_strategies Character vector of available technical strategy names
#' @param interventions Optional intervention strategy names
#' @param comparators Optional comparator strategy names
#' @return List of lists with intervention and comparator entries
#' @keywords internal
build_dsa_comparison_pairs <- function(all_strategies, interventions = NULL, comparators = NULL) {
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    for (int_strat in interventions) {
      for (comp_strat in comparators) {
        if (int_strat != comp_strat) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = int_strat,
            comparator = comp_strat
          )
        }
      }
    }
  } else if (!is.null(interventions)) {
    for (int_strat in interventions) {
      other_strategies <- setdiff(all_strategies, int_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = other
        )
      }
    }
  } else if (!is.null(comparators)) {
    for (comp_strat in comparators) {
      other_strategies <- setdiff(all_strategies, comp_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = other,
          comparator = comp_strat
        )
      }
    }
  }

  comparison_pairs
}

#' Prepare DSA Triplet Data
#'
#' Internal helper to reshape DSA summary data into low/base/high format.
#'
#' @param results DSA results object from run_dsa()
#' @param summary_name Name of the summary to plot or table
#' @param groups Group selection
#' @param strategies Optional strategy filter
#' @param interventions Optional intervention strategies
#' @param comparators Optional comparator strategies
#' @param discounted Use discounted values?
#' @param show_parameter_values Logical. Include parameter values in labels?
#' @param locale Optional locale override
#' @param value_type Type of summary values
#' @return Tibble with low/base/high/range and ordering attributes
#' @keywords internal
prepare_dsa_triplet_data <- function(results,
                                     summary_name,
                                     groups,
                                     strategies = NULL,
                                     interventions = NULL,
                                     comparators = NULL,
                                     discounted = TRUE,
                                     show_parameter_values = TRUE,
                                     locale = NULL,
                                     value_type = "all") {
  dsa_data <- extract_dsa_summaries(
    results,
    summary_name = summary_name,
    value_type = value_type,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  base_data <- dsa_data %>%
    filter(.data$run_id == 1) %>%
    select("strategy", "group", base = "amount")

  low_data <- dsa_data %>%
    filter(.data$variation == "low") %>%
    select("strategy", "group", "parameter", "parameter_display_name", low = "amount")

  high_data <- dsa_data %>%
    filter(.data$variation == "high") %>%
    select("strategy", "group", "parameter", "parameter_display_name", high = "amount")

  triplet_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group")) %>%
    mutate(range = pmax(.data$low, .data$base, .data$high) - pmin(.data$low, .data$base, .data$high))

  differences_created <- FALSE
  strategy_order <- NULL

  if (!is.null(interventions) || !is.null(comparators)) {
    triplet_wide <- triplet_data %>%
      pivot_wider(
        names_from = "strategy",
        values_from = c("low", "base", "high", "range"),
        id_cols = c("group", "parameter", "parameter_display_name")
      )

    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      all_strategies <- results$metadata$strategies$name
      strategies_in_data <- unique(dsa_data$strategy)
      all_strategies <- all_strategies[all_strategies %in% strategies_in_data]
    } else {
      all_strategies <- unique(dsa_data$strategy)
    }

    comparison_pairs <- build_dsa_comparison_pairs(all_strategies, interventions, comparators)
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }

    diff_data <- vector("list", length(comparison_pairs))
    for (i in seq_along(comparison_pairs)) {
      pair <- comparison_pairs[[i]]
      int_strat <- pair$intervention
      comp_strat <- pair$comparator

      int_mapped <- map_names(int_strat, results$metadata$strategies, "display_name")
      comp_mapped <- map_names(comp_strat, results$metadata$strategies, "display_name")
      comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

      diff_data[[i]] <- triplet_wide %>%
        mutate(
          strategy = comp_label,
          low = !!sym(paste0("low_", int_strat)) - !!sym(paste0("low_", comp_strat)),
          base = !!sym(paste0("base_", int_strat)) - !!sym(paste0("base_", comp_strat)),
          high = !!sym(paste0("high_", int_strat)) - !!sym(paste0("high_", comp_strat)),
          range = pmax(.data$low, .data$base, .data$high) - pmin(.data$low, .data$base, .data$high)
        ) %>%
        select("group", "parameter", "parameter_display_name", "strategy", "low", "base", "high", "range")
    }

    triplet_data <- bind_rows(diff_data)
    differences_created <- TRUE
    strategy_order <- unique(triplet_data$strategy)
  }

  if (!differences_created && !is.null(results$metadata)) {
    if (is.null(strategy_order)) {
      strategy_order <- unique(triplet_data$strategy)
    }
    if (!is.null(results$metadata$strategies)) {
      triplet_data$strategy <- map_names(triplet_data$strategy, results$metadata$strategies, "display_name")
      strategy_order <- map_names(strategy_order, results$metadata$strategies, "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      triplet_data$group <- map_names(triplet_data$group, results$metadata$groups, "display_name")
    }
  } else if (differences_created && !is.null(results$metadata) && !is.null(results$metadata$groups)) {
    triplet_data$group <- map_names(triplet_data$group, results$metadata$groups, "display_name")
  }

  if (show_parameter_values) {
    if (is.null(locale)) {
      locale <- get_results_locale(results)
    }
    param_values <- extract_dsa_parameter_values(
      results = results,
      data = triplet_data,
      interventions = interventions,
      comparators = comparators
    )
    triplet_data <- enhance_dsa_parameter_labels(
      data = triplet_data,
      results = results,
      locale = locale,
      parameter_values = param_values
    )
  }

  if (!is.null(strategy_order)) {
    attr(triplet_data, "strategy_order") <- strategy_order
  }

  group_order <- get_group_order(unique(triplet_data$group), results$metadata)
  attr(triplet_data, "group_order") <- group_order

  triplet_data
}

#' Compatibility alias for older internal name
#' @keywords internal
prepare_dsa_tornado_data <- function(results,
                                     summary_name,
                                     groups,
                                     strategies = NULL,
                                     interventions = NULL,
                                     comparators = NULL,
                                     discounted = TRUE,
                                     show_parameter_values = TRUE,
                                     locale = NULL,
                                     value_type = "all") {
  prepare_dsa_triplet_data(
    results = results,
    summary_name = summary_name,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted,
    show_parameter_values = show_parameter_values,
    locale = locale,
    value_type = value_type
  )
}

#' Prepare DSA CE Pairwise Data
#'
#' Internal helper to prepare cost/outcome pairwise data for CE outputs.
#'
#' @param results DSA results object
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param interventions Optional intervention strategies
#' @param comparators Optional comparator strategies
#' @return Tibble with pairwise ICER inputs and labels
#' @keywords internal
prepare_dsa_ce_pairwise_data <- function(results,
                                         health_outcome,
                                         cost_outcome,
                                         groups = "overall",
                                         interventions = NULL,
                                         comparators = NULL) {
  cost_data <- extract_dsa_summaries(
    results,
    summary_name = cost_outcome,
    value_type = "all",
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  outcome_data <- extract_dsa_summaries(
    results,
    summary_name = health_outcome,
    value_type = "all",
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  combined_data <- cost_data %>%
    inner_join(
      outcome_data %>% select("run_id", "strategy", "group", outcome = "amount"),
      by = c("run_id", "strategy", "group"),
      suffix = c("", "_outcome")
    ) %>%
    rename(cost = "amount")

  all_strategies <- unique(combined_data$strategy)
  comparison_pairs <- build_dsa_comparison_pairs(all_strategies, interventions, comparators)

  if (length(comparison_pairs) == 0) {
    stop("No valid comparisons after excluding self-comparisons")
  }

  all_icer_data <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    int_mapped <- map_names(int_strat, results$metadata$strategies, "display_name")
    comp_mapped <- map_names(comp_strat, results$metadata$strategies, "display_name")
    comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

    int_data <- combined_data %>%
      filter(.data$strategy == int_strat) %>%
      select("run_id", "group", "parameter", "parameter_display_name", "variation",
             cost_int = "cost", outcome_int = "outcome")

    comp_data <- combined_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("run_id", "group", cost_comp = "cost", outcome_comp = "outcome")

    icer_data <- int_data %>%
      inner_join(comp_data, by = c("run_id", "group")) %>%
      mutate(
        strategy = comp_label,
        intervention_name = int_mapped,
        comparator_name = comp_mapped,
        dcost = .data$cost_int - .data$cost_comp,
        doutcome = .data$outcome_int - .data$outcome_comp,
        icer_value = icer(.data$dcost, .data$doutcome)
      ) %>%
      select("run_id", "group", "parameter", "parameter_display_name", "variation",
             "strategy", "intervention_name", "comparator_name", "icer_value")

    all_icer_data[[length(all_icer_data) + 1]] <- icer_data
  }

  combined_icer <- bind_rows(all_icer_data)

  if (!is.null(results$metadata) && !is.null(results$metadata$groups)) {
    combined_icer$group <- map_names(combined_icer$group, results$metadata$groups, "display_name")
  }

  combined_icer
}
