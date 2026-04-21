#' Abbreviate Time Unit for Display
#'
#' Internal helper to convert time unit names to abbreviated forms for display.
#'
#' @param unit Character string of time unit (e.g., "years", "months")
#' @return Abbreviated unit string (e.g., "yrs", "mos")
#' @keywords internal
abbreviate_time_unit <- function(unit) {
  if (is.null(unit) || is.na(unit) || unit == "") {
    return("")
  }
  unit_lower <- tolower(unit)
  switch(unit_lower,
    "years" = "yrs",
    "year" = "yrs",
    "months" = "mos",
    "month" = "mos",
    "weeks" = "wks",
    "week" = "wks",
    "days" = "days",
    "day" = "days",
    "cycles" = "cycles",
    "cycle" = "cycles",
    unit
  )
}

#' Get Unit Suffix for DSA Setting Parameter
#'
#' Internal helper to determine the appropriate unit suffix for a DSA setting parameter.
#'
#' @param param_name Name of the setting parameter
#' @param settings Model settings list containing timeframe_unit, cycle_length_unit, etc.
#' @return Unit suffix string or empty string if no unit
#' @keywords internal
get_setting_unit_suffix <- function(param_name, settings) {
  if (is.null(settings)) {
    return("")
  }

  switch(param_name,
    "discount_cost" = "%",
    "discount_outcomes" = "%",
    "discount_rate" = "%",
    "timeframe" = abbreviate_time_unit(settings$timeframe_unit),
    "cycle_length" = abbreviate_time_unit(settings$cycle_length_unit),
    ""
  )
}

#' Extract Parameter Values from DSA Results
#'
#' Internal helper to extract the actual low and high parameter values
#' from DSA segments for display in labels.
#'
#' @param results DSA results object
#' @param data DSA data tibble with parameter, strategy, group columns
#' @param interventions Intervention strategies (if showing differences)
#' @param comparators Comparator strategies (if showing differences)
#' @return Tibble with parameter, strategy, group, param_low_value, param_high_value
#' @keywords internal
extract_dsa_parameter_values <- function(results, data, interventions = NULL, comparators = NULL) {
  param_combos <- data %>%
    distinct(.data$parameter, .data$strategy, .data$group)

  param_values_list <- list()

  for (i in seq_len(nrow(param_combos))) {
    param_name <- param_combos$parameter[i]
    strat <- param_combos$strategy[i]
    grp <- param_combos$group[i]

    param_info <- results$dsa_metadata %>%
      filter(.data$parameter == param_name)

    if (nrow(param_info) == 0) {
      next
    }

    low_run_id <- param_info %>% filter(.data$variation == "low") %>% pull(.data$run_id)
    high_run_id <- param_info %>% filter(.data$variation == "high") %>% pull(.data$run_id)

    if (length(low_run_id) == 0 || length(high_run_id) == 0) {
      next
    }

    low_run_id <- low_run_id[1]
    high_run_id <- high_run_id[1]

    extract_strategy <- strat
    if (!is.null(interventions) || !is.null(comparators)) {
      if (!is.null(interventions)) {
        extract_strategy <- interventions
      } else {
        extract_strategy <- results$metadata$strategies$name[1]
      }
    }

    search_group <- grp
    if (search_group %in% c("_aggregated", "Aggregated", "Overall")) {
      low_segment <- results$segments %>%
        filter(.data$run_id == low_run_id, .data$strategy == extract_strategy) %>%
        slice(1)
      high_segment <- results$segments %>%
        filter(.data$run_id == high_run_id, .data$strategy == extract_strategy) %>%
        slice(1)
    } else {
      low_segment <- results$segments %>%
        filter(.data$run_id == low_run_id, .data$strategy == extract_strategy, .data$group == search_group) %>%
        slice(1)
      high_segment <- results$segments %>%
        filter(.data$run_id == high_run_id, .data$strategy == extract_strategy, .data$group == search_group) %>%
        slice(1)

      if (nrow(low_segment) == 0) {
        low_segment <- results$segments %>%
          filter(.data$run_id == low_run_id, .data$strategy == extract_strategy) %>%
          slice(1)
      }
      if (nrow(high_segment) == 0) {
        high_segment <- results$segments %>%
          filter(.data$run_id == high_run_id, .data$strategy == extract_strategy) %>%
          slice(1)
      }
    }

    param_type <- param_info$parameter_type[1]
    low_value <- NA_real_
    high_value <- NA_real_

    if (nrow(low_segment) > 0) {
      if (param_type == "variable") {
        param_overrides <- low_segment$parameter_overrides[[1]]
        if (!is.null(param_overrides) && param_name %in% names(param_overrides)) {
          low_value <- param_overrides[[param_name]]
        }
      } else if (param_type == "setting") {
        setting_overrides <- low_segment$setting_overrides[[1]]
        if (!is.null(setting_overrides) && param_name %in% names(setting_overrides)) {
          low_value <- setting_overrides[[param_name]]
        }
      }
    }

    if (nrow(high_segment) > 0) {
      if (param_type == "variable") {
        param_overrides <- high_segment$parameter_overrides[[1]]
        if (!is.null(param_overrides) && param_name %in% names(param_overrides)) {
          high_value <- param_overrides[[param_name]]
        }
      } else if (param_type == "setting") {
        setting_overrides <- high_segment$setting_overrides[[1]]
        if (!is.null(setting_overrides) && param_name %in% names(setting_overrides)) {
          high_value <- setting_overrides[[param_name]]
        }
      }
    }

    param_values_list[[i]] <- tibble(
      parameter = param_name,
      strategy = strat,
      group = grp,
      param_low_value = low_value,
      param_high_value = high_value,
      param_type = param_type
    )
  }

  if (length(param_values_list) > 0) {
    bind_rows(param_values_list)
  } else {
    tibble(
      parameter = character(),
      strategy = character(),
      group = character(),
      param_low_value = numeric(),
      param_high_value = numeric(),
      param_type = character()
    )
  }
}

#' Enhance DSA Parameter Labels with Input Values
#'
#' Adds input range values to parameter_display_name labels for DSA output.
#'
#' @param data Data frame with parameter and parameter_display_name columns
#' @param results DSA results object containing dsa_metadata
#' @param locale Optional locale for formatting
#' @param parameter_values Optional tibble from extract_dsa_parameter_values()
#' @return Data frame with enhanced parameter_display_name
#' @keywords internal
enhance_dsa_parameter_labels <- function(data, results, locale = NULL, parameter_values = NULL) {
  if (is.null(results$dsa_metadata)) {
    return(data)
  }

  if (!is.null(results$dsa_metadata) && "range_label" %in% names(results$dsa_metadata)) {
    range_labels <- results$dsa_metadata %>%
      filter(!is.na(.data$range_label)) %>%
      distinct(.data$parameter, .data$range_label)
    if (nrow(range_labels) > 0) {
      data <- data %>%
        left_join(range_labels, by = "parameter")
    } else {
      data$range_label <- NA_character_
    }
  } else {
    data$range_label <- NA_character_
  }

  if (is.null(parameter_values)) {
    meta <- results$dsa_metadata %>%
      filter(.data$parameter_type != "base")

    low_vals <- meta %>%
      filter(.data$variation == "low") %>%
      select("parameter", "parameter_display_name", "parameter_type", "override_value",
             "range_label") %>%
      rename(param_low_value = "override_value", param_type = "parameter_type")

    high_vals <- meta %>%
      filter(.data$variation == "high") %>%
      select("parameter_display_name", param_high_value = "override_value")

    parameter_values <- low_vals %>%
      inner_join(high_vals, by = "parameter_display_name")

    if (nrow(parameter_values) > 0) {
      data <- data %>%
        left_join(
          parameter_values %>% select(
            "parameter", "parameter_display_name", "param_type",
            "param_low_value", "param_high_value"
          ),
          by = c("parameter", "parameter_display_name")
        )
    }
  } else if (nrow(parameter_values) > 0) {
    join_cols <- intersect(c("parameter", "strategy", "group"), names(parameter_values))
    data <- data %>%
      left_join(parameter_values, by = join_cols)
  }

  settings <- results$metadata$settings

  data <- data %>%
    rowwise() %>%
    mutate(
      parameter_display_name = if_else(
        !is.na(.data$range_label),
        sprintf("%s (%s)", .data$parameter_display_name, .data$range_label),
        if_else(
          !is.na(.data$param_low_value) & !is.na(.data$param_high_value),
          {
            unit_suffix <- if (!is.na(.data$param_type) && .data$param_type == "setting") {
              get_setting_unit_suffix(.data$parameter, settings)
            } else {
              ""
            }

            d <- auto_precision(
              c(as.numeric(.data$param_low_value), as.numeric(.data$param_high_value)),
              exact = TRUE, require_unique = TRUE, base_precision = 3
            )

            if (unit_suffix != "") {
              sprintf("%s (%s%s - %s%s)",
                      .data$parameter_display_name,
                      oq_format(as.numeric(.data$param_low_value), decimals = d, locale = locale),
                      unit_suffix,
                      oq_format(as.numeric(.data$param_high_value), decimals = d, locale = locale),
                      unit_suffix)
            } else {
              sprintf("%s (%s - %s)",
                      .data$parameter_display_name,
                      oq_format(as.numeric(.data$param_low_value), decimals = d, locale = locale),
                      oq_format(as.numeric(.data$param_high_value), decimals = d, locale = locale))
            }
          },
          .data$parameter_display_name
        )
      )
    ) %>%
    ungroup() %>%
    select(-any_of(c("param_low_value", "param_high_value", "param_type", "range_label")))

  data
}

#' Compatibility alias for older internal name
#' @keywords internal
extract_parameter_values <- function(results, tornado_data, interventions = NULL, comparators = NULL) {
  extract_dsa_parameter_values(
    results = results,
    data = tornado_data,
    interventions = interventions,
    comparators = comparators
  )
}

#' Compatibility alias for older internal name
#' @keywords internal
enhance_table_parameter_labels <- function(data, results, locale = NULL) {
  enhance_dsa_parameter_labels(data = data, results = results, locale = locale)
}
