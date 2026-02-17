#' Threshold Analysis Visualization - Plots
#'
#' Plot functions for threshold analysis results.
#'
#' @name threshold_plots
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline geom_vline
#'   geom_label facet_wrap labs scale_x_continuous scale_y_continuous theme_bw
#' @importFrom scales comma pretty_breaks
#' @importFrom glue glue
#' @importFrom dplyr left_join arrange filter
#' @importFrom tibble tibble
NULL

# ============================================================================
# Shared Helpers
# ============================================================================

#' Prepare Threshold History Data
#'
#' Extracts and filters root_finder_history, threshold_values, and analyses
#' specs by analysis name.
#'
#' @param results Threshold results from run_threshold()
#' @param analyses Optional character vector of analysis names to include
#' @return List with history tibble, threshold_values tibble, and analyses list
#' @keywords internal
prepare_threshold_history_data <- function(results, analyses = NULL) {
  history <- results$root_finder_history
  tv <- results$threshold_values
  specs <- results$analyses

  if (!is.null(analyses)) {
    history <- history[history$name %in% analyses, ]
    tv <- tv[tv$name %in% analyses, ]
    specs <- Filter(function(a) a$name %in% analyses, specs)
  }

  list(history = history, threshold_values = tv, analyses = specs)
}

#' Get Threshold Output Label
#'
#' Returns a human-readable label for the output metric based on
#' condition type. Used for y-axis labels and table column headers.
#'
#' @param analysis A single threshold analysis spec
#' @param metadata Model metadata (with summaries, strategies, states, settings)
#' @return Character string label
#' @keywords internal
get_threshold_output_label <- function(analysis, metadata) {
  cond <- analysis$condition
  output_type <- cond$output

  if (output_type %in% c("outcomes", "costs")) {
    # Get display name for the summary or value
    summary_name <- cond$summary %||% cond$value %||% ""
    display_name <- summary_name
    if (!is.null(metadata$summaries) && nrow(metadata$summaries) > 0) {
      display_name <- map_names(summary_name, metadata$summaries, "display_name")
    }

    if (!is.null(cond$type) && cond$type == "difference") {
      ref <- cond$referent
      comp <- cond$comparator
      if (!is.null(metadata$strategies) && nrow(metadata$strategies) > 0) {
        ref <- map_names(ref, metadata$strategies, "display_name")
        comp <- map_names(comp, metadata$strategies, "display_name")
      }
      glue("\u0394 {display_name} ({ref} vs {comp})")
    } else {
      display_name
    }

  } else if (output_type %in% c("nmb", "ce")) {
    # NMB label format matching bc_nmb_plots.R pattern
    health_name <- cond$health_summary
    cost_name <- cond$cost_summary
    outcome_label <- health_name
    cost_label <- cost_name

    if (!is.null(metadata$summaries) && nrow(metadata$summaries) > 0) {
      outcome_label <- map_names(health_name, metadata$summaries, "display_name")
      cost_label <- map_names(cost_name, metadata$summaries, "display_name")
    }

    # Get WTP
    wtp <- NA
    if (!is.null(metadata$summaries) && nrow(metadata$summaries) > 0 &&
        "wtp" %in% names(metadata$summaries)) {
      summ_row <- metadata$summaries[metadata$summaries$name == health_name, ]
      if (nrow(summ_row) > 0 && !is.na(summ_row$wtp[1])) {
        wtp <- summ_row$wtp[1]
      }
    }

    wtp_formatted <- if (!is.na(wtp)) scales::comma(wtp) else "?"
    glue("Net Monetary Benefit ({cost_label}, {outcome_label}, \u03bb = {wtp_formatted})")

  } else if (output_type == "trace") {
    state_name <- cond$state
    if (!is.null(metadata$states) && nrow(metadata$states) > 0) {
      state_name <- map_names(state_name, metadata$states, "display_name")
    }

    time_val <- cond$time
    time_unit <- cond$time_unit %||% "cycle"
    time_str <- paste(time_val, time_unit)
    if (time_val != 1) {
      time_str <- paste(time_val, paste0(time_unit, "s"))
    }

    if (!is.null(cond$type) && cond$type == "difference") {
      ref <- cond$referent
      comp <- cond$comparator
      if (!is.null(metadata$strategies) && nrow(metadata$strategies) > 0) {
        ref <- map_names(ref, metadata$strategies, "display_name")
        comp <- map_names(comp, metadata$strategies, "display_name")
      }
      glue("\u0394 P({state_name}) at {time_str} ({ref} vs {comp})")
    } else {
      glue("P({state_name}) at {time_str}")
    }

  } else {
    "Output"
  }
}

#' Get Threshold Input Label
#'
#' Returns a display name for the threshold variable.
#'
#' @param analysis A single threshold analysis spec
#' @param metadata Model metadata
#' @return Character string label
#' @keywords internal
get_threshold_input_label <- function(analysis, metadata) {
  var_name <- analysis$variable
  if (!is.null(metadata$variables) && nrow(metadata$variables) > 0 &&
      "display_name" %in% names(metadata$variables)) {
    mapped <- map_names(var_name, metadata$variables, "display_name")
    if (!is.na(mapped) && mapped != "") return(mapped)
  }
  var_name
}

# ============================================================================
# Plot Functions
# ============================================================================

#' Threshold Plot
#'
#' Line chart showing input parameter vs output metric, with the threshold
#' point highlighted. Shows how the output changes as the input parameter
#' varies, with the goal reference line and threshold value marked.
#'
#' @param results Threshold results from run_threshold()
#' @param analyses Optional character vector of analysis names to include
#' @param decimals Number of decimal places for threshold value label (default: 4)
#' @return A ggplot object
#' @export
threshold_plot <- function(results, analyses = NULL, decimals = 4) {
  data <- prepare_threshold_history_data(results, analyses)
  history <- data$history
  tv <- data$threshold_values
  specs <- data$analyses

  if (nrow(history) == 0) {
    stop("No history data available for the selected analyses", call. = FALSE)
  }

  # Sort by input within each analysis
  history <- history %>% arrange(.data$name, .data$input)

  # Join threshold values for vertical line / diamond point
  tv_for_join <- tv
  names(tv_for_join)[names(tv_for_join) == "value"] <- "threshold_value"

  # Build y-axis label (use first analysis if multiple)
  if (length(specs) == 1) {
    ylab_text <- as.character(get_threshold_output_label(specs[[1]], results$metadata))
    xlab_text <- get_threshold_input_label(specs[[1]], results$metadata)
  } else {
    ylab_text <- "Output"
    xlab_text <- "Input"
  }

  # Get goal values per analysis for geom_hline
  goal_df <- history %>%
    dplyr::group_by(.data$name) %>%
    dplyr::summarise(goal = .data$goal[1], .groups = "drop")

  # Calculate threshold output values (output at the threshold input)
  threshold_points <- tv_for_join[!is.na(tv_for_join$threshold_value), ]
  if (nrow(threshold_points) > 0) {
    threshold_points$threshold_output <- vapply(seq_len(nrow(threshold_points)), function(i) {
      nm <- threshold_points$name[i]
      th_val <- threshold_points$threshold_value[i]
      h <- history[history$name == nm, ]
      g <- goal_df$goal[goal_df$name == nm]
      if (length(g) > 0) g[1] else NA_real_
    }, numeric(1))
  }

  # Build plot
  p <- ggplot2::ggplot(history, ggplot2::aes(x = .data$input, y = .data$output)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "steelblue", size = 1.5)

  # Goal reference line (per facet)
  p <- p + ggplot2::geom_hline(
    data = goal_df,
    ggplot2::aes(yintercept = .data$goal),
    linetype = "dashed", color = "gray50"
  )

  # Threshold vertical line and point (converged analyses only)
  if (nrow(threshold_points) > 0) {
    p <- p + ggplot2::geom_vline(
      data = threshold_points,
      ggplot2::aes(xintercept = .data$threshold_value),
      linetype = "dotted", color = "red"
    )

    p <- p + ggplot2::geom_point(
      data = threshold_points,
      ggplot2::aes(x = .data$threshold_value, y = .data$threshold_output),
      shape = 18, size = 4, color = "red"
    )

    # Label with threshold value
    threshold_points$label <- vapply(threshold_points$threshold_value, function(v) {
      format_param_value(v, digits = decimals)
    }, character(1))

    p <- p + ggplot2::geom_label(
      data = threshold_points,
      ggplot2::aes(x = .data$threshold_value, y = .data$threshold_output, label = .data$label),
      vjust = -0.5, hjust = 0.5, size = 3, color = "red",
      fill = "white", linewidth = 0.25
    )
  }

  # "Did not converge" annotation for non-converged analyses
  failed_names <- tv_for_join$name[is.na(tv_for_join$threshold_value)]
  if (length(failed_names) > 0) {
    failed_labels <- do.call(rbind, lapply(failed_names, function(nm) {
      h <- history[history$name == nm, ]
      g <- goal_df$goal[goal_df$name == nm]
      # Include goal line in y-range so label centers in visible area
      y_range <- range(c(h$output, g))
      data.frame(
        name = nm,
        x = mean(range(h$input)),
        y = mean(y_range),
        stringsAsFactors = FALSE
      )
    }))

    p <- p + ggplot2::geom_label(
      data = failed_labels,
      ggplot2::aes(x = .data$x, y = .data$y, label = "Did not converge"),
      hjust = 0.5, vjust = 0.5, size = 3.5, color = "gray40",
      fill = "white", linewidth = 0.25
    )
  }

  # Faceting for multiple analyses
  if (length(unique(history$name)) > 1) {
    p <- p + ggplot2::facet_wrap(~ name, scales = "free")
  }

  # Scales and theme
  p <- p +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::comma) +
    ggplot2::labs(x = xlab_text, y = ylab_text) +
    ggplot2::theme_bw()

  p
}


#' Threshold Convergence Plot
#'
#' Diagnostic plot showing solver convergence — difference from goal
#' per iteration. Useful for understanding how the root-finding algorithm
#' converged to the threshold value.
#'
#' @param results Threshold results from run_threshold()
#' @param analyses Optional character vector of analysis names to include
#' @return A ggplot object
#' @export
threshold_convergence_plot <- function(results, analyses = NULL) {
  data <- prepare_threshold_history_data(results, analyses)
  history <- data$history
  tv <- data$threshold_values

  if (nrow(history) == 0) {
    stop("No history data available for the selected analyses", call. = FALSE)
  }

  # Sort by iteration within each analysis
  history <- history %>% arrange(.data$name, .data$iteration)

  # Find final iteration per analysis
  final_points <- history %>%
    dplyr::group_by(.data$name) %>%
    dplyr::filter(.data$iteration == max(.data$iteration)) %>%
    dplyr::ungroup()

  # Determine convergence status per analysis
  converged_names <- tv$name[!is.na(tv$value)]
  final_converged <- final_points[final_points$name %in% converged_names, ]
  final_failed <- final_points[!final_points$name %in% converged_names, ]

  # Build plot
  p <- ggplot2::ggplot(history, ggplot2::aes(x = .data$iteration, y = .data$diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "steelblue", size = 1.5)

  # Converged: red diamond on final iteration
  if (nrow(final_converged) > 0) {
    p <- p + ggplot2::geom_point(
      data = final_converged,
      ggplot2::aes(x = .data$iteration, y = .data$diff),
      shape = 18, size = 4, color = "red"
    )
  }

  # Non-converged: gray diamond + centered "Did not converge" label
  if (nrow(final_failed) > 0) {
    p <- p + ggplot2::geom_point(
      data = final_failed,
      ggplot2::aes(x = .data$iteration, y = .data$diff),
      shape = 18, size = 4, color = "gray50"
    )

    # Center label in the visible area (include y=0 reference line)
    failed_center <- do.call(rbind, lapply(unique(final_failed$name), function(nm) {
      h <- history[history$name == nm, ]
      y_range <- range(c(h$diff, 0))
      data.frame(
        name = nm,
        x = mean(range(h$iteration)),
        y = mean(y_range),
        stringsAsFactors = FALSE
      )
    }))

    p <- p + ggplot2::geom_label(
      data = failed_center,
      ggplot2::aes(x = .data$x, y = .data$y, label = "Did not converge"),
      hjust = 0.5, vjust = 0.5, size = 3.5, color = "gray40",
      fill = "white", linewidth = 0.25
    )
  }

  # Faceting for multiple analyses
  if (length(unique(history$name)) > 1) {
    p <- p + ggplot2::facet_wrap(~ name, scales = "free")
  }

  # Scales and theme
  p <- p +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = "Iteration", y = "Difference from Goal") +
    ggplot2::theme_bw()

  p
}
