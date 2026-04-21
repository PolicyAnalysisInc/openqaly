#' Prepare DSA Cost-Effectiveness Tornado Data
#'
#' Internal helper to prepare DSA cost-effectiveness data for tornado plot rendering.
#'
#' @param results DSA results object from run_dsa()
#' @param health_outcome Name of health outcome summary
#' @param cost_outcome Name of cost summary
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param show_parameter_values Logical. Include parameter values in labels?
#' @param locale Optional locale override
#' @return A list with tornado_data tibble and facet_metadata tibble
#' @keywords internal
prepare_dsa_ce_tornado_data <- function(results,
                                        health_outcome,
                                        cost_outcome,
                                        groups,
                                        interventions,
                                        comparators,
                                        show_parameter_values = TRUE,
                                        locale = NULL) {
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
    distinct(.data$strategy, .data$group, .keep_all = TRUE) %>%
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

  tornado_data <- low_data %>%
    inner_join(high_data, by = c("strategy", "group", "parameter", "parameter_display_name")) %>%
    inner_join(base_data, by = c("strategy", "group")) %>%
    mutate(
      low_icer = as.numeric(.data$low_icer),
      high_icer = as.numeric(.data$high_icer),
      base_icer = as.numeric(.data$base_icer)
    )

  processed_rows <- lapply(seq_len(nrow(tornado_data)), function(i) {
    row <- tornado_data[i, ]

    base_class <- classify_base_case(row$base_icer)
    is_flipped_facet <- (base_class == "flipped")

    low_error_info <- detect_variation_error(base_class, row$low_icer)
    high_error_info <- detect_variation_error(base_class, row$high_icer)

    low_footnote_type <- NA_character_
    if (!is.null(low_error_info$type) && low_error_info$type == "direction_change") {
      low_footnote_type <- "direction_change"
    } else if (!is.null(low_error_info$type) && low_error_info$type == "identical") {
      low_footnote_type <- "identical"
    } else if (is_flipped_facet && low_error_info$show_bar) {
      low_footnote_type <- "flipped_bar"
    }

    high_footnote_type <- NA_character_
    if (!is.null(high_error_info$type) && high_error_info$type == "direction_change") {
      high_footnote_type <- "direction_change"
    } else if (!is.null(high_error_info$type) && high_error_info$type == "identical") {
      high_footnote_type <- "identical"
    } else if (is_flipped_facet && high_error_info$show_bar) {
      high_footnote_type <- "flipped_bar"
    }

    low_label_raw <- if (is.na(low_error_info$label)) {
      format_icer_label(row$low_icer, asterisk = "")
    } else {
      low_error_info$label
    }
    high_label_raw <- if (is.na(high_error_info$label)) {
      format_icer_label(row$high_icer, asterisk = "")
    } else {
      high_error_info$label
    }

    tibble(
      base_class = base_class,
      is_flipped_facet = is_flipped_facet,
      low_error_type = if (is.null(low_error_info$type)) NA_character_ else low_error_info$type,
      low_show_bar = low_error_info$show_bar,
      low_bar_type = if (is.na(low_error_info$bar_type)) NA_character_ else low_error_info$bar_type,
      high_error_type = if (is.null(high_error_info$type)) NA_character_ else high_error_info$type,
      high_show_bar = high_error_info$show_bar,
      high_bar_type = if (is.na(high_error_info$bar_type)) NA_character_ else high_error_info$bar_type,
      low_label_raw = low_label_raw,
      high_label_raw = high_label_raw,
      low_footnote_type = low_footnote_type,
      high_footnote_type = high_footnote_type,
      has_error = !is.null(low_error_info$type) || !is.null(high_error_info$type),
      displayable_range = calculate_displayable_range(
        row$low_icer, row$base_icer, row$high_icer,
        low_error_info$show_bar, high_error_info$show_bar, base_class
      ),
      display_low = if (is.finite(row$low_icer)) abs(row$low_icer) else NA_real_,
      display_base = if (is.finite(row$base_icer)) {
        abs(row$base_icer)
      } else if (base_class == "dominant") {
        0
      } else {
        NA_real_
      },
      display_high = if (is.finite(row$high_icer)) abs(row$high_icer) else NA_real_
    )
  })

  tornado_data <- bind_cols(tornado_data, bind_rows(processed_rows))

  if (show_parameter_values) {
    if (is.null(locale)) {
      locale <- get_results_locale(results)
    }
    param_values <- extract_dsa_parameter_values(
      results = results,
      data = tornado_data,
      interventions = interventions,
      comparators = comparators
    )
    tornado_data <- enhance_dsa_parameter_labels(
      data = tornado_data,
      results = results,
      locale = locale,
      parameter_values = param_values
    )
  }

  facet_metadata <- tornado_data %>%
    distinct(.data$strategy, .data$group, .data$intervention_name, .data$comparator_name,
             .data$base_icer, .data$base_class, .data$is_flipped_facet) %>%
    rowwise() %>%
    mutate(
      has_full_chart_error = (.data$base_class == "identical"),
      full_chart_error_msg = if_else(
        .data$has_full_chart_error,
        sprintf(
          "Tornado plot for %s vs. %s cannot be displayed because the\ndifference in outcomes and costs in the base case is zero,\nresulting in an undefined ICER.",
          .data$intervention_name, .data$comparator_name
        ),
        NA_character_
      ),
      base_footnote_type = if_else(.data$is_flipped_facet, "flipped_base", NA_character_),
      base_label_raw = case_when(
        .data$base_class == "dominated" ~ "Base Case: Dominated",
        .data$base_class == "dominant" ~ "Base Case: Dominant",
        .data$base_class == "flipped" ~ paste0("Base Case: ", format_icer_label(.data$base_icer, asterisk = "")),
        .data$base_class == "normal" ~ paste0("Base Case: ", format_icer_label(.data$base_icer, asterisk = "")),
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()

  strategy_order <- unique(tornado_data$strategy)
  group_order <- get_group_order(unique(tornado_data$group), results$metadata)
  attr(tornado_data, "strategy_order") <- strategy_order
  attr(tornado_data, "group_order") <- group_order

  list(
    tornado_data = tornado_data,
    facet_metadata = facet_metadata
  )
}

#' Render DSA Cost-Effectiveness Tornado Plot
#'
#' Internal helper to create CE tornado plot visualization from prepared data.
#'
#' @param tornado_data Prepared tornado data tibble
#' @param facet_metadata Facet metadata tibble
#' @param dominated_position Optional X position for dominated base case line
#' @param axis_decimals Axis decimals
#' @param label_decimals Label decimals
#' @param locale Optional locale
#' @param abbreviate Logical. Use abbreviated values?
#' @return A ggplot2 object
#' @keywords internal
render_dsa_ce_tornado_plot <- function(tornado_data, facet_metadata, dominated_position = NULL,
                                       axis_decimals = NULL, label_decimals = NULL,
                                       locale = NULL, abbreviate = FALSE) {
  valid_facets <- facet_metadata %>%
    filter(.data$base_class != "identical")

  identical_facets <- facet_metadata %>%
    filter(.data$base_class == "identical")

  if (nrow(valid_facets) > 0) {
    tornado_data <- tornado_data %>%
      semi_join(valid_facets, by = c("strategy", "group"))
  } else {
    tornado_data <- tornado_data %>%
      filter(FALSE)
  }

  if (nrow(tornado_data) == 0 && nrow(identical_facets) == 0) {
    stop("No valid data for tornado plot")
  }

  if (nrow(valid_facets) == 0) {
    msg <- paste(
      "Tornado plot cannot be displayed because the difference in",
      "outcomes and costs in the base case is zero,",
      "resulting in an undefined ICER."
    )
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = msg, size = 5, hjust = 0.5, vjust = 0.5) +
        xlim(0, 1) + ylim(0, 1) +
        theme_void()
    )
  }

  omitted_caption <- NULL
  if (nrow(identical_facets) > 0) {
    has_multiple_groups <- length(unique(facet_metadata$group)) > 1
    has_multiple_strategies <- length(unique(facet_metadata$strategy)) > 1

    omitted_descriptions <- if (has_multiple_groups && has_multiple_strategies) {
      paste0(identical_facets$intervention_name, " vs. ", identical_facets$comparator_name,
             " / ", identical_facets$group)
    } else if (has_multiple_groups) {
      identical_facets$group
    } else {
      paste0(identical_facets$intervention_name, " vs. ", identical_facets$comparator_name)
    }

    omitted_caption <- paste0(
      "The following panels could not be displayed due to identical costs and outcomes ",
      "resulting in an undefined ICER for the base case: ",
      paste(omitted_descriptions, collapse = "; "),
      "."
    )

    facet_metadata <- valid_facets
  }

  n_groups <- length(unique(facet_metadata$group))
  n_strategies <- length(unique(facet_metadata$strategy))
  n_facets <- if (n_groups > 1 && n_strategies > 1) {
    n_groups * n_strategies
  } else if (n_groups > 1) {
    n_groups
  } else if (n_strategies > 1) {
    n_strategies
  } else {
    1
  }
  ncol <- min(2, ceiling(n_facets / 3))

  facet_component <- NULL
  if ((n_groups > 1) && (n_strategies > 1)) {
    facet_component <- facet_wrap(vars(.data$strategy, .data$group), scales = "free_y", ncol = n_groups)
  } else if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group), scales = "free_y", ncol = ncol)
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy), scales = "free_y", ncol = ncol)
  }

  if (nrow(tornado_data) > 0) {
    if (n_groups > 1 && n_strategies > 1) {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = interaction(.data$strategy, .data$group, drop = TRUE))
    } else if (n_groups > 1) {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = .data$group)
    } else if (n_strategies > 1) {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = .data$strategy)
    } else {
      tornado_data <- tornado_data %>%
        mutate(reorder_group = factor("all"))
    }
  } else {
    tornado_data <- tornado_data %>%
      mutate(reorder_group = factor())
  }

  finite_icers <- c(
    tornado_data$display_low[is.finite(tornado_data$display_low)],
    tornado_data$display_base[is.finite(tornado_data$display_base)],
    tornado_data$display_high[is.finite(tornado_data$display_high)]
  )

  if (length(finite_icers) == 0 || max(finite_icers, na.rm = TRUE) == 0) {
    x_data_max <- 100000
  } else {
    x_data_max <- max(finite_icers, na.rm = TRUE)
  }

  breaks_fn <- pretty_breaks(n = 5)
  x_breaks <- breaks_fn(c(0, x_data_max))
  x_breaks <- x_breaks[x_breaks >= 0]
  x_tick_max <- max(x_breaks)

  if (is.null(dominated_position)) {
    dominated_position <- x_tick_max * 1.15
  }
  left_label_margin <- x_tick_max * 0.08
  x_limits <- c(-left_label_margin, dominated_position)

  facet_metadata <- facet_metadata %>%
    mutate(
      base_display_value = case_when(
        .data$base_class == "dominated" ~ dominated_position,
        .data$base_class == "dominant" ~ 0,
        .data$base_class == "identical" ~ NA_real_,
        TRUE ~ abs(as.numeric(.data$base_icer))
      )
    )

  tornado_data <- tornado_data %>%
    left_join(
      facet_metadata %>% select("strategy", "group", "base_display_value"),
      by = c("strategy", "group")
    ) %>%
    mutate(
      low_at_base = .data$low_show_bar &
        .data$low_bar_type == "standard" &
        abs(.data$display_low - .data$base_display_value) < 1e-9,
      high_at_base = .data$high_show_bar &
        .data$high_bar_type == "standard" &
        abs(.data$display_high - .data$base_display_value) < 1e-9,
      low_natural_right = case_when(
        !.data$low_show_bar ~ NA,
        .data$low_bar_type == "arrow" ~ TRUE,
        .data$low_bar_type == "to_zero" ~ FALSE,
        .data$low_bar_type == "standard" ~ .data$display_low > .data$base_display_value,
        TRUE ~ NA
      ),
      high_natural_right = case_when(
        !.data$high_show_bar ~ NA,
        .data$high_bar_type == "arrow" ~ TRUE,
        .data$high_bar_type == "to_zero" ~ FALSE,
        .data$high_bar_type == "standard" ~ .data$display_high > .data$base_display_value,
        TRUE ~ NA
      ),
      low_goes_right = case_when(
        !.data$low_show_bar ~ NA,
        .data$base_class == "dominated" ~ FALSE,
        .data$low_at_base & !.data$high_at_base ~ !.data$high_natural_right,
        TRUE ~ .data$low_natural_right
      ),
      high_goes_right = case_when(
        !.data$high_show_bar ~ NA,
        .data$base_class == "dominated" ~ FALSE,
        .data$high_at_base & !.data$low_at_base ~ !.data$low_natural_right,
        TRUE ~ .data$high_natural_right
      ),
      same_side = (.data$low_show_bar & .data$high_show_bar) &
        !(!(.data$base_class == "dominated") & (.data$low_at_base | .data$high_at_base)) &
        dplyr::coalesce(
          (.data$low_goes_right == TRUE & .data$high_goes_right == TRUE) |
            (.data$low_goes_right == FALSE & .data$high_goes_right == FALSE),
          FALSE
        )
    ) %>%
    select(-"low_at_base", -"high_at_base", -"low_natural_right", -"high_natural_right")

  tornado_long <- tornado_data %>%
    pivot_longer(
      cols = c("low_icer", "high_icer"),
      names_to = "variation_raw",
      values_to = "icer_value"
    ) %>%
    mutate(
      icer_value_num = as.numeric(.data$icer_value),
      variation = if_else(.data$variation_raw == "low_icer", "Low", "High"),
      show_bar = if_else(.data$variation == "Low", .data$low_show_bar, .data$high_show_bar),
      bar_type = dplyr::case_when(
        .data$variation == "Low" ~ .data$low_bar_type,
        TRUE ~ .data$high_bar_type
      ),
      label_raw = if_else(.data$variation == "Low", .data$low_label_raw, .data$high_label_raw),
      error_type = if_else(.data$variation == "Low", .data$low_error_type, .data$high_error_type),
      footnote_type = if_else(.data$variation == "Low", .data$low_footnote_type, .data$high_footnote_type),
      display_value = if_else(
        is.finite(.data$icer_value_num),
        abs(.data$icer_value_num),
        if_else(.data$bar_type == "arrow", dominated_position, NA_real_)
      ),
      other_goes_right = if_else(.data$variation == "Low", .data$high_goes_right, .data$low_goes_right),
      other_has_bar = if_else(.data$variation == "Low", .data$high_show_bar, .data$low_show_bar),
      asterisk = ""
    )

  y_spacing <- 1.2

  tornado_long <- tornado_long %>%
    group_by(.data$reorder_group) %>%
    mutate(
      y_base = dense_rank(desc(interaction(.data$displayable_range, .data$parameter_display_name, lex.order = TRUE))) * y_spacing
    ) %>%
    ungroup()

  asterisk_result <- assign_facet_asterisks(tornado_long, facet_metadata, y_spacing)
  tornado_long <- asterisk_result$tornado_long
  facet_metadata <- asterisk_result$facet_metadata
  facet_footnotes <- asterisk_result$facet_footnotes

  tornado_long <- tornado_long %>%
    mutate(
      label = paste0(.data$label_raw, .data$asterisk),
      bar_height = if_else(.data$same_side, 0.4, 0.8),
      y_offset = case_when(
        .data$same_side & .data$variation == "High" ~ -0.2,
        .data$same_side & .data$variation == "Low" ~ 0.2,
        TRUE ~ 0
      ),
      y_numeric = .data$y_base + .data$y_offset,
      xmin = case_when(
        .data$bar_type == "standard" ~ pmin(.data$base_display_value, .data$display_value),
        .data$bar_type == "to_zero" ~ 0,
        .data$bar_type == "arrow" ~ .data$base_display_value,
        TRUE ~ NA_real_
      ),
      xmax = case_when(
        .data$bar_type == "standard" ~ pmax(.data$base_display_value, .data$display_value),
        .data$bar_type == "to_zero" ~ .data$base_display_value,
        .data$bar_type == "arrow" ~ dominated_position,
        TRUE ~ NA_real_
      ),
      x_center = (.data$xmin + .data$xmax) / 2,
      x_width = .data$xmax - .data$xmin,
      has_base_label = !.data$show_bar & (!is.na(.data$error_type) | !is.na(.data$label)),
      base_label_goes_left = .data$base_class == "dominated" | .data$variation == "Low"
    ) %>%
    group_by(.data$strategy, .data$group, .data$parameter_display_name, .data$base_label_goes_left) %>%
    mutate(
      n_base_labels_same_side = sum(.data$has_base_label),
      base_label_rank = cumsum(.data$has_base_label) * .data$has_base_label
    ) %>%
    ungroup() %>%
    mutate(
      base_label_y_offset = case_when(
        !.data$has_base_label ~ 0,
        .data$n_base_labels_same_side == 1 ~ 0,
        .data$base_label_rank == 1 ~ 0.15,
        .data$base_label_rank == 2 ~ -0.15,
        TRUE ~ 0
      ),
      y_adjusted_base = .data$y_numeric + .data$base_label_y_offset
    )

  y_labels <- tornado_long %>%
    distinct(.data$reorder_group, .data$parameter_display_name, .data$y_base) %>%
    arrange(.data$reorder_group, .data$y_base)

  max_y <- max(tornado_long$y_base, na.rm = TRUE)
  if (!is.finite(max_y) || max_y <= 0) {
    max_y <- 1
  }

  standard_bars <- tornado_long %>% filter(.data$show_bar, .data$bar_type == "standard")
  to_zero_bars <- tornado_long %>% filter(.data$show_bar, .data$bar_type == "to_zero")
  arrow_bars <- tornado_long %>% filter(.data$show_bar, .data$bar_type == "arrow")

  base_case_lines <- facet_metadata %>%
    filter(.data$base_class != "identical", .data$base_class != "dominated") %>%
    select("strategy", "group", "base_display_value")

  base_case_labels <- facet_metadata %>%
    filter(.data$base_class != "identical") %>%
    select("strategy", "group", "base_display_value", "base_label", "base_class") %>%
    mutate(
      y_pos = 0.5 * y_spacing,
      near_left_edge = .data$base_display_value < dominated_position * 0.15,
      label_hjust = case_when(
        .data$base_class == "dominated" ~ 1.1,
        .data$near_left_edge ~ -0.1,
        TRUE ~ 0.5
      ),
      label_x = .data$base_display_value
    )

  p <- ggplot()

  if (nrow(standard_bars) > 0) {
    p <- p + geom_tile(
      data = standard_bars,
      aes(
        y = .data$y_numeric,
        x = .data$x_center,
        width = .data$x_width,
        height = .data$bar_height,
        fill = .data$variation
      ),
      color = "black",
      linewidth = 0.2
    )
  }

  if (nrow(to_zero_bars) > 0) {
    p <- p + geom_tile(
      data = to_zero_bars,
      aes(
        y = .data$y_numeric,
        x = .data$x_center,
        width = .data$x_width,
        height = .data$bar_height,
        fill = .data$variation
      ),
      color = "black",
      linewidth = 0.2
    )
  }

  arrow_notch_y <- 0.15
  arrow_head_width <- arrow_notch_y * 2 * (dominated_position / (max_y + 1))

  if (nrow(arrow_bars) > 0) {
    arrow_polygons <- arrow_bars %>%
      rowwise() %>%
      do(
        bind_cols(
          .,
          tibble(polygon_data = list(tibble(
            x = c(
              .$xmin,
              dominated_position - arrow_head_width,
              dominated_position - arrow_head_width,
              dominated_position,
              dominated_position - arrow_head_width,
              dominated_position - arrow_head_width,
              .$xmin
            ),
            y = c(
              .$y_numeric - .$bar_height/2,
              .$y_numeric - .$bar_height/2,
              .$y_numeric - .$bar_height/2 - arrow_notch_y,
              .$y_numeric,
              .$y_numeric + .$bar_height/2 + arrow_notch_y,
              .$y_numeric + .$bar_height/2,
              .$y_numeric + .$bar_height/2
            )
          )))
        )
      ) %>%
      ungroup()

    arrow_polygons_expanded <- arrow_polygons %>%
      select("parameter", "variation", "strategy", "group", "polygon_data") %>%
      unnest("polygon_data")

    p <- p + geom_polygon(
      data = arrow_polygons_expanded,
      aes(x = .data$x, y = .data$y,
          group = interaction(.data$parameter, .data$variation, .data$strategy, .data$group),
          fill = .data$variation),
      color = "black",
      linewidth = 0.2
    )
  }

  p <- p + geom_vline(
    data = base_case_lines,
    aes(xintercept = .data$base_display_value),
    linewidth = 0.5,
    color = "black"
  )

  p <- p + geom_label(
    data = base_case_labels,
    aes(x = .data$label_x, y = .data$y_pos, label = .data$base_label, hjust = .data$label_hjust),
    vjust = 0,
    size = 2.5,
    fontface = "bold",
    fill = "white",
    label.padding = unit(0.25, "lines")
  )

  label_data <- tornado_long %>%
    filter(.data$show_bar, !is.na(.data$label)) %>%
    mutate(
      is_no_impact = abs(.data$display_value - .data$base_display_value) < .Machine$double.eps * 100,
      effective_goes_right = case_when(
        .data$bar_type == "arrow" ~ TRUE,
        .data$bar_type == "to_zero" ~ FALSE,
        TRUE ~ .data$display_value > .data$base_display_value
      ),
      effective_goes_left = case_when(
        .data$bar_type == "arrow" ~ FALSE,
        .data$bar_type == "to_zero" ~ TRUE,
        TRUE ~ .data$display_value < .data$base_display_value
      )
    ) %>%
    group_by(.data$strategy, .data$group, .data$parameter_display_name) %>%
    mutate(
      other_goes_right = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$effective_goes_right),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$effective_goes_right),
        TRUE ~ FALSE
      ),
      other_goes_left = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$effective_goes_left),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$effective_goes_left),
        TRUE ~ FALSE
      ),
      other_is_no_impact = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$is_no_impact),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$is_no_impact),
        TRUE ~ FALSE
      )
    ) %>%
    ungroup() %>%
    mutate(
      goes_right = case_when(
        .data$base_class == "dominated" ~ FALSE,
        !.data$is_no_impact ~ .data$effective_goes_right,
        .data$is_no_impact & .data$other_goes_left ~ TRUE,
        .data$is_no_impact & .data$other_goes_right ~ FALSE,
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "High" ~ TRUE,
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "Low" ~ FALSE,
        TRUE ~ .data$effective_goes_right
      ),
      is_arrow = .data$bar_type == "arrow",
      is_to_zero = .data$bar_type == "to_zero",
      label_x = case_when(
        .data$is_arrow ~ dominated_position - arrow_head_width,
        .data$is_to_zero ~ .data$xmin,
        .data$goes_right ~ .data$xmax,
        TRUE ~ .data$xmin
      ),
      label_hjust = case_when(
        .data$is_arrow ~ 1.1,
        .data$is_to_zero & .data$base_class == "dominant" & .data$other_goes_right ~ 1.1,
        .data$is_to_zero ~ -0.1,
        .data$goes_right ~ -0.1,
        TRUE ~ 1.1
      )
    )

  if (nrow(label_data) > 0) {
    p <- p + geom_text(
      data = label_data,
      aes(x = .data$label_x, y = .data$y_numeric, label = .data$label,
          hjust = .data$label_hjust),
      vjust = 0.5,
      size = 2.5
    )
  }

  error_label_data <- tornado_long %>%
    filter(!.data$show_bar, !is.na(.data$error_type)) %>%
    mutate(
      formatted_icer = oq_format_icer(abs(.data$icer_value_num), decimals = label_decimals, locale = locale, abbreviate = abbreviate),
      label_goes_left = .data$base_class == "dominated" | .data$variation == "Low",
      error_label = dplyr::case_when(
        .data$error_type == "direction_change" ~ paste0(.data$formatted_icer, .data$asterisk),
        .data$error_type == "identical" ~ paste0("Identical", .data$asterisk),
        TRUE ~ NA_character_
      ),
      label_x = .data$base_display_value,
      label_hjust = if_else(.data$label_goes_left, 1.1, -0.1)
    ) %>%
    filter(!is.na(.data$error_label))

  if (nrow(error_label_data) > 0) {
    p <- p + geom_text(
      data = error_label_data,
      aes(x = .data$label_x, y = .data$y_adjusted_base, label = .data$error_label,
          hjust = .data$label_hjust),
      vjust = 0.5,
      size = 2.5
    )
  }

  same_status_labels <- tornado_long %>%
    filter(!.data$show_bar, !is.na(.data$label), is.na(.data$error_type)) %>%
    mutate(
      label_goes_left = case_when(
        .data$base_class == "dominated" ~ TRUE,
        .data$base_class == "dominant" & .data$other_has_bar & !is.na(.data$other_goes_right) ~ .data$other_goes_right,
        TRUE ~ .data$variation == "Low"
      ),
      label_x = .data$base_display_value,
      label_hjust = if_else(.data$label_goes_left, 1.1, -0.1)
    )

  if (nrow(same_status_labels) > 0) {
    p <- p + geom_text(
      data = same_status_labels,
      aes(x = .data$label_x, y = .data$y_adjusted_base, label = .data$label,
          hjust = .data$label_hjust),
      vjust = 0.5,
      size = 2.5
    )
  }

  p <- p +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = oq_label_fn(decimals = axis_decimals, locale = locale, abbreviate = TRUE),
      expand = expansion(mult = c(0, 0), add = c(0, 0))
    ) +
    scale_y_reverse(
      breaks = seq_len(ceiling(max_y / y_spacing)) * y_spacing,
      labels = function(y) {
        sapply(y, function(yval) {
          match_row <- y_labels %>% filter(.data$y_base == yval) %>% slice(1)
          if (nrow(match_row) > 0) match_row$parameter_display_name else ""
        })
      },
      expand = expansion(mult = c(0, 0), add = c(0.3, 0.7))
    )

  has_fill_data <- (nrow(standard_bars) > 0 || nrow(to_zero_bars) > 0 ||
                      nrow(arrow_bars) > 0)
  if (has_fill_data) {
    p <- p + scale_fill_manual(values = c("Low" = "#F8766D", "High" = "#00BFC4"))
  }

  footnote_data_list <- list()
  for (i in seq_len(nrow(facet_metadata))) {
    strat <- facet_metadata$strategy[i]
    grp <- facet_metadata$group[i]
    facet_key <- paste(strat, grp, sep = "|||")

    if (facet_key %in% names(facet_footnotes)) {
      fn_texts <- facet_footnotes[[facet_key]]
      if (length(fn_texts) > 0) {
        facet_max_y <- tornado_long %>%
          filter(.data$strategy == strat, .data$group == grp) %>%
          pull(.data$y_base) %>%
          max(na.rm = TRUE)

        if (!is.finite(facet_max_y)) facet_max_y <- max_y

        combined_footnotes <- paste(fn_texts, collapse = "\n")
        footnote_line_count <- stringr::str_count(combined_footnotes, "\n") + 1
        footnote_lane_top <- facet_max_y + 0.7
        footnote_lane_height <- 0.7 + 0.75 * footnote_line_count
        footnote_lane_bottom <- footnote_lane_top + footnote_lane_height
        footnote_x <- x_limits[1] + 0.02 * diff(x_limits)

        footnote_data_list[[length(footnote_data_list) + 1]] <- tibble(
          strategy = strat,
          group = grp,
          x = footnote_x,
          y = footnote_lane_top + footnote_lane_height / 2,
          footnote_lane_bottom = footnote_lane_bottom,
          footnote_text = combined_footnotes
        )
      }
    }
  }

  if (length(footnote_data_list) > 0) {
    footnote_data <- bind_rows(footnote_data_list)
    p <- p + geom_blank(
      data = footnote_data,
      aes(x = .data$x, y = .data$footnote_lane_bottom)
    )

    p <- p + geom_label(
      data = footnote_data,
      aes(x = .data$x, y = .data$y, label = .data$footnote_text),
      hjust = 0,
      vjust = 0.5,
      size = 2.2,
      fontface = "italic",
      fill = "white",
      color = "black",
      linewidth = 0.3,
      label.padding = unit(0.3, "lines")
    )
  }

  p <- p +
    labs(
      y = NULL,
      x = "ICER",
      fill = "Parameter Value",
      caption = omitted_caption
    ) +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0, size = 9, margin = margin(t = 15))
    )

  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}

#' Plot DSA Cost-Effectiveness as Tornado Diagram
#'
#' Creates a tornado plot showing the impact of varying each DSA parameter on
#' the Incremental Cost-Effectiveness Ratio (ICER).
#'
#' @param results A openqaly DSA results object (output from run_dsa)
#' @param health_outcome Name of the health outcome summary
#' @param cost_outcome Name of the cost summary
#' @param groups Group selection
#' @param interventions Character vector of intervention strategy name(s)
#' @param comparators Character vector of comparator strategy name(s)
#' @param show_parameter_values Logical. Include parameter values in Y-axis labels?
#' @param drop_zero_impact Logical. Remove parameters with zero impact on ICER?
#' @param top_n Integer or NULL. Top N parameters by impact range.
#' @return A ggplot2 object
#' @export
dsa_ce_plot <- function(results,
                        health_outcome,
                        cost_outcome,
                        groups = "overall",
                        interventions = NULL,
                        comparators = NULL,
                        show_parameter_values = TRUE,
                        drop_zero_impact = TRUE,
                        top_n = NULL) {
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for CE plot")
  }

  prepared <- prepare_dsa_ce_tornado_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    show_parameter_values = show_parameter_values
  )

  tornado_data <- prepared$tornado_data
  facet_metadata <- prepared$facet_metadata

  if (is.null(tornado_data) || nrow(tornado_data) == 0) {
    stop("No data available for CE tornado plot with specified parameters")
  }

  if (drop_zero_impact) {
    n_params_before <- nrow(tornado_data)
    tornado_data <- tornado_data %>%
      filter(!is.na(.data$displayable_range) & abs(.data$displayable_range) > .Machine$double.eps * 100)

    if (nrow(tornado_data) == 0) {
      stop(sprintf("All %d parameters have zero impact on ICER. No data to plot.",
                   n_params_before))
    }
  }

  if (!is.null(top_n)) {
    tornado_data <- tornado_data %>%
      group_by(.data$strategy, .data$group) %>%
      slice_max(order_by = abs(.data$displayable_range), n = top_n, with_ties = FALSE) %>%
      ungroup()
  }

  strategy_levels <- attr(prepared$tornado_data, "strategy_order")
  if (is.null(strategy_levels)) {
    strategy_levels <- unique(tornado_data$strategy)
  }

  group_levels <- attr(prepared$tornado_data, "group_order")
  if (is.null(group_levels)) {
    group_levels <- unique(tornado_data$group)
  }

  tornado_data <- tornado_data %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels),
      group = factor(.data$group, levels = group_levels)
    )

  facet_metadata <- facet_metadata %>%
    mutate(
      strategy = factor(.data$strategy, levels = strategy_levels),
      group = factor(.data$group, levels = group_levels)
    )

  render_dsa_ce_tornado_plot(tornado_data, facet_metadata)
}
