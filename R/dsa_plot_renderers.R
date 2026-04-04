#' Render Tornado Plot from Prepared Data
#'
#' Internal helper to create tornado plot visualization from prepared tornado data.
#'
#' @param tornado_data Prepared tornado data
#' @param summary_label String for x-axis label
#' @param facet_component Optional faceting component
#' @param axis_decimals Axis decimals
#' @param label_decimals Label decimals
#' @param locale Optional locale
#' @param abbreviate Logical. Use abbreviated values?
#' @param currency Logical. Format as currency?
#' @return A ggplot2 object
#' @keywords internal
render_tornado_plot <- function(tornado_data, summary_label, facet_component = NULL,
                                axis_decimals = NULL, label_decimals = NULL,
                                locale = NULL, abbreviate = FALSE, currency = FALSE) {

  n_groups <- length(unique(tornado_data$group))
  n_strategies <- length(unique(tornado_data$strategy))

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

  if (is.null(facet_component)) {
    if ((n_groups > 1) && (n_strategies > 1)) {
      facet_component <- facet_wrap(vars(.data$strategy, .data$group), scales = "free_y", ncol = n_groups)
    } else if ((n_groups > 1) && (n_strategies == 1)) {
      facet_component <- facet_wrap(vars(.data$group), scales = "free_y", ncol = ncol)
    } else if ((n_strategies > 1) && (n_groups == 1)) {
      facet_component <- facet_wrap(vars(.data$strategy), scales = "free_y", ncol = ncol)
    } else {
      facet_component <- NULL
    }
  }

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

  y_spacing <- 1.2

  tornado_data <- tornado_data %>%
    mutate(same_side = (.data$low > .data$base & .data$high > .data$base) |
                       (.data$low < .data$base & .data$high < .data$base))

  tornado_long <- tornado_data %>%
    pivot_longer(
      cols = c("low", "high"),
      names_to = "variation",
      values_to = "value"
    ) %>%
    mutate(
      variation = factor(.data$variation, levels = c("low", "high"),
                         labels = c("Low", "High")),
      xmin = pmin(.data$base, .data$value),
      xmax = pmax(.data$base, .data$value),
      x_center = (.data$xmin + .data$xmax) / 2,
      x_width = .data$xmax - .data$xmin,
      bar_height = if_else(.data$same_side, 0.4, 0.8),
      y_offset = case_when(
        .data$same_side & .data$variation == "High" ~ -0.2,
        .data$same_side & .data$variation == "Low" ~ 0.2,
        TRUE ~ 0
      )
    )

  base_case_data <- tornado_long %>%
    distinct(.data$strategy, .data$group, .data$base, .data$reorder_group)

  base_case_labels <- base_case_data %>%
    mutate(
      base_label = paste0("Base Case: ", oq_format(.data$base, decimals = label_decimals, locale = locale, abbreviate = abbreviate, currency = currency)),
      y_pos = 0.5 * y_spacing
    )

  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, tornado_long$xmin, tornado_long$xmax))
  x_breaks <- breaks_fn(x_range)
  x_tick_max <- max(abs(x_breaks))
  left_label_margin <- x_tick_max * 0.08
  x_limits <- c(min(x_breaks) - left_label_margin, max(x_breaks))

  param_ranks <- tornado_long %>%
    distinct(.data$reorder_group, .data$parameter_display_name, .data$range) %>%
    group_by(.data$reorder_group) %>%
    arrange(desc(.data$range), .data$parameter_display_name, .by_group = TRUE) %>%
    mutate(y_base = row_number() * y_spacing) %>%
    ungroup() %>%
    select("reorder_group", "parameter_display_name", "y_base")

  tornado_long <- tornado_long %>%
    left_join(param_ranks, by = c("reorder_group", "parameter_display_name")) %>%
    mutate(y_numeric = .data$y_base + .data$y_offset)

  y_labels <- tornado_long %>%
    distinct(.data$reorder_group, .data$parameter_display_name, .data$y_base) %>%
    arrange(.data$reorder_group, .data$y_base)

  max_y <- max(tornado_long$y_base)

  label_data <- tornado_long %>%
    mutate(
      label = oq_format(.data$value, decimals = label_decimals, locale = locale, abbreviate = abbreviate, currency = currency),
      is_no_impact = abs(.data$value - .data$base) < .Machine$double.eps * 100,
      natural_goes_right = .data$value > .data$base,
      natural_goes_left = .data$value < .data$base
    ) %>%
    group_by(.data$reorder_group, .data$parameter_display_name) %>%
    mutate(
      other_goes_right = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$natural_goes_right),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$natural_goes_right),
        TRUE ~ FALSE
      ),
      other_goes_left = case_when(
        .data$variation == "Low" ~ any(.data$variation == "High" & .data$natural_goes_left),
        .data$variation == "High" ~ any(.data$variation == "Low" & .data$natural_goes_left),
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
        !.data$is_no_impact ~ .data$natural_goes_right,
        .data$is_no_impact & .data$other_goes_left ~ TRUE,
        .data$is_no_impact & .data$other_goes_right ~ FALSE,
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "High" ~ TRUE,
        .data$is_no_impact & .data$other_is_no_impact & .data$variation == "Low" ~ FALSE,
        TRUE ~ .data$natural_goes_right
      ),
      label_x = if_else(.data$goes_right, .data$xmax, .data$xmin),
      label_hjust = if_else(.data$goes_right, -0.1, 1.1)
    )

  p <- ggplot(tornado_long, aes(
    y = .data$y_numeric,
    x = .data$x_center,
    width = .data$x_width,
    height = .data$bar_height,
    fill = .data$variation
  )) +
    geom_tile(color = "black", linewidth = 0.2) +
    geom_vline(data = base_case_data, aes(xintercept = .data$base), linewidth = 0.5) +
    geom_label(
      data = base_case_labels,
      aes(x = .data$base, y = .data$y_pos, label = .data$base_label),
      vjust = 0,
      size = 2.5,
      fontface = "bold",
      fill = "white",
      label.padding = unit(0.25, "lines"),
      inherit.aes = FALSE
    ) +
    geom_text(
      data = label_data,
      aes(x = .data$label_x, y = .data$y_numeric, label = .data$label,
          hjust = .data$label_hjust),
      vjust = 0.5,
      size = 2.5,
      inherit.aes = FALSE
    ) +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, labels = oq_label_fn(decimals = axis_decimals, locale = locale, abbreviate = TRUE, currency = currency)) +
    scale_y_reverse(
      breaks = seq_len(ceiling(max_y / y_spacing)) * y_spacing,
      labels = function(y) {
        sapply(y, function(yval) {
          match_row <- y_labels %>% filter(.data$y_base == yval) %>% slice(1)
          if (nrow(match_row) > 0) match_row$parameter_display_name else ""
        })
      },
      expand = expansion(mult = c(0, 0), add = c(0.3, 0.7))
    ) +
    scale_fill_manual(values = c("Low" = "#F8766D", "High" = "#00BFC4")) +
    labs(y = NULL, x = summary_label, fill = "Parameter Value") +
    theme_bw() +
    theme(
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )

  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}
