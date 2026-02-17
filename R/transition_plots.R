#' Transition Matrix Heatmap Plot
#'
#' Visualizes transition probabilities as a heatmap for a specific cycle.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param cycle Integer. Which cycle's transition matrix to display (default: 1).
#' @param groups Group selection: "overall" (default), specific group name,
#'   vector of groups, "all", "all_groups", or NULL
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param collapsed Logical. If TRUE (default), use collapsed state names.
#'   If FALSE, use expanded state names (tunnel states).
#' @param decimals Integer. Number of decimal places for probability labels (default: 3).
#' @param use_display_names Logical. If TRUE (default), use display names for entities.
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Basic heatmap for cycle 1
#' transition_plot_heatmap(results)
#'
#' # Heatmap for cycle 5 with more decimal places
#' transition_plot_heatmap(results, cycle = 5, decimals = 4)
#' }
#'
#' @export
transition_plot_heatmap <- function(res,
                                    cycle = 1,
                                    groups = "overall",
                                    strategies = NULL,
                                    collapsed = TRUE,
                                    decimals = 3,
                                    use_display_names = TRUE) {

  # Get transition data in long format
  trans_data <- get_transitions(
    res,
    format = "long",
    collapsed = collapsed,
    strategies = strategies,
    groups = groups,
    cycles = cycle,
    use_display_names = use_display_names
  )

  # Get state ordering from metadata
  if (!is.null(res$metadata) && !is.null(res$metadata$states)) {
    base_levels <- if (use_display_names) {
      res$metadata$states$display_name
    } else {
      res$metadata$states$name
    }

    all_states <- unique(c(trans_data$from_state, trans_data$to_state))

    if (collapsed) {
      state_levels <- base_levels[base_levels %in% all_states]
    } else {
      # For expanded view, sort by base state order then tunnel index
      state_levels <- sort_expanded_states(all_states, base_levels)
    }

    trans_data$from_state <- factor(trans_data$from_state, levels = rev(state_levels))
    trans_data$to_state <- factor(trans_data$to_state, levels = state_levels)
  }

  # Apply consistent group ordering
  group_levels <- get_group_order(unique(trans_data$group), res$metadata)
  trans_data$group <- factor(trans_data$group, levels = group_levels)

  # Format probability labels
  trans_data$label <- format(round(trans_data$probability, decimals),
                             nsmall = decimals, scientific = FALSE, trim = TRUE)

  # Determine faceting
  n_strategies <- length(unique(trans_data$strategy))
  n_groups <- length(unique(trans_data$group))

  if (n_strategies > 1 && n_groups > 1) {
    facet_component <- facet_grid(rows = vars(.data$group), cols = vars(.data$strategy))
  } else if (n_strategies > 1) {
    facet_component <- facet_wrap(vars(.data$strategy))
  } else if (n_groups > 1) {
    facet_component <- facet_wrap(vars(.data$group))
  } else {
    facet_component <- NULL
  }

  # Build plot
  p <- ggplot(trans_data, aes(x = .data$to_state, y = .data$from_state, fill = .data$probability)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = .data$label), size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, 1)) +
    scale_x_discrete(sec.axis = dup_axis()) +
    scale_y_discrete(sec.axis = dup_axis()) +
    theme_bw() +
    labs(
      x = "To State",
      y = "From State",
      fill = "Probability",
      title = paste("Transition Matrix - Cycle", cycle)
    ) +
    theme(
      axis.text.x.bottom = element_text(angle = 45, hjust = 1),
      axis.text.x.top = element_text(angle = 45, hjust = 0),
      axis.title.x.top = element_blank(),
      axis.title.y.right = element_blank(),
      panel.grid = element_blank()
    )

  # Add faceting
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}
