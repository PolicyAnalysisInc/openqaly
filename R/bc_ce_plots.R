#' Plot Incremental Cost-Effectiveness Frontier
#'
#' Creates a cost-effectiveness plane showing all strategies as points with line
#' segments connecting strategies on the efficiency frontier.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#'
#' @return A ggplot2 object
#'
#' @details
#' The cost-effectiveness plane plots outcome (x-axis) vs. cost (y-axis).
#' Strategies on the efficiency frontier are connected with line segments.
#' Dominated and extended dominated strategies are shown with different styling.
#'
#' When \code{group = NULL}, creates faceted plots for each group and aggregated results.
#'
#' CE calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # CE frontier for aggregated population
#' incremental_ce_plot(results, "total_qalys", "total_cost")
#'
#' # For all groups with faceting
#' incremental_ce_plot(results, "total_qalys", "total_cost", group = NULL)
#' }
#'
#' @export
incremental_ce_plot <- function(res,
                                outcome_summary,
                                cost_summary,
                                groups = "overall",
                                strategies = NULL) {

  # Calculate incremental CE (always uses discounted values)
  ce_data <- calculate_incremental_ce(
    res,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Create status factor for coloring/shaping
  ce_data <- ce_data %>%
    mutate(
      status = case_when(
        .data$strictly_dominated ~ "Dominated",
        .data$extendedly_dominated ~ "Extended Dominated",
        .data$on_frontier ~ "On Frontier",
        TRUE ~ "Other"
      ),
      status = factor(.data$status, levels = c("On Frontier", "Extended Dominated", "Dominated", "Other"))
    )

  # Get frontier data for line segments
  frontier_data <- ce_data %>%
    filter(.data$on_frontier) %>%
    arrange(.data$group, .data$cost)

  # Map summary names for axis labels
  outcome_label <- outcome_summary
  cost_label <- cost_summary
  if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
    outcome_label <- map_names(outcome_summary, res$metadata$summaries, "display_name")
    cost_label <- map_names(cost_summary, res$metadata$summaries, "display_name")
  }

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(ce_data$group), res$metadata)
  ce_data <- ce_data %>%
    mutate(group = factor(.data$group, levels = group_levels))
  frontier_data <- frontier_data %>%
    mutate(group = factor(.data$group, levels = group_levels))

  # Map strategy names to display names
  if (!is.null(res$metadata) && !is.null(res$metadata$strategies)) {
    ce_data$strategy <- map_names(ce_data$strategy, res$metadata$strategies, "display_name")
    frontier_data$strategy <- map_names(frontier_data$strategy, res$metadata$strategies, "display_name")
  }

  # Determine faceting
  n_groups <- length(unique(ce_data$group))

  facet_component <- NULL
  if (n_groups > 1) {
    facet_component <- facet_wrap(~ group, scales = "free")
  }

  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, ce_data$outcome))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, ce_data$cost))
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # Create the plot
  p <- ggplot(ce_data, aes(x = .data$outcome, y = .data$cost)) +
    geom_point(aes(color = .data$strategy)) +
    geom_line(data = frontier_data, aes(group = .data$group)) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = comma
    ) +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits,
      labels = comma
    ) +
    labs(x = outcome_label, y = cost_label, color = "Strategy") +
    theme_bw() +
    theme(legend.position = "bottom")

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}


#' Plot Pairwise Cost-Effectiveness Comparisons
#'
#' Creates a cost-effectiveness plane showing pairwise comparisons against a single
#' reference strategy. Uses delta axes (incremental cost vs. incremental outcome)
#' with the reference strategy at the origin.
#'
#' @param res A openqaly model results object (output from run_model)
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL (all groups + aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Character vector of intervention strategies (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of comparator strategies (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param wtp Willingness-to-pay threshold for the WTP line. If NULL (default), uses
#'   the WTP from the outcome summary metadata if available. If neither is available,
#'   no WTP line is drawn.
#'
#' @return A ggplot2 object
#'
#' @details
#' The cost-effectiveness plane plots incremental outcome (x-axis) vs. incremental cost (y-axis).
#' The origin (0,0) represents the reference strategy. A dotted WTP threshold line is drawn
#' through the origin if a WTP value is available (from metadata or explicit argument).
#'
#' When comparators is specified:
#' - All other strategies are plotted as points showing their incremental values vs. comparator
#' - Lines connect origin to each point, with slope representing ICER
#' - Single plot (or faceted by group if group=NULL)
#'
#' When interventions is specified:
#' - Each comparison gets its own facet panel showing intervention vs. one other strategy
#' - One point per panel at the intervention's incremental position
#' - Line shows ICER slope from origin
#' - If group=NULL: uses facet_grid with group on rows, comparison on columns
#'
#' CE calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Pairwise CE plane vs control (comparator perspective)
#' pairwise_ce_plot(results, "total_qalys", "total_cost", comparators = "control")
#'
#' # New treatment vs others (intervention perspective)
#' pairwise_ce_plot(results, "total_qalys", "total_cost", interventions = "new_treatment")
#'
#' # For all groups
#' pairwise_ce_plot(results, "total_qalys", "total_cost", group = NULL,
#'                  comparators = "control")
#'
#' # With explicit WTP threshold line
#' pairwise_ce_plot(results, "total_qalys", "total_cost",
#'                  comparators = "control", wtp = 100000)
#' }
#'
#' @export
pairwise_ce_plot <- function(res,
                             outcome_summary,
                             cost_summary,
                             groups = "overall",
                             strategies = NULL,
                             interventions = NULL,
                             comparators = NULL,
                             wtp = NULL) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Calculate pairwise CE (always uses discounted values)
  ce_data <- calculate_pairwise_ce(
    res,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators
  )

  # Get WTP for threshold line (optional)
  effective_wtp <- wtp
  if (is.null(effective_wtp)) {
    if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
      outcome_meta <- res$metadata$summaries %>%
        filter(.data$name == outcome_summary)
      if (nrow(outcome_meta) > 0 && !is.na(outcome_meta$wtp[1])) {
        effective_wtp <- outcome_meta$wtp[1]
      }
    }
  }

  # Map summary names for axis labels
  outcome_label <- outcome_summary
  cost_label <- cost_summary
  if (!is.null(res$metadata) && !is.null(res$metadata$summaries)) {
    outcome_label <- map_names(outcome_summary, res$metadata$summaries, "display_name")
    cost_label <- map_names(cost_summary, res$metadata$summaries, "display_name")
  }

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(ce_data$group), res$metadata)
  ce_data <- ce_data %>%
    mutate(group = factor(.data$group, levels = group_levels))

  # Map strategy names to display names
  if (!is.null(res$metadata) && !is.null(res$metadata$strategies)) {
    ce_data$strategy <- map_names(ce_data$strategy, res$metadata$strategies, "display_name")
    ce_data$comparator <- map_names(ce_data$comparator, res$metadata$strategies, "display_name")
  }

  # Get unique groups and comparisons
  n_groups <- length(unique(ce_data$group))
  n_comparisons <- nrow(ce_data) / n_groups  # Approximate

  # Create comparison label
  ce_data <- ce_data %>%
    mutate(comparison = paste(.data$strategy, "vs.", .data$comparator))

  # Calculate symmetric axis limits centered on zero
  x_max <- max(abs(ce_data$doutcome), na.rm = TRUE)
  y_max <- max(abs(ce_data$dcost), na.rm = TRUE)
  x_limits <- c(-x_max, x_max)
  y_limits <- c(-y_max, y_max)

  # Determine plot mode based on number of comparisons
  if (n_comparisons <= 5) {
    # Few comparisons: all on one plot with color by comparison
    p <- ggplot(ce_data, aes(x = .data$doutcome, y = .data$dcost)) +
      geom_point(aes(color = .data$comparison), size = 3) +
      geom_segment(aes(xend = .data$doutcome, yend = .data$dcost, color = .data$comparison),
                   x = 0, y = 0, alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      scale_x_continuous(labels = comma, limits = x_limits) +
      scale_y_continuous(labels = comma, limits = y_limits) +
      labs(x = paste0("\u0394 ", outcome_label),
           y = paste0("\u0394 ", cost_label),
           color = "Comparison") +
      theme_bw() +
      theme(legend.position = "bottom")

    # Add faceting if multiple groups
    if (n_groups > 1) {
      p <- p + facet_wrap(~ group)
    }

  } else {
    # Many comparisons: separate panel per comparison
    p <- ggplot(ce_data, aes(x = .data$doutcome, y = .data$dcost)) +
      geom_point(color = "blue", size = 3) +
      geom_segment(aes(xend = .data$doutcome, yend = .data$dcost),
                   x = 0, y = 0, color = "gray50", alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
      scale_x_continuous(labels = comma, limits = x_limits) +
      scale_y_continuous(labels = comma, limits = y_limits) +
      labs(x = paste0("\u0394 ", outcome_label),
           y = paste0("\u0394 ", cost_label)) +
      theme_bw() +
      theme(legend.position = "bottom")

    # Add faceting
    if (n_groups > 1) {
      # Multi-group: facet_grid with group on rows, comparison on columns
      p <- p + facet_grid(group ~ comparison)
    } else {
      # Single group: facet_wrap by comparison
      p <- p + facet_wrap(~ comparison)
    }
  }

  # Add WTP threshold line if available
  if (!is.null(effective_wtp)) {
    p <- p +
      geom_abline(slope = effective_wtp, intercept = 0,
                  linetype = "dotted", color = "black", alpha = 0.7)
  }

  p
}
