#' Plot Incremental Cost-Effectiveness Acceptability Curves
#'
#' Creates a line plot showing the probability that each strategy is the most
#' cost-effective at different willingness-to-pay thresholds in a multi-way
#' (incremental) comparison.
#'
#' @param results A heRomod2 PSA results object, or pre-calculated CEAC data
#'   from calculate_incremental_ceac()
#' @param outcome_summary Name of the outcome summary (required if results is
#'   a model object)
#' @param cost_summary Name of the cost summary (required if results is a
#'   model object)
#' @param wtp_range Numeric vector with min and max WTP values for x-axis.
#'   Default is c(0, 100000)
#' @param wtp_step Step size for WTP thresholds. Default is 5000
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (triggers faceting by group)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param show_frontier Logical. Overlay the CE acceptability frontier?
#' @param title Plot title. If NULL, auto-generated
#' @param xlab X-axis label. Default is "Willingness to Pay"
#' @param ylab Y-axis label. Default is "Probability of Being Most Cost-Effective"
#' @param legend_position Legend position: "bottom" (default), "right", "top",
#'   "left", or "none"
#'
#' @return A ggplot2 object
#'
#' @details
#' The incremental CEAC plot shows how the probability of each strategy being
#' most cost-effective changes with the willingness-to-pay threshold in a
#' multi-way comparison. The probabilities across all strategies sum to 1 at
#' each WTP value.
#'
#' When multiple groups are present, the plot is automatically faceted by group
#' using \code{facet_wrap(~ group, scales = "fixed")}.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Basic incremental CEAC plot
#' incremental_ceac_plot(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP range
#' incremental_ceac_plot(psa_results, "total_qalys", "total_cost",
#'                       wtp_range = c(0, 200000), wtp_step = 10000)
#'
#' # With frontier overlay
#' incremental_ceac_plot(psa_results, "total_qalys", "total_cost",
#'                       show_frontier = TRUE)
#' }
#'
#' @export
incremental_ceac_plot <- function(results,
                                  outcome_summary = NULL,
                                  cost_summary = NULL,
                                  wtp_range = c(0, 100000),
                                  wtp_step = 5000,
                                  group = "aggregated",
                                  strategies = NULL,
                                  discounted = FALSE,
                                  show_frontier = FALSE,
                                  title = NULL,
                                  xlab = "Willingness to Pay",
                                  ylab = "Probability of Being Most Cost-Effective",
                                  legend_position = "bottom") {

  # Check if results is pre-calculated CEAC data or model results
  if ("wtp" %in% names(results) && "probability" %in% names(results)) {
    # Pre-calculated CEAC data
    ceac_data <- results
  } else {
    # Model results - calculate CEAC
    if (is.null(outcome_summary) || is.null(cost_summary)) {
      stop("outcome_summary and cost_summary required when results is a model object")
    }

    # Generate WTP sequence
    wtp_seq <- seq(wtp_range[1], wtp_range[2], by = wtp_step)

    ceac_data <- calculate_incremental_ceac(
      results = results,
      outcome_summary = outcome_summary,
      cost_summary = cost_summary,
      wtp = wtp_seq,
      group = group,
      strategies = strategies,
      discounted = discounted
    )
  }

  # Filter strategies if specified
  if (!is.null(strategies)) {
    ceac_data <- ceac_data %>%
      filter(strategy %in% strategies)
  }

  # Create base plot
  p <- ggplot(ceac_data, aes(x = wtp, y = probability, color = strategy)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_x_continuous(labels = scales::comma) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Strategy"
    )

  # Add frontier if requested
  if (show_frontier) {
    if ("wtp" %in% names(results) && "probability" %in% names(results)) {
      warning("Cannot add frontier to pre-calculated CEAC data. Pass model results instead.")
    } else {
      frontier_data <- calculate_incremental_ceac_frontier(
        results = results,
        outcome_summary = outcome_summary,
        cost_summary = cost_summary,
        wtp = wtp_seq,
        group = group,
        strategies = strategies,
        discounted = discounted
      )

      # Add frontier points
      p <- p +
        geom_point(
          data = frontier_data,
          aes(x = wtp, y = probability),
          color = "black",
          size = 2,
          shape = 16
        )
    }
  }

  # Facet if multiple groups
  n_groups <- length(unique(ceac_data$group))
  if (n_groups > 1) {
    p <- p + facet_wrap(~ group, scales = "fixed")
  }

  # Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}


#' Plot Incremental Cost-Effectiveness Acceptability Frontier
#'
#' Creates a plot showing which strategy is optimal (highest expected NMB)
#' at each willingness-to-pay threshold in a multi-way (incremental) comparison,
#' along with the probability that it is actually the best choice.
#'
#' @inheritParams incremental_ceac_plot
#' @param show_probability Logical. Show probability as line thickness or
#'   transparency? Default is FALSE
#'
#' @return A ggplot2 object
#'
#' @details
#' The incremental CE acceptability frontier shows the optimal strategy choice
#' based on expected net monetary benefit at each WTP threshold in a multi-way
#' comparison. Vertical lines or shaded regions indicate where the optimal
#' strategy changes.
#'
#' When multiple groups are present, the plot is automatically faceted by group
#' using \code{facet_wrap(~ group, scales = "fixed")}.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Basic frontier plot
#' incremental_ceac_frontier_plot(psa_results, "total_qalys", "total_cost")
#'
#' # With probability shown
#' incremental_ceac_frontier_plot(psa_results, "total_qalys", "total_cost",
#'                                show_probability = TRUE)
#' }
#'
#' @export
incremental_ceac_frontier_plot <- function(results,
                                           outcome_summary,
                                           cost_summary,
                                           wtp_range = c(0, 100000),
                                           wtp_step = 5000,
                                           group = "aggregated",
                                           strategies = NULL,
                                           discounted = FALSE,
                                           show_probability = FALSE,
                                           title = NULL,
                                           xlab = "Willingness to Pay",
                                           ylab = "Optimal Strategy",
                                           legend_position = "bottom") {

  # Generate WTP sequence
  wtp_seq <- seq(wtp_range[1], wtp_range[2], by = wtp_step)

  # Calculate frontier
  frontier_data <- calculate_incremental_ceac_frontier(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    wtp = wtp_seq,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Identify strategy transitions
  frontier_data <- frontier_data %>%
    group_by(group) %>%
    mutate(
      strategy_change = optimal_strategy != lag(optimal_strategy, default = first(optimal_strategy))
    ) %>%
    ungroup()

  # Create base plot
  if (show_probability) {
    # Show probability as alpha or size
    p <- ggplot(frontier_data, aes(x = wtp, y = optimal_strategy)) +
      geom_line(aes(alpha = probability, group = group), linewidth = 2) +
      geom_point(aes(alpha = probability), size = 1) +
      scale_alpha_continuous(range = c(0.3, 1), name = "Probability")
  } else {
    # Simple step plot
    p <- ggplot(frontier_data, aes(x = wtp, y = optimal_strategy)) +
      geom_step(aes(group = group), linewidth = 1.5, color = "darkblue") +
      geom_point(data = frontier_data %>% filter(strategy_change),
                size = 3, color = "red")
  }

  p <- p +
    scale_x_continuous(labels = scales::comma) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab
    )

  # Add vertical lines at transitions
  transition_points <- frontier_data %>%
    filter(strategy_change) %>%
    pull(wtp)

  if (length(transition_points) > 0) {
    p <- p +
      geom_vline(xintercept = transition_points,
                linetype = "dashed",
                color = "gray50",
                alpha = 0.5)
  }

  # Facet if multiple groups
  n_groups <- length(unique(frontier_data$group))
  if (n_groups > 1) {
    p <- p + facet_wrap(~ group, scales = "fixed")
  }

  # Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}


#' Plot PSA Scatter Plot
#'
#' Creates a scatter plot showing the distribution of costs and outcomes
#' from PSA simulations. Each point represents one simulation, colored by strategy.
#'
#' @inheritParams incremental_ceac_plot
#' @param show_means Logical. Show mean values as larger points? Default is TRUE
#' @param alpha Transparency of scatter points (0-1). Default is 0.3
#'
#' @return A ggplot2 object
#'
#' @details
#' The scatter plot shows each PSA simulation as a point, colored by strategy.
#' Mean values for each strategy are shown as larger, highlighted points to
#' indicate the central tendency of the distribution.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Basic scatter plot
#' psa_scatter_plot(psa_results, "total_qalys", "total_cost")
#'
#' # Without mean points
#' psa_scatter_plot(psa_results, "total_qalys", "total_cost", show_means = FALSE)
#' }
#'
#' @export
psa_scatter_plot <- function(results,
                             outcome_summary,
                             cost_summary,
                             group = "aggregated",
                             strategies = NULL,
                             discounted = FALSE,
                             show_means = TRUE,
                             alpha = 0.3,
                             title = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             legend_position = "right") {

  # Get PSA simulation data
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Set default labels if not provided
  if (is.null(xlab)) {
    xlab <- if (!is.null(results$metadata$summaries)) {
      summary_meta <- results$metadata$summaries %>%
        filter(name == outcome_summary)
      if (nrow(summary_meta) > 0 && !is.na(summary_meta$display_name[1])) {
        summary_meta$display_name[1]
      } else {
        outcome_summary
      }
    } else {
      outcome_summary
    }
  }

  if (is.null(ylab)) {
    ylab <- if (!is.null(results$metadata$summaries)) {
      summary_meta <- results$metadata$summaries %>%
        filter(name == cost_summary)
      if (nrow(summary_meta) > 0 && !is.na(summary_meta$display_name[1])) {
        summary_meta$display_name[1]
      } else {
        cost_summary
      }
    } else {
      cost_summary
    }
  }

  # Calculate axis breaks and limits to include 0,0 and extend beyond data
  breaks_fn <- scales::pretty_breaks(n = 5)
  x_range <- range(c(0, psa_data$outcome))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, psa_data$cost))
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # Create base scatter plot
  # Note: scatter points use color but are hidden from legend (show.legend = FALSE)
  p <- ggplot(psa_data, aes(x = outcome, y = cost, color = strategy)) +
    geom_point(alpha = alpha, size = 1, show.legend = FALSE) +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits,
      labels = scales::comma
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = scales::number
    ) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      fill = "Strategy"
    ) +
    guides(color = "none")

  # Add mean points with enhanced visibility
  if (show_means) {
    mean_data <- psa_data %>%
      group_by(strategy, group) %>%
      summarize(
        mean_outcome = mean(outcome, na.rm = TRUE),
        mean_cost = mean(cost, na.rm = TRUE),
        .groups = "drop"
      )

    p <- p +
      geom_point(
        data = mean_data,
        aes(x = mean_outcome, y = mean_cost, fill = strategy),
        size = 3,
        shape = 21,
        color = "black",
        stroke = 0.5
      )
  }

  # Facet if multiple groups
  n_groups <- length(unique(psa_data$group))
  if (n_groups > 1) {
    p <- p + facet_wrap(~ group, scales = "free")
  }

  # Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}


#' Plot Pairwise CEAC
#'
#' Creates a line plot showing the probability from pairwise comparisons at
#' different WTP thresholds.
#'
#' @inheritParams incremental_ceac_plot
#' @param intervention Reference strategy for comparison (intervention perspective: A vs. B, A vs. C).
#'   Mutually exclusive with comparator.
#' @param comparator Reference strategy for comparison (comparator perspective: B vs. A, C vs. A).
#'   Mutually exclusive with intervention.
#'
#' @return A ggplot2 object
#'
#' @details
#' Pairwise comparisons calculate P(intervention > comparator) for each pair.
#' The plot displays these probabilities across different WTP thresholds.
#'
#' \strong{Intervention mode}: Shows P(intervention > each other strategy)
#'
#' \strong{Comparator mode}: Shows P(each other strategy > comparator)
#'
#' A probability > 0.5 indicates higher likelihood of being cost-effective
#' in the comparison.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Comparator perspective: Compare strategies to control
#' pairwise_ceac_plot(psa_results, "total_qalys", "total_cost",
#'                    comparator = "control")
#'
#' # Intervention perspective: Compare new treatment to others
#' pairwise_ceac_plot(psa_results, "total_qalys", "total_cost",
#'                    intervention = "new_treatment")
#' }
#'
#' @export
pairwise_ceac_plot <- function(results,
                              outcome_summary,
                              cost_summary,
                              intervention = NULL,
                              comparator = NULL,
                              wtp_range = c(0, 100000),
                              wtp_step = 5000,
                              group = "aggregated",
                              strategies = NULL,
                              discounted = FALSE,
                              title = NULL,
                              xlab = "Willingness to Pay",
                              ylab = NULL,
                              legend_position = "bottom") {

  # Generate WTP sequence
  wtp_seq <- seq(wtp_range[1], wtp_range[2], by = wtp_step)

  # Calculate pairwise CEAC
  pairwise_data <- calculate_pairwise_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    intervention = intervention,
    comparator = comparator,
    wtp = wtp_seq,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Set y-axis label if not provided
  if (is.null(ylab)) {
    reference_name <- unique(pairwise_data$comparator)[1]
    if (!is.null(intervention)) {
      ylab <- paste0("Probability of Cost-Effectiveness for ", reference_name)
    } else {
      ylab <- paste0("Probability of Cost-Effectiveness vs. ", reference_name)
    }
  }

  # Create plot
  p <- ggplot(pairwise_data, aes(x = wtp, y = probability, color = strategy)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    scale_x_continuous(labels = scales::comma) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      color = if (!is.null(intervention)) "Comparator" else "Strategy"
    )

  # Facet if multiple groups
  n_groups <- length(unique(pairwise_data$group))
  if (n_groups > 1) {
    p <- p + facet_wrap(~ group, scales = "fixed")
  }

  # Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}


#' Plot PSA Parameter Scatter Matrix
#'
#' Creates a scatterplot matrix visualizing the sampled input parameter values
#' from probabilistic sensitivity analysis. Handles strategy/group-specific
#' variables with appropriate labeling.
#'
#' @param results A heRomod2 PSA results object (from run_psa)
#' @param variables Character vector of variable names to visualize (required)
#' @param group Group selection with three modes:
#'   \itemize{
#'     \item NULL or "all": include all groups
#'     \item Single value: filter to that group
#'     \item Vector same length as variables: specify group for each variable
#'   }
#' @param strategies Strategy selection with three modes:
#'   \itemize{
#'     \item NULL: include all strategies
#'     \item Single value: filter to that strategy
#'     \item Vector same length as variables: specify strategy for each variable
#'   }
#' @param alpha Transparency level for scatter points (default: 0.3)
#' @param title Plot title (optional)
#' @param label_wrap_width Maximum width (in characters) before wrapping axis labels.
#'   Set to NULL to disable wrapping. Default is 20 characters. This helps prevent
#'   long variable names from overlapping in the plot.
#' @param upper List specifying upper triangle display. Default shows correlations.
#'   See GGally::ggpairs documentation for options.
#' @param lower List specifying lower triangle display. Default shows scatter points.
#' @param diag List specifying diagonal display. Default shows density plots.
#'
#' @return A GGally ggpairs object
#'
#' @details
#' This function creates a scatterplot matrix (pairs plot) showing relationships
#' between sampled input parameters from PSA. For strategy/group-specific variables,
#' labels use parenthetical notation:
#'
#' - Generic variable: "Variable Name"
#' - Strategy-specific: "Variable Name (Strategy A)"
#' - Group-specific: "Variable Name (Group 1)"
#' - Both: "Variable Name (Strategy A, Group 1)"
#'
#' Variable names are automatically wrapped to prevent overlapping. The
#' `label_wrap_width` parameter controls the maximum number of characters
#' before wrapping. Underscores, parentheses, and commas in variable names
#' are replaced with spaces to allow proper line breaking at word boundaries.
#'
#' The matrix shows:
#' - Lower triangle: Scatter plots of parameter pairs
#' - Diagonal: Density plots of individual parameters
#' - Upper triangle: Correlation coefficients
#'
#' This is useful for:
#' - Checking correlations between sampled parameters
#' - Verifying multivariate sampling behavior
#' - Understanding parameter distributions
#' - Quality checking PSA inputs
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Visualize selected parameters (all strategies/groups)
#' psa_parameter_scatter_matrix(
#'   psa_results,
#'   variables = c("p_transition", "utility_well", "utility_sick", "cost_treatment")
#' )
#'
#' # Focus on specific strategy
#' psa_parameter_scatter_matrix(
#'   psa_results,
#'   variables = c("p_transition", "utility_well"),
#'   strategies = "treatment_a"
#' )
#'
#' # Visualize specific (variable, strategy, group) combinations
#' psa_parameter_scatter_matrix(
#'   psa_results,
#'   variables = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#'   strategies = c("treatment_a", "treatment_a", "treatment_a"),
#'   group = c("group_1", "group_1", "group_1")
#' )
#'
#' # Control label wrapping for long variable names
#' psa_parameter_scatter_matrix(
#'   psa_results,
#'   variables = c("very_long_parameter_name", "another_long_parameter"),
#'   label_wrap_width = 15  # Wrap at 15 characters
#' )
#'
#' # Disable wrapping for short variable names
#' psa_parameter_scatter_matrix(
#'   psa_results,
#'   variables = c("param1", "param2", "param3"),
#'   label_wrap_width = NULL  # No wrapping
#' )
#' }
#'
#' @export
psa_parameter_scatter_matrix <- function(results,
                                         variables,
                                         group = NULL,
                                         strategies = NULL,
                                         alpha = 0.3,
                                         title = NULL,
                                         label_wrap_width = 20,
                                         upper = list(continuous = "cor"),
                                         lower = list(continuous = "points"),
                                         diag = list(continuous = "densityDiag")) {

  # Check that GGally is available
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required for psa_parameter_scatter_matrix(). Please install it with: install.packages('GGally')")
  }

  # Validate inputs
  if (missing(variables) || is.null(variables) || length(variables) == 0) {
    stop("'variables' parameter is required and must specify which variables to visualize")
  }

  # Get sampled parameters
  param_data <- get_sampled_parameters(
    results = results,
    variables = variables,
    group = group,
    strategies = strategies
  )

  # Remove simulation column for plotting
  plot_data <- param_data %>%
    select(-simulation)

  # Check we have at least 2 variables
  if (ncol(plot_data) < 2) {
    stop("Need at least 2 variables to create a scatter matrix. Found ", ncol(plot_data))
  }

  # Set default title if not provided
  if (is.null(title)) {
    n_sim <- nrow(plot_data)
    title <- sprintf("PSA Parameter Scatter Matrix (n=%d simulations)", n_sim)
  }

  # Prepare wrapped column labels if label_wrap_width is specified
  if (!is.null(label_wrap_width)) {
    # Add spaces around punctuation for proper wrapping, but KEEP the punctuation
    # label_wrap_gen() needs spaces to know where to break lines
    wrapped_labels <- colnames(plot_data) %>%
      gsub("_", " ", .) %>%           # Replace underscores with spaces
      gsub("\\(", " (", .) %>%        # Add space before opening parenthesis
      gsub(",", ", ", .) %>%          # Ensure space after comma
      gsub("  +", " ", .)             # Clean up any multiple spaces

    # Create scatter matrix with label wrapping
    p <- GGally::ggpairs(
      plot_data,
      columnLabels = wrapped_labels,
      labeller = ggplot2::label_wrap_gen(width = label_wrap_width),
      upper = upper,
      lower = lower,
      diag = diag,
      title = title,
      progress = FALSE
    )
  } else {
    # Create scatter matrix without label wrapping
    p <- GGally::ggpairs(
      plot_data,
      upper = upper,
      lower = lower,
      diag = diag,
      title = title,
      progress = FALSE
    )
  }

  # Adjust alpha for scatter plots if points are in lower triangle
  if (!is.null(lower$continuous) && lower$continuous == "points") {
    p <- p + ggplot2::theme_bw()

    # Apply alpha to scatter plots in lower triangle
    for (i in 2:ncol(plot_data)) {
      for (j in 1:(i-1)) {
        p[i, j] <- p[i, j] +
          ggplot2::geom_point(alpha = alpha, size = 0.5)
      }
    }
  }

  return(p)
}


#' Plot Expected Value of Perfect Information (EVPI)
#'
#' Creates a line plot showing how EVPI changes across different willingness-to-pay
#' thresholds. EVPI represents the expected value of eliminating all parameter
#' uncertainty.
#'
#' @param results A heRomod2 PSA results object, or pre-calculated EVPI data
#'   from calculate_evpi()
#' @param outcome_summary Name of the outcome summary (required if results is
#'   a model object)
#' @param cost_summary Name of the cost summary (required if results is a
#'   model object)
#' @param wtp_range Numeric vector with min and max WTP values for x-axis.
#'   Default is c(0, 100000)
#' @param wtp_step Step size for WTP thresholds. Default is 5000
#' @param group Group selection: "aggregated" (default) uses aggregated results,
#'   specific group name filters to that group, NULL or "all" includes all groups
#'   (triggers faceting by group)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param title Plot title. If NULL, auto-generated
#' @param xlab X-axis label. Default is "Willingness to Pay"
#' @param ylab Y-axis label. Default is "Expected Value of Perfect Information"
#' @param legend_position Legend position: "bottom" (default), "right", "top",
#'   "left", or "none"
#'
#' @return A ggplot2 object
#'
#' @details
#' The EVPI plot shows how the expected value of eliminating all parameter
#' uncertainty changes with the willingness-to-pay threshold. Higher EVPI values
#' indicate greater decision uncertainty and potentially higher value of
#' conducting additional research.
#'
#' EVPI = 0 indicates that the same strategy is optimal in all PSA simulations,
#' meaning parameter uncertainty does not affect the optimal decision at that
#' WTP threshold.
#'
#' When multiple groups are present, the plot is automatically faceted by group
#' using \code{facet_wrap(~ group, scales = "fixed")}.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Basic EVPI plot
#' evpi_plot(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP range
#' evpi_plot(psa_results, "total_qalys", "total_cost",
#'           wtp_range = c(0, 200000), wtp_step = 10000)
#'
#' # With custom title
#' evpi_plot(psa_results, "total_qalys", "total_cost",
#'           title = "EVPI Analysis for Treatment Decision")
#' }
#'
#' @export
evpi_plot <- function(results,
                      outcome_summary = NULL,
                      cost_summary = NULL,
                      wtp_range = c(0, 100000),
                      wtp_step = 5000,
                      group = "aggregated",
                      strategies = NULL,
                      discounted = FALSE,
                      title = NULL,
                      xlab = "Willingness to Pay",
                      ylab = "Expected Value of Perfect Information",
                      legend_position = "bottom") {

  # Check if results is pre-calculated EVPI data or model results
  if ("wtp" %in% names(results) && "evpi" %in% names(results)) {
    # Pre-calculated EVPI data
    evpi_data <- results
  } else {
    # Model results - calculate EVPI
    if (is.null(outcome_summary) || is.null(cost_summary)) {
      stop("outcome_summary and cost_summary required when results is a model object")
    }

    # Generate WTP sequence
    wtp_seq <- seq(wtp_range[1], wtp_range[2], by = wtp_step)

    evpi_data <- calculate_evpi(
      results = results,
      outcome_summary = outcome_summary,
      cost_summary = cost_summary,
      wtp = wtp_seq,
      group = group,
      strategies = strategies,
      discounted = discounted
    )
  }

  # Calculate axis breaks and limits to include 0 and extend beyond data
  breaks_fn <- scales::pretty_breaks(n = 5)
  x_range <- range(c(0, evpi_data$wtp))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, evpi_data$evpi))
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # Get number of groups for styling decisions
  n_groups <- length(unique(evpi_data$group))

  # Create base plot
  # For multiple groups, color by group; otherwise use default styling
  if (n_groups > 1) {
    p <- ggplot(evpi_data, aes(x = wtp, y = evpi, color = group, group = group)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(color = "Group")
  } else {
    p <- ggplot(evpi_data, aes(x = wtp, y = evpi)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2)
  }

  p <- p +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits,
      labels = scales::comma
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = scales::comma
    ) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab
    )

  # Facet if multiple groups
  if (n_groups > 1) {
    p <- p + facet_wrap(~ group, scales = "fixed")
  }

  # Set legend position (though EVPI plot doesn't typically have a legend)
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}


#' Plot Pairwise PSA Scatter Plot on Incremental Cost-Effectiveness Plane
#'
#' Creates scatter plots showing the distribution of incremental costs and outcomes
#' from PSA simulations for pairwise comparisons. Each point represents one simulation's
#' incremental values relative to a comparator (at origin). Points can be colored by
#' cost-effectiveness status when a WTP threshold is provided.
#'
#' @inheritParams pairwise_ceac_plot
#' @param wtp Numeric WTP threshold for cost-effectiveness evaluation. When provided,
#'   points are colored by cost-effectiveness status and a WTP threshold line is drawn.
#'   Default is NULL (no coloring or threshold line)
#' @param alpha Transparency of scatter points (0-1). Default is 0.3
#' @param xlab X-axis label. If NULL, auto-generated from outcome_summary
#' @param ylab Y-axis label. If NULL, auto-generated from cost_summary
#'
#' @return A ggplot2 object
#'
#' @details
#' This function creates incremental cost-effectiveness planes showing PSA simulation
#' results for pairwise comparisons. The comparator is always at the origin (0, 0),
#' and each point represents one simulation's incremental cost and outcome.
#'
#' Both intervention and comparator modes display each comparison in a separate panel.
#'
#' \strong{Intervention mode} (intervention = 'A'): Each panel shows intervention A
#' compared to one other strategy
#' - X-axis: Incremental outcome (A - comparator)
#' - Y-axis: Incremental cost (A - comparator)
#' - Panel labels: "A vs. Strategy B", "A vs. Strategy C", etc.
#'
#' \strong{Comparator mode} (comparator = 'B'): Each panel shows one strategy compared to B
#' - X-axis: Incremental outcome (strategy - B)
#' - Y-axis: Incremental cost (strategy - B)
#' - Panel labels: "Strategy A vs. B", "Strategy C vs. B", etc.
#'
#' \strong{WTP Threshold}: When wtp is provided:
#' - Points are colored by cost-effectiveness: green if NMB > 0, red if NMB <= 0
#' - NMB (Net Monetary Benefit) = incremental_outcome * wtp - incremental_cost
#' - A dashed line with slope = wtp is drawn through the origin
#' - Legend shows cost-effectiveness status
#'
#' \strong{Faceting}:
#' - Multiple groups: facet_grid with groups on rows, comparisons on columns
#' - Single group: facet_wrap by comparison
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Comparator perspective: Each strategy vs control in separate panels
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           comparator = "control")
#'
#' # With WTP threshold showing cost-effectiveness
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           comparator = "control",
#'                           wtp = 100000)
#'
#' # Intervention perspective: New treatment vs others
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           intervention = "new_treatment",
#'                           wtp = 50000)
#'
#' # All groups with intervention mode (facet_grid)
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           intervention = "new_treatment",
#'                           group = NULL,
#'                           wtp = 100000)
#' }
#'
#' @export
pairwise_psa_scatter_plot <- function(results,
                                      outcome_summary,
                                      cost_summary,
                                      intervention = NULL,
                                      comparator = NULL,
                                      wtp = NULL,
                                      group = "aggregated",
                                      strategies = NULL,
                                      discounted = FALSE,
                                      alpha = 0.3,
                                      title = NULL,
                                      xlab = NULL,
                                      ylab = NULL,
                                      legend_position = "right") {

  # 1. Parameter Validation
  if (is.null(intervention) && is.null(comparator)) {
    stop("One of 'intervention' or 'comparator' must be provided")
  }
  if (!is.null(intervention) && !is.null(comparator)) {
    stop("Only one of 'intervention' or 'comparator' should be provided, not both")
  }

  # Determine which strategy is fixed and the comparison mode
  fixed_strategy <- if (!is.null(comparator)) comparator else intervention
  use_intervention <- !is.null(intervention)

  # 2. Data Preparation
  # Get PSA simulation data
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    group = group,
    strategies = strategies,
    discounted = discounted
  )

  # Check fixed strategy exists
  all_strategies <- unique(psa_data$strategy)

  # Try to find fixed strategy by matching against display names
  if (!fixed_strategy %in% all_strategies) {
    # If not found, try mapping technical name to display name
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      # Try mapping: technical name -> display name
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == fixed_strategy]

      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        fixed_strategy <- matched_display[1]
      } else {
        stop(sprintf("Fixed strategy '%s' not found in results", fixed_strategy))
      }
    } else {
      stop(sprintf("Fixed strategy '%s' not found in results", fixed_strategy))
    }
  }

  # Get unique groups and other strategies
  groups <- unique(psa_data$group)
  all_strategies <- unique(psa_data$strategy)
  other_strategies <- setdiff(all_strategies, fixed_strategy)

  if (length(other_strategies) == 0) {
    stop("No other strategies to compare")
  }

  # Create grid of all (simulation, group, other_strategy) combinations
  comparison_grid <- expand.grid(
    simulation = unique(psa_data$simulation),
    group = groups,
    other_strategy = other_strategies,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  # Join with fixed strategy data
  fixed_data <- psa_data %>%
    filter(strategy == fixed_strategy) %>%
    select(simulation, group, cost_fixed = cost, outcome_fixed = outcome)

  comparison_data <- comparison_grid %>%
    left_join(fixed_data, by = c("simulation", "group"))

  # Join with other strategies data
  other_data <- psa_data %>%
    filter(strategy != fixed_strategy) %>%
    select(simulation, group, strategy, cost_other = cost, outcome_other = outcome)

  comparison_data <- comparison_data %>%
    left_join(other_data, by = c("simulation", "group", "other_strategy" = "strategy"))

  # Calculate incremental values based on mode
  if (use_intervention) {
    # Intervention mode: intervention - comparator (fixed - other)
    incremental_data <- comparison_data %>%
      mutate(
        doutcome = outcome_fixed - outcome_other,
        dcost = cost_fixed - cost_other,
        strategy = other_strategy
      )
  } else {
    # Comparator mode: intervention - comparator (other - fixed)
    incremental_data <- comparison_data %>%
      mutate(
        doutcome = outcome_other - outcome_fixed,
        dcost = cost_other - cost_fixed,
        strategy = other_strategy
      )
  }

  incremental_data <- incremental_data %>%
    select(simulation, group, strategy, doutcome, dcost)

  # Strategy and group names are already display names from get_psa_simulations
  # Store fixed strategy for labeling
  fixed_strategy_display <- fixed_strategy

  # 3. Create comparison labels for both modes
  if (use_intervention) {
    # Intervention mode: "Intervention vs. Strategy"
    incremental_data <- incremental_data %>%
      mutate(comparison = paste(fixed_strategy_display, "vs.", strategy))
  } else {
    # Comparator mode: "Strategy vs. Comparator"
    incremental_data <- incremental_data %>%
      mutate(comparison = paste(strategy, "vs.", fixed_strategy_display))
  }

  # 4. Calculate cost-effectiveness if wtp provided
  if (!is.null(wtp)) {
    incremental_data <- incremental_data %>%
      mutate(
        nmb = doutcome * wtp - dcost,
        cost_effective = factor(
          ifelse(nmb > 0, "Cost-Effective", "Not Cost-Effective"),
          levels = c("Cost-Effective", "Not Cost-Effective")
        )
      )
  }

  # 5. Axis Label Generation
  if (is.null(xlab)) {
    outcome_label <- if (!is.null(results$metadata$summaries)) {
      summary_meta <- results$metadata$summaries %>%
        filter(name == outcome_summary)
      if (nrow(summary_meta) > 0 && !is.na(summary_meta$display_name[1])) {
        summary_meta$display_name[1]
      } else {
        outcome_summary
      }
    } else {
      outcome_summary
    }
    xlab <- paste0("\u0394 ", outcome_label)
  }

  if (is.null(ylab)) {
    cost_label <- if (!is.null(results$metadata$summaries)) {
      summary_meta <- results$metadata$summaries %>%
        filter(name == cost_summary)
      if (nrow(summary_meta) > 0 && !is.na(summary_meta$display_name[1])) {
        summary_meta$display_name[1]
      } else {
        cost_summary
      }
    } else {
      cost_summary
    }
    ylab <- paste0("\u0394 ", cost_label)
  }

  # 6. Axis Limits and Breaks
  breaks_fn <- scales::pretty_breaks(n = 5)
  x_range <- range(c(0, incremental_data$doutcome), na.rm = TRUE)
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, incremental_data$dcost), na.rm = TRUE)
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # 7. Unified Plot Construction
  if (!is.null(wtp)) {
    # With WTP: color by cost-effectiveness
    p <- ggplot(incremental_data, aes(x = doutcome, y = dcost, color = cost_effective))
  } else {
    # Without WTP: single color
    p <- ggplot(incremental_data, aes(x = doutcome, y = dcost))
  }

  p <- p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")

  # Add WTP threshold line if provided
  if (!is.null(wtp)) {
    p <- p +
      geom_abline(slope = wtp, intercept = 0, linetype = "dashed", color = "black", alpha = 0.5)
  }

  # Add points
  if (!is.null(wtp)) {
    p <- p +
      geom_point(alpha = alpha, size = 1) +
      scale_color_manual(
        values = c("Cost-Effective" = "#00BA38", "Not Cost-Effective" = "#F8766D"),
        name = paste0("Cost-Effective\nat \u03BB = ", scales::dollar(wtp, scale = 1/1000, suffix = "K"))
      )
  } else {
    p <- p +
      geom_point(alpha = alpha, size = 1, color = "gray40")
  }

  # Add scales and theme
  p <- p +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, labels = scales::number) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits, labels = scales::comma) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab
    )

  # 8. Unified Faceting Logic
  n_groups <- length(unique(incremental_data$group))

  if (n_groups > 1) {
    # Multiple groups: facet_grid with groups on rows, comparisons on columns
    p <- p + facet_grid(group ~ comparison, scales = "free")
  } else {
    # Single group: facet_wrap by comparison
    p <- p + facet_wrap(~ comparison, scales = "free")
  }

  # 9. Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}