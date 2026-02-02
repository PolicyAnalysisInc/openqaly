#' Plot Incremental Cost-Effectiveness Acceptability Curves
#'
#' Creates a line plot showing the probability that each strategy is the most
#' cost-effective at different willingness-to-pay thresholds in a multi-way
#' (incremental) comparison.
#'
#' @param results A openqaly PSA results object, or pre-calculated CEAC data
#'   from calculate_incremental_ceac()
#' @param outcome_summary Name of the outcome summary (required if results is
#'   a model object)
#' @param cost_summary Name of the cost summary (required if results is a
#'   model object)
#' @param wtp_range Numeric vector with min and max WTP values for x-axis.
#'   Default is c(0, 100000)
#' @param wtp_step Step size for WTP thresholds. Default is 5000
#' @param groups Group selection: "overall" (default) uses overall/aggregated results,
#'   "all" includes overall + all groups, "all_groups" includes all groups without overall,
#'   specific group name(s) filter to those groups (triggers faceting by group)
#' @param strategies Character vector of strategy names to include (NULL for all)
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
#' CEAC calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' When multiple groups are present, the plot is automatically faceted by group
#' using \code{facet_wrap(~ group, scales = "fixed")}.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Basic incremental CEAC plot
#' incremental_ceac_plot(psa_results, "total_qalys", "total_cost")
#'
#' # Custom WTP range
#' incremental_ceac_plot(psa_results, "total_qalys", "total_cost",
#'                       wtp_range = c(0, 200000), wtp_step = 10000)
#' }
#'
#' @export
incremental_ceac_plot <- function(results,
                                  outcome_summary = NULL,
                                  cost_summary = NULL,
                                  wtp_range = c(0, 100000),
                                  wtp_step = 5000,
                                  groups = "overall",
                                  strategies = NULL,
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

    # Always use discounted values (cost-effectiveness measure)
    ceac_data <- calculate_incremental_ceac(
      results = results,
      outcome_summary = outcome_summary,
      cost_summary = cost_summary,
      wtp = wtp_seq,
      groups = groups,
      strategies = strategies
    )
  }

  # Filter strategies if specified
  if (!is.null(strategies)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies, results$metadata)
    ceac_data <- ceac_data %>%
      filter(.data$strategy %in% strategies)
  }

  # Apply consistent group ordering (Overall first, then model order)
  metadata <- if ("metadata" %in% names(results) && !is.null(results$metadata)) results$metadata else NULL
  group_levels <- get_group_order(unique(ceac_data$group), metadata)
  ceac_data <- ceac_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Create base plot
  p <- ggplot(ceac_data, aes(x = .data$wtp, y = .data$probability, color = .data$strategy)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    scale_x_continuous(labels = comma) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = ylab,
      color = "Strategy"
    )

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
#' PSA scatter plots always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
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
                             groups = "overall",
                             strategies = NULL,
                             show_means = TRUE,
                             alpha = 0.3,
                             title = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             legend_position = "right") {

  # Get PSA simulation data (always uses discounted values for CE-related analysis)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(psa_data$group), results$metadata)
  psa_data <- psa_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Set default labels if not provided
  if (is.null(xlab)) {
    xlab <- if (!is.null(results$metadata$summaries)) {
      summary_meta <- results$metadata$summaries %>%
        filter(.data$name == outcome_summary)
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
        filter(.data$name == cost_summary)
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
  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, psa_data$outcome))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, psa_data$cost))
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # Create base scatter plot
  # Note: scatter points use color but are hidden from legend (show.legend = FALSE)
  p <- ggplot(psa_data, aes(x = .data$outcome, y = .data$cost, color = .data$strategy)) +
    geom_point(alpha = alpha, size = 1, show.legend = FALSE) +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits,
      labels = comma
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = number
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
      group_by(.data$strategy, .data$group) %>%
      summarize(
        mean_outcome = mean(.data$outcome, na.rm = TRUE),
        mean_cost = mean(.data$cost, na.rm = TRUE),
        .groups = "drop"
      )

    p <- p +
      geom_point(
        data = mean_data,
        aes(x = .data$mean_outcome, y = .data$mean_cost, fill = .data$strategy),
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
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary
#' @param cost_summary Name of the cost summary
#' @param interventions Reference strategies for comparison (intervention perspective: A vs. B, A vs. C).
#' @param comparators Reference strategies for comparison (comparator perspective: B vs. A, C vs. A).
#' @param wtp_range Numeric vector with min and max WTP values for x-axis.
#'   Default is c(0, 100000)
#' @param wtp_step Step size for WTP thresholds. Default is 5000
#' @param groups Group selection: "overall" (default) uses overall/aggregated results,
#'   "all" includes overall + all groups, "all_groups" includes all groups without overall,
#'   specific group name(s) filter to those groups (triggers faceting by group)
#' @param title Plot title. If NULL, auto-generated
#' @param xlab X-axis label. Default is "Willingness to Pay"
#' @param ylab Y-axis label. Default is "Probability of Cost-Effectiveness"
#' @param legend_position Legend position: "bottom" (default), "right", "top",
#'   "left", or "none"
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
#' CEAC calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
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
                              interventions = NULL,
                              comparators = NULL,
                              wtp_range = c(0, 100000),
                              wtp_step = 5000,
                              groups = "overall",
                              title = NULL,
                              xlab = "Willingness to Pay",
                              ylab = "Probability of Cost-Effectiveness",
                              legend_position = "bottom") {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Generate WTP sequence
  wtp_seq <- seq(wtp_range[1], wtp_range[2], by = wtp_step)

  # Calculate pairwise CEAC (always use discounted values - cost-effectiveness measure)
  pairwise_data <- calculate_pairwise_ceac(
    results = results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    interventions = interventions,
    comparators = comparators,
    wtp = wtp_seq,
    groups = groups
  )

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(pairwise_data$group), results$metadata)
  pairwise_data <- pairwise_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Get counts for adaptive faceting (matching pairwise_ce_plot pattern)
  n_groups <- length(unique(pairwise_data$group))
  n_comparisons <- length(unique(pairwise_data$comparison))

  # Determine plot mode based on number of comparisons
  if (n_comparisons <= 5) {
    # Few comparisons: color by comparison, facet by group only
    p <- ggplot(pairwise_data, aes(x = .data$wtp, y = .data$probability, color = .data$comparison)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
      scale_y_continuous(limits = c(0, 1), labels = percent) +
      scale_x_continuous(labels = comma) +
      theme_bw() +
      labs(
        title = title,
        x = xlab,
        y = ylab,
        color = "Comparison"
      )

    # Facet by group if multiple groups
    if (n_groups > 1) {
      p <- p + facet_wrap(~ group, scales = "fixed")
    }

  } else {
    # Many comparisons: facet by comparison (and group if multiple)
    p <- ggplot(pairwise_data, aes(x = .data$wtp, y = .data$probability)) +
      geom_line(linewidth = 1, color = "blue") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
      scale_y_continuous(limits = c(0, 1), labels = percent) +
      scale_x_continuous(labels = comma) +
      theme_bw() +
      labs(
        title = title,
        x = xlab,
        y = ylab
      )

    # Add faceting
    if (n_groups > 1) {
      # Multi-group: facet_grid with group on rows, comparison on columns
      p <- p + facet_grid(group ~ comparison, scales = "fixed")
    } else {
      # Single group: facet_wrap by comparison
      p <- p + facet_wrap(~ comparison, scales = "fixed")
    }
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
#' @param results A openqaly PSA results object (from run_psa)
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
#' model <- read_model(system.file("models/example", package = "openqaly"))
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
    select(-"simulation")

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
      labeller = label_wrap_gen(width = label_wrap_width),
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
    p <- p + theme_bw()

    # Apply alpha to scatter plots in lower triangle
    for (i in 2:ncol(plot_data)) {
      for (j in 1:(i-1)) {
        p[i, j] <- p[i, j] +
          geom_point(alpha = alpha, size = 0.5)
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
#' @param results A openqaly PSA results object, or pre-calculated EVPI data
#'   from calculate_evpi()
#' @param outcome_summary Name of the outcome summary (required if results is
#'   a model object)
#' @param cost_summary Name of the cost summary (required if results is a
#'   model object)
#' @param wtp_range Numeric vector with min and max WTP values for x-axis.
#'   Default is c(0, 100000)
#' @param wtp_step Step size for WTP thresholds. Default is 5000
#' @param groups Group selection: "overall" (default) uses overall/aggregated results,
#'   "all" includes overall + all groups, "all_groups" includes all groups without overall,
#'   specific group name(s) filter to those groups (triggers faceting by group)
#' @param strategies Character vector of strategy names to include (NULL for all)
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
#' EVPI calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' When multiple groups are present, the plot is automatically faceted by group
#' using \code{facet_wrap(~ group, scales = "fixed")}.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
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
                      groups = "overall",
                      strategies = NULL,
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

    # Always use discounted values (cost-effectiveness measure)
    evpi_data <- calculate_evpi(
      results = results,
      outcome_summary = outcome_summary,
      cost_summary = cost_summary,
      wtp = wtp_seq,
      groups = groups,
      strategies = strategies
    )
  }

  # Apply consistent group ordering (Overall first, then model order)
  metadata <- if ("metadata" %in% names(results) && !is.null(results$metadata)) results$metadata else NULL
  group_levels <- get_group_order(unique(evpi_data$group), metadata)
  evpi_data <- evpi_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Calculate axis breaks and limits to include 0 and extend beyond data
  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, evpi_data$wtp))
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, evpi_data$evpi))
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # Get number of groups for faceting decisions
  n_groups <- length(unique(evpi_data$group))

  # Create base plot - use consistent styling regardless of group count
  # When faceted, each panel shows one line so coloring by group is redundant
  p <- ggplot(evpi_data, aes(x = .data$wtp, y = .data$evpi, group = .data$group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2)

  p <- p +
    scale_y_continuous(
      breaks = y_breaks,
      limits = y_limits,
      labels = comma
    ) +
    scale_x_continuous(
      breaks = x_breaks,
      limits = x_limits,
      labels = comma
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
#' \strong{Comparison Modes:}
#'
#' When interventions and/or comparators are specified:
#' \itemize{
#'   \item Both as single values: shows one comparison (intervention - comparator)
#'   \item One as vector, other NULL: shows each vs. all others
#'   \item Both as vectors (N x M mode): shows all pairwise comparisons (excluding self)
#' }
#'
#' \strong{Interventions mode} (interventions = c('A', 'B')): Each panel shows an
#' intervention compared to other strategies
#' - X-axis: Incremental outcome (intervention - comparator)
#' - Y-axis: Incremental cost (intervention - comparator)
#' - Panel labels: "A vs. Strategy C", "B vs. Strategy C", etc.
#'
#' \strong{Comparators mode} (comparators = c('C')): Each panel shows one strategy
#' compared to the comparator
#' - X-axis: Incremental outcome (strategy - comparator)
#' - Y-axis: Incremental cost (strategy - comparator)
#' - Panel labels: "Strategy A vs. C", "Strategy B vs. C", etc.
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
#' PSA scatter plots always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Comparator perspective: Each strategy vs control in separate panels
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           comparators = "control")
#'
#' # With WTP threshold showing cost-effectiveness
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           comparators = "control",
#'                           wtp = 100000)
#'
#' # Intervention perspective: New treatment vs others
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           interventions = "new_treatment",
#'                           wtp = 50000)
#'
#' # N x M explicit comparisons
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           interventions = c("treatment_a", "treatment_b"),
#'                           comparators = "control",
#'                           wtp = 100000)
#'
#' # All groups with multiple comparisons (facet_grid)
#' pairwise_psa_scatter_plot(psa_results, "total_qalys", "total_cost",
#'                           interventions = "new_treatment",
#'                           groups = "all",
#'                           wtp = 100000)
#' }
#'
#' @export
pairwise_psa_scatter_plot <- function(results,
                                      outcome_summary,
                                      cost_summary,
                                      interventions = NULL,
                                      comparators = NULL,
                                      wtp = NULL,
                                      groups = "overall",
                                      strategies = NULL,
                                      alpha = 0.3,
                                      title = NULL,
                                      xlab = NULL,
                                      ylab = NULL,
                                      legend_position = "right") {

  # 1. Parameter Validation
  # Validate strategies is mutually exclusive with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'. ",
         "Use interventions/comparators vectors to specify exact comparisons.")
  }

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # 2. Data Preparation
  # Get PSA simulation data (always uses discounted values for CE-related analysis)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(psa_data$group), results$metadata)
  psa_data <- psa_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Get all strategies from data (already has display names)
  all_strategies <- unique(psa_data$strategy)

  # Helper function to resolve strategy names (technical -> display if needed)
  resolve_strategy <- function(strat_name) {
    if (strat_name %in% all_strategies) {
      return(strat_name)
    }
    # Try mapping technical name to display name
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == strat_name]
      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        return(matched_display[1])
      }
    }
    stop(sprintf("Strategy '%s' not found in results", strat_name))
  }

  # Resolve user-provided interventions/comparators to display names
  if (!is.null(interventions)) {
    interventions <- sapply(interventions, resolve_strategy, USE.NAMES = FALSE)
  }
  if (!is.null(comparators)) {
    comparators <- sapply(comparators, resolve_strategy, USE.NAMES = FALSE)
  }

  # 3. Determine comparison pairs
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N x M explicit comparisons
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
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }
  } else if (!is.null(interventions)) {
    # Intervention only: each intervention vs all others
    for (int_strat in interventions) {
      other_strategies <- setdiff(all_strategies, int_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = other
        )
      }
    }
  } else {
    # Comparator only: all others vs each comparator
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

  # 4. Calculate incremental values for each comparison pair
  incremental_data_list <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    # Create comparison label
    comp_label <- paste0(int_strat, " vs. ", comp_strat)

    # Get data for intervention and comparator
    int_data <- psa_data %>%
      filter(.data$strategy == int_strat) %>%
      select("simulation", "group", int_outcome = "outcome", int_cost = "cost")

    comp_data <- psa_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("simulation", "group", comp_outcome = "outcome", comp_cost = "cost")

    # Join and calculate incremental values
    pair_data <- int_data %>%
      inner_join(comp_data, by = c("simulation", "group")) %>%
      mutate(
        doutcome = .data$int_outcome - .data$comp_outcome,
        dcost = .data$int_cost - .data$comp_cost,
        comparison = comp_label
      ) %>%
      select("simulation", "group", "comparison", "doutcome", "dcost")

    incremental_data_list[[length(incremental_data_list) + 1]] <- pair_data
  }

  # Combine all comparison data
  incremental_data <- bind_rows(incremental_data_list)

  # Preserve comparison order as factor
  comparison_order <- unique(incremental_data$comparison)
  incremental_data <- incremental_data %>%
    mutate(comparison = factor(.data$comparison, levels = comparison_order))

  # 4. Calculate cost-effectiveness if wtp provided
  if (!is.null(wtp)) {
    incremental_data <- incremental_data %>%
      mutate(
        nmb = .data$doutcome * wtp - .data$dcost,
        cost_effective = factor(
          ifelse(.data$nmb > 0, "Cost-Effective", "Not Cost-Effective"),
          levels = c("Cost-Effective", "Not Cost-Effective")
        )
      )
  }

  # 5. Axis Label Generation
  if (is.null(xlab)) {
    outcome_label <- if (!is.null(results$metadata$summaries)) {
      summary_meta <- results$metadata$summaries %>%
        filter(.data$name == outcome_summary)
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
        filter(.data$name == cost_summary)
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
  breaks_fn <- pretty_breaks(n = 5)
  x_range <- range(c(0, incremental_data$doutcome), na.rm = TRUE)
  x_breaks <- breaks_fn(x_range)
  x_limits <- range(x_breaks)
  y_range <- range(c(0, incremental_data$dcost), na.rm = TRUE)
  y_breaks <- breaks_fn(y_range)
  y_limits <- range(y_breaks)

  # 7. Unified Plot Construction
  if (!is.null(wtp)) {
    # With WTP: color by cost-effectiveness
    p <- ggplot(incremental_data, aes(x = .data$doutcome, y = .data$dcost, color = .data$cost_effective))
  } else {
    # Without WTP: single color
    p <- ggplot(incremental_data, aes(x = .data$doutcome, y = .data$dcost))
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
        name = paste0("Cost-Effective\nat \u03BB = ", dollar(wtp, scale = 1/1000, suffix = "K"))
      )
  } else {
    p <- p +
      geom_point(alpha = alpha, size = 1, color = "gray40")
  }

  # Add scales and theme
  p <- p +
    scale_x_continuous(breaks = x_breaks, limits = x_limits, labels = number) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits, labels = comma) +
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


#' Incremental Net Monetary Benefit Density Plot
#'
#' Creates a density plot showing the distribution of incremental Net Monetary
#' Benefit (NMB) relative to a comparator at a given willingness-to-pay threshold.
#'
#' @param results A openqaly PSA results object
#' @param outcome_summary Name of the outcome summary (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary (e.g., "total_cost")
#' @param wtp Willingness-to-pay threshold for NMB calculation
#' @param interventions Character vector of intervention strategy name(s).
#'   Can be combined with comparators for N x M comparisons.
#' @param comparators Character vector of comparator strategy name(s).
#'   Can be combined with interventions for N x M comparisons.
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategies to include (NULL for all).
#'   Mutually exclusive with interventions/comparators.
#' @param show_mean Logical. Show vertical lines at mean incremental NMB per comparison?
#' @param alpha Transparency for density fill (default: 0.4)
#' @param title Optional plot title
#' @param xlab Optional x-axis label
#' @param legend_position Legend position ("right", "bottom", "top", "left", "none")
#'
#' @return A ggplot object
#'
#' @details
#' Incremental Net Monetary Benefit is calculated as:
#' \deqn{\Delta NMB = \lambda \times \Delta Outcomes - \Delta Costs}
#'
#' where \eqn{\lambda} is the willingness-to-pay threshold and \eqn{\Delta}
#' represents the difference between intervention and comparator. Positive
#' incremental NMB indicates the intervention is cost-effective at the given
#' WTP threshold.
#'
#' **Comparison Modes:**
#'
#' When interventions and/or comparators are specified:
#' \itemize{
#'   \item Both as single values: shows one comparison (intervention - comparator)
#'   \item One as vector, other NULL: shows each vs. all others
#'   \item Both as vectors (N x M mode): shows all pairwise comparisons (excluding self)
#' }
#'
#' A vertical dashed line at x = 0 indicates the threshold for cost-effectiveness:
#' positive values indicate the intervention is cost-effective.
#'
#' NMB calculations always use discounted values as this is a cost-effectiveness measure.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Incremental NMB density relative to a comparator
#' nmb_density_plot(psa_results, "total_qalys", "total_cost",
#'                  wtp = 50000, comparators = "control")
#'
#' # Intervention perspective
#' nmb_density_plot(psa_results, "total_qalys", "total_cost",
#'                  wtp = 100000, interventions = "new_treatment", show_mean = TRUE)
#' }
#'
#' @export
nmb_density_plot <- function(results,
                             outcome_summary,
                             cost_summary,
                             wtp,
                             interventions = NULL,
                             comparators = NULL,
                             groups = "overall",
                             strategies = NULL,
                             show_mean = TRUE,
                             alpha = 0.4,
                             title = NULL,
                             xlab = NULL,
                             legend_position = "right") {

  # Validate that strategies is mutually exclusive with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'. ",
         "Use interventions/comparators vectors to specify exact comparisons.")
  }

  # Require at least one of interventions or comparators
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided. ",
         "NMB is a comparative measure and requires a reference strategy.")
  }

  # Get PSA simulation data (always uses discounted values for NMB - cost-effectiveness measure)
  psa_data <- get_psa_simulations(
    results,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary,
    groups = groups,
    strategies = strategies
  )

  # Apply consistent group ordering (Overall first, then model order)
  group_levels <- get_group_order(unique(psa_data$group), results$metadata)
  psa_data <- psa_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Get all strategies from data (already has display names)
  all_strategies <- unique(psa_data$strategy)

  # Helper function to resolve strategy names (technical -> display if needed)
  resolve_strategy <- function(strat_name) {
    if (strat_name %in% all_strategies) {
      return(strat_name)
    }
    # Try mapping technical name to display name
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == strat_name]
      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        return(matched_display[1])
      }
    }
    stop(sprintf("Strategy '%s' not found in results", strat_name))
  }

  # Resolve user-provided interventions/comparators to display names
  if (!is.null(interventions)) {
    interventions <- sapply(interventions, resolve_strategy, USE.NAMES = FALSE)
  }
  if (!is.null(comparators)) {
    comparators <- sapply(comparators, resolve_strategy, USE.NAMES = FALSE)
  }

  # Determine comparison pairs
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N x M explicit comparisons
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
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }
  } else if (!is.null(interventions)) {
    # Intervention only: each intervention vs all others
    for (int_strat in interventions) {
      other_strategies <- setdiff(all_strategies, int_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = other
        )
      }
    }
  } else {
    # Comparator only: all others vs each comparator
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

  # Calculate incremental NMB for each comparison pair
  incremental_data_list <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    # Create comparison label (strategies are already display names)
    comp_label <- paste0(int_strat, " vs. ", comp_strat)

    # Get data for intervention and comparator
    int_data <- psa_data %>%
      filter(.data$strategy == int_strat) %>%
      select("simulation", "group", int_outcome = "outcome", int_cost = "cost")

    comp_data <- psa_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("simulation", "group", comp_outcome = "outcome", comp_cost = "cost")

    # Join and calculate incremental NMB
    pair_data <- int_data %>%
      inner_join(comp_data, by = c("simulation", "group")) %>%
      mutate(
        delta_outcome = .data$int_outcome - .data$comp_outcome,
        delta_cost = .data$int_cost - .data$comp_cost,
        nmb = .data$delta_outcome * wtp - .data$delta_cost,
        strategy = comp_label
      ) %>%
      select("simulation", "group", "strategy", "nmb")

    incremental_data_list[[length(incremental_data_list) + 1]] <- pair_data
  }

  # Combine all comparison data
  plot_data <- bind_rows(incremental_data_list)

  # Preserve comparison order as factor
  strategy_order <- unique(plot_data$strategy)
  plot_data <- plot_data %>%
    mutate(strategy = factor(.data$strategy, levels = strategy_order))

  # Map group display names
  if (!is.null(results$metadata) && !is.null(results$metadata$groups)) {
    plot_data$group <- map_names(as.character(plot_data$group),
                                  results$metadata$groups, "display_name")
    # Reapply group factor with correct order
    group_levels_mapped <- map_names(group_levels, results$metadata$groups, "display_name")
    plot_data <- plot_data %>%
      mutate(group = factor(.data$group, levels = group_levels_mapped))
  }

  # Set default x-axis label if not provided
  if (is.null(xlab)) {
    xlab <- "Incremental Net Monetary Benefit"
  }

  # Set default title if not provided
  if (is.null(title)) {
    title <- paste0("Incremental NMB Distribution at WTP = ", dollar(wtp))
  }

  # Create density plot
  p <- ggplot(plot_data, aes(x = .data$nmb, fill = .data$strategy, color = .data$strategy)) +
    geom_density(alpha = alpha) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = number) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = "Density",
      fill = "Comparison",
      color = "Comparison"
    )

  # Add mean lines if requested
  if (show_mean) {
    mean_data <- plot_data %>%
      group_by(.data$strategy, .data$group) %>%
      summarize(
        mean_nmb = mean(.data$nmb, na.rm = TRUE),
        .groups = "drop"
      )

    p <- p +
      geom_vline(
        data = mean_data,
        aes(xintercept = .data$mean_nmb, color = .data$strategy),
        linetype = "dashed",
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  # Faceting based on number of groups and comparisons
  n_groups <- length(unique(plot_data$group))
  n_strategies <- length(unique(plot_data$strategy))

  if (n_groups > 1 && n_strategies > 1) {
    p <- p + facet_wrap(vars(.data$strategy, .data$group), ncol = n_groups, scales = "free_y")
  } else if (n_groups > 1) {
    p <- p + facet_wrap(vars(.data$group), scales = "free_y")
  } else if (n_strategies > 1) {
    p <- p + facet_wrap(vars(.data$strategy), scales = "free_y")
  }

  # Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}


#' Outcome Density Plot
#'
#' Creates a density plot of outcome values from PSA simulations. Can display
#' either absolute outcome distributions per strategy or distributions of
#' outcome differences between intervention/comparator pairs.
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param outcome_summary Name of the outcome summary to plot (e.g., "total_qalys")
#' @param interventions Reference strategies for comparison (intervention perspective).
#'   Cannot be used with \code{strategies}.
#' @param comparators Reference strategies for comparison (comparator perspective).
#'   Cannot be used with \code{strategies}.
#' @param groups Which groups to include. Options: "overall" (default), "all",
#'   "all_groups", or a character vector of specific group names.
#' @param strategies Character vector of strategies to include. For absolute outcome
#'   values. Cannot be used with \code{interventions} or \code{comparators}.
#' @param discounted Logical. Use discounted outcome values? Default TRUE.
#' @param show_mean Logical. Add vertical dashed lines at means? Default TRUE.
#' @param alpha Numeric. Transparency level for density fill (0-1). Default 0.4.
#' @param title Character. Plot title. If NULL (default), auto-generated.
#' @param xlab Character. X-axis label. If NULL (default), auto-generated.
#' @param legend_position Position of the legend. Default "right".
#'
#' @return A ggplot object
#'
#' @export
outcome_density_plot <- function(results,
                                 outcome_summary,
                                 interventions = NULL,
                                 comparators = NULL,
                                 groups = "overall",
                                 strategies = NULL,
                                 discounted = TRUE,
                                 show_mean = TRUE,
                                 alpha = 0.4,
                                 title = NULL,
                                 xlab = NULL,
                                 legend_position = "right") {

  # Get data from getter (handles all validation and data retrieval)
  plot_data <- get_psa_outcome_simulations(
    results,
    outcome_summary = outcome_summary,
    interventions = interventions,
    comparators = comparators,
    groups = groups,
    strategies = strategies,
    discounted = discounted
  )

  # Determine if absolute or difference mode
  is_difference_mode <- !is.null(interventions) || !is.null(comparators)

  # Get outcome display name for labels
  outcome_label <- outcome_summary
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    summary_meta <- results$metadata$summaries %>%
      filter(.data$name == outcome_summary)
    if (nrow(summary_meta) > 0 && !is.na(summary_meta$display_name[1])) {
      outcome_label <- summary_meta$display_name[1]
    }
  }

  # Set default x-axis label
  if (is.null(xlab)) {
    if (is_difference_mode) {
      xlab <- paste0("\u0394 ", outcome_label)
    } else {
      xlab <- outcome_label
    }
  }

  # Set default title
  if (is.null(title)) {
    if (is_difference_mode) {
      title <- paste0("Incremental ", outcome_label, " Distribution")
    } else {
      title <- paste0(outcome_label, " Distribution")
    }
  }

  # Determine column and legend labels based on mode
  if (is_difference_mode) {
    fill_col <- "comparison"
    legend_label <- "Comparison"
  } else {
    fill_col <- "strategy"
    legend_label <- "Strategy"
  }

  # Create density plot (NO geom_vline at 0)
  p <- ggplot(plot_data, aes(x = .data$outcome, fill = .data[[fill_col]], color = .data[[fill_col]])) +
    geom_density(alpha = alpha) +
    scale_x_continuous(labels = number) +
    scale_y_continuous(labels = number) +
    theme_bw() +
    labs(
      title = title,
      x = xlab,
      y = "Density",
      fill = legend_label,
      color = legend_label
    )

  # Add mean lines if requested
  if (show_mean) {
    mean_data <- plot_data %>%
      group_by(.data[[fill_col]], .data$group) %>%
      summarize(
        mean_outcome = mean(.data$outcome, na.rm = TRUE),
        .groups = "drop"
      )

    p <- p +
      geom_vline(
        data = mean_data,
        aes(xintercept = .data$mean_outcome, color = .data[[fill_col]]),
        linetype = "dashed",
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  # Faceting based on number of groups and strategies/comparisons
  n_groups <- length(unique(plot_data$group))
  n_strategies <- length(unique(plot_data[[fill_col]]))

  if (n_groups > 1 && n_strategies > 1) {
    p <- p + facet_wrap(vars(.data[[fill_col]], .data$group), ncol = n_groups, scales = "free_y")
  } else if (n_groups > 1) {
    p <- p + facet_wrap(vars(.data$group), scales = "free_y")
  } else if (n_strategies > 1) {
    p <- p + facet_wrap(vars(.data[[fill_col]]), scales = "free_y")
  }

  # Set legend position
  if (legend_position != "default") {
    p <- p + theme(legend.position = legend_position)
  }

  p
}