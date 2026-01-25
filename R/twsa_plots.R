#' Two-Way Sensitivity Analysis Visualizations
#'
#' Functions for creating heatmap visualizations of two-way sensitivity
#' analysis (TWSA) results.
#'
#' @name twsa_plots
#' @importFrom ggplot2 ggplot aes geom_tile geom_point geom_text scale_fill_viridis_c
#' @importFrom ggplot2 facet_wrap vars labs theme_bw theme element_text element_blank
#' @importFrom ggplot2 coord_cartesian scale_x_continuous scale_y_continuous
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete
#' @importFrom scales comma pretty_breaks
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup summarize slice n all_of
#' @importFrom tibble tibble
#' @importFrom glue glue
NULL

#' Format Cell Value for Heatmap Display
#'
#' Internal helper to format numeric values for display inside heatmap cells.
#' Uses appropriate precision based on value magnitude.
#'
#' @param x Numeric vector of values
#' @return Character vector of formatted values
#' @keywords internal
format_cell_value <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return("")
    abs_val <- abs(val)
    if (abs_val >= 10000) {
      format(round(val), big.mark = ",", scientific = FALSE)
    } else if (abs_val >= 100) {
      format(round(val, 1), nsmall = 1, big.mark = ",")
    } else if (abs_val >= 1) {
      format(round(val, 2), nsmall = 2)
    } else {
      format(round(val, 3), nsmall = 3)
    }
  })
}

#' Prepare TWSA Outcomes Data
#'
#' Internal helper function that prepares TWSA data for heatmap rendering.
#' Extracts grid data and optionally calculates incremental values.
#'
#' @param results TWSA results object from run_twsa()
#' @param summary_name Name of summary to display
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection: "overall", specific group, or NULL
#' @param strategies Character vector of strategies to include (NULL for all)
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param discounted Logical. Use discounted values?
#'
#' @return Tibble with x_value, y_value, value, strategy, group, and parameter info
#' @keywords internal
prepare_twsa_outcomes_data <- function(results,
                                       summary_name,
                                       twsa_name = NULL,
                                       groups = "overall",
                                       strategies = NULL,
                                       interventions = NULL,
                                       comparators = NULL,
                                       discounted = FALSE) {

  # Extract TWSA summaries
  twsa_data <- extract_twsa_summaries(
    results,
    summary_name = summary_name,
    value_type = "all",
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Filter to specific TWSA analysis if multiple exist
  if (!is.null(twsa_name)) {
    twsa_data <- twsa_data %>%
      filter(.data$twsa_name == !!twsa_name | is.na(.data$twsa_name))
  } else {
    # Use first non-base-case TWSA if multiple exist
    available_twsa <- unique(twsa_data$twsa_name[!is.na(twsa_data$twsa_name)])
    if (length(available_twsa) > 1) {
      warning(glue("Multiple TWSA analyses found: {paste(available_twsa, collapse=', ')}. ",
                   "Using '{available_twsa[1]}'. Specify twsa_name to select a different one."))
      twsa_data <- twsa_data %>%
        filter(.data$twsa_name == available_twsa[1] | is.na(.data$twsa_name))
    }
  }

  # Filter out base case (run_id = 1) for heatmap - we'll mark it separately
  base_case_data <- twsa_data %>%
    filter(is.na(.data$twsa_name))

  grid_data <- twsa_data %>%
    filter(!is.na(.data$twsa_name))

  if (nrow(grid_data) == 0) {
    stop("No TWSA grid data found for plotting", call. = FALSE)
  }

  # Calculate differences if interventions/comparators provided
  if (!is.null(interventions) || !is.null(comparators)) {
    intervention_strategy <- if (!is.null(interventions)) interventions else comparators

    # Get all strategies present
    all_strats <- unique(grid_data$strategy)
    other_strategies <- setdiff(all_strats, intervention_strategy)

    if (length(other_strategies) == 0) {
      stop("No comparator strategies found for incremental calculation", call. = FALSE)
    }

    # Calculate incremental values
    diff_data <- list()
    for (other in other_strategies) {
      int_data <- grid_data %>%
        filter(.data$strategy == intervention_strategy) %>%
        select("x_value", "y_value", "group",
               "x_param_display_name", "y_param_display_name",
               "x_bc_value", "y_bc_value",
               int_amount = "amount")

      other_data <- grid_data %>%
        filter(.data$strategy == other) %>%
        select("x_value", "y_value", "group", other_amount = "amount")

      # Map display names
      int_mapped <- map_names_if_available(intervention_strategy, results$metadata$strategies)
      other_mapped <- map_names_if_available(other, results$metadata$strategies)

      if (!is.null(interventions)) {
        comparison_label <- paste0(int_mapped, " vs. ", other_mapped)
      } else {
        comparison_label <- paste0(other_mapped, " vs. ", int_mapped)
      }

      joined <- int_data %>%
        inner_join(other_data, by = c("x_value", "y_value", "group")) %>%
        mutate(
          strategy = comparison_label,
          value = if (!is.null(interventions)) {
            .data$int_amount - .data$other_amount
          } else {
            .data$other_amount - .data$int_amount
          }
        ) %>%
        select("x_value", "y_value", "group", "strategy", "value",
               "x_param_display_name", "y_param_display_name",
               "x_bc_value", "y_bc_value")

      diff_data[[length(diff_data) + 1]] <- joined
    }

    heatmap_data <- bind_rows(diff_data)
  } else {
    # No incremental - use raw values
    heatmap_data <- grid_data %>%
      mutate(value = .data$amount) %>%
      select("run_id", "x_value", "y_value", "group", "strategy", "value",
             "x_param_display_name", "y_param_display_name",
             "x_bc_value", "y_bc_value")

    # Map strategy display names
    if (!is.null(results$metadata$strategies)) {
      heatmap_data$strategy <- map_names(heatmap_data$strategy,
                                          results$metadata$strategies,
                                          "display_name")
    }
  }

  # Map group display names
  if (!is.null(results$metadata$groups)) {
    heatmap_data$group <- map_names(heatmap_data$group,
                                     results$metadata$groups,
                                     "display_name")
  }

  heatmap_data
}

#' Render TWSA Heatmap
#'
#' Internal helper to create heatmap visualization from prepared data.
#'
#' @param heatmap_data Prepared tibble from prepare_twsa_heatmap_data
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param legend_title Legend title
#' @param viridis_option Viridis palette option
#' @param show_base_case Logical. Mark base case point?
#'
#' @return A ggplot2 object
#' @keywords internal
render_twsa_heatmap <- function(heatmap_data,
                                 title = NULL,
                                 xlab = NULL,
                                 ylab = NULL,
                                 legend_title = "Value",
                                 viridis_option = "viridis",
                                 show_base_case = TRUE) {

  # Get parameter display names for axis labels
  x_param_name <- unique(heatmap_data$x_param_display_name)[1]
  y_param_name <- unique(heatmap_data$y_param_display_name)[1]

  if (is.null(xlab)) xlab <- x_param_name
  if (is.null(ylab)) ylab <- y_param_name

  # Determine faceting
  n_groups <- length(unique(heatmap_data$group))
  n_strategies <- length(unique(heatmap_data$strategy))

  # Calculate number of facets and optimal column count
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
    facet_component <- facet_wrap(vars(.data$strategy, .data$group), ncol = n_groups)
  } else if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group), ncol = ncol)
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy), ncol = ncol)
  }

  # Convert to categorical axes - TWSA varies parameters at discrete grid points
  x_levels <- sort(unique(heatmap_data$x_value))
  y_levels <- sort(unique(heatmap_data$y_value))

  heatmap_data <- heatmap_data %>%
    mutate(
      x_factor = factor(.data$x_value, levels = x_levels,
        labels = format(x_levels, big.mark = ",", trim = TRUE)),
      y_factor = factor(.data$y_value, levels = y_levels,
        labels = format(y_levels, big.mark = ",", trim = TRUE))
    )

  # Create base case tile border data (for highlighting with black border)
  base_case_tile <- NULL
  if (show_base_case && "x_bc_value" %in% names(heatmap_data)) {
    x_bc <- unique(heatmap_data$x_bc_value)[1]
    y_bc <- unique(heatmap_data$y_bc_value)[1]
    if (!is.na(x_bc) && !is.na(y_bc)) {
      # Find the factor labels for base case values
      x_bc_label <- format(x_bc, big.mark = ",", trim = TRUE)
      y_bc_label <- format(y_bc, big.mark = ",", trim = TRUE)
      # Only create base case tile if the base case is in the grid
      if (x_bc_label %in% levels(heatmap_data$x_factor) &&
          y_bc_label %in% levels(heatmap_data$y_factor)) {
        base_case_tile <- heatmap_data %>%
          distinct(.data$strategy, .data$group) %>%
          mutate(
            x_factor = factor(x_bc_label, levels = levels(heatmap_data$x_factor)),
            y_factor = factor(y_bc_label, levels = levels(heatmap_data$y_factor))
          )
      }
    }
  }

  # Format labels for display inside cells
  heatmap_data <- heatmap_data %>%
    mutate(
      label = format_cell_value(.data$value)
    )

  # Create heatmap with categorical axes
  p <- ggplot(heatmap_data, aes(
    x = .data$x_factor,
    y = .data$y_factor,
    fill = .data$value
  )) +
    geom_tile(alpha = 0.9) +
    geom_text(aes(label = .data$label), size = 3, color = "white") +
    scale_fill_viridis_c(option = viridis_option, name = legend_title) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 8),
      axis.ticks = element_blank(),
      legend.position = "right"
    )

  # Add base case tile border (black outline around base case cell)
  if (!is.null(base_case_tile)) {
    p <- p +
      geom_tile(
        data = base_case_tile,
        aes(x = .data$x_factor, y = .data$y_factor),
        inherit.aes = FALSE,
        fill = NA,
        color = "black",
        linewidth = 1
      )
  }

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}

#' TWSA Outcomes Heatmap Plot
#'
#' Creates a heatmap visualization of two-way sensitivity analysis results.
#' The heatmap shows how model outcomes vary across a grid of two parameter
#' values.
#'
#' @param results TWSA results object from run_twsa()
#' @param summary_name Name of the summary to display (e.g., "total_qalys")
#' @param twsa_name Name of specific TWSA analysis to plot (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param interventions Intervention strategy name(s) for incremental calculation
#' @param comparators Comparator strategy name(s) for incremental calculation
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param title Optional plot title
#' @param xlab Optional x-axis label (defaults to X parameter name)
#' @param ylab Optional y-axis label (defaults to Y parameter name)
#' @param legend_title Legend title (default: summary_name)
#' @param viridis_option Viridis color palette: "viridis" (default), "magma",
#'   "plasma", "inferno", or "cividis"
#' @param show_base_case Logical. Show base case point marker? (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model)
#'
#' # Basic heatmap
#' twsa_outcomes_plot(results, "total_qalys")
#'
#' # Incremental heatmap (intervention vs comparator)
#' twsa_outcomes_plot(results, "total_qalys",
#'   interventions = "treatment",
#'   comparators = "standard_care")
#'
#' # With custom styling
#' twsa_outcomes_plot(results, "total_cost",
#'   title = "Cost Sensitivity",
#'   viridis_option = "magma",
#'   discounted = TRUE)
#' }
twsa_outcomes_plot <- function(results,
                               summary_name,
                               twsa_name = NULL,
                               groups = "overall",
                               strategies = NULL,
                               interventions = NULL,
                               comparators = NULL,
                               discounted = FALSE,
                               title = NULL,
                               xlab = NULL,
                               ylab = NULL,
                               legend_title = NULL,
                               viridis_option = "viridis",
                               show_base_case = TRUE) {

  # Validate viridis option
  viridis_option <- match.arg(viridis_option,
                               c("viridis", "magma", "plasma", "inferno", "cividis"))

  # Set default legend title
  if (is.null(legend_title)) {
    legend_title <- summary_name
  }

  # Prepare data
  heatmap_data <- prepare_twsa_outcomes_data(
    results = results,
    summary_name = summary_name,
    twsa_name = twsa_name,
    groups = groups,
    strategies = strategies,
    interventions = interventions,
    comparators = comparators,
    discounted = discounted
  )

  # Render plot
  render_twsa_heatmap(
    heatmap_data = heatmap_data,
    title = title,
    xlab = xlab,
    ylab = ylab,
    legend_title = legend_title,
    viridis_option = viridis_option,
    show_base_case = show_base_case
  )
}

#' Helper to map names if metadata available
#'
#' @param names Names to map
#' @param metadata_df Metadata dataframe with name and display_name columns
#' @return Mapped names or original if metadata unavailable
#' @keywords internal
map_names_if_available <- function(names, metadata_df) {
  if (is.null(metadata_df)) return(names)
  map_names(names, metadata_df, "display_name")
}

#' Prepare TWSA NMB Data
#'
#' Internal helper function that prepares TWSA data for NMB heatmap rendering.
#' Calculates Net Monetary Benefit for each grid point.
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary
#' @param cost_outcome Name of the cost summary
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#' @param wtp Willingness-to-pay threshold
#'
#' @return Tibble with x_value, y_value, value, strategy, group, and parameter info
#' @keywords internal
prepare_twsa_nmb_data <- function(results,
                                   health_outcome,
                                   cost_outcome,
                                   twsa_name = NULL,
                                   groups = "overall",
                                   interventions = NULL,
                                   comparators = NULL,
                                   wtp) {

  # Get outcome data (always discounted for NMB)
  outcome_data <- prepare_twsa_outcomes_data(
    results = results,
    summary_name = health_outcome,
    twsa_name = twsa_name,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Get cost data (always discounted for NMB)
  cost_data <- prepare_twsa_outcomes_data(
    results = results,
    summary_name = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Join outcome and cost data
  nmb_data <- outcome_data %>%
    inner_join(
      cost_data %>% select("x_value", "y_value", "strategy", "group", cost_value = "value"),
      by = c("x_value", "y_value", "strategy", "group")
    ) %>%
    mutate(
      # NMB = (delta_outcomes * WTP) - delta_costs
      value = (.data$value * wtp) - .data$cost_value
    ) %>%
    select(-"cost_value")

  nmb_data
}

#' TWSA Net Monetary Benefit Heatmap Plot
#'
#' Creates a heatmap visualization showing the Net Monetary Benefit (NMB)
#' across a 2D grid of parameter values. NMB = (delta_outcomes x WTP) - delta_costs.
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param twsa_name Name of specific TWSA analysis to plot (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param interventions Intervention strategy name(s). At least one of interventions
#'   or comparators must be specified.
#' @param comparators Comparator strategy name(s). At least one of interventions
#'   or comparators must be specified.
#' @param wtp Willingness-to-pay threshold. If NULL, extracts from outcome metadata.
#' @param title Optional plot title
#' @param xlab Optional x-axis label (defaults to X parameter name)
#' @param ylab Optional y-axis label (defaults to Y parameter name)
#' @param viridis_option Viridis color palette: "viridis" (default), "magma",
#'   "plasma", "inferno", or "cividis"
#' @param show_base_case Logical. Show base case point marker? (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model)
#'
#' # NMB heatmap with explicit WTP
#' twsa_nmb_plot(results, "total_qalys", "total_cost",
#'   interventions = "treatment", wtp = 50000)
#'
#' # NMB heatmap using WTP from metadata
#' twsa_nmb_plot(results, "total_qalys", "total_cost",
#'   comparators = "standard_care")
#' }
twsa_nmb_plot <- function(results,
                           health_outcome,
                           cost_outcome,
                           twsa_name = NULL,
                           groups = "overall",
                           interventions = NULL,
                           comparators = NULL,
                           wtp = NULL,
                           title = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           viridis_option = "viridis",
                           show_base_case = TRUE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided for NMB calculation",
         call. = FALSE)
  }

  # Validate viridis option
  viridis_option <- match.arg(viridis_option,
                               c("viridis", "magma", "plasma", "inferno", "cividis"))

  # Get WTP if not provided
  if (is.null(wtp)) {
    if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Provide explicit wtp parameter.",
           call. = FALSE)
    }
    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == health_outcome)
    if (nrow(outcome_meta) == 0) {
      stop(sprintf("Health outcome '%s' not found in metadata", health_outcome),
           call. = FALSE)
    }
    wtp <- outcome_meta[["wtp"]][1]
    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for health outcome '%s'. Provide explicit wtp parameter.",
                   health_outcome), call. = FALSE)
    }
  }

  # Prepare NMB data
  nmb_data <- prepare_twsa_nmb_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    interventions = interventions,
    comparators = comparators,
    wtp = wtp
  )

  # Check if data is valid
  if (nrow(nmb_data) == 0) {
    stop("No data available for NMB heatmap with specified parameters",
         call. = FALSE)
  }

  # Create legend title with WTP
  wtp_formatted <- format(wtp, big.mark = ",")
  legend_title <- glue("NMB (\u03bb = {wtp_formatted})")

  # Render heatmap
  render_twsa_heatmap(
    heatmap_data = nmb_data,
    title = title,
    xlab = xlab,
    ylab = ylab,
    legend_title = legend_title,
    viridis_option = viridis_option,
    show_base_case = show_base_case
  )
}

#' Prepare TWSA CE Data
#'
#' Internal helper function that prepares TWSA data for ICER heatmap rendering.
#' Calculates ICER with handling for edge cases (dominated, dominant, equivalent).
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary
#' @param cost_outcome Name of the cost summary
#' @param twsa_name Name of specific TWSA analysis (NULL for first/only)
#' @param groups Group selection
#' @param interventions Intervention strategy name(s)
#' @param comparators Comparator strategy name(s)
#'
#' @return Tibble with x_value, y_value, icer, ce_class, display_value, strategy, group
#' @keywords internal
prepare_twsa_ce_data <- function(results,
                                  health_outcome,
                                  cost_outcome,
                                  twsa_name = NULL,
                                  groups = "overall",
                                  interventions,
                                  comparators) {

  # Get outcome data (always discounted for CE)
  outcome_data <- prepare_twsa_outcomes_data(
    results = results,
    summary_name = health_outcome,
    twsa_name = twsa_name,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Get cost data (always discounted for CE)
  cost_data <- prepare_twsa_outcomes_data(
    results = results,
    summary_name = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    strategies = NULL,
    interventions = interventions,
    comparators = comparators,
    discounted = TRUE
  )

  # Join and calculate ICER
  ce_data <- outcome_data %>%
    rename(delta_outcome = "value") %>%
    inner_join(
      cost_data %>% select("x_value", "y_value", "strategy", "group", delta_cost = "value"),
      by = c("x_value", "y_value", "strategy", "group")
    ) %>%
    mutate(
      # Calculate ICER with special case handling
      icer = case_when(
        abs(.data$delta_outcome) < .Machine$double.eps ~ NaN,  # Equivalent (undefined)
        .data$delta_outcome < 0 & .data$delta_cost > 0 ~ Inf,  # Dominated
        .data$delta_outcome > 0 & .data$delta_cost <= 0 ~ 0,   # Dominant (or cost-saving)
        TRUE ~ .data$delta_cost / .data$delta_outcome          # Normal ICER
      ),
      # Classify for display
      ce_class = case_when(
        is.nan(.data$icer) ~ "equivalent",
        is.infinite(.data$icer) & .data$icer > 0 ~ "dominated",
        .data$icer == 0 ~ "dominant",
        .data$delta_outcome < 0 & .data$delta_cost < 0 ~ "sw_quadrant",  # SW quadrant
        TRUE ~ "normal"
      )
    )

  # Calculate display values for heatmap
  # Get max normal ICER for capping dominated values
  normal_icers <- ce_data$icer[ce_data$ce_class == "normal"]
  max_normal_icer <- if (length(normal_icers) > 0) max(normal_icers, na.rm = TRUE) else 100000
  max_display <- max_normal_icer * 1.5

  ce_data <- ce_data %>%
    mutate(
      display_value = case_when(
        .data$ce_class == "equivalent" ~ NA_real_,        # Gray tile
        .data$ce_class == "dominated" ~ max_display,      # High end of scale
        .data$ce_class == "dominant" ~ 0,                 # Low end of scale
        .data$ce_class == "sw_quadrant" ~ abs(.data$icer), # Show absolute value
        TRUE ~ .data$icer                                 # Normal ICER
      ),
      value = .data$display_value  # For render_twsa_heatmap compatibility
    )

  ce_data
}

#' Render TWSA CE Heatmap
#'
#' Internal helper to create CE heatmap visualization from prepared data.
#' Similar to render_twsa_heatmap but with special handling for edge cases.
#'
#' @param ce_data Prepared tibble from prepare_twsa_ce_data
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param viridis_option Viridis palette option
#' @param show_base_case Logical. Mark base case point?
#'
#' @return A ggplot2 object
#' @keywords internal
render_twsa_ce_heatmap <- function(ce_data,
                                    title = NULL,
                                    xlab = NULL,
                                    ylab = NULL,
                                    viridis_option = "viridis",
                                    show_base_case = TRUE) {

  # Get parameter display names for axis labels
  x_param_name <- unique(ce_data$x_param_display_name)[1]
  y_param_name <- unique(ce_data$y_param_display_name)[1]

  if (is.null(xlab)) xlab <- x_param_name
  if (is.null(ylab)) ylab <- y_param_name

  # Determine faceting
  n_groups <- length(unique(ce_data$group))
  n_strategies <- length(unique(ce_data$strategy))

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
    facet_component <- facet_wrap(vars(.data$strategy, .data$group), ncol = n_groups)
  } else if ((n_groups > 1) && (n_strategies == 1)) {
    facet_component <- facet_wrap(vars(.data$group), ncol = ncol)
  } else if ((n_strategies > 1) && (n_groups == 1)) {
    facet_component <- facet_wrap(vars(.data$strategy), ncol = ncol)
  }

  # Convert to categorical axes - TWSA varies parameters at discrete grid points
  x_levels <- sort(unique(ce_data$x_value))
  y_levels <- sort(unique(ce_data$y_value))

  ce_data <- ce_data %>%
    mutate(
      x_factor = factor(.data$x_value, levels = x_levels,
        labels = format(x_levels, big.mark = ",", trim = TRUE)),
      y_factor = factor(.data$y_value, levels = y_levels,
        labels = format(y_levels, big.mark = ",", trim = TRUE))
    )

  # Create base case tile border data (for highlighting with black border)
  base_case_tile <- NULL
  if (show_base_case && "x_bc_value" %in% names(ce_data)) {
    x_bc <- unique(ce_data$x_bc_value)[1]
    y_bc <- unique(ce_data$y_bc_value)[1]
    if (!is.na(x_bc) && !is.na(y_bc)) {
      # Find the factor labels for base case values
      x_bc_label <- format(x_bc, big.mark = ",", trim = TRUE)
      y_bc_label <- format(y_bc, big.mark = ",", trim = TRUE)
      # Only create base case tile if the base case is in the grid
      if (x_bc_label %in% levels(ce_data$x_factor) &&
          y_bc_label %in% levels(ce_data$y_factor)) {
        base_case_tile <- ce_data %>%
          distinct(.data$strategy, .data$group) %>%
          mutate(
            x_factor = factor(x_bc_label, levels = levels(ce_data$x_factor)),
            y_factor = factor(y_bc_label, levels = levels(ce_data$y_factor))
          )
      }
    }
  }

  # Format labels for CE display (with special case handling)
  max_display <- max(ce_data$display_value, na.rm = TRUE)
  ce_data <- ce_data %>%
    mutate(
      label = case_when(
        .data$ce_class == "equivalent" ~ "Equiv.",
        .data$ce_class == "dominated" ~ "Dom'd",
        .data$ce_class == "dominant" ~ "Dom't",
        TRUE ~ format_cell_value(.data$icer)
      )
    )

  # Create heatmap with NA handling for equivalent cases
  p <- ggplot(ce_data, aes(
    x = .data$x_factor,
    y = .data$y_factor,
    fill = .data$display_value
  )) +
    geom_tile(alpha = 0.9) +
    geom_text(aes(label = .data$label), size = 3, color = "white") +
    scale_fill_viridis_c(
      option = viridis_option,
      name = "ICER",
      na.value = "gray80",  # Gray for equivalent/undefined
      labels = function(x) {
        ifelse(is.na(x), "Equiv.",
               ifelse(x >= max_display * 0.95, "Dominated",
                      ifelse(x == 0, "Dominant", scales::comma(x, accuracy = 1))))
      }
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 8),
      axis.ticks = element_blank(),
      legend.position = "right"
    )

  # Add base case tile border (black outline around base case cell)
  if (!is.null(base_case_tile)) {
    p <- p +
      geom_tile(
        data = base_case_tile,
        aes(x = .data$x_factor, y = .data$y_factor),
        inherit.aes = FALSE,
        fill = NA,
        color = "black",
        linewidth = 1
      )
  }

  # Add faceting if needed
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  p
}

#' TWSA Cost-Effectiveness (ICER) Heatmap Plot
#'
#' Creates a heatmap visualization showing the Incremental Cost-Effectiveness
#' Ratio (ICER) across a 2D grid of parameter values. Handles edge cases:
#' Dominated (more costly, less effective), Dominant (less costly, more effective),
#' and Equivalent (identical outcomes).
#'
#' @param results TWSA results object from run_twsa()
#' @param health_outcome Name of the health outcome summary (e.g., "total_qalys")
#' @param cost_outcome Name of the cost summary (e.g., "total_cost")
#' @param twsa_name Name of specific TWSA analysis to plot (NULL for first/only)
#' @param groups Group selection: "overall" (default), "all", "all_groups", or
#'   specific group name(s)
#' @param interventions Intervention strategy name(s). Required.
#' @param comparators Comparator strategy name(s). Required.
#' @param title Optional plot title
#' @param xlab Optional x-axis label (defaults to X parameter name)
#' @param ylab Optional y-axis label (defaults to Y parameter name)
#' @param viridis_option Viridis color palette: "viridis" (default), "magma",
#'   "plasma", "inferno", or "cividis"
#' @param show_base_case Logical. Show base case point marker? (default: TRUE)
#'
#' @return A ggplot2 object
#'
#' @details
#' ICER = (Cost_intervention - Cost_comparator) / (Outcome_intervention - Outcome_comparator)
#'
#' Special cases displayed in the heatmap:
#' \itemize{
#'   \item Dominated: Intervention is more costly and less effective (shown at high end of scale)
#'   \item Dominant: Intervention is less costly and more effective (shown at low end of scale)
#'   \item Equivalent: Identical outcomes result in undefined ICER (shown as gray)
#' }
#'
#' ICER calculations always use discounted values.
#'
#' @export
#' @examples
#' \dontrun{
#' results <- run_twsa(model)
#'
#' # CE heatmap
#' twsa_ce_plot(results, "total_qalys", "total_cost",
#'   interventions = "treatment",
#'   comparators = "standard_care")
#' }
twsa_ce_plot <- function(results,
                          health_outcome,
                          cost_outcome,
                          twsa_name = NULL,
                          groups = "overall",
                          interventions,
                          comparators,
                          title = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          viridis_option = "viridis",
                          show_base_case = TRUE) {

  # Validate viridis option
  viridis_option <- match.arg(viridis_option,
                               c("viridis", "magma", "plasma", "inferno", "cividis"))

  # Prepare CE data
  ce_data <- prepare_twsa_ce_data(
    results = results,
    health_outcome = health_outcome,
    cost_outcome = cost_outcome,
    twsa_name = twsa_name,
    groups = groups,
    interventions = interventions,
    comparators = comparators
  )

  # Check if data is valid
  if (nrow(ce_data) == 0) {
    stop("No data available for CE heatmap with specified parameters",
         call. = FALSE)
  }

  # Render CE heatmap
  render_twsa_ce_heatmap(
    ce_data = ce_data,
    title = title,
    xlab = xlab,
    ylab = ylab,
    viridis_option = viridis_option,
    show_base_case = show_base_case
  )
}
