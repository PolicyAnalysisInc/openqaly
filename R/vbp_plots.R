#' VBP Plot Output Functions
#'
#' Functions for generating value-based pricing plots showing VBP across
#' different willingness-to-pay thresholds.
#'
#' @name vbp_plots
#' @keywords internal
NULL

#' Prepare VBP Plot Data
#'
#' Internal helper function that prepares VBP data for rendering as a plot.
#' Calculates VBP prices across a range of WTP thresholds for each comparator.
#'
#' @param vbp_results Results from run_vbp()
#' @param wtp_range Numeric vector with min and max WTP values.
#'   If NULL, auto-generates based on outcome metadata.
#' @param wtp_step Step size for WTP thresholds. If NULL, auto-calculated.
#' @param comparators Comparator selection:
#'   \itemize{
#'     \item \code{"all"} - All comparators plus aggregate (default)
#'     \item \code{"overall"} - Aggregate only ("vs. All Comparators")
#'     \item \code{"all_comparators"} - Individual comparators only (no aggregate)
#'     \item Character vector - Specific comparator name(s)
#'   }
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{NULL} or \code{"all"} - All groups (triggers faceting)
#'   }
#'
#' @return Tibble with columns: wtp, comparator, vbp_price, group
#' @keywords internal
prepare_vbp_plot_data <- function(vbp_results,
                                  wtp_range = NULL,
                                  wtp_step = NULL,
                                  comparators = "all",
                                  groups = "overall") {

 # Handle group selection for plotting
  if (is.null(groups) || identical(groups, "all")) {
    # Use all groups from metadata + overall (overall last, matching CE plot pattern)
    groups_to_plot <- c(vbp_results$vbp_metadata$groups, "overall")
    groups_to_plot <- unique(groups_to_plot)
  } else if (identical(groups, "all_groups")) {
    # All groups except overall
    groups_to_plot <- setdiff(vbp_results$vbp_metadata$groups, "overall")
  } else {
    groups_to_plot <- groups
  }

  # Reorder groups: Overall first, then model definition order (using internal names)
  metadata <- attr(vbp_results$aggregated, "metadata")
  has_overall <- "overall" %in% groups_to_plot
  if (!is.null(metadata$groups)) {
    model_order <- metadata$groups$name
    ordered_groups <- model_order[model_order %in% groups_to_plot]
  } else {
    ordered_groups <- setdiff(groups_to_plot, "overall")
  }
  groups_to_plot <- if (has_overall) c("overall", ordered_groups) else ordered_groups

  # Process each group
  all_data <- list()

  for (grp in groups_to_plot) {
    # Get available comparators from VBP equations
    if (grp == "overall") {
      equations <- vbp_results$vbp_equations
    } else {
      equations <- vbp_results$vbp_equations_by_group %>%
        filter(.data$group == grp)
    }

    if (nrow(equations) == 0) {
      next
    }

    available_comparators <- unique(equations$comparator)

    # Process comparators based on selection pattern (matching DSA VBP pattern)
    include_aggregate <- FALSE
    if (identical(comparators, "overall")) {
      # Just the aggregate "All Comparators"
      comparators_for_calc <- available_comparators
      include_aggregate <- length(available_comparators) > 0
      show_individuals <- FALSE
    } else if (identical(comparators, "all")) {
      # All individuals + aggregate
      comparators_for_calc <- available_comparators
      include_aggregate <- length(available_comparators) > 1
      show_individuals <- TRUE
    } else if (identical(comparators, "all_comparators")) {
      # Just individuals, no aggregate
      comparators_for_calc <- available_comparators
      show_individuals <- TRUE
    } else {
      # Specific comparator names - filter
      invalid <- setdiff(comparators, available_comparators)
      if (length(invalid) > 0) {
        stop(sprintf("Comparator(s) not found: %s. Available: %s",
                     paste(invalid, collapse = ", "),
                     paste(available_comparators, collapse = ", ")))
      }
      comparators_for_calc <- comparators
      show_individuals <- TRUE
    }

    # Generate WTP range if not provided
    if (is.null(wtp_range)) {
      default_wtp <- extract_vbp_default_wtp(vbp_results)
      if (is.null(default_wtp) || is.na(default_wtp) || default_wtp <= 0) {
        wtp_range <- c(0, 200000)
      } else {
        wtp_range <- c(0, default_wtp * 2)
      }
    }

    # Generate WTP step if not provided
    if (is.null(wtp_step)) {
      range_span <- wtp_range[2] - wtp_range[1]
      wtp_step <- range_span / 40  # ~40 points for smooth curve
      # Round to nice number
      magnitude <- 10^floor(log10(wtp_step))
      wtp_step <- ceiling(wtp_step / magnitude) * magnitude
    }

    # Generate WTP sequence
    wtp_seq <- seq(wtp_range[1], wtp_range[2], by = wtp_step)

    # Calculate VBP at each WTP for each comparator
    vbp_data <- NULL
    if (show_individuals) {
      vbp_data <- expand_grid(
        wtp = wtp_seq,
        comparator = comparators_for_calc
      ) %>%
        rowwise() %>%
        mutate(
          vbp_price = calculate_vbp_price(
            vbp_results,
            wtp = .data$wtp,
            comparator = .data$comparator,
            group = grp
          )
        ) %>%
        ungroup() %>%
        mutate(group = grp)
    }

    # Calculate "All Comparators" series (minimum VBP at each WTP)
    if (include_aggregate) {
      # Calculate minimum VBP across all available comparators at each WTP
      all_comp_data <- expand_grid(
        wtp = wtp_seq,
        comparator = available_comparators
      ) %>%
        rowwise() %>%
        mutate(
          vbp_price = calculate_vbp_price(
            vbp_results,
            wtp = .data$wtp,
            comparator = .data$comparator,
            group = grp
          )
        ) %>%
        ungroup() %>%
        group_by(.data$wtp) %>%
        summarize(vbp_price = min(.data$vbp_price), .groups = "drop") %>%
        mutate(comparator = "All Comparators", group = grp)

      if (is.null(vbp_data)) {
        vbp_data <- all_comp_data
      } else {
        vbp_data <- bind_rows(vbp_data, all_comp_data)
      }
    }

    all_data[[grp]] <- vbp_data
  }

  bind_rows(all_data)
}

#' Plot Value-Based Pricing
#'
#' Creates a line plot showing the value-based price at different
#' willingness-to-pay thresholds for each comparator strategy.
#'
#' @param vbp_results Results from run_vbp()
#' @param wtp_range Numeric vector with min and max WTP values for x-axis.
#'   If NULL, auto-generates based on outcome summary metadata (0 to 2x default WTP).
#' @param wtp_step Step size for WTP thresholds. If NULL, auto-calculated
#'   for smooth curve (~40 points).
#' @param comparators Comparator selection:
#'   \itemize{
#'     \item \code{"all"} - All comparators plus aggregate (default)
#'     \item \code{"overall"} - Aggregate only ("vs. All Comparators")
#'     \item \code{"all_comparators"} - Individual comparators only (no aggregate)
#'     \item Character vector - Specific comparator name(s)
#'   }
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{NULL} or \code{"all"} - All groups (triggers faceting)
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param show_default_wtp Logical. Show vertical reference line at the
#'   default WTP from outcome metadata? Default TRUE.
#' @param show_vbp_at_wtp Logical. Show VBP values at the default WTP as
#'   point markers with labels on each line? Default TRUE. Only applies
#'   when show_default_wtp is TRUE.
#'
#' @return A ggplot2 object
#'
#' @details
#' The VBP plot shows how the maximum cost-effective price for the intervention
#' varies with the willingness-to-pay threshold. Each line represents a different
#' comparator - the VBP is the price at which the intervention becomes cost-neutral
#' versus that comparator.
#'
#' When multiple groups are present, the plot is automatically faceted by group.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#'
#' vbp_results <- run_vbp(
#'   model,
#'   price_variable = "c_drug",
#'   intervention_strategy = "targeted"
#' )
#'
#' # Basic VBP plot with auto-generated WTP range (individuals only by default)
#' vbp_plot(vbp_results)
#'
#' # Custom WTP range
#' vbp_plot(vbp_results, wtp_range = c(0, 150000), wtp_step = 10000)
#'
#' # VBP plot for specific comparator
#' vbp_plot(vbp_results, comparators = "chemo")
#'
#' # VBP plot with aggregate line
#' vbp_plot(vbp_results, comparators = "all")
#' }
#'
#' @export
vbp_plot <- function(vbp_results,
                     wtp_range = NULL,
                     wtp_step = NULL,
                     comparators = "all_comparators",
                     groups = "overall",
                     show_default_wtp = TRUE,
                     show_vbp_at_wtp = TRUE) {

  # Prepare plot data
  plot_data <- prepare_vbp_plot_data(
    vbp_results = vbp_results,
    wtp_range = wtp_range,
    wtp_step = wtp_step,
    comparators = comparators,
    groups = groups
  )

  # Check for empty data
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No VBP data available for the specified comparators and groups")
  }

  # Map comparator and group names to display names

  metadata <- attr(vbp_results$aggregated, "metadata")
  if (!is.null(metadata) && !is.null(metadata$strategies)) {
    plot_data$comparator <- map_names(plot_data$comparator,
                                       metadata$strategies,
                                       "display_name")
  }
  # Handle "overall" -> "Overall" special case, then map other groups
  plot_data$group <- ifelse(plot_data$group == "overall", "Overall", plot_data$group)
  if (!is.null(metadata) && !is.null(metadata$groups)) {
    plot_data$group <- map_names(plot_data$group,
                                  metadata$groups,
                                  "display_name")
  }

  # Determine line types: solid for individual comparators, dashed for "All"
  comparators_in_data <- unique(plot_data$comparator)
  has_all_comparators <- "All Comparators" %in% comparators_in_data

  # Create base plot
  if (has_all_comparators) {
    # Use linetype aesthetic for "All Comparators"
    plot_data <- plot_data %>%
      mutate(
        line_type = ifelse(.data$comparator == "All Comparators", "dashed", "solid"),
        comparator_label = ifelse(
          .data$comparator == "All Comparators",
          "All Comparators",
          paste0("vs. ", .data$comparator)
        )
      )

    p <- ggplot(plot_data, aes(
      x = .data$wtp,
      y = .data$vbp_price,
      color = .data$comparator_label,
      linetype = .data$line_type
    )) +
      geom_line(linewidth = 1) +
      scale_linetype_identity() +
      guides(linetype = "none")
  } else {
    plot_data <- plot_data %>%
      mutate(comparator_label = paste0("vs. ", .data$comparator))

    p <- ggplot(plot_data, aes(
      x = .data$wtp,
      y = .data$vbp_price,
      color = .data$comparator_label
    )) +
      geom_line(linewidth = 1)
  }

  # Add scales and theme
  p <- p +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    theme_bw() +
    labs(
      x = "Willingness-to-Pay Threshold",
      y = "Value-Based Price",
      color = "Comparator"
    )

  # Add default WTP reference line if requested
  if (show_default_wtp) {
    default_wtp <- extract_vbp_default_wtp(vbp_results)
    if (!is.null(default_wtp) && !is.na(default_wtp) && default_wtp > 0) {
      p <- p +
        geom_vline(
          xintercept = default_wtp,
          linetype = "dotted",
          color = "gray40",
          linewidth = 0.8
        ) +
        annotate(
          "text",
          x = default_wtp,
          y = Inf,
          label = paste0("Default WTP: ", comma(default_wtp)),
          hjust = -0.1,
          vjust = 1.5,
          size = 3,
          color = "gray40"
        )

      # Add VBP value callouts at default WTP
      if (show_vbp_at_wtp) {
        # Find the closest WTP point in the data to the default WTP
        wtp_tolerance <- (max(plot_data$wtp) - min(plot_data$wtp)) / 100
        vbp_at_wtp <- plot_data %>%
          filter(abs(.data$wtp - default_wtp) <= wtp_tolerance) %>%
          group_by(.data$comparator_label, .data$group) %>%
          slice(1) %>%
          ungroup()

        # If no exact match found, calculate VBP at default WTP directly
        if (nrow(vbp_at_wtp) == 0) {
          unique_comparators <- unique(plot_data$comparator)
          unique_groups <- unique(plot_data$group)

          vbp_at_wtp <- expand_grid(
            comparator = unique_comparators,
            group = unique_groups
          ) %>%
            rowwise() %>%
            mutate(
              vbp_price = calculate_vbp_price(
                vbp_results,
                wtp = default_wtp,
                comparator = .data$comparator,
                group = .data$group
              ),
              comparator_label = paste0("vs. ", .data$comparator)
            ) %>%
            ungroup()
        }

        if (nrow(vbp_at_wtp) > 0) {
          # Add points at intersections
          p <- p + geom_point(
            data = vbp_at_wtp,
            aes(x = default_wtp, y = .data$vbp_price, color = .data$comparator_label),
            size = 3,
            show.legend = FALSE
          )

          # Add labels with VBP values, staggered vertically
          vbp_at_wtp <- vbp_at_wtp %>%
            arrange(.data$group, desc(.data$vbp_price)) %>%
            group_by(.data$group) %>%
            mutate(vjust_offset = row_number() * 1.5) %>%
            ungroup()

          p <- p + geom_text(
            data = vbp_at_wtp,
            aes(x = default_wtp, y = .data$vbp_price,
                label = paste0(.data$comparator_label, ": ", comma(round(.data$vbp_price)))),
            hjust = -0.05,
            vjust = vbp_at_wtp$vjust_offset,
            size = 2.8,
            color = "gray30",
            show.legend = FALSE
          )
        }
      }
    }
  }

  # Facet if multiple groups
  n_groups <- length(unique(plot_data$group))
  if (n_groups > 1) {
    p <- p + facet_wrap(~ group, scales = "fixed")
  }

  p
}
