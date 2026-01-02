#' Convert use_display_names to field name
#'
#' Internal helper to convert boolean use_display_names parameter to field name.
#'
#' @param use_display_names Logical. If TRUE, use "display_name", else use "name"
#'
#' @return Character string: "display_name" or "name"
#' @keywords internal
field_from_display_names <- function(use_display_names = TRUE) {
  if (use_display_names) "display_name" else "name"
}

#' Filter Column Names by Value Type
#'
#' Internal helper to filter value column names based on their type (cost/outcome).
#'
#' @param column_names Character vector of column names to filter
#' @param metadata Results metadata containing values information
#' @param value_type Type to filter: "all", "cost", or "outcome"
#'
#' @return Character vector of filtered column names
#' @keywords internal
filter_by_value_type <- function(column_names, metadata, value_type = "all") {
  if (value_type == "all") {
    return(column_names)
  }

  if (is.null(metadata) || is.null(metadata$values)) {
    warning("No value metadata available for type filtering. Returning all columns.")
    return(column_names)
  }

  # Get values that match the requested type
  matching_values <- metadata$values %>%
    filter(.data$type == value_type) %>%
    pull(.data$name)

  # Return intersection of column names and matching values
  intersect(column_names, matching_values)
}


#' Map Value Names Using Metadata
#'
#' Internal helper to map technical value names to display names based on metadata.
#'
#' @param names Character vector of technical names to map
#' @param metadata Results metadata containing values information
#' @param field Which field to use: "name" or "display_name"
#'
#' @return Character vector of mapped names
#' @keywords internal
map_value_names <- function(names, metadata, field = "name") {
  # If no metadata or empty, return original names
  if (is.null(metadata) || is.null(metadata$values) || is.null(field)) {
    return(names)
  }

  values_meta <- metadata$values

  # Check if field exists
  if (!field %in% colnames(values_meta)) {
    warning(sprintf("Field '%s' not found in value metadata, using 'name' instead", field))
    field <- "name"
  }

  # Create mapping
  mapped_names <- names
  for (i in seq_along(names)) {
    row_idx <- which(values_meta$name == names[i])
    if (length(row_idx) > 0) {
      mapped_value <- values_meta[[field]][row_idx[1]]
      if (!is.na(mapped_value) && mapped_value != "") {
        mapped_names[i] <- mapped_value
      }
    }
  }

  mapped_names
}


#' Extract Values Data from Model Results
#'
#' Extracts cycle-by-cycle value data (costs and outcomes) from model results
#' in various formats suitable for visualization and export.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param format Output format: "long" (one row per cycle×value×strategy×group) or
#'   "wide" (one row per cycle, values as columns)
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (aggregated, default)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups (no overall)
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param values Character vector of value names to include (NULL for all)
#' @param value_type Filter by value type: "all" (default), "cost", or "outcome"
#' @param discounted Logical. If TRUE, use discounted values. If FALSE (default),
#'   use undiscounted values
#' @param cycles Integer vector of cycles to include (NULL for all)
#' @param time_unit Time unit for output: "cycle" (default), "day", "week", "month", "year"
#' @param use_display_names Logical. If TRUE (default), use display names for all entities
#'   (strategies, groups, values). If FALSE, use technical names.
#' @param interventions Character vector of intervention strategy names (optional filter)
#' @param comparators Character vector of comparator strategy names (optional filter)
#'
#' @return A data frame in the requested format
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Get overall population outcomes in long format
#' outcomes_long <- get_values(results, value_type = "outcome")
#'
#' # Get cost data for a specific group
#' costs_group <- get_values(results, groups = "moderate", value_type = "cost")
#'
#' # Get all groups plus overall for faceting
#' all_data <- get_values(results, groups = NULL)
#'
#' # Get multiple specific groups + overall
#' selected_data <- get_values(results, groups = c("overall", "moderate", "severe"))
#'
#' # Use technical names instead of display names
#' outcomes_tech <- get_values(results, use_display_names = FALSE)
#' }
#'
#' @export
get_values <- function(results,
                      format = c("long", "wide"),
                      groups = "overall",
                      strategies = NULL,
                      values = NULL,
                      value_type = c("all", "cost", "outcome"),
                      discounted = FALSE,
                      cycles = NULL,
                      time_unit = c("cycle", "day", "week", "month", "year"),
                      use_display_names = TRUE,
                      interventions = NULL,
                      comparators = NULL) {

  format <- match.arg(format)
  value_type <- match.arg(value_type)
  time_unit <- match.arg(time_unit)

  # Validate mutual exclusivity of strategies with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'")
  }

  # Convert use_display_names to field names
  name_field <- field_from_display_names(use_display_names)

  # Validate interventions/comparators parameters (use technical names)
  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: explicit N×M comparisons
    # Validate all strategies exist
    all_strats <- c(interventions, comparators)
    check_strategies_exist(all_strats, results$metadata)
  } else if (!is.null(interventions) || !is.null(comparators)) {
    # Only one provided: validate those strategies
    provided_strats <- if (!is.null(interventions)) interventions else comparators
    check_strategies_exist(provided_strats, results$metadata)
  }

  # Select source data based on groups parameter
  source_data <- select_source_data(groups, results)

  # Filter by strategy if specified
  if (!is.null(strategies)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies, results$metadata)
    source_data <- source_data %>% filter(.data$strategy %in% strategies)
  }

  if (nrow(source_data) == 0) {
    stop("No data remaining after filtering")
  }

  # Determine which values field to use (discounted or not)
  values_field <- if (discounted) "values_discounted" else "values"

  # Extract values based on format
  if (format == "wide") {
    result <- extract_values_wide(source_data, values_field, values, value_type,
                                  cycles, time_unit, results$metadata,
                                  name_field)
  } else {
    result <- extract_values_long(source_data, values_field, values, value_type,
                                 cycles, time_unit, results$metadata,
                                 name_field)
  }

  # Calculate differences if interventions/comparators provided
  differences_created <- FALSE
  if (!is.null(interventions) || !is.null(comparators)) {
    # Get all strategies from source data (technical names)
    all_strategies <- unique(source_data$strategy)

    # Determine comparison pairs based on DSA pattern
    comparison_pairs <- list()

    if (!is.null(interventions) && !is.null(comparators)) {
      # Both provided: N×M explicit comparisons
      for (int_strat in interventions) {
        for (comp_strat in comparators) {
          # Skip self-comparisons
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

    if (format == "wide") {
      # For wide format: calculate differences and create new columns
      diff_results <- list()

      for (pair_idx in seq_along(comparison_pairs)) {
        pair <- comparison_pairs[[pair_idx]]
        int_strat <- pair$intervention
        comp_strat <- pair$comparator

        # Map names for comparison label
        int_mapped <- map_names(int_strat, results$metadata$strategies, name_field)
        comp_mapped <- map_names(comp_strat, results$metadata$strategies, name_field)
        comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

        # Calculate difference (intervention - comparator)
        int_data <- result[result$strategy == int_mapped, ]
        comp_data <- result[result$strategy == comp_mapped, ]

        if (nrow(int_data) > 0 && nrow(comp_data) > 0) {
          diff_data <- int_data
          diff_data$strategy <- comp_label
          # Calculate differences for value columns
          value_cols <- setdiff(colnames(result), c("strategy", "group", "cycle", "day", "week", "month", "year"))
          for (col in value_cols) {
            diff_data[[col]] <- int_data[[col]] - comp_data[[col]]
          }
          diff_results[[pair_idx]] <- diff_data
        }
      }

      result <- bind_rows(diff_results)
      differences_created <- TRUE

    } else {
      # For long format: pivot, calculate, pivot back
      result_wide <- result %>%
        pivot_wider(
          names_from = "strategy",
          values_from = "amount",
          values_fill = NA
        )

      diff_data <- list()
      for (pair_idx in seq_along(comparison_pairs)) {
        pair <- comparison_pairs[[pair_idx]]
        int_strat <- pair$intervention
        comp_strat <- pair$comparator

        # Map names for comparison label and column access
        int_mapped <- map_names(int_strat, results$metadata$strategies, name_field)
        comp_mapped <- map_names(comp_strat, results$metadata$strategies, name_field)
        comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

        # Calculate difference
        result_wide[[comp_label]] <- result_wide[[int_mapped]] - result_wide[[comp_mapped]]
        diff_data[[pair_idx]] <- comp_label
      }

      # Keep only difference columns and pivot back
      time_cols <- c("cycle", "day", "week", "month", "year")
      time_col_present <- intersect(time_cols, colnames(result_wide))

      result <- result_wide %>%
        select("group", all_of(time_col_present), "value_name", all_of(unlist(diff_data))) %>%
        pivot_longer(
          cols = all_of(unlist(diff_data)),
          names_to = "strategy",
          values_to = "amount"
        )

      differences_created <- TRUE
    }
  }

  # Remap names for display if metadata available AND we didn't already create comparison strings
  if (!is.null(results$metadata) && !differences_created) {
    if (format == "wide") {
      # Wide format doesn't have strategy column to remap here
    } else {
      if (!is.null(results$metadata$strategies) && name_field != "name") {
        result$strategy <- map_names(result$strategy, results$metadata$strategies, name_field)
      }
    }
  }

  result
}


#' Extract Values as Wide Format
#' @keywords internal
extract_values_wide <- function(source_data, values_field, values_filter,
                                value_type, cycles, time_unit, metadata,
                                name_field = "display_name") {

  dfs <- lapply(seq_len(nrow(source_data)), function(i) {
    # Extract values from trace_and_values nested structure
    values_data <- source_data$trace_and_values[[i]][[values_field]]

    if (is.null(values_data)) {
      return(NULL)
    }

    # Convert to data frame if matrix and add cycle column
    if (is.matrix(values_data)) {
      values_data <- as.data.frame(values_data)
      # Add cycle column based on row indices
      values_data$cycle <- seq_len(nrow(values_data))
    }

    # Get value column names (exclude time columns)
    time_cols <- c("cycle", "day", "week", "month", "year")
    value_cols <- setdiff(colnames(values_data), time_cols)

    # Filter by value type
    value_cols <- filter_by_value_type(value_cols, metadata, value_type)

    # Filter to specific values if requested
    if (!is.null(values_filter)) {
      value_cols <- intersect(value_cols, values_filter)
    }

    # Select columns to keep
    cols_to_keep <- c(intersect(time_cols, colnames(values_data)), value_cols)
    values_data <- values_data[, cols_to_keep, drop = FALSE]

    # Filter cycles if requested
    if (!is.null(cycles)) {
      if ("cycle" %in% colnames(values_data)) {
        values_data <- values_data[values_data$cycle %in% cycles, , drop = FALSE]
      }
    }

    # Add strategy and group
    values_data$strategy <- source_data$strategy[i]
    values_data$group <- source_data$group[i]

    # Reorder columns
    time_col_present <- intersect(c("cycle", "day", "week", "month", "year"), colnames(values_data))
    values_data <- values_data[, c("strategy", "group", time_col_present, value_cols)]

    values_data
  })

  # Remove NULLs and combine
  dfs <- dfs[!sapply(dfs, is.null)]
  result <- do.call(rbind, dfs)

  # Map names for display
  if (!is.null(metadata)) {
    if (!is.null(metadata$strategies) && name_field != "name") {
      result$strategy <- map_names(result$strategy, metadata$strategies, name_field)
    }
    if (!is.null(metadata$groups) && name_field != "name") {
      result$group <- map_names(result$group, metadata$groups, name_field)
    }
    # For wide format, map column names
    if (!is.null(metadata$values) && name_field != "name") {
      value_cols <- setdiff(colnames(result), c("strategy", "group", "cycle", "day", "week", "month", "year"))
      colnames(result)[colnames(result) %in% value_cols] <-
        map_value_names(value_cols, metadata, name_field)
    }
  }

  result
}


#' Extract Values as Long Format
#' @keywords internal
extract_values_long <- function(source_data, values_field, values_filter,
                                value_type, cycles, time_unit, metadata,
                                name_field = "display_name") {

  dfs <- lapply(seq_len(nrow(source_data)), function(i) {
    # Extract values from trace_and_values nested structure
    values_data <- source_data$trace_and_values[[i]][[values_field]]

    if (is.null(values_data)) {
      return(NULL)
    }

    # Convert to data frame if matrix and add cycle column
    if (is.matrix(values_data)) {
      values_data <- as.data.frame(values_data)
      # Add cycle column based on row indices
      values_data$cycle <- seq_len(nrow(values_data))
    }

    # Determine which time column to use
    time_col <- switch(time_unit,
                      "cycle" = "cycle",
                      "day" = "day",
                      "week" = "week",
                      "month" = "month",
                      "year" = "year",
                      "cycle")

    # Check if time column exists - for now, we only have cycle
    if (!time_col %in% colnames(values_data)) {
      if (time_unit != "cycle") {
        warning(sprintf("Time unit '%s' not available, using cycle instead", time_unit))
      }
      time_col <- "cycle"
    }

    # Get value column names
    time_cols <- c("cycle", "day", "week", "month", "year")
    value_cols <- setdiff(colnames(values_data), time_cols)

    # Filter by value type
    value_cols <- filter_by_value_type(value_cols, metadata, value_type)

    # Filter to specific values if requested
    if (!is.null(values_filter)) {
      value_cols <- intersect(value_cols, values_filter)
    }

    # Select time and value columns
    df <- values_data[, c(time_col, value_cols), drop = FALSE]
    names(df)[1] <- "time"

    # Filter cycles if requested
    if (!is.null(cycles)) {
      if ("cycle" %in% colnames(values_data)) {
        df <- df[values_data$cycle %in% cycles, , drop = FALSE]
      }
    }

    # Add strategy and group
    df$strategy <- source_data$strategy[i]
    df$group <- source_data$group[i]

    # Reshape to long
    df_long <- pivot_longer(
      df,
      cols = -c("strategy", "group", "time"),
      names_to = "value_name",
      values_to = "amount"
    )

    # Rename time column to requested time unit
    time_col_name <- switch(time_unit,
                           "cycle" = "cycle",
                           "day" = "day",
                           "week" = "week",
                           "month" = "month",
                           "year" = "year",
                           "cycle")

    names(df_long)[names(df_long) == "time"] <- time_col_name

    df_long
  })

  # Remove NULLs and combine
  dfs <- dfs[!sapply(dfs, is.null)]
  result <- do.call(rbind, dfs)

  # Map names for display
  if (!is.null(metadata)) {
    if (!is.null(metadata$strategies) && name_field != "name") {
      result$strategy <- map_names(result$strategy, metadata$strategies, name_field)
    }
    if (!is.null(metadata$groups) && name_field != "name") {
      result$group <- map_names(result$group, metadata$groups, name_field)
    }
    # For long format, map value_name column
    if (!is.null(metadata$values) && name_field != "name") {
      result$value_name <- map_value_names(result$value_name, metadata, name_field)
    }
  }

  result
}


#' Extract Summary Data from Model Results
#'
#' Extracts summary totals (e.g., total costs, total QALYs) from model results.
#'
#' @inheritParams get_values
#' @param summaries Character vector of summary names to include (NULL for all)
#'
#' @return A data frame with columns: strategy, group, summary, value, amount
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Get all summaries (overall population)
#' summaries <- get_summaries(results)
#'
#' # Get cost summaries for all groups plus overall
#' cost_summaries <- get_summaries(results, groups = NULL, value_type = "cost")
#' }
#'
#' @export
get_summaries <- function(results,
                         groups = "overall",
                         strategies = NULL,
                         summaries = NULL,
                         values = NULL,
                         value_type = c("all", "cost", "outcome"),
                         discounted = FALSE,
                         use_display_names = TRUE,
                         interventions = NULL,
                         comparators = NULL) {

  value_type <- match.arg(value_type)

  # Validate mutual exclusivity of strategies with interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'")
  }

  # Convert use_display_names to field names
  name_field <- field_from_display_names(use_display_names)

  # Validate interventions/comparators parameters (use technical names)
  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: explicit N×M comparisons
    all_strats <- c(interventions, comparators)
    check_strategies_exist(all_strats, results$metadata)
  } else if (!is.null(interventions) || !is.null(comparators)) {
    # Only one provided: validate those strategies
    provided_strats <- if (!is.null(interventions)) interventions else comparators
    check_strategies_exist(provided_strats, results$metadata)
  }

  # Select source data based on groups parameter
  source_data <- select_source_data(groups, results)

  # Filter by strategy if specified
  if (!is.null(strategies)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies, results$metadata)
    source_data <- source_data %>% filter(.data$strategy %in% strategies)
  }

  if (nrow(source_data) == 0) {
    stop("No data remaining after filtering")
  }

  # Determine which summary field to use
  summary_field <- if (discounted) "summaries_discounted" else "summaries"

  # Extract summaries from each row
  summary_dfs <- lapply(seq_len(nrow(source_data)), function(i) {
    summary_data <- source_data[[summary_field]][[i]]

    if (is.null(summary_data) || nrow(summary_data) == 0) {
      return(NULL)
    }

    # Add strategy and group
    summary_data$strategy <- source_data$strategy[i]
    summary_data$group <- source_data$group[i]

    summary_data
  })

  # Remove NULLs and combine
  summary_dfs <- summary_dfs[!sapply(summary_dfs, is.null)]
  combined <- do.call(rbind, summary_dfs)

  # Filter by summary names if specified
  if (!is.null(summaries)) {
    # Validate that all requested summaries exist using the helper
    # This will throw an informative error with table if any are missing
    for (summary_name in summaries) {
      check_summary_exists(summary_name, results$metadata)
    }
    combined <- combined %>% filter(summary %in% summaries)
  }

  # Filter by value names if specified
  if (!is.null(values)) {
    combined <- combined %>% filter(value %in% values)
  }

  # Filter by value type
  if (value_type != "all") {
    # Get values that match the type
    if (!is.null(results$metadata) && !is.null(results$metadata$values)) {
      matching_values <- results$metadata$values %>%
        filter(.data$type == value_type) %>%
        pull(.data$name)

      combined <- combined %>% filter(value %in% matching_values)
    } else {
      warning("No value metadata available for type filtering.")
    }
  }

  # Validate that we have data after all filtering
  if (nrow(combined) == 0) {
    # Since summaries are now validated above with check_summary_exists(),
    # if we get here with no data, it's due to values or value_type filtering
    filter_description <- c()
    if (!is.null(values)) filter_description <- c(filter_description, paste("values:", paste(values, collapse = ", ")))
    if (value_type != "all") filter_description <- c(filter_description, paste("value_type:", value_type))

    error_msg <- sprintf("No data found matching filters: %s", paste(filter_description, collapse = "; "))
    stop(error_msg)
  }

  # Reorder columns
  combined <- combined %>%
    select("strategy", "group", "summary", "value", "amount")

  # Calculate differences if interventions/comparators provided
  differences_created <- FALSE
  if (!is.null(interventions) || !is.null(comparators)) {
    # Get all strategies from source data (technical names)
    all_strategies <- unique(combined$strategy)

    # Determine comparison pairs based on DSA pattern
    comparison_pairs <- list()

    if (!is.null(interventions) && !is.null(comparators)) {
      # Both provided: N×M explicit comparisons
      for (int_strat in interventions) {
        for (comp_strat in comparators) {
          # Skip self-comparisons
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

    # Pivot to wide format (strategies as columns with technical names)
    combined_wide <- combined %>%
      pivot_wider(names_from = "strategy", values_from = "amount", values_fill = NA)

    # Calculate differences for each comparison pair
    diff_cols <- character()
    for (pair_idx in seq_along(comparison_pairs)) {
      pair <- comparison_pairs[[pair_idx]]
      int_strat <- pair$intervention
      comp_strat <- pair$comparator

      # Map names for comparison label
      int_mapped <- map_names(int_strat, results$metadata$strategies, name_field)
      comp_mapped <- map_names(comp_strat, results$metadata$strategies, name_field)
      comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

      # Calculate difference (intervention - comparator)
      combined_wide[[comp_label]] <- combined_wide[[int_strat]] - combined_wide[[comp_strat]]
      diff_cols <- c(diff_cols, comp_label)
    }

    # Keep only difference columns and pivot back
    combined <- combined_wide %>%
      select("group", "summary", "value", all_of(diff_cols)) %>%
      pivot_longer(cols = all_of(diff_cols),
                   names_to = "strategy",
                   values_to = "amount")

    differences_created <- TRUE
  }

  # Map names for display if metadata available
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies) && name_field != "name" &&
        !differences_created) {
      # Only map if we didn't already create comparison strings with mapped names
      combined$strategy <- map_names(combined$strategy, results$metadata$strategies, name_field)
    }
    if (!is.null(results$metadata$groups) && name_field != "name") {
      combined$group <- map_names(combined$group, results$metadata$groups, name_field)
    }
    if (!is.null(results$metadata$values) && name_field != "name") {
      combined$value <- map_value_names(combined$value, results$metadata, name_field)
    }
  }

  combined
}


#' Calculate Net Monetary Benefit
#'
#' Calculates Net Monetary Benefit (NMB) as: (Difference in Outcomes × WTP) - Difference in Costs
#' Requires comparison between strategies using either intervention or comparator.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param outcome_summary Name of the outcome summary (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary (e.g., "total_cost")
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL
#' @param interventions Character vector of reference strategies for intervention perspective (e.g., "new_treatment").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with comparators.
#' @param comparators Character vector of reference strategies for comparator perspective (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with interventions.
#' @param wtp Optional override for willingness-to-pay value. If NULL, attempts to extract from
#'   outcome summary metadata. Must be numeric and positive.
#' @param discounted Logical. If TRUE, use discounted values. If FALSE (default),
#'   use undiscounted values
#' @param use_display_names Logical. If TRUE (default), use display names for entities
#' @param return_components Logical. If TRUE, returns breakdown of Outcome Benefit,
#'   Cost Difference, and Total. If FALSE (default), returns only total NMB.
#'
#' @return A data frame with columns:
#'   - If return_components = FALSE: strategy, group, nmb_amount
#'   - If return_components = TRUE: strategy, group, component, amount
#'     where component is one of "Outcome Benefit", "Cost Difference", "Total"
#'
#' @details
#' One of `intervention` or `comparator` must be provided. The function calculates NMB as:
#' - When intervention specified: (intervention_outcome - other_outcome) × WTP - (intervention_cost - other_cost)
#' - When comparator specified: (other_outcome - comparator_outcome) × WTP - (other_cost - comparator_cost)
#'
#' WTP is automatically extracted from the outcome summary metadata if available,
#' but can be overridden using the `wtp` parameter.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Calculate NMB with WTP from metadata
#' nmb <- calculate_nmb(
#'   results,
#'   outcome_summary = "total_qalys",
#'   cost_summary = "total_cost",
#'   comparator = "control"
#' )
#'
#' # Calculate NMB with explicit WTP override
#' nmb <- calculate_nmb(
#'   results,
#'   outcome_summary = "total_qalys",
#'   cost_summary = "total_cost",
#'   intervention = "new_treatment",
#'   wtp = 50000
#' )
#' }
#'
#' @export
calculate_nmb <- function(results,
                          outcome_summary,
                          cost_summary,
                          groups = "overall",
                          interventions = NULL,
                          comparators = NULL,
                          wtp = NULL,
                          discounted = FALSE,
                          use_display_names = TRUE,
                          return_components = FALSE) {

  # Validate that at least one of interventions or comparators is provided
  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Get outcome and cost summaries
  outcome_data <- get_summaries(
    results,
    groups = groups,
    summaries = outcome_summary,
    value_type = "outcome",
    discounted = discounted,
    use_display_names = use_display_names
  )

  cost_data <- get_summaries(
    results,
    groups = groups,
    summaries = cost_summary,
    value_type = "cost",
    discounted = discounted,
    use_display_names = use_display_names
  )

  # Aggregate outcome and cost by strategy and group (sum across value components)
  # NOTE: outcome_data and cost_data have technical names in strategy column
  outcome_agg <- outcome_data %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(outcome_total = sum(.data$amount, na.rm = TRUE), .groups = 'drop')

  cost_agg <- cost_data %>%
    group_by(.data$strategy, .data$group) %>%
    summarize(cost_total = sum(.data$amount, na.rm = TRUE), .groups = 'drop')

  # Get WTP value
  if (is.null(wtp)) {
    # Extract WTP from outcome summary metadata
    if (is.null(results$metadata) || is.null(results$metadata$summaries)) {
      stop("Cannot extract WTP from metadata. Metadata not available.")
    }

    # Check if outcome summary exists using validation helper
    check_summary_exists(outcome_summary, results$metadata)

    outcome_meta <- results$metadata$summaries %>%
      filter(.data$name == outcome_summary)

    wtp <- outcome_meta$wtp[1]

    if (length(wtp) == 0 || is.na(wtp)) {
      stop(sprintf("WTP not found for outcome summary '%s'. Provide explicit wtp parameter.", outcome_summary))
    }
  }

  # Validate WTP
  if (!is.numeric(wtp) || is.na(wtp)) {
    stop("WTP must be a numeric value")
  }

  # Pivot to wide format using TECHNICAL names
  outcome_wide <- outcome_agg %>%
    pivot_wider(names_from = "strategy", values_from = "outcome_total", values_fill = NA)

  cost_wide <- cost_agg %>%
    pivot_wider(names_from = "strategy", values_from = "cost_total", values_fill = NA)

  # Get all strategies (technical names from combined data)
  all_strategies <- unique(c(unique(outcome_agg$strategy), unique(cost_agg$strategy)))

  # Determine comparison pairs based on DSA pattern
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N×M explicit comparisons
    for (int_strat in interventions) {
      for (comp_strat in comparators) {
        # Skip self-comparisons
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

  # Map strategy names for display
  name_field <- field_from_display_names(use_display_names)

  # Initialize results
  if (return_components) {
    nmb_results <- data.frame(
      strategy = character(),
      group = character(),
      component = character(),
      amount = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    nmb_results <- data.frame(
      strategy = character(),
      group = character(),
      nmb_amount = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # Calculate NMB for each comparison pair
  for (pair_idx in seq_along(comparison_pairs)) {
    pair <- comparison_pairs[[pair_idx]]
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    # Create comparison label with display names
    int_mapped <- map_names(int_strat, results$metadata$strategies, name_field)
    comp_mapped <- map_names(comp_strat, results$metadata$strategies, name_field)
    comp_label <- paste0(int_mapped, " vs. ", comp_mapped)

    for (grp in unique(outcome_wide$group)) {
      # Access outcome values using technical names
      outcome_int <- outcome_wide %>%
        filter(.data$group == grp) %>%
        pull(!!int_strat)

      outcome_comp <- outcome_wide %>%
        filter(.data$group == grp) %>%
        pull(!!comp_strat)

      # Access cost values using technical names
      cost_int <- cost_wide %>%
        filter(.data$group == grp) %>%
        pull(!!int_strat)

      cost_comp <- cost_wide %>%
        filter(.data$group == grp) %>%
        pull(!!comp_strat)

      # Handle empty results
      if (length(outcome_int) == 0) outcome_int <- NA_real_
      if (length(outcome_comp) == 0) outcome_comp <- NA_real_
      if (length(cost_int) == 0) cost_int <- NA_real_
      if (length(cost_comp) == 0) cost_comp <- NA_real_

      # Calculate differences (always intervention - comparator)
      outcome_diff <- if (!is.na(outcome_int) && !is.na(outcome_comp)) {
        outcome_int - outcome_comp
      } else {
        NA_real_
      }

      cost_diff <- if (!is.na(cost_int) && !is.na(cost_comp)) {
        cost_int - cost_comp
      } else {
        NA_real_
      }

      # Calculate NMB
      nmb <- (outcome_diff * wtp) - cost_diff

      if (return_components) {
        # Add three rows: outcome benefit, cost difference, and total
        nmb_results <- rbind(nmb_results, data.frame(
          strategy = comp_label,
          group = grp,
          component = "Outcome Benefit",
          amount = outcome_diff * wtp,
          stringsAsFactors = FALSE
        ))

        nmb_results <- rbind(nmb_results, data.frame(
          strategy = comp_label,
          group = grp,
          component = "Cost Difference",
          amount = -cost_diff,
          stringsAsFactors = FALSE
        ))

        nmb_results <- rbind(nmb_results, data.frame(
          strategy = comp_label,
          group = grp,
          component = "Total",
          amount = nmb,
          stringsAsFactors = FALSE
        ))
      } else {
        nmb_results <- rbind(nmb_results, data.frame(
          strategy = comp_label,
          group = grp,
          nmb_amount = nmb,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Map group names at the end (follows get_summaries pattern, lines 563-564)
  if (!is.null(results$metadata) && !is.null(results$metadata$groups) &&
      name_field != "name") {
    nmb_results$group <- map_names(nmb_results$group, results$metadata$groups, name_field)
  }

  # Factor component column if returning components
  if (return_components) {
    nmb_results <- nmb_results %>%
      mutate(component = factor(.data$component, levels = c("Outcome Benefit", "Cost Difference", "Total")))
  }

  as_tibble(nmb_results)
}
