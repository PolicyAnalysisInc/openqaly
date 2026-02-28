#' Transition Matrix Data Extraction
#'
#' Functions for extracting transition probability data from model results.
#'
#' @name transition_output
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
NULL


#' Extract Transition Data from Model Results
#'
#' Extracts conditional transition probabilities from model results into
#' data frames suitable for visualization and export. Transition data is
#' available per-segment (not aggregated), so when \code{groups = "overall"},
#' transitions are taken from the first group's segment for each strategy
#' (transition probabilities are identical across groups since formulas
#' don't depend on group).
#'
#' @param results A openqaly model results object (output from run_model)
#' @param format Output format: "long" (one row per cycle x from_state x to_state)
#'   or "wide" (one row per cycle, from->to pairs as columns)
#' @param collapsed Logical. If TRUE (default), use collapsed state names.
#'   If FALSE, use expanded state names (tunnel states).
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param groups Group selection:
#'   \itemize{
#'     \item \code{"overall"} - Overall population (default, uses first group's transitions)
#'     \item \code{"group_name"} - Specific group by name
#'     \item \code{c("group1", "group2")} - Multiple specific groups
#'     \item \code{c("overall", "group1")} - Specific groups + overall
#'     \item \code{"all"} or \code{NULL} - All groups + overall
#'     \item \code{"all_groups"} - All groups without overall
#'   }
#' @param from_states Character vector of from-state names to include (NULL for all)
#' @param to_states Character vector of to-state names to include (NULL for all)
#' @param cycles Integer vector of cycles to include (NULL for all)
#' @param time_unit Time unit for output: "cycle" (default), "day", "week", "month", "year"
#' @param use_display_names Logical. If TRUE (default), use display names for entities
#' @param state_times Numeric vector of tunnel state indices to include when
#'   \code{collapsed=FALSE}. Use \code{Inf} for the last tunnel state of each
#'   base state. Non-tunnel states are always included. Ignored when
#'   \code{collapsed=TRUE}.
#' @param exclude_zero_residency Logical. Exclude expanded states with zero
#'   residency at the plotted cycle. Defaults to TRUE when \code{collapsed=FALSE},
#'   FALSE when \code{collapsed=TRUE}. Requires \code{cycles} to be specified.
#'
#' @return A data frame with columns depending on format:
#'   \itemize{
#'     \item Long: strategy, group, cycle, [time_unit], from_state, to_state, probability
#'     \item Wide: strategy, group, cycle, [time_unit], from->to pair columns
#'   }
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "openqaly"))
#' results <- run_model(model)
#'
#' # Get long format transition data
#' trans_long <- get_transitions(results, format = "long")
#'
#' # Get transitions for specific cycle
#' trans_c1 <- get_transitions(results, cycles = 1)
#'
#' # Get wide format
#' trans_wide <- get_transitions(results, format = "wide")
#' }
#'
#' @export
get_transitions <- function(results,
                            format = c("long", "wide"),
                            collapsed = TRUE,
                            strategies = NULL,
                            groups = "overall",
                            from_states = NULL,
                            to_states = NULL,
                            cycles = NULL,
                            time_unit = c("cycle", "day", "week", "month", "year"),
                            use_display_names = TRUE,
                            state_times = NULL,
                            exclude_zero_residency = NULL) {

  format <- match.arg(format)
  time_unit <- match.arg(time_unit)

  # Convert use_display_names to field names
  name_field <- field_from_display_names(use_display_names)

  # Select source data based on groups parameter
  source_data <- select_source_data(groups, results)

  # For aggregated rows (group == "_aggregated" or "Overall"), transitions

  # are not stored. Fall back to segments filtered to matching strategies.
  has_aggregated <- any(source_data$group %in% c("_aggregated", "Overall"))
  if (has_aggregated) {
    # Get the strategies from aggregated rows
    agg_rows <- source_data[source_data$group %in% c("_aggregated", "Overall"), ]
    agg_strategies <- unique(agg_rows$strategy)

    # Get non-aggregated rows
    non_agg_rows <- source_data[!source_data$group %in% c("_aggregated", "Overall"), ]

    # For each aggregated strategy, take the first group's segment
    fallback_rows <- results$segments[
      results$segments$strategy %in% agg_strategies, ]
    # Take only the first group per strategy
    first_groups <- fallback_rows[!duplicated(fallback_rows$strategy), ]
    # Mark these as "Overall" for display
    first_groups$group <- "Overall"

    # Combine with any non-aggregated rows
    if (nrow(non_agg_rows) > 0) {
      source_data <- bind_rows(first_groups, non_agg_rows)
    } else {
      source_data <- first_groups
    }
  }

  # Filter by strategy
  if (!is.null(strategies)) {
    source_data <- source_data[source_data$strategy %in% strategies, ]
  }

  if (nrow(source_data) == 0) {
    stop("No data remaining after filtering")
  }

  # Resolve defaults for state filtering
  if (is.null(exclude_zero_residency)) {
    exclude_zero_residency <- !collapsed
  }

  # Warn on misuse
  internal_state_filter <- NULL
  if (!collapsed) {
    if (!is.null(state_times)) {
      # Gather all expanded state names from transitions
      all_expanded <- character(0)
      for (i in seq_len(nrow(source_data))) {
        trans <- source_data$trace_and_values[[i]]$transitions
        if (!is.null(trans) && nrow(trans) > 0) {
          all_expanded <- union(all_expanded, union(trans$from_expanded, trans$to_expanded))
        }
      }
      internal_state_filter <- filter_states_by_time(all_expanded, state_times)
    }

    if (exclude_zero_residency) {
      if (is.null(cycles)) {
        warning("exclude_zero_residency requires 'cycles' to be specified; skipping")
      } else {
        # Gather all expanded state names if not already done
        if (is.null(internal_state_filter)) {
          all_expanded <- character(0)
          for (i in seq_len(nrow(source_data))) {
            trans <- source_data$trace_and_values[[i]]$transitions
            if (!is.null(trans) && nrow(trans) > 0) {
              all_expanded <- union(all_expanded, union(trans$from_expanded, trans$to_expanded))
            }
          }
          internal_state_filter <- all_expanded
        }
        internal_state_filter <- filter_states_by_residency(
          internal_state_filter, source_data, cycles
        )
      }
    }
  } else {
    if (!is.null(state_times)) {
      warning("state_times is ignored when collapsed=TRUE")
    }
  }

  # Extract transition data in long format
  result <- extract_transitions_long(source_data, collapsed, results$metadata, name_field,
                                     internal_state_filter = internal_state_filter)

  # Filter by from_states
  if (!is.null(from_states)) {
    result <- result[result$from_state %in% from_states, ]
  }

  # Filter by to_states
  if (!is.null(to_states)) {
    result <- result[result$to_state %in% to_states, ]
  }

  # Filter by cycles
  if (!is.null(cycles)) {
    result <- result[result$cycle %in% cycles, ]
  }

  # Add time unit columns
  result <- add_time_columns(result, time_unit, results)

  if (nrow(result) == 0) {
    stop("No data remaining after filtering")
  }

  # Convert to wide format if requested

  if (format == "wide") {
    # Create from->to pair column names
    result <- pivot_wider(
      result,
      names_from = c("from_state", "to_state"),
      values_from = "probability",
      names_sep = " -> "
    )
  }

  result
}


#' Extract Transition Data from Segments (Long Format)
#'
#' @param source_data Data frame of segments
#' @param collapsed Logical. Use collapsed or expanded state names.
#' @param metadata Model metadata
#' @param name_field Which name field to use
#' @param internal_state_filter Optional character vector of expanded state names
#'   to keep. Applied before display name mapping.
#' @return Data frame in long format
#' @keywords internal
extract_transitions_long <- function(source_data, collapsed, metadata, name_field,
                                     internal_state_filter = NULL) {
  dfs <- lapply(seq_len(nrow(source_data)), function(i) {
    tv <- source_data$trace_and_values[[i]]

    # Get transitions DataFrame from trace_and_values
    trans <- tv$transitions

    if (is.null(trans) || nrow(trans) == 0) {
      return(NULL)
    }

    # Select collapsed or expanded names
    if (collapsed) {
      from_col <- "from_collapsed"
      to_col <- "to_collapsed"
    } else {
      from_col <- "from_expanded"
      to_col <- "to_expanded"
    }

    df <- data.frame(
      strategy = source_data$strategy[i],
      group = source_data$group[i],
      cycle = trans$cycle,
      from_state = trans[[from_col]],
      to_state = trans[[to_col]],
      probability = trans$value,
      stringsAsFactors = FALSE
    )

    # When using collapsed names, aggregate expanded tunnel transitions
    if (collapsed) {
      df <- df[!duplicated(df[, c("strategy", "group", "cycle", "from_state", "to_state")]), ]
    }

    # Apply state filter on internal (expanded) names before display mapping
    if (!is.null(internal_state_filter)) {
      df <- df[df$from_state %in% internal_state_filter &
               df$to_state %in% internal_state_filter, ]
    }

    df
  })

  result <- do.call(rbind, Filter(Negate(is.null), dfs))

  if (is.null(result) || nrow(result) == 0) {
    stop("No transition data available in results")
  }

  # Map names for display
  if (!is.null(metadata)) {
    if (!is.null(metadata$strategies) && name_field != "name") {
      result$strategy <- map_names(result$strategy, metadata$strategies, name_field)
    }
    if (!is.null(metadata$groups) && name_field != "name") {
      result$group <- map_names(result$group, metadata$groups, name_field)
    }
    if (!is.null(metadata$states) && name_field != "name") {
      if (collapsed) {
        result$from_state <- map_names(result$from_state, metadata$states, name_field)
        result$to_state <- map_names(result$to_state, metadata$states, name_field)
      } else {
        result$from_state <- map_expanded_names(result$from_state, metadata$states, name_field)
        result$to_state <- map_expanded_names(result$to_state, metadata$states, name_field)
      }
    }
  }

  result
}


#' Add Time Unit Columns to Transition Data
#'
#' @param df Data frame with a cycle column
#' @param time_unit Requested time unit
#' @param results Full results object for settings access
#' @return Data frame with time columns added
#' @keywords internal
add_time_columns <- function(df, time_unit, results) {
  if (time_unit == "cycle") {
    return(df)
  }

  # Get cycle length info from model settings
  settings <- results$metadata$settings
  if (is.null(settings)) {
    warning(paste("Time unit", time_unit, "not available without model settings, using cycle instead"))
    return(df)
  }

  cycle_length_days <- settings$cycle_length_days
  if (is.null(cycle_length_days) || is.na(cycle_length_days) || cycle_length_days <= 0) {
    warning(paste("Time unit", time_unit, "not available, using cycle instead"))
    return(df)
  }

  days_per_year <- if (!is.null(settings$days_per_year)) settings$days_per_year else 365.25
  days_per_month <- days_per_year / 12

  df[[time_unit]] <- switch(time_unit,
    "day" = df$cycle * cycle_length_days,
    "week" = df$cycle * cycle_length_days / 7,
    "month" = df$cycle * cycle_length_days / days_per_month,
    "year" = df$cycle * cycle_length_days / days_per_year
  )

  df
}


#' Map Expanded State Names to Display Names
#'
#' For expanded names like "progressed.3", strips the tunnel suffix,
#' maps the base name to the display name, and re-appends the suffix.
#'
#' @param names Character vector of expanded state names
#' @param metadata States metadata data frame
#' @param field Which field to map to (e.g. "display_name")
#' @return Character vector of mapped names
#' @keywords internal
map_expanded_names <- function(names, metadata, field = "display_name") {
  pattern <- "^(.+)\\.(\\d+)$"
  has_suffix <- grepl(pattern, names)
  base_names <- ifelse(has_suffix, sub(pattern, "\\1", names), names)
  suffixes <- ifelse(has_suffix, sub(pattern, ".\\2", names), "")

  mapped_base <- map_names(base_names, metadata, field)

  ifelse(has_suffix, paste0(mapped_base, suffixes), mapped_base)
}


#' Sort Expanded State Names by Base State Order
#'
#' Orders expanded state names (e.g. "Progressed.1", "Progressed.2")
#' according to the base state order from metadata, then by tunnel index.
#'
#' @param expanded_names Character vector of expanded state names
#' @param base_order Character vector of base state names in desired order
#' @return Character vector sorted by base state position then tunnel index
#' @keywords internal
sort_expanded_states <- function(expanded_names, base_order) {
  pattern <- "^(.+)\\.(\\d+)$"
  has_suffix <- grepl(pattern, expanded_names)
  base_names <- ifelse(has_suffix, sub(pattern, "\\1", expanded_names), expanded_names)
  tunnel_idx <- ifelse(has_suffix, as.integer(sub(pattern, "\\2", expanded_names)), 1L)

  base_positions <- match(base_names, base_order)
  base_positions[is.na(base_positions)] <- length(base_order) + 1L

  order_idx <- order(base_positions, tunnel_idx)
  expanded_names[order_idx]
}


#' Filter State Names by Tunnel Index
#'
#' Keeps non-tunnel states unconditionally and tunnel states whose index
#' is in \code{state_times}. \code{Inf} maps to the maximum tunnel index
#' for each base state.
#'
#' @param state_names Character vector of expanded state names
#' @param state_times Numeric vector of tunnel indices to keep (use \code{Inf}
#'   for the last tunnel state of each base state)
#' @return Filtered character vector
#' @keywords internal
filter_states_by_time <- function(state_names, state_times) {
  pattern <- "^(.+)\\.(\\d+)$"
  has_suffix <- grepl(pattern, state_names)

  # Non-tunnel states always pass
  keep <- !has_suffix

  if (any(has_suffix)) {
    base_names <- sub(pattern, "\\1", state_names[has_suffix])
    indices <- as.integer(sub(pattern, "\\2", state_names[has_suffix]))

    # Compute max tunnel index per base state
    max_indices <- tapply(indices, base_names, max)

    # Resolve Inf -> per-base-state max
    finite_times <- state_times[is.finite(state_times)]
    has_inf <- any(is.infinite(state_times) & state_times > 0)

    tunnel_keep <- indices %in% finite_times
    if (has_inf) {
      tunnel_keep <- tunnel_keep | (indices == max_indices[base_names])
    }

    keep[has_suffix] <- tunnel_keep
  }

  state_names[keep]
}


#' Filter States by Residency in Trace
#'
#' Removes expanded states that have zero occupancy at the specified cycles.
#' Non-tunnel states always pass. A state passes if it has non-zero residency
#' in any of the relevant trace rows in any segment.
#'
#' @param state_names Character vector of expanded state names to filter
#' @param source_data Data frame of segments (with \code{expanded_trace} column)
#' @param cycles Integer vector of cycles to check
#' @return Filtered character vector
#' @keywords internal
filter_states_by_residency <- function(state_names, source_data, cycles) {
  pattern <- "^(.+)\\.(\\d+)$"
  has_suffix <- grepl(pattern, state_names)

  # Non-tunnel states always pass
  tunnel_names <- state_names[has_suffix]
  if (length(tunnel_names) == 0) return(state_names)

  # Check trace rows at cycle boundaries (start and end of each cycle)
  # Trace row index = cycle (0-indexed trace: row 1 = cycle 0 start)
  trace_rows <- unique(c(cycles, cycles + 1))
  trace_rows <- trace_rows[trace_rows >= 1]

  occupied <- character(0)

  for (i in seq_len(nrow(source_data))) {
    trace <- source_data$expanded_trace[[i]]
    if (is.null(trace)) next

    # Clamp to available rows
    valid_rows <- trace_rows[trace_rows <= nrow(trace)]
    if (length(valid_rows) == 0) next

    subset <- trace[valid_rows, , drop = FALSE]
    # Find columns with any non-zero value
    nonzero_cols <- colnames(subset)[colSums(abs(subset), na.rm = TRUE) > 0]
    occupied <- union(occupied, nonzero_cols)
  }

  # Keep non-tunnel states + tunnel states that are occupied
  keep <- !has_suffix | (state_names %in% occupied)
  state_names[keep]
}
