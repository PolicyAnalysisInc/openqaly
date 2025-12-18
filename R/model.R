#' Run Probabilistic Sensitivity Analysis
#'
#' Runs probabilistic sensitivity analysis by sampling from distributions
#' and running the model for each set of sampled parameters.
#'
#' @param model A heRomodel object with sampling specifications
#' @param n_sim Number of PSA simulations to run
#' @param seed Random seed for reproducibility
#' @param ... Additional arguments passed to run_segment
#' @return Results list with segments and aggregated results (includes simulation dimension)
#' @export
run_psa <- function(model, n_sim, seed = NULL, ...) {
  # Finalize builders (convert to heRomodel)
  if ("heRomodel_builder" %in% class(model)) {
    model <- normalize_and_validate_model(model, preserve_builder = FALSE)
  }

  # Parse model
  parsed_model <- parse_model(model, ...)

  # Validate sampling specifications
  validate_sampling_spec(parsed_model)

  # Get simple segments (strategy × group)
  segments <- get_segments(parsed_model)

  # Enrich segments with evaluated variables
  # This is needed so distribution formulas can reference variable values
  enriched_segments <- segments %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      prepare_segment_for_sampling(parsed_model, as_tibble(seg))
    }) %>%
    ungroup()

  # Resample to get sampled variable values for each simulation
  sampled_vars <- resample(parsed_model, n = n_sim, segments = enriched_segments, seed = seed)

  # Run segments with sampled values
  # sampled_vars has columns: strategy, group, simulation, var1, var2, ...
  res <- list()

  res$segments <- sampled_vars %>%
    rowwise() %>%
    group_split() %>%
    future_map(function(segment) run_segment(segment, parsed_model, ...), .progress = TRUE, .options = furrr_options(seed=1)) %>%
    bind_rows()

  # Aggregate by simulation + strategy
  res$aggregated <- aggregate_segments(res$segments, parsed_model)

  # Store metadata
  res$metadata <- parsed_model$metadata

  return(res)
}

#' Run a Model
#'
#' Takes a model specification object and runs the model for the base case.
#' For probabilistic sensitivity analysis, use [run_psa()] instead.
#'
#' @param model A heRo_model object.
#' @param ... additional arguments.
#'
#' @return A list containing the results of the model.
#'
#' @export
run_model <- function(model, ...) {

  # Base case: existing logic
  # Finalize builders (convert to heRomodel)
  if ("heRomodel_builder" %in% class(model)) {
    model <- normalize_and_validate_model(model, preserve_builder = FALSE)
  }

  # Capture the extra arguments
  dots <- list(...)

  # Create a results object
  res <- list()

  # Parse the model
  parsed_model <- parse_model(model, ...)

  # Get model segments
  if (is.null(dots$newdata)) segments <- get_segments(parsed_model)
  else segments <- dots$newdata

  # Split by segment, evaluate each segment in parallel, combine results
  res$segments <- segments %>%
    rowwise() %>%
    group_split() %>%
    future_map(function(segment) run_segment(segment, parsed_model, ...), .progress = TRUE, .options = furrr_options(seed=1)) %>%
    bind_rows()

  # Process the results

  # Aggregate results by strategy
  res$aggregated <- aggregate_segments(res$segments, parsed_model)

  # Store metadata for flexible display options
  res$metadata <- parsed_model$metadata

  # Return
  res
}


parse_model <- function(model, ...) {
  
  # Create a new environment from the calling environment which will be used
  # to store model variables.
  model$env <- new.env(parent = parent.frame())

  # Load tables & trees into the environment
  load_tables(model$tables, model$env)
  load_trees(model$trees, model$env)

  # Run any model scripts within that environment
  run_scripts(model$scripts, model$env)

  # If no groups are defined, create one group representing entire population
  if (nrow(model$groups) == 0) {
    model$groups <- create_default_group()
  }

  model$names <- list(
    states = if (nrow(model$states) > 0) unique(model$states$name) else character(0),
    strategies = if (nrow(model$strategies) > 0) unique(model$strategies$name) else character(0),
    values = if (nrow(model$values) > 0) unique(model$values$name) else character(0)
  )

  # Store complete metadata for flexible display options
  model$metadata <- list(
    states = if (nrow(model$states) > 0) model$states %>%
      select(any_of(c("name", "display_name", "description", "abbreviation"))) else tibble(),
    strategies = if (nrow(model$strategies) > 0) model$strategies %>%
      select(any_of(c("name", "display_name", "description", "abbreviation"))) else tibble(),
    groups = if (nrow(model$groups) > 0) model$groups %>%
      select(any_of(c("name", "display_name", "description", "abbreviation"))) else tibble(),
    summaries = if (nrow(model$summaries) > 0) model$summaries %>%
      select(any_of(c("name", "display_name", "description", "wtp", "values"))) else tibble(),
    values = if (nrow(model$values) > 0) model$values %>%
      select(any_of(c("name", "display_name", "description", "abbreviation", "type"))) else tibble(),
    variables = if (nrow(model$variables) > 0) model$variables %>%
      select(any_of(c("name", "display_name", "description", "strategy", "group"))) else tibble()
  )

  model$settings$cycle_length_days <- get_cycle_length_days(model$settings)
  # Note: n_cycles is calculated in run_segment after apply_setting_overrides
  # This ensures DSA timeframe overrides work correctly

  # Set the class of the object based on model type
  # Note: model_type should already be normalized to canonical form by normalize_and_validate_model
  model_type <- tolower(model$settings$model_type)
  if (model_type == "psm") {
    model <- parse_psm(model)
  } else if (model_type %in% c("custom_psm", "custom psm", "psm_custom")) {
    model <- parse_psm_custom(model)
  } else if (model_type == "markov") {
    model <- parse_markov(model)
  } else {
    # Default to markov if not specified
    warning("Model type not specified or unrecognized. Defaulting to 'markov'.")
    model <- parse_markov(model)
  }

  model
}


#' Aggregate Segment Results
#'
#' Aggregates model results across groups within each strategy using weighted averages.
#'
#' @param segments Data frame of model segments with evaluated weights
#' @param parsed_model The parsed model object
#'
#' @return A data frame with one row per strategy containing aggregated results
#' @keywords internal
aggregate_segments <- function(segments, parsed_model) {
  # Check if this is PSA (has simulation column)
  if ("simulation" %in% names(segments)) {
    # PSA mode: group by simulation first, then aggregate within each simulation
    simulations <- unique(segments$simulation)

    # Process each simulation
    aggregated_results <- map_dfr(simulations, function(sim) {
      # Get segments for this simulation
      sim_segments <- segments %>%
        filter(simulation == sim)

      # Get unique strategies for this simulation
      strategies <- unique(sim_segments$strategy)

      # Process each strategy within this simulation
      map_dfr(strategies, function(strat) {
        # Get segments for this strategy
        strat_segments <- sim_segments %>%
          filter(strategy == strat)

        # Perform standard aggregation
        result <- aggregate_strategy_segments(strat_segments, parsed_model)

        # Add simulation column
        result %>%
          mutate(simulation = sim, .before = 1)
      })
    })

    return(aggregated_results)
  }

  # Check if this is DSA (has run_id column)
  if ("run_id" %in% names(segments)) {
    # DSA mode: group by run_id first, then aggregate within each run
    run_ids <- unique(segments$run_id)

    # Process each run
    aggregated_results <- map_dfr(run_ids, function(run) {
      # Get segments for this run
      run_segments <- segments %>%
        filter(run_id == run)

      # Get unique strategies for this run
      strategies <- unique(run_segments$strategy)

      # Process each strategy within this run
      map_dfr(strategies, function(strat) {
        # Get segments for this strategy
        strat_segments <- run_segments %>%
          filter(strategy == strat)

        # Perform standard aggregation
        result <- aggregate_strategy_segments(strat_segments, parsed_model)

        # Add run_id column
        result %>%
          mutate(run_id = run, .before = 1)
      })
    })

    return(aggregated_results)
  }

  # Base case: no simulation or run_id dimension
  # Get unique strategies
  strategies <- unique(segments$strategy)

  # Process each strategy
  aggregated_results <- map_dfr(strategies, function(strat) {
    # Get segments for this strategy
    strat_segments <- segments %>%
      filter(strategy == strat)

    aggregate_strategy_segments(strat_segments, parsed_model)
  })

  return(aggregated_results)
}

#' Aggregate Strategy Segments
#'
#' Helper function to aggregate segments for a single strategy.
#' Extracted to avoid code duplication between base case and PSA.
#'
#' @param strat_segments Segments for a single strategy
#' @param parsed_model Parsed model object
#' @return Aggregated results for this strategy
#' @keywords internal
aggregate_strategy_segments <- function(strat_segments, parsed_model) {
  strat <- unique(strat_segments$strategy)[1]

  # Check if weight exists
  if (!("weight" %in% colnames(strat_segments))) {
    warning("No weight column found in segments. Using equal weights.")
    strat_segments$weight <- 1
  }

  # Check for NA weights
  na_weights <- is.na(strat_segments$weight)
  if (any(na_weights)) {
    groups_with_na <- unique(strat_segments$group[na_weights])
    warning(glue("Groups with invalid weights in strategy '{strat}': {paste(groups_with_na, collapse = ', ')}. Using equal weights for all groups."))
    # If any weight is NA, use equal weights for all groups in this strategy
    strat_segments$weight <- 1
  }

  # Calculate total weight for normalization
  total_weight <- sum(strat_segments$weight)

  if (total_weight == 0) {
    warning(glue("Total weight for strategy '{strat}' is 0. Using equal weights."))
    total_weight <- nrow(strat_segments)
    strat_segments$weight <- 1
  }

  # Normalize weights
  strat_segments <- strat_segments %>%
    mutate(normalized_weight = weight / total_weight)

  # Aggregate collapsed trace
  aggregated_trace <- aggregate_trace(strat_segments)

  # Aggregate expanded trace (if available)
  aggregated_expanded_trace <- aggregate_expanded_trace(strat_segments)

  # Aggregate values
  aggregated_values <- aggregate_values(strat_segments)

  # Aggregate summaries
  aggregated_summaries <- aggregate_summaries(strat_segments)

  # Return aggregated results for this strategy as a single-row tibble
  result_tibble <- tibble(
    strategy = strat,
    group = "_aggregated",  # Special marker for aggregated results
    weight = total_weight,  # Total weight for the strategy
    collapsed_trace = list(aggregated_trace),
    trace_and_values = list(aggregated_values),  # Contains both values and values_discounted
    summaries = list(aggregated_summaries$summaries),
    summaries_discounted = list(aggregated_summaries$summaries_discounted)
  )

  # Add expanded_trace if it exists
  if (!is.null(aggregated_expanded_trace)) {
    result_tibble$expanded_trace <- list(aggregated_expanded_trace)
  }

  result_tibble
}

#' Aggregate Trace Results
#'
#' Calculates weighted average of trace results across segments.
#'
#' @param segments Data frame of segments for a single strategy with normalized weights
#'
#' @return A matrix containing the weighted average trace
#' @keywords internal
aggregate_trace <- function(segments) {
  # Extract traces and weights
  if ("collapsed_trace" %in% colnames(segments)) {
    traces <- segments$collapsed_trace
    # Ensure each trace is properly extracted
    if (is.list(traces) && length(traces) > 0) {
      traces <- lapply(traces, function(x) {
        # If it's a list containing a single element, extract it
        while (is.list(x) && !is.data.frame(x) && length(x) == 1) {
          x <- x[[1]]
        }
        x
      })
    }
  } else {
    return(NULL)
  }
  weights <- segments$normalized_weight
  
  if (length(traces) == 0) return(NULL)

  # Initialize aggregated trace with same dimensions as first trace
  first_trace <- traces[[1]]

  # If first_trace is still a list, extract it
  if (is.list(first_trace) && !is.data.frame(first_trace)) {
    first_trace <- first_trace[[1]]
  }

  # Check if trace is a data frame with time columns (new format) or matrix (old format)
  if (is.data.frame(first_trace)) {
    # New format with time columns
    time_cols <- intersect(c("cycle", "day", "week", "month", "year"), colnames(first_trace))
    state_cols <- setdiff(colnames(first_trace), time_cols)

    if (length(time_cols) > 0) {
      # Initialize with time columns from first trace
      aggregated <- first_trace[, time_cols, drop = FALSE]

      # Initialize state columns with zeros
      for (col in state_cols) {
        aggregated[[col]] <- 0
      }

      # Weight and sum state columns only
      for (i in seq_along(traces)) {
        trace_i <- traces[[i]]
        for (col in state_cols) {
          aggregated[[col]] <- aggregated[[col]] + trace_i[[col]] * weights[i]
        }
      }
    } else {
      # Data frame but no time columns
      aggregated <- first_trace
      for (col in colnames(aggregated)) {
        aggregated[[col]] <- 0
      }
      for (i in seq_along(traces)) {
        for (col in colnames(aggregated)) {
          aggregated[[col]] <- aggregated[[col]] + traces[[i]][[col]] * weights[i]
        }
      }
    }
  } else {
    # Old format - matrix
    aggregated <- matrix(0, nrow = nrow(first_trace), ncol = ncol(first_trace))
    rownames(aggregated) <- rownames(first_trace)
    colnames(aggregated) <- colnames(first_trace)

    # Weight and sum traces
    for (i in seq_along(traces)) {
      aggregated <- aggregated + traces[[i]] * weights[i]
    }
  }
  
  aggregated
}

#' Aggregate Expanded Trace Results
#'
#' Aggregates expanded traces across segments, handling the case where
#' different strategies may have different expanded state structures.
#'
#' @param segments Data frame with segments containing expanded_trace column
#' @return Aggregated expanded trace data frame
#' @keywords internal
aggregate_expanded_trace <- function(segments) {
  # Extract traces and weights
  if (!("expanded_trace" %in% colnames(segments))) {
    return(NULL)
  }

  traces <- segments$expanded_trace
  # Ensure each trace is properly extracted
  if (is.list(traces) && length(traces) > 0) {
    traces <- lapply(traces, function(x) {
      # If it's a list containing a single element, extract it
      while (is.list(x) && !is.data.frame(x) && length(x) == 1) {
        x <- x[[1]]
      }
      x
    })
  }

  weights <- segments$normalized_weight

  if (length(traces) == 0) return(NULL)

  # Get unique expanded state names across all segments
  all_expanded_states <- character()
  for (i in seq_along(traces)) {
    trace <- traces[[i]]
    if (!is.null(trace)) {
      state_cols <- setdiff(colnames(trace), c("cycle", "day", "week", "month", "year"))
      all_expanded_states <- union(all_expanded_states, state_cols)
    }
  }

  # Get time columns from first trace
  first_trace <- traces[[1]]
  if (is.list(first_trace) && !is.data.frame(first_trace)) {
    first_trace <- first_trace[[1]]
  }

  time_cols <- intersect(c("cycle", "day", "week", "month", "year"), colnames(first_trace))

  # Initialize aggregated trace with time columns
  aggregated <- first_trace[, time_cols, drop = FALSE]

  # Initialize all expanded state columns with zeros
  for (state in all_expanded_states) {
    aggregated[[state]] <- 0
  }

  # Weight and sum traces
  for (i in seq_along(traces)) {
    trace_i <- traces[[i]]
    if (!is.null(trace_i)) {
      state_cols_i <- setdiff(colnames(trace_i), time_cols)

      # Add weighted values for states that exist in this trace
      for (col in state_cols_i) {
        if (col %in% colnames(aggregated)) {
          aggregated[[col]] <- aggregated[[col]] + trace_i[[col]] * weights[i]
        }
      }
    }
  }

  return(aggregated)
}

#' Aggregate Values Results
#'
#' Calculates weighted average of values results across segments.
#'
#' @param segments Data frame of segments for a single strategy with normalized weights
#'
#' @return A matrix containing the weighted average values
#' @keywords internal
aggregate_values <- function(segments) {
  # Helper function to aggregate a specific value type
  aggregate_value_type <- function(value_field) {
    # Extract values matrices and weights
    if ("trace_and_values" %in% colnames(segments)) {
      values_list <- segments$trace_and_values
      # Extract values from the nested structure
      if (is.list(values_list) && length(values_list) > 0) {
        values_list <- map(values_list, function(x) {
          if (is.list(x) && length(x) > 0) {
            if (is.list(x[[1]]) && value_field %in% names(x[[1]])) {
              return(x[[1]][[value_field]])
            } else if (value_field %in% names(x)) {
              return(x[[value_field]])
            } else if (is.list(x[[1]]) && "values" %in% names(x[[1]]) && value_field == "values") {
              return(x[[1]]$values)
            } else if ("values" %in% names(x) && value_field == "values") {
              return(x$values)
            }
          }
          return(NULL)
        })
      }
    } else {
      return(NULL)
    }
    weights <- segments$normalized_weight

    if (length(values_list) == 0) return(NULL)

    # Get first non-null values matrix to determine structure
    first_values <- NULL
    for (v in values_list) {
      if (!is.null(v) && length(v) > 0) {
        first_values <- v
        break
      }
    }

    if (is.null(first_values)) return(NULL)

    # Initialize aggregated values with same dimensions
    if (is.matrix(first_values)) {
      aggregated <- matrix(0, nrow = nrow(first_values), ncol = ncol(first_values))
      rownames(aggregated) <- rownames(first_values)
      colnames(aggregated) <- colnames(first_values)
    } else if (is.data.frame(first_values)) {
      # If values is a data frame, convert to matrix for aggregation
      value_cols <- setdiff(colnames(first_values), "cycle")
      aggregated <- as.matrix(first_values[, value_cols, drop = FALSE]) * 0
    } else {
      return(NULL)
    }

    # Weight and sum values
    for (i in seq_along(values_list)) {
      if (!is.null(values_list[[i]]) && length(values_list[[i]]) > 0) {
        if (is.matrix(values_list[[i]])) {
          aggregated <- aggregated + values_list[[i]] * weights[i]
        } else if (is.data.frame(values_list[[i]])) {
          value_cols <- setdiff(colnames(values_list[[i]]), "cycle")
          aggregated <- aggregated + as.matrix(values_list[[i]][, value_cols, drop = FALSE]) * weights[i]
        }
      }
    }

    aggregated
  }

  # Aggregate both undiscounted and discounted values
  list(
    values = aggregate_value_type("values"),
    values_discounted = aggregate_value_type("values_discounted")
  )
}

#' Aggregate Summaries Results
#'
#' Combines summaries from all segments into a single dataframe with weights
#' and calculates the weighted average of amounts by strategy and value.
#'
#' @param segments Data frame of segments for a single strategy with normalized weights
#'
#' @return A data frame containing the aggregated summaries with weighted amounts
#' @keywords internal
aggregate_summaries <- function(segments) {
  # Helper function to aggregate summaries from a list
  aggregate_summary_list <- function(summaries_list, weights) {
    if (length(summaries_list) == 0) {
      return(tibble(summary = character(), value = character(), amount = numeric()))
    }

    # Extract summaries from each segment
    combined_summaries <- map2_dfr(summaries_list, weights, function(summary_obj, weight) {
      # Extract the nested structure
      if (is.list(summary_obj) && length(summary_obj) == 1) {
        summary_df <- summary_obj[[1]]
      } else {
        summary_df <- summary_obj
      }

      # Skip if summary_df is NULL or not a data frame
      if (is.null(summary_df) || !is.data.frame(summary_df)) {
        return(tibble(summary = character(), value = character(), amount = numeric(), weight = numeric()))
      }

      summary_df %>%
        mutate(weight = weight)
    })

    # If no valid summaries were found, return empty result
    if (nrow(combined_summaries) == 0) {
      return(tibble(summary = character(), value = character(), amount = numeric()))
    }

    # Calculate weighted average by grouping by summary and value
    combined_summaries %>%
      group_by(summary, value) %>%
      summarize(
        weighted_sum = sum(amount * weight, na.rm = TRUE),
        weight_sum = sum(weight, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        amount = weighted_sum / weight_sum
      ) %>%
      select(summary, value, amount)
  }

  weights <- segments$normalized_weight

  # Aggregate undiscounted summaries
  undiscounted_summaries <- if ("summaries" %in% colnames(segments)) {
    aggregate_summary_list(segments$summaries, weights)
  } else {
    tibble(summary = character(), value = character(), amount = numeric())
  }

  # Aggregate discounted summaries
  discounted_summaries <- if ("summaries_discounted" %in% colnames(segments)) {
    aggregate_summary_list(segments$summaries_discounted, weights)
  } else {
    tibble(summary = character(), value = character(), amount = numeric())
  }

  # Return both as separate fields (not nested)
  list(
    summaries = undiscounted_summaries,
    summaries_discounted = discounted_summaries
  )
}
