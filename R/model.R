#' Run a Model
#'
#' Takes a model specification object and runs the model.
#'
#' @param model A heRo_model object.
#' @param ... additional arguments.
#' 
#' @return A list containing the results of the model.
#' 
#' @export
run_model <- function(model, ...) {
  
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
    map(function(segment) run_segment(segment, parsed_model, ...)) %>%
    bind_rows()
  
  # Process the results
  
  # Aggregate results by strategy
  res$aggregated <- aggregate_segments(res$segments, parsed_model)
  
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
    states = unique(model$states$name),
    strategies = unique(model$strategies$name),
    values = unique(model$values$name)
  )

  model$settings$cycle_length_days <- get_cycle_length_days(model$settings)
  model$settings$n_cycles <- get_n_cycles(model$settings)

  # Set the class of the object based on model type
  model_type <- tolower(model$settings$model_type)
  if (model_type == "psm") {
    model <- parse_psm(model)
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
  # Get unique strategies
  strategies <- unique(segments$strategy)
  
  # Process each strategy
  aggregated_results <- map_dfr(strategies, function(strat) {
    # Get segments for this strategy
    strat_segments <- segments %>%
      filter(strategy == strat)
    
    # Check if weight exists
    if (!("weight" %in% colnames(strat_segments))) {
      warning("No weight column found in segments. Using equal weights.")
      strat_segments$weight <- 1
    }
    
    # Check for NA weights
    na_weights <- is.na(strat_segments$weight)
    if (any(na_weights)) {
      groups_with_na <- unique(strat_segments$group[na_weights])
      warning(paste0("Groups with invalid weights in strategy '", strat, "': ", 
                     paste(groups_with_na, collapse = ", "), 
                     ". Using equal weights for all groups."))
      # If any weight is NA, use equal weights for all groups in this strategy
      strat_segments$weight <- 1
    }
    
    # Calculate total weight for normalization
    total_weight <- sum(strat_segments$weight)
    
    if (total_weight == 0) {
      warning(paste0("Total weight for strategy '", strat, "' is 0. Using equal weights."))
      total_weight <- nrow(strat_segments)
      strat_segments$weight <- 1
    }
    
    # Normalize weights
    strat_segments <- strat_segments %>%
      mutate(normalized_weight = weight / total_weight)
    
    # Aggregate collapsed trace
    aggregated_trace <- aggregate_trace(strat_segments)
    
    # Aggregate values  
    aggregated_values <- aggregate_values(strat_segments)
    
    # Aggregate summaries
    aggregated_summaries <- aggregate_summaries(strat_segments)
    
    # Return aggregated results for this strategy as a single-row tibble
    tibble(
      strategy = strat,
      group = "_aggregated",  # Special marker for aggregated results
      weight = total_weight,  # Total weight for the strategy
      collapsed_trace = list(aggregated_trace),
      trace_and_values = list(list(values = aggregated_values)),  # Match segment structure
      summaries = list(aggregated_summaries)
    )
  })
  
  # Return the aggregated results dataframe
  aggregated_results
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
    # If traces is already a list of matrices, use it directly
    # Otherwise extract the first element of each list
    if (is.list(traces) && length(traces) > 0) {
      if (is.list(traces[[1]])) {
        traces <- map(traces, ~ .[[1]])
      }
    }
  } else {
    return(NULL)
  }
  weights <- segments$normalized_weight
  
  if (length(traces) == 0) return(NULL)
  
  # Initialize aggregated trace with same dimensions as first trace
  first_trace <- traces[[1]]
  aggregated <- matrix(0, nrow = nrow(first_trace), ncol = ncol(first_trace))
  rownames(aggregated) <- rownames(first_trace)
  colnames(aggregated) <- colnames(first_trace)
  
  # Weight and sum traces
  for (i in seq_along(traces)) {
    aggregated <- aggregated + traces[[i]] * weights[i]
  }
  
  aggregated
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
  # Extract values matrices and weights
  if ("trace_and_values" %in% colnames(segments)) {
    values_list <- segments$trace_and_values
    # Extract values from the nested structure
    if (is.list(values_list) && length(values_list) > 0) {
      values_list <- map(values_list, function(x) {
        if (is.list(x) && length(x) > 0) {
          if (is.list(x[[1]]) && "values" %in% names(x[[1]])) {
            return(x[[1]]$values)
          } else if ("values" %in% names(x)) {
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
  # Extract summaries and weights
  if (!("summaries" %in% colnames(segments))) {
    return(tibble::tibble(summary = character(), value = character(), amount = numeric()))
  }
  
  summaries_list <- segments$summaries
  weights <- segments$normalized_weight
  
  if (length(summaries_list) == 0) {
    return(tibble::tibble(summary = character(), value = character(), amount = numeric()))
  }
  
  # Combine all summaries with their segment weights
  combined_summaries <- map2_dfr(summaries_list, weights, function(summary_df, weight) {
    # Extract the summary dataframe from the list if necessary
    if (is.list(summary_df) && length(summary_df) == 1) {
      summary_df <- summary_df[[1]]
    }
    
    # Skip if summary_df is NULL or not a data frame
    if (is.null(summary_df) || !is.data.frame(summary_df)) {
      return(tibble::tibble(summary = character(), value = character(), amount = numeric(), weight = numeric()))
    }
    
    # Add weight to each row
    summary_df %>%
      mutate(weight = weight)
  })
  
  # If no valid summaries were found, return empty result
  if (nrow(combined_summaries) == 0) {
    return(tibble::tibble(summary = character(), value = character(), amount = numeric()))
  }
  
  # Calculate weighted average by grouping by summary and value
  aggregated_summaries <- combined_summaries %>%
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
  
  aggregated_summaries
}
