#' Threshold Analysis
#'
#' Functions for running threshold analysis using iterative root-finding.
#'
#' @name threshold
#' @importFrom tibble tibble
#' @importFrom dplyr filter bind_rows mutate
#' @importFrom purrr map map_dfr
#' @importFrom stats optimize uniroot
NULL

# ============================================================================
# Format Conversion
# ============================================================================

#' Flatten Threshold Analysis
#'
#' Converts a nested threshold analysis to flat format (for Excel serialization).
#'
#' @param analysis A nested threshold analysis list
#' @return A flat list with all condition fields at top level
#' @keywords internal
flatten_threshold_analysis <- function(analysis) {
  flat <- list(
    name = analysis$name,
    variable = analysis$variable,
    variable_strategy = analysis$variable_strategy %||% "",
    variable_group = analysis$variable_group %||% "",
    lower = analysis$lower,
    upper = analysis$upper,
    active = analysis$active %||% TRUE,
    output = analysis$condition$output
  )
  for (field in setdiff(names(analysis$condition), "output")) {
    flat[[field]] <- analysis$condition[[field]]
  }
  flat
}

#' Nest Threshold Analysis
#'
#' Converts a flat threshold analysis to nested format (from Excel deserialization).
#'
#' @param flat A flat threshold analysis list
#' @return A nested list with condition sub-list
#' @keywords internal
nest_threshold_analysis <- function(flat) {
  top_fields <- c("name", "variable", "variable_strategy", "variable_group",
                   "lower", "upper", "active")
  condition_fields <- setdiff(names(flat), c(top_fields, "output"))

  condition <- list(output = flat$output)
  for (field in condition_fields) {
    val <- flat[[field]]
    if (is.null(val) || (length(val) == 1 && is.na(val))) next
    if (is.logical(val)) {
      condition[[field]] <- val
    } else if (is.numeric(val)) {
      condition[[field]] <- val
    } else if (is.character(val) && val != "") {
      condition[[field]] <- val
    }
  }

  result <- list()
  for (f in top_fields) result[[f]] <- flat[[f]]
  result$condition <- condition
  result
}

# ============================================================================
# Validation
# ============================================================================

#' Validate Threshold Spec
#'
#' Validates threshold analysis specifications on a parsed model.
#'
#' @param model A parsed model object
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_threshold_spec <- function(model) {
  if (is.null(model$threshold_analyses) || length(model$threshold_analyses) == 0) {
    stop("No threshold analyses found. Add analyses with add_threshold_analysis()", call. = FALSE)
  }

  errors <- character()

  for (i in seq_along(model$threshold_analyses)) {
    analysis <- model$threshold_analyses[[i]]
    name <- analysis$name %||% paste("Analysis", i)

    # Check variable exists
    if (!analysis$variable %in% model$variables$name) {
      errors <- c(errors, sprintf("Threshold '%s': variable '%s' not found in model",
                                   name, analysis$variable))
    }

    # Check bounds
    if (analysis$lower >= analysis$upper) {
      errors <- c(errors, sprintf("Threshold '%s': lower (%s) must be less than upper (%s)",
                                   name, analysis$lower, analysis$upper))
    }

    # Check condition
    cond <- analysis$condition
    if (is.null(cond) || is.null(cond$output)) {
      errors <- c(errors, sprintf("Threshold '%s': missing condition or output type", name))
      next
    }

    if (!cond$output %in% c("outcomes", "costs", "nmb", "ce", "vbp", "trace")) {
      errors <- c(errors, sprintf("Threshold '%s': invalid output type '%s'", name, cond$output))
      next
    }

    # Validate condition fields by output type
    if (cond$output %in% c("outcomes", "costs")) {
      has_summary <- !is.null(cond$summary) && cond$summary != ""
      has_value <- !is.null(cond$value) && cond$value != ""
      if (!has_summary && !has_value) {
        errors <- c(errors, sprintf("Threshold '%s': must specify 'summary' or 'value'", name))
      }
      if (cond$type == "absolute" && (is.null(cond$strategy) || cond$strategy == "")) {
        errors <- c(errors, sprintf("Threshold '%s': absolute type requires 'strategy'", name))
      }
      if (cond$type == "difference") {
        if (is.null(cond$referent) || cond$referent == "") {
          errors <- c(errors, sprintf("Threshold '%s': difference type requires 'referent'", name))
        }
        if (is.null(cond$comparator) || cond$comparator == "") {
          errors <- c(errors, sprintf("Threshold '%s': difference type requires 'comparator'", name))
        }
      }
    } else if (cond$output %in% c("nmb", "ce")) {
      required <- c("health_summary", "cost_summary", "referent", "comparator")
      for (f in required) {
        if (is.null(cond[[f]]) || cond[[f]] == "") {
          errors <- c(errors, sprintf("Threshold '%s': %s output requires '%s'", name, cond$output, f))
        }
      }
    } else if (cond$output == "trace") {
      # Validate state exists
      if (!is.null(cond$state) && cond$state != "") {
        state_names <- model$names$states
        if (!cond$state %in% state_names) {
          errors <- c(errors, sprintf("Threshold '%s': state '%s' not found in model states",
                                       name, cond$state))
        }
      } else {
        errors <- c(errors, sprintf("Threshold '%s': trace output requires 'state'", name))
      }
      # Validate resolved cycle is within model timeframe
      tryCatch({
        cycle_num <- resolve_trace_cycle(cond, model)
        n_cycles <- get_n_cycles(model$settings)
        if (!is.na(n_cycles) && (cycle_num < 0 || cycle_num > n_cycles)) {
          errors <- c(errors, sprintf("Threshold '%s': cycle %d is outside model timeframe (0 to %d)",
                                       name, cycle_num, n_cycles))
        }
      }, error = function(e) {
        errors <<- c(errors, sprintf("Threshold '%s': %s", name, e$message))
      })
      # Validate strategy/referent/comparator
      if (!is.null(cond$type) && cond$type == "absolute") {
        if (is.null(cond$strategy) || cond$strategy == "") {
          errors <- c(errors, sprintf("Threshold '%s': absolute type requires 'strategy'", name))
        } else if (!cond$strategy %in% model$names$strategies) {
          errors <- c(errors, sprintf("Threshold '%s': strategy '%s' not found in model",
                                       name, cond$strategy))
        }
      } else if (!is.null(cond$type) && cond$type == "difference") {
        if (!is.null(cond$referent) && cond$referent != "" &&
            !cond$referent %in% model$names$strategies) {
          errors <- c(errors, sprintf("Threshold '%s': referent '%s' not found in model",
                                       name, cond$referent))
        }
        if (!is.null(cond$comparator) && cond$comparator != "" &&
            !cond$comparator %in% model$names$strategies) {
          errors <- c(errors, sprintf("Threshold '%s': comparator '%s' not found in model",
                                       name, cond$comparator))
        }
      }
    }

    # Validate condition group exists in model
    if (!is.null(cond$group) && cond$group != "" &&
        cond$group != "_aggregated" && cond$group != "overall") {
      if (!is.null(model$groups) && nrow(model$groups) > 0) {
        if (!cond$group %in% model$groups$name) {
          errors <- c(errors, sprintf("Threshold '%s': group '%s' not found in model groups",
                                      name, cond$group))
        }
      } else {
        errors <- c(errors, sprintf("Threshold '%s': group '%s' specified but model has no groups",
                                    name, cond$group))
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(c("Threshold analysis validation errors:", errors), collapse = "\n  "), call. = FALSE)
  }

  TRUE
}

# ============================================================================
# Runtime
# ============================================================================

#' Run Threshold Analysis
#'
#' Runs threshold analyses on a model using iterative root-finding to find
#' the input parameter value that produces a desired output condition.
#'
#' @param model An oq_model_builder or oq_model object with threshold analyses defined
#' @param ... Additional arguments passed to run_segment
#' @return A list with threshold_values tibble, root_finder_history tibble, and metadata
#' @export
run_threshold <- function(model, ...) {
  # Finalize builders
  if ("oq_model_builder" %in% class(model)) {
    model <- normalize_and_validate_model(model, preserve_builder = FALSE)
  }

  # Parse model
  parsed_model <- parse_model(model, ...)

  # Validate threshold specs
  validate_threshold_spec(parsed_model)

  # Filter to active analyses
  active_analyses <- Filter(function(a) {
    is.null(a$active) || isTRUE(a$active)
  }, parsed_model$threshold_analyses)

  if (length(active_analyses) == 0) {
    stop("No active threshold analyses found", call. = FALSE)
  }

  # Run each analysis sequentially
  res_list <- list()
  for (i in seq_along(active_analyses)) {
    res_list[[i]] <- run_single_threshold(parsed_model, active_analyses[[i]], ...)
  }

  # Aggregate results
  list(
    threshold_values = map_dfr(res_list, function(x) {
      tibble(
        name = x$name,
        variable = x$variable,
        value = x$threshold_value,
        converged = !is.na(x$threshold_value)
      )
    }),
    root_finder_history = map_dfr(res_list, function(x) {
      if (nrow(x$history) > 0) {
        x$history$name <- x$name
        x$history$variable <- x$variable
        x$history[, c("name", "variable", setdiff(names(x$history), c("name", "variable")))]
      } else {
        tibble(name = character(), variable = character(), iteration = integer(),
               input = numeric(), output = numeric(), goal = numeric(), diff = numeric())
      }
    }),
    metadata = parsed_model$metadata,
    analyses = active_analyses
  )
}

#' Run Single Threshold Analysis
#' @keywords internal
run_single_threshold <- function(parsed_model, analysis, ...) {
  solver <- create_threshold_solver(parsed_model, analysis, ...)
  threshold_value <- find_threshold_value(solver$run_iteration, analysis)

  list(
    name = analysis$name,
    variable = analysis$variable,
    threshold_value = threshold_value,
    history = solver$get_history()
  )
}

#' Create Threshold Solver
#'
#' Creates a solver instance (closure) for a single threshold analysis.
#'
#' @param parsed_model Parsed model object
#' @param analysis Threshold analysis specification
#' @param ... Additional arguments passed to run_segment
#' @return A list with run_iteration() and get_history() functions
#' @keywords internal
create_threshold_solver <- function(parsed_model, analysis, ...) {
  history <- list()
  iteration <- 0

  list(
    run_iteration = function(x) {
      # Get base segments
      segments <- get_segments(parsed_model)

      # Apply override categories
      segments <- apply_override_categories(parsed_model, segments)

      # Add parameter overrides for the threshold variable
      if (!"parameter_overrides" %in% names(segments)) {
        segments$parameter_overrides <- map(1:nrow(segments), ~ list())
      }

      # Apply the threshold variable override, respecting strategy/group targeting
      for (seg_idx in 1:nrow(segments)) {
        seg_strategy <- segments$strategy[[seg_idx]]
        seg_group <- segments$group[[seg_idx]]

        applies <- TRUE
        if (!is.null(analysis$variable_strategy) && analysis$variable_strategy != "" &&
            analysis$variable_strategy != seg_strategy) {
          applies <- FALSE
        }
        if (!is.null(analysis$variable_group) && analysis$variable_group != "" &&
            analysis$variable_group != seg_group) {
          applies <- FALSE
        }

        if (applies) {
          segments$parameter_overrides[[seg_idx]][[analysis$variable]] <- x
        }
      }

      # Run all segments
      results <- segments %>%
        dplyr::rowwise() %>%
        dplyr::group_split() %>%
        lapply(function(segment) run_segment(segment, parsed_model, ...)) %>%
        bind_rows()

      # Aggregate results
      aggregated <- aggregate_segments(results, parsed_model)

      # Choose data source based on condition group
      cond_group <- analysis$condition$group %||% ""
      if (cond_group != "" && cond_group != "_aggregated" && cond_group != "overall") {
        data_for_extraction <- results %>% dplyr::filter(.data$group == cond_group)
        if (nrow(data_for_extraction) == 0) {
          stop(sprintf("Group '%s' not found in results", cond_group), call. = FALSE)
        }
      } else {
        data_for_extraction <- aggregated
      }

      # Extract the target metric
      target_res <- extract_threshold_output(data_for_extraction, analysis, parsed_model)
      goal <- get_threshold_goal(analysis)
      diff <- target_res - goal

      iteration <<- iteration + 1
      history[[iteration]] <<- tibble(
        iteration = iteration,
        input = x,
        output = target_res,
        goal = goal,
        diff = diff
      )

      diff
    },
    get_history = function() {
      if (length(history) == 0) {
        return(tibble(iteration = integer(), input = numeric(),
                      output = numeric(), goal = numeric(), diff = numeric()))
      }
      bind_rows(history)
    }
  )
}

#' Extract Threshold Output
#'
#' Extracts the target metric value from aggregated model results
#' based on the threshold condition specification.
#'
#' @param aggregated Aggregated model results
#' @param analysis Threshold analysis specification
#' @param parsed_model Parsed model object (needed for WTP in NMB/CE)
#' @return Numeric value of the target metric
#' @keywords internal
extract_threshold_output <- function(aggregated, analysis, parsed_model) {
  cond <- analysis$condition
  output_type <- cond$output

  # Choose discounted or undiscounted summaries
  summary_col <- if (isTRUE(cond$discounted)) "summaries_discounted" else "summaries"

  if (output_type %in% c("outcomes", "costs")) {
    extract_threshold_outcomes_costs(aggregated, cond, summary_col)
  } else if (output_type %in% c("nmb", "ce")) {
    extract_threshold_nmb(aggregated, cond, summary_col, parsed_model)
  } else if (output_type == "trace") {
    extract_threshold_trace(aggregated, cond, parsed_model)
  } else {
    stop(sprintf("Unsupported threshold output type: %s", output_type), call. = FALSE)
  }
}

#' Extract Outcomes/Costs Threshold Output
#' @keywords internal
extract_threshold_outcomes_costs <- function(aggregated, cond, summary_col) {
  extract_for_strategy <- function(strategy_name) {
    strat_row <- aggregated[aggregated$strategy == strategy_name, ]
    if (nrow(strat_row) == 0) {
      stop(sprintf("Strategy '%s' not found in results", strategy_name), call. = FALSE)
    }

    summ <- strat_row[[summary_col]][[1]]
    if (is.null(summ) || !is.data.frame(summ) || nrow(summ) == 0) {
      stop(sprintf("No summaries found for strategy '%s'", strategy_name), call. = FALSE)
    }

    if (!is.null(cond$summary) && cond$summary != "") {
      # Target a summary: sum all values within that summary
      filtered <- summ[summ$summary == cond$summary, ]
      if (nrow(filtered) == 0) {
        stop(sprintf("Summary '%s' not found for strategy '%s'", cond$summary, strategy_name), call. = FALSE)
      }
      sum(filtered$amount, na.rm = TRUE)
    } else {
      # Target an individual value
      filtered <- summ[summ$value == cond$value, ]
      if (nrow(filtered) == 0) {
        stop(sprintf("Value '%s' not found for strategy '%s'", cond$value, strategy_name), call. = FALSE)
      }
      sum(filtered$amount, na.rm = TRUE)
    }
  }

  if (cond$type == "absolute") {
    extract_for_strategy(cond$strategy)
  } else {
    extract_for_strategy(cond$referent) - extract_for_strategy(cond$comparator)
  }
}

#' Extract NMB/CE Threshold Output
#' @keywords internal
extract_threshold_nmb <- function(aggregated, cond, summary_col, parsed_model) {
  extract_summary_for_strategy <- function(strategy_name, summary_name) {
    strat_row <- aggregated[aggregated$strategy == strategy_name, ]
    if (nrow(strat_row) == 0) {
      stop(sprintf("Strategy '%s' not found in results", strategy_name), call. = FALSE)
    }

    summ <- strat_row[[summary_col]][[1]]
    if (is.null(summ) || !is.data.frame(summ) || nrow(summ) == 0) {
      stop(sprintf("No summaries found for strategy '%s'", strategy_name), call. = FALSE)
    }

    filtered <- summ[summ$summary == summary_name, ]
    if (nrow(filtered) == 0) {
      stop(sprintf("Summary '%s' not found for strategy '%s'", summary_name, strategy_name), call. = FALSE)
    }
    sum(filtered$amount, na.rm = TRUE)
  }

  # Get health and cost totals for both strategies
  ref_health <- extract_summary_for_strategy(cond$referent, cond$health_summary)
  comp_health <- extract_summary_for_strategy(cond$comparator, cond$health_summary)
  ref_cost <- extract_summary_for_strategy(cond$referent, cond$cost_summary)
  comp_cost <- extract_summary_for_strategy(cond$comparator, cond$cost_summary)

  # Incremental values
  incr_health <- ref_health - comp_health
  incr_cost <- ref_cost - comp_cost

  # Get WTP: condition-level wtp takes precedence, then fall back to summary
  wtp <- if (!is.null(cond$wtp) && !is.na(cond$wtp)) cond$wtp else get_summary_wtp(parsed_model, cond$health_summary)

  incr_health * wtp - incr_cost
}

#' Get WTP from Model Metadata
#' @keywords internal
get_summary_wtp <- function(parsed_model, health_summary) {
  if (!is.null(parsed_model$metadata$summaries) &&
      nrow(parsed_model$metadata$summaries) > 0 &&
      "wtp" %in% names(parsed_model$metadata$summaries)) {
    summ_row <- parsed_model$metadata$summaries[parsed_model$metadata$summaries$name == health_summary, ]
    if (nrow(summ_row) > 0 && !is.na(summ_row$wtp[1])) {
      return(summ_row$wtp[1])
    }
  }
  # Also check the raw summaries
  if (!is.null(parsed_model$summaries) &&
      nrow(parsed_model$summaries) > 0 &&
      "wtp" %in% names(parsed_model$summaries)) {
    summ_row <- parsed_model$summaries[parsed_model$summaries$name == health_summary, ]
    if (nrow(summ_row) > 0 && !is.na(summ_row$wtp[1])) {
      return(summ_row$wtp[1])
    }
  }
  stop(sprintf("Could not determine WTP for health summary '%s'. Ensure the summary has a WTP value defined.", health_summary), call. = FALSE)
}

#' Resolve Trace Cycle Number
#'
#' Converts a time value and unit into a cycle number for trace extraction.
#'
#' @param cond A threshold condition with time, time_unit fields
#' @param parsed_model Parsed model object with settings
#' @return Integer cycle number
#' @keywords internal
resolve_trace_cycle <- function(cond, parsed_model) {
  if (cond$time_unit == "cycle") {
    cycle_num <- cond$time
    if (abs(cycle_num - round(cycle_num)) > 1e-6) {
      stop(sprintf("Cycle must be a whole number, got %s", cycle_num), call. = FALSE)
    }
    return(as.integer(round(cycle_num)))
  }

  settings <- parsed_model$settings
  cycle_length_days <- settings$cycle_length_days
  days_per_year <- get_days_per_year(settings)
  days_per_month <- days_per_year / 12

  target_days <- switch(cond$time_unit,
    "day" = cond$time,
    "week" = cond$time * 7,
    "month" = cond$time * days_per_month,
    "year" = cond$time * days_per_year
  )

  target_cycle <- target_days / cycle_length_days
  rounded_cycle <- round(target_cycle)

  if (abs(target_cycle - rounded_cycle) / max(abs(target_cycle), 1) > 1e-6) {
    stop(sprintf("Time %s %s does not correspond to a whole cycle number (got %.6f cycles)",
                 cond$time, cond$time_unit, target_cycle), call. = FALSE)
  }

  as.integer(rounded_cycle)
}

#' Extract Trace Threshold Output
#'
#' Extracts trace values from model results for threshold analysis.
#'
#' @param data_for_extraction Model results data (aggregated or group-filtered)
#' @param cond Threshold condition specification
#' @param parsed_model Parsed model object
#' @return Numeric trace value (absolute or difference)
#' @keywords internal
extract_threshold_trace <- function(data_for_extraction, cond, parsed_model) {
  cycle_num <- resolve_trace_cycle(cond, parsed_model)

  extract_for_strategy <- function(strategy_name) {
    strat_row <- data_for_extraction[data_for_extraction$strategy == strategy_name, ]
    if (nrow(strat_row) == 0) {
      stop(sprintf("Strategy '%s' not found in results", strategy_name), call. = FALSE)
    }

    trace_df <- strat_row$collapsed_trace[[1]]
    cycle_row_idx <- which(trace_df$cycle == cycle_num)
    if (length(cycle_row_idx) == 0) {
      stop(sprintf("Cycle %d not found in trace", cycle_num), call. = FALSE)
    }

    if (!cond$state %in% colnames(trace_df)) {
      stop(sprintf("State '%s' not found in trace", cond$state), call. = FALSE)
    }
    trace_df[[cond$state]][cycle_row_idx]
  }

  if (cond$type == "absolute") {
    extract_for_strategy(cond$strategy)
  } else {
    extract_for_strategy(cond$referent) - extract_for_strategy(cond$comparator)
  }
}

#' Get Threshold Goal
#' @keywords internal
get_threshold_goal <- function(analysis) {
  if (analysis$condition$output == "ce") {
    0
  } else {
    analysis$condition$target_value
  }
}

#' Find Threshold Value
#'
#' Uses uniroot with fallback to optimize.
#'
#' @param solver_callback Function that takes a numeric value and returns the diff from goal
#' @param analysis Threshold analysis specification
#' @return The threshold value, or NA if not found
#' @keywords internal
find_threshold_value <- function(solver_callback, analysis) {
  tryCatch(
    threshold_root_finder(solver_callback, analysis),
    error = function(e) {
      if (grepl("f() values at end points not of opposite sign", e$message, fixed = TRUE)) {
        warning(sprintf("Threshold '%s': values at endpoints not of opposite sign, using optimizer fallback",
                        analysis$name), call. = FALSE)
        threshold_optimizer(solver_callback, analysis)
      } else {
        stop(e$message, call. = FALSE)
      }
    }
  )
}

#' Root Finder for Threshold
#' @keywords internal
threshold_root_finder <- function(solver_callback, analysis) {
  res <- uniroot(solver_callback, c(analysis$lower, analysis$upper))
  res$root
}

#' Optimizer Fallback for Threshold
#' @keywords internal
threshold_optimizer <- function(solver_callback, analysis) {
  goal_func <- function(x) abs(solver_callback(x))
  tol <- .Machine$double.eps^0.25
  res <- optimize(
    goal_func,
    lower = analysis$lower,
    upper = analysis$upper,
    maximum = FALSE,
    tol = tol
  )

  if (goal_func(res$minimum) > (tol * 2)) {
    return(NA)
  }

  res$minimum
}
