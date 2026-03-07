#' Parse Decision Tree Model
#'
#' Sets up a standalone decision tree model for execution.
#'
#' @param model The model object
#' @return The model with class set to 'decision_tree'
#' @keywords internal
parse_decision_tree_model <- function(model) {
  define_object_(model, class = 'decision_tree')
}

#' Run Segment for Decision Tree Model
#'
#' Executes a standalone decision tree model segment. Evaluates DT-phase values
#' and produces output compatible with aggregate_segments().
#'
#' @param segment The segment (strategy x group)
#' @param model The parsed model object
#' @param env The model environment
#' @param ... Additional arguments
#' @return The segment with results
#' @export
run_segment.decision_tree <- function(segment, model, env, ...) {

  # Capture the extra arguments provided to function
  dots <- list(...)
  .progress_callback <- dots$.progress_callback

  # Apply setting overrides if present (DSA mode)
  model <- apply_setting_overrides(segment, model)

  # Parse variables (no states/transitions for standalone DT)
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)
  uneval_values <- parse_values(model$values, tibble(), uneval_vars)

  # Determine value_names
  model_value_names <- character(0)
  if (nrow(model$values) > 0 && "name" %in% colnames(model$values)) {
    valid_names <- model$values$name[!is.na(model$values$name)]
    if (length(valid_names) > 0) {
      model_value_names <- unique(valid_names)
    }
  }

  # Parse summaries
  if (!is.null(model$summaries) && nrow(model$summaries) > 0) {
    parsed_summaries <- parse_summaries(model$summaries, model_value_names)
  } else {
    parsed_summaries <- tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      values = character(0),
      type = character(0),
      wtp = numeric(0),
      parsed_values = list()
    )
  }

  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # Create a namespace (1 cycle at cycle 0)
  # We need minimal time variables for the namespace
  ns <- create_dt_namespace(model, segment)

  # Apply parameter overrides if present (PSA, DSA, or VBP mode)
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # Evaluate variables
  eval_vars <- eval_variables(uneval_vars, ns)
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # Evaluate DT-phase values: each formula -> scalar amount
  value_names <- model_value_names
  values_matrix <- matrix(0, nrow = 1, ncol = length(value_names))
  colnames(values_matrix) <- value_names
  rownames(values_matrix) <- "1"

  for (i in seq_len(nrow(uneval_values))) {
    value_row <- uneval_values[i, ]
    value_name <- value_row$name

    if (is.na(value_name) || !(value_name %in% value_names)) {
      next
    }

    # Evaluate formula
    evaluated <- eval_formula(value_row$formula[[1]], eval_vars)

    if (is_oq_error(evaluated)) {
      accumulate_oq_error(evaluated, context_msg = glue("Value '{value_name}'"))
      evaluated <- 0
    } else if (!is.numeric(evaluated)) {
      accumulate_oq_error(
        define_error(glue("Value '{value_name}' must evaluate to a numeric value.")),
        context_msg = "Decision tree value validation"
      )
      evaluated <- 0
    }

    # Use first element (scalar)
    values_matrix[1, value_name] <- values_matrix[1, value_name] + as.numeric(evaluated[1])
  }

  oq_error_checkpoint()

  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # Build trace_and_values structure compatible with calculate_summaries
  # No discounting for standalone DT (unless discounting_override is set)
  calculated_trace_and_values <- list(
    matrix(1, nrow = 1, ncol = 0),  # empty trace (no states)
    values = values_matrix,
    values_discounted = values_matrix  # initially identical (no discounting)
  )

  # Apply discounting override formulas (if any)
  discount_cost_rate <- model$settings$discount_cost %||% 0
  discount_outcomes_rate <- model$settings$discount_outcomes %||% 0
  type_mapping <- setNames(uneval_values$type, uneval_values$name)
  type_mapping <- type_mapping[!is.na(names(type_mapping))]
  # For standalone DT, discount factors are just 1 (single cycle)
  discount_factors_cost <- 1
  discount_factors_outcomes <- 1
  calculated_trace_and_values$values_discounted <- apply_discount_weights(
    calculated_trace_and_values$values,
    calculated_trace_and_values$values_discounted,
    NULL,  # no trace matrix for standalone DT
    discount_factors_cost, discount_factors_outcomes,
    discount_cost_rate, discount_outcomes_rate,
    uneval_values, type_mapping, eval_vars
  )

  # Store trace_and_values
  segment$trace_and_values <- list(calculated_trace_and_values)
  if (!"parameter_overrides" %in% names(segment)) {
    segment$uneval_vars <- list(uneval_vars)
    segment$eval_vars <- list(eval_vars)
  }

  # Build collapsed trace (1-row tibble)
  collapsed_trace_with_time <- data.frame(cycle = 0)
  segment$collapsed_trace <- list(collapsed_trace_with_time)
  segment$expanded_trace <- list(collapsed_trace_with_time)
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  # Calculate summaries (same for discounted and undiscounted)
  if (!is.null(parsed_summaries) && nrow(parsed_summaries) > 0) {
    summaries <- calculate_summaries(parsed_summaries, values_matrix)
    segment$summaries <- list(summaries)
    segment$summaries_discounted <- list(summaries)
  } else {
    empty_summary <- tibble(summary = character(), value = character(), amount = numeric())
    segment$summaries <- list(empty_summary)
    segment$summaries_discounted <- list(empty_summary)
  }

  # Calculate segment weight
  segment$weight <- calculate_segment_weight(segment, model, eval_vars)

  # Reorder columns
  col_order <- c("strategy", "group", "weight")
  other_cols <- setdiff(names(segment), col_order)
  segment <- segment[, c(col_order, other_cols)]
  if (!is.null(.progress_callback)) .progress_callback(amount = 1L)

  segment
}

#' Create Namespace for Decision Tree Model
#'
#' Creates a minimal namespace for standalone decision tree models.
#' Uses 1 row (cycle 0) for variable evaluation.
#'
#' @param model The model object
#' @param segment The segment
#' @return A namespace object
#' @keywords internal
create_dt_namespace <- function(model, segment) {
  # Create minimal time variables with 1 row
  time_df <- data.frame(
    cycle = 0,
    day = 0,
    week = 0,
    month = 0,
    year = 0,
    state_time = 1
  )

  cl_vars <- cycle_length_variables(model$settings)
  unit_vars <- time_unit_variables(model$settings)

  ns <- define_namespace(model$env, time_df, cl_vars, unit_vars) %>%
    update_segment_ns(segment)

  ns
}
