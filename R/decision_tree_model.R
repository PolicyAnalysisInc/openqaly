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

  tick <- make_progress(...)
  diagnostics_policy <- get_diagnostics_policy(...)

  # Segments in analysis modes (DSA, scenarios) can override model settings
  # like discount rates. Apply those before computing anything.
  model <- apply_setting_overrides(segment, model)

  # --- Parse phase ---
  # Variables are filtered to this segment's strategy/group. The trees argument

  # triggers parse_tree_vars(), which creates a synthetic variable for each
  # decision tree (e.g. a tree named "my_tree" becomes a variable whose formula
  # calls decision_tree(.trees, "my_tree", cycle)). When evaluated later, this
  # builds the full tree structure with joint probabilities at terminal nodes.
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)
  # States is an empty tibble — DT models have no health states. parse_values
  # still parses value formulas but skips all state-expansion logic (no "All"/
  # "All Other" targeting, no tunnel expansion). Values are flat formulas that
  # reference tree terminal-node probabilities to compute expected costs/outcomes.
  uneval_values <- parse_values(model$values, tibble(), uneval_vars)
  value_names <- get_value_names(model$values)
  # Summaries aggregate named values (e.g. "Total Cost" = sum of cost values),
  # same mechanism as Markov/PSM.
  parsed_summaries <- parse_summaries(model$summaries, value_names)
  tick()

  # --- Namespace + override phase ---
  # DT uses a minimal 1-row namespace (cycle=0, all time vars=0, state_time=1)
  # instead of Markov's n_cycles rows. Everything is evaluated at a single
  # point in time — there is no temporal progression.
  ns <- create_dt_namespace(model, segment)
  # Parameter overrides (from DSA/PSA) inject fixed values into the namespace,
  # replacing the variable's formula. Same mechanism as Markov/PSM.
  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

  # --- Evaluation phase ---
  # Variables are evaluated in dependency order. Tree variables are resolved
  # here: each tree's decision_tree() call traverses nodes level-by-level,
  # evaluates conditional probabilities (including complement "C" branches),
  # and computes joint probabilities at terminal nodes. The result is an
  # eval_decision_tree object stored in the namespace for value formulas to
  # reference (e.g. via subtree tag lookups).
  eval_vars <- eval_variables(uneval_vars, ns)
  tick()
  # Extra tick: DT skips steps that Markov/PSM perform (trace computation,
  # state evaluation) but the progress system expects 8 ticks per segment.
  # Double-ticks here and below pad the count to 8.
  tick()

  # DT-specific value evaluation: creates a 1-row matrix (one column per value
  # name, initialized to zero). Iterates over all value formulas, evaluating
  # each against the namespace. Multiple value rows with the same name are
  # accumulated with addition — this is how values from different tree branches
  # or components sum into a single expected value (e.g. total expected cost =
  # sum of branch-specific costs weighted by branch probabilities).
  values_matrix <- evaluate_dt_values(uneval_values, eval_vars, value_names)
  tick()
  tick()
  # Apply discounting. For DT, discount factors are scalar 1 (everything is at
  # time zero, so 1/(1+r)^0 = 1). However, values with a discounting_override
  # formula still get evaluated — the override can reference discount_rate but
  # NOT trace() (which throws an error since DT has no state trace). The trace
  # argument is NULL and discount factor vectors are just the scalar 1.
  calculated <- apply_dt_discounting(values_matrix, uneval_values, eval_vars, model)

  # --- Storage phase ---
  # Store diagnostic data (variables, values) on the segment for debugging.
  # eval_states is NULL because DT has no health states or initial distribution.
  segment <- clear_segment_diagnostics(segment)
  segment <- store_segment_diagnostics(
    segment, calculated, uneval_vars, eval_vars, NULL,
    policy = diagnostics_policy
  )
  # DT has no real trace — insert a placeholder single-row data.frame(cycle=0)
  # so the segment structure stays compatible with downstream aggregation code
  # that expects collapsed_trace and expanded_trace list columns.
  segment$collapsed_trace <- list(data.frame(cycle = 0))
  segment$expanded_trace <- segment$collapsed_trace
  tick()
  # Compute user-defined summary totals from the 1-row values matrix.
  segment <- store_segment_summaries(segment, parsed_summaries, values_matrix, calculated$values_discounted)
  # Calculate group weight and reorder columns to standard layout.
  segment <- finalize_segment(segment, model, eval_vars)
  tick()

  segment
}

evaluate_dt_values <- function(uneval_values, eval_vars, value_names) {
  values_matrix <- matrix(0, nrow = 1, ncol = length(value_names))
  colnames(values_matrix) <- value_names
  rownames(values_matrix) <- "1"

  for (i in seq_len(nrow(uneval_values))) {
    value_row <- uneval_values[i, ]
    value_name <- value_row$name

    if (is.na(value_name) || !(value_name %in% value_names)) {
      next
    }

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

    values_matrix[1, value_name] <- values_matrix[1, value_name] + as.numeric(evaluated[1])
  }

  oq_error_checkpoint()
  values_matrix
}

apply_dt_discounting <- function(values_matrix, uneval_values, eval_vars, model) {
  calculated <- list(
    matrix(1, nrow = 1, ncol = 0),
    values = values_matrix,
    values_discounted = values_matrix
  )
  discount_cost_rate <- model$settings$discount_cost %||% 0
  discount_outcomes_rate <- model$settings$discount_outcomes %||% 0
  type_mapping <- setNames(uneval_values$type, uneval_values$name)
  type_mapping <- type_mapping[!is.na(names(type_mapping))]
  calculated$values_discounted <- apply_discount_weights(
    calculated$values, calculated$values_discounted,
    NULL, 1, 1,
    discount_cost_rate, discount_outcomes_rate,
    uneval_values, type_mapping, eval_vars
  )
  calculated
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
