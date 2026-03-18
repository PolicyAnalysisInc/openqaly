#' Calculate Discount Factors
#'
#' Calculate discount factors for each cycle using the formula 1/(1+r)^t,
#' where t is the time in years from the start of the model.
#' The first cycle (t=0) has a discount factor of 1 (no discounting).
#'
#' @param n_cycles Number of cycles
#' @param discount_rate Annual discount rate as a percentage (e.g., 3.5 for 3.5\%)
#' @param cycle_length_years Length of each cycle in years (e.g., 1/12 for monthly, 1/52 for weekly)
#' @param discount_timing Which point in the cycle to use for the discount formula:
#'   \code{"start"} (default, cycle start), \code{"end"} (cycle end), or \code{"midpoint"} (cycle midpoint).
#' @param discount_method Whether discounting steps annually or continuously:
#'   \code{"by_cycle"} (default, continuous per-cycle time) or \code{"by_year"} (floor to whole years).
#' @param offset_years Offset in years added to all time points before applying
#'   discounting. Used when a decision tree phase precedes the model, shifting
#'   all discounting forward. Default is 0.
#'
#' @return Vector of discount factors for each cycle
#' @export
calculate_discount_factors <- function(n_cycles, discount_rate, cycle_length_years = 1,
                                       discount_timing = "start",
                                       discount_method = "by_cycle",
                                       offset_years = 0) {
  if (n_cycles <= 0) {
    return(numeric(0))
  }

  # Validate discount_timing
  valid_timings <- c("start", "end", "midpoint")
  if (!discount_timing %in% valid_timings) {
    stop("Invalid discount_timing: '", discount_timing, "'. Must be one of: ",
         paste(valid_timings, collapse = ", "))
  }

  # Validate discount_method
  valid_methods <- c("by_cycle", "by_year")
  if (!discount_method %in% valid_methods) {
    stop("Invalid discount_method: '", discount_method, "'. Must be one of: ",
         paste(valid_methods, collapse = ", "))
  }

  # Calculate time in years for each cycle based on timing
  cycles <- seq_len(n_cycles)
  offset <- switch(discount_timing,
    start = 1,
    end = 0,
    midpoint = 0.5
  )
  time_years <- (cycles - offset) * cycle_length_years + offset_years

  # Apply annual stepping if requested
  if (discount_method == "by_year") {
    time_years <- floor(time_years)
  }

  # Calculate discount factors based on time in years
  1 / (1 + discount_rate / 100)^time_years
}

#' Apply Discounting to Values Matrix
#'
#' Applies discount factors to a values matrix based on value types.
#' Values identified as costs use the cost discount rate, while
#' outcomes use the outcomes discount rate.
#'
#' @param values_matrix Matrix or data frame of undiscounted values
#' @param discount_factors_cost Vector of discount factors for costs
#' @param discount_factors_outcomes Vector of discount factors for outcomes
#' @param types Named vector indicating type ("cost" or "outcome") for each value
#' @param skip Character vector of value names to skip (leave undiscounted). Default is empty.
#'
#' @return Matrix or data frame with discounted values
#' @export
apply_discounting <- function(values_matrix,
                             discount_factors_cost,
                             discount_factors_outcomes,
                             types,
                             skip = character(0)) {

  # Handle case where values_matrix is a data frame
  if (is.data.frame(values_matrix)) {
    values_matrix <- as.matrix(values_matrix)
    was_df <- TRUE
  } else {
    was_df <- FALSE
  }

  # Get dimensions
  n_cycles <- nrow(values_matrix)
  value_names <- colnames(values_matrix)

  # Create result matrix
  discounted_matrix <- values_matrix

  # Apply discounting column by column based on value type
  for (i in seq_along(value_names)) {
    value_name <- value_names[i]

    if (value_name %in% skip) next

    # Determine which discount factors to use
    if (!is.null(types) && value_name %in% names(types)) {
      type <- types[value_name]
    } else {
      # Default to outcome if type not specified
      type <- "outcome"
    }

    # Apply appropriate discount factors
    if (type == "cost") {
      discount_factors <- discount_factors_cost
    } else {
      discount_factors <- discount_factors_outcomes
    }

    # Ensure discount factors match the number of cycles
    if (length(discount_factors) >= n_cycles) {
      discounted_matrix[, i] <- values_matrix[, i] * discount_factors[seq_len(n_cycles)]
    } else {
      warning(paste("Discount factors length", length(discount_factors),
                   "is less than number of cycles", n_cycles))
      # Use available factors and pad with 1s
      factors_to_use <- c(discount_factors, rep(1, n_cycles - length(discount_factors)))
      discounted_matrix[, i] <- values_matrix[, i] * factors_to_use
    }
  }

  # Convert back to data frame if input was data frame
  if (was_df) {
    discounted_matrix <- as.data.frame(discounted_matrix)
  }

  discounted_matrix
}

#' Apply Discounting Override Formulas
#'
#' For each value that has a non-empty `discounting_override` formula,
#' evaluates the formula and replaces the per-cycle discount factor entirely.
#' The formulas are user-authored model definitions (same trust level as
#' value/variable formulas throughout the codebase), not untrusted external input.
#'
#' @param values_matrix Matrix of undiscounted values
#' @param values_discounted Matrix of discounted values (modified in place)
#' @param trace_matrix Collapsed trace matrix (rows = cycles 0..n, cols = states),
#'   or NULL for standalone decision tree models
#' @param discount_factors_cost Vector of per-cycle cost discount factors
#' @param discount_factors_outcomes Vector of per-cycle outcome discount factors
#' @param discount_cost_rate Cost discount rate (percentage)
#' @param discount_outcomes_rate Outcome discount rate (percentage)
#' @param uneval_values The unevaluated values data frame (with discounting_override column)
#' @param type_mapping Named vector mapping value names to types ("cost"/"outcome")
#' @param eval_vars The evaluated variables namespace (with $df and $env)
#'
#' @return Modified values_discounted matrix
#' @keywords internal
apply_discount_weights <- function(values_matrix, values_discounted, trace_matrix,
                                   discount_factors_cost, discount_factors_outcomes,
                                   discount_cost_rate, discount_outcomes_rate,
                                   uneval_values, type_mapping, eval_vars) {

  if (is.null(uneval_values) || nrow(uneval_values) == 0) return(values_discounted)
  if (!"discounting_override" %in% names(uneval_values)) return(values_discounted)

  n_cycles <- nrow(values_matrix)

  for (i in seq_len(nrow(uneval_values))) {
    v_name <- uneval_values$name[i]
    override_formula <- uneval_values$discounting_override[i]

    # Skip values without an override
    if (is.na(override_formula) || override_formula == "") next
    # Skip values not in the matrix
    if (!(v_name %in% colnames(values_matrix))) next

    # Determine type-appropriate discount rate and factors
    v_type <- if (v_name %in% names(type_mapping)) type_mapping[[v_name]] else "outcome"
    if (v_type == "cost") {
      discount_rate <- discount_cost_rate
      discount_factors <- discount_factors_cost
    } else {
      discount_rate <- discount_outcomes_rate
      discount_factors <- discount_factors_outcomes
    }

    # Build evaluation environment from eval_vars
    # Uses R's standard expression evaluation, consistent with how all other
    # model formulas are evaluated (eval_formula in R/formula.R uses eval_tidy).
    # These are user-authored model definitions, not untrusted external input.
    override_env <- new.env(parent = baseenv())

    # Copy scalars from eval_vars$env
    for (nm in ls(eval_vars$env)) {
      assign(nm, get(nm, envir = eval_vars$env), envir = override_env)
    }

    # Copy per-cycle vectors from eval_vars$df
    for (nm in colnames(eval_vars$df)) {
      assign(nm, eval_vars$df[[nm]], envir = override_env)
    }

    # Add discount_rate and discount_factors
    assign("discount_rate", discount_rate, envir = override_env)
    assign("discount_factors", discount_factors[seq_len(n_cycles)], envir = override_env)

    # Add trace() function - trace_matrix is already half-cycle corrected (n_cycles rows)
    if (!is.null(trace_matrix)) {
      local({
        tm <- trace_matrix
        trace_fn <- function(state_name) {
          if (!(state_name %in% colnames(tm))) {
            stop("State '", state_name, "' not found in trace. Available states: ",
                 paste(colnames(tm), collapse = ", "), call. = FALSE)
          }
          tm[, state_name]
        }
        assign("trace", trace_fn, envir = override_env)
      })
    } else {
      assign("trace", function(state_name) {
        stop("trace() is not available in standalone decision tree models.", call. = FALSE)
      }, envir = override_env)
    }

    # Evaluate the override formula (trusted user-authored model definition)
    parsed_expr <- parse(text = override_formula)
    result <- tryCatch(
      base::eval(parsed_expr, envir = override_env),
      error = function(e) {
        stop("Error evaluating discounting_override for value '", v_name, "': ",
             e$message, call. = FALSE)
      }
    )

    # Validate result
    if (!is.numeric(result)) {
      stop("discounting_override for value '", v_name,
           "' must evaluate to a numeric value, got ", class(result)[1], call. = FALSE)
    }
    if (length(result) != 1 && length(result) != n_cycles) {
      stop("discounting_override for value '", v_name,
           "' must evaluate to length 1 or ", n_cycles,
           ", got length ", length(result), call. = FALSE)
    }

    # Replace discounted values: undiscounted * override result
    values_discounted[, v_name] <- values_matrix[, v_name] * result
  }

  values_discounted
}

apply_segment_discounting <- function(model, eval_vars, uneval_values,
                                       values_undiscounted, corrected_trace,
                                       n_cycles, cycle_length_days) {
  discount_cost <- model$settings$discount_cost
  discount_outcomes <- model$settings$discount_outcomes

  days_per_year <- get_days_per_year(model$settings)
  cycle_length_years <- cycle_length_days / days_per_year

  discount_timing <- model$settings$discount_timing %||% "start"
  discount_method <- model$settings$discount_method %||% "by_cycle"

  dt_offset_years <- evaluate_dt_duration_years(model, eval_vars)

  discount_factors_cost <- calculate_discount_factors(n_cycles, discount_cost, cycle_length_years, discount_timing, discount_method, offset_years = dt_offset_years)
  discount_factors_outcomes <- calculate_discount_factors(n_cycles, discount_outcomes, cycle_length_years, discount_timing, discount_method, offset_years = dt_offset_years)

  type_mapping <- setNames(uneval_values$type, uneval_values$name)
  type_mapping <- type_mapping[!is.na(names(type_mapping))]

  # Identify DT values without overrides — these should never be discounted
  dt_value_names <- uneval_values$name[!is.na(uneval_values$state) & uneval_values$state == "decision_tree"]
  has_override <- if ("discounting_override" %in% names(uneval_values)) !is.na(uneval_values$discounting_override) & uneval_values$discounting_override != "" else rep(FALSE, nrow(uneval_values))
  dt_override_names <- uneval_values$name[!is.na(uneval_values$state) & uneval_values$state == "decision_tree" & has_override]
  dt_skip <- setdiff(dt_value_names, dt_override_names)

  values_discounted <- apply_discounting(
    values_undiscounted,
    discount_factors_cost,
    discount_factors_outcomes,
    type_mapping,
    skip = dt_skip
  )

  values_discounted <- apply_discount_weights(
    values_undiscounted,
    values_discounted,
    corrected_trace,
    discount_factors_cost, discount_factors_outcomes,
    discount_cost, discount_outcomes,
    uneval_values, type_mapping, eval_vars
  )

  values_discounted
}