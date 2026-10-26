#' Calculate Discount Factors
#'
#' Calculate discount factors at each time point t = 0..n using
#' 1/(1+r)^tau(t), where tau(t) is the time in years from the start of the
#' model: t times the cycle length in years, plus `offset_years`. The
#' half-cycle method sets which time points a cycle's amount is discounted at:
#' its start (time point k-1), its end (time point k), or half at each
#' (life-table). Time point 0 has a discount factor of 1 (no discounting) when
#' there is no offset.
#'
#' @param n_cycles Number of cycles
#' @param discount_rate Annual discount rate as a percentage (e.g., 3.5 for 3.5\%)
#' @param cycle_length_years Length of each cycle in years (e.g., 1/12 for monthly, 1/52 for weekly)
#' @param discount_method Whether discounting steps annually or continuously:
#'   \code{"by_cycle"} (default, continuous time) or \code{"by_year"} (floor to whole years).
#' @param offset_years Offset in years added to all time points before applying
#'   discounting. Used when a decision tree phase precedes the model, shifting
#'   all discounting forward. Default is 0.
#'
#' @return Vector of n_cycles + 1 discount factors, for time points 0..n_cycles
#' @export
calculate_discount_factors <- function(n_cycles, discount_rate, cycle_length_years = 1,
                                       discount_method = "by_cycle",
                                       offset_years = 0) {
  if (n_cycles <= 0) {
    return(numeric(0))
  }

  # Validate discount_method
  if (!discount_method %in% valid_discount_methods) {
    stop("Invalid discount_method: '", discount_method, "'. Must be one of: ",
         paste(valid_discount_methods, collapse = ", "))
  }

  time_years <- discount_times(n_cycles, cycle_length_years, offset_years)

  # Apply annual stepping if requested
  if (discount_method == "by_year") {
    time_years <- floor(time_years)
  }

  # Calculate discount factors based on time in years
  1 / (1 + discount_rate / 100)^time_years
}

# Time in years of each time point 0..n, including the decision tree offset
discount_times <- function(n_cycles, cycle_length_years, offset_years = 0) {
  (0:n_cycles) * cycle_length_years + offset_years
}

#' Evaluate a Discounting Override Formula
#'
#' A value with a non-empty `discounting_override` formula replaces its
#' discount factors with the formula's result. The formulas are user-authored
#' model definitions (same trust level as value/variable formulas throughout
#' the codebase), not untrusted external input.
#'
#' The formula is evaluated with:
#' \itemize{
#'   \item model variables and time variables (`cycle`, `year`, ...) for cycles
#'     1..n (model time only: the `state_cycle == 1` block);
#'   \item `discount_rate`: the value's discount rate (percentage);
#'   \item `discount_factors`: the value's default discount factors d(t) at time
#'     points 0..n;
#'   \item `discount_time`: the time in years of each time point, including any
#'     decision tree offset, before `by_year` flooring;
#'   \item `trace(state)`: person-time weight of each time point, i.e. occupancy
#'     at t times the weight the value's half-cycle method gives time point t
#'     (start: 1, ..., 1, 0; end: 0, 1, ..., 1; life-table: 1/2, 1, ..., 1, 1/2).
#'     `sum(trace(s))` is the undiscounted person-cycles in state s, and
#'     `sum(discount_factors * trace(s)) / sum(trace(s))` is the average
#'     discount factor of time spent in state s.
#' }
#'
#' The result must have length 1 (a multiplier for the value's undiscounted
#' cycle totals) or n + 1 (discount factors for time points 0..n, used in place
#' of d(t)).
#'
#' @param formula The override formula text.
#' @param value_name The value name (for error messages).
#' @param eval_vars The evaluated namespace, or `NULL`.
#' @param trace_matrix Collapsed trace (rows = time points 0..n), or `NULL`.
#' @param weights Half-cycle weights of the time points 0..n, or `NULL`.
#' @param discount_rate The value's discount rate (percentage).
#' @param discount_factors The value's discount factors at time points 0..n.
#' @param discount_time Time in years of each time point.
#' @param n_points Number of time points (n + 1), or 1 for decision trees.
#'
#' @return Numeric vector of length 1 or `n_points`.
#' @keywords internal
evaluate_discounting_override <- function(formula, value_name, eval_vars, trace_matrix, weights,
                                          discount_rate, discount_factors, discount_time,
                                          n_points) {
  # Uses R's standard expression evaluation, consistent with how all other
  # model formulas are evaluated (eval_formula in R/formula.R uses eval_tidy).
  override_env <- new.env(parent = baseenv())

  if (!is.null(eval_vars)) {
    # Copy scalars from eval_vars$env
    for (nm in ls(eval_vars$env)) {
      assign(nm, get(nm, envir = eval_vars$env), envir = override_env)
    }
    # Copy per-cycle vectors from eval_vars$df (cycles 1..n, model time only)
    df <- eval_vars$df
    if ("cycle" %in% colnames(df)) {
      df <- df[df$cycle != 0, , drop = FALSE]
    }
    if ("state_cycle" %in% colnames(df)) {
      df <- df[df$state_cycle == min(df$state_cycle), , drop = FALSE]
    }
    for (nm in colnames(df)) {
      assign(nm, df[[nm]], envir = override_env)
    }
  }

  assign("discount_rate", discount_rate, envir = override_env)
  assign("discount_factors", discount_factors, envir = override_env)
  assign("discount_time", discount_time, envir = override_env)

  if (!is.null(trace_matrix)) {
    local({
      tm <- trace_matrix
      w <- weights
      trace_fn <- function(state_name) {
        if (!(state_name %in% colnames(tm))) {
          stop("State '", state_name, "' not found in trace. Available states: ",
               paste(colnames(tm), collapse = ", "), call. = FALSE)
        }
        as.numeric(tm[, state_name]) * w
      }
      assign("trace", trace_fn, envir = override_env)
    })
  } else {
    assign("trace", function(state_name) {
      stop("trace() is not available in standalone decision tree models.", call. = FALSE)
    }, envir = override_env)
  }

  # Evaluate the override formula (trusted user-authored model definition)
  parsed_expr <- parse(text = formula)
  result <- tryCatch(
    base::eval(parsed_expr, envir = override_env),
    error = function(e) {
      stop("Error evaluating discounting_override for value '", value_name, "': ",
           e$message, call. = FALSE)
    }
  )

  if (!is.numeric(result)) {
    stop("discounting_override for value '", value_name,
         "' must evaluate to a numeric value, got ", class(result)[1], call. = FALSE)
  }
  if (length(result) != 1 && length(result) != n_points) {
    if (n_points == 1) {
      stop("discounting_override for value '", value_name,
           "' must evaluate to a single number, got length ", length(result), call. = FALSE)
    }
    hint <- if (length(result) == n_points - 1) {
      paste0(" Model variables have one value per cycle; for one factor per time point use ",
             "discount_time or discount_factors (e.g. 1.015^-discount_time).")
    } else ""
    stop("discounting_override for value '", value_name,
         "' must evaluate to length 1 or ", n_points, " (time points 0..", n_points - 1,
         "), got length ", length(result), ".", hint, call. = FALSE)
  }
  as.numeric(result)
}

#' Effective Half-Cycle Method of Each Value
#'
#' A value's own `half_cycle_method` if set, otherwise the model setting.
#'
#' @param model The model.
#' @param value_names Value names.
#' @return Character vector named by value name.
#' @keywords internal
get_value_half_cycle_methods <- function(model, value_names) {
  value_half_cycle_methods(model$values, value_names, model$settings$half_cycle_method)
}

#' Correct and Discount Segment Values
#'
#' Combines each value's start and end parts with its half-cycle method and
#' discounts each part at its own time point. The half-cycle method sets both:
#' start uses the start of each cycle, discounted at d(k-1); end the end,
#' discounted at d(k); life-table half of each, each discounted at its own
#' time. Transitional values are charged in the cycle they occur under every
#' method and discounted with the same weights. Model start values are charged
#' in cycle 1 at d(0); decision tree values are not discounted. Values with a
#' `discounting_override` use the override result instead of d (see
#' [evaluate_discounting_override()]).
#'
#' @param model The model.
#' @param eval_vars Evaluated namespace.
#' @param uneval_values Unevaluated values (with type and discounting_override).
#' @param value_parts List with `start`, `end` and `transitional` (cycles x
#'   values matrices) and `model_start` and `dt` (named vectors of cycle 1
#'   totals).
#' @param trace_matrix Collapsed trace (rows = time points 0..n).
#' @param n_cycles Number of cycles.
#' @param cycle_length_days Cycle length in days.
#'
#' @return List with `values` and `values_discounted` (n x values matrices).
#' @keywords internal
calculate_segment_values <- function(model, eval_vars, uneval_values, value_parts,
                                     trace_matrix, n_cycles, cycle_length_days) {
  value_names <- colnames(value_parts$start)
  n_points <- n_cycles + 1
  methods <- get_value_half_cycle_methods(model, value_names)

  start_part <- setNames(rep(0, length(value_names)), value_names)
  dt_part <- start_part
  ms <- value_parts$model_start[intersect(names(value_parts$model_start), value_names)]
  dt <- value_parts$dt[intersect(names(value_parts$dt), value_names)]
  start_part[names(ms)] <- ms
  dt_part[names(dt)] <- dt

  # Undiscounted values
  values <- combine_half_cycle_parts(value_parts, methods)
  values[1, ] <- values[1, ] + start_part + dt_part

  # Discount factors at time points
  days_per_year <- get_days_per_year(model$settings)
  cycle_length_years <- cycle_length_days / days_per_year
  discount_method <- model$settings$discount_method %||% "by_cycle"
  dt_offset_years <- evaluate_dt_duration_years(model, eval_vars)
  rates <- c(cost = model$settings$discount_cost, outcome = model$settings$discount_outcomes)
  factors <- lapply(rates, function(r) {
    calculate_discount_factors(n_cycles, r, cycle_length_years, discount_method, offset_years = dt_offset_years)
  })
  disc_time <- discount_times(n_cycles, cycle_length_years, dt_offset_years)

  type_mapping <- setNames(uneval_values$type, uneval_values$name)
  type_mapping <- type_mapping[!is.na(names(type_mapping))]
  overrides <- get_discounting_overrides(uneval_values)

  values_discounted <- values
  for (v_name in value_names) {
    v_type <- if (v_name %in% names(type_mapping) && identical(type_mapping[[v_name]], "cost")) "cost" else "outcome"
    d <- factors[[v_type]]
    override <- overrides[v_name]

    if (!is.na(override)) {
      result <- evaluate_discounting_override(
        override, v_name, eval_vars, trace_matrix, point_weights(n_cycles, methods[[v_name]]),
        rates[[v_type]], d, disc_time, n_points
      )
      if (length(result) == 1) {
        values_discounted[, v_name] <- values[, v_name] * result
        next
      }
      d <- result
      dt_factor <- result[1]
    } else {
      dt_factor <- 1
    }

    trans <- value_parts$transitional[, v_name]
    disc <- half_cycle_amounts(
      value_parts$start[, v_name] + trans, value_parts$end[, v_name] + trans,
      methods[[v_name]], d_start = d[-n_points], d_end = d[-1]
    )
    disc[1] <- disc[1] + scale_nonzero(start_part[[v_name]], d[1]) +
      scale_nonzero(dt_part[[v_name]], dt_factor)
    values_discounted[, v_name] <- disc
  }

  list(values = values, values_discounted = values_discounted)
}

# x * f, treating a zero amount as zero even when f is NA (unused factor)
scale_nonzero <- function(x, f) {
  if (x == 0) 0 else x * f
}

# Named vector of override formula text by value name (NA when none)
get_discounting_overrides <- function(uneval_values) {
  if (is.null(uneval_values) || nrow(uneval_values) == 0 ||
      !"discounting_override" %in% names(uneval_values)) {
    return(setNames(character(0), character(0)))
  }
  ov <- as.character(uneval_values$discounting_override)
  has <- !is.na(ov) & ov != "" & ov != "NA" & !is.na(uneval_values$name)
  res <- ov[has]
  names(res) <- uneval_values$name[has]
  res[!duplicated(names(res))]
}

#' Apply Discounting Overrides to Standalone Decision Tree Values
#'
#' Decision tree values occur at time zero and are not discounted, unless a
#' value has a `discounting_override`, whose single-number result multiplies the
#' value. `trace()` is not available.
#'
#' @param values_matrix 1-row matrix of values.
#' @param uneval_values Unevaluated values.
#' @param eval_vars Evaluated namespace.
#' @param model The model.
#' @return The discounted 1-row matrix.
#' @keywords internal
apply_dt_discount_overrides <- function(values_matrix, uneval_values, eval_vars, model) {
  overrides <- get_discounting_overrides(uneval_values)
  type_mapping <- setNames(uneval_values$type, uneval_values$name)
  values_discounted <- values_matrix
  for (v_name in intersect(names(overrides), colnames(values_matrix))) {
    v_type <- if (identical(type_mapping[[v_name]], "cost")) "cost" else "outcome"
    rate <- if (v_type == "cost") model$settings$discount_cost %||% 0 else model$settings$discount_outcomes %||% 0
    result <- evaluate_discounting_override(
      overrides[[v_name]], v_name, eval_vars, NULL, NULL, rate, 1, 0, n_points = 1
    )
    values_discounted[, v_name] <- values_matrix[, v_name] * result
  }
  values_discounted
}
