
get_cycle_length_days <- function(settings) {

  # Extract cycle length & unit from settings
  cl_unit <- settings$cycle_length_unit
  cl <- settings$cycle_length

  # Return NA if required settings are missing
  if (is.null(cl_unit) || is.null(cl)) {
    return(NA_real_)
  }

  if (cl_unit == 'cycles') {
    # Can't define cycle length in terms of cycles!
    stop('Error: Cannot define cycle length in terms of a number of cycles, please select another unit.', call. = FALSE)
  }

  days_per_cl_unit <- days_per_unit(cl_unit, settings$cycle_length_days, settings$days_per_year)
  # Calculate cycle length
  cl * days_per_cl_unit
}

get_days_per_year <- function(settings) {
  days_per_year <- default_days_per_year
  if (!is.null(settings$days_per_year)) {
    days_per_year <- settings$days_per_year
  }
  
  days_per_year
}

get_n_cycles <- function(settings, dt_duration_days = 0) {
  # If n_cycles is directly specified, use it
  if (!is.null(settings$n_cycles) && !is.na(settings$n_cycles)) {
    return(as.integer(settings$n_cycles))
  }

  # Otherwise calculate from timeframe
  # Extract timeframe & unit
  tf_unit <- settings$timeframe_unit
  tf <- settings$timeframe

  # Extract cycle length & unit
  cl_unit <- settings$cycle_length_unit
  cl <- settings$cycle_length

  # Return NA if required settings are missing
  if (is.null(tf_unit) || is.null(tf) || is.null(cl_unit) || is.null(cl)) {
    return(NA_integer_)
  }

  # Compute cycle_length_days fresh from raw settings
  cld <- get_cycle_length_days(settings)
  days_per_tf_unit <- days_per_unit(tf_unit, cld, settings$days_per_year)
  days_per_cl_unit <- days_per_unit(cl_unit, cld, settings$days_per_year)

  # Subtract decision tree duration from total timeframe
  total_days <- tf * days_per_tf_unit
  markov_days <- total_days - dt_duration_days
  if (markov_days <= 0) {
    stop("Decision tree duration (", dt_duration_days, " days) exceeds or equals ",
         "the total model timeframe (", total_days, " days). ",
         "Increase the timeframe or reduce the decision tree duration.",
         call. = FALSE)
  }

  # Calculate number of cycles
  as.integer(ceiling(markov_days / (cl * days_per_cl_unit)))
}

days_per_unit <- function(unit, cycle_length_days, days_per_year) {
  vswitch(
    tolower(unit),
    "day" = 1,
    "days" = 1,
    "week" = 7,
    "weeks" = 7,
    "month" = days_per_year / 12,
    "months" = days_per_year / 12,
    "year" = days_per_year,
    "years" = days_per_year,
    'cycles' = cycle_length_days
  )
}

convert_time <- function(x, from, to, settings) {
  x * days_per_unit(from, settings$cycle_length_days, settings$days_per_year) / days_per_unit (to, settings$cycle_length_days, settings$days_per_year)
}

# Generate time variables
time_variables <- function(settings, states, include_cycle_zero = FALSE, n_cycles = NULL,
                           st_cycles = NULL) {

  n_cycles <- n_cycles %||% get_n_cycles(settings)
  cl <- get_cycle_length_days(settings)

  # For Markov models with tunnel states, calculate max state time: a limit of
  # L cycles needs L + 1 state-cycle blocks (the last holds "more than L").
  # For PSM models, states don't have state_cycle_limit, so st_cycles = 1
  if (!is.null(st_cycles)) {
    st_cycles <- max(1, min(n_cycles, st_cycles))
  } else if ("state_cycle_limit_unit" %in% names(states) && "state_cycle_limit" %in% names(states)) {
    st_days <- days_per_unit(states$state_cycle_limit_unit, cl, settings$days_per_year) *
        as.numeric(states$state_cycle_limit)
    st_days[st_days == 0] <- Inf
    st_days_max <- max(st_days, na.rm = TRUE)
    if (!is.finite(st_days_max)) st_days_max <- cl * n_cycles
    st_cycles <- min(n_cycles, max(1, ceiling(st_days_max / cl) + 1), na.rm = TRUE)
    if (is.na(st_cycles)) st_cycles <- n_cycles
  } else {
    st_cycles <- 1
  }
  # Create vectors for model time variables
  # include_cycle_zero adds cycle 0 at the start of each state_cycle block (used
  # by PSMs to evaluate survival from time zero).
  if (include_cycle_zero) {
    cycle_vec <- rep(c(0L, seq_len(n_cycles)), times = st_cycles)
  } else {
    cycle_vec <- rep(seq_len(n_cycles), times = st_cycles)
  }
  day_vec <- cycle_vec * cl
  day_lag_vec <- day_vec - cl

  # Create vectors for state time variables
  # When include_cycle_zero, need n_cycles + 1 rows per state cycle. Each block
  # carries its own state_cycle for every time point, including time point 0.
  n_rows_per_st_cycle <- if (include_cycle_zero) n_cycles + 1 else n_cycles
  state_cycle_vec <- rep(seq_len(st_cycles), each = n_rows_per_st_cycle)
  state_day_vec <- state_cycle_vec * cl
  state_day_lag_vec <- state_day_vec - cl

  # Create a table of model & state time variables
  tibble(
    # Model time variables (i.e. time since start of model)
    cycle = cycle_vec,
    day = day_vec,
    week = convert_time(day_vec, from = 'Days', to = 'Weeks', settings),
    month = convert_time(day_vec, from = 'Days', to = 'Months', settings),
    year = convert_time(day_vec, from = 'Days', to = 'Years', settings),
    cycle_lag = cycle_vec - 1,
    day_lag = day_lag_vec,
    week_lag = convert_time(day_lag_vec, from = 'Days', to = 'Weeks', settings),
    month_lag = convert_time(day_lag_vec, from = 'Days', to = 'Months', settings),
    year_lag = convert_time(day_lag_vec, from = 'Days', to = 'Years', settings),
    # State time variables (i.e. time since entering state)
    state_cycle = state_cycle_vec,
    state_day = state_day_vec,
    state_week = convert_time(state_day_vec, from = 'Days', to = 'Weeks', settings),
    state_month = convert_time(state_day_vec, from = 'Days', to = 'Months', settings),
    state_year = convert_time(state_day_vec, from = 'Days', to = 'Years', settings),
    state_cycle_lag = state_cycle_vec - 1,
    state_day_lag = state_day_lag_vec,
    state_week_lag = convert_time(state_day_lag_vec, from = 'Days', to = 'Weeks', settings),
    state_month_lag = convert_time(state_day_lag_vec, from = 'Days', to = 'Months', settings),
    state_year_lag = convert_time(state_day_lag_vec, from = 'Days', to = 'Years', settings),
  )
}

# Generate cycle length variables
cycle_length_variables <- function(settings) {
  cl <- get_cycle_length_days(settings)

  # Return NA values if cycle length couldn't be determined
  if (is.na(cl)) {
    return(list(
      cycle_length_days = NA_real_,
      cycle_length_weeks = NA_real_,
      cycle_length_months = NA_real_,
      cycle_length_years = NA_real_
    ))
  }

  list(
    cycle_length_days = cl,
    cycle_length_weeks = convert_time(cl, from = 'Days', to = 'Weeks', settings),
    cycle_length_months = convert_time(cl, from = 'Days', to = 'Months', settings),
    cycle_length_years = convert_time(cl, from = 'Days', to = 'Years', settings)
  )
}

# Generate time unit variables
#
# Returns a list (not a data frame) so that these constants are assigned to the
# namespace environment as scalars, mirroring cycle_length_variables(). If they
# were returned as a 1-row data frame they would be cbind-ed into the per-cycle
# namespace data frame and become vectors of length n_cycles, which silently
# breaks formulas that combine them with fixed-length tables (R recycling).
time_unit_variables <- function(settings) {
  days_per_year <- get_days_per_year(settings)
  list(
    days_per_year = days_per_year,
    days_per_month = days_per_year / 12
  )
}

compute_time_context <- function(settings, dt_duration_days = 0) {
  list(
    cycle_length_days = get_cycle_length_days(settings),
    n_cycles = get_n_cycles(settings, dt_duration_days = dt_duration_days),
    days_per_year = get_days_per_year(settings)
  )
}


# --- Half-cycle allocation of point values ---------------------------------
#
# Values are evaluated at time points t = 0..n. A point value X(t) (value x
# occupancy, or value x flow arriving at t) is allocated to cycle k = 1..n by
# the half-cycle method:
#   start: X(k-1)   end: X(k)   life-table: (X(k-1) + X(k)) / 2
# The same rule is applied to discounted point values X(t) x d(t).

valid_half_cycle_methods <- c("start", "end", "life-table")

#' Allocate Point Values to Cycles
#'
#' @param x A vector of length n + 1, or a matrix with n + 1 rows (time points
#'   0..n) and one column per value.
#' @param methods Half-cycle method: a single string, or one per column of `x`.
#' @return A vector of length n, or a matrix with n rows (cycles 1..n).
#' @keywords internal
allocate_point_values <- function(x, methods) {
  if (is.null(dim(x))) {
    # Elementwise (not a matrix product) so NA at an unused time point stays unused
    return(as.numeric(allocate_columns(matrix(x, ncol = 1), methods[1])))
  }
  n_cycles <- nrow(x) - 1
  methods <- rep_len(methods, ncol(x))
  res <- matrix(0, nrow = n_cycles, ncol = ncol(x), dimnames = list(seq_len(n_cycles), colnames(x)))
  for (m in unique(methods)) {
    cols <- which(methods == m)
    res[, cols] <- allocate_columns(x[, cols, drop = FALSE], m)
  }
  res
}

allocate_columns <- function(x, method) {
  n_points <- nrow(x)
  first <- x[-n_points, , drop = FALSE]
  last <- x[-1, , drop = FALSE]
  switch(method,
    "end" = last,
    "life-table" = (first + last) / 2,
    first  # "start" default
  )
}

point_allocation_matrix <- function(n_cycles, method) {
  first <- cbind(diag(n_cycles), 0)
  last <- cbind(0, diag(n_cycles))
  switch(method,
    "end" = last,
    "life-table" = (first + last) / 2,
    first  # "start" default
  )
}

#' Half-Cycle Weights of Time Points
#'
#' The total weight each time point 0..n receives when point values are
#' allocated to cycles with `method`: start 1..1, 0; end 0, 1..1;
#' life-table 1/2, 1..1, 1/2.
#'
#' @param n_cycles Number of cycles.
#' @param method Half-cycle method.
#' @return A numeric vector of length n_cycles + 1.
#' @keywords internal
point_weights <- function(n_cycles, method) {
  colSums(point_allocation_matrix(n_cycles, method))
}
