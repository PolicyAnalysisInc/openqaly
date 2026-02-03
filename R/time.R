
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

get_n_cycles <- function(settings) {
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

  # Calculate days per timeframe/cycle length unit
  days_per_tf_unit <- days_per_unit(tf_unit, settings$cycle_length_days, settings$days_per_year)
  days_per_cl_unit <- days_per_unit(cl_unit, settings$cycle_length_days, settings$days_per_year)

  # Calculate number of cycles
  as.integer(ceiling((tf * days_per_tf_unit) / (cl * days_per_cl_unit)))
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
time_variables <- function(settings, states, include_cycle_zero = FALSE) {

  n_cycles <- get_n_cycles(settings)
  cl <- get_cycle_length_days(settings)

  # For Markov models with tunnel states, calculate max state time
  # For PSM models, states don't have state_cycle_limit, so st_cycles = 1
  if ("state_cycle_limit_unit" %in% names(states) && "state_cycle_limit" %in% names(states)) {
    st_days <- days_per_unit(states$state_cycle_limit_unit, settings$cycle_length_days, settings$days_per_year) *
        as.numeric(states$state_cycle_limit)
    st_days[st_days == 0] <- Inf
    st_days_max <- max(st_days, na.rm = TRUE)
    if (!is.finite(st_days_max)) st_days_max <- cl * n_cycles
    st_cycles <- min(n_cycles, max(1, floor(st_days_max / cl)), na.rm = TRUE)
    if (is.na(st_cycles)) st_cycles <- n_cycles
  } else {
    st_cycles <- 1
  }
  # Create vectors for model time variables
  # include_cycle_zero adds cycle 0 at the start (used by PSM Custom for trace calculation)
  if (include_cycle_zero) {
    cycle_vec <- rep(c(0L, seq_len(n_cycles)), times = st_cycles)
  } else {
    cycle_vec <- rep(seq_len(n_cycles), times = st_cycles)
  }
  day_vec <- cycle_vec * cl
  day_lag_vec <- day_vec - cl

  # Create vectors for state time variables
  # When include_cycle_zero, need n_cycles + 1 rows per state cycle
  n_rows_per_st_cycle <- if (include_cycle_zero) n_cycles + 1 else n_cycles
  if (include_cycle_zero) {
    # For cycle 0, state_cycle is also 0; for cycles 1:n_cycles, state_cycle is 1:st_cycles pattern
    state_cycle_vec <- rep(c(0L, seq_len(st_cycles)), each = n_rows_per_st_cycle)[seq_along(cycle_vec)]
  } else {
    state_cycle_vec <- rep(seq_len(st_cycles), each = n_rows_per_st_cycle)
  }
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
    return(tibble(
      cycle_length_days = NA_real_,
      cycle_length_weeks = NA_real_,
      cycle_length_months = NA_real_,
      cycle_length_years = NA_real_
    ))
  }

  tibble(
    cycle_length_days = cl,
    cycle_length_weeks = convert_time(cl, from = 'Days', to = 'Weeks', settings),
    cycle_length_months = convert_time(cl, from = 'Days', to = 'Months', settings),
    cycle_length_years = convert_time(cl, from = 'Days', to = 'Years', settings)
  )
}

# Generate time unit variables
time_unit_variables <- function(settings) {
  days_per_year <- get_days_per_year(settings)
  tibble(
    days_per_year = days_per_year,
    days_per_month = days_per_year / 12
  )
}

