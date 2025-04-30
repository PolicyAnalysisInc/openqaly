
get_cycle_length_days <- function(settings) {

  # Extract cycle length & unit from settings
  cl_unit <- settings$cycle_length_unit
  if (cl_unit == 'cycles') {
    # Can't define cycle length in terms of cycles!
    stop('Error: Cannot define cycle length in terms of a number of cycles, please select another unit.', call. = FALSE)
  }
  
  days_per_cl_unit <- days_per_unit(cl_unit, settings$cycle_length_days, settings$days_per_year)
  cl <- settings$cycle_length
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
  

  # Extract timeframe & unit
  tf_unit <- settings$timeframe_unit
  tf <- settings$timeframe
  
  # Extract cycle length & unit
  cl_unit <- settings$cycle_length_unit
  cl <- settings$cycle_length
  
  # Calculate days per timeframe/cycle length unit
  days_per_tf_unit <- days_per_unit(tf_unit, settings$cycle_length_days, settings$days_per_year)
  days_per_cl_unit <- days_per_unit(cl_unit, settings$cycle_length_days, settings$days_per_year)
  
  # Calculate number of cycles
  ceiling((tf * days_per_tf_unit) / (cl * days_per_cl_unit))
}

days_per_unit <- function(unit, cycle_length_days, days_per_year) {
  vswitch(
    tolower(unit),
    "days" = 1,
    "weeks" = 7,
    "months" = days_per_year / 12,
    "years" = days_per_year,
    'cycles' = cycle_length_days
  )
}

convert_time <- function(x, from, to, settings) {
  x * days_per_unit(from, settings$cycle_length_days, settings$days_per_year) / days_per_unit (to, settings$cycle_length_days, settings$days_per_year)
}

# Generate time variables
time_variables <- function(settings, states) {

  n_cycles <- get_n_cycles(settings)
  cl <- get_cycle_length_days(settings)
  st_days_max <- max(
    days_per_unit(states$state_cycle_limit_unit, settings$cycle_length_days, settings$days_per_year) *
      as.numeric(states$state_cycle_limit)
  )
  st_cycles <- min(n_cycles, max(1, floor(st_days_max / cl)), na.rm = TRUE)
  if (is.na(st_cycles)) st_cycles <- n_cycles
  # Create a table of model & state time variables
  tibble(
    # Model time variables (i.e. time since start of model)
    cycle = rep(seq_len(n_cycles), times = st_cycles),
    day = cycle * cl,
    week = convert_time(day, from = 'Days', to = 'Weeks', settings),
    month = convert_time(day, from = 'Days', to = 'Months', settings),
    year = convert_time(day, from = 'Days', to = 'Years', settings),
    cycle_lag = cycle - 1,
    day_lag = day - cl,
    week_lag = convert_time(day_lag, from = 'Days', to = 'Weeks', settings),
    month_lag = convert_time(day_lag, from = 'Days', to = 'Months', settings),
    year_lag = convert_time(day_lag, from = 'Days', to = 'Years', settings),
    # State time variables (i.e. time since entering state)
    state_cycle = rep(seq_len(st_cycles), each = n_cycles),
    state_day = state_cycle * cl,
    state_week = convert_time(state_day, from = 'Days', to = 'Weeks', settings),
    state_month = convert_time(state_day, from = 'Days', to = 'Months', settings),
    state_year = convert_time(state_day, from = 'Days', to = 'Years', settings),
    state_cycle_lag = state_cycle - 1,
    state_day_lag = state_day - cl,
    state_week_lag = convert_time(state_day_lag, from = 'Days', to = 'Weeks', settings),
    state_month_lag = convert_time(state_day_lag, from = 'Days', to = 'Months', settings),
    state_year_lag = convert_time(state_day_lag, from = 'Days', to = 'Years', settings),
  )
}

# Generate cycle length variables
cycle_length_variables <- function(settings) {
  cl <- get_cycle_length_days(settings)
  tibble(
    cycle_length_days = cl,
    cycle_length_weeks = convert_time(cl, from = 'Days', to = 'Weeks', settings),
    cycle_length_months = convert_time(cl, from = 'Days', to = 'Months', settings),
    cycle_length_years = convert_time(cl, from = 'Days', to = 'Years', settings)
  )
}

