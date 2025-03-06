#' @import dplyr
#' @import furrr
#' @import future
#' @import tidyr
#' @import purrr tidygraph ggraph ggplot2
#' @importFrom stringr str_split_fixed
#' @importFrom openxlsx read_workbook
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom glue glue
#' @importFrom lazyeval lazy lazy_eval interp as.lazy as.lazy_dots
#' @importFrom rlang .data sym
#' @importFrom readr read_csv
#' @importFrom utils capture.output
NULL

## usethis namespace: start
#' @useDynLib heRomod2
## usethis namespace: end
NULL

heRo_keywords <- c("cycle_length_days", "cycle_length_weeks", "cycle_length_months", "cycle_length_years",
                   "cycle", "day", "week", "month", "year",
                   "cycle_lag", "day_lag", "week_lag", "month_lag", "year_lag",
                   "state_cycle", "state_day", "state_week", "state_month", "state_year",
                   "state_cycle_lag", "state_day_lag", "state_week_lag", "state_month_lag", "state_year_lag",
                    "group", "strategy", "simulation", "bc", "analysis_type", '.trees', 'class')

heRo_vars_keywords <- c("cycle_length_days", "cycle_length_weeks", "cycle_length_months", "cycle_length_years",
                        "cycle", "day", "week", "month", "year",
                        "cycle_lag", "day_lag", "week_lag", "month_lag", "year_lag",
                        "state_cycle", "state_day", "state_week", "state_month", "state_year",
                        "state_cycle_lag", "state_day_lag", "state_week_lag", "state_month_lag", "state_year_lag",
                        "group", "strategy")

error_codes <- list(
  generic = '#ERR: ',
  invalid_expression = '#ERR: Invalid Expression'
)

#' @export
C <- -pi
strat_var_code <- 'strategy'
group_var_code <- 'group'
segment_vars <- c(strat_var_code, group_var_code)
global_var_codes <- c('global', '')

tf_unit_code <- 'timeframe_unit'
tf_code <- 'timeframe'

cl_unit_code <- 'cycle_length_unit'
cl_code <- 'cycle_length'

default_days_per_year <- 365

# Keywords that represent a reference to state time
state_time_keywords <- c('state_cycle', 'state_day', 'state_week',
                         'state_month', 'state_year')

# Columns that are required in a variables definition
vars_def_columns <- c('name', 'display_name',	'description')

# Columns that are required in trees definition
tree_def_columns <- c('name', 'node', 'tags', 'parent', 'formula')

trans_markov_cols <- c('from', 'to', 'formula')

. <- NULL
