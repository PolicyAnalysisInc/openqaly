
#' @importFrom rlang .data sym new_quosure enexpr expr is_call call2 env global_env empty_env enquo expr_text env_clone
#' @importFrom rlang parse_expr parse_quo eval_tidy quo_get_expr quo_set_env
#' @import tidygraph ggraph ggplot2 future dplyr furrr tidyr
#' @importFrom purrr map map_chr map_lgl map_dfr map2_dfr set_names flatten_chr flatten keep discard iwalk walk2 reduce
#' @importFrom dplyr select mutate group_by summarize left_join
#' @importFrom openxlsx readWorkbook getSheetNames createWorkbook addWorksheet writeData saveWorkbook write.xlsx
#' @importFrom tidyr separate_rows pivot_longer pivot_wider
#' @importFrom jsonlite fromJSON toJSON asJSON write_json validate
#' @importFrom tibble rownames_to_column tibble as_tibble
#' @importFrom stringr str_split_fixed
#' @importFrom utils capture.output write.csv
#' @importFrom readr read_csv read_file
#' @importFrom glue glue
#' @importFrom stats qnorm qlnorm pnorm
#' @importFrom tools file_ext
#' @importFrom mvnfast rmvn
#' @importFrom furrr future_map
#' @importFrom future plan multisession
#' @importFrom scales comma dollar dollar_format
#' @importFrom flextable flextable colformat_double add_header_row merge_at compose as_paragraph bg bold align border_remove hline hline_top hline_bottom width void border autofit
#' @importFrom officer fp_border
#' @importFrom knitr kable
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
                   "days_per_year", "days_per_month",
                    "group", "strategy", "simulation", "bc", "analysis_type", '.trees', 'class')

heRo_vars_keywords <- c("cycle_length_days", "cycle_length_weeks", "cycle_length_months", "cycle_length_years",
                        "cycle", "day", "week", "month", "year",
                        "cycle_lag", "day_lag", "week_lag", "month_lag", "year_lag",
                        "state_cycle", "state_day", "state_week", "state_month", "state_year",
                        "state_cycle_lag", "state_day_lag", "state_week_lag", "state_month_lag", "state_year_lag",
                        "days_per_year", "days_per_month",
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
                         'state_month', 'state_year', 'state_cycle_lag',
                         'state_day_lag', 'state_week_lag', 'state_month_lag', 'state_year_lag'
                        )

# Columns that are required in a variables definition
vars_def_columns <- c('name', 'display_name',	'description')

# Columns that are required in trees definition
tree_def_columns <- c('name', 'node', 'tags', 'parent', 'formula')

trans_markov_cols <- c('from_state', 'to_state', 'formula')

. <- NULL
