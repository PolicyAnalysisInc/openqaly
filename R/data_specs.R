#' Input Data Specifications
#' 
#' A list of dataframes describing the format of model input dataframes,
#' including which columns are required and how missing values should be
#' imputed.
#' 
#' @format A list of dataframes
model_input_specs <- system.file('model_input_specs', package = 'heRomod2') %>%
  list.files() %>%
  purrr::set_names(stringr::str_split_fixed(., '\\.', Inf)[,1]) %>%
  purrr::map(function(x) {
    suppressWarnings(readr::read_csv(
      system.file('model_input_specs', x, package = 'heRomod2'),
      col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c', 'default' = 'c', 'fallback' = 'c'),
      progress = FALSE
    ))
  })
