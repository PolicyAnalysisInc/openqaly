#' @export
read_model <- function(path) {

  model <- read_workbook(file.path(path, 'model.xlsx'))

  model$tables <- list.files(file.path(path, 'data')) %>%
    purrr::set_names(., gsub('.csv$', '', .)) %>%
    purrr::map(~read.csv(file.path(path, 'data', .), stringsAsFactor = F, check.names = F))

  model$scripts <- list.files(file.path(path, 'scripts')) %>%
    purrr::set_names(., gsub('.R$', '', ., fixed = T)) %>%
    purrr::map(~readr::read_file(file.path(path,'scripts', .)))

  model$settings <- convert_settings_from_df(model$settings)
  
  define_object_(model, 'heRomodel')
}

convert_settings_from_df <- function(settings_df) {
  settings <- map(settings_df$value, function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (is.na(num)) return(tolower(x))
    num
  })
  names(settings) <- settings_df$setting
  settings
}


#' Read an Excel Workbook
#' 
#' Takes the path of an excel workbook and reads it in as a named list of
#' data frames.
#' 
#' @param path the path to the workbook.
#' 
#' @return a named list of data.frames
#' 
#' @export
read_workbook <- function(path) {
  sheet_names <- getSheetNames(path)
  names(sheet_names) <- sheet_names
  lapply(sheet_names, function(x) as_tibble(readWorkbook(path, sheet = x)))
}

define_object <- function(..., class) {
  define_object_(list(...), class)
}

define_object_ <- function(obj, class) {
  class(obj) <- class
  obj
}

create_default_group <- function() {
  tibble(
    name = 'all',
    display_name = 'All Patients',
    description = 'Entire model population.',
    weight = 1,
    enabled = 1
  )
}

load_tables <- function(tables, env) {
  for (name in names(tables)) {
    assign(name, tables[[name]], envir = env)
  }
}

load_trees <- function(trees, env) {
  if ((!is.null(trees)) && nrow(trees) > 0) {
    env$.trees <- trees
  }
}

run_scripts <- function(scripts, env) {
  for (name in names(scripts)) {
    eval(parse(text = scripts[[name]]), envir = env)
  }
}


get_segments <- function(model) {
  
  if (nrow(model$groups) == 0) {
    model$groups <- tibble::tibble(
      name = 'all',
      display_name = 'All Patients',
      description = 'All Patients',
      weight = 1,
      enabled = 1
    )
  }
  
  expand.grid(
    group = model$groups$name,
    strategy =  model$strategies$name,
    stringsAsFactors = FALSE
  )
}

check_missing_colnames <- function(x, names) {
  names[which(!(names %in% colnames(x)))]
}

is_in_segment <- function(segment, strat = NULL, grp = NULL, include_globals = T) {
  
  if (!is.null(strat)) {
    is_my_strat <- segment$strategy == strat & !is.na(strat)
    not_strat_spec <- (is.na(strat) | is.null(strat) | strat == '') & include_globals
  } else {
    is_my_strat <- F
    not_strat_spec <- include_globals
  }
  if (!is.null(grp)) {
    is_my_group <- segment$group == grp & !is.na(grp)
    not_group_spec <- (is.na(grp) | is.null(grp) | grp == '') & include_globals
  } else {
    is_my_group <- F
    not_group_spec <- include_globals
  }
  
  (is_my_strat | not_strat_spec) & (is_my_group | not_group_spec)
}

parse_csl <- function(string, flatten = T) {
  gsub('\\s', '', string) %>%
    strsplit('[,\\s]+', perl = T) %>%
    {if (flatten) flatten_chr(.) else .}
}


#' Vectorized Switch Statement
#' 
#' @param x The condition statement
#' @param ... name-value pairs where name repesents cases of condition
#' statement and values represent the value it will take on in each case.
#' 
#' @return Returns value for the given case.
#' 
#' @export
vswitch <- function(x, ...) {
  args <- list(...)
  opts <- names(args)
  xMat <- matrix(rep(x, length(args)), nrow = length(x), ncol = length(args), byrow = T)
  optsMat <- matrix(rep(opts, length(x)), nrow = length(x), ncol = length(args), byrow = T)
  mat <- as.matrix(do.call(tibble, args))
  if (nrow(mat) == 1) {
    mat <- mat[rep(1, nrow(xMat)), ]
  }
  unname(mat[which(xMat == optsMat)])
}

is.empty <- function(x) {
  is.na(x) | x == ''
}

`%&%` <- function(a,b) { paste0(a,b)}

extract_call_vars <- function(expr) {
  call_vars <- lapply(expr, function(x) all.vars(x))
  names(call_vars) <- c('func', paste0('arg', seq_len(length(expr) - 1)))
  call_vars
}

extract_func_calls <- function(expr, funcs) {
  if (is.call(expr)) {
    if (as.character(expr[[1]]) %in% funcs) {
      ret <- list(extract_call_vars(expr))
    } else {
      ret <- unlist(
        lapply(expr, function(x) extract_func_calls(x, funcs)),
        recursive = F
      )
    }
  } else if (is.name(expr) || is.atomic(expr)) {
    ret <- list()
  } else {
    ret <- unlist(
      lapply(expr, function(x) extract_func_calls(x, funcs)),
      recursive = F
    )
  }
  
  ret
}

has_st_dependency <- function(x, extras = NULL) {
  any(x$depends %in% c(state_time_keywords, extras))
}


check_state_time <- function(vars, states, transitions, values) {

  # Identify which vars have references to state time
  st_vars <- vars$name[map_lgl(vars$formula, ~has_st_dependency(.))]
  
  # Combine values & transitions, group by state, and identify
  # references to state time or variables referencing state time
  st_df <- select(transitions, from, formula) %>%
    rename(state = from) %>%
    rbind(
      select(values, state, formula)
    ) %>%
    filter(!is.na(state)) %>%
    group_by(state) %>%
    do({
      tibble(
        state = .$state[1],
        uses_st = any(map_lgl(.$formula, ~has_st_dependency(., extras = st_vars)))
      )
    }) %>%
    left_join(select(states, name, max_state_time), by = c('state' = 'name')) %>%
    transmute(
      state,
      uses_st = ifelse(max_state_time > 1 && uses_st, TRUE, FALSE)
    )
  
  st_df
}

#' Generate List of Names for Error Messages
#' 
#' Generates a comma-separated character of quoted names in order to list
#' items referenced in error messages.
#' 
#' @param x character vector of names
#' 
#' @return character with comma-separated list of quoted names
err_name_string <- function(x) {
  paste0('"', x, '"', collapse = ', ')
}

#' Check if Name is Valid
#' 
#' Checks character vector to see if elements follow the rules of heRomod2 variable names,
#' which must start with a letter and include only letters, numbers, and underscores.
#' 
#' @param x character vector of names to check
#' 
#' @return logical vector indicating whether the elements of `x` are valid.
is_valid_name <- function(x) grepl('^[[:alpha:]]+[[:alnum:]_]*$', x)

#' Check If Character is Empty/Missing
#' 
#' Check if a character is either `NA` or empty.
#' 
#' @param x character vector to be checked
#' 
#' @return logical vector indicating whether the elements of `x` are empty or missing.
is_faslsy_chr <- function(x) {
  is.na(x) | x == ''
}


#' Check Table
#' 
#' Check a model inputs dataframe based on a given specification dataframe. Specification
#' dataframes are used to check input dataframes, ensure that required columns are present
#' and impute missing values for non-required columns.
#' 
#' @param df The input dataframe to be checked
#' @param spec The specification dataframe
#' @param context A string used in error messages to indicate the type of input dataframe
#' 
#' @return The input dataframe with missing values imputed
check_tbl <- function(df, spec, context) {
  
  spec_cn <- spec$name
  n_col <- nrow(spec)
  n_row <- nrow(df)
  
  for (i in seq_len(n_col)) {
    col_name <- spec$name[i]
    required <- spec$required[i]
    type <- spec$type[i]
    default <- spec$default[i]
    fallback <- spec$fallback[i]
    values <- df[[spec_cn[i]]]
    
    # Handle case where the entire column is missing
    if (is.null(values)) {
      miss_data <- rep(TRUE, n_row)
    } else {
      miss_data <- is_faslsy_chr(values)
    }
    
    # Impute missing values
    if (any(miss_data)) {
      if (required) {
        # Throw error if a required column has missing values
        err_msg <- glue('{context} has missing values in required column: {col_name}.')
        stop(err_msg, call. = F)
      } else if (!is.na(fallback)) {
        # If a fallback is specified, replace missing values with fallback
        values[miss_data] <- df[[fallback]][miss_data]
      } else {
        # If a default is specified, replace missing values with default
        values[miss_data] <- default
      }
    }
    
    # Update data frame
    df[[col_name]] <- convert_to_type(values, type)
  }
  
  # Use only the columns defined in the spec
  select(df, {{spec_cn}})
}

convert_to_type <- function(x, type) {
  func <- eval(parse(text = paste0('as.', type)))
  func(x)
}

#' Read Model from JSON
#' 
#' Takes a JSON string and parses it into a heRomodel object.
#' 
#' @param json_string A string containing the model in JSON format.
#' 
#' @return A heRomodel object
#' 
#' @export
read_model_json <- function(json_string) {
  # Parse JSON string into a list
  model <- fromJSON(json_string, simplifyVector = TRUE)
  
  # Process tables - use from JSON or empty if not present
  if (is.null(model$tables)) {
    model$tables <- list()
  } else if (!is.list(model$tables)) {
    # If tables exists but isn't a list, convert it
    model$tables <- as.list(model$tables)
  }
  
  # Process scripts - use from JSON or empty if not present
  if (is.null(model$scripts)) {
    model$scripts <- list()
  } else if (!is.list(model$scripts)) {
    # If scripts exists but isn't a list, convert it
    model$scripts <- as.list(model$scripts)
  }
  
  # Convert settings if they exist
  if (!is.null(model$settings) && is.data.frame(model$settings)) {
    model$settings <- convert_settings_from_df(model$settings)
  } else {
    stop('Settings must be a data frame')
  }

  if (class(model$trees) == 'list') {
    model$trees <- NULL
  }
  
  # Convert data frames that might have been simplified too much
  model <- convert_json_dataframes(model)
  
  # Define as heRomodel object
  define_object_(model, 'heRomodel')
}

#' Convert JSON data frames
#' 
#' Ensures that data frames in the model are properly formatted
#' 
#' @param model The model list parsed from JSON
#' @return The model with properly formatted data frames
convert_json_dataframes <- function(model) {
  # List of expected data frames in the model
  expected_dfs <- c("strategies", "groups", "states", "transitions", "values")
  
  for (df_name in expected_dfs) {
    if (!is.null(model[[df_name]])) {
      # Ensure it's a data frame
      if (!is.data.frame(model[[df_name]])) {
        if (is.list(model[[df_name]])) {
          # Convert list to data frame
          model[[df_name]] <- as.data.frame(model[[df_name]], stringsAsFactors = FALSE)
        } else {
          # Create empty data frame if it's not a valid structure
          model[[df_name]] <- data.frame()
        }
      }
      
      # Convert to tibble for consistency with the rest of the code
      model[[df_name]] <- as_tibble(model[[df_name]])
      
      # Convert empty strings to NA
      model[[df_name]] <- model[[df_name]] %>%
        mutate(across(everything(), ~ifelse(. == "", NA, .)))
      
      # Convert column names in transitions from "from_state" and "to_state" to "from" and "to"
      if (df_name == "transitions") {
        if ("from_state" %in% colnames(model[[df_name]])) {
          model[[df_name]] <- rename(model[[df_name]], from = from_state)
        }
        if ("to_state" %in% colnames(model[[df_name]])) {
          model[[df_name]] <- rename(model[[df_name]], to = to_state)
        }
      }
    } else {
      # Create empty tibble if missing
      model[[df_name]] <- tibble::tibble()
    }
  }
  
  return(model)
}