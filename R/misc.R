#' @export
read_model <- function(path) {

  # Read raw Excel data
  model <- read_workbook(file.path(path, 'model.xlsx'))

  # Read tables from CSV files
  data_path <- file.path(path, 'data')
  if (dir.exists(data_path)) {
    model$tables <- list.files(data_path) %>%
      set_names(., gsub('.csv$', '', .)) %>%
      map(~read.csv(file.path(data_path, .), stringsAsFactor = FALSE, check.names = FALSE))
  } else {
    model$tables <- list()
  }

  # Read scripts from R files
  scripts_path <- file.path(path, 'scripts')
  if (dir.exists(scripts_path)) {
    model$scripts <- list.files(scripts_path) %>%
      set_names(., gsub('.R$', '', ., fixed = TRUE)) %>%
      map(~read_file(file.path(scripts_path, .)))
  } else {
    model$scripts <- list()
  }

  # UNIFIED VALIDATION - applies type-specific specs
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  return(model)
}

convert_settings_from_df <- function(settings_df) {
  settings <- map(setNames(settings_df$value, settings_df$setting), function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (!is.na(num)) return(num)
    tolower(x)
  })
  # Explicitly convert reduce_state_cycle to logical
  if ("reduce_state_cycle" %in% names(settings)) {
    val <- settings[["reduce_state_cycle"]]
    # Handle potential TRUE/FALSE, T/F, 1/0 string representations
    if (is.character(val)) {
        val_lower <- tolower(val)
        if (val_lower %in% c("true", "t", "1")) {
            settings[["reduce_state_cycle"]] <- TRUE
        } else if (val_lower %in% c("false", "f", "0")) {
            settings[["reduce_state_cycle"]] <- FALSE
        } else {
            # Handle unexpected string values, maybe default to FALSE or raise warning/error
            warning(glue("Unexpected string value for reduce_state_cycle: {val} - defaulting to FALSE"))
            settings[["reduce_state_cycle"]] <- FALSE 
        }
    } else if (is.numeric(val)) {
        settings[["reduce_state_cycle"]] <- as.logical(val)
    } else if (!is.logical(val)) {
        # Handle other non-logical types if necessary, default to FALSE
         warning(glue("Unexpected type for reduce_state_cycle: {class(val)} - defaulting to FALSE"))
        settings[["reduce_state_cycle"]] <- FALSE
    }
    # If it's already logical, it remains unchanged.
  }

  # Handle half_cycle_method setting
  if ("half_cycle_method" %in% names(settings)) {
    val <- settings[["half_cycle_method"]]
    if (is.character(val)) {
      val_lower <- tolower(val)
      if (val_lower %in% c("start", "end", "life-table")) {
        settings[["half_cycle_method"]] <- val_lower
      } else {
        warning(glue("Invalid half_cycle_method: {val} - must be 'start', 'end', or 'life-table'. Defaulting to 'start'"))
        settings[["half_cycle_method"]] <- "start"
      }
    } else {
      warning(glue("half_cycle_method must be a string, got: {class(val)} - defaulting to 'start'"))
      settings[["half_cycle_method"]] <- "start"
    }
  } else {
    # Default to "start" if not specified (maintains backward compatibility)
    settings[["half_cycle_method"]] <- "start"
  }

  # Set default discount rates if not provided
  if (!("discount_cost" %in% names(settings))) {
    settings[["discount_cost"]] <- 0.035  # Default 3.5% for costs
  }
  if (!("discount_outcomes" %in% names(settings))) {
    settings[["discount_outcomes"]] <- 0.035  # Default 3.5% for outcomes
  }

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
  if ((!is.null(trees)) && is.data.frame(trees) && nrow(trees) > 0) {
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
    model$groups <- tibble(
      name = 'all',
      display_name = 'All Patients',
      description = 'All Patients',
      weight = 1,
      enabled = 1
    )
  }

  # Filter to only enabled strategies and groups
  # Treat missing enabled column as enabled=1 (for backward compatibility)
  enabled_strategies <- model$strategies
  if ("enabled" %in% colnames(model$strategies)) {
    enabled_strategies <- enabled_strategies %>%
      filter(is.na(enabled) | enabled == 1 | enabled == TRUE | enabled != 0)
  }

  enabled_groups <- model$groups
  if ("enabled" %in% colnames(model$groups)) {
    enabled_groups <- enabled_groups %>%
      filter(is.na(enabled) | enabled == 1 | enabled == TRUE | enabled != 0)
  }

  # Create segments only from enabled strategy × group combinations
  expand.grid(
    group = enabled_groups$name,
    strategy = enabled_strategies$name,
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
  
  target_length <- length(x)
  
  first_val <- Filter(Negate(is.null), args)
  na_val <- if (length(first_val) > 0) { 
      as(NA, class(first_val[[1]]))
  } else { 
      NA
  } 
  res <- rep(na_val, target_length)
  
  for (i in seq_along(args)) {
    opt_name <- opts[i]
    opt_value <- args[[i]]
    
    match_indices <- which(x == opt_name & !is.na(x))
    
    if (length(match_indices) > 0) {
      if (length(opt_value) == 1) {
        res[match_indices] <- opt_value
      } else {
        res[match_indices] <- opt_value[match_indices]
      }
    }
  }
  
  res
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
  # Base case: empty or NULL
  if (is.null(expr) || length(expr) == 0) {
    return(list())
  }
  
  # When expression is a call
  if (is.call(expr)) {
    # Get function name safely
    func_name <- NULL
    if (is.name(expr[[1]]) || is.character(expr[[1]])) {
      func_name <- as.character(expr[[1]])
    }
    
    # Check if this is a target function call
    if (length(func_name) == 1 && func_name %in% funcs) {
      # Found a matching function call
      ret <- list(extract_call_vars(expr))
    } else {
      # Not a target function, but check all parts of the call
      ret <- list()
      # Process all elements of the call
      for (i in seq_along(expr)) {
        nested_calls <- extract_func_calls(expr[[i]], funcs)
        if (length(nested_calls) > 0) {
          ret <- c(ret, nested_calls)
        }
      }
    }
  } else if (is.pairlist(expr)) {
    # Handle function argument pairlists
    ret <- list()
    for (i in seq_along(expr)) {
      nested_calls <- extract_func_calls(expr[[i]], funcs)
      if (length(nested_calls) > 0) {
        ret <- c(ret, nested_calls)
      }
    }
  } else if (is.expression(expr) || is.list(expr)) {
    # Handle expression or list objects
    ret <- list()
    for (i in seq_along(expr)) {
      nested_calls <- extract_func_calls(expr[[i]], funcs)
      if (length(nested_calls) > 0) {
        ret <- c(ret, nested_calls)
      }
    }
  } else {
    # Atomic values, names, etc. - no function calls here
    ret <- list()
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
  st_df <- select(transitions, from_state, formula) %>%
    rename(state = from_state) %>%
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
      uses_st = ifelse((max_state_time > 1 | max_state_time == 0) && uses_st, TRUE, FALSE)
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
  
  # First pass: ensure all columns exist and have correct types
  for (i in seq_len(n_col)) {
    col_name <- spec$name[i]
    type <- spec$type[i]
    values <- df[[col_name]]

    # Handle case where the entire column is missing or has wrong type
    if (is.null(values)) {
      # Column is completely missing - create with NAs of correct type
      na_val <- switch(type,
        "character" = NA_character_,
        "numeric" = NA_real_,
        "integer" = NA_integer_,
        "logical" = NA,
        NA_character_
      )
      df[[col_name]] <- rep(na_val, n_row)
    } else {
      # Column exists - convert to correct type
      df[[col_name]] <- convert_to_type(values, type)
    }
  }

  # Second pass: handle defaults and fallbacks
  for (i in seq_len(n_col)) {
    col_name <- spec$name[i]
    required <- spec$required[i]
    default <- spec$default[i]
    fallback <- spec$fallback[i]
    type <- spec$type[i]
    values <- df[[col_name]]

    # Check for missing data
    miss_data <- is_faslsy_chr(values)

    # Impute missing values
    if (any(miss_data)) {
      if (required && all(miss_data)) {
        # Throw error only if a required column has ALL missing values
        err_msg <- glue('{context} has missing values in required column: {col_name}.')
        stop(err_msg, call. = F)
      } else if (!is.na(fallback) && fallback %in% colnames(df)) {
        # If a fallback column is specified and exists, use its values
        df[[col_name]][miss_data] <- df[[fallback]][miss_data]
      } else if (!is.na(default)) {
        # If a default is specified, use it
        df[[col_name]][miss_data] <- default
      }

      # Re-apply type conversion after defaults to ensure correct type
      df[[col_name]] <- convert_to_type(df[[col_name]], type)
    }
  }
  
  # Use only the columns defined in the spec
  select(df, {{spec_cn}})
}

convert_to_type <- function(x, type) {
  func <- eval(parse(text = glue('as.{type}')))
  func(x)
}

#' Read Model from JSON
#' Normalize NULLs and empty strings in model data
#'
#' Converts NULL values and empty strings to NA for consistent handling
#' throughout the model processing pipeline.
#'
#' @param model The model object from JSON parsing
#' @return The model with normalized values
normalize_model_nulls <- function(model) {
  # Helper function to normalize a single value
  normalize_value <- function(x) {
    if (is.null(x)) return(NA)
    if (is.character(x) && length(x) == 1 && !is.na(x) && x == "") return(NA_character_)
    return(x)
  }

  # Helper function to normalize a dataframe
  normalize_dataframe <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(df)

    # Apply normalization to each column
    for (col in names(df)) {
      if (is.list(df[[col]])) {
        # For list columns, apply element-wise
        df[[col]] <- lapply(df[[col]], normalize_value)
      } else {
        # For regular columns, vectorize the normalization
        df[[col]] <- sapply(df[[col]], normalize_value, USE.NAMES = FALSE)
      }
    }
    return(df)
  }

  # Normalize each dataframe in the model
  if (!is.null(model$states)) {
    model$states <- normalize_dataframe(model$states)
  }

  if (!is.null(model$transitions)) {
    model$transitions <- normalize_dataframe(model$transitions)
  }

  if (!is.null(model$values)) {
    model$values <- normalize_dataframe(model$values)
  }

  if (!is.null(model$variables)) {
    model$variables <- normalize_dataframe(model$variables)
  }

  if (!is.null(model$summaries)) {
    model$summaries <- normalize_dataframe(model$summaries)
  }

  if (!is.null(model$strategies)) {
    model$strategies <- normalize_dataframe(model$strategies)
  }

  if (!is.null(model$groups)) {
    model$groups <- normalize_dataframe(model$groups)
  }

  return(model)
}

#' Validate model data structure and types
#'
#' Ensures that model data has correct structure and types after normalization
#'
#' @param model The normalized model object
#' @return The validated model object
validate_model_data <- function(model) {
  # Validate states
  if (!is.null(model$states) && is.data.frame(model$states)) {
    if (nrow(model$states) == 0) {
      stop("Model must have at least one state defined")
    }

    # Ensure critical fields exist
    if (!"name" %in% names(model$states)) {
      stop("States must have 'name' field")
    }

    # Ensure initial_probability exists and is valid
    if (!"initial_probability" %in% names(model$states)) {
      stop("States must have 'initial_probability' field")
    }

    # Validate state names are unique and non-empty
    state_names <- model$states$name
    if (any(is.na(state_names)) || any(state_names == "")) {
      stop("All states must have non-empty names")
    }
    if (length(unique(state_names)) != length(state_names)) {
      stop("State names must be unique")
    }
  } else {
    stop("Model must have a 'states' dataframe")
  }

  # Validate transitions - just check structure, not state references (those are expanded later)
  if (!is.null(model$transitions) && is.data.frame(model$transitions)) {
    if (nrow(model$transitions) > 0) {
      # PSM models have different transition structure
      # Settings might be a list or a dataframe (setting/value columns)
      is_psm <- FALSE
      if (!is.null(model$settings)) {
        if (is.list(model$settings) && !is.data.frame(model$settings)) {
          # Settings is a list
          is_psm <- !is.null(model$settings$model_type) && tolower(model$settings$model_type) == "psm"
        } else if (is.data.frame(model$settings)) {
          # Settings is a dataframe (from JSON)
          model_type_row <- model$settings[model$settings$setting == "model_type", ]
          if (nrow(model_type_row) > 0) {
            is_psm <- tolower(model_type_row$value[1]) == "psm"
          }
        }
      }

      if (is_psm) {
        # PSM transitions must have endpoint, time_unit, formula
        required_cols <- c("endpoint", "time_unit", "formula")
        if (!all(required_cols %in% names(model$transitions))) {
          missing <- setdiff(required_cols, names(model$transitions))
          stop(paste("PSM transitions missing required fields:", paste(missing, collapse = ", ")))
        }
      } else {
        # Markov transitions must have from_state and to_state fields
        if (!all(c("from_state", "to_state") %in% names(model$transitions))) {
          stop("Transitions must have 'from_state' and 'to_state' fields")
        }
      }
    }
  }

  # Validate values if present
  if (!is.null(model$values) && is.data.frame(model$values)) {
    if (nrow(model$values) > 0) {
      # Ensure name field exists
      if (!"name" %in% names(model$values)) {
        stop("Values must have 'name' field")
      }

      # Values state validation removed - states are expanded later
    }
  }

  return(model)
}

#' Normalize and Validate Model Structure
#'
#' Applies type-specific CSV specs to enforce correct model structure.
#' Used by all three input paths (Excel, JSON, R Builder) to ensure consistency.
#'
#' @param model Raw model list
#' @param preserve_builder If TRUE, keeps heRomodel_builder class; if FALSE, returns heRomodel
#'
#' @return Validated model with correct structure
#' @export
normalize_and_validate_model <- function(model, preserve_builder = FALSE) {

  # Extract model_type (handle both dataframe and list formats)
  if (is.list(model$settings) && !is.data.frame(model$settings)) {
    model_type <- model$settings$model_type
  } else if (is.data.frame(model$settings)) {
    type_row <- model$settings[model$settings$setting == "model_type", ]
    model_type <- if (nrow(type_row) > 0) type_row$value[1] else NULL
  } else {
    model_type <- NULL
  }

  # Normalize model_type to standard strings
  model_type <- model_type %||% "markov"

  # Handle case-insensitive matching and normalize to canonical form (lowercase)
  model_type_lower <- tolower(model_type)
  model_type <- if (model_type_lower == "markov") {
    "markov"
  } else if (model_type_lower == "psm") {
    "psm"
  } else if (model_type_lower %in% c("custom psm", "custompsm", "psm_custom")) {
    "custom_psm"
  } else {
    # Try to match partial strings
    if (model_type_lower %in% c("markov")) {
      "markov"
    } else if (model_type_lower %in% c("psm")) {
      "psm"
    } else if (model_type_lower %in% c("custom psm", "custompsm", "psm_custom")) {
      "custom_psm"
    } else {
      warning("Invalid model_type '", model_type, "'. Defaulting to 'markov'.")
      "markov"
    }
  }

  # Update model_type in settings to canonical form
  if (is.list(model$settings) && !is.data.frame(model$settings)) {
    model$settings$model_type <- model_type
  }

  # Load type-specific specs
  spec_path <- system.file('model_input_specs', package = 'heRomod2')

  states_spec_file <- if (model_type %in% c("psm", "custom_psm")) {
    "psm_states.csv"
  } else {
    "states.csv"
  }

  trans_spec_file <- if (model_type == "psm") {
    "psm_transitions.csv"
  } else if (model_type == "custom_psm") {
    "psm_custom_transitions.csv"
  } else {
    "transitions.csv"
  }

  specs <- list(
    states = read_csv(file.path(spec_path, states_spec_file),
                            col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                         'default' = 'c', 'fallback' = 'c'),
                            progress = FALSE, show_col_types = FALSE),
    transitions = read_csv(file.path(spec_path, trans_spec_file),
                                  col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                               'default' = 'c', 'fallback' = 'c'),
                                  progress = FALSE, show_col_types = FALSE),
    values = read_csv(file.path(spec_path, "values.csv"),
                            col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                         'default' = 'c', 'fallback' = 'c'),
                            progress = FALSE, show_col_types = FALSE),
    strategies = read_csv(file.path(spec_path, "strategies.csv"),
                                 col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                              'default' = 'c', 'fallback' = 'c'),
                                 progress = FALSE, show_col_types = FALSE),
    groups = read_csv(file.path(spec_path, "groups.csv"),
                            col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                         'default' = 'c', 'fallback' = 'c'),
                            progress = FALSE, show_col_types = FALSE),
    variables = read_csv(file.path(spec_path, "variables.csv"),
                                col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                             'default' = 'c', 'fallback' = 'c'),
                                progress = FALSE, show_col_types = FALSE),
    summaries = read_csv(file.path(spec_path, "summaries.csv"),
                                col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c',
                                             'default' = 'c', 'fallback' = 'c'),
                                progress = FALSE, show_col_types = FALSE)
  )

  # Convert settings to list if needed
  if (is.data.frame(model$settings)) {
    model$settings <- convert_settings_from_df(model$settings)
  }

  # Apply specs to each component
  if (!is.null(model$states) && is.data.frame(model$states) && nrow(model$states) > 0) {
    model$states <- check_tbl(model$states, specs$states, "States")
  }

  if (!is.null(model$transitions) && is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
    model$transitions <- check_tbl(model$transitions, specs$transitions, "Transitions")
  }

  if (!is.null(model$values) && is.data.frame(model$values) && nrow(model$values) > 0) {
    model$values <- check_tbl(model$values, specs$values, "Values")
  }

  if (!is.null(model$strategies) && is.data.frame(model$strategies) && nrow(model$strategies) > 0) {
    model$strategies <- check_tbl(model$strategies, specs$strategies, "Strategies")
  }

  if (!is.null(model$groups) && is.data.frame(model$groups) && nrow(model$groups) > 0) {
    model$groups <- check_tbl(model$groups, specs$groups, "Groups")
  }

  if (!is.null(model$variables) && is.data.frame(model$variables) && nrow(model$variables) > 0) {
    model$variables <- check_tbl(model$variables, specs$variables, "Variables")
  }

  if (!is.null(model$summaries) && is.data.frame(model$summaries) && nrow(model$summaries) > 0) {
    model$summaries <- check_tbl(model$summaries, specs$summaries, "Summaries")
  }

  # Ensure empty components have correct structure
  if (is.null(model$values) || !is.data.frame(model$values) || nrow(model$values) == 0) {
    model$values <- create_empty_values_stubs()
  }
  if (is.null(model$summaries) || !is.data.frame(model$summaries) || nrow(model$summaries) == 0) {
    model$summaries <- create_empty_summaries_stubs()
  }
  if (is.null(model$variables) || !is.data.frame(model$variables) || nrow(model$variables) == 0) {
    model$variables <- create_empty_variables_stubs()
  }
  if (is.null(model$transitions) || !is.data.frame(model$transitions) || nrow(model$transitions) == 0) {
    model$transitions <- if (model_type == "psm") {
      create_empty_psm_transitions_stubs()
    } else if (model_type == "custom_psm") {
      tibble(state = character(0), formula = character(0))
    } else {
      tibble(from_state = character(0), to_state = character(0), formula = character(0))
    }
  }
  if (is.null(model$strategies) || !is.data.frame(model$strategies)) {
    model$strategies <- tibble()
  }
  if (is.null(model$groups) || !is.data.frame(model$groups)) {
    model$groups <- tibble()
  }
  if (is.null(model$states) || !is.data.frame(model$states)) {
    model$states <- tibble()
  }

  # Ensure tables and scripts are named lists
  if (is.null(model$tables)) model$tables <- list()
  if (is.null(model$scripts)) model$scripts <- list()

  # Set class
  if (preserve_builder && "heRomodel_builder" %in% class(model)) {
    class(model) <- c("heRomodel_builder", "heRomodel")
  } else {
    class(model) <- "heRomodel"
  }

  return(model)
}

#'
#' Takes a JSON string and parses it into a heRomodel object.
#'
#' @param json_string A string containing the model in JSON format.
#'
#' @return A heRomodel object
#'
#' @export
read_model_json <- function(json_string) {
  # Parse JSON to list
  model <- fromJSON(json_string, simplifyVector = TRUE)

  # Normalize NULLs
  model <- normalize_model_nulls(model)

  # Convert tables array-of-objects to named list
  if (!is.null(model$tables) && is.data.frame(model$tables) &&
      all(c("name", "data") %in% colnames(model$tables))) {
    table_list <- list()
    for (i in 1:nrow(model$tables)) {
      table_name <- model$tables$name[i]
      table_data <- model$tables$data$rows[[i]]
      table_list[[table_name]] <- as_tibble(table_data)
    }
    model$tables <- table_list
  } else {
    model$tables <- list()
  }

  # Convert scripts array-of-objects to named list
  if (!is.null(model$scripts) && is.data.frame(model$scripts) &&
      all(c("name", "code") %in% colnames(model$scripts))) {
    script_list <- list()
    for (i in 1:nrow(model$scripts)) {
      script_name <- model$scripts$name[i]
      script_code <- model$scripts$code[i]
      script_list[[script_name]] <- script_code
    }
    model$scripts <- script_list
  } else {
    model$scripts <- list()
  }

  # UNIFIED VALIDATION - applies type-specific specs
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

  return(model)
}

#' Convert JSON data frames
#' 
#' Ensures that data frames in the model are properly formatted
#' 
#' @param model The model list parsed from JSON
#' @return The model with properly formatted data frames
convert_json_dataframes <- function(model, values_spec = NULL) {
  # Load all specs if not provided
  if (is.null(values_spec)) {
    model_input_specs <- system.file('model_input_specs', package = 'heRomod2') %>%
      list.files() %>%
      set_names(str_split_fixed(., '\\.', Inf)[,1]) %>%
      map(function(x) {
        suppressWarnings(read_csv(
          system.file('model_input_specs', x, package = 'heRomod2'),
          col_types = c('name' = 'c', 'required' = 'l', 'type' = 'c', 'default' = 'c', 'fallback' = 'c'),
          progress = FALSE
        ))
      })
  }

  expected_dfs <- c("strategies", "groups", "states", "transitions",
                    "values", "summaries", "variables")

  # Determine model type
  model_type_lower <- if (!is.null(model$settings) && !is.null(model$settings$model_type)) {
    tolower(model$settings$model_type)
  } else {
    "markov"
  }

  is_psm <- model_type_lower == "psm"
  is_custom_psm <- model_type_lower %in% c("custom psm", "custompsm", "psm_custom")

  df_stubs <- list(
    values = create_empty_values_stubs(),
    summaries = create_empty_summaries_stubs(),
    transitions = if (is_psm) {
      create_empty_psm_transitions_stubs()
    } else if (is_custom_psm) {
      tibble(state = character(0), formula = character(0))
    } else {
      NULL
    },
    variables = create_empty_variables_stubs()
  )

  for (df_name in expected_dfs) {
    stub_to_use <- df_stubs[[df_name]]

    if (is.null(model[[df_name]])) {
      model[[df_name]] <- if (!is.null(stub_to_use)) stub_to_use else tibble()
    } else {
      current_input <- model[[df_name]]
      if (!is.data.frame(current_input)) {
        if (is.list(current_input) && length(current_input) > 0) {
          attempt_df <- tryCatch(as.data.frame(current_input, stringsAsFactors = FALSE), error = function(e) NULL)
          if (!is.null(attempt_df) && is.data.frame(attempt_df)) {
            current_input <- attempt_df
          } else {
            current_input <- if (!is.null(stub_to_use)) stub_to_use else tibble()
          }
        } else {
          current_input <- if (!is.null(stub_to_use)) stub_to_use else tibble()
        }
      }
      model[[df_name]] <- as_tibble(current_input)

      # Use spec-based validation if spec exists for this component
      # Skip spec validation for PSM transitions (they have different structure)
      should_validate <- TRUE
      if (df_name == "transitions" && (is_psm || is_custom_psm)) {
        should_validate <- FALSE
      }

      spec_name <- df_name

      if (should_validate && !is.null(model_input_specs) && spec_name %in% names(model_input_specs)) {
        # Apply spec validation
        spec <- model_input_specs[[spec_name]]
        context_name <- paste0(toupper(substring(df_name, 1, 1)), substring(df_name, 2))
        model[[df_name]] <- check_tbl(model[[df_name]], spec, context_name)

        # Special handling for values columns
        if (df_name == "values" && nrow(model[[df_name]]) > 0) {
          if ("state" %in% names(model[[df_name]])) {
            model[[df_name]]$state[!is.na(model[[df_name]]$state) & model[[df_name]]$state == ""] <- NA_character_
          }
          if ("destination" %in% names(model[[df_name]])) {
            model[[df_name]]$destination[!is.na(model[[df_name]]$destination) & model[[df_name]]$destination == ""] <- NA_character_
          }
        }
      } else {
        # No spec available, use stub-based approach
        if (!is.null(stub_to_use)) {
          model[[df_name]] <- ensure_tibble_columns(model[[df_name]], stub_to_use)
        }

        # Apply empty string to NA conversion for non-spec dataframes
        if (nrow(model[[df_name]]) > 0) {
          model[[df_name]] <- model[[df_name]] %>%
            mutate(across(everything(), ~ifelse(. == "", NA, .)))
        }
      }

      if (df_name == "transitions") {
        # Handle transitions differently for PSM vs Custom PSM vs Markov models
        if (is_psm) {
          # PSM transitions have different structure
          # Ensure required columns exist
          psm_required <- c("endpoint", "time_unit", "formula")
          for (col in psm_required) {
            if (!(col %in% colnames(model[[df_name]]))) {
              model[[df_name]][[col]] <- NA_character_
            }
          }
        } else if (is_custom_psm) {
          # Custom PSM transitions have state + formula structure
          # Ensure required columns exist
          custom_psm_required <- c("state", "formula")
          for (col in custom_psm_required) {
            if (!(col %in% colnames(model[[df_name]]))) {
              model[[df_name]][[col]] <- NA_character_
            }
          }
        }
      }
    }
  }
  
  return(model)
}

create_empty_values_stubs <- function() {
  tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    state = character(0),
    destination = character(0),
    formula = character(0),
    type = character(0)
  )
}

create_empty_summaries_stubs <- function() {
  tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    values = character(0)
  )
}

create_empty_psm_transitions_stubs <- function() {
  tibble(
    endpoint = character(0),
    time_unit = character(0),
    formula = character(0)
  )
}

create_empty_variables_stubs <- function() {
  tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    formula = character(0),
    strategy = character(0),
    group = character(0)
  )
}

# Ensure columns exist in a tibble, adding them as NA of the correct type if missing
ensure_tibble_columns <- function(tbl, required_cols_spec_tibble) {
  if (!inherits(tbl, "tbl_df") && !is.data.frame(tbl)) {
     # If it's not even a data.frame, it's too far off; return the clean stub directly.
     # This might happen if as_tibble(model$values) failed for some reason before this call
     # though read_model attempts as_tibble first.
    return(required_cols_spec_tibble) 
  }
  if (!inherits(tbl, "tbl_df")) {
    tbl <- as_tibble(tbl)
  }

  for (col_name in colnames(required_cols_spec_tibble)) {
    spec_col_vector <- required_cols_spec_tibble[[col_name]]
    spec_target_class <- class(spec_col_vector)[1] # Get target class like "character", "numeric"

    if (!col_name %in% colnames(tbl)) {
      # Column is missing, add it
      # Determine NA type based on the typeof the spec_col_vector
      na_to_use <- switch(typeof(spec_col_vector),
                          "character" = NA_character_,
                          "double" = NA_real_,
                          "integer" = NA_integer_,
                          "logical" = NA, # Default base NA for logical
                          NA_character_) # Fallback, though spec should be well-defined

      if (nrow(tbl) > 0) {
        tbl[[col_name]] <- rep(na_to_use, nrow(tbl))
      } else {
        # If tbl has 0 rows, assign the empty typed vector from spec
        tbl[[col_name]] <- spec_col_vector
      }
    } else {
      # Column exists, ensure it has the correct type
      current_col_class <- class(tbl[[col_name]])[1]
      if (current_col_class != spec_target_class) {
        # Special handling for factors if target is character
        if (spec_target_class == "character" && current_col_class == "factor") {
            tbl[[col_name]] <- as.character(tbl[[col_name]])
        } else {
            # Use the existing convert_to_type function for coercion
            tbl[[col_name]] <- convert_to_type(tbl[[col_name]], spec_target_class)
        }
      }
    }
  }
  return(tbl)
}

#' Write Model to JSON
#'
#' Takes a heRomodel object (typically loaded from Excel) and converts it
#' to a JSON string format.
#'
#' @param model A heRomodel object to be converted to JSON.
#'
#' @return A JSON string representing the model.
#'
#' @export
write_model_json <- function(model) {
  # Ensure model is a heRomodel object
  if (!"heRomodel" %in% class(model)) {
    stop("Input must be a heRomodel object", call. = FALSE)
  }

  # Build JSON structure
  json_model <- list()

  # Convert settings from list to dataframe format expected by JSON
  if (!is.null(model$settings) && is.list(model$settings)) {
    settings_df <- tibble(
      setting = names(model$settings),
      value = as.character(unlist(model$settings))
    )
    json_model$settings <- settings_df
  } else {
    stop("Model settings must be a list", call. = FALSE)
  }

  # Copy standard dataframes
  standard_dfs <- c("strategies", "groups", "states", "transitions",
                    "values", "summaries", "variables", "trees")
  for (df_name in standard_dfs) {
    if (!is.null(model[[df_name]])) {
      # Ensure it's a data frame
      if (is.data.frame(model[[df_name]])) {
        # Convert factors to characters
        df_copy <- model[[df_name]]
        df_copy[] <- lapply(df_copy, function(x) {
          if (is.factor(x)) as.character(x) else x
        })
        json_model[[df_name]] <- df_copy
      } else {
        warning(glue("Skipping {df_name} - not a data frame"))
      }
    }
  }

  # Convert tables to array format expected by JSON
  if (!is.null(model$tables) && is.list(model$tables)) {
    table_names <- names(model$tables)
    if (length(table_names) > 0) {
      tables_array <- list()
      for (i in seq_along(table_names)) {
        table_name <- table_names[i]
        table_data <- model$tables[[table_name]]

        # Ensure table_data is a data frame
        if (is.data.frame(table_data)) {
          # Convert factors to characters
          table_data[] <- lapply(table_data, function(x) {
            if (is.factor(x)) as.character(x) else x
          })

          tables_array[[i]] <- list(
            name = table_name,
            data = list(rows = table_data)
          )
        }
      }
      json_model$tables <- tables_array
    } else {
      json_model$tables <- list()
    }
  } else {
    json_model$tables <- list()
  }

  # Convert scripts to array format expected by JSON
  if (!is.null(model$scripts) && is.list(model$scripts)) {
    script_names <- names(model$scripts)
    if (length(script_names) > 0) {
      scripts_array <- list()
      for (i in seq_along(script_names)) {
        script_name <- script_names[i]
        script_code <- model$scripts[[script_name]]

        scripts_array[[i]] <- list(
          name = script_name,
          code = as.character(script_code)
        )
      }
      json_model$scripts <- scripts_array
    } else {
      json_model$scripts <- list()
    }
  } else {
    json_model$scripts <- list()
  }

  # Convert to JSON using jsonlite
  json_string <- toJSON(
    json_model,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null",
    digits = 17  # Preserve full double precision
  )

  as.character(json_string)
}