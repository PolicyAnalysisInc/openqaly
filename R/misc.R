#' @export
read_model <- function(path) {

  model <- read_workbook(file.path(path, 'model.xlsx'))

  # Load the values spec
  values_spec <- system.file('model_input_specs', 'values.csv', package = 'heRomod2') %>%
    readr::read_csv(col_types = 'clccc', progress = FALSE)

  # Ensure model$values has correct structure AND types
  if (is.null(model$values) || nrow(model$values) == 0) {
    model$values <- create_empty_values_stubs()
  } else {
    # Convert to tibble and apply spec to enforce types
    model$values <- tibble::as_tibble(model$values)
    model$values <- check_tbl(model$values, values_spec, 'Values')
  }

  # Ensure model$summaries has a defined structure
  if (is.null(model$summaries)) {
    model$summaries <- create_empty_summaries_stubs()
  } else {
    model$summaries <- tibble::as_tibble(model$summaries)
    model$summaries <- ensure_tibble_columns(model$summaries, create_empty_summaries_stubs())
  }

  model$tables <- list.files(file.path(path, 'data')) %>%
    purrr::set_names(., gsub('.csv$', '', .)) %>%
    purrr::map(~read.csv(file.path(path, 'data', .), stringsAsFactor = F, check.names = F))

  model$scripts <- list.files(file.path(path, 'scripts')) %>%
    purrr::set_names(., gsub('.R$', '', ., fixed = T)) %>%
    purrr::map(~readr::read_file(file.path(path,'scripts', .)))

  model$settings <- convert_settings_from_df(model$settings)
  
  # Handle PSM-specific transitions structure
  if (!is.null(model$settings$model_type) && tolower(model$settings$model_type) == "psm") {
    if (!is.null(model$transitions)) {
      # Ensure PSM transitions have required columns
      psm_required <- c("endpoint", "time_unit", "formula")
      for (col in psm_required) {
        if (!(col %in% colnames(model$transitions))) {
          if (nrow(model$transitions) > 0) {
            model$transitions[[col]] <- NA_character_
          } else {
            model$transitions <- create_empty_psm_transitions_stubs()
          }
        }
      }
    } else {
      model$transitions <- create_empty_psm_transitions_stubs()
    }
  }
  
  define_object_(model, 'heRomodel')
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
            warning(paste("Unexpected string value for reduce_state_cycle:", val, "- defaulting to FALSE"))
            settings[["reduce_state_cycle"]] <- FALSE 
        }
    } else if (is.numeric(val)) {
        settings[["reduce_state_cycle"]] <- as.logical(val)
    } else if (!is.logical(val)) {
        # Handle other non-logical types if necessary, default to FALSE
         warning(paste("Unexpected type for reduce_state_cycle:", class(val), "- defaulting to FALSE"))
        settings[["reduce_state_cycle"]] <- FALSE
    }
    # If it's already logical, it remains unchanged.
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
    }
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

  # Load the values spec
  values_spec <- system.file('model_input_specs', 'values.csv', package = 'heRomod2') %>%
    readr::read_csv(col_types = 'clccc', progress = FALSE)

  # Process tables - convert to named list of data frames
  if (is.null(model$tables) ||
      (is.data.frame(model$tables) && nrow(model$tables) == 0) ||
      (is.list(model$tables) && length(model$tables) == 0)) {
    model$tables <- list()
  } else if (!is.data.frame(model$tables) || !all(c("name", "data") %in% colnames(model$tables))) {
    stop("Tables must be provided as an array of objects with 'name' and 'data' fields")
  } else {
    # Convert array of objects to named list of tables
    table_list <- list()
    for (i in 1:nrow(model$tables)) {
      table_name <- model$tables$name[i]
      table_data <- model$tables$data$rows[[i]]

      # Convert to tibble for consistency
      table_list[[table_name]] <- as_tibble(table_data)
    }
    model$tables <- table_list
  }
  
  # Process scripts - convert from array of objects to named list
  if (is.null(model$scripts) || 
      (is.data.frame(model$scripts) && nrow(model$scripts) == 0) ||
      (is.list(model$scripts) && length(model$scripts) == 0)) {
    model$scripts <- list()
  } else if (!is.data.frame(model$scripts) || !all(c("name", "code") %in% colnames(model$scripts))) {
    stop("Scripts must be provided as an array of objects with 'name' and 'code' fields")
  } else {
    # Convert array of objects to named list of scripts
    script_list <- list()
    for (i in 1:nrow(model$scripts)) {
      script_name <- model$scripts$name[i]
      script_code <- model$scripts$code[i]
      script_list[[script_name]] <- script_code
    }
    model$scripts <- script_list
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
  model <- convert_json_dataframes(model, values_spec)

  # Define as heRomodel object
  define_object_(model, 'heRomodel')
}

#' Convert JSON data frames
#' 
#' Ensures that data frames in the model are properly formatted
#' 
#' @param model The model list parsed from JSON
#' @return The model with properly formatted data frames
convert_json_dataframes <- function(model, values_spec = NULL) {
  expected_dfs <- c("strategies", "groups", "states", "transitions", "values", "summaries")

  # Determine if this is a PSM model
  is_psm <- !is.null(model$settings) && !is.null(model$settings$model_type) &&
            tolower(model$settings$model_type) == "psm"

  df_stubs <- list(
    values = create_empty_values_stubs(),
    summaries = create_empty_summaries_stubs(),
    transitions = if (is_psm) create_empty_psm_transitions_stubs() else NULL
  )

  for (df_name in expected_dfs) {
    stub_to_use <- df_stubs[[df_name]]

    if (is.null(model[[df_name]])) {
      model[[df_name]] <- if (!is.null(stub_to_use)) stub_to_use else tibble::tibble()
    } else {
      current_input <- model[[df_name]]
      if (!is.data.frame(current_input)) {
        if (is.list(current_input) && length(current_input) > 0) {
          attempt_df <- tryCatch(as.data.frame(current_input, stringsAsFactors = FALSE), error = function(e) NULL)
          if (!is.null(attempt_df) && is.data.frame(attempt_df)) {
            current_input <- attempt_df
          } else {
            current_input <- if (!is.null(stub_to_use)) stub_to_use else tibble::tibble()
          }
        } else {
          current_input <- if (!is.null(stub_to_use)) stub_to_use else tibble::tibble()
        }
      }
      model[[df_name]] <- tibble::as_tibble(current_input)

      # Use spec-based validation for values, otherwise use ensure_tibble_columns
      if (df_name == "values" && !is.null(values_spec)) {
        # Always apply spec, even for empty dataframes (to get correct types)
        model[[df_name]] <- check_tbl(model[[df_name]], values_spec, 'Values')
        # Don't apply the empty string to NA conversion for values - spec handles it
      } else {
        if (!is.null(stub_to_use)) {
          model[[df_name]] <- ensure_tibble_columns(model[[df_name]], stub_to_use)
        }

        # Apply empty string to NA conversion for non-values dataframes
        if (nrow(model[[df_name]]) > 0) {
          model[[df_name]] <- model[[df_name]] %>%
            dplyr::mutate(dplyr::across(everything(), ~ifelse(. == "", NA, .)))
        }
      }

      if (df_name == "transitions") {
        # Handle transitions differently for PSM vs Markov models
        if (is_psm) {
          # PSM transitions have different structure
          # Ensure required columns exist
          psm_required <- c("endpoint", "time_unit", "formula")
          for (col in psm_required) {
            if (!(col %in% colnames(model[[df_name]]))) {
              model[[df_name]][[col]] <- NA_character_
            }
          }
        } else {
          # Markov transitions - handle column renaming
          if ("from_state" %in% colnames(model[[df_name]])) {
            model[[df_name]] <- dplyr::rename(model[[df_name]], from = from_state)
          }
          if ("to_state" %in% colnames(model[[df_name]])) {
            model[[df_name]] <- dplyr::rename(model[[df_name]], to = to_state)
          }
        }
      }
    }
  }
  
  return(model)
}

create_empty_values_stubs <- function() {
  tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    state = character(0),
    destination = character(0),
    formula = character(0)
  )
}

create_empty_summaries_stubs <- function() {
  tibble::tibble(
    name = character(0),
    display_name = character(0),
    description = character(0),
    values = character(0)
  )
}

create_empty_psm_transitions_stubs <- function() {
  tibble::tibble(
    endpoint = character(0),
    time_unit = character(0),
    formula = character(0)
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
    tbl <- tibble::as_tibble(tbl)
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

  # Create a copy to avoid modifying the original
  json_model <- list()

  # Convert settings from list to dataframe format expected by JSON
  if (!is.null(model$settings) && is.list(model$settings)) {
    settings_df <- tibble::tibble(
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
        warning(paste0("Skipping ", df_name, " - not a data frame"))
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
  json_string <- jsonlite::toJSON(
    json_model,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    null = "null",
    digits = 17  # Preserve full double precision
  )

  as.character(json_string)
}