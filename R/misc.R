#' Read a Model from Directory
#'
#' Reads a complete openqaly model from a directory structure containing
#' an Excel workbook (`model.xlsx`), optional data CSVs, and optional R scripts.
#'
#' @param path Path to the model directory containing `model.xlsx` and
#'   optional `data/` and `scripts/` subdirectories.
#'
#' @return A normalized and validated `oq_model` object.
#'
#' @export
read_model <- function(path) {

  # Read raw Excel data
  model <- read_workbook(file.path(path, 'model.xlsx'))

  # Convert multivariate sampling tables from Excel format to internal list structure
  if ("multivariate_sampling" %in% names(model) &&
      "multivariate_sampling_variables" %in% names(model)) {

    mv_sampling <- model$multivariate_sampling
    mv_variables <- model$multivariate_sampling_variables

    # Join and convert to list structure
    # Preserve original order from mv_sampling sheet
    sampling_order <- mv_sampling$name

    model$multivariate_sampling <- mv_variables %>%
      left_join(mv_sampling, by = c("sampling_name" = "name")) %>%
      split(.$sampling_name) %>%
      .[sampling_order] %>%  # Reorder to match original order
      lapply(function(vars) {
        list(
          name = vars$sampling_name[1],
          distribution = vars$distribution[1],
          description = if ("description" %in% names(vars) && !is.na(vars$description[1])) {
            vars$description[1]
          } else {
            ""
          },
          variables = vars %>%
            select("variable", "strategy", "group") %>%
            as_tibble()
        )
      }) %>%
      unname()

    # Remove the raw tables
    model$multivariate_sampling_variables <- NULL
  }

  # Read metadata sheet for table/script descriptions
  metadata <- NULL
  if ("_metadata" %in% names(model)) {
    metadata <- model$`_metadata`
    model$`_metadata` <- NULL
  }

  # Read DSA parameters sheet
  if ("dsa_parameters" %in% names(model)) {
    dsa_df <- model$dsa_parameters
    model$dsa_parameters <- lapply(seq_len(nrow(dsa_df)), function(i) {
      row <- dsa_df[i, ]
      param_type <- row$type
      list(
        type = param_type,
        name = row$name,
        low = deserialize_to_formula(row$low, param_type),
        high = deserialize_to_formula(row$high, param_type),
        strategy = if (is.na(row$strategy) || row$strategy == "") "" else row$strategy,
        group = if (is.na(row$group) || row$group == "") "" else row$group,
        display_name = if (is.na(row$display_name) || row$display_name == "") NULL else row$display_name
      )
    })
    class(model$dsa_parameters) <- "dsa_parameters"
  } else {
    model$dsa_parameters <- structure(list(), class = "dsa_parameters")
  }

  # Read scenarios sheets
  if ("scenarios" %in% names(model) && "scenario_overrides" %in% names(model)) {
    scenarios_df <- model$scenarios
    overrides_df <- model$scenario_overrides

    model$scenarios <- lapply(seq_len(nrow(scenarios_df)), function(i) {
      s <- scenarios_df[i, ]
      s_overrides <- overrides_df[overrides_df$scenario_name == s$name, ]

      var_overrides <- list()
      var_indices <- which(s_overrides$override_type == "variable")
      if (length(var_indices) > 0) {
        var_overrides <- lapply(var_indices, function(j) {
          row <- s_overrides[j, ]
          list(
            name = row$name,
            value = deserialize_to_formula(row$value, "variable"),
            strategy = if (is.na(row$strategy) || row$strategy == "") "" else row$strategy,
            group = if (is.na(row$group) || row$group == "") "" else row$group
          )
        })
      }

      setting_overrides <- list()
      setting_indices <- which(s_overrides$override_type == "setting")
      if (length(setting_indices) > 0) {
        setting_overrides <- lapply(setting_indices, function(j) {
          row <- s_overrides[j, ]
          list(name = row$name, value = row$value)
        })
      }

      list(
        name = s$name,
        description = if (is.na(s$description)) "" else s$description,
        variable_overrides = var_overrides,
        setting_overrides = setting_overrides
      )
    })

    # Remove raw tables
    model$scenario_overrides <- NULL
  } else {
    model$scenarios <- list()
  }

  # Read TWSA sheets
  if ("twsa_analyses" %in% names(model) && "twsa_parameters" %in% names(model)) {
    twsa_df <- model$twsa_analyses
    params_df <- model$twsa_parameters

    model$twsa_analyses <- lapply(seq_len(nrow(twsa_df)), function(i) {
      t <- twsa_df[i, ]
      t_params <- params_df[params_df$twsa_name == t$name, ]

      parameters <- list()
      if (nrow(t_params) > 0) {
        parameters <- lapply(seq_len(nrow(t_params)), function(j) {
          p <- t_params[j, ]
          param <- list(
            param_type = p$param_type,
            name = p$name,
            type = p$type,
            strategy = if (is.na(p$strategy) || p$strategy == "") "" else p$strategy,
            group = if (is.na(p$group) || p$group == "") "" else p$group,
            display_name = if (is.na(p$display_name) || p$display_name == "") NULL else p$display_name,
            include_base_case = if (is.na(p$include_base_case)) TRUE else p$include_base_case
          )
          if (!is.na(p$min) && p$min != "") param$min <- deserialize_to_formula(p$min, p$param_type)
          if (!is.na(p$max) && p$max != "") param$max <- deserialize_to_formula(p$max, p$param_type)
          if (!is.na(p$radius) && p$radius != "") param$radius <- deserialize_to_formula(p$radius, p$param_type)
          if (!is.na(p$steps)) param$steps <- p$steps
          if (!is.na(p$values) && p$values != "") {
            param$values <- strsplit(p$values, ",")[[1]]
          }
          param
        })
      }

      list(
        name = t$name,
        description = if (is.na(t$description)) "" else t$description,
        parameters = parameters
      )
    })

    # Remove raw tables
    model$twsa_parameters <- NULL
  } else {
    model$twsa_analyses <- list()
  }

  # Read override categories sheets
  if ("override_categories" %in% names(model)) {
    cats_df <- model$override_categories
    overrides_df <- if ("overrides" %in% names(model)) model$overrides else NULL
    dropdown_df <- if ("override_dropdown_options" %in% names(model)) model$override_dropdown_options else NULL

    model$override_categories <- lapply(seq_len(nrow(cats_df)), function(i) {
      cat_row <- cats_df[i, ]
      cat_name <- cat_row$category_name

      # Get overrides for this category
      overrides_list <- list()
      if (!is.null(overrides_df)) {
        cat_overrides <- overrides_df[overrides_df$category_name == cat_name, ]
        if (nrow(cat_overrides) > 0) {
          overrides_list <- lapply(seq_len(nrow(cat_overrides)), function(j) {
            row <- cat_overrides[j, ]

            # Build input_config based on input_type
            input_config <- list()
            if (row$input_type %in% c("numeric", "slider")) {
              if (!is.na(row$config_min) && row$config_min != "") {
                input_config$min <- as.numeric(row$config_min)
              }
              if (!is.na(row$config_max) && row$config_max != "") {
                input_config$max <- as.numeric(row$config_max)
              }
            }
            if (row$input_type == "slider") {
              if (!is.na(row$config_step_size) && row$config_step_size != "") {
                input_config$step_size <- as.numeric(row$config_step_size)
              }
            }
            if (row$input_type == "dropdown" && !is.null(dropdown_df)) {
              dd_opts <- dropdown_df[dropdown_df$category_name == cat_name &
                                     dropdown_df$override_title == row$title, ]
              if (nrow(dd_opts) > 0) {
                input_config$options <- lapply(seq_len(nrow(dd_opts)), function(k) {
                  list(
                    label = dd_opts$label[k],
                    value = as.character(dd_opts$value[k]),
                    is_base_case = as.logical(dd_opts$is_base_case[k])
                  )
                })
              } else {
                input_config$options <- list()
              }
            }

            list(
              title = row$title,
              description = if (is.na(row$description)) "" else row$description,
              type = row$type,
              name = row$name,
              strategy = if (is.na(row$strategy) || row$strategy == "") "" else row$strategy,
              group = if (is.na(row$group) || row$group == "") "" else row$group,
              general = as.logical(row$general),
              input_type = row$input_type,
              overridden_expression = as.character(row$overridden_expression),
              input_config = input_config
            )
          })
        }
      }

      list(
        name = cat_name,
        general = as.logical(cat_row$general),
        overrides = overrides_list
      )
    })

    # Clean up raw tables
    model$overrides <- NULL
    model$override_dropdown_options <- NULL
  } else {
    model$override_categories <- list()
  }

  # Read tables from CSV files
  data_path <- file.path(path, 'data')
  if (dir.exists(data_path)) {
    table_files <- list.files(data_path)
    table_names <- gsub('.csv$', '', table_files)
    model$tables <- list()
    for (i in seq_along(table_files)) {
      table_data <- read.csv(file.path(data_path, table_files[i]),
                              stringsAsFactors = FALSE, check.names = FALSE)
      # Look up description from metadata
      table_description <- NULL
      if (!is.null(metadata)) {
        tbl_meta <- metadata[metadata$component_type == "table" & metadata$name == table_names[i], ]
        if (nrow(tbl_meta) > 0) {
          table_description <- tbl_meta$description[1]
        }
      }
      model$tables[[table_names[i]]] <- list(
        data = table_data,
        description = table_description
      )
    }
  } else {
    model$tables <- list()
  }

  # Read scripts from R files
  scripts_path <- file.path(path, 'scripts')
  if (dir.exists(scripts_path)) {
    script_files <- list.files(scripts_path)
    script_names <- gsub("\\.R$", "", script_files)
    model$scripts <- list()
    for (i in seq_along(script_files)) {
      script_code <- read_file(file.path(scripts_path, script_files[i]))
      # Look up description from metadata
      script_description <- NULL
      if (!is.null(metadata)) {
        scr_meta <- metadata[metadata$component_type == "script" & metadata$name == script_names[i], ]
        if (nrow(scr_meta) > 0) {
          script_description <- scr_meta$description[1]
        }
      }
      model$scripts[[script_names[i]]] <- list(
        code = script_code,
        description = script_description
      )
    }
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

  # Validate that discount rates are provided (mandatory)
  if (!("discount_cost" %in% names(settings))) {
    stop("discount_cost is required but was not provided in settings")
  }
  if (!("discount_outcomes" %in% names(settings))) {
    stop("discount_outcomes is required but was not provided in settings")
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
  lapply(sheet_names, function(x) {
    suppressWarnings(as_tibble(readWorkbook(path, sheet = x)))
  })
}

define_object_ <- function(obj, class) {
  class(obj) <- class
  obj
}

create_default_group <- function() {
  tibble(
    name = 'all_patients',
    display_name = 'All Patients',
    description = 'Entire model population.',
    weight = 1,
    enabled = 1
  )
}

load_tables <- function(tables, env) {
  for (name in names(tables)) {
    table_entry <- tables[[name]]
    # Handle both old format (direct data frame) and new format (list with data/description)
    if (is.data.frame(table_entry)) {
      # Old format - direct data frame
      assign(name, table_entry, envir = env)
    } else if (is.list(table_entry) && "data" %in% names(table_entry)) {
      # New format - extract data from list
      assign(name, table_entry$data, envir = env)
    } else {
      # Fallback - assign as-is
      assign(name, table_entry, envir = env)
    }
  }
}

load_trees <- function(trees, env) {
  if ((!is.null(trees)) && is.data.frame(trees) && nrow(trees) > 0) {
    env$.trees <- trees
  }
}

run_scripts <- function(scripts, env) {
  for (name in names(scripts)) {
    script_entry <- scripts[[name]]
    # Handle both old format (direct code string) and new format (list with code/description)
    if (is.character(script_entry)) {
      # Old format - direct code string
      eval(parse(text = script_entry), envir = env)
    } else if (is.list(script_entry) && "code" %in% names(script_entry)) {
      # New format - extract code from list
      eval(parse(text = script_entry$code), envir = env)
    }
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
      filter(is.na(.data$enabled) | .data$enabled == 1 | .data$enabled == TRUE | .data$enabled != 0)
  }

  enabled_groups <- model$groups
  if ("enabled" %in% colnames(model$groups)) {
    enabled_groups <- enabled_groups %>%
      filter(is.na(.data$enabled) | .data$enabled == 1 | .data$enabled == TRUE | .data$enabled != 0)
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

`%&%` <- function(a, b) { paste0(a, b) }

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
  st_df <- select(transitions, "from_state", "formula") %>%
    rename(state = "from_state") %>%
    rbind(
      select(values, "state", "formula")
    ) %>%
    filter(!is.na(.data$state)) %>%
    group_by(.data$state) %>%
    do({
      tibble(
        state = .$state[1],
        uses_st = any(map_lgl(.$formula, ~has_st_dependency(., extras = st_vars)))
      )
    }) %>%
    left_join(select(states, "name", "max_state_time"), by = c('state' = 'name')) %>%
    transmute(
      state = .data$state,
      uses_st = ifelse((.data$max_state_time > 1 | .data$max_state_time == 0) && .data$uses_st, TRUE, FALSE)
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
#' Checks character vector to see if elements follow the rules of openqaly variable names,
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

is.empty <- function(x) {
  is.na(x) | x == ''
}


#' Auto-Generate Display Names for Variables
#'
#' Generates display names for variables based on name, strategy, and group.
#' Only fills in missing display_name values, preserving any user-provided names.
#'
#' Format:
#' - No strategy/group: "var_name"
#' - Strategy only: "var_name, strategy_name"
#' - Group only: "var_name, group_name"
#' - Both: "var_name, strategy_name, group_name"
#'
#' This function is called after \code{\link{validate_variable_display_names}} to ensure
#' consistency. Variables with multiple rows (strategy/group specific) must have display
#' names provided for either all rows or no rows.
#'
#' @param df A dataframe with columns: name, display_name, strategy (optional), group (optional)
#'
#' @return The dataframe with auto-generated display names where missing
#' @keywords internal
auto_generate_display_name_variables <- function(df) {
  # Only process rows with missing display_name
  missing_display <- is.na(df$display_name) | df$display_name == ""

  if (!any(missing_display)) {
    return(df)  # Nothing to do
  }

  # Generate display names for missing rows
  for (i in which(missing_display)) {
    display_name <- df$name[i]

    # Add strategy if present and non-empty
    if ("strategy" %in% colnames(df)) {
      strategy_val <- df$strategy[i]
      if (!is.na(strategy_val) && strategy_val != "") {
        display_name <- paste0(display_name, ", ", strategy_val)
      }
    }

    # Add group if present and non-empty
    if ("group" %in% colnames(df)) {
      group_val <- df$group[i]
      if (!is.na(group_val) && group_val != "") {
        display_name <- paste0(display_name, ", ", group_val)
      }
    }

    df$display_name[i] <- display_name
  }

  return(df)
}

#' Validate Display Names for Variables
#'
#' Ensures that for variables with multiple rows (strategy/group specific),
#' if a display_name is provided for at least one row, it must be provided
#' for all rows of that variable.
#'
#' @param df Data frame containing variables with columns: name, display_name, strategy, group
#'
#' @return Empty string if valid, error message if invalid
#' @keywords internal
validate_variable_display_names <- function(df) {
  # Group by variable name
  var_names <- unique(df$name)

  for (var_name in var_names) {
    var_rows <- df[df$name == var_name, ]

    if (nrow(var_rows) == 1) {
      next  # Single row, no consistency check needed
    }

    # Check which rows have user-provided display names
    # User-provided means: not NA/empty
    has_display <- !is.na(var_rows$display_name) & var_rows$display_name != ""

    if (!any(has_display)) {
      next  # All missing, will be auto-generated - valid
    }

    if (all(has_display)) {
      next  # All provided - valid
    }

    # Some but not all have display names - ERROR
    # Build informative error message with tabular format
    missing_indices <- which(!has_display)
    table_string <- format_missing_display_names_table(var_rows, missing_indices)

    error_msg <- sprintf(
      "Variable '%s': display_name must be provided for ALL definitions or NONE.\n\nMissing display_name for the following definitions:\n\n%s",
      var_name,
      table_string
    )

    return(error_msg)
  }

  return("")  # All valid
}

#' Format Data Frame as Markdown Table
#'
#' Generic helper function to format any data frame as a markdown-style table.
#' Handles NA values, calculates column widths dynamically, and creates a properly
#' formatted markdown table suitable for console output or error messages.
#'
#' @param df Data frame to format
#' @param col_names Optional custom column names (defaults to colnames(df))
#' @return Formatted markdown table as a character string
#' @keywords internal
format_dataframe_as_markdown_table <- function(df, col_names = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return("")
  }

  # Use custom column names if provided, otherwise use data frame column names
  if (is.null(col_names)) {
    col_names <- colnames(df)
  }

  if (length(col_names) != ncol(df)) {
    stop("col_names must have same length as number of columns in df")
  }

  # Convert all columns to character, handling NA values
  df_char <- as.data.frame(lapply(df, function(col) {
    ifelse(is.na(col), "NA", as.character(col))
  }), stringsAsFactors = FALSE)

  # Calculate column widths (max of header and data)
  col_widths <- numeric(ncol(df))
  for (i in seq_along(col_names)) {
    col_widths[i] <- max(
      nchar(col_names[i]),
      max(nchar(df_char[[i]]), na.rm = TRUE)
    )
  }

  # Build header
  header_parts <- character(length(col_names))
  for (i in seq_along(col_names)) {
    header_parts[i] <- format(col_names[i], width = col_widths[i], justify = "left")
  }
  header <- paste0("| ", paste(header_parts, collapse = " | "), " |")

  # Build separator
  separator_parts <- character(length(col_names))
  for (i in seq_along(col_names)) {
    separator_parts[i] <- paste(rep("-", col_widths[i]), collapse = "")
  }
  separator <- paste0("|-", paste(separator_parts, collapse = "-|-"), "-|")

  # Build data rows
  rows <- character(nrow(df))
  for (row_idx in seq_len(nrow(df))) {
    row_parts <- character(ncol(df))
    for (col_idx in seq_len(ncol(df))) {
      row_parts[col_idx] <- format(df_char[row_idx, col_idx],
                                   width = col_widths[col_idx],
                                   justify = "left")
    }
    rows[row_idx] <- paste0("| ", paste(row_parts, collapse = " | "), " |")
  }

  # Combine all parts
  paste(c(header, separator, rows), collapse = "\n")
}

#' Format Missing Display Names as Table
#'
#' Helper function to format missing display name information as a markdown-style table.
#' Used in validation error messages.
#'
#' @param vars_df Dataframe of variables
#' @param missing_indices Indices of rows with missing display names
#'
#' @return Formatted table string
#' @keywords internal
format_missing_display_names_table <- function(vars_df, missing_indices) {
  # Build table data
  table_data <- data.frame(
    Strategy = character(length(missing_indices)),
    Group = character(length(missing_indices)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(missing_indices)) {
    row_idx <- missing_indices[i]
    strategy_val <- vars_df$strategy[row_idx]
    group_val <- vars_df$group[row_idx]

    table_data$Strategy[i] <- if (!is.na(strategy_val) && strategy_val != "") strategy_val else "N/A"
    table_data$Group[i] <- if (!is.na(group_val) && group_val != "") group_val else "N/A"
  }

  # Use the generic formatter
  format_dataframe_as_markdown_table(table_data)
}

#' Validate Display Names for Variables (Builder Context)
#'
#' Validates display name consistency for variables being added via the R model builder.
#' This is called after each add_variable() to ensure that if any variable with a given
#' name has a user-provided display_name, all variables with that name must have one.
#'
#' @param vars_df Dataframe of variables with the same name
#' @param var_name The variable name being validated
#'
#' @return Empty string if valid, error message if invalid
#' @keywords internal
validate_variable_display_names_for_builder <- function(vars_df, var_name) {
  if (nrow(vars_df) <= 1) {
    return("")  # Single variable, no consistency check needed
  }

  # Check which rows have user-provided display names
  # User-provided means: not NA/empty
  has_display <- !is.na(vars_df$display_name) & vars_df$display_name != ""

  if (!any(has_display)) {
    return("")  # All missing, will be auto-generated - valid
  }

  if (all(has_display)) {
    return("")  # All provided - valid
  }

  # Some but not all have display names - ERROR
  # Build informative error message with tabular format
  missing_indices <- which(!has_display)
  table_string <- format_missing_display_names_table(vars_df, missing_indices)

  error_msg <- sprintf(
    "Variable '%s': display_name must be provided for ALL definitions or NONE.\n\nMissing display_name for the following definitions:\n\n%s",
    var_name,
    table_string
  )

  return(error_msg)
}

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
      } else {
        # No default specified - convert empty strings to NA for consistency
        df[[col_name]][miss_data] <- NA
      }

      # Re-apply type conversion after defaults to ensure correct type
      df[[col_name]] <- convert_to_type(df[[col_name]], type)
    }
  }

  # Auto-generate display names for variables (which have strategy/group context)
  if ("display_name" %in% spec_cn &&
      all(c("strategy", "group") %in% colnames(df))) {
    # First validate that display names are consistently provided
    error_msg <- validate_variable_display_names(df)
    if (error_msg != "") {
      stop(error_msg, call. = FALSE)
    }
    # Then auto-generate for missing display names
    df <- auto_generate_display_name_variables(df)
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

#' Normalize and Validate Model Structure
#'
#' Applies type-specific CSV specs to enforce correct model structure.
#' Used by all three input paths (Excel, JSON, R Builder) to ensure consistency.
#'
#' @param model Raw model list
#' @param preserve_builder If TRUE, keeps oq_model_builder class; if FALSE, returns oq_model
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
  } else if (model_type_lower %in% c("custom_psm", "custom psm", "custompsm", "psm_custom")) {
    "custom_psm"
  } else {
    # Try to match partial strings
    if (model_type_lower %in% c("markov")) {
      "markov"
    } else if (model_type_lower %in% c("psm")) {
      "psm"
    } else if (model_type_lower %in% c("custom_psm", "custom psm", "custompsm", "psm_custom")) {
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
  spec_path <- system.file('model_input_specs', package = 'openqaly')

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

    # Validate that WTP is not specified for cost summaries
    if ("type" %in% names(model$summaries) && "wtp" %in% names(model$summaries)) {
      invalid_summaries <- model$summaries %>%
        filter(.data$type == "cost", !is.na(.data$wtp))

      if (nrow(invalid_summaries) > 0) {
        stop(paste0(
          "WTP cannot be specified for cost summaries. Invalid summaries: ",
          paste(invalid_summaries$name, collapse = ", ")
        ))
      }
    }
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

  # Ensure override_categories exists and is valid
  if (is.null(model$override_categories)) {
    model$override_categories <- list()
  }
  if (is.list(model$override_categories)) {
    for (i in seq_along(model$override_categories)) {
      cat_item <- model$override_categories[[i]]
      if (is.null(cat_item$name)) cat_item$name <- ""
      if (is.null(cat_item$general)) cat_item$general <- FALSE
      if (is.null(cat_item$overrides)) cat_item$overrides <- list()
      # Validate each override item has required fields
      for (j in seq_along(cat_item$overrides)) {
        ovr <- cat_item$overrides[[j]]
        if (is.null(ovr$title)) ovr$title <- ""
        if (is.null(ovr$description)) ovr$description <- ""
        if (is.null(ovr$type)) ovr$type <- "variable"
        if (is.null(ovr$name)) ovr$name <- ""
        if (is.null(ovr$strategy)) ovr$strategy <- ""
        if (is.null(ovr$group)) ovr$group <- ""
        if (is.null(ovr$general)) ovr$general <- FALSE
        if (is.null(ovr$input_type)) ovr$input_type <- "numeric"
        if (is.null(ovr$overridden_expression)) ovr$overridden_expression <- ""
        if (is.null(ovr$input_config)) ovr$input_config <- list()
        cat_item$overrides[[j]] <- ovr
      }
      model$override_categories[[i]] <- cat_item
    }
  }

  # Validate group names (no reserved keywords)
  if (!is.null(model$groups) && is.data.frame(model$groups) && nrow(model$groups) > 0) {
    validate_group_names(model$groups$name)
  }

  # Set class
  if (preserve_builder && "oq_model_builder" %in% class(model)) {
    class(model) <- c("oq_model_builder", "oq_model")
  } else {
    class(model) <- "oq_model"
  }

  return(model)
}

#'
#' Takes a JSON string and parses it into a oq_model object.
#'
#' @param json_string A string containing the model in JSON format.
#'
#' @return A oq_model object
#'
#' @export
read_model_json <- function(json_string) {
  # Parse JSON to list
  model <- fromJSON(json_string, simplifyVector = TRUE)

  # Normalize NULLs
  model <- normalize_model_nulls(model)

  # Convert tables array-of-objects to named list with description support
  if (!is.null(model$tables) && is.data.frame(model$tables) &&
      all(c("name", "data") %in% colnames(model$tables))) {
    table_list <- list()
    for (i in 1:nrow(model$tables)) {
      table_name <- model$tables$name[i]
      table_data <- model$tables$data$rows[[i]]
      # Read optional description field
      table_description <- if ("description" %in% colnames(model$tables)) {
        desc <- model$tables$description[i]
        if (is.na(desc) || desc == "") NULL else desc
      } else {
        NULL
      }
      # Store in new format with data and description
      table_list[[table_name]] <- list(
        data = as_tibble(table_data),
        description = table_description
      )
    }
    model$tables <- table_list
  } else {
    model$tables <- list()
  }

  # Convert scripts array-of-objects to named list with description support
  if (!is.null(model$scripts) && is.data.frame(model$scripts) &&
      all(c("name", "code") %in% colnames(model$scripts))) {
    script_list <- list()
    for (i in 1:nrow(model$scripts)) {
      script_name <- model$scripts$name[i]
      script_code <- model$scripts$code[i]
      # Read optional description field
      script_description <- if ("description" %in% colnames(model$scripts)) {
        desc <- model$scripts$description[i]
        if (is.na(desc) || desc == "") NULL else desc
      } else {
        NULL
      }
      # Store in new format with code and description
      script_list[[script_name]] <- list(
        code = script_code,
        description = script_description
      )
    }
    model$scripts <- script_list
  } else {
    model$scripts <- list()
  }

  # Convert multivariate_sampling from JSON nested structure to internal list structure
  # JSON format is an array of objects with: name, distribution, description, variables
  # where variables is itself an array of objects with: variable, strategy, group
  if (!is.null(model$multivariate_sampling)) {
    if (is.data.frame(model$multivariate_sampling)) {
      # Convert from dataframe format
      mv_list <- list()
      for (i in 1:nrow(model$multivariate_sampling)) {
        mv_spec <- model$multivariate_sampling[i, ]

        # Extract variables (could be a nested dataframe or list)
        variables_df <- if ("variables" %in% names(mv_spec)) {
          vars_data <- mv_spec$variables[[1]]
          if (is.data.frame(vars_data)) {
            as_tibble(vars_data)
          } else if (is.list(vars_data)) {
            as_tibble(do.call(rbind, lapply(vars_data, as.data.frame, stringsAsFactors = FALSE)))
          } else {
            tibble(variable = character(0), strategy = character(0), group = character(0))
          }
        } else {
          tibble(variable = character(0), strategy = character(0), group = character(0))
        }

        mv_list[[i]] <- list(
          name = mv_spec$name,
          distribution = mv_spec$distribution,
          description = if ("description" %in% names(mv_spec) && !is.na(mv_spec$description)) {
            mv_spec$description
          } else {
            ""
          },
          variables = variables_df
        )
      }
      model$multivariate_sampling <- mv_list
    } else if (is.list(model$multivariate_sampling) && !is.data.frame(model$multivariate_sampling)) {
      # Already in list format, just ensure variables are tibbles
      for (i in seq_along(model$multivariate_sampling)) {
        if ("variables" %in% names(model$multivariate_sampling[[i]])) {
          vars_data <- model$multivariate_sampling[[i]]$variables
          if (!is.data.frame(vars_data)) {
            model$multivariate_sampling[[i]]$variables <- as_tibble(vars_data)
          }
        }
      }
    }
  }

  # Deserialize DSA parameters
  if (!is.null(model$dsa_parameters) && is.list(model$dsa_parameters)) {
    dsa_list <- list()
    # Handle both list and data.frame formats
    params_list <- if (is.data.frame(model$dsa_parameters)) {
      # Convert data frame rows to list
      lapply(seq_len(nrow(model$dsa_parameters)), function(i) {
        as.list(model$dsa_parameters[i, ])
      })
    } else {
      model$dsa_parameters
    }

    for (i in seq_along(params_list)) {
      p <- params_list[[i]]
      param_type <- p$type %||% "variable"
      dsa_list[[i]] <- list(
        type = param_type,
        name = p$name,
        low = deserialize_to_formula(p$low, param_type),
        high = deserialize_to_formula(p$high, param_type),
        strategy = p$strategy %||% "",
        group = p$group %||% "",
        display_name = p$display_name
      )
    }
    model$dsa_parameters <- dsa_list
    class(model$dsa_parameters) <- "dsa_parameters"
  } else {
    model$dsa_parameters <- structure(list(), class = "dsa_parameters")
  }

  # Deserialize scenarios
  if (!is.null(model$scenarios) && is.list(model$scenarios)) {
    scenarios_list <- list()
    # Handle both list and data.frame formats
    scenario_items <- if (is.data.frame(model$scenarios)) {
      lapply(seq_len(nrow(model$scenarios)), function(i) {
        as.list(model$scenarios[i, ])
      })
    } else {
      model$scenarios
    }

    for (i in seq_along(scenario_items)) {
      s <- scenario_items[[i]]

      # Handle variable_overrides which may be nested
      var_overrides <- list()
      if (!is.null(s$variable_overrides) && length(s$variable_overrides) > 0) {
        override_items <- if (is.data.frame(s$variable_overrides)) {
          lapply(seq_len(nrow(s$variable_overrides)), function(j) {
            as.list(s$variable_overrides[j, ])
          })
        } else {
          s$variable_overrides
        }

        for (j in seq_along(override_items)) {
          v <- override_items[[j]]
          # Skip empty entries (can occur with simplifyVector = TRUE on empty arrays)
          if (is.null(v$name) || length(v) == 0) next

          # Handle case where simplifyVector=TRUE combined multiple overrides into one
          # with array values (e.g., name = c("a", "b"), value = c(1, 2))
          if (length(v$name) > 1) {
            for (k in seq_along(v$name)) {
              var_overrides[[length(var_overrides) + 1]] <- list(
                name = v$name[k],
                value = deserialize_to_formula(v$value[k], "variable"),
                strategy = if (!is.null(v$strategy) && length(v$strategy) >= k) v$strategy[k] else "",
                group = if (!is.null(v$group) && length(v$group) >= k) v$group[k] else ""
              )
            }
          } else {
            var_overrides[[length(var_overrides) + 1]] <- list(
              name = v$name,
              value = deserialize_to_formula(v$value, "variable"),
              strategy = v$strategy %||% "",
              group = v$group %||% ""
            )
          }
        }
      }

      # Handle setting_overrides
      setting_overrides <- list()
      if (!is.null(s$setting_overrides) && length(s$setting_overrides) > 0) {
        override_items <- if (is.data.frame(s$setting_overrides)) {
          lapply(seq_len(nrow(s$setting_overrides)), function(j) {
            as.list(s$setting_overrides[j, ])
          })
        } else {
          s$setting_overrides
        }

        for (j in seq_along(override_items)) {
          st <- override_items[[j]]
          # Skip empty entries (can occur with simplifyVector = TRUE on empty arrays)
          if (is.null(st$name) || length(st) == 0) next
          setting_overrides[[length(setting_overrides) + 1]] <- list(name = st$name, value = st$value)
        }
      }

      scenarios_list[[i]] <- list(
        name = s$name,
        description = s$description %||% "",
        variable_overrides = var_overrides,
        setting_overrides = setting_overrides
      )
    }
    model$scenarios <- scenarios_list
  } else {
    model$scenarios <- list()
  }

  # Deserialize TWSA analyses
  if (!is.null(model$twsa_analyses) && is.list(model$twsa_analyses)) {
    twsa_list <- list()
    # Handle both list and data.frame formats
    twsa_items <- if (is.data.frame(model$twsa_analyses)) {
      lapply(seq_len(nrow(model$twsa_analyses)), function(i) {
        as.list(model$twsa_analyses[i, ])
      })
    } else {
      model$twsa_analyses
    }

    for (i in seq_along(twsa_items)) {
      t <- twsa_items[[i]]

      # Handle parameters
      params_list <- list()
      if (!is.null(t$parameters)) {
        # Handle various parameter formats from JSON parsing:
        # 1. Direct data.frame (all params in one df)
        # 2. List containing a data.frame (jsonlite nested array parsing)
        # 3. List of parameter objects (simplifyVector=FALSE or already parsed)
        param_items <- if (is.data.frame(t$parameters)) {
          # Direct data.frame
          lapply(seq_len(nrow(t$parameters)), function(j) {
            as.list(t$parameters[j, ])
          })
        } else if (is.list(t$parameters) && length(t$parameters) == 1 &&
                   is.data.frame(t$parameters[[1]])) {
          # List containing a single data.frame (common from jsonlite)
          lapply(seq_len(nrow(t$parameters[[1]])), function(j) {
            as.list(t$parameters[[1]][j, ])
          })
        } else {
          # Assume it's already a list of parameter objects
          t$parameters
        }

        for (j in seq_along(param_items)) {
          p <- param_items[[j]]
          # Ensure scalar extraction for all fields
          param_type <- (p$param_type %||% "variable")[1]
          params_list[[j]] <- list(
            param_type = param_type,
            name = p$name[1],
            type = p$type[1],
            min = deserialize_to_formula(p$min, param_type),
            max = deserialize_to_formula(p$max, param_type),
            radius = deserialize_to_formula(p$radius, param_type),
            steps = p$steps[1],
            values = deserialize_to_formula(p$values, param_type),
            strategy = (p$strategy %||% "")[1],
            group = (p$group %||% "")[1],
            display_name = if (!is.null(p$display_name)) p$display_name[1] else NULL,
            include_base_case = (p$include_base_case %||% TRUE)[1]
          )
        }
      }

      twsa_list[[i]] <- list(
        name = t$name,
        description = t$description %||% "",
        parameters = params_list
      )
    }
    model$twsa_analyses <- twsa_list
  } else {
    model$twsa_analyses <- list()
  }

  # Parse override_categories from JSON
  if (!is.null(model$override_categories)) {
    if (is.data.frame(model$override_categories)) {
      # Simplified by fromJSON into a dataframe
      oc_list <- list()
      for (i in seq_len(nrow(model$override_categories))) {
        row <- model$override_categories[i, ]
        overrides_raw <- row$overrides[[1]]
        overrides_list <- list()
        if (!is.null(overrides_raw) && length(overrides_raw) > 0) {
          if (is.data.frame(overrides_raw)) {
            # input_config is a nested data.frame column when fromJSON simplifies
            ic_df <- overrides_raw$input_config
            for (j in seq_len(nrow(overrides_raw))) {
              ovr_row <- overrides_raw[j, ]
              # Extract input_config for this row from the nested data.frame
              input_config <- list()
              if (is.data.frame(ic_df)) {
                for (col_name in names(ic_df)) {
                  val <- ic_df[[col_name]]
                  if (is.list(val)) {
                    # List columns (e.g., options) - extract element for this row
                    elem <- val[[j]]
                    if (!is.null(elem)) {
                      if (is.data.frame(elem)) {
                        # Convert data.frame rows to list of lists (dropdown options)
                        input_config[[col_name]] <- lapply(
                          seq_len(nrow(elem)),
                          function(k) as.list(elem[k, ])
                        )
                      } else {
                        input_config[[col_name]] <- elem
                      }
                    }
                  } else {
                    # Scalar columns (min, max, step_size) - skip NA values
                    scalar_val <- val[[j]]
                    if (!is.na(scalar_val)) {
                      input_config[[col_name]] <- scalar_val
                    }
                  }
                }
              }
              overrides_list[[j]] <- list(
                title = ovr_row$title,
                description = ovr_row$description %||% "",
                type = ovr_row$type,
                name = ovr_row$name,
                strategy = ovr_row$strategy %||% "",
                group = ovr_row$group %||% "",
                general = as.logical(ovr_row$general),
                input_type = ovr_row$input_type,
                overridden_expression = as.character(ovr_row$overridden_expression),
                input_config = input_config
              )
            }
          } else if (is.list(overrides_raw)) {
            for (j in seq_along(overrides_raw)) {
              ovr <- overrides_raw[[j]]
              input_config <- if (is.null(ovr$input_config)) list() else ovr$input_config
              if (!is.null(input_config$options) && is.data.frame(input_config$options)) {
                input_config$options <- lapply(seq_len(nrow(input_config$options)), function(k) {
                  as.list(input_config$options[k, ])
                })
              }
              overrides_list[[j]] <- list(
                title = ovr$title,
                description = ovr$description %||% "",
                type = ovr$type,
                name = ovr$name,
                strategy = ovr$strategy %||% "",
                group = ovr$group %||% "",
                general = as.logical(ovr$general %||% FALSE),
                input_type = ovr$input_type,
                overridden_expression = as.character(ovr$overridden_expression),
                input_config = input_config
              )
            }
          }
        }
        oc_list[[i]] <- list(
          name = row$name,
          general = as.logical(row$general),
          overrides = overrides_list
        )
      }
      model$override_categories <- oc_list
    } else if (is.list(model$override_categories) && !is.data.frame(model$override_categories)) {
      # Already in list format, normalize
      for (i in seq_along(model$override_categories)) {
        cat_item <- model$override_categories[[i]]
        if (is.null(cat_item$overrides)) cat_item$overrides <- list()
        for (j in seq_along(cat_item$overrides)) {
          ovr <- cat_item$overrides[[j]]
          if (is.null(ovr$input_config)) ovr$input_config <- list()
          if (!is.null(ovr$input_config$options) && is.data.frame(ovr$input_config$options)) {
            ovr$input_config$options <- lapply(seq_len(nrow(ovr$input_config$options)), function(k) {
              as.list(ovr$input_config$options[k, ])
            })
          }
          ovr$description <- ovr$description %||% ""
          ovr$strategy <- ovr$strategy %||% ""
          ovr$group <- ovr$group %||% ""
          ovr$general <- as.logical(ovr$general %||% FALSE)
          ovr$overridden_expression <- as.character(ovr$overridden_expression)
          cat_item$overrides[[j]] <- ovr
        }
        cat_item$general <- as.logical(cat_item$general %||% FALSE)
        model$override_categories[[i]] <- cat_item
      }
    }
  } else {
    model$override_categories <- list()
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
#' @param values_spec Optional values specification list
#' @return The model with properly formatted data frames
convert_json_dataframes <- function(model, values_spec = NULL) {
  # Load all specs if not provided
  if (is.null(values_spec)) {
    model_input_specs <- system.file('model_input_specs', package = 'openqaly') %>%
      list.files() %>%
      set_names(str_split_fixed(., '\\.', Inf)[,1]) %>%
      map(function(x) {
        suppressWarnings(read_csv(
          system.file('model_input_specs', x, package = 'openqaly'),
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
    values = character(0),
    type = character(0),
    wtp = numeric(0)
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

#' Serialize Formula or Value for JSON
#'
#' Converts oq_formula objects to strings and leaves other values as-is.
#'
#' @param x Value to serialize (oq_formula, numeric, character, or NULL)
#' @return Serialized value suitable for JSON
#' @keywords internal
serialize_formula_or_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "oq_formula")) {
    return(as.character(x))  # Convert to string
  }
  return(x)  # Return as-is for literals
}

#' Deserialize Value to Formula
#'
#' Converts string values to oq_formula objects where appropriate.
#'
#' @param x Value from JSON (string or numeric)
#' @param type Parameter type ("variable" or "setting")
#' @return Deserialized value (oq_formula for variables, literal for settings)
#' @keywords internal
deserialize_to_formula <- function(x, type) {
  if (is.null(x)) return(NULL)
  # Ensure type is scalar
  type <- type[1]
  # Settings use literal values
  if (type == "setting") return(x)
  # Variables: convert strings to oq_formula, keep numerics as-is
  if (is.character(x)) return(as.oq_formula(x))
  if (is.numeric(x)) return(x)
  return(x)
}

#' Write Model to JSON
#'
#' Takes a oq_model object (typically loaded from Excel) and converts it
#' to a JSON string format.
#'
#' @param model A oq_model object to be converted to JSON.
#'
#' @return A JSON string representing the model.
#'
#' @export
write_model_json <- function(model) {
  # Ensure model is a oq_model object
  if (!"oq_model" %in% class(model)) {
    stop("Input must be a oq_model object", call. = FALSE)
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
        table_entry <- model$tables[[table_name]]

        # Handle both old format (direct data frame) and new format (list with data/description)
        if (is.data.frame(table_entry)) {
          # Old format - direct data frame
          table_data <- table_entry
          table_description <- NULL
        } else if (is.list(table_entry) && "data" %in% names(table_entry)) {
          # New format - extract from list
          table_data <- table_entry$data
          table_description <- table_entry$description
        } else {
          next  # Skip invalid entries
        }

        # Ensure table_data is a data frame
        if (is.data.frame(table_data)) {
          # Convert factors to characters
          table_data[] <- lapply(table_data, function(x) {
            if (is.factor(x)) as.character(x) else x
          })

          tables_array[[i]] <- list(
            name = table_name,
            description = table_description,
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
        script_entry <- model$scripts[[script_name]]

        # Handle both old format (direct code string) and new format (list with code/description)
        if (is.character(script_entry)) {
          # Old format - direct code string
          script_code <- script_entry
          script_description <- NULL
        } else if (is.list(script_entry) && "code" %in% names(script_entry)) {
          # New format - extract from list
          script_code <- script_entry$code
          script_description <- script_entry$description
        } else {
          next  # Skip invalid entries
        }

        scripts_array[[i]] <- list(
          name = script_name,
          description = script_description,
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

  # Convert multivariate_sampling to array format expected by JSON
  if (!is.null(model$multivariate_sampling) && is.list(model$multivariate_sampling)) {
    if (length(model$multivariate_sampling) > 0) {
      mv_array <- list()
      for (i in seq_along(model$multivariate_sampling)) {
        mv_spec <- model$multivariate_sampling[[i]]

        # Ensure variables dataframe exists
        vars_df <- if ("variables" %in% names(mv_spec) && is.data.frame(mv_spec$variables)) {
          mv_spec$variables
        } else {
          tibble(variable = character(0), strategy = character(0), group = character(0))
        }

        mv_array[[i]] <- list(
          name = mv_spec$name,
          distribution = mv_spec$distribution,
          description = if ("description" %in% names(mv_spec)) mv_spec$description else "",
          variables = vars_df
        )
      }
      json_model$multivariate_sampling <- mv_array
    } else {
      json_model$multivariate_sampling <- list()
    }
  }

  # Convert DSA parameters to array format
  if (!is.null(model$dsa_parameters) && length(model$dsa_parameters) > 0) {
    dsa_array <- list()
    for (i in seq_along(model$dsa_parameters)) {
      param <- model$dsa_parameters[[i]]
      dsa_entry <- list(
        type = param$type,
        name = param$name,
        low = serialize_formula_or_value(param$low),
        high = serialize_formula_or_value(param$high)
      )
      # Add optional fields for variable-type DSA
      if (param$type == "variable") {
        dsa_entry$strategy <- param$strategy %||% ""
        dsa_entry$group <- param$group %||% ""
      }
      dsa_entry$display_name <- param$display_name
      dsa_array[[i]] <- dsa_entry
    }
    json_model$dsa_parameters <- dsa_array
  }

  # Convert scenarios to array format
  if (!is.null(model$scenarios) && length(model$scenarios) > 0) {
    scenarios_array <- list()
    for (i in seq_along(model$scenarios)) {
      scenario <- model$scenarios[[i]]

      # Convert variable overrides
      var_overrides <- lapply(scenario$variable_overrides, function(v) {
        list(
          name = v$name,
          value = serialize_formula_or_value(v$value),
          strategy = v$strategy %||% "",
          group = v$group %||% ""
        )
      })

      # Convert setting overrides
      setting_overrides <- lapply(scenario$setting_overrides, function(s) {
        list(name = s$name, value = s$value)
      })

      scenarios_array[[i]] <- list(
        name = scenario$name,
        description = scenario$description %||% "",
        variable_overrides = var_overrides,
        setting_overrides = setting_overrides
      )
    }
    json_model$scenarios <- scenarios_array
  }

  # Convert TWSA analyses to array format
  if (!is.null(model$twsa_analyses) && length(model$twsa_analyses) > 0) {
    twsa_array <- list()
    for (i in seq_along(model$twsa_analyses)) {
      twsa <- model$twsa_analyses[[i]]

      # Convert parameters
      params <- lapply(twsa$parameters, function(p) {
        param_entry <- list(
          param_type = p$param_type,
          name = p$name,
          type = p$type,
          min = serialize_formula_or_value(p$min),
          max = serialize_formula_or_value(p$max),
          radius = serialize_formula_or_value(p$radius),
          steps = p$steps,
          values = serialize_formula_or_value(p$values),
          display_name = p$display_name,
          include_base_case = p$include_base_case %||% TRUE
        )
        # Add strategy/group for variable-type parameters
        if (p$param_type == "variable") {
          param_entry$strategy <- p$strategy %||% ""
          param_entry$group <- p$group %||% ""
        }
        param_entry
      })

      twsa_array[[i]] <- list(
        name = twsa$name,
        description = twsa$description %||% "",
        parameters = params
      )
    }
    json_model$twsa_analyses <- twsa_array
  }

  # Convert override_categories to array format
  if (!is.null(model$override_categories) && length(model$override_categories) > 0) {
    oc_array <- lapply(model$override_categories, function(cat_item) {
      overrides_array <- lapply(cat_item$overrides, function(ovr) {
        result <- list(
          title = ovr$title,
          description = ovr$description %||% "",
          type = ovr$type,
          name = ovr$name,
          strategy = ovr$strategy %||% "",
          group = ovr$group %||% "",
          general = ovr$general,
          input_type = ovr$input_type,
          overridden_expression = ovr$overridden_expression,
          input_config = ovr$input_config
        )
        result
      })

      list(
        name = cat_item$name,
        general = cat_item$general,
        overrides = overrides_array
      )
    })
    json_model$override_categories <- oc_array
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


#' Validate Group Names for Reserved Keywords
#'
#' Checks that group names do not use reserved keywords that are used
#' for special operations in the groups argument.
#'
#' @param group_names Character vector of group names to validate
#'
#' @return Invisibly returns TRUE if valid, otherwise stops with error
#' @keywords internal
validate_group_names <- function(group_names) {
  reserved <- c("overall", "all", "all_groups")
  conflicts <- intersect(group_names, reserved)

  if (length(conflicts) > 0) {
    stop(sprintf(
      "Reserved group names detected: %s\nThese names are reserved for special operations. Please rename your groups to avoid these reserved keywords.",
      paste(conflicts, collapse = ", ")
    ))
  }

  invisible(TRUE)
}


#' Resolve Groups Argument to Actual Group Names
#'
#' Internal helper that resolves the groups argument (which can be NULL, strings, or vectors)
#' into a list specifying which groups and whether to include overall.
#'
#' @param groups The groups argument value (NULL, character vector, or string)
#' @param results A openqaly model results object
#'
#' @return List with elements:
#'   \item{include_overall}{Logical - whether to include overall/aggregated}
#'   \item{include_groups}{Character vector of group names to include}
#'
#' @keywords internal
resolve_groups <- function(groups, results) {
  # Get available groups from results
  available_groups <- if (!is.null(results$segments) && nrow(results$segments) > 0) {
    unique(results$segments$group)
  } else {
    character(0)
  }

  # Reserved keywords
  reserved <- c("overall", "all", "all_groups")

  # Check for conflicts (should have been caught at model load, but double-check)
  conflicts <- intersect(available_groups, reserved)
  if (length(conflicts) > 0) {
    stop(sprintf(
      "Model contains reserved group names: %s\nThis should have been caught during model loading.",
      paste(conflicts, collapse = ", ")
    ))
  }

  # Expand special keywords
  if (is.character(groups) && length(groups) == 1 && groups == "all") {
    return(list(
      include_overall = TRUE,
      include_groups = available_groups
    ))
  }

  if (is.character(groups) && length(groups) == 1 && groups == "all_groups") {
    return(list(
      include_overall = FALSE,
      include_groups = available_groups
    ))
  }

  if (is.character(groups) && length(groups) == 1 && groups == "overall") {
    return(list(
      include_overall = TRUE,
      include_groups = character(0)
    ))
  }

  # Multiple values - check for "overall" and actual group names
  include_overall <- "overall" %in% groups
  include_groups <- setdiff(groups, "overall")

  # Validate that all non-reserved groups exist
  if (length(include_groups) > 0) {
    # Use check_groups_exist helper for consistent formatted error messages
    # Create a mock results object with segments for validation
    mock_results <- list(segments = data.frame(group = available_groups))
    check_groups_exist(include_groups, mock_results)
  }

  list(
    include_overall = include_overall,
    include_groups = include_groups
  )
}


#' Select Source Data Based on Groups Argument
#'
#' Internal helper that selects the appropriate source data from results
#' based on the resolved groups specification.
#'
#' @param groups The groups argument value
#' @param results A openqaly model results object
#'
#' @return Tibble with combined data from requested groups
#' @keywords internal
select_source_data <- function(groups, results) {
  resolved <- resolve_groups(groups, results)

  parts <- list()

  if (resolved$include_overall) {
    parts[[length(parts) + 1]] <- results$aggregated %>%
      mutate(group = "Overall")
  }

  if (length(resolved$include_groups) > 0) {
    parts[[length(parts) + 1]] <- results$segments %>%
      filter(.data$group %in% resolved$include_groups)
  }

  if (length(parts) == 0) {
    stop("No groups selected")
  }

  bind_rows(parts)
}


#' Get Consistent Group Order
#'
#' Returns group ordering with "Overall" first (if present), followed by
#' groups in model definition order from metadata.
#'
#' @param groups Character vector of group names present in data
#' @param metadata Model metadata containing groups definition
#' @return Character vector of ordered group names
#' @keywords internal
get_group_order <- function(groups, metadata) {
  has_overall <- "Overall" %in% groups
  has_aggregated <- "_aggregated" %in% groups

  # Get model-defined group order from metadata
  if (!is.null(metadata$groups)) {
    model_order <- metadata$groups$display_name
    ordered_groups <- model_order[model_order %in% groups]
  } else {
    ordered_groups <- setdiff(groups, c("Overall", "_aggregated"))
  }

  # Overall/_aggregated first, then model order
  if (has_overall) {
    c("Overall", ordered_groups)
  } else if (has_aggregated) {
    c("_aggregated", ordered_groups)
  } else {
    ordered_groups
  }
}


# =============================================================================
# Override Application Functions
# =============================================================================

#' Apply Setting Overrides to Model
#'
#' Checks segment for setting_overrides column and applies them to the model.
#' This helper is called from within run_segment methods to modify model
#' settings before creating the namespace.
#'
#' @param segment Segment tibble (single row)
#' @param model Model object
#' @return Modified model object (or original if no overrides)
#' @keywords internal
apply_setting_overrides <- function(segment, model) {
  # Check if segment has setting_overrides column
  if (!"setting_overrides" %in% names(segment)) {
    return(model)
  }

  # Get overrides (handle both list column and direct list)
  overrides <- if (is.list(segment$setting_overrides)) {
    segment$setting_overrides[[1]]
  } else {
    segment$setting_overrides
  }

  # If no overrides, return original model
  if (is.null(overrides) || length(overrides) == 0) {
    return(model)
  }

  # Clone model to avoid modifying original
  modified_model <- model

  # Apply each override
  for (setting_name in names(overrides)) {
    override_value <- overrides[[setting_name]]

    # Apply to model settings
    modified_model$settings[[setting_name]] <- override_value

    # Note: cycle_length_days and n_cycles are recalculated in run_segment
    # after this function returns, ensuring correct values with overrides
  }

  modified_model
}

#' Apply Parameter Overrides to Namespace
#'
#' Applies parameter overrides from a segment to the namespace and filters
#' unevaluated variables to exclude overridden ones. This is the counterpart
#' to apply_setting_overrides() for variable-level overrides.
#'
#' @param segment Segment containing parameter_overrides column
#' @param ns Namespace object
#' @param uneval_vars Unevaluated variables tibble
#' @return List with ns (modified namespace) and uneval_vars (filtered)
#' @keywords internal
apply_parameter_overrides <- function(segment, ns, uneval_vars) {
  if (!"parameter_overrides" %in% names(segment)) {
    return(list(ns = ns, uneval_vars = uneval_vars))
  }

  override_vals <- segment$parameter_overrides[[1]]

  if (is.null(override_vals) || length(override_vals) == 0) {
    return(list(ns = ns, uneval_vars = uneval_vars))
  }

  for (var_name in names(override_vals)) {
    val <- override_vals[[var_name]]
    assign(var_name, val, envir = ns$env)
  }

  uneval_vars <- uneval_vars %>%
    filter(!.data$name %in% names(override_vals))

  list(ns = ns, uneval_vars = uneval_vars)
}

#' Parse Override Expression to a Value
#'
#' Converts an override's overridden_expression to the appropriate value
#' for injection into parameter_overrides or setting_overrides.
#'
#' @param override An override item list
#' @return The parsed value (numeric, character, or oq_formula)
#' @keywords internal
parse_override_expression <- function(override) {
  expr_str <- override$overridden_expression

  switch(override$input_type,
    "numeric" = ,
    "slider" = {
      as.numeric(expr_str)
    },
    "dropdown" = {
      # Try numeric first, fall back to character
      num_val <- suppressWarnings(as.numeric(expr_str))
      if (!is.na(num_val)) num_val else expr_str
    },
    "formula" = {
      as.oq_formula(expr_str)
    },
    "timeframe" = {
      # Split "value|unit" format
      parts <- strsplit(expr_str, "\\|")[[1]]
      list(
        timeframe = as.numeric(parts[1]),
        timeframe_unit = if (length(parts) > 1) parts[2] else "years"
      )
    },
    # Default fallback
    {
      num_val <- suppressWarnings(as.numeric(expr_str))
      if (!is.na(num_val)) num_val else expr_str
    }
  )
}

#' Apply Override Categories to Segments
#'
#' Injects parameter_overrides and setting_overrides columns into the segments
#' tibble based on active override definitions from model$override_categories.
#'
#' @param model Parsed model object with override_categories
#' @param segments Segments tibble from get_segments()
#' @return Modified segments tibble with parameter_overrides and setting_overrides columns
#' @keywords internal
apply_override_categories <- function(model, segments) {
  # If no override categories or no overrides, return segments unchanged
  if (is.null(model$override_categories) || length(model$override_categories) == 0) {
    return(segments)
  }

  # Count total overrides
  total_overrides <- sum(sapply(model$override_categories, function(c) length(c$overrides)))
  if (total_overrides == 0) {
    return(segments)
  }

  # Initialize override columns if not present
  if (!"parameter_overrides" %in% names(segments)) {
    segments$parameter_overrides <- lapply(seq_len(nrow(segments)), function(i) list())
  }
  if (!"setting_overrides" %in% names(segments)) {
    segments$setting_overrides <- lapply(seq_len(nrow(segments)), function(i) list())
  }

  for (seg_idx in seq_len(nrow(segments))) {
    seg_strategy <- segments$strategy[[seg_idx]]
    seg_group <- segments$group[[seg_idx]]

    for (cat_item in model$override_categories) {
      for (override in cat_item$overrides) {
        if (override$type == "variable") {
          # Check if override applies to this segment
          applies <- TRUE
          if (!is.na(override$strategy) && override$strategy != "" &&
              override$strategy != seg_strategy) {
            applies <- FALSE
          }
          if (!is.na(override$group) && override$group != "" &&
              override$group != seg_group) {
            applies <- FALSE
          }

          if (applies) {
            parsed_val <- parse_override_expression(override)
            segments$parameter_overrides[[seg_idx]][[override$name]] <- parsed_val
          }
        } else if (override$type == "setting") {
          # Settings are global - apply to all segments
          if (override$input_type == "timeframe") {
            parsed_val <- parse_override_expression(override)
            segments$setting_overrides[[seg_idx]][["timeframe"]] <- parsed_val$timeframe
            segments$setting_overrides[[seg_idx]][["timeframe_unit"]] <- parsed_val$timeframe_unit
          } else {
            parsed_val <- parse_override_expression(override)
            segments$setting_overrides[[seg_idx]][[override$name]] <- parsed_val
          }
        }
      }
    }
  }

  segments
}

#' Emit Override Notification Message
#'
#' Prints an informational message about active overrides being applied.
#'
#' @param model Model object with override_categories
#' @keywords internal
notify_active_overrides <- function(model) {
  if (is.null(model$override_categories) || length(model$override_categories) == 0) {
    return(invisible(NULL))
  }

  overrides_info <- list()
  for (cat_item in model$override_categories) {
    for (override in cat_item$overrides) {
      overrides_info <- c(overrides_info, list(override))
    }
  }

  if (length(overrides_info) == 0) {
    return(invisible(NULL))
  }

  lines <- sprintf("Note: Model has %d active override(s) that will be applied:", length(overrides_info))
  for (ovr in overrides_info) {
    lines <- c(lines, sprintf("  - %s = %s (%s override)",
      ovr$name, ovr$overridden_expression, ovr$type))
  }
  message(paste(lines, collapse = "\n"))
  invisible(NULL)
}