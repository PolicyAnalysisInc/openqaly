#' Read a Model from Directory
#'
#' Reads a complete openqaly model from a directory containing
#' a YAML model file (`model.yaml`).
#'
#' @param path Path to the model directory containing `model.yaml`.
#'
#' @return A normalized and validated `oq_model` object.
#'
#' @export
read_model <- function(path) {
  yaml_file <- file.path(path, "model.yaml")
  if (!file.exists(yaml_file)) {
    stop("No model.yaml found in directory: ", path)
  }
  read_model_yaml(file = yaml_file)
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
  } else {
    # Default to FALSE if not specified
    settings[["reduce_state_cycle"]] <- FALSE
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

  # Handle discount_timing setting
  if ("discount_timing" %in% names(settings)) {
    val <- settings[["discount_timing"]]
    if (is.character(val)) {
      val_lower <- tolower(val)
      if (val_lower %in% c("start", "end", "midpoint")) {
        settings[["discount_timing"]] <- val_lower
      } else {
        warning(glue("Invalid discount_timing: {val} - must be 'start', 'end', or 'midpoint'. Defaulting to 'start'"))
        settings[["discount_timing"]] <- "start"
      }
    } else {
      warning(glue("discount_timing must be a string, got: {class(val)} - defaulting to 'start'"))
      settings[["discount_timing"]] <- "start"
    }
  } else {
    settings[["discount_timing"]] <- "start"
  }

  # Handle discount_method setting
  if ("discount_method" %in% names(settings)) {
    val <- settings[["discount_method"]]
    if (is.character(val)) {
      val_lower <- tolower(val)
      if (val_lower %in% c("by_cycle", "by_year")) {
        settings[["discount_method"]] <- val_lower
      } else {
        warning(glue("Invalid discount_method: {val} - must be 'by_cycle' or 'by_year'. Defaulting to 'by_cycle'"))
        settings[["discount_method"]] <- "by_cycle"
      }
    } else {
      warning(glue("discount_method must be a string, got: {class(val)} - defaulting to 'by_cycle'"))
      settings[["discount_method"]] <- "by_cycle"
    }
  } else {
    settings[["discount_method"]] <- "by_cycle"
  }

  # Validate that discount rates are provided (mandatory)
  if (!("discount_cost" %in% names(settings))) {
    stop("discount_cost is required but was not provided in settings")
  }
  if (!("discount_outcomes" %in% names(settings))) {
    stop("discount_outcomes is required but was not provided in settings")
  }

  # Warn if discount rates look unreasonably high
  if (settings$discount_cost > 100) {
    warning("discount_cost is > 100. Discount rates should be percentages (e.g., 3 for 3%).")
  }
  if (settings$discount_outcomes > 100) {
    warning("discount_outcomes is > 100. Discount rates should be percentages (e.g., 3 for 3%).")
  }

  settings
}



define_object_ <- function(obj, class) {
  class(obj) <- class
  obj
}

#' @keywords internal
fast_tibble <- function(...) {
  args <- list(...)
  args <- lapply(args, function(x) if (is.null(x)) NA else x)
  as_tibble(args)
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

#' Format Conflicting Value Fields as Table
#'
#' Helper function to format conflicting value field information as a markdown-style table.
#' Used in validation error messages for value display_name/description consistency.
#'
#' @param values_df Dataframe of values with the same name
#' @param field_name The field that has conflicting values (e.g., "display_name" or "description")
#'
#' @return Formatted table string
#' @keywords internal
format_conflicting_value_fields_table <- function(values_df, field_name) {
  table_data <- data.frame(
    State = character(nrow(values_df)),
    Destination = character(nrow(values_df)),
    stringsAsFactors = FALSE
  )
  table_data[[field_name]] <- character(nrow(values_df))

  for (i in seq_len(nrow(values_df))) {
    state_val <- values_df$state[i]
    dest_val <- values_df$destination[i]

    table_data$State[i] <- if (!is.na(state_val) && state_val != "") state_val else "NA"
    table_data$Destination[i] <- if (!is.na(dest_val) && dest_val != "") dest_val else "NA"
    table_data[[field_name]][i] <- as.character(values_df[[field_name]][i])
  }

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

#' Validate Value Display Names and Descriptions
#'
#' Ensures that for values with multiple rows (different state/destination),
#' display_name and description are consistent across all rows with the same name.
#'
#' @param df Data frame containing values with columns: name, display_name, description, state, destination
#'
#' @return Empty string if valid, error message if invalid
#' @keywords internal
validate_value_display_names <- function(df) {
  value_names <- unique(df$name)

  for (val_name in value_names) {
    if (is.na(val_name)) next
    val_rows <- df[!is.na(df$name) & df$name == val_name, ]

    if (nrow(val_rows) <= 1) next

    # Check display_name consistency
    display_names <- val_rows$display_name
    display_names <- display_names[!is.na(display_names)]
    if (length(unique(display_names)) > 1) {
      table_string <- format_conflicting_value_fields_table(val_rows, "display_name")
      return(sprintf(
        "Value '%s': display_name must be consistent across all definitions.\n\nConflicting values found:\n\n%s",
        val_name, table_string
      ))
    }

    # Check description consistency
    descriptions <- val_rows$description
    descriptions <- descriptions[!is.na(descriptions)]
    if (length(unique(descriptions)) > 1) {
      table_string <- format_conflicting_value_fields_table(val_rows, "description")
      return(sprintf(
        "Value '%s': description must be consistent across all definitions.\n\nConflicting values found:\n\n%s",
        val_name, table_string
      ))
    }
  }

  return("")
}

#' Validate Value Display Names for Builder Context
#'
#' Validates display_name and description consistency for values being added via
#' the R model builder. Called after each add_value() to catch conflicts early.
#'
#' @param values_df Dataframe of values with the same name
#' @param value_name The value name being validated
#'
#' @return Empty string if valid, error message if invalid
#' @keywords internal
validate_value_display_names_for_builder <- function(values_df, value_name) {
  if (nrow(values_df) <= 1) {
    return("")
  }

  # Check display_name consistency
  display_names <- values_df$display_name
  display_names <- display_names[!is.na(display_names)]
  if (length(unique(display_names)) > 1) {
    table_string <- format_conflicting_value_fields_table(values_df, "display_name")
    return(sprintf(
      "Value '%s': display_name must be consistent across all definitions.\n\nConflicting values found:\n\n%s",
      value_name, table_string
    ))
  }

  # Check description consistency
  descriptions <- values_df$description
  descriptions <- descriptions[!is.na(descriptions)]
  if (length(unique(descriptions)) > 1) {
    table_string <- format_conflicting_value_fields_table(values_df, "description")
    return(sprintf(
      "Value '%s': description must be consistent across all definitions.\n\nConflicting values found:\n\n%s",
      value_name, table_string
    ))
  }

  return("")
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

  # Validate value display_name/description consistency (values have state/destination but not strategy/group)
  if ("display_name" %in% spec_cn &&
      all(c("state", "destination") %in% colnames(df)) &&
      !all(c("strategy", "group") %in% colnames(df))) {
    error_msg <- validate_value_display_names(df)
    if (error_msg != "") stop(error_msg, call. = FALSE)
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
#' Validate model settings
#'
#' @param settings A list of model settings
#' @param model_type The canonical model type string
#' @keywords internal
validate_settings <- function(settings, model_type) {
  valid_units <- c("days", "weeks", "months", "years")
  valid_cycle_units <- c(valid_units, "cycles")

  # Non-decision-tree models require timeframe and cycle settings
  if (model_type != "decision_tree") {
    if (is.null(settings[["timeframe"]])) {
      stop("Model settings must include 'timeframe'.", call. = FALSE)
    }
    if (is.null(settings[["timeframe_unit"]])) {
      stop("Model settings must include 'timeframe_unit'.", call. = FALSE)
    }
    if (is.null(settings[["cycle_length"]])) {
      stop("Model settings must include 'cycle_length'.", call. = FALSE)
    }
    if (is.null(settings[["cycle_length_unit"]])) {
      stop("Model settings must include 'cycle_length_unit'.", call. = FALSE)
    }

    tf <- suppressWarnings(as.numeric(settings[["timeframe"]]))
    if (is.na(tf)) {
      stop(sprintf("Setting 'timeframe' must be numeric, got '%s'.", settings[["timeframe"]]), call. = FALSE)
    }
    if (tf <= 0) {
      stop(sprintf("Setting 'timeframe' must be positive, got %s.", tf), call. = FALSE)
    }

    cl <- suppressWarnings(as.numeric(settings[["cycle_length"]]))
    if (is.na(cl)) {
      stop(sprintf("Setting 'cycle_length' must be numeric, got '%s'.", settings[["cycle_length"]]), call. = FALSE)
    }
    if (cl <= 0) {
      stop(sprintf("Setting 'cycle_length' must be positive, got %s.", cl), call. = FALSE)
    }

    tu <- tolower(as.character(settings[["timeframe_unit"]]))
    if (!tu %in% valid_cycle_units) {
      stop(sprintf("Invalid timeframe_unit '%s'. Valid options: %s",
                   settings[["timeframe_unit"]], paste(valid_cycle_units, collapse = ", ")), call. = FALSE)
    }

    cu <- tolower(as.character(settings[["cycle_length_unit"]]))
    if (!cu %in% valid_cycle_units) {
      stop(sprintf("Invalid cycle_length_unit '%s'. Valid options: %s",
                   settings[["cycle_length_unit"]], paste(valid_cycle_units, collapse = ", ")), call. = FALSE)
    }
  }

  # Validate discount rates (all model types)
  for (field in c("discount_cost", "discount_outcomes")) {
    val <- settings[[field]]
    if (!is.null(val)) {
      num_val <- suppressWarnings(as.numeric(val))
      if (!is.na(num_val)) {
        if (num_val < 0) {
          stop(sprintf("Setting '%s' must be non-negative, got %s.", field, num_val), call. = FALSE)
        }
        if (num_val > 100) {
          stop(sprintf("Setting '%s' is %s, which exceeds 100. Discount rates are specified as percentages (e.g., 3 for 3%%).",
                       field, num_val), call. = FALSE)
        }
      } else {
        stop(sprintf("Setting '%s' must be numeric, got '%s'.", field, val), call. = FALSE)
      }
    }
  }
}

#' Applies type-specific CSV specs to enforce correct model structure.
#' Used by all three input paths (Excel, JSON, R Builder) to ensure consistency.
#'
#' @param model Raw model list
#'
#' @return Validated model with correct structure
#' @export
normalize_and_validate_model <- function(model) {

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
  } else if (model_type_lower %in% c("decision_tree", "decision tree", "decisiontree")) {
    "decision_tree"
  } else {
    # Try to match partial strings
    if (model_type_lower %in% c("markov")) {
      "markov"
    } else if (model_type_lower %in% c("psm")) {
      "psm"
    } else if (model_type_lower %in% c("custom_psm", "custom psm", "custompsm", "psm_custom")) {
      "custom_psm"
    } else if (model_type_lower %in% c("decision_tree", "decision tree", "decisiontree")) {
      "decision_tree"
    } else {
      warning("Invalid model_type '", model_type, "'. Defaulting to 'markov'.")
      "markov"
    }
  }

  # Update model_type in settings to canonical form
  if (is.list(model$settings) && !is.data.frame(model$settings)) {
    model$settings$model_type <- model_type
  }

  # Load type-specific specs from cached model_input_specs
  states_spec_key <- if (model_type %in% c("psm", "custom_psm", "decision_tree")) {
    "psm_states"
  } else {
    "states"
  }

  trans_spec_key <- if (model_type == "psm") {
    "psm_transitions"
  } else if (model_type == "custom_psm") {
    "psm_custom_transitions"
  } else {
    "transitions"
  }

  specs <- list(
    states = model_input_specs[[states_spec_key]],
    transitions = model_input_specs[[trans_spec_key]],
    values = model_input_specs[["values"]],
    strategies = model_input_specs[["strategies"]],
    groups = model_input_specs[["groups"]],
    variables = model_input_specs[["variables"]],
    summaries = model_input_specs[["summaries"]]
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

  # Ensure threshold_analyses exists
  if (is.null(model$threshold_analyses)) {
    model$threshold_analyses <- list()
  }

  # Ensure vbp is valid if present
  if (!is.null(model$vbp) && !is.list(model$vbp)) {
    model$vbp <- NULL
  }

  # Ensure psa is valid if present
  if (!is.null(model$psa) && !is.list(model$psa)) {
    model$psa <- NULL
  }

  # Ensure documentation is valid if present
  if (!is.null(model$documentation)) {
    if (!is.character(model$documentation) || length(model$documentation) != 1) {
      model$documentation <- NULL
    }
  }

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

  # Ensure decision_tree field exists
  if (is.null(model$decision_tree)) {
    # Leave as NULL
  }

  # Validate decision tree configuration if present
  validate_decision_tree(model)

  # Validate group names (no reserved keywords)
  if (!is.null(model$groups) && is.data.frame(model$groups) && nrow(model$groups) > 0) {
    validate_group_names(model$groups$name)
  }

  # Validate no name collisions between tables and values
  if (length(model$tables) > 0 && is.data.frame(model$values) && nrow(model$values) > 0) {
    table_names <- names(model$tables)
    value_names <- unique(model$values$name[!is.na(model$values$name)])
    collisions <- intersect(table_names, value_names)
    if (length(collisions) > 0) {
      stop(sprintf(
        "Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.",
        paste(collisions, collapse = ", ")
      ))
    }
  }

  # Validate no name collisions between trees and other components
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    validate_tree_name_collisions(unique(model$trees$name), model)
  }

  # Check model environment if lockfile is present
  if (!is.null(model$lockfile)) {
    check_action <- getOption("openqaly.env_check", default = "warn")
    if (!identical(check_action, "none")) {
      check_model_environment(model, action = check_action)
    }
  }

  # Set class based on model type
  model_type_str <- tolower(model$settings$model_type %||% "markov")
  class(model) <- switch(model_type_str,
    markov = c("oq_markov", "oq_model"),
    psm = c("oq_psm", "oq_model"),
    custom_psm = c("oq_custom_psm", "oq_model"),
    decision_tree = c("oq_decision_tree", "oq_model"),
    c("oq_markov", "oq_model")
  )

  return(model)
}

#'
#' Read a model from JSON.
#'
#' Supply exactly one of \code{file} or \code{text}.
#'
#' @param file Path to a JSON file containing the model.
#' @param text A string containing the model in JSON format.
#'
#' @return An oq_model object
#'
#' @export
read_model_json <- function(file = NULL, text = NULL) {
  if (!is.null(file) && !is.null(text)) {
    stop("Provide either 'file' or 'text', not both.")
  }
  if (is.null(file) && is.null(text)) {
    stop("One of 'file' or 'text' must be provided.")
  }
  json_string <- if (!is.null(file)) {
    paste(readLines(file, warn = FALSE), collapse = "\n")
  } else {
    text
  }
  # Parse JSON to list
  model <- fromJSON(json_string, simplifyVector = TRUE)

  # Extract renv lockfile fields if present
  if (!is.null(model$R) && !is.null(model$Packages)) {
    # Re-parse with simplifyVector=FALSE to preserve lockfile structure
    raw <- fromJSON(json_string, simplifyVector = FALSE)
    model$lockfile <- list(R = raw$R, Packages = raw$Packages)
    if (!is.null(raw$Bioconductor)) model$lockfile$Bioconductor <- raw$Bioconductor
    if (!is.null(raw$Python)) model$lockfile$Python <- raw$Python
    model$R <- NULL
    model$Packages <- NULL
    model$Bioconductor <- NULL
    model$Python <- NULL
  }

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
  if (!is.null(model$multivariate_sampling)) {
    parse_mv_spec <- function(mv) {
      # Extract variables — may be a list column from fromJSON
      vars <- mv[["variables"]]
      if (is.list(vars) && !is.character(vars)) vars <- unlist(vars)
      vars <- as.character(vars)

      result <- list(
        name = mv[["name"]],
        type = mv[["type"]],
        strategy = mv[["strategy"]] %||% "",
        group = mv[["group"]] %||% "",
        description = mv[["description"]] %||% "",
        variables = vars
      )
      # Use [[ to avoid R partial matching (e.g., $n matching $name)
      cov_val <- mv[["covariance"]]
      n_val <- mv[["n"]]
      if (!is.null(cov_val) && !all(is.na(cov_val))) result$covariance <- as.oq_formula(cov_val)
      if (!is.null(n_val) && !all(is.na(n_val))) result$n <- as.numeric(n_val)
      result
    }

    if (is.data.frame(model$multivariate_sampling)) {
      mv_list <- list()
      for (i in 1:nrow(model$multivariate_sampling)) {
        mv_list[[i]] <- suppressWarnings(parse_mv_spec(as.list(model$multivariate_sampling[i, ])))
      }
      model$multivariate_sampling <- mv_list
    } else if (is.list(model$multivariate_sampling) && !is.data.frame(model$multivariate_sampling)) {
      model$multivariate_sampling <- lapply(model$multivariate_sampling, parse_mv_spec)
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
        display_name = if (!is.null(p$display_name) && !is.na(p$display_name) && p$display_name != "") p$display_name else NULL,
        range_label = if (!is.null(p$range_label) && !is.na(p$range_label) && p$range_label != "") p$range_label else NULL
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
        # Unwrap list-wrapped data.frame (fromJSON with simplifyVector=TRUE on
        # data.frame scenarios wraps nested arrays in a length-1 list)
        vo <- s$variable_overrides
        if (is.list(vo) && !is.data.frame(vo) && length(vo) == 1 && is.data.frame(vo[[1]])) {
          vo <- vo[[1]]
        }

        override_items <- if (is.data.frame(vo)) {
          lapply(seq_len(nrow(vo)), function(j) {
            as.list(vo[j, ])
          })
        } else {
          vo
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
        # Unwrap list-wrapped data.frame (fromJSON with simplifyVector=TRUE on
        # data.frame scenarios wraps nested arrays in a length-1 list)
        so <- s$setting_overrides
        if (is.list(so) && !is.data.frame(so) && length(so) == 1 && is.data.frame(so[[1]])) {
          so <- so[[1]]
        }

        override_items <- if (is.data.frame(so)) {
          lapply(seq_len(nrow(so)), function(j) {
            as.list(so[j, ])
          })
        } else {
          so
        }

        for (j in seq_along(override_items)) {
          st <- override_items[[j]]
          # Skip empty entries (can occur with simplifyVector = TRUE on empty arrays)
          if (is.null(st$name) || length(st) == 0) next
          # Handle case where st is still a data.frame (multi-row)
          if (is.data.frame(st)) {
            for (k in seq_len(nrow(st))) {
              setting_overrides[[length(setting_overrides) + 1]] <- list(
                name = st$name[k], value = st$value[k]
              )
            }
          } else {
            setting_overrides[[length(setting_overrides) + 1]] <- list(name = st$name, value = st$value)
          }
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
            display_name = if (!is.null(p$display_name) && !is.na(p$display_name[1]) && p$display_name[1] != "") p$display_name[1] else NULL,
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

  # Parse threshold analyses from JSON (nested format)
  if (!is.null(model$threshold_analyses) && is.list(model$threshold_analyses)) {
    threshold_list <- list()
    # Handle both list and data.frame formats
    threshold_items <- if (is.data.frame(model$threshold_analyses)) {
      lapply(seq_len(nrow(model$threshold_analyses)), function(i) {
        as.list(model$threshold_analyses[i, ])
      })
    } else {
      model$threshold_analyses
    }

    for (i in seq_along(threshold_items)) {
      t <- threshold_items[[i]]

      # Handle condition field: may be a data.frame (fromJSON), list, or nested
      cond_raw <- t$condition
      condition <- list()
      if (is.data.frame(cond_raw)) {
        # fromJSON simplifies the condition object into a 1-row data.frame
        condition <- as.list(cond_raw[1, ])
        # Clean up NA and NULL values
        condition <- lapply(condition, function(v) {
          if (length(v) == 1 && is.na(v)) NULL else v
        })
        condition <- condition[!sapply(condition, is.null)]
      } else if (is.list(cond_raw) && length(cond_raw) == 1 && is.data.frame(cond_raw[[1]])) {
        condition <- as.list(cond_raw[[1]][1, ])
        condition <- lapply(condition, function(v) {
          if (length(v) == 1 && is.na(v)) NULL else v
        })
        condition <- condition[!sapply(condition, is.null)]
      } else if (is.list(cond_raw)) {
        condition <- cond_raw
      }

      threshold_list[[i]] <- list(
        name = t$name,
        variable = t$variable,
        variable_strategy = (t$variable_strategy %||% "")[1],
        variable_group = (t$variable_group %||% "")[1],
        lower = as.numeric(t$lower[1]),
        upper = as.numeric(t$upper[1]),
        active = if (!is.null(t$active)) as.logical(t$active[1]) else TRUE,
        condition = condition
      )
    }
    model$threshold_analyses <- threshold_list
  } else {
    model$threshold_analyses <- list()
  }

  # Parse VBP configuration from JSON
  if (!is.null(model$vbp)) {
    # fromJSON may simplify to a data.frame; convert back to list
    if (is.data.frame(model$vbp)) {
      model$vbp <- as.list(model$vbp[1, ])
    }
    # Ensure it has the expected fields
    expected_fields <- c("price_variable", "intervention_strategy",
                         "outcome_summary", "cost_summary")
    if (!all(expected_fields %in% names(model$vbp))) {
      model$vbp <- NULL
    }
  }

  # Parse PSA configuration from JSON
  if (!is.null(model$psa)) {
    if (is.data.frame(model$psa)) {
      model$psa <- as.list(model$psa[1, ])
    }
    if (!is.null(model$psa$n_sim)) {
      model$psa$n_sim <- as.integer(model$psa$n_sim)
      if (is.null(model$psa$seed) || (length(model$psa$seed) == 1 && is.na(model$psa$seed))) {
        model$psa$seed <- NULL
      }
    } else {
      model$psa <- NULL
    }
  }

  # Parse documentation from JSON
  if (!is.null(model$documentation)) {
    if (is.character(model$documentation) && length(model$documentation) == 1) {
      # Already valid
    } else {
      model$documentation <- NULL
    }
  }

  # Parse decision_tree configuration from JSON
  if (!is.null(model$decision_tree)) {
    if (is.data.frame(model$decision_tree)) {
      model$decision_tree <- as.list(model$decision_tree[1, ])
    }
    # Ensure it has the expected fields
    if (!all(c("tree_name", "duration", "duration_unit") %in% names(model$decision_tree))) {
      model$decision_tree <- NULL
    }
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
  model <- normalize_and_validate_model(model)

  return(model)
}

#' Convert JSON data frames
#' 
#' Ensures that data frames in the model are properly formatted
#' 
#' @param model The model list parsed from JSON
#' @return The model with properly formatted data frames
convert_json_dataframes <- function(model) {

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
  # Settings use literal values - ensure numeric conversion after JSON round-trip
  if (type == "setting") {
    if (is.character(x)) {
      num <- suppressWarnings(as.numeric(x))
      if (!is.na(num)) return(num)
    }
    return(x)
  }
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

        mv_obj <- list(
          name = mv_spec[["name"]],
          type = mv_spec[["type"]],
          strategy = mv_spec[["strategy"]] %||% "",
          group = mv_spec[["group"]] %||% "",
          description = mv_spec[["description"]] %||% "",
          variables = as.list(mv_spec[["variables"]])
        )

        if (!is.null(mv_spec[["covariance"]])) mv_obj$covariance <- as.character(mv_spec[["covariance"]])
        if (!is.null(mv_spec[["n"]])) mv_obj$n <- mv_spec[["n"]]

        mv_array[[i]] <- mv_obj
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
      dsa_entry$range_label <- param$range_label
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

  # Convert threshold analyses to array format (nested, as-is)
  if (!is.null(model$threshold_analyses) && length(model$threshold_analyses) > 0) {
    json_model$threshold_analyses <- model$threshold_analyses
  }

  # VBP configuration
  if (!is.null(model$vbp)) {
    json_model$vbp <- model$vbp
  }

  # PSA configuration
  if (!is.null(model$psa)) {
    json_model$psa <- model$psa
  }

  # Decision tree configuration
  if (!is.null(model$decision_tree)) {
    json_model$decision_tree <- model$decision_tree
  }

  # Documentation
  if (!is.null(model$documentation)) {
    json_model$documentation <- model$documentation
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

  # Merge renv lockfile fields at top level
  if (!is.null(model$lockfile)) {
    json_model$R <- model$lockfile$R
    json_model$Packages <- model$lockfile$Packages
    if (!is.null(model$lockfile$Bioconductor)) {
      json_model$Bioconductor <- model$lockfile$Bioconductor
    }
    if (!is.null(model$lockfile$Python)) {
      json_model$Python <- model$lockfile$Python
    }
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

    # Apply to model settings (discount_rate is an alias for both discount fields)
    if (setting_name == "discount_rate") {
      modified_model$settings$discount_cost <- override_value
      modified_model$settings$discount_outcomes <- override_value
    } else {
      modified_model$settings[[setting_name]] <- override_value
    }

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
    # Evaluate oq_formula objects in the namespace so formula-type overrides
    # resolve to concrete values before being assigned to the environment.
    if (inherits(val, "oq_formula")) {
      val <- rlang::eval_tidy(val$quo, env = ns$env)
    }
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