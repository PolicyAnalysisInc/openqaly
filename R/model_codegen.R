#' Model Code Generation
#'
#' Functions for generating executable R code from openqaly models.
#'
#' @name model_codegen
#' @importFrom glue glue
#' @importFrom purrr map_chr
NULL

#' Convert Model to R Code
#'
#' Generate executable R code that recreates a openqaly model.
#'
#' @param model A oq_model object
#' @param file Optional file path to write the code to
#'
#' @return A character vector containing the R code lines
#'
#' @export
#' @examples
#' \dontrun{
#' # Generate R code as string
#' code <- model_to_r_code(model)
#'
#' # Write R code to file
#' model_to_r_code(model, "my_model.R")
#' }
model_to_r_code <- function(model, file = NULL) {
  code <- character()

  # Header
  code <- c(code,
    "# Generated openqaly model code",
    "# Created: " %&% Sys.Date(),
    "",
    "library(openqaly)",
    ""
  )

  # Start model definition
  model_type <- model$settings$model_type %||% "markov"
  code <- c(code,
    glue('model <- define_model("{model_type}") |>')
  )

  # Add settings
  if (!is.null(model$settings) && length(model$settings) > 0) {
    code <- c(code, generate_settings_code(model$settings))
  }

  # Add strategies
  if (!is.null(model$strategies) && nrow(model$strategies) > 0) {
    code <- c(code, generate_strategies_code(model$strategies))
  }

  # Add groups
  if (!is.null(model$groups) && nrow(model$groups) > 0) {
    code <- c(code, generate_groups_code(model$groups))
  }

  # Determine model type once for use by multiple generators
  is_psm <- tolower(model_type) == "psm"

  # Add states
  if (!is.null(model$states) && nrow(model$states) > 0) {
    code <- c(code, generate_states_code(model$states, is_psm))
  }

  # Add variables
  if (!is.null(model$variables) && nrow(model$variables) > 0) {
    code <- c(code, generate_variables_code(model$variables))
  }

  # Add transitions
  if (!is.null(model$transitions) && nrow(model$transitions) > 0) {
    code <- c(code, generate_transitions_code(model$transitions, is_psm))
  }

  # Add values
  if (!is.null(model$values) && nrow(model$values) > 0) {
    code <- c(code, generate_values_code(model$values))
  }

  # Add summaries
  if (!is.null(model$summaries) && nrow(model$summaries) > 0) {
    code <- c(code, generate_summaries_code(model$summaries))
  }

  # Add multivariate sampling (before removing trailing pipe)
  if (!is.null(model$multivariate_sampling) && length(model$multivariate_sampling) > 0) {
    mv_code <- generate_multivariate_sampling_code(model$multivariate_sampling)
    if (length(mv_code) > 0) {
      code <- c(code, mv_code)
    }
  }

  # Remove trailing pipe from last line
  last_line <- length(code)
  if (grepl("\\|>$", code[last_line])) {
    code[last_line] <- gsub(" \\|>$", "", code[last_line])
  }

  # Add tables and scripts (separate from pipe chain)
  if (!is.null(model$tables) && length(model$tables) > 0) {
    code <- c(code, "", generate_tables_code(model, model$tables))
  }

  if (!is.null(model$scripts) && length(model$scripts) > 0) {
    code <- c(code, "", generate_scripts_code(model, model$scripts))
  }

  # Add decision trees (if any)
  if (!is.null(model$trees) && length(model$trees) > 0) {
    tree_code <- generate_trees_code(model, model$trees)
    if (length(tree_code) > 0) {
      code <- c(code, "", tree_code)
    }
  }

  # Add DSA parameters (if any)
  if (!is.null(model$dsa_parameters) && length(model$dsa_parameters) > 0) {
    dsa_code <- generate_dsa_code(model$dsa_parameters)
    if (length(dsa_code) > 0) {
      code <- c(code, "", dsa_code)
    }
  }

  # Add scenarios (if any)
  if (!is.null(model$scenarios) && length(model$scenarios) > 0) {
    scenarios_code <- generate_scenarios_code(model$scenarios)
    if (length(scenarios_code) > 0) {
      code <- c(code, "", scenarios_code)
    }
  }

  # Add TWSA analyses (if any)
  if (!is.null(model$twsa_analyses) && length(model$twsa_analyses) > 0) {
    twsa_code <- generate_twsa_code(model$twsa_analyses)
    if (length(twsa_code) > 0) {
      code <- c(code, "", twsa_code)
    }
  }

  # Add threshold analyses (if any)
  if (!is.null(model$threshold_analyses) && length(model$threshold_analyses) > 0) {
    threshold_code <- generate_threshold_code(model$threshold_analyses)
    if (length(threshold_code) > 0) {
      code <- c(code, "", threshold_code)
    }
  }

  # Add override categories (if any)
  if (!is.null(model$override_categories) && length(model$override_categories) > 0) {
    override_code <- generate_override_code(model$override_categories)
    if (length(override_code) > 0) {
      code <- c(code, "", override_code)
    }
  }

  # Add VBP configuration (if any)
  if (!is.null(model$vbp)) {
    vbp_code <- generate_vbp_code(model$vbp)
    if (length(vbp_code) > 0) {
      code <- c(code, "", vbp_code)
    }
  }

  # Write to file if specified
  if (!is.null(file)) {
    writeLines(code, file)
  }

  invisible(code)
}

#' Generate Settings Code
#' @keywords internal
generate_settings_code <- function(settings) {
  # Defensive check
  if (is.null(settings) || length(settings) == 0) return(character(0))

  # Filter out model_type as it's already in define_model
  settings <- settings[names(settings) != "model_type"]

  if (length(settings) == 0) return(character(0))

  args <- character()
  for (name in names(settings)) {
    val <- settings[[name]]
    # Quote strings, leave numbers unquoted
    if (suppressWarnings(is.na(as.numeric(val)))) {
      # It's a string
      if (tolower(val) %in% c("true", "false")) {
        # Boolean value
        args <- c(args, glue('    {name} = {toupper(val)}'))
      } else {
        # Regular string
        args <- c(args, glue('    {name} = "{val}"'))
      }
    } else {
      # It's a number
      args <- c(args, glue('    {name} = {val}'))
    }
  }

  c(
    "  set_settings(",
    paste0(args, collapse = ",\n"),
    "  ) |>"
  )
}

#' Generate States Code
#' @keywords internal
generate_states_code <- function(states, is_psm = FALSE) {
  # Defensive check
  if (is.null(states) || !is.data.frame(states) || nrow(states) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(states))) {
    s <- states[i, ]

    # PSM states only have name, display_name, description
    # Markov states also have initial_probability and other fields
    if (is_psm) {
      args <- glue('"{s$name}"')
    } else {
      args <- glue('"{s$name}", initial_prob = {s$initial_probability}')
    }

    # Add optional arguments (display_name and description apply to both model types)
    if ("display_name" %in% names(s) && !is.na(s$display_name) && s$display_name != s$name) {
      args <- args %&% glue(', display_name = "{s$display_name}"')
    }
    if ("description" %in% names(s) && !is.na(s$description) &&
        s$description != s$name &&
        (!("display_name" %in% names(s)) || s$description != s$display_name)) {
      args <- args %&% glue(', description = "{s$description}"')
    }

    # Markov-only optional arguments
    if (!is_psm) {
      if ("state_group" %in% names(s) && !is.na(s$state_group)) {
        args <- args %&% glue(', state_group = "{s$state_group}"')
      }
      if ("share_state_time" %in% names(s) && !is.na(s$share_state_time) && s$share_state_time) {
        args <- args %&% glue(', share_state_time = TRUE')
      }
      if ("state_cycle_limit" %in% names(s) && !is.na(s$state_cycle_limit)) {
        args <- args %&% glue(', state_cycle_limit = {s$state_cycle_limit}')
      }
      if ("state_cycle_limit_unit" %in% names(s) && !is.na(s$state_cycle_limit_unit) && s$state_cycle_limit_unit != "cycles") {
        args <- args %&% glue(', state_cycle_limit_unit = "{s$state_cycle_limit_unit}"')
      }
    }

    code <- c(code, glue('  add_state({args}) |>'))
  }

  code
}

#' Generate Transitions Code
#' @keywords internal
generate_transitions_code <- function(transitions, is_psm = FALSE) {
  # Defensive check
  if (is.null(transitions) || !is.data.frame(transitions) || nrow(transitions) == 0) {
    return(character(0))
  }

  code <- character()

  if (is_psm) {
    for (i in seq_len(nrow(transitions))) {
      t <- transitions[i, ]
      code <- c(code,
        glue('  add_psm_transition("{t$endpoint}", "{t$time_unit}", {t$formula}) |>')
      )
    }
  } else {
    for (i in seq_len(nrow(transitions))) {
      t <- transitions[i, ]
      # For transitions, formula should be unquoted expression
      code <- c(code,
        glue('  add_transition("{t$from_state}", "{t$to_state}", {t$formula}) |>')
      )
    }
  }

  code
}

#' Generate Values Code
#' @keywords internal
generate_values_code <- function(values) {
  # Defensive check
  if (is.null(values) || !is.data.frame(values) || nrow(values) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(values))) {
    v <- values[i, ]
    # Formula should be unquoted expression
    args <- glue('"{v$name}", {v$formula}')

    # Add optional arguments
    if ("state" %in% names(v) && !is.na(v$state) && v$state != "") {
      args <- args %&% glue(', state = "{v$state}"')
    }
    if ("destination" %in% names(v) && !is.na(v$destination) && v$destination != "") {
      args <- args %&% glue(', destination = "{v$destination}"')
    }
    if ("display_name" %in% names(v) && !is.na(v$display_name) && v$display_name != v$name) {
      args <- args %&% glue(', display_name = "{v$display_name}"')
    }
    if ("description" %in% names(v) && !is.na(v$description) &&
        v$description != v$name &&
        (!("display_name" %in% names(v)) || v$description != v$display_name)) {
      args <- args %&% glue(', description = "{v$description}"')
    }
    if ("type" %in% names(v) && !is.na(v$type) && v$type != "outcome") {
      args <- args %&% glue(', type = "{v$type}"')
    }

    code <- c(code, glue('  add_value({args}) |>'))
  }

  code
}

#' Generate Variables Code
#' @keywords internal
generate_variables_code <- function(variables) {
  # Defensive check
  if (is.null(variables) || !is.data.frame(variables) || nrow(variables) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(variables))) {
    v <- variables[i, ]
    # Formula should be unquoted expression
    args <- glue('"{v$name}", {v$formula}')

    # Add optional arguments
    if ("display_name" %in% names(v) && !is.na(v$display_name) && v$display_name != v$name) {
      args <- args %&% glue(', display_name = "{v$display_name}"')
    }
    if ("description" %in% names(v) && !is.na(v$description) &&
        v$description != v$name &&
        (!("display_name" %in% names(v)) || v$description != v$display_name)) {
      args <- args %&% glue(', description = "{v$description}"')
    }
    if ("strategy" %in% names(v) && !is.na(v$strategy) && v$strategy != "") {
      args <- args %&% glue(', strategy = "{v$strategy}"')
    }
    if ("group" %in% names(v) && !is.na(v$group) && v$group != "") {
      args <- args %&% glue(', group = "{v$group}"')
    }
    if ("source" %in% names(v) && !is.na(v$source) && v$source != "") {
      # Escape quotes in source
      source_escaped <- gsub('"', '\\"', v$source)
      args <- args %&% glue(', source = "{source_escaped}"')
    }
    if ("sampling" %in% names(v) && !is.na(v$sampling) && v$sampling != "") {
      # Output sampling as unquoted expression (NSE)
      args <- args %&% glue(', sampling = {v$sampling}')
    }

    code <- c(code, glue('  add_variable({args}) |>'))
  }

  code
}

#' Generate Strategies Code
#' @keywords internal
generate_strategies_code <- function(strategies) {
  # Defensive check
  if (is.null(strategies) || !is.data.frame(strategies) || nrow(strategies) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(strategies))) {
    s <- strategies[i, ]
    args <- glue('"{s$name}"')

    # Add optional arguments
    if ("display_name" %in% names(s) && !is.na(s$display_name) && s$display_name != s$name) {
      args <- args %&% glue(', display_name = "{s$display_name}"')
    }
    if ("description" %in% names(s) && !is.na(s$description) &&
        s$description != s$name &&
        (!("display_name" %in% names(s)) || s$description != s$display_name)) {
      args <- args %&% glue(', description = "{s$description}"')
    }
    if ("enabled" %in% names(s) && !is.na(s$enabled) && s$enabled != 1) {
      args <- args %&% glue(', enabled = {s$enabled}')
    }

    code <- c(code, glue('  add_strategy({args}) |>'))
  }

  code
}

#' Generate Groups Code
#' @keywords internal
generate_groups_code <- function(groups) {
  # Defensive check
  if (is.null(groups) || !is.data.frame(groups) || nrow(groups) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(groups))) {
    g <- groups[i, ]
    args <- glue('"{g$name}"')

    # Add optional arguments
    if ("display_name" %in% names(g) && !is.na(g$display_name) && g$display_name != g$name) {
      args <- args %&% glue(', display_name = "{g$display_name}"')
    }
    if ("description" %in% names(g) && !is.na(g$description) &&
        g$description != g$name &&
        (!("display_name" %in% names(g)) || g$description != g$display_name)) {
      args <- args %&% glue(', description = "{g$description}"')
    }
    if ("weight" %in% names(g) && !is.na(g$weight) && g$weight != "1") {
      # Check if weight is a number or expression
      if (suppressWarnings(!is.na(as.numeric(g$weight)))) {
        args <- args %&% glue(', weight = {g$weight}')
      } else {
        args <- args %&% glue(', weight = "{g$weight}"')
      }
    }
    if ("enabled" %in% names(g) && !is.na(g$enabled) && g$enabled != 1) {
      args <- args %&% glue(', enabled = {g$enabled}')
    }

    code <- c(code, glue('  add_group({args}) |>'))
  }

  code
}

#' Generate Summaries Code
#' @keywords internal
generate_summaries_code <- function(summaries) {
  # Defensive check
  if (is.null(summaries) || !is.data.frame(summaries) || nrow(summaries) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(summaries))) {
    s <- summaries[i, ]
    args <- glue('"{s$name}", "{s$values}"')

    # Add optional arguments
    if ("display_name" %in% names(s) && !is.na(s$display_name) && s$display_name != s$name) {
      args <- args %&% glue(', display_name = "{s$display_name}"')
    }
    if ("description" %in% names(s) && !is.na(s$description) &&
        s$description != s$name &&
        (!("display_name" %in% names(s)) || s$description != s$display_name)) {
      args <- args %&% glue(', description = "{s$description}"')
    }

    code <- c(code, glue('  add_summary({args}) |>'))
  }

  code
}

#' Generate Multivariate Sampling Code
#' @keywords internal
generate_multivariate_sampling_code <- function(multivariate_sampling) {
  # Defensive check
  if (is.null(multivariate_sampling) || length(multivariate_sampling) == 0) {
    return(character(0))
  }

  code <- character()

  for (mv_spec in multivariate_sampling) {
    # Build the function call arguments
    args <- character()
    args <- c(args, glue('name = "{mv_spec$name}"'))
    args <- c(args, glue('distribution = {mv_spec$distribution}'))

    # Handle variables - can be a character vector or a tibble
    if ("variables" %in% names(mv_spec) && !is.null(mv_spec$variables)) {
      vars_df <- mv_spec$variables

      if (is.data.frame(vars_df)) {
        # Check if it's a simple case (just variable names, no strategy/group)
        if (all(is.na(vars_df$strategy) | vars_df$strategy == "") &&
            all(is.na(vars_df$group) | vars_df$group == "")) {
          # Simple case - just variable names
          var_names <- paste0('"', vars_df$variable, '"', collapse = ", ")
          args <- c(args, glue('variables = c({var_names})'))
        } else {
          # Complex case - need to generate tibble
          tibble_lines <- format_tribble(vars_df)
          if (length(tibble_lines) == 1) {
            # Single line tibble
            args <- c(args, paste0('variables = ', tibble_lines[1]))
          } else {
            # Multi-line tibble - need special handling
            # We'll use a simplified approach for now
            tibble_str <- paste(tibble_lines, collapse = "\n    ")
            args <- c(args, paste0('variables = ', tibble_str))
          }
        }
      } else if (is.character(vars_df)) {
        # Character vector case
        var_names <- paste0('"', vars_df, '"', collapse = ", ")
        args <- c(args, glue('variables = c({var_names})'))
      }
    }

    # Add description if present
    if ("description" %in% names(mv_spec) && !is.na(mv_spec$description) &&
        mv_spec$description != "") {
      # Escape quotes in description
      desc_escaped <- gsub('"', '\\"', mv_spec$description)
      args <- c(args, glue('description = "{desc_escaped}"'))
    }

    # Build the function call
    if (length(args) == 1) {
      code <- c(code, glue('  add_multivariate_sampling({args}) |>'))
    } else {
      code <- c(code,
        '  add_multivariate_sampling(',
        paste0('    ', args, collapse = ',\n'),
        '  ) |>'
      )
    }
  }

  code
}

#' Format data.frame as tribble code
#' @keywords internal
format_tribble <- function(df) {
  if (nrow(df) == 0) {
    return("tibble::tibble()")
  }

  # Get column names and prepare them
  col_names <- names(df)
  formatted_names <- vapply(col_names, function(name) {
    # Use backticks for names with spaces or special characters
    if (grepl("[^a-zA-Z0-9_.]", name) || grepl("^[0-9]", name)) {
      paste0("`", name, "`")
    } else {
      name
    }
  }, character(1))

  # Prepare values for each column
  formatted_values <- list()
  col_widths <- numeric(length(col_names))

  for (i in seq_along(col_names)) {
    col <- df[[col_names[i]]]

    # Format values based on type
    if (is.character(col)) {
      vals <- paste0('"', col, '"')
    } else if (is.logical(col)) {
      vals <- toupper(as.character(col))
    } else {
      vals <- as.character(col)
    }

    formatted_values[[i]] <- vals

    # Calculate column width (including ~ for header)
    header_width <- nchar(paste0("~", formatted_names[i]))
    max_val_width <- max(nchar(vals))
    col_widths[i] <- max(header_width, max_val_width)
  }

  # Build the tribble code
  lines <- character()

  # Add the opening
  lines <- c(lines, "tibble::tribble(")

  # Add the header row
  header_parts <- character(length(col_names))
  for (i in seq_along(col_names)) {
    header_parts[i] <- sprintf("%-*s", col_widths[i], paste0("~", formatted_names[i]))
  }
  lines <- c(lines, paste0("  ", paste(header_parts, collapse = ", "), ","))

  # Add data rows
  for (row_idx in seq_len(nrow(df))) {
    row_parts <- character(length(col_names))
    for (col_idx in seq_along(col_names)) {
      row_parts[col_idx] <- sprintf("%-*s", col_widths[col_idx], formatted_values[[col_idx]][row_idx])
    }
    row_line <- paste0("  ", paste(row_parts, collapse = ", "))

    # Add comma except for last row
    if (row_idx < nrow(df)) {
      row_line <- paste0(row_line, ",")
    }

    lines <- c(lines, row_line)
  }

  # Add the closing
  lines <- c(lines, ")")

  return(lines)
}

#' Generate Tables Code
#' @keywords internal
generate_tables_code <- function(model, tables) {
  # Defensive check
  if (is.null(tables) || length(tables) == 0) {
    return(character(0))
  }

  code <- c("# Add tables")

  for (table_name in names(tables)) {
    table_entry <- tables[[table_name]]

    # Handle both old format (direct data frame) and new format (list with data + description)
    if (is.data.frame(table_entry)) {
      table_data <- table_entry
      table_description <- NULL
    } else if (is.list(table_entry) && "data" %in% names(table_entry)) {
      table_data <- table_entry$data
      table_description <- table_entry$description
    } else {
      table_data <- table_entry
      table_description <- NULL
    }

    tribble_code <- format_tribble(table_data)

    code <- c(code,
      "",
      paste0('# Table: ', table_name),
      paste0(table_name, '_data <- ', tribble_code[1])
    )

    # Add the rest of the tribble lines (if multi-line)
    if (length(tribble_code) > 1) {
      code <- c(code, tribble_code[-1])
    }

    # Build add_table call with optional description
    if (!is.null(table_description) && table_description != "") {
      desc_escaped <- gsub('"', '\\"', table_description)
      code <- c(code,
        paste0('model <- add_table(model, "', table_name, '", ', table_name, '_data, description = "', desc_escaped, '")')
      )
    } else {
      code <- c(code,
        paste0('model <- add_table(model, "', table_name, '", ', table_name, '_data)')
      )
    }
  }

  code
}

#' Generate Scripts Code
#' @keywords internal
generate_scripts_code <- function(model, scripts) {
  # Defensive check
  if (is.null(scripts) || length(scripts) == 0) {
    return(character(0))
  }

  code <- c("# Add scripts")

  for (script_name in names(scripts)) {
    script_entry <- scripts[[script_name]]

    # Handle both old format (direct string) and new format (list with code + description)
    if (is.character(script_entry)) {
      script_content <- script_entry
      script_description <- NULL
    } else if (is.list(script_entry) && "code" %in% names(script_entry)) {
      script_content <- script_entry$code
      script_description <- script_entry$description
    } else {
      script_content <- as.character(script_entry)
      script_description <- NULL
    }

    # Escape quotes and backslashes in the script content
    script_escaped <- gsub("\\\\", "\\\\\\\\", script_content)  # Escape backslashes first
    script_escaped <- gsub('"', '\\\\"', script_escaped)        # Escape double quotes

    code <- c(code,
      "",
      paste0('# Script: ', script_name),
      paste0(script_name, '_code <- "', script_escaped, '"')
    )

    # Build add_script call with optional description
    if (!is.null(script_description) && script_description != "") {
      desc_escaped <- gsub('"', '\\"', script_description)
      code <- c(code,
        paste0('model <- add_script(model, "', script_name, '", ', script_name, '_code, description = "', desc_escaped, '")')
      )
    } else {
      code <- c(code,
        paste0('model <- add_script(model, "', script_name, '", ', script_name, '_code)')
      )
    }
  }

  code
}

#' Generate Trees Code
#' @keywords internal
generate_trees_code <- function(model, trees) {
  # Defensive check for NULL, empty, or malformed trees
  if (is.null(trees)) {
    return(character(0))
  }

  # Trees can be either a data frame or a list
  if (is.data.frame(trees)) {
    if (nrow(trees) == 0) {
      return(character(0))
    }
  } else if (is.list(trees)) {
    if (length(trees) == 0) {
      return(character(0))
    }
  } else {
    # Unknown tree format, return empty
    return(character(0))
  }

  # Trees in openqaly are typically parsed into variables via decision_tree()
  # rather than being added directly. The tree parsing happens automatically
  # when the model is read from Excel/JSON. For now, we'll just add a comment
  # indicating that trees are present but handled internally.

  code <- c(
    "# Decision trees are parsed into variables automatically",
    "# Tree data is embedded in the model structure"
  )

  return(code)
}

#' Generate DSA Code
#' @keywords internal
generate_dsa_code <- function(dsa_parameters) {
  # Defensive check
  if (is.null(dsa_parameters) || length(dsa_parameters) == 0) {
    return(character(0))
  }

  code <- c("# Add DSA parameters")

  for (param in dsa_parameters) {
    if (param$type == "variable") {
      # Generate add_dsa_variable call
      args <- glue('"{param$name}"')

      # Add low and high (convert oq_formula to string)
      low_str <- if (inherits(param$low, "oq_formula")) as.character(param$low) else param$low
      high_str <- if (inherits(param$high, "oq_formula")) as.character(param$high) else param$high
      args <- args %&% glue(', low = {low_str}, high = {high_str}')

      # Add optional strategy/group
      if (!is.null(param$strategy) && param$strategy != "") {
        args <- args %&% glue(', strategy = "{param$strategy}"')
      }
      if (!is.null(param$group) && param$group != "") {
        args <- args %&% glue(', group = "{param$group}"')
      }
      # Add optional display_name
      if (!is.null(param$display_name) && param$display_name != "") {
        args <- args %&% glue(', display_name = "{param$display_name}"')
      }

      code <- c(code, glue('model <- add_dsa_variable(model, {args})'))

    } else if (param$type == "setting") {
      # Generate add_dsa_setting call
      args <- glue('"{param$name}"')

      # Add low and high (literal values for settings)
      args <- args %&% glue(', low = {param$low}, high = {param$high}')

      # Add optional display_name
      if (!is.null(param$display_name) && param$display_name != "" && param$display_name != param$name) {
        args <- args %&% glue(', display_name = "{param$display_name}"')
      }

      code <- c(code, glue('model <- add_dsa_setting(model, {args})'))
    }
  }

  code
}

#' Generate Scenarios Code
#' @keywords internal
generate_scenarios_code <- function(scenarios) {
  # Defensive check
  if (is.null(scenarios) || length(scenarios) == 0) {
    return(character(0))
  }

  code <- c("# Add scenarios")

  for (scenario in scenarios) {
    # Generate add_scenario call
    args <- glue('"{scenario$name}"')
    if (!is.null(scenario$description) && scenario$description != "" && scenario$description != scenario$name) {
      desc_escaped <- gsub('"', '\\"', scenario$description)
      args <- args %&% glue(', description = "{desc_escaped}"')
    }
    code <- c(code, glue('model <- add_scenario(model, {args})'))

    # Generate add_scenario_variable calls
    if (!is.null(scenario$variable_overrides) && length(scenario$variable_overrides) > 0) {
      for (override in scenario$variable_overrides) {
        var_args <- glue('"{scenario$name}", "{override$name}"')

        # Value can be numeric or oq_formula
        val_str <- if (inherits(override$value, "oq_formula")) {
          as.character(override$value)
        } else {
          override$value
        }
        var_args <- var_args %&% glue(', {val_str}')

        # Add optional strategy/group
        if (!is.null(override$strategy) && override$strategy != "") {
          var_args <- var_args %&% glue(', strategy = "{override$strategy}"')
        }
        if (!is.null(override$group) && override$group != "") {
          var_args <- var_args %&% glue(', group = "{override$group}"')
        }

        code <- c(code, glue('model <- add_scenario_variable(model, {var_args})'))
      }
    }

    # Generate add_scenario_setting calls
    if (!is.null(scenario$setting_overrides) && length(scenario$setting_overrides) > 0) {
      for (override in scenario$setting_overrides) {
        setting_args <- glue('"{scenario$name}", "{override$name}", {override$value}')
        code <- c(code, glue('model <- add_scenario_setting(model, {setting_args})'))
      }
    }
  }

  code
}

#' Generate TWSA Code
#' @keywords internal
generate_twsa_code <- function(twsa_analyses) {
  # Defensive check
  if (is.null(twsa_analyses) || length(twsa_analyses) == 0) {
    return(character(0))
  }

  code <- c("# Add TWSA analyses")

  for (twsa in twsa_analyses) {
    # Generate add_twsa call
    args <- glue('"{twsa$name}"')
    if (!is.null(twsa$description) && twsa$description != "" && twsa$description != twsa$name) {
      desc_escaped <- gsub('"', '\\"', twsa$description)
      args <- args %&% glue(', description = "{desc_escaped}"')
    }
    code <- c(code, glue('model <- add_twsa(model, {args})'))

    # Generate parameter calls
    if (!is.null(twsa$parameters) && length(twsa$parameters) > 0) {
      for (param in twsa$parameters) {
        if (param$param_type == "variable") {
          # Generate add_twsa_variable call
          param_args <- glue('"{twsa$name}", "{param$name}", type = "{param$type}"')

          if (param$type == "range") {
            min_str <- if (inherits(param$min, "oq_formula")) as.character(param$min) else param$min
            max_str <- if (inherits(param$max, "oq_formula")) as.character(param$max) else param$max
            param_args <- param_args %&% glue(', min = {min_str}, max = {max_str}, steps = {param$steps}')
          } else if (param$type == "radius") {
            radius_str <- if (inherits(param$radius, "oq_formula")) as.character(param$radius) else param$radius
            param_args <- param_args %&% glue(', radius = {radius_str}, steps = {param$steps}')
          } else if (param$type == "custom") {
            values_str <- if (inherits(param$values, "oq_formula")) as.character(param$values) else {
              paste0("c(", paste(param$values, collapse = ", "), ")")
            }
            param_args <- param_args %&% glue(', values = {values_str}')
          }

          # Add optional strategy/group
          if (!is.null(param$strategy) && param$strategy != "") {
            param_args <- param_args %&% glue(', strategy = "{param$strategy}"')
          }
          if (!is.null(param$group) && param$group != "") {
            param_args <- param_args %&% glue(', group = "{param$group}"')
          }
          # Add optional display_name
          if (!is.null(param$display_name) && param$display_name != "") {
            param_args <- param_args %&% glue(', display_name = "{param$display_name}"')
          }
          # Add include_base_case if FALSE
          if (!is.null(param$include_base_case) && param$include_base_case == FALSE) {
            param_args <- param_args %&% ', include_base_case = FALSE'
          }

          code <- c(code, glue('model <- add_twsa_variable(model, {param_args})'))

        } else if (param$param_type == "setting") {
          # Generate add_twsa_setting call
          param_args <- glue('"{twsa$name}", "{param$name}", type = "{param$type}"')

          if (param$type == "range") {
            param_args <- param_args %&% glue(', min = {param$min}, max = {param$max}, steps = {param$steps}')
          } else if (param$type == "radius") {
            param_args <- param_args %&% glue(', radius = {param$radius}, steps = {param$steps}')
          } else if (param$type == "custom") {
            values_str <- paste0("c(", paste(param$values, collapse = ", "), ")")
            param_args <- param_args %&% glue(', values = {values_str}')
          }

          # Add optional display_name
          if (!is.null(param$display_name) && param$display_name != "" && param$display_name != param$name) {
            param_args <- param_args %&% glue(', display_name = "{param$display_name}"')
          }
          # Add include_base_case if FALSE
          if (!is.null(param$include_base_case) && param$include_base_case == FALSE) {
            param_args <- param_args %&% ', include_base_case = FALSE'
          }

          code <- c(code, glue('model <- add_twsa_setting(model, {param_args})'))
        }
      }
    }
  }

  code
}

#' Generate VBP Code
#' @keywords internal
generate_vbp_code <- function(vbp) {
  if (is.null(vbp)) return(character(0))

  c(
    "# Set VBP configuration",
    glue('model <- set_vbp(model,'),
    glue('  price_variable = "{vbp$price_variable}",'),
    glue('  intervention_strategy = "{vbp$intervention_strategy}",'),
    glue('  outcome_summary = "{vbp$outcome_summary}",'),
    glue('  cost_summary = "{vbp$cost_summary}"'),
    ")"
  )
}

#' Generate Override Code
#' @keywords internal
generate_override_code <- function(override_categories) {
  if (is.null(override_categories) || length(override_categories) == 0) {
    return(character(0))
  }

  code <- c("# Add override categories and overrides")

  for (cat_item in override_categories) {
    # Generate add_override_category call
    cat_args <- glue('"{cat_item$name}"')
    if (isTRUE(cat_item$general)) {
      cat_args <- cat_args %&% ', general = TRUE'
    }
    code <- c(code, glue('model <- add_override_category(model, {cat_args})'))

    # Generate add_override calls for each override
    for (ovr in cat_item$overrides) {
      ovr_args <- glue('"{cat_item$name}"')
      ovr_args <- ovr_args %&% glue(',\n    title = "{ovr$title}"')
      ovr_args <- ovr_args %&% glue(',\n    name = "{ovr$name}"')
      ovr_args <- ovr_args %&% glue(',\n    type = "{ovr$type}"')
      ovr_args <- ovr_args %&% glue(',\n    input_type = "{ovr$input_type}"')

      # Expression - output as unquoted if numeric, quoted if string
      expr_val <- suppressWarnings(as.numeric(ovr$overridden_expression))
      if (!is.na(expr_val)) {
        ovr_args <- ovr_args %&% glue(',\n    expression = {ovr$overridden_expression}')
      } else {
        ovr_args <- ovr_args %&% glue(',\n    expression = "{ovr$overridden_expression}"')
      }

      # Optional description
      if (!is.null(ovr$description) && ovr$description != "") {
        desc_escaped <- gsub('"', '\\"', ovr$description)
        ovr_args <- ovr_args %&% glue(',\n    description = "{desc_escaped}"')
      }

      # Optional strategy/group
      if (!is.null(ovr$strategy) && ovr$strategy != "") {
        ovr_args <- ovr_args %&% glue(',\n    strategy = "{ovr$strategy}"')
      }
      if (!is.null(ovr$group) && ovr$group != "") {
        ovr_args <- ovr_args %&% glue(',\n    group = "{ovr$group}"')
      }

      # General flag
      if (isTRUE(ovr$general)) {
        ovr_args <- ovr_args %&% ',\n    general = TRUE'
      }

      # Input config params
      if (!is.null(ovr$input_config$min)) {
        ovr_args <- ovr_args %&% glue(',\n    min = {ovr$input_config$min}')
      }
      if (!is.null(ovr$input_config$max)) {
        ovr_args <- ovr_args %&% glue(',\n    max = {ovr$input_config$max}')
      }
      if (!is.null(ovr$input_config$step_size)) {
        ovr_args <- ovr_args %&% glue(',\n    step_size = {ovr$input_config$step_size}')
      }

      # Dropdown options
      if (ovr$input_type == "dropdown" && !is.null(ovr$input_config$options) &&
          length(ovr$input_config$options) > 0) {
        opts_code <- sapply(ovr$input_config$options, function(opt) {
          opt_args <- glue('"{opt$label}", "{opt$value}"')
          if (isTRUE(opt$is_base_case)) {
            opt_args <- opt_args %&% ', is_base_case = TRUE'
          }
          glue('      override_option({opt_args})')
        })
        ovr_args <- ovr_args %&% ',\n    options = list(\n' %&%
          paste(opts_code, collapse = ",\n") %&% '\n    )'
      }

      code <- c(code, glue('model <- add_override(model, {ovr_args}\n  )'))
    }
  }

  code
}

#' Generate Threshold Analysis Code
#' @keywords internal
generate_threshold_code <- function(threshold_analyses) {
  if (is.null(threshold_analyses) || length(threshold_analyses) == 0) {
    return(character(0))
  }

  code <- c("# Add threshold analyses")

  for (a in threshold_analyses) {
    # Generate condition constructor call
    cond <- a$condition
    cond_func <- switch(cond$output,
      "outcomes" = "threshold_condition_outcomes",
      "costs" = "threshold_condition_costs",
      "nmb" = "threshold_condition_nmb",
      "ce" = "threshold_condition_ce",
      "trace" = "threshold_condition_trace"
    )

    cond_args <- character()

    if (cond$output %in% c("outcomes", "costs")) {
      if (!is.null(cond$summary) && cond$summary != "") {
        cond_args <- c(cond_args, glue('summary = "{cond$summary}"'))
      }
      if (!is.null(cond$value) && cond$value != "") {
        cond_args <- c(cond_args, glue('value = "{cond$value}"'))
      }
      if (!is.null(cond$type)) {
        cond_args <- c(cond_args, glue('type = "{cond$type}"'))
      }
      if (!is.null(cond$strategy) && cond$strategy != "") {
        cond_args <- c(cond_args, glue('strategy = "{cond$strategy}"'))
      }
      if (!is.null(cond$referent) && cond$referent != "") {
        cond_args <- c(cond_args, glue('referent = "{cond$referent}"'))
      }
      if (!is.null(cond$comparator) && cond$comparator != "") {
        cond_args <- c(cond_args, glue('comparator = "{cond$comparator}"'))
      }
      if (!is.null(cond$discounted) && !isTRUE(cond$discounted)) {
        cond_args <- c(cond_args, "discounted = FALSE")
      }
      if (!is.null(cond$target_value) && cond$target_value != 0) {
        cond_args <- c(cond_args, glue('target_value = {cond$target_value}'))
      }
      if (!is.null(cond$group) && cond$group != "") {
        cond_args <- c(cond_args, glue('group = "{cond$group}"'))
      }
    } else if (cond$output %in% c("nmb", "ce")) {
      cond_args <- c(cond_args, glue('"{cond$health_summary}"'))
      cond_args <- c(cond_args, glue('"{cond$cost_summary}"'))
      cond_args <- c(cond_args, glue('"{cond$referent}"'))
      cond_args <- c(cond_args, glue('"{cond$comparator}"'))
      if (!is.null(cond$discounted) && !isTRUE(cond$discounted)) {
        cond_args <- c(cond_args, "discounted = FALSE")
      }
      if (cond$output == "nmb" && !is.null(cond$target_value) && cond$target_value != 0) {
        cond_args <- c(cond_args, glue('target_value = {cond$target_value}'))
      }
      if (!is.null(cond$group) && cond$group != "") {
        cond_args <- c(cond_args, glue('group = "{cond$group}"'))
      }
      if (!is.null(cond$wtp) && !is.na(cond$wtp)) {
        cond_args <- c(cond_args, glue('wtp = {cond$wtp}'))
      }
    } else if (cond$output == "trace") {
      cond_args <- c(cond_args, glue('state = "{cond$state}"'))
      cond_args <- c(cond_args, glue('time = {cond$time}'))
      if (!is.null(cond$time_unit) && cond$time_unit != "cycle") {
        cond_args <- c(cond_args, glue('time_unit = "{cond$time_unit}"'))
      }
      if (!is.null(cond$type)) {
        cond_args <- c(cond_args, glue('type = "{cond$type}"'))
      }
      if (!is.null(cond$strategy) && cond$strategy != "") {
        cond_args <- c(cond_args, glue('strategy = "{cond$strategy}"'))
      }
      if (!is.null(cond$referent) && cond$referent != "") {
        cond_args <- c(cond_args, glue('referent = "{cond$referent}"'))
      }
      if (!is.null(cond$comparator) && cond$comparator != "") {
        cond_args <- c(cond_args, glue('comparator = "{cond$comparator}"'))
      }
      cond_args <- c(cond_args, glue('target_value = {cond$target_value}'))
      if (!is.null(cond$group) && cond$group != "") {
        cond_args <- c(cond_args, glue('group = "{cond$group}"'))
      }
    }

    cond_call <- glue('{cond_func}({paste(cond_args, collapse = ", ")})')

    # Generate add_threshold_analysis call
    args <- glue('"{a$name}", "{a$variable}", {a$lower}, {a$upper}')
    args <- args %&% ",\n    condition = " %&% cond_call

    if (!is.null(a$variable_strategy) && a$variable_strategy != "") {
      args <- args %&% glue(',\n    variable_strategy = "{a$variable_strategy}"')
    }
    if (!is.null(a$variable_group) && a$variable_group != "") {
      args <- args %&% glue(',\n    variable_group = "{a$variable_group}"')
    }
    if (!is.null(a$active) && !isTRUE(a$active)) {
      args <- args %&% ',\n    active = FALSE'
    }

    code <- c(code, glue('model <- add_threshold_analysis(model, {args})'))
  }

  code
}