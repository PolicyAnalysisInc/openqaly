#' Model Code Generation
#'
#' Functions for generating executable R code from heRomod2 models.
#'
#' @name model_codegen
#' @importFrom glue glue
#' @importFrom purrr map_chr
NULL

#' Convert Model to R Code
#'
#' Generate executable R code that recreates a heRomod2 model.
#'
#' @param model A heRomodel object
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
    "# Generated heRomod2 model code",
    "# Created: " %&% Sys.Date(),
    "",
    "library(heRomod2)",
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

  # Add states
  if (!is.null(model$states) && nrow(model$states) > 0) {
    code <- c(code, generate_states_code(model$states))
  }

  # Add variables
  if (!is.null(model$variables) && nrow(model$variables) > 0) {
    code <- c(code, generate_variables_code(model$variables))
  }

  # Add transitions
  if (!is.null(model$transitions) && nrow(model$transitions) > 0) {
    is_psm <- tolower(model_type) == "psm"
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
generate_states_code <- function(states) {
  # Defensive check
  if (is.null(states) || !is.data.frame(states) || nrow(states) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(states))) {
    s <- states[i, ]
    args <- glue('"{s$name}", initial_prob = {s$initial_probability}')

    # Add optional arguments
    if ("display_name" %in% names(s) && !is.na(s$display_name) && s$display_name != s$name) {
      args <- args %&% glue(', display_name = "{s$display_name}"')
    }
    if ("description" %in% names(s) && !is.na(s$description) &&
        s$description != s$name &&
        (!("display_name" %in% names(s)) || s$description != s$display_name)) {
      args <- args %&% glue(', description = "{s$description}"')
    }
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
        glue('  add_transition("{t$from}", "{t$to}", {t$formula}) |>')
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
      args <- args %&% glue(', sampling = "{v$sampling}"')
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
    if ("abbreviation" %in% names(s) && !is.na(s$abbreviation)) {
      args <- args %&% glue(', abbreviation = "{s$abbreviation}"')
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
    tribble_code <- format_tribble(tables[[table_name]])

    code <- c(code,
      "",
      paste0('# Table: ', table_name),
      paste0(table_name, '_data <- ', tribble_code[1])
    )

    # Add the rest of the tribble lines (if multi-line)
    if (length(tribble_code) > 1) {
      code <- c(code, tribble_code[-1])
    }

    code <- c(code,
      paste0('model <- add_table(model, "', table_name, '", ', table_name, '_data)')
    )
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
    script_content <- scripts[[script_name]]

    # Escape quotes and backslashes in the script content
    script_escaped <- gsub("\\\\", "\\\\\\\\", script_content)  # Escape backslashes first
    script_escaped <- gsub('"', '\\\\"', script_escaped)        # Escape double quotes

    code <- c(code,
      "",
      paste0('# Script: ', script_name),
      paste0(script_name, '_code <- "', script_escaped, '"'),
      paste0('model <- add_script(model, "', script_name, '", ', script_name, '_code)')
    )
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

  # Trees in heRomod2 are typically parsed into variables via decision_tree()
  # rather than being added directly. The tree parsing happens automatically
  # when the model is read from Excel/JSON. For now, we'll just add a comment
  # indicating that trees are present but handled internally.

  code <- c(
    "# Decision trees are parsed into variables automatically",
    "# Tree data is embedded in the model structure"
  )

  return(code)
}