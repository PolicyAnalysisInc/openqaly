#' Model Code Generation
#'
#' Functions for generating executable R code from openqaly models.
#'
#' @name model_codegen
#' @importFrom glue glue
#' @importFrom purrr map_chr
NULL

# =============================================================================
# Internal Helpers
# =============================================================================

#' Escape double quotes in a string for code generation
#' @keywords internal
escape_quotes <- function(s) gsub('"', '\\"', s)

#' Convert oq_formula to character, pass through other types
#' @keywords internal
resolve_val <- function(x) if (inherits(x, "oq_formula")) as.character(x) else x

#' Emit an optional named argument fragment, or NULL if skipped.
#'
#' Returns NULL when the value should be skipped (NULL, or matches skip_if),
#' which is silently dropped by c().
#'
#' @param name The argument name to emit
#' @param val The value to check and emit
#' @param quoted Whether to quote the value (default TRUE)
#' @param skip_if Values that should cause the argument to be skipped (default "")
#' @keywords internal
opt_str <- function(name, val, quoted = TRUE, skip_if = "") {
  if (is.null(val)) return(NULL)
  if (any(!is.na(skip_if) & val == skip_if)) return(NULL)
  if (isTRUE(quoted)) glue('{name} = "{val}"') else glue('{name} = {val}')
}

#' Assemble a `model <- func(model, ...)` call, filtering NULLs and
#' flattening vectors (e.g. from switch() results).
#'
#' @param func The function name string
#' @param ... Argument fragments (strings or NULL)
#' @keywords internal
codegen_call <- function(func, ...) {
  args <- unlist(Filter(Negate(is.null), list(...)))
  glue('model <- {func}(model, {paste(args, collapse = ", ")})')
}

#' Append a code section with a blank line separator, only if non-empty
#' @keywords internal
append_section <- function(code, section_code) {
  if (length(section_code) > 0) c(code, "", section_code) else code
}

#' Emit a description argument for scenarios/TWSA, with escaping and skip logic
#' @keywords internal
emit_desc <- function(desc, name) {
  if (is.null(desc) || desc == "" || desc == name) NULL
  else glue('description = "{escape_quotes(desc)}"')
}

#' Render custom values vector, handling oq_formula
#' @keywords internal
rv_values <- function(vals, is_var) {
  if (is_var && inherits(vals, "oq_formula")) as.character(vals)
  else paste0("c(", paste(vals, collapse = ", "), ")")
}

#' Emit shared condition fields (discounted, target_value, group) for threshold analysis
#' @keywords internal
cond_common <- function(cond, include_target = TRUE) {
  c(if (!is.null(cond$discounted) && !isTRUE(cond$discounted)) "discounted = FALSE",
    if (include_target && !is.null(cond$target_value) && cond$target_value != 0)
      glue('target_value = {cond$target_value}'),
    opt_str("group", cond$group))
}

# =============================================================================
# Spec-Based Code Generation Engine
# =============================================================================

# Reusable display_name + description field specs
# display_name is skipped when it matches name
# description is skipped when it matches name or display_name
.dn_desc <- list(
  list(col = "display_name", quoted = TRUE, skip_if_col = "name"),
  list(col = "description", quoted = TRUE, skip_if_col = c("name", "display_name"))
)

#' Generate pipe-chain code from a declarative field spec.
#'
#' Instead of writing separate loop-and-if-block functions for each model
#' component, this engine takes a spec describing the function name, positional
#' arguments, and optional named arguments, and generates the code lines.
#'
#' @param data A dataframe to iterate over
#' @param spec A spec list with: func (string), positional (list of field specs),
#'   fields (list of optional field specs)
#' @return Character vector of code lines like '  add_X(...) |>'
#' @keywords internal
generate_from_spec <- function(data, spec) {
  # Defensive check
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(character(0))
  }

  code <- character()

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]

    # Build positional args
    pos_parts <- vapply(spec$positional, function(p) {
      val <- row[[p$col]]
      frag <- if (isTRUE(p$quoted)) glue('"{val}"') else as.character(val)
      if (!is.null(p$name)) paste0(p$name, " = ", frag) else frag
    }, character(1))
    args <- paste(pos_parts, collapse = ", ")

    # Process optional fields
    for (f in spec$fields %||% list()) {
      col <- f$col
      name <- f$name %||% col
      if (!(col %in% names(row))) next
      val <- row[[col]]
      if (is.na(val)) next

      # Bool emit: only emit ', name = TRUE' when truthy
      if (isTRUE(f$bool_emit)) {
        if (isTRUE(val)) args <- args %&% glue(', {name} = TRUE')
        next
      }

      # Skip if matches a default value
      if (!is.null(f$skip_if) && val %in% f$skip_if) next

      # Skip if matches another column's value (for display_name/description cascade)
      skip <- FALSE
      for (ref_col in f$skip_if_col %||% character(0)) {
        if (ref_col %in% names(row) && !is.na(row[[ref_col]]) && val == row[[ref_col]]) {
          skip <- TRUE
          break
        }
      }
      if (skip) next

      # Skip empty strings (default behavior for optional fields without skip_if_col)
      if (is.character(val) && val == "" && is.null(f$skip_if_col)) next

      # Escape quotes if requested
      if (isTRUE(f$escape)) val <- escape_quotes(val)

      # Auto-numeric: decide quoting at runtime (for groups weight)
      if (isTRUE(f$auto_numeric)) {
        if (suppressWarnings(!is.na(as.numeric(val)))) {
          args <- args %&% glue(', {name} = {val}')
        } else {
          args <- args %&% glue(', {name} = "{val}"')
        }
        next
      }

      # Standard emit: quoted or unquoted
      if (isTRUE(f$quoted)) {
        args <- args %&% glue(', {name} = "{val}"')
      } else {
        args <- args %&% glue(', {name} = {val}')
      }
    }

    code <- c(code, glue('  {spec$func}({args}) |>'))
  }

  code
}

# --- Specs for all pipe-chain generators ---

spec_states_markov <- list(
  func = "add_state",
  positional = list(
    list(col = "name", quoted = TRUE),
    list(col = "initial_probability", name = "initial_prob", quoted = FALSE)
  ),
  fields = c(.dn_desc, list(
    list(col = "state_group", quoted = TRUE),
    list(col = "share_state_time", bool_emit = TRUE),
    list(col = "state_cycle_limit", quoted = FALSE),
    list(col = "state_cycle_limit_unit", quoted = TRUE, skip_if = "cycles")
  ))
)

spec_states_psm <- list(
  func = "add_state",
  positional = list(list(col = "name", quoted = TRUE)),
  fields = .dn_desc)

spec_transitions_markov <- list(
  func = "add_transition",
  positional = list(
    list(col = "from_state", quoted = TRUE),
    list(col = "to_state", quoted = TRUE),
    list(col = "formula", quoted = FALSE)
  ),
  fields = list()
)

spec_transitions_psm <- list(
  func = "add_transition",
  positional = list(
    list(col = "endpoint", quoted = TRUE),
    list(col = "time_unit", quoted = TRUE),
    list(col = "formula", quoted = FALSE)
  ),
  fields = list()
)

spec_transitions_custom_psm <- list(
  func = "add_transition",
  positional = list(
    list(col = "state", quoted = TRUE),
    list(col = "formula", quoted = FALSE)
  ),
  fields = list()
)

spec_values <- list(
  func = "add_value",
  positional = list(
    list(col = "name", quoted = TRUE),
    list(col = "formula", quoted = FALSE)
  ),
  fields = c(
    list(
      list(col = "state", quoted = TRUE),
      list(col = "destination", quoted = TRUE)
    ),
    .dn_desc,
    list(
      list(col = "type", quoted = TRUE, skip_if = "outcome"),
      list(col = "discounting_override", quoted = TRUE)
    )
  )
)

spec_variables <- list(
  func = "add_variable",
  positional = list(
    list(col = "name", quoted = TRUE),
    list(col = "formula", quoted = FALSE)
  ),
  fields = c(.dn_desc, list(
    list(col = "strategy", quoted = TRUE),
    list(col = "group", quoted = TRUE),
    list(col = "source", quoted = TRUE, escape = TRUE),
    list(col = "sampling", quoted = FALSE)
  ))
)

spec_strategies <- list(
  func = "add_strategy",
  positional = list(list(col = "name", quoted = TRUE)),
  fields = c(.dn_desc, list(
    list(col = "enabled", quoted = FALSE, skip_if = 1)
  ))
)

spec_groups <- list(
  func = "add_group",
  positional = list(list(col = "name", quoted = TRUE)),
  fields = c(.dn_desc, list(
    list(col = "weight", auto_numeric = TRUE, skip_if = "1"),
    list(col = "enabled", quoted = FALSE, skip_if = 1)
  ))
)

spec_summaries <- list(
  func = "add_summary",
  positional = list(
    list(col = "name", quoted = TRUE),
    list(col = "values", quoted = TRUE)
  ),
  fields = .dn_desc)

# =============================================================================
# Main Entry Point
# =============================================================================

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

  # Determine model type once for use by multiple generators
  mt <- tolower(model_type)

  # Pipe-chain components: settings, strategies, groups, states, variables,
  # transitions, values, summaries
  pipe_generators <- list(
    list(data = model$settings, fn = generate_settings_code),
    list(data = model$strategies, fn = generate_strategies_code),
    list(data = model$groups, fn = generate_groups_code),
    list(data = model$states, fn = generate_states_code, extra = list(mt)),
    list(data = model$variables, fn = generate_variables_code),
    list(data = model$transitions, fn = generate_transitions_code, extra = list(mt)),
    list(data = model$values, fn = generate_values_code),
    list(data = model$summaries, fn = generate_summaries_code)
  )
  for (gen in pipe_generators) {
    result <- do.call(gen$fn, c(list(gen$data), gen$extra %||% list()))
    if (length(result) > 0) code <- c(code, result)
  }

  # Remove trailing pipe from last line
  last_line <- length(code)
  if (grepl("\\|>$", code[last_line])) {
    code[last_line] <- gsub(" \\|>$", "", code[last_line])
  }

  # Standalone components (separate from pipe chain):
  # tables, multivariate sampling, scripts, trees, decision tree config,
  # DSA parameters, scenarios, TWSA analyses, threshold analyses,
  # override categories, VBP config, PSA config, documentation
  standalone_sections <- list(
    generate_tables_code(model, model$tables),
    generate_multivariate_sampling_code(model$multivariate_sampling),
    generate_scripts_code(model, model$scripts),
    generate_trees_code(model, model$trees),
    generate_decision_tree_code(model$decision_tree),
    generate_dsa_code(model$dsa_parameters),
    generate_scenarios_code(model$scenarios),
    generate_twsa_code(model$twsa_analyses),
    generate_threshold_code(model$threshold_analyses),
    generate_override_code(model$override_categories),
    generate_vbp_code(model$vbp),
    generate_psa_code(model$psa),
    generate_documentation_code(model$documentation)
  )
  for (section in standalone_sections) {
    code <- append_section(code, section)
  }

  # Write to file if specified
  if (!is.null(file)) {
    writeLines(code, file)
  }

  invisible(code)
}

# =============================================================================
# Pipe-Chain Generators (spec-based wrappers)
# =============================================================================

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
generate_states_code <- function(states, model_type = "markov") {
  # Markov states have initial_probability and other fields; all others use simple spec
  spec <- if (model_type == "markov") spec_states_markov else spec_states_psm
  generate_from_spec(states, spec)
}

#' Generate Transitions Code
#' @keywords internal
generate_transitions_code <- function(transitions, model_type = "markov") {
  spec <- switch(model_type,
    psm = spec_transitions_psm,
    custom_psm = spec_transitions_custom_psm,
    spec_transitions_markov
  )
  generate_from_spec(transitions, spec)
}

#' Generate Values Code
#' @keywords internal
generate_values_code <- function(values) {
  generate_from_spec(values, spec_values)
}

#' Generate Variables Code
#' @keywords internal
generate_variables_code <- function(variables) {
  generate_from_spec(variables, spec_variables)
}

#' Generate Strategies Code
#' @keywords internal
generate_strategies_code <- function(strategies) {
  generate_from_spec(strategies, spec_strategies)
}

#' Generate Groups Code
#' @keywords internal
generate_groups_code <- function(groups) {
  generate_from_spec(groups, spec_groups)
}

#' Generate Summaries Code
#' @keywords internal
generate_summaries_code <- function(summaries) {
  generate_from_spec(summaries, spec_summaries)
}

# =============================================================================
# Standalone Generators
# =============================================================================

#' Generate Multivariate Sampling Code
#' @keywords internal
generate_multivariate_sampling_code <- function(multivariate_sampling) {
  # Defensive check
  if (is.null(multivariate_sampling) || length(multivariate_sampling) == 0) {
    return(character(0))
  }

  code <- character()

  for (mv_spec in multivariate_sampling) {
    # Build args using opt_str where possible
    args <- c(
      glue('name = "{mv_spec[["name"]]}"'),
      glue('type = "{mv_spec[["type"]]}"'),
      # Variables as character vector
      if (!is.null(mv_spec[["variables"]])) {
        var_names <- paste0('"', mv_spec[["variables"]], '"', collapse = ", ")
        glue('variables = c({var_names})')
      },
      # Strategy/group if non-empty
      opt_str("strategy", mv_spec[["strategy"]]),
      opt_str("group", mv_spec[["group"]]),
      # Type-specific parameters
      if (!is.null(mv_spec[["covariance"]])) glue('covariance = "{as.character(mv_spec[["covariance"]])}"'),
      if (!is.null(mv_spec[["n"]])) glue('n = {mv_spec[["n"]]}'),
      # Description
      opt_str("description", if (!is.null(mv_spec[["description"]]) && !is.na(mv_spec[["description"]])) escape_quotes(mv_spec[["description"]]))
    )
    # Filter NULLs from opt_str calls
    args <- Filter(Negate(is.null), args)

    # Build the function call (standalone, not in pipe chain)
    if (length(args) == 1) {
      code <- c(code, glue('model <- add_multivariate_sampling(model, {args})'))
    } else {
      code <- c(code,
        'model <- add_multivariate_sampling(model,',
        paste0('  ', args, collapse = ',\n'),
        ')'
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

  # Get column names and prepare them (backtick special names)
  col_names <- names(df)
  formatted_names <- ifelse(
    grepl("[^a-zA-Z0-9_.]", col_names) | grepl("^[0-9]", col_names),
    paste0("`", col_names, "`"), col_names
  )

  # Format values for each column based on type
  formatted_values <- lapply(seq_along(col_names), function(i) {
    col <- df[[col_names[i]]]
    if (is.character(col)) paste0('"', col, '"')
    else if (is.logical(col)) toupper(as.character(col))
    else as.character(col)
  })

  # Calculate column widths (including ~ for header)
  headers <- paste0("~", formatted_names)
  col_widths <- mapply(function(h, v) max(nchar(h), max(nchar(v))),
                       headers, formatted_values)

  # Helper to pad values to column width
  pad <- function(val, w) sprintf(paste0("%-", w, "s"), val)

  # Build the tribble code
  header_line <- paste0("  ", paste(mapply(pad, headers, col_widths), collapse = ", "), ",")

  # Add data rows
  row_lines <- vapply(seq_len(nrow(df)), function(r) {
    cells <- paste(mapply(function(v, w) pad(v[r], w), formatted_values, col_widths), collapse = ", ")
    row_line <- paste0("  ", cells)
    # Add comma except for last row
    if (r < nrow(df)) paste0(row_line, ",") else row_line
  }, character(1))

  c("tibble::tribble(", header_line, row_lines, ")")
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
    desc_part <- if (!is.null(table_description) && table_description != "") {
      paste0(', description = "', escape_quotes(table_description), '"')
    } else ""
    code <- c(code,
      paste0('model <- add_table(model, "', table_name, '", ', table_name, '_data', desc_part, ')')
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
    desc_part <- if (!is.null(script_description) && script_description != "") {
      paste0(', description = "', escape_quotes(script_description), '"')
    } else ""
    code <- c(code,
      paste0('model <- add_script(model, "', script_name, '", ', script_name, '_code', desc_part, ')')
    )
  }

  code
}

#' Generate Trees Code
#' @keywords internal
generate_trees_code <- function(model, trees) {
  # Defensive check for NULL, empty, or malformed trees
  # Trees can be either a data frame or a list
  is_empty <- is.null(trees) ||
    (is.data.frame(trees) && nrow(trees) == 0) ||
    (is.list(trees) && !is.data.frame(trees) && length(trees) == 0) ||
    (!is.data.frame(trees) && !is.list(trees))
  if (is_empty) return(character(0))

  # Trees in openqaly are typically parsed into variables via decision_tree()
  # rather than being added directly. The tree parsing happens automatically
  # when the model is read from YAML/JSON. For now, we'll just add a comment
  # indicating that trees are present but handled internally.
  c("# Decision trees are parsed into variables automatically",
    "# Tree data is embedded in the model structure")
}

# =============================================================================
# Analysis Generators
# =============================================================================

#' Generate DSA Code
#' @keywords internal
generate_dsa_code <- function(dsa_parameters) {
  # Defensive check
  if (is.null(dsa_parameters) || length(dsa_parameters) == 0) {
    return(character(0))
  }

  code <- c("# Add DSA parameters")

  for (param in dsa_parameters) {
    is_var <- param$type == "variable"
    func <- if (is_var) "add_dsa_variable" else "add_dsa_setting"

    # Convert oq_formula to string for variable low/high
    low <- if (is_var) resolve_val(param$low) else param$low
    high <- if (is_var) resolve_val(param$high) else param$high

    # display_name: for settings, skip when it matches the name
    dn_skip <- if (is_var) "" else param$name

    code <- c(code, codegen_call(func,
      glue('"{param$name}"'),
      glue('low = {low}'), glue('high = {high}'),
      # Strategy/group only for variables
      if (is_var) opt_str("strategy", param$strategy),
      if (is_var) opt_str("group", param$group),
      opt_str("display_name", param$display_name, skip_if = dn_skip)
    ))
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
    code <- c(code, codegen_call("add_scenario",
      glue('"{scenario$name}"'),
      emit_desc(scenario$description, scenario$name)
    ))

    # Generate add_scenario_variable calls
    for (override in scenario$variable_overrides %||% list()) {
      # Value can be numeric or oq_formula
      code <- c(code, codegen_call("add_scenario_variable",
        glue('"{scenario$name}"'), glue('"{override$name}"'), resolve_val(override$value),
        # Add optional strategy/group
        opt_str("strategy", override$strategy),
        opt_str("group", override$group)
      ))
    }

    # Generate add_scenario_setting calls
    for (override in scenario$setting_overrides %||% list()) {
      code <- c(code, codegen_call("add_scenario_setting",
        glue('"{scenario$name}"'), glue('"{override$name}"'), override$value
      ))
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
    code <- c(code, codegen_call("add_twsa",
      glue('"{twsa$name}"'),
      emit_desc(twsa$description, twsa$name)
    ))

    # Generate parameter calls
    for (param in twsa$parameters %||% list()) {
      is_var <- param$param_type == "variable"
      rv <- if (is_var) resolve_val else identity
      func <- if (is_var) "add_twsa_variable" else "add_twsa_setting"

      # Build type-specific arguments
      type_args <- switch(param$type,
        range = c(glue('min = {rv(param$min)}'), glue('max = {rv(param$max)}'),
                  glue('steps = {param$steps}')),
        radius = c(glue('radius = {rv(param$radius)}'), glue('steps = {param$steps}')),
        custom = glue('values = {rv_values(param$values, is_var)}')
      )

      # display_name: for settings, skip when it matches the name
      dn_skip <- if (is_var) "" else param$name

      code <- c(code, codegen_call(func,
        glue('"{twsa$name}"'), glue('"{param$name}"'), glue('type = "{param$type}"'),
        type_args,
        # Strategy/group only for variables
        if (is_var) opt_str("strategy", param$strategy),
        if (is_var) opt_str("group", param$group),
        opt_str("display_name", param$display_name, skip_if = dn_skip),
        # Add include_base_case if FALSE
        if (identical(param$include_base_case, FALSE)) "include_base_case = FALSE"
      ))
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

#' Generate PSA Code
#' @keywords internal
generate_psa_code <- function(psa) {
  if (is.null(psa)) return(character(0))

  args <- glue('  n_sim = {psa$n_sim}')
  if (!is.null(psa$seed)) {
    args <- c(paste0(args, ","), glue('  seed = {psa$seed}'))
  }

  c(
    "# Set PSA configuration",
    "model <- set_psa(model,",
    args,
    ")"
  )
}

#' Generate Documentation Code
#' @keywords internal
generate_documentation_code <- function(documentation) {
  if (is.null(documentation)) return(character(0))

  c(
    "# Set documentation",
    paste0("model <- set_documentation(model, ", deparse(documentation), ")")
  )
}

#' Generate Decision Tree Code
#' @keywords internal
generate_decision_tree_code <- function(decision_tree) {
  if (is.null(decision_tree)) return(character(0))

  tree_name <- decision_tree$tree_name
  duration <- decision_tree$duration
  duration_unit <- decision_tree$duration_unit

  c(
    "# Set decision tree configuration",
    glue('model <- set_decision_tree(model, "{tree_name}", {duration}, duration_unit = "{duration_unit}")')
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
    code <- c(code, codegen_call("add_override_category",
      glue('"{cat_item$name}"'),
      if (isTRUE(cat_item$general)) "general = TRUE"
    ))

    # Generate add_override calls for each override
    for (ovr in cat_item$overrides) {
      # Expression - output as unquoted if numeric, quoted if string
      expr_val <- suppressWarnings(as.numeric(ovr$overridden_expression))
      expr_str <- if (!is.na(expr_val)) ovr$overridden_expression
                  else glue('"{ovr$overridden_expression}"')

      ovr_args <- glue('"{cat_item$name}"') %&%
        glue(',\n    title = "{ovr$title}"') %&%
        glue(',\n    name = "{ovr$name}"') %&%
        glue(',\n    type = "{ovr$type}"') %&%
        glue(',\n    input_type = "{ovr$input_type}"') %&%
        glue(',\n    expression = {expr_str}')

      # Optional description
      if (!is.null(ovr$description) && ovr$description != "") {
        ovr_args <- ovr_args %&% glue(',\n    description = "{escape_quotes(ovr$description)}"')
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

      # Input config params (min, max, step_size)
      for (cfg_param in c("min", "max", "step_size")) {
        if (!is.null(ovr$input_config[[cfg_param]])) {
          ovr_args <- ovr_args %&% glue(',\n    {cfg_param} = {ovr$input_config[[cfg_param]]}')
        }
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
    cond_func <- paste0("threshold_condition_", cond$output)

    cond_args <- if (cond$output %in% c("outcomes", "costs")) {
      c(opt_str("summary", cond$summary),
        opt_str("value", cond$value),
        opt_str("type", cond$type, skip_if = character(0)),
        opt_str("strategy", cond$strategy),
        opt_str("referent", cond$referent),
        opt_str("comparator", cond$comparator),
        cond_common(cond))
    } else if (cond$output %in% c("nmb", "ce")) {
      c(glue('"{cond$health_summary}"'), glue('"{cond$cost_summary}"'),
        glue('"{cond$referent}"'), glue('"{cond$comparator}"'),
        cond_common(cond, include_target = cond$output == "nmb"),
        opt_str("wtp", cond$wtp, quoted = FALSE, skip_if = NA))
    } else if (cond$output == "trace") {
      c(glue('state = "{cond$state}"'), glue('time = {cond$time}'),
        opt_str("time_unit", cond$time_unit, skip_if = "cycle"),
        opt_str("type", cond$type, skip_if = character(0)),
        opt_str("strategy", cond$strategy),
        opt_str("referent", cond$referent),
        opt_str("comparator", cond$comparator),
        glue('target_value = {cond$target_value}'),
        opt_str("group", cond$group))
    }

    cond_call <- glue('{cond_func}({paste(Filter(Negate(is.null), cond_args), collapse = ", ")})')

    # Generate add_threshold_analysis call
    code <- c(code, codegen_call("add_threshold_analysis",
      glue('"{a$name}"'), glue('"{a$variable}"'), a$lower, a$upper,
      glue('condition = {cond_call}'),
      opt_str("variable_strategy", a$variable_strategy),
      opt_str("variable_group", a$variable_group),
      if (!is.null(a$active) && !isTRUE(a$active)) "active = FALSE"
    ))
  }

  code
}
