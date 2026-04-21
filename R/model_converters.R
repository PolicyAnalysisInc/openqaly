#' Model Format Converters
#'
#' Functions for converting openqaly models between different formats
#' (JSON files, YAML files, and R code).
#'
#' @name model_converters
#' @importFrom jsonlite validate
#' @importFrom tools file_ext
#' @importFrom yaml read_yaml write_yaml as.yaml
NULL

#' Write Model to File
#'
#' Write a oq_model object to a file in the specified format.
#' The path should include the filename.
#'
#' @param model A oq_model object
#' @param path Character string specifying the output path
#' @param format Character string specifying the format ("json", "r", or "yaml")
#'
#' @return Invisibly returns the path
#'
#' @export
#' @examples
#' \dontrun{
#' # Write to JSON file
#' write_model(model, "output/my_model.json", format = "json")
#'
#' # Write to R script
#' write_model(model, "output/my_model.R", format = "r")
#'
#' # Write to YAML file
#' write_model(model, "output/my_model.yaml", format = "yaml")
#' }
write_model <- function(model, path, format = c("json", "r", "yaml")) {
  format <- match.arg(format)

  if (format == "yaml") {
    # YAML format - single file
    if (!grepl("\\.(yaml|yml)$", path, ignore.case = TRUE)) {
      path <- paste0(path, ".yaml")
    }
    write_model_yaml(model, path)

  } else if (format %in% c("json", "r")) {
    # JSON and R formats require a file path
    if (file_ext(path) == "") {
      stop("For ", format, " format, path must include a filename. Got: ", path)
    }

    # Ensure parent directory exists
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

    if (format == "json") {
      # Write JSON file
      json_str <- write_model_json(model)
      writeLines(json_str, path)
    } else {
      # Write R code file
      r_code <- model_to_r_code(model)
      writeLines(r_code, path)
    }
  }

  invisible(path)
}

#' Convert Model Between Formats
#'
#' Universal converter function that can detect input format and convert
#' to the specified output format.
#'
#' @param input Either a oq_model object, a path to a model file/folder, or a JSON string
#' @param output Output path for the converted model
#' @param from Input format ("auto" to detect, or "json", "yaml", "r", "object")
#' @param to Output format ("auto" to detect from extension, or "json", "yaml", "r")
#'
#' @return Invisibly returns the output path
#'
#' @export
#' @examples
#' \dontrun{
#' # Convert JSON to R code
#' convert_model("models/my_model.json", "models/my_model.R")
#'
#' # Convert YAML to JSON
#' convert_model("models/my_model.yaml", "models/my_model.json")
#'
#' # Convert directory (model.yaml) to JSON
#' convert_model("models/my_model/", "models/my_model.json")
#' }
convert_model <- function(input, output, from = "auto", to = "auto") {

  # Auto-detect input format and load model
  if (from == "auto") {
    if (inherits(input, "oq_model")) {
      # Already a model object
      model <- input
    } else if (is.character(input) && length(input) == 1) {
      # Parse raw JSON strings before filesystem checks to avoid path warnings
      # when long JSON content is passed as input.
      if (validate(input)) {
        model <- read_model_json(text = input)
      } else if (dir.exists(input)) {
        # Check if it's a YAML model folder
        model <- read_model(input)
      } else if (file.exists(input)) {
        # It's a file - check extension
        ext <- file_ext(input)
        if (ext == "json") {
          model <- read_model_json(file = input)
        } else if (ext %in% c("yaml", "yml")) {
          model <- read_model_yaml(file = input)
        } else if (ext == "R") {
          # Execute R file to get model
          env <- new.env()
          source(input, local = env)
          # Find the model object (first oq_model object found)
          objs <- ls(env)
          models <- Filter(function(x) inherits(env[[x]], "oq_model"), objs)
          if (length(models) == 0) {
            stop("No oq_model object found in R file: ", input)
          }
          if (length(models) > 1) {
            warning("Multiple model objects found in R file, using first: ", models[1])
          }
          model <- env[[models[1]]]
        } else {
          stop("Unknown file extension: ", ext)
        }
      } else {
        stop("Input not recognized as file, folder, or valid JSON: ", input)
      }
    } else {
      stop("Invalid input type")
    }
  } else {
    # Explicit format specified
    model <- switch(from,
      json = {
        if (file.exists(input)) {
          read_model_json(file = input)
        } else {
          read_model_json(text = input)
        }
      },
      yaml = read_model_yaml(file = input),
      r = {
        env <- new.env()
        source(input, local = env)
        objs <- ls(env)
        models <- Filter(function(x) inherits(env[[x]], "oq_model"), objs)
        if (length(models) == 0) stop("No model found in R file")
        env[[models[1]]]
      },
      object = input,
      stop("Unknown input format: ", from)
    )
  }

  # Auto-detect output format from path extension
  if (to == "auto") {
    ext <- file_ext(output)
    if (ext == "json") {
      to <- "json"
    } else if (ext %in% c("yaml", "yml")) {
      to <- "yaml"
    } else if (ext == "R") {
      to <- "r"
    } else {
      # Default to YAML for folders/unknown extensions
      to <- "yaml"
    }
  }

  # Write in target format
  write_model(model, output, format = to)

  invisible(output)
}

#' Read Model from YAML
#'
#' Reads a complete openqaly model from YAML. Tables are embedded
#' as CSV strings, scripts as code blocks, and trees use nested structure.
#'
#' Supply exactly one of \code{file} or \code{text}.
#'
#' @param file Path to YAML file (.yaml or .yml)
#' @param text A string containing the model in YAML format.
#'
#' @return A normalized and validated oq_model object
#'
#' @export
read_model_yaml <- function(file = NULL, text = NULL) {
  if (!is.null(file) && !is.null(text)) {
    stop("Provide either 'file' or 'text', not both.")
  }
  if (is.null(file) && is.null(text)) {
    stop("One of 'file' or 'text' must be provided.")
  }

  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("YAML file does not exist: ", file)
    }
    ext <- file_ext(file)
    if (!ext %in% c("yaml", "yml")) {
      stop("File must have .yaml or .yml extension: ", file)
    }
    yaml_data <- read_yaml(file)
  } else {
    yaml_data <- yaml::yaml.load(text)
  }

  # Initialize model structure
  model <- list()

  # Extract model type and format version
  model_type <- yaml_data$model_type %||% "markov"

  # Read settings - can be in flat key-value format
  if (!is.null(yaml_data$settings)) {
    model$settings <- yaml_data$settings
  } else {
    model$settings <- list()
  }
  model$settings$model_type <- model_type

  # Read tibble components
  component_names <- c("strategies", "groups", "states", "variables", "summaries")

  for (comp in component_names) {
    if (!is.null(yaml_data[[comp]])) {
      model[[comp]] <- yaml_list_to_tibble(yaml_data[[comp]])
    }
  }

  # Handle transitions - may have different column names based on model type
  if (!is.null(yaml_data$transitions)) {
    model$transitions <- yaml_list_to_tibble(yaml_data$transitions)
    # Rename YAML-friendly column names to internal names
    if ("from" %in% names(model$transitions)) {
      model$transitions <- model$transitions %>%
        rename(from_state = "from", to_state = "to")
    }
  }

  # Handle values
  if (!is.null(yaml_data$values)) {
    model$values <- yaml_list_to_tibble(yaml_data$values)
  }

  # Read and parse embedded tables
  if (!is.null(yaml_data$tables)) {
    model$tables <- list()
    for (table_name in names(yaml_data$tables)) {
      table_def <- yaml_data$tables[[table_name]]
      # Handle both simple (data only) and structured (data + description) format
      if (is.character(table_def)) {
        # Simple format - just CSV string
        csv_string <- table_def
        table_description <- NULL
      } else if (is.list(table_def)) {
        # Structured format with data and optional description
        csv_string <- table_def$data
        table_description <- table_def$description
      } else {
        next
      }
      model$tables[[table_name]] <- list(
        data = parse_embedded_csv(csv_string),
        description = table_description
      )
    }
  } else {
    model$tables <- list()
  }

  # Read and parse embedded scripts
  if (!is.null(yaml_data$scripts)) {
    model$scripts <- list()
    for (script_name in names(yaml_data$scripts)) {
      script_def <- yaml_data$scripts[[script_name]]
      # Handle both simple (code only) and structured (code + description) format
      if (is.character(script_def)) {
        # Simple format - just code string
        script_code <- script_def
        script_description <- NULL
      } else if (is.list(script_def)) {
        # Structured format with code and optional description
        script_code <- script_def$code
        script_description <- script_def$description
      } else {
        next
      }
      model$scripts[[script_name]] <- list(
        code = script_code,
        description = script_description
      )
    }
  } else {
    model$scripts <- list()
  }

  # Read and flatten nested trees
  if (!is.null(yaml_data$trees)) {
    model$trees <- flatten_trees(yaml_data$trees)
  }

  # Read decision_tree configuration
  if (!is.null(yaml_data$decision_tree)) {
    model$decision_tree <- as.list(yaml_data$decision_tree)
  }

  # Read multivariate sampling
  if (!is.null(yaml_data$multivariate_sampling)) {
    model$multivariate_sampling <- parse_yaml_multivariate_sampling(
      yaml_data$multivariate_sampling
    )
  }

  # Read DSA parameters
  if (!is.null(yaml_data$dsa)) {
    model$dsa_parameters <- parse_yaml_dsa_parameters(yaml_data$dsa)
  } else {
    model$dsa_parameters <- structure(list(), class = "dsa_parameters")
  }

  # Read scenarios
  if (!is.null(yaml_data$scenarios)) {
    model$scenarios <- parse_yaml_scenarios(yaml_data$scenarios)
  } else {
    model$scenarios <- list()
  }

  # Read TWSA
  if (!is.null(yaml_data$twsa)) {
    model$twsa_analyses <- parse_yaml_twsa(yaml_data$twsa)
  } else {
    model$twsa_analyses <- list()
  }

  # Read threshold analyses
  if (!is.null(yaml_data$threshold_analyses)) {
    model$threshold_analyses <- parse_yaml_threshold(yaml_data$threshold_analyses)
  } else {
    model$threshold_analyses <- list()
  }

  # Read VBP configuration
  if (!is.null(yaml_data$vbp)) {
    model$vbp <- list(
      price_variable = yaml_data$vbp$price_variable,
      intervention_strategy = yaml_data$vbp$intervention_strategy,
      outcome_summary = yaml_data$vbp$outcome_summary,
      cost_summary = yaml_data$vbp$cost_summary
    )
  } else {
    model$vbp <- NULL
  }

  # Read PSA configuration
  if (!is.null(yaml_data$psa)) {
    model$psa <- list(
      n_sim = as.integer(yaml_data$psa$n_sim),
      seed = yaml_data$psa$seed
    )
  } else {
    model$psa <- NULL
  }

  # Read documentation
  if (!is.null(yaml_data$documentation)) {
    model$documentation <- as.character(yaml_data$documentation)
  } else {
    model$documentation <- NULL
  }

  # Read override categories
  if (!is.null(yaml_data$override_categories)) {
    model$override_categories <- parse_yaml_override_categories(yaml_data$override_categories)
  } else {
    model$override_categories <- list()
  }

  # Validate and normalize
  model <- normalize_and_validate_model(model)

  return(model)
}

#' Parse Embedded CSV String into Data Frame
#' @keywords internal
parse_embedded_csv <- function(csv_string) {
  if (is.null(csv_string) || csv_string == "") {
    return(data.frame())
  }
  # Read CSV from string, R infers column types
  read.csv(
    text = csv_string,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Convert YAML List to Tibble
#' @keywords internal
yaml_list_to_tibble <- function(lst) {
  if (is.null(lst) || length(lst) == 0) return(tibble())

  # Convert list of objects to tibble
  rows <- lapply(lst, function(item) {
    # Convert NULL to NA for proper tibble handling
    item <- lapply(item, function(x) if (is.null(x)) NA else x)
    as.data.frame(item, stringsAsFactors = FALSE)
  })

  as_tibble(dplyr::bind_rows(rows))
}

#' Flatten Nested Tree Structure to Flat Tibble
#' @keywords internal
flatten_trees <- function(trees_list) {
  if (is.null(trees_list) || length(trees_list) == 0) {
    return(tibble())
  }

  rows <- list()
  for (tree_name in names(trees_list)) {
    nodes <- trees_list[[tree_name]]
    for (node_def in nodes) {
      rows <- c(rows, list(list(
        name = tree_name,
        node = node_def$node,
        parent = if (is.null(node_def$parent)) NA_character_ else node_def$parent,
        formula = node_def$formula,
        tags = if (is.null(node_def$tags)) NA_character_ else node_def$tags
      )))
    }
  }

  as_tibble(do.call(rbind, lapply(rows, function(r) {
    as.data.frame(r, stringsAsFactors = FALSE)
  })))
}

#' Parse YAML Multivariate Sampling
#' @keywords internal
parse_yaml_multivariate_sampling <- function(mv_list) {
  if (is.null(mv_list) || length(mv_list) == 0) {
    return(list())
  }

  lapply(mv_list, function(mv) {
    # Variables is a character vector
    vars <- if (!is.null(mv$variables)) {
      as.character(mv$variables)
    } else {
      character(0)
    }

    result <- list(
      name = mv$name,
      type = mv$type,
      strategy = mv$strategy %||% "",
      group = mv$group %||% "",
      description = mv$description %||% "",
      variables = vars
    )

    if (!is.null(mv[["covariance"]]) && !all(is.na(mv[["covariance"]]))) result$covariance <- as.oq_formula(mv[["covariance"]])
    if (!is.null(mv[["n"]]) && !all(is.na(mv[["n"]]))) result$n <- suppressWarnings(as.numeric(mv[["n"]]))
    if (!is.null(mv[["size"]]) && !all(is.na(mv[["size"]]))) result$size <- suppressWarnings(as.integer(mv[["size"]]))

    result
  })
}

#' Parse YAML DSA Parameters
#' @keywords internal
parse_yaml_dsa_parameters <- function(dsa_list) {
  if (is.null(dsa_list) || length(dsa_list) == 0) {
    return(structure(list(), class = "dsa_parameters"))
  }

  params <- lapply(dsa_list, function(p) {
    param_type <- p$type %||% "variable"
    list(
      type = param_type,
      name = p$name,
      low = deserialize_to_formula(p$low, param_type),
      high = deserialize_to_formula(p$high, param_type),
      strategy = p$strategy %||% "",
      group = p$group %||% "",
      display_name = p$display_name,
      range_label = p$range_label
    )
  })

  structure(params, class = "dsa_parameters")
}

#' Parse YAML Scenarios
#' @keywords internal
parse_yaml_scenarios <- function(scenario_list) {
  if (is.null(scenario_list) || length(scenario_list) == 0) {
    return(list())
  }

  lapply(scenario_list, function(s) {
    # Parse variable overrides
    var_overrides <- if (!is.null(s$variable_overrides)) {
      lapply(s$variable_overrides, function(v) {
        list(
          name = v$name,
          value = deserialize_to_formula(v$value, "variable"),
          strategy = v$strategy %||% "",
          group = v$group %||% ""
        )
      })
    } else {
      list()
    }

    # Parse setting overrides
    setting_overrides <- if (!is.null(s$setting_overrides)) {
      lapply(s$setting_overrides, function(st) {
        list(name = st$name, value = st$value)
      })
    } else {
      list()
    }

    list(
      name = s$name,
      description = s$description %||% "",
      variable_overrides = var_overrides,
      setting_overrides = setting_overrides
    )
  })
}

#' Parse YAML TWSA Analyses
#' @keywords internal
parse_yaml_twsa <- function(twsa_list) {
  if (is.null(twsa_list) || length(twsa_list) == 0) {
    return(list())
  }

  lapply(twsa_list, function(t) {
    # Parse parameters
    params <- if (!is.null(t$parameters)) {
      lapply(t$parameters, function(p) {
        param_type <- p$param_type %||% "variable"
        list(
          param_type = param_type,
          name = p$name,
          type = p$type,
          min = deserialize_to_formula(p$min, param_type),
          max = deserialize_to_formula(p$max, param_type),
          radius = deserialize_to_formula(p$radius, param_type),
          steps = p$steps,
          values = deserialize_to_formula(p$values, param_type),
          strategy = p$strategy %||% "",
          group = p$group %||% "",
          display_name = p$display_name,
          include_base_case = p$include_base_case %||% TRUE
        )
      })
    } else {
      list()
    }

    list(
      name = t$name,
      description = t$description %||% "",
      parameters = params
    )
  })
}

#' Parse YAML Threshold Analyses
#' @keywords internal
parse_yaml_threshold <- function(threshold_list) {
  if (is.null(threshold_list) || length(threshold_list) == 0) {
    return(list())
  }

  lapply(threshold_list, function(a) {
    condition <- if (!is.null(a$condition)) {
      a$condition
    } else {
      list()
    }

    list(
      name = a$name,
      variable = a$variable,
      variable_strategy = a$variable_strategy %||% "",
      variable_group = a$variable_group %||% "",
      lower = as.numeric(a$lower),
      upper = as.numeric(a$upper),
      active = if (!is.null(a$active)) as.logical(a$active) else TRUE,
      condition = condition
    )
  })
}

#' Write Model to YAML File
#'
#' Writes a oq_model object to a single YAML file. Tables are embedded as
#' CSV strings, scripts as code blocks, and trees use nested structure.
#'
#' @param model A oq_model object
#' @param path Output file path (must end in .yaml or .yml)
#'
#' @return Invisibly returns the path
#'
#' @export
write_model_yaml <- function(model, path) {
  # Ensure model is valid
  if (!"oq_model" %in% class(model)) {
    stop("Input must be a oq_model object")
  }

  # Validate extension
  ext <- file_ext(path)
  if (!ext %in% c("yaml", "yml")) {
    stop("Output path must have .yaml or .yml extension")
  }

  # Ensure parent directory exists
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # Build YAML structure
  yaml_data <- list()

  # Format version and model type
  yaml_data$format_version <- "1.0"
  yaml_data$model_type <- model$settings$model_type %||% "markov"

  # Settings (without model_type, stored at top level)
  settings <- model$settings
  settings$model_type <- NULL
  if (length(settings) > 0) {
    yaml_data$settings <- settings
  }

  # Tibble components
  if (!is.null(model$strategies) && nrow(model$strategies) > 0) {
    yaml_data$strategies <- tibble_to_yaml_list(model$strategies)
  }

  if (!is.null(model$groups) && nrow(model$groups) > 0) {
    yaml_data$groups <- tibble_to_yaml_list(model$groups)
  }

  if (!is.null(model$states) && nrow(model$states) > 0) {
    yaml_data$states <- tibble_to_yaml_list(model$states)
  }

  if (!is.null(model$variables) && nrow(model$variables) > 0) {
    yaml_data$variables <- tibble_to_yaml_list(model$variables)
  }

  if (!is.null(model$transitions) && nrow(model$transitions) > 0) {
    trans <- model$transitions
    # Rename columns to YAML-friendly names for Markov models
    if ("from_state" %in% names(trans)) {
      trans <- trans %>% rename(from = "from_state", to = "to_state")
    }
    yaml_data$transitions <- tibble_to_yaml_list(trans)
  }

  if (!is.null(model$values) && nrow(model$values) > 0) {
    yaml_data$values <- tibble_to_yaml_list(model$values)
  }

  if (!is.null(model$summaries) && nrow(model$summaries) > 0) {
    yaml_data$summaries <- tibble_to_yaml_list(model$summaries)
  }

  # Embed tables as CSV strings
  if (!is.null(model$tables) && length(model$tables) > 0) {
    yaml_data$tables <- list()
    for (table_name in names(model$tables)) {
      extracted <- extract_table_entry(model$tables[[table_name]])
      if (is.null(extracted)) next

      yaml_data$tables[[table_name]] <- list(
        description = extracted$description,
        data = strip_trailing_whitespace(format_csv_for_yaml(extracted$data))
      )
    }
  }

  # Embed scripts with code blocks
  if (!is.null(model$scripts) && length(model$scripts) > 0) {
    yaml_data$scripts <- list()
    for (script_name in names(model$scripts)) {
      extracted <- extract_script_entry(model$scripts[[script_name]])
      if (is.null(extracted)) next

      yaml_data$scripts[[script_name]] <- list(
        description = extracted$description,
        code = strip_trailing_whitespace(extracted$code)
      )
    }
  }

  # Convert trees to nested structure
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    yaml_data$trees <- nest_trees(model$trees)
  }

  # Decision tree configuration
  if (!is.null(model$decision_tree)) {
    yaml_data$decision_tree <- model$decision_tree
  }

  # Multivariate sampling
  if (!is.null(model$multivariate_sampling) &&
      length(model$multivariate_sampling) > 0) {
    yaml_data$multivariate_sampling <- format_multivariate_sampling_yaml(
      model$multivariate_sampling
    )
  }

  # DSA parameters
  if (length(model$dsa_parameters) > 0) {
    yaml_data$dsa <- format_dsa_yaml(model$dsa_parameters)
  }

  # Scenarios
  if (length(model$scenarios) > 0) {
    yaml_data$scenarios <- format_scenarios_yaml(model$scenarios)
  }

  # TWSA
  if (length(model$twsa_analyses) > 0) {
    yaml_data$twsa <- format_twsa_yaml(model$twsa_analyses)
  }

  # Threshold analyses
  if (!is.null(model$threshold_analyses) && length(model$threshold_analyses) > 0) {
    yaml_data$threshold_analyses <- format_threshold_yaml(model$threshold_analyses)
  }

  # VBP configuration
  if (!is.null(model$vbp)) {
    yaml_data$vbp <- model$vbp
  }

  # PSA configuration
  if (!is.null(model$psa)) {
    yaml_data$psa <- model$psa
  }

  # Override categories
  if (!is.null(model$override_categories) && length(model$override_categories) > 0) {
    yaml_data$override_categories <- format_override_categories_yaml(model$override_categories)
  }

  # Documentation
  if (!is.null(model$documentation)) {
    yaml_data$documentation <- model$documentation
  }

  # Write YAML file
  write_yaml_file(yaml_data, path)

  invisible(path)
}

#' Convert Tibble to YAML-friendly List
#' @keywords internal
tibble_to_yaml_list <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) return(list())

  lapply(seq_len(nrow(tbl)), function(i) {
    row <- as.list(tbl[i, ])
    # Convert NA to NULL for cleaner YAML
    lapply(row, function(x) if (is.na(x)) NULL else x)
  })
}

#' Format Data Frame as CSV String for YAML Embedding
#' @keywords internal
format_csv_for_yaml <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return("")
  }
  # Write to string
  csv_output <- NULL
  conn <- textConnection("csv_output", "w", local = TRUE)
  on.exit(close(conn))
  write.csv(df, conn, row.names = FALSE)
  paste(csv_output, collapse = "\n")
}

#' Convert Flat Tree Tibble to Nested Structure for YAML
#' @keywords internal
nest_trees <- function(trees_tbl) {
  if (is.null(trees_tbl) || nrow(trees_tbl) == 0) {
    return(list())
  }

  tree_names <- unique(trees_tbl$name)
  result <- list()
  for (tn in tree_names) {
    tree_rows <- trees_tbl[trees_tbl$name == tn, ]
    result[[tn]] <- lapply(seq_len(nrow(tree_rows)), function(i) {
      row <- tree_rows[i, ]
      node_list <- list(node = row$node, formula = row$formula)
      if (!is.na(row$parent)) node_list$parent <- row$parent
      if (!is.na(row$tags)) node_list$tags <- row$tags
      node_list
    })
  }
  result
}

#' Format Multivariate Sampling for YAML
#' @keywords internal
format_multivariate_sampling_yaml <- function(mv_list) {
  lapply(mv_list, function(mv) {
    result <- list(
      name = mv$name,
      type = mv$type,
      strategy = mv$strategy %||% "",
      group = mv$group %||% "",
      description = mv$description
    )

    if (!is.null(mv[["variables"]])) result$variables <- as.list(mv[["variables"]])
    if (!is.null(mv[["covariance"]])) result$covariance <- as.character(mv[["covariance"]])
    if (!is.null(mv[["n"]])) result$n <- mv[["n"]]
    if (!is.null(mv[["size"]])) result$size <- mv[["size"]]

    result
  })
}

#' Add Optional Field to List if Non-Empty
#'
#' Adds a field to a list only if the value is non-null, scalar, non-NA, and non-empty string.
#' Used for optional fields in DSA, Scenarios, and TWSA serialization.
#'
#' @param lst The list to add to
#' @param field_name The name of the field to add
#' @param value The value to check and potentially add
#' @return The modified list
#' @keywords internal
add_optional_field <- function(lst, field_name, value) {
  if (!is.null(value) && length(value) == 1 && !is.na(value) && value != "") {
    lst[[field_name]] <- value
  }
  lst
}

#' Extract Table Data from Entry
#'
#' Handles both legacy (direct data frame) and current (list with data/description) formats.
#'
#' @param table_entry A table entry (data.frame or list with data/description)
#' @return List with data and description, or NULL if invalid
#' @keywords internal
extract_table_entry <- function(table_entry) {
  if (is.data.frame(table_entry)) {
    list(data = table_entry, description = NULL)
  } else if (is.list(table_entry) && "data" %in% names(table_entry)) {
    list(data = table_entry$data, description = table_entry$description)
  } else {
    NULL
  }
}

#' Extract Script Code from Entry
#'
#' Handles both legacy (direct code string) and current (list with code/description) formats.
#'
#' @param script_entry A script entry (character or list with code/description)
#' @return List with code and description, or NULL if invalid
#' @keywords internal
extract_script_entry <- function(script_entry) {
  if (is.character(script_entry)) {
    list(code = script_entry, description = NULL)
  } else if (is.list(script_entry) && "code" %in% names(script_entry)) {
    list(code = script_entry$code, description = script_entry$description)
  } else {
    NULL
  }
}

#' Format DSA Parameters for YAML
#' @keywords internal
format_dsa_yaml <- function(dsa_params) {
  lapply(dsa_params, function(p) {
    result <- list(
      type = p$type,
      name = p$name,
      low = serialize_formula_or_value(p$low),
      high = serialize_formula_or_value(p$high)
    )
    result <- add_optional_field(result, "strategy", p$strategy)
    result <- add_optional_field(result, "group", p$group)
    result <- add_optional_field(result, "display_name", p$display_name)
    result <- add_optional_field(result, "range_label", p$range_label)
    result
  })
}

#' Format Scenarios for YAML
#' @keywords internal
format_scenarios_yaml <- function(scenarios) {
  lapply(scenarios, function(s) {
    result <- list(
      name = s$name,
      description = s$description %||% ""
    )

    # Variable overrides
    if (length(s$variable_overrides) > 0) {
      result$variable_overrides <- lapply(s$variable_overrides, function(v) {
        override <- list(
          name = v$name,
          value = serialize_formula_or_value(v$value)
        )
        override <- add_optional_field(override, "strategy", v$strategy)
        override <- add_optional_field(override, "group", v$group)
        override
      })
    }

    # Setting overrides
    if (length(s$setting_overrides) > 0) {
      result$setting_overrides <- lapply(s$setting_overrides, function(st) {
        list(name = st$name, value = st$value)
      })
    }

    result
  })
}

#' Format TWSA for YAML
#' @keywords internal
format_twsa_yaml <- function(twsa_list) {
  lapply(twsa_list, function(t) {
    result <- list(
      name = t$name,
      description = t$description %||% ""
    )

    if (length(t$parameters) > 0) {
      result$parameters <- lapply(t$parameters, function(p) {
        param <- list(
          param_type = p$param_type,
          name = p$name,
          type = p$type
        )
        if (!is.null(p$min)) param$min <- serialize_formula_or_value(p$min)
        if (!is.null(p$max)) param$max <- serialize_formula_or_value(p$max)
        if (!is.null(p$radius)) param$radius <- serialize_formula_or_value(p$radius)
        if (!is.null(p$steps)) param$steps <- p$steps
        if (!is.null(p$values)) {
          # Handle vector of values
          param$values <- if (inherits(p$values, "oq_formula")) {
            serialize_formula_or_value(p$values)
          } else {
            p$values
          }
        }
        param <- add_optional_field(param, "strategy", p$strategy)
        param <- add_optional_field(param, "group", p$group)
        param <- add_optional_field(param, "display_name", p$display_name)
        if (!is.null(p$include_base_case)) {
          param$include_base_case <- p$include_base_case
        }
        param
      })
    }

    result
  })
}

#' Format Threshold Analyses for YAML
#' @keywords internal
format_threshold_yaml <- function(threshold_list) {
  lapply(threshold_list, function(a) {
    result <- list(
      name = a$name,
      variable = a$variable
    )
    result <- add_optional_field(result, "variable_strategy", a$variable_strategy)
    result <- add_optional_field(result, "variable_group", a$variable_group)
    result$lower <- a$lower
    result$upper <- a$upper
    if (!is.null(a$active)) result$active <- a$active

    # Condition: write nested, omitting NULL/NA/empty fields
    if (!is.null(a$condition)) {
      cond <- list(output = a$condition$output)
      for (field in setdiff(names(a$condition), "output")) {
        val <- a$condition[[field]]
        if (is.null(val)) next
        if (is.logical(val) || is.numeric(val)) {
          cond[[field]] <- val
        } else if (is.character(val) && val != "") {
          cond[[field]] <- val
        }
      }
      result$condition <- cond
    }
    result
  })
}

#' Format Override Categories for YAML
#' @keywords internal
format_override_categories_yaml <- function(override_categories) {
  lapply(override_categories, function(cat_item) {
    result <- list(
      name = cat_item$name,
      general = cat_item$general
    )

    if (length(cat_item$overrides) > 0) {
      result$overrides <- lapply(cat_item$overrides, function(ovr) {
        override <- list(
          title = ovr$title,
          type = ovr$type,
          name = ovr$name,
          input_type = ovr$input_type,
          overridden_expression = ovr$overridden_expression
        )
        override <- add_optional_field(override, "description", ovr$description)
        override <- add_optional_field(override, "strategy", ovr$strategy)
        override <- add_optional_field(override, "group", ovr$group)
        override$general <- ovr$general

        # Add input_config if it has contents
        if (length(ovr$input_config) > 0) {
          override$input_config <- ovr$input_config
        }

        override
      })
    }

    result
  })
}

#' Parse YAML Override Categories
#' @keywords internal
parse_yaml_override_categories <- function(oc_list) {
  if (is.null(oc_list) || length(oc_list) == 0) {
    return(list())
  }

  lapply(oc_list, function(cat_item) {
    overrides <- if (!is.null(cat_item$overrides)) {
      lapply(cat_item$overrides, function(ovr) {
        input_config <- ovr$input_config %||% list()
        # Ensure dropdown options are properly structured
        if (!is.null(input_config$options) && is.list(input_config$options)) {
          input_config$options <- lapply(input_config$options, function(opt) {
            list(
              label = opt$label,
              value = as.character(opt$value),
              is_base_case = as.logical(opt$is_base_case %||% FALSE)
            )
          })
        }
        list(
          title = ovr$title,
          description = ovr$description %||% "",
          type = ovr$type %||% "variable",
          name = ovr$name,
          strategy = ovr$strategy %||% "",
          group = ovr$group %||% "",
          general = as.logical(ovr$general %||% FALSE),
          input_type = ovr$input_type %||% "numeric",
          overridden_expression = as.character(ovr$overridden_expression),
          input_config = input_config
        )
      })
    } else {
      list()
    }

    list(
      name = cat_item$name,
      general = as.logical(cat_item$general %||% FALSE),
      overrides = overrides
    )
  })
}

#' Strip trailing whitespace from lines for YAML literal block compatibility
#'
#' YAML literal block scalars require no trailing whitespace on lines.
#' This function removes trailing spaces/tabs from each line while preserving
#' the actual content and newline structure.
#'
#' @param x Character string with possible trailing whitespace on lines
#' @return String with trailing whitespace removed from each line
#' @keywords internal
strip_trailing_whitespace <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(x)
  # Remove trailing whitespace from each line
  gsub("[ \t]+(\r?\n)", "\\1", gsub("[ \t]+$", "", x))
}

#' Write YAML with Formatting Options
#' @keywords internal
write_yaml_file <- function(data, path) {
  yaml_str <- as.yaml(
    data,
    indent = 2,
    indent.mapping.sequence = TRUE,
    line.sep = "\n"
  )
  writeLines(yaml_str, path)
}
