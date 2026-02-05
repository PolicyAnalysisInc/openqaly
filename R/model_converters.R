#' Model Format Converters
#'
#' Functions for converting openqaly models between different formats
#' (Excel folders, JSON files, YAML files, and R code).
#'
#' @name model_converters
#' @importFrom jsonlite validate
#' @importFrom tools file_ext
#' @importFrom yaml read_yaml write_yaml as.yaml
NULL

#' Write Model to File
#'
#' Write a oq_model object to a file in the specified format.
#'
#' For Excel format, the path should be a folder (will be created if needed).
#' For JSON, R, and YAML formats, the path should include the filename.
#'
#' @param model A oq_model object
#' @param path Character string specifying the output path
#' @param format Character string specifying the format ("excel", "json", "r", or "yaml")
#'
#' @return Invisibly returns the path
#'
#' @export
#' @examples
#' \dontrun{
#' # Write to Excel folder structure
#' write_model(model, "output/my_model/", format = "excel")
#'
#' # Write to JSON file
#' write_model(model, "output/my_model.json", format = "json")
#'
#' # Write to R script
#' write_model(model, "output/my_model.R", format = "r")
#'
#' # Write to YAML file
#' write_model(model, "output/my_model.yaml", format = "yaml")
#' }
write_model <- function(model, path, format = c("excel", "json", "r", "yaml")) {
  format <- match.arg(format)

  if (format == "excel") {
    # Excel format requires a folder path
    if (file_ext(path) != "") {
      stop("For Excel format, path must be a folder, not a file. Got: ", path)
    }

    # Create folder structure if needed
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    # Write Excel workbook
    write_model_excel(model, path)

  } else if (format == "yaml") {
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

#' Write Model as Excel
#'
#' Internal function to write a model to Excel folder structure.
#'
#' @param model A oq_model object
#' @param path Folder path
#'
#' @keywords internal
write_model_excel <- function(model, path) {
  # Prepare settings dataframe
  if (!is.null(model$settings) && is.list(model$settings)) {
    settings_df <- tibble(
      setting = names(model$settings),
      value = as.character(unlist(model$settings))
    )
  } else {
    settings_df <- model$settings
  }

  # Create workbook with all sheets
  wb_list <- list(
    settings = settings_df,
    strategies = model$strategies %||% tibble(),
    groups = model$groups %||% tibble(),
    states = model$states %||% tibble(),
    transitions = model$transitions %||% tibble(),
    values = model$values %||% tibble(),
    variables = model$variables %||% tibble(),
    summaries = model$summaries %||% tibble()
  )

  # Handle trees if present
  if (!is.null(model$trees) && nrow(model$trees) > 0) {
    wb_list$trees <- model$trees
  }

  # Handle multivariate sampling if present
  if (!is.null(model$multivariate_sampling) && length(model$multivariate_sampling) > 0) {
    # Create multivariate_sampling sheet
    mv_sampling_df <- tibble()
    mv_variables_df <- tibble()

    for (mv_spec in model$multivariate_sampling) {
      # Add to multivariate_sampling sheet
      mv_sampling_df <- bind_rows(
        mv_sampling_df,
        tibble(
          name = mv_spec$name,
          distribution = mv_spec$distribution,
          description = mv_spec$description %||% ""
        )
      )

      # Add to multivariate_sampling_variables sheet
      if (!is.null(mv_spec$variables)) {
        vars_df <- mv_spec$variables
        if (is.data.frame(vars_df)) {
          vars_df$sampling_name <- mv_spec$name
          # Ensure all columns exist
          if (!"strategy" %in% names(vars_df)) vars_df$strategy <- ""
          if (!"group" %in% names(vars_df)) vars_df$group <- ""
          # Reorder columns
          vars_df <- vars_df %>%
            select("sampling_name", "variable", "strategy", "group")
        } else if (is.character(vars_df)) {
          # Convert character vector to dataframe
          vars_df <- tibble(
            sampling_name = mv_spec$name,
            variable = vars_df,
            strategy = "",
            group = ""
          )
        }
        mv_variables_df <- bind_rows(mv_variables_df, vars_df)
      }
    }

    wb_list$multivariate_sampling <- mv_sampling_df
    wb_list$multivariate_sampling_variables <- mv_variables_df
  }

  # Build metadata sheet for table/script descriptions
  metadata_df <- tibble(
    component_type = character(),
    name = character(),
    description = character()
  )

  # Collect table metadata
  if (!is.null(model$tables)) {
    for (tbl_name in names(model$tables)) {
      tbl <- model$tables[[tbl_name]]
      desc <- if (is.list(tbl)) tbl$description else NULL
      if (!is.null(desc) && !is.na(desc) && desc != "") {
        metadata_df <- bind_rows(metadata_df, tibble(
          component_type = "table",
          name = tbl_name,
          description = desc
        ))
      }
    }
  }

  # Collect script metadata
  if (!is.null(model$scripts)) {
    for (scr_name in names(model$scripts)) {
      scr <- model$scripts[[scr_name]]
      desc <- if (is.list(scr)) scr$description else NULL
      if (!is.null(desc) && !is.na(desc) && desc != "") {
        metadata_df <- bind_rows(metadata_df, tibble(
          component_type = "script",
          name = scr_name,
          description = desc
        ))
      }
    }
  }

  if (nrow(metadata_df) > 0) {
    wb_list$`_metadata` <- metadata_df
  }

  # Add DSA parameters sheet
  if (length(model$dsa_parameters) > 0) {
    dsa_df <- tibble(
      type = character(),
      name = character(),
      low = character(),
      high = character(),
      strategy = character(),
      group = character(),
      display_name = character()
    )
    for (p in model$dsa_parameters) {
      dsa_df <- bind_rows(dsa_df, tibble(
        type = p$type,
        name = p$name,
        low = as.character(serialize_formula_or_value(p$low)),
        high = as.character(serialize_formula_or_value(p$high)),
        strategy = p$strategy %||% "",
        group = p$group %||% "",
        display_name = p$display_name %||% ""
      ))
    }
    wb_list$dsa_parameters <- dsa_df
  }

  # Add scenarios sheets
  if (length(model$scenarios) > 0) {
    scenarios_df <- tibble(name = character(), description = character())
    overrides_df <- tibble(
      scenario_name = character(),
      override_type = character(),
      name = character(),
      value = character(),
      strategy = character(),
      group = character()
    )

    for (s in model$scenarios) {
      scenarios_df <- bind_rows(scenarios_df, tibble(
        name = s$name,
        description = s$description %||% ""
      ))

      if (!is.null(s$variable_overrides)) {
        for (v in s$variable_overrides) {
          overrides_df <- bind_rows(overrides_df, tibble(
            scenario_name = s$name,
            override_type = "variable",
            name = v$name,
            value = as.character(serialize_formula_or_value(v$value)),
            strategy = v$strategy %||% "",
            group = v$group %||% ""
          ))
        }
      }

      if (!is.null(s$setting_overrides)) {
        for (st in s$setting_overrides) {
          overrides_df <- bind_rows(overrides_df, tibble(
            scenario_name = s$name,
            override_type = "setting",
            name = st$name,
            value = as.character(st$value),
            strategy = "",
            group = ""
          ))
        }
      }
    }

    wb_list$scenarios <- scenarios_df
    wb_list$scenario_overrides <- overrides_df
  }

  # Add TWSA sheets
  if (length(model$twsa_analyses) > 0) {
    twsa_df <- tibble(name = character(), description = character())
    params_df <- tibble(
      twsa_name = character(),
      param_type = character(),
      name = character(),
      type = character(),
      min = character(),
      max = character(),
      radius = character(),
      steps = numeric(),
      values = character(),
      strategy = character(),
      group = character(),
      display_name = character(),
      include_base_case = logical()
    )

    for (t in model$twsa_analyses) {
      twsa_df <- bind_rows(twsa_df, tibble(
        name = t$name,
        description = t$description %||% ""
      ))

      if (!is.null(t$parameters)) {
        for (p in t$parameters) {
          params_df <- bind_rows(params_df, tibble(
            twsa_name = t$name,
            param_type = p$param_type,
            name = p$name,
            type = p$type,
            min = if (!is.null(p$min)) as.character(serialize_formula_or_value(p$min)) else "",
            max = if (!is.null(p$max)) as.character(serialize_formula_or_value(p$max)) else "",
            radius = if (!is.null(p$radius)) as.character(serialize_formula_or_value(p$radius)) else "",
            steps = p$steps %||% NA_real_,
            values = if (!is.null(p$values)) paste(p$values, collapse = ",") else "",
            strategy = p$strategy %||% "",
            group = p$group %||% "",
            display_name = p$display_name %||% "",
            include_base_case = p$include_base_case %||% TRUE
          ))
        }
      }
    }

    wb_list$twsa_analyses <- twsa_df
    wb_list$twsa_parameters <- params_df
  }

  # Write the Excel file
  excel_path <- file.path(path, "model.xlsx")
  write.xlsx(wb_list, excel_path)

  # Write tables as CSV files
  if (!is.null(model$tables) && length(model$tables) > 0) {
    data_dir <- file.path(path, "data")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    for (table_name in names(model$tables)) {
      extracted <- extract_table_entry(model$tables[[table_name]])
      if (is.null(extracted)) next
      csv_path <- file.path(data_dir, paste0(table_name, ".csv"))
      write.csv(extracted$data, csv_path, row.names = FALSE)
    }
  }

  # Write scripts as R files
  if (!is.null(model$scripts) && length(model$scripts) > 0) {
    scripts_dir <- file.path(path, "scripts")
    dir.create(scripts_dir, recursive = TRUE, showWarnings = FALSE)

    for (script_name in names(model$scripts)) {
      extracted <- extract_script_entry(model$scripts[[script_name]])
      if (is.null(extracted)) next
      # Don't add .R if already present
      file_name <- if (grepl("\\.R$", script_name, ignore.case = TRUE)) {
        script_name
      } else {
        paste0(script_name, ".R")
      }
      r_path <- file.path(scripts_dir, file_name)
      writeLines(extracted$code, r_path)
    }
  }
}

#' Convert Model Between Formats
#'
#' Universal converter function that can detect input format and convert
#' to the specified output format.
#'
#' @param input Either a oq_model object, a path to a model file/folder, or a JSON string
#' @param output Output path for the converted model
#' @param from Input format ("auto" to detect, or "excel", "json", "yaml", "r", "object")
#' @param to Output format ("auto" to detect from extension, or "excel", "json", "yaml", "r")
#'
#' @return Invisibly returns the output path
#'
#' @export
#' @examples
#' \dontrun{
#' # Convert Excel to JSON
#' convert_model("models/my_model/", "models/my_model.json")
#'
#' # Convert JSON to R code
#' convert_model("models/my_model.json", "models/my_model.R")
#'
#' # Convert R code to Excel
#' convert_model("models/my_model.R", "models/my_model_output/")
#'
#' # Convert Excel to YAML
#' convert_model("models/my_model/", "models/my_model.yaml")
#'
#' # Convert YAML to JSON
#' convert_model("models/my_model.yaml", "models/my_model.json")
#' }
convert_model <- function(input, output, from = "auto", to = "auto") {

  # Auto-detect input format and load model
  if (from == "auto") {
    if (inherits(input, "oq_model")) {
      # Already a model object
      model <- input
    } else if (is.character(input) && length(input) == 1) {
      if (dir.exists(input)) {
        # Check if it's an Excel model folder
        excel_file <- file.path(input, "model.xlsx")
        if (file.exists(excel_file)) {
          model <- read_model(input)
        } else {
          stop("Directory exists but no model.xlsx found: ", input)
        }
      } else if (file.exists(input)) {
        # It's a file - check extension
        ext <- file_ext(input)
        if (ext == "json") {
          json_str <- paste(readLines(input), collapse = "\n")
          model <- read_model_json(json_str)
        } else if (ext %in% c("yaml", "yml")) {
          model <- read_model_yaml(input)
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
        # Try to parse as JSON string
        if (validate(input)) {
          model <- read_model_json(input)
        } else {
          stop("Input not recognized as file, folder, or valid JSON: ", input)
        }
      }
    } else {
      stop("Invalid input type")
    }
  } else {
    # Explicit format specified
    model <- switch(from,
      excel = read_model(input),
      json = {
        if (file.exists(input)) {
          read_model_json(paste(readLines(input), collapse = "\n"))
        } else {
          read_model_json(input)
        }
      },
      yaml = read_model_yaml(input),
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
      # Default to Excel for folders
      to <- "excel"
    }
  }

  # Write in target format
  write_model(model, output, format = to)

  invisible(output)
}

#' Read Model from YAML File
#'
#' Reads a complete openqaly model from a single YAML file. Tables are embedded
#' as CSV strings, scripts as code blocks, and trees use nested structure.
#'
#' @param path Path to YAML file (.yaml or .yml)
#'
#' @return A normalized and validated oq_model object
#'
#' @export
read_model_yaml <- function(path) {
  # Validate path
  if (!file.exists(path)) {
    stop("YAML file does not exist: ", path)
  }

  ext <- file_ext(path)
  if (!ext %in% c("yaml", "yml")) {
    stop("File must have .yaml or .yml extension: ", path)
  }

  # Read YAML file
  yaml_data <- read_yaml(path)

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

  # Validate and normalize
  model <- normalize_and_validate_model(model, preserve_builder = FALSE)

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

  as_tibble(do.call(rbind, rows))
}

#' Flatten Nested Tree Structure to Flat Tibble
#' @keywords internal
flatten_trees <- function(trees_list) {
  if (is.null(trees_list) || length(trees_list) == 0) {
    return(tibble())
  }

  rows <- list()
  node_id <- 1

  flatten_node <- function(node, parent_id) {
    current_id <- node_id
    node_id <<- node_id + 1

    row <- list(
      node_id = current_id,
      parent_id = parent_id,
      name = node$name,
      type = node$type,
      probability = node$probability,
      payoff = node$payoff
    )
    rows <<- c(rows, list(row))

    if (!is.null(node$children)) {
      for (child in node$children) {
        flatten_node(child, current_id)
      }
    }
  }

  for (tree in trees_list) {
    flatten_node(tree, NA)
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
    # Handle variables - can be array or tibble-like
    vars_df <- if (!is.null(mv$variables)) {
      if (is.data.frame(mv$variables)) {
        mv$variables
      } else {
        yaml_list_to_tibble(mv$variables)
      }
    } else {
      tibble(variable = character(0), strategy = character(0), group = character(0))
    }

    list(
      name = mv$name,
      distribution = mv$distribution,
      description = mv$description %||% "",
      variables = vars_df
    )
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
      display_name = p$display_name
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

  # Find root nodes (no parent)
  roots <- trees_tbl[is.na(trees_tbl$parent_id), ]

  build_node <- function(row) {
    node <- list(
      name = row$name,
      type = row$type
    )
    if (!is.na(row$probability)) {
      node$probability <- as.character(row$probability)
    }
    if (!is.na(row$payoff)) {
      node$payoff <- as.character(row$payoff)
    }

    # Find children
    children_rows <- trees_tbl[
      !is.na(trees_tbl$parent_id) & trees_tbl$parent_id == row$node_id,
    ]

    if (nrow(children_rows) > 0) {
      node$children <- lapply(seq_len(nrow(children_rows)), function(i) {
        build_node(children_rows[i, ])
      })
    }

    node
  }

  lapply(seq_len(nrow(roots)), function(i) {
    build_node(roots[i, ])
  })
}

#' Format Multivariate Sampling for YAML
#' @keywords internal
format_multivariate_sampling_yaml <- function(mv_list) {
  lapply(mv_list, function(mv) {
    result <- list(
      name = mv$name,
      distribution = mv$distribution,
      description = mv$description
    )

    # Handle variables
    if (!is.null(mv$variables)) {
      if (is.data.frame(mv$variables)) {
        result$variables <- tibble_to_yaml_list(mv$variables)
      } else {
        result$variables <- mv$variables
      }
    }

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

