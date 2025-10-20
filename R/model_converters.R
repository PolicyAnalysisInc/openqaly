#' Model Format Converters
#'
#' Functions for converting heRomod2 models between different formats
#' (Excel folders, JSON files, and R code).
#'
#' @name model_converters
#' @importFrom jsonlite validate
#' @importFrom tools file_ext
NULL

#' Write Model to File
#'
#' Write a heRomodel object to a file in the specified format.
#'
#' For Excel format, the path should be a folder (will be created if needed).
#' For JSON and R formats, the path should include the filename.
#'
#' @param model A heRomodel object
#' @param path Character string specifying the output path
#' @param format Character string specifying the format ("excel", "json", or "r")
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
#' }
write_model <- function(model, path, format = c("excel", "json", "r")) {
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
#' @param model A heRomodel object
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

  # Write the Excel file
  excel_path <- file.path(path, "model.xlsx")
  write.xlsx(wb_list, excel_path)

  # Write tables as CSV files
  if (!is.null(model$tables) && length(model$tables) > 0) {
    data_dir <- file.path(path, "data")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

    for (table_name in names(model$tables)) {
      table_data <- model$tables[[table_name]]
      csv_path <- file.path(data_dir, paste0(table_name, ".csv"))
      write.csv(table_data, csv_path, row.names = FALSE)
    }
  }

  # Write scripts as R files
  if (!is.null(model$scripts) && length(model$scripts) > 0) {
    scripts_dir <- file.path(path, "scripts")
    dir.create(scripts_dir, recursive = TRUE, showWarnings = FALSE)

    for (script_name in names(model$scripts)) {
      script_code <- model$scripts[[script_name]]
      r_path <- file.path(scripts_dir, paste0(script_name, ".R"))
      writeLines(script_code, r_path)
    }
  }
}

#' Convert Model Between Formats
#'
#' Universal converter function that can detect input format and convert
#' to the specified output format.
#'
#' @param input Either a heRomodel object, a path to a model file/folder, or a JSON string
#' @param output Output path for the converted model
#' @param from Input format ("auto" to detect, or "excel", "json", "r", "object")
#' @param to Output format ("auto" to detect from extension, or "excel", "json", "r")
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
#' }
convert_model <- function(input, output, from = "auto", to = "auto") {

  # Auto-detect input format and load model
  if (from == "auto") {
    if (inherits(input, "heRomodel")) {
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
        } else if (ext == "R") {
          # Execute R file to get model
          env <- new.env()
          source(input, local = env)
          # Find the model object (first heRomodel object found)
          objs <- ls(env)
          models <- Filter(function(x) inherits(env[[x]], "heRomodel"), objs)
          if (length(models) == 0) {
            stop("No heRomodel object found in R file: ", input)
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
      r = {
        env <- new.env()
        source(input, local = env)
        objs <- ls(env)
        models <- Filter(function(x) inherits(env[[x]], "heRomodel"), objs)
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

#' Detect Model Format
#'
#' Internal function to detect the format of a model input.
#'
#' @param input The input to check
#'
#' @return A string indicating the format
#'
#' @keywords internal
detect_model_format <- function(input) {
  if (inherits(input, "heRomodel")) {
    return("object")
  }

  if (is.character(input) && length(input) == 1) {
    if (dir.exists(input)) {
      if (file.exists(file.path(input, "model.xlsx"))) {
        return("excel")
      }
    } else if (file.exists(input)) {
      ext <- file_ext(input)
      if (ext == "xlsx") return("excel")
      if (ext == "json") return("json")
      if (ext == "R") return("r")
    } else if (validate(input)) {
      return("json_string")
    }
  }

  return("unknown")
}