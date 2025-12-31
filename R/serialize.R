#' Prepare Model Results for JSON Serialization
#'
#' Converts model results to a JSON-serializable format by converting
#' namespace objects to dataframes and oq_formula objects to strings.
#' This function creates a copy of the results to avoid modifying the original.
#'
#' @param model_results Results from run_model()
#' @return Model results in a JSON-serializable format
#' @export
prepare_model_results_for_json <- function(model_results) {
  # Create a deep copy to avoid modifying the original
  serializable <- model_results
  
  # Convert segments if present
  if (!is.null(serializable$segments)) {
    serializable$segments <- prepare_segments_for_json(serializable$segments)
  }
  
  serializable
}

#' Prepare Segments for JSON Serialization
#'
#' Internal function to convert segments to JSON-serializable format.
#'
#' @param segments Segments dataframe from model results
#' @return Segments with serializable data
#' @keywords internal
prepare_segments_for_json <- function(segments) {
  # Process each row if segments is a dataframe/tibble
  if (is.data.frame(segments)) {
    # Convert oq_formula objects in uneval_vars
    if ("uneval_vars" %in% names(segments) && !is.null(segments$uneval_vars)) {
      segments$uneval_vars <- lapply(segments$uneval_vars, convert_oq_formulas)
    }
    
    # Convert namespace objects to dataframes
    if ("eval_vars" %in% names(segments) && !is.null(segments$eval_vars)) {
      segments$eval_vars <- lapply(segments$eval_vars, function(ns) {
        if (inherits(ns, "namespace")) {
          summary(ns)
        } else {
          ns
        }
      })
    }
  }
  
  segments
}

#' Convert oq_formula Objects to Strings
#'
#' Recursively converts oq_formula objects to character strings
#' throughout a nested data structure.
#'
#' @param obj Object to convert
#' @return Object with oq_formula objects converted to strings
#' @keywords internal
convert_oq_formulas <- function(obj) {
  if (inherits(obj, "oq_formula")) {
    as.character(obj)
  } else if (is.list(obj)) {
    # Recursively convert lists
    lapply(obj, convert_oq_formulas)
  } else if (is.data.frame(obj)) {
    # Convert formulas in dataframe columns
    obj[] <- lapply(obj, function(col) {
      if (is.list(col)) {
        lapply(col, convert_oq_formulas)
      } else {
        col
      }
    })
    obj
  } else {
    obj
  }
}

#' Export Model Results to JSON File
#'
#' Prepares model results for JSON serialization and writes them to a JSON file.
#' This function first converts the results to a serializable format
#' using prepare_model_results_for_json(), then exports to JSON.
#'
#' @param model_results Results from run_model()
#' @param file Path to output JSON file
#' @param pretty Whether to format JSON with indentation (default TRUE)
#' @param auto_unbox Whether to automatically unbox single-element vectors (default TRUE)
#' @export
export_model_json <- function(model_results, file, pretty = TRUE, auto_unbox = TRUE) {
  # Convert to serializable format
  serializable <- prepare_model_results_for_json(model_results)
  
  # Convert to JSON and write to file
  json_output <- toJSON(serializable,
                        pretty = pretty,
                        auto_unbox = auto_unbox,
                        null = "null",
                        na = "null")
  
  writeLines(json_output, file)
  
  invisible(serializable)
}