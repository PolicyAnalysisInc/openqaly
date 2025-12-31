# Internal error state environment
.oq_error_env <- new.env(parent = emptyenv())
.oq_error_env$errors <- list()

#' Clear Accumulated openqaly Errors (Primarily for Testing)
#'
#' Resets the internal list of accumulated errors.
#' @export
clear_oq_errors <- function() {
  .oq_error_env$errors <- list()
}

#' Get Accumulated openqaly Errors (Primarily for Testing)
#'
#' Returns the current internal list of accumulated errors without clearing it.
#' @return A list of accumulated error objects.
#' @export
get_accumulated_errors <- function() {
  .oq_error_env$errors
}

# Core error definitions
define_error <- function(x) {
  define_object(
    message = modify_error_msg(as.character(x)),
    class = 'oq_error'
  )
}

modify_error_msg <- function(x) {
  # Convert input to character defensively and store original
  original_x <- as.character(x)
  x_char <- original_x

  # Clean error message prefixes
  x_cleaned_prefixes <- gsub("^Error in [^:]+: ", "", x_char)
  x_cleaned_prefixes <- gsub("^Error: ", "", x_cleaned_prefixes, fixed = TRUE)
  
  # --- Pattern Matching --- 
  # Check 1: Standard object not found (use cleaned string for matching)
  if (grepl("object.*not found", x_cleaned_prefixes)) {
    name_match <- regmatches(x_cleaned_prefixes, regexpr("'([^']*)'", x_cleaned_prefixes))
    if (length(name_match) > 0) {
        name <- gsub("'", "", name_match)
        return(glue('Variable "{name}" not found.')) # Return formatted message
    }
  }
  # Check 2: 'could not find function' (use cleaned string for matching)
  else if (grepl("could not find function", x_cleaned_prefixes)) {
     func_match <- regmatches(x_cleaned_prefixes, regexpr('\\"([^\\\"]*)\\"\'', x_cleaned_prefixes))
     
     # Only construct the custom message if function name parsing was successful
     if (length(func_match) > 0) {
       func_name <- gsub('\\"', '', func_match)
       context_match <- regmatches(original_x, regexpr("^Error in ([^:]+):", original_x))
       context_hint <- if(length(context_match) > 1) glue(" in expression '{context_match[2]}'") else ""
       return(glue('Evaluation failed: Undefined variable likely used with `{func_name}`{context_hint}.')) # Return formatted message
     }
     # If func_match has length 0, parsing failed.
     # We do nothing further in this block, allowing execution to proceed
     # to the final fallback logic which returns the cleaned original message.
  }

  # Return cleaned message as fallback
  final_fallback <- x_cleaned_prefixes
  if (startsWith(final_fallback, "Error: ")) {
      final_fallback <- substring(final_fallback, 8)
  }
  final_fallback <- gsub("\\n$", "", final_fallback) 
  return(final_fallback)
}

#' @export
is_oq_error <- function(x) {
  inherits(x, 'oq_error') # Use inherits for class hierarchy check
}

#' @export
is_oq_dependency_error <- function(x) {
  inherits(x, 'oq_dependency_error')
}

#' @export
print.oq_error <- function(x, ...) {
  print(glue("Error: {x$message}"))
}

define_dependency_error <- function(msg) {
  define_object(
    message = msg,
    class = c('oq_dependency_error', 'oq_error')
  )
}


# Error accumulation and checkpoint logic

#' Accumulate an oq_error (INTERNAL).
#'
#' Adds an oq_error object to the internal error list.
#'
#' @param error_obj An object of class `oq_error`.
#' @param context_msg A character string describing the context.
#' @return Invisibly returns NULL.
accumulate_oq_error <- function(error_obj, context_msg) {
  if (!is_oq_error(error_obj)) return(invisible(NULL)) # Silently ignore non-errors

  context_msg <- paste(context_msg, collapse = " ")
  new_error <- list(context = context_msg, error = error_obj)

  # Append directly to the list in the dedicated environment
  current_errors <- .oq_error_env$errors
  .oq_error_env$errors <- c(current_errors, list(new_error))

  invisible(NULL)
}

#' Format and throw collected errors (INTERNAL HELPER).
#'
#' Takes a list of error entries, formats them into a single markdown table,
#' and calls `stop()`. Filters out dependency errors before stopping.
#'
#' @param error_list A list where each element is `list(context=..., error=...)`.
#' @return This function does not return; it stops execution if `error_list` contains non-dependency errors.
format_and_throw_errors <- function(error_list) {
  # Filter out dependency errors before formatting
  root_cause_errors <- Filter(function(item) {
    !is_oq_dependency_error(item$error)
  }, error_list)

  # Only proceed if there are non-dependency errors remaining
  if (length(root_cause_errors) > 0) {
    # Create data frame for easy formatting using only root cause errors
    error_df <- do.call(rbind, lapply(root_cause_errors, function(item) {
      error_text <- if (is_oq_error(item$error) && !is.null(item$error$message)) {
        item$error$message
      } else {
         as.character(item$error)
      }
      data.frame(Context = as.character(item$context), Error = as.character(error_text), stringsAsFactors = FALSE)
    }))

    # Use the generic formatter to create the markdown table
    table_string <- format_dataframe_as_markdown_table(error_df)

    # Combine with prefix message
    prefix <- "Multiple errors found during evaluation:"
    final_message <- glue("{prefix}\n\n{table_string}")

    stop(final_message, call. = FALSE)
  }
  # If only dependency errors existed or list was empty, the function implicitly returns NULL here
}

#' Check for and report errors at a checkpoint (INTERNAL).
#'
#' Called by package functions at specific points.
#' This checks the internal error list (.oq_error_env$errors).
#' If non-dependency errors exist, they are formatted and thrown via `format_and_throw_errors`.
#' Regardless of whether errors were thrown, the internal error list is cleared.
#'
#' @return Invisibly returns NULL.
oq_error_checkpoint <- function() {
  # Get the errors directly from the dedicated environment
  errors <- .oq_error_env$errors

  # Always clear the list *before* potentially stopping
  clear_oq_errors()

  # If errors were accumulated, format and throw them
  if (!is.null(errors) && length(errors) > 0) {
    format_and_throw_errors(errors) # This stops if there are non-dependency errors
  }

  invisible(NULL)
}
