# --- Internal Environment for Error State ---
.hero_error_env <- new.env(parent = emptyenv())
.hero_error_env$errors <- list()

#' Clear Accumulated heRo Errors (Primarily for Testing)
#'
#' Resets the internal list of accumulated errors.
#' @export
clear_hero_errors <- function() {
  .hero_error_env$errors <- list()
}

#' Get Accumulated heRo Errors (Primarily for Testing)
#'
#' Returns the current internal list of accumulated errors without clearing it.
#' @return A list of accumulated error objects.
#' @export
get_accumulated_errors <- function() {
  .hero_error_env$errors
}

# --- Core Error Definitions (Unchanged) ---
define_error <- function(x) {
  define_object(
    message = modify_error_msg(as.character(x)),
    class = 'heRo_error'
  )
}

modify_error_msg <- function(x) {
  x <- gsub("Error in eval(x$expr, data, x$env): ", "", x, fixed = T)
  x <- gsub("Error: ", "", x, fixed = T)
  if (grepl('object.*not found', x)) {
    name <- strsplit(x, "'")[[1]][2]
    x <- glue('Variable "{name}" not found.')
  }
  x
}

#' @export
is_hero_error <- function(x) {
  inherits(x, 'heRo_error') # Use inherits for class hierarchy check
}

#' @export
is_hero_dependency_error <- function(x) {
  inherits(x, 'heRo_dependency_error')
}

#' @export
print.heRo_error <- function(x, ...) {
  print(glue("Error: {x$message}"))
}

define_dependency_error <- function(msg) {
  define_object(
    message = msg,
    class = c('heRo_dependency_error', 'heRo_error')
  )
}

# --- REMOVED Error Capture Mechanism ---
# with_hero_error_capture is no longer needed.

# --- REMOVED Internal Helper for Stack Traversal ---
# find_capture_helper_in_stack is no longer needed.

# --- Error Accumulation and Checkpoint Logic (Revised) ---

#' Accumulate a heRo_error (INTERNAL).
#'
#' Adds a heRo_error object to the internal error list.
#'
#' @param error_obj An object of class `heRo_error`.
#' @param context_msg A character string describing the context.
#' @return Invisibly returns NULL.
accumulate_hero_error <- function(error_obj, context_msg) {
  if (!is_hero_error(error_obj)) return(invisible(NULL)) # Silently ignore non-errors

  context_msg <- paste(context_msg, collapse = " ")
  new_error <- list(context = context_msg, error = error_obj)
  
  # Append directly to the list in the dedicated environment
  current_errors <- .hero_error_env$errors
  .hero_error_env$errors <- c(current_errors, list(new_error))
  
  invisible(NULL)
}

#' Format and throw collected errors (INTERNAL HELPER - Unchanged Logic).
#'
#' Takes a list of error entries, formats them into a single markdown table,
#' and calls `stop()`. Filters out dependency errors before stopping.
#'
#' @param error_list A list where each element is `list(context=..., error=...)`.
#' @return This function does not return; it stops execution if `error_list` contains non-dependency errors.
format_and_throw_errors <- function(error_list) {
  # Filter out dependency errors before formatting
  root_cause_errors <- Filter(function(item) {
    !is_hero_dependency_error(item$error) 
  }, error_list)
  
  # Only proceed if there are non-dependency errors remaining
  if (length(root_cause_errors) > 0) {
    # Create data frame for easy formatting using only root cause errors
    error_df <- do.call(rbind, lapply(root_cause_errors, function(item) {
      error_text <- if (is_hero_error(item$error) && !is.null(item$error$message)) {
        item$error$message 
      } else {
         as.character(item$error)
      }
      data.frame(Context = as.character(item$context), Error = as.character(error_text), stringsAsFactors = FALSE)
    }))

    # Determine max column widths for formatting
    context_width <- max(nchar("Context"), max(nchar(error_df$Context)), na.rm = TRUE)
    error_width <- max(nchar("Error"), max(nchar(error_df$Error)), na.rm = TRUE)
    
    # Create table components with left justification
    header <- paste0("| ", format("Context", width = context_width, justify = "left"), 
                     " | ", 
                     format("Error", width = error_width, justify = "left"), 
                     " |")
    separator <- paste0("|-", paste(rep("-", context_width), collapse = ""), 
                        "-|-", paste(rep("-", error_width), collapse = ""), 
                        "-|")
    
    rows <- apply(error_df, 1, function(row) {
        paste0("| ", 
               format(row["Context"], width = context_width, justify = "left"), 
               " | ", 
               format(row["Error"], width = error_width, justify = "left"), 
               " |")
    })
    
    # Combine components into final message
    prefix <- "Multiple errors found during evaluation:"
    table_string <- paste(header, separator, paste(rows, collapse = "\n"), sep = "\n")
    final_message <- paste0(prefix, "\n\n", table_string)
    
    stop(final_message, call. = FALSE)
  }
  # If only dependency errors existed or list was empty, the function implicitly returns NULL here
}

#' Check for and report errors at a checkpoint (INTERNAL - Revised).
#'
#' Called by package functions at specific points.
#' If the global option `heRomod2.error_mode` is "checkpoint", this checks
#' the internal error list (.hero_error_env$errors).
#' If non-dependency errors exist, they are formatted and thrown via `format_and_throw_errors`.
#' Regardless of whether errors were thrown, the internal error list is cleared.
#'
#' @return Invisibly returns NULL.
hero_error_checkpoint <- function() {
  # Only act if mode is 'checkpoint'
  if (getOption("heRomod2.error_mode", default = "warning") != "checkpoint") return(invisible(NULL))

  # Get the errors directly from the dedicated environment
  errors <- .hero_error_env$errors

  # Always clear the list *before* potentially stopping
  clear_hero_errors() 

  # If errors were accumulated, format and throw them
  if (!is.null(errors) && length(errors) > 0) {
    format_and_throw_errors(errors) # This stops if there are non-dependency errors
  }
  
  invisible(NULL)
}
