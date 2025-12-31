#' Define a Formula
#'
#' Takes a string representing an R-expression and turns it into an oq_formula
#' object. If the resulting R-expression is invalid, it will be replaced
#' with an expression that evaluates to an error string.
#'
#' @param text An atomic character vector containing an R-expression.
#' @return An oq_formula object representing the given R-expression.
#' 
#' @keywords internal
define_formula <- function(string) {

  # Try to capture the expression as a quosure
  tryExpr <- tryCatch(
    parse_quo(string, env = global_env()),
    error = function(e) NULL
  )

  if (is.null(tryExpr)) {
    # If the expression can't be parsed, generate an error object.
    res_list <- list(
      text = string,
      quo = new_quosure(
        expr(tryCatch(
          stop("Error in formula syntax.", call. = FALSE),
          error = function(e) e
        )),
        empty_env()
      ),
      err = "parse-error",
      depends = character(),
      fo_depends = character(),
      after = character()
    )
  } else {
    # Extract expression for dependency analysis
    expr_obj <- quo_get_expr(tryExpr)
    # Build data for oq_formula object
    res_list <- list(
      text = string,
      quo = tryExpr,
      depends = all.vars(expr_obj, functions = TRUE),
      fo_depends = all.vars(expr_obj, functions = TRUE),
      after = character()
    )
  }

  # Return oq_formula object
  as.oq_formula(res_list)
}

# Evaluate Formula
eval_formula <- function(x, ns, max_st = NULL) {

  # Update quosure environment (no suppressWarnings needed)
  x$quo <- quo_set_env(x$quo, ns$env)

  df <- ns$df
  if (!is.null(max_st)) {
    df <- filter(df, state_cycle <= max_st)
  }

  # Evaluate with data masking
  res <- safe_eval(eval_tidy(x$quo, data = df))

  # If the initial evaluation did not result in an oq_error, return it
  # Otherwise, check if it was caused by a dependency error
  if (is_oq_error(res)) {
    # Check if any of the variables referenced is an error
    vars <- x$depends
    for (i in rev(vars)) {
      if (i %in% get_names(ns, 'all', keywords = F)) {
        value <- ns[i]
        if (is_oq_error(value)) {
          # Use the specific constructor for dependency errors
          error_msg <- glue('Error in dependency "{i}".')
          # Overwrite the original error with a dependency error
          res <- define_dependency_error(error_msg)
          # Once a dependency error is found, no need to check others for this formula
          break
        }
      }
    }
  }

  # Return the result (which might be the original error or a dependency error)
  res
}

# Safely evaluate an arbitrary statement and return the result if
# successful or an error object if not.
safe_eval <- function(x) {
  
  # Evaluate the expression
  res <- tryCatch(x, error = function(e) e, silent = T)
  
  # If an error occurs, create an error message
  if ('error' %in% class(res)) {
    res <- define_error(res)
  }
  
  # Return the result
  res
}

#' Convert a Formula to Character
#'
#' Takes an oq_formula object and converts it to its character representation.
#'
#' @param x An oq_formula object.
#' @param ... additional unused arguments.
#'
#' @return An atomic character vector representing the R-expression.
#'
#' @export
as.character.oq_formula <- function(x, ...) {
  x$text
}

#' @export
print.oq_formula <- function(x, ...) {
  cat(paste0('FORMULA: ', x$text))
}

#' Convert to Formula
#'
#' Convert an object to an oq_formula object.
#'
#' @param x Object to be converted to oq_formula.
#' @return An oq_formula object.
#'
#' @keywords internal
#' @export
as.oq_formula <- function(x) {
  UseMethod('as.oq_formula', x)
}

# oq_formula => oq_formula
#' @export
as.oq_formula.oq_formula <- function(x) {
  # Identity
  x
}

# character => oq_formula
#' @export
as.oq_formula.character <- function(x) {
  # Run define_formula on it
  define_formula(x)
}

# numeric => oq_formula
#' @export
as.oq_formula.numeric <- function(x) {
  # Run define_formula on it
  define_formula(as.character(x))
}

# list => oq_formula
#' @export
as.oq_formula.list <- function(x) {
  # Check for essential fields then set class property.
  props_to_check <- c('text', 'depends', 'quo')
  for (prop in props_to_check) {
    if (is.null(x[[prop]])) stop(glue('Property "{prop}" was missing.'))
  }
  class(x) <- c('oq_formula', 'list')
  x
}
