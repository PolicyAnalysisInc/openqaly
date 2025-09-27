#' AST Rewriting Utilities for rlang Migration
#'
#' This file contains utilities for rewriting abstract syntax trees (AST)
#' as part of the migration from lazyeval to rlang.
#'
#' @keywords internal

#' Rewrite Unary Minus to Not Function
#'
#' Recursively replaces unary minus operators (-x) with not(x) function calls.
#' Binary minus operations (a - b) are left unchanged.
#'
#' @param expr An R expression to rewrite
#' @return The rewritten expression with unary minus replaced by not()
#'
#' @examples
#' \dontrun{
#' rewrite_unary_minus_to_not(quote(-x))        # Returns: not(x)
#' rewrite_unary_minus_to_not(quote(a - b))     # Returns: a - b (unchanged)
#' rewrite_unary_minus_to_not(quote(-(a | b)))  # Returns: not(a | b)
#' }
#' @keywords internal
rewrite_unary_minus_to_not <- function(expr) {
  # Handle NULL, atomic values, and names
  if (is.null(expr) || is.atomic(expr) || is.name(expr)) {
    return(expr)
  }

  # Handle function calls
  if (is.call(expr)) {
    # Check for unary minus (2 elements: operator + argument)
    if (is_call(expr, "-") && length(expr) == 2L) {
      # Transform: -x → not(x)
      # Recursively process the argument
      return(call2(
        quote(not),
        rewrite_unary_minus_to_not(expr[[2L]])
      ))
    }

    # For all other calls (including binary minus with 3 elements)
    # Preserve the function/operator and recursively process arguments
    fn <- expr[[1L]]
    args <- lapply(as.list(expr)[-1L], rewrite_unary_minus_to_not)
    return(call2(fn, !!!args))
  }

  # Handle pairlists (function formals)
  if (is.pairlist(expr)) {
    return(as.pairlist(lapply(expr, rewrite_unary_minus_to_not)))
  }

  # Handle lists and expressions
  if (is.list(expr) || is.expression(expr)) {
    return(lapply(expr, rewrite_unary_minus_to_not))
  }

  # Default: return unchanged
  expr
}