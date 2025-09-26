#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2017  Matt Wiener
#*
#* This program is free software: you can redistribute it and/or modify
#* it under the terms of the GNU General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or
#* (at your option) any later version.
#*
#* This program is distributed in the hope that it will be useful,
#* but WITHOUT ANY WARRANTY; without even the implied warranty of
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#* GNU General Public License for more details.
#*
#* You should have received a copy of the GNU General Public License
#* along with this program.  If not, see <http://www.gnu.org/licenses/>.
#**************************************************************************

#' Look Up Values in a Data Frame
#' 
#' A convenience function to easily look for values in a 
#' data frame.
#' 
#' This function is mostly used to extract population 
#' informations (such as mortality rates), given some 
#' individual caracteristics.
#' 
#' If binning is activated, numeric individual 
#' characteristics are matched to the corresponding 
#' reference value that is directly inferior.
#' 
#' @param data A reference data frame.
#' @param value The value to extract ffrom the reference 
#'   data frame.
#' @param ... Individual characteristics, should be named 
#'   like the columns of `data`.
#' @param bin Either logical: should all numeric variable be
#'   binned, or character vector giving the names of
#'   variables to bin (see examples).
#'   
#' @return A vector of values, same lenght as `...`.
#' @examples
#' 
#' # Create a data frame with some example data
#' tbl <- data.frame(
#'   age = c(20, 30, 40, 50, 60),
#'   income = c(30000, 40000, 50000, 60000, 70000)
#' )
#' 
#' # Look up the value of income for age 30
#' look_up(tbl, age = 30, value = "income")
#' @export
look_up <- function(data, ..., bin = FALSE, value = "value") {

  # --- Argument Pre-checks (Before Evaluation) ---
  call <- match.call(expand.dots = FALSE)
  dots <- call$...

  if (length(dots) > 0) {
    dot_names <- names(dots)
    if (is.null(dot_names)) dot_names <- rep("", length(dots)) # Handle case where no args are named
    
    unnamed_indices <- which(dot_names == "")
    
    if (length(unnamed_indices) > 0) {
      is_equality_check <- sapply(dots[unnamed_indices], function(arg) {
        is.call(arg) && length(arg) == 3 && deparse(arg[[1]]) == "=="
      })
      
      if (any(is_equality_check)) {
        offending_arg_expr <- deparse(dots[unnamed_indices][is_equality_check][[1]])
        # Try to reconstruct a corrected example
        corrected_example <- gsub("==", "=", offending_arg_expr, fixed = TRUE)
        
        stop(paste0(
          "Error in look_up: Use '=' instead of '==' to specify lookup variables.",
          " Found an argument like ", sQuote(offending_arg_expr), ".",
          " Did you mean ", sQuote(corrected_example), "?"
        ))
      } else {
        # Original error for other unnamed args
        stop("All lookup variables passed to 'look_up()' must be named using '='.")
      }
    }
  }
  # --- End Argument Pre-checks ---

  # Handle case where this may be a vector by using first el
  value <- value[1]

  if (!inherits(data, "data.frame"))
    stop("'data' must be a data.frame")

  # Now evaluate the ... arguments safely
  list_specs <- list(...)
  
  data <- clean_factors(data)

  df_vars <- do.call(
    tibble::tibble,
    list_specs
  ) %>%
    clean_factors

  if (any(pb <- !c(names(df_vars), value) %in% names(data))) {
    stop(sprintf(
      "Names passed to 'look_up()' not found in 'data': %s.",
      paste(c(names(df_vars), value)[pb], collapse = ", ")
    ))
  }

  num_vars <- names(df_vars)[unlist(Map(is.numeric, df_vars))]

  if (isTRUE(bin)) {
    bin <- num_vars

  } else if (is.character(bin)) {
    if (any(pb <- !bin %in% names(df_vars))) {
      stop(sprintf(
        "Names in 'bin' not found in source data: %s.",
        paste(bin[pb], collapse = ", ")
      ))
    }

    if (any(pb <- !bin %in% num_vars)) {
      stop(sprintf(
        "Some variables in 'bin' are not numeric in the selection data: %s.",
        paste(bin[pb], collapse = ", ")
      ))
    }
  } else {
    bin <- character(0)
  }

  if (length(bin)) {
    pb_bin_src <- bin[unlist(Map(
      function(x) !is.numeric(x), data[bin]))]
    if (length(pb_bin_src)) {
      stop(sprintf(
        "Some variables in 'bin' are not numeric in the source data: %s.",
        paste(pb_bin_src, collapse = ", ")
      ))
    }
    with_infinite <- bin[sapply(data[bin], function(x){any(is.infinite(x))})]
    if (length(with_infinite))
      stop("infinite values in look_up table element",
           plur(length(with_infinite)),
           ": ",
           paste(with_infinite, collapse = ", ")
      )
    for (n in bin) {
      bin_values <- c(sort(unique(data[[n]])), +Inf)
      data[[n]] <- cut(data[[n]], bin_values,
                       include.lowest = TRUE,
                       right = FALSE)
      df_vars[[n]] <- cut(df_vars[[n]], bin_values,
                          include.lowest = TRUE,
                          right = FALSE)
    }
  }

  # do this test after binning
  if (any(pb <- duplicated(data[names(df_vars)]))) {
    stop(sprintf(
      "Some rows in 'data' are duplicates: %s.",
      paste(which(pb), collapse = ", ")
    ))
  }
  res <- suppressMessages(
    dplyr::left_join(
      df_vars,
      data,
      by = names(df_vars)
    )[[value]]
  )


  if (length(res) != nrow(df_vars)) {
    stop("Ooops, something went unexpectedly wrong...")
  }

  if (any(is.na(res))) {
    warning("Some values were not found, returning missing data:\n",
            "arguments to look_up: ",
            paste(names(list_specs), "=", unlist(list_specs), collapse = ", "),
            ", value = ", value
    )
  }

  res
}

#' Convert Data Frame Factor Variables to Character
#'
#' @param x A data frame.
#'
#' @return A data frame.
#'
#' @keywords internal
clean_factors <- function(x) {
  if (any(unlist(lapply(x, is.factor)))) {
    for (i in seq_along(x)) {
      if (is.factor(x[[i]])) {
        x[[i]] <- as.character(x[[i]])
      }
    }
  }
  x
}

#' @keywords internal
plur <- function(x) {
  if (x > 1) "s" else ""
}
