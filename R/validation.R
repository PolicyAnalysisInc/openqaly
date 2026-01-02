# Validation Helper Functions for openqaly
# These functions ensure type safety before passing data to C++ functions

#' Validate that a value is numeric
#'
#' @param value The value to validate
#' @param context Description of where this value came from (for error messages)
#' @param formula_text Optional original formula text for better error hints
#' @return The validated numeric value
#' @export
validate_numeric_result <- function(value, context, formula_text = NULL) {
  if (!is.numeric(value) && !is.integer(value)) {
    # Create descriptive error message based on actual type
    type_desc <- if (is.character(value)) {
      # Show the actual string value for clarity
      if (length(value) > 0) {
        val_str <- utils::head(value, 1)
        # If the value matches the formula text, it's likely an undefined variable
        # Guard against character(0) formula_text which would return logical(0) in comparison
        if (!is.null(formula_text) && length(formula_text) > 0 && val_str == formula_text) {
          glue("undefined variable '{val_str}'")
        } else {
          glue("character string '{val_str}'")
        }
      } else {
        "empty character vector"
      }
    } else if (is.logical(value)) {
      glue("logical value '{utils::head(value, 1)}'")
    } else if (is.list(value)) {
      "list object"
    } else {
      glue("type '{class(value)[1]}'")
    }

    # Add helpful hints based on the error type
    formula_hint <- ""
    # Guard against character(0) formula_text
    if (!is.null(formula_text) && length(formula_text) > 0 && is.character(formula_text)) {
      if (is.character(value) && length(value) > 0 && value[1] == formula_text) {
        # Variable name evaluated to itself - likely undefined
        formula_hint <- glue("\nCheck that variable '{formula_text}' is defined and numeric.")
      } else if (grepl("^['\"].*['\"]$", formula_text)) {
        # The formula itself is a quoted string
        formula_hint <- "\nFormula appears to be a quoted string. Remove quotes to use numeric value."
      } else if (is.character(value) && length(value) > 0) {
        # Check if the evaluated result looks like a number
        if (grepl("^[0-9.+-]+$", value[1])) {
          formula_hint <- glue("\nFormula evaluated to string '{value[1]}'. Use numeric {value[1]} instead.")
        }
      }
    }

    stop(glue("{context}: Formula '{formula_text}' evaluated to {type_desc}, expected numeric.{formula_hint}"),
         call. = FALSE)
  }

  # Return the validated value
  value
}

#' Validate inputs before calling C++ functions
#'
#' @param init Initial state probabilities
#' @param transitions Transition matrix
#' @param values Values data frame
#' @param value_names Character vector of value names
#' @param state_names Character vector of state names
#' @param expanded_state_map Data frame mapping states
#' @param half_cycle_method Method for half-cycle correction
#' @return TRUE if all validations pass
#' @export
validate_cpp_inputs <- function(init, transitions, values, value_names,
                               state_names, expanded_state_map,
                               half_cycle_method = "start") {

  # Validate init (initial state probabilities)
  if (!is.numeric(init)) {
    stop("Initial state probabilities must be numeric", call. = FALSE)
  }
  if (anyNA(init)) {
    na_positions <- which(is.na(init))
    # Try to get state names if available
    if (length(state_names) == length(init)) {
      na_states <- state_names[na_positions]
      stop(glue("Initial state probabilities contain NA values for states: {paste(na_states, collapse=', ')}"),
           call. = FALSE)
    } else {
      stop(glue("Initial state probabilities contain NA values at positions: {paste(na_positions, collapse=', ')}"),
           call. = FALSE)
    }
  }
  if (any(!is.finite(init))) {
    inf_positions <- which(!is.finite(init))
    if (length(state_names) == length(init)) {
      inf_states <- state_names[inf_positions]
      stop(glue("Initial state probabilities contain non-finite values for states: {paste(inf_states, collapse=', ')}"),
           call. = FALSE)
    } else {
      stop(glue("Initial state probabilities contain non-finite values at positions: {paste(inf_positions, collapse=', ')}"),
           call. = FALSE)
    }
  }
  if (any(init < 0)) {
    neg_positions <- which(init < 0)
    if (length(state_names) == length(init)) {
      neg_states <- state_names[neg_positions]
      neg_values <- init[neg_positions]
      stop(glue("Initial state probabilities must be non-negative. States with negative values: {paste(paste0(neg_states, '=', neg_values), collapse=', ')}"),
           call. = FALSE)
    } else {
      stop("Initial state probabilities must be non-negative", call. = FALSE)
    }
  }
  if (abs(sum(init) - 1) > 1e-10) {
    stop(glue("Initial state probabilities must sum to 1, but sum to {format(sum(init), digits=6)}"), call. = FALSE)
  }

  # Validate transitions matrix
  if (!is.matrix(transitions) && !is.data.frame(transitions)) {
    stop("Transitions must be a matrix or data frame", call. = FALSE)
  }
  trans_matrix <- as.matrix(transitions)
  if (!is.numeric(trans_matrix)) {
    stop("Transitions matrix contains non-numeric values", call. = FALSE)
  }
  if (anyNA(trans_matrix)) {
    # Find which transitions have NA values for better error message
    na_rows <- which(rowSums(is.na(trans_matrix)) > 0)
    stop(glue("Transitions matrix contains NA values in rows: {paste(na_rows, collapse=', ')}"),
         call. = FALSE)
  }
  if (any(!is.finite(trans_matrix))) {
    stop("Transitions matrix contains non-finite values", call. = FALSE)
  }
  # Note: Range validation [0,1] is NOT done here because:
  # - Special values like complement (C) evaluate to -pi as a marker
  # - These are processed correctly by the C++ code
  # - Early range validation would reject valid models

  # Validate values DataFrame structure
  if (!is.data.frame(values)) {
    stop("Values must be a data frame", call. = FALSE)
  }

  # Check required columns exist
  required_cols <- c("state", "destination")
  missing_cols <- setdiff(required_cols, names(values))
  if (length(missing_cols) > 0) {
    stop(glue("Values data frame missing required columns: {paste(missing_cols, collapse=', ')}"),
         call. = FALSE)
  }

  # Note: NA checking for values_list is handled by C++ code which provides
  # better error reporting with markdown tables showing exactly which values
  # and cycles have NA values

  # Validate character vectors
  if (!is.character(value_names)) {
    stop("value_names must be a character vector", call. = FALSE)
  }
  if (anyNA(value_names)) {
    stop("value_names contains NA values", call. = FALSE)
  }

  if (!is.character(state_names)) {
    stop("state_names must be a character vector", call. = FALSE)
  }
  if (anyNA(state_names)) {
    stop("state_names contains NA values", call. = FALSE)
  }

  # Validate expanded_state_map
  if (!is.data.frame(expanded_state_map)) {
    stop("expanded_state_map must be a data frame", call. = FALSE)
  }

  # Validate half_cycle_method
  valid_methods <- c("start", "end", "life-table")
  if (!half_cycle_method %in% valid_methods) {
    stop(glue("half_cycle_method must be one of: {paste(valid_methods, collapse=', ')}"),
         call. = FALSE)
  }

  return(TRUE)
}

#' Validate a transition formula result
#'
#' @param value The evaluated formula result
#' @param from_state Source state name
#' @param to_state Destination state name
#' @param trans_name Optional transition name
#' @param formula_text Optional formula text for better errors
#' @return The validated transition value
#' @export
validate_transition_result <- function(value, from_state, to_state,
                                     trans_name = NULL, formula_text = NULL) {

  # Create context description - use name if available, otherwise state info
  context <- if (!is.null(trans_name) && trans_name != "") {
    glue("Transition '{trans_name}'")
  } else {
    glue("Transition from '{from_state}' to '{to_state}'")
  }

  # Only validate that it's numeric (NOT the range)
  # Range validation happens in C++ after processing special values like complement
  validate_numeric_result(value, context, formula_text)
}

#' Validate a value formula result
#'
#' @param value The evaluated formula result
#' @param value_name Name of the value
#' @param state Optional state name
#' @param destination Optional destination name
#' @param formula_text Optional formula text
#' @return The validated numeric value
#' @export
validate_value_result <- function(value, value_name, state = NULL,
                                destination = NULL, formula_text = NULL) {

  # Create context description
  context_parts <- c(glue("Value '{value_name}'"))
  if (!is.null(state) && !is.na(state)) {
    context_parts <- c(context_parts, glue("in state '{state}'"))
  }
  if (!is.null(destination) && !is.na(destination)) {
    context_parts <- c(context_parts, glue("to destination '{destination}'"))
  }
  context <- paste(context_parts, collapse = " ")

  # Validate as numeric
  validate_numeric_result(value, context, formula_text)
}

#' Check if a summary exists and throw informative error if not
#'
#' Validates that a requested summary exists in the model metadata.
#' If the summary doesn't exist, throws an error with a formatted table
#' showing all available summaries and their definitions.
#'
#' @param summary_name The requested summary name
#' @param metadata Model metadata containing available summaries
#' @return TRUE if summary exists (otherwise throws an error)
#' @keywords internal
check_summary_exists <- function(summary_name, metadata) {
  # Check if metadata and summaries exist
  if (is.null(metadata) || is.null(metadata$summaries)) {
    stop(paste0("Summary '", summary_name, "' was not found. No summary metadata available."))
  }

  available_summaries <- metadata$summaries

  # Check if the requested summary exists
  if (!summary_name %in% available_summaries$name) {
    # Build error message with table of available summaries
    error_msg <- paste0("Summary '", summary_name, "' was not found.")

    if (nrow(available_summaries) > 0) {
      error_msg <- paste0(error_msg, "\n\nAvailable summaries in the model:")

      # Prepare data frame for formatting, handling optional columns
      table_df <- data.frame(
        Name = available_summaries$name,
        stringsAsFactors = FALSE
      )

      # Add optional columns if they exist
      if ("display_name" %in% names(available_summaries)) {
        table_df$`Display Name` <- available_summaries$display_name
      }

      if ("description" %in% names(available_summaries)) {
        table_df$Description <- available_summaries$description
      }

      if ("wtp" %in% names(available_summaries)) {
        # Format WTP, showing "NA" for NA values
        table_df$WTP <- ifelse(is.na(available_summaries$wtp),
                               "NA",
                               format(available_summaries$wtp, scientific = FALSE))
      }

      if ("values" %in% names(available_summaries)) {
        table_df$Values <- available_summaries$values
      }

      # Format as markdown table
      table_string <- format_dataframe_as_markdown_table(table_df)
      error_msg <- paste0(error_msg, "\n", table_string)
    }

    stop(error_msg, call. = FALSE)
  }

  # Summary exists
  invisible(TRUE)
}

#' Check if strategies exist and throw informative error if not
#'
#' Validates that requested strategies exist in the model metadata.
#' If any strategies don't exist, throws an error with a formatted table
#' showing all available strategies and their definitions.
#'
#' @param strategy_names Character vector of requested strategy names
#' @param metadata Model metadata containing available strategies
#' @return TRUE if all strategies exist (otherwise throws an error)
#' @keywords internal
check_strategies_exist <- function(strategy_names, metadata) {
  # Check if metadata and strategies exist
  if (is.null(metadata) || is.null(metadata$strategies)) {
    stop(paste0("Strategies not found in results. No strategy metadata available."))
  }

  available_strategies <- metadata$strategies

  # Check which strategies are missing
  missing <- setdiff(strategy_names, available_strategies$name)

  if (length(missing) > 0) {
    # Build error message with table of available strategies
    error_msg <- sprintf("Strategies not found in results: %s",
                        paste(missing, collapse = ", "))

    if (nrow(available_strategies) > 0) {
      error_msg <- paste0(error_msg, "\n\nAvailable strategies in the model:")

      # Prepare data frame for formatting, handling optional columns
      table_df <- data.frame(
        Name = available_strategies$name,
        stringsAsFactors = FALSE
      )

      # Add optional columns if they exist
      if ("display_name" %in% names(available_strategies)) {
        table_df$`Display Name` <- available_strategies$display_name
      }

      if ("description" %in% names(available_strategies)) {
        table_df$Description <- available_strategies$description
      }

      # Format as markdown table
      table_string <- format_dataframe_as_markdown_table(table_df)
      error_msg <- paste0(error_msg, "\n", table_string)
    }

    stop(error_msg, call. = FALSE)
  }

  # All strategies exist
  invisible(TRUE)
}

#' Check if a single strategy exists and throw informative error if not
#'
#' Convenience wrapper around check_strategies_exist for single strategy.
#'
#' @param strategy_name The requested strategy name
#' @param metadata Model metadata containing available strategies
#' @return TRUE if strategy exists (otherwise throws an error)
#' @keywords internal
check_strategy_exists <- function(strategy_name, metadata) {
  check_strategies_exist(strategy_name, metadata)
}

#' Check if a group exists and throw informative error if not
#'
#' Validates that a requested group exists in the model results.
#' If the group doesn't exist, throws an error with a formatted table
#' showing all available groups.
#'
#' @param group_name The requested group name
#' @param results Model results containing groups
#' @return TRUE if group exists (otherwise throws an error)
#' @keywords internal
check_group_exists <- function(group_name, results) {
  # Handle special keywords
  if (group_name %in% c("overall", "all", "all_groups")) {
    # These are valid keywords, not groups
    return(invisible(TRUE))
  }

  # Determine available groups from the results (NOT including "overall")
  available_groups <- character(0)

  # Check in segments results (this is where group data is stored)
  if (!is.null(results$segments)) {
    if (is.data.frame(results$segments) && "group" %in% names(results$segments)) {
      available_groups <- c(available_groups, unique(results$segments$group))
    }
  }

  # Check metadata for group information
  if (!is.null(results$metadata) && !is.null(results$metadata$groups)) {
    group_meta <- results$metadata$groups
    if ("name" %in% names(group_meta)) {
      available_groups <- c(available_groups, group_meta$name)
    }
  }

  # Remove duplicates
  available_groups <- unique(available_groups)

  # Check if the requested group exists
  if (!group_name %in% available_groups) {
    # Build error message with table of available groups
    error_msg <- sprintf("Group '%s' not found in results.", group_name)

    if (length(available_groups) > 0) {
      error_msg <- paste0(error_msg, "\n\nAvailable groups in the model:")

      # Prepare data frame for formatting
      table_df <- data.frame(
        Name = available_groups,
        stringsAsFactors = FALSE
      )

      # Add display names if metadata is available
      if (!is.null(results$metadata) && !is.null(results$metadata$groups)) {
        group_meta <- results$metadata$groups
        if ("display_name" %in% names(group_meta)) {
          # Match and add display names
          for (i in seq_along(table_df$Name)) {
            group_name_i <- table_df$Name[i]
            if (group_name_i != "overall") {
              idx <- which(group_meta$name == group_name_i)
              if (length(idx) > 0) {
                table_df$`Display Name`[i] <- group_meta$display_name[idx[1]]
              }
            } else {
              table_df$`Display Name`[i] <- "Overall (weighted average)"
            }
          }
        }
      }

      # Format as markdown table
      table_string <- format_dataframe_as_markdown_table(table_df)
      error_msg <- paste0(error_msg, "\n", table_string)
    }

    stop(error_msg, call. = FALSE)
  }

  # Group exists
  invisible(TRUE)
}

#' Check if groups exist and throw informative error if not
#'
#' Validates that requested groups exist in the model results.
#' If any groups don't exist, throws an error with a formatted table
#' showing all available groups and keywords that can be used.
#'
#' @param group_names Character vector of requested group names
#' @param results Model results containing groups
#' @return TRUE if all groups exist (otherwise throws an error)
#' @keywords internal
check_groups_exist <- function(group_names, results) {
  # Get available groups from segments (NOT including "overall" which is a keyword)
  available_groups <- character(0)

  if (!is.null(results$segments)) {
    if (is.data.frame(results$segments) && "group" %in% names(results$segments)) {
      available_groups <- unique(results$segments$group)
    }
  }

  # Check which groups are missing
  missing <- setdiff(group_names, available_groups)

  if (length(missing) > 0) {
    # Build error message with missing groups
    error_msg <- sprintf("Groups not found in results: %s",
                        paste(missing, collapse = ", "))

    # Add information about what can be used
    error_msg <- paste0(error_msg, "\n\nYou can use:")

    # Add keywords section
    error_msg <- paste0(error_msg, "\n  \"overall\"    - Weighted average across all groups")
    error_msg <- paste0(error_msg, "\n  \"all\"        - Include overall and all groups")
    error_msg <- paste0(error_msg, "\n  \"all_groups\" - Include all groups (without overall)")

    # Add available groups if any exist
    if (length(available_groups) > 0) {
      error_msg <- paste0(error_msg, "\n\nOr these groups:")

      # Prepare data frame for formatting
      table_df <- data.frame(
        Name = available_groups,
        stringsAsFactors = FALSE
      )

      # Add metadata if available
      if (!is.null(results$metadata) && !is.null(results$metadata$groups)) {
        group_meta <- results$metadata$groups

        # Add display names if available
        if ("display_name" %in% names(group_meta)) {
          table_df$`Display Name` <- character(nrow(table_df))
          for (i in seq_along(table_df$Name)) {
            idx <- which(group_meta$name == table_df$Name[i])
            if (length(idx) > 0) {
              table_df$`Display Name`[i] <- group_meta$display_name[idx[1]]
            } else {
              table_df$`Display Name`[i] <- ""
            }
          }
        }

        # Add weights if available
        if ("weight" %in% names(group_meta)) {
          table_df$Weight <- character(nrow(table_df))
          for (i in seq_along(table_df$Name)) {
            idx <- which(group_meta$name == table_df$Name[i])
            if (length(idx) > 0) {
              table_df$Weight[i] <- format(group_meta$weight[idx[1]], digits = 2)
            } else {
              table_df$Weight[i] <- ""
            }
          }
        }
      }

      # Format as markdown table
      table_string <- format_dataframe_as_markdown_table(table_df)
      error_msg <- paste0(error_msg, "\n", table_string)
    }

    stop(error_msg, call. = FALSE)
  }

  # All groups exist
  invisible(TRUE)
}