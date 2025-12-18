#' Prepare Segment for Sampling
#'
#' Evaluates variables for a segment without running the full model.
#' This creates the evaluation environment needed for sampling distribution formulas.
#' Distribution formulas can reference any evaluated variable (including variables
#' being sampled for their base case values, and helper variables like SEs, alphas, correlations).
#'
#' @param model Parsed model object
#' @param segment Simple segment (strategy × group tibble row)
#' @return Segment enriched with eval_vars and uneval_vars list columns
#' @keywords internal
prepare_segment_for_sampling <- function(model, segment) {
  # Parse variables for this segment
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)

  # Create namespace
  ns <- create_namespace(model, segment)

  # Evaluate ALL variables (including those that will be sampled)
  # This gives us base case values for distribution formulas to reference
  eval_vars <- eval_variables(uneval_vars, ns)

  # Add to segment
  segment$uneval_vars <- list(uneval_vars)
  segment$eval_vars <- list(eval_vars)

  return(segment)
}

#' Resample Model Parameters
#'
#' Resamples the parameters for a model based on the sampling specifications.
#' Handles both univariate distributions (from variables.sampling column) and
#' multivariate distributions (from model$multivariate_sampling).
#'
#' @param model A heRomodel object
#' @param n the number of simulations
#' @param segments the segments for which resampling will be done (must have eval_vars and uneval_vars columns)
#' @param seed random seed for reproducibility
#'
#' @return a data.frame with resampled data by segment
#'
#' @export
resample <- function(model, n, segments, seed = NULL) {

  # Check the model's sampling specification early
  # Check if we have any form of sampling (univariate or multivariate)
  has_univariate <- FALSE
  has_multivariate <- FALSE

  if (!is.null(model$variables) && "sampling" %in% names(model$variables)) {
    has_univariate <- any(!is.empty(model$variables$sampling))
  }

  if (!is.null(model$multivariate_sampling) && length(model$multivariate_sampling) > 0) {
    has_multivariate <- TRUE
  }

  # Error if no sampling is specified at all
  if (!has_univariate && !has_multivariate) {
    stop('Error in variables specification, no sampling distributions were specified.', call. = FALSE)
  }

  # Segments must have eval_vars and uneval_vars columns
  if (!("eval_vars" %in% names(segments))) {
    stop("Segments must be enriched with eval_vars. Call prepare_segment_for_sampling() first.")
  }

  if (!is.null(seed)) set.seed(seed)

  results <- tibble()

  for (seg_idx in 1:nrow(segments)) {
    segment <- segments[seg_idx, ]

    # Clone the evaluated namespace for this segment
    # This contains ALL evaluated variables (including those being sampled)
    seg_ns <- clone_namespace(segment$eval_vars[[1]])
    seg_vars <- segment$uneval_vars[[1]]

    # Parameter overrides generated via probabilistic sampling
    parameter_overrides <- list()

    # PART 1: Univariate sampling from variables.sampling
    if (!is.null(model$variables) && "sampling" %in% names(model$variables)) {
      univ_vars <- model$variables %>%
        filter(!is.na(sampling) & sampling != "")

      for (i in seq_len(nrow(univ_vars))) {
        var_row <- univ_vars[i, ]

        # Safely extract strategy and group from segment (as scalars)
        seg_strategy <- segment$strategy[[1]]
        seg_group <- segment$group[[1]]

        # Check if this variable applies to this segment
        # If variable has specific strategy/group, use it; otherwise use segment's
        row_strat <- if (!is.na(var_row$strategy) && var_row$strategy != "") {
          var_row$strategy
        } else {
          seg_strategy
        }

        row_group <- if (!is.na(var_row$group) && var_row$group != "") {
          var_row$group
        } else {
          seg_group
        }

        # Skip if doesn't match this segment (NA-safe comparison)
        if (!identical(row_strat, seg_strategy) || !identical(row_group, seg_group)) {
          next
        }

        var_name <- var_row$name

        # Set bc (base case) to this variable's evaluated value
        seg_ns$env$bc <- seg_ns[var_name]

        # Evaluate distribution formula in namespace
        formula <- as.heRoFormula(var_row$sampling)
        dist_fn <- eval_formula(formula, seg_ns)

        if (is_hero_error(dist_fn)) {
          stop(glue("Failed to evaluate sampling distribution for parameter '{var_name}': {dist_fn}"), call. = FALSE)
        }

        # Sample using inverse CDF approach
        u <- runif(n)
        samples <- safe_eval(dist_fn(u))

        if (is_hero_error(samples)) {
          stop(glue("Failed to sample parameter '{var_name}': {samples}"), call. = FALSE)
        }

        parameter_overrides[[var_name]] <- samples
      }
    }

    # PART 2: Multivariate sampling
    if (!is.null(model$multivariate_sampling) && length(model$multivariate_sampling) > 0) {
      # Safely extract strategy and group from segment (as scalars)
      seg_strategy <- segment$strategy[[1]]
      seg_group <- segment$group[[1]]

      for (mv_spec in model$multivariate_sampling) {
        # Filter variables for this segment (NA-safe)
        relevant_vars <- mv_spec$variables %>%
          filter(
            (is.na(strategy) | strategy == "" | strategy == "all" | strategy == seg_strategy) &
            (is.na(group) | group == "" | group == "all" | group == seg_group)
          )

        if (nrow(relevant_vars) == 0) next

        # Evaluate distribution formula
        # The namespace contains ALL evaluated variables, including:
        # - Variables being sampled (with their base case values)
        # - Helper variables (alpha parameters, SE, correlations, etc.)
        formula <- as.heRoFormula(mv_spec$distribution)
        dist_fn <- eval_formula(formula, seg_ns)

        if (is_hero_error(dist_fn)) {
          stop(glue("Failed to evaluate multivariate distribution '{mv_spec$name}': {dist_fn}"), call. = FALSE)
        }

        # Sample (returns n × k matrix)
        samples_matrix <- safe_eval(dist_fn(n))

        if (is_hero_error(samples_matrix)) {
          stop(glue("Failed to sample multivariate distribution '{mv_spec$name}': {samples_matrix}"), call. = FALSE)
        }

        # Validate dimensionality
        if (ncol(samples_matrix) != nrow(relevant_vars)) {
          stop(glue(
            "Distribution '{mv_spec$name}' returned {ncol(samples_matrix)} columns ",
            "but {nrow(relevant_vars)} variables specified"
          ), call. = FALSE)
        }

        # Assign columns to variables (row order determines mapping)
        for (i in 1:nrow(relevant_vars)) {
          var_name <- relevant_vars$variable[i]
          parameter_overrides[[var_name]] <- samples_matrix[, i]
        }
      }
    }

    # Build result for this segment
    # Convert parameter_overrides from list-of-vectors to list-of-named-lists
    # Each element is one simulation's parameter overrides as a named list
    if (length(parameter_overrides) == 0) {
      # No sampling for this segment - use empty lists (base case values will be used)
      override_list <- rep(list(list()), n)
    } else {
      override_list <- purrr::transpose(parameter_overrides)
    }

    seg_result <- tibble(
      strategy = segment$strategy,
      group = segment$group,
      simulation = 1:n,
      parameter_overrides = override_list  # List column containing parameter overrides
    )

    results <- bind_rows(results, seg_result)
  }

  return(results)
}

#' Validate Sampling Specification
#'
#' Validates that sampling specifications are consistent and do not have conflicts.
#' Checks that variables are not specified in both univariate (variables.sampling)
#' and multivariate (multivariate_sampling) specifications.
#'
#' @param model Parsed model object
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_sampling_spec <- function(model) {
  errors <- character()

  # Get univariate sampled variables
  univ_vars <- character()
  if (!is.null(model$variables) && "sampling" %in% names(model$variables)) {
    univ_vars <- model$variables %>%
      filter(!is.na(sampling) & sampling != "") %>%
      pull(name)
  }

  # Get multivariate sampled variables
  multi_vars <- character()
  if (!is.null(model$multivariate_sampling)) {
    for (mv_spec in model$multivariate_sampling) {
      multi_vars <- c(multi_vars, mv_spec$variables$variable)
    }
    multi_vars <- unique(multi_vars)

    # Check for conflicts
    conflicts <- intersect(univ_vars, multi_vars)
    if (length(conflicts) > 0) {
      errors <- c(errors, glue(
        "Variables appear in both variables.sampling and multivariate_sampling: {paste(conflicts, collapse=', ')}"
      ))
    }

    # Check variables exist
    all_var_names <- model$variables$name
    missing_vars <- setdiff(multi_vars, all_var_names)
    if (length(missing_vars) > 0) {
      errors <- c(errors, glue(
        "Variables in multivariate_sampling not found in variables table: {paste(missing_vars, collapse=', ')}"
      ))
    }
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  return(TRUE)
}

check_sampling_spec <- function(x) {

  # Check that it is a data.frame
  is_df <- 'data.frame' %in% class(x)
  if (!is_df) {
    msg <- paste0(
      'Error in variables specification, specification was of class "',
      class(x)[1],
      '" rather than "data.frame".'
    )
    stop(msg, call. = F)
  }

  # Check that sampling column is present
  missing_col <- check_missing_colnames(x, 'sampling')
  if (length(missing_col) > 0) {
    stop(
      'Error in variables specification, "sampling" column was missing.',
      call. = F
    )
  }

  # Check that at least one variable is sampled
  none_sampled <- all(is.empty(x$sampling))
  if (none_sampled) {
    stop(
      'Error in variables specification, no sampling distributions were specified.',
      call. = F
    )
  }

}
