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
  # Convert list to proper 1-row tibble (needed when called from rowwise do())
  if (is.list(segment) && !is.data.frame(segment)) {
    segment <- as_tibble(lapply(segment, function(x) if (is.list(x)) list(x) else x))
  }

  model <- apply_setting_overrides(segment, model)

  # Parse variables for this segment
  uneval_vars <- parse_seg_variables(model$variables, segment, trees = model$trees)

  # Create namespace
  ns <- create_namespace(model, segment)

  override_result <- apply_parameter_overrides(segment, ns, uneval_vars)
  ns <- override_result$ns
  uneval_vars <- override_result$uneval_vars

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
#' @param model An openqaly model object
#' @param n the number of simulations
#' @param segments the segments for which resampling will be done (must have eval_vars and uneval_vars columns)
#' @param seed random seed for reproducibility
#'
#' @return a data.frame with resampled data by segment
#'
#' @export
resample <- function(model, n, segments, seed = NULL) {

  # Validate the variables specification if provided
  if (!is.null(model$variables)) {
    check_sampling_spec(model$variables)
  }

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

  # Validate cross-segment consistency before any sampling
  validate_sampling_consistency(model, segments)

  # ============================================================
  # PRE-DRAW PHASE: Sample everything once before the segment loop
  # ============================================================

  # Helper: check if a variable row matches a given segment
  var_matches_segment <- function(var_strategy, var_group, seg_strategy, seg_group) {
    strat_match <- is.empty(var_strategy) || identical(var_strategy, seg_strategy)
    group_match <- is.empty(var_group) || identical(var_group, seg_group)
    strat_match && group_match
  }

  # Helper: find first segment matching a strategy/group scope
  find_first_matching_segment <- function(var_strategy, var_group) {
    for (s in seq_len(nrow(segments))) {
      if (var_matches_segment(var_strategy, var_group,
                              segments$strategy[s], segments$group[s])) {
        return(s)
      }
    }
    return(NULL)
  }

  # Pre-draw univariate samples (one draw per variable row)
  pre_draws_univ <- list()
  univ_rows <- NULL

  if (has_univariate) {
    univ_vars <- model$variables %>%
      filter(!is.na(.data$sampling) & .data$sampling != "")
    univ_rows <- seq_len(nrow(univ_vars))

    for (i in univ_rows) {
      var_row <- univ_vars[i, ]
      var_name <- var_row$name
      var_strat <- if (is.na(var_row$strategy)) "" else var_row$strategy
      var_group <- if (is.na(var_row$group)) "" else var_row$group

      # Find first matching segment to evaluate dist_fn
      ref_seg_idx <- find_first_matching_segment(var_strat, var_group)
      if (is.null(ref_seg_idx)) next

      # Clone namespace, set bc, evaluate dist_fn
      ref_ns <- clone_namespace(segments$eval_vars[[ref_seg_idx]])
      ref_ns$env$bc <- ref_ns[var_name]

      formula <- as.oq_formula(var_row$sampling)
      dist_fn <- eval_formula(formula, ref_ns)

      if (is_oq_error(dist_fn)) {
        stop(glue("Failed to evaluate sampling distribution for parameter '{var_name}': {dist_fn}"),
             call. = FALSE)
      }

      # Draw once
      u <- runif(n)
      samples <- safe_eval(dist_fn(u))

      if (is_oq_error(samples)) {
        stop(glue("Failed to sample parameter '{var_name}': {samples}"), call. = FALSE)
      }

      pre_draws_univ[[as.character(i)]] <- list(
        name = var_name,
        samples = samples,
        strategy = var_strat,
        group = var_group
      )
    }
  }

  # Pre-draw multivariate samples (one draw per spec)
  pre_draws_mv <- list()

  if (has_multivariate) {
    for (mv_spec in model$multivariate_sampling) {
      spec_strat <- mv_spec$strategy %||% ""
      spec_group <- mv_spec$group %||% ""
      var_names <- mv_spec$variables

      # Find first matching segment
      ref_seg_idx <- find_first_matching_segment(spec_strat, spec_group)
      if (is.null(ref_seg_idx)) next

      ref_ns <- clone_namespace(segments$eval_vars[[ref_seg_idx]])

      # Construct distribution function from stored data
      dist_fn <- switch(mv_spec$type,
        "dirichlet" = {
          base_vals <- sapply(var_names, function(v) ref_ns[v])
          dirichlet(mv_spec$n * base_vals)
        },
        "mvnormal" = {
          mean_vec <- sapply(var_names, function(v) ref_ns[v])
          cov_data <- eval_formula(mv_spec$covariance, ref_ns)
          if (is_oq_error(cov_data)) {
            stop(glue("Failed to evaluate covariance formula for '{mv_spec$name}': {cov_data}"),
                 call. = FALSE)
          }
          cov_matrix <- as.matrix(cov_data)
          if (!is.numeric(cov_matrix)) {
            stop(glue("Covariance for '{mv_spec$name}' must contain only numeric values."),
                 call. = FALSE)
          }
          if (nrow(cov_matrix) != ncol(cov_matrix)) {
            stop(glue("Covariance for '{mv_spec$name}' must be square (got {nrow(cov_matrix)} x {ncol(cov_matrix)})."),
                 call. = FALSE)
          }
          if (nrow(cov_matrix) != length(var_names) || ncol(cov_matrix) != length(var_names)) {
            stop(glue(
              "Covariance for '{mv_spec$name}' dimensions ({nrow(cov_matrix)}x{ncol(cov_matrix)}) ",
              "don't match number of variables ({length(var_names)})."
            ), call. = FALSE)
          }
          mvnormal(mean = mean_vec, cov = cov_matrix)
        },
        "multinomial" = {
          # Base case values are counts; normalize to probabilities for rmultinom
          count_vec <- sapply(var_names, function(v) ref_ns[v])
          total <- sum(count_vec)
          if (total <= 0) {
            stop(glue("Multinomial spec '{mv_spec$name}': base case counts sum to {total}, must be positive."),
                 call. = FALSE)
          }
          prob_vec <- count_vec / total
          multinomial(size = as.integer(total), prob = prob_vec)
        },
        stop(glue("Unknown multivariate distribution type: {mv_spec$type}"), call. = FALSE)
      )

      # Draw once
      samples_matrix <- safe_eval(dist_fn(n))

      if (is_oq_error(samples_matrix)) {
        stop(glue("Failed to sample multivariate distribution '{mv_spec$name}': {samples_matrix}"),
             call. = FALSE)
      }

      if (ncol(samples_matrix) != length(var_names)) {
        stop(glue(
          "Distribution '{mv_spec$name}' returned {ncol(samples_matrix)} columns ",
          "but {length(var_names)} variables specified"
        ), call. = FALSE)
      }

      pre_draws_mv[[mv_spec$name]] <- list(
        var_names = var_names,
        samples_matrix = samples_matrix,
        strategy = spec_strat,
        group = spec_group
      )
    }
  }

  # ============================================================
  # DISTRIBUTION PHASE: Assign pre-drawn samples to segments
  # ============================================================

  results <- tibble()

  for (seg_idx in 1:nrow(segments)) {
    segment <- segments[seg_idx, ]
    seg_strategy <- segment$strategy[[1]]
    seg_group <- segment$group[[1]]

    parameter_overrides <- list()

    # Assign pre-drawn univariate samples
    for (key in names(pre_draws_univ)) {
      draw <- pre_draws_univ[[key]]
      if (var_matches_segment(draw$strategy, draw$group, seg_strategy, seg_group)) {
        parameter_overrides[[draw$name]] <- draw$samples
      }
    }

    # Assign pre-drawn multivariate columns
    for (mv_name in names(pre_draws_mv)) {
      draw <- pre_draws_mv[[mv_name]]
      if (var_matches_segment(draw$strategy, draw$group, seg_strategy, seg_group)) {
        for (i in seq_along(draw$var_names)) {
          parameter_overrides[[draw$var_names[i]]] <- draw$samples_matrix[, i]
        }
      }
    }

    # Build result for this segment
    if (length(parameter_overrides) == 0) {
      override_list <- rep(list(list()), n)
    } else {
      override_list <- transpose(parameter_overrides)
    }

    strat_name <- segment$strategy
    grp_name <- segment$group
    strat_vec <- rep(strat_name, n)
    grp_vec <- rep(grp_name, n)
    seg_result <- as.data.frame(
      setNames(
        list(strat_vec, grp_vec, 1:n),
        c("strategy", "group", "simulation")
      ),
      stringsAsFactors = FALSE
    )
    seg_result$parameter_overrides <- override_list

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

  # Build univariate sampling targets as (name, strategy, group) triples
  univ_triples <- list()
  if (!is.null(model$variables) && "sampling" %in% names(model$variables)) {
    univ_sampled <- model$variables %>%
      filter(!is.na(.data$sampling) & .data$sampling != "")
    for (i in seq_len(nrow(univ_sampled))) {
      row <- univ_sampled[i, ]
      strat <- if (is.na(row$strategy)) "" else row$strategy
      grp <- if (is.na(row$group)) "" else row$group
      univ_triples[[length(univ_triples) + 1]] <- list(
        name = row$name, strategy = strat, group = grp
      )
    }
  }

  # Validate and build multivariate sampling targets
  multi_triples <- list()
  if (!is.null(model$multivariate_sampling)) {
    for (mv_spec in model$multivariate_sampling) {
      # Validate type
      valid_types <- c("dirichlet", "mvnormal", "multinomial")
      if (is.null(mv_spec$type) || !mv_spec$type %in% valid_types) {
        errors <- c(errors, glue(
          "Multivariate spec '{mv_spec$name}' has invalid type. Must be one of: {paste(valid_types, collapse=', ')}"
        ))
      }

      # Validate type-specific fields
      if (!is.null(mv_spec$type)) {
        if (mv_spec$type == "mvnormal" && (is.null(mv_spec$covariance) || !inherits(mv_spec$covariance, "oq_formula"))) {
          errors <- c(errors, glue("Multivariate spec '{mv_spec$name}' (mvnormal) must have a 'covariance' formula."))
        }
        if (mv_spec$type == "dirichlet" && is.null(mv_spec$n)) {
          errors <- c(errors, glue("Multivariate spec '{mv_spec$name}' (dirichlet) needs 'n' (effective sample size)."))
        }
        # multinomial: size is derived from base case counts at runtime, no spec-level field needed
      }

      spec_strat <- mv_spec$strategy %||% ""
      spec_group <- mv_spec$group %||% ""
      for (var_name in mv_spec$variables) {
        multi_triples[[length(multi_triples) + 1]] <- list(
          name = var_name, strategy = spec_strat, group = spec_group,
          spec_name = mv_spec$name
        )
      }
    }

    # Check for conflicts between univariate and multivariate (segment-aware)
    # Two triples conflict if they share a variable name AND their segment scopes overlap
    for (ut in univ_triples) {
      for (mt in multi_triples) {
        if (ut$name != mt$name) next
        # Check overlap: "" (global) overlaps with anything
        strat_overlap <- (ut$strategy == "" || mt$strategy == "" || ut$strategy == mt$strategy)
        group_overlap <- (ut$group == "" || mt$group == "" || ut$group == mt$group)
        if (strat_overlap && group_overlap) {
          errors <- c(errors, glue(
            "Variable '{ut$name}' appears in both variables.sampling and multivariate_sampling ",
            "(spec '{mt$spec_name}') with overlapping segment scope."
          ))
        }
      }
    }

    # Check variables exist
    all_var_names <- model$variables$name
    multi_var_names <- unique(sapply(multi_triples, function(t) t$name))
    missing_vars <- setdiff(multi_var_names, all_var_names)
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

#' Validate Sampling Consistency Across Segments
#'
#' Checks that sampled variables which appear in multiple segments have identical
#' base case values and sampling distribution parameters across those segments.
#' A sampled variable must be strategy-specific if its bc varies by strategy,
#' and group-specific if its bc varies by group.
#'
#' @param model Parsed model object
#' @param segments Enriched segments (with eval_vars from prepare_segment_for_sampling)
#' @return TRUE if valid, stops with error if inconsistent
#' @keywords internal
validate_sampling_consistency <- function(model, segments) {
  errors <- character()

  # Build segment namespace lookup: key -> cloned namespace
  seg_ns_list <- list()
  for (i in seq_len(nrow(segments))) {
    key <- paste0(segments$strategy[i], "||", segments$group[i])
    seg_ns_list[[key]] <- clone_namespace(segments$eval_vars[[i]])
  }

  # Helper: check if a variable's value is identical across segments that

  # differ on the specified axis (strategy or group)
  check_axis_consistency <- function(var_name, var_strategy, var_group, axis) {
    # Find all segments this variable appears in
    matching_keys <- character()
    for (i in seq_len(nrow(segments))) {
      seg_s <- segments$strategy[i]
      seg_g <- segments$group[i]
      strat_match <- is.empty(var_strategy) || identical(var_strategy, seg_s)
      group_match <- is.empty(var_group) || identical(var_group, seg_g)
      if (strat_match && group_match) {
        matching_keys <- c(matching_keys, paste0(seg_s, "||", seg_g))
      }
    }

    if (length(matching_keys) <= 1) return(NULL)

    # Get the reference value from the first matching segment
    ref_val <- seg_ns_list[[matching_keys[1]]][var_name]

    # Skip non-numeric values (e.g., bootstrap functions, data frames)
    # These are produced by bootstrap() or table lookups and cannot meaningfully
    # differ by segment in a way this validation can detect
    if (!is.numeric(ref_val)) return(NULL)

    for (k in matching_keys[-1]) {
      other_val <- seg_ns_list[[k]][var_name]
      if (!identical(ref_val, other_val)) {
        # Determine which axis differs
        ref_parts <- strsplit(matching_keys[1], "||", fixed = TRUE)[[1]]
        other_parts <- strsplit(k, "||", fixed = TRUE)[[1]]
        # Format values for error message (handle non-scalar types like functions)
        fmt_ref <- if (is.numeric(ref_val) && length(ref_val) == 1) as.character(ref_val) else class(ref_val)[1]
        fmt_other <- if (is.numeric(other_val) && length(other_val) == 1) as.character(other_val) else class(other_val)[1]
        if (axis == "strategy" && ref_parts[1] != other_parts[1]) {
          return(glue(
            "Variable '{var_name}' is sampled but its base case value differs across strategies ",
            "(e.g., {fmt_ref} in '{ref_parts[1]}' vs {fmt_other} in '{other_parts[1]}'). ",
            "Define strategy-specific versions of '{var_name}' instead."
          ))
        }
        if (axis == "group" && ref_parts[2] != other_parts[2]) {
          return(glue(
            "Variable '{var_name}' is sampled but its base case value differs across groups ",
            "(e.g., {fmt_ref} in '{ref_parts[2]}' vs {fmt_other} in '{other_parts[2]}'). ",
            "Define group-specific versions of '{var_name}' instead."
          ))
        }
      }
    }
    return(NULL)
  }

  # Helper: check sampling formula dependencies for a univariate variable
  check_formula_deps <- function(var_name, var_strategy, var_group, sampling_str) {
    # Parse dependencies from sampling formula
    deps <- tryCatch(
      all.vars(parse(text = sampling_str)),
      error = function(e) character()
    )
    # Filter to model variable names only (exclude bc, function names, operators)
    model_var_names <- model$variables$name
    deps <- intersect(deps, model_var_names)
    # Remove bc (already checked via bc consistency)
    deps <- setdiff(deps, "bc")

    dep_errors <- character()
    for (dep in deps) {
      err <- check_axis_consistency(dep, var_strategy, var_group, "strategy")
      if (!is.null(err)) {
        dep_errors <- c(dep_errors, glue(
          "Variable '{var_name}' has sampling formula referencing '{dep}', ",
          "which varies by strategy. Make '{var_name}' strategy-specific ",
          "or make '{dep}' global."
        ))
      }
      err <- check_axis_consistency(dep, var_strategy, var_group, "group")
      if (!is.null(err)) {
        dep_errors <- c(dep_errors, glue(
          "Variable '{var_name}' has sampling formula referencing '{dep}', ",
          "which varies by group. Make '{var_name}' group-specific ",
          "or make '{dep}' global."
        ))
      }
    }
    dep_errors
  }

  # Check univariate sampled variables
  if (!is.null(model$variables) && "sampling" %in% names(model$variables)) {
    univ_vars <- model$variables %>%
      filter(!is.na(.data$sampling) & .data$sampling != "")

    for (i in seq_len(nrow(univ_vars))) {
      var_row <- univ_vars[i, ]
      var_name <- var_row$name
      var_strat <- if (is.na(var_row$strategy)) "" else var_row$strategy
      var_group <- if (is.na(var_row$group)) "" else var_row$group

      # Check bc consistency on strategy axis if variable is not strategy-specific
      if (var_strat == "") {
        err <- check_axis_consistency(var_name, var_strat, var_group, "strategy")
        if (!is.null(err)) errors <- c(errors, err)
      }

      # Check bc consistency on group axis if variable is not group-specific
      if (var_group == "") {
        err <- check_axis_consistency(var_name, var_strat, var_group, "group")
        if (!is.null(err)) errors <- c(errors, err)
      }

      # Check sampling formula dependencies
      dep_errs <- check_formula_deps(var_name, var_strat, var_group, var_row$sampling)
      errors <- c(errors, dep_errs)
    }
  }

  # Check multivariate sampled variables (bc consistency for their component variables)
  if (!is.null(model$multivariate_sampling)) {
    for (mv_spec in model$multivariate_sampling) {
      spec_strat <- mv_spec$strategy %||% ""
      spec_group <- mv_spec$group %||% ""

      for (var_name in mv_spec$variables) {
        # Check bc consistency on strategy axis if spec is not strategy-specific
        if (spec_strat == "") {
          err <- check_axis_consistency(var_name, spec_strat, spec_group, "strategy")
          if (!is.null(err)) errors <- c(errors, err)
        }
        # Check bc consistency on group axis if spec is not group-specific
        if (spec_group == "") {
          err <- check_axis_consistency(var_name, spec_strat, spec_group, "group")
          if (!is.null(err)) errors <- c(errors, err)
        }
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(unique(errors), collapse = "\n"), call. = FALSE)
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

}
