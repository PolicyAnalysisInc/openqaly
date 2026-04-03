#' Two-Way Sensitivity Analysis
#'
#' Functions for running two-way sensitivity analysis (2WSA) that varies
#' two parameters simultaneously across a grid of values.
#'
#' @name twsa
#' @importFrom dplyr filter mutate select arrange left_join inner_join bind_rows
#' @importFrom dplyr distinct group_by ungroup summarize slice n all_of
#' @importFrom dplyr rowwise
#' @importFrom tidyr unnest expand_grid crossing
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map map2
#' @importFrom furrr future_map furrr_options
#' @importFrom glue glue
NULL

#' Validate Two-Way Sensitivity Analysis Specification
#'
#' Validates that TWSA specifications are consistent. Checks that each TWSA
#' has exactly 2 parameters, variable names exist, and setting names are valid.
#'
#' @param model Parsed model object
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_twsa_spec <- function(model) {
  errors <- character()

  # Check if there are any TWSA analyses defined
  if (is.null(model$twsa_analyses) || length(model$twsa_analyses) == 0) {
    stop("No TWSA analyses defined. Use add_twsa() to define at least one analysis.",
         call. = FALSE)
  }

  # Valid setting names (same as DSA)
  valid_settings <- c(
    "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
    "discount_cost", "discount_outcomes", "half_cycle_method",
    "reduce_state_cycle", "days_per_year",
    "discount_rate"
  )

  all_var_names <- model$variables$name

  # Check for duplicate TWSA names
  twsa_names <- sapply(model$twsa_analyses, function(s) s$name)
  dup_names <- unique(twsa_names[duplicated(twsa_names)])
  if (length(dup_names) > 0) {
    errors <- c(errors, glue(
      "Duplicate TWSA names: {paste(dup_names, collapse=', ')}"
    ))
  }

  # Validate each TWSA analysis
  for (twsa in model$twsa_analyses) {
    # Check exactly 2 parameters
    n_params <- length(twsa$parameters)
    if (n_params != 2) {
      errors <- c(errors, glue(
        "TWSA '{twsa$name}' must have exactly 2 parameters, found {n_params}"
      ))
    }

    # Validate each parameter
    for (param in twsa$parameters) {
      if (param$param_type == "variable") {
        # Check that variable exists
        if (!(param$name %in% all_var_names)) {
          errors <- c(errors, glue(
            "TWSA '{twsa$name}': variable '{param$name}' not found in model variables"
          ))
        }
      } else if (param$param_type == "setting") {
        # Check that setting name is valid
        if (!(param$name %in% valid_settings)) {
          errors <- c(errors, glue(
            "TWSA '{twsa$name}': invalid setting name '{param$name}'. ",
            "Valid settings: {paste(valid_settings, collapse=', ')}"
          ))
        }
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  return(TRUE)
}

#' Estimate TWSA Parameter Grid Size
#'
#' Returns an upper-bound estimate of the number of values
#' for one TWSA parameter dimension. Used to compute progress
#' totals before the actual build begins.
#'
#' @param param Parameter specification from TWSA
#' @return Integer estimate of value count
#' @keywords internal
estimate_twsa_param_size <- function(param) {
  if (param$type == "radius") {
    param$steps * 2L + 1L
  } else if (param$type == "range") {
    param$steps + 1L
  } else {
    # custom: conservative fallback
    (param$steps %||% 4L) + 1L
  }
}

#' Generate Values for TWSA Parameter
#'
#' Evaluates the range specification for a TWSA parameter and returns a vector
#' of values to use.
#'
#' @param param Parameter specification from TWSA
#' @param namespace Evaluation namespace with variable values (for bc context)
#' @param settings Model settings (for setting base values)
#' @param include_base_case Logical. Ensure base case value is in the grid? (default: TRUE)
#' @return Numeric vector of values
#' @keywords internal
generate_twsa_values <- function(param, namespace = NULL, settings = NULL,
                                  include_base_case = TRUE) {


  # Helper to check if bc_value is already in values (with floating point tolerance)
  is_bc_in_values <- function(values, bc_value) {
    if (length(values) == 0 || is.null(bc_value)) return(FALSE)
    # Use relative tolerance based on the range of values
    val_range <- max(values) - min(values)
    tol <- if (val_range > 0) val_range * 1e-9 else abs(bc_value) * 1e-9
    tol <- max(tol, .Machine$double.eps * 100)  # Minimum absolute tolerance
    any(abs(values - bc_value) < tol)
  }

  # Helper to add bc_value to values if not present (with deduplication)
  add_bc_to_values <- function(values, bc_value) {
    if (is_bc_in_values(values, bc_value)) {
      return(sort(values))
    }
    sort(c(values, bc_value))
  }

  if (param$param_type == "variable") {
    # Get base case value from namespace
    bc_value <- if (!is.null(namespace)) namespace[param$name] else NULL

    if (param$type == "range") {
      # Evaluate min and max formulas
      if (!is.null(namespace)) {
        ns_clone <- clone_namespace(namespace)
        if (!is.null(bc_value)) ns_clone$env$bc <- bc_value
        min_val <- eval_formula(param$min, ns_clone)
        max_val <- eval_formula(param$max, ns_clone)
      } else {
        min_val <- as.numeric(as.character(param$min))
        max_val <- as.numeric(as.character(param$max))
      }
      values <- seq(from = min_val, to = max_val, length.out = param$steps)

      # Include base case if within range
      if (include_base_case && !is.null(bc_value) &&
          bc_value >= min_val && bc_value <= max_val) {
        values <- add_bc_to_values(values, bc_value)
      }

    } else if (param$type == "radius") {
      # Evaluate radius formula
      if (!is.null(namespace)) {
        ns_clone <- clone_namespace(namespace)
        if (!is.null(bc_value)) ns_clone$env$bc <- bc_value
        radius_val <- eval_formula(param$radius, ns_clone)
      } else {
        radius_val <- as.numeric(as.character(param$radius))
      }
      # Create symmetric range around base case
      if (is.null(bc_value)) {
        stop(glue("Cannot evaluate radius for variable '{param$name}': no base case value available"),
             call. = FALSE)
      }
      values <- seq(from = bc_value - radius_val, to = bc_value + radius_val,
                    length.out = param$steps * 2 + 1)
      # Radius type always includes base case by construction (centered on bc)

    } else if (param$type == "custom") {
      # Evaluate custom values expression
      if (!is.null(namespace)) {
        ns_clone <- clone_namespace(namespace)
        if (!is.null(bc_value)) ns_clone$env$bc <- bc_value
        values <- eval_formula(param$values, ns_clone)
      } else {
        # Try to evaluate as R expression
        values <- eval(parse(text = as.character(param$values)))
      }
      # For custom, include base case if requested and within range
      if (include_base_case && !is.null(bc_value) && length(values) > 0 &&
          bc_value >= min(values) && bc_value <= max(values)) {
        values <- add_bc_to_values(values, bc_value)
      }
    }

  } else {
    # Setting parameter
    bc_value <- if (!is.null(settings)) settings[[param$name]] else NULL

    if (param$type == "range") {
      min_val <- param$min
      max_val <- param$max
      values <- seq(from = min_val, to = max_val, length.out = param$steps)

      # Include base case if within range
      if (include_base_case && !is.null(bc_value) &&
          bc_value >= min_val && bc_value <= max_val) {
        values <- add_bc_to_values(values, bc_value)
      }

    } else if (param$type == "radius") {
      if (is.null(bc_value)) {
        stop(glue("Cannot evaluate radius for setting '{param$name}': no base value available"),
             call. = FALSE)
      }
      values <- seq(from = bc_value - param$radius, to = bc_value + param$radius,
                    length.out = param$steps * 2 + 1)
      # Radius type always includes base case by construction

    } else if (param$type == "custom") {
      values <- param$values
      # For custom, include base case if requested and within range
      if (include_base_case && !is.null(bc_value) && length(values) > 0 &&
          bc_value >= min(values) && bc_value <= max(values)) {
        values <- add_bc_to_values(values, bc_value)
      }
    }
  }

  return(values)
}

#' Build TWSA Segments with Overrides
#'
#' Creates a unified segments tibble for all TWSA runs, encoding all deviations
#' from base case in parameter_overrides and setting_overrides columns.
#'
#' @param model Parsed model object
#' @return Tibble with all segments for all TWSA runs, with run_id,
#'   parameter_overrides, setting_overrides, and twsa metadata columns
#' @keywords internal
build_twsa_segments <- function(model, progress = NULL) {
  # Get base segments (strategy x group combinations)
  base_segments <- get_segments(model)
  base_segments <- apply_override_categories(
    model, base_segments
  )

  # Enrich with evaluated variables for bc context
  # (per-segment ticks for progress)
  n_base <- nrow(base_segments)
  enriched_list <- vector("list", n_base)
  for (i in seq_len(n_base)) {
    enriched_list[[i]] <- prepare_segment_for_sampling(
      model, base_segments[i, ]
    )
    if (!is.null(progress)) progress(amount = 8L)
  }
  enriched_segments <- bind_rows(enriched_list)

  n_segments <- nrow(enriched_segments)
  all_segments <- list()
  run_id <- 1

  # Run 1: Base case - all segments with override category values as baseline
  base_case_segments <- enriched_segments %>%
    mutate(
      run_id = run_id,
      twsa_name = NA_character_,
      x_param_name = NA_character_,
      y_param_name = NA_character_,
      x_value = NA_real_,
      y_value = NA_real_
    )
  # Initialize override columns if not present (from apply_override_categories)
  if (!"parameter_overrides" %in% names(base_case_segments)) {
    base_case_segments$parameter_overrides <- vector("list", n_segments)
    for (i in seq_len(n_segments)) {
      base_case_segments$parameter_overrides[[i]] <- list()
    }
  }
  if (!"setting_overrides" %in% names(base_case_segments)) {
    base_case_segments$setting_overrides <- vector("list", n_segments)
    for (i in seq_len(n_segments)) {
      base_case_segments$setting_overrides[[i]] <- list()
    }
  }
  all_segments[[length(all_segments) + 1]] <- base_case_segments
  run_id <- run_id + 1

  # Process each TWSA analysis
  for (twsa in model$twsa_analyses) {
    if (length(twsa$parameters) != 2) {
      next  # Skip incomplete TWSA (should be caught by validation)
    }

    x_param <- twsa$parameters[[1]]
    y_param <- twsa$parameters[[2]]

    # Generate values and base case per segment (each segment may have different bc value)
    # This mirrors the DSA pattern for strategy/group-specific parameters
    x_values_per_segment <- vector("list", n_segments)
    y_values_per_segment <- vector("list", n_segments)
    x_bc_per_segment <- vector("list", n_segments)
    y_bc_per_segment <- vector("list", n_segments)

    for (seg_idx in seq_len(n_segments)) {
      segment <- enriched_segments[seg_idx, ]
      seg_model <- apply_setting_overrides(segment, model)
      seg_strategy <- segment$strategy[[1]]
      seg_group <- segment$group[[1]]
      seg_ns <- segment$eval_vars[[1]]

      # Check if x_param applies to this segment
      x_applies <- TRUE
      if (x_param$param_type == "variable") {
        if (!is.null(x_param$strategy) && x_param$strategy != "" && x_param$strategy != seg_strategy) {
          x_applies <- FALSE
        }
        if (!is.null(x_param$group) && x_param$group != "" && x_param$group != seg_group) {
          x_applies <- FALSE
        }
      }

      # Check if y_param applies to this segment
      y_applies <- TRUE
      if (y_param$param_type == "variable") {
        if (!is.null(y_param$strategy) && y_param$strategy != "" && y_param$strategy != seg_strategy) {
          y_applies <- FALSE
        }
        if (!is.null(y_param$group) && y_param$group != "" && y_param$group != seg_group) {
          y_applies <- FALSE
        }
      }

      # Generate values using this segment's namespace (for correct bc reference)
      if (x_applies) {
        x_values_per_segment[[seg_idx]] <- generate_twsa_values(
          x_param, seg_ns, seg_model$settings,
          include_base_case = x_param$include_base_case %||% TRUE
        )
        x_bc_per_segment[[seg_idx]] <- if (x_param$param_type == "variable") {
          seg_ns[x_param$name]
        } else {
          seg_model$settings[[x_param$name]]
        }
      }

      if (y_applies) {
        y_values_per_segment[[seg_idx]] <- generate_twsa_values(
          y_param, seg_ns, seg_model$settings,
          include_base_case = y_param$include_base_case %||% TRUE
        )
        y_bc_per_segment[[seg_idx]] <- if (y_param$param_type == "variable") {
          seg_ns[y_param$name]
        } else {
          seg_model$settings[[y_param$name]]
        }
      }
      if (!is.null(progress)) progress(amount = 8L)
    }

    # For grid creation, use the first applicable segment's values
    # (all applicable segments should have the same number of steps)
    x_values_for_grid <- NULL
    y_values_for_grid <- NULL
    for (seg_idx in seq_len(n_segments)) {
      if (is.null(x_values_for_grid) && !is.null(x_values_per_segment[[seg_idx]])) {
        x_values_for_grid <- x_values_per_segment[[seg_idx]]
      }
      if (is.null(y_values_for_grid) && !is.null(y_values_per_segment[[seg_idx]])) {
        y_values_for_grid <- y_values_per_segment[[seg_idx]]
      }
    }

    # Create grid indices
    n_x <- length(x_values_for_grid)
    n_y <- length(y_values_for_grid)
    grid_indices <- expand_grid(
      x_idx = seq_len(n_x),
      y_idx = seq_len(n_y)
    )
    n_grid <- nrow(grid_indices)

    # Pre-compute fallback bc values (once per TWSA)
    x_bc_fallback <- x_bc_per_segment[[
      which(!sapply(x_bc_per_segment, is.null))[1]
    ]]
    y_bc_fallback <- y_bc_per_segment[[
      which(!sapply(y_bc_per_segment, is.null))[1]
    ]]

    # Build value lookup matrices [seg_idx, value_idx]
    x_val_lookup <- matrix(
      NA_real_, nrow = n_segments, ncol = n_x
    )
    y_val_lookup <- matrix(
      NA_real_, nrow = n_segments, ncol = n_y
    )
    x_bc_vec <- numeric(n_segments)
    y_bc_vec <- numeric(n_segments)
    x_has_vals <- logical(n_segments)
    y_has_vals <- logical(n_segments)
    for (si in seq_len(n_segments)) {
      xv <- x_values_per_segment[[si]]
      x_has_vals[si] <- !is.null(xv)
      x_val_lookup[si, ] <- if (
        !is.null(xv) && x_param$param_type == "variable"
      ) xv else x_values_for_grid
      yv <- y_values_per_segment[[si]]
      y_has_vals[si] <- !is.null(yv)
      y_val_lookup[si, ] <- if (
        !is.null(yv) && y_param$param_type == "variable"
      ) yv else y_values_for_grid
      x_bc_vec[si] <- x_bc_per_segment[[si]] %||%
        x_bc_fallback
      y_bc_vec[si] <- y_bc_per_segment[[si]] %||%
        y_bc_fallback
    }

    # Full expansion: grid_row x seg_idx
    expansion <- expand_grid(
      .grid_row = seq_len(n_grid),
      .seg_idx = seq_len(n_segments)
    )
    n_total <- nrow(expansion)
    x_idx_vec <- grid_indices$x_idx[expansion$.grid_row]
    y_idx_vec <- grid_indices$y_idx[expansion$.grid_row]
    seg_idx_vec <- expansion$.seg_idx

    # Scalar columns via matrix indexing
    run_id_vec <- run_id - 1L + expansion$.grid_row
    x_value_vec <- x_val_lookup[
      cbind(seg_idx_vec, x_idx_vec)
    ]
    y_value_vec <- y_val_lookup[
      cbind(seg_idx_vec, y_idx_vec)
    ]
    x_bc_value_vec <- x_bc_vec[seg_idx_vec]
    y_bc_value_vec <- y_bc_vec[seg_idx_vec]

    # Build override lists
    has_po <- "parameter_overrides" %in%
      names(enriched_segments)
    has_so <- "setting_overrides" %in%
      names(enriched_segments)
    x_is_var <- x_param$param_type == "variable"
    y_is_var <- y_param$param_type == "variable"
    x_is_set <- x_param$param_type == "setting"
    y_is_set <- y_param$param_type == "setting"

    po_list <- vector("list", n_total)
    so_list <- vector("list", n_total)
    for (i in seq_len(n_total)) {
      si <- seg_idx_vec[i]
      po <- if (has_po) {
        enriched_segments$parameter_overrides[[si]]
      } else {
        list()
      }
      so <- if (has_so) {
        enriched_segments$setting_overrides[[si]]
      } else {
        list()
      }
      if (x_is_var && x_has_vals[si]) {
        po[[x_param$name]] <- x_value_vec[i]
      } else if (x_is_set) {
        so[[x_param$name]] <- x_value_vec[i]
      }
      if (y_is_var && y_has_vals[si]) {
        po[[y_param$name]] <- y_value_vec[i]
      } else if (y_is_set) {
        so[[y_param$name]] <- y_value_vec[i]
      }
      po_list[[i]] <- po
      so_list[[i]] <- so
    }

    # Single tibble construction for this TWSA
    twsa_segs <- enriched_segments[seg_idx_vec, ]
    twsa_segs$run_id <- run_id_vec
    twsa_segs$twsa_name <- twsa$name
    twsa_segs$x_param_name <- x_param$name
    twsa_segs$y_param_name <- y_param$name
    twsa_segs$x_value <- x_value_vec
    twsa_segs$y_value <- y_value_vec
    twsa_segs$x_bc_value <- x_bc_value_vec
    twsa_segs$y_bc_value <- y_bc_value_vec
    twsa_segs$parameter_overrides <- po_list
    twsa_segs$setting_overrides <- so_list

    all_segments[[length(all_segments) + 1]] <- twsa_segs
    run_id <- run_id + n_grid
    if (!is.null(progress)) progress(amount = 8L)
  }

  bind_rows(all_segments)
}

#' Generate TWSA Metadata
#'
#' Creates metadata tibble describing each TWSA run for use in analysis functions.
#'
#' @param model Parsed model object
#' @param segments Segments tibble from build_twsa_segments()
#' @return Tibble with columns: run_id, twsa_name, x_param_name, y_param_name,
#'   x_param_display_name, y_param_display_name, x_value, y_value, x_bc_value, y_bc_value
#' @keywords internal
generate_twsa_metadata <- function(model, segments) {
  # Extract unique run_ids with their metadata
  metadata <- segments %>%
    distinct(
      .data$run_id,
      .data$twsa_name,
      .data$x_param_name,
      .data$y_param_name,
      .data$x_value,
      .data$y_value
    )

  # Add x_bc_value and y_bc_value if present
  if ("x_bc_value" %in% names(segments)) {
    bc_data <- segments %>%
      distinct(.data$run_id, .data$x_bc_value, .data$y_bc_value)
    metadata <- metadata %>%
      left_join(bc_data, by = "run_id")
  }

  # Add display names for parameters
  metadata <- metadata %>%
    mutate(
      x_param_display_name = map_chr(.data$x_param_name, function(name) {
        if (is.na(name)) return(NA_character_)
        # Look up display name from TWSA spec or variable definition
        for (twsa in model$twsa_analyses) {
          for (param in twsa$parameters) {
            if (param$name == name) {
              if (!is.null(param$display_name)) return(param$display_name)
            }
          }
        }
        # Fall back to variable display name
        var_row <- model$variables %>% filter(.data$name == name) %>% slice(1)
        if (nrow(var_row) > 0 && !is.na(var_row$display_name)) {
          return(var_row$display_name)
        }
        return(name)
      }),
      y_param_display_name = map_chr(.data$y_param_name, function(name) {
        if (is.na(name)) return(NA_character_)
        # Look up display name from TWSA spec or variable definition
        for (twsa in model$twsa_analyses) {
          for (param in twsa$parameters) {
            if (param$name == name) {
              if (!is.null(param$display_name)) return(param$display_name)
            }
          }
        }
        # Fall back to variable display name
        var_row <- model$variables %>% filter(.data$name == name) %>% slice(1)
        if (nrow(var_row) > 0 && !is.na(var_row$display_name)) {
          return(var_row$display_name)
        }
        return(name)
      })
    )

  # Mark base case
  metadata <- metadata %>%
    mutate(is_base_case = is.na(.data$twsa_name))

  metadata
}

#' Run Two-Way Sensitivity Analysis
#'
#' Runs two-way sensitivity analysis by executing the model for each grid point
#' defined in the TWSA specifications. Each TWSA analysis creates a grid of
#' X values × Y values, and the model is run for each combination.
#'
#' @param model An openqaly model object with TWSA specifications
#' @param vbp_price_variable Name of the price variable to vary for VBP analysis.
#'   If NULL (default), standard TWSA without VBP is performed.
#' @param vbp_intervention Intervention strategy name for VBP analysis.
#'   Required if vbp_price_variable is specified.
#' @param vbp_outcome_summary Outcome summary name for VBP analysis.
#'   Defaults to "total_qalys" from model metadata if available.
#' @param vbp_cost_summary Cost summary name for VBP analysis.
#'   Defaults to "total_cost" from model metadata if available.
#' @param progress Optional progress callback function. Called with
#'   \code{progress(total = N)} to declare total units, then
#'   \code{progress(amount = K)} for each completed unit.
#' @param ... Additional arguments passed to run_segment
#' @return Results list with segments and aggregated results (includes run_id dimension).
#'   When VBP is enabled, also includes twsa_vbp_equations tibble.
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("cost_tx", 1000) |>
#'   add_variable("efficacy", 0.8) |>
#'   add_twsa("Cost vs Efficacy") |>
#'   add_twsa_variable("Cost vs Efficacy", "cost_tx",
#'     type = "range", min = 500, max = 1500, steps = 5) |>
#'   add_twsa_variable("Cost vs Efficacy", "efficacy",
#'     type = "radius", radius = 0.1, steps = 3)
#'
#' results <- run_twsa(model)
#' twsa_heatmap_plot(results, "total_qalys")
#' }
run_twsa <- function(model,
                     vbp_price_variable = NULL,
                     vbp_intervention = NULL,
                     vbp_outcome_summary = NULL,
                     vbp_cost_summary = NULL,
                     progress = NULL,
                     keep_diagnostics = FALSE,
                     ...) {
  # Finalize builders (convert to openqaly model)
  if ("oq_model_builder" %in% class(model)) {
    model <- normalize_and_validate_model(model, preserve_builder = FALSE)
  }

  # Parse model
  parsed_model <- parse_model(model, ...)

  # Validate TWSA specifications
  validate_twsa_spec(parsed_model)

  # Fall back to model$vbp for VBP parameters if not provided
  if (is.null(vbp_price_variable) && !is.null(model$vbp)) {
    vbp_price_variable <- model$vbp$price_variable
    if (is.null(vbp_intervention)) vbp_intervention <- model$vbp$intervention_strategy
    if (is.null(vbp_outcome_summary)) vbp_outcome_summary <- model$vbp$outcome_summary
    if (is.null(vbp_cost_summary)) vbp_cost_summary <- model$vbp$cost_summary
  }

  # Check if VBP mode is enabled
  vbp_enabled <- !is.null(vbp_price_variable)

  # Validate VBP parameters if enabled
  vbp_spec <- NULL
  if (vbp_enabled) {
    # Require intervention if price variable specified
    if (is.null(vbp_intervention)) {
      stop("vbp_intervention is required when vbp_price_variable is specified",
           call. = FALSE)
    }

    # Default outcome/cost summaries from metadata if not specified
    if (is.null(vbp_outcome_summary)) {
      if (!is.null(parsed_model$metadata$outcome_summary)) {
        vbp_outcome_summary <- parsed_model$metadata$outcome_summary
      } else {
        vbp_outcome_summary <- "total_qalys"
      }
    }
    if (is.null(vbp_cost_summary)) {
      if (!is.null(parsed_model$metadata$cost_summary)) {
        vbp_cost_summary <- parsed_model$metadata$cost_summary
      } else {
        vbp_cost_summary <- "total_cost"
      }
    }

    # Validate VBP spec
    validate_twsa_vbp_spec(
      parsed_model,
      vbp_price_variable,
      vbp_intervention,
      vbp_outcome_summary,
      vbp_cost_summary
    )

    # Create VBP spec
    vbp_spec <- list(
      price_variable = vbp_price_variable,
      intervention_strategy = vbp_intervention,
      outcome_summary = vbp_outcome_summary,
      cost_summary = vbp_cost_summary,
      price_values = c(0, 1000, 2000)
    )
  }

  # Estimate segment count and compute progress total
  n_base <- nrow(get_segments(parsed_model))
  n_twsa <- length(parsed_model$twsa_analyses)
  est_segments <- n_base
  for (ts in parsed_model$twsa_analyses) {
    if (length(ts$parameters) == 2) {
      est_nx <- estimate_twsa_param_size(
        ts$parameters[[1]]
      )
      est_ny <- estimate_twsa_param_size(
        ts$parameters[[2]]
      )
      est_segments <- est_segments +
        est_nx * est_ny * n_base
    }
  }
  if (vbp_enabled) est_segments <- est_segments * 3L

  # Build ticks weighted at 8 each (same as a segment)
  # so build phase is visible in the progress bar
  build_steps <- 1L + n_base +
    n_twsa * n_base + n_twsa
  build_ticks <- build_steps * 8L

  if (!is.null(progress)) {
    progress(
      total = build_ticks + est_segments * 8L + 2L
    )
    progress(amount = 8L)
  }

  # Build segments (ticks fire inside build)
  if (vbp_enabled) {
    all_segments <- build_twsa_vbp_segments(
      parsed_model, vbp_spec, progress = progress
    )
  } else {
    all_segments <- build_twsa_segments(
      parsed_model, progress = progress
    )
  }

  # Correction if estimate was over actual
  actual_segments <- nrow(all_segments)
  if (!is.null(progress) &&
      est_segments > actual_segments) {
    progress(
      amount = (est_segments - actual_segments) * 8L
    )
  }

  # Drop build-only columns before execution
  # (run_segment recreates namespace and variables)
  all_segments$eval_vars <- NULL
  all_segments$uneval_vars <- NULL

  segment_list <- all_segments %>%
    rowwise() %>%
    group_split()

  results <- future_map(
    segment_list,
    function(segment) {
      run_segment(
        segment, parsed_model, ...,
        .progress_callback = progress,
        .diagnostics_policy = if (
          isTRUE(keep_diagnostics)
        ) "all" else "base_case"
      )
    },
    .progress = is.null(progress),
    .options = furrr_options(
      seed = 1, packages = .packages()
    )
  ) %>% bind_rows()

  # Generate TWSA metadata
  if (vbp_enabled) {
    twsa_segments_for_meta <- all_segments %>%
      filter(.data$vbp_price_level == 1)
  } else {
    twsa_segments_for_meta <- all_segments
  }
  twsa_metadata <- generate_twsa_metadata(
    parsed_model, twsa_segments_for_meta
  )

  # Aggregate
  aggregated <- aggregate_segments(
    results, parsed_model
  )
  if (!is.null(progress)) progress(amount = 1L)

  # Return results
  res <- list()
  res$segments <- results
  res$aggregated <- aggregated
  res$metadata <- parsed_model$metadata
  res$twsa_metadata <- twsa_metadata

  if (vbp_enabled) {
    res$twsa_vbp_equations <- analyze_twsa_vbp_results(
      results,
      aggregated,
      vbp_spec,
      twsa_metadata,
      parsed_model
    )
    res$vbp_spec <- vbp_spec
  }

  if (!is.null(progress)) progress(amount = 1L)
  class(res) <- c("twsa_results", "list")
  res
}

#' Extract TWSA Summary Values
#'
#' Helper function to extract summary values from TWSA results across all runs.
#' Efficiently extracts summaries without processing full trace data.
#'
#' @param results TWSA results object from run_twsa()
#' @param summary_name Name of the summary to extract (e.g., "total_qalys", "total_cost")
#' @param value_type Type of value: "all", "cost", or "outcome"
#' @param groups Group selection: "overall" (default), "all" (overall + all groups),
#'   "all_groups" (all groups without overall), or specific group name(s)
#' @param strategies Character vector of strategy names to include (NULL for all).
#' @param interventions Character vector of intervention strategy name(s).
#' @param comparators Character vector of comparator strategy name(s).
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A tibble with columns: run_id, twsa_name, x_param_name, y_param_name,
#'   x_value, y_value, strategy, group, amount
#'
#' @importFrom tidyr unnest
#' @keywords internal
extract_twsa_summaries <- function(results,
                                   summary_name,
                                   value_type = c("all", "cost", "outcome"),
                                   groups = "overall",
                                   strategies = NULL,
                                   interventions = NULL,
                                   comparators = NULL,
                                   discounted = TRUE) {

  value_type <- match.arg(value_type)

  # Use standardized group selection
  source_data <- select_source_data(groups, results)

  # Filter out VBP sub-simulations (keep only price_level 1 or no price_level)
  if ("vbp_price_level" %in% names(source_data)) {
    source_data <- source_data %>%
      filter(is.na(.data$vbp_price_level) | .data$vbp_price_level == 1)
  }

  # Determine which strategies to include
  strategies_to_include <- strategies

  # If interventions/comparators specified, resolve which strategies are needed
  if (is.null(strategies_to_include) && (!is.null(interventions) || !is.null(comparators))) {
    all_available <- unique(source_data$strategy)

    if (!is.null(interventions) && !is.null(comparators)) {
      # Both specified: only need the explicitly listed ones
      strategies_to_include <- unique(c(interventions, comparators))
    } else {
      # One or the other: need all strategies
      strategies_to_include <- all_available
    }
  }

  # Filter to specified strategies
  if (!is.null(strategies_to_include)) {
    source_data <- source_data %>%
      filter(.data$strategy %in% strategies_to_include)
  }

  # Check for run_id column
  if (!"run_id" %in% names(source_data)) {
    stop("No run_id column found. Ensure model was run with run_twsa().",
         call. = FALSE)
  }

  # Determine which summary column to use (nested summaries)
  summary_col <- if (discounted) "summaries_discounted" else "summaries"

  # Check if summary column exists
  if (!summary_col %in% names(source_data)) {
    return(tibble(
      run_id = source_data$run_id,
      strategy = source_data$strategy,
      group = source_data$group,
      amount = NA_real_
    ))
  }

  # Vectorized extraction and aggregation
  result <- source_data %>%
    select("run_id", "strategy", "group", summary_data = all_of(summary_col)) %>%
    unnest("summary_data", keep_empty = TRUE) %>%
    filter(.data$summary == summary_name | is.na(.data$summary))

  # Filter by value type if specified
  if (value_type != "all") {
    if (!is.null(results$metadata) && !is.null(results$metadata$values)) {
      matching_values <- results$metadata$values %>%
        filter(.data$type == value_type) %>%
        pull(.data$name)
      result <- result %>%
        filter(.data$value %in% matching_values | is.na(.data$value))
    }
  }

  # Aggregate by run_id, strategy, group
  result <- result %>%
    group_by(.data$run_id, .data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  # Join with TWSA metadata
  result <- result %>%
    left_join(results$twsa_metadata, by = "run_id")

  result
}
