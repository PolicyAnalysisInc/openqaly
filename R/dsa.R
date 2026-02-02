#' Validate Deterministic Sensitivity Analysis Specification
#'
#' Validates that DSA specifications are consistent. Checks that variable
#' names exist in model$variables and setting names are valid.
#'
#' @param model Parsed model object
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_dsa_spec <- function(model) {
  errors <- character()

  # Check if there are any DSA specifications at all
  if (is.null(model$dsa_parameters) || length(model$dsa_parameters) == 0) {
    stop("No DSA specifications found. Use add_dsa_variable() or add_dsa_setting() to specify parameters to vary.", call. = FALSE)
  }

  # Validate each DSA parameter
  all_var_names <- model$variables$name
  valid_settings <- c(
    "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
    "discount_cost", "discount_outcomes", "half_cycle_method",
    "reduce_state_cycle", "days_per_year"
  )

  for (i in seq_along(model$dsa_parameters)) {
    param <- model$dsa_parameters[[i]]

    if (param$type == "variable") {
      # Check that variable exists in model
      if (!(param$name %in% all_var_names)) {
        errors <- c(errors, glue(
          "DSA variable '{param$name}' not found in model variables"
        ))
      }
    } else if (param$type == "setting") {
      # Check that setting name is valid
      if (!(param$name %in% valid_settings)) {
        errors <- c(errors, glue(
          "Invalid DSA setting name: '{param$name}'. ",
          "Valid settings: {paste(valid_settings, collapse=', ')}"
        ))
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  # Validate unique display names for DSA parameters
  # Build a list of effective display names by resolving DSA spec > variable > param name
  display_names <- character()
  param_descriptions <- character()

  for (i in seq_along(model$dsa_parameters)) {
    param <- model$dsa_parameters[[i]]

    # Resolve effective display name
    effective_name <- NULL

    if (param$type == "variable") {
      # Check DSA spec first
      if (!is.null(param$display_name)) {
        effective_name <- param$display_name
      } else {
        # Get from variable definition, filtering by strategy/group
        var_row <- model$variables %>% filter(.data$name == param$name)

        if (!is.na(param$strategy) && param$strategy != "") {
          var_row <- var_row %>% filter(.data$strategy == param$strategy)
        }
        if (!is.na(param$group) && param$group != "") {
          var_row <- var_row %>% filter(.data$group == param$group)
        }

        var_row <- var_row %>% slice(1)
        effective_name <- if (nrow(var_row) > 0) var_row$display_name else param$name
      }

      # Create description for error messages
      param_desc <- param$name
      if (!is.na(param$strategy) && param$strategy != "") {
        param_desc <- paste0(param_desc, ", strategy=", param$strategy)
      }
      if (!is.na(param$group) && param$group != "") {
        param_desc <- paste0(param_desc, ", group=", param$group)
      }
    } else {
      # Setting
      effective_name <- param$display_name %||% param$name
      param_desc <- param$name
    }

    display_names <- c(display_names, effective_name)
    param_descriptions <- c(param_descriptions, param_desc)
  }

  # Check for duplicates
  dup_names <- unique(display_names[duplicated(display_names)])
  if (length(dup_names) > 0) {
    dup_errors <- character()
    for (dup_name in dup_names) {
      dup_indices <- which(display_names == dup_name)
      dup_params <- param_descriptions[dup_indices]
      dup_errors <- c(dup_errors, glue(
        "Display name '{dup_name}' is used by multiple DSA parameters:\n  ",
        "{paste(dup_params, collapse = '\n  ')}\n",
        "Each DSA parameter must have a unique display name. ",
        "Specify different display names either in add_variable() or add_dsa_variable()."
      ))
    }
    stop(paste(dup_errors, collapse = "\n\n"), call. = FALSE)
  }

  return(TRUE)
}

#' Build DSA Segments with Overrides
#'
#' Creates a unified segments tibble for all DSA runs, encoding all deviations
#' from base case in parameter_overrides and setting_overrides columns.
#'
#' @param model Parsed model object
#' @return Tibble with all segments for all DSA runs, with run_id,
#'   parameter_overrides, and setting_overrides columns
#' @keywords internal
build_dsa_segments <- function(model) {
  # Get base segments (strategy x group combinations)
  base_segments <- get_segments(model)

  # Enrich with evaluated variables for bc context
  enriched_segments <- base_segments %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      prepare_segment_for_sampling(model, as_tibble(seg))
    }) %>%
    ungroup()

  all_segments <- list()
  run_id <- 1

  # Run 1: Base case - all segments with no overrides
  base_case_segments <- enriched_segments %>%
    mutate(
      run_id = run_id,
      parameter_overrides = map(1:n(), ~ list()),
      setting_overrides = map(1:n(), ~ list())
    )
  all_segments[[length(all_segments) + 1]] <- base_case_segments
  run_id <- run_id + 1

  # Process each DSA parameter in list order
  for (param in model$dsa_parameters) {
    if (param$type == "variable") {
      # Evaluate bounds per segment (each segment may have different bc value)
      # Initialize list with correct length (all NULL by default)
      bounds_per_segment <- vector("list", nrow(enriched_segments))

      for (seg_idx in 1:nrow(enriched_segments)) {
        segment <- enriched_segments[seg_idx, ]
        seg_strategy <- segment$strategy[[1]]
        seg_group <- segment$group[[1]]

        # Check if parameter applies to this segment
        applies <- TRUE
        if (!is.na(param$strategy) && param$strategy != "" && param$strategy != seg_strategy) {
          applies <- FALSE
        }
        if (!is.na(param$group) && param$group != "" && param$group != seg_group) {
          applies <- FALSE
        }

        if (applies) {
          # Clone namespace and set bc to this variable's base case value
          seg_ns <- clone_namespace(segment$eval_vars[[1]])
          seg_ns$env$bc <- seg_ns[param$name]

          # Evaluate bounds with bc and other variables available
          low_val <- eval_formula(param$low, seg_ns)
          high_val <- eval_formula(param$high, seg_ns)

          # Validate results
          if (is_oq_error(low_val)) {
            stop(glue("Failed to evaluate low bound for DSA variable '{param$name}': {low_val}"),
                 call. = FALSE)
          }
          if (is_oq_error(high_val)) {
            stop(glue("Failed to evaluate high bound for DSA variable '{param$name}': {high_val}"),
                 call. = FALSE)
          }
          if (!is.numeric(low_val) || length(low_val) != 1) {
            stop(glue("Low bound for DSA variable '{param$name}' must evaluate to a single numeric value"),
                 call. = FALSE)
          }
          if (!is.numeric(high_val) || length(high_val) != 1) {
            stop(glue("High bound for DSA variable '{param$name}' must evaluate to a single numeric value"),
                 call. = FALSE)
          }
          if (low_val >= high_val) {
            stop(glue("For DSA variable '{param$name}': low ({low_val}) must be less than high ({high_val})"),
                 call. = FALSE)
          }

          bounds_per_segment[[seg_idx]] <- list(low = low_val, high = high_val)
        }
        # else: leave as NULL (already initialized)
      }

      # Create runs for low and high variations
      for (variation in c("low", "high")) {
        seg_with_overrides <- enriched_segments %>%
          mutate(
            run_id = run_id,
            parameter_overrides = map(1:n(), function(i) {
              if (!is.null(bounds_per_segment[[i]])) {
                override_val <- if (variation == "low") bounds_per_segment[[i]]$low else bounds_per_segment[[i]]$high
                list_obj <- list()
                list_obj[[param$name]] <- override_val
                list_obj
              } else {
                list()  # Empty override for segments where parameter doesn't apply
              }
            }),
            setting_overrides = map(1:n(), ~ list())
          )

        all_segments[[length(all_segments) + 1]] <- seg_with_overrides
        run_id <- run_id + 1
      }

    } else if (param$type == "setting") {
      # Settings apply globally to all segments
      for (variation in c("low", "high")) {
        override_val <- if (variation == "low") param$low else param$high

        seg_with_override <- enriched_segments %>%
          mutate(
            run_id = run_id,
            parameter_overrides = map(1:n(), ~ list()),
            setting_overrides = map(1:n(), function(i) {
              list_obj <- list()
              list_obj[[param$name]] <- override_val
              list_obj
            })
          )

        all_segments[[length(all_segments) + 1]] <- seg_with_override
        run_id <- run_id + 1
      }
    }
  }

  bind_rows(all_segments)
}

#' Generate DSA Metadata from Segments
#'
#' Creates metadata tibble describing each DSA run for use in analysis functions.
#' This replaces the old generate_dsa_runs() output with metadata extracted from
#' the unified segments structure.
#'
#' @param model Parsed model object
#' @param segments Segments tibble from build_dsa_segments()
#' @return Tibble with columns: run_id, parameter, parameter_type, variation,
#'   parameter_display_name (compatible with old dsa_runs structure)
#' @keywords internal
generate_dsa_metadata_from_segments <- function(model, segments) {
  # Get unique run_ids
  run_ids <- unique(segments$run_id)

  metadata <- list()

  # For each run_id, determine what parameter it represents
  for (rid in run_ids) {
    if (rid == 1) {
      # Base case
      metadata[[length(metadata) + 1]] <- tibble(
        run_id = rid,
        parameter = "base",
        parameter_type = "base",
        variation = "base",
        parameter_display_name = "Base Case",
        strategy = NA_character_,
        group = NA_character_,
        override_value = NA_character_
      )
    } else {
      # Find a segment with this run_id that has non-empty overrides
      # (For strategy-specific params, only some segments will have overrides)
      run_segments <- segments %>% filter(.data$run_id == rid)

      # Find segment with non-empty param_overrides first
      seg_with_override <- NULL
      param_overrides <- list()
      setting_overrides <- list()

      for (i in seq_len(nrow(run_segments))) {
        seg_param <- run_segments$parameter_overrides[[i]]
        seg_setting <- run_segments$setting_overrides[[i]]

        if (length(seg_param) > 0) {
          seg_with_override <- run_segments[i, ]
          param_overrides <- seg_param
          break
        } else if (length(seg_setting) > 0 && is.null(seg_with_override)) {
          seg_with_override <- run_segments[i, ]
          setting_overrides <- seg_setting
        }
      }

      # Fall back to first segment if none have overrides
      seg <- if (!is.null(seg_with_override)) seg_with_override else run_segments %>% slice(1)

      if (length(param_overrides) > 0) {
        # Variable DSA
        param_name <- names(param_overrides)[1]
        param_value <- param_overrides[[1]]

        # Get segment's group and strategy to help match the correct DSA spec
        seg_group <- seg$group[[1]]
        seg_strategy <- seg$strategy[[1]]

        # Find the parameter in dsa_parameters to get display info
        # Match on name AND group/strategy if specified in the DSA spec
        param_spec <- NULL
        for (p in model$dsa_parameters) {
          if (p$type == "variable" && p$name == param_name) {
            # Check if this DSA spec is for the segment's group
            p_group <- if (!is.na(p$group) && p$group != "") p$group else ""
            seg_grp <- if (!is.na(seg_group) && seg_group != "_aggregated") seg_group else ""

            # Check if this DSA spec is for the segment's strategy
            p_strategy <- if (!is.na(p$strategy) && p$strategy != "") p$strategy else ""
            seg_strat <- if (!is.na(seg_strategy)) seg_strategy else ""

            # Match if: DSA has no restriction, or group matches, or strategy matches
            group_ok <- (p_group == "" || p_group == seg_grp)
            strategy_ok <- (p_strategy == "" || p_strategy == seg_strat)

            if (group_ok && strategy_ok) {
              param_spec <- p
              break
            }
          }
        }

        # Get display name: priority is DSA spec > variable definition > parameter name
        display_name <- NULL

        # First, check if DSA spec provides a display name
        if (!is.null(param_spec) && !is.null(param_spec$display_name)) {
          display_name <- param_spec$display_name
        }

        # If not, get from model variable, filtering by strategy/group if specified
        if (is.null(display_name)) {
          var_row <- model$variables %>% filter(.data$name == param_name)

          # Filter by strategy if specified in param_spec
          if (!is.null(param_spec) && !is.na(param_spec$strategy) && param_spec$strategy != "") {
            var_row <- var_row %>% filter(.data$strategy == param_spec$strategy)
          }

          # Filter by group if specified in param_spec
          if (!is.null(param_spec) && !is.na(param_spec$group) && param_spec$group != "") {
            var_row <- var_row %>% filter(.data$group == param_spec$group)
          }

          var_row <- var_row %>% slice(1)
          display_name <- if (nrow(var_row) > 0) var_row$display_name else param_name
        }

        # Determine variation based on param index
        # (This is approximate - we're inferring from run_id ordering)
        # Odd run_ids after base are "low", even are "high"
        variation <- if ((rid - 1) %% 2 == 1) "low" else "high"

        # Extract strategy and group from param_spec (use NA for unspecified)
        param_strategy <- if (!is.null(param_spec) && !is.na(param_spec$strategy) && param_spec$strategy != "") {
          param_spec$strategy
        } else {
          NA_character_
        }

        param_group <- if (!is.null(param_spec) && !is.na(param_spec$group) && param_spec$group != "") {
          param_spec$group
        } else {
          NA_character_
        }

        metadata[[length(metadata) + 1]] <- tibble(
          run_id = rid,
          parameter = param_name,
          parameter_type = "variable",
          variation = variation,
          parameter_display_name = display_name,
          strategy = param_strategy,
          group = param_group,
          override_value = as.character(param_value)
        )

      } else if (length(setting_overrides) > 0) {
        # Setting DSA
        setting_name <- names(setting_overrides)[1]
        setting_value <- setting_overrides[[1]]

        # Find the parameter in dsa_parameters to get display info
        param_spec <- NULL
        for (p in model$dsa_parameters) {
          if (p$type == "setting" && p$name == setting_name) {
            param_spec <- p
            break
          }
        }

        display_name <- if (!is.null(param_spec)) param_spec$display_name else setting_name

        # Determine variation
        variation <- if ((rid - 1) %% 2 == 1) "low" else "high"

        # Settings apply to all strategies and groups
        metadata[[length(metadata) + 1]] <- tibble(
          run_id = rid,
          parameter = setting_name,
          parameter_type = "setting",
          variation = variation,
          parameter_display_name = display_name,
          strategy = NA_character_,
          group = NA_character_,
          override_value = as.character(setting_value)
        )
      }
    }
  }

  bind_rows(metadata)
}

#' Run Deterministic Sensitivity Analysis
#'
#' Runs deterministic sensitivity analysis by executing the model multiple times
#' with different parameter values. DSA runs the model once for the base case,
#' then twice for each specified parameter (once at low value, once at high value).
#'
#' DSA variable bounds support flexible specification using:
#' - Literal values (e.g., 0.01, 0.05)
#' - The `bc` keyword to reference base case values (e.g., bc * 0.5)
#' - Other model variables (e.g., bc - 2 * cost_se)
#'
#' When VBP parameters are provided, DSA+VBP mode is activated. In this mode,
#' each DSA run includes 3 VBP price level sub-simulations to calculate the
#' VBP relationship at each parameter variation.
#'
#' @param model An openqaly model object with DSA specifications
#' @param vbp_price_variable Name of the price variable to vary for VBP analysis.
#'   If NULL (default), standard DSA without VBP is performed.
#' @param vbp_intervention Intervention strategy name for VBP analysis.
#'   Required if vbp_price_variable is specified.
#' @param vbp_outcome_summary Outcome summary name for VBP analysis.
#'   Defaults to "total_qalys" from model metadata if available.
#' @param vbp_cost_summary Cost summary name for VBP analysis.
#'   Defaults to "total_cost" from model metadata if available.
#' @param ... Additional arguments passed to run_segment
#' @return Results list with segments and aggregated results (includes run_id dimension).
#'   When VBP is enabled, also includes dsa_vbp_equations tibble with VBP equations
#'   for each DSA run.
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Basic DSA with literal values
#' model <- define_model("markov") %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05) %>%
#'   add_dsa_setting("discount_cost", low = 0, high = 5)
#'
#' dsa_results <- run_dsa(model)
#'
#' # Example 2: Using bc keyword for relative ranges
#' model <- define_model("markov") %>%
#'   add_variable("cost_tx", 1000) %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_dsa_variable("cost_tx", low = bc * 0.75, high = bc * 1.25) %>%
#'   add_dsa_variable("p_disease", low = bc * 0.5, high = bc * 1.5)
#'
#' dsa_results <- run_dsa(model)
#'
#' # Example 3: Using standard errors
#' model <- define_model("markov") %>%
#'   add_variable("cost_tx", 1000) %>%
#'   add_variable("cost_tx_se", 100) %>%
#'   add_variable("p_disease", 0.03) %>%
#'   add_variable("p_disease_se", 0.005) %>%
#'   add_dsa_variable("cost_tx",
#'                    low = bc - 1.96 * cost_tx_se,
#'                    high = bc + 1.96 * cost_tx_se) %>%
#'   add_dsa_variable("p_disease",
#'                    low = bc - 1.96 * p_disease_se,
#'                    high = bc + 1.96 * p_disease_se)
#'
#' dsa_results <- run_dsa(model)
#'
#' # Example 4: DSA with VBP add-on
#' model <- define_model("markov") %>%
#'   add_variable("cost_tx", 1000) %>%
#'   add_dsa_variable("cost_tx", low = bc * 0.75, high = bc * 1.25) %>%
#'   # ... other model setup ...
#'   add_strategy("treatment") %>%
#'   add_strategy("comparator")
#'
#' dsa_vbp_results <- run_dsa(
#'   model,
#'   vbp_price_variable = "cost_tx",
#'   vbp_intervention = "treatment"
#' )
#'
#' # Get VBP at base case
#' vbp_base <- calculate_dsa_vbp_price(dsa_vbp_results, wtp = 50000)
#'
#' # Get VBP for a specific DSA variation
#' vbp_low <- calculate_dsa_vbp_price(dsa_vbp_results, wtp = 50000, run_id = 2)
#' }
run_dsa <- function(model,
                    vbp_price_variable = NULL,
                    vbp_intervention = NULL,
                    vbp_outcome_summary = NULL,
                    vbp_cost_summary = NULL,
                    ...) {
  # Finalize builders (convert to openqaly model)
  if ("oq_model_builder" %in% class(model)) {
    model <- normalize_and_validate_model(model, preserve_builder = FALSE)
  }

  # Parse model
  parsed_model <- parse_model(model, ...)

  # Validate DSA specifications
  validate_dsa_spec(parsed_model)

  # Check if VBP mode is enabled
  vbp_enabled <- !is.null(vbp_price_variable)

  # Validate VBP parameters if enabled
  vbp_spec <- NULL
  if (vbp_enabled) {
    # Require intervention if price variable specified
    if (is.null(vbp_intervention)) {
      stop("vbp_intervention is required when vbp_price_variable is specified")
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
    validate_dsa_vbp_spec(
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

  # Build segments based on mode
  if (vbp_enabled) {
    # Build DSA+VBP segments (includes vbp_price_level)
    all_segments <- build_dsa_vbp_segments(parsed_model, vbp_spec)
  } else {
    # Build standard DSA segments
    all_segments <- build_dsa_segments(parsed_model)
  }

  # Get unique run_ids for metadata generation
  run_ids <- unique(all_segments$run_id)

  # Run all segments in parallel with progress bar
  results <- all_segments %>%
    rowwise() %>%
    group_split() %>%
    future_map(function(segment) run_segment(segment, parsed_model, ...),
               .progress = TRUE, .options = furrr_options(seed = 1)) %>%
    bind_rows()

  # Generate metadata for DSA runs (for analysis functions and plots)
  # Use only base DSA segments (before VBP expansion) for metadata
  if (vbp_enabled) {
    dsa_segments_for_meta <- all_segments %>%
      filter(.data$vbp_price_level == 1)
  } else {
    dsa_segments_for_meta <- all_segments
  }
  dsa_metadata <- generate_dsa_metadata_from_segments(parsed_model, dsa_segments_for_meta)

  # Aggregate by run_id + strategy (and vbp_price_level if present)
  aggregated <- aggregate_segments(results, parsed_model)

  # Return results
  res <- list()
  res$segments <- results
  res$aggregated <- aggregated
  res$metadata <- parsed_model$metadata
  res$dsa_metadata <- dsa_metadata

  # Add VBP analysis if enabled
  if (vbp_enabled) {
    res$dsa_vbp_equations <- analyze_dsa_vbp_results(
      results,
      aggregated,
      vbp_spec,
      dsa_metadata,
      parsed_model
    )
    res$vbp_spec <- vbp_spec
  }

  return(res)
}


#' Extract DSA Summary Values
#'
#' Helper function to extract summary values from DSA results across all runs.
#' Efficiently extracts summaries without processing full trace data, similar to
#' extract_psa_summaries but for DSA run_id dimension.
#'
#' @param results DSA results object from run_dsa()
#' @param summary_name Name of the summary to extract (e.g., "total_qalys", "total_cost")
#' @param value_type Type of value: "all", "cost", or "outcome"
#' @param groups Group selection: "overall" (default), "all" (overall + all groups),
#'   "all_groups" (all groups without overall), or specific group name(s)
#' @param strategies Character vector of strategy names to include (NULL for all).
#'   Mutually exclusive with interventions/comparators.
#' @param interventions Character vector of intervention strategy name(s). Used with
#'   comparators to determine which strategies to extract. When both interventions and
#'   comparators are specified, only those strategies are extracted. When only interventions
#'   is specified, all strategies are extracted (for "interventions vs all others" comparisons).
#' @param comparators Character vector of comparator strategy name(s). Used with
#'   interventions to determine which strategies to extract. When both interventions and
#'   comparators are specified, only those strategies are extracted. When only comparators
#'   is specified, all strategies are extracted (for "all others vs comparators" comparisons).
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A tibble with columns: run_id, strategy, group, parameter,
#'   parameter_display_name, variation, amount
#'
#' @keywords internal
extract_dsa_summaries <- function(results,
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
  # This ensures extract_dsa_summaries returns one row per DSA run, not 3 per run
  if ("vbp_price_level" %in% names(source_data)) {
    source_data <- source_data %>%
      filter(is.na(.data$vbp_price_level) | .data$vbp_price_level == 1)
  }

  # Determine which strategies to include
  strategies_to_include <- strategies

  # If interventions/comparators specified, resolve which strategies are needed
  if (is.null(strategies_to_include) && (!is.null(interventions) || !is.null(comparators))) {
    # Get all available strategies (technical names)
    all_available <- unique(source_data$strategy)

    if (!is.null(interventions) && !is.null(comparators)) {
      # Both specified: only need the explicitly listed ones
      strategies_to_include <- unique(c(interventions, comparators))

      # Validate they exist
      missing <- setdiff(strategies_to_include, all_available)
      if (length(missing) > 0) {
        stop(sprintf(
          "Strategy(ies) not found: %s\nAvailable: %s",
          paste(missing, collapse = ", "),
          paste(all_available, collapse = ", ")
        ))
      }
    } else if (!is.null(interventions)) {
      # Intervention vs all others: need all strategies
      strategies_to_include <- all_available

      # Validate intervention exists
      missing <- setdiff(interventions, all_available)
      if (length(missing) > 0) {
        stop(sprintf(
          "Intervention strategy(ies) not found: %s\nAvailable: %s",
          paste(missing, collapse = ", "),
          paste(all_available, collapse = ", ")
        ))
      }
    } else {
      # Comparator vs all others: need all strategies
      strategies_to_include <- all_available

      # Validate comparator exists
      missing <- setdiff(comparators, all_available)
      if (length(missing) > 0) {
        stop(sprintf(
          "Comparator strategy(ies) not found: %s\nAvailable: %s",
          paste(missing, collapse = ", "),
          paste(all_available, collapse = ", ")
        ))
      }
    }
  }

  # Filter to selected strategies
  if (!is.null(strategies_to_include)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies_to_include, results$metadata)
    source_data <- source_data %>%
      filter(.data$strategy %in% strategies_to_include)
  }

  # Check for run_id column
  if (!"run_id" %in% names(source_data)) {
    stop("No run_id column found. Ensure model was run with run_dsa().")
  }

  # Determine which summary column to use
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

  # Vectorized extraction and aggregation (like extract_psa_summaries)
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

  # Join with DSA metadata to add parameter info
  # Note: We do NOT filter by strategy/group match here because:
  # 1. When calculating differences, we need ALL strategies' data for each run
  # 2. For strategy-specific parameters, non-applying strategies keep their base value
  # 3. The strategy/group columns in metadata are for display/labeling purposes only
  if (!is.null(results$dsa_metadata)) {
    result <- result %>%
      left_join(results$dsa_metadata, by = "run_id") %>%
      select("run_id", strategy = "strategy.x", group = "group.x", "parameter",
             "parameter_display_name", "parameter_type", "variation", "amount",
             param_strategy = "strategy.y", param_group = "group.y")
  } else {
    # If no metadata, add placeholder columns
    result <- result %>%
      mutate(
        parameter = NA_character_,
        parameter_display_name = NA_character_,
        parameter_type = NA_character_,
        variation = NA_character_,
        param_strategy = NA_character_,
        param_group = NA_character_
      )
  }

  result
}
