#' Validate Scenario Analysis Specification
#'
#' Validates that scenario specifications are consistent. Checks that variable
#' names exist in model$variables and setting names are valid.
#'
#' @param model Parsed model object
#' @return TRUE if valid, stops with error if invalid
#' @keywords internal
validate_scenario_spec <- function(model) {
  errors <- character()

  # Check if there are any scenarios defined
  if (is.null(model$scenarios) || length(model$scenarios) == 0) {
    stop("No scenarios defined. Use add_scenario() to define at least one scenario.",
         call. = FALSE)
  }

  # Valid setting names (same as DSA)
  valid_settings <- c(
    "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
    "discount_cost", "discount_outcomes", "half_cycle_method",
    "reduce_state_cycle", "days_per_year"
  )

  all_var_names <- model$variables$name

  # Check for duplicate scenario names
  scenario_names <- sapply(model$scenarios, function(s) s$name)
  dup_names <- unique(scenario_names[duplicated(scenario_names)])
  if (length(dup_names) > 0) {
    errors <- c(errors, glue(
      "Duplicate scenario names: {paste(dup_names, collapse=', ')}"
    ))
  }

  # Validate each scenario
  for (scenario in model$scenarios) {
    # Validate variable overrides
    for (override in scenario$variable_overrides) {
      if (!(override$name %in% all_var_names)) {
        errors <- c(errors, glue(
          "Scenario '{scenario$name}': variable '{override$name}' not found in model variables"
        ))
      }
    }

    # Validate setting overrides
    for (override in scenario$setting_overrides) {
      if (!(override$name %in% valid_settings)) {
        errors <- c(errors, glue(
          "Scenario '{scenario$name}': invalid setting name '{override$name}'. ",
          "Valid settings: {paste(valid_settings, collapse=', ')}"
        ))
      }
    }
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"), call. = FALSE)
  }

  return(TRUE)
}

#' Build Scenario Segments with Overrides
#'
#' Creates a unified segments tibble for all scenario runs, encoding all deviations
#' from base case in parameter_overrides and setting_overrides columns.
#'
#' @param model Parsed model object
#' @return Tibble with all segments for all scenario runs, with scenario_id,
#'   parameter_overrides, and setting_overrides columns
#' @keywords internal
build_scenario_segments <- function(model) {

  # Get base segments (strategy x group combinations)
  base_segments <- get_segments(model)

  # Enrich with evaluated variables for potential expressions in overrides
  enriched_segments <- base_segments %>%
    rowwise() %>%
    do({
      seg <- as.list(.)
      prepare_segment_for_sampling(model, as_tibble(seg))
    }) %>%
    ungroup()

  # Apply override categories as baseline overrides
  enriched_segments <- apply_override_categories(model, enriched_segments)

  n_segments <- nrow(enriched_segments)
  all_segments <- list()
  scenario_id <- 1

  # Scenario 1: Base case - all segments with override category values as baseline
  base_case_segments <- enriched_segments %>%
    mutate(
      scenario_id = scenario_id
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
  scenario_id <- scenario_id + 1

  # Process each user-defined scenario
  for (scenario in model$scenarios) {
    # Initialize override lists for this scenario
    param_overrides_list <- vector("list", n_segments)
    setting_overrides_list <- vector("list", n_segments)

    # Build parameter overrides per segment using explicit loop (like DSA)
    for (seg_idx in seq_len(n_segments)) {
      segment <- enriched_segments[seg_idx, ]
      seg_strategy <- segment$strategy[[1]]
      seg_group <- segment$group[[1]]

      # Start with baseline overrides from override categories
      overrides <- if ("parameter_overrides" %in% names(enriched_segments)) {
        enriched_segments$parameter_overrides[[seg_idx]]
      } else {
        list()
      }

      for (override in scenario$variable_overrides) {
        # Check if override applies to this segment
        override_strategy <- if (!is.na(override$strategy) && override$strategy != "") override$strategy else NULL
        override_group <- if (!is.na(override$group) && override$group != "") override$group else NULL

        applies <- TRUE
        if (!is.null(override_strategy) && override_strategy != seg_strategy) {
          applies <- FALSE
        }
        if (!is.null(override_group) && override_group != seg_group) {
          applies <- FALSE
        }

        if (applies) {
          # Evaluate value if it's an oq_formula
          if (inherits(override$value, "oq_formula")) {
            seg_ns <- clone_namespace(segment$eval_vars[[1]])
            eval_value <- eval_formula(override$value, seg_ns)

            if (is_oq_error(eval_value)) {
              stop(glue("Failed to evaluate scenario variable '{override$name}' in scenario '{scenario$name}': {eval_value}"),
                   call. = FALSE)
            }
            overrides[[override$name]] <- eval_value
          } else {
            # Literal value
            overrides[[override$name]] <- override$value
          }
        }
      }

      param_overrides_list[[seg_idx]] <- overrides

      # Build setting overrides - start with baseline from override categories
      setting_overrides <- if ("setting_overrides" %in% names(enriched_segments)) {
        enriched_segments$setting_overrides[[seg_idx]]
      } else {
        list()
      }
      for (override in scenario$setting_overrides) {
        setting_overrides[[override$name]] <- override$value
      }
      setting_overrides_list[[seg_idx]] <- setting_overrides
    }

    # Create scenario segments with the pre-built override lists
    scenario_segs <- enriched_segments %>%
      mutate(
        scenario_id = !!scenario_id,
        parameter_overrides = param_overrides_list,
        setting_overrides = setting_overrides_list
      )

    all_segments[[length(all_segments) + 1]] <- scenario_segs
    scenario_id <- scenario_id + 1
  }

  bind_rows(all_segments)
}

#' Generate Scenario Metadata
#'
#' Creates metadata tibble describing each scenario for use in analysis functions.
#'
#' @param model Parsed model object
#' @param segments Segments tibble from build_scenario_segments()
#' @return Tibble with columns: scenario_id, scenario_name, scenario_description
#' @keywords internal
generate_scenario_metadata <- function(model, segments) {
  # Get unique scenario_ids
  scenario_ids <- sort(unique(segments$scenario_id))

  metadata <- tibble(
    scenario_id = integer(),
    scenario_name = character(),
    scenario_description = character()
  )

  for (sid in scenario_ids) {
    if (sid == 1) {
      # Base case
      metadata <- bind_rows(metadata, tibble(
        scenario_id = 1L,
        scenario_name = "Base Case",
        scenario_description = "Default model parameters"
      ))
    } else {
      # User-defined scenario (index = sid - 1 because Base Case is sid=1)
      scenario_idx <- sid - 1
      if (scenario_idx <= length(model$scenarios)) {
        scenario <- model$scenarios[[scenario_idx]]
        metadata <- bind_rows(metadata, tibble(
          scenario_id = as.integer(sid),
          scenario_name = scenario$name,
          scenario_description = scenario$description
        ))
      }
    }
  }

  metadata
}

#' Run Scenario Analysis
#'
#' Runs scenario analysis by executing the model for each defined scenario.
#' A "Base Case" scenario is automatically created with default parameters,
#' followed by each user-defined scenario with its specified overrides.
#'
#' When VBP parameters are provided, Scenario+VBP mode is activated. In this mode,
#' each scenario includes 3 VBP price level sub-simulations to calculate the
#' VBP relationship for each scenario.
#'
#' @param model An openqaly model object with scenario specifications
#' @param vbp_price_variable Name of the price variable to vary for VBP analysis.
#'   If NULL (default), standard scenario analysis without VBP is performed.
#' @param vbp_intervention Intervention strategy name for VBP analysis.
#'   Required if vbp_price_variable is specified.
#' @param vbp_outcome_summary Outcome summary name for VBP analysis.
#'   Defaults to "total_qalys" from model metadata if available.
#' @param vbp_cost_summary Cost summary name for VBP analysis.
#'   Defaults to "total_cost" from model metadata if available.
#' @param ... Additional arguments passed to run_segment
#' @return Results list with segments and aggregated results (includes scenario_id dimension).
#'   When VBP is enabled, also includes scenario_vbp_equations tibble with VBP equations
#'   for each scenario.
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Basic scenario analysis
#' model <- define_model("markov") %>%
#'   add_variable("efficacy", 0.8) %>%
#'   add_variable("cost", 5000) %>%
#'   add_scenario("Optimistic", description = "Best case") %>%
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) %>%
#'   add_scenario_variable("Optimistic", "cost", 4000) %>%
#'   add_scenario("Pessimistic", description = "Conservative") %>%
#'   add_scenario_variable("Pessimistic", "efficacy", 0.70) %>%
#'   add_scenario_variable("Pessimistic", "cost", 7000)
#'
#' results <- run_scenario(model)
#'
#' # Example 2: Scenario analysis with setting overrides
#' model <- define_model("markov") %>%
#'   set_settings(timeframe = 20, discount_cost = 3) %>%
#'   add_scenario("Extended Horizon") %>%
#'   add_scenario_setting("Extended Horizon", "timeframe", 30) %>%
#'   add_scenario("No Discounting") %>%
#'   add_scenario_setting("No Discounting", "discount_cost", 0) %>%
#'   add_scenario_setting("No Discounting", "discount_outcomes", 0)
#'
#' results <- run_scenario(model)
#'
#' # Example 3: Scenario analysis with VBP
#' model <- define_model("markov") %>%
#'   add_variable("cost_tx", 5000) %>%
#'   add_scenario("Optimistic") %>%
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) %>%
#'   add_strategy("treatment") %>%
#'   add_strategy("comparator")
#'
#' results <- run_scenario(
#'   model,
#'   vbp_price_variable = "cost_tx",
#'   vbp_intervention = "treatment"
#' )
#' }
run_scenario <- function(model,
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

  # Validate scenario specifications
  validate_scenario_spec(parsed_model)

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

    # Validate VBP spec (reuse from VBP module)
    validate_scenario_vbp_spec(
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
    # Build Scenario+VBP segments (includes vbp_price_level)
    all_segments <- build_scenario_vbp_segments(parsed_model, vbp_spec)
  } else {
    # Build standard scenario segments
    all_segments <- build_scenario_segments(parsed_model)
  }

  # Run all segments in parallel with progress bar
  results <- all_segments %>%
    rowwise() %>%
    group_split() %>%
    future_map(function(segment) run_segment(segment, parsed_model, ...),
               .progress = TRUE, .options = furrr_options(seed = 1)) %>%
    bind_rows()

  # Generate scenario metadata
  # Use only base scenario segments (before VBP expansion) for metadata
  if (vbp_enabled) {
    scenario_segments_for_meta <- all_segments %>%
      filter(.data$vbp_price_level == 1)
  } else {
    scenario_segments_for_meta <- all_segments
  }
  scenario_metadata <- generate_scenario_metadata(parsed_model, scenario_segments_for_meta)

  # Aggregate by scenario_id + strategy (and vbp_price_level if present)
  aggregated <- aggregate_segments(results, parsed_model)

  # Return results
  res <- list()
  res$segments <- results
  res$aggregated <- aggregated
  res$metadata <- parsed_model$metadata
  res$scenario_metadata <- scenario_metadata

  # Add VBP analysis if enabled
  if (vbp_enabled) {
    res$scenario_vbp_equations <- analyze_scenario_vbp_results(
      results,
      aggregated,
      vbp_spec,
      scenario_metadata,
      parsed_model
    )
    res$vbp_spec <- vbp_spec
  }

  class(res) <- c("scenario_results", "list")
  return(res)
}


#' Extract Scenario Summary Values
#'
#' Helper function to extract summary values from scenario results across all scenarios.
#' Efficiently extracts summaries without processing full trace data.
#'
#' @param results Scenario results object from run_scenario()
#' @param summary_name Name of the summary to extract (e.g., "qalys", "costs")
#' @param value_type Type of value: "all", "cost", or "outcome"
#' @param groups Group selection: "overall" (default), "all" (overall + all groups),
#'   "all_groups" (all groups without overall), or specific group name(s)
#' @param strategies Character vector of strategy names to include (NULL for all).
#'   Mutually exclusive with interventions/comparators.
#' @param interventions Character vector of intervention strategy name(s). Used with
#'   comparators to determine which strategies to extract.
#' @param comparators Character vector of comparator strategy name(s). Used with
#'   interventions to determine which strategies to extract.
#' @param discounted Logical. Use discounted values? (default: FALSE)
#'
#' @return A tibble with columns: scenario_id, scenario_name, scenario_description,
#'   strategy, group, amount
#'
#' @importFrom tidyr unnest
#' @keywords internal
extract_scenario_summaries <- function(results,
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

  # Check for scenario_id column
  if (!"scenario_id" %in% names(source_data)) {
    stop("No scenario_id column found. Ensure model was run with run_scenario().",
         call. = FALSE)
  }

  # Determine which summary column to use (nested summaries)
  summary_col <- if (discounted) "summaries_discounted" else "summaries"

  # Check if summary column exists
  if (!summary_col %in% names(source_data)) {
    return(tibble(
      scenario_id = source_data$scenario_id,
      strategy = source_data$strategy,
      group = source_data$group,
      amount = NA_real_
    ))
  }

  # Vectorized extraction and aggregation (like extract_dsa_summaries)
  result <- source_data %>%
    select("scenario_id", "strategy", "group", summary_data = all_of(summary_col)) %>%
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

  # Aggregate by scenario_id, strategy, group
  result <- result %>%
    group_by(.data$scenario_id, .data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  # Join with scenario metadata
  result <- result %>%
    left_join(results$scenario_metadata, by = "scenario_id")

  # Reorder columns
  result %>%
    select("scenario_id", "scenario_name", "scenario_description",
           "strategy", "group", "amount")
}
