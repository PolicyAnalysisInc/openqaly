#' Extract PSA Simulation Data
#'
#' Helper function to extract simulation-level costs and outcomes from PSA results.
#'
#' @inheritParams calculate_incremental_ceac
#'
#' @return A tibble with columns: simulation, strategy, group, cost, outcome
#'
#' @keywords internal
get_psa_simulations <- function(results,
                                outcome_summary,
                                cost_summary,
                                groups = "overall",
                                strategies = NULL) {

  source_data <- select_source_data(groups, results)

  # Filter strategies if specified
  if (!is.null(strategies)) {
    # Validate strategies exist using helper
    check_strategies_exist(strategies, results$metadata)
    source_data <- source_data %>%
      filter(.data$strategy %in% !!strategies)
  }

  # Check for simulation column
  if (!"simulation" %in% names(source_data)) {
    stop("No simulation data found. Ensure model was run with run_psa().")
  }

  # Extract cost summaries (always discounted for CE analysis)
  cost_data <- extract_psa_summaries(
    source_data,
    summary_name = cost_summary,
    value_type = "cost"
  ) %>%
    rename(cost = "amount")

  # Extract outcome summaries (always discounted for CE analysis)
  outcome_data <- extract_psa_summaries(
    source_data,
    summary_name = outcome_summary,
    value_type = "outcome"
  ) %>%
    rename(outcome = "amount")

  # Combine cost and outcome data
  psa_data <- cost_data %>%
    inner_join(outcome_data,
               by = c("simulation", "strategy", "group"))

  # Map display names if metadata available
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      psa_data$strategy <- map_names(psa_data$strategy,
                                     results$metadata$strategies,
                                     "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      psa_data$group <- map_names(psa_data$group,
                                  results$metadata$groups,
                                  "display_name")
    }
  }

  psa_data
}


#' Get PSA Outcome Simulations
#'
#' Extract simulation-level outcome data from PSA results for density plots.
#' Supports both absolute mode (raw outcomes per strategy) and difference mode
#' (outcome differences between intervention/comparator pairs).
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param outcome_summary Name of the outcome summary to extract (e.g., "total_qalys")
#' @param interventions Reference strategies for comparison (intervention perspective).
#'   Cannot be used with \code{strategies}.
#' @param comparators Reference strategies for comparison (comparator perspective).
#'   Cannot be used with \code{strategies}.
#' @param groups Which groups to include. Options: "overall" (default), "all",
#'   "all_groups", or a character vector of specific group names.
#' @param strategies Character vector of strategies to include. For absolute outcome
#'   values. Cannot be used with \code{interventions} or \code{comparators}.
#' @param discounted Logical. Use discounted outcome values? Default TRUE.
#' @param value_type Character. Type of value to extract: "outcome" or "cost". Default "outcome".
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{simulation}: simulation index
#'     \item \code{strategy} (absolute mode) or \code{comparison} (difference mode): strategy/comparison label
#'     \item \code{group}: group name
#'     \item \code{outcome}: outcome value (raw or difference)
#'   }
#'
#' @export
get_psa_outcome_simulations <- function(results,
                                        outcome_summary,
                                        interventions = NULL,
                                        comparators = NULL,
                                        groups = "overall",
                                        strategies = NULL,
                                        discounted = TRUE,
                                        value_type = "outcome") {

  # Validate mutual exclusivity of strategies vs interventions/comparators
  if (!is.null(strategies) && (!is.null(interventions) || !is.null(comparators))) {
    stop("'strategies' parameter cannot be used with 'interventions' or 'comparators'. ",
         "Use strategies for absolute values or interventions/comparators for differences.")
  }

  # Require at least one mode to be specified
  if (is.null(strategies) && is.null(interventions) && is.null(comparators)) {
    stop("Either 'strategies' (for absolute values) or at least one of ",
         "'interventions'/'comparators' (for differences) must be provided.")
  }

  source_data <- select_source_data(groups, results)

  # Check for simulation column
  if (!"simulation" %in% names(source_data)) {
    stop("No simulation data found. Ensure model was run with run_psa().")
  }

  # Extract outcome summaries
  outcome_data <- extract_psa_summaries(
    source_data,
    summary_name = outcome_summary,
    value_type = value_type,
    discounted = discounted
  ) %>%
    rename(outcome = "amount")

  # Map display names if metadata available
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies)) {
      outcome_data$strategy <- map_names(outcome_data$strategy,
                                         results$metadata$strategies,
                                         "display_name")
    }
    if (!is.null(results$metadata$groups)) {
      outcome_data$group <- map_names(outcome_data$group,
                                      results$metadata$groups,
                                      "display_name")
    }
  }

  # Apply consistent group ordering
  group_levels <- get_group_order(unique(outcome_data$group), results$metadata)
  outcome_data <- outcome_data %>% mutate(group = factor(.data$group, levels = group_levels))

  # Get all strategies (already display names)
  all_strategies <- unique(outcome_data$strategy)

  # Helper function to resolve strategy names (technical -> display if needed)
  resolve_strategy <- function(strat_name) {
    if (strat_name %in% all_strategies) {
      return(strat_name)
    }
    if (!is.null(results$metadata) && !is.null(results$metadata$strategies)) {
      strategy_map <- results$metadata$strategies
      matched_display <- strategy_map$display_name[strategy_map$name == strat_name]
      if (length(matched_display) > 0 && !is.na(matched_display[1])) {
        return(matched_display[1])
      }
    }
    stop(sprintf("Strategy '%s' not found in results", strat_name))
  }

  # ABSOLUTE MODE: Return raw outcomes per strategy
  if (!is.null(strategies)) {
    # Resolve and filter strategies
    resolved_strategies <- sapply(strategies, resolve_strategy, USE.NAMES = FALSE)
    psa_data <- outcome_data %>%
      filter(.data$strategy %in% resolved_strategies) %>%
      mutate(strategy = factor(.data$strategy, levels = resolved_strategies))

    return(psa_data)
  }

  # DIFFERENCE MODE: Calculate pairwise outcome differences
  # Resolve user-provided interventions/comparators to display names
  if (!is.null(interventions)) {
    interventions <- sapply(interventions, resolve_strategy, USE.NAMES = FALSE)
  }
  if (!is.null(comparators)) {
    comparators <- sapply(comparators, resolve_strategy, USE.NAMES = FALSE)
  }

  # Determine comparison pairs
  comparison_pairs <- list()

  if (!is.null(interventions) && !is.null(comparators)) {
    # Both provided: N x M explicit comparisons
    for (int_strat in interventions) {
      for (comp_strat in comparators) {
        if (int_strat != comp_strat) {
          comparison_pairs[[length(comparison_pairs) + 1]] <- list(
            intervention = int_strat,
            comparator = comp_strat
          )
        }
      }
    }
    if (length(comparison_pairs) == 0) {
      stop("No valid comparisons after excluding self-comparisons")
    }
  } else if (!is.null(interventions)) {
    # Intervention only: each intervention vs all others
    for (int_strat in interventions) {
      other_strategies <- setdiff(all_strategies, int_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = int_strat,
          comparator = other
        )
      }
    }
  } else {
    # Comparator only: all others vs each comparator
    for (comp_strat in comparators) {
      other_strategies <- setdiff(all_strategies, comp_strat)
      for (other in other_strategies) {
        comparison_pairs[[length(comparison_pairs) + 1]] <- list(
          intervention = other,
          comparator = comp_strat
        )
      }
    }
  }

  # Calculate outcome differences for each comparison pair
  diff_data_list <- list()

  for (pair in comparison_pairs) {
    int_strat <- pair$intervention
    comp_strat <- pair$comparator

    # Create comparison label
    comp_label <- paste0(int_strat, " vs. ", comp_strat)

    # Get data for intervention and comparator
    int_data <- outcome_data %>%
      filter(.data$strategy == int_strat) %>%
      select("simulation", "group", int_outcome = "outcome")

    comp_data <- outcome_data %>%
      filter(.data$strategy == comp_strat) %>%
      select("simulation", "group", comp_outcome = "outcome")

    # Join and calculate outcome difference
    pair_data <- int_data %>%
      inner_join(comp_data, by = c("simulation", "group")) %>%
      mutate(
        outcome = .data$int_outcome - .data$comp_outcome,
        comparison = comp_label
      ) %>%
      select("simulation", "group", "comparison", "outcome")

    diff_data_list[[length(diff_data_list) + 1]] <- pair_data
  }

  # Combine all comparison data
  plot_data <- bind_rows(diff_data_list)

  # Preserve comparison order as factor
  comparison_order <- unique(plot_data$comparison)
  plot_data <- plot_data %>%
    mutate(comparison = factor(.data$comparison, levels = comparison_order))

  plot_data
}


#' Extract PSA Summary Values
#'
#' Helper function to extract summary values from PSA simulation data.
#'
#' @param source_data PSA results data with simulation column
#' @param summary_name Name of the summary to extract
#' @param value_type Type of value: "cost" or "outcome"
#' @param discounted Logical. Use discounted values? Default TRUE.
#'
#' @return A tibble with simulation, strategy, group, and amount columns
#'
#' @keywords internal
extract_psa_summaries <- function(source_data,
                                  summary_name,
                                  value_type,
                                  discounted = TRUE) {

  # Determine which column to use
  summary_col <- if (discounted) "summaries_discounted" else "summaries"

  # Check if summary column exists
  if (!summary_col %in% names(source_data)) {
    return(tibble(
      simulation = source_data$simulation,
      strategy = source_data$strategy,
      group = source_data$group,
      amount = NA_real_
    ))
  }

  # Vectorized extraction and aggregation
  result <- source_data %>%
    select("simulation", "strategy", "group", summary_data = all_of(summary_col)) %>%
    unnest("summary_data", keep_empty = TRUE) %>%
    filter(.data$summary == summary_name | is.na(.data$summary)) %>%
    group_by(.data$simulation, .data$strategy, .data$group) %>%
    summarize(amount = sum(.data$amount, na.rm = TRUE), .groups = "drop")

  # Handle rows where summary wasn't found
  # If a (simulation, strategy, group) combo is missing, fill with NA
  all_combos <- source_data %>%
    distinct(.data$simulation, .data$strategy, .data$group)

  result <- all_combos %>%
    left_join(result, by = c("simulation", "strategy", "group")) %>%
    mutate(amount = if_else(is.na(.data$amount), NA_real_, .data$amount))

  result
}


#' Extract Sampled Parameter Values from PSA Results
#'
#' Extracts sampled input parameter values from PSA results, handling
#' strategy/group-specific variables with appropriate labeling.
#'
#' @param results A openqaly PSA results object (from run_psa)
#' @param variables Character vector of variable names to extract (NULL for all)
#' @param group Group selection with three modes:
#'   \itemize{
#'     \item NULL or "all": include all groups
#'     \item Single value: filter to that group
#'     \item Vector same length as variables: specify group for each variable
#'   }
#' @param strategies Strategy selection with three modes:
#'   \itemize{
#'     \item NULL: include all strategies
#'     \item Single value: filter to that strategy
#'     \item Vector same length as variables: specify strategy for each variable
#'   }
#'
#' @return A tibble with columns:
#'   \item{simulation}{Simulation number}
#'   \item{...}{One column per unique (variable, strategy, group) combination}
#'
#' @details
#' This function extracts sampled parameter values from PSA results and creates
#' appropriately labeled columns using variable display names. For strategy/group-specific
#' variables, ensure each has a unique display_name defined in add_variable().
#'
#' Each simulation appears once in the output, with all sampled parameters from
#' all strategy/group combinations included as separate columns.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example", package = "openqaly"))
#' psa_results <- run_psa(model, n_sim = 1000)
#'
#' # Extract all sampled parameters
#' sampled_params <- get_sampled_parameters(psa_results)
#'
#' # Extract specific variables (all strategies/groups)
#' sampled_params <- get_sampled_parameters(
#'   psa_results,
#'   variables = c("p_transition", "utility_well", "cost_treatment")
#' )
#'
#' # Filter to specific strategy
#' sampled_params <- get_sampled_parameters(
#'   psa_results,
#'   variables = c("p_transition", "utility_well"),
#'   strategies = "treatment_a"
#' )
#'
#' # Extract specific (variable, strategy, group) combinations
#' sampled_params <- get_sampled_parameters(
#'   psa_results,
#'   variables = c("p_well_to_sick", "util_well", "cost_med"),
#'   strategies = c("treatment_a", "treatment_b", "treatment_c"),
#'   group = c("group_1", "group_1", "group_1")
#' )
#' # This extracts:
#' # - p_well_to_sick from treatment_a, group_1
#' # - util_well from treatment_b, group_1
#' # - cost_med from treatment_c, group_1
#' }
#'
#' @export
get_sampled_parameters <- function(results,
                                   variables = NULL,
                                   group = NULL,
                                   strategies = NULL) {

  # Check if results have PSA simulations
  if (!"simulation" %in% names(results$segments)) {
    stop("Results do not contain PSA simulations. Use run_psa() instead of run_model().")
  }

  # Check for parameter_overrides column
  if (!"parameter_overrides" %in% names(results$segments)) {
    stop("Results do not contain parameter override values.")
  }

  # Check if using tuple specification (vector-form strategies/group)
  use_tuple_specification <- FALSE
  desired_tuples <- NULL

  if (!is.null(variables)) {
    # Check if strategies is a vector matching variables length
    if (!is.null(strategies) && length(strategies) > 1) {
      if (length(strategies) != length(variables)) {
        stop("When 'strategies' is a vector, it must have the same length as 'variables'")
      }
      use_tuple_specification <- TRUE
    }

    # Check if group is a vector matching variables length
    if (!is.null(group) && length(group) > 1 && !group[1] %in% c("aggregated", "overall", "all")) {
      if (length(group) != length(variables)) {
        stop("When 'group' is a vector, it must have the same length as 'variables'")
      }
      use_tuple_specification <- TRUE
    }

    # Create desired tuples if using tuple specification
    if (use_tuple_specification) {
      desired_tuples <- tibble(
        variable = variables,
        strategy = if (!is.null(strategies) && length(strategies) == length(variables))
          strategies else rep(NA_character_, length(variables)),
        group = if (!is.null(group) && length(group) == length(variables))
          group else rep(NA_character_, length(variables))
      )
    }
  } else if (!is.null(strategies) && length(strategies) > 1) {
    stop("Cannot use vector-form 'strategies' when 'variables' is NULL")
  } else if (!is.null(group) && length(group) > 1) {
    stop("Cannot use vector-form 'group' when 'variables' is NULL")
  }

  # Filter source data by group and strategies (only if NOT using tuple specification)
  source_data <- results$segments

  if (!use_tuple_specification) {
    # Use global filtering when not using tuple specification
    if (!is.null(group) && !group %in% c("aggregated", "overall", "all")) {
      # Validate groups exist using helper
      for (g in group) {
        check_group_exists(g, results)
      }

      # Filter to specific group(s)
      source_data <- source_data %>%
        filter(.data$group %in% !!group)
    }

    # Filter strategies if specified
    if (!is.null(strategies)) {
      source_data <- source_data %>%
        filter(.data$strategy %in% !!strategies)

      if (nrow(source_data) == 0) {
        stop("No data found for specified strategies")
      }
    }
  }
  # If using tuple specification, we'll filter at the variable level in the loop

  # Get metadata for name mappings
  metadata <- results$metadata

  # Create name mapping functions
  map_var_name <- function(var_name, strat = NA, grp = NA) {
    if (!is.null(metadata$variables)) {
      var_meta <- metadata$variables %>% filter(.data$name == var_name)
      # Narrow by strategy if multiple rows match and strategy info is available
      if ("strategy" %in% colnames(var_meta) && !is.na(strat) && nrow(var_meta) > 1) {
        strat_match <- var_meta %>% filter(
          (!is.na(.data$strategy) & .data$strategy == strat) |
          (is.na(.data$strategy) | .data$strategy == "")
        )
        if (nrow(strat_match) > 0) var_meta <- strat_match
      }
      # Narrow by group if multiple rows match and group info is available
      if ("group" %in% colnames(var_meta) && !is.na(grp) && nrow(var_meta) > 1) {
        grp_match <- var_meta %>% filter(
          (!is.na(.data$group) & .data$group == grp) |
          (is.na(.data$group) | .data$group == "")
        )
        if (nrow(grp_match) > 0) var_meta <- grp_match
      }
      if (nrow(var_meta) > 0 && !is.na(var_meta$display_name[1])) {
        return(var_meta$display_name[1])
      }
    }
    return(var_name)
  }

  map_strategy_name <- function(strat_name) {
    if (!is.null(metadata$strategies)) {
      strat_meta <- metadata$strategies %>% filter(.data$name == strat_name)
      if (nrow(strat_meta) > 0 && !is.na(strat_meta[["display_name"]][1])) {
        return(strat_meta[["display_name"]][1])
      }
    }
    return(strat_name)
  }

  map_group_name <- function(grp_name) {
    if (!is.null(metadata$groups)) {
      group_meta <- metadata$groups %>% filter(.data$name == grp_name)
      if (nrow(group_meta) > 0 && !is.na(group_meta[["display_name"]][1])) {
        return(group_meta[["display_name"]][1])
      }
    }
    return(grp_name)
  }

  # Extract all sampled values and create properly labeled columns
  all_params <- list()

  for (i in 1:nrow(source_data)) {
    row <- source_data[i, ]
    sim <- row$simulation
    strat <- row$strategy
    grp <- row$group
    override_vals <- row$parameter_overrides[[1]]

    if (length(override_vals) == 0) next

    # Get display names
    strat_display <- map_strategy_name(strat)
    grp_display <- map_group_name(grp)

    for (var_name in names(override_vals)) {
      # Filter based on specification mode
      if (use_tuple_specification) {
        # Check if this (variable, strategy, group) tuple is in desired_tuples
        matching_tuple <- desired_tuples %>%
          filter(
            .data$variable == var_name,
            (is.na(.data$strategy) | .data$strategy == strat),
            (is.na(.data$group) | .data$group == grp)
          )

        if (nrow(matching_tuple) == 0) next
      } else {
        # Use simple variable filter
        if (!is.null(variables) && !var_name %in% variables) next
      }

      var_value <- override_vals[[var_name]]

      # Get variable display name (no automatic suffix addition)
      col_label <- map_var_name(var_name, strat, grp)

      # Create internal technical name for uniqueness
      col_key <- paste(var_name, strat, grp, sep = "__")

      # Store value
      if (is.null(all_params[[col_key]])) {
        all_params[[col_key]] <- list(
          label = col_label,
          values = list()
        )
      }

      all_params[[col_key]]$values[[as.character(sim)]] <- var_value
    }
  }

  # Validate unique display names before creating tibble
  labels <- sapply(all_params, function(p) p$label)
  dup_labels <- unique(labels[duplicated(labels)])

  if (length(dup_labels) > 0) {
    # Build error message showing which parameters have duplicate labels
    dup_errors <- character()
    for (dup_label in dup_labels) {
      dup_keys <- names(all_params)[labels == dup_label]
      # Parse col_key back to name__strategy__group
      param_details <- sapply(dup_keys, function(key) {
        parts <- strsplit(key, "__")[[1]]
        if (length(parts) == 3) {
          sprintf("variable=%s, strategy=%s, group=%s", parts[1], parts[2], parts[3])
        } else {
          key
        }
      })
      dup_errors <- c(dup_errors, sprintf(
        "Display name '%s' is used by multiple sampled parameters:\n  %s\n",
        dup_label,
        paste(param_details, collapse = "\n  ")
      ))
    }
    stop(
      paste(dup_errors, collapse = "\n"),
      "\nEach sampled parameter must have a unique display name. ",
      "Specify different display names in add_variable() for strategy/group-specific variables.",
      call. = FALSE
    )
  }

  # Convert to tibble format
  simulations <- unique(source_data$simulation)
  result <- tibble(simulation = simulations)

  for (col_key in names(all_params)) {
    param_info <- all_params[[col_key]]
    col_label <- param_info$label
    values <- param_info$values

    # Create vector of values for all simulations
    col_values <- sapply(simulations, function(sim) {
      val <- values[[as.character(sim)]]
      if (is.null(val)) NA_real_ else val
    })

    result[[col_label]] <- col_values
  }

  return(result)
}
