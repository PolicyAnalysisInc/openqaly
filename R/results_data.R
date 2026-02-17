#' Get Outcome Summary Data
#'
#' Returns all summary totals (costs and outcomes) as a tidy tibble,
#' including both discounted and undiscounted amounts.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL for all
#' @param strategies Character vector of strategy names to include. NULL (default) includes all.
#'
#' @return A tibble with columns: strategy, group, type, summary, value, discounted, amount
#'
#' @examples
#' \dontrun{
#' model <- read_model("inst/models/markov_medium")
#' results <- run_model(model)
#' outcomes_data(results)
#' }
#'
#' @export
outcomes_data <- function(results,
                          groups = "overall",
                          strategies = NULL) {

  # Get both discounted and undiscounted summaries
  disc <- get_summaries(
    results,
    groups = groups,
    strategies = strategies,
    summaries = NULL,
    value_type = "all",
    discounted = TRUE,
    use_display_names = TRUE
  )
  disc$discounted <- TRUE

  undisc <- get_summaries(
    results,
    groups = groups,
    strategies = strategies,
    summaries = NULL,
    value_type = "all",
    discounted = FALSE,
    use_display_names = TRUE
  )
  undisc$discounted <- FALSE

  summaries <- rbind(disc, undisc)

  # Build type lookup from metadata$values (display_name -> type)
  type_lookup <- NULL
  if (!is.null(results$metadata) && !is.null(results$metadata$values)) {
    values_meta <- results$metadata$values
    type_lookup <- stats::setNames(values_meta$type, values_meta$display_name)
    # Also add technical name mappings as fallback
    type_lookup_tech <- stats::setNames(values_meta$type, values_meta$name)
    type_lookup <- c(type_lookup, type_lookup_tech)
  }

  # Add type column
  if (!is.null(type_lookup)) {
    summaries$type <- unname(type_lookup[summaries$value])
  } else {
    summaries$type <- NA_character_
  }

  # Map summary names to display names
  if (!is.null(results$metadata) && !is.null(results$metadata$summaries)) {
    summaries$summary <- map_names(
      summaries$summary, results$metadata$summaries, "display_name"
    )
  }

  # Reorder columns
  summaries <- summaries[, c("strategy", "group", "type", "summary", "value",
                              "discounted", "amount")]

  tibble::as_tibble(summaries)
}


#' Get Incremental Cost-Effectiveness Data
#'
#' Auto-detects all cost/outcome summary pairs and produces incremental
#' cost-effectiveness analysis for each pair.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL for all
#' @param strategies Character vector of strategy names to include. NULL (default) includes all.
#'
#' @return A tibble with columns: outcome_summary, cost_summary, group, strategy,
#'   comparator, cost, outcome, dcost, doutcome, icer, on_frontier, dominated,
#'   strictly_dominated, extendedly_dominated
#'
#' @examples
#' \dontrun{
#' model <- read_model("inst/models/markov_medium")
#' results <- run_model(model)
#' incremental_ce_data(results)
#' }
#'
#' @export
incremental_ce_data <- function(results,
                                groups = "overall",
                                strategies = NULL) {

  # Get cost/outcome summary pairs from metadata
  pairs <- get_summary_pairs(results$metadata)

  # Calculate incremental CE for each pair
  result_list <- lapply(seq_len(nrow(pairs)), function(i) {
    ce <- calculate_incremental_ce(
      results,
      outcome_summary = pairs$outcome[i],
      cost_summary = pairs$cost[i],
      groups = groups,
      strategies = strategies
    )

    # Add summary pair display names
    ce$outcome_summary <- pairs$outcome_display[i]
    ce$cost_summary <- pairs$cost_display[i]

    ce
  })

  result <- dplyr::bind_rows(result_list)

  if (nrow(result) == 0) {
    return(tibble::tibble(
      outcome_summary = character(),
      cost_summary = character(),
      group = character(),
      strategy = character(),
      comparator = character(),
      cost = numeric(),
      outcome = numeric(),
      dcost = numeric(),
      doutcome = numeric(),
      icer = numeric(),
      on_frontier = logical(),
      dominated = logical(),
      strictly_dominated = logical(),
      extendedly_dominated = logical()
    ))
  }

  # Reorder columns
  result <- result[, c("outcome_summary", "cost_summary", "group", "strategy",
                        "comparator", "cost", "outcome", "dcost", "doutcome",
                        "icer", "on_frontier", "dominated", "strictly_dominated",
                        "extendedly_dominated")]

  tibble::as_tibble(result)
}


#' Get Pairwise Cost-Effectiveness Data
#'
#' Auto-detects all cost/outcome summary pairs and produces pairwise
#' cost-effectiveness comparisons for each pair. At least one of interventions
#' or comparators must be specified.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param interventions Character vector of intervention strategy names.
#' @param comparators Character vector of comparator strategy names.
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL for all
#'
#' @return A tibble with columns: outcome_summary, cost_summary, group, strategy,
#'   comparator, cost, outcome, dcost, doutcome, icer
#'
#' @examples
#' \dontrun{
#' model <- read_model("inst/models/markov_medium")
#' results <- run_model(model)
#' pairwise_ce_data(results, comparators = "chemoplatin")
#' }
#'
#' @export
pairwise_ce_data <- function(results,
                             interventions = NULL,
                             comparators = NULL,
                             groups = "overall") {

  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Get cost/outcome summary pairs from metadata
  pairs <- get_summary_pairs(results$metadata)

  # Calculate pairwise CE for each pair
  result_list <- lapply(seq_len(nrow(pairs)), function(i) {
    ce <- calculate_pairwise_ce(
      results,
      outcome_summary = pairs$outcome[i],
      cost_summary = pairs$cost[i],
      groups = groups,
      interventions = interventions,
      comparators = comparators
    )

    # Add summary pair display names
    ce$outcome_summary <- pairs$outcome_display[i]
    ce$cost_summary <- pairs$cost_display[i]

    ce
  })

  result <- dplyr::bind_rows(result_list)

  if (nrow(result) == 0) {
    return(tibble::tibble(
      outcome_summary = character(),
      cost_summary = character(),
      group = character(),
      strategy = character(),
      comparator = character(),
      cost = numeric(),
      outcome = numeric(),
      dcost = numeric(),
      doutcome = numeric(),
      icer = numeric()
    ))
  }

  # Reorder columns
  result <- result[, c("outcome_summary", "cost_summary", "group", "strategy",
                        "comparator", "cost", "outcome", "dcost", "doutcome",
                        "icer")]

  tibble::as_tibble(result)
}


#' Get Net Monetary Benefit Data
#'
#' Auto-detects cost/outcome summary pairs and computes per-value NMB component
#' breakdown. At least one of interventions or comparators must be specified.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param interventions Character vector of intervention strategy names.
#' @param comparators Character vector of comparator strategy names.
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL for all
#' @param wtp Optional override for willingness-to-pay value. If NULL, extracts from metadata.
#'
#' @return A tibble with columns: outcome_summary, cost_summary, strategy, group,
#'   type, value, amount
#'
#' @examples
#' \dontrun{
#' model <- read_model("inst/models/markov_medium")
#' results <- run_model(model)
#' nmb_data(results, comparators = "chemoplatin")
#' }
#'
#' @export
nmb_data <- function(results,
                     interventions = NULL,
                     comparators = NULL,
                     groups = "overall",
                     wtp = NULL) {

  if (is.null(interventions) && is.null(comparators)) {
    stop("At least one of 'interventions' or 'comparators' must be provided")
  }

  # Get cost/outcome summary pairs from metadata
  pairs <- get_summary_pairs(results$metadata)

  # Calculate NMB for each pair
  result_list <- lapply(seq_len(nrow(pairs)), function(i) {
    outcome_summary <- pairs$outcome[i]
    cost_summary <- pairs$cost[i]

    # Get WTP for this outcome summary
    pair_wtp <- wtp
    if (is.null(pair_wtp)) {
      if (is.null(results$metadata$summaries)) {
        stop("Cannot extract WTP from metadata.")
      }
      outcome_meta <- results$metadata$summaries %>%
        dplyr::filter(.data$name == outcome_summary)
      pair_wtp <- outcome_meta$wtp[1]
      if (length(pair_wtp) == 0 || is.na(pair_wtp)) {
        stop(sprintf(
          "WTP not found for outcome summary '%s'. Provide explicit wtp parameter.",
          outcome_summary
        ))
      }
    }

    # Get outcome diffs (multiply by WTP)
    outcome_data <- get_summaries(
      results,
      groups = groups,
      summaries = outcome_summary,
      value_type = "outcome",
      discounted = TRUE,
      use_display_names = TRUE,
      interventions = interventions,
      comparators = comparators
    )
    outcome_data$amount <- outcome_data$amount * pair_wtp
    outcome_data$type <- "outcome"

    # Get cost diffs (negate)
    cost_data <- get_summaries(
      results,
      groups = groups,
      summaries = cost_summary,
      value_type = "cost",
      discounted = TRUE,
      use_display_names = TRUE,
      interventions = interventions,
      comparators = comparators
    )
    cost_data$amount <- -cost_data$amount
    cost_data$type <- "cost"

    # Combine
    combined <- dplyr::bind_rows(outcome_data, cost_data) %>%
      dplyr::select("strategy", "group", "type", "value", "amount")

    # Add summary pair display names
    combined$outcome_summary <- pairs$outcome_display[i]
    combined$cost_summary <- pairs$cost_display[i]

    combined
  })

  result <- dplyr::bind_rows(result_list)

  if (nrow(result) == 0) {
    return(tibble::tibble(
      outcome_summary = character(),
      cost_summary = character(),
      strategy = character(),
      group = character(),
      type = character(),
      value = character(),
      amount = numeric()
    ))
  }

  # Reorder columns
  result <- result[, c("outcome_summary", "cost_summary", "strategy", "group",
                        "type", "value", "amount")]

  tibble::as_tibble(result)
}


#' Get Trace Data
#'
#' Returns all state occupancy probabilities in long format as a tidy tibble.
#'
#' @param results A openqaly model results object (output from run_model)
#' @param strategies Character vector of strategy names to include. NULL (default) includes all.
#' @param groups Group selection: "overall" (default), specific group name, vector of groups, or NULL for all
#' @param states Character vector of state names to include. NULL (default) includes all.
#' @param cycles Numeric vector of cycle numbers to include. NULL (default) includes all.
#' @param time_unit Time unit for output: "cycle" (default), "day", "week", "month", or "year"
#'
#' @return A tibble with columns: strategy, group, cycle, state, probability
#'   (plus additional time unit column if time_unit != "cycle")
#'
#' @examples
#' \dontrun{
#' model <- read_model("inst/models/markov_medium")
#' results <- run_model(model)
#' trace_data(results)
#' }
#'
#' @export
trace_data <- function(results,
                       strategies = NULL,
                       groups = "overall",
                       states = NULL,
                       cycles = NULL,
                       time_unit = "cycle") {

  trace <- get_trace(
    results,
    format = "long",
    collapsed = TRUE,
    strategies = strategies,
    groups = groups,
    states = states,
    cycles = cycles,
    time_unit = time_unit,
    use_display_names = TRUE
  )

  tibble::as_tibble(trace)
}


#' Get Cost/Outcome Summary Pairs from Metadata
#'
#' Internal helper that identifies all cost/outcome summary pairs from metadata.
#' Creates the Cartesian product of outcome and cost summaries.
#' Determines summary type by checking the types of constituent values:
#' a summary is "cost" if all its values are cost-type, "outcome" if all
#' its values are outcome-type.
#'
#' @param metadata Results metadata containing summaries and values information
#'
#' @return A data frame with columns: outcome, cost, outcome_display, cost_display
#' @keywords internal
get_summary_pairs <- function(metadata) {
  if (is.null(metadata) || is.null(metadata$summaries)) {
    stop("No summary metadata available to auto-detect cost/outcome pairs.")
  }
  if (is.null(metadata$values)) {
    stop("No value metadata available to determine summary types.")
  }

  summaries <- metadata$summaries
  values_meta <- metadata$values

  # Build value type lookup
  value_types <- stats::setNames(values_meta$type, values_meta$name)

  # Determine effective type for each summary from its constituent values
  effective_types <- vapply(seq_len(nrow(summaries)), function(i) {
    value_names <- trimws(strsplit(summaries$values[i], ",")[[1]])
    types <- unique(value_types[value_names])
    types <- types[!is.na(types)]
    if (length(types) == 1) {
      types
    } else if (length(types) == 0) {
      summaries$type[i]  # fallback to declared type
    } else {
      "mixed"
    }
  }, character(1))

  outcome_idx <- which(effective_types == "outcome")
  cost_idx <- which(effective_types == "cost")

  if (length(outcome_idx) == 0) {
    stop("No outcome summaries found in metadata.")
  }
  if (length(cost_idx) == 0) {
    stop("No cost summaries found in metadata.")
  }

  # Create all outcome x cost pairs
  pairs <- expand.grid(
    outcome_idx = outcome_idx,
    cost_idx = cost_idx,
    stringsAsFactors = FALSE
  )

  data.frame(
    outcome = summaries$name[pairs$outcome_idx],
    cost = summaries$name[pairs$cost_idx],
    outcome_display = map_names(
      summaries$name[pairs$outcome_idx],
      metadata$summaries, "display_name"
    ),
    cost_display = map_names(
      summaries$name[pairs$cost_idx],
      metadata$summaries, "display_name"
    ),
    stringsAsFactors = FALSE
  )
}
