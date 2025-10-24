#' Construct an ICER vector with custom printing
#'
#' @export
#' @param dcost   numeric vector of cost differences (intervention - comparator)
#' @param deffect numeric vector of outcome differences (intervention - comparator)
#' @return An object of class "icer" (numeric under the hood) with this encoding:
#'   - More costly & more effective  :  +ICER (ΔC/ΔE)
#'   - Less costly & less effective  :  -ICER (negative whose opposite is ICER of comparator vs referent)
#'   - Dominant (less costly, more effective OR same cost & more effective OR less costly & same effect): 0
#'   - Dominated (more costly, less effective OR ties that imply dominance against): Inf
#'   - Equivalent (same cost AND same effect): NaN
#'   - No comparison (reference strategy with NA deltas): NA (displays as blank)
icer <- function(dcost, deffect) {
  if (length(dcost) != length(deffect))
    stop("dcost and deffect must have the same length.")
  if (!is.numeric(dcost) || !is.numeric(deffect))
    stop("dcost and deffect must be numeric.")

  n <- length(dcost)
  res <- rep(NA_real_, n)

  # Handle NA cases explicitly - leave as NA (will display as blank)
  na_cases <- is.na(dcost) | is.na(deffect)

  # Only process non-NA cases
  valid <- !na_cases
  if (!any(valid)) {
    class(res) <- c("icer", class(res))
    return(res)
  }

  # Process only valid (non-NA) indices
  # Equivalent: no differences at all
  same <- valid & (dcost == 0 & deffect == 0)
  res[same] <- NaN

  # Dominant (weakly): better or equal effects at lower or equal cost with at least one strict improvement
  dominant <- valid & ((deffect > 0 & dcost < 0) | (deffect > 0 & dcost == 0) | (deffect == 0 & dcost < 0))
  res[dominant] <- 0

  # Dominated (weakly): worse or equal effects at higher or equal cost with at least one strict detriment
  dominated <- valid & ((deffect < 0 & dcost > 0) | (deffect < 0 & dcost == 0) | (deffect == 0 & dcost > 0))
  res[dominated] <- Inf

  # Remaining valid cases have ΔE != 0 and are in NE (+/+) or SW (-/-) quadrants
  other <- valid & !(same | dominant | dominated)
  if (any(other)) {
    idx <- which(other)
    ratio <- dcost[idx] / deffect[idx]

    # NE quadrant (+/+): store +ICER
    ne <- dcost[idx] > 0 & deffect[idx] > 0
    res[idx[ne]] <- ratio[ne]

    # SW quadrant (-/-): store negative value whose opposite is ICER of comparator vs. referent
    sw <- dcost[idx] < 0 & deffect[idx] < 0
    res[idx[sw]] <- -ratio[sw]
  }

  class(res) <- c("icer", class(res))
  res
}

#' Format method for ICER (used by tibbles and data frames)
#'
#' Formats ICER values for display in tables and tibbles.
#' This method is called automatically when ICERs are printed in tibbles.
#'
#' @param x An icer object
#' @param digits Number of decimal places (default: 2 for tables)
#' @param big.mark Character to use as thousands separator
#' @param ... Additional arguments (ignored)
#' @return A character vector with formatted ICER values
#' @export
format.icer <- function(x, digits = 2, big.mark = ",", ...) {
  fmt_num <- function(v) {
    prettyNum(round(v, digits = digits),
              big.mark = big.mark,
              preserve.width = "none",
              scientific = FALSE)
  }

  out <- character(length(x))
  out[is.na(x)] <- ""  # Explicit NA handling for blank display (reference strategy)
  out[is.nan(x)] <- "Equivalent"
  out[is.infinite(x)] <- "Dominated"
  out[!is.nan(x) & !is.infinite(x) & !is.na(x) & x == 0] <- "Dominant"

  pos <- !is.nan(x) & !is.infinite(x) & !is.na(x) & x > 0
  out[pos] <- fmt_num(x[pos])

  neg <- !is.nan(x) & !is.infinite(x) & !is.na(x) & x < 0
  out[neg] <- paste0(fmt_num(-x[neg]), "*")  # print the opposite + asterisk

  out
}

#' Printing for ICER vectors
#' Formats values as:
#'   +number = more costly/more effective (ICER)
#'   -number = less costly/less effective (prints as opposite with asterisk)
#'   0       = "Dominant"
#'   Inf     = "Dominated"
#'   NaN     = "Equivalent"
#'   NA      = "" (blank - reference strategy with no comparison)
#' @export
print.icer <- function(x, digits = 3, big.mark = ",", ...) {
  fmt_num <- function(v) {
    prettyNum(round(v, digits = digits),
              big.mark = big.mark,
              preserve.width = "none",
              scientific = FALSE)
  }

  out <- character(length(x))
  out[is.na(x)] <- ""  # Explicit NA handling for blank display (reference strategy)
  out[is.nan(x)] <- "Equivalent"
  out[is.infinite(x)] <- "Dominated"
  out[!is.nan(x) & !is.infinite(x) & !is.na(x) & x == 0] <- "Dominant"

  pos <- !is.nan(x) & !is.infinite(x) & !is.na(x) & x > 0
  out[pos] <- fmt_num(x[pos])

  neg <- !is.nan(x) & !is.infinite(x) & !is.na(x) & x < 0
  out[neg] <- paste0(fmt_num(-x[neg]), "*")  # print the opposite + asterisk

  nm <- names(x)
  if (!is.null(nm)) {
    for (i in seq_along(x)) cat(nm[i], ": ", out[i], "\n", sep = "")
  } else {
    cat(paste(out, collapse = "  "), "\n")
  }
  invisible(x)
}

#' Calculate Incremental Cost-Effectiveness Analysis
#'
#' Performs sequential incremental cost-effectiveness analysis to identify the
#' cost-effectiveness frontier. Follows a 6-step process: (1) sort by cost,
#' (2) calculate incremental cost/outcome, (3) remove dominated strategies,
#' (4) calculate ICERs, (5) remove extended dominance, (6) recalculate final ICERs.
#'
#' @param results A heRomod2 model results object
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param group Group selection: "aggregated" (default), specific group name, or NULL (all groups + aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param strategy_name_field Field to use for strategy names: "name", "display_name", or "abbreviation" (default: "display_name")
#' @param group_name_field Field to use for group names: "name" or "display_name" (default: "display_name")
#' @param summary_name_field Field to use for summary labels: "name" or "display_name" (default: "display_name")
#'
#' @return A tibble with columns:
#'   \item{group}{Group name (mapped via group_name_field)}
#'   \item{strategy}{Strategy name (mapped via strategy_name_field)}
#'   \item{comparator}{Name of strategy being compared against (NA for reference)}
#'   \item{cost}{Total cost}
#'   \item{outcome}{Total outcome}
#'   \item{dcost}{Incremental cost vs. last frontier strategy}
#'   \item{doutcome}{Incremental outcome vs. last frontier strategy}
#'   \item{icer}{ICER (icer class object)}
#'   \item{on_frontier}{Is strategy on the efficiency frontier?}
#'   \item{dominated}{Is strategy dominated (strictly or extended)?}
#'   \item{strictly_dominated}{Is strategy strictly dominated?}
#'   \item{extendedly_dominated}{Is strategy extended dominated?}
#'
#' @details
#' The function implements sequential ICER analysis:
#' \itemize{
#'   \item Strategies are sorted by increasing cost
#'   \item Dominated strategies (more costly, less effective) are identified
#'   \item Extended dominance is detected (ICER higher than next strategy's ICER)
#'   \item Final deltas are always vs. the previous strategy on the frontier
#' }
#'
#' When \code{group = NULL}, analysis is performed separately for each group and aggregated results.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Incremental CE for aggregated population
#' ce <- calculate_incremental_ce(results, "total_qalys", "total_cost")
#'
#' # For all groups
#' ce_all <- calculate_incremental_ce(results, "total_qalys", "total_cost", group = NULL)
#' }
#'
#' @export
calculate_incremental_ce <- function(results,
                                     outcome_summary,
                                     cost_summary,
                                     group = "aggregated",
                                     strategies = NULL,
                                     discounted = FALSE,
                                     strategy_name_field = "display_name",
                                     group_name_field = "display_name",
                                     summary_name_field = "display_name") {

  # Get cost summaries
  cost_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = cost_summary,
    value_type = "cost",
    discounted = discounted,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = "name"  # We don't need value names, just totals
  ) %>%
    group_by(strategy, group) %>%
    summarize(cost = sum(amount, na.rm = TRUE), .groups = "drop")

  # Get outcome summaries
  outcome_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = outcome_summary,
    value_type = "outcome",
    discounted = discounted,
    strategy_name_field = strategy_name_field,
    group_name_field = group_name_field,
    value_name_field = "name"
  ) %>%
    group_by(strategy, group) %>%
    summarize(outcome = sum(amount, na.rm = TRUE), .groups = "drop")

  # Combine cost and outcome data
  combined_data <- cost_data %>%
    inner_join(outcome_data, by = c("strategy", "group"))

  # Perform incremental CE analysis for each group separately
  result_list <- combined_data %>%
    group_by(group) %>%
    group_split() %>%
    lapply(function(grp_data) {
      # Step 1: Sort by cost
      grp_data <- grp_data %>%
        arrange(cost, outcome)

      n_strategies <- nrow(grp_data)

      # Initialize tracking columns
      grp_data$on_frontier <- FALSE
      grp_data$strictly_dominated <- FALSE
      grp_data$extendedly_dominated <- FALSE
      grp_data$dcost <- NA_real_
      grp_data$doutcome <- NA_real_
      grp_data$icer <- icer(rep(0, n_strategies), rep(0, n_strategies))
      grp_data$comparator <- NA_character_

      if (n_strategies == 0) {
        return(grp_data)
      }

      # First strategy is always on the frontier with NA deltas
      grp_data$on_frontier[1] <- TRUE
      grp_data$dcost[1] <- NA_real_
      grp_data$doutcome[1] <- NA_real_
      grp_data$icer[1] <- NA_real_
      grp_data$comparator[1] <- NA_character_

      if (n_strategies == 1) {
        return(grp_data)
      }

      # Steps 2-3: Calculate incrementals and identify dominated strategies
      frontier_idx <- 1

      for (i in 2:n_strategies) {
        # Calculate vs. last frontier strategy
        prev_idx <- frontier_idx
        grp_data$dcost[i] <- grp_data$cost[i] - grp_data$cost[prev_idx]
        grp_data$doutcome[i] <- grp_data$outcome[i] - grp_data$outcome[prev_idx]
        grp_data$comparator[i] <- grp_data$strategy[prev_idx]

        # Calculate ICER
        grp_data$icer[i] <- icer(grp_data$dcost[i], grp_data$doutcome[i])

        # Check for dominance (more costly and less/equal effective)
        if (grp_data$dcost[i] >= 0 && grp_data$doutcome[i] <= 0) {
          grp_data$strictly_dominated[i] <- TRUE
          # Keep comparisons vs. last frontier strategy
        } else {
          # Not dominated, add to frontier
          grp_data$on_frontier[i] <- TRUE
          frontier_idx <- i
        }
      }

      # Step 4-5: Detect extended dominance
      # Extended dominance: ICER[i] > ICER[j] where j > i on the frontier
      frontier_indices <- which(grp_data$on_frontier)

      if (length(frontier_indices) > 2) {
        # Check each frontier strategy (except first and last)
        for (idx in 2:(length(frontier_indices) - 1)) {
          i <- frontier_indices[idx]
          j <- frontier_indices[idx + 1]

          # If current ICER > next ICER, current is extended dominated
          if (!is.na(grp_data$icer[i]) && !is.na(grp_data$icer[j]) &&
              is.finite(grp_data$icer[i]) && is.finite(grp_data$icer[j]) &&
              grp_data$icer[i] > grp_data$icer[j]) {
            grp_data$extendedly_dominated[i] <- TRUE
            grp_data$on_frontier[i] <- FALSE
          }
        }

        # Step 6: Recalculate ICERs after extended dominance removal
        frontier_indices <- which(grp_data$on_frontier)

        for (idx in 2:length(frontier_indices)) {
          i <- frontier_indices[idx]
          prev_i <- frontier_indices[idx - 1]

          # Recalculate vs. previous frontier strategy
          grp_data$dcost[i] <- grp_data$cost[i] - grp_data$cost[prev_i]
          grp_data$doutcome[i] <- grp_data$outcome[i] - grp_data$outcome[prev_i]
          grp_data$icer[i] <- icer(grp_data$dcost[i], grp_data$doutcome[i])
          grp_data$comparator[i] <- grp_data$strategy[prev_i]
        }
      }

      # Create combined dominated column
      grp_data$dominated <- grp_data$strictly_dominated | grp_data$extendedly_dominated

      grp_data
    })

  # Combine results from all groups
  result <- bind_rows(result_list)

  # Return with proper column order
  result %>%
    select(group, strategy, comparator, cost, outcome, dcost, doutcome, icer,
           on_frontier, dominated, strictly_dominated, extendedly_dominated)
}


#' Calculate Pairwise Cost-Effectiveness Comparisons
#'
#' Performs pairwise cost-effectiveness analysis comparing all strategies against
#' a single reference strategy (either comparator or referent). Unlike incremental
#' CE which compares along the efficiency frontier, this creates independent
#' comparisons useful for evaluating specific interventions.
#'
#' @param results A heRomod2 model results object
#' @param outcome_summary Name of the outcome summary to use (e.g., "total_qalys")
#' @param cost_summary Name of the cost summary to use (e.g., "total_cost")
#' @param group Group selection: "aggregated" (default), specific group name, or NULL (all groups + aggregated)
#' @param strategies Character vector of strategy names to include (NULL for all)
#' @param referent Single reference strategy for intervention perspective (e.g., "new_treatment").
#'   If provided, shows referent - comparator comparisons. Mutually exclusive with comparator.
#' @param comparator Single reference strategy for comparator perspective (e.g., "control").
#'   If provided, shows intervention - comparator comparisons. Mutually exclusive with referent.
#' @param discounted Logical. Use discounted values? (default: FALSE)
#' @param strategy_name_field Field to use for strategy names: "name", "display_name", or "abbreviation" (default: "display_name")
#' @param group_name_field Field to use for group names: "name" or "display_name" (default: "display_name")
#' @param summary_name_field Field to use for summary labels: "name" or "display_name" (default: "display_name")
#'
#' @return A tibble with columns:
#'   \item{group}{Group name (mapped via group_name_field)}
#'   \item{strategy}{Strategy name (mapped via strategy_name_field)}
#'   \item{comparator}{Name of strategy being compared against}
#'   \item{cost}{Total cost of the strategy}
#'   \item{outcome}{Total outcome of the strategy}
#'   \item{dcost}{Incremental cost vs. comparator}
#'   \item{doutcome}{Incremental outcome vs. comparator}
#'   \item{icer}{ICER (icer class object)}
#'
#' @details
#' The function creates pairwise comparisons based on the perspective:
#' \itemize{
#'   \item When comparator is specified: Creates N-1 comparisons showing (strategy - comparator) for each non-comparator strategy
#'   \item When referent is specified: Creates N-1 comparisons showing (referent - strategy) for each non-referent strategy
#' }
#'
#' Exactly one of referent or comparator must be provided.
#'
#' When \code{group = NULL}, analysis is performed separately for each group and aggregated results.
#'
#' @examples
#' \dontrun{
#' model <- read_model(system.file("models/example_psm", package = "heRomod2"))
#' results <- run_model(model)
#'
#' # Pairwise comparisons vs control (comparator perspective)
#' pw_ce <- calculate_pairwise_ce(results, "total_qalys", "total_cost",
#'                                 comparator = "control")
#'
#' # Pairwise comparisons from new treatment perspective (referent)
#' pw_ce <- calculate_pairwise_ce(results, "total_qalys", "total_cost",
#'                                 referent = "new_treatment")
#' }
#'
#' @export
calculate_pairwise_ce <- function(results,
                                  outcome_summary,
                                  cost_summary,
                                  group = "aggregated",
                                  strategies = NULL,
                                  referent = NULL,
                                  comparator = NULL,
                                  discounted = FALSE,
                                  strategy_name_field = "display_name",
                                  group_name_field = "display_name",
                                  summary_name_field = "display_name") {

  # Validate that exactly one of referent or comparator is provided
  if (is.null(referent) && is.null(comparator)) {
    stop("One of 'referent' or 'comparator' must be provided")
  }
  if (!is.null(referent) && !is.null(comparator)) {
    stop("Only one of 'referent' or 'comparator' should be provided, not both")
  }

  # Get cost summaries (use technical names internally for matching)
  cost_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = cost_summary,
    value_type = "cost",
    discounted = discounted,
    strategy_name_field = "name",  # Use technical names internally
    group_name_field = "name",  # Use technical names internally
    value_name_field = "name"  # We don't need value names, just totals
  ) %>%
    group_by(strategy, group) %>%
    summarize(cost = sum(amount, na.rm = TRUE), .groups = "drop")

  # Get outcome summaries (use technical names internally for matching)
  outcome_data <- get_summaries(
    results,
    group = group,
    strategies = strategies,
    summaries = outcome_summary,
    value_type = "outcome",
    discounted = discounted,
    strategy_name_field = "name",  # Use technical names internally
    group_name_field = "name",  # Use technical names internally
    value_name_field = "name"
  ) %>%
    group_by(strategy, group) %>%
    summarize(outcome = sum(amount, na.rm = TRUE), .groups = "drop")

  # Combine cost and outcome data
  combined_data <- cost_data %>%
    inner_join(outcome_data, by = c("strategy", "group"))

  # Determine reference strategy
  reference_strategy <- if (!is.null(comparator)) comparator else referent

  # Perform pairwise comparisons for each group separately
  result_list <- combined_data %>%
    group_by(group) %>%
    group_split() %>%
    lapply(function(grp_data) {
      # Get reference strategy data
      ref_data <- grp_data %>%
        filter(strategy == reference_strategy)

      if (nrow(ref_data) == 0) {
        stop(sprintf("Reference strategy '%s' not found in group '%s'",
                     reference_strategy, grp_data$group[1]))
      }

      ref_cost <- ref_data$cost[1]
      ref_outcome <- ref_data$outcome[1]

      # Get all other strategies
      other_strategies <- grp_data %>%
        filter(strategy != reference_strategy)

      if (nrow(other_strategies) == 0) {
        # No comparisons to make
        return(tibble())
      }

      # Create comparison rows
      if (!is.null(comparator)) {
        # Comparator perspective: (strategy - comparator)
        comparisons <- other_strategies %>%
          mutate(
            comparator = reference_strategy,
            dcost = cost - ref_cost,
            doutcome = outcome - ref_outcome,
            icer = icer(dcost, doutcome)
          )
      } else {
        # Referent perspective: (referent - strategy)
        # Here, each other strategy becomes the comparator
        comparisons <- other_strategies %>%
          mutate(
            comparator = strategy,  # The other strategy is the comparator
            strategy = reference_strategy,  # Referent is the strategy
            cost = ref_cost,  # Use referent's values
            outcome = ref_outcome,
            dcost = ref_cost - other_strategies$cost,
            doutcome = ref_outcome - other_strategies$outcome,
            icer = icer(dcost, doutcome)
          )
      }

      comparisons
    })

  # Combine results from all groups
  result <- bind_rows(result_list)

  # Map strategy names for display
  if (!is.null(results$metadata) && !is.null(results$metadata$strategies) &&
      strategy_name_field != "name") {
    result$strategy <- map_names(result$strategy, results$metadata$strategies,
                                 strategy_name_field)
    result$comparator <- map_names(result$comparator, results$metadata$strategies,
                                   strategy_name_field)
  }

  # Map group names for display
  if (!is.null(results$metadata) && !is.null(results$metadata$groups) &&
      group_name_field != "name") {
    result$group <- map_names(result$group, results$metadata$groups,
                             group_name_field)
  }

  # Return with proper column order
  result %>%
    select(group, strategy, comparator, cost, outcome, dcost, doutcome, icer)
}
