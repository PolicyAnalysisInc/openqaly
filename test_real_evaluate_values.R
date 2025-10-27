library(heRomod2)
library(microbenchmark)
library(dplyr)
library(purrr)

cat("Testing evaluate_values optimization with real example...\n\n")

# Use the example from scratch.R or similar
set.seed(123)

# Create a realistic test case - simple Markov model
states <- tibble(
  name = c("Healthy", "Sick", "Dead"),
  initial_probability = c("1", "0", "0"),
  display_name = c("Healthy", "Sick", "Dead"),
  description = c("Healthy state", "Sick state", "Dead state")
)

# Create simple values
values_df <- tibble(
  name = rep(c("cost", "qaly"), each = 9),
  state = rep(rep(c("Healthy", "Sick", "Dead"), each = 3), 2),
  destination = rep(rep(c("Healthy", "Sick", "Dead"), 3), 2),
  formula = c(
    # costs
    "100", "0", "0",      # Healthy -> *
    "0", "500", "0",      # Sick -> *
    "0", "0", "0",        # Dead -> *
    # qalys
    "0.95", "0", "0",     # Healthy -> *
    "0", "0.7", "0",      # Sick -> *
    "0", "0", "0"         # Dead -> *
  ),
  display_name = paste("Value", 1:18),
  description = paste("Value", 1:18),
  type = rep(c("cost", "outcome"), each = 9),
  max_st = 1
)

# Convert formula strings to heRoFormula objects
formula_list <- list()
for (i in 1:nrow(values_df)) {
  formula_list[[i]] <- list(
    quo = rlang::parse_quo(values_df$formula[[i]], env = baseenv()),
    depends = character(0),
    after = character(0),
    fo_depends = character(0)
  )
  class(formula_list[[i]]) <- "heRoFormula"
}
values_df$formula <- formula_list

# Create namespace
test_ns <- list(
  df = data.frame(
    cycle = rep(1:100, each = 1),
    state_cycle = rep(1, 100)
  ),
  env = new.env()
)
class(test_ns) <- "namespace"

value_names <- c("cost", "qaly")
state_names <- c("Healthy", "Sick", "Dead")

# Store original implementation for comparison
evaluate_values_no_cpp <- function(df, ns, value_names, state_names, simplify = FALSE) {

  if (nrow(df) == 0) {
    empty_evaluated_values <- tibble(
      state = character(0),
      destination = character(0),
      max_st = numeric(0),
      state_cycle = numeric(0),
      values_list = list(),
      .rows = 0
    )
    return(arrange(empty_evaluated_values, factor(state, levels = state_names)))
  }

  # Original R implementation without C++ optimization
  grouped_df_list <- df %>%
    group_by(.data$state, .data$destination) %>%
    group_split()

  processed_groups_and_na_info <- map(grouped_df_list, function(x_group) {
      state_ns <- heRomod2:::eval_variables(x_group, heRomod2:::clone_namespace(ns), FALSE)
      state_res <- state_ns$df

      value_cols_in_stateres <- intersect(colnames(state_res), value_names)

      current_group_na_details_list <- list()
      if (nrow(state_res) > 0 && length(value_cols_in_stateres) > 0) {
          for (v_col_name in value_cols_in_stateres) {
              if (v_col_name %in% colnames(state_res)) {
                  if (anyNA(state_res[[v_col_name]])) {
                      na_indices <- which(is.na(state_res[[v_col_name]]))
                      if (length(na_indices) > 0) {
                          current_group_na_details_list[[length(current_group_na_details_list) + 1]] <-
                              tibble(
                                  value_name = v_col_name,
                                  state = x_group$state[1],
                                  destination = if (is.na(x_group$destination[1])) NA_character_ else as.character(x_group$destination[1]),
                                  cycle = state_res$cycle[na_indices],
                                  state_cycle = state_res$state_cycle[na_indices]
                              )
                      }
                  }
              }
          }
      }
      na_report_for_this_group <- bind_rows(current_group_na_details_list)

      state_res$state <- x_group$state[1]

      value_names_in_df <- intersect(colnames(state_res), value_names)
      value_names_in_env <- intersect(names(state_ns$env), value_names)

      current_max_st <- x_group$max_st[1]

      if (simplify && length(value_names_in_df) > 0 && nrow(state_res) > 0) {
        cols_for_pivot <- c("state", "cycle", "state_cycle", value_names_in_df)
        missing_pivot_cols <- setdiff(cols_for_pivot, colnames(state_res))
        if (length(missing_pivot_cols) > 0) {
            warning(paste("evaluate_values simplify: missing columns in state_res:", paste(missing_pivot_cols, collapse=", ")))
        }
      }

      expanded_state_res_list <- state_res %>%
        group_by(state_cycle) %>%
        group_split()

      inner_mapped_rows <- map(expanded_state_res_list, function(state_cycle_df) {
          expanded_state_values_list <- append(
            as.list(state_cycle_df[ ,value_names_in_df, drop = FALSE]),
            as.list(state_ns$env)[value_names_in_env]
          )

          tibble(
            state = x_group$state[1],
            destination = x_group$destination[1],
            max_st = current_max_st,
            state_cycle = state_cycle_df$state_cycle[1],
            values_list = list(expanded_state_values_list)
          )
      })

      list(
          data_output = bind_rows(inner_mapped_rows),
          na_report = na_report_for_this_group
      )
  })

  mapped_results_list <- map(processed_groups_and_na_info, "data_output")
  all_na_reports_list <- map(processed_groups_and_na_info, "na_report")

  all_na_df <- bind_rows(all_na_reports_list)

  if (nrow(all_na_df) > 0) {
    na_summary <- all_na_df %>%
      group_by(value_name, state, destination) %>%
      summarise(
        state_cycles = list(unique(state_cycle)),
        .groups = "drop"
      )

    error_rows <- map_chr(seq_len(nrow(na_summary)), function(i) {
      row <- na_summary[i, ]
      range_str <- format_ranges_for_eval_values(unlist(row$state_cycles))
      glue::glue("| {row$state} | {row$destination} | {row$value_name} | {range_str} |")
    })

    table_string <- paste(
      "| State | Destination | Value | State cycles |",
      "|---|---|---|---|",
      paste(error_rows, collapse = "\n"),
      sep = "\n"
    )

    error_message <- format_error_with_heRo_header(
      "The following values returned NAs during evaluation:",
      table_string
    )
    stop(error_message, call. = FALSE)
  }

  bind_rows(mapped_results_list) %>%
    arrange(factor(state, levels = state_names))
}

# Test with C++ optimization
cat("Testing with C++ optimization:\n")
result_cpp <- tryCatch({
  evaluate_values(values_df, test_ns, value_names, state_names, FALSE)
}, error = function(e) {
  cat("Error with C++:", e$message, "\n")
  NULL
})

if (!is.null(result_cpp)) {
  cat("✓ C++ version succeeded\n")
  cat("  Result: ", nrow(result_cpp), " rows\n\n")

  # Test without C++ optimization
  cat("Testing without C++ optimization:\n")
  result_r <- evaluate_values_no_cpp(values_df, test_ns, value_names, state_names, FALSE)
  cat("✓ R version succeeded\n")
  cat("  Result: ", nrow(result_r), " rows\n\n")

  # Check if results are identical
  cat("Comparing results:\n")
  cat("  Dimensions match: ", identical(dim(result_cpp), dim(result_r)), "\n")
  cat("  States match: ", identical(result_cpp$state, result_r$state), "\n")
  cat("  Destinations match: ", identical(result_cpp$destination, result_r$destination), "\n")

  # Benchmark
  cat("\nBenchmarking performance:\n")
  bench <- microbenchmark(
    R_only = evaluate_values_no_cpp(values_df, test_ns, value_names, state_names, FALSE),
    With_Cpp = evaluate_values(values_df, test_ns, value_names, state_names, FALSE),
    times = 50
  )

  print(bench)

  summary_bench <- summary(bench)
  speedup <- summary_bench$mean[1] / summary_bench$mean[2]
  cat("\n")
  cat("Average time (R only): ", round(summary_bench$mean[1]/1000, 2), " ms\n", sep="")
  cat("Average time (with C++): ", round(summary_bench$mean[2]/1000, 2), " ms\n", sep="")
  cat("Speedup factor: ", round(speedup, 2), "x\n", sep="")
}