library(heRomod2)
library(microbenchmark)
library(dplyr)

# Load the markov_medium model
model <- system.file("models", "markov_medium", package = "heRomod2") %>%
  read_model()

cat("Testing evaluate_values performance on markov_medium model\n")
cat("Model characteristics:\n")
cat("  States:", length(model$states$name), "\n")
cat("  Values:", nrow(model$values), "\n")
cat("  Variables:", nrow(model$variables), "\n\n")

# Extract the components needed for evaluate_values
# Based on the code in markov.r around line 177-183
uneval_values <- model$values
state_names <- model$states$name
value_names <- unique(model$values$name)

# Create a simple namespace for testing
test_ns <- list(
  df = data.frame(
    cycle = 1:100,
    state_cycle = rep(1, 100)
  ),
  env = new.env()
)
class(test_ns) <- "namespace"

# Test with C++ enabled (default)
cat("Testing with C++ optimization...\n")
result_cpp <- tryCatch({
  evaluate_values(uneval_values, test_ns, value_names, state_names, FALSE)
}, error = function(e) {
  cat("Error with C++:", e$message, "\n")
  NULL
})

# Force R-only implementation
evaluate_values_r_only <- function(df, ns, value_names, state_names, simplify = FALSE) {
  if (nrow(df) == 0) {
    empty_evaluated_values <- tibble::tibble(
      state = character(0),
      destination = character(0),
      max_st = numeric(0),
      state_cycle = numeric(0),
      values_list = list(),
      .rows = 0
    )
    return(dplyr::arrange(empty_evaluated_values, factor(state, levels = state_names)))
  }

  grouped_df_list <- df %>%
    dplyr::group_by(state, destination) %>%
    dplyr::group_split()

  processed_groups_and_na_info <- purrr::map(grouped_df_list, function(x_group) {
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
                              tibble::tibble(
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
      na_report_for_this_group <- dplyr::bind_rows(current_group_na_details_list)

      state_res$state <- x_group$state[1]

      value_names_in_df <- intersect(colnames(state_res), value_names)
      value_names_in_env <- intersect(names(state_ns$env), value_names)

      current_max_st <- x_group$max_st[1]

      expanded_state_res_list <- state_res %>%
        dplyr::group_by(state_cycle) %>%
        dplyr::group_split()

      inner_mapped_rows <- purrr::map(expanded_state_res_list, function(state_cycle_df) {
          expanded_state_values_list <- append(
            as.list(state_cycle_df[ ,value_names_in_df, drop = FALSE]),
            as.list(state_ns$env)[value_names_in_env]
          )

          tibble::tibble(
            state = x_group$state[1],
            destination = x_group$destination[1],
            max_st = current_max_st,
            state_cycle = state_cycle_df$state_cycle[1],
            values_list = list(expanded_state_values_list)
          )
      })

      list(
          data_output = dplyr::bind_rows(inner_mapped_rows),
          na_report = na_report_for_this_group
      )
  })

  mapped_results_list <- purrr::map(processed_groups_and_na_info, "data_output")
  final_evaluated_values <- dplyr::bind_rows(mapped_results_list)
  dplyr::arrange(final_evaluated_values, factor(state, levels = state_names))
}

cat("Testing with R-only implementation...\n")
result_r <- tryCatch({
  evaluate_values_r_only(uneval_values, test_ns, value_names, state_names, FALSE)
}, error = function(e) {
  cat("Error with R:", e$message, "\n")
  NULL
})

if (!is.null(result_cpp) && !is.null(result_r)) {
  cat("\nResults:\n")
  cat("  C++ result rows:", nrow(result_cpp), "\n")
  cat("  R result rows:", nrow(result_r), "\n")

  # Benchmark
  cat("\nBenchmarking (10 iterations)...\n")
  bench <- microbenchmark(
    R_only = evaluate_values_r_only(uneval_values, test_ns, value_names, state_names, FALSE),
    With_Cpp = evaluate_values(uneval_values, test_ns, value_names, state_names, FALSE),
    times = 10
  )

  summary_bench <- summary(bench)
  cat("\nPerformance Results:\n")
  cat("  R-only median time:", sprintf("%.2f ms", summary_bench$median[1]/1000), "\n")
  cat("  C++ median time:", sprintf("%.2f ms", summary_bench$median[2]/1000), "\n")
  cat("  Speedup factor:", sprintf("%.2fx", summary_bench$median[1]/summary_bench$median[2]), "\n")

  if (summary_bench$median[2] > summary_bench$median[1]) {
    cat("\n⚠️ WARNING: C++ implementation is SLOWER than R!\n")
  }
}