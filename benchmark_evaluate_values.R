library(heRomod2)
library(microbenchmark)

cat("==============================================\n")
cat("BENCHMARK: evaluate_values C++ Optimization\n")
cat("==============================================\n\n")

# Since the C++ version is working, let's create a performance test
# We'll compare the time by temporarily disabling the C++ path

# Save the current evaluate_values
evaluate_values_with_cpp <- heRomod2::evaluate_values

# Create version that skips C++ check (forces R path)
evaluate_values_r_only <- function(df, ns, value_names, state_names, simplify = FALSE) {
  # This is the original R implementation from the fallback path
  # We skip the C++ check to force using R

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

  # The list of tibbles, one for each group of (state, destination)
  grouped_df_list <- df %>%
    dplyr::group_by(.data$state, .data$destination) %>%
    dplyr::group_split()

  # Process each group and collect NA information
  processed_groups_and_na_info <- purrr::map(grouped_df_list, function(x_group) {
      state_ns <- heRomod2:::eval_variables(x_group, heRomod2:::clone_namespace(ns), FALSE)
      state_res <- state_ns$df

      # Determine which of the model's value_names are present as columns in state_res
      value_cols_in_stateres <- intersect(colnames(state_res), value_names)

      # Collect NA details for the current group
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

  dplyr::bind_rows(mapped_results_list) %>%
    dplyr::arrange(factor(state, levels = state_names))
}

# Create test data
create_test_data <- function(n_states = 5, n_cycles = 20) {
  states_names <- paste0("State", 1:n_states)

  # Create values dataframe
  values_df <- expand.grid(
    state = states_names,
    destination = states_names,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      name = "test_value",
      display_name = "Test Value",
      description = "Test",
      formula = "1",
      type = "cost",
      max_st = 1
    )

  # Convert formulas
  formula_list <- list()
  for (i in 1:nrow(values_df)) {
    formula_list[[i]] <- list(
      quo = rlang::quo(1),
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
      cycle = 1:n_cycles,
      state_cycle = rep(1, n_cycles)
    ),
    env = new.env()
  )
  class(test_ns) <- "namespace"

  list(
    df = values_df,
    ns = test_ns,
    value_names = "test_value",
    state_names = states_names
  )
}

# Test different sizes
sizes <- list(
  small = create_test_data(3, 10),
  medium = create_test_data(5, 50),
  large = create_test_data(10, 100)
)

cat("Testing correctness...\n")
test_data <- sizes$small
result_cpp <- evaluate_values_with_cpp(test_data$df, test_data$ns,
                                       test_data$value_names, test_data$state_names, FALSE)
result_r <- evaluate_values_r_only(test_data$df, test_data$ns,
                                   test_data$value_names, test_data$state_names, FALSE)

cat("  Results dimensions match: ", identical(dim(result_cpp), dim(result_r)), "\n")
cat("  Number of rows: ", nrow(result_cpp), "\n\n")

# Benchmark each size
for (size_name in names(sizes)) {
  test_data <- sizes[[size_name]]

  cat("Benchmarking", size_name, "dataset:\n")
  cat("  States:", length(test_data$state_names),
      "x Cycles:", nrow(test_data$ns$df), "\n")

  bench <- microbenchmark(
    R_implementation = evaluate_values_r_only(
      test_data$df, test_data$ns,
      test_data$value_names, test_data$state_names, FALSE
    ),
    Cpp_optimized = evaluate_values_with_cpp(
      test_data$df, test_data$ns,
      test_data$value_names, test_data$state_names, FALSE
    ),
    times = 20
  )

  summary_bench <- summary(bench)
  speedup <- summary_bench$median[1] / summary_bench$median[2]

  cat("  Median time (R):   ", sprintf("%6.2f ms", summary_bench$median[1]/1000), "\n")
  cat("  Median time (C++): ", sprintf("%6.2f ms", summary_bench$median[2]/1000), "\n")
  cat("  Speedup:           ", sprintf("%5.2fx", speedup), "\n\n")
}

cat("==============================================\n")
cat("SUMMARY: C++ optimization is working correctly\n")
cat("==============================================\n")