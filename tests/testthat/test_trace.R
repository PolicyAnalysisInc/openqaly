context("Trace output")

# =============================================================================
# Trace Extraction Tests
# =============================================================================

test_that("get_trace extracts data in long format", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get trace in long format
  trace_long <- get_trace(results, format = "long")

  # Check structure
  expect_s3_class(trace_long, "data.frame")
  expect_true("cycle" %in% colnames(trace_long))
  expect_true("state" %in% colnames(trace_long))
  expect_true("probability" %in% colnames(trace_long))
  expect_true("strategy" %in% colnames(trace_long))

  # Check that probabilities are valid
  expect_true(all(trace_long$probability >= 0))
  expect_true(all(trace_long$probability <= 1))

  # Check that we have data for all strategies (default uses display names)
  expect_setequal(unique(trace_long$strategy), c("Chemotherapy", "Targeted Therapy", "Immunotherapy"))

  # Check that we have all three states (default uses display names)
  expect_equal(length(unique(trace_long$state)), 3)
  expect_true(all(c("Progression Free", "Progressed", "Dead") %in% unique(trace_long$state)))

  # Verify technical names work when explicitly requested
  trace_long_technical <- get_trace(results, format = "long",
                                     use_display_names = FALSE)
  expect_setequal(unique(trace_long_technical$strategy), c("chemo", "targeted", "immuno"))
  expect_setequal(unique(trace_long_technical$state), c("progression_free", "progressed", "dead"))
})


test_that("get_trace extracts data in wide format", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get trace in wide format
  trace_wide <- get_trace(results, format = "wide")

  # Check structure
  expect_s3_class(trace_wide, "data.frame")
  expect_true("cycle" %in% colnames(trace_wide))
  expect_true("strategy" %in% colnames(trace_wide))

  # Check that state columns exist
  time_and_meta_cols <- c("strategy", "group", "cycle", "day", "week", "month", "year")
  state_cols <- setdiff(colnames(trace_wide), time_and_meta_cols)
  expect_equal(length(state_cols), 3)
})


test_that("get_trace extracts data in matrix format", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Get trace in matrix format
  trace_matrices <- get_trace(results, format = "matrix")

  # Check structure
  expect_type(trace_matrices, "list")
  expect_true(length(trace_matrices) >= 2)

  # Check first matrix
  first_matrix <- trace_matrices[[1]]
  expect_true(is.matrix(first_matrix))
  expect_equal(ncol(first_matrix), 3)  # 3 states
  expect_true(nrow(first_matrix) > 0)  # Has cycles
})


test_that("get_trace filters by strategy", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Filter to one strategy (filtering uses technical names)
  # Result defaults to display names
  trace_filtered <- get_trace(results, format = "long", strategies = "chemo")

  # Check that only chemo strategy is present (returns display name)
  expect_equal(unique(trace_filtered$strategy), "Chemotherapy")

  # Test filtering with explicit technical name output
  trace_filtered_tech <- get_trace(results, format = "long", strategies = "chemo",
                                   use_display_names = FALSE)
  expect_equal(unique(trace_filtered_tech$strategy), "chemo")
})


test_that("get_trace filters by states", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Filter to specific states (filtering uses technical names)
  # Result defaults to display names
  trace_filtered <- get_trace(results, format = "long", states = c("progression_free", "dead"))

  # Check that only specified states are present (returns display names)
  expect_setequal(unique(trace_filtered$state), c("Progression Free", "Dead"))

  # Test filtering with explicit technical name output
  trace_filtered_tech <- get_trace(results, format = "long",
                                   states = c("progression_free", "dead"),
                                   use_display_names = FALSE)
  expect_setequal(unique(trace_filtered_tech$state), c("progression_free", "dead"))

  # Verify filtering works
  expect_equal(length(unique(trace_filtered_tech$state)), 2)
})


test_that("get_trace filters by cycles", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Filter to specific cycles
  trace_filtered <- get_trace(results, format = "long", cycles = 0:10)

  # Check that only specified cycles are present
  expect_true(all(trace_filtered$cycle %in% 0:10))
  expect_true(max(trace_filtered$cycle) <= 10)
})

# =============================================================================
# Trace Visualization Tests
# =============================================================================

test_that("trace_plot_area creates a ggplot object", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create plot
  p <- trace_plot_area(results)

  # Check that it's a ggplot
  expect_s3_class(p, "ggplot")

  # Check that it has the expected layers
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomArea"))))
})


test_that("trace_plot_area auto-facets with multiple strategies", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create plot (should auto-facet when there are multiple strategies)
  p <- trace_plot_area(results)

  # Check that it's faceted (auto-facets when there are multiple strategies)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("trace_plot_line creates a ggplot with lines", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create line plot
  p <- trace_plot_line(results)

  # Check that it's a ggplot with line geom
  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

# =============================================================================
# Trace Export Tests
# =============================================================================

test_that("export_trace exports to CSV", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create temp file
  temp_file <- tempfile(fileext = ".csv")

  # Export
  export_trace(results, temp_file)

  # Check file exists
  expect_true(file.exists(temp_file))

  # Check content
  trace_read <- read.csv(temp_file)
  expect_s3_class(trace_read, "data.frame")
  expect_true(nrow(trace_read) > 0)

  # Clean up
  unlink(temp_file)
})


test_that("export_trace exports to Excel", {
  skip_if_not_installed("openxlsx")

  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create temp file
  temp_file <- tempfile(fileext = ".xlsx")

  # Export with separate sheets
  export_trace(results, temp_file, separate_sheets = TRUE)

  # Check file exists
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})


test_that("export_trace exports to JSON", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create temp file
  temp_file <- tempfile(fileext = ".json")

  # Export
  export_trace(results, temp_file)

  # Check file exists
  expect_true(file.exists(temp_file))

  # Check content
  trace_read <- jsonlite::read_json(temp_file, simplifyVector = TRUE)
  expect_type(trace_read, "list")
  expect_true(length(trace_read) > 0)

  # Clean up
  unlink(temp_file)
})


test_that("export_trace infers format from extension", {
  model_path <- system.file("models/example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create temp file with .csv extension
  temp_file <- tempfile(fileext = ".csv")

  # Export without specifying format
  export_trace(results, temp_file, format = NULL)

  # Check file exists
  expect_true(file.exists(temp_file))

  # Clean up
  unlink(temp_file)
})

# =============================================================================
# Trace Metadata and Name Fields Tests
# =============================================================================

test_that("metadata is stored in parse_model", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years",
                timeframe = 10, timeframe_unit = "years") |>
    add_state("healthy", display_name = "Healthy", initial_prob = 1) |>
    add_state("sick", display_name = "Sick", initial_prob = 0) |>
    add_state("dead", display_name = "Dead", initial_prob = 0) |>
    add_transition("healthy", "healthy", 1 - 0.1 - 0.01) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("healthy", "dead", 0.01) |>
    add_transition("sick", "sick", 1 - 0.05) |>
    add_transition("sick", "dead", 0.05) |>
    add_transition("dead", "dead", 1) |>
    add_strategy("standard", display_name = "Standard Care") |>
    add_strategy("treatment", display_name = "New Treatment")

  results <- run_model(model)

  # Check that metadata exists
  expect_true(!is.null(results$metadata))
  expect_true(!is.null(results$metadata$states))
  expect_true(!is.null(results$metadata$strategies))

  # Check that metadata has the right fields
  expect_true("display_name" %in% colnames(results$metadata$states))
  expect_true("display_name" %in% colnames(results$metadata$strategies))

  # Check that values are preserved
  expect_equal(results$metadata$states$display_name, c("Healthy", "Sick", "Dead"))
  expect_equal(results$metadata$strategies$display_name, c("Standard Care", "New Treatment"))
})


test_that("map_names function works correctly", {
  # Create sample metadata
  metadata <- tibble::tibble(
    name = c("state1", "state2", "state3"),
    display_name = c("State One", "State Two", "State Three")
  )

  # Test mapping to display_name
  names <- c("state1", "state3")
  mapped <- openqaly:::map_names(names, metadata, "display_name")
  expect_equal(mapped, c("State One", "State Three"))

  # Test with missing field (should warn and fallback)
  expect_warning(
    mapped <- openqaly:::map_names(names, metadata, "nonexistent"),
    "Field 'nonexistent' not found"
  )
  expect_equal(mapped, names)

  # Test with NULL metadata
  mapped <- openqaly:::map_names(names, NULL, "display_name")
  expect_equal(mapped, names)
})


test_that("trace_table uses name fields correctly", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  model <- define_model("markov") |>
    set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years",
                timeframe = 5, timeframe_unit = "years") |>
    add_state("pf", display_name = "Progression-Free", initial_prob = 1) |>
    add_state("pd", display_name = "Progressive Disease", initial_prob = 0) |>
    add_state("dead", display_name = "Death", initial_prob = 0) |>
    add_transition("pf", "pf", 1 - 0.1 - 0.01) |>
    add_transition("pf", "pd", 0.1) |>
    add_transition("pf", "dead", 0.01) |>
    add_transition("pd", "pd", 1 - 0.05) |>
    add_transition("pd", "dead", 0.05) |>
    add_transition("dead", "dead", 1) |>
    add_strategy("std", display_name = "Standard of Care") |>
    add_strategy("new", display_name = "Novel Therapy")

  results <- run_model(model)

  # Test that trace_table produces valid output
  ft_display <- trace_table(results, cycles = 0:3, table_format = "flextable")
  ft_data <- ft_display$body$dataset

  # The flextable should have data
  expect_true(ncol(ft_data) > 0)
  expect_s3_class(ft_display, "flextable")
})


test_that("name fields work with missing metadata", {
  skip_if_not_installed("flextable")

  # Create a model and manually remove metadata to test fallback
  model <- define_model("markov") |>
    set_settings(n_cycles = 5, cycle_length = 1, cycle_length_unit = "years",
                timeframe = 5, timeframe_unit = "years") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Remove metadata to test fallback
  results$metadata <- NULL

  # Should work without error using technical names
  ft <- trace_table(results, cycles = 0:3, table_format = "flextable")
  expect_s3_class(ft, "flextable")
})

# =============================================================================
# Time Unit Tests
# =============================================================================

test_that("time variables are stored in trace", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 10,
                 cycle_length = 7,
                 cycle_length_unit = "days",
                 timeframe = 70,
                 timeframe_unit = "days") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Check that collapsed trace includes time columns
  trace_data <- results$aggregated$collapsed_trace[[1]]

  # Should be a data frame with time columns
  expect_true(is.data.frame(trace_data))
  expect_true("cycle" %in% colnames(trace_data))
  expect_true("day" %in% colnames(trace_data))
  expect_true("week" %in% colnames(trace_data))
  expect_true("month" %in% colnames(trace_data))
  expect_true("year" %in% colnames(trace_data))

  # Check that time values are calculated correctly
  # With cycle_length = 7 days:
  expect_equal(trace_data$cycle[1:3], c(1, 2, 3))
  expect_equal(trace_data$day[1:3], c(7, 14, 21))
  expect_equal(trace_data$week[1:3], c(1, 2, 3))
})


test_that("get_trace respects time_unit parameter", {
  model <- define_model("markov") |>
    set_settings(n_cycles = 5,
                 cycle_length = 1,
                 cycle_length_unit = "months",
                 timeframe = 5,
                 timeframe_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Test different time units
  trace_cycles <- get_trace(results, format = "long", time_unit = "cycle")
  expect_true("cycle" %in% colnames(trace_cycles))
  expect_equal(unique(trace_cycles$cycle), 1:6)

  trace_days <- get_trace(results, format = "long", time_unit = "day")
  expect_true("day" %in% colnames(trace_days))

  trace_months <- get_trace(results, format = "long", time_unit = "month")
  expect_true("month" %in% colnames(trace_months))

  trace_years <- get_trace(results, format = "long", time_unit = "year")
  expect_true("year" %in% colnames(trace_years))
})


test_that("plot functions use time_unit parameter", {
  skip_if_not_installed("ggplot2")

  model <- define_model("markov") |>
    set_settings(n_cycles = 12,
                 cycle_length = 1,
                 cycle_length_unit = "months",
                 timeframe = 12,
                 timeframe_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.95) |>
    add_transition("healthy", "sick", 0.05) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Test trace_plot_area with different time units
  p_cycles <- trace_plot_area(results, time_unit = "cycle")
  expect_s3_class(p_cycles, "ggplot")
  expect_equal(p_cycles$labels$x, "Cycle")

  p_months <- trace_plot_area(results, time_unit = "month")
  expect_s3_class(p_months, "ggplot")
  expect_equal(p_months$labels$x, "Months")

  p_years <- trace_plot_area(results, time_unit = "year")
  expect_s3_class(p_years, "ggplot")
  expect_equal(p_years$labels$x, "Years")

  # Test trace_plot_line with different time units
  p_lines_weeks <- trace_plot_line(results, time_unit = "week")
  expect_s3_class(p_lines_weeks, "ggplot")
  expect_equal(p_lines_weeks$labels$x, "Weeks")
})


test_that("table function uses time_unit parameter", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  model <- define_model("markov") |>
    set_settings(n_cycles = 4,
                 cycle_length = 1,
                 cycle_length_unit = "weeks",
                 timeframe = 4,
                 timeframe_unit = "weeks") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Test table with different time units
  ft_cycles <- trace_table(results, time_unit = "cycle", cycles = 1:3, table_format = "flextable")
  expect_s3_class(ft_cycles, "flextable")

  ft_weeks <- trace_table(results, time_unit = "week", cycles = 1:3, table_format = "flextable")
  expect_s3_class(ft_weeks, "flextable")

  ft_days <- trace_table(results, time_unit = "day", cycles = 1:3, table_format = "flextable")
  expect_s3_class(ft_days, "flextable")
})


test_that("backward compatibility is maintained", {
  # Test that models without time columns still work
  model <- define_model("markov") |>
    set_settings(n_cycles = 5,
                 cycle_length = 1,
                 cycle_length_unit = "months",
                 timeframe = 5,
                 timeframe_unit = "months") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_transition("healthy", "healthy", 0.9) |>
    add_transition("healthy", "sick", 0.1) |>
    add_transition("sick", "sick", 1) |>
    add_strategy("standard")

  results <- run_model(model)

  # Should still work with default time_unit = "cycle"
  trace_data <- get_trace(results, format = "long")
  expect_true("cycle" %in% colnames(trace_data))

  # Requesting other time units should warn but not fail
  expect_warning(
    trace_years <- get_trace(results, format = "long", time_unit = "year"),
    NA  # Allow warning or no warning
  )
})
