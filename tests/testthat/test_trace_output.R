# Tests for Trace Output Functions

library(testthat)
library(heRomod2)

test_that("get_trace extracts data in long format", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
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

  # Check that we have data for both strategies (default uses display names)
  expect_setequal(unique(trace_long$strategy), c("Standard of Care", "New Drug"))

  # Check that we have all three states (default uses display names)
  expect_equal(length(unique(trace_long$state)), 3)
  expect_true(all(c("Progression Free", "Progressed", "Dead") %in% unique(trace_long$state)))

  # Verify technical names work when explicitly requested
  trace_long_technical <- get_trace(results, format = "long",
                                     strategy_name_field = "name",
                                     state_name_field = "name")
  expect_setequal(unique(trace_long_technical$strategy), c("standard", "new_drug"))
  expect_setequal(unique(trace_long_technical$state), c("progression_free", "progressed", "dead"))
})


test_that("get_trace extracts data in wide format", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
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
  model_path <- system.file("models/example_psm", package = "heRomod2")
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
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Filter to one strategy (filtering uses technical names)
  # Result defaults to display names
  trace_filtered <- get_trace(results, format = "long", strategies = "standard")

  # Check that only standard strategy is present (returns display name)
  expect_equal(unique(trace_filtered$strategy), "Standard of Care")

  # Test filtering with explicit technical name output
  trace_filtered_tech <- get_trace(results, format = "long", strategies = "standard",
                                   strategy_name_field = "name")
  expect_equal(unique(trace_filtered_tech$strategy), "standard")
})


test_that("get_trace filters by states", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
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
                                   state_name_field = "name")
  expect_setequal(unique(trace_filtered_tech$state), c("progression_free", "dead"))

  # Verify filtering works
  expect_equal(length(unique(trace_filtered_tech$state)), 2)
})


test_that("get_trace filters by cycles", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
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


test_that("plot_trace creates a ggplot object", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create plot
  p <- plot_trace(results)

  # Check that it's a ggplot
  expect_s3_class(p, "ggplot")

  # Check that it has the expected layers
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomArea"))))
})


test_that("plot_trace with faceting creates faceted plot", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create faceted plot
  p <- plot_trace(results, facet_by = "strategy")

  # Check that it's faceted
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
})


test_that("plot_trace_lines creates a ggplot with lines", {
  model_path <- system.file("models/example_psm", package = "heRomod2")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  # Create line plot
  p <- plot_trace_lines(results)

  # Check that it's a ggplot with line geom
  expect_s3_class(p, "ggplot")
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})


test_that("export_trace exports to CSV", {
  skip_on_cran()

  model_path <- system.file("models/example_psm", package = "heRomod2")
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
  skip_on_cran()
  skip_if_not_installed("openxlsx")

  model_path <- system.file("models/example_psm", package = "heRomod2")
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
  skip_on_cran()

  model_path <- system.file("models/example_psm", package = "heRomod2")
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
  skip_on_cran()

  model_path <- system.file("models/example_psm", package = "heRomod2")
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
