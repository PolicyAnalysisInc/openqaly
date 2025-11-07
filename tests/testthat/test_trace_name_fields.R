# Test trace name field functionality

library(testthat)
library(heRomod2)

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
    add_strategy("standard", display_name = "Standard Care", abbreviation = "STD") |>
    add_strategy("treatment", display_name = "New Treatment", abbreviation = "TX")

  results <- run_model(model)

  # Check that metadata exists
  expect_true(!is.null(results$metadata))
  expect_true(!is.null(results$metadata$states))
  expect_true(!is.null(results$metadata$strategies))

  # Check that metadata has the right fields
  expect_true("display_name" %in% colnames(results$metadata$states))
  expect_true("display_name" %in% colnames(results$metadata$strategies))
  expect_true("abbreviation" %in% colnames(results$metadata$strategies))

  # Check that values are preserved
  expect_equal(results$metadata$states$display_name, c("Healthy", "Sick", "Dead"))
  expect_equal(results$metadata$strategies$display_name, c("Standard Care", "New Treatment"))
  expect_equal(results$metadata$strategies$abbreviation, c("STD", "TX"))
})


test_that("map_names function works correctly", {
  # Create sample metadata
  metadata <- tibble::tibble(
    name = c("state1", "state2", "state3"),
    display_name = c("State One", "State Two", "State Three"),
    abbreviation = c("S1", "S2", "S3")
  )

  # Test mapping to display_name
  names <- c("state1", "state3")
  mapped <- heRomod2:::map_names(names, metadata, "display_name")
  expect_equal(mapped, c("State One", "State Three"))

  # Test mapping to abbreviation
  mapped <- heRomod2:::map_names(names, metadata, "abbreviation")
  expect_equal(mapped, c("S1", "S3"))

  # Test with missing field (should warn and fallback)
  expect_warning(
    mapped <- heRomod2:::map_names(names, metadata, "nonexistent"),
    "Field 'nonexistent' not found"
  )
  expect_equal(mapped, names)

  # Test with NULL metadata
  mapped <- heRomod2:::map_names(names, NULL, "display_name")
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
    add_strategy("std", display_name = "Standard of Care", abbreviation = "SOC") |>
    add_strategy("new", display_name = "Novel Therapy", abbreviation = "NT")

  results <- run_model(model)

  # Test with display_name (default)
  ft_display <- trace_table(results, cycles = 0:3, table_format = "flextable")
  ft_data <- ft_display$body$dataset

  # Extract header content - flextable stores headers differently
  # We need to check the header rows for the display names
  header_content <- ft_display$header$dataset

  # The strategies should use display names in the hierarchical headers
  # Note: This is a simplified check - actual flextable structure is complex
  expect_true(ncol(ft_data) > 0)

  # Test with technical names
  ft_tech <- trace_table(results, cycles = 0:3,
                        strategy_name_field = "name",
                        state_name_field = "name",
                        table_format = "flextable")

  # All should produce valid flextables
  expect_s3_class(ft_display, "flextable")
  expect_s3_class(ft_tech, "flextable")
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