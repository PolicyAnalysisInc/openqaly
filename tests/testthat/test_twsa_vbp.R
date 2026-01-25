context("TWSA+VBP")

# ============================================================================
# Test Fixtures
# ============================================================================

get_example_model <- function() {
  model_path <- system.file("models", "example_markov", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_markov"
  }
  read_model(model_path)
}

run_example_twsa_vbp <- function() {
  model <- get_example_model()

  # Add TWSA with parameters that affect outcomes (NOT c_treatment which is VBP price)
  model <- model |>
    add_twsa("Utility Sensitivity") |>
    add_twsa_variable("Utility Sensitivity", "u_mild",
      type = "range", min = 0.75, max = 0.95, steps = 3) |>
    add_twsa_variable("Utility Sensitivity", "u_severe",
      type = "range", min = 0.40, max = 0.70, steps = 3)

  run_twsa(
    model,
    vbp_price_variable = "c_treatment",
    vbp_intervention = "volantor",
    vbp_outcome_summary = "qalys",
    vbp_cost_summary = "costs"
  )
}

# ============================================================================
# 1. Validation Tests
# ============================================================================

test_that("run_twsa() errors when vbp_intervention missing but vbp_price_variable set", {
  model <- get_example_model() |>
    add_twsa("Test") |>
    add_twsa_variable("Test", "u_mild", type = "range", min = 0.75, max = 0.95, steps = 3) |>
    add_twsa_variable("Test", "u_severe", type = "range", min = 0.40, max = 0.70, steps = 3)

  expect_error(
    run_twsa(
      model,
      vbp_price_variable = "c_treatment"
    ),
    "vbp_intervention is required"
  )
})

test_that("run_twsa() errors on invalid VBP intervention strategy", {
  model <- get_example_model() |>
    add_twsa("Test") |>
    add_twsa_variable("Test", "u_mild", type = "range", min = 0.75, max = 0.95, steps = 3) |>
    add_twsa_variable("Test", "u_severe", type = "range", min = 0.40, max = 0.70, steps = 3)

  expect_error(
    run_twsa(
      model,
      vbp_price_variable = "c_treatment",
      vbp_intervention = "nonexistent_strategy"
    ),
    "not found"
  )
})

test_that("run_twsa() errors on invalid VBP price variable", {
  model <- get_example_model() |>
    add_twsa("Test") |>
    add_twsa_variable("Test", "u_mild", type = "range", min = 0.75, max = 0.95, steps = 3) |>
    add_twsa_variable("Test", "u_severe", type = "range", min = 0.40, max = 0.70, steps = 3)

  expect_error(
    run_twsa(
      model,
      vbp_price_variable = "nonexistent_variable",
      vbp_intervention = "volantor"
    ),
    "not found in model variables"
  )
})

# ============================================================================
# 2. Structure Tests
# ============================================================================

test_that("run_twsa() with VBP returns all expected components", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  expect_true("segments" %in% names(result))
  expect_true("aggregated" %in% names(result))
  expect_true("twsa_metadata" %in% names(result))
  expect_true("twsa_vbp_equations" %in% names(result))
  expect_true("vbp_spec" %in% names(result))
})

test_that("twsa_vbp_equations has correct column structure", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  expected_cols <- c(
    "run_id", "comparator", "group",
    "vbp_slope", "vbp_intercept"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(result$twsa_vbp_equations),
                info = sprintf("Missing column: %s", col))
  }
})

test_that("vbp_spec preserves input parameters", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  expect_equal(result$vbp_spec$price_variable, "c_treatment")
  expect_equal(result$vbp_spec$intervention_strategy, "volantor")
  expect_equal(result$vbp_spec$price_values, c(0, 1000, 2000))
})

# ============================================================================
# 3. Segment Structure Tests
# ============================================================================

test_that("TWSA+VBP creates 3x segments per TWSA run", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Check vbp_price_level exists
  expect_true("vbp_price_level" %in% names(result$segments))

  # Should have 3 price levels (1, 2, 3)
  expect_equal(sort(unique(result$segments$vbp_price_level)), c(1, 2, 3))
})

test_that("TWSA+VBP segments have correct price overrides", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Intervention segments should have price overrides
  int_segs <- result$segments %>%
    filter(.data$strategy == "volantor", .data$vbp_price_level == 1)

  for (i in seq_len(nrow(int_segs))) {
    override <- int_segs$parameter_overrides[[i]]
    expect_true("c_treatment" %in% names(override),
                info = sprintf("Segment %d missing c_treatment override", i))
    expect_equal(override$c_treatment, 0)  # price_level 1 = price 0
  }

  # Check price level 2 has price 1000
  int_segs_pl2 <- result$segments %>%
    filter(.data$strategy == "volantor", .data$vbp_price_level == 2)

  for (i in seq_len(nrow(int_segs_pl2))) {
    override <- int_segs_pl2$parameter_overrides[[i]]
    expect_equal(override$c_treatment, 1000)
  }
})

test_that("TWSA+VBP comparator segments have no price overrides", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  comp_segs <- result$segments %>%
    filter(.data$strategy == "seritinib")

  for (i in seq_len(nrow(comp_segs))) {
    override <- comp_segs$parameter_overrides[[i]]
    # Comparator should not have c_treatment override from VBP
    # (may have empty list or TWSA overrides, but not VBP price override)
    expect_false(
      "c_treatment" %in% names(override),
      info = sprintf("Comparator segment %d should not have c_treatment override from VBP", i)
    )
  }
})

# ============================================================================
# 4. Mathematical Correctness Tests
# ============================================================================

test_that("VBP equations: vbp_slope and vbp_intercept exist and are numeric", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()
  eq <- result$twsa_vbp_equations[1, ]

  expect_true(is.numeric(eq$vbp_slope))
  expect_true(is.numeric(eq$vbp_intercept))
})

test_that("VBP equations exist for all TWSA runs", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Get all TWSA run_ids
  twsa_run_ids <- unique(result$twsa_metadata$run_id)

  # Get run_ids in VBP equations
  vbp_run_ids <- unique(result$twsa_vbp_equations$run_id)

  # Should have equations for all TWSA runs
  expect_true(all(twsa_run_ids %in% vbp_run_ids))
})

# ============================================================================
# 5. Price Calculation Tests
# ============================================================================

test_that("calculate_twsa_vbp_price() applies formula correctly", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Get first equation (use internal group name "_aggregated")
  eq <- result$twsa_vbp_equations %>%
    filter(.data$run_id == 1, .data$group == "_aggregated") %>%
    slice(1)

  wtp <- 50000
  price <- calculate_twsa_vbp_price(result, wtp, eq$comparator, run_id = 1)
  expected <- eq$vbp_slope * wtp + eq$vbp_intercept

  expect_equal(price, expected, tolerance = 1e-10)
})

test_that("calculate_twsa_vbp_price() at WTP=0 equals intercept", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  eq <- result$twsa_vbp_equations %>%
    filter(.data$run_id == 1, .data$group == "_aggregated") %>%
    slice(1)

  price <- calculate_twsa_vbp_price(result, 0, eq$comparator, run_id = 1)
  expect_equal(unname(price), unname(eq$vbp_intercept), tolerance = 1e-10)
})

test_that("calculate_twsa_vbp_price() is linear with WTP", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  eq <- result$twsa_vbp_equations %>%
    filter(.data$run_id == 1, .data$group == "_aggregated") %>%
    slice(1)

  p0 <- calculate_twsa_vbp_price(result, 0, eq$comparator, run_id = 1)
  p50k <- calculate_twsa_vbp_price(result, 50000, eq$comparator, run_id = 1)
  p100k <- calculate_twsa_vbp_price(result, 100000, eq$comparator, run_id = 1)

  diff1 <- p50k - p0
  diff2 <- p100k - p50k

  expect_equal(diff1, diff2, tolerance = 1e-10)
})

test_that("calculate_twsa_vbp_price() with NULL comparator returns minimum VBP", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Calculate VBP for all comparators individually
  all_comparators <- unique(result$twsa_vbp_equations %>%
                             filter(.data$run_id == 1, .data$group == "_aggregated") %>%
                             pull(.data$comparator))

  all_prices <- sapply(all_comparators, function(comp) {
    calculate_twsa_vbp_price(result, 50000, comp, run_id = 1)
  })

  # NULL comparator should return minimum
  min_price <- calculate_twsa_vbp_price(result, 50000, run_id = 1)

  expect_equal(min_price, min(all_prices), tolerance = 1e-10)
})

test_that("calculate_twsa_vbp_price() works for different TWSA runs", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Get comparator
  comp <- result$twsa_vbp_equations %>%
    filter(.data$group == "_aggregated") %>%
    pull(.data$comparator) %>%
    unique() %>%
    .[1]

  # Calculate VBP for base case and a variation
  vbp_base <- calculate_twsa_vbp_price(result, 50000, comp, run_id = 1)
  vbp_var <- calculate_twsa_vbp_price(result, 50000, comp, run_id = 2)

  # Should be different (TWSA varies parameters that affect VBP)
  expect_false(vbp_base == vbp_var)
})

# ============================================================================
# 6. Error Handling Tests
# ============================================================================

test_that("calculate_twsa_vbp_price() errors on missing VBP equations", {
  # Create a result without VBP
  mock_result <- list(
    twsa_vbp_equations = NULL
  )

  expect_error(
    calculate_twsa_vbp_price(mock_result, 50000),
    "No VBP equations found"
  )
})

test_that("calculate_twsa_vbp_price() errors on invalid run_id", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  expect_error(
    calculate_twsa_vbp_price(result, 50000, run_id = 9999),
    "No VBP equation found"
  )
})

test_that("calculate_twsa_vbp_price() errors on invalid comparator", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  expect_error(
    calculate_twsa_vbp_price(result, 50000, "nonexistent", run_id = 1),
    "No VBP equation found"
  )
})

test_that("calculate_twsa_vbp_price() errors on invalid group", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  expect_error(
    calculate_twsa_vbp_price(result, 50000, run_id = 1, group = "nonexistent"),
    "No VBP equation found"
  )
})

# ============================================================================
# 7. extract_twsa_summaries Tests (VBP filtering)
# ============================================================================

test_that("extract_twsa_summaries filters out VBP sub-simulations", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- run_example_twsa_vbp()

  # Extract summaries
  summaries <- openqaly:::extract_twsa_summaries(
    result,
    summary_name = "qalys",
    groups = "overall"
  )

  # Should not have vbp_price_level in extracted data
  # (it gets filtered to price_level = 1 only)
  expect_false("vbp_price_level" %in% names(summaries))

  # Should have one row per run_id per strategy (not 3x)
  n_runs <- length(unique(result$twsa_metadata$run_id))
  n_strategies <- length(unique(result$aggregated$strategy))
  expected_rows <- n_runs * n_strategies

  expect_equal(nrow(summaries), expected_rows)
})

# ============================================================================
# 8. NMB Consistency Test (Critical Verification)
# ============================================================================

test_that("TWSA+VBP correctness: VBP at each grid point yields NMB=0", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- get_example_model()

  # TWSA varies utilities (NOT c_treatment which is VBP price variable)
  model <- model |>
    add_twsa("Utility Sensitivity") |>
    add_twsa_variable("Utility Sensitivity", "u_mild",
      type = "range", min = 0.75, max = 0.95, steps = 3) |>
    add_twsa_variable("Utility Sensitivity", "u_severe",
      type = "range", min = 0.40, max = 0.70, steps = 3)

  results <- run_twsa(
    model,
    vbp_price_variable = "c_treatment",
    vbp_intervention = "volantor",
    vbp_outcome_summary = "qalys",
    vbp_cost_summary = "costs"
  )

  wtp <- 100000

  # Test a sample of grid points (base + variations)
  test_run_ids <- c(1, 2, 5)  # Base case and two grid variations

  for (rid in test_run_ids) {
    # Get grid point metadata
    grid_point <- results$twsa_metadata %>%
      filter(.data$run_id == rid) %>%
      slice(1)

    # Get VBP for this grid point (must specify comparator to match NMB comparison)
    vbp_price <- calculate_twsa_vbp_price(results, wtp, comparator = "seritinib", run_id = rid)

    # Create model at this grid point with VBP price
    model_at_vbp <- model

    # Set TWSA parameters to grid point values
    if (!is.na(grid_point$x_value)) {
      model_at_vbp$variables <- model_at_vbp$variables %>%
        mutate(formula = ifelse(
          .data$name == grid_point$x_param_name,
          as.character(grid_point$x_value),
          .data$formula
        ))
    }
    if (!is.na(grid_point$y_value)) {
      model_at_vbp$variables <- model_at_vbp$variables %>%
        mutate(formula = ifelse(
          .data$name == grid_point$y_param_name,
          as.character(grid_point$y_value),
          .data$formula
        ))
    }

    # Set price variable to VBP (for intervention strategy)
    model_at_vbp$variables <- model_at_vbp$variables %>%
      mutate(formula = ifelse(
        .data$name == "c_treatment" & .data$strategy == "volantor",
        as.character(vbp_price),
        .data$formula
      ))

    # Run and calculate NMB
    bc_results <- run_model(model_at_vbp)

    nmb <- calculate_nmb(
      bc_results,
      outcome_summary = "qalys",
      cost_summary = "costs",
      interventions = "Volantor",
      comparators = "Seritinib",
      wtp = wtp,
      groups = "overall"
    )

    total_nmb <- nmb %>%
      filter(grepl("Volantor.*Seritinib", .data$strategy)) %>%
      pull(.data$nmb_amount) %>%
      sum()

    expect_true(
      abs(total_nmb) < 1e-6,
      info = sprintf("NMB should be ~0 for run_id=%d (x=%.3f, y=%.3f, VBP=%.2f), got: %f",
                    rid,
                    ifelse(is.na(grid_point$x_value), NA, grid_point$x_value),
                    ifelse(is.na(grid_point$y_value), NA, grid_point$y_value),
                    vbp_price, total_nmb)
    )
  }
})

# ============================================================================
# 9. Backward Compatibility Tests
# ============================================================================

test_that("run_twsa() without VBP params works as before", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  model <- get_example_model() |>
    add_twsa("Test") |>
    add_twsa_variable("Test", "u_mild", type = "range", min = 0.75, max = 0.95, steps = 3) |>
    add_twsa_variable("Test", "u_severe", type = "range", min = 0.40, max = 0.70, steps = 3)

  # Run standard TWSA (no VBP params)
  result <- run_twsa(model)

  # Should NOT have VBP-specific fields
  expect_null(result$twsa_vbp_equations)
  expect_null(result$vbp_spec)

  # Should have standard TWSA fields
  expect_true("segments" %in% names(result))
  expect_true("aggregated" %in% names(result))
  expect_true("twsa_metadata" %in% names(result))

  # Should NOT have vbp_price_level in segments
  expect_false("vbp_price_level" %in% names(result$segments))
})
