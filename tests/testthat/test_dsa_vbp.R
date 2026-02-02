context("DSA+VBP")

# Uses cached fixtures from setup.R:
# - get_cached_dsa_vbp_results() for test results
# - get_cached_dsa_vbp_model() for fresh model (NMB test)
# - build_dsa_vbp_model() for validation tests requiring fresh models

# ============================================================================
# 1. Validation Tests
# ============================================================================

test_that("run_dsa() errors when vbp_intervention missing but vbp_price_variable set", {
  model <- build_dsa_vbp_model()

  expect_error(
    run_dsa(
      model,
      vbp_price_variable = "c_drug"
    ),
    "vbp_intervention is required"
  )
})

test_that("run_dsa() errors on invalid VBP intervention strategy", {
  # Need fresh model with different DSA variable for this test
  model_path <- system.file("models", "example_psm", package = "openqaly")
  if (model_path == "") model_path <- "inst/models/example_psm"
  model <- read_model(model_path) %>%
    add_dsa_variable("c_drug", low = 0, high = 1000, strategy = "targeted")

  expect_error(
    run_dsa(
      model,
      vbp_price_variable = "c_drug",
      vbp_intervention = "nonexistent_strategy"
    ),
    "not found in model strategies"
  )
})

test_that("run_dsa() errors on invalid VBP price variable", {
  # Need fresh model with different DSA variable for this test
  model_path <- system.file("models", "example_psm", package = "openqaly")
  if (model_path == "") model_path <- "inst/models/example_psm"
  model <- read_model(model_path) %>%
    add_dsa_variable("c_drug", low = 0, high = 1000, strategy = "targeted")

  expect_error(
    run_dsa(
      model,
      vbp_price_variable = "nonexistent_variable",
      vbp_intervention = "targeted"
    ),
    "not found in model variables"
  )
})

# ============================================================================
# 2. Structure Tests
# ============================================================================

test_that("run_dsa() with VBP returns all expected components", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  expect_true("segments" %in% names(result))
  expect_true("aggregated" %in% names(result))
  expect_true("dsa_metadata" %in% names(result))
  expect_true("dsa_vbp_equations" %in% names(result))
  expect_true("vbp_spec" %in% names(result))
})

test_that("dsa_vbp_equations has correct column structure", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  expected_cols <- c(
    "run_id", "parameter", "parameter_display_name", "parameter_type",
    "variation", "override_value", "comparator", "group",
    "outcome_difference", "cost_slope", "cost_intercept",
    "vbp_slope", "vbp_intercept", "weight"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(result$dsa_vbp_equations),
                info = sprintf("Missing column: %s", col))
  }
})

test_that("vbp_spec preserves input parameters", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  expect_equal(result$vbp_spec$price_variable, "c_drug")
  expect_equal(result$vbp_spec$intervention_strategy, "targeted")
  expect_equal(result$vbp_spec$price_values, c(0, 1000, 2000))
})

# ============================================================================
# 3. Segment Structure Tests
# ============================================================================

test_that("DSA+VBP creates 3x segments per DSA run", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Check vbp_price_level exists
  expect_true("vbp_price_level" %in% names(result$segments))

  # Should have 3 price levels (1, 2, 3)
  expect_equal(sort(unique(result$segments$vbp_price_level)), c(1, 2, 3))
})

test_that("DSA+VBP segments have correct price overrides", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Intervention segments should have price overrides
  int_segs <- result$segments %>%
    filter(.data$strategy == "targeted", .data$vbp_price_level == 1)

  for (i in seq_len(nrow(int_segs))) {
    override <- int_segs$parameter_overrides[[i]]
    expect_true("c_drug" %in% names(override),
                info = sprintf("Segment %d missing c_drug override", i))
    expect_equal(override$c_drug, 0)  # price_level 1 = price 0
  }

  # Check price level 2 has price 1000
  int_segs_pl2 <- result$segments %>%
    filter(.data$strategy == "targeted", .data$vbp_price_level == 2)

  for (i in seq_len(nrow(int_segs_pl2))) {
    override <- int_segs_pl2$parameter_overrides[[i]]
    expect_equal(override$c_drug, 1000)
  }
})

test_that("DSA+VBP comparator segments have no price overrides", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  comp_segs <- result$segments %>%
    filter(.data$strategy == "chemo")

  for (i in seq_len(nrow(comp_segs))) {
    override <- comp_segs$parameter_overrides[[i]]
    # Comparator should not have c_drug override from VBP
    # (may have empty list or DSA overrides, but not VBP price override)
    expect_false(
      "c_drug" %in% names(override),
      info = sprintf("Comparator segment %d should not have c_drug override from VBP", i)
    )
  }
})

# ============================================================================
# 4. Mathematical Correctness Tests
# ============================================================================

test_that("VBP equations: vbp_slope = outcome_difference / cost_slope", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()
  eq <- result$dsa_vbp_equations[1, ]

  expected <- eq$outcome_difference / eq$cost_slope
  expect_equal(eq$vbp_slope, expected, tolerance = 1e-10)
})

test_that("VBP equations: vbp_intercept = -cost_intercept / cost_slope", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()
  eq <- result$dsa_vbp_equations[1, ]

  expected <- -eq$cost_intercept / eq$cost_slope
  expect_equal(eq$vbp_intercept, expected, tolerance = 1e-10)
})

test_that("VBP equations exist for all DSA runs", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Get all DSA run_ids
  dsa_run_ids <- unique(result$dsa_metadata$run_id)

  # Get run_ids in VBP equations
  vbp_run_ids <- unique(result$dsa_vbp_equations$run_id)

  # Should have equations for all DSA runs
  expect_true(all(dsa_run_ids %in% vbp_run_ids))
})

# ============================================================================
# 5. Price Calculation Tests
# ============================================================================

test_that("calculate_dsa_vbp_price() applies formula correctly", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Get first equation
  eq <- result$dsa_vbp_equations %>%
    filter(.data$run_id == 1, .data$group == "overall") %>%
    slice(1)

  wtp <- 50000
  price <- calculate_dsa_vbp_price(result, wtp, eq$comparator, run_id = 1)
  expected <- eq$vbp_slope * wtp + eq$vbp_intercept

  expect_equal(price, expected, tolerance = 1e-10)
})

test_that("calculate_dsa_vbp_price() at WTP=0 equals intercept", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  eq <- result$dsa_vbp_equations %>%
    filter(.data$run_id == 1, .data$group == "overall") %>%
    slice(1)

  price <- calculate_dsa_vbp_price(result, 0, eq$comparator, run_id = 1)
  expect_equal(unname(price), unname(eq$vbp_intercept), tolerance = 1e-10)
})

test_that("calculate_dsa_vbp_price() is linear with WTP", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  eq <- result$dsa_vbp_equations %>%
    filter(.data$run_id == 1, .data$group == "overall") %>%
    slice(1)

  p0 <- calculate_dsa_vbp_price(result, 0, eq$comparator, run_id = 1)
  p50k <- calculate_dsa_vbp_price(result, 50000, eq$comparator, run_id = 1)
  p100k <- calculate_dsa_vbp_price(result, 100000, eq$comparator, run_id = 1)

  diff1 <- p50k - p0
  diff2 <- p100k - p50k

  expect_equal(diff1, diff2, tolerance = 1e-10)
})

test_that("calculate_dsa_vbp_price() with NULL comparator returns minimum VBP", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Calculate VBP for all comparators individually
  all_comparators <- unique(result$dsa_vbp_equations %>%
                             filter(.data$run_id == 1, .data$group == "overall") %>%
                             pull(.data$comparator))

  all_prices <- sapply(all_comparators, function(comp) {
    calculate_dsa_vbp_price(result, 50000, comp, run_id = 1)
  })

  # NULL comparator should return minimum
  min_price <- calculate_dsa_vbp_price(result, 50000, run_id = 1)

  expect_equal(min_price, min(all_prices), tolerance = 1e-10)
})

test_that("calculate_dsa_vbp_price() works for different DSA runs", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Get comparator
  comp <- result$dsa_vbp_equations %>%
    filter(.data$group == "overall") %>%
    pull(.data$comparator) %>%
    unique() %>%
    .[1]

  # Calculate VBP for base case and a variation
  vbp_base <- calculate_dsa_vbp_price(result, 50000, comp, run_id = 1)
  vbp_low <- calculate_dsa_vbp_price(result, 50000, comp, run_id = 2)
  vbp_high <- calculate_dsa_vbp_price(result, 50000, comp, run_id = 3)

  # All should be different (DSA affects VBP)
  expect_false(vbp_base == vbp_low)
  expect_false(vbp_base == vbp_high)
})

# ============================================================================
# 6. Error Handling Tests
# ============================================================================

test_that("calculate_dsa_vbp_price() errors on missing VBP equations", {
  # Create a result without VBP
  mock_result <- list(
    dsa_vbp_equations = NULL
  )

  expect_error(
    calculate_dsa_vbp_price(mock_result, 50000),
    "No VBP equations found"
  )
})

test_that("calculate_dsa_vbp_price() errors on invalid run_id", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  expect_error(
    calculate_dsa_vbp_price(result, 50000, run_id = 9999),
    "No VBP equations found"
  )
})

test_that("calculate_dsa_vbp_price() errors on invalid comparator", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  expect_error(
    calculate_dsa_vbp_price(result, 50000, "nonexistent", run_id = 1),
    "No VBP equation found"
  )
})

test_that("calculate_dsa_vbp_price() errors on invalid group", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  expect_error(
    calculate_dsa_vbp_price(result, 50000, run_id = 1, group = "nonexistent"),
    "No VBP equations found"
  )
})

# ============================================================================
# 7. extract_dsa_summaries Tests (VBP filtering)
# ============================================================================

test_that("extract_dsa_summaries filters out VBP sub-simulations", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Extract summaries
  summaries <- openqaly:::extract_dsa_summaries(
    result,
    summary_name = "total_qalys",
    groups = "overall"
  )

  # Should not have vbp_price_level in extracted data
  # (it gets filtered to price_level = 1 only)
  expect_false("vbp_price_level" %in% names(summaries))

  # Should have one row per run_id per strategy (not 3x)
  n_runs <- length(unique(result$dsa_metadata$run_id))
  n_strategies <- length(unique(result$aggregated$strategy))
  expected_rows <- n_runs * n_strategies

  expect_equal(nrow(summaries), expected_rows)
})

# ============================================================================
# 8. NMB Consistency Test (Critical Verification)
# ============================================================================

test_that("DSA+VBP correctness: low/base/high VBP all yield NMB=0", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # Use cached model and results instead of building fresh
  model <- get_cached_dsa_vbp_model()
  results <- get_cached_dsa_vbp_results()

  wtp <- 50000

  # Get original u_pfs value
  original_u_pfs <- model$variables %>%
    filter(.data$name == "u_pfs") %>%
    pull(.data$formula) %>%
    as.numeric() %>%
    .[1]

  # Define DSA variations: base (run_id=1), low (run_id=2), high (run_id=3)
  dsa_variations <- list(
    base = list(run_id = 1, u_pfs = original_u_pfs),
    low = list(run_id = 2, u_pfs = original_u_pfs * 0.8),
    high = list(run_id = 3, u_pfs = original_u_pfs * 1.2)
  )

  # Verify each VBP yields NMB=0 when model uses same parameter values
  for (variation_name in names(dsa_variations)) {
    variation <- dsa_variations[[variation_name]]

    # Get VBP for this DSA variation
    vbp_price <- calculate_dsa_vbp_price(results, wtp, "chemo", run_id = variation$run_id)

    # Create model with:
    # 1. DSA parameter (u_pfs) at its variation value
    # 2. VBP price (c_drug) at the calculated VBP
    model_at_vbp <- model

    # Set u_pfs to the DSA variation value
    model_at_vbp$variables <- model_at_vbp$variables %>%
      mutate(formula = ifelse(
        .data$name == "u_pfs",
        as.character(variation$u_pfs),
        .data$formula
      ))

    # Set c_drug to VBP price (for targeted strategy)
    model_at_vbp$variables <- model_at_vbp$variables %>%
      mutate(formula = ifelse(
        .data$name == "c_drug" & .data$strategy == "targeted",
        as.character(vbp_price),
        .data$formula
      ))

    # Run model with both overrides
    bc_results <- run_model(model_at_vbp)

    # Calculate NMB
    nmb <- calculate_nmb(
      bc_results,
      outcome_summary = "total_qalys",
      cost_summary = "total_cost",
      interventions = "Targeted Therapy",
      comparators = "Chemotherapy",
      wtp = wtp,
      groups = "overall"
    )

    total_nmb <- nmb %>%
      filter(grepl("Targeted Therapy.*Chemotherapy", .data$strategy)) %>%
      pull(.data$nmb_amount) %>%
      sum()

    expect_true(
      abs(total_nmb) < 1e-6,
      info = sprintf("NMB should be ~0 for %s (u_pfs=%.3f, VBP=%.2f), got: %f",
                    variation_name, variation$u_pfs, vbp_price, total_nmb)
    )
  }
})

# ============================================================================
# 9. Visualization Tests
# ============================================================================

test_that("dsa_vbp_plot() creates valid ggplot object", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  p <- dsa_vbp_plot(result, wtp = 50000)

  expect_s3_class(p, "ggplot")
})

test_that("dsa_vbp_plot() errors on results without VBP", {
  # Create mock result without VBP equations
  mock_result <- list(
    dsa_vbp_equations = NULL
  )

  expect_error(
    dsa_vbp_plot(mock_result, wtp = 50000),
    "No VBP equations found"
  )
})

test_that("dsa_vbp_plot() works with specific comparator", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  p <- dsa_vbp_plot(result, wtp = 50000, comparators = "chemo")

  expect_s3_class(p, "ggplot")
})

# ============================================================================
# 10. Table Tests
# ============================================================================

test_that("dsa_vbp_table() creates table with correct structure", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  result <- get_cached_dsa_vbp_results()

  # Should not error
  tbl <- dsa_vbp_table(result, wtp = 50000)

  # Table should be created
  expect_true(!is.null(tbl))
})

test_that("dsa_vbp_table() errors on results without VBP", {
  mock_result <- list(
    dsa_vbp_equations = NULL
  )

  expect_error(
    dsa_vbp_table(mock_result, wtp = 50000),
    "No VBP equations found"
  )
})

# ============================================================================
# 11. Standard DSA Backward Compatibility Tests
# ============================================================================

test_that("run_dsa() without VBP params works as before", {
  skip_if(Sys.getenv("QUICK_TEST") == "true")

  # Use cached standard DSA results (no VBP)
  result <- get_cached_dsa_results()

  # Should NOT have VBP-specific fields
  expect_null(result$dsa_vbp_equations)
  expect_null(result$vbp_spec)

  # Should have standard DSA fields
  expect_true("segments" %in% names(result))
  expect_true("aggregated" %in% names(result))
  expect_true("dsa_metadata" %in% names(result))

  # Should NOT have vbp_price_level in segments
  expect_false("vbp_price_level" %in% names(result$segments))
})
