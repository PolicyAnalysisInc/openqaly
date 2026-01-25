context("VBP")

# ============================================================================
# Test Fixtures (no mocks - only model loading helpers)
# ============================================================================

get_example_model <- function() {
  model_path <- system.file("models", "example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }
  read_model(model_path)
}

run_example_vbp <- function() {
  model <- get_example_model()
  run_vbp(
    model,
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost"
  )
}

# ============================================================================
# 1. Validation Tests (validate_vbp_spec)
# ============================================================================

test_that("run_vbp() errors on invalid intervention strategy", {
  model <- get_example_model()

  expect_error(
    run_vbp(
      model,
      price_variable = "c_drug",
      intervention_strategy = "nonexistent_strategy",
      outcome_summary = "total_qalys",
      cost_summary = "total_cost"
    ),
    "not found in model strategies"
  )
})

test_that("run_vbp() errors on invalid price variable", {
  model <- get_example_model()

  expect_error(
    run_vbp(
      model,
      price_variable = "nonexistent_variable",
      intervention_strategy = "targeted",
      outcome_summary = "total_qalys",
      cost_summary = "total_cost"
    ),
    "not found in model variables"
  )
})

test_that("run_vbp() errors on invalid outcome summary", {
  model <- get_example_model()

  expect_error(
    run_vbp(
      model,
      price_variable = "c_drug",
      intervention_strategy = "targeted",
      outcome_summary = "nonexistent_summary",
      cost_summary = "total_cost"
    )
  )
})

test_that("run_vbp() errors on invalid cost summary", {
  model <- get_example_model()

  expect_error(
    run_vbp(
      model,
      price_variable = "c_drug",
      intervention_strategy = "targeted",
      outcome_summary = "total_qalys",
      cost_summary = "nonexistent_summary"
    )
  )
})

test_that("validate_vbp_spec() returns TRUE for valid spec", {
  model <- get_example_model()
  parsed <- openqaly:::parse_model(model)
  spec <- list(
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost"
  )

  expect_true(openqaly:::validate_vbp_spec(parsed, spec))
})

# ============================================================================
# 2. Structure Tests (run_vbp output)
# ============================================================================

test_that("run_vbp() returns all expected components", {
  result <- run_example_vbp()

  expect_true("segments" %in% names(result))
  expect_true("aggregated" %in% names(result))
  expect_true("vbp_equations" %in% names(result))
  expect_true("vbp_equations_by_group" %in% names(result))
  expect_true("vbp_metadata" %in% names(result))
  expect_true("spec" %in% names(result))
})

test_that("vbp_equations has correct column structure", {
  result <- run_example_vbp()

  expected_cols <- c(
    "group", "intervention", "comparator", "outcome_difference",
    "cost_slope", "cost_intercept", "vbp_slope", "vbp_intercept"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(result$vbp_equations),
                info = sprintf("Missing column: %s", col))
  }
})

test_that("vbp_metadata contains expected fields", {
  result <- run_example_vbp()

  expect_true("price_values_tested" %in% names(result$vbp_metadata))
  expect_true("linearity_verified" %in% names(result$vbp_metadata))
  expect_true("groups" %in% names(result$vbp_metadata))
})

test_that("spec preserves input parameters", {
  result <- run_example_vbp()

  expect_equal(result$spec$price_variable, "c_drug")
  expect_equal(result$spec$intervention_strategy, "targeted")
  expect_equal(result$spec$outcome_summary, "total_qalys")
  expect_equal(result$spec$cost_summary, "total_cost")
})

# ============================================================================
# 3. Internal Function Tests
# ============================================================================

test_that("near() correctly checks approximate equality", {
  # Identical values

  expect_true(openqaly:::near(1.0, 1.0))

  # Values within default tolerance
  expect_true(openqaly:::near(1.0, 1.0 + 1e-10))

  # Values outside default tolerance
  expect_false(openqaly:::near(1.0, 1.1))

  # Custom tolerance - values within
  expect_true(openqaly:::near(1.0, 1.05, tol = 0.1))

  # Custom tolerance - values outside
  expect_false(openqaly:::near(1.0, 1.05, tol = 0.01))
})

test_that("build_vbp_segments() creates correct structure", {
  model <- get_example_model()
  parsed <- openqaly:::parse_model(model)
  spec <- list(
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    price_values = c(0, 1000, 2000)
  )

  segments <- openqaly:::build_vbp_segments(parsed, spec)

  # 3x base segments
  base_count <- nrow(openqaly:::get_segments(parsed))
  expect_equal(nrow(segments), base_count * 3)

  # Price levels 1, 2, 3
  expect_equal(sort(unique(segments$price_level)), c(1, 2, 3))

  # Intervention has overrides with correct values (filter by price_level)
  int_segs <- segments[segments$strategy == "targeted", ]
  int_segs_pl1 <- int_segs[int_segs$price_level == 1, ]
  int_segs_pl2 <- int_segs[int_segs$price_level == 2, ]
  int_segs_pl3 <- int_segs[int_segs$price_level == 3, ]
  expect_equal(int_segs_pl1$parameter_overrides[[1]]$c_drug, 0)
  expect_equal(int_segs_pl2$parameter_overrides[[1]]$c_drug, 1000)
  expect_equal(int_segs_pl3$parameter_overrides[[1]]$c_drug, 2000)

  # Comparator has no c_drug overrides
  comp_segs <- segments[segments$strategy == "chemo", ]
  expect_false("c_drug" %in% names(comp_segs$parameter_overrides[[1]]))
})

test_that("extract_segment_summary_values() extracts values correctly", {
  result <- run_example_vbp()
  test_data <- result$segments %>%
    filter(strategy == "targeted", price_level == 1)

  values <- openqaly:::extract_segment_summary_values(test_data, "total_qalys")

  expect_type(values, "double")
  expect_equal(length(values), nrow(test_data))
  expect_true(all(!is.na(values)))
  expect_true(all(values > 0))
})

test_that("extract_summary_values() extracts from aggregated", {
  result <- run_example_vbp()
  test_data <- result$aggregated %>%
    filter(strategy == "targeted")

  values <- openqaly:::extract_summary_values(test_data, "total_qalys")

  expect_type(values, "double")
  expect_equal(length(values), nrow(test_data))
  expect_true(all(!is.na(values)))
})

# ============================================================================
# 3b. Internal Function Edge Case Tests
# ============================================================================

test_that("extract_segment_summary_values() returns NA for NULL summaries", {
  test_data <- tibble(
    strategy = c("A", "A", "A"),
    summaries = list(
      tibble(summary = "total_cost", value = "total_cost", amount = 100),
      NULL,
      tibble(summary = "total_cost", value = "total_cost", amount = 300)
    )
  )

  values <- openqaly:::extract_segment_summary_values(test_data, "total_cost")

  expect_type(values, "double")
  expect_equal(length(values), 3)
  expect_equal(values[1], 100)
  expect_true(is.na(values[2]))
  expect_equal(values[3], 300)
})

test_that("analyze_vbp_results() errors on non-linear group costs", {
  vbp_spec <- list(
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    price_values = c(0, 1000, 2000)
  )

  make_summaries <- function(cost, qalys) {
    tibble(summary = c("total_cost", "total_qalys"),
           value = c("total_cost", "total_qalys"),
           amount = c(cost, qalys))
  }

  # Segments with NON-LINEAR intervention costs
  segments <- tibble(
    strategy = c(rep("targeted", 3), rep("chemo", 3)),
    group = rep("all", 6),
    price_level = rep(1:3, 2),
    weight = rep(1, 6),
    summaries = list(
      make_summaries(1000, 5), make_summaries(2500, 5), make_summaries(3000, 5),
      make_summaries(1000, 4), make_summaries(1000, 4), make_summaries(1000, 4)
    )
  )

  aggregated <- tibble(
    strategy = c(rep("targeted", 3), rep("chemo", 3)),
    run_id = rep(1:3, 2),
    summaries = segments$summaries
  )

  expect_error(
    openqaly:::analyze_vbp_results(segments, aggregated, vbp_spec, list()),
    "Cost not linear with price for targeted vs chemo in group all"
  )
})

test_that("analyze_vbp_results() errors on non-linear aggregated costs", {
  vbp_spec <- list(
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    price_values = c(0, 1000, 2000)
  )

  make_summaries <- function(cost, qalys) {
    tibble(summary = c("total_cost", "total_qalys"),
           value = c("total_cost", "total_qalys"),
           amount = c(cost, qalys))
  }

  # Aggregated: intervention costs non-linear (1000, 2500, 3000)
  # Delta costs: 0, 1500, 2000 - NOT linear (should be 0, 1000, 2000)
  aggregated <- tibble(
    strategy = c(rep("targeted", 3), rep("chemo", 3)),
    run_id = rep(1:3, 2),
    summaries = list(
      make_summaries(1000, 5), make_summaries(2500, 5), make_summaries(3000, 5),
      make_summaries(1000, 4), make_summaries(1000, 4), make_summaries(1000, 4)
    )
  )

  # Segments with LINEAR costs (so group check passes, aggregated fails)
  segments <- tibble(
    strategy = c(rep("targeted", 3), rep("chemo", 3)),
    group = rep("all", 6),
    price_level = rep(1:3, 2),
    weight = rep(1, 6),
    summaries = list(
      make_summaries(1000, 5), make_summaries(2000, 5), make_summaries(3000, 5),
      make_summaries(1000, 4), make_summaries(1000, 4), make_summaries(1000, 4)
    )
  )

  expect_error(
    openqaly:::analyze_vbp_results(segments, aggregated, vbp_spec, list()),
    "VBP error: Cost not linear with price for targeted vs chemo"
  )
})

# ============================================================================
# 4. Mathematical Correctness Tests
# ============================================================================

test_that("vbp_slope = outcome_difference / cost_slope", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  expected <- eq$outcome_difference / eq$cost_slope
  expect_equal(eq$vbp_slope, expected, tolerance = 1e-10)
})

test_that("vbp_intercept = -cost_intercept / cost_slope", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  expected <- -eq$cost_intercept / eq$cost_slope
  expect_equal(eq$vbp_intercept, expected, tolerance = 1e-10)
})

test_that("outcomes unchanged across price levels", {
  result <- run_example_vbp()

  outcomes <- sapply(1:3, function(pl) {
    int_data <- result$segments %>%
      filter(strategy == "targeted", price_level == pl)
    int <- int_data$summaries[[1]] %>%
      filter(summary == "total_qalys") %>%
      pull(amount) %>%
      sum()

    comp_data <- result$segments %>%
      filter(strategy == "chemo", price_level == pl)
    comp <- comp_data$summaries[[1]] %>%
      filter(summary == "total_qalys") %>%
      pull(amount) %>%
      sum()

    int - comp
  })

  expect_equal(outcomes[1], outcomes[2], tolerance = 1e-10)
  expect_equal(outcomes[2], outcomes[3], tolerance = 1e-10)
})

test_that("costs are linear with price", {
  result <- run_example_vbp()
  price_values <- c(0, 1000, 2000)

  delta_costs <- sapply(1:3, function(pl) {
    int_data <- result$segments %>%
      filter(strategy == "targeted", price_level == pl)
    int <- int_data$summaries[[1]] %>%
      filter(summary == "total_cost") %>%
      pull(amount) %>%
      sum()

    comp_data <- result$segments %>%
      filter(strategy == "chemo", price_level == pl)
    comp <- comp_data$summaries[[1]] %>%
      filter(summary == "total_cost") %>%
      pull(amount) %>%
      sum()

    int - comp
  })

  fit <- lm(delta_costs ~ price_values)
  residuals <- delta_costs - predict(fit)
  expect_true(max(abs(residuals)) < 1e-8)
})

test_that("cost coefficients match linear model fit", {
  result <- run_example_vbp()
  price_values <- c(0, 1000, 2000)

  # Use the first comparator from the overall equations
  first_comp <- result$vbp_equations$comparator[1]

  # Use group-specific equations for testing (pick first group)
  first_group <- result$vbp_equations_by_group$group[1]

  # Get costs from segments filtered by group (use summaries, not summaries_discounted
  # because VBP analysis uses undiscounted values)
  delta_costs <- sapply(1:3, function(pl) {
    int_data <- result$segments %>%
      filter(.data$strategy == "targeted", .data$price_level == pl, .data$group == first_group)
    int <- int_data$summaries[[1]] %>%
      filter(.data$summary == "total_cost") %>%
      pull(.data$amount) %>%
      sum()

    comp_data <- result$segments %>%
      filter(.data$strategy == first_comp, .data$price_level == pl, .data$group == first_group)
    comp <- comp_data$summaries[[1]] %>%
      filter(.data$summary == "total_cost") %>%
      pull(.data$amount) %>%
      sum()

    int - comp
  })

  fit <- lm(delta_costs ~ price_values)

  # Match against group-specific equations
  group_eq <- result$vbp_equations_by_group %>%
    filter(.data$group == first_group, .data$comparator == first_comp)

  expect_equal(
    unname(coef(fit)[2]),
    unname(group_eq$cost_slope),
    tolerance = 1e-6
  )
  expect_equal(
    unname(coef(fit)[1]),
    unname(group_eq$cost_intercept),
    tolerance = 1e-6
  )
})

# ============================================================================
# 5. Price Calculation Tests
# ============================================================================

test_that("calculate_vbp_price() applies formula correctly", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  price <- calculate_vbp_price(result, 50000, eq$comparator, "overall")
  expected <- eq$vbp_slope * 50000 + eq$vbp_intercept

  expect_equal(price, expected, tolerance = 1e-10)
})

test_that("price at WTP=0 equals intercept", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  price <- calculate_vbp_price(result, 0, eq$comparator, "overall")
  expect_equal(unname(price), unname(eq$vbp_intercept), tolerance = 1e-10)
})

test_that("price is linear with WTP", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  p0 <- calculate_vbp_price(result, 0, eq$comparator, "overall")
  p50k <- calculate_vbp_price(result, 50000, eq$comparator, "overall")
  p100k <- calculate_vbp_price(result, 100000, eq$comparator, "overall")

  diff1 <- p50k - p0
  diff2 <- p100k - p50k

  expect_equal(diff1, diff2, tolerance = 1e-10)
})

# ============================================================================
# 6. NMB Consistency Tests
# ============================================================================

test_that("VBP price yields NMB = 0", {
  model <- get_example_model()
  vbp_results <- run_vbp(
    model,
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost"
  )

  wtp <- 50000
  vbp_price <- calculate_vbp_price(vbp_results, wtp, "chemo", "overall")

  model_at_vbp <- model
  model_at_vbp$variables <- model_at_vbp$variables %>%
    mutate(formula = ifelse(
      name == "c_drug" & strategy == "targeted",
      as.character(vbp_price),
      formula
    ))

  results <- run_model(model_at_vbp)

  nmb <- calculate_nmb(
    results,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    interventions = "Targeted Therapy",
    comparators = "Chemotherapy",
    wtp = wtp,
    groups = "overall"
  )

  total_nmb <- nmb %>%
    filter(grepl("Targeted Therapy.*Chemotherapy", strategy)) %>%
    pull(nmb_amount) %>%
    sum()

  expect_true(abs(total_nmb) < 1e-6,
              info = sprintf("NMB should be ~0, got: %f", total_nmb))
})

test_that("NMB = 0 at multiple WTP thresholds", {
  model <- get_example_model()
  vbp_results <- run_vbp(
    model,
    price_variable = "c_drug",
    intervention_strategy = "targeted",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost"
  )

  for (wtp in c(25000, 50000, 100000, 150000)) {
    vbp_price <- calculate_vbp_price(vbp_results, wtp, "chemo", "overall")

    model_at_vbp <- model
    model_at_vbp$variables <- model_at_vbp$variables %>%
      mutate(formula = ifelse(
        name == "c_drug" & strategy == "targeted",
        as.character(vbp_price),
        formula
      ))

    results <- run_model(model_at_vbp)

    nmb <- calculate_nmb(
      results,
      outcome_summary = "total_qalys",
      cost_summary = "total_cost",
      interventions = "Targeted Therapy",
      comparators = "Chemotherapy",
      wtp = wtp,
      groups = "overall"
    )

    total_nmb <- nmb %>%
      filter(grepl("Targeted Therapy.*Chemotherapy", strategy)) %>%
      pull(nmb_amount) %>%
      sum()

    expect_true(
      abs(total_nmb) < 1e-6,
      info = sprintf("NMB should be ~0 at WTP=%d, got: %f", wtp, total_nmb)
    )
  }
})

# ============================================================================
# 7. Segment Building Tests (via run_vbp output)
# ============================================================================

test_that("segments have 3x base count", {
  result <- run_example_vbp()

  segments_per_level <- result$segments %>%
    filter(price_level == 1) %>%
    nrow()

  expect_equal(nrow(result$segments), segments_per_level * 3)
})

test_that("segments have price_level 1, 2, 3", {
  result <- run_example_vbp()
  expect_equal(sort(unique(result$segments$price_level)), c(1, 2, 3))
})

test_that("intervention segments have price overrides", {
  result <- run_example_vbp()
  int_segs <- result$segments %>%
    filter(strategy == "targeted")

  for (i in seq_len(nrow(int_segs))) {
    override <- int_segs$parameter_overrides[[i]]
    expect_true("c_drug" %in% names(override),
                info = sprintf("Segment %d missing c_drug override", i))
  }
})

test_that("comparator segments have no price overrides", {
  result <- run_example_vbp()
  comp_segs <- result$segments %>%
    filter(strategy == "chemo")

  for (i in seq_len(nrow(comp_segs))) {
    override <- comp_segs$parameter_overrides[[i]]
    expect_false("c_drug" %in% names(override),
                 info = sprintf("Segment %d should not have c_drug override", i))
  }
})

# ============================================================================
# 8. Metadata Tests
# ============================================================================

test_that("linearity_verified is TRUE", {
  result <- run_example_vbp()
  expect_true(result$vbp_metadata$linearity_verified)
})

test_that("price_values_tested is c(0, 1000, 2000)", {
  result <- run_example_vbp()
  expect_equal(result$vbp_metadata$price_values_tested, c(0, 1000, 2000))
})

# ============================================================================
# 9. Group Handling Tests (with real model)
# ============================================================================

test_that("single group model produces group equations", {
  result <- run_example_vbp()

  expect_true(nrow(result$vbp_equations_by_group) >= 1)
  expect_true("weight" %in% names(result$vbp_equations_by_group))
  expect_true(all(!is.na(result$vbp_equations_by_group$weight)))
})

test_that("overall equations have group = 'overall'", {
  result <- run_example_vbp()
  expect_true(all(result$vbp_equations$group == "overall"))
})

test_that("calculate_vbp_price() works with group parameter", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  price_default <- calculate_vbp_price(result, 50000, eq$comparator)
  price_overall <- calculate_vbp_price(result, 50000, eq$comparator, "overall")

  expect_equal(price_default, price_overall)
})

# ============================================================================
# 10. Error Handling Tests
# ============================================================================

test_that("calculate_vbp_price() errors on missing comparator", {
  result <- run_example_vbp()

  expect_error(
    calculate_vbp_price(result, 50000, "nonexistent", "overall"),
    "No VBP equation found"
  )
})

test_that("calculate_vbp_price() errors on missing group", {
  result <- run_example_vbp()
  eq <- result$vbp_equations[1, ]

  expect_error(
    calculate_vbp_price(result, 50000, eq$comparator, "nonexistent"),
    "No VBP equations found"
  )
})

test_that("calculate_vbp_price() with NULL comparator returns minimum VBP", {
  result <- run_example_vbp()

  # Calculate VBP for all comparators individually
  all_prices <- sapply(result$vbp_equations$comparator, function(comp) {
    calculate_vbp_price(result, 50000, comp, "overall")
  })

  # Calculate VBP with NULL comparator (should return minimum)
  min_price <- calculate_vbp_price(result, 50000)

  expect_equal(min_price, min(all_prices), tolerance = 1e-10)
})

test_that("calculate_vbp_price() with NULL comparator works at multiple WTP values", {
  result <- run_example_vbp()

  for (wtp in c(0, 25000, 50000, 100000)) {
    # Calculate individual prices
    all_prices <- sapply(result$vbp_equations$comparator, function(comp) {
      calculate_vbp_price(result, wtp, comp, "overall")
    })

    # NULL comparator should return minimum
    min_price <- calculate_vbp_price(result, wtp)

    expect_equal(min_price, min(all_prices), tolerance = 1e-10,
                 info = sprintf("Failed at WTP = %d", wtp))
  }
})

# ============================================================================
# 11. Multiple Comparators Test (real model verification)
# ============================================================================

test_that("run_vbp() handles multiple comparators correctly", {
  result <- run_example_vbp()

  # Now there are 2 comparators (chemo and immuno) with targeted as intervention
  expect_equal(nrow(result$vbp_equations), 2)
  expect_true("chemo" %in% result$vbp_equations$comparator)
  expect_true("immuno" %in% result$vbp_equations$comparator)
  expect_true(all(result$vbp_equations$intervention == "targeted"))

  expect_true(is.numeric(result$vbp_equations$vbp_slope))
  expect_true(is.numeric(result$vbp_equations$vbp_intercept))
  expect_true(is.numeric(result$vbp_equations$cost_slope))
  expect_true(is.numeric(result$vbp_equations$cost_intercept))
  expect_true(is.numeric(result$vbp_equations$outcome_difference))
})
