context("VBP NMB Consistency")

# ============================================================================
# Tests for VBP calculations - verifies that at calculated VBP price,
# the Net Monetary Benefit (NMB) equals zero
# ============================================================================

test_that("VBP price results in zero NMB for overall group", {
  model <- read_model(system.file("models", "example_psm", package = "openqaly"))

  vbp_results <- run_vbp(
    model,
    price_variable = "c_drug",
    intervention_strategy = "new_drug",
    outcome_summary = "total_qalys",
    cost_summary = "total_cost"
  )

  wtp_test <- 50000
  comparator_internal <- "standard"

  vbp_price_overall <- calculate_vbp_price(vbp_results, comparator_internal, wtp_test, "overall")

  model_at_vbp <- model
  model_at_vbp$variables <- model_at_vbp$variables %>%
    dplyr::mutate(formula = ifelse(
      name == "c_drug" & strategy == "new_drug",
      as.character(vbp_price_overall),
      formula
    ))

  results_at_vbp <- run_model(model_at_vbp)

  nmb_results <- calculate_nmb(
    results_at_vbp,
    outcome_summary = "total_qalys",
    cost_summary = "total_cost",
    interventions = "New Drug",
    comparators = "Standard of Care",
    wtp = wtp_test,
    groups = "overall"
  )

  total_nmb <- nmb_results %>%
    dplyr::filter(grepl("New Drug.*Standard of Care", strategy)) %>%
    dplyr::pull(nmb_amount) %>%
    sum()

  expect_true(
    abs(total_nmb) < 1e-6,
    info = sprintf("Overall NMB should be ~0 at VBP price, got: %f", total_nmb)
  )
})
