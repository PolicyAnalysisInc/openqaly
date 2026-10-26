# NMB, CEAC and PSA outputs: strategy names and summary validation.

example_markov_dir <- function() system.file("models", "example_markov", package = "openqaly")

# Shared PSA run (small n_sim, fixed seed).
psa_res <- run_psa(read_model(example_markov_dir()), n_sim = 20, seed = 1)

# the default calculate_nmb call must work with display names (example_markov).
test_that("calculate_nmb works with default display names", {
  res <- run_model(read_model(example_markov_dir()))
  default <- calculate_nmb(res, "qalys", "costs", interventions = "seritinib")
  technical <- calculate_nmb(res, "qalys", "costs", interventions = "seritinib",
                             use_display_names = FALSE)
  expect_equal(default$nmb_amount, technical$nmb_amount)
  expect_equal(nrow(default), nrow(technical))
})

# each CEAC data column must sit under its own header. Upstream CEAC data is
# mocked with the strategies in an order that differs from the header order.
test_that("incremental CEAC columns follow header strategy order", {
  mock_ceac <- tibble::tibble(
    wtp = c(0, 50000, 0, 50000, 0, 50000, 0, 50000),
    strategy = c("B", "B", "A", "A", "B", "B", "A", "A"),
    probability = c(0.9, 0.2, 0.1, 0.8, 0.7, 0.3, 0.3, 0.7),
    group = c("Other", "Other", "Other", "Other", "Overall", "Overall", "Overall", "Overall")
  )
  ns <- asNamespace("openqaly")
  prep_fn <- get("prepare_incremental_ceac_table_data", envir = ns)
  environment(prep_fn) <- list2env(
    list(calculate_incremental_ceac = function(...) mock_ceac),
    parent = ns
  )
  results <- list(metadata = list(
    strategies = data.frame(display_name = c("A", "B"), stringsAsFactors = FALSE),
    groups = data.frame(display_name = "Other", stringsAsFactors = FALSE),
    locale = "en-US"
  ))
  prep <- prep_fn(results, "qalys", "costs", wtp_thresholds = c(0, 50000), groups = "all")
  # headers[[2]] is the strategy header row, aligned with the data columns.
  hdr <- vapply(prep$headers[[2]], function(x) x$text, character(1))
  cols <- names(prep$data)
  strategy_cols <- which(grepl("_", cols) & !startsWith(cols, "spacer_"))
  expect_gt(length(strategy_cols), 0)
  for (k in strategy_cols) {
    expect_equal(sub(".*_", "", cols[k]), hdr[k], info = sprintf("column '%s'", cols[k]))
  }
})

# a technical strategy name must select rows in the incremental CEAC plot.
test_that("incremental_ceac_plot keeps rows when strategies is given by technical name", {
  p <- incremental_ceac_plot(psa_res, health_outcome = "qalys",
                             cost_outcome = "costs", strategies = "seritinib")
  expect_gt(nrow(p$data), 0)
})

# a misspelled summary name must error, not return a 1/0 CEAC.
test_that("PSA CEAC errors on an unknown summary name", {
  expect_error(calculate_incremental_ceac(psa_res, "qalyz", "costs", wtp = 50000),
               "not found|does not exist")
})

test_that("comparative NMB plots take interventions/comparators, not strategies", {
  for (plot_fn in list(nmb_density_plot, nmb_ridgeline_plot, pairwise_psa_scatter_plot)) {
    expect_false("strategies" %in% names(formals(plot_fn)))
    expect_error(plot_fn(psa_res, "qalys", "costs", wtp = 50000, strategies = c("seritinib", "volantor")),
                 "unused argument")
    expect_error(plot_fn(psa_res, "qalys", "costs", wtp = 50000),
                 "At least one of 'interventions' or 'comparators' must be provided")
    expect_s3_class(plot_fn(psa_res, "qalys", "costs", wtp = 50000, comparators = "volantor"), "ggplot")
  }
})
