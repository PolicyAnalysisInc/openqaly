# TWSA results: ICERs, tables, metadata, VBP equations and axis labels.

example_markov_dir <- function() system.file("models", "example_markov", package = "openqaly")

# Shared TWSA run: one strategy-specific range plus one shared range.
twsa_res <- run_twsa(
  read_model(example_markov_dir()) |>
    add_twsa("Cost Sensitivity") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment",
                      type = "range", min = 1000, max = 4000, steps = 3,
                      strategy = "volantor") |>
    add_twsa_variable("Cost Sensitivity", "u_mild",
                      type = "range", min = 0.7, max = 0.9, steps = 3)
)

# a TWSA cell in the SW quadrant (both deltas negative) must carry a
# negative ICER whose formatted value ends in "*".
test_that("TWSA ICER in SW quadrant is negative and asterisked", {
  pr <- prepare_twsa_ce_data(
    results = twsa_res, health_outcome = "qalys", cost_outcome = "costs",
    twsa_name = "Cost Sensitivity", groups = "overall",
    interventions = "seritinib", comparators = "volantor"
  )
  sw <- pr[pr$delta_outcome < 0 & pr$delta_cost < 0, ]
  expect_gt(nrow(sw), 0)
  icer_sw <- as.numeric(sw$icer[1])
  expect_lt(icer_sw, 0)
  expect_match(oq_format_icer(icer_sw), "\\*$")
})

# the TWSA outcomes table must render every strategy, not just the first.
test_that("TWSA outcomes table includes every strategy", {
  out <- twsa_outcomes_table(twsa_res, "qalys", twsa_name = "Cost Sensitivity", backend = "kable")
  txt <- paste(as.character(out), collapse = "\n")
  for (lbl in c("Cendralimab", "Seritinib", "Volantor")) {
    expect_true(grepl(lbl, txt, fixed = TRUE), info = paste("missing strategy:", lbl))
  }
})

# strategy-specific base cases with a radius range must give
# one metadata row per run_id.
test_that("TWSA metadata has one row per run_id", {
  m <- read_model(example_markov_dir()) |>
    add_twsa("Cost Sensitivity") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment", type = "radius",
                      radius = 1000, steps = 3, strategy = "volantor") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment", type = "radius",
                      radius = 2000, steps = 3, strategy = "cendralimab")
  segs <- openqaly:::build_twsa_segments(openqaly:::parse_model(m))
  md <- openqaly:::generate_twsa_metadata(m, segs)
  expect_equal(nrow(md), length(unique(md$run_id)))
})

# strategy segments with different step counts must build
# without error.
test_that("TWSA grid handles segments with different value counts", {
  m <- read_model(example_markov_dir()) |>
    add_twsa("Cost Sensitivity") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment", type = "radius",
                      radius = 1000, steps = 3, strategy = "volantor") |>
    add_twsa_variable("Cost Sensitivity", "c_treatment", type = "radius",
                      radius = 500, steps = 5, strategy = "cendralimab")
  expect_error(run_twsa(m), NA)
})

# TWSA VBP with two groups must return equations for each group.
test_that("TWSA VBP returns equations for every group", {
  m <- define_model("markov") %>%
    set_settings(timeframe = 1, timeframe_unit = "years",
                 cycle_length = 1, cycle_length_unit = "years") %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_group("young", weight = 0.6) %>%
    add_group("old", weight = 0.4) %>%
    add_state("alive", initial_prob = 1) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("drug_price", 500) %>%
    add_variable("drug_units", 1, strategy = "control") %>%
    add_variable("drug_units", 3, strategy = "treatment") %>%
    add_variable("utility", 1, strategy = "control") %>%
    add_variable("utility", 2, strategy = "treatment") %>%
    add_transition("alive", "alive", 1) %>%
    add_transition("dead", "dead", 1) %>%
    add_value("cost", drug_price * drug_units, state = "alive", type = "cost") %>%
    add_value("qaly", utility, state = "alive", type = "outcome") %>%
    add_summary("total_cost", "cost", type = "cost") %>%
    add_summary("total_qalys", "qaly", type = "outcome") %>%
    add_twsa("Utility Grid") %>%
    add_twsa_variable("Utility Grid", "utility", type = "range",
                      min = 1.5, max = 2.5, steps = 3, strategy = "treatment") %>%
    add_twsa_variable("Utility Grid", "drug_units", type = "range",
                      min = 2, max = 4, steps = 3, strategy = "treatment")
  res <- run_twsa(m, vbp_price_variable = "drug_price", vbp_intervention = "treatment",
                  vbp_outcome_summary = "total_qalys", vbp_cost_summary = "total_cost")
  model_groups <- unique(res$segments$group)
  expect_true(all(model_groups %in% unique(res$twsa_vbp_equations$group)))
})

test_that("TWSA axes show their own parameters' display names", {
  md <- twsa_res$twsa_metadata
  md <- md[!is.na(md$x_param_display_name), ]
  expect_equal(unique(md$x_param_display_name), "Treatment cost")
  expect_equal(unique(md$y_param_display_name), "Utility (mild disease)")
})
