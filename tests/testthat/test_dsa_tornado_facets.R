# DSA tornado plots: per-facet labels and the zero-impact filter.

example_markov_dir <- function() system.file("models", "example_markov", package = "openqaly")

# Shared DSA run with two strategy-specific cost parameters, two facets.
dsa_res <- local({
  m <- read_model(example_markov_dir())
  m <- add_dsa_variable(m, "p_death_healthy", low = 0.01, high = 0.03)
  m <- add_dsa_variable(m, "p_death_mild", low = 0.02, high = 0.06)
  m <- add_dsa_variable(m, "u_healthy", low = 0.8, high = 1.0)
  m <- add_dsa_variable(m, "u_mild", low = 0.5, high = 0.8)
  m <- add_dsa_variable(m, "c_mild", low = 500, high = 1500)
  m <- add_dsa_variable(m, "c_treatment", low = 1000, high = 9000,
                        strategy = "seritinib", display_name = "Treatment cost (seritinib)")
  m <- add_dsa_variable(m, "c_treatment", low = 4000, high = 4100,
                        strategy = "cendralimab", display_name = "Treatment cost (cendralimab)")
  run_dsa(m)
})

# in every facet, each rendered y label must equal the parameter that owns
# that y position in that facet.
test_that("DSA tornado labels match each facet's own parameters", {
  p <- dsa_ce_plot(dsa_res, "qalys", "costs",
                   interventions = c("seritinib", "cendralimab"),
                   comparators = "volantor", drop_zero_impact = FALSE)
  lay <- ggplot2::ggplot_build(p)
  own <- unique(as.data.frame(p$layers[[1]]$data)[, c("reorder_group", "parameter_display_name", "y_base")])
  panel_map <- lay$layout$layout
  rows <- lapply(seq_len(nrow(panel_map)), function(i) {
    pan <- lay$layout$panel_params[[i]]
    brk <- pan$y$get_breaks()
    lab <- pan$y$get_labels()
    keep <- !is.na(brk) & nzchar(lab)
    y_pos <- round(-brk[keep], 1)
    panel_name <- as.character(panel_map$strategy[i])
    own_i <- own[as.character(own$reorder_group) == panel_name, ]
    own_name <- own_i$parameter_display_name[match(y_pos, round(own_i$y_base, 1))]
    data.frame(panel = panel_name, rendered_label = lab[keep],
               own_parameter = own_name, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  expect_gt(nrow(out), 0)
  mismatch <- is.na(out$own_parameter) | out$rendered_label != out$own_parameter
  expect_equal(sum(mismatch), 0,
               info = paste(capture.output(print(out[mismatch, ])), collapse = "\n"))
})

# a parameter whose low, base and high ICERs are equal has zero range and
# must be dropped by drop_zero_impact.
test_that("zero-impact parameter has displayable range 0", {
  expect_equal(calculate_displayable_range(5000, 5000, 5000, TRUE, TRUE, "normal"), 0)
})
