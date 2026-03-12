context("Transition heatmap plots")

test_that("transition_plot_heatmap includes legend ticks at 0 and 1", {
  model_path <- system.file("models", "example_markov", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_markov"
  }

  model <- read_model(model_path)
  results <- run_model(model)

  p <- transition_plot_heatmap(results, cycle = 1)
  built <- ggplot2::ggplot_build(p)
  breaks <- built$plot$scales$get_scales("fill")$get_breaks()

  expect_equal(breaks[1], 0)
  expect_equal(breaks[length(breaks)], 1)
})
