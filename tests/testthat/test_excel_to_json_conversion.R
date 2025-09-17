# Load required functions if testing locally
tryCatch({
  # Try to load from installed package first
  write_model_json <- heRomod2::write_model_json
}, error = function(e) {
  # If not available, source directly
  if (file.exists("../../R/misc.R")) {
    source("../../R/misc.R")
  }
})

test_that("Excel to JSON conversion preserves model structure and results", {
  # Check if function is available
  if (!exists("write_model_json")) {
    skip("write_model_json not available - package may need to be rebuilt")
  }
  
  # Load the markov_medium model from Excel
  excel_model_path <- system.file("models/markov_medium", package = "heRomod2")
  if (excel_model_path == "") {
    excel_model_path <- "inst/models/markov_medium"
  }
  
  excel_model <- read_model(excel_model_path)
  
  # Convert to JSON
  json_string <- write_model_json(excel_model)
  
  # Test 1: JSON string is valid
  expect_type(json_string, "character")
  expect_true(nchar(json_string) > 0)
  
  # Test 2: JSON can be parsed back
  json_model <- read_model_json(json_string)
  expect_s3_class(json_model, "heRomodel")
  
  # Test 3: Compare model components structure
  # Settings should be preserved (though format changes from list to df and back)
  expect_equal(length(excel_model$settings), length(json_model$settings))
  expect_equal(names(excel_model$settings), names(json_model$settings))
  
  # Compare key settings values
  expect_equal(
    as.character(excel_model$settings$model_type),
    as.character(json_model$settings$model_type)
  )
  expect_equal(
    as.numeric(excel_model$settings$n_cycles),
    as.numeric(json_model$settings$n_cycles)
  )
  
  # Compare dataframe components
  components_to_check <- c("strategies", "groups", "states", "transitions", "values", "summaries", "variables")
  
  for (comp in components_to_check) {
    if (!is.null(excel_model[[comp]]) && !is.null(json_model[[comp]])) {
      # Check same number of rows
      expect_equal(
        nrow(excel_model[[comp]]),
        nrow(json_model[[comp]]),
        info = paste("Component", comp, "should have same number of rows")
      )
      
      # Check same columns exist (order may differ)
      expect_setequal(
        colnames(excel_model[[comp]]),
        colnames(json_model[[comp]])
      )
    }
  }
  
  # Compare tables
  expect_equal(length(excel_model$tables), length(json_model$tables))
  expect_setequal(names(excel_model$tables), names(json_model$tables))
  
  # Compare scripts
  expect_equal(length(excel_model$scripts), length(json_model$scripts))
  expect_setequal(names(excel_model$scripts), names(json_model$scripts))
  
  # Test 4: Run both models and compare results
  set.seed(123)  # For reproducibility if there's any randomness
  
  excel_results <- run_model(excel_model)
  json_results <- run_model(json_model)
  
  # Compare segment results structure
  expect_equal(nrow(excel_results$segments), nrow(json_results$segments))
  
  # Compare aggregated results
  expect_equal(nrow(excel_results$aggregated), nrow(json_results$aggregated))
  
  # For each strategy in aggregated results, compare key outputs
  strategies <- unique(excel_results$aggregated$strategy)
  
  for (strat in strategies) {
    excel_strat <- excel_results$aggregated[excel_results$aggregated$strategy == strat, ]
    json_strat <- json_results$aggregated[json_results$aggregated$strategy == strat, ]
    
    # Compare trace dimensions
    excel_trace <- excel_strat$collapsed_trace[[1]]
    json_trace <- json_strat$collapsed_trace[[1]]
    
    expect_equal(dim(excel_trace), dim(json_trace),
                 info = paste("Trace dimensions should match for strategy", strat))
    
    # Compare trace values (allowing for small numerical differences)
    expect_equal(excel_trace, json_trace, tolerance = 1e-10,
                 info = paste("Trace values should match for strategy", strat))
    
    # Compare values if they exist
    if (!is.null(excel_strat$trace_and_values[[1]]$values) && 
        !is.null(json_strat$trace_and_values[[1]]$values)) {
      excel_values <- excel_strat$trace_and_values[[1]]$values
      json_values <- json_strat$trace_and_values[[1]]$values
      
      expect_equal(dim(excel_values), dim(json_values),
                   info = paste("Values dimensions should match for strategy", strat))
      
      # Compare actual values with tolerance for floating point
      expect_equal(excel_values, json_values, tolerance = 1e-10,
                   info = paste("Values should match for strategy", strat))
    }
    
    # Compare summaries
    excel_summaries <- excel_strat$summaries[[1]]
    json_summaries <- json_strat$summaries[[1]]
    
    if (!is.null(excel_summaries) && !is.null(json_summaries)) {
      # Sort by summary and value for consistent comparison
      excel_summaries <- excel_summaries[order(excel_summaries$summary, excel_summaries$value), ]
      json_summaries <- json_summaries[order(json_summaries$summary, json_summaries$value), ]
      
      expect_equal(nrow(excel_summaries), nrow(json_summaries),
                   info = paste("Summaries should have same number of rows for strategy", strat))
      
      # Compare summary amounts with tolerance
      expect_equal(excel_summaries$amount, json_summaries$amount, tolerance = 1e-10,
                   info = paste("Summary amounts should match for strategy", strat))
    }
  }
})

test_that("Excel to JSON conversion handles special cases", {
  # Check if function is available
  if (!exists("write_model_json")) {
    skip("write_model_json not available - package may need to be rebuilt")
  }
  
  # Test with empty tables/scripts
  excel_model_path <- system.file("models/markov_medium", package = "heRomod2")
  if (excel_model_path == "") {
    excel_model_path <- "inst/models/markov_medium"
  }
  
  excel_model <- read_model(excel_model_path)
  
  # Test model with empty components
  test_model <- excel_model
  test_model$tables <- list()
  test_model$scripts <- list()
  
  json_string <- write_model_json(test_model)
  json_model <- read_model_json(json_string)
  
  expect_equal(length(json_model$tables), 0)
  expect_equal(length(json_model$scripts), 0)
  
  # Test model with factors in dataframes
  test_model2 <- excel_model
  if (!is.null(test_model2$strategies)) {
    test_model2$strategies$name <- as.factor(test_model2$strategies$name)
  }
  
  json_string2 <- write_model_json(test_model2)
  json_model2 <- read_model_json(json_string2)
  
  # Should convert factors to characters
  expect_type(json_model2$strategies$name, "character")
})

test_that("write_model_json validates input", {
  # Check if function is available
  if (!exists("write_model_json")) {
    skip("write_model_json not available - package may need to be rebuilt")
  }
  
  # Test with non-heRomodel object
  expect_error(
    write_model_json(list(a = 1, b = 2)),
    "Input must be a heRomodel object"
  )
  
  # Test with NULL settings
  fake_model <- list(strategies = data.frame(name = "test"))
  class(fake_model) <- "heRomodel"
  fake_model$settings <- NULL
  
  expect_error(
    write_model_json(fake_model),
    "Model settings must be a list"
  )
})