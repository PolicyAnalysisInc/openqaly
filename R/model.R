#' Run a Model
#'
#' Takes a model specification object and runs the model.
#'
#' @param model A heRo_model object.
#' @param ... additional arguments.
#' 
#' @return A list containing the results of the model.
#' 
#' @export
run_model <- function(model, ...) {
  
  # Capture the extra arguments
  dots <- list(...)
  
  # Create a results object
  res <- list()

  # Parse the model
  parsed_model <- parse_model(model, ...)
  
  # Get model segments
  if (is.null(dots$newdata)) segments <- get_segments(parsed_model)
  else segments <- dots$newdata
  
  # Split by segment, evaluate each segment in parallel, combine results
  res$segments <- segments %>%
    rowwise() %>%
    group_split() %>%
    map(function(segment) run_segment(segment, parsed_model, ...)) %>%
    bind_rows()
  
  # Process the results
  
  # Return
  res
}


parse_model <- function(model, ...) {
  
  # Create a new environment from the calling environment which will be used
  # to store model variables.
  model$env <- new.env(parent = parent.frame())

  # Load tables & trees into the environment
  load_tables(model$tables, model$env)
  load_trees(model$trees, model$env)

  # Run any model scripts within that environment
  run_scripts(model$scripts, model$env)

  # If no groups are defined, create one group representing entire population
  if (nrow(model$groups) == 0) {
    model$groups <- create_default_group()
  }

  model$names <- list(
    states = unique(model$states$name),
    strategies = unique(model$strategies$name),
    values = unique(model$values$name)
  )

  model$settings$cycle_length_days <- get_cycle_length_days(model$settings)
  model$settings$n_cycles <- get_n_cycles(model$settings)

  # Set the class of the object based on model type
  class(model) <- c(model$settings$model_type, 'list')

  model
}
