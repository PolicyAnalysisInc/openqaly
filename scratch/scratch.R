devtools::build()
devtools::install()
library(heRomod2)

library(jsonlite)

#library(bench)

model_name <- "checkimab_simple"
  model <- system.file("models", model_name, package = "heRomod2") %>%
    read_model()
  res <- run_model(model)

  jsonlite::toJSON(res)
  
# #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# res <- run_model(model)




#' TODO
#' - Enforce naming restrictions
#' - Unified state expansion across values, trans, init
#' - Implement values calcualtions in rcpp

#' FEATURE LIST
#' 
#' 
#' MARKOV TRANSITIONS
#' - State-time limiting
#' - Shared state time
#' - Execute through errors
#' - Fractional Complements
#' 
#' Markov Values
#' - Residency vs. Transitional vs. Starting
#' - Positive/negative valence
#' - Per-value half-cycle correction methods
#' - Execute through errors