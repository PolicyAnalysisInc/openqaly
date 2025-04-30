Rcpp::compileAttributes()
devtools::document()
roxygen2::roxygenize()
devtools::build()
devtools::install(upgrade=FALSE)
library(heRomod2)

library(jsonlite)

#library(bench)

# model_name <- "checkimab_simple"
#   model <- system.file("models", model_name, package = "heRomod2") %>%
#     read_model()
options(heRomod2.stop_on_error = TRUE)
model <- read_model_json("~/Downloads/test1.json")

  res <- run_model(model)

  #jsonlite::toJSON(res)
  
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