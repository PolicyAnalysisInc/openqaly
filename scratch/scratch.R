Rcpp::compileAttributes()
devtools::document()
roxygen2::roxygenize()
devtools::build()
devtools::install(upgrade=FALSE)
library(heRomod2)
#devtools::test()

library(jsonlite)

#library(bench)

#model_name <- "checkimab_simple"
#model <- system.file("models", model_name, package = "heRomod2") %>%
    #read_model()
#options(heRomod2.stop_on_error = TRUE)
options(heRomod2.error_mode = "checkpoint")

model <- read_model_json("/Users/jrdnmdhl/Code/GenAICER-python/model_run_input.json")
res <- run_model(model)
#jsonlite::toJSON(res)
  
# #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# res <- run_model(model)
