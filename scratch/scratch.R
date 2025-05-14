Rcpp::compileAttributes()
devtools::document()
roxygen2::roxygenize()
devtools::build()
devtools::install(upgrade=FALSE)
library(heRomod2)
#devtools::test()

library(jsonlite)
library(dplyr)
#library(bench)

#model_name <- "checkimab_simple"
#model <- system.file("models", model_name, package = "heRomod2") %>%
    #read_model()
#options(heRomod2.stop_on_error = TRUE)
options(heRomod2.error_mode = "checkpoint")

model <- read_model_json("/Users/jrdnmdhl/Code/GenAICER-python/model_run_input.json")

res <- run_model(model)


ref_model <- system.file("models", "markov_medium", package = "heRomod2") %>% read_model()
ref_res <- run_model(ref_model)

state_mapper <- function(x) {
    index <- match(x,model$states$name)
    ref_model$states$name[index]
}

reorder_indices <- c(1,4,3,2,5,6,7,8)
head(ref_res$segments$collapsed_trace[[1]] - res$segments$collapsed_trace[[1]][,reorder_indices], 10)

round((colSums(ref_res$segments$collapsed_trace[[1]]) - colSums(res$segments$collapsed_trace[[1]][,reorder_indices])) * 7/365, 3)

res_mat <- res$segments$trace_and_values[[1]]$transitions %>%
    mutate(
        from_collapsed = state_mapper(from_collapsed),
        to_collapsed=state_mapper(to_collapsed),
        state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
    ) %>%
    select(cycle, from_collapsed, state_time, to_collapsed, value)


ref_res_mat <- ref_res$segments$trace_and_values[[1]]$transitions %>%
    mutate(
        state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
    ) %>%
    select(cycle, from_collapsed, state_time, to_collapsed, value_ref = value)

mat_comp <- full_join(ref_res_mat, res_mat, by = c("cycle", "from_collapsed", "to_collapsed", "state_time")) %>%
    mutate(diff = value - value_ref)

#jsonlite::toJSON(res)
  
# #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# res <- run_model(model)
