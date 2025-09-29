# Rcpp::compileAttributes()
# devtools::document()
# roxygen2::roxygenize()
# devtools::build()
# devtools::install(upgrade=FALSE)
# library(heRomod2)
# #devtools::test()

# library(jsonlite)
# library(dplyr)
# #library(bench)

# #model_name <- "checkimab_simple"
# #model <- system.file("models", model_name, package = "heRomod2") %>%
#     #read_model()
# # Error handling options removed - checkpoint mode is now always used

# model <- read_model_json("/Users/jrdnmdhl/downloads/model_68d1f913575d326468fff8ca_2025-09-23.json")

# res <- run_model(model)


# ref_model <- system.file("models", "markov_medium", package = "heRomod2") %>% read_model()
# ref_res <- run_model(ref_model)

# # write_json(ref_res, '~/downloads/test.json')


# # state_mapper <- function(x) {
# #     index <- match(x,model$states$name)
# #     ref_model$states$name[index]
# # }

# # reorder_indices <- c(1,2,7,3,4,5,6,8)
# # ref_trace <- ref_res$segments$collapsed_trace[[1]]
# # res_trace <- res$segments$collapsed_trace[[1]]
# # colnames(res_trace) <- state_mapper(colnames(res_trace))
# # res_trace <- res_trace[,colnames(ref_trace)]

# # round((colSums(res_trace) - colSums(ref_trace)) * 7/365, 3)

# # res_mat <- res$segments$trace_and_values[[1]]$transitions %>%
# #     mutate(
# #         from_collapsed = state_mapper(from_collapsed),
# #         to_collapsed=state_mapper(to_collapsed),
# #         state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
# #     ) %>%
# #     select(cycle, from_collapsed, state_time, to_collapsed, value)


# # ref_res_mat <- ref_res$segments$trace_and_values[[1]]$transitions %>%
# #     mutate(
# #         state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
# #     ) %>%
# #     select(cycle, from_collapsed, state_time, to_collapsed, value_ref = value)

# # mat_comp <- full_join(ref_res_mat, res_mat, by = c("cycle", "from_collapsed", "to_collapsed", "state_time")) %>%
# #     mutate(diff = value - value_ref)

# # #jsonlite::toJSON(res)
  
# # # #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# # # res <- run_model(model)
# # ref_outcomes <- ref_res$segments %>%
# #   rowwise() %>%
# #   group_split() %>%
# #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
# #   bind_rows() %>%
# #   group_by(group,strategy,summary) %>%
# #   summarize(value=sum(amount), .groups = 'drop')

# # outcomes <- res$segments %>%
# #   rowwise() %>%
# #   group_split() %>%
# #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
# #   bind_rows() %>%
# #   group_by(group,strategy,summary) %>%
# #   summarize(value=sum(amount), .groups = 'drop')





