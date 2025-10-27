Rcpp::compileAttributes()
devtools::document()
roxygen2::roxygenize()
devtools::build()
devtools::install(upgrade=FALSE)
library(heRomod2)
#devtools::test()

library(jsonlite)
library(dplyr)
library(future)
#library(bench)

model_name <- "markov_medium"
model <- system.file("models", model_name, package = "heRomod2") %>%
    read_model()

# model <- read_model_json("/Users/jrdnmdhl/downloads/model_68e48d6e9ff29813a41997b6_2025-10-07.json")
print(model$summaries)
# model$variables$formula[35]<-'2000000'


res <- run_model(model)
# incremental_ce_table(res,"qalys", "costs")

# calculate_pairwise_ce(res, "qalys", "costs", referent = "checkimab")
# pairwise_ce_table(res, 'qalys', 'costs', referent = 'checkimab')
# pairwise_ce_plot(res, 'qalys', 'costs', referent = 'checkimab')
# pairwise_ce_plot(res, 'qalys', 'costs', comparator = 'chemoplatin')

plan(multisession)
model <- model |>
    add_variable(name = "p_stable_mean", formula = 0.7) |>
    add_variable(name = "p_prog_mean", formula = 0.2) |>
    add_variable(name = "p_death_mean", formula = 0.1) |>
    add_variable(name = "alpha_stable", formula = 10) |>
    add_variable(name = "alpha_prog", formula = 3) |>
    add_variable(name = "alpha_death", formula = 2) |>
    add_multivariate_sampling(
        name = "transition_probs",
        distribution = "dirichlet(c(alpha_stable, alpha_prog, alpha_death))",
        variables = c("p_stable_mean", "p_prog_mean", "p_death_mean")
    )

psa_results <- run_model(model, psa = TRUE, n_sim = 1000, seed = 123)


# trace_table(res,group=NULL)
# trace_table(res,group=NULL,table_format='kable')


# trace_table(res)
# trace_table(res, table_format='kable')

# outcomes_table(res, 'qalys', group=NULL)
# outcomes_table(res, 'qalys', group=NULL, table_format='kable')


# outcomes_table(res, 'qalys')
# outcomes_table(res, 'qalys', table_format='kable')

# outcomes_table(res, 'qalys', group=NULL, referent = 'checkimab')
# outcomes_table(res, 'qalys', group=NULL, table_format='kable', referent = 'checkimab')


# outcomes_table(res, 'qalys', referent = 'checkimab')
# outcomes_table(res, 'qalys', table_format='kable', referent = 'checkimab')

# nmb_table(res, 'qalys', 'costs', group=NULL, referent = 'checkimab', wtp = 30000)
# nmb_table(res, 'qalys', 'costs', group=NULL, table_format='kable', referent = 'checkimab', wtp = 30000)


# nmb_table(res, 'qalys', 'costs', referent = 'checkimab', wtp = 30000)
# nmb_table(res, 'qalys', 'costs', table_format='kable', referent = 'checkimab', wtp = 30000)

# nmb_table(res, 'qalys', 'costs', referent = 'checkimab', wtp = 30000)
# outcomes_table(res, 'qalys', group=NULL)

# outcomes_table(res, 'qalys', group=NULL, table_format='kable')
# outcomes_plot_bar(res, 'qalys', group=NULL)
# outcomes_plot_line(res, 'costs')
# trace_plot_line(res)
# nmb_plot_bar# outcomes_plot_bar(res, 'costs', referent = 'checkimab')
# nmb_plot_bar(res, "qalys", "costs", wtp=30000, referent = "checkimab")
# trace_table(res, strategy_name_field = "abbreviation", state_name_field = "display_name")
# nmb_table(res, 'qalys', 'costs', referent = 'checkimab', wtp = 30000)
# outcomes_table(res, 'qalys', group = NULL)
# ref_model <- system.file("models", "markov_medium", package = "heRomod2") %>% read_model()
# ref_res <- run_model(ref_model)
# outcomes_plot(res, "costs", group = NULL)
# write_json(ref_res, '~/downloads/test.json')


# state_mapper <- function(x) {
#     index <- match(x,model$states$name)
#     ref_model$states$name[index]
# }

# reorder_indices <- c(1,2,7,3,4,5,6,8)
# ref_trace <- ref_res$segments$collapsed_trace[[1]]
# res_trace <- res$segments$collapsed_trace[[1]]
# colnames(res_trace) <- state_mapper(colnames(res_trace))
# res_trace <- res_trace[,colnames(ref_trace)]

# round((colSums(res_trace) - colSums(ref_trace)) * 7/365, 3)

# res_mat <- res$segments$trace_and_values[[1]]$transitions %>%
#     mutate(
#         from_collapsed = state_mapper(from_collapsed),
#         to_collapsed=state_mapper(to_collapsed),
#         state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
#     ) %>%
#     select(cycle, from_collapsed, state_time, to_collapsed, value)


# ref_res_mat <- ref_res$segments$trace_and_values[[1]]$transitions %>%
#     mutate(
#         state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
#     ) %>%
#     select(cycle, from_collapsed, state_time, to_collapsed, value_ref = value)

# mat_comp <- full_join(ref_res_mat, res_mat, by = c("cycle", "from_collapsed", "to_collapsed", "state_time")) %>%
#     mutate(diff = value - value_ref)

# #jsonlite::toJSON(res)
  
# # #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# # res <- run_model(model)
# ref_outcomes <- ref_res$segments %>%
#   rowwise() %>%
#   group_split() %>%
#   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
#   bind_rows() %>%
#   group_by(group,strategy,summary) %>%
#   summarize(value=sum(amount), .groups = 'drop')

# outcomes <- res$segments %>%
#   rowwise() %>%
#   group_split() %>%
#   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
#   bind_rows() %>%
#   group_by(group,strategy,summary) %>%
#   summarize(value=sum(amount), .groups = 'drop')

# model <- read_model_json("~/downloads/model_68edcb86d9a2c0a36f0bd762_2025-10-14.json")
# library(purrr)
# library(tidyr)

# res <- run_model(model)
# print('discounted')
# res$segments %>%
#   rowwise() %>%
#   group_split() %>%
#   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries_discounted[[1]])),], x$summaries_discounted[[1]])) %>%
#   bind_rows() %>%
#   group_by(group,strategy,summary) %>%
#   summarize(value=sum(amount), .groups = 'drop') %>%
#   pivot_wider(names_from = summary, values_from = value)

# res$segments %>%
#   rowwise() %>%
#   group_split() %>%
#   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries_discounted[[1]])),], x$summaries_discounted[[1]])) %>%
#   bind_rows() %>%
#   pivot_wider(names_from = strategy, values_from = amount)



# print('undiscounted')
# res$segments %>%
#   rowwise() %>%
#   group_split() %>%
#   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
#   bind_rows() %>%
#   group_by(group,strategy,summary) %>%
#   summarize(value=sum(amount), .groups = 'drop') %>%
#   pivot_wider(names_from = summary, values_from = value)



# library(kable)
# library(kableExtra)
# library(flextable)
# library(knitr)

# kable(head(mtcars), format = "pipe") %>%
# add_header_above(c("a" = 2, "Car Attributes" = 10))

# headers <- list(
#     list(
#         list(span = 2, text = " ", borders = c(0,0,1,0)),
#         list(span = 10, text = "Car Attributes", borders = c(0,0,1,0))),
#     ),
#     list(
#         list(span = 1, text = "MPG", borders = c(0,0,1,0)),
#         list(span = 1, text = "Cylinders", borders = c(0,0,1,0))),
#         etc...
#     )
# )
