# Rcpp::compileAttributes()
# devtools::document()
# roxygen2::roxygenize()
# devtools::build()
# devtools::install(upgrade=FALSE)
library(openqaly)
#devtools::test()

library(jsonlite)
library(dplyr)
library(future)
#library(bench)

# model_name <- "markov_medium"
# model <- system.file("models", model_name, package = "openqaly") %>%
#     read_model()

# model <- read_model_json("/Users/jrdnmdhl/downloads/model_68e48d6e9ff29813a41997b6_2025-10-07.json")
# print(model$summaries)
# model$variables$formula[35]<-'2000000'


# res <- run_model(model)
# incremental_ce_table(res,"qalys", "costs")

# calculate_pairwise_ce(res, "qalys", "costs", intervention = "checkimab")
#  pairwise_ce_table(res, 'qalys', 'costs', intervention = 'checkimab')
# pairwise_ce_plot(res, 'qalys', 'costs', intervention = 'checkimab')
# pairwise_ce_plot(res, 'qalys', 'costs', comparator = 'chemoplatin')
# on.exit(plan(sequential), add = TRUE)
# plan(multisession)
# model <- define_model('markov') |>
#     set_settings(
#         timeframe = 3,
#         timeframe_unit = 'years',
#         cycle_length = 1,
#         cycle_length_unit = 'months',
#         reduce_state_cycle = TRUE,
#         discount_outcomes = 3.5,
#         discount_cost = 3.5,
#         half_cycle_method = 'life-table'
#     ) |>
#     add_strategy(name = "treatment_a", display_name = "Treatment A") |>
#     add_strategy(name = "treatment_b", display_name = "Treatment B") |>
#     add_strategy(name = "treatment_c", display_name = "Treatment C") |>
#     add_state(name = "well", display_name = "Well", initial_prob = 1) |>
#     add_state(name = "sick", display_name = "Sick", initial_prob = 0) |>
#     add_state(name = "dead", display_name = "Dead", initial_prob = 0) |>
#     add_group(name = "group_1", display_name = 'Group 1', weight = 0.5) |>
#     add_group(name = "group_2", display_name = 'Group 2', weight = 0.5) |>
#     add_variable(name = 'med_cost_per_month', strategy = 'treatment_a', formula = 20000) |>
#     add_variable(name = 'med_cost_per_month', strategy = 'treatment_b', formula = 10000) |>
#     add_variable(name = 'med_cost_per_month', strategy = 'treatment_c', formula = 15000) |>
#     add_variable(name = 'p_well_to_sick', display_name = "P(Well => Sick), Treatment A, Group 1", strategy = 'treatment_a', group = 'group_1', formula = 0.3) |>
#     add_variable(name = 'p_well_to_sick', display_name = "P(Well => Sick), Treatment B, Group 1", strategy = 'treatment_b', group = 'group_1', formula = 0.18) |>
#     add_variable(name = 'p_well_to_sick', display_name = "P(Well => Sick), Treatment C, Group 1", strategy = 'treatment_c', group = 'group_1', formula = 0.24) |>
#     add_variable(name = 'p_well_to_sick', display_name = "P(Well => Sick), Treatment A, Group 2", strategy = 'treatment_a', group = 'group_2', formula = 0.32) |>
#     add_variable(name = 'p_well_to_sick', display_name = "P(Well => Sick), Treatment B, Group 2", strategy = 'treatment_b', group = 'group_2', formula = 0.14) |>
#     add_variable(name = 'p_well_to_sick', display_name = "P(Well => Sick), Treatment C, Group 2", strategy = 'treatment_c', group = 'group_2', formula = 0.26) |>
#     add_variable(name = 'p_well_to_well', display_name = "P(Well => Well), Treatment A, Group 1", strategy = 'treatment_a', group = 'group_1', formula = 1 - p_well_to_sick - p_well_to_dead) |>
#     add_variable(name = 'p_well_to_well', display_name = "P(Well => Well), Treatment B, Group 1", strategy = 'treatment_b', group = 'group_1', formula = 1 - p_well_to_sick - p_well_to_dead) |>
#     add_variable(name = 'p_well_to_well', display_name = "P(Well => Well), Treatment C, Group 1", strategy = 'treatment_c', group = 'group_1', formula = 1 - p_well_to_sick - p_well_to_dead) |>
#     add_variable(name = 'p_well_to_well', display_name = "P(Well => Well), Treatment A, Group 2", strategy = 'treatment_a', group = 'group_2', formula = 1 - p_well_to_sick - p_well_to_dead) |>
#     add_variable(name = 'p_well_to_well', display_name = "P(Well => Well), Treatment B, Group 2", strategy = 'treatment_b', group = 'group_2', formula = 1 - p_well_to_sick - p_well_to_dead) |>
#     add_variable(name = 'p_well_to_well', display_name = "P(Well => Well), Treatment C, Group 2", strategy = 'treatment_c', group = 'group_2', formula = 1 - p_well_to_sick - p_well_to_dead) |>
#     add_variable(name = 'p_well_to_dead', display_name = "P(Well => Dead), Treatment A, Group 1", strategy = 'treatment_a', group = 'group_1', formula = 0.02) |>
#     add_variable(name = 'p_well_to_dead', display_name = "P(Well => Dead), Treatment B, Group 1", strategy = 'treatment_b', group = 'group_1', formula = 0.02) |>
#     add_variable(name = 'p_well_to_dead', display_name = "P(Well => Dead), Treatment C, Group 1", strategy = 'treatment_c', group = 'group_1', formula = 0.02) |>
#     add_variable(name = 'p_well_to_dead', display_name = "P(Well => Dead), Treatment A, Group 2", strategy = 'treatment_a', group = 'group_2', formula = 0.02) |>
#     add_variable(name = 'p_well_to_dead', display_name = "P(Well => Dead), Treatment B, Group 2", strategy = 'treatment_b', group = 'group_2', formula = 0.02) |>
#     add_variable(name = 'p_well_to_dead', display_name = "P(Well => Dead), Treatment C, Group 2", strategy = 'treatment_c', group = 'group_2', formula = 0.02) |>
#     add_variable(name = 'p_sick_to_dead', formula = 0.1) |>
#     add_variable(name = 'hospice_stay_cost', formula = 30000) |>
#     add_variable(name = 'util_well', display_name = 'Utility Value, Well', formula = 0.92) |>
#     add_variable(name = 'util_sick', display_name = 'Utility Value, Sick', formula = 0.68) |>
#     add_transition(from = 'well', to = 'well', formula = p_well_to_well) |>
#     add_transition(from = 'well', to = 'sick', formula = p_well_to_sick) |>
#     add_transition(from = 'well', to = 'dead', formula = p_well_to_dead) |>
#     add_transition(from = 'sick', to = 'dead', formula = p_sick_to_dead) |>
#     add_transition(from = 'sick', to = 'sick', formula = C) |>
#     add_transition(from = 'dead', to = 'dead', formula = 1) |>
#     add_value(name = 'well_lys', display_name = 'Life-Years, Well', state = 'well', formula = cycle_length_years) |>
#     add_value(name = 'sick_lys', display_name = 'Life-Years, Sick', state = 'sick', formula = cycle_length_years) |>
#     add_value(name = 'well_qalys', display_name = 'QALYs, Well', state = 'well', formula = well_lys * util_well) |>
#     add_value(name = 'sick_qalys', display_name = 'QALYs, Sick', state = 'sick', formula = sick_lys * util_sick) |>
#     add_value(name = 'med_cost', display_name = 'Medication Cost', state = 'well', formula = med_cost_per_month * cycle_length_months, type = 'cost') |>
#     add_value(name = 'term_cost', display_name = 'Terminal Care Cost', state = 'well', destination = 'dead', formula = hospice_stay_cost, type = 'cost') |>
#     add_value(name = 'term_cost', display_name = 'Terminal Care Cost', state = 'sick', destination = 'dead', formula = hospice_stay_cost, type = 'cost') |>
#     add_summary(name = 'lys', display_name = 'Life-Years', values = "well_lys,sick_lys") |>
#     add_summary(name = 'qalys', display_name = 'QALYs', values = "well_qalys,sick_qalys", wtp = 50000) |>
#     add_summary(name = 'costs', display_name = 'Costs', values = "med_cost,term_cost") |>
#     add_multivariate_sampling(
#         name = "transitions_from_well_tx_a_group_1",
#         distribution = dirichlet(c(30, 2, 68)),
#         variables = tibble(
#             variable = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#             strategy = c('treatment_a', 'treatment_a', 'treatment_a'),
#             group = c('group_1', 'group_1', 'group_1')
#         )
#     ) |>
#     add_multivariate_sampling(
#         name = "transitions_from_well_tx_b_group_1",
#         distribution = dirichlet(c(18, 2, 80)),
#         variables = tibble(
#             variable = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#             strategy = c('treatment_b', 'treatment_b', 'treatment_b'),
#             group = c('group_1', 'group_1', 'group_1')
#         )
#     ) |>
#     add_multivariate_sampling(
#         name = "transitions_from_well_tx_c_group_1",
#         distribution = dirichlet(c(24, 2, 74)),
#         variables = tibble(
#             variable = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#             strategy = c('treatment_c', 'treatment_c', 'treatment_c'),
#             group = c('group_1', 'group_1', 'group_1')
#         )
#     ) |>
#     add_multivariate_sampling(
#         name = "transitions_from_well_tx_a_group_2",
#         distribution = dirichlet(c(32, 2, 64)),
#         variables = tibble(
#             variable = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#             strategy = c('treatment_a', 'treatment_a', 'treatment_a'),
#             group = c('group_2', 'group_2', 'group_2')
#         )
#     ) |>
#     add_multivariate_sampling(
#         name = "transitions_from_well_tx_b_group_2",
#         distribution = dirichlet(c(14, 2, 84)),
#         variables = tibble(
#             variable = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#             strategy = c('treatment_b', 'treatment_b', 'treatment_b'),
#             group = c('group_2', 'group_2', 'group_2')
#         )
#     ) |>
#     add_multivariate_sampling(
#         name = "transitions_from_well_tx_c_group_2",
#         distribution = dirichlet(c(26, 2, 72)),
#         variables = tibble(
#             variable = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well"),
#             strategy = c('treatment_c', 'treatment_c', 'treatment_c'),
#             group = c('group_2', 'group_2', 'group_2')
#         )
#     ) |>
#     add_multivariate_sampling(
#         name = "utility_sampling",
#         distribution = mvnormal(c(0.92, 0.68), cov = matrix(c(0.01, 0, 0, 0.02), nrow=2)),
#         variables = c("util_well", "util_sick")
#     ) |>
#     add_dsa_setting('timeframe', low = 2, high = 10, display_name = 'Model Timeframe') |>
#     add_dsa_variable('util_well', low = bc - 0.04, high = bc + 0.02) |>
#     add_dsa_variable('util_sick', low = bc - 0.1, high = bc + 0.1) |>
#     add_dsa_variable('med_cost_per_month', strategy = 'treatment_a', low = bc * 0.75, high = bc*1.25)
# res1 <- run_dsa(model)
# plan(sequential)
# # res2 <- run_psa(model, n_sim = 1000, seed = 123)
# dsa_outcomes_plot(res1, "qalys", strategies='treatment_a')
# ggplot2::ggsave('~/downloads/tornado1.png', dsa_outcomes_plot(res1, "qalys"))

# # Test new dsa_nmb_plot function
# cat("\n=== Testing dsa_nmb_plot ===\n")

# # Single comparison
# cat("1. Single comparison (treatment_a vs treatment_b):\n")
# p1 <- dsa_nmb_plot(res1, health_outcome = "qalys", cost_outcome = "costs",
#                    intervention = "treatment_a",
#                    comparator = "treatment_b",
#                    wtp = 50000)
# ggplot2::ggsave('~/downloads/nmb_single.png', p1, width = 10, height = 6)

# # N×M comparison
# cat("2. N×M comparison (treatment_a, treatment_b vs treatment_c):\n")
# p2 <- dsa_nmb_plot(res1, health_outcome = "qalys", cost_outcome = "costs",
#                    intervention = c("treatment_a", "treatment_b"),
#                    comparator = "treatment_c",
#                    wtp = 50000)
# ggplot2::ggsave('~/downloads/nmb_nxm.png', p2, width = 10, height = 10)

# # Test dsa_outcomes_plot with N×M
# cat("3. Outcomes plot with N×M (treatment_a vs treatment_b, treatment_c):\n")
# p3 <- dsa_outcomes_plot(res1, "qalys",
#                         intervention = "treatment_a",
#                         comparator = c("treatment_b", "treatment_c"))
# ggplot2::ggsave('~/downloads/outcomes_nxm.png', p3, width = 10, height = 10)

# dsa_outcomes_table(
#   res1,
#   outcome = "qalys",
#   groups = NULL
# )

# cat("=== Tests complete ===\n")

# # saveRDS(res, '~/downloads/testit.rds')
# # res <- readRDS('~/downloads/testit.rds')
# # ggplot2::ggsave('~/downloads/testit.png', psa_parameter_scatter_matrix(
# #   res,
# #   variables = c("p_well_to_sick", "p_well_to_dead", "p_well_to_well", "util_well", "util_sick"),
# #   strategies = c("treatment_a", "treatment_a", "treatment_a", NA, NA),
# #   group = c("group_1", "group_1", "group_1", NA, NA)
# # ))
# # evpi_table(res, "qalys", "costs", group = NULL)
# # ggplot2::ggsave('~/downloads/testit_evpi.png', evpi_plot(res, "qalys", "costs", group = NULL))
# # ggplot2::ggsave('~/downloads/testit_psa_pair_scatter1.png',pairwise_psa_scatter_plot(res, 'qalys', 'costs', intervention = 'treatment_a', group = NULL, wtp = 50000))
# # ggplot2::ggsave('~/downloads/testit_psa_pair_scatter2.png',pairwise_psa_scatter_plot(res, 'qalys', 'costs', comparator = 'treatment_c', group = NULL, wtp = 50000))
# # ggplot2::ggsave('~/downloads/testit_psa_pair_scatter3.png',pairwise_psa_scatter_plot(res, 'qalys', 'costs', intervention = 'treatment_a', wtp = 50000))
# # ggplot2::ggsave('~/downloads/testit_psa_pair_scatter4.png',pairwise_psa_scatter_plot(res, 'qalys', 'costs', comparator = 'treatment_c', wtp = 50000))


# # # psa_scatter_plot(res, 'qalys', 'costs')
# # # pairwise_ceac_table(res, 'qalys', 'costs', intervention = 'treatment_a', group = NULL)
# # # incremental_ceac_table(res, 'qalys', 'costs', group = NULL)
# # # incremental_ceac_plot(res, 'qalys', 'costs')

# # # psa_scatter_plot(res, 'qalys', 'costs')

# # # Test pairwise CEAC with technical name (comparator perspective)


# # # pairwise_ceac_plot(res, 'qalys', 'costs', comparator = 'treatment_a', group = NULL)

# # # pairwise_ceac_plot(res, 'qalys', 'costs', intervention = 'treatment_a', group = NULL)
# # # incremental_ceac_plot(res, 'qalys', 'costs', group = NULL)

# # # Test pairwise CEAC with referent perspective
# # # pairwise_ceac_plot(res, 'qalys', 'costs', intervention = 'treatment_b')

# # # print(res)

# # # model <- model |>
# # #     add_variable(name = "p_stable_mean", formula = 0.7) |>
# # #     add_variable(name = "p_prog_mean", formula = 0.2) |>
# # #     add_variable(name = "p_death_mean", formula = 0.1) |>
# # #     add_variable(name = "alpha_stable", formula = 10) |>
# # #     add_variable(name = "alpha_prog", formula = 3) |>
# # #     add_variable(name = "alpha_death", formula = 2) |>
# # #     add_multivariate_sampling(
# # #         name = "transition_probs",
# # #         distribution = "dirichlet(c(alpha_stable, alpha_prog, alpha_death))",
# # #         variables = c("p_stable_mean", "p_prog_mean", "p_death_mean")
# # #     )

# # # psa_results <- run_psa(model, n_sim = 1000, seed = 123)


# # # trace_table(res,group=NULL)
# # # trace_table(res,group=NULL,table_format='kable')


# # # trace_table(res)
# # # trace_table(res, table_format='kable')

# # # outcomes_table(res, 'qalys', group=NULL)
# # # outcomes_table(res, 'qalys', group=NULL, table_format='kable')


# # # outcomes_table(res, 'qalys')
# # # outcomes_table(res, 'qalys', table_format='kable')

# # # outcomes_table(res, 'qalys', group=NULL, intervention = 'checkimab')
# # # outcomes_table(res, 'qalys', group=NULL, table_format='kable', intervention = 'checkimab')


# # # outcomes_table(res, 'qalys', intervention = 'checkimab')
# # # outcomes_table(res, 'qalys', table_format='kable', intervention = 'checkimab')

# # # nmb_table(res, health_outcome = 'qalys', cost_outcome = 'costs', group=NULL, intervention = 'checkimab', wtp = 30000)
# # # nmb_table(res, health_outcome = 'qalys', cost_outcome = 'costs', group=NULL, table_format='kable', intervention = 'checkimab', wtp = 30000)


# # # nmb_table(res, health_outcome = 'qalys', cost_outcome = 'costs', intervention = 'checkimab', wtp = 30000)
# # # nmb_table(res, health_outcome = 'qalys', cost_outcome = 'costs', table_format='kable', intervention = 'checkimab', wtp = 30000)

# # # nmb_table(res, health_outcome = 'qalys', cost_outcome = 'costs', intervention = 'checkimab', wtp = 30000)
# # # outcomes_table(res, 'qalys', group=NULL)

# # # outcomes_table(res, 'qalys', group=NULL, table_format='kable')
# # # outcomes_plot_bar(res, 'qalys', group=NULL)
# # # outcomes_plot_line(res, 'costs')
# # # trace_plot_line(res)
# # # nmb_plot_bar# outcomes_plot_bar(res, 'costs', intervention = 'checkimab')
# # # nmb_plot_bar(res, health_outcome = "qalys", cost_outcome = "costs", wtp=30000, intervention = "checkimab")
# # # trace_table(res, strategy_name_field = "abbreviation", state_name_field = "display_name")
# # # nmb_table(res, health_outcome = 'qalys', cost_outcome = 'costs', intervention = 'checkimab', wtp = 30000)
# # # outcomes_table(res, 'qalys', group = NULL)
# # # ref_model <- system.file("models", "markov_medium", package = "openqaly") %>% read_model()
# # # ref_res <- run_model(ref_model)
# # # outcomes_plot(res, "costs", group = NULL)
# # # write_json(ref_res, '~/downloads/test.json')


# # # state_mapper <- function(x) {
# # #     index <- match(x,model$states$name)
# # #     ref_model$states$name[index]
# # # }

# # # reorder_indices <- c(1,2,7,3,4,5,6,8)
# # # ref_trace <- ref_res$segments$collapsed_trace[[1]]
# # # res_trace <- res$segments$collapsed_trace[[1]]
# # # colnames(res_trace) <- state_mapper(colnames(res_trace))
# # # res_trace <- res_trace[,colnames(ref_trace)]

# # # round((colSums(res_trace) - colSums(ref_trace)) * 7/365, 3)

# # # res_mat <- res$segments$trace_and_values[[1]]$transitions %>%
# # #     mutate(
# # #         from_collapsed = state_mapper(from_collapsed),
# # #         to_collapsed=state_mapper(to_collapsed),
# # #         state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
# # #     ) %>%
# # #     select(cycle, from_collapsed, state_time, to_collapsed, value)


# # # ref_res_mat <- ref_res$segments$trace_and_values[[1]]$transitions %>%
# # #     mutate(
# # #         state_time = stringr::str_extract(from_expanded, "\\d+$") %>% as.integer()
# # #     ) %>%
# # #     select(cycle, from_collapsed, state_time, to_collapsed, value_ref = value)

# # # mat_comp <- full_join(ref_res_mat, res_mat, by = c("cycle", "from_collapsed", "to_collapsed", "state_time")) %>%
# # #     mutate(diff = value - value_ref)

# # # #jsonlite::toJSON(res)
  
# # # # #mark(rcpp = res <- run_model(model), max_iterations=1,check=F)


# # # # res <- run_model(model)
# # # ref_outcomes <- ref_res$segments %>%
# # #   rowwise() %>%
# # #   group_split() %>%
# # #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
# # #   bind_rows() %>%
# # #   group_by(group,strategy,summary) %>%
# # #   summarize(value=sum(amount), .groups = 'drop')

# # # outcomes <- res$segments %>%
# # #   rowwise() %>%
# # #   group_split() %>%
# # #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
# # #   bind_rows() %>%
# # #   group_by(group,strategy,summary) %>%
# # #   summarize(value=sum(amount), .groups = 'drop')

# # # model <- read_model_json("~/downloads/model_68edcb86d9a2c0a36f0bd762_2025-10-14.json")
# # # library(purrr)
# # # library(tidyr)

# # # res <- run_model(model)
# # # print('discounted')
# # # res$segments %>%
# # #   rowwise() %>%
# # #   group_split() %>%
# # #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries_discounted[[1]])),], x$summaries_discounted[[1]])) %>%
# # #   bind_rows() %>%
# # #   group_by(group,strategy,summary) %>%
# # #   summarize(value=sum(amount), .groups = 'drop') %>%
# # #   pivot_wider(names_from = summary, values_from = value)

# # # res$segments %>%
# # #   rowwise() %>%
# # #   group_split() %>%
# # #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries_discounted[[1]])),], x$summaries_discounted[[1]])) %>%
# # #   bind_rows() %>%
# # #   pivot_wider(names_from = strategy, values_from = amount)



# # # print('undiscounted')
# # # res$segments %>%
# # #   rowwise() %>%
# # #   group_split() %>%
# # #   map(function(x) cbind(select(x, group, strategy)[rep(1, nrow(x$summaries[[1]])),], x$summaries[[1]])) %>%
# # #   bind_rows() %>%
# # #   group_by(group,strategy,summary) %>%
# # #   summarize(value=sum(amount), .groups = 'drop') %>%
# # #   pivot_wider(names_from = summary, values_from = value)



# # # library(kable)
# # # library(kableExtra)
# # # library(flextable)
# # # library(knitr)

# # # kable(head(mtcars), format = "pipe") %>%
# # # add_header_above(c("a" = 2, "Car Attributes" = 10))

# # # headers <- list(
# # #     list(
# # #         list(span = 2, text = " ", borders = c(0,0,1,0)),
# # #         list(span = 10, text = "Car Attributes", borders = c(0,0,1,0))),
# # #     ),
# # #     list(
# # #         list(span = 1, text = "MPG", borders = c(0,0,1,0)),
# # #         list(span = 1, text = "Cylinders", borders = c(0,0,1,0))),
# # #         etc...
# # #     )
# # # )
