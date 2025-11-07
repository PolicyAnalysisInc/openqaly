# EVPI Example - Testing the new EVPI implementation
# This script demonstrates how to calculate and visualize EVPI from PSA results

library(heRomod2)

# Load a model with PSA
# Assuming you have a model with PSA already set up
# model <- read_model(system.file("models/example", package = "heRomod2"))

# Run PSA (with a reasonable number of simulations)
# psa_results <- run_psa(model, n_sim = 1000, seed = 123)

# Example 1: Calculate EVPI at default WTP thresholds
# evpi <- calculate_evpi(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost"
# )
# print(evpi)

# Example 2: Calculate EVPI at custom WTP thresholds
# evpi_custom <- calculate_evpi(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost",
#   wtp = c(0, 20000, 30000, 50000, 75000, 100000, 150000)
# )
# print(evpi_custom)

# Example 3: Create EVPI table
# evpi_table(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost",
#   wtp_thresholds = c(20000, 50000, 100000, 150000),
#   table_format = "kable"
# )

# Example 4: Create EVPI plot
# evpi_plot(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost",
#   wtp_range = c(0, 200000),
#   wtp_step = 5000,
#   title = "Expected Value of Perfect Information",
#   xlab = "Willingness to Pay ($/QALY)",
#   ylab = "EVPI ($)"
# )

# Example 5: Multiple groups
# If your model has multiple groups, EVPI is calculated separately for each
# evpi_multigroup <- calculate_evpi(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost",
#   group = NULL  # Include all groups
# )

# evpi_table(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost",
#   group = NULL,  # Show all groups as columns
#   table_format = "kable"
# )

# evpi_plot(
#   psa_results,
#   outcome_summary = "total_qalys",
#   cost_summary = "total_cost",
#   group = NULL  # Will facet by group
# )

# Interpretation:
# - EVPI represents the maximum value of eliminating all parameter uncertainty
# - EVPI = 0 means the same strategy is optimal in all PSA simulations
# - Higher EVPI indicates greater decision uncertainty
# - EVPI provides an upper bound on the value of conducting further research
# - EVPI typically increases with WTP threshold as decision uncertainty grows
