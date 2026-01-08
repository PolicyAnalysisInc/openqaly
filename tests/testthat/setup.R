# Load common packages used across tests
library(testthat)
library(openqaly)
library(dplyr)
library(ggplot2)

# Force sequential execution in tests to avoid parallelization issues
# This prevents FutureInterruptError from MultisessionFuture workers
future::plan(future::sequential)

# ============================================================================
# Shared PSA Test Fixtures
# ============================================================================

# Build a simple Markov model with sampling distributions for PSA testing
# This avoids loading large example models and runs quickly
build_simple_psa_model <- function() {
  define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    add_strategy("standard") |>
    add_strategy("new_treatment") |>
    add_strategy("experimental") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    # Variables with sampling distributions for PSA
    add_variable("p_sick", 0.1, sampling = normal(0.1, 0.02)) |>
    add_variable("p_death_healthy", 0.02, sampling = normal(0.02, 0.005)) |>
    add_variable("p_death_sick", 0.15, sampling = normal(0.15, 0.03)) |>
    add_variable("p_stay_healthy", 0.88) |>
    add_variable("p_stay_sick", 0.85) |>
    add_variable("c_healthy", 1000, sampling = normal(1000, 100)) |>
    add_variable("c_sick", 5000, sampling = normal(5000, 500)) |>
    add_variable("c_treatment", 2000,
                 strategy = "standard",
                 sampling = normal(2000, 200)) |>
    add_variable("c_treatment", 8000,
                 strategy = "new_treatment",
                 sampling = normal(8000, 800)) |>
    add_variable("c_treatment", 12000,
                 strategy = "experimental",
                 sampling = normal(12000, 1200)) |>
    add_variable("u_healthy", 0.9, sampling = normal(0.9, 0.05)) |>
    add_variable("u_sick", 0.5, sampling = normal(0.5, 0.1)) |>
    # Transitions
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death_healthy") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death_healthy") |>
    add_transition("sick", "dead", "p_death_sick") |>
    add_transition("sick", "sick", "1 - p_death_sick") |>
    add_transition("dead", "dead", "1") |>
    # Values
    add_value("cost", "c_healthy + c_treatment", state = "healthy") |>
    add_value("cost", "c_sick + c_treatment", state = "sick") |>
    add_value("cost", "0", state = "dead") |>
    add_value("qalys", "u_healthy", state = "healthy") |>
    add_value("qalys", "u_sick", state = "sick") |>
    add_value("qalys", "0", state = "dead") |>
    # Summaries
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qalys")
}

# Cached PSA results - computed once, reused across all tests
# This dramatically reduces test runtime (from ~740s to ~30s)
.psa_cache <- new.env(parent = emptyenv())
get_cached_psa_results <- function(n_sim = 50) {
  key <- paste0("psa_", n_sim)
  if (!exists(key, envir = .psa_cache)) {
    set.seed(42)
    .psa_cache[[key]] <- run_psa(build_simple_psa_model(), n_sim = n_sim)
  }
  .psa_cache[[key]]
}
