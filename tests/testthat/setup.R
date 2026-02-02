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

# ============================================================================
# Shared DSA Test Fixtures
# ============================================================================

# Build a simple DSA model with multiple parameters
build_simple_dsa_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10,
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years"
    ) %>%
    add_strategy("standard") %>%
    add_strategy("new_treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    # Variables with DSA specifications
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 2000, strategy = "standard") %>%
    add_variable("c_treatment", 8000, strategy = "new_treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    # DSA parameter specifications
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_healthy", low = 800, high = 1200,
                     display_name = "Cost (Healthy)") %>%
    add_dsa_variable("u_healthy", low = 0.8, high = 1.0,
                     display_name = "Utility (Healthy)") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summaries
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# DSA settings model for testing unit formatting
build_dsa_settings_model <- function() {
  define_model("markov") %>%
    set_settings(
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years",
      discount_cost = 3,
      discount_outcomes = 3
    ) %>%
    add_strategy("standard") %>%
    add_strategy("new_treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 2000, strategy = "standard") %>%
    add_variable("c_treatment", 8000, strategy = "new_treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    # DSA variables (should have no unit suffix)
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    # DSA settings (should have appropriate unit suffixes)
    add_dsa_setting("discount_cost", low = 0, high = 5,
                    display_name = "Cost Discount") %>%
    add_dsa_setting("timeframe", low = 5, high = 20,
                    display_name = "Time Horizon") %>%
    # Transitions
    add_transition("healthy", "sick", "p_sick") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy", "1 - p_sick - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summaries
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Normal ICER model: Treatment more costly AND more effective (NE quadrant)
build_normal_icer_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 10000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.5, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 5000, high = 15000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("u_sick", low = 0.3, high = 0.7,
                     display_name = "Utility (Sick)") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Dominated model: Treatment more costly AND less effective (SE quadrant)
build_dominated_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 10000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 1.5, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 5000, high = 15000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("treatment_effect", low = 1.2, high = 2.0,
                     display_name = "Treatment Harm", strategy = "treatment") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Dominant model: Treatment less costly AND more effective (NW quadrant)
build_dominant_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 5000, strategy = "control") %>%
    add_variable("c_treatment", 1000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.3, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 500, high = 2000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("treatment_effect", low = 0.1, high = 0.5,
                     display_name = "Treatment Effect", strategy = "treatment") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Flipped model: Treatment less costly BUT less effective (SW quadrant)
build_flipped_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 8000, strategy = "control") %>%
    add_variable("c_treatment", 2000, strategy = "treatment") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 1.3, strategy = "treatment") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 1000, high = 3000,
                     display_name = "Treatment Cost", strategy = "treatment") %>%
    add_dsa_variable("treatment_effect", low = 1.1, high = 1.5,
                     display_name = "Treatment Effect", strategy = "treatment") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Multiple strategies model (3 strategies)
build_multi_strategy_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10, timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) %>%
    add_strategy("control") %>%
    add_strategy("treatment_a") %>%
    add_strategy("treatment_b") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    add_variable("p_sick", 0.1) %>%
    add_variable("p_death", 0.05) %>%
    add_variable("c_healthy", 1000) %>%
    add_variable("c_sick", 5000) %>%
    add_variable("c_treatment", 0, strategy = "control") %>%
    add_variable("c_treatment", 8000, strategy = "treatment_a") %>%
    add_variable("c_treatment", 15000, strategy = "treatment_b") %>%
    add_variable("u_healthy", 0.9) %>%
    add_variable("u_sick", 0.5) %>%
    add_variable("treatment_effect", 1.0, strategy = "control") %>%
    add_variable("treatment_effect", 0.6, strategy = "treatment_a") %>%
    add_variable("treatment_effect", 0.3, strategy = "treatment_b") %>%
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Prob. Getting Sick") %>%
    add_dsa_variable("c_treatment", low = 5000, high = 12000,
                     display_name = "Treatment A Cost", strategy = "treatment_a") %>%
    add_dsa_variable("c_treatment", low = 10000, high = 20000,
                     display_name = "Treatment B Cost", strategy = "treatment_b") %>%
    add_transition("healthy", "sick", "p_sick * treatment_effect") %>%
    add_transition("healthy", "dead", "p_death") %>%
    add_transition("healthy", "healthy",
                   "1 - p_sick * treatment_effect - p_death") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    add_value("cost", "c_healthy + c_treatment", state = "healthy") %>%
    add_value("cost", "c_sick + c_treatment", state = "sick") %>%
    add_value("cost", "0", state = "dead") %>%
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    add_summary("total_cost", "cost") %>%
    add_summary("total_qalys", "qalys", wtp = 50000)
}

# Cached DSA results - computed once, reused across all tests
.dsa_cache <- new.env(parent = emptyenv())

get_cached_dsa_results <- function() {
  if (!exists("dsa_simple", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_simple <- run_dsa(build_simple_dsa_model())
  }
  .dsa_cache$dsa_simple
}

get_cached_dsa_settings_results <- function() {
  if (!exists("dsa_settings", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_settings <- run_dsa(build_dsa_settings_model())
  }
  .dsa_cache$dsa_settings
}

get_cached_normal_icer_dsa_results <- function() {
  if (!exists("dsa_normal_icer", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_normal_icer <- run_dsa(build_normal_icer_model())
  }
  .dsa_cache$dsa_normal_icer
}

get_cached_dominated_dsa_results <- function() {
  if (!exists("dsa_dominated", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_dominated <- run_dsa(build_dominated_model())
  }
  .dsa_cache$dsa_dominated
}

get_cached_dominant_dsa_results <- function() {
  if (!exists("dsa_dominant", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_dominant <- run_dsa(build_dominant_model())
  }
  .dsa_cache$dsa_dominant
}

get_cached_flipped_dsa_results <- function() {
  if (!exists("dsa_flipped", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_flipped <- run_dsa(build_flipped_model())
  }
  .dsa_cache$dsa_flipped
}

get_cached_multi_strategy_dsa_results <- function() {
  if (!exists("dsa_multi_strategy", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_multi_strategy <- run_dsa(build_multi_strategy_model())
  }
  .dsa_cache$dsa_multi_strategy
}

# Same-side bars model: both low and high DSA values produce results on same side of base
build_same_side_bars_model <- function() {
  define_model("markov") %>%
    set_settings(
      n_cycles = 10,
      timeframe = 10,
      timeframe_unit = "years",
      cycle_length = 1,
      cycle_length_unit = "years"
    ) %>%
    add_strategy("standard") %>%
    add_state("healthy", initial_prob = 1) %>%
    add_state("sick", initial_prob = 0) %>%
    add_state("dead", initial_prob = 0) %>%
    # Variable that affects outcomes positively
    add_variable("u_healthy", 0.8) %>%
    add_variable("u_sick", 0.5) %>%
    # DSA specification where both low AND high are ABOVE base case
    # (both increase utility, so QALYs will be higher in both cases)
    add_dsa_variable("u_healthy", low = 0.85, high = 0.95,
                     display_name = "Utility (Both Above Base)") %>%
    # Transitions
    add_transition("healthy", "sick", "0.1") %>%
    add_transition("healthy", "dead", "0.05") %>%
    add_transition("healthy", "healthy", "0.85") %>%
    add_transition("sick", "dead", "0.2") %>%
    add_transition("sick", "sick", "0.8") %>%
    add_transition("dead", "dead", "1") %>%
    # Values
    add_value("qalys", "u_healthy", state = "healthy") %>%
    add_value("qalys", "u_sick", state = "sick") %>%
    add_value("qalys", "0", state = "dead") %>%
    # Summary
    add_summary("total_qalys", "qalys")
}

get_cached_same_side_bars_dsa_results <- function() {
  if (!exists("dsa_same_side", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_same_side <- run_dsa(build_same_side_bars_model())
  }
  .dsa_cache$dsa_same_side
}

# DSA NMB test model - 2 strategies with WTP metadata for NMB calculations
build_dsa_nmb_test_model <- function() {
  define_model("markov") |>
    set_settings(
      timeframe = 10, timeframe_unit = "years",
      cycle_length = 1, cycle_length_unit = "years"
    ) |>
    add_strategy("standard", display_name = "Standard") |>
    add_strategy("new_treatment", display_name = "New Treatment") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_sick", 0.1) |>
    add_variable("p_death", 0.05) |>
    add_variable("c_healthy", 1000, strategy = "standard") |>
    add_variable("c_healthy", 3000, strategy = "new_treatment") |>
    add_variable("c_sick", 5000) |>
    add_variable("u_healthy", 0.9) |>
    add_variable("u_sick", 0.5) |>
    add_dsa_variable("p_sick", low = 0.05, high = 0.15,
                     display_name = "Probability of Sickness") |>
    add_dsa_variable("c_sick", low = 3000, high = 7000,
                     display_name = "Cost of Sick State") |>
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death") |>
    add_transition("sick", "dead", "0.15") |>
    add_transition("sick", "sick", "0.85") |>
    add_transition("dead", "dead", "1") |>
    add_value("cost", "c_healthy", state = "healthy") |>
    add_value("cost", "c_sick", state = "sick") |>
    add_value("qaly", "u_healthy", state = "healthy") |>
    add_value("qaly", "u_sick", state = "sick") |>
    add_summary("total_cost", "cost") |>
    add_summary("total_qalys", "qaly", wtp = 50000)
}

get_cached_dsa_nmb_results <- function() {
  if (!exists("dsa_nmb", envir = .dsa_cache)) {
    set.seed(42)
    .dsa_cache$dsa_nmb <- run_dsa(build_dsa_nmb_test_model())
  }
  .dsa_cache$dsa_nmb
}

# ============================================================================
# Shared TWSA+VBP Test Fixtures
# ============================================================================

# Build a simple Markov model for TWSA+VBP testing
# Uses inline definition (no Excel I/O) and minimal 2x2 grid for speed
build_simple_twsa_vbp_model <- function() {
  define_model("markov") |>
    set_settings(n_cycles = 10, cycle_length = 1, cycle_length_unit = "years") |>
    add_strategy("seritinib") |>
    add_strategy("volantor") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_variable("p_sick", 0.1) |>
    add_variable("p_death_healthy", 0.02) |>
    add_variable("p_death_sick", 0.15) |>
    add_variable("c_healthy", 1000) |>
    add_variable("c_sick", 5000) |>
    add_variable("c_treatment", 500, strategy = "seritinib") |>
    add_variable("c_treatment", 3000, strategy = "volantor") |>
    add_variable("u_mild", 0.85) |>
    add_variable("u_severe", 0.55) |>
    add_transition("healthy", "sick", "p_sick") |>
    add_transition("healthy", "dead", "p_death_healthy") |>
    add_transition("healthy", "healthy", "1 - p_sick - p_death_healthy") |>
    add_transition("sick", "dead", "p_death_sick") |>
    add_transition("sick", "sick", "1 - p_death_sick") |>
    add_transition("dead", "dead", "1") |>
    add_value("cost", "c_healthy + c_treatment", state = "healthy", type = "cost") |>
    add_value("cost", "c_sick + c_treatment", state = "sick", type = "cost") |>
    add_value("cost", "0", state = "dead", type = "cost") |>
    add_value("qalys", "u_mild", state = "healthy", type = "outcome") |>
    add_value("qalys", "u_severe", state = "sick", type = "outcome") |>
    add_value("qalys", "0", state = "dead", type = "outcome") |>
    add_summary("qalys", "qalys", type = "outcome") |>
    add_summary("costs", "cost", type = "cost") |>
    add_twsa("Utility Sensitivity") |>
    add_twsa_variable("Utility Sensitivity", "u_mild",
      type = "range", min = 0.75, max = 0.95, steps = 2) |>
    add_twsa_variable("Utility Sensitivity", "u_severe",
      type = "range", min = 0.40, max = 0.70, steps = 2)
}

# Cached TWSA+VBP results - computed once, reused across all tests
.twsa_vbp_cache <- new.env(parent = emptyenv())
get_cached_twsa_vbp_results <- function() {
  key <- "twsa_vbp_default"
  if (!exists(key, envir = .twsa_vbp_cache)) {
    set.seed(42)
    .twsa_vbp_cache[[key]] <- run_twsa(
      build_simple_twsa_vbp_model(),
      vbp_price_variable = "c_treatment",
      vbp_intervention = "volantor",
      vbp_outcome_summary = "qalys",
      vbp_cost_summary = "costs"
    )
  }
  .twsa_vbp_cache[[key]]
}

# ============================================================================
# Shared DSA+VBP Test Fixtures
# ============================================================================

build_dsa_vbp_model <- function() {
  model_path <- system.file("models", "example_psm", package = "openqaly")
  if (model_path == "") {
    model_path <- "inst/models/example_psm"
  }
  read_model(model_path) %>%
    add_dsa_variable("u_pfs", low = bc * 0.8, high = bc * 1.2)
}

.dsa_vbp_cache <- new.env(parent = emptyenv())

get_cached_dsa_vbp_results <- function() {
  key <- "dsa_vbp_default"
  if (!exists(key, envir = .dsa_vbp_cache)) {
    set.seed(42)
    .dsa_vbp_cache[[key]] <- run_dsa(
      build_dsa_vbp_model(),
      vbp_price_variable = "c_drug",
      vbp_intervention = "targeted"
    )
  }
  .dsa_vbp_cache[[key]]
}

get_cached_dsa_vbp_model <- function() {
  if (!exists("dsa_vbp_model", envir = .dsa_vbp_cache)) {
    .dsa_vbp_cache$dsa_vbp_model <- build_dsa_vbp_model()
  }
  .dsa_vbp_cache$dsa_vbp_model
}

# ============================================================================
# Shared VBP Test Fixtures
# ============================================================================

# Cached VBP results - computed once, reused across all tests
.vbp_cache <- new.env(parent = emptyenv())

get_cached_vbp_results <- function() {
  if (!exists("vbp_default", envir = .vbp_cache)) {
    set.seed(42)
    model_path <- system.file("models", "example_psm", package = "openqaly")
    if (model_path == "") model_path <- "inst/models/example_psm"
    model <- read_model(model_path)
    .vbp_cache$vbp_default <- run_vbp(
      model,
      price_variable = "c_drug",
      intervention_strategy = "targeted",
      outcome_summary = "total_qalys",
      cost_summary = "total_cost"
    )
  }
  .vbp_cache$vbp_default
}

get_cached_vbp_model <- function() {
  if (!exists("vbp_model", envir = .vbp_cache)) {
    model_path <- system.file("models", "example_psm", package = "openqaly")
    if (model_path == "") model_path <- "inst/models/example_psm"
    .vbp_cache$vbp_model <- read_model(model_path)
  }
  .vbp_cache$vbp_model
}
