library(openqaly)

build_diabetes_model <- function() {

  # ── Core model definition ──────────────────────────────────────────────────
  model <- define_model("markov") |>
    set_settings(
      timeframe      = 20,
      timeframe_unit = "years",
      cycle_length   = 1,
      cycle_length_unit = "years",
      discount_cost     = 3.5,
      discount_outcomes = 3.5
    ) |>

    # ── States ───────────────────────────────────────────────────────────────
    add_state("well_controlled", initial_prob = 1) |>
    add_state("complications",   initial_prob = 0) |>
    add_state("dead",            initial_prob = 0) |>

    # ── Strategies ───────────────────────────────────────────────────────────
    add_strategy("standard")  |>
    add_strategy("intensive") |>

    # ── Groups ───────────────────────────────────────────────────────────────
    add_group("younger", weight = "0.6") |>
    add_group("older",   weight = "0.4") |>

    # ── Variables ────────────────────────────────────────────────────────────
    # Complication probability (group-specific)
    add_variable("p_complications", 0.08,
                 group = "younger",
                 sampling = beta(mean = 0.08, sd = 0.02)) |>
    add_variable("p_complications", 0.12,
                 group = "older",
                 sampling = beta(mean = 0.12, sd = 0.03)) |>

    # Treatment efficacy (strategy-specific)
    add_variable("efficacy", 0.5, strategy = "standard") |>
    add_variable("efficacy", 0.7, strategy = "intensive",
                 sampling = beta(mean = 0.7, sd = 0.08)) |>

    # Death probabilities
    add_variable("p_death_well", 0.01,
                 sampling = beta(mean = 0.01, sd = 0.003)) |>
    add_variable("p_death_comp", 0.05,
                 sampling = beta(mean = 0.05, sd = 0.01)) |>

    # Utilities
    add_variable("u_well", 0.85,
                 sampling = beta(mean = 0.85, sd = 0.05)) |>
    add_variable("u_comp", 0.6,
                 sampling = beta(mean = 0.6, sd = 0.08)) |>

    # Costs (strategy-specific drug cost)
    add_variable("c_drug", 2000, strategy = "standard",
                 sampling = gamma(mean = 2000, sd = 300)) |>
    add_variable("c_drug", 5000, strategy = "intensive",
                 sampling = gamma(mean = 5000, sd = 700)) |>
    add_variable("c_comp", 8000) |>
    add_variable("c_monitoring", 500) |>

    # Adverse-event parameters
    add_variable("p_ae", 0.15) |>
    add_variable("c_ae", 3000) |>
    add_variable("u_ae_decrement", 0.05) |>

    # Tree-derived variable
    add_variable("ae_cost_adj", p(severe, ae_tree) * c_ae) |>

    # ── Transitions ──────────────────────────────────────────────────────────
    add_transition("well_controlled", "complications",
                   p_complications * (1 - efficacy)) |>
    add_transition("well_controlled", "dead", p_death_well) |>
    add_transition("well_controlled", "well_controlled",
                   1 - p_complications * (1 - efficacy) - p_death_well) |>
    add_transition("complications", "dead", p_death_comp) |>
    add_transition("complications", "complications", 1 - p_death_comp) |>
    add_transition("dead", "dead", 1) |>

    # ── Values ───────────────────────────────────────────────────────────────
    add_value("cost_treat", c_drug + c_monitoring + ae_cost_adj,
              state = "well_controlled", type = "cost") |>
    add_value("cost_comp", c_comp + c_monitoring,
              state = "complications", type = "cost") |>
    add_value("cost_dead", 0, state = "dead", type = "cost") |>
    add_value("qaly", u_well - p(adverse_event, ae_tree) * u_ae_decrement,
              state = "well_controlled", type = "outcome") |>
    add_value("qaly", u_comp,
              state = "complications", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>

    # ── Summaries ────────────────────────────────────────────────────────────
    add_summary("total_costs", "cost_treat,cost_comp,cost_dead",
                type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000) |>

    # ── Multivariate Sampling ────────────────────────────────────────────────
    add_multivariate_sampling(
      name = "utility_correlation",
      distribution = mvnormal(
        mean = c(0.85, 0.6),
        sd   = c(0.05, 0.08),
        cor  = 0.6
      ),
      variables = c("u_well", "u_comp")
    ) |>

    # ── DSA ──────────────────────────────────────────────────────────────────
    add_dsa_variable("p_complications", low = bc * 0.5, high = bc * 1.5,
                     group = "younger") |>
    add_dsa_variable("efficacy", low = bc * 0.8, high = bc * 1.2,
                     strategy = "intensive") |>
    add_dsa_variable("c_drug", low = bc * 0.5, high = bc * 1.5,
                     strategy = "intensive") |>
    add_dsa_setting("discount_cost", low = 0, high = 5) |>

    # ── TWSA ─────────────────────────────────────────────────────────────────
    add_twsa("Drug Cost vs Efficacy") |>
    add_twsa_variable("Drug Cost vs Efficacy", "c_drug",
                      type = "range", min = 3000, max = 8000, steps = 5,
                      strategy = "intensive") |>
    add_twsa_variable("Drug Cost vs Efficacy", "efficacy",
                      type = "range", min = 0.5, max = 0.9, steps = 5,
                      strategy = "intensive") |>

    # ── Scenarios ────────────────────────────────────────────────────────────
    add_scenario("High Efficacy") |>
    add_scenario_variable("High Efficacy", "efficacy", 0.85,
                          strategy = "intensive") |>
    add_scenario_setting("High Efficacy", "timeframe", 30) |>

    add_scenario("Low Cost") |>
    add_scenario_variable("Low Cost", "c_drug", 3000,
                          strategy = "intensive") |>
    add_scenario_setting("Low Cost", "discount_cost", 0) |>

    # ── Threshold Analysis ───────────────────────────────────────────────────
    add_threshold_analysis(
      name               = "Max Drug Cost",
      variable           = "c_drug",
      lower              = 0,
      upper              = 20000,
      variable_strategy  = "intensive",
      condition = threshold_condition_nmb(
        health_summary = "total_qalys",
        cost_summary   = "total_costs",
        referent       = "intensive",
        comparator     = "standard"
      )
    ) |>

    # ── PSA & VBP ────────────────────────────────────────────────────────────
    set_psa(n_sim = 1000, seed = 42) |>
    set_vbp(
      price_variable        = "c_drug",
      intervention_strategy = "intensive",
      outcome_summary       = "total_qalys",
      cost_summary          = "total_costs"
    ) |>

    # ── Override Categories ──────────────────────────────────────────────────
    add_override_category("Model Settings", general = TRUE) |>
    add_override("Model Settings",
                 title = "Time Horizon", name = "timeframe",
                 type = "setting", input_type = "timeframe",
                 expression = 20) |>
    add_override("Model Settings",
                 title = "Discount Rate (Costs)", name = "discount_cost",
                 type = "setting", input_type = "slider",
                 expression = 3.5, min = 0, max = 10, step_size = 0.5) |>
    add_override("Model Settings",
                 title = "Discount Rate (Outcomes)", name = "discount_outcomes",
                 type = "setting", input_type = "numeric",
                 expression = 3.5, min = 0, max = 10) |>

    add_override_category("Treatment Parameters") |>
    add_override("Treatment Parameters",
                 title = "Drug Cost (Intensive)", name = "c_drug",
                 type = "variable", input_type = "numeric",
                 expression = 5000, strategy = "intensive",
                 min = 0, max = 20000) |>
    add_override("Treatment Parameters",
                 title = "Treatment Efficacy", name = "efficacy",
                 type = "variable", input_type = "slider",
                 expression = 0.7, strategy = "intensive",
                 min = 0, max = 1, step_size = 0.05) |>
    add_override("Treatment Parameters",
                 title = "Complication Rate (Young)", name = "p_complications",
                 type = "variable", input_type = "dropdown",
                 expression = 0.08, group = "younger",
                 options = list(
                   override_option("Low",       0.04),
                   override_option("Base Case", 0.08, is_base_case = TRUE),
                   override_option("High",      0.12)
                 )) |>
    add_override("Treatment Parameters",
                 title = "AE Probability", name = "p_ae",
                 type = "variable", input_type = "formula",
                 expression = 0.15)

  # ── Decision Tree ──────────────────────────────────────────────────────────
  model$trees <- tibble::tibble(
    name    = rep("ae_tree", 4),
    node    = c("ae_event", "no_ae", "ae_mild", "ae_severe"),
    tags    = c("adverse_event", "no_adverse_event", "mild", "severe"),
    parent  = c(NA,         NA,      "ae_event", "ae_event"),
    formula = c("p_ae",     "C",     "0.7",      "C")
  )

  model
}

# Build and write model
model <- build_diabetes_model()
write_model(model, "inst/models/example_markov_full/model.json", format = "json")
