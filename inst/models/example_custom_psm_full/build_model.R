library(openqaly)

build_heart_failure_model <- function() {

  # ── Core model definition ──────────────────────────────────────────────────
  model <- define_model("custom_psm") |>
    set_settings(
      timeframe         = 10,
      timeframe_unit    = "years",
      cycle_length      = 1,
      cycle_length_unit = "months",
      discount_cost     = 3.5,
      discount_outcomes = 3.5
    ) |>

    # ── States ───────────────────────────────────────────────────────────────
    add_state("nyha_i_ii") |>
    add_state("nyha_iii")  |>
    add_state("nyha_iv")   |>
    add_state("dead")      |>

    # ── Strategies ───────────────────────────────────────────────────────────
    add_strategy("standard_care")  |>
    add_strategy("novel_therapy")  |>

    # ── Groups ───────────────────────────────────────────────────────────────
    add_group("low_risk",  weight = "0.65") |>
    add_group("high_risk", weight = "0.35") |>

    # ── Variables ────────────────────────────────────────────────────────────
    # Base proportions (strategy-specific)
    add_variable("p_mild", 0.7,  strategy = "standard_care") |>
    add_variable("p_mild", 0.8,  strategy = "novel_therapy") |>
    add_variable("p_moderate_base", 0.2, strategy = "standard_care") |>
    add_variable("p_moderate_base", 0.15, strategy = "novel_therapy") |>

    # Risk factor (group-specific)
    add_variable("risk_factor", 1.0, group = "low_risk")  |>
    add_variable("risk_factor", 1.3, group = "high_risk") |>

    # Disease progression rate (per-month)
    add_variable("decay_rate", 0.005,
                 sampling = beta(mean = 0.005, sd = 0.001)) |>

    # Adjusted proportions (time- and risk-dependent, capped to valid range)
    add_variable("p_mild_adj",
                 p_mild * exp(-decay_rate * cycle) * (1 / risk_factor)) |>
    add_variable("p_moderate_adj",
                 pmin(p_moderate_base * (1 + decay_rate * cycle) * risk_factor,
                      1 - p_mild_adj)) |>

    # Utilities
    add_variable("u_mild",     0.8,
                 sampling = beta(mean = 0.8, sd = 0.05)) |>
    add_variable("u_moderate", 0.6,
                 sampling = beta(mean = 0.6, sd = 0.07)) |>
    add_variable("u_severe",   0.35,
                 sampling = beta(mean = 0.35, sd = 0.08)) |>

    # Drug costs (strategy-specific)
    add_variable("c_drug", 1500, strategy = "standard_care",
                 sampling = gamma(mean = 1500, sd = 200)) |>
    add_variable("c_drug", 8000, strategy = "novel_therapy",
                 sampling = gamma(mean = 8000, sd = 1000)) |>

    # Care costs
    add_variable("c_mild",     3000)  |>
    add_variable("c_moderate", 8000)  |>
    add_variable("c_severe",   18000) |>

    # Hospitalization parameters
    add_variable("c_hospitalization", 25000) |>
    add_variable("p_hosp",     0.1) |>
    add_variable("p_hosp_icu", 0.3) |>

    # Tree-derived variable
    add_variable("hosp_cost", p(hosp, hosp_tree) * c_hospitalization) |>

    # ── Custom PSM Transitions ───────────────────────────────────────────────
    add_transition("nyha_i_ii", p_mild_adj) |>
    add_transition("nyha_iii",  p_moderate_adj) |>
    add_transition("nyha_iv",
                              pmax(0, 1 - p_mild_adj - p_moderate_adj - 0.005 * cycle)) |>
    add_transition("dead", C) |>

    # ── Values ───────────────────────────────────────────────────────────────
    add_value("cost_treat", c_drug + c_mild,
              state = "nyha_i_ii", type = "cost") |>
    add_value("cost_treat", c_drug + c_moderate + hosp_cost,
              state = "nyha_iii", type = "cost") |>
    add_value("cost_treat", c_drug + c_severe + hosp_cost * 1.5,
              state = "nyha_iv", type = "cost") |>
    add_value("cost_treat", 0,
              state = "dead", type = "cost") |>
    add_value("qaly", u_mild,
              state = "nyha_i_ii", type = "outcome") |>
    add_value("qaly", u_moderate,
              state = "nyha_iii", type = "outcome") |>
    add_value("qaly", u_severe,
              state = "nyha_iv", type = "outcome") |>
    add_value("qaly", 0,
              state = "dead", type = "outcome") |>

    # ── Summaries ────────────────────────────────────────────────────────────
    add_summary("total_costs", "cost_treat", type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 50000) |>

    # ── Multivariate Sampling ────────────────────────────────────────────────
    add_multivariate_sampling(
      name = "utility_correlation",
      distribution = mvnormal(
        mean = c(0.8, 0.6, 0.35),
        sd   = c(0.05, 0.07, 0.08),
        cor  = matrix(c(1, 0.6, 0.3,
                         0.6, 1, 0.5,
                         0.3, 0.5, 1), nrow = 3)
      ),
      variables = c("u_mild", "u_moderate", "u_severe")
    ) |>

    # ── DSA ──────────────────────────────────────────────────────────────────
    add_dsa_variable("c_drug", low = bc * 0.5, high = bc * 1.5,
                     strategy = "novel_therapy") |>
    add_dsa_variable("decay_rate", low = bc * 0.5, high = bc * 2) |>
    add_dsa_variable("u_mild", low = bc * 0.8, high = bc * 1.2) |>
    add_dsa_setting("discount_cost", low = 0, high = 5) |>

    # ── TWSA ─────────────────────────────────────────────────────────────────
    add_twsa("Drug Cost vs Decay") |>
    add_twsa_variable("Drug Cost vs Decay", "c_drug",
                      type = "range", min = 4000, max = 12000, steps = 5,
                      strategy = "novel_therapy") |>
    add_twsa_variable("Drug Cost vs Decay", "decay_rate",
                      type = "range", min = 0.002, max = 0.01, steps = 5) |>

    # ── Scenarios ────────────────────────────────────────────────────────────
    add_scenario("Optimistic Decay") |>
    add_scenario_variable("Optimistic Decay", "decay_rate", 0.002) |>
    add_scenario_setting("Optimistic Decay", "timeframe", 15) |>

    add_scenario("High Hospitalization") |>
    add_scenario_variable("High Hospitalization", "p_hosp", 0.2) |>
    add_scenario_variable("High Hospitalization", "c_hospitalization", 30000) |>

    # ── Threshold Analysis ───────────────────────────────────────────────────
    add_threshold_analysis(
      name              = "Max Novel Drug Cost",
      variable          = "c_drug",
      lower             = 0,
      upper             = 25000,
      variable_strategy = "novel_therapy",
      condition = threshold_condition_nmb(
        health_summary = "total_qalys",
        cost_summary   = "total_costs",
        referent       = "novel_therapy",
        comparator     = "standard_care"
      )
    ) |>

    # ── PSA & VBP ────────────────────────────────────────────────────────────
    set_psa(n_sim = 1000, seed = 99) |>
    set_vbp(
      price_variable        = "c_drug",
      intervention_strategy = "novel_therapy",
      outcome_summary       = "total_qalys",
      cost_summary          = "total_costs"
    ) |>

    # ── Override Categories ──────────────────────────────────────────────────
    add_override_category("General Settings", general = TRUE) |>
    add_override("General Settings",
                 title = "Time Horizon", name = "timeframe",
                 type = "setting", input_type = "timeframe",
                 expression = 10) |>
    add_override("General Settings",
                 title = "Discount Rate (Costs)", name = "discount_cost",
                 type = "setting", input_type = "slider",
                 expression = 3.5, min = 0, max = 10, step_size = 0.5) |>
    add_override("General Settings",
                 title = "Discount Rate (Outcomes)", name = "discount_outcomes",
                 type = "setting", input_type = "numeric",
                 expression = 3.5, min = 0, max = 10) |>

    add_override_category("Clinical Parameters") |>
    add_override("Clinical Parameters",
                 title = "Drug Cost (Novel)", name = "c_drug",
                 type = "variable", input_type = "numeric",
                 expression = 8000, strategy = "novel_therapy",
                 min = 0, max = 25000) |>
    add_override("Clinical Parameters",
                 title = "Disease Progression", name = "decay_rate",
                 type = "variable", input_type = "slider",
                 expression = 0.005,
                 min = 0, max = 0.02, step_size = 0.001) |>
    add_override("Clinical Parameters",
                 title = "Risk Profile (High)", name = "risk_factor",
                 type = "variable", input_type = "dropdown",
                 expression = 1.3, group = "high_risk",
                 options = list(
                   override_option("Mild",      1.1),
                   override_option("Base Case", 1.3, is_base_case = TRUE),
                   override_option("Severe",    1.6)
                 )) |>
    add_override("Clinical Parameters",
                 title = "Hospitalization Cost", name = "c_hospitalization",
                 type = "variable", input_type = "formula",
                 expression = 25000)

  # ── Decision Tree ──────────────────────────────────────────────────────────
  model$trees <- tibble::tibble(
    name    = rep("hosp_tree", 4),
    node    = c("hospitalized", "not_hospitalized",
                "icu_stay", "ward_stay"),
    tags    = c("hosp",         "no_hosp",
                "icu",       "ward"),
    parent  = c(NA,             NA,
                "hospitalized", "hospitalized"),
    formula = c("p_hosp",       "C",
                "p_hosp_icu",   "C")
  )

  model
}

# Build and write model
model <- build_heart_failure_model()
write_model(model, "inst/models/example_custom_psm_full/model.json", format = "json")
