library(openqaly)

build_nsclc_model <- function() {

  # ── Core model definition ──────────────────────────────────────────────────
  model <- define_model("psm") |>
    set_settings(
      timeframe         = 5,
      timeframe_unit    = "years",
      cycle_length      = 1,
      cycle_length_unit = "months",
      discount_cost     = 3.5,
      discount_outcomes = 3.5
    ) |>

    # ── States (exactly 3 for PSM) ──────────────────────────────────────────
    add_state("progression_free") |>
    add_state("progressed")       |>
    add_state("dead")             |>

    # ── Strategies ───────────────────────────────────────────────────────────
    add_strategy("chemo")  |>
    add_strategy("immuno") |>

    # ── Groups ───────────────────────────────────────────────────────────────
    add_group("pd_l1_high", weight = "0.45") |>
    add_group("pd_l1_low",  weight = "0.55") |>

    # ── Variables ────────────────────────────────────────────────────────────
    # Survival distributions (strategy-specific)
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.12),
                 strategy = "chemo") |>
    add_variable("pfs_dist", define_surv_param("exp", rate = 0.06),
                 strategy = "immuno") |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.06),
                 strategy = "chemo") |>
    add_variable("os_dist", define_surv_param("exp", rate = 0.03),
                 strategy = "immuno") |>

    # Utilities
    add_variable("u_pfs",  0.75,
                 sampling = beta(mean = 0.75, sd = 0.05)) |>
    add_variable("u_prog", 0.5,
                 sampling = beta(mean = 0.5, sd = 0.08)) |>

    # Drug costs (strategy-specific)
    add_variable("c_drug", 3000, strategy = "chemo",
                 sampling = gamma(mean = 3000, sd = 400)) |>
    add_variable("c_drug", 12000, strategy = "immuno",
                 sampling = gamma(mean = 12000, sd = 1500)) |>

    # Other costs
    add_variable("c_admin",      500) |>
    add_variable("c_prog_care", 6000) |>
    add_variable("c_bsc",       2000) |>

    # Adverse-event parameters (strategy-specific)
    add_variable("p_ae_grade3", 0.2, strategy = "chemo") |>
    add_variable("p_ae_grade3", 0.1, strategy = "immuno") |>
    add_variable("c_ae", 5000) |>

    # Tree-derived variable
    add_variable("ae_cost", p(ae, ae_tree) * c_ae) |>

    # ── PSM Transitions ──────────────────────────────────────────────────────
    add_transition("PFS", "months", pfs_dist) |>
    add_transition("OS",  "months", os_dist)  |>

    # ── Values ───────────────────────────────────────────────────────────────
    add_value("cost_treat", c_drug + c_admin + ae_cost,
              state = "progression_free", type = "cost") |>
    add_value("cost_prog", c_prog_care + c_bsc,
              state = "progressed", type = "cost") |>
    add_value("cost_dead", 0, state = "dead", type = "cost") |>
    add_value("qaly", u_pfs,
              state = "progression_free", type = "outcome") |>
    add_value("qaly", u_prog,
              state = "progressed", type = "outcome") |>
    add_value("qaly", 0, state = "dead", type = "outcome") |>

    # ── Summaries ────────────────────────────────────────────────────────────
    add_summary("total_costs", "cost_treat,cost_prog,cost_dead",
                type = "cost") |>
    add_summary("total_qalys", "qaly", type = "outcome", wtp = 100000) |>

    # ── Multivariate Sampling ────────────────────────────────────────────────
    add_multivariate_sampling(
      name = "utility_correlation",
      distribution = mvnormal(
        mean = c(0.75, 0.5),
        sd   = c(0.05, 0.08),
        cor  = 0.5
      ),
      variables = c("u_pfs", "u_prog")
    ) |>

    # ── DSA ──────────────────────────────────────────────────────────────────
    add_dsa_variable("c_drug", low = bc * 0.5, high = bc * 1.5,
                     strategy = "immuno") |>
    add_dsa_variable("u_pfs", low = bc * 0.8, high = bc * 1.2) |>
    add_dsa_variable("c_prog_care", low = bc * 0.5, high = bc * 1.5) |>
    add_dsa_setting("discount_cost", low = 0, high = 5) |>

    # ── TWSA ─────────────────────────────────────────────────────────────────
    add_twsa("Drug Cost vs PFS Utility") |>
    add_twsa_variable("Drug Cost vs PFS Utility", "c_drug",
                      type = "range", min = 8000, max = 18000, steps = 5,
                      strategy = "immuno") |>
    add_twsa_variable("Drug Cost vs PFS Utility", "u_pfs",
                      type = "range", min = 0.6, max = 0.9, steps = 5) |>

    # ── Scenarios ────────────────────────────────────────────────────────────
    add_scenario("Extended Horizon") |>
    add_scenario_setting("Extended Horizon", "timeframe", 10) |>

    add_scenario("No Discounting") |>
    add_scenario_setting("No Discounting", "discount_cost", 0) |>
    add_scenario_setting("No Discounting", "discount_outcomes", 0) |>

    add_scenario("Lower Drug Cost") |>
    add_scenario_variable("Lower Drug Cost", "c_drug", 8000,
                          strategy = "immuno") |>

    # ── Threshold Analysis ───────────────────────────────────────────────────
    add_threshold_analysis(
      name              = "Max Immuno Cost",
      variable          = "c_drug",
      lower             = 0,
      upper             = 30000,
      variable_strategy = "immuno",
      condition = threshold_condition_nmb(
        health_summary = "total_qalys",
        cost_summary   = "total_costs",
        referent       = "immuno",
        comparator     = "chemo"
      )
    ) |>

    # ── PSA & VBP ────────────────────────────────────────────────────────────
    set_psa(n_sim = 1000, seed = 123) |>
    set_vbp(
      price_variable        = "c_drug",
      intervention_strategy = "immuno",
      outcome_summary       = "total_qalys",
      cost_summary          = "total_costs"
    ) |>

    # ── Override Categories ──────────────────────────────────────────────────
    add_override_category("General Settings", general = TRUE) |>
    add_override("General Settings",
                 title = "Time Horizon", name = "timeframe",
                 type = "setting", input_type = "timeframe",
                 expression = 5) |>
    add_override("General Settings",
                 title = "Discount Rate (Costs)", name = "discount_cost",
                 type = "setting", input_type = "slider",
                 expression = 3.5, min = 0, max = 10, step_size = 0.5) |>
    add_override("General Settings",
                 title = "Discount Rate (Outcomes)", name = "discount_outcomes",
                 type = "setting", input_type = "numeric",
                 expression = 3.5, min = 0, max = 10) |>

    add_override_category("Drug Parameters") |>
    add_override("Drug Parameters",
                 title = "Immunotherapy Cost", name = "c_drug",
                 type = "variable", input_type = "numeric",
                 expression = 12000, strategy = "immuno",
                 min = 0, max = 30000) |>
    add_override("Drug Parameters",
                 title = "PFS Utility", name = "u_pfs",
                 type = "variable", input_type = "slider",
                 expression = 0.75,
                 min = 0, max = 1, step_size = 0.01) |>
    add_override("Drug Parameters",
                 title = "AE Rate (Immuno)", name = "p_ae_grade3",
                 type = "variable", input_type = "dropdown",
                 expression = 0.1, strategy = "immuno",
                 options = list(
                   override_option("Low",       0.05),
                   override_option("Base Case", 0.1, is_base_case = TRUE),
                   override_option("High",      0.2)
                 )) |>
    add_override("Drug Parameters",
                 title = "Progressed Care Cost", name = "c_prog_care",
                 type = "variable", input_type = "formula",
                 expression = 6000)

  # ── Decision Tree ──────────────────────────────────────────────────────────
  model$trees <- tibble::tibble(
    name    = rep("ae_tree", 4),
    node    = c("has_ae", "no_ae", "ae_hospitalized", "ae_outpatient"),
    tags    = c("ae",     "none",  "hosp",            "outpt"),
    parent  = c(NA,       NA,      "has_ae",          "has_ae"),
    formula = c("p_ae_grade3", "C", "0.3",            "C")
  )

  model
}

# Build and write model
model <- build_nsclc_model()
write_model(model, "inst/models/example_psm_full/model.json", format = "json")
