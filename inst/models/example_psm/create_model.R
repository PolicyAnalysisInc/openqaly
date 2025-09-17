# Create example PSM model
library(openxlsx)
library(tibble)

# Create workbook
wb <- createWorkbook()

# Settings sheet
addWorksheet(wb, "settings")
settings_data <- tibble(
  setting = c("model_type", "cycle_length", "cycle_length_unit", "timeframe", "timeframe_unit", "days_per_year"),
  value = c("psm", "1", "months", "10", "years", "365.25")
)
writeData(wb, "settings", settings_data)

# Strategies sheet
addWorksheet(wb, "strategies")
strategies_data <- tibble(
  name = c("standard", "new_drug"),
  display_name = c("Standard of Care", "New Drug"),
  description = c("Current standard treatment", "Novel therapeutic agent")
)
writeData(wb, "strategies", strategies_data)

# Groups sheet
addWorksheet(wb, "groups")
groups_data <- tibble(
  name = c("all"),
  display_name = c("All Patients"),
  description = c("Full patient population"),
  weight = c(1),
  enabled = c(1)
)
writeData(wb, "groups", groups_data)

# States sheet (3 states for PSM)
addWorksheet(wb, "states")
states_data <- tibble(
  name = c("progression_free", "progressed", "dead"),
  display_name = c("Progression Free", "Progressed", "Dead"),
  description = c("No disease progression", "Disease has progressed", "Death"),
  initial_probability = c(1, 0, 0),
  state_group = c("alive", "alive", "dead"),
  max_state_time = c(0, 0, 0),
  share_state_time = c(0, 0, 0),
  state_cycle_limit = c(0, 0, 0),
  state_cycle_limit_unit = c("cycles", "cycles", "cycles")
)
writeData(wb, "states", states_data)

# Transitions sheet (PSM format)
addWorksheet(wb, "transitions")
transitions_data <- tibble(
  endpoint = c("PFS", "OS"),
  time_unit = c("months", "months"),
  formula = c("pfs_dist", "os_dist")
)
writeData(wb, "transitions", transitions_data)

# Variables sheet
addWorksheet(wb, "variables")
variables_data <- tibble(
  name = c(
    # Standard of care distributions
    "pfs_dist", "os_dist",
    # New drug distributions  
    "pfs_dist", "os_dist",
    # Utilities
    "u_pfs", "u_prog",
    # Costs per cycle
    "c_drug", "c_admin", "c_ae", "c_prog"
  ),
  display_name = c(
    "PFS Distribution", "OS Distribution",
    "PFS Distribution", "OS Distribution",
    "Utility PFS", "Utility Progressed",
    "Drug Cost", "Administration Cost", "AE Cost", "Progression Cost"
  ),
  description = c(
    "Progression-free survival", "Overall survival",
    "Progression-free survival", "Overall survival",
    "Quality of life in PFS", "Quality of life after progression",
    "Monthly drug cost", "Monthly admin cost", "Monthly AE cost", "Monthly progression cost"
  ),
  formula = c(
    # Standard of care
    'define_surv_param("weibull", shape = 1.2, scale = 8)', 
    'define_surv_param("weibull", shape = 1.1, scale = 14)',
    # New drug
    'apply_hr(define_surv_param("weibull", shape = 1.2, scale = 8), 0.65)',
    'apply_hr(define_surv_param("weibull", shape = 1.1, scale = 14), 0.7)',
    # Utilities
    "0.8", "0.6",
    # Costs
    "vswitch(strategy, 'standard' = 5000, 'new_drug' = 12000)",
    "vswitch(strategy, 'standard' = 500, 'new_drug' = 800)",
    "vswitch(strategy, 'standard' = 200, 'new_drug' = 400)",
    "1500"
  ),
  strategy = c(
    "standard", "standard",
    "new_drug", "new_drug",
    NA, NA,
    NA, NA, NA, NA
  ),
  group = rep(NA, 10)
)
writeData(wb, "variables", variables_data)

# Values sheet
addWorksheet(wb, "values")
values_data <- tibble(
  name = c(
    # QALYs
    "qalys", "qalys",
    # Costs
    "cost", "cost", "cost", "cost"
  ),
  display_name = c(
    "QALYs", "QALYs",
    "Total Cost", "Total Cost", "Total Cost", "Total Cost"
  ),
  description = c(
    "Quality-adjusted life years in PFS", "Quality-adjusted life years progressed",
    "Drug costs", "Administration costs", "AE costs", "Progression costs"
  ),
  state = c(
    "progression_free", "progressed",
    NA, NA, NA, "progressed"
  ),
  destination = rep(NA, 6),
  formula = c(
    "u_pfs", "u_prog",
    "c_drug", "c_admin", "c_ae", "c_prog"
  )
)
writeData(wb, "values", values_data)

# Summaries sheet
addWorksheet(wb, "summaries")
summaries_data <- tibble(
  name = c("total_qalys", "total_cost"),
  display_name = c("Total QALYs", "Total Cost"),
  description = c("Total quality-adjusted life years", "Total costs"),
  values = c("qalys", "cost")
)
writeData(wb, "summaries", summaries_data)

# Save workbook
saveWorkbook(wb, "/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm/model.xlsx", overwrite = TRUE)

cat("PSM model created successfully!\n")