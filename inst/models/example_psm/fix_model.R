# Fix the PSM model values
library(openxlsx)
library(tibble)

# Read existing workbook
wb <- loadWorkbook("/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm/model.xlsx")

# Create new values sheet with unique names
values_data <- tibble(
  name = c(
    # QALYs
    "qalys", "qalys",
    # Costs - make names unique
    "cost_drug", "cost_admin", "cost_ae", "cost_prog"
  ),
  display_name = c(
    "QALYs", "QALYs",
    "Drug Cost", "Admin Cost", "AE Cost", "Progression Cost"
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

# Remove old sheet and add new one
removeWorksheet(wb, "values")
addWorksheet(wb, "values")
writeData(wb, "values", values_data)

# Update summaries to use new value names
summaries_data <- tibble(
  name = c("total_qalys", "total_cost"),
  display_name = c("Total QALYs", "Total Cost"),
  description = c("Total quality-adjusted life years", "Total costs"),
  values = c("qalys", "cost_drug,cost_admin,cost_ae,cost_prog")
)

removeWorksheet(wb, "summaries")
addWorksheet(wb, "summaries")
writeData(wb, "summaries", summaries_data)

# Save workbook
saveWorkbook(wb, "/Users/jrdnmdhl/Code/heRomod2/inst/models/example_psm/model.xlsx", overwrite = TRUE)

cat("Model fixed!\n")