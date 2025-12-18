calc_med_cost_ <- function(time, model_cycle_length, dose_unit, dose, doses_per_cycle, med_cycle_length, max_cycles, cost_per_mg, bsa, weight) {
    if (max_cycles == 0) max_cycles <- Inf
    dose_mult <- 1
    if (dose_unit[1] == 'mg per kg') {
        dose_mult <- weight[1]
    } else if(dose_unit[1] == 'mg per m^2') {
        dose_mult <- bsa[1]
    }
    cost_per_dose <- dose_mult * dose[1] * cost_per_mg[1]
    cost_per_cycle <- cost_per_dose * doses_per_cycle
    cost_per_time <- cost_per_cycle / med_cycle_length
    max_time <- max_cycles * med_cycle_length
    interval_start <- pmin(time - model_cycle_length, max_time)
    interval_end <- pmin(time, max_time)
    interval_length <- interval_end - interval_start
    interval_cost <- interval_length * cost_per_time
    
    interval_cost
}

calc_med_cost <- function(time, cycle_length, bsa, weight, regimen) {
    calc_med_cost_(time, cycle_length, regimen[['Dosing Unit']], regimen[['Dose']], regimen[['Doses per Cycle']], regimen[['Cycle Length (days)']], regimen[['Max Cycles']], regimen[['Cost per mg']], bsa, weight)
}