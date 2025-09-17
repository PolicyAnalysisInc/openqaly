# Example PSM Model

This is an example Partitioned Survival Model (PSM) for heRomod2.

## Model Structure

### States
- **progression_free**: Patients without disease progression
- **progressed**: Patients with disease progression  
- **dead**: Death state

### Strategies
- **standard**: Standard of care
- **new_drug**: Novel therapeutic agent with improved PFS and OS

### Endpoints
The PSM uses two survival endpoints defined in the transitions sheet:
- **PFS** (Progression-Free Survival): References the `pfs_dist` variable
- **OS** (Overall Survival): References the `os_dist` variable

Both endpoints use months as the time unit.

### Key Features
- Survival distributions vary by strategy (defined in variables sheet)
- The new drug applies hazard ratios of 0.65 for PFS and 0.7 for OS
- Values include quality of life (utilities) and costs
- Model runs for 10 years with monthly cycles

## Running the Model

```r
library(heRomod2)
library(herosurv)

# Read the model
model <- read_model("path/to/example_psm")

# Run the model
results <- run_model(model)

# View results
results$aggregated  # Aggregated results by strategy
```

## Files
- `model.xlsx`: Main model definition
- `test_final_simple.R`: Test script to verify the model runs correctly

## Results
The model calculates total QALYs and costs for each strategy over the 10-year time horizon.