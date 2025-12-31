# `add_hazards`: Add Hazards

## Description

Combine two or more survival distributions as independent risks
by adding their hazards.

## Usage

```r
add_hazards(dist1, dist2, ...)
```

## Arguments

* `dist1`: survival distribution to add
* `dist2`: second survival distribution to add
* `...`: additional survival distributions to add

## Value

a `surv_add_haz` object

## Examples

```r
dist1 <- define_surv_param("exp", rate = .125)
dist2 <- define_surv_param("weibull", shape = 1.2, scale = 50)
combined_dist <- add_hazards(dist1, dist2)
```

# `apply_af`: Apply Acceleration Factor

## Description

Apply an acceleration factor to proportionally increase or reduce
the time-to-event of a survival distribution.

## Usage

```r
apply_af(dist, af, log_af = FALSE)
```

## Arguments

* `dist`: a survival distribution
* `af`: an acceleration factor to be applied to survival distribution
* `log_af`: optional argument (defaults to `FALSE`) to indicate that
provided acceleration factor is on log scale

## Value

A `surv_aft` object.

## Examples

```r
dist1 <- define_surv_param("exp", rate = 0.25)
aft_dist <- apply_af(dist1, 1.5)
```

# `apply_hr`: Apply Hazard Ratio

## Description

Apply hazard ratio to a distribution to proportionally reduce or increase
the hazard rate.

## Usage

```r
apply_hr(dist, hr, log_hr = FALSE)
```

## Arguments

* `dist`: a survival distribution
* `hr`: a hazard ratio to be applied to survival distribution
* `log_hr`: optional argument (defaults to `FALSE`) to indicate that provided
hazard ratio is on log scale

## Value

A `surv_ph` object.

## Examples

```r
ph_dist <- apply_hr(
 define_surv_param("exp", rate = 0.25),
 0.5
)
```

# `apply_or`: Apply Odds Ratio

## Description

Apply an odds ratio to proportionally increase or reduce
the odds of survival

## Usage

```r
apply_or(dist, or, log_or = FALSE)
```

## Arguments

* `dist`: a survival distribution
* `or`: an odds ratio to be applied to survival distribution
* `log_or`: optional argument (defaults to `FALSE`) to indicate that
provided odds ratio is on log scale

## Value

A `surv_po` object.

## Examples

```r
dist1 <- define_surv_param("exp", rate = 0.25)
po_dist <- apply_or(dist1, 1.12)
```

# `apply_shift`: Apply Fixed Shift in Time

## Description

Apply a fixed shift in time to move the hazards of a survival
distribution forwards or backwards.

## Usage

```r
apply_shift(dist, shift)
```

## Arguments

* `dist`: a survival distribution
* `shift`: amount of time to shift distribution, where a positive
value moves hazards forward and a negative value delays hazards.

## Value

A `surv_shift` object.

## Examples

```r
shifted_dist <- apply_shift(
 define_surv_param("exp", rate = 0.25),
 3
)
```

# `define_surv_cure`: Define Parametric Mixture or Non-Mixture Cure Distribution

## Description

Define parametric mixture or not mixture cure distribution
with given parameter values.

## Usage

```r
define_surv_cure(distribution, theta, ..., mixture = TRUE)

define_survival_cure(distribution, theta, ..., mixture = TRUE)
```

## Arguments

* `distribution`: A parametric survival distribution. See
details for a listing of valid distributions.
* `theta`: A numeric value representing cure fraction
* `...`: Additional distribution parameters
(see details section below)
* `mixture`: a logical determining whether a mixture
or non-mixture model is being defined.

## Details

Supported distributions are listed in the table below.

|                  |                            |                 |                                                       |
|------------------|----------------------------|-----------------|-------------------------------------------------------|
|***Distribution***|***Description***           |***Parameters*** |***Notes***                                            |
|"exp"             |Exponential                 |rate             |                                                       |
|"lnorm"           |Lognormal                   |meanlog, sdlog   |                                                       |
|"llogis"          |Log-Logistic                |shape, scale     |                                                       |
|"weibull"         |Weibull (AFT)               |shape, scale     |                                                       |
|"weibullPH"       |Weibull (PH)                |shape, scale     |                                                       |
|"gompertz"        |Gompertz                    |shape, rate      |                                                       |
|"gamma"           |Gamma                       |shape, scale     |                                                       |
|"gengamma"        |Generalized Gamma (stable)  |mu, sigma, Q     |Parameterization from Prentice (1974)                  |
|"gengamma.orig"   |Generalized Gamma (original)|shape, scale, k  |Original parameterization from Stacy (1962)            |
|"genf"            |Generalized F (stable)      |mu, sigma, Q, P  |Stable reparameterization from Prentice (1975)         |
|"genf.org"        |Generalized F (original)    |mu, sigma, s1, s2|Origninal parameterization described by Prentice (1975)|

## References

Stacy, E. W. (1962). A generalization of the gamma
distribution.  Annals of Mathematical Statistics 33:1187-92.
Prentice, R. L. (1974). A log gamma model and its maximum likelihood
estimation. Biometrika 61(3):539-544.
R. L. Prentice (1975). Discrimination among some parametric
models. Biometrika 62(3):607-614.

## Value

a `surv_dist_cure` object.

## Examples

```r
define_surv_cure(distribution = "exp", theta = 0.34, rate = .5)
define_surv_cure(
 distribution = "weibull",
 theta = 0.5, shape = 1.5,
 scale = 34.43,
 mixture = TRUE
)
# Deprecated alias included for backwards compatability with openqaly
define_survival_cure(distribution = "exp", theta = 0.24, rate = 0.023)
```

# `define_surv_func`: Define survival distribution based on a function

## Description

Define survival distribution based on a function

## Usage

```r
define_surv_func(f, ...)
```

## Arguments

* `f`: a function that takes a vector of times and returns a vector
of corresponding survival probabilities
* `...`: additional arguments to be passed to f

## Value

a `surv_function` object.

# `define_surv_km`: Define Kaplan-Meier Distribution

## Description

Define a survival distribution based on a table of Kaplan-Meier
output containing times and survival probabilities.

## Usage

```r
define_surv_km(x, time_col = "time", surv_col = "survival")

define_surv_table(x, time_col = "time", surv_col = "survival")
```

## Arguments

* `x`: a data frame with columns for time and survival probability. By
default, these columns are assumed to be named `time` and `survival`, but
these can be configured by via the optional parameters.
* `time_col`: the name of the time column (defaults to `time`)
* `surv_col`: the name of the time column (defaults to `survival`)

## Value

a `surv_km` object.

## Examples

```r
df <- data.frame(
     time = c(0, 1, 5, 10),
     survival = c(1, 0.9, 0.7, 0.5)
)
define_surv_km(df)
```

# `define_surv_lifetable`: Define Life-Table Distribution

## Description

Define a survival distribution based on a life-table containing mortality rates by age
and gender.

## Usage

```r
define_surv_lifetable(
  x,
  start_age,
  percent_male,
  output_unit = "years",
  age_col = "age",
  male_col = "male",
  female_col = "female",
  dpy = get_dpy(),
  percent_female = NULL
)
```

## Arguments

* `x`: a data frame with columns for the starting age in each age band, the
conditional probability of death at each age for men, and the conditional probability
of death at each for women. Default column names are "age", "male", and "female", but can
be set via optional arguments.
* `start_age`: starting age of the population.
* `percent_male`: percent of population that is male. Calculated as 1 - percent_female if not provided.
* `output_unit`: optional arguemnt for time unit resulting survival distribution will be defined
in terms of. Valid options are `c("days", "weeks", "months", "years")`. Defaults to `"years"`.
* `age_col`: optional argument to change name of the `age` column accepted by the
first argument.
* `male_col`: optional argument to change name of the `male` column accepted by the
first argument.
* `female_col`: optional argument to change name of the `female` column accepted by the
first argument.
* `dpy`: optional argument specifying the number of days per year used in time unit
conversion. This argument will be populated automatically in an openqaly model.
* `percent_female`: percent of population that is female. Calculated as 1 - percent_male if not provided.

## Value

a `surv_lifetable` object.

## Examples

```r
x <- data.frame(
     age = c(0, 1, 2, 3),
     male = c(0.011, 0.005, 0.003, 0.002),
     female = c(0.010, 0.005, 0.004, 0.002)
 )
 define_surv_lifetable(x, 1, 0.45)
```

# `define_surv_param`: Define Parametric Distribution

## Description

Define parametric survival distribution with given
parameter values. A complete listing of supported
distributions is provided in the details section.

## Usage

```r
define_surv_param(distribution, ...)

define_survival(distribution, ...)
```

## Arguments

* `distribution`: a parametric survival distribution.
* `...`: additional distribution parameters
(see details section below)

## Details

Supported distributions are listed in the table below.

|                  |                            |                 |                            |
|------------------|----------------------------|-----------------|----------------------------|
|***Distribution***|***Description***           |***Parameters*** |***Notes***                 |
|"exp"             |Exponential                 |rate             |                            |
|"lnorm"           |Lognormal                   |meanlog, sdlog   |                            |
|"llogis"          |Log-Logistic                |shape, scale     |                            |
|"weibull"         |Weibull (AFT)               |shape, scale     |                            |
|"weibullPH"       |Weibull (PH)                |shape, scale     |                            |
|"gompertz"        |Gompertz                    |shape, rate      |                            |
|"gamma"           |Gamma                       |shape, scale     |                            |
|"gengamma"        |Generalized Gamma (stable)  |mu, sigma, Q     |Described in Prentice (1974)|
|"gengamma.orig"   |Generalized Gamma (original)|shape, scale, k  |Described in Stacy (1962)   |
|"genf"            |Generalized F (stable)      |mu, sigma, Q, P  |Described in Prentice (1975)|
|"genf.org"        |Generalized F (original)    |mu, sigma, s1, s2|Described in Prentice (1975)|

## References

Stacy, E. W. (1962). A generalization of the gamma
distribution.  Annals of Mathematical Statistics 33:1187-92.
Prentice, R. L. (1974). A log gamma model and its maximum likelihood
estimation. Biometrika 61(3):539-544.
R. L. Prentice (1975). Discrimination among some parametric
models. Biometrika 62(3):607-614.

## Value

a `surv_parametric` object.

## Examples

```r
define_surv_param(distribution = "exp", rate = .5)
define_surv_param(distribution = "gompertz", rate = .5, shape = 1)

# Deprecated alias included for backwards compatability with openqaly
define_survival(distribution = "exp", rate = 0.05)
```

# `define_surv_spline`: Define Royston & Parmar Spline Survival Distribution

## Description

Define Royston & Parmar restricted cubic spline parametric
survival distribution.

## Usage

```r
define_surv_spline(scale, ...)

define_spline_survival(scale, ...)
```

## Arguments

* `scale`: "hazard", "odds", or "normal", as described
in flexsurvspline. With the default of no knots in
addition to the boundaries, these models reduce to the
Weibull, log-logistic and log-normal respectively. The
scale must be common to all times.
* `...`: parameters and knot log times of spline distribution,
which can be provided either in order starting with spline
parameters followed by knot log times, or by names (e.g
gamma1, gamma2, ... gammaN, knots1, knots2, ... knotsN). See
examples below for named and unnamed calls.

## References

Royston, P. and Parmar, M. (2002).  Flexible parametric
proportional-hazards and proportional-odds models for censored survival
data, with application to prognostic modelling and estimation of treatment
effects. Statistics in Medicine 21(1):2175-2197.

## Value

a `surv_spline` object.

## Examples

```r
define_surv_spline(
 scale = 'hazard',
 -2.08, 2.75, 0.23, # parameters
 -1.62, 0.57, 1.191 # knot times
)
```

# `event_prob`: Evaluate Event Probabilities

## Description

Generate the conditional probability of an even during an
interval of time.

## Usage

```r
event_prob(x, start, end, ...)
```

## Arguments

* `x`: A `surv_dist` object
* `start`: A numeric vector of interval start times
* `end`: A numeric vector of interval end times
* `...`: additional arguments passed to methods

## Examples

```r
dist1 <- define_surv_param('exp', rate = 0.12)
surv_prob(dist1, c(0, 1, 2, 3))
```

# `extract_strata`: Extract Product-Limit Tables

## Description

Extracts the product-limit table from a survfit object
for all strata. Only `survfit` and unstratified
`survfit.coxph` objects are supported.

## Usage

```r
extract_strata(sf)
```

## Arguments

* `sf`: A survit object.

## Keyword

internal

## Value

A tidy data.frame of the product-limit tables for
all strata.

# `extract_stratum`: Extract Product-Limit Table for Stratum

## Description

Extracts the product-limit table from a survfit object
for a given stratum. Only `[survival::survfit()](https://rdrr.io/pkg/survival/man/survfit.html)` and
unstratified `[survival::survfit.coxph()](https://rdrr.io/pkg/survival/man/survfit.coxph.html)` objects are
supported.

## Usage

```r
extract_stratum(sf, index)
```

## Arguments

* `sf`: A survit object.
* `index`: The index number of the strata to extract.

## Keyword

internal

## Value

A data frame of the product-limit table for the
given stratum.

# `join`: Join Distributions

## Description

Join two or more distributions together at the specified cut points.

## Usage

```r
join(dist1, cut1, dist2, ...)
```

## Arguments

* `dist1`: survival distribution to use from time `0` to `cut`
* `cut1`: cut point between `dist1` and `dist2`
* `dist2`: survival distribution to use from `cut`
* `...`: Additional cutpoints and distributions

## Value

A `surv_join` object

## Examples

```r
dist1 <- define_survival(distribution = "exp", rate = 0.05)
dist2 <- define_survival(distribution = "gompertz", rate = .5, shape = 1)
dist3 <- define_survival(distribution = "exp", rate = 0.25)
join_dist <- join(dist1, 20, dist2)
join_dist2 <- join(dist1, 20, dist2, 50, dist3)
```

# `mix`: Mix Two or More Survival Distributions

## Description

Mix a set of survival distributions using the specified
weights.

## Usage

```r
mix(dist1, weight1, dist2, weight2, ...)
```

## Arguments

* `dist1`: first survival distribution to mix
* `weight1`: probability weight for first distribution
* `dist2`: second survival distribution to mix
* `weight2`: probability weight for second distribution
* `...`: additional distributions and weights

## Value

A `surv_mix` object.

## Examples

```r
dist1 <- define_surv_param("exp", rate = .5)
dist2 <- define_surv_param("gompertz", rate = .5, shape = 1)
pooled_dist <- mix(dist1, 0.25, dist2, 0.75)
```

# `set_covariates`: Set Covariates of a Survival Model

## Description

Generate a survival distribution representing model predictions
for a specified cohort. The cohort can be defined by providing a
data frame of covariate values (for multiple subjects) or by providing
covariate values as named arguments (for a single subject).

## Usage

```r
set_covariates(dist, data, ...)
```

## Arguments

* `dist`: a survfit or flexsurvreg object
* `data`: a data.frame representing subjets for which predictions
will be generated
* `...`: optional argument representing covariate values to generate
predictions for, can be used instead of data argument

## Value

a `surv_model` object

## Examples

```r
library(flexsurv)
fs1 <- flexsurvreg(
  Surv(rectime, censrec)~group,
  data=flexsurv::bc,
  dist = "llogis"
)
good_model <- set_covariates(fs1, group = "Good")
cohort <- data.frame(group=c("Good", "Good", "Medium", "Poor"))
mixed_model <- set_covariates(fs1, data = cohort)
```

# `surv_prob`: Evaluate Survival Probabilities

## Description

Generate survival probabilities for a survival distribution
at the specified times.

## Usage

```r
surv_prob(x, time, ...)

eval_surv(x, time, ...)

compute_surv(x, time, cycle_length = 1, type = "prob", ...)
```

## Arguments

* `x`: A `surv_dist` object
* `time`: A numeric vector of times
* `...`: additional arguments passed to methods

## Value

A numeric vector of survival probabilities

## Examples

```r
dist1 <- define_surv_param('exp', rate = 0.12)
surv_prob(dist1, c(0, 1, 2, 3))
```

