# openqaly 1.0.0

## Half-cycle correction and discounting

The half-cycle correction is applied to occupancy, as in 0.1.3. Cycle k (from
time k-1 to time k) is priced with cycle k's value:

| Method | Residency, cycle k | Transitional, cycle k | Discounted at |
|---|---|---|---|
| `"start"` (default) | occupancy(k-1) x value(k) | flow(k) x value(k) | d(k-1) |
| `"end"` | occupancy(k) x value(k) | flow(k) x value(k) | d(k) |
| `"life-table"` | average of the two | flow(k) x value(k) | each half at its own end |

- The half-cycle method now also sets when values are discounted, because both
  describe when a value accrues: at the start of the cycle, at its end, or
  spread over it. Under `"life-table"`, cycle k's amount is
  value(k) x [occupancy(k-1) x d(k-1) + occupancy(k) x d(k)] / 2, and a
  transitional value is discounted at [d(k-1) + d(k)] / 2.
- d(t) = 1 / (1 + r)^(t x cycle length in years + decision tree offset),
  floored to whole years under `discount_method = "by_year"`.
- Transitional values are charged in the cycle the transitions occur under
  every method; the method only sets when they are discounted.
- Undiscounted results are unchanged from 0.1.3 for Markov and custom PSM
  models. Discounted results are unchanged where 0.1.3's `discount_timing`
  matched the half-cycle method (`"start"` with `"start"`, `"end"` with
  `"end"`), except that model start values are now discounted at time 0 under
  `"end"`. Models that combined `"life-table"` with `discount_timing =
  "start"` (or `"midpoint"`) give different discounted results.
- Model start values are charged in cycle 1 at time 0. Decision tree values
  are not discounted.
- The corrected traces (`corrected_collapsed_trace`,
  `corrected_expanded_trace`, the engine's `correctedTrace`) use the model's
  half-cycle method.

### New

- Values can set their own `half_cycle_method` (`add_value(...,
  half_cycle_method = "end")`), overriding the model setting for that value's
  occupancy and discounting. All rows of a value share one setting: a new row
  takes the value's setting, `edit_value(..., half_cycle_method = )` changes
  every row, and renaming a row into an existing value adopts that value's
  setting.
- One-off amounts written as residency values can use this: a one-off cost
  written as `ifelse(cycle == 1, cost, 0)` is charged in full with `"start"`,
  and an entry cost written as `cost * (state_cycle == 1)` equals the
  transitional value on entry with `"end"`.

### Removed

- The `discount_timing` setting. When a value is discounted is now set by its
  half-cycle method.
- `apply_discounting()`, which was no longer used.
- The `strategies` argument of `nmb_density_plot()`, `nmb_ridgeline_plot()`
  and `pairwise_psa_scatter_plot()`. These plots compare strategies, so they
  take `interventions` and/or `comparators`; every call using `strategies`
  failed.

### Changed

- `calculate_discount_factors()` returns n + 1 factors (time points 0..n) and
  no longer has a `discount_timing` argument.

## `state_cycle_limit`

A limit of L now tracks state cycles 1..L exactly, plus one final tunnel with
`state_cycle = L + 1` that holds everyone with more than L cycles in the
state. Previously the last tunnel was labelled L, so a formula such as
`cost * (state_cycle <= L)` never stopped charging. Formulas should compare
`state_cycle` with values up to L; tables looked up by `state_cycle` need a row
for L + 1.

## `discounting_override`

The formula is evaluated with:

- model variables and time variables, one value per cycle (model time only);
- `discount_rate`;
- `discount_factors`: the value's discount factors d(t);
- `discount_time`: the time in years of each time point, including any
  decision tree offset, before `by_year` flooring;
- `trace("state")`: person-time at each time point, i.e. occupancy times the
  weight the value's half-cycle method gives that time point (start 1..1, 0;
  end 0, 1..1; life-table 1/2, 1..1, 1/2). `sum(trace("s"))` is the undiscounted
  person-cycles in state s, and
  `sum(discount_factors * trace("s")) / sum(trace("s"))` is the average
  discount factor of time spent in s (e.g. for a one-time cost on progression).

The result must have length 1 (a multiplier for the value's undiscounted cycle
totals) or n + 1 (discount factors for time points 0..n, used in place of d(t),
e.g. a declining discount rate written with `discount_time`). Standalone
decision trees accept length 1 only.

## Fixes

- `calculate_incremental_ce()`: of two equally costly strategies the less
  effective one is strictly dominated, and extended dominance is resolved by
  walking the frontier, so chains of extendedly dominated strategies are all
  removed and ICERs are against the right comparator.
- `calculate_nmb()` works with display names (the default), and accepts
  strategies by technical or display name.
- Incremental and pairwise CEAC tables put every column under its own header
  when there are several groups.
- `incremental_ceac_plot()` with `strategies` no longer returns an empty plot.
- PSA functions stop with an error for an unknown summary name instead of
  returning results based on zeros.
- DSA tornado plots with several facets label each facet's parameters
  correctly.
- `drop_zero_impact = TRUE` drops parameters with no impact.
- TWSA: ICERs in the south-west quadrant are negative and marked with an
  asterisk, tables show every strategy and group, VBP equations are calculated
  for every group, and each axis shows its own parameter's display name.
- `set_settings()` rejects an invalid `half_cycle_method` or
  `discount_method`, and `n_cycles` always sets the timeframe unit to cycles.
- `discount_method` can be used as a scenario and TWSA setting.
- `model_to_r_code()` generates the decision tree nodes and keeps DSA range
  labels.
- Clearer errors for a blank transition formula, a missing custom PSM state
  probability, a missing group weight, an override's minimum not below its
  maximum, and a non-numeric value without formula text.
- PSM transitional values are charged in the cycle the events occur (the
  decline in progression-free or overall survival during cycle k) under every
  half-cycle method. Previously each cycle's events were charged in the next
  cycle, so cycle 1 was charged nothing and the last cycle's events were lost.
- A value with the same name as a variable is rejected with a "Name collision
  detected" error, instead of crashing R in the dependency sort.
- PSM endpoints accept any survival distribution openqalysurv can evaluate,
  including raw `flexsurvreg` fits.
- A PSM whose survival curve is missing (NA) before the model horizon (e.g. a
  survival table that ends early) stops with an error naming the endpoint and
  the first missing cycle.
- PSM transitional values on a state pair other than PFS -> post-progression or
  post-progression -> dead are an error instead of silently 0.
- States that never use state time no longer expand the namespace to
  n_cycles x n_cycles rows, so a blank `state_cycle_limit` no longer makes
  memory grow quadratically with the number of cycles.
- A `state_cycle_limit` longer than the model no longer crashes R.
- A Markov model with a decision tree and no variables failed while parsing
  variables.
- The bundled `checkimab_simple` model referenced a strategy and groups it does
  not define.
