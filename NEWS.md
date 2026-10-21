# openqaly (development version)

## Values, half-cycle correction and discounting at time points

Half-cycle correction is applied to the values calculated from the trace, not
to the trace. Formulas are still evaluated per cycle: in cycle k the lag time
variables (`cycle_lag`, `year_lag`, ...) are the start of the cycle and the
others (`cycle`, `year`, ...) its end, so cycle 1 has `cycle_lag = 0` and
`cycle = 1`. Cycle k's value prices time point k (its end); time point 0, the
start of cycle 1, takes cycle 1's value. At each time point t = 0..n:

- residency values: X(t) = occupancy(t) x value(t)
- transitional values: X(t) = arrivals at t (during cycle t) x value(t), with X(0) = 0

Point values are allocated to cycle k as:

- `"start"`: X(k-1)
- `"end"`: X(k)
- `"life-table"`: (X(k-1) + X(k)) / 2

Each time point has one price, so the half removed at a time point and the half
added back there always cancel.

Discounting uses the same rule: each time point is discounted at its own time,
d(t) = 1 / (1 + r)^(t x cycle length in years + decision tree offset), floored
to whole years under `discount_method = "by_year"`, and the discounted point
values are allocated to cycles in the same way.

- `"end"` gives the same undiscounted results as before for every value.
- Constant values give the same undiscounted results as before under every
  method.
- Under `"start"` and `"life-table"`, cycle k uses time point k - 1, priced
  with cycle k - 1's value (cycle 1's value at time point 0): values that vary
  over time move one cycle later under `"start"`.
- An explicit transitional value into a state gives exactly the same per-cycle
  result as `value * (state_cycle == 1)` on that state, under every method.
  Under `"start"` the last cycle's arrivals fall outside the horizon, and under
  `"life-table"` half of them.
- PSM transitional values are priced on the decline in progression-free and
  overall survival observed at each time point.
- Model start values are charged in cycle 1 without correction and discounted
  at time point 0. Decision tree values are not discounted.
- `"start"` and `"end"` discounting matches the former `discount_timing` of
  the same name; `"life-table"` discounts each half at its own time point.

## `state_cycle_limit`

A limit of L now tracks state cycles 1..L exactly, plus one final tunnel with
`state_cycle = L + 1` that holds everyone with more than L cycles in the
state. Previously the last tunnel was labelled L, so a formula such as
`cost * (state_cycle <= L)` never stopped charging. Formulas should compare
`state_cycle` with values up to L; tables looked up by `state_cycle` need a row
for L + 1.

### Removed

- The `discount_timing` setting. When a cycle's amount is discounted is now
  set by the half-cycle method.
- The half-cycle-corrected trace outputs (`corrected_collapsed_trace`,
  `corrected_expanded_trace`, the engine's `correctedTrace`).

### Changed

- The default `half_cycle_method` is now `"life-table"` (was `"start"`). Models
  that do not set it give different results; set `half_cycle_method = "start"`
  to keep the old behaviour.
- `calculate_discount_factors()` returns n + 1 factors (time points 0..n) and
  no longer has a `discount_timing` argument.

### New

- Values can set their own `half_cycle_method` (`add_value(...,
  half_cycle_method = "end")`), overriding the model setting for that value.
  All rows of a value share one setting: a new row takes the value's setting,
  `edit_value(..., half_cycle_method = )` changes every row, and renaming a row
  into an existing value adopts that value's setting.

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
