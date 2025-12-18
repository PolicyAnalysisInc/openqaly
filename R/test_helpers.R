create_test_ns <- function(segment) {
  create_namespace(
    list(
      settings = list(
        timeframe = 1,
        timeframe_unit = "Years",
        cycle_length = 1,
        cycle_length_unit = "Months",
        days_per_year = 365
      ),
      states = tibble(
        name = c("A","B","C"),
        state_cycle_limit = 12,
        state_cycle_limit_unit = "Months"
      ),
      env = new.env(parent = parent.frame())
    ),
    segment
  )
}
