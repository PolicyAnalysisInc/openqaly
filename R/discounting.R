#' Calculate Discount Factors
#'
#' Calculate discount factors for each cycle using the formula 1/(1+r)^t,
#' where t is the time in years from the start of the model.
#' The first cycle (t=0) has a discount factor of 1 (no discounting).
#'
#' @param n_cycles Number of cycles
#' @param discount_rate Annual discount rate (e.g., 0.035 for 3.5\%)
#' @param cycle_length_years Length of each cycle in years (e.g., 1/12 for monthly, 1/52 for weekly)
#'
#' @return Vector of discount factors for each cycle
#' @export
calculate_discount_factors <- function(n_cycles, discount_rate, cycle_length_years = 1) {
  if (n_cycles <= 0) {
    return(numeric(0))
  }

  # Calculate time in years for each cycle (start of cycle timing)
  # Cycle 1 is at time 0, Cycle 2 is at time cycle_length_years, etc.
  cycles <- seq_len(n_cycles)
  time_years <- (cycles - 1) * cycle_length_years

  # Calculate discount factors based on time in years
  1 / (1 + discount_rate)^time_years
}

#' Apply Discounting to Values Matrix
#'
#' Applies discount factors to a values matrix based on value types.
#' Values identified as costs use the cost discount rate, while
#' outcomes use the outcomes discount rate.
#'
#' @param values_matrix Matrix or data frame of undiscounted values
#' @param discount_factors_cost Vector of discount factors for costs
#' @param discount_factors_outcomes Vector of discount factors for outcomes
#' @param types Named vector indicating type ("cost" or "outcome") for each value
#'
#' @return Matrix or data frame with discounted values
#' @export
apply_discounting <- function(values_matrix,
                             discount_factors_cost,
                             discount_factors_outcomes,
                             types) {

  # Handle case where values_matrix is a data frame
  if (is.data.frame(values_matrix)) {
    values_matrix <- as.matrix(values_matrix)
    was_df <- TRUE
  } else {
    was_df <- FALSE
  }

  # Get dimensions
  n_cycles <- nrow(values_matrix)
  value_names <- colnames(values_matrix)

  # Create result matrix
  discounted_matrix <- values_matrix

  # Apply discounting column by column based on value type
  for (i in seq_along(value_names)) {
    value_name <- value_names[i]

    # Determine which discount factors to use
    if (!is.null(types) && value_name %in% names(types)) {
      type <- types[value_name]
    } else {
      # Default to outcome if type not specified
      type <- "outcome"
    }

    # Apply appropriate discount factors
    if (type == "cost") {
      discount_factors <- discount_factors_cost
    } else {
      discount_factors <- discount_factors_outcomes
    }

    # Ensure discount factors match the number of cycles
    if (length(discount_factors) >= n_cycles) {
      discounted_matrix[, i] <- values_matrix[, i] * discount_factors[seq_len(n_cycles)]
    } else {
      warning(paste("Discount factors length", length(discount_factors),
                   "is less than number of cycles", n_cycles))
      # Use available factors and pad with 1s
      factors_to_use <- c(discount_factors, rep(1, n_cycles - length(discount_factors)))
      discounted_matrix[, i] <- values_matrix[, i] * factors_to_use
    }
  }

  # Convert back to data frame if input was data frame
  if (was_df) {
    discounted_matrix <- as.data.frame(discounted_matrix)
  }

  discounted_matrix
}