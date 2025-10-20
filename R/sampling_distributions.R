
#' Normal distribution
#'
#' The `normal` function can be used to sample a normally-distributed
#' parameter.
#'
#' @author Antoine Filipovic-Pierucci
#'
#' @param mean Mean of parameter
#' @param sd Standard deviation of parameter
#'
#' @export
normal <- function(mean, sd) {
  function(x) qnorm(p = x, mean = mean, sd = sd)
}

#' Lognormal distribution
#'
#' The `lognormal` function can be used to sample a parameter
#' whose natural log is distributed normally.  Lognormal distribution
#' can be specified by providing the mean and standard deviation of the
#' parameter either on the natural or log scale by supplying the
#' appropriate arguments.
#'
#' @author Antoine Filipovic-Pierucci
#'
#' @param mean Mean of parameter
#' @param sd Standard deviation of parameter
#' @param meanlog Mean of `log(parameter)`
#' @param sdlog Standard deviation of `log(parameter)`
#'
#' @export
lognormal <- function(mean, sd, meanlog, sdlog) {
  if (missing(sdlog)) sdlog <- sqrt(log(1 + sd^2/mean^2))
  if (missing(meanlog)) meanlog <- log(mean) - sdlog^2/2

  function(x) qlnorm(p = x, meanlog = meanlog, sdlog = sdlog)
}

#' Bootstrap sampling of a data frame
#'
#' The `bootstrap` function can be used to handle sampling
#' of tables of individual patient data by selecting observations
#' from the table with replacement
#'
#' @param x The data frame to be sampled
#' @param id An optional column name representing the unique identifier. If
#' not specified, each row is assumed to represent a unique observation.
#' @param strata An optional character vector of column names representing the
#' strata within which resampling should be performed.
#' @param weight An optional column name representing the probability weight for each
#' observation.
#'
#' @export
bootstrap <- function(x, id = NULL, strata = NULL, weight = NULL) {
  if (is.null(id)) {
    x$.id <- seq_len(nrow(x))
    id <- ".id"
  }
  function(q) {
    n <- length(q)
    resampled_df <- group_by(x, !!strata) %>%
      do({
        
        # Get the set of unique observations
        unique <- distinct(x, !!sym(id))
        n_unique <- nrow(unique)
        
        # Handle the weights if provided
        if (is.null(weight)) {
          prob <- NULL
        } else {
          prob <- unique[[weight]]
        }
        
        # Resample data frame
        sampled_indices <- sample(seq_len(n_unique), n * n_unique, replace = T, prob = prob)
        sampled_df <- slice(unique, sampled_indices) %>%
          mutate(.sim = rep(seq_len(n), each = n_unique)) %>%
          select(!!c(".sim", id)) %>%
          left_join(x, by = id, relationship = 'many-to-many')
        
        sampled_df
      }) %>%
      ungroup()
    
    sim_index <- resampled_df$.sim
    select(resampled_df, -.sim) %>%
      split(sim_index)
  }
}
