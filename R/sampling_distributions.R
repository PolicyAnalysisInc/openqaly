
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

#' Gamma distribution
#'
#' The `gamma` function can be used to sample a gamma-distributed
#' parameter, commonly used for costs or positive continuous parameters.
#' Can be specified using either (mean, sd) or (shape, scale) parameterization.
#'
#' @param mean Mean of parameter (alternative parameterization)
#' @param sd Standard deviation of parameter (alternative parameterization)
#' @param shape Shape parameter (traditional parameterization)
#' @param scale Scale parameter (traditional parameterization)
#'
#' @export
gamma <- function(mean = NULL, sd = NULL, shape = NULL, scale = NULL) {
  # Convert mean/sd to shape/scale if provided
  if (!is.null(mean) && !is.null(sd)) {
    shape <- (mean / sd)^2
    scale <- sd^2 / mean
  }

  if (is.null(shape) || is.null(scale)) {
    stop("Must provide either (mean, sd) or (shape, scale)")
  }

  function(x) qgamma(p = x, shape = shape, scale = scale)
}

#' Beta distribution
#'
#' The `beta` function can be used to sample a beta-distributed
#' parameter, commonly used for probabilities bounded between 0 and 1.
#' Can be specified using either (mean, sd) or (shape1, shape2) parameterization.
#'
#' @param mean Mean of parameter (alternative parameterization)
#' @param sd Standard deviation of parameter (alternative parameterization)
#' @param shape1 Alpha parameter (traditional parameterization)
#' @param shape2 Beta parameter (traditional parameterization)
#'
#' @export
beta <- function(mean = NULL, sd = NULL, shape1 = NULL, shape2 = NULL) {
  # Convert mean/sd to shape1/shape2 if provided
  if (!is.null(mean) && !is.null(sd)) {
    if (mean <= 0 || mean >= 1) {
      stop("mean must be in (0, 1) for beta distribution")
    }
    alpha_plus_beta <- mean * (1 - mean) / sd^2 - 1
    if (alpha_plus_beta <= 0) {
      stop("Invalid sd for given mean (sd too large relative to mean)")
    }
    shape1 <- mean * alpha_plus_beta
    shape2 <- (1 - mean) * alpha_plus_beta
  }

  if (is.null(shape1) || is.null(shape2)) {
    stop("Must provide either (mean, sd) or (shape1, shape2)")
  }

  function(x) qbeta(p = x, shape1 = shape1, shape2 = shape2)
}

#' Uniform distribution
#'
#' The `uniform` function can be used to sample a uniformly-distributed
#' parameter between specified bounds.
#'
#' @param min Minimum value
#' @param max Maximum value
#'
#' @export
uniform <- function(min, max) {
  if (min >= max) {
    stop("min must be less than max")
  }

  function(x) qunif(p = x, min = min, max = max)
}

#' Triangular distribution
#'
#' The `triangular` function can be used to sample a triangular-distributed
#' parameter, useful when only min, mode, and max are known (e.g., from expert opinion).
#'
#' @param min Minimum value
#' @param mode Mode (peak) value
#' @param max Maximum value
#'
#' @export
triangular <- function(min, mode, max) {
  if (min >= max) {
    stop("min must be less than max")
  }
  if (mode < min || mode > max) {
    stop("mode must be between min and max")
  }

  function(x) {
    fc <- (mode - min) / (max - min)
    ifelse(x < fc,
           min + sqrt(x * (max - min) * (mode - min)),
           max - sqrt((1 - x) * (max - min) * (max - mode)))
  }
}

#' Dirichlet distribution
#'
#' The `dirichlet` function can be used to sample probability vectors that sum to 1,
#' commonly used for transition probabilities from a single state to multiple destination states.
#' This is a multivariate distribution - it generates multiple correlated probabilities.
#'
#' @param alpha Concentration parameter vector (length k for k variables).
#'   Higher values indicate more certainty around that probability.
#'
#' @return Function that takes n (number of samples) and returns n × k matrix where each row sums to 1
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # For 3 transition probabilities with different concentrations
#' dist_fn <- dirichlet(c(3, 2, 1))
#' samples <- dist_fn(1000)  # 1000 × 3 matrix, each row sums to 1
#' }
dirichlet <- function(alpha) {
  if (!is.numeric(alpha) || any(alpha <= 0)) {
    stop("alpha must be a numeric vector with all positive values")
  }

  function(n) {
    k <- length(alpha)
    gamma_samples <- matrix(nrow = n, ncol = k)
    for (i in 1:k) {
      gamma_samples[, i] <- rgamma(n, shape = alpha[i], scale = 1)
    }
    # Normalize each row to sum to 1
    gamma_samples / rowSums(gamma_samples)
  }
}

#' Multivariate Normal distribution
#'
#' The `mvnormal` function can be used to sample correlated normal random variables.
#' Useful when parameters are known to be correlated (e.g., cost and effectiveness).
#'
#' @param mean Mean vector (length k for k variables)
#' @param sd Standard deviation vector (for correlation parameterization)
#' @param cor Correlation (scalar for bivariate, matrix for multivariate)
#' @param cov Covariance matrix (alternative to sd/cor)
#'
#' @return Function that takes n (number of samples) and returns n × k matrix
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Bivariate with correlation
#' dist_fn <- mvnormal(mean = c(0.5, 1000), sd = c(0.1, 100), cor = 0.5)
#' samples <- dist_fn(1000)  # 1000 × 2 matrix
#'
#' # With full covariance matrix
#' cov_mat <- matrix(c(0.01, 5, 5, 10000), nrow = 2)
#' dist_fn <- mvnormal(mean = c(0.5, 1000), cov = cov_mat)
#' }
mvnormal <- function(mean, sd = NULL, cor = NULL, cov = NULL) {
  if (!is.numeric(mean)) {
    stop("mean must be a numeric vector")
  }

  # Construct covariance matrix
  if (!is.null(cov)) {
    cov_matrix <- as.matrix(cov)
    if (nrow(cov_matrix) != length(mean) || ncol(cov_matrix) != length(mean)) {
      stop("cov matrix dimensions must match length of mean")
    }
  } else if (!is.null(sd) && !is.null(cor)) {
    if (length(sd) != length(mean)) {
      stop("sd vector must have same length as mean")
    }

    if (length(mean) == 2 && length(cor) == 1) {
      # Bivariate shorthand: scalar correlation
      cov_matrix <- matrix(c(
        sd[1]^2, cor * sd[1] * sd[2],
        cor * sd[1] * sd[2], sd[2]^2
      ), nrow = 2)
    } else {
      # General case: cor is correlation matrix
      cor <- as.matrix(cor)
      if (nrow(cor) != length(mean) || ncol(cor) != length(mean)) {
        stop("cor matrix dimensions must match length of mean")
      }
      # Convert correlation matrix and sd vector to covariance
      D <- diag(sd)
      cov_matrix <- D %*% cor %*% D
    }
  } else {
    stop("Must provide either 'cov' or both 'sd' and 'cor'")
  }

  function(n) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      stop("MASS package required for multivariate normal sampling")
    }
    MASS::mvrnorm(n = n, mu = mean, Sigma = cov_matrix)
  }
}

#' Multinomial distribution
#'
#' The `multinomial` function can be used to sample categorical outcomes.
#' Each sample is a vector of counts for each category.
#' Commonly used with size=1 to sample a single category (one-hot encoded).
#'
#' @param size Number of trials (usually 1 for categorical sampling)
#' @param prob Probability vector (must sum to 1)
#'
#' @return Function that takes n (number of samples) and returns n × k matrix
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Sample initial health state (one of 3 states)
#' dist_fn <- multinomial(size = 1, prob = c(0.2, 0.3, 0.5))
#' samples <- dist_fn(1000)  # 1000 × 3 matrix of 0s and 1s
#' }
multinomial <- function(size, prob) {
  if (!is.numeric(prob) || any(prob < 0) || any(prob > 1)) {
    stop("prob must be a numeric vector with values in [0, 1]")
  }
  if (abs(sum(prob) - 1) > 1e-10) {
    stop("prob must sum to 1")
  }
  if (!is.numeric(size) || size < 1) {
    stop("size must be a positive integer")
  }

  function(n) {
    k <- length(prob)
    result <- matrix(nrow = n, ncol = k)
    for (i in 1:n) {
      result[i, ] <- rmultinom(1, size = size, prob = prob)
    }
    result
  }
}
