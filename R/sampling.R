#' Resample Model Parameters
#' 
#' Resamples the parameters for a model based on the sampling specifications.
#' 
#' @param model A heRomodel object
#' @param n the number of simulations
#' @param segments the segments for which resampling will be done
#' @param corr an optional correlation matrix
#' 
#' @return a data.frame with resampled data by segment
#' 
#' @export
resample <- function(model, n, segments, corr = NULL, seed = NULL) {
  
  check_sampling_spec(model$variables)
  
  sampled_vars <- model$variables %>%
    filter(!is.na(sampling), sampling != '') %>%
    mutate(.index = seq_len(n()))
  n_var <- nrow(sampled_vars)
  var_names <- unique(sampled_vars$name)
  
  # Assume independence if missing correlation matrix
  if (is.null(corr)) {
    corr <- diag(rep(1, n_var))
  }
  
  set.seed(seed)
  r_norm <- rmvn(n = n, mu = rep(0, n_var), sigma = corr)
  mat_p <- pnorm(r_norm)
  
  # Prepopulate a list to store simulations
  cols <- vector(mode = "list", length = n_var + 1)
  cols[[1]] <- seq_len(n)
  
  # Fill list with sampled values
  for (i in seq_len(n_var)) {
    
    var_row <- sampled_vars[i, ]
    var_name <- var_row$name
    row_strat <- var_row$strategy
    row_group <-  var_row$group
    is_ss <- !is.na(row_strat) && row_strat != ''
    is_gs <- !is.na(row_group) && row_group != ''
    if (!is_ss) row_strat <- segments$strategy[1]
    if (!is_gs) row_group <- segments$group[1]
    
    seg <- filter(segments, group == row_group, strategy == row_strat)
    seg_ns <- clone_namespace(seg$eval_vars[[1]])
    seg_vars <- seg$uneval_vars[[1]]
    
    # Warn about strategy or group dependence
    deps <- seg_vars$formula[[which(seg_vars$name == var_name)]]$depends
    fo_deps <- seg_vars$formula[[which(seg_vars$name == var_name)]]$fo_depends
    if (('strategy' %in% deps) && !('strategy' %in% fo_deps )) {
      msg <- paste0(
        'Variable ',
        err_name_string(var_name),
        ' references a strategy-dependent variable but was assigned a sampling distribution.',
        ' Sampling variables that reference strategy-dependent variables is not reccomended',
        ' and will result in a loss of strategy-dependence in PSA.'
      )
      warning(msg, call. = F)
    } else if (('group' %in% deps) && !('group' %in% fo_deps )) {
      msg <- paste0(
        'Variable ',
        err_name_string(var_name),
        ' references a group-dependent variable but was assigned a sampling distribution.',
        ' Sampling variables that reference group-dependent variables is not reccomended',
        ' and will result in a loss of group-dependence in PSA.'
      )
      warning(msg, call. = F)
    }
    
    # Set up the parameter
    formula <- as.heRoFormula(var_row$sampling)
    seg_ns$env$bc <- seg_ns[var_name]
    
    # Evaluate distribution function
    dist_func <- eval_formula(formula, seg_ns)
    
    # Check if value was an evaluation error
    if (is_hero_error(dist_func)) {
      msg <- paste0(
        'Error in evaluation of sampling distribution for parameter ',
        err_name_string(var_row$name),
        ": ",
        dist_func
      )
      warning(msg, call. = F)
      res <- as.numeric(NA)
    } else {
      
      # Try to draw from distribution function
      res <- safe_eval(dist_func(mat_p[ ,i]))
      
      # Check whether drawing samples resulted in error
      if (is_hero_error(res)) {
        msg <- paste0(
          'Error in evaluation of sampling distribution for parameter ',
          err_name_string(var_row$name),
          ":",
          res
        )
        warning(msg, call. = F)
        res <- as.numeric(NA)
      }
    }
    
    # Assign result to column
    cols[[i + 1]] <- res
  }
  
  # Make a data.frame with sampled variables by segment
  seg_samples <- segments %>%
    rowwise() %>%
    group_split() %>%
    map(function(x) {
      indices <- sampled_vars %>%
        filter(is_in_segment(x, grp = group, strat = strategy)) %>%
        .$.index
      samp_list <- cols[c(1, indices + 1)] %>%
        setNames(c('simulation', var_names)) %>%
        append(list(strategy = x$strategy, group = x$group), after = 0)
      vars_df <- do.call(tibble, samp_list)
      vars_df
    }) %>%
    bind_rows()
  
  seg_samples
}

check_sampling_spec <- function(x) {
  
  # Check that it is a data.frame
  is_df <- 'data.frame' %in% class(x)
  if (!is_df) {
    msg <- paste0(
      'Error in variables specification, specification was of class "',
      class(x)[1],
      '" rather than "data.frame".'
    )
    stop(msg, call. = F)
  }
  
  # Check that sampling column is present
  missing_col <- check_missing_colnames(x, 'sampling')
  if (length(missing_col) > 0) {
    stop(
      'Error in variables specification, "sampling" column was missing.',
      call. = F
    )
  }
  
  # Check that at least one variable is sampled
  none_sampled <- all(is.empty(x$sampling))
  if (none_sampled) {
    stop(
      'Error in variables specification, no sampling distributions were specified.',
      call. = F
    )
  }
  
}
