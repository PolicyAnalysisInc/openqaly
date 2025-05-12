#' Convert a longform table to a multi-dimensional array
lf_to_arr <- function(df, dimcols, value) {
  n_dims <- length(dimcols)
  index <- integer(nrow(df))
  lengths <- integer(n_dims)
  uniques <- vector(mode = 'list', length = n_dims)
  factors <- vector(mode = 'list', length = n_dims)
  for (i in seq_len(n_dims)) {
    if (class(df[[dimcols[i]]]) == 'factor') {
      factors[[i]] <- df[[dimcols[[i]]]]
      lengths[[i]] <- length(levels(df[[dimcols[[i]]]]))
    } else {
      uniques[[i]] <- unique(df[[dimcols[i]]])
      lengths[i] <- length(uniques[[i]])
      factors[[i]] <- as.numeric(factor(df[[dimcols[i]]], levels = uniques[[i]]))
    }
  }
  for (i in seq_len(n_dims)) {
    if (i == 1) {
      index <- index + as.numeric(factors[[1]])
    } else {
      prev_indices <- seq_len(i - 1)
      multiplier <- prod(lengths[prev_indices])
      index <- index + (as.numeric(factors[[i]]) - 1) * multiplier
    }
  }
  vec <- numeric(prod(lengths))
  vec[index] <- df[[value]]
  array(vec, dim = lengths)
}

#' Determine the last index containing a unique value
arr_last_unique <- function(mat, dim_index) {
  dims <- dim(mat)
  n_dim <- length(dims)
  st_dim_l <- dims[dim_index]
  access_str <- paste0(
    'mat[',
    strrep(',', dim_index - 1),
    st_dim_l,
    strrep(',', n_dim - dim_index),
    ']'
  )
  last_mat <- eval(parse(text = access_str))
  diff_from_last <- apply(mat, dim_index, function(x) {
    any(x != last_mat)
  })

  if (!any(diff_from_last)) return(1)
  max(which(diff_from_last)) + 1
}
