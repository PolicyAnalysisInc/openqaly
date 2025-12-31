create_namespace <- function(model, segment) {
  
  # Calculate cycle lengths and times in days/weeks/months/years
  # for each model cycle. 
  cl_vars <- cycle_length_variables(model$settings)
  time_vars <- time_variables(model$settings, model$states)
  unit_vars <- time_unit_variables(model$settings)

  # Create a "namespace" which will contain evaluated
  # variables so that they can be referenced.
  ns <- define_namespace(model$env, time_vars, cl_vars, unit_vars) %>%
    update_segment_ns(segment)
  
  ns
}

#' Define a Namespace Object
#'
#' A namespace object stores evaluated parameters by combining
#' a data frame for vector parameters with an environment
#' for non-vector parameters.
#'
#' @param df A data frame of pre-existing parameter values
#' @param env An environment of pre-existing values
#'
#' @export
define_namespace <- function(env, df, additional = NULL, ...) {
  # Merge dataframes if additional ones are provided
  extra_dfs <- list(...)
  if (length(extra_dfs) > 0) {
    # Bind all dataframes column-wise
    all_dfs <- list(df, additional)
    all_dfs <- c(all_dfs, extra_dfs)
    all_dfs <- all_dfs[!sapply(all_dfs, is.null)]
    
    # Check if all are dataframes
    df_mask <- sapply(all_dfs, is.data.frame)
    df_list <- all_dfs[df_mask]
    non_df_list <- all_dfs[!df_mask]
    
    # Merge dataframes by columns
    if (length(df_list) > 1) {
      df <- do.call(cbind, df_list)
    } else if (length(df_list) == 1) {
      df <- df_list[[1]]
    }
    
    # Handle non-dataframe additional items
    additional <- if (length(non_df_list) > 0) non_df_list[[1]] else NULL
  } else if (!is.null(additional) && is.data.frame(additional)) {
    # If additional is a dataframe, merge it with df
    df <- cbind(df, additional)
    additional <- NULL
  }
  
  ns <- list(df = df, env = env_clone(env))
  if (!is.null(additional)) {
    for (nm in names(additional)) {
      assign(nm, additional[[nm]], envir = ns$env)
    }
  }
  class(ns) <- 'namespace'
  ns
}

get_names <- function(ns, type = "all", keywords = T) {

  # Pull out names defined in ns
  if (type == "all") {
    res <- c(colnames(ns$df), ls(ns$env))
  } else if (type == "df") {
    res <- colnames(ns$df)
  } else if (type == "env") {
    res <- ls(ns$env)
  } else {
    stop("Invalid value for argument 'type'")
  }

  # Exclude keywords if requested
  if (!keywords) {
    res <- setdiff(res, oq_keywords)
  }

  res
}
#' Clone a Namespace
#' 
#' Clones a namespace object.
#' 
#' @param x The namespace to be cloned.
#' 
#' @return An identical copy of the namespace.
#' 
#' @keywords internal
clone_namespace <- function(x) {
  new <- x
  new$env <- env_clone(x$env)
  new
}

#' @export
summary.namespace <- function(object, ...) {

  # Extract names from namespaces
  env_names <- get_names(object, "env", keywords = F)
  df_names <- get_names(object, "df", keywords = F)
  
  if (length(df_names) > 0) {
    res_df <- pivot_longer(
        object$df,
        names_to = 'name',
        values_to = 'value',
        all_of(df_names)
      ) %>%
      mutate(
        print = NA,
        summary = NA
      ) %>%
      select(
        name,
        cycle,
        state_cycle,
        value,
        print,
        summary
      )
  } else {
    res_df <- data.frame()
  }
  res_env <- tibble(
    name = env_names,
    cycle = NA,
    state_cycle = NA,
    value = NA,
    summary = NA,
    print = NA
  )
  for (i in seq_len(length(env_names))) {
    name <- env_names[i]
    export_res <- export(get(name, envir = object$env))
    res_env$print[i] <- if (!is.null(export_res$print)) {
      export_res$print
    } else {
      NA_character_
    }
    res_env$summary[i] <- if (!is.null(export_res$summary)) {
      export_res$summary
    } else {
      NA_character_
    }
  }

  res_env <- select(
    res_env,
    name,
    cycle,
    state_cycle,
    value,
    print,
    summary
  )
  res <- rbind(res_df, res_env)
  res
}

#' Export a Variable
#' 
#' Generates output representing the evaluated value for a given variable. This
#' is done by outputing its class, and the result of its print and summary
#' methods.
#' 
#' @param x The object being exported.
#' 
#' @return a list containing the class of the result, and the output of its
#' print and summary methods.
#' 
#' @export
export <- function(x) {
  UseMethod("export", x)
}


#' @export
export.default <- function(x) {
  res <- list()
  res$class <- tryCatch({
    class(x)
  }, error = function(e) {
    NULL
  })
  res$print <- tryCatch({
    paste(capture.output(x, split = F), collapse = "\n")
  }, error = function(e) {
    NULL
  })
  res$summary <- tryCatch({
    paste(capture.output(summary(x)), collapse = "\n")
  }, error = function(e) {
    NULL
  })
  res
}

update_segment_ns <- function(x, newdata) {
  
  # Clone the namespace
  new_ns <- clone_namespace(x)
  
  # Clear overlapping column names from dataframe
  names_to_clear <- intersect(colnames(newdata), colnames(new_ns$df))
  new_ns$df <- select(new_ns$df, !all_of(names_to_clear))
  
  # Store new data in environment
  iwalk(newdata, function(x, n) {
    assign(n, x[[1]], envir = new_ns$env)
  })
  
  # Return namespace
  new_ns
  
}

#' @export
`[.namespace` <- function(x, i, ...) {
  df_names <- get_names(x, 'df')
  if (i %in% df_names) return(x$df[[i]])
  
  env_names <- get_names(x, 'env')
  if (i %in% env_names) return(get(i, envir = x$env))
  
  stop('Object not found')
}
