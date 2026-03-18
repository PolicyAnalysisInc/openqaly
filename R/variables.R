# Parses a variables input table and optional trees table
# and produces an object of class uneval_variables
parse_seg_variables <- function(x, segment = NULL, trees = NULL,
                                formula_column = 'formula',
                                context = 'Variables') {

  # Check that all necessary columns are present
  missing_cols <- check_missing_colnames(x, c(vars_def_columns, formula_column, segment_vars))
  if (length(missing_cols) > 0) {
    missing_msg <- err_name_string(missing_cols)
    stop(context, ' definition was missing columns: ', missing_msg, '.', call. = F)
  }
  
  # Filter to only variables in this segment
  df <- x %>%
    mutate_all(~as.character(.)) %>%
    filter(
      is_in_segment(segment, strat = .data$strategy, grp = .data$group),
      !.data$name %in% names(segment)
    ) %>%
    mutate(
      .is_ss = !(is.na(.data$strategy) | .data$strategy == '' | is.null(.data$strategy)),
      .is_gs = !(is.na(.data$group) | .data$group == ''  | is.null(.data$group))
    )
  
  # Check that variables definition is valid
  check_variables_df(df, context = context)
  
  # Parse decision tree variables
  tree_vars <- parse_tree_vars(trees) #20ms
  
  # Parse formulas, combine with trees, and sort
  parse_variables(df, formula_column, context, tree_vars) #40ms
}

parse_variables <- function(x, formula_column = 'formula', context = 'Variables', extras = NULL) {

  # Check that all necessary columns are present
  missing_cols <- check_missing_colnames(x, c(vars_def_columns, formula_column))
  if (length(missing_cols) > 0) {
    missing_msg <- err_name_string(missing_cols)
    stop(context, ' definition was missing columns: ', missing_msg, '.', call. = F)
  }

  # Handle empty variables case
  if (nrow(x) == 0 && (is.null(extras) || nrow(extras) == 0)) {
    empty_vars <- tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      formula = list()
    )
    return(as.variables(empty_vars))
  }

  # Parse formulas, and sort
  vars <- x %>%
    rowwise() %>%
    group_split() %>%
    map(function(var) {
      formula <- as.oq_formula(var[[formula_column]])
      if (('.is_ss' %in% colnames(var)) && var$.is_ss) {
        formula$depends <- c(formula$depends, 'strategy')
        formula$fo_depends <- c(formula$fo_depends, 'strategy')
      }
      if (('.is_gs' %in% colnames(var)) && var$.is_gs) {
        formula$depends <- c(formula$depends, 'group')
        formula$fo_depends <- c(formula$fo_depends, 'group')
      }
      var$formula <- list(formula)
      var
    }) %>%
    bind_rows() %>%
    select("name", "display_name", "description", !!enquo(formula_column)) %>%
    rbind(extras) %>%
    {try({sort_variables(.)}, silent = TRUE)}
  
  if (class(vars)[1] == 'try-error') {
    stop(
      context,
      ' definition contained circular references in variables: ',
      strsplit(gsub('\n','', vars), ': ')[[1]][3],
      '.',
      call. = F
    )
  }
  
  # Construct Object & Return
  as.variables(vars)
}

# Sort a dataframe of variables based on dependency tree
sort_variables <- function(x, extra_vars = NULL) {

  # Deal with extra vars if given
  if (is.null(extra_vars)) {
    extras <- list()
  } else {
    extras <- set_names(extra_vars$formula, extra_vars$name)
  }

  par_names <- x$name

  # Make a named list of all variables (needed for tree dep extraction)
  all_vars <- c(
    set_names(x$formula, x$name),
    extras,
    c('strategy', 'group')
  )

  # Pre-extract sort deps (depends + after, filtered to par_names)
  dep_lists <- lapply(x$formula, function(y) {
    vars <- c(y$depends, y$after)
    vars[vars %in% par_names]
  })

  # Pre-extract first-order deps including tree p() call deps
  fo_dep_lists <- lapply(seq_len(nrow(x)), function(i) {
    formula <- x$formula[[i]]

    # Extract any decision tree probability calls
    p_calls <- extract_func_calls(quo_get_expr(formula$quo), 'p')
    tree_deps <- unlist(lapply(p_calls, function(y) {
      referenced_nodes <- all_vars[[y$arg2]]$node_depends %>%
        keep(~any(.$tags %in% y$arg1)) %>%
        map(~.$depends) %>%
        flatten_chr()
      referenced_nodes
    }), use.names = FALSE)
    if (is.null(tree_deps)) tree_deps <- character()

    unique(c(formula$depends, tree_deps))
  })

  # Extra vars deps
  extra_names <- if (length(extras) > 0) names(extras) else character()
  extra_dep_lists <- lapply(extras, function(y) {
    if (is.null(y$depends)) character() else y$depends
  })
  if (length(extra_dep_lists) == 0) extra_dep_lists <- list()

  # Call C++
  result <- cppSortVariables(
    names = par_names,
    dep_lists = dep_lists,
    fo_dep_lists = fo_dep_lists,
    extra_names = extra_names,
    extra_dep_lists = extra_dep_lists
  )

  # Reorder dataframe
  sorted_x <- x[result$order, ]

  # Write back expanded deps
  for (i in seq_len(nrow(sorted_x))) {
    sorted_x$formula[[i]]$depends <- result$expanded_deps[[i]]
  }

  as_tibble(sorted_x)
}

# Evaluate a variables object
eval_variables <- function(x, ns, df_only = FALSE, context = 'variables') {
  
  # Iterate over each parameter and its name
  walk2(x$name, x$formula, function(name, value) {
    
    # Evaluate it
    res <- eval_formula(value, ns)
    # Check if the object was an error
    if (is_oq_error(res)) {
      # Always accumulate errors for checkpoint handling
      accumulate_oq_error(res, context_msg = glue("Evaluation of {context} '{name}'"))
    }
    
    # Determine whether result is a vector or object parameter
    vector_type <- is.vector(res) && !is.list(res)
    if (df_only || (vector_type && (length(res) == nrow(ns$df)))) {
      # If a vector parameter, assign to data frame
      ns$df[name] <<- res 
    } else {
      # If an object parameter, assign to environment
      assign(name, res, envir = ns$env)
    }
    
  })

  # Trigger error checkpoint mechanism (checks mode internally)
  oq_error_checkpoint()
  
  return(ns)
}

# Checks that a variables definition table is valid
check_variables_df <- function(x, context = "Variables") {
  
  error_msg = ''
  
  # Check that there are no duplicate names
  dupe <- duplicated(x$name)
  if (any(dupe)) {
    dupe_names <- unique(x$name[dupe])
    dupe_msg <- err_name_string(dupe_names)
    error_msg <- glue("{context} definition contained duplicate names for variables: {dupe_msg}.")
  }
  
  # Check that variable names are valid
  invalid <- !is_valid_name(x$name)
  if (any(invalid)) {
    invalid_names <- x$name[invalid]
    invalid_name_msg <- err_name_string(invalid_names)
    error_msg <- paste0(
      context,
      ' definition contained invalid names for variables: ',
      invalid_name_msg,
      '. Names must start with a letter and contain only letters, numbers, and underscores.'
    )
  }
  
  # Check that no reserved names are used
  used_reserved <- x$name %in% oq_keywords
  if (any(used_reserved)) {
    reserved_index <- which(used_reserved)
    reserved_names <- x$name[reserved_index]
    reserved_msg <- err_name_string(reserved_names)
    error_msg <- paste0(
      context,
      ' definition contained names reserved for keywords for variables: ',
      reserved_msg,
      '.'
    )
  }
  
  if (error_msg != '') stop(error_msg, call. = F)
}

# Type coercion methods
as.variables <- function(x) {
  UseMethod('as.variables', x)
}
as.variables.variables <- function(x) {
  x
}
#' @export
as.variables.data.frame <- function(x) {
  class(x) <- c('variables', class(x))
  x
}
