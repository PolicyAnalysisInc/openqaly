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
      is_in_segment(segment, strat = strategy, grp = group),
      !name %in% names(segment)
    ) %>%
    mutate(
      .is_ss = !(is.na(strategy) | strategy == '' | is.null(strategy)),
      .is_gs = !(is.na(group) | group == ''  | is.null(group))
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
    empty_vars <- tibble::tibble(
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
      formula <- as.heRoFormula(var[[formula_column]])
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
    select(name, display_name, description, !!enquo(formula_column)) %>%
    rbind(extras) %>%
    {try({sort_variables(.)}, silent = T)}
  
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
# CONVERT_TO_CPP
sort_variables <- function(x, extra_vars = NULL) {

  # Deal with extra vars if given
  if (is.null(extra_vars)) {
    extras <- list()
  } else {
    extras <- set_names(extra_vars$formula, extra_vars$name)
  }
  
  # Extract variable names
  par_names <- x$name
  
  # Extract the variable names referenced in each variable's
  # formula
  var_list <-  purrr::map(x$formula, function(y) {
      vars <- c(y$depends, y$after)
      vars[vars %in% par_names]
    }) %>%
    set_names(x$name)
  
  # Define the lists of ordered and unordered variables
  ordered <- c()
  unordered <- var_list
  
  # While we still have variables in the unordered list...
  while (length(unordered) > 0) {
    
    # Define a vector which will hold the indices of each
    # variable to be moved to the ordered list
    to_remove <- c()
    
    # Loop through each unordered variable
    for (i in seq_len(length(unordered))) {
      
      # If all the variables its formula references are in the ordered
      # list then it can be added to ordered list.
      if (all(unordered[[i]] %in% ordered)) {
        
        # Append it to the list of ordered variables
        ordered <- c(ordered, names(unordered)[i])
        
        # Get current variable
        current_var <-  names(unordered)[i]
        
        # Make a named list of all variables
        all_vars <- c(
          set_names(x$formula, x$name),
          extras,
          c('strategy', 'group')
        )
        
        # Extract any decision tree probability calls
        p_calls <- extract_func_calls(quo_get_expr(x$formula[[which(x$name == current_var)]]$quo), 'p')
        tree_deps <- map(p_calls, function(y) {
          referenced_nodes <- all_vars[[y$arg2]]$node_depends %>%
            keep(~any(.$tags %in% y$arg1)) %>%
            map(~.$depends) %>%
            flatten_chr()
          
          referenced_nodes
        }) %>% flatten_chr()
        
        # Assemble first-order depencies
        fo_deps <- unique(c(x$formula[[which(x$name == current_var)]]$depends, tree_deps))
        
        # Add second+ order dependencies to variable
        x$formula[[which(x$name == current_var)]]$depends <- x$formula %>%
          set_names(x$name) %>%
          c(extras) %>%
          .[fo_deps] %>%
          lapply(function(y) y$depends) %>%
          discard(is.null) %>%
          flatten_chr(.) %>%
          union(fo_deps)
        
        # and mark it  for removal
        to_remove <- c(to_remove, i)
      }
    }
    
    if (length(to_remove) == 0) {
      # If we didn't find anything to move to the ordered list,
      # throw a circular reference error
      err_txt <- err_name_string(names(unordered))
      stop('Circular reference detected: ', err_txt)
    } else {
      # Otherwise, remove from the unordered list the variables
      # that were appended to the ordered list
      unordered <- unordered[-to_remove]
    }
  }
  
  # Return variables in sorted order
  as_tibble(x[order(factor(x$name, levels = ordered)), ])
}

# Evaluate a variables object
eval_variables <- function(x, ns, df_only = FALSE, context = 'variables') {
  
  # Iterate over each parameter and its name
  walk2(x$name, x$formula, function(name, value) {
    
    # Evaluate it
    res <- eval_formula(value, ns)
    # Check if the object was an error
    if (is_hero_error(res)) {
      # Always accumulate errors for checkpoint handling
      accumulate_hero_error(res, context_msg = glue("Evaluation of {context} '{name}'"))
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
  hero_error_checkpoint()
  
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
    error_msg <- paste0(context, ' definition contained duplicate names for variables: ', dupe_msg, '.')
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
  used_reserved <- x$name %in% heRo_keywords
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
as.variables.data.frame <- function(x) {
  class(x) <- c('variables', class(x))
  x
}
