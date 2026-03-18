#' Model Builder Functions
#'
#' Functions for building openqaly models programmatically using a fluent API
#' with non-standard evaluation (NSE) for formula expressions.
#'
#' @name model_builder
#' @importFrom rlang enquo expr_text
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble tibble as_tibble
NULL

#' Define a New Model
#'
#' Initialize a new openqaly model object with the specified type.
#'
#' @param type Character string specifying the model type ("markov", "psm", or "custom_psm")
#'
#' @return An oq_model_builder object that can be piped to other builder functions
#'
#' @export
#' @examples
#' model <- define_model("markov")
#' model <- define_model("psm")
#' model <- define_model("custom_psm")
define_model <- function(type = "markov") {
  type <- match.arg(tolower(type), c("markov", "psm", "custom_psm", "decision_tree"))

  # Type-specific state initialization
  # PSM and Custom PSM use same 3-column state structure
  # Decision tree models have no states
  states_init <- if (type == "decision_tree") {
    fast_tibble(
      name = character(0),
      display_name = character(0),
      description = character(0)
    )
  } else if (type %in% c("psm", "custom_psm")) {
    fast_tibble(
      name = character(0),
      display_name = character(0),
      description = character(0)
    )
  } else {
    fast_tibble(
      name = character(0),
      initial_probability = character(0),
      display_name = character(0),
      description = character(0),
      state_group = character(0),
      share_state_time = logical(0),
      state_cycle_limit = numeric(0),
      state_cycle_limit_unit = character(0)
    )
  }

  # Type-specific transitions initialization
  # Decision tree models have no transitions
  transitions_init <- if (type == "decision_tree") {
    fast_tibble(
      from_state = character(0),
      to_state = character(0),
      formula = character(0)
    )
  } else if (type == "psm") {
    fast_tibble(
      endpoint = character(0),
      time_unit = character(0),
      formula = character(0)
    )
  } else if (type == "custom_psm") {
    fast_tibble(
      state = character(0),
      formula = character(0)
    )
  } else {
    fast_tibble(
      from_state = character(0),
      to_state = character(0),
      formula = character(0)
    )
  }

  model <- list(
    settings = list(
      model_type = type,
      days_per_year = 365,
      half_cycle_method = "start",
      discount_cost = 0,
      discount_outcomes = 0,
      discount_timing = "start",
      discount_method = "by_cycle",
      reduce_state_cycle = FALSE
    ),
    states = states_init,
    transitions = transitions_init,
    values = fast_tibble(
      name = character(0),
      formula = character(0),
      state = character(0),
      destination = character(0),
      display_name = character(0),
      description = character(0),
      type = character(0)
    ),
    variables = fast_tibble(
      name = character(0),
      formula = character(0),
      display_name = character(0),
      description = character(0),
      strategy = character(0),
      group = character(0),
      source = character(0),
      sampling = character(0)
    ),
    strategies = fast_tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      enabled = numeric(0)
    ),
    groups = fast_tibble(
      name = character(0),
      display_name = character(0),
      description = character(0),
      weight = character(0),
      enabled = numeric(0)
    ),
    summaries = fast_tibble(
      name = character(0),
      values = character(0),
      display_name = character(0),
      description = character(0),
      type = character(0),
      wtp = numeric(0)
    ),
    tables = list(),
    scripts = list(),
    trees = NULL,
    decision_tree = NULL,
    multivariate_sampling = list(),
    dsa_parameters = structure(list(), class = "dsa_parameters"),
    scenarios = list(),
    twsa_analyses = list(),
    override_categories = list(),
    threshold_analyses = list(),
    vbp = NULL,
    psa = NULL,
    documentation = NULL
  )

  class(model) <- c("oq_model_builder", "oq_model")
  model
}

#' Set Model Settings
#'
#' Configure model settings such as number of cycles, cycle length, discount rates, etc.
#'
#' @param model An oq_model_builder object
#' @param ... Named arguments for settings (e.g., n_cycles = 100)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   set_settings(n_cycles = 100, cycle_length = "year")
set_settings <- function(model, ...) {
  dots <- list(...)

  # PREVENT model_type changes
  if ("model_type" %in% names(dots)) {
    stop("model_type cannot be changed after model creation. It was set to '",
         model$settings$model_type, "' in define_model().")
  }

  # Convert setting names to match expected format
  setting_map <- c(
    n_cycles = "timeframe",
    timeframe = "timeframe",
    timeframe_unit = "timeframe_unit",
    cycle_length_unit = "cycle_length_unit",
    cycle_length = "cycle_length",
    discount_cost = "discount_cost",
    discount_outcomes = "discount_outcomes",
    half_cycle_method = "half_cycle_method",
    discount_timing = "discount_timing",
    discount_method = "discount_method",
    reduce_state_cycle = "reduce_state_cycle",
    days_per_year = "days_per_year",
    country = "country",
    number_country = "number_country"
  )

  # Apply settings
  for (name in names(dots)) {
    setting_name <- if (name %in% names(setting_map)) setting_map[name] else name
    model$settings[[setting_name]] <- dots[[name]]
  }

  # If n_cycles was used and timeframe_unit is not set, default to "cycles"
  if ("n_cycles" %in% names(dots) && is.null(model$settings$timeframe_unit)) {
    model$settings$timeframe_unit <- "cycles"
  }

  # Apply defaults for settings not explicitly provided
  if (is.null(model$settings$days_per_year)) {
    model$settings$days_per_year <- 365
  }
  if (is.null(model$settings$half_cycle_method)) {
    model$settings$half_cycle_method <- "start"
  }
  if (is.null(model$settings$discount_cost)) {
    model$settings$discount_cost <- 0
  }
  if (is.null(model$settings$discount_outcomes)) {
    model$settings$discount_outcomes <- 0
  }

  model
}

#' Set VBP Configuration
#'
#' Configure value-based pricing (VBP) parameters on the model so they can be
#' serialized and used as defaults by \code{run_vbp()}, \code{run_dsa()},
#' \code{run_scenario()}, and \code{run_twsa()}.
#'
#' @param model An oq_model_builder object
#' @param price_variable Name of the variable representing the intervention's price
#' @param intervention_strategy Name of the intervention strategy
#' @param outcome_summary Name of the outcome summary to use
#' @param cost_summary Name of the cost summary to use
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   set_vbp(
#'     price_variable = "c_drug",
#'     intervention_strategy = "treatment",
#'     outcome_summary = "total_qalys",
#'     cost_summary = "total_costs"
#'   )
set_vbp <- function(model, price_variable, intervention_strategy,
                    outcome_summary, cost_summary) {
  # Validate all params are non-empty strings
  for (param_name in c("price_variable", "intervention_strategy",
                        "outcome_summary", "cost_summary")) {
    val <- get(param_name)
    if (!is.character(val) || length(val) != 1 || nchar(val) == 0) {
      stop(param_name, " must be a non-empty string", call. = FALSE)
    }
  }

  model$vbp <- list(
    price_variable = price_variable,
    intervention_strategy = intervention_strategy,
    outcome_summary = outcome_summary,
    cost_summary = cost_summary
  )

  model
}

#' Set PSA Configuration
#'
#' Configure probabilistic sensitivity analysis (PSA) parameters on the model
#' so they can be serialized and used as defaults by \code{run_psa()}.
#'
#' @param model An oq_model_builder object
#' @param n_sim Number of PSA simulations to run (positive integer)
#' @param seed Random seed for reproducibility (NULL or single numeric value)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   set_psa(n_sim = 1000, seed = 42)
set_psa <- function(model, n_sim, seed = NULL) {
  # Validate n_sim
  if (!is.numeric(n_sim) || length(n_sim) != 1 || is.na(n_sim) ||
      n_sim < 1 || n_sim != as.integer(n_sim)) {
    stop("n_sim must be a positive integer", call. = FALSE)
  }

  # Validate seed
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed)) {
      stop("seed must be NULL or a single numeric value", call. = FALSE)
    }
  }

  model$psa <- list(
    n_sim = as.integer(n_sim),
    seed = seed
  )

  model
}

#' Set Model Documentation
#'
#' Set a markdown documentation string on the model describing its purpose,
#' assumptions, and other relevant details.
#'
#' @param model An oq_model_builder object
#' @param text A character string containing the documentation (typically markdown)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov")
#' model <- set_documentation(model, "# My Model\n\nA cost-effectiveness model.")
set_documentation <- function(model, text) {
  if (!is.character(text) || length(text) != 1) {
    stop("text must be a single character string", call. = FALSE)
  }
  model$documentation <- text
  model
}

#' Add a Tree Node
#'
#' Add a node to a decision tree in the model. Creates the trees tibble if it
#' doesn't exist yet.
#'
#' @param model An oq_model_builder object
#' @param tree_name Character string for the tree name
#' @param node Character string for the node name
#' @param parent Character string for the parent node name (NA for root)
#' @param formula An unquoted R expression for the node probability
#' @param tags Optional character string for node tags
#'
#' @return The modified model object
#'
#' @export
add_tree_node <- function(model, tree_name, node, parent = NA, formula, tags = NA) {
  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_row <- fast_tibble(
    name = tree_name,
    node = node,
    parent = if (is.na(parent)) NA_character_ else as.character(parent),
    formula = formula_str,
    tags = if (is.na(tags)) NA_character_ else as.character(tags)
  )

  if (is.null(model$trees)) {
    model$trees <- new_row
  } else {
    model$trees <- bind_rows(model$trees, new_row)
  }

  validate_tree_name_collisions(tree_name, model)

  model
}

#' Set Decision Tree Configuration
#'
#' Configure a decision tree to run before the main model. The tree produces
#' one-time payoffs and a duration that offsets downstream discounting.
#'
#' @param model An oq_model_builder object
#' @param tree_name Character string referencing a tree in model$trees
#' @param duration An unquoted R expression for the duration (can reference variables for PSA)
#' @param duration_unit Character string for the time unit ("days", "weeks", "months", "years")
#'
#' @return The modified model object
#'
#' @export
set_decision_tree <- function(model, tree_name, duration, duration_unit = "days") {
  # Validate duration_unit
  valid_units <- c("days", "weeks", "months", "years")
  duration_unit <- match.arg(tolower(duration_unit), valid_units)

  # Capture the duration expression using NSE
  duration_quo <- enquo(duration)
  duration_expr <- quo_get_expr(duration_quo)
  duration_str <- expr_text(duration_expr)
  if (is.character(duration_expr) && length(duration_expr) == 1) {
    duration_str <- duration_expr
  }

  model$decision_tree <- list(
    tree_name = tree_name,
    duration = duration_str,
    duration_unit = duration_unit
  )

  model
}

#' Remove Decision Tree Configuration
#'
#' Remove the decision tree configuration from the model.
#'
#' @param model An oq_model_builder object
#'
#' @return The modified model object
#'
#' @export
remove_decision_tree <- function(model) {
  model$decision_tree <- NULL
  model
}

#' Edit a Decision Tree Node
#'
#' Edit an existing tree node identified by tree_name and node.
#'
#' @param model An oq_model_builder object
#' @param tree_name Character string for the tree name
#' @param node Character string for the node name
#' @param formula An unquoted R expression for the node probability
#' @param tags Optional character string for node tags
#' @param parent Character string for the parent node name
#' @param new_node_name Character string to rename the node
#' @param new_tree_name Character string to rename the entire tree
#'
#' @return The modified model object
#'
#' @export
edit_tree_node <- function(model, tree_name, node, formula, tags, parent,
                           new_node_name, new_tree_name) {
  if (is.null(model$trees)) {
    stop("No trees exist in this model.", call. = FALSE)
  }

  match_idx <- which(model$trees$name == tree_name & model$trees$node == node)
  if (length(match_idx) == 0) {
    stop(sprintf('Tree node "%s" in tree "%s" not found in model.', node, tree_name),
         call. = FALSE)
  }

  if (!missing(formula)) {
    formula_quo <- enquo(formula)
    formula_expr <- quo_get_expr(formula_quo)
    formula_str <- expr_text(formula_expr)
    if (is.character(formula_expr) && length(formula_expr) == 1) {
      formula_str <- formula_expr
    }
    model$trees$formula[match_idx] <- formula_str
  }

  if (!missing(tags)) {
    model$trees$tags[match_idx] <- if (is.na(tags)) NA_character_ else as.character(tags)
  }

  if (!missing(parent)) {
    model$trees$parent[match_idx] <- if (is.na(parent)) NA_character_ else as.character(parent)
  }

  if (!missing(new_node_name)) {
    conflict <- which(model$trees$name == tree_name & model$trees$node == new_node_name)
    if (length(conflict) > 0) {
      stop(sprintf('Node "%s" already exists in tree "%s".', new_node_name, tree_name),
           call. = FALSE)
    }
    # Cascade: update parent references for children in same tree
    child_mask <- model$trees$name == tree_name & model$trees$parent %in% node
    model$trees$parent[child_mask] <- new_node_name
    model$trees$node[match_idx] <- new_node_name
  }

  if (!missing(new_tree_name)) {
    validate_tree_name_collisions(new_tree_name, model)
    tree_mask <- model$trees$name == tree_name
    model$trees$name[tree_mask] <- new_tree_name
    if (!is.null(model$decision_tree) && identical(model$decision_tree$tree_name, tree_name)) {
      model$decision_tree$tree_name <- new_tree_name
    }
  }

  model
}

#' Remove a Decision Tree Node
#'
#' Remove a tree node and all its descendants from the model.
#'
#' @param model An oq_model_builder object
#' @param tree_name Character string for the tree name
#' @param node Character string for the node name
#'
#' @return The modified model object
#'
#' @export
remove_tree_node <- function(model, tree_name, node) {
  if (is.null(model$trees)) {
    stop("No trees exist in this model.", call. = FALSE)
  }

  match_idx <- which(model$trees$name == tree_name & model$trees$node == node)
  if (length(match_idx) == 0) {
    stop(sprintf('Tree node "%s" in tree "%s" not found in model.', node, tree_name),
         call. = FALSE)
  }

  # BFS to find all descendants
  nodes_to_remove <- node
  queue <- node
  while (length(queue) > 0) {
    children <- model$trees$node[model$trees$name == tree_name & model$trees$parent %in% queue]
    queue <- setdiff(children, nodes_to_remove)
    nodes_to_remove <- c(nodes_to_remove, queue)
  }

  model$trees <- model$trees[!(model$trees$name == tree_name & model$trees$node %in% nodes_to_remove), ]

  # If no nodes remain for this tree and it's the config tree, clear config
  remaining_in_tree <- sum(model$trees$name == tree_name)
  if (remaining_in_tree == 0 && !is.null(model$decision_tree) &&
      identical(model$decision_tree$tree_name, tree_name)) {
    model$decision_tree <- NULL
  }

  # If no trees remain at all, set to NULL
  if (nrow(model$trees) == 0) {
    model$trees <- NULL
  }

  model
}

#' Remove an Entire Decision Tree
#'
#' Remove all nodes of a tree from the model.
#'
#' @param model An oq_model_builder object
#' @param tree_name Character string for the tree name
#'
#' @return The modified model object
#'
#' @export
remove_tree <- function(model, tree_name) {
  if (is.null(model$trees) || !any(model$trees$name == tree_name)) {
    stop(sprintf('Tree "%s" not found in model.', tree_name), call. = FALSE)
  }

  model$trees <- model$trees[model$trees$name != tree_name, ]

  if (!is.null(model$decision_tree) && identical(model$decision_tree$tree_name, tree_name)) {
    model$decision_tree <- NULL
  }

  if (nrow(model$trees) == 0) {
    model$trees <- NULL
  }

  model
}

#' Edit Decision Tree Configuration
#'
#' Edit the decision tree configuration on the model.
#'
#' @param model An oq_model_builder object
#' @param tree_name Character string referencing a tree in model$trees
#' @param duration An unquoted R expression for the duration
#' @param duration_unit Character string for the time unit
#'
#' @return The modified model object
#'
#' @export
edit_decision_tree <- function(model, tree_name, duration, duration_unit) {
  if (is.null(model$decision_tree)) {
    stop("No decision tree configuration exists. Use set_decision_tree() first.",
         call. = FALSE)
  }

  if (!missing(tree_name)) {
    model$decision_tree$tree_name <- tree_name
  }

  if (!missing(duration)) {
    duration_quo <- enquo(duration)
    duration_expr <- quo_get_expr(duration_quo)
    duration_str <- expr_text(duration_expr)
    if (is.character(duration_expr) && length(duration_expr) == 1) {
      duration_str <- duration_expr
    }
    model$decision_tree$duration <- duration_str
  }

  if (!missing(duration_unit)) {
    valid_units <- c("days", "weeks", "months", "years")
    duration_unit <- match.arg(tolower(duration_unit), valid_units)
    model$decision_tree$duration_unit <- duration_unit
  }

  model
}

#' Add States to Model
#'
#' Add one or more states to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the state name
#' @param display_name Optional display name for the state
#' @param description Optional description of the state
#' @param state_group Optional state group for grouping related states
#' @param share_state_time Logical indicating whether to share state time with other states in the group
#' @param state_cycle_limit Optional limit on the number of cycles in this state
#' @param state_cycle_limit_unit Unit for the cycle limit (default: "cycles")
#' @param initial_prob Optional initial probability for this state
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_state("healthy", initial_prob = 1) |>
#'   add_state("sick", initial_prob = 0)
add_state <- function(model, name, display_name = NULL,
                     description = NULL, state_group = NULL,
                     share_state_time = FALSE, state_cycle_limit = NULL,
                     state_cycle_limit_unit = "cycles", initial_prob = NULL) {

  # Validate state name
  if (missing(name) || !is.character(name) || length(name) != 1 || is.na(name) || trimws(name) == "") {
    stop("State name must be a non-empty character string.", call. = FALSE)
  }

  # Block reserved state names
  if (name %in% c("All", "All Other")) {
    stop(glue("'{name}' is a reserved state name and cannot be used as a state name. ",
              "It is used for special value targeting in add_value()."), call. = FALSE)
  }

  # Get immutable model type
  model_type <- tolower(model$settings$model_type)

  # Decision tree models don't have states
  if (model_type == "decision_tree") {
    stop("Decision tree models do not support states. Use add_value() with state = 'decision_tree' instead.",
         call. = FALSE)
  }

  # Check for name collision with trees
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    if (name %in% unique(model$trees$name)) {
      stop(sprintf(
        'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
        name
      ), call. = FALSE)
    }
  }

  # Check for duplicate state name
  if (!is.null(model$states) && is.data.frame(model$states) && nrow(model$states) > 0) {
    if (name %in% model$states$name) {
      stop(sprintf("A state named '%s' already exists. State names must be unique.", name), call. = FALSE)
    }
  }

  if (model_type %in% c("psm", "custom_psm")) {
    # PSM and Custom PSM: Reject Markov-specific parameters
    if (!is.null(initial_prob)) {
      stop("PSM/Custom PSM models don't use initial_prob parameter. Remove it from add_state() call.")
    }
    if (!is.null(state_group)) {
      stop("PSM/Custom PSM models don't use state_group parameter. Remove it from add_state() call.")
    }
    if (share_state_time != FALSE) {
      stop("PSM/Custom PSM models don't use share_state_time parameter. Remove it from add_state() call.")
    }
    if (!is.null(state_cycle_limit)) {
      stop("PSM/Custom PSM models don't use state_cycle_limit parameter. Remove it from add_state() call.")
    }

    # Create PSM/Custom PSM state (3 columns only)
    new_state <- fast_tibble(
      name = name,
      display_name = display_name %||% name,
      description = description %||% display_name %||% name
    )
  } else {
    # Markov: Require initial_prob
    if (is.null(initial_prob)) {
      stop("initial_prob is required for Markov models. Specify it in add_state() call.")
    }

    # Create Markov state (8 columns)
    new_state <- fast_tibble(
      name = name,
      initial_probability = as.character(initial_prob),
      display_name = display_name %||% name,
      description = description %||% display_name %||% name,
      state_group = state_group,
      share_state_time = share_state_time,
      state_cycle_limit = state_cycle_limit %||% Inf,
      state_cycle_limit_unit = state_cycle_limit_unit
    )
  }

  model$states <- bind_rows(model$states, new_state)

  return(model)
}

#' Edit a State in the Model
#'
#' Modify an existing state's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the state are updated (transitions, values,
#' and threshold analyses).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the state to edit
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param state_group Optional new state group (Markov only)
#' @param share_state_time Optional new share_state_time flag (Markov only)
#' @param state_cycle_limit Optional new cycle limit (Markov only)
#' @param state_cycle_limit_unit Optional new cycle limit unit (Markov only)
#' @param initial_prob Optional new initial probability (Markov only)
#' @param new_name Optional new name for the state. If provided, the state
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#'
#' @export
edit_state <- function(model, name, display_name, description, state_group,
                       share_state_time, state_cycle_limit,
                       state_cycle_limit_unit, initial_prob, new_name) {
  model_type <- tolower(model$settings$model_type)

  if (model_type == "decision_tree") {
    stop("Decision tree models do not support states.", call. = FALSE)
  }

  match_idx <- which(model$states$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('State "%s" not found in model.', name), call. = FALSE)
  }

  # Scalar field updates — common to all model types
  if (!missing(display_name)) {
    model$states$display_name[match_idx] <- display_name
  }
  if (!missing(description)) {
    model$states$description[match_idx] <- description
  }

  # Markov-only fields
  markov_fields_provided <- !missing(state_group) || !missing(share_state_time) ||
    !missing(state_cycle_limit) || !missing(state_cycle_limit_unit) || !missing(initial_prob)

  if (markov_fields_provided && model_type %in% c("psm", "custom_psm")) {
    stop("PSM/Custom PSM models don't support state_group, share_state_time, state_cycle_limit, state_cycle_limit_unit, or initial_prob parameters.",
         call. = FALSE)
  }

  if (!missing(state_group)) {
    model$states$state_group[match_idx] <- state_group
  }
  if (!missing(share_state_time)) {
    model$states$share_state_time[match_idx] <- share_state_time
  }
  if (!missing(state_cycle_limit)) {
    model$states$state_cycle_limit[match_idx] <- state_cycle_limit
  }
  if (!missing(state_cycle_limit_unit)) {
    model$states$state_cycle_limit_unit[match_idx] <- state_cycle_limit_unit
  }
  if (!missing(initial_prob)) {
    model$states$initial_probability[match_idx] <- as.character(initial_prob)
  }

  # Cascade rename
  if (!missing(new_name)) {
    if (new_name %in% c("All", "All Other")) {
      stop(sprintf('"%s" is a reserved state name and cannot be used.', new_name), call. = FALSE)
    }

    model$states$name[match_idx] <- new_name

    # Transitions — model-type-dependent
    if (is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
      if (model_type == "markov") {
        model$transitions$from_state[model$transitions$from_state %in% name] <- new_name
        model$transitions$to_state[model$transitions$to_state %in% name] <- new_name
      } else if (model_type == "custom_psm") {
        model$transitions$state[model$transitions$state %in% name] <- new_name
      }
      # PSM transitions use endpoint, not state — no rename needed
    }

    # Values
    if (is.data.frame(model$values) && nrow(model$values) > 0) {
      model$values$state[model$values$state %in% name] <- new_name
      model$values$destination[model$values$destination %in% name] <- new_name
    }

    # Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      for (i in seq_along(model$threshold_analyses)) {
        if (!is.null(model$threshold_analyses[[i]]$condition) &&
            identical(model$threshold_analyses[[i]]$condition$state, name)) {
          model$threshold_analyses[[i]]$condition$state <- new_name
        }
      }
    }
  }

  model
}

#' Remove a State from the Model
#'
#' Remove an existing state and optionally cascade-remove all downstream
#' references (transitions, values, and threshold analyses).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the state to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the state has
#'   downstream dependencies, an error of class \code{"state_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the state and all
#'   downstream references.
#'
#' @return The modified model object
#'
#' @export
remove_state <- function(model, name, error_on_dependencies = FALSE) {
  model_type <- tolower(model$settings$model_type)

  if (model_type == "decision_tree") {
    stop("Decision tree models do not support states.", call. = FALSE)
  }
  if (model_type == "psm") {
    stop("Removing states from PSM models is not supported.", call. = FALSE)
  }

  match_idx <- which(model$states$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('State "%s" not found in model.', name), call. = FALSE)
  }

  if (error_on_dependencies) {
    deps <- list()

    # Transitions — model-type-dependent
    if (is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
      if (model_type == "markov") {
        dep_rows <- model$transitions$from_state %in% name | model$transitions$to_state %in% name
        if (any(dep_rows)) {
          deps$transitions <- paste(model$transitions$from_state[dep_rows], "->",
                                    model$transitions$to_state[dep_rows])
        }
      } else if (model_type == "custom_psm") {
        dep_rows <- model$transitions$state %in% name
        if (any(dep_rows)) {
          deps$transitions <- model$transitions$state[dep_rows]
        }
      }
    }

    # Values
    if (is.data.frame(model$values) && nrow(model$values) > 0) {
      dep_vals <- model$values$state %in% name | model$values$destination %in% name
      if (any(dep_vals)) {
        deps$values <- unique(model$values$name[dep_vals])
      }
    }

    # Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      dep_thresh <- character(0)
      for (i in seq_along(model$threshold_analyses)) {
        if (!is.null(model$threshold_analyses[[i]]$condition) &&
            identical(model$threshold_analyses[[i]]$condition$state, name)) {
          dep_thresh <- c(dep_thresh, model$threshold_analyses[[i]]$name %||% paste0("threshold_", i))
        }
      }
      if (length(dep_thresh) > 0) deps$threshold_analyses <- dep_thresh
    }

    deps <- Filter(function(x) length(x) > 0, deps)

    if (length(deps) > 0) {
      cond <- structure(
        class = c("state_has_dependencies", "error", "condition"),
        list(
          message = sprintf('Cannot remove state "%s": it has downstream dependencies.', name),
          dependencies = deps
        )
      )
      stop(cond)
    }
  }

  # Remove the state row
  model$states <- model$states[-match_idx, , drop = FALSE]

  # Cascade remove downstream references

  # Transitions — model-type-dependent
  if (is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
    if (model_type == "markov") {
      keep <- !(model$transitions$from_state %in% name | model$transitions$to_state %in% name)
      model$transitions <- model$transitions[keep, , drop = FALSE]
    } else if (model_type == "custom_psm") {
      keep <- !(model$transitions$state %in% name)
      model$transitions <- model$transitions[keep, , drop = FALSE]
    }
  }

  # Values
  if (is.data.frame(model$values) && nrow(model$values) > 0) {
    keep <- !(model$values$state %in% name | model$values$destination %in% name)
    model$values <- model$values[keep, , drop = FALSE]
  }

  # Threshold analyses
  if (length(model$threshold_analyses) > 0) {
    keep <- vapply(model$threshold_analyses, function(a) {
      !((!is.null(a$condition)) && identical(a$condition$state, name))
    }, logical(1))
    model$threshold_analyses <- model$threshold_analyses[keep]
  }

  model
}

#' Add Transitions to Model
#'
#' Add one or more transitions to the model. Uses NSE to capture formula expressions.
#'
#' @param model A oq_model_builder object
#' @param from_state Character string specifying the source state
#' @param to_state Character string specifying the destination state
#' @param formula An unquoted R expression for the transition probability
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_transition("healthy", "sick", p_disease * 0.1) |>
#'   add_transition("sick", "dead", 0.2)
add_transition <- function(model, from_state, to_state, formula) {
  # Check model type for appropriate structure
  model_type <- tolower(model$settings$model_type %||% "markov")

  if (model_type == "decision_tree") {
    stop("Decision tree models do not support transitions.", call. = FALSE)
  }
  if (model_type == "psm") {
    stop("For PSM models, use add_psm_transition() instead")
  }
  if (model_type == "custom_psm") {
    stop("For Custom PSM models, use add_custom_psm_transition() instead")
  }

  # Validate from_state and to_state exist
  if (!is.null(model$states) && is.data.frame(model$states) && nrow(model$states) > 0) {
    available <- model$states$name
    if (!from_state %in% available) {
      stop(sprintf("Transition references undefined from_state '%s'. Available states: %s",
                   from_state, paste(available, collapse = ", ")), call. = FALSE)
    }
    if (!to_state %in% available) {
      stop(sprintf("Transition references undefined to_state '%s'. Available states: %s",
                   to_state, paste(available, collapse = ", ")), call. = FALSE)
    }
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_trans <- fast_tibble(
    from_state = from_state,
    to_state = to_state,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)

  return(model)
}

#' Add PSM Transitions to Model
#'
#' Add transitions for a PSM (Partitioned Survival Model).
#'
#' @param model A oq_model_builder object
#' @param endpoint Character string for the endpoint
#' @param time_unit Character string for the time unit
#' @param formula An unquoted R expression for the transition
#'
#' @return The modified model object
#'
#' @export
add_psm_transition <- function(model, endpoint, time_unit, formula) {
  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_trans <- fast_tibble(
    endpoint = endpoint,
    time_unit = time_unit,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)

  return(model)
}

#' Add Custom PSM Transition
#'
#' Add a state probability formula for a Custom PSM model. Each state must have
#' exactly one probability formula that defines the probability of being in that
#' state at each cycle.
#'
#' @param model An oq_model_builder object (must be type "custom_psm")
#' @param state Character string for the state name (must exist in model states)
#' @param formula An unquoted R expression for the state probability.
#'   Use `C` for complement (1 - sum of other states). Only one state can use `C`.
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("custom_psm") |>
#'   add_state("alive") |>
#'   add_state("progressed") |>
#'   add_state("dead") |>
#'   add_custom_psm_transition("alive", surv_prob(pfs_dist, month)) |>
#'   add_custom_psm_transition("progressed", surv_prob(os_dist, month) - surv_prob(pfs_dist, month)) |>
#'   add_custom_psm_transition("dead", C)
add_custom_psm_transition <- function(model, state, formula) {
  # Check model type
  is_custom_psm <- !is.null(model$settings$model_type) &&
    tolower(model$settings$model_type) == "custom_psm"

  if (!is_custom_psm) {
    stop("add_custom_psm_transition() is only for Custom PSM models. ",
         "Use add_transition() for Markov or add_psm_transition() for standard PSM.")
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  new_trans <- fast_tibble(
    state = state,
    formula = formula_str
  )

  model$transitions <- bind_rows(model$transitions, new_trans)

  return(model)
}

#' Edit a Markov Transition
#'
#' Edit the formula of an existing Markov transition.
#'
#' @param model An oq_model_builder object
#' @param from_state Character string specifying the source state
#' @param to_state Character string specifying the destination state
#' @param formula An unquoted R expression for the new transition probability
#'
#' @return The modified model object
#'
#' @export
edit_transition <- function(model, from_state, to_state, formula) {
  model_type <- tolower(model$settings$model_type %||% "markov")

  if (model_type == "decision_tree") {
    stop("Decision tree models do not support transitions.", call. = FALSE)
  }
  if (model_type == "psm") {
    stop("For PSM models, use edit_psm_transition() instead")
  }
  if (model_type == "custom_psm") {
    stop("For Custom PSM models, use edit_custom_psm_transition() instead")
  }

  match_idx <- which(model$transitions$from_state == from_state &
                       model$transitions$to_state == to_state)
  if (length(match_idx) == 0) {
    stop("No transition found from '", from_state, "' to '", to_state, "'", call. = FALSE)
  }

  formula_quo <- enquo(formula)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  model$transitions$formula[match_idx] <- formula_str

  return(model)
}

#' Remove a Markov Transition
#'
#' Remove an existing Markov transition.
#'
#' @param model An oq_model_builder object
#' @param from_state Character string specifying the source state
#' @param to_state Character string specifying the destination state
#'
#' @return The modified model object
#'
#' @export
remove_transition <- function(model, from_state, to_state) {
  model_type <- tolower(model$settings$model_type %||% "markov")

  if (model_type == "decision_tree") {
    stop("Decision tree models do not support transitions.", call. = FALSE)
  }
  if (model_type == "psm") {
    stop("For PSM models, use remove_psm_transition() instead")
  }
  if (model_type == "custom_psm") {
    stop("For Custom PSM models, use remove_custom_psm_transition() instead")
  }

  match_idx <- which(model$transitions$from_state == from_state &
                       model$transitions$to_state == to_state)
  if (length(match_idx) == 0) {
    stop("No transition found from '", from_state, "' to '", to_state, "'", call. = FALSE)
  }

  model$transitions <- model$transitions[-match_idx, , drop = FALSE]

  return(model)
}

#' Edit a PSM Transition
#'
#' Edit an existing PSM transition's formula and/or time unit.
#'
#' @param model An oq_model_builder object
#' @param endpoint Character string for the endpoint
#' @param formula An unquoted R expression for the new transition
#' @param time_unit Character string for the new time unit
#'
#' @return The modified model object
#'
#' @export
edit_psm_transition <- function(model, endpoint, formula, time_unit) {
  model_type <- tolower(model$settings$model_type %||% "markov")

  if (model_type == "markov") {
    stop("For Markov models, use edit_transition() instead")
  }
  if (model_type == "custom_psm") {
    stop("For Custom PSM models, use edit_custom_psm_transition() instead")
  }
  if (model_type == "decision_tree") {
    stop("Decision tree models do not support transitions.", call. = FALSE)
  }

  match_idx <- which(model$transitions$endpoint == endpoint)
  if (length(match_idx) == 0) {
    stop("No PSM transition found for endpoint '", endpoint, "'", call. = FALSE)
  }

  if (!missing(formula)) {
    formula_quo <- enquo(formula)
    formula_expr <- quo_get_expr(formula_quo)
    formula_str <- expr_text(formula_expr)
    if (is.character(formula_expr) && length(formula_expr) == 1) {
      formula_str <- formula_expr
    }
    model$transitions$formula[match_idx] <- formula_str
  }

  if (!missing(time_unit)) {
    model$transitions$time_unit[match_idx] <- time_unit
  }

  return(model)
}

#' Remove a PSM Transition
#'
#' Remove an existing PSM transition.
#'
#' @param model An oq_model_builder object
#' @param endpoint Character string for the endpoint
#'
#' @return The modified model object
#'
#' @export
remove_psm_transition <- function(model, endpoint) {
  model_type <- tolower(model$settings$model_type %||% "markov")

  if (model_type == "markov") {
    stop("For Markov models, use remove_transition() instead")
  }
  if (model_type == "custom_psm") {
    stop("For Custom PSM models, use remove_custom_psm_transition() instead")
  }
  if (model_type == "decision_tree") {
    stop("Decision tree models do not support transitions.", call. = FALSE)
  }

  match_idx <- which(model$transitions$endpoint == endpoint)
  if (length(match_idx) == 0) {
    stop("No PSM transition found for endpoint '", endpoint, "'", call. = FALSE)
  }

  model$transitions <- model$transitions[-match_idx, , drop = FALSE]

  return(model)
}

#' Edit a Custom PSM Transition
#'
#' Edit the formula of an existing Custom PSM state probability.
#'
#' @param model An oq_model_builder object (must be type "custom_psm")
#' @param state Character string for the state name
#' @param formula An unquoted R expression for the new state probability
#'
#' @return The modified model object
#'
#' @export
edit_custom_psm_transition <- function(model, state, formula) {
  is_custom_psm <- !is.null(model$settings$model_type) &&
    tolower(model$settings$model_type) == "custom_psm"

  if (!is_custom_psm) {
    stop("edit_custom_psm_transition() is only for Custom PSM models. ",
         "Use edit_transition() for Markov or edit_psm_transition() for standard PSM.")
  }

  match_idx <- which(model$transitions$state == state)
  if (length(match_idx) == 0) {
    stop("No Custom PSM transition found for state '", state, "'", call. = FALSE)
  }

  formula_quo <- enquo(formula)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  model$transitions$formula[match_idx] <- formula_str

  return(model)
}

#' Remove a Custom PSM Transition
#'
#' Remove an existing Custom PSM state probability.
#'
#' @param model An oq_model_builder object (must be type "custom_psm")
#' @param state Character string for the state name
#'
#' @return The modified model object
#'
#' @export
remove_custom_psm_transition <- function(model, state) {
  is_custom_psm <- !is.null(model$settings$model_type) &&
    tolower(model$settings$model_type) == "custom_psm"

  if (!is_custom_psm) {
    stop("remove_custom_psm_transition() is only for Custom PSM models. ",
         "Use remove_transition() for Markov or remove_psm_transition() for standard PSM.")
  }

  match_idx <- which(model$transitions$state == state)
  if (length(match_idx) == 0) {
    stop("No Custom PSM transition found for state '", state, "'", call. = FALSE)
  }

  model$transitions <- model$transitions[-match_idx, , drop = FALSE]

  return(model)
}

#' Add Values to Model
#'
#' Add one or more values to the model. Uses NSE to capture formula expressions.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the value name
#' @param formula An unquoted R expression for the value calculation
#' @param state Optional state association
#' @param destination Optional destination state
#' @param display_name Optional display name
#' @param description Optional description
#' @param type Value type (default: "outcome")
#' @param discounting_override Optional formula string that overrides per-cycle
#'   discounting. When provided, the formula result replaces the standard discount
#'   factor entirely: \code{values_discounted = values * result}. The formula can
#'   return a scalar or a vector of length n_cycles. Available variables in the
#'   formula: all model variables, \code{discount_rate}, \code{discount_factors},
#'   and \code{trace(state_name)}.
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_value("cost", base_cost + extra_cost, state = "sick")
add_value <- function(model, name, formula, state = NA, destination = NA,
                     display_name = NULL, description = NULL,
                     type = "outcome", discounting_override = NA) {

  # Validate value name
  if (missing(name) || !is.character(name) || length(name) != 1 || is.na(name) || trimws(name) == "") {
    stop("Value name must be a non-empty string.", call. = FALSE)
  }

  # Validate value type
  if (!type %in% c("outcome", "cost")) {
    stop("Value type must be 'outcome' or 'cost'.", call. = FALSE)
  }

  # Check for transitional values in custom_psm (not supported)
  model_type <- tolower(model$settings$model_type %||% "markov")
  if (model_type == "custom_psm") {
    has_state <- !is.na(state) && state != "NA"
    has_dest <- !is.na(destination) && destination != "NA"
    if (has_state && has_dest) {
      stop("Custom PSM models do not support transitional values (both state and destination). ",
           "Use residency values (state only) or model start values (no state/destination).")
    }
  }

  # Validate decision_tree state: destination must be NA
  if (!is.na(state) && as.character(state) == "decision_tree") {
    if (!is.na(destination) && as.character(destination) != "NA" && as.character(destination) != "") {
      stop("Values with state = 'decision_tree' cannot have a destination. ",
           "Decision tree values are one-time payoffs, not transitional values.",
           call. = FALSE)
    }
  }

  # Transitional values require both state and destination
  has_state_for_tv <- !is.na(state) && as.character(state) != "NA" && as.character(state) != ""
  has_dest_for_tv <- !is.na(destination) && as.character(destination) != "NA" && as.character(destination) != ""
  if (has_dest_for_tv && !has_state_for_tv) {
    stop("Transitional values must specify both 'state' and 'destination'. ",
         "Value '", name, "' has destination '", destination, "' but no state.",
         call. = FALSE)
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  # Validate formula is not empty
  if (!nzchar(trimws(formula_str))) {
    stop("Value formula must be a non-empty expression.", call. = FALSE)
  }

  # Validate "All" / "All Other" state targeting rules
  state_str <- as.character(state)
  dest_str <- as.character(destination)
  is_residence <- is.na(destination) || dest_str == "NA"

  if (state_str %in% c("All", "All Other")) {
    if (!is_residence) {
      stop(glue("'{state_str}' cannot be used for transition values (where destination is set). ",
                "It is only valid for residence values."), call. = FALSE)
    }

    existing <- model$values
    if (!is.null(existing) && nrow(existing) > 0) {
      existing_residence <- existing[existing$name == name &
        (is.na(existing$destination) | existing$destination == "NA"), , drop = FALSE]

      if (state_str == "All" && nrow(existing_residence) > 0) {
        stop(glue("Value '{name}' already has residence value rows. ",
                  "Cannot add 'All' when other residence rows exist."), call. = FALSE)
      }
      if (state_str == "All Other" && any(existing_residence$state == "All Other")) {
        stop(glue("Value '{name}' already has an 'All Other' residence row. ",
                  "Only one 'All Other' row is allowed per value name."), call. = FALSE)
      }
    }
  } else if (is_residence && !is.na(state) && state_str != "NA") {
    # Adding an explicit state — check if "All" already exists for this name
    existing <- model$values
    if (!is.null(existing) && nrow(existing) > 0) {
      existing_residence <- existing[existing$name == name &
        (is.na(existing$destination) | existing$destination == "NA"), , drop = FALSE]
      if (any(existing_residence$state == "All")) {
        stop(glue("Value '{name}' already uses state 'All'. ",
                  "Cannot add explicit state rows when 'All' is used."), call. = FALSE)
      }
    }
  }

  # Check for name collision with trees
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    if (name %in% unique(model$trees$name)) {
      stop(sprintf(
        'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
        name
      ), call. = FALSE)
    }
  }

  # Check for name collision with tables
  if (length(model$tables) > 0 && name %in% names(model$tables)) {
    stop(sprintf(
      "Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.",
      name
    ))
  }

  # Check for duplicate value (name + state + destination)
  if (!is.null(model$values) && is.data.frame(model$values) && nrow(model$values) > 0) {
    name_match <- model$values$name == name & !is.na(model$values$name)
    state_match <- model$values$state == state_str & !is.na(model$values$state)
    # Handle NA/\"NA\" destination comparison
    dest_is_na <- is.na(dest_str) | dest_str == "NA"
    existing_dest_is_na <- is.na(model$values$destination) | model$values$destination == "NA"
    dest_match <- if (dest_is_na) existing_dest_is_na else (model$values$destination == dest_str & !existing_dest_is_na)
    dupes <- name_match & state_match & dest_match
    if (any(dupes, na.rm = TRUE)) {
      stop(sprintf("Duplicate value: name '%s', state '%s', destination '%s' already exists.",
                   name, state_str, dest_str), call. = FALSE)
    }
  }

  new_value <- fast_tibble(
    name = name,
    formula = formula_str,
    state = state_str,
    destination = dest_str,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type,
    discounting_override = as.character(discounting_override)
  )

  model$values <- bind_rows(model$values, new_value)

  # Validate display_name/description consistency across all definitions of this value
  values_with_same_name <- model$values[model$values$name == name, ]
  if (nrow(values_with_same_name) > 1) {
    error_msg <- validate_value_display_names_for_builder(values_with_same_name, name)
    if (error_msg != "") stop(error_msg, call. = FALSE)
  }

  return(model)
}

#' Update Summary Value References
#'
#' Internal helper that updates value name references in summary CSV strings.
#' Used by \code{edit_value} (rename) and \code{remove_value} (cascade removal).
#'
#' @param summaries The summaries tibble from a model
#' @param old_name The value name to find
#' @param new_name The replacement name (NULL to remove the reference)
#'
#' @return Updated summaries tibble
#' @keywords internal
update_summary_value_refs <- function(summaries, old_name, new_name = NULL) {
  if (is.null(summaries) || !is.data.frame(summaries) || nrow(summaries) == 0) {
    return(summaries)
  }
  for (i in seq_len(nrow(summaries))) {
    tokens <- trimws(strsplit(summaries$values[i], ",")[[1]])
    if (old_name %in% tokens) {
      if (is.null(new_name)) {
        tokens <- tokens[tokens != old_name]
      } else {
        if (new_name %in% tokens) {
          # new_name already present, just remove old
          tokens <- tokens[tokens != old_name]
        } else {
          tokens[tokens == old_name] <- new_name
        }
      }
      summaries$values[i] <- paste(tokens, collapse = ", ")
    }
  }
  summaries
}

#' Edit a Value in the Model
#'
#' Modify an existing value's properties. The value is identified by its
#' \code{name}, \code{state}, and \code{destination} combination. Only the
#' fields that are explicitly provided will be updated.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the value to edit
#' @param state State used for lookup (default NA)
#' @param destination Destination used for lookup (default NA)
#' @param formula An unquoted R expression for the value calculation
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param type Optional new type ("outcome" or "cost")
#' @param discounting_override Optional discounting override formula
#' @param new_name Optional new name for the value row(s)
#' @param rename_all Logical. When TRUE and \code{new_name} is provided, renames
#'   ALL rows with the old name and cascades to summaries. Default FALSE.
#' @param error_on_field_changes Logical. When TRUE and merging into an existing
#'   name, throws error class \code{"value_field_changes"} if display_name or
#'   description would change. Default FALSE.
#' @param error_on_name_sharing Logical. When TRUE and \code{new_name} is provided,
#'   throws error class \code{"value_name_shared"} if the old name has multiple
#'   rows. Default FALSE.
#'
#' @return The modified model object
#'
#' @export
edit_value <- function(model, name, state = NA, destination = NA,
                       formula, display_name, description, type,
                       discounting_override, new_name,
                       rename_all = FALSE,
                       error_on_field_changes = FALSE,
                       error_on_name_sharing = FALSE) {

  # NA-safe comparison
  safe_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) == as.character(b))

  match_idx <- which(
    model$values$name == name &
    safe_eq(model$values$state, as.character(state)) &
    safe_eq(model$values$destination, as.character(destination))
  )

  if (length(match_idx) == 0) {
    target_desc <- name
    if (!is.na(state) && as.character(state) != "NA") target_desc <- paste0(target_desc, ", state='", state, "'")
    if (!is.na(destination) && as.character(destination) != "NA") target_desc <- paste0(target_desc, ", destination='", destination, "'")
    stop(sprintf("No value found matching: %s", target_desc), call. = FALSE)
  }

  # --- Simple field updates (when new_name NOT provided) ---

  if (!missing(formula)) {
    formula_quo <- enquo(formula)
    formula_expr <- quo_get_expr(formula_quo)
    formula_str <- expr_text(formula_expr)
    if (is.character(formula_expr) && length(formula_expr) == 1) {
      formula_str <- formula_expr
    }
    model$values$formula[match_idx] <- formula_str
  }

  if (!missing(discounting_override)) {
    disc_quo <- enquo(discounting_override)
    disc_expr <- quo_get_expr(disc_quo)
    if (is.character(disc_expr)) {
      disc_str <- disc_expr
    } else {
      disc_str <- expr_text(disc_expr)
    }
    model$values$discounting_override[match_idx] <- disc_str
  }

  if (!missing(type)) {
    if (!type %in% c("outcome", "cost")) {
      stop("Value type must be 'outcome' or 'cost'.", call. = FALSE)
    }
    model$values$type[match_idx] <- type
  }

  # --- Rename logic ---
  if (!missing(new_name)) {

    # Step 0: Name sharing check
    old_name_rows <- which(model$values$name == name)
    if (error_on_name_sharing && length(old_name_rows) > 1) {
      states_data <- fast_tibble(
        state = model$values$state[old_name_rows],
        destination = model$values$destination[old_name_rows]
      )
      cond <- structure(
        class = c("value_name_shared", "error", "condition"),
        list(
          message = sprintf('Value "%s" is shared across %d rows.', name, length(old_name_rows)),
          shared_count = length(old_name_rows),
          states = states_data
        )
      )
      stop(cond)
    }

    # Step 1: Name collision checks
    if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
      if (new_name %in% unique(model$trees$name)) {
        stop(sprintf(
          'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
          new_name
        ), call. = FALSE)
      }
    }
    if (length(model$tables) > 0 && new_name %in% names(model$tables)) {
      stop(sprintf(
        "Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.",
        new_name
      ), call. = FALSE)
    }

    # Step 2: Check if merging into existing name
    target_rows <- which(model$values$name == new_name)
    merging_into_existing <- length(target_rows) > 0

    # Step 3: Handle merge — auto-adopt with optional error
    if (merging_into_existing) {
      target_display <- model$values$display_name[target_rows[1]]
      target_desc <- model$values$description[target_rows[1]]
      current_display <- model$values$display_name[match_idx]
      current_desc <- model$values$description[match_idx]

      fields_differ <- !identical(current_display, target_display) ||
                       !identical(current_desc, target_desc)

      if (fields_differ && error_on_field_changes) {
        field_changes <- list(
          display_name = list(old = current_display, new = target_display),
          description = list(old = current_desc, new = target_desc)
        )
        cond <- structure(
          class = c("value_field_changes", "error", "condition"),
          list(
            message = sprintf('Moving value to "%s" would change display_name and/or description.', new_name),
            field_changes = field_changes
          )
        )
        stop(cond)
      }

      # Auto-adopt target's display_name/description
      if (!missing(display_name)) {
        model$values$display_name[match_idx] <- display_name
      } else {
        model$values$display_name[match_idx] <- target_display
      }
      if (!missing(description)) {
        model$values$description[match_idx] <- description
      } else {
        model$values$description[match_idx] <- target_desc
      }
    } else {
      # Not merging — apply explicit display_name/description if provided
      if (!missing(display_name)) {
        model$values$display_name[match_idx] <- display_name
      }
      if (!missing(description)) {
        model$values$description[match_idx] <- description
      }
    }

    # Step 4: Apply the name change
    if (rename_all) {
      model$values$name[model$values$name == name] <- new_name
      model$summaries <- update_summary_value_refs(model$summaries, name, new_name)
    } else {
      model$values$name[match_idx] <- new_name
      # Step 5: Handle orphaned old name
      old_name_remaining <- any(model$values$name == name)
      if (!old_name_remaining) {
        model$summaries <- update_summary_value_refs(model$summaries, name, new_name)
      }
    }

    # Step 6: Post-move validation
    values_with_new_name <- model$values[model$values$name == new_name, ]
    if (nrow(values_with_new_name) > 1) {
      error_msg <- validate_value_display_names_for_builder(values_with_new_name, new_name)
      if (error_msg != "") stop(error_msg, call. = FALSE)
    }

  } else {
    # No rename — apply display_name/description and validate
    if (!missing(display_name)) {
      model$values$display_name[match_idx] <- display_name
    }
    if (!missing(description)) {
      model$values$description[match_idx] <- description
    }

    if (!missing(display_name) || !missing(description)) {
      values_with_same_name <- model$values[model$values$name == name, ]
      if (nrow(values_with_same_name) > 1) {
        error_msg <- validate_value_display_names_for_builder(values_with_same_name, name)
        if (error_msg != "") stop(error_msg, call. = FALSE)
      }
    }
  }

  model
}

#' Remove a Value from the Model
#'
#' Remove one or more value rows from the model. If only \code{name} is given,
#' all rows with that value name are removed. If \code{state} and/or
#' \code{destination} are specified, only matching rows are removed.
#'
#' When the last row for a given value name is removed, references to that
#' value are also removed from summary CSV strings. If \code{error_on_dependencies}
#' is TRUE and the value name would be fully removed, an error of class
#' \code{"value_has_dependencies"} is thrown instead.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the value to remove
#' @param state Optional state to narrow removal. If NULL, all states are matched.
#' @param destination Optional destination to narrow removal. If NULL, all
#'   destinations are matched.
#' @param error_on_dependencies Logical. If TRUE and the value name would be
#'   fully removed while referenced in summaries, an error of class
#'   \code{"value_has_dependencies"} is thrown. Default FALSE.
#'
#' @return The modified model object
#'
#' @export
remove_value <- function(model, name, state = NULL, destination = NULL,
                         error_on_dependencies = FALSE) {

  safe_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & as.character(a) == as.character(b))

  # Build match mask

  match_mask <- model$values$name == name

  if (!is.null(state)) {
    match_mask <- match_mask & safe_eq(model$values$state, as.character(state))
  }

  if (!is.null(destination)) {
    match_mask <- match_mask & safe_eq(model$values$destination, as.character(destination))
  }

  if (!any(match_mask)) {
    target_desc <- name
    if (!is.null(state)) target_desc <- paste0(target_desc, ", state='", state, "'")
    if (!is.null(destination)) target_desc <- paste0(target_desc, ", destination='", destination, "'")
    stop(sprintf("No value found matching: %s", target_desc), call. = FALSE)
  }

  # Determine if name would be fully removed
  remaining <- model$values$name[!match_mask]
  name_fully_removed <- !(name %in% remaining)

  # Dependency detection
  if (name_fully_removed && error_on_dependencies) {
    if (!is.null(model$summaries) && is.data.frame(model$summaries) && nrow(model$summaries) > 0) {
      dep_summaries <- character(0)
      for (i in seq_len(nrow(model$summaries))) {
        tokens <- trimws(strsplit(model$summaries$values[i], ",")[[1]])
        if (name %in% tokens) {
          dep_summaries <- c(dep_summaries, model$summaries$name[i])
        }
      }
      if (length(dep_summaries) > 0) {
        cond <- structure(
          class = c("value_has_dependencies", "error", "condition"),
          list(
            message = sprintf('Cannot remove value "%s": it has downstream dependencies.', name),
            dependencies = list(summaries = dep_summaries)
          )
        )
        stop(cond)
      }
    }
  }

  # Remove matched rows
  model$values <- model$values[!match_mask, , drop = FALSE]

  # Cascade: strip from summaries if name fully removed
  if (name_fully_removed) {
    model$summaries <- update_summary_value_refs(model$summaries, name, NULL)
  }

  model
}

#' Add Variables to Model
#'
#' Add one or more variables to the model. Uses NSE to capture formula expressions.
#'
#' **Display Names**: The `display_name` parameter is optional. If not provided, display names
#' are automatically generated based on the variable name, strategy, and group:
#' - No strategy/group: `"var_name"`
#' - Strategy only: `"var_name, strategy_name"`
#' - Group only: `"var_name, group_name"`
#' - Both: `"var_name, strategy_name, group_name"`
#'
#' This auto-generation ensures unique display names for strategy/group-specific variables,
#' which is important for DSA/PSA plots and tables.
#'
#' **Important**: When a variable has multiple definitions (e.g., one per strategy/group), if you
#' provide a custom `display_name` for at least one definition, you must provide it for ALL definitions
#' of that variable. Mixing custom and auto-generated display names for the same variable will result
#' in a validation error **immediately** when you call `add_variable()` with the inconsistent definition.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the variable name
#' @param formula An unquoted R expression for the variable calculation
#' @param display_name Optional display name. If not provided, will be auto-generated
#'   from name, strategy, and group. You can override with a custom name if desired.
#' @param description Optional description
#' @param strategy Optional strategy association
#' @param group Optional group association
#' @param source Optional source information
#' @param sampling Optional sampling information
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_variable("p_disease", look_up(table, age = cohort_age))
#'
#' # Strategy-specific variables - display names auto-generated
#' model <- model |>
#'   add_variable("cost_med", strategy = "treatment_a", formula = 5000,
#'                sampling = normal(5000, 500)) |>
#'   add_variable("cost_med", strategy = "treatment_b", formula = 3000,
#'                sampling = normal(3000, 300))
#' # Auto-generates: "cost_med, treatment_a" and "cost_med, treatment_b"
#'
#' # Or provide custom display names if preferred
#' model <- model |>
#'   add_variable("cost_admin", display_name = "Admin Cost (Treatment A)",
#'                strategy = "treatment_a", formula = 500) |>
#'   add_variable("cost_admin", display_name = "Admin Cost (Treatment B)",
#'                strategy = "treatment_b", formula = 300)
add_variable <- function(model, name, formula, display_name = NULL,
                        description = NULL, strategy = "", group = "",
                        source = "", sampling) {

  # Validate variable name
  if (missing(name) || !is.character(name) || length(name) != 1 || is.na(name) || trimws(name) == "") {
    stop("Variable name must be a non-empty string.", call. = FALSE)
  }

  # Capture the formula expression using NSE
  formula_quo <- enquo(formula)
  # Remove the ~ if it exists (enquo adds it)
  formula_expr <- quo_get_expr(formula_quo)
  formula_str <- expr_text(formula_expr)
  # If formula was passed as a string, use it directly (expr_text adds extra quotes)
  if (is.character(formula_expr) && length(formula_expr) == 1) {
    formula_str <- formula_expr
  }

  # Validate formula is not empty
  if (!nzchar(trimws(formula_str))) {
    stop("Variable formula must be a non-empty expression.", call. = FALSE)
  }

  # Capture the sampling expression using NSE
  # Handle optional parameter: if missing, use empty string
  if (missing(sampling)) {
    sampling_str <- ""
  } else {
    sampling_quo <- enquo(sampling)
    sampling_expr <- quo_get_expr(sampling_quo)

    # Handle sampling: if already a string, use as-is (backward compatibility)
    # If an expression/call, convert to string using NSE
    if (is.character(sampling_expr)) {
      sampling_str <- sampling_expr
    } else {
      sampling_str <- expr_text(sampling_expr)
    }
  }

  # Store display_name as-is (don't auto-generate yet)
  # Auto-generation happens later in normalize_and_validate_model()
  # Convert NULL to empty string for consistency
  if (is.null(display_name)) {
    display_name <- ""
  }

  # If sampling is provided, validate strategy/group targeting
  # This ensures PSA sampling distributions are properly targeted
  if (sampling_str != "") {
    existing_vars <- model$variables[model$variables$name == name, ]
    if (nrow(existing_vars) > 0) {
      # Check if any existing definition has strategy-specific values
      existing_strategies <- unique(existing_vars$strategy[existing_vars$strategy != ""])
      if (length(existing_strategies) > 0 && strategy == "") {
        stop(sprintf(
          paste0(
            "Variable '%s' already has strategy-specific definitions: %s\n",
            "You must specify which strategy this sampling applies to using the 'strategy' parameter.\n",
            "Example: add_variable(\"%s\", ..., strategy = \"%s\", sampling = ...)"
          ),
          name,
          paste(existing_strategies, collapse = ", "),
          name,
          existing_strategies[1]
        ), call. = FALSE)
      }

      # Check if any existing definition has group-specific values
      existing_groups <- unique(existing_vars$group[existing_vars$group != ""])
      if (length(existing_groups) > 0 && group == "") {
        stop(sprintf(
          paste0(
            "Variable '%s' already has group-specific definitions: %s\n",
            "You must specify which group this sampling applies to using the 'group' parameter.\n",
            "Example: add_variable(\"%s\", ..., group = \"%s\", sampling = ...)"
          ),
          name,
          paste(existing_groups, collapse = ", "),
          name,
          existing_groups[1]
        ), call. = FALSE)
      }
    }
  }

  # Check for name collision with trees
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    if (name %in% unique(model$trees$name)) {
      stop(sprintf(
        'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
        name
      ), call. = FALSE)
    }
  }

  new_var <- fast_tibble(
    name = name,
    formula = formula_str,
    display_name = display_name,
    description = description %||% "",
    strategy = strategy,
    group = group,
    source = source,
    sampling = sampling_str
  )

  model$variables <- bind_rows(model$variables, new_var)

  # Validate display name consistency for all variables with this name
  vars_with_same_name <- model$variables[model$variables$name == name, ]
  if (nrow(vars_with_same_name) > 1) {
    error_msg <- validate_variable_display_names_for_builder(vars_with_same_name, name)
    if (error_msg != "") {
      stop(error_msg, call. = FALSE)
    }
  }

  model
}

#' Edit a Variable in the Model
#'
#' Modify an existing variable's properties. The variable is identified by its
#' name, strategy, and group combination. Only the fields that are explicitly
#' provided will be updated.
#'
#' Note: You cannot change a variable's strategy or group targeting. To change
#' targeting, remove the variable and add it again with the new targeting.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the variable to edit
#' @param formula An unquoted R expression for the variable calculation
#' @param display_name Optional display name
#' @param description Optional description
#' @param strategy Strategy association used for lookup (default: "")
#' @param group Group association used for lookup (default: "")
#' @param source Optional source information
#' @param sampling Optional sampling information
#' @param new_name Optional new name for the variable. If provided, the
#'   variable will be renamed. Must not collide with existing decision tree
#'   names.
#'
#' @return The modified model object
#'
#' @export
edit_variable <- function(model, name, formula, display_name,
                          description, strategy = "", group = "",
                          source, sampling, new_name) {

  # Find the matching variable row (NA-safe comparison)
  safe_eq <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  match_idx <- which(
    model$variables$name == name &
    safe_eq(model$variables$strategy, strategy) &
    safe_eq(model$variables$group, group)
  )

  if (length(match_idx) == 0) {
    target_desc <- name
    if (!is.na(strategy) && strategy != "") target_desc <- paste0(target_desc, ", strategy='", strategy, "'")
    if (!is.na(group) && group != "") target_desc <- paste0(target_desc, ", group='", group, "'")
    stop(sprintf("No variable found matching: %s", target_desc), call. = FALSE)
  }

  # Update formula if provided
  if (!missing(formula)) {
    formula_quo <- enquo(formula)
    formula_expr <- quo_get_expr(formula_quo)
    formula_str <- expr_text(formula_expr)
    if (is.character(formula_expr) && length(formula_expr) == 1) {
      formula_str <- formula_expr
    }
    model$variables$formula[match_idx] <- formula_str
  }

  # Update sampling if provided
  if (!missing(sampling)) {
    sampling_quo <- enquo(sampling)
    sampling_expr <- quo_get_expr(sampling_quo)
    if (is.character(sampling_expr)) {
      sampling_str <- sampling_expr
    } else {
      sampling_str <- expr_text(sampling_expr)
    }
    model$variables$sampling[match_idx] <- sampling_str
  }

  # Update simple string fields if provided
  if (!missing(display_name)) {
    if (is.null(display_name)) display_name <- ""
    model$variables$display_name[match_idx] <- display_name
  }

  if (!missing(description)) {
    model$variables$description[match_idx] <- description %||% ""
  }

  if (!missing(source)) {
    model$variables$source[match_idx] <- source
  }

  # Rename variable if new_name provided
  if (!missing(new_name)) {
    if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
      if (new_name %in% unique(model$trees$name)) {
        stop(sprintf(
          'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
          new_name
        ), call. = FALSE)
      }
    }
    model$variables$name[match_idx] <- new_name
  }

  # Validate display name consistency for all variables with this name
  lookup_name <- if (!missing(new_name)) new_name else name
  vars_with_same_name <- model$variables[model$variables$name == lookup_name, ]
  if (nrow(vars_with_same_name) > 1) {
    error_msg <- validate_variable_display_names_for_builder(vars_with_same_name, lookup_name)
    if (error_msg != "") {
      stop(error_msg, call. = FALSE)
    }
  }

  model
}

#' Remove a Variable from the Model
#'
#' Remove one or more variable definitions from the model. If only `name` is
#' given, all rows with that variable name are removed (all strategy/group
#' combinations). If `strategy` and/or `group` are specified, only matching
#' rows are removed.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the variable to remove
#' @param strategy Optional strategy to narrow removal. If NULL, all strategies
#'   are matched.
#' @param group Optional group to narrow removal. If NULL, all groups are
#'   matched.
#'
#' @return The modified model object
#'
#' @export
remove_variable <- function(model, name, strategy = NULL, group = NULL) {

  # Build the match condition
  match_mask <- model$variables$name == name

  if (!is.null(strategy)) {
    match_mask <- match_mask & model$variables$strategy == strategy
  }

  if (!is.null(group)) {
    match_mask <- match_mask & model$variables$group == group
  }

  if (!any(match_mask)) {
    target_desc <- name
    if (!is.null(strategy)) target_desc <- paste0(target_desc, ", strategy='", strategy, "'")
    if (!is.null(group)) target_desc <- paste0(target_desc, ", group='", group, "'")
    stop(sprintf("No variable found matching: %s", target_desc), call. = FALSE)
  }

  model$variables <- model$variables[!match_mask, ]

  model
}

#' Add Strategies to Model
#'
#' Add one or more strategies to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the strategy name
#' @param display_name Optional display name
#' @param description Optional description
#' @param enabled Numeric indicating if strategy is enabled (default: 1)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_strategy("treatment_a", "Treatment A")
add_strategy <- function(model, name, display_name = NULL,
                        description = NULL, enabled = 1) {

  # Check for duplicate strategy name
  if (!is.null(model$strategies) && is.data.frame(model$strategies) && nrow(model$strategies) > 0) {
    if (name %in% model$strategies$name) {
      stop(sprintf('Duplicate strategy name "%s". Each strategy must have a unique name.', name), call. = FALSE)
    }
  }

  new_strat <- fast_tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    enabled = as.numeric(enabled)
  )

  model$strategies <- bind_rows(model$strategies, new_strat)
  model
}

#' Edit a Strategy in the Model
#'
#' Modify an existing strategy's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the strategy are updated (variables, DSA parameters,
#' scenarios, TWSA analyses, override categories, threshold analyses, and
#' multivariate sampling specifications).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the strategy to edit
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param enabled Optional new enabled flag (will be coerced to numeric)
#' @param new_name Optional new name for the strategy. If provided, the strategy
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#'
#' @export
edit_strategy <- function(model, name, display_name, description, enabled, new_name) {
  match_idx <- which(model$strategies$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('Strategy "%s" not found in model.', name), call. = FALSE)
  }

  if (!missing(display_name)) {
    model$strategies$display_name[match_idx] <- display_name
  }

  if (!missing(description)) {
    model$strategies$description[match_idx] <- description
  }

  if (!missing(enabled)) {
    model$strategies$enabled[match_idx] <- as.numeric(enabled)
  }

  if (!missing(new_name)) {
    # Update the strategy name itself
    model$strategies$name[match_idx] <- new_name

    # Cascade rename through all downstream references

    # 1. Variables
    if (is.data.frame(model$variables) && nrow(model$variables) > 0) {
      model$variables$strategy[model$variables$strategy %in% name] <- new_name
    }

    # 2. DSA parameters
    if (length(model$dsa_parameters) > 0) {
      for (i in seq_along(model$dsa_parameters)) {
        if (identical(model$dsa_parameters[[i]]$strategy, name)) {
          model$dsa_parameters[[i]]$strategy <- new_name
        }
      }
    }

    # 3. Scenarios -> variable_overrides
    if (length(model$scenarios) > 0) {
      for (i in seq_along(model$scenarios)) {
        if (length(model$scenarios[[i]]$variable_overrides) > 0) {
          for (j in seq_along(model$scenarios[[i]]$variable_overrides)) {
            if (identical(model$scenarios[[i]]$variable_overrides[[j]]$strategy, name)) {
              model$scenarios[[i]]$variable_overrides[[j]]$strategy <- new_name
            }
          }
        }
      }
    }

    # 4. TWSA analyses -> parameters
    if (length(model$twsa_analyses) > 0) {
      for (i in seq_along(model$twsa_analyses)) {
        if (length(model$twsa_analyses[[i]]$parameters) > 0) {
          for (j in seq_along(model$twsa_analyses[[i]]$parameters)) {
            if (identical(model$twsa_analyses[[i]]$parameters[[j]]$strategy, name)) {
              model$twsa_analyses[[i]]$parameters[[j]]$strategy <- new_name
            }
          }
        }
      }
    }

    # 5. Override categories -> overrides
    if (length(model$override_categories) > 0) {
      for (i in seq_along(model$override_categories)) {
        if (length(model$override_categories[[i]]$overrides) > 0) {
          for (j in seq_along(model$override_categories[[i]]$overrides)) {
            if (identical(model$override_categories[[i]]$overrides[[j]]$strategy, name)) {
              model$override_categories[[i]]$overrides[[j]]$strategy <- new_name
            }
          }
        }
      }
    }

    # 6. Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      for (i in seq_along(model$threshold_analyses)) {
        if (identical(model$threshold_analyses[[i]]$variable_strategy, name)) {
          model$threshold_analyses[[i]]$variable_strategy <- new_name
        }
        if (!is.null(model$threshold_analyses[[i]]$condition) &&
            identical(model$threshold_analyses[[i]]$condition$strategy, name)) {
          model$threshold_analyses[[i]]$condition$strategy <- new_name
        }
      }
    }

    # 7. Multivariate sampling -> variables tibble
    if (length(model$multivariate_sampling) > 0) {
      for (i in seq_along(model$multivariate_sampling)) {
        if (is.data.frame(model$multivariate_sampling[[i]]$variables) &&
            nrow(model$multivariate_sampling[[i]]$variables) > 0) {
          model$multivariate_sampling[[i]]$variables$strategy[
            model$multivariate_sampling[[i]]$variables$strategy %in% name
          ] <- new_name
        }
      }
    }
  }

  model
}

#' Remove a Strategy from the Model
#'
#' Remove an existing strategy and optionally cascade-remove all downstream
#' references (variables, DSA parameters, scenario overrides, TWSA analyses,
#' override categories, threshold analyses, and multivariate sampling specs).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the strategy to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the strategy has
#'   downstream dependencies, an error of class \code{"strategy_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the strategy and all
#'   downstream references.
#'
#' @return The modified model object
#'
#' @export
remove_strategy <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- which(model$strategies$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('Strategy "%s" not found in model.', name), call. = FALSE)
  }

  if (error_on_dependencies) {
    deps <- list()

    # Variables
    if (is.data.frame(model$variables) && nrow(model$variables) > 0) {
      dep_vars <- model$variables$name[model$variables$strategy %in% name]
      if (length(dep_vars) > 0) deps$variables <- dep_vars
    }

    # DSA parameters
    if (length(model$dsa_parameters) > 0) {
      dep_dsa <- character(0)
      for (i in seq_along(model$dsa_parameters)) {
        if (identical(model$dsa_parameters[[i]]$strategy, name)) {
          dep_dsa <- c(dep_dsa, model$dsa_parameters[[i]]$name %||% paste0("param_", i))
        }
      }
      if (length(dep_dsa) > 0) deps$dsa_parameters <- dep_dsa
    }

    # Scenarios
    if (length(model$scenarios) > 0) {
      dep_scenarios <- list()
      for (i in seq_along(model$scenarios)) {
        if (length(model$scenarios[[i]]$variable_overrides) > 0) {
          ovr_names <- character(0)
          for (j in seq_along(model$scenarios[[i]]$variable_overrides)) {
            if (identical(model$scenarios[[i]]$variable_overrides[[j]]$strategy, name)) {
              ovr_names <- c(ovr_names, model$scenarios[[i]]$variable_overrides[[j]]$name %||% paste0("override_", j))
            }
          }
          if (length(ovr_names) > 0) {
            dep_scenarios[[model$scenarios[[i]]$name]] <- ovr_names
          }
        }
      }
      if (length(dep_scenarios) > 0) deps$scenarios <- dep_scenarios
    }

    # TWSA analyses
    if (length(model$twsa_analyses) > 0) {
      dep_twsa <- character(0)
      for (i in seq_along(model$twsa_analyses)) {
        if (length(model$twsa_analyses[[i]]$parameters) > 0) {
          for (j in seq_along(model$twsa_analyses[[i]]$parameters)) {
            if (identical(model$twsa_analyses[[i]]$parameters[[j]]$strategy, name)) {
              dep_twsa <- c(dep_twsa, model$twsa_analyses[[i]]$name)
              break
            }
          }
        }
      }
      if (length(dep_twsa) > 0) deps$twsa_analyses <- dep_twsa
    }

    # Override categories
    if (length(model$override_categories) > 0) {
      dep_overrides <- list()
      for (i in seq_along(model$override_categories)) {
        if (length(model$override_categories[[i]]$overrides) > 0) {
          ovr_titles <- character(0)
          for (j in seq_along(model$override_categories[[i]]$overrides)) {
            if (identical(model$override_categories[[i]]$overrides[[j]]$strategy, name)) {
              ovr_titles <- c(ovr_titles, model$override_categories[[i]]$overrides[[j]]$title %||%
                model$override_categories[[i]]$overrides[[j]]$name %||% paste0("override_", j))
            }
          }
          if (length(ovr_titles) > 0) {
            dep_overrides[[model$override_categories[[i]]$name]] <- ovr_titles
          }
        }
      }
      if (length(dep_overrides) > 0) deps$override_categories <- dep_overrides
    }

    # Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      dep_thresh <- character(0)
      for (i in seq_along(model$threshold_analyses)) {
        if (identical(model$threshold_analyses[[i]]$variable_strategy, name) ||
            (!is.null(model$threshold_analyses[[i]]$condition) &&
             identical(model$threshold_analyses[[i]]$condition$strategy, name))) {
          dep_thresh <- c(dep_thresh, model$threshold_analyses[[i]]$name %||% paste0("threshold_", i))
        }
      }
      if (length(dep_thresh) > 0) deps$threshold_analyses <- dep_thresh
    }

    # Multivariate sampling
    if (length(model$multivariate_sampling) > 0) {
      dep_mvs <- character(0)
      for (i in seq_along(model$multivariate_sampling)) {
        if (is.data.frame(model$multivariate_sampling[[i]]$variables) &&
            nrow(model$multivariate_sampling[[i]]$variables) > 0 &&
            any(model$multivariate_sampling[[i]]$variables$strategy %in% name)) {
          dep_mvs <- c(dep_mvs, model$multivariate_sampling[[i]]$name %||% paste0("mvs_", i))
        }
      }
      if (length(dep_mvs) > 0) deps$multivariate_sampling <- dep_mvs
    }

    deps <- Filter(function(x) length(x) > 0, deps)

    if (length(deps) > 0) {
      cond <- structure(
        class = c("strategy_has_dependencies", "error", "condition"),
        list(
          message = sprintf('Cannot remove strategy "%s": it has downstream dependencies.', name),
          dependencies = deps
        )
      )
      stop(cond)
    }
  }

  # Remove the strategy row
  model$strategies <- model$strategies[-match_idx, , drop = FALSE]

  # Cascade remove downstream references

  # 1. Variables
  if (is.data.frame(model$variables) && nrow(model$variables) > 0) {
    model$variables <- model$variables[!model$variables$strategy %in% name, , drop = FALSE]
  }

  # 2. DSA parameters
  if (length(model$dsa_parameters) > 0) {
    keep <- vapply(model$dsa_parameters, function(p) !identical(p$strategy, name), logical(1))
    model$dsa_parameters <- model$dsa_parameters[keep]
  }

  # 3. Scenarios -> variable_overrides
  if (length(model$scenarios) > 0) {
    for (i in seq_along(model$scenarios)) {
      if (length(model$scenarios[[i]]$variable_overrides) > 0) {
        keep <- vapply(model$scenarios[[i]]$variable_overrides,
                       function(o) !identical(o$strategy, name), logical(1))
        model$scenarios[[i]]$variable_overrides <- model$scenarios[[i]]$variable_overrides[keep]
      }
    }
  }

  # 4. TWSA analyses -> parameters (remove entire analysis if < 2 params remain)
  if (length(model$twsa_analyses) > 0) {
    for (i in seq_along(model$twsa_analyses)) {
      if (length(model$twsa_analyses[[i]]$parameters) > 0) {
        keep <- vapply(model$twsa_analyses[[i]]$parameters,
                       function(p) !identical(p$strategy, name), logical(1))
        model$twsa_analyses[[i]]$parameters <- model$twsa_analyses[[i]]$parameters[keep]
      }
    }
    keep_analyses <- vapply(model$twsa_analyses,
                            function(a) length(a$parameters) >= 2, logical(1))
    model$twsa_analyses <- model$twsa_analyses[keep_analyses]
  }

  # 5. Override categories -> overrides
  if (length(model$override_categories) > 0) {
    for (i in seq_along(model$override_categories)) {
      if (length(model$override_categories[[i]]$overrides) > 0) {
        keep <- vapply(model$override_categories[[i]]$overrides,
                       function(o) !identical(o$strategy, name), logical(1))
        model$override_categories[[i]]$overrides <- model$override_categories[[i]]$overrides[keep]
      }
    }
  }

  # 6. Threshold analyses
  if (length(model$threshold_analyses) > 0) {
    keep <- vapply(model$threshold_analyses, function(a) {
      !identical(a$variable_strategy, name) &&
        !((!is.null(a$condition)) && identical(a$condition$strategy, name))
    }, logical(1))
    model$threshold_analyses <- model$threshold_analyses[keep]
  }

  # 7. Multivariate sampling
  if (length(model$multivariate_sampling) > 0) {
    for (i in seq_along(model$multivariate_sampling)) {
      if (is.data.frame(model$multivariate_sampling[[i]]$variables) &&
          nrow(model$multivariate_sampling[[i]]$variables) > 0) {
        model$multivariate_sampling[[i]]$variables <-
          model$multivariate_sampling[[i]]$variables[
            !model$multivariate_sampling[[i]]$variables$strategy %in% name, , drop = FALSE
          ]
      }
    }
    keep_specs <- vapply(model$multivariate_sampling, function(s) {
      is.data.frame(s$variables) && nrow(s$variables) > 0
    }, logical(1))
    model$multivariate_sampling <- model$multivariate_sampling[keep_specs]
  }

  model
}

#' Add Groups to Model
#'
#' Add one or more groups to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the group name
#' @param display_name Optional display name
#' @param description Optional description
#' @param weight Weight expression (default: "1")
#' @param enabled Whether group is enabled (default: 1)
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_group("moderate", weight = "w_moderate")
add_group <- function(model, name, display_name = NULL,
                     description = NULL, weight = "1", enabled = 1) {

  # Check for duplicate group name
  if (!is.null(model$groups) && is.data.frame(model$groups) && nrow(model$groups) > 0) {
    if (name %in% model$groups$name) {
      stop(sprintf('Duplicate group name "%s". Each group must have a unique name.', name), call. = FALSE)
    }
  }

  new_group <- fast_tibble(
    name = name,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    weight = as.character(weight),
    enabled = as.numeric(enabled)
  )

  model$groups <- bind_rows(model$groups, new_group)
  model
}

#' Edit a Group in the Model
#'
#' Modify an existing group's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the group are updated (variables, DSA parameters,
#' scenarios, TWSA analyses, override categories, threshold analyses, and
#' multivariate sampling specifications).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the group to edit
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param weight Optional new weight expression (will be coerced to character)
#' @param enabled Optional new enabled flag (will be coerced to numeric)
#' @param new_name Optional new name for the group. If provided, the group
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#'
#' @export
edit_group <- function(model, name, display_name, description, weight, enabled, new_name) {
  match_idx <- which(model$groups$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('Group "%s" not found in model.', name), call. = FALSE)
  }

  if (!missing(display_name)) {
    model$groups$display_name[match_idx] <- display_name
  }

  if (!missing(description)) {
    model$groups$description[match_idx] <- description
  }

  if (!missing(weight)) {
    model$groups$weight[match_idx] <- as.character(weight)
  }

  if (!missing(enabled)) {
    model$groups$enabled[match_idx] <- as.numeric(enabled)
  }

  if (!missing(new_name)) {
    # Update the group name itself
    model$groups$name[match_idx] <- new_name

    # Cascade rename through all downstream references

    # 1. Variables
    if (is.data.frame(model$variables) && nrow(model$variables) > 0) {
      model$variables$group[model$variables$group %in% name] <- new_name
    }

    # 2. DSA parameters
    if (length(model$dsa_parameters) > 0) {
      for (i in seq_along(model$dsa_parameters)) {
        if (identical(model$dsa_parameters[[i]]$group, name)) {
          model$dsa_parameters[[i]]$group <- new_name
        }
      }
    }

    # 3. Scenarios -> variable_overrides
    if (length(model$scenarios) > 0) {
      for (i in seq_along(model$scenarios)) {
        if (length(model$scenarios[[i]]$variable_overrides) > 0) {
          for (j in seq_along(model$scenarios[[i]]$variable_overrides)) {
            if (identical(model$scenarios[[i]]$variable_overrides[[j]]$group, name)) {
              model$scenarios[[i]]$variable_overrides[[j]]$group <- new_name
            }
          }
        }
      }
    }

    # 4. TWSA analyses -> parameters
    if (length(model$twsa_analyses) > 0) {
      for (i in seq_along(model$twsa_analyses)) {
        if (length(model$twsa_analyses[[i]]$parameters) > 0) {
          for (j in seq_along(model$twsa_analyses[[i]]$parameters)) {
            if (identical(model$twsa_analyses[[i]]$parameters[[j]]$group, name)) {
              model$twsa_analyses[[i]]$parameters[[j]]$group <- new_name
            }
          }
        }
      }
    }

    # 5. Override categories -> overrides
    if (length(model$override_categories) > 0) {
      for (i in seq_along(model$override_categories)) {
        if (length(model$override_categories[[i]]$overrides) > 0) {
          for (j in seq_along(model$override_categories[[i]]$overrides)) {
            if (identical(model$override_categories[[i]]$overrides[[j]]$group, name)) {
              model$override_categories[[i]]$overrides[[j]]$group <- new_name
            }
          }
        }
      }
    }

    # 6. Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      for (i in seq_along(model$threshold_analyses)) {
        if (identical(model$threshold_analyses[[i]]$variable_group, name)) {
          model$threshold_analyses[[i]]$variable_group <- new_name
        }
        if (!is.null(model$threshold_analyses[[i]]$condition) &&
            identical(model$threshold_analyses[[i]]$condition$group, name)) {
          model$threshold_analyses[[i]]$condition$group <- new_name
        }
      }
    }

    # 7. Multivariate sampling -> variables tibble
    if (length(model$multivariate_sampling) > 0) {
      for (i in seq_along(model$multivariate_sampling)) {
        if (is.data.frame(model$multivariate_sampling[[i]]$variables) &&
            nrow(model$multivariate_sampling[[i]]$variables) > 0) {
          model$multivariate_sampling[[i]]$variables$group[
            model$multivariate_sampling[[i]]$variables$group %in% name
          ] <- new_name
        }
      }
    }
  }

  model
}

#' Remove a Group from the Model
#'
#' Remove an existing group and optionally cascade-remove all downstream
#' references (variables, DSA parameters, scenario overrides, TWSA analyses,
#' override categories, threshold analyses, and multivariate sampling specs).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the group to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the group has
#'   downstream dependencies, an error of class \code{"group_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the group and all
#'   downstream references.
#'
#' @return The modified model object
#'
#' @export
remove_group <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- which(model$groups$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('Group "%s" not found in model.', name), call. = FALSE)
  }

  if (error_on_dependencies) {
    deps <- list()

    # Variables
    if (is.data.frame(model$variables) && nrow(model$variables) > 0) {
      dep_vars <- model$variables$name[model$variables$group %in% name]
      if (length(dep_vars) > 0) deps$variables <- dep_vars
    }

    # DSA parameters
    if (length(model$dsa_parameters) > 0) {
      dep_dsa <- character(0)
      for (i in seq_along(model$dsa_parameters)) {
        if (identical(model$dsa_parameters[[i]]$group, name)) {
          dep_dsa <- c(dep_dsa, model$dsa_parameters[[i]]$name %||% paste0("param_", i))
        }
      }
      if (length(dep_dsa) > 0) deps$dsa_parameters <- dep_dsa
    }

    # Scenarios
    if (length(model$scenarios) > 0) {
      dep_scenarios <- list()
      for (i in seq_along(model$scenarios)) {
        if (length(model$scenarios[[i]]$variable_overrides) > 0) {
          ovr_names <- character(0)
          for (j in seq_along(model$scenarios[[i]]$variable_overrides)) {
            if (identical(model$scenarios[[i]]$variable_overrides[[j]]$group, name)) {
              ovr_names <- c(ovr_names, model$scenarios[[i]]$variable_overrides[[j]]$name %||% paste0("override_", j))
            }
          }
          if (length(ovr_names) > 0) {
            dep_scenarios[[model$scenarios[[i]]$name]] <- ovr_names
          }
        }
      }
      if (length(dep_scenarios) > 0) deps$scenarios <- dep_scenarios
    }

    # TWSA analyses
    if (length(model$twsa_analyses) > 0) {
      dep_twsa <- character(0)
      for (i in seq_along(model$twsa_analyses)) {
        if (length(model$twsa_analyses[[i]]$parameters) > 0) {
          for (j in seq_along(model$twsa_analyses[[i]]$parameters)) {
            if (identical(model$twsa_analyses[[i]]$parameters[[j]]$group, name)) {
              dep_twsa <- c(dep_twsa, model$twsa_analyses[[i]]$name)
              break
            }
          }
        }
      }
      if (length(dep_twsa) > 0) deps$twsa_analyses <- dep_twsa
    }

    # Override categories
    if (length(model$override_categories) > 0) {
      dep_overrides <- list()
      for (i in seq_along(model$override_categories)) {
        if (length(model$override_categories[[i]]$overrides) > 0) {
          ovr_titles <- character(0)
          for (j in seq_along(model$override_categories[[i]]$overrides)) {
            if (identical(model$override_categories[[i]]$overrides[[j]]$group, name)) {
              ovr_titles <- c(ovr_titles, model$override_categories[[i]]$overrides[[j]]$title %||%
                model$override_categories[[i]]$overrides[[j]]$name %||% paste0("override_", j))
            }
          }
          if (length(ovr_titles) > 0) {
            dep_overrides[[model$override_categories[[i]]$name]] <- ovr_titles
          }
        }
      }
      if (length(dep_overrides) > 0) deps$override_categories <- dep_overrides
    }

    # Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      dep_thresh <- character(0)
      for (i in seq_along(model$threshold_analyses)) {
        if (identical(model$threshold_analyses[[i]]$variable_group, name) ||
            (!is.null(model$threshold_analyses[[i]]$condition) &&
             identical(model$threshold_analyses[[i]]$condition$group, name))) {
          dep_thresh <- c(dep_thresh, model$threshold_analyses[[i]]$name %||% paste0("threshold_", i))
        }
      }
      if (length(dep_thresh) > 0) deps$threshold_analyses <- dep_thresh
    }

    # Multivariate sampling
    if (length(model$multivariate_sampling) > 0) {
      dep_mvs <- character(0)
      for (i in seq_along(model$multivariate_sampling)) {
        if (is.data.frame(model$multivariate_sampling[[i]]$variables) &&
            nrow(model$multivariate_sampling[[i]]$variables) > 0 &&
            any(model$multivariate_sampling[[i]]$variables$group %in% name)) {
          dep_mvs <- c(dep_mvs, model$multivariate_sampling[[i]]$name %||% paste0("mvs_", i))
        }
      }
      if (length(dep_mvs) > 0) deps$multivariate_sampling <- dep_mvs
    }

    deps <- Filter(function(x) length(x) > 0, deps)

    if (length(deps) > 0) {
      cond <- structure(
        class = c("group_has_dependencies", "error", "condition"),
        list(
          message = sprintf('Cannot remove group "%s": it has downstream dependencies.', name),
          dependencies = deps
        )
      )
      stop(cond)
    }
  }

  # Remove the group row
  model$groups <- model$groups[-match_idx, , drop = FALSE]

  # Cascade remove downstream references

  # 1. Variables
  if (is.data.frame(model$variables) && nrow(model$variables) > 0) {
    model$variables <- model$variables[!model$variables$group %in% name, , drop = FALSE]
  }

  # 2. DSA parameters
  if (length(model$dsa_parameters) > 0) {
    keep <- vapply(model$dsa_parameters, function(p) !identical(p$group, name), logical(1))
    model$dsa_parameters <- model$dsa_parameters[keep]
  }

  # 3. Scenarios -> variable_overrides
  if (length(model$scenarios) > 0) {
    for (i in seq_along(model$scenarios)) {
      if (length(model$scenarios[[i]]$variable_overrides) > 0) {
        keep <- vapply(model$scenarios[[i]]$variable_overrides,
                       function(o) !identical(o$group, name), logical(1))
        model$scenarios[[i]]$variable_overrides <- model$scenarios[[i]]$variable_overrides[keep]
      }
    }
  }

  # 4. TWSA analyses -> parameters (remove entire analysis if < 2 params remain)
  if (length(model$twsa_analyses) > 0) {
    for (i in seq_along(model$twsa_analyses)) {
      if (length(model$twsa_analyses[[i]]$parameters) > 0) {
        keep <- vapply(model$twsa_analyses[[i]]$parameters,
                       function(p) !identical(p$group, name), logical(1))
        model$twsa_analyses[[i]]$parameters <- model$twsa_analyses[[i]]$parameters[keep]
      }
    }
    keep_analyses <- vapply(model$twsa_analyses,
                            function(a) length(a$parameters) >= 2, logical(1))
    model$twsa_analyses <- model$twsa_analyses[keep_analyses]
  }

  # 5. Override categories -> overrides
  if (length(model$override_categories) > 0) {
    for (i in seq_along(model$override_categories)) {
      if (length(model$override_categories[[i]]$overrides) > 0) {
        keep <- vapply(model$override_categories[[i]]$overrides,
                       function(o) !identical(o$group, name), logical(1))
        model$override_categories[[i]]$overrides <- model$override_categories[[i]]$overrides[keep]
      }
    }
  }

  # 6. Threshold analyses
  if (length(model$threshold_analyses) > 0) {
    keep <- vapply(model$threshold_analyses, function(a) {
      !identical(a$variable_group, name) &&
        !((!is.null(a$condition)) && identical(a$condition$group, name))
    }, logical(1))
    model$threshold_analyses <- model$threshold_analyses[keep]
  }

  # 7. Multivariate sampling
  if (length(model$multivariate_sampling) > 0) {
    for (i in seq_along(model$multivariate_sampling)) {
      if (is.data.frame(model$multivariate_sampling[[i]]$variables) &&
          nrow(model$multivariate_sampling[[i]]$variables) > 0) {
        model$multivariate_sampling[[i]]$variables <-
          model$multivariate_sampling[[i]]$variables[
            !model$multivariate_sampling[[i]]$variables$group %in% name, , drop = FALSE
          ]
      }
    }
    keep_specs <- vapply(model$multivariate_sampling, function(s) {
      is.data.frame(s$variables) && nrow(s$variables) > 0
    }, logical(1))
    model$multivariate_sampling <- model$multivariate_sampling[keep_specs]
  }

  model
}

#' Add Summaries to Model
#'
#' Add one or more summaries to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the summary name
#' @param values Comma-separated string of value names to include
#' @param display_name Optional display name
#' @param description Optional description
#' @param type Summary type: "outcome" (default) for health outcomes or "cost" for costs.
#'   WTP can only be specified for outcome summaries.
#' @param wtp Optional willingness-to-pay value (numeric). Only valid for outcome summaries
#'   in net monetary benefit calculations. Must be NULL for cost summaries.
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_summary("total_cost", "cost1,cost2,cost3", type = "cost") |>
#'   add_summary("total_qalys", "qaly1,qaly2", type = "outcome", wtp = 50000)
add_summary <- function(model, name, values, display_name = NULL,
                       description = NULL, type = "outcome", wtp = NULL) {

  # Validate summary name
  if (missing(name) || !is.character(name) || length(name) != 1 || is.na(name) || trimws(name) == "") {
    stop("Summary name must be a non-empty character string.", call. = FALSE)
  }

  # Validate values string
  if (missing(values) || !is.character(values) || length(values) != 1 || is.na(values) || trimws(values) == "") {
    stop("Summary values must be a non-empty character string.", call. = FALSE)
  }

  # Validate type
  if (!type %in% c("outcome", "cost")) {
    stop("Summary type must be 'outcome' or 'cost'")
  }

  # Check for duplicate summary name
  if (!is.null(model$summaries) && is.data.frame(model$summaries) && nrow(model$summaries) > 0) {
    if (name %in% model$summaries$name) {
      stop(sprintf("Summary '%s' already exists. Use a unique name.", name), call. = FALSE)
    }
  }

  # Check for name collision with trees
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    if (name %in% unique(model$trees$name)) {
      stop(sprintf(
        'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
        name
      ), call. = FALSE)
    }
  }

  # Validate that WTP is not specified for cost summaries
  if (type == "cost" && !is.null(wtp)) {
    stop(sprintf("WTP cannot be specified for cost summary '%s'. WTP is only valid for outcome summaries.", name))
  }

  new_summary <- fast_tibble(
    name = name,
    values = values,
    display_name = display_name %||% name,
    description = description %||% display_name %||% name,
    type = type,
    wtp = wtp %||% NA_real_
  )

  model$summaries <- bind_rows(model$summaries, new_summary)
  model
}

#' Edit a Summary in the Model
#'
#' Modify an existing summary's properties or rename it. Only the fields that
#' are explicitly provided will be updated. If \code{new_name} is provided,
#' all downstream references to the summary are updated (threshold analyses
#' and VBP config).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the summary to edit
#' @param values Optional new comma-separated string of value names
#' @param display_name Optional new display name
#' @param description Optional new description
#' @param type Optional new type: "outcome" or "cost". Changing to "cost" will
#'   clear the WTP field.
#' @param wtp Optional new willingness-to-pay value. Cannot be set on cost summaries.
#' @param new_name Optional new name for the summary. If provided, the summary
#'   and all downstream references will be renamed.
#'
#' @return The modified model object
#'
#' @export
edit_summary <- function(model, name, values, display_name, description,
                         type, wtp, new_name) {
  match_idx <- which(model$summaries$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('Summary "%s" not found in model.', name), call. = FALSE)
  }

  if (!missing(values)) {
    model$summaries$values[match_idx] <- values
  }

  if (!missing(display_name)) {
    model$summaries$display_name[match_idx] <- display_name
  }

  if (!missing(description)) {
    model$summaries$description[match_idx] <- description
  }

  if (!missing(type)) {
    if (!type %in% c("outcome", "cost")) {
      stop("Summary type must be 'outcome' or 'cost'", call. = FALSE)
    }
    model$summaries$type[match_idx] <- type
    # Changing to cost clears WTP
    if (type == "cost") {
      model$summaries$wtp[match_idx] <- NA_real_
    }
  }

  if (!missing(wtp)) {
    # Determine current type (possibly just changed above)
    current_type <- model$summaries$type[match_idx]
    if (current_type == "cost") {
      stop(sprintf(
        "WTP cannot be specified for cost summary '%s'. WTP is only valid for outcome summaries.",
        name
      ), call. = FALSE)
    }
    model$summaries$wtp[match_idx] <- wtp %||% NA_real_
  }

  if (!missing(new_name)) {
    # Name collision check with trees
    if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
      if (new_name %in% unique(model$trees$name)) {
        stop(sprintf(
          'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
          new_name
        ), call. = FALSE)
      }
    }

    # Update the name
    model$summaries$name[match_idx] <- new_name

    # Cascade rename to threshold analyses
    if (length(model$threshold_analyses) > 0) {
      for (i in seq_along(model$threshold_analyses)) {
        cond <- model$threshold_analyses[[i]]$condition
        if (!is.null(cond)) {
          if (identical(cond$summary, name))
            model$threshold_analyses[[i]]$condition$summary <- new_name
          if (identical(cond$health_summary, name))
            model$threshold_analyses[[i]]$condition$health_summary <- new_name
          if (identical(cond$cost_summary, name))
            model$threshold_analyses[[i]]$condition$cost_summary <- new_name
        }
      }
    }

    # Cascade rename to VBP config
    if (!is.null(model$vbp)) {
      if (identical(model$vbp$outcome_summary, name))
        model$vbp$outcome_summary <- new_name
      if (identical(model$vbp$cost_summary, name))
        model$vbp$cost_summary <- new_name
    }
  }

  model
}

#' Remove a Summary from the Model
#'
#' Remove an existing summary and optionally cascade-remove all downstream
#' references (threshold analyses and VBP config).
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the summary to remove
#' @param error_on_dependencies Logical. If \code{TRUE} and the summary has
#'   downstream dependencies, an error of class \code{"summary_has_dependencies"}
#'   is thrown instead of removing anything. The error condition contains a
#'   \code{dependencies} field with a named list of affected components.
#'   Default is \code{FALSE}, which silently removes the summary and all
#'   downstream references.
#'
#' @return The modified model object
#'
#' @export
remove_summary <- function(model, name, error_on_dependencies = FALSE) {
  match_idx <- which(model$summaries$name == name)
  if (length(match_idx) == 0) {
    stop(sprintf('Summary "%s" not found in model.', name), call. = FALSE)
  }

  if (error_on_dependencies) {
    deps <- list()

    # Threshold analyses
    if (length(model$threshold_analyses) > 0) {
      dep_thresh <- character(0)
      for (i in seq_along(model$threshold_analyses)) {
        cond <- model$threshold_analyses[[i]]$condition
        if (!is.null(cond)) {
          if (identical(cond$summary, name) ||
              identical(cond$health_summary, name) ||
              identical(cond$cost_summary, name)) {
            dep_thresh <- c(dep_thresh, model$threshold_analyses[[i]]$name %||% paste0("threshold_", i))
          }
        }
      }
      if (length(dep_thresh) > 0) deps$threshold_analyses <- dep_thresh
    }

    # VBP config
    if (!is.null(model$vbp)) {
      vbp_refs <- character(0)
      if (identical(model$vbp$outcome_summary, name)) vbp_refs <- c(vbp_refs, "outcome_summary")
      if (identical(model$vbp$cost_summary, name)) vbp_refs <- c(vbp_refs, "cost_summary")
      if (length(vbp_refs) > 0) deps$vbp <- vbp_refs
    }

    deps <- Filter(function(x) length(x) > 0, deps)

    if (length(deps) > 0) {
      cond <- structure(
        class = c("summary_has_dependencies", "error", "condition"),
        list(
          message = sprintf('Cannot remove summary "%s": it has downstream dependencies.', name),
          dependencies = deps
        )
      )
      stop(cond)
    }
  }

  # Remove the summary row
  model$summaries <- model$summaries[-match_idx, , drop = FALSE]

  # Cascade remove threshold analyses that reference this summary
  if (length(model$threshold_analyses) > 0) {
    keep <- vapply(model$threshold_analyses, function(a) {
      cond <- a$condition
      if (is.null(cond)) return(TRUE)
      !(identical(cond$summary, name) ||
        identical(cond$health_summary, name) ||
        identical(cond$cost_summary, name))
    }, logical(1))
    model$threshold_analyses <- model$threshold_analyses[keep]
  }

  # Clear VBP config references
  if (!is.null(model$vbp)) {
    if (identical(model$vbp$outcome_summary, name))
      model$vbp$outcome_summary <- ""
    if (identical(model$vbp$cost_summary, name))
      model$vbp$cost_summary <- ""
  }

  model
}

#' Add Table to Model
#'
#' Add a data table to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the table name
#' @param data A data frame containing the table data
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_table("costs", data.frame(state = c("A", "B"), cost = c(100, 200)))
add_table <- function(model, name, data, description = NULL) {
  # Validate table name
  if (missing(name) || !is.character(name) || length(name) != 1 || is.na(name) || trimws(name) == "") {
    stop("Table name must be a non-empty character string.", call. = FALSE)
  }

  # Check for name collision with trees
  if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
    if (name %in% unique(model$trees$name)) {
      stop(sprintf(
        'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
        name
      ), call. = FALSE)
    }
  }
  # Check for name collision with values
  if (is.data.frame(model$values) && nrow(model$values) > 0 && name %in% model$values$name) {
    stop(sprintf(
      "Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.",
      name
    ))
  }
  # Store as structured list with data and optional description
  model$tables[[name]] <- list(
    data = data,
    description = description
  )
  model
}

#' Edit a Table in the Model
#'
#' Modify an existing table's data, description, or name. Only the fields that
#' are explicitly provided will be updated.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the table to edit
#' @param data A data frame to replace the table data with
#' @param description Optional new description
#' @param new_name Optional new name for the table. If provided, the table
#'   will be renamed. Must not collide with existing decision tree or value
#'   names.
#'
#' @return The modified model object
#'
#' @export
edit_table <- function(model, name, data, description, new_name) {
  if (is.null(model$tables[[name]])) {
    stop(sprintf('Table "%s" not found in model.', name), call. = FALSE)
  }

  if (!missing(data)) {
    model$tables[[name]]$data <- data
  }

  if (!missing(description)) {
    model$tables[[name]]$description <- description
  }

  if (!missing(new_name)) {
    # Check for name collision with trees
    if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
      if (new_name %in% unique(model$trees$name)) {
        stop(sprintf(
          'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.',
          new_name
        ), call. = FALSE)
      }
    }
    # Check for name collision with values
    if (is.data.frame(model$values) && nrow(model$values) > 0 && new_name %in% model$values$name) {
      stop(sprintf(
        "Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.",
        new_name
      ))
    }
    idx <- which(names(model$tables) == name)
    names(model$tables)[idx] <- new_name
  }

  model
}

#' Remove a Table from the Model
#'
#' Remove an existing table from the model by name.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the table to remove
#'
#' @return The modified model object
#'
#' @export
remove_table <- function(model, name) {
  if (is.null(model$tables[[name]])) {
    stop(sprintf('Table "%s" not found in model.', name), call. = FALSE)
  }
  model$tables[[name]] <- NULL
  model
}

#' Add Script to Model
#'
#' Add an R script to the model.
#'
#' @param model A oq_model_builder object
#' @param name Character string for the script name
#' @param code Character string containing the R code
#' @param description Optional description
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' model <- define_model("markov") |>
#'   add_script("preprocess", "# Data preprocessing\nlibrary(dplyr)")
add_script <- function(model, name, code, description = NULL) {
  # Store as structured list with code and optional description
  model$scripts[[name]] <- list(
    code = code,
    description = description
  )
  model
}

#' Edit a Script in the Model
#'
#' Modify an existing script's code, description, or name. Only the fields that
#' are explicitly provided will be updated.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the script to edit
#' @param code Character string containing the new R code
#' @param description Optional new description
#' @param new_name Optional new name for the script. If provided, the script
#'   will be renamed.
#'
#' @return The modified model object
#'
#' @export
edit_script <- function(model, name, code, description, new_name) {
  if (is.null(model$scripts[[name]])) {
    stop(sprintf('Script "%s" not found in model.', name), call. = FALSE)
  }

  if (!missing(code)) {
    model$scripts[[name]]$code <- code
  }

  if (!missing(description)) {
    model$scripts[[name]]$description <- description
  }

  if (!missing(new_name)) {
    idx <- which(names(model$scripts) == name)
    names(model$scripts)[idx] <- new_name
  }

  model
}

#' Remove a Script from the Model
#'
#' Remove an existing script from the model by name.
#'
#' @param model A oq_model_builder object
#' @param name Character string identifying the script to remove
#'
#' @return The modified model object
#'
#' @export
remove_script <- function(model, name) {
  if (is.null(model$scripts[[name]])) {
    stop(sprintf('Script "%s" not found in model.', name), call. = FALSE)
  }
  model$scripts[[name]] <- NULL
  model
}

#' Convert Model to JSON String
#'
#' Convert a openqaly model object to a JSON string representation.
#'
#' @param model A openqaly model object
#'
#' @return A JSON string
#'
#' @export
as_json <- function(model) {
  # Use existing function
  write_model_json(model)
}

#' Convert Model to R Code String
#'
#' Convert a openqaly model object to executable R code.
#'
#' @param model A openqaly model object
#'
#' @return A character vector of R code lines
#'
#' @export
as_r_code <- function(model) {
  # This will call the code generation function from model_codegen.R
  model_to_r_code(model)
}

#' Add Multivariate Sampling Specification to Model
#'
#' Define a multivariate distribution for sampling correlated parameters.
#' This is used for Probabilistic Sensitivity Analysis (PSA) when multiple
#' parameters need to be sampled together with a specified correlation structure.
#'
#' @param model A oq_model_builder object
#' @param name Character string naming this sampling specification
#' @param distribution Expression defining the distribution function (uses NSE).
#'   The distribution should return a function(n) that generates n × k samples.
#'   Can reference any evaluated variables including base case values of sampled variables.
#'   For backward compatibility, can also be a character string.
#' @param variables Character vector of variable names to be sampled together,
#'   or a tibble with columns: variable, strategy, group (for segment-specific sampling)
#' @param description Optional description of this sampling specification
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' # Dirichlet distribution for transition probabilities
#' model <- define_model("markov") |>
#'   add_multivariate_sampling(
#'     name = "transition_probs",
#'     distribution = dirichlet(c(alpha_stable, alpha_progression, alpha_death)),
#'     variables = c("p_stable", "p_progression", "p_death"),
#'     description = "Transition probabilities from sick state"
#'   )
#'
#' # Multivariate normal for correlated cost and effectiveness
#' model <- model |>
#'   add_multivariate_sampling(
#'     name = "cost_qaly_correlation",
#'     distribution = mvnormal(mean = c(bc_cost, bc_qaly), sd = c(se_cost, se_qaly), cor = 0.6),
#'     variables = c("cost_treatment", "qaly_treatment")
#'   )
#'
#' # Segment-specific sampling with tibble specification
#' model <- model |>
#'   add_multivariate_sampling(
#'     name = "strategy_specific",
#'     distribution = mvnormal(mean = c(cost_mean, qaly_mean), sd = c(cost_sd, qaly_sd), cor = 0.5),
#'     variables = tibble(
#'       variable = c("treatment_cost", "treatment_qaly"),
#'       strategy = c("intervention", "intervention"),
#'       group = c(NA, NA)
#'     )
#'   )
#' }
add_multivariate_sampling <- function(model, name, distribution, variables, description = NULL) {

  # Capture the distribution expression using NSE
  distribution_quo <- enquo(distribution)
  distribution_expr <- quo_get_expr(distribution_quo)

  # Handle distribution: if already a string, use as-is (backward compatibility)
  # If an expression/call, convert to string using NSE
  if (is.character(distribution_expr)) {
    distribution_str <- distribution_expr
  } else {
    distribution_str <- expr_text(distribution_expr)
  }

  # Convert variables to tibble format
  if (is.character(variables)) {
    # Simple character vector -> tibble with just variable names
    variables_df <- fast_tibble(
      variable = variables,
      strategy = NA_character_,
      group = NA_character_
    )
  } else if (is.data.frame(variables)) {
    # Already a dataframe/tibble
    variables_df <- as_tibble(variables)
    # Ensure required columns exist
    if (!"variable" %in% names(variables_df)) {
      stop("variables tibble must have a 'variable' column")
    }
    # Add missing columns with NA
    if (!"strategy" %in% names(variables_df)) {
      variables_df$strategy <- NA_character_
    }
    if (!"group" %in% names(variables_df)) {
      variables_df$group <- NA_character_
    }
    # Select only the required columns in the correct order
    variables_df <- variables_df %>% select("variable", "strategy", "group")
  } else {
    stop("variables must be either a character vector or a tibble/data.frame")
  }

  # Initialize multivariate_sampling if it doesn't exist
  if (is.null(model$multivariate_sampling)) {
    model$multivariate_sampling <- list()
  }

  # Create the sampling specification
  new_spec <- list(
    name = name,
    distribution = distribution_str,
    description = description %||% "",
    variables = variables_df
  )

  # Add to model
  model$multivariate_sampling <- c(model$multivariate_sampling, list(new_spec))

  model
}

#' Validate Variable Targeting for Sensitivity Analyses
#'
#' Checks if a variable requires strategy and/or group specification when used
#' in sensitivity analyses. If the variable is defined with specific strategies
#' or groups, this validation ensures the user explicitly specifies which
#' strategy/group to target.
#'
#' @param model A model object (oq_model_builder or parsed)
#' @param variable Character string naming the variable
#' @param strategy Strategy specification (empty string if not provided)
#' @param group Group specification (empty string if not provided)
#' @param analysis_type Character string for error message (e.g., "DSA", "TWSA")
#' @param add_function_name Character string for example in error message
#'
#' @return Invisible NULL if valid, stops with error if invalid
#' @keywords internal
validate_variable_targeting <- function(model, variable, strategy, group,
                                         analysis_type, add_function_name) {
  matching_vars <- model$variables[model$variables$name == variable, ]
  if (nrow(matching_vars) == 0) return(invisible(NULL))

  # Check group-specific
  non_na_groups <- matching_vars$group[!is.na(matching_vars$group)]
  has_group_specific <- length(non_na_groups) > 0 && any(non_na_groups != "")
  if (has_group_specific && group == "") {
    defined_groups <- unique(non_na_groups[non_na_groups != ""])
    stop(sprintf(
      paste0(
        "Variable '%s' is defined for specific group(s): %s\n",
        "You must specify which group to vary using the 'group' parameter.\n",
        "Example: %s(\"%s\", ..., group = \"%s\")"
      ),
      variable,
      paste(defined_groups, collapse = ", "),
      add_function_name,
      variable,
      defined_groups[1]
    ), call. = FALSE)
  }

  # Check strategy-specific
  non_na_strategies <- matching_vars$strategy[!is.na(matching_vars$strategy)]
  has_strategy_specific <- length(non_na_strategies) > 0 && any(non_na_strategies != "")
  if (has_strategy_specific && strategy == "") {
    defined_strategies <- unique(non_na_strategies[non_na_strategies != ""])
    stop(sprintf(
      paste0(
        "Variable '%s' is defined for specific strategy(ies): %s\n",
        "You must specify which strategy to vary using the 'strategy' parameter.\n",
        "Example: %s(\"%s\", ..., strategy = \"%s\")"
      ),
      variable,
      paste(defined_strategies, collapse = ", "),
      add_function_name,
      variable,
      defined_strategies[1]
    ), call. = FALSE)
  }

  invisible(NULL)
}

#' Add Deterministic Sensitivity Analysis Variable to Model
#'
#' Define a variable to include in deterministic sensitivity analysis (DSA).
#' DSA will run the model with this variable set to its low and high values
#' to assess parameter sensitivity.
#'
#' The low and high bounds can be specified as:
#' - Literal numeric values (e.g., 0.01, 0.05)
#' - Expressions using the `bc` keyword to reference the base case value (e.g., bc * 0.5)
#' - Expressions referencing other model variables (e.g., bc - 2 * cost_se)
#'
#' **Display Names**: DSA parameters automatically inherit display names from their variables.
#' Since variables auto-generate unique display names for strategy/group combinations
#' (e.g., "cost, treatment_a"), DSA parameters will also have unique names. You can optionally
#' override with custom `display_name` values in either `add_variable()` or `add_dsa_variable()`.
#'
#' @param model A oq_model_builder object
#' @param variable Character string naming the variable to vary (must exist in model$variables)
#' @param low Expression or numeric value for the low bound of the sensitivity range.
#'   Can use `bc` keyword to reference the base case value and other model variables.
#' @param high Expression or numeric value for the high bound of the sensitivity range.
#'   Can use `bc` keyword to reference the base case value and other model variables.
#' @param strategy Optional strategy name to limit DSA to specific strategy
#' @param group Optional group name to limit DSA to specific group
#' @param display_name Optional display name for plots and tables. If not provided,
#'   inherits from the variable definition. Required to be unique across all DSA parameters.
#' @param range_label Optional custom text for the input range portion of tornado labels.
#'   When provided, labels show "Parameter Name (range_label)" instead of auto-generated
#'   "Parameter Name (low - high)". For example, use "Base Case ± 20\%" or "±50\%".
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Literal values (backward compatible)
#' model <- define_model("markov") |>
#'   add_variable("p_disease", 0.03) |>
#'   add_dsa_variable("p_disease", low = 0.01, high = 0.05)
#'
#' # Example 2: Relative to base case using bc keyword
#' model <- define_model("markov") |>
#'   add_variable("p_disease", 0.03) |>
#'   add_dsa_variable("p_disease",
#'                    low = bc * 0.5,    # 50% of base case
#'                    high = bc * 1.5)   # 150% of base case
#'
#' # Example 3: Using other variables (standard error pattern)
#' model <- define_model("markov") |>
#'   add_variable("cost_tx", 1000) |>
#'   add_variable("cost_tx_se", 100) |>
#'   add_dsa_variable("cost_tx",
#'                    low = bc - 2 * cost_tx_se,  # Base - 2 SE
#'                    high = bc + 2 * cost_tx_se) # Base + 2 SE
#'
#' # Example 4: Strategy-specific DSA with auto-generated display names
#' model <- define_model("markov") |>
#'   add_variable("p_prog", strategy = "treatment_a", formula = 0.3) |>
#'   add_variable("p_prog", strategy = "treatment_b", formula = 0.2) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_a", low = 0.2, high = 0.4) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_b", low = 0.1, high = 0.3)
#' # Auto-generates: "p_prog, treatment_a" and "p_prog, treatment_b"
#'
#' # Example 5: Override auto-generated names with custom display names
#' model <- define_model("markov") |>
#'   add_variable("p_prog", display_name = "Treatment A Progression",
#'                strategy = "treatment_a", formula = 0.3) |>
#'   add_variable("p_prog", display_name = "Treatment B Progression",
#'                strategy = "treatment_b", formula = 0.2) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_a", low = 0.2, high = 0.4) |>
#'   add_dsa_variable("p_prog", strategy = "treatment_b", low = 0.1, high = 0.3)
#' }
add_dsa_variable <- function(model, variable, low, high,
                             strategy = "", group = "",
                             display_name = NULL,
                             range_label = NULL) {

  # Validate variable name
  if (!is.character(variable) || length(variable) != 1 || is.na(variable) || nchar(trimws(variable)) == 0) {
    stop("variable must be a non-empty character string", call. = FALSE)
  }

  # Validate strategy/group targeting for strategy/group-specific variables
  validate_variable_targeting(model, variable, strategy, group,
                              "DSA", "add_dsa_variable")

  # Check for existing DSA parameter with same name/strategy/group and replace if found
  if (length(model$dsa_parameters) > 0) {
    for (i in seq_along(model$dsa_parameters)) {
      existing <- model$dsa_parameters[[i]]
      if (existing$type == "variable" &&
          existing$name == variable &&
          existing$strategy == as.character(strategy) &&
          existing$group == as.character(group)) {
        warning(sprintf(
          "Replacing existing DSA specification for variable '%s'%s%s",
          variable,
          if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
          if (group != "") paste0(" (group: ", group, ")") else ""
        ), call. = FALSE)
        model$dsa_parameters <- model$dsa_parameters[-i]
        break
      }
    }
  }

  # Capture low expression using NSE
  low_quo <- enquo(low)
  low_expr <- quo_get_expr(low_quo)

  # Convert to oq_formula object for evaluation later
  if (is.numeric(low_expr)) {
    low_formula <- as.oq_formula(as.character(low_expr))
  } else {
    low_formula <- as.oq_formula(expr_text(low_expr))
  }

  # Capture high expression using NSE
  high_quo <- enquo(high)
  high_expr <- quo_get_expr(high_quo)

  if (is.numeric(high_expr)) {
    high_formula <- as.oq_formula(as.character(high_expr))
  } else {
    high_formula <- as.oq_formula(expr_text(high_expr))
  }

  # Create new DSA parameter specification as a list
  new_param <- list(
    type = "variable",
    name = variable,
    low = low_formula,      # Store as oq_formula object
    high = high_formula,    # Store as oq_formula object
    strategy = as.character(strategy),
    group = as.character(group),
    display_name = display_name,
    range_label = range_label
  )

  # Add to model's dsa_parameters list
  model$dsa_parameters <- c(model$dsa_parameters, list(new_param))
  class(model$dsa_parameters) <- "dsa_parameters"

  model
}

#' Add Deterministic Sensitivity Analysis Setting to Model
#'
#' Define a model setting to include in deterministic sensitivity analysis (DSA).
#' Settings include parameters like discount rates, timeframe, and other global
#' model configuration values that are not part of the variables table.
#'
#' @param model A oq_model_builder object
#' @param setting Character string naming the setting to vary
#'   (e.g., "discount_cost", "discount_outcomes", "timeframe")
#' @param low Value for the low bound (numeric or character depending on setting)
#' @param high Value for the high bound (numeric or character depending on setting)
#' @param display_name Optional display name for plots and tables
#' @param range_label Optional custom text for the input range portion of tornado labels.
#'   When provided, labels show "Parameter Name (range_label)" instead of auto-generated
#'   "Parameter Name (low - high)". For example, use "Base Case ± 20\%" or "±50\%".
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_dsa_setting("discount_cost", low = 0, high = 5) |>
#'   add_dsa_setting("timeframe", low = 10, high = 30)
#' }
add_dsa_setting <- function(model, setting, low, high,
                            display_name = NULL,
                            range_label = NULL) {

  # Validate inputs
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string")
  }

  # Check for existing DSA parameter with same setting name and replace if found
  if (length(model$dsa_parameters) > 0) {
    for (i in seq_along(model$dsa_parameters)) {
      existing <- model$dsa_parameters[[i]]
      if (existing$type == "setting" && existing$name == setting) {
        warning(sprintf(
          "Replacing existing DSA specification for setting '%s'",
          setting
        ), call. = FALSE)
        model$dsa_parameters <- model$dsa_parameters[-i]
        break
      }
    }
  }

  # Create new DSA parameter specification as a list
  # Store low/high as literal values (not formulas)
  new_param <- list(
    type = "setting",
    name = setting,
    low = low,           # Store as literal value
    high = high,         # Store as literal value
    display_name = display_name %||% setting,
    range_label = range_label
  )

  # Add to model's dsa_parameters list
  model$dsa_parameters <- c(model$dsa_parameters, list(new_param))
  class(model$dsa_parameters) <- "dsa_parameters"

  model
}

#' Print DSA Parameters
#'
#' Print method for dsa_parameters objects
#'
#' @param x A dsa_parameters object (list)
#' @param ... Additional arguments (unused)
#'
#' @export
print.dsa_parameters <- function(x, ...) {
  if (length(x) == 0) {
    cat("No DSA parameters defined\n")
    return(invisible(x))
  }

  cat("DSA Parameters (", length(x), "):\n", sep = "")
  for (i in seq_along(x)) {
    param <- x[[i]]
    display <- if (param$type == "variable") {
      # Variables use their own display_name from the model
      param$name
    } else {
      # Settings have display_name in the parameter
      param$display_name
    }
    cat(sprintf("  %d. [%s] %s\n", i, param$type, display))
  }
  invisible(x)
}

#' Add a Scenario to Model
#'
#' Define a named scenario for scenario analysis. Scenarios allow you to define
#' alternate sets of parameter values and model settings to test different
#' assumptions. Each scenario is given a name and optional description, and
#' variable/setting overrides can be added using `add_scenario_variable()` and
#' `add_scenario_setting()`.
#'
#' A "Base Case" scenario is automatically created when running scenario analysis,
#' representing the model with default parameters. User-defined scenarios are
#' compared against this base case.
#'
#' @param model An oq_model_builder object
#' @param name Unique name for the scenario (e.g., "Optimistic", "Pessimistic")
#' @param description Optional description explaining the scenario assumptions
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_scenario("Optimistic", description = "Best case assumptions") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   add_scenario_variable("Optimistic", "cost", 4000) |>
#'   add_scenario("Pessimistic", description = "Conservative assumptions") |>
#'   add_scenario_variable("Pessimistic", "efficacy", 0.70)
#' }
add_scenario <- function(model, name, description = NULL) {
  # Validate inputs

if (!is.character(name) || length(name) != 1 || name == "") {
    stop("Scenario name must be a non-empty character string", call. = FALSE)
  }

  # Check for reserved name "Base Case"
  if (tolower(name) == "base case") {
    stop("'Base Case' is a reserved scenario name that is automatically created",
         call. = FALSE)
  }

  # Initialize scenarios list if NULL
  if (is.null(model$scenarios)) {
    model$scenarios <- list()
  }

  # Check for duplicate names
  existing_names <- sapply(model$scenarios, function(s) s$name)
  if (name %in% existing_names) {
    stop(sprintf("Scenario '%s' already exists. Use a unique name.", name),
         call. = FALSE)
  }

  # Create new scenario
  new_scenario <- list(
    name = name,
    description = description %||% name,
    variable_overrides = list(),
    setting_overrides = list()
  )

  model$scenarios <- c(model$scenarios, list(new_scenario))
  model
}

#' Add Variable Override to Scenario
#'
#' Add a variable value override for a specific scenario. When the scenario
#' analysis runs this scenario, the specified variable will use the override
#' value instead of its default formula.
#'
#' Values can be specified as:
#' - Literal numeric values (e.g., 0.95, 5000)
#' - Expressions using NSE (e.g., base_efficacy * 1.2)
#'
#' @param model An oq_model_builder object
#' @param scenario Name of the scenario to add the override to (must exist)
#' @param variable Name of the variable to override (must exist in model$variables)
#' @param value Value expression for the override (uses NSE)
#' @param strategy Optional strategy name to limit override to specific strategy
#' @param group Optional group name to limit override to specific group
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("efficacy", 0.8) |>
#'   add_variable("cost", 5000) |>
#'   add_scenario("Optimistic") |>
#'   add_scenario_variable("Optimistic", "efficacy", 0.95) |>
#'   add_scenario_variable("Optimistic", "cost", 4000)
#' }
add_scenario_variable <- function(model, scenario, variable, value,
                                   strategy = "", group = "") {
  # Validate inputs
  if (!is.character(scenario) || length(scenario) != 1) {
    stop("scenario must be a single character string", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string", call. = FALSE)
  }

  # Validate strategy/group targeting for strategy/group-specific variables
  validate_variable_targeting(model, variable, strategy, group,
                              "Scenario", "add_scenario_variable")

  # Find scenario index
  scenario_idx <- which(sapply(model$scenarios, function(s) s$name) == scenario)
  if (length(scenario_idx) == 0) {
    stop(sprintf("Scenario '%s' not found. Use add_scenario() first.", scenario),
         call. = FALSE)
  }

  # Capture value with NSE
  value_quo <- enquo(value)
  value_expr <- quo_get_expr(value_quo)

  # Store as literal numeric or oq_formula depending on type
  if (is.numeric(value_expr)) {
    stored_value <- value_expr
  } else {
    stored_value <- as.oq_formula(expr_text(value_expr))
  }

  # Create override entry
  override <- list(
    name = variable,
    value = stored_value,
    strategy = as.character(strategy),
    group = as.character(group)
  )

  # Add to scenario's variable_overrides
  model$scenarios[[scenario_idx]]$variable_overrides <- c(
    model$scenarios[[scenario_idx]]$variable_overrides,
    list(override)
  )

  model
}

#' Add Setting Override to Scenario
#'
#' Add a model setting override for a specific scenario. When the scenario
#' analysis runs this scenario, the specified setting will use the override
#' value instead of its default.
#'
#' Common settings that can be overridden include:
#' - `timeframe`: Model time horizon
#' - `discount_cost`: Discount rate for costs (percentage, e.g. 3 for 3%)
#' - `discount_outcomes`: Discount rate for outcomes (percentage, e.g. 3 for 3%)
#' - `cycle_length`: Length of each model cycle
#'
#' @param model An oq_model_builder object
#' @param scenario Name of the scenario to add the override to (must exist)
#' @param setting Name of the setting to override
#' @param value Value for the setting override
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_scenario("Extended Horizon") |>
#'   add_scenario_setting("Extended Horizon", "timeframe", 30) |>
#'   add_scenario("No Discounting") |>
#'   add_scenario_setting("No Discounting", "discount_cost", 0) |>
#'   add_scenario_setting("No Discounting", "discount_outcomes", 0)
#' }
add_scenario_setting <- function(model, scenario, setting, value) {
  # Validate inputs
  if (!is.character(scenario) || length(scenario) != 1) {
    stop("scenario must be a single character string", call. = FALSE)
  }
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string", call. = FALSE)
  }

  # Find scenario index
  scenario_idx <- which(sapply(model$scenarios, function(s) s$name) == scenario)
  if (length(scenario_idx) == 0) {
    stop(sprintf("Scenario '%s' not found. Use add_scenario() first.", scenario),
         call. = FALSE)
  }

  # Create override entry
  override <- list(
    name = setting,
    value = value
  )

  # Add to scenario's setting_overrides
  model$scenarios[[scenario_idx]]$setting_overrides <- c(
    model$scenarios[[scenario_idx]]$setting_overrides,
    list(override)
  )

  model
}

#' Print Scenarios
#'
#' Print method for displaying scenarios defined in a model
#'
#' @param model An oq_model_builder object with scenarios
#'
#' @return Invisible model
#' @keywords internal
print_scenarios <- function(model) {
  if (is.null(model$scenarios) || length(model$scenarios) == 0) {
    cat("No scenarios defined\n")
    return(invisible(model))
  }

  cat("Scenarios (", length(model$scenarios), "):\n", sep = "")
  for (i in seq_along(model$scenarios)) {
    scenario <- model$scenarios[[i]]
    cat(sprintf("  %d. %s\n", i, scenario$name))
    if (scenario$description != scenario$name) {
      cat(sprintf("     Description: %s\n", scenario$description))
    }
    n_vars <- length(scenario$variable_overrides)
    n_settings <- length(scenario$setting_overrides)
    cat(sprintf("     Overrides: %d variable(s), %d setting(s)\n", n_vars, n_settings))
  }
  invisible(model)
}

#' Add a Two-Way Sensitivity Analysis to Model
#'
#' Define a named two-way sensitivity analysis (2WSA) that varies two parameters
#' simultaneously across a grid of values. Each 2WSA must have exactly two
#' variable or setting specifications added via `add_twsa_variable()` or
#' `add_twsa_setting()`.
#'
#' @param model An oq_model_builder object
#' @param name Unique name for the 2WSA analysis (e.g., "Cost vs Efficacy")
#' @param description Optional description explaining what this analysis explores
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_twsa("Cost vs Efficacy") |>
#'   add_twsa_variable("Cost vs Efficacy", "cost_tx",
#'     type = "range", min = 1000, max = 3000, steps = 5) |>
#'   add_twsa_variable("Cost vs Efficacy", "efficacy",
#'     type = "radius", radius = 0.1, steps = 5)
#' }
add_twsa <- function(model, name, description = NULL) {
  # Validate inputs
  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("TWSA name must be a non-empty character string", call. = FALSE)
  }

  # Check for reserved name "Base Case"
  if (tolower(name) == "base case") {
    stop("'Base Case' is a reserved name that cannot be used for TWSA analyses",
         call. = FALSE)
  }

  # Initialize twsa_analyses list if NULL
  if (is.null(model$twsa_analyses)) {
    model$twsa_analyses <- list()
  }

  # Check for duplicate names
  existing_names <- sapply(model$twsa_analyses, function(s) s$name)
  if (name %in% existing_names) {
    stop(sprintf("TWSA analysis '%s' already exists. Use a unique name.", name),
         call. = FALSE)
  }

  # Create new TWSA analysis
  new_twsa <- list(
    name = name,
    description = description %||% name,
    parameters = list()  # Will hold exactly 2 variable/setting specs
  )

  model$twsa_analyses <- c(model$twsa_analyses, list(new_twsa))
  model
}

#' Add Variable to Two-Way Sensitivity Analysis
#'
#' Add a variable to vary in a two-way sensitivity analysis. Each TWSA must have
#' exactly two parameters (variables or settings). Values can be specified using
#' three different range types:
#'
#' - `type = "range"`: Vary from `min` to `max` in `steps` evenly-spaced values
#' - `type = "radius"`: Vary from `bc - radius` to `bc + radius` where `bc` is
#'   the base case value, creating `steps * 2 + 1` values centered on the base case
#' - `type = "custom"`: Use explicit values provided as a numeric vector
#'
#' @param model An oq_model_builder object
#' @param twsa_name Name of the TWSA analysis to add the variable to (must exist)
#' @param variable Name of the variable to vary (must exist in model$variables)
#' @param type Type of range specification: "range", "radius", or "custom"
#' @param min For type="range": minimum value (can use NSE expressions)
#' @param max For type="range": maximum value (can use NSE expressions)
#' @param radius For type="radius": distance from base case (can use NSE)
#' @param steps For type="range" or "radius": number of steps
#' @param values For type="custom": numeric vector of values to use
#' @param strategy Optional strategy name to limit the variable to specific strategy
#' @param group Optional group name to limit the variable to specific group
#' @param display_name Optional display name for plots and tables
#' @param include_base_case Logical. If TRUE (default), ensures the base case
#'   value is included in the parameter grid. If the base case value is already
#'   present in the values, no duplicate is added.
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   add_variable("cost_tx", 1000) |>
#'   add_variable("efficacy", 0.8) |>
#'   add_twsa("Cost vs Efficacy") |>
#'   # Using range type
#'   add_twsa_variable("Cost vs Efficacy", "cost_tx",
#'     type = "range", min = 500, max = 1500, steps = 5) |>
#'   # Using radius type (varies around base case)
#'   add_twsa_variable("Cost vs Efficacy", "efficacy",
#'     type = "radius", radius = 0.1, steps = 3)
#'
#' # Using custom values
#' model <- define_model("markov") |>
#'   add_variable("discount_rate", 0.03) |>
#'   add_twsa("Discount Analysis") |>
#'   add_twsa_variable("Discount Analysis", "discount_rate",
#'     type = "custom", values = c(0, 0.015, 0.03, 0.05))
#' }
add_twsa_variable <- function(model, twsa_name, variable, type,
                               min = NULL, max = NULL,
                               radius = NULL, steps = NULL,
                               values = NULL,
                               strategy = "", group = "",
                               display_name = NULL,
                               include_base_case = TRUE) {
  # Validate inputs
  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a single character string", call. = FALSE)
  }

  # Validate strategy/group targeting for strategy/group-specific variables
  validate_variable_targeting(model, variable, strategy, group,
                              "TWSA", "add_twsa_variable")

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found. Use add_twsa() first.", twsa_name),
         call. = FALSE)
  }

  # Check if TWSA already has 2 parameters
  current_params <- length(model$twsa_analyses[[twsa_idx]]$parameters)
  if (current_params >= 2) {
    stop(sprintf(
      "TWSA analysis '%s' already has 2 parameters. Each TWSA must have exactly 2 parameters.",
      twsa_name
    ), call. = FALSE)
  }

  # Validate type
  type <- match.arg(type, c("range", "radius", "custom"))

  # Validate type-specific parameters
  if (type == "range") {
    if (is.null(substitute(min)) || is.null(substitute(max)) || is.null(steps)) {
      stop("For type='range', min, max, and steps are required", call. = FALSE)
    }
  } else if (type == "radius") {
    if (is.null(substitute(radius)) || is.null(steps)) {
      stop("For type='radius', radius and steps are required", call. = FALSE)
    }
  } else if (type == "custom") {
    if (is.null(substitute(values))) {
      stop("For type='custom', values is required", call. = FALSE)
    }
  }

  # Capture expressions with NSE
  min_formula <- NULL
  max_formula <- NULL
  radius_formula <- NULL
  values_formula <- NULL

  if (type == "range") {
    min_quo <- enquo(min)
    min_expr <- quo_get_expr(min_quo)
    if (is.numeric(min_expr)) {
      min_formula <- as.oq_formula(as.character(min_expr))
    } else {
      min_formula <- as.oq_formula(expr_text(min_expr))
    }

    max_quo <- enquo(max)
    max_expr <- quo_get_expr(max_quo)
    if (is.numeric(max_expr)) {
      max_formula <- as.oq_formula(as.character(max_expr))
    } else {
      max_formula <- as.oq_formula(expr_text(max_expr))
    }
  } else if (type == "radius") {
    radius_quo <- enquo(radius)
    radius_expr <- quo_get_expr(radius_quo)
    if (is.numeric(radius_expr)) {
      radius_formula <- as.oq_formula(as.character(radius_expr))
    } else {
      radius_formula <- as.oq_formula(expr_text(radius_expr))
    }
  } else if (type == "custom") {
    values_quo <- enquo(values)
    values_expr <- quo_get_expr(values_quo)
    # For custom, we need to handle vector expressions
    values_formula <- as.oq_formula(expr_text(values_expr))
  }

  # Create parameter specification
  param_spec <- list(
    param_type = "variable",
    name = variable,
    type = type,
    min = min_formula,
    max = max_formula,
    radius = radius_formula,
    steps = steps,
    values = values_formula,
    strategy = as.character(strategy),
    group = as.character(group),
    display_name = display_name,
    include_base_case = include_base_case
  )

  # Add to TWSA's parameters list
  model$twsa_analyses[[twsa_idx]]$parameters <- c(
    model$twsa_analyses[[twsa_idx]]$parameters,
    list(param_spec)
  )

  model
}

#' Add Setting to Two-Way Sensitivity Analysis
#'
#' Add a model setting to vary in a two-way sensitivity analysis. Each TWSA must
#' have exactly two parameters (variables or settings). Settings include parameters
#' like discount rates, timeframe, and other global model configuration values.
#'
#' Values can be specified using three different range types:
#'
#' - `type = "range"`: Vary from `min` to `max` in `steps` evenly-spaced values
#' - `type = "radius"`: Vary from `base - radius` to `base + radius` where `base`
#'   is the current setting value, creating `steps * 2 + 1` values
#' - `type = "custom"`: Use explicit values provided as a numeric vector
#'
#' @param model An oq_model_builder object
#' @param twsa_name Name of the TWSA analysis to add the setting to (must exist)
#' @param setting Name of the setting to vary (e.g., "discount_cost", "timeframe")
#' @param type Type of range specification: "range", "radius", or "custom"
#' @param min For type="range": minimum value
#' @param max For type="range": maximum value
#' @param radius For type="radius": distance from base value
#' @param steps For type="range" or "radius": number of steps
#' @param values For type="custom": numeric vector of values to use
#' @param display_name Optional display name for plots and tables
#' @param include_base_case Logical. If TRUE (default), ensures the base case
#'   value is included in the parameter grid. If the base case value is already
#'   present in the values, no duplicate is added.
#'
#' @return The modified model object
#'
#' @export
#' @examples
#' \dontrun{
#' model <- define_model("markov") |>
#'   set_settings(timeframe = 20, discount_cost = 3) |>
#'   add_twsa("Time vs Discount") |>
#'   add_twsa_setting("Time vs Discount", "timeframe",
#'     type = "range", min = 10, max = 30, steps = 5) |>
#'   add_twsa_setting("Time vs Discount", "discount_cost",
#'     type = "custom", values = c(0, 1.5, 3, 5))
#' }
add_twsa_setting <- function(model, twsa_name, setting, type,
                              min = NULL, max = NULL,
                              radius = NULL, steps = NULL,
                              values = NULL,
                              display_name = NULL,
                              include_base_case = TRUE) {
  # Validate inputs
  if (!is.character(twsa_name) || length(twsa_name) != 1) {
    stop("twsa_name must be a single character string", call. = FALSE)
  }
  if (!is.character(setting) || length(setting) != 1) {
    stop("setting must be a single character string", call. = FALSE)
  }

  # Find TWSA index
  twsa_idx <- which(sapply(model$twsa_analyses, function(s) s$name) == twsa_name)
  if (length(twsa_idx) == 0) {
    stop(sprintf("TWSA analysis '%s' not found. Use add_twsa() first.", twsa_name),
         call. = FALSE)
  }

  # Check if TWSA already has 2 parameters
  current_params <- length(model$twsa_analyses[[twsa_idx]]$parameters)
  if (current_params >= 2) {
    stop(sprintf(
      "TWSA analysis '%s' already has 2 parameters. Each TWSA must have exactly 2 parameters.",
      twsa_name
    ), call. = FALSE)
  }

  # Validate type
  type <- match.arg(type, c("range", "radius", "custom"))

  # Validate type-specific parameters
  if (type == "range") {
    if (is.null(min) || is.null(max) || is.null(steps)) {
      stop("For type='range', min, max, and steps are required", call. = FALSE)
    }
  } else if (type == "radius") {
    if (is.null(radius) || is.null(steps)) {
      stop("For type='radius', radius and steps are required", call. = FALSE)
    }
  } else if (type == "custom") {
    if (is.null(values)) {
      stop("For type='custom', values is required", call. = FALSE)
    }
  }

  # Create parameter specification (store literal values for settings)
  param_spec <- list(
    param_type = "setting",
    name = setting,
    type = type,
    min = min,
    max = max,
    radius = radius,
    steps = steps,
    values = values,
    display_name = display_name %||% setting,
    include_base_case = include_base_case
  )

  # Add to TWSA's parameters list
  model$twsa_analyses[[twsa_idx]]$parameters <- c(
    model$twsa_analyses[[twsa_idx]]$parameters,
    list(param_spec)
  )

  model
}

#' Print TWSA Analyses
#'
#' Print method for displaying two-way sensitivity analyses defined in a model
#'
#' @param model An oq_model_builder object with TWSA analyses
#'
#' @return Invisible model
#' @keywords internal
print_twsa <- function(model) {
  if (is.null(model$twsa_analyses) || length(model$twsa_analyses) == 0) {
    cat("No TWSA analyses defined\n")
    return(invisible(model))
  }

  cat("Two-Way Sensitivity Analyses (", length(model$twsa_analyses), "):\n", sep = "")
  for (i in seq_along(model$twsa_analyses)) {
    twsa <- model$twsa_analyses[[i]]
    cat(sprintf("  %d. %s\n", i, twsa$name))
    if (twsa$description != twsa$name) {
      cat(sprintf("     Description: %s\n", twsa$description))
    }
    n_params <- length(twsa$parameters)
    cat(sprintf("     Parameters: %d/2\n", n_params))
    for (j in seq_along(twsa$parameters)) {
      param <- twsa$parameters[[j]]
      param_display <- param$display_name %||% param$name
      cat(sprintf("       - [%s] %s (%s)\n", param$param_type, param_display, param$type))
    }
  }
  invisible(model)
}

#' Add an Override Category to Model
#'
#' Create a named category for grouping related override controls.
#' Override categories organize UI controls that allow users to modify
#' model variables and settings at runtime.
#'
#' @param model An oq_model_builder object
#' @param name Character string for the category name (must be unique, case-insensitive)
#' @param general Logical indicating if this is a system category (default: FALSE)
#'
#' @return The modified model object
#' @export
add_override_category <- function(model, name, general = FALSE) {
  # Validate inputs
  if (!is.character(name) || length(name) != 1 || nchar(trimws(name)) == 0) {
    stop("Override category name must be a non-empty character string", call. = FALSE)
  }

  # Check for duplicate category name (case-insensitive)
  if (length(model$override_categories) > 0) {
    existing_names <- tolower(sapply(model$override_categories, function(c) c$name))
    if (tolower(name) %in% existing_names) {
      stop(sprintf("Override category '%s' already exists", name), call. = FALSE)
    }
  }

  # Create new category
  new_category <- list(
    name = name,
    general = as.logical(general),
    overrides = list()
  )

  # Add to model
  model$override_categories <- c(model$override_categories, list(new_category))

  model
}

#' Create a Dropdown Option for Override Controls
#'
#' Helper function to create a dropdown option for use with
#' \code{\link{add_override}} when \code{input_type = "dropdown"}.
#'
#' @param label Display text for the option
#' @param value Actual value when selected
#' @param is_base_case Whether this is the default option (default: FALSE)
#'
#' @return A list representing a dropdown option
#' @export
override_option <- function(label, value, is_base_case = FALSE) {
  if (!is.character(label) || length(label) != 1 || nchar(trimws(label)) == 0) {
    stop("Dropdown option label must be a non-empty character string", call. = FALSE)
  }
  list(
    label = label,
    value = as.character(value),
    is_base_case = as.logical(is_base_case)
  )
}

#' Add an Override Control to a Category
#'
#' Define a UI override control that allows users to modify a model variable
#' or setting at runtime. When the model is rendered in a Shiny UI, these
#' controls will be displayed to the user.
#'
#' @param model An oq_model_builder object
#' @param category Character string naming the category to add to (must exist)
#' @param title Character string for the display name
#' @param name Character string for the variable or setting name to override
#' @param type Character string: "variable" or "setting"
#' @param input_type Character string: "numeric", "slider", "dropdown", "formula", or "timeframe"
#' @param expression Default/initial override value as a string or unquoted expression (uses NSE)
#' @param description Optional description text
#' @param strategy Optional strategy name (for variable overrides only)
#' @param group Optional group name (for variable overrides only)
#' @param general Logical indicating if this is a system override (default: FALSE)
#' @param min Numeric minimum (for numeric/slider input types)
#' @param max Numeric maximum (for numeric/slider input types)
#' @param step_size Numeric step size (for slider input type)
#' @param options List of dropdown options (for dropdown input type).
#'   Each option should be created with \code{\link{override_option}}.
#'
#' @return The modified model object
#' @export
add_override <- function(model, category, title, name, type = "variable",
                         input_type = "numeric", expression,
                         description = NULL, strategy = "", group = "",
                         general = FALSE,
                         min = NULL, max = NULL, step_size = NULL,
                         options = NULL) {

  # Validate category exists
  if (length(model$override_categories) == 0) {
    stop(sprintf("Override category '%s' not found. Use add_override_category() first.", category),
         call. = FALSE)
  }
  cat_idx <- which(sapply(model$override_categories, function(c) c$name) == category)
  if (length(cat_idx) == 0) {
    stop(sprintf("Override category '%s' not found. Use add_override_category() first.", category),
         call. = FALSE)
  }

  # Validate title
  if (!is.character(title) || length(title) != 1 || nchar(trimws(title)) == 0) {
    stop("Override title must be a non-empty character string", call. = FALSE)
  }

  # Validate name
  if (!is.character(name) || length(name) != 1 || nchar(trimws(name)) == 0) {
    stop("Override name must be a non-empty character string", call. = FALSE)
  }

  # Validate type
  if (!type %in% c("variable", "setting")) {
    stop("Override type must be 'variable' or 'setting'", call. = FALSE)
  }

  # Validate input_type
  valid_input_types <- c("numeric", "slider", "dropdown", "formula", "timeframe")
  if (!input_type %in% valid_input_types) {
    stop(paste0("Override input_type must be one of: ", paste(valid_input_types, collapse = ", ")),
         call. = FALSE)
  }

  # Capture expression using NSE
  expr_quo <- enquo(expression)
  expr_val <- quo_get_expr(expr_quo)

  if (is.numeric(expr_val)) {
    expression_str <- as.character(expr_val)
  } else if (is.character(expr_val) && length(expr_val) == 1) {
    expression_str <- expr_val
  } else {
    expression_str <- expr_text(expr_val)
  }

  # Validate expression non-empty
  if (nchar(trimws(expression_str)) == 0) {
    stop("Override expression must be non-empty", call. = FALSE)
  }

  # Validate type-specific rules
  if (type == "setting") {
    # Settings don't use strategy/group
    if (strategy != "" || group != "") {
      stop("Strategy and group cannot be specified for setting overrides", call. = FALSE)
    }
    # Validate setting name
    valid_settings <- c(
      "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
      "discount_cost", "discount_outcomes", "half_cycle_method",
      "reduce_state_cycle", "days_per_year"
    )
    if (!(name %in% valid_settings)) {
      stop(sprintf(
        "Invalid override setting name: '%s'. Valid settings: %s",
        name, paste(valid_settings, collapse = ", ")
      ), call. = FALSE)
    }
  }

  if (type == "variable") {
    # Validate variable exists and strategy/group targeting
    validate_variable_targeting(model, name, strategy, group,
                                "Override", "add_override")
  }

  # Validate min < max
  if (!is.null(min) && !is.null(max)) {
    if (min >= max) {
      stop(sprintf("Override min (%s) must be less than max (%s)", min, max), call. = FALSE)
    }
  }

  # Validate step_size > 0
  if (!is.null(step_size) && step_size <= 0) {
    stop("Override step_size must be greater than 0", call. = FALSE)
  }

  # Validate dropdown options
  if (input_type == "dropdown" && !is.null(options) && length(options) == 0) {
    stop("Dropdown override must have at least one option", call. = FALSE)
  }

  # Check for duplicate override in this category
  existing_overrides <- model$override_categories[[cat_idx]]$overrides
  if (length(existing_overrides) > 0) {
    for (existing in existing_overrides) {
      if (existing$type == type && existing$name == name &&
          existing$strategy == as.character(strategy) &&
          existing$group == as.character(group)) {
        stop(sprintf(
          "An override for %s '%s'%s%s already exists in category '%s'",
          type, name,
          if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
          if (group != "") paste0(" (group: ", group, ")") else "",
          category
        ), call. = FALSE)
      }
    }
  }

  # Build input_config with defaults
  input_config <- switch(input_type,
    "numeric" = list(
      min = min %||% 0,
      max = max %||% 100
    ),
    "slider" = list(
      min = min %||% 0,
      max = max %||% 1,
      step_size = step_size %||% 0.05
    ),
    "dropdown" = list(
      options = options %||% list()
    ),
    "formula" = list(),
    "timeframe" = list()
  )

  # Create override item
  override_item <- list(
    title = title,
    description = description %||% "",
    type = type,
    name = name,
    strategy = as.character(strategy),
    group = as.character(group),
    general = as.logical(general),
    input_type = input_type,
    overridden_expression = expression_str,
    input_config = input_config
  )

  # Add to category
  model$override_categories[[cat_idx]]$overrides <- c(
    model$override_categories[[cat_idx]]$overrides,
    list(override_item)
  )

  model
}

#' Print Override Categories Summary
#'
#' Displays a formatted summary of override categories and their overrides.
#'
#' @param model A model object with override_categories
#' @return Invisible model
#' @keywords internal
print_override_categories <- function(model) {
  if (is.null(model$override_categories) || length(model$override_categories) == 0) {
    return(invisible(model))
  }

  # Count total overrides
  total_overrides <- sum(sapply(model$override_categories, function(c) length(c$overrides)))
  if (total_overrides == 0) {
    return(invisible(model))
  }

  cat(sprintf("\n  [!] Active overrides (%d):\n", total_overrides))
  for (cat_item in model$override_categories) {
    if (length(cat_item$overrides) == 0) next
    cat(sprintf("    %s:\n", cat_item$name))
    for (override in cat_item$overrides) {
      config_str <- switch(override$input_type,
        "numeric" = sprintf("[numeric: %s-%s]",
          override$input_config$min, override$input_config$max),
        "slider" = sprintf("[slider: %s-%s]",
          override$input_config$min, override$input_config$max),
        "dropdown" = "[dropdown]",
        "formula" = "[formula]",
        "timeframe" = "[timeframe]"
      )
      cat(sprintf("      - %s (%s): %s %s\n",
        override$title, override$name,
        override$overridden_expression, config_str))
    }
  }
  invisible(model)
}

#' Edit an Override Category
#'
#' Modify the name or general flag of an existing override category.
#'
#' @param model An oq_model_builder object
#' @param name Character string identifying the category to edit
#' @param new_name Optional new name for the category
#' @param general Optional logical for the general flag
#'
#' @return The modified model object
#' @export
edit_override_category <- function(model, name, new_name, general) {
  cat_idx <- which(sapply(model$override_categories, function(c) c$name) == name)
  if (length(cat_idx) == 0) {
    stop(sprintf("Override category '%s' not found.", name), call. = FALSE)
  }

  if (!missing(general)) {
    model$override_categories[[cat_idx]]$general <- as.logical(general)
  }

  if (!missing(new_name)) {
    if (!is.character(new_name) || length(new_name) != 1 || nchar(trimws(new_name)) == 0) {
      stop("Override category name must be a non-empty character string", call. = FALSE)
    }
    # Case-insensitive duplicate check excluding self
    if (length(model$override_categories) > 1) {
      other_names <- tolower(sapply(model$override_categories[-cat_idx], function(c) c$name))
      if (tolower(new_name) %in% other_names) {
        stop(sprintf("Override category '%s' already exists", new_name), call. = FALSE)
      }
    }
    model$override_categories[[cat_idx]]$name <- new_name
  }

  model
}

#' Remove an Override Category
#'
#' Remove an existing override category and all its overrides.
#'
#' @param model An oq_model_builder object
#' @param name Character string identifying the category to remove
#'
#' @return The modified model object
#' @export
remove_override_category <- function(model, name) {
  cat_idx <- which(sapply(model$override_categories, function(c) c$name) == name)
  if (length(cat_idx) == 0) {
    stop(sprintf("Override category '%s' not found.", name), call. = FALSE)
  }
  model$override_categories <- model$override_categories[-cat_idx]
  model
}

#' Edit an Override Control
#'
#' Modify an existing override control within a category. The override is
#' identified by its composite key of category, type, name, strategy, and group.
#'
#' @param model An oq_model_builder object
#' @param category Character string naming the category containing the override
#' @param type Character string: "variable" or "setting" (current type)
#' @param name Character string for the variable or setting name (current name)
#' @param strategy Current strategy targeting (default: "")
#' @param group Current group targeting (default: "")
#' @param new_type Optional new type
#' @param new_name Optional new name
#' @param new_strategy Optional new strategy
#' @param new_group Optional new group
#' @param title Optional new title
#' @param description Optional new description
#' @param expression Optional new expression (supports NSE)
#' @param input_type Optional new input type
#' @param min Optional new minimum value
#' @param max Optional new maximum value
#' @param step_size Optional new step size
#' @param options Optional new dropdown options
#' @param general Optional new general flag
#'
#' @return The modified model object
#' @export
edit_override <- function(model, category, type, name, strategy = "", group = "",
                          new_type, new_name, new_strategy, new_group,
                          title, description, expression,
                          input_type, min, max, step_size, options, general) {

  # Find category
  cat_idx <- which(sapply(model$override_categories, function(c) c$name) == category)
  if (length(cat_idx) == 0) {
    stop(sprintf("Override category '%s' not found.", category), call. = FALSE)
  }

  # Find override by composite key
  existing_overrides <- model$override_categories[[cat_idx]]$overrides
  ovr_idx <- 0L
  if (length(existing_overrides) > 0) {
    for (i in seq_along(existing_overrides)) {
      ovr <- existing_overrides[[i]]
      if (ovr$type == type && ovr$name == name &&
          ovr$strategy == as.character(strategy) &&
          ovr$group == as.character(group)) {
        ovr_idx <- i
        break
      }
    }
  }
  if (ovr_idx == 0L) {
    stop(sprintf(
      "Override for %s '%s'%s%s not found in category '%s'.",
      type, name,
      if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
      if (group != "") paste0(" (group: ", group, ")") else "",
      category
    ), call. = FALSE)
  }

  ovr <- existing_overrides[[ovr_idx]]

  # Simple field updates
  if (!missing(title)) {
    if (!is.character(title) || length(title) != 1 || nchar(trimws(title)) == 0) {
      stop("Override title must be a non-empty character string", call. = FALSE)
    }
    ovr$title <- title
  }

  if (!missing(description)) {
    ovr$description <- description %||% ""
  }

  if (!missing(general)) {
    ovr$general <- as.logical(general)
  }

  # Expression update (NSE)
  if (!missing(expression)) {
    expr_quo <- enquo(expression)
    expr_val <- quo_get_expr(expr_quo)

    if (is.numeric(expr_val)) {
      expression_str <- as.character(expr_val)
    } else if (is.character(expr_val) && length(expr_val) == 1) {
      expression_str <- expr_val
    } else {
      expression_str <- expr_text(expr_val)
    }

    if (nchar(trimws(expression_str)) == 0) {
      stop("Override expression must be non-empty", call. = FALSE)
    }
    ovr$overridden_expression <- expression_str
  }

  # Re-keying
  if (!missing(new_type) || !missing(new_name) || !missing(new_strategy) || !missing(new_group)) {
    eff_type <- if (!missing(new_type)) new_type else ovr$type
    eff_name <- if (!missing(new_name)) new_name else ovr$name
    eff_strategy <- if (!missing(new_strategy)) as.character(new_strategy) else ovr$strategy
    eff_group <- if (!missing(new_group)) as.character(new_group) else ovr$group

    # Duplicate check excluding self
    if (length(existing_overrides) > 1) {
      for (i in seq_along(existing_overrides)) {
        if (i == ovr_idx) next
        other <- existing_overrides[[i]]
        if (other$type == eff_type && other$name == eff_name &&
            other$strategy == eff_strategy && other$group == eff_group) {
          stop(sprintf(
            "An override for %s '%s'%s%s already exists in category '%s'",
            eff_type, eff_name,
            if (eff_strategy != "") paste0(" (strategy: ", eff_strategy, ")") else "",
            if (eff_group != "") paste0(" (group: ", eff_group, ")") else "",
            category
          ), call. = FALSE)
        }
      }
    }

    # Type validation
    if (eff_type == "setting") {
      if (eff_strategy != "" || eff_group != "") {
        stop("Strategy and group cannot be specified for setting overrides", call. = FALSE)
      }
      valid_settings <- c(
        "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
        "discount_cost", "discount_outcomes", "half_cycle_method",
        "reduce_state_cycle", "days_per_year"
      )
      if (!(eff_name %in% valid_settings)) {
        stop(sprintf(
          "Invalid override setting name: '%s'. Valid settings: %s",
          eff_name, paste(valid_settings, collapse = ", ")
        ), call. = FALSE)
      }
    }

    if (eff_type == "variable") {
      validate_variable_targeting(model, eff_name, eff_strategy, eff_group,
                                  "Override", "edit_override")
    }

    ovr$type <- eff_type
    ovr$name <- eff_name
    ovr$strategy <- eff_strategy
    ovr$group <- eff_group
  }

  # input_type change
  if (!missing(input_type)) {
    valid_input_types <- c("numeric", "slider", "dropdown", "formula", "timeframe")
    if (!input_type %in% valid_input_types) {
      stop(paste0("Override input_type must be one of: ", paste(valid_input_types, collapse = ", ")),
           call. = FALSE)
    }

    if (input_type != ovr$input_type) {
      # Rebuild input_config entirely
      ovr$input_config <- switch(input_type,
        "numeric" = list(
          min = if (!missing(min)) min else 0,
          max = if (!missing(max)) max else 100
        ),
        "slider" = list(
          min = if (!missing(min)) min else 0,
          max = if (!missing(max)) max else 1,
          step_size = if (!missing(step_size)) step_size else 0.05
        ),
        "dropdown" = list(
          options = if (!missing(options)) options else list()
        ),
        "formula" = list(),
        "timeframe" = list()
      )
      ovr$input_type <- input_type
    } else {
      # Same input_type, update config fields in-place
      if (!missing(min)) ovr$input_config$min <- min
      if (!missing(max)) ovr$input_config$max <- max
      if (!missing(step_size)) ovr$input_config$step_size <- step_size
      if (!missing(options)) ovr$input_config$options <- options
    }
  } else {
    # No input_type change, but config fields may be updated
    if (!missing(min)) ovr$input_config$min <- min
    if (!missing(max)) ovr$input_config$max <- max
    if (!missing(step_size)) ovr$input_config$step_size <- step_size
    if (!missing(options)) ovr$input_config$options <- options
  }

  # Validate min < max using effective values
  eff_min <- if (!missing(min)) min else ovr$input_config$min
  eff_max <- if (!missing(max)) max else ovr$input_config$max
  if (!is.null(eff_min) && !is.null(eff_max)) {
    if (eff_min >= eff_max) {
      stop(sprintf("Override min (%s) must be less than max (%s)", eff_min, eff_max), call. = FALSE)
    }
  }

  # Validate step_size > 0
  eff_step <- if (!missing(step_size)) step_size else ovr$input_config$step_size
  if (!is.null(eff_step) && eff_step <= 0) {
    stop("Override step_size must be greater than 0", call. = FALSE)
  }

  # Write back
  model$override_categories[[cat_idx]]$overrides[[ovr_idx]] <- ovr

  model
}

#' Remove an Override Control
#'
#' Remove an existing override control from a category.
#'
#' @param model An oq_model_builder object
#' @param category Character string naming the category containing the override
#' @param type Character string: "variable" or "setting"
#' @param name Character string for the variable or setting name
#' @param strategy Strategy targeting (default: "")
#' @param group Group targeting (default: "")
#'
#' @return The modified model object
#' @export
remove_override <- function(model, category, type, name, strategy = "", group = "") {
  # Find category
  cat_idx <- which(sapply(model$override_categories, function(c) c$name) == category)
  if (length(cat_idx) == 0) {
    stop(sprintf("Override category '%s' not found.", category), call. = FALSE)
  }

  # Find override by composite key
  existing_overrides <- model$override_categories[[cat_idx]]$overrides
  ovr_idx <- 0L
  if (length(existing_overrides) > 0) {
    for (i in seq_along(existing_overrides)) {
      ovr <- existing_overrides[[i]]
      if (ovr$type == type && ovr$name == name &&
          ovr$strategy == as.character(strategy) &&
          ovr$group == as.character(group)) {
        ovr_idx <- i
        break
      }
    }
  }
  if (ovr_idx == 0L) {
    stop(sprintf(
      "Override for %s '%s'%s%s not found in category '%s'.",
      type, name,
      if (strategy != "") paste0(" (strategy: ", strategy, ")") else "",
      if (group != "") paste0(" (group: ", group, ")") else "",
      category
    ), call. = FALSE)
  }

  model$override_categories[[cat_idx]]$overrides <- model$override_categories[[cat_idx]]$overrides[-ovr_idx]

  model
}

# ============================================================================
# Threshold Analysis
# ============================================================================

#' Create Threshold Condition for Outcomes
#'
#' @param summary Target a summary (e.g., "total_qalys"). Mutually exclusive with \code{value}.
#' @param value Target an individual value (e.g., "qaly_sick"). Mutually exclusive with \code{summary}.
#' @param type Either "absolute" (single strategy) or "difference" (referent minus comparator)
#' @param strategy Strategy name for absolute type
#' @param referent Referent strategy for difference type
#' @param comparator Comparator strategy for difference type
#' @param discounted Whether to use discounted results
#' @param target_value Target value to find threshold for
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_outcomes <- function(
  summary = NULL,
  value = NULL,
  type = c("absolute", "difference"),
  strategy = NULL,
  referent = NULL,
  comparator = NULL,
  discounted = TRUE,
  target_value = 0,
  group = ""
) {
  type <- match.arg(type)
  list(output = "outcomes", summary = summary, value = value, type = type,
       strategy = strategy, referent = referent, comparator = comparator,
       discounted = discounted, target_value = target_value, group = group)
}

#' Create Threshold Condition for Costs
#'
#' @param summary Target a summary (e.g., "total_cost"). Mutually exclusive with \code{value}.
#' @param value Target an individual value (e.g., "cost_drug"). Mutually exclusive with \code{summary}.
#' @param type Either "absolute" (single strategy) or "difference" (referent minus comparator)
#' @param strategy Strategy name for absolute type
#' @param referent Referent strategy for difference type
#' @param comparator Comparator strategy for difference type
#' @param discounted Whether to use discounted results
#' @param target_value Target value to find threshold for
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_costs <- function(
  summary = NULL,
  value = NULL,
  type = c("absolute", "difference"),
  strategy = NULL,
  referent = NULL,
  comparator = NULL,
  discounted = TRUE,
  target_value = 0,
  group = ""
) {
  type <- match.arg(type)
  list(output = "costs", summary = summary, value = value, type = type,
       strategy = strategy, referent = referent, comparator = comparator,
       discounted = discounted, target_value = target_value, group = group)
}

#' Create Threshold Condition for NMB
#'
#' @param health_summary Health outcome summary name
#' @param cost_summary Cost outcome summary name
#' @param referent Referent strategy
#' @param comparator Comparator strategy
#' @param discounted Whether to use discounted results
#' @param target_value Target NMB value
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @param wtp Willingness-to-pay value. If NULL (default), the WTP from the health summary is used.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_nmb <- function(health_summary, cost_summary, referent, comparator,
                                     discounted = TRUE, target_value = 0, group = "",
                                     wtp = NULL) {
  list(output = "nmb", health_summary = health_summary, cost_summary = cost_summary,
       referent = referent, comparator = comparator, discounted = discounted,
       target_value = target_value, group = group, wtp = wtp)
}

#' Create Threshold Condition for CE
#'
#' @param health_summary Health outcome summary name
#' @param cost_summary Cost outcome summary name
#' @param referent Referent strategy
#' @param comparator Comparator strategy
#' @param discounted Whether to use discounted results
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @param wtp Willingness-to-pay value. If NULL (default), the WTP from the health summary is used.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_ce <- function(health_summary, cost_summary, referent, comparator,
                                    discounted = TRUE, group = "",
                                    wtp = NULL) {
  list(output = "ce", health_summary = health_summary, cost_summary = cost_summary,
       referent = referent, comparator = comparator, discounted = discounted,
       group = group, wtp = wtp)
}

#' Create Threshold Condition for Trace
#'
#' @param state State name to target in the trace
#' @param time Numeric time value at which to read the trace
#' @param time_unit Time unit for the time parameter: "cycle", "year", "month", "week", or "day"
#' @param type Whether to use absolute or difference: "absolute" or "difference"
#' @param strategy Strategy name for absolute type
#' @param referent Referent strategy for difference type
#' @param comparator Comparator strategy for difference type
#' @param target_value Target trace value to find threshold for
#' @param group Group name to target for results extraction. Empty string (default) uses aggregated results.
#' @return A condition list for use in \code{add_threshold_analysis()}
#' @export
threshold_condition_trace <- function(
  state,
  time,
  time_unit = c("cycle", "year", "month", "week", "day"),
  type = c("absolute", "difference"),
  strategy = NULL,
  referent = NULL,
  comparator = NULL,
  target_value,
  group = ""
) {
  time_unit <- match.arg(time_unit)
  type <- match.arg(type)
  list(output = "trace", state = state, time = time, time_unit = time_unit,
       type = type, strategy = strategy, referent = referent,
       comparator = comparator, target_value = target_value, group = group)
}

#' Add Threshold Analysis to Model
#'
#' Define a threshold analysis that finds the input parameter value producing
#' a desired output condition using iterative root-finding.
#'
#' @param model An oq_model_builder object
#' @param name Unique name for this threshold analysis
#' @param variable Variable to solve for
#' @param lower Lower bound for search range
#' @param upper Upper bound for search range
#' @param condition A condition list created by \code{threshold_condition_*} functions
#' @param variable_strategy Strategy targeting for the variable (empty string = all)
#' @param variable_group Group targeting for the variable (empty string = all)
#' @param active Whether this analysis is active
#' @return The modified model object
#' @export
add_threshold_analysis <- function(
  model, name, variable, lower, upper,
  condition,
  variable_strategy = "",
  variable_group = "",
  active = TRUE
) {
  if (!inherits(model, "oq_model_builder") && !inherits(model, "oq_model")) {
    stop("model must be an oq_model_builder or oq_model object", call. = FALSE)
  }

  # Validate name
  if (!is.character(name) || length(name) != 1 || name == "") {
    stop("name must be a non-empty character string", call. = FALSE)
  }

  # Validate variable
  if (!is.character(variable) || length(variable) != 1 || variable == "") {
    stop("variable must be a non-empty character string", call. = FALSE)
  }

  # Validate bounds
  if (!is.numeric(lower) || !is.numeric(upper) || length(lower) != 1 || length(upper) != 1) {
    stop("lower and upper must be single numeric values", call. = FALSE)
  }
  if (lower >= upper) {
    stop("lower must be less than upper", call. = FALSE)
  }

  # Validate condition
  if (!is.list(condition) || is.null(condition$output)) {
    stop("condition must be a list with an 'output' field (use threshold_condition_* functions)", call. = FALSE)
  }

  valid_outputs <- c("outcomes", "costs", "nmb", "ce", "vbp", "trace")
  if (!condition$output %in% valid_outputs) {
    stop(sprintf("Invalid output type '%s'. Must be one of: %s",
                 condition$output, paste(valid_outputs, collapse = ", ")), call. = FALSE)
  }

  if (condition$output == "vbp") {
    stop("VBP output type is not yet supported for threshold analysis", call. = FALSE)
  }

  # Validate output-specific fields
  validate_threshold_condition(condition)

  # Validate variable targeting
  validate_variable_targeting(model, variable, variable_strategy, variable_group,
                               "threshold", "add_threshold_analysis")

  # Check for duplicate name - warn and replace
  existing_idx <- which(sapply(model$threshold_analyses, function(a) a$name) == name)
  if (length(existing_idx) > 0) {
    warning(sprintf("Threshold analysis '%s' already exists and will be replaced", name), call. = FALSE)
    model$threshold_analyses[[existing_idx]] <- NULL
  }

  analysis <- list(
    name = name,
    variable = variable,
    variable_strategy = variable_strategy,
    variable_group = variable_group,
    lower = lower,
    upper = upper,
    active = active,
    condition = condition
  )

  model$threshold_analyses <- c(model$threshold_analyses, list(analysis))
  model
}

#' Validate Threshold Condition Fields
#' @param condition A threshold condition list
#' @keywords internal
validate_threshold_condition <- function(condition) {
  output <- condition$output

  if (output %in% c("outcomes", "costs")) {
    has_summary <- !is.null(condition$summary) && condition$summary != ""
    has_value <- !is.null(condition$value) && condition$value != ""
    if (!has_summary && !has_value) {
      stop(sprintf("Threshold condition for '%s' must specify either 'summary' or 'value'", output), call. = FALSE)
    }
    if (has_summary && has_value) {
      stop(sprintf("Threshold condition for '%s' must specify either 'summary' or 'value', not both", output), call. = FALSE)
    }

    type <- condition$type
    if (is.null(type) || !type %in% c("absolute", "difference")) {
      stop("Threshold condition 'type' must be 'absolute' or 'difference'", call. = FALSE)
    }
    if (type == "absolute") {
      if (is.null(condition$strategy) || condition$strategy == "") {
        stop("Threshold condition with type='absolute' requires 'strategy'", call. = FALSE)
      }
    } else {
      if (is.null(condition$referent) || condition$referent == "") {
        stop("Threshold condition with type='difference' requires 'referent'", call. = FALSE)
      }
      if (is.null(condition$comparator) || condition$comparator == "") {
        stop("Threshold condition with type='difference' requires 'comparator'", call. = FALSE)
      }
    }

    if (is.null(condition$target_value)) {
      stop(sprintf("Threshold condition for '%s' requires 'target_value'", output), call. = FALSE)
    }

  } else if (output == "nmb") {
    required <- c("health_summary", "cost_summary", "referent", "comparator")
    missing <- required[!sapply(required, function(f) !is.null(condition[[f]]) && condition[[f]] != "")]
    if (length(missing) > 0) {
      stop(sprintf("Threshold condition for 'nmb' requires: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }

  } else if (output == "ce") {
    required <- c("health_summary", "cost_summary", "referent", "comparator")
    missing <- required[!sapply(required, function(f) !is.null(condition[[f]]) && condition[[f]] != "")]
    if (length(missing) > 0) {
      stop(sprintf("Threshold condition for 'ce' requires: %s", paste(missing, collapse = ", ")), call. = FALSE)
    }
  } else if (output == "trace") {
    if (is.null(condition$state) || condition$state == "") {
      stop("Threshold condition for 'trace' requires 'state'", call. = FALSE)
    }
    if (is.null(condition$time) || !is.numeric(condition$time)) {
      stop("Threshold condition for 'trace' requires numeric 'time'", call. = FALSE)
    }
    valid_time_units <- c("cycle", "year", "month", "week", "day")
    if (is.null(condition$time_unit) || !condition$time_unit %in% valid_time_units) {
      stop(sprintf("Threshold condition 'time_unit' must be one of: %s",
                   paste(valid_time_units, collapse = ", ")), call. = FALSE)
    }
    if (is.null(condition$target_value)) {
      stop("Threshold condition for 'trace' requires 'target_value'", call. = FALSE)
    }
    type <- condition$type
    if (is.null(type) || !type %in% c("absolute", "difference")) {
      stop("Threshold condition 'type' must be 'absolute' or 'difference'", call. = FALSE)
    }
    if (type == "absolute") {
      if (is.null(condition$strategy) || condition$strategy == "") {
        stop("Threshold condition with type='absolute' requires 'strategy'", call. = FALSE)
      }
    } else {
      if (is.null(condition$referent) || condition$referent == "") {
        stop("Threshold condition with type='difference' requires 'referent'", call. = FALSE)
      }
      if (is.null(condition$comparator) || condition$comparator == "") {
        stop("Threshold condition with type='difference' requires 'comparator'", call. = FALSE)
      }
    }
  }

  # Validate group field if provided
  if (!is.null(condition$group) && !identical(condition$group, "")) {
    if (!is.character(condition$group) || length(condition$group) != 1) {
      stop("Threshold condition 'group' must be a single character string", call. = FALSE)
    }
  }
}
