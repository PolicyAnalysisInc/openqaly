# ============================================================================
# Entity Schema Definitions + Callbacks
# ============================================================================
# Each entity type has a schema (declarative) and optional callbacks (imperative).
# Schemas describe storage, keys, duplicate handling, and cascade relationships.
# Callbacks contain domain-specific validation and transformation logic.
# ============================================================================

# --- Shared cascade ref builders --------------------------------------------

.make_strategy_cascade_refs <- function() {
  list(
    list(container = "variables", storage = "tibble", fields = "strategy"),
    list(container = "dsa_parameters", storage = "list", fields = "strategy"),
    list(container = "scenarios", storage = "list",
         child_list = "variable_overrides", fields = "strategy"),
    list(container = "twsa_analyses", storage = "list",
         child_list = "parameters", fields = "strategy",
         min_children = 2),
    list(container = "override_categories", storage = "list",
         child_list = "overrides", fields = "strategy"),
    list(container = "threshold_analyses", storage = "list",
         fields = "variable_strategy",
         accessor = function(item) {
           c(item$variable_strategy,
             if (!is.null(item$condition)) item$condition$strategy)
         },
         mutator = function(item, old_val, new_val) {
           if (identical(item$variable_strategy, old_val))
             item$variable_strategy <- new_val
           if (!is.null(item$condition) && identical(item$condition$strategy, old_val))
             item$condition$strategy <- new_val
           item
         },
         dep_predicate = function(item, val) {
           identical(item$variable_strategy, val) ||
             (!is.null(item$condition) && identical(item$condition$strategy, val))
         },
         remove_predicate = function(item, val) {
           identical(item$variable_strategy, val) ||
             (!is.null(item$condition) && identical(item$condition$strategy, val))
         }),
    list(container = "multivariate_sampling", storage = "list", fields = "strategy")
  )
}

.make_group_cascade_refs <- function() {
  list(
    list(container = "variables", storage = "tibble", fields = "group"),
    list(container = "dsa_parameters", storage = "list", fields = "group"),
    list(container = "scenarios", storage = "list",
         child_list = "variable_overrides", fields = "group"),
    list(container = "twsa_analyses", storage = "list",
         child_list = "parameters", fields = "group",
         min_children = 2),
    list(container = "override_categories", storage = "list",
         child_list = "overrides", fields = "group"),
    list(container = "threshold_analyses", storage = "list",
         fields = "variable_group",
         accessor = function(item) {
           c(item$variable_group,
             if (!is.null(item$condition)) item$condition$group)
         },
         mutator = function(item, old_val, new_val) {
           if (identical(item$variable_group, old_val))
             item$variable_group <- new_val
           if (!is.null(item$condition) && identical(item$condition$group, old_val))
             item$condition$group <- new_val
           item
         },
         dep_predicate = function(item, val) {
           identical(item$variable_group, val) ||
             (!is.null(item$condition) && identical(item$condition$group, val))
         },
         remove_predicate = function(item, val) {
           identical(item$variable_group, val) ||
             (!is.null(item$condition) && identical(item$condition$group, val))
         }),
    list(container = "multivariate_sampling", storage = "list", fields = "group")
  )
}

# --- Strategy Schema ---------------------------------------------------------

.schema_strategy <- list(
  entity_name      = "strategy",
  container        = "strategies",
  storage          = "tibble",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  duplicate_message = 'Duplicate strategy name "%s". Each strategy must have a unique name.',
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = .make_strategy_cascade_refs()
)

# --- Group Schema ------------------------------------------------------------

.schema_group <- list(
  entity_name      = "group",
  container        = "groups",
  storage          = "tibble",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  duplicate_message = 'Duplicate group name "%s". Each group must have a unique name.',
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = .make_group_cascade_refs()
)

# --- Summary Schema ----------------------------------------------------------

.schema_summary <- list(
  entity_name      = "summary",
  not_found_message = 'Summary "%s" not found in model.',
  duplicate_message = "Summary '%s' already exists.",
  container        = "summaries",
  storage          = "tibble",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(
    list(container = "trees", field = "name", unique = TRUE,
         message = 'Name collision: "%s" is already used as a decision tree name.')
  ),
  cascade_refs     = list()
)

.callbacks_add_summary <- list(
  pre_add = function(model, item) {
    if (item$type == "cost" && !is.null(item$wtp) && !is.na(item$wtp)) {
      stop(sprintf("WTP cannot be specified for cost summary '%s'.", item$name),
           call. = FALSE)
    }
    model
  }
)

.callbacks_edit_summary <- list(
  pre_edit = function(model, match_info, updates) {
    if (!is.null(updates$type) && updates$type == "cost") {
      model$summaries$wtp[match_info$idx] <- NA_real_
    }
    if (!is.null(updates$wtp)) {
      current_type <- if (!is.null(updates$type)) updates$type else {
        model$summaries$type[match_info$idx]
      }
      if (current_type == "cost") {
        stop(sprintf("WTP cannot be specified for cost summary '%s'. WTP is only valid for outcome summaries.", model$summaries$name[match_info$idx]), call. = FALSE)
      }
    }
    model
  },
  pre_rename = function(model, match_info, new_name, updates) {
    # Tree collision check on rename
    if (!is.null(model$trees) && is.data.frame(model$trees) && nrow(model$trees) > 0) {
      if (new_name %in% unique(model$trees$name)) {
        stop(sprintf('Name collision detected: "%s" is already used as a decision tree name. Please use a different name.', new_name), call. = FALSE)
      }
    }
    model
  },
  post_rename = function(model, old_name, new_name) {
    if (length(model$threshold_analyses) > 0) {
      for (i in seq_along(model$threshold_analyses)) {
        cond <- model$threshold_analyses[[i]]$condition
        if (!is.null(cond)) {
          if (identical(cond$summary, old_name))
            model$threshold_analyses[[i]]$condition$summary <- new_name
          if (identical(cond$health_summary, old_name))
            model$threshold_analyses[[i]]$condition$health_summary <- new_name
          if (identical(cond$cost_summary, old_name))
            model$threshold_analyses[[i]]$condition$cost_summary <- new_name
        }
      }
    }
    if (!is.null(model$vbp)) {
      if (identical(model$vbp$outcome_summary, old_name))
        model$vbp$outcome_summary <- new_name
      if (identical(model$vbp$cost_summary, old_name))
        model$vbp$cost_summary <- new_name
    }
    model
  }
)

# --- Table Schema ------------------------------------------------------------

.schema_table <- list(
  entity_name      = "table",
  container        = "tables",
  storage          = "named_list",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(
    list(container = "trees", field = "name", unique = TRUE,
         message = 'Name collision: "%s" is already used as a decision tree name.'),
    list(container = "values", field = "name", unique = FALSE,
         message = 'Name collision: "%s" is used as both a table and a value name.')
  ),
  cascade_refs     = list()
)

# --- Script Schema -----------------------------------------------------------

.schema_script <- list(
  entity_name      = "script",
  container        = "scripts",
  storage          = "named_list",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

# --- Scenario Schema ---------------------------------------------------------

.schema_scenario <- list(
  entity_name             = "scenario",
  container               = "scenarios",
  storage                 = "list",
  key_fields              = "name",
  name_field              = "name",
  duplicate_action        = "error",
  reserved_names          = "Base Case",
  reserved_name_message   = "'Base Case' is a reserved scenario name that cannot be used",
  rename_duplicate_message = "Scenario '%s' already exists.",
  container_class         = NULL,
  na_safe_keys            = FALSE,
  auto_update_description = TRUE,
  collision_checks        = list(),
  cascade_refs            = list()
)

# --- TWSA Schema -------------------------------------------------------------

.schema_twsa <- list(
  entity_name             = "TWSA analysis",
  container               = "twsa_analyses",
  storage                 = "list",
  key_fields              = "name",
  name_field              = "name",
  duplicate_action        = "error",
  reserved_names          = "Base Case",
  reserved_name_message   = "'Base Case' is a reserved name that cannot be used for TWSA analyses",
  rename_duplicate_message = "TWSA analysis '%s' already exists.",
  container_class         = NULL,
  na_safe_keys            = FALSE,
  auto_update_description = TRUE,
  collision_checks        = list(),
  cascade_refs            = list()
)

# --- Override Category Schema ------------------------------------------------

.schema_override_category <- list(
  entity_name      = "override category",
  container        = "override_categories",
  storage          = "list",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  duplicate_message = "Override category '%s' already exists",
  case_insensitive_keys = TRUE,
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

# --- Multivariate Sampling Schema -------------------------------------------

.schema_multivariate_sampling <- list(
  entity_name      = "multivariate sampling specification",
  container        = "multivariate_sampling",
  storage          = "list",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  duplicate_message = "A multivariate sampling specification named '%s' already exists.",
  rename_duplicate_message = "A multivariate sampling specification named '%s' already exists.",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

.callbacks_add_mv_sampling <- list(
  pre_add = function(model, item) {
    if (item$type == "mvnormal" && (is.null(item$covariance) || identical(item$covariance, ""))) {
      stop("For mvnormal, 'covariance' must be provided.", call. = FALSE)
    }
    if (item$type == "dirichlet" && (is.null(item$n) || is.na(item$n))) {
      stop("For dirichlet, 'n' (effective sample size) must be a positive number.",
           call. = FALSE)
    }
    model
  }
)

# --- Threshold Analysis Schema -----------------------------------------------

.schema_threshold_analysis <- list(
  entity_name      = "threshold analysis",
  warn_replace_fn = function(key_fields, key_values) {
    sprintf("Threshold analysis '%s' already exists. Replacing.", key_values[[1]])
  },
  container        = "threshold_analyses",
  storage          = "list",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "warn_replace",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

# --- DSA Parameter Schema (nested, flat container) ---------------------------

.schema_dsa_param <- list(
  entity_name      = "DSA parameter",
  warn_replace_fn = function(key_fields, key_values) {
    # key_values: type, name, strategy, group
    nm <- key_values[[2]]; strat <- key_values[[3]]; grp <- key_values[[4]]
    suffix <- paste0(
      if (strat != "") paste0(" (strategy: ", strat, ")") else "",
      if (grp != "") paste0(" (group: ", grp, ")") else ""
    )
    sprintf("Replacing existing DSA specification for variable '%s'%s", nm, suffix)
  },
  container        = "dsa_parameters",
  storage          = "list",
  key_fields       = c("type", "name", "strategy", "group"),
  name_field       = "name",
  duplicate_action = "warn_replace",
  container_class  = "dsa_parameters",
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

.schema_dsa_setting <- list(
  entity_name      = "DSA setting",
  warn_replace_fn = function(key_fields, key_values) {
    sprintf("Replacing existing DSA specification for setting '%s'", key_values[[2]])
  },
  container        = "dsa_parameters",
  storage          = "list",
  key_fields       = c("type", "name"),
  name_field       = "name",
  duplicate_action = "warn_replace",
  container_class  = "dsa_parameters",
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

# --- Nested Child Schemas (scenario/TWSA/override children) ------------------

.schema_scenario_variable_override <- list(
  entity_name      = "scenario variable override",
  warn_replace_fn = function(key_fields, key_values) {
    nm <- key_values[[1]]; strat <- key_values[[2]]; grp <- key_values[[3]]
    suffix <- paste0(
      if (strat != "") paste0(" (strategy: ", strat, ")") else "",
      if (grp != "") paste0(" (group: ", grp, ")") else ""
    )
    sprintf("Replacing existing scenario variable override for '%s'%s", nm, suffix)
  },
  child_container  = "variable_overrides",
  key_fields       = c("name", "strategy", "group"),
  name_field       = "name",
  duplicate_action = "warn_replace"
)

.schema_scenario_setting_override <- list(
  entity_name      = "scenario setting override",
  warn_replace_fn = function(key_fields, key_values) {
    sprintf("Replacing existing scenario setting override for '%s'", key_values[[1]])
  },
  child_container  = "setting_overrides",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "warn_replace"
)

.schema_twsa_parameter <- list(
  entity_name      = "TWSA parameter",
  child_container  = "parameters",
  key_fields       = c("param_type", "name", "strategy", "group"),
  name_field       = "name",
  duplicate_action = "error"
)

.schema_twsa_setting_parameter <- list(
  entity_name      = "TWSA setting parameter",
  child_container  = "parameters",
  key_fields       = c("param_type", "name"),
  name_field       = "name",
  duplicate_action = "error"
)

.schema_override <- list(
  entity_name      = "override",
  duplicate_fn = function(key_fields, key_values) {
    tp <- key_values[[1]]; nm <- key_values[[2]]; strat <- key_values[[3]]; grp <- key_values[[4]]
    suffix <- paste0(
      if (strat != "") paste0(" (strategy: ", strat, ")") else "",
      if (grp != "") paste0(" (group: ", grp, ")") else ""
    )
    sprintf("An override for %s '%s'%s already exists in category", tp, nm, suffix)
  },
  child_container  = "overrides",
  key_fields       = c("type", "name", "strategy", "group"),
  name_field       = "name",
  duplicate_action = "error"
)

# --- State Schema ------------------------------------------------------------

.schema_state <- list(
  entity_name      = "state",
  duplicate_message = "A state named '%s' already exists. State names must be unique.",
  container        = "states",
  storage          = "tibble",
  key_fields       = "name",
  name_field       = "name",
  duplicate_action = "error",
  reserved_names   = c("All", "All Other"),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(
    list(container = "trees", field = "name", unique = TRUE,
         message = 'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.')
  ),
  cascade_refs     = list() # State cascade is model-type-dependent → handled by callbacks
)

.callbacks_add_state <- list(
  pre_add = function(model, item) {
    model_type <- tolower(model$settings$model_type)
    if (model_type == "decision_tree") {
      stop("Decision tree models do not support states. Use add_value() with state = 'decision_tree' instead.", call. = FALSE)
    }
    if (model_type %in% c("psm", "custom_psm")) {
      if ("initial_probability" %in% names(item) && !is.null(item$initial_probability) && !is.na(item$initial_probability)) {
        stop("PSM/Custom PSM models don't use initial_prob parameter. Remove it from add_state() call.")
      }
    }
    model
  }
)

.callbacks_edit_state <- list(
  pre_edit = function(model, match_info, updates) {
    model_type <- tolower(model$settings$model_type)
    if (model_type == "decision_tree") {
      stop("Decision tree models do not support states.", call. = FALSE)
    }
    markov_fields <- any(c("state_group", "share_state_time", "state_cycle_limit",
                           "state_cycle_limit_unit", "initial_probability") %in% names(updates))
    if (markov_fields && model_type %in% c("psm", "custom_psm")) {
      stop("PSM/Custom PSM models don't support state_group, share_state_time, state_cycle_limit, state_cycle_limit_unit, or initial_prob parameters.", call. = FALSE)
    }
    model
  },
  pre_rename = function(model, match_info, new_name, updates) {
    if (new_name %in% c("All", "All Other")) {
      stop(sprintf('"%s" is a reserved state name and cannot be used.', new_name), call. = FALSE)
    }
    model
  },
  post_rename = function(model, old_name, new_name) {
    model_type <- tolower(model$settings$model_type)
    # Cascade rename in transitions (model-type-dependent)
    if (is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
      if (model_type == "markov") {
        model$transitions$from_state[model$transitions$from_state %in% old_name] <- new_name
        model$transitions$to_state[model$transitions$to_state %in% old_name] <- new_name
      } else if (model_type == "custom_psm") {
        model$transitions$state[model$transitions$state %in% old_name] <- new_name
      }
    }
    # Cascade rename in values
    if (is.data.frame(model$values) && nrow(model$values) > 0) {
      model$values$state[model$values$state %in% old_name] <- new_name
      model$values$destination[model$values$destination %in% old_name] <- new_name
    }
    # Cascade rename in threshold analyses
    if (length(model$threshold_analyses) > 0) {
      for (i in seq_along(model$threshold_analyses)) {
        if (!is.null(model$threshold_analyses[[i]]$condition) &&
            identical(model$threshold_analyses[[i]]$condition$state, old_name)) {
          model$threshold_analyses[[i]]$condition$state <- new_name
        }
      }
    }
    model
  }
)

.callbacks_remove_state <- list(
  pre_remove = function(model, match_info, flags) {
    model_type <- tolower(model$settings$model_type)
    if (model_type == "decision_tree") stop("Decision tree models do not support states.", call. = FALSE)
    if (model_type == "psm") stop("Removing states from PSM models is not supported.", call. = FALSE)
    if (isTRUE(flags$error_on_dependencies)) {
      deps <- list()
      name <- match_info$name
      if (is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
        if (model_type == "markov") {
          dep_rows <- model$transitions$from_state %in% name | model$transitions$to_state %in% name
          if (any(dep_rows)) deps$transitions <- paste(model$transitions$from_state[dep_rows], "->", model$transitions$to_state[dep_rows])
        } else if (model_type == "custom_psm") {
          dep_rows <- model$transitions$state %in% name
          if (any(dep_rows)) deps$transitions <- model$transitions$state[dep_rows]
        }
      }
      if (is.data.frame(model$values) && nrow(model$values) > 0) {
        dep_vals <- model$values$state %in% name | model$values$destination %in% name
        if (any(dep_vals)) deps$values <- unique(model$values$name[dep_vals])
      }
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
        cond <- structure(class = c("state_has_dependencies", "error", "condition"),
          list(message = sprintf('Cannot remove state "%s": it has downstream dependencies.', name), dependencies = deps))
        stop(cond)
      }
    }
    model
  },
  post_remove = function(model, old_name) {
    model_type <- tolower(model$settings$model_type)
    if (is.data.frame(model$transitions) && nrow(model$transitions) > 0) {
      if (model_type == "markov") {
        keep <- !(model$transitions$from_state %in% old_name | model$transitions$to_state %in% old_name)
        model$transitions <- model$transitions[keep, , drop = FALSE]
      } else if (model_type == "custom_psm") {
        keep <- !(model$transitions$state %in% old_name)
        model$transitions <- model$transitions[keep, , drop = FALSE]
      }
    }
    if (is.data.frame(model$values) && nrow(model$values) > 0) {
      keep <- !(model$values$state %in% old_name | model$values$destination %in% old_name)
      model$values <- model$values[keep, , drop = FALSE]
    }
    if (length(model$threshold_analyses) > 0) {
      keep <- vapply(model$threshold_analyses, function(a) {
        !((!is.null(a$condition)) && identical(a$condition$state, old_name))
      }, logical(1))
      model$threshold_analyses <- model$threshold_analyses[keep]
    }
    model
  }
)

# --- Transition Schemas (3 model types) --------------------------------------

.schema_transition_markov <- list(
  entity_name      = "Markov transition",
  container        = "transitions",
  storage          = "tibble",
  key_fields       = c("from_state", "to_state"),
  name_field       = "from_state",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

.callbacks_add_transition_markov <- list(
  pre_add = function(model, item) {
    if (!is.null(model$states) && is.data.frame(model$states) && nrow(model$states) > 0) {
      available <- model$states$name
      if (!item$from_state %in% available)
        stop(sprintf("Transition references undefined from_state '%s'. Available states: %s", item$from_state, paste(available, collapse = ", ")), call. = FALSE)
      if (!item$to_state %in% available)
        stop(sprintf("Transition references undefined to_state '%s'. Available states: %s", item$to_state, paste(available, collapse = ", ")), call. = FALSE)
    }
    model
  }
)

.schema_transition_psm <- list(
  entity_name      = "PSM transition",
  container        = "transitions",
  storage          = "tibble",
  key_fields       = "endpoint",
  name_field       = "endpoint",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

.schema_transition_custom_psm <- list(
  entity_name      = "Custom PSM transition",
  container        = "transitions",
  storage          = "tibble",
  key_fields       = "state",
  name_field       = "state",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = FALSE,
  collision_checks = list(),
  cascade_refs     = list()
)

# --- Value Schema ------------------------------------------------------------

.schema_value <- list(
  entity_name      = "value",
  duplicate_fn = function(key_fields, key_values) {
    sprintf("Duplicate value: name '%s', state '%s', destination '%s' already exists.",
            key_values[[1]], key_values[[2]], key_values[[3]])
  },
  container        = "values",
  storage          = "tibble",
  key_fields       = c("name", "state", "destination"),
  name_field       = "name",
  duplicate_action = "error",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = TRUE,
  collision_checks = list(
    list(container = "trees", field = "name", unique = TRUE,
         message = 'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.')
  ),
  cascade_refs     = list() # Value cascade is custom (orphan detection) → handled by callbacks
)

.callbacks_add_value <- list(
  pre_add = function(model, item) {
    model_type <- tolower(model$settings$model_type %||% "markov")
    # custom_psm: no transitional values
    if (model_type == "custom_psm") {
      has_state <- !is.na(item$state) && item$state != "NA"
      has_dest <- !is.na(item$destination) && item$destination != "NA"
      if (has_state && has_dest) {
        stop("Custom PSM models do not support transitional values (both state and destination). Use residency values (state only) or model start values (no state/destination).")
      }
    }
    # decision_tree: no destination
    if (!is.na(item$state) && item$state == "decision_tree") {
      if (!is.na(item$destination) && item$destination != "NA" && item$destination != "") {
        stop("Values with state = 'decision_tree' cannot have a destination. Decision tree values are one-time payoffs, not transitional values.", call. = FALSE)
      }
    }
    # Transitional values need both state and destination
    has_state_for_tv <- !is.na(item$state) && item$state != "NA" && item$state != ""
    has_dest_for_tv <- !is.na(item$destination) && item$destination != "NA" && item$destination != ""
    if (has_dest_for_tv && !has_state_for_tv) {
      stop("Transitional values must specify both 'state' and 'destination'. Value '", item$name, "' has destination '", item$destination, "' but no state.", call. = FALSE)
    }
    # All/AllOther mutual exclusion
    state_str <- item$state
    is_residence <- is.na(item$destination) || item$destination == "NA"
    if (state_str %in% c("All", "All Other")) {
      if (!is_residence) stop(glue::glue("'{state_str}' cannot be used for transition values (where destination is set). It is only valid for residence values."), call. = FALSE)
      existing <- model$values
      if (!is.null(existing) && nrow(existing) > 0) {
        existing_residence <- existing[existing$name == item$name & (is.na(existing$destination) | existing$destination == "NA"), , drop = FALSE]
        if (state_str == "All" && nrow(existing_residence) > 0) stop(glue::glue("Value '{item$name}' already has residence value rows. Cannot add 'All' when other residence rows exist."), call. = FALSE)
        if (state_str == "All Other" && any(existing_residence$state == "All Other")) stop(glue::glue("Value '{item$name}' already has an 'All Other' residence row. Only one 'All Other' row is allowed per value name."), call. = FALSE)
      }
    } else if (is_residence && !is.na(item$state) && state_str != "NA") {
      existing <- model$values
      if (!is.null(existing) && nrow(existing) > 0) {
        existing_residence <- existing[existing$name == item$name & (is.na(existing$destination) | existing$destination == "NA"), , drop = FALSE]
        if (any(existing_residence$state == "All")) stop(glue::glue("Value '{item$name}' already uses state 'All'. Cannot add explicit state rows when 'All' is used."), call. = FALSE)
      }
    }
    # Table collision check
    if (length(model$tables) > 0 && item$name %in% names(model$tables)) {
      stop(sprintf("Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.", item$name))
    }
    model
  },
  post_add = function(model, item) {
    values_with_same_name <- model$values[model$values$name == item$name, ]
    if (nrow(values_with_same_name) > 1) {
      error_msg <- validate_value_display_names_for_builder(values_with_same_name, item$name)
      if (error_msg != "") stop(error_msg, call. = FALSE)
    }
    model
  }
)

.callbacks_edit_value <- list(
  pre_rename = function(model, match_info, new_name, updates) {
    # Name sharing check
    if (isTRUE(updates$.error_on_name_sharing)) {
      old_name_rows <- which(model$values$name == match_info$name)
      if (length(old_name_rows) > 1) {
        states_data <- fast_tibble(state = model$values$state[old_name_rows], destination = model$values$destination[old_name_rows])
        cond <- structure(class = c("value_name_shared", "error", "condition"),
          list(message = sprintf('Value "%s" is shared across %d rows.', match_info$name, length(old_name_rows)),
               shared_count = length(old_name_rows), states = states_data))
        stop(cond)
      }
    }
    # Store rename_all flag for post_rename
    if (isTRUE(updates$.rename_all)) attr(model, ".rename_all_flag") <- TRUE
    # Table collision
    if (length(model$tables) > 0 && new_name %in% names(model$tables)) {
      stop(sprintf("Name collision detected: the following names are used as both tables and values: %s. Please rename the table(s) or value(s) to avoid this conflict.", new_name), call. = FALSE)
    }
    # Merge detection and field adoption
    target_rows <- which(model$values$name == new_name)
    if (length(target_rows) > 0) {
      target_display <- model$values$display_name[target_rows[1]]
      target_desc <- model$values$description[target_rows[1]]
      current_display <- model$values$display_name[match_info$idx]
      current_desc <- model$values$description[match_info$idx]
      fields_differ <- !identical(current_display, target_display) || !identical(current_desc, target_desc)
      if (fields_differ && isTRUE(updates$.error_on_field_changes)) {
        field_changes <- list(display_name = list(old = current_display, new = target_display),
                              description = list(old = current_desc, new = target_desc))
        cond <- structure(class = c("value_field_changes", "error", "condition"),
          list(message = sprintf('Moving value to "%s" would change display_name and/or description.', new_name), field_changes = field_changes))
        stop(cond)
      }
      # Auto-adopt target fields unless explicitly provided
      if (is.null(updates$display_name)) model$values$display_name[match_info$idx] <- target_display
      if (is.null(updates$description)) model$values$description[match_info$idx] <- target_desc
    }
    model
  },
  post_rename = function(model, old_name, new_name) {
    # Check if rename_all was requested (stored in updates by wrapper)
    # The engine already renamed match_idx. For rename_all, rename remaining rows too.
    rename_all <- isTRUE(attr(model, ".rename_all_flag"))
    if (rename_all) {
      model$values$name[model$values$name == old_name] <- new_name
      model$summaries <- update_summary_value_refs(model$summaries, old_name, new_name)
      attr(model, ".rename_all_flag") <- NULL
    } else {
      # Orphan detection: if old name no longer exists, cascade to summaries
      if (!any(model$values$name == old_name)) {
        model$summaries <- update_summary_value_refs(model$summaries, old_name, new_name)
      }
    }
    # Post-rename display name validation
    values_with_new_name <- model$values[model$values$name == new_name, ]
    if (nrow(values_with_new_name) > 1) {
      error_msg <- validate_value_display_names_for_builder(values_with_new_name, new_name)
      if (error_msg != "") stop(error_msg, call. = FALSE)
    }
    model
  },
  post_edit = function(model, match_info, updates) {
    # If display_name or description changed (no rename), validate consistency
    if (!is.null(updates$new_name)) return(model) # already handled by post_rename
    if (!is.null(updates$display_name) || !is.null(updates$description)) {
      values_with_same_name <- model$values[model$values$name == match_info$name, ]
      if (nrow(values_with_same_name) > 1) {
        error_msg <- validate_value_display_names_for_builder(values_with_same_name, match_info$name)
        if (error_msg != "") stop(error_msg, call. = FALSE)
      }
    }
    model
  }
)

.callbacks_remove_value <- list(
  pre_remove = function(model, match_info, flags) {
    name <- match_info$name
    remaining <- model$values$name[-match_info$idx]
    name_fully_removed <- !(name %in% remaining)
    if (name_fully_removed && isTRUE(flags$error_on_dependencies)) {
      if (!is.null(model$summaries) && is.data.frame(model$summaries) && nrow(model$summaries) > 0) {
        dep_summaries <- character(0)
        for (i in seq_len(nrow(model$summaries))) {
          tokens <- trimws(strsplit(model$summaries$values[i], ",")[[1]])
          if (name %in% tokens) dep_summaries <- c(dep_summaries, model$summaries$name[i])
        }
        if (length(dep_summaries) > 0) {
          cond <- structure(class = c("value_has_dependencies", "error", "condition"),
            list(message = sprintf('Cannot remove value "%s": it has downstream dependencies.', name),
                 dependencies = list(summaries = dep_summaries)))
          stop(cond)
        }
      }
    }
    model
  },
  post_remove = function(model, old_name) {
    # If name fully gone, cascade to summaries
    if (!any(model$values$name == old_name)) {
      model$summaries <- update_summary_value_refs(model$summaries, old_name, NULL)
    }
    model
  }
)

# --- Variable Schema ---------------------------------------------------------

.schema_variable <- list(
  entity_name      = "variable",
  container        = "variables",
  storage          = "tibble",
  key_fields       = c("name", "strategy", "group"),
  name_field       = "name",
  duplicate_action = "skip",
  reserved_names   = character(0),
  container_class  = NULL,
  na_safe_keys     = TRUE,
  collision_checks = list(
    list(container = "trees", field = "name", unique = TRUE,
         message = 'Name collision detected: "%s" is already used as a decision tree name. Please use a different name.')
  ),
  cascade_refs     = list()
)

.callbacks_add_variable <- list(
  pre_add = function(model, item) {
    # Sampling targeting validation
    if (!is.null(item$sampling) && item$sampling != "") {
      existing_vars <- model$variables[model$variables$name == item$name, ]
      if (nrow(existing_vars) > 0) {
        existing_strategies <- unique(existing_vars$strategy[existing_vars$strategy != ""])
        if (length(existing_strategies) > 0 && item$strategy == "") {
          stop(sprintf(
            paste0("Variable '%s' already has strategy-specific definitions: %s\n",
                   "You must specify which strategy this sampling applies to using the 'strategy' parameter.\n",
                   "Example: add_variable(\"%s\", ..., strategy = \"%s\", sampling = ...)"),
            item$name, paste(existing_strategies, collapse = ", "), item$name, existing_strategies[1]
          ), call. = FALSE)
        }
        existing_groups <- unique(existing_vars$group[existing_vars$group != ""])
        if (length(existing_groups) > 0 && item$group == "") {
          stop(sprintf(
            paste0("Variable '%s' already has group-specific definitions: %s\n",
                   "You must specify which group this sampling applies to using the 'group' parameter.\n",
                   "Example: add_variable(\"%s\", ..., group = \"%s\", sampling = ...)"),
            item$name, paste(existing_groups, collapse = ", "), item$name, existing_groups[1]
          ), call. = FALSE)
        }
      }
    }
    model
  },
  post_add = function(model, item) {
    vars_with_same_name <- model$variables[model$variables$name == item$name, ]
    if (nrow(vars_with_same_name) > 1) {
      error_msg <- validate_variable_display_names_for_builder(vars_with_same_name, item$name)
      if (error_msg != "") stop(error_msg, call. = FALSE)
    }
    model
  }
)

# --- Shared validation constants --------------------------------------------

.valid_settings <- c(
  "timeframe", "timeframe_unit", "cycle_length", "cycle_length_unit",
  "discount_cost", "discount_outcomes", "half_cycle_method",
  "discount_timing", "discount_method",
  "reduce_state_cycle", "days_per_year", "discount_rate"
)

.valid_input_types <- c("numeric", "slider", "dropdown", "formula", "timeframe")

.valid_outputs <- c("outcomes", "costs", "nmb", "ce", "vbp", "trace")

# --- Input config builder (for overrides) -----------------------------------

build_input_config <- function(input_type, min = NULL, max = NULL,
                               step_size = NULL, options = NULL) {
  switch(input_type,
    "numeric"   = list(min = min %||% 0, max = max %||% 100),
    "slider"    = list(min = min %||% 0, max = max %||% 1,
                       step_size = step_size %||% 0.05),
    "dropdown"  = list(options = options %||% list()),
    "formula"   = list(),
    "timeframe" = list(),
    stop(sprintf("Invalid input_type: '%s'", input_type), call. = FALSE)
  )
}
