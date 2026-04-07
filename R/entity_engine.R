# ============================================================================
# Generic Entity CRUD Engine
# ============================================================================
# Provides schema-driven add/edit/remove operations for model entities.
# Domain-specific logic lives in callbacks defined per entity schema.
# ============================================================================

# --- Helpers -----------------------------------------------------------------

capture_nse <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr) && length(expr) == 1) return(expr)
  if (is.numeric(expr)) return(as.character(expr))
  rlang::expr_text(expr)
}

capture_nse_formula <- function(quo) {
  expr <- rlang::quo_get_expr(quo)
  if (is.numeric(expr)) return(as.oq_formula(as.character(expr)))
  if (is.character(expr) && length(expr) == 1) return(as.oq_formula(expr))
  as.oq_formula(rlang::expr_text(expr))
}

safe_field_eq <- function(a, b) {
  a_empty <- is.na(a) | (!is.na(a) & a == "")
  b_empty <- is.na(b) | (!is.na(b) & b == "")
  (a_empty & b_empty) | (!a_empty & !b_empty & as.character(a) == as.character(b))
}

validate_string <- function(value, label = "name", type_word = "character string") {
  if (missing(value) || !is.character(value) || length(value) != 1 ||
      is.na(value) || trimws(value) == "") {
    stop(sprintf("%s must be a non-empty %s.", label, type_word), call. = FALSE)
  }
}

check_collision <- function(model, name, collision) {
  target <- model[[collision$container]]
  if (is.null(target)) return(invisible(NULL))
  if (is.data.frame(target) && nrow(target) > 0) {
    existing <- if (is.null(collision$unique) || collision$unique) {
      unique(target[[collision$field]])
    } else {
      target[[collision$field]]
    }
    if (name %in% existing) {
      stop(sprintf(collision$message, name), call. = FALSE)
    }
  }
}

# --- Finding items -----------------------------------------------------------

find_in_tibble <- function(df, key_fields, key_values, entity_name,
                           na_safe = FALSE, not_found_message = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    msg <- if (!is.null(not_found_message)) {
      sprintf(not_found_message, key_values[[1]])
    } else {
      sprintf('%s not found.', entity_name)
    }
    stop(msg, call. = FALSE)
  }
  mask <- rep(TRUE, nrow(df))
  for (k in seq_along(key_fields)) {
    col <- df[[key_fields[k]]]
    val <- key_values[[k]]
    if (na_safe) {
      mask <- mask & safe_field_eq(col, val)
    } else {
      mask <- mask & (col == val)
    }
  }
  idx <- which(mask)
  if (length(idx) == 0) {
    msg <- if (!is.null(not_found_message)) {
      sprintf(not_found_message, key_values[[1]])
    } else {
      desc <- paste(sprintf("%s='%s'", key_fields, key_values), collapse = ", ")
      sprintf('%s not found: %s', entity_name, desc)
    }
    stop(msg, call. = FALSE)
  }
  idx
}

find_in_list <- function(items, key_fields, key_values, entity_name,
                         not_found_message = NULL) {
  .not_found <- function() {
    msg <- if (!is.null(not_found_message)) {
      sprintf(not_found_message, key_values[[1]])
    } else {
      desc <- paste(sprintf("%s='%s'", key_fields, key_values), collapse = ", ")
      sprintf('%s not found: %s', entity_name, desc)
    }
    stop(msg, call. = FALSE)
  }
  if (length(items) == 0) .not_found()
  for (i in seq_along(items)) {
    match <- TRUE
    for (k in seq_along(key_fields)) {
      if (!identical(as.character(items[[i]][[key_fields[k]]]),
                     as.character(key_values[[k]]))) {
        match <- FALSE
        break
      }
    }
    if (match) return(i)
  }
  .not_found()
}

# --- Duplicate checking ------------------------------------------------------

check_duplicate_tibble <- function(df, key_fields, key_values, schema, na_safe = FALSE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  mask <- rep(TRUE, nrow(df))
  case_insensitive <- isTRUE(schema$case_insensitive_keys)
  for (k in seq_along(key_fields)) {
    col <- df[[key_fields[k]]]
    val <- key_values[[k]]
    if (na_safe) {
      mask <- mask & safe_field_eq(col, val)
    } else if (case_insensitive) {
      mask <- mask & (tolower(col) == tolower(val))
    } else {
      mask <- mask & (col == val)
    }
  }
  if (!any(mask)) return(NULL)
  dup_idx <- which(mask)
  name_val <- key_values[[1]]
  if (identical(schema$duplicate_action, "warn_replace")) {
    msg <- if (is.function(schema$warn_replace_fn)) {
      schema$warn_replace_fn(key_fields, key_values)
    } else if (!is.null(schema$warn_replace_message)) {
      do.call(sprintf, c(list(schema$warn_replace_message), as.list(key_values)))
    } else {
      key_desc <- paste(sprintf("%s='%s'", key_fields, key_values), collapse = ", ")
      sprintf("Replacing existing %s (%s).", schema$entity_name, key_desc)
    }
    warning(msg, call. = FALSE)
    return(list(action = "replace", idx = dup_idx))
  }
  msg <- if (is.function(schema$duplicate_fn)) {
    schema$duplicate_fn(key_fields, key_values)
  } else if (!is.null(schema$duplicate_message)) {
    sprintf(schema$duplicate_message, name_val)
  } else {
    key_desc <- paste(sprintf("%s='%s'", key_fields, key_values), collapse = ", ")
    sprintf('%s already exists: %s', schema$entity_name, key_desc)
  }
  stop(msg, call. = FALSE)
}

check_duplicate_list <- function(items, key_fields, key_values, schema,
                                 exclude_idx = NULL) {
  if (length(items) == 0) return(NULL)
  case_insensitive <- isTRUE(schema$case_insensitive_keys)
  for (i in seq_along(items)) {
    if (!is.null(exclude_idx) && i == exclude_idx) next
    match <- TRUE
    for (k in seq_along(key_fields)) {
      a <- as.character(items[[i]][[key_fields[k]]])
      b <- as.character(key_values[[k]])
      if (case_insensitive) {
        if (tolower(a) != tolower(b)) { match <- FALSE; break }
      } else {
        if (!identical(a, b)) { match <- FALSE; break }
      }
    }
    if (match) {
      name_val <- key_values[[1]]
      if (identical(schema$duplicate_action, "warn_replace")) {
        msg <- if (is.function(schema$warn_replace_fn)) {
          schema$warn_replace_fn(key_fields, key_values)
        } else if (!is.null(schema$warn_replace_message)) {
          do.call(sprintf, c(list(schema$warn_replace_message), as.list(key_values)))
        } else {
          key_desc <- paste(sprintf("%s='%s'", key_fields, key_values), collapse = ", ")
          sprintf("Replacing existing %s (%s).", schema$entity_name, key_desc)
        }
        warning(msg, call. = FALSE)
        return(list(action = "replace", idx = i))
      }
      msg <- if (is.function(schema$duplicate_fn)) {
        schema$duplicate_fn(key_fields, key_values)
      } else if (!is.null(schema$duplicate_message)) {
        sprintf(schema$duplicate_message, name_val)
      } else {
        key_desc <- paste(sprintf("%s='%s'", key_fields, key_values), collapse = ", ")
        sprintf('%s already exists: %s', schema$entity_name, key_desc)
      }
      stop(msg, call. = FALSE)
    }
  }
  NULL
}

# --- CRUD: Tibble-stored entities -------------------------------------------

entity_add_tibble <- function(model, schema, new_row, callbacks = list()) {
  name_val <- new_row[[schema$name_field]]

  if (length(schema$reserved_names) > 0 && name_val %in% schema$reserved_names) {
    stop(sprintf("'%s' is a reserved %s name.", name_val, schema$entity_name),
         call. = FALSE)
  }

  for (cc in schema$collision_checks) {
    check_collision(model, name_val, cc)
  }

  # pre_add runs before duplicate check so domain validation (e.g. All/AllOther) fires first
  if (is.function(callbacks$pre_add)) {
    model <- callbacks$pre_add(model, new_row)
  }

  if (!identical(schema$duplicate_action, "skip")) {
    dup <- check_duplicate_tibble(
      model[[schema$container]], schema$key_fields,
      lapply(schema$key_fields, function(k) new_row[[k]]),
      schema, na_safe = isTRUE(schema$na_safe_keys)
    )
    if (!is.null(dup) && dup$action == "replace") {
      model[[schema$container]] <- model[[schema$container]][-dup$idx, , drop = FALSE]
    }
  }

  model[[schema$container]] <- dplyr::bind_rows(model[[schema$container]], new_row)

  if (is.function(callbacks$post_add)) {
    model <- callbacks$post_add(model, new_row)
  }

  model
}

entity_edit_tibble <- function(model, schema, match_idx, updates,
                               callbacks = list()) {
  if (length(match_idx) == 0) {
    stop(sprintf('%s not found in model.', schema$entity_name), call. = FALSE)
  }

  match_info <- list(
    idx = match_idx,
    name = model[[schema$container]][[schema$name_field]][match_idx[1]]
  )

  if (is.function(callbacks$pre_edit)) {
    model <- callbacks$pre_edit(model, match_info, updates)
  }

  new_name <- updates[["new_name"]]
  field_updates <- updates[setdiff(names(updates), "new_name")]
  for (field_name in names(field_updates)) {
    model[[schema$container]][[field_name]][match_idx] <- field_updates[[field_name]]
  }

  if (!is.null(new_name)) {
    old_name <- match_info$name

    if (is.function(callbacks$pre_rename)) {
      model <- callbacks$pre_rename(model, match_info, new_name, updates)
    }

    model[[schema$container]][[schema$name_field]][match_idx] <- new_name

    if (length(schema$cascade_refs) > 0) {
      model <- cascade_rename(model, schema$cascade_refs, old_name, new_name)
    }

    if (is.function(callbacks$post_rename)) {
      model <- callbacks$post_rename(model, old_name, new_name)
    }
  }

  # Check for duplicate keys after edits
  df <- model[[schema$container]]
  new_key_values <- lapply(schema$key_fields, function(k) df[[k]][match_idx[1]])
  other_mask <- rep(TRUE, nrow(df))
  other_mask[match_idx] <- FALSE
  if (any(other_mask)) {
    dup_mask <- other_mask
    na_safe <- isTRUE(schema$na_safe_keys)
    for (k in seq_along(schema$key_fields)) {
      col <- df[[schema$key_fields[k]]]
      val <- new_key_values[[k]]
      if (na_safe) {
        dup_mask <- dup_mask & safe_field_eq(col, val)
      } else if (isTRUE(schema$case_insensitive_keys)) {
        dup_mask <- dup_mask & (tolower(col) == tolower(val))
      } else {
        dup_mask <- dup_mask & (col == val)
      }
    }
    if (any(dup_mask)) {
      key_desc <- paste(sprintf("%s='%s'", schema$key_fields, new_key_values),
                        collapse = ", ")
      stop(sprintf('%s already exists: %s', schema$entity_name, key_desc),
           call. = FALSE)
    }
  }

  if (is.function(callbacks$post_edit)) {
    model <- callbacks$post_edit(model, match_info, updates)
  }

  model
}

entity_remove_tibble <- function(model, schema, match_idx, callbacks = list(),
                                 flags = list()) {
  if (length(match_idx) == 0) {
    stop(sprintf('%s not found in model.', schema$entity_name), call. = FALSE)
  }

  old_name <- model[[schema$container]][[schema$name_field]][match_idx[1]]
  match_info <- list(idx = match_idx, name = old_name)

  if (is.function(callbacks$pre_remove)) {
    model <- callbacks$pre_remove(model, match_info, flags)
  }

  if (isTRUE(flags$error_on_dependencies) && length(schema$cascade_refs) > 0) {
    deps <- cascade_collect_deps(model, schema$cascade_refs, old_name)
    if (length(deps) > 0) {
      cond <- structure(
        class = c(paste0(schema$entity_name, "_has_dependencies"), "error", "condition"),
        list(
          message = sprintf('Cannot remove %s "%s": it has downstream dependencies.',
                            schema$entity_name, old_name),
          dependencies = deps
        )
      )
      stop(cond)
    }
  }

  model[[schema$container]] <- model[[schema$container]][-match_idx, , drop = FALSE]

  if (length(schema$cascade_refs) > 0) {
    model <- cascade_remove_refs(model, schema$cascade_refs, old_name)
  }

  if (is.function(callbacks$post_remove)) {
    model <- callbacks$post_remove(model, old_name)
  }

  model
}

# --- CRUD: List-stored entities ----------------------------------------------

entity_add_list <- function(model, schema, new_item, callbacks = list()) {
  if (is.null(model[[schema$container]])) {
    model[[schema$container]] <- list()
  }

  name_val <- new_item[[schema$name_field]]

  if (length(schema$reserved_names) > 0) {
    if (tolower(name_val) %in% tolower(schema$reserved_names)) {
      msg <- if (!is.null(schema$reserved_name_message)) {
        schema$reserved_name_message
      } else {
        sprintf("'%s' is a reserved %s name.", name_val, schema$entity_name)
      }
      stop(msg, call. = FALSE)
    }
  }

  dup <- check_duplicate_list(
    model[[schema$container]], schema$key_fields,
    lapply(schema$key_fields, function(k) new_item[[k]]),
    schema
  )
  if (!is.null(dup) && dup$action == "replace") {
    model[[schema$container]] <- model[[schema$container]][-dup$idx]
  }

  if (is.function(callbacks$pre_add)) {
    model <- callbacks$pre_add(model, new_item)
  }

  model[[schema$container]] <- c(model[[schema$container]], list(new_item))

  if (!is.null(schema$container_class)) {
    class(model[[schema$container]]) <- schema$container_class
  }

  if (is.function(callbacks$post_add)) {
    model <- callbacks$post_add(model, new_item)
  }

  model
}

entity_edit_list <- function(model, schema, item_idx, updates,
                             callbacks = list()) {
  container <- model[[schema$container]]
  if (item_idx == 0L || item_idx > length(container)) {
    stop(sprintf('%s not found.', schema$entity_name), call. = FALSE)
  }

  item <- container[[item_idx]]
  match_info <- list(idx = item_idx, name = item[[schema$name_field]])

  if (is.function(callbacks$pre_edit)) {
    model <- callbacks$pre_edit(model, match_info, updates)
  }

  new_name <- updates[["new_name"]]
  field_updates <- updates[setdiff(names(updates), "new_name")]
  for (field_name in names(field_updates)) {
    model[[schema$container]][[item_idx]][[field_name]] <- field_updates[[field_name]]
  }

  if (!is.null(new_name)) {
    old_name <- match_info$name

    # Check reserved names on rename
    if (length(schema$reserved_names) > 0) {
      if (tolower(new_name) %in% tolower(schema$reserved_names)) {
        msg <- if (!is.null(schema$reserved_name_message)) {
          schema$reserved_name_message
        } else {
          sprintf("'%s' is a reserved %s name.", new_name, schema$entity_name)
        }
        stop(msg, call. = FALSE)
      }
    }

    # Check duplicates on rename (excluding self)
    dup <- check_duplicate_list(
      model[[schema$container]], schema$key_fields, list(new_name),
      list(entity_name = schema$entity_name, duplicate_action = "error",
           duplicate_message = schema$rename_duplicate_message,
           case_insensitive_keys = isTRUE(schema$case_insensitive_keys)),
      exclude_idx = item_idx
    )

    if (is.function(callbacks$pre_rename)) {
      model <- callbacks$pre_rename(model, match_info, new_name, updates)
    }

    model[[schema$container]][[item_idx]][[schema$name_field]] <- new_name

    if (isTRUE(schema$auto_update_description)) {
      if (identical(model[[schema$container]][[item_idx]]$description, old_name)) {
        if (is.null(updates$description)) {
          model[[schema$container]][[item_idx]]$description <- new_name
        }
      }
    }

    if (length(schema$cascade_refs) > 0) {
      model <- cascade_rename(model, schema$cascade_refs, old_name, new_name)
    }

    if (is.function(callbacks$post_rename)) {
      model <- callbacks$post_rename(model, old_name, new_name)
    }
  }

  if (!is.null(schema$container_class)) {
    class(model[[schema$container]]) <- schema$container_class
  }

  if (is.function(callbacks$post_edit)) {
    model <- callbacks$post_edit(model, match_info, updates)
  }

  model
}

entity_remove_list <- function(model, schema, item_idx, callbacks = list(),
                               flags = list()) {
  container <- model[[schema$container]]
  if (item_idx == 0L || item_idx > length(container)) {
    stop(sprintf('%s not found.', schema$entity_name), call. = FALSE)
  }

  old_name <- container[[item_idx]][[schema$name_field]]
  match_info <- list(idx = item_idx, name = old_name)

  if (is.function(callbacks$pre_remove)) {
    model <- callbacks$pre_remove(model, match_info, flags)
  }

  model[[schema$container]] <- model[[schema$container]][-item_idx]

  if (!is.null(schema$container_class)) {
    class(model[[schema$container]]) <- schema$container_class
  }

  if (is.function(callbacks$post_remove)) {
    model <- callbacks$post_remove(model, old_name)
  }

  model
}

# --- CRUD: Nested entities (child inside parent list) ------------------------

entity_add_nested <- function(model, parent_schema, parent_key,
                              child_schema, new_item, callbacks = list()) {
  parent_container <- model[[parent_schema$container]]
  parent_idx <- find_in_list(
    parent_container, parent_schema$key_fields, list(parent_key),
    parent_schema$entity_name
  )

  child_list <- model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]]
  if (is.null(child_list)) child_list <- list()

  dup <- check_duplicate_list(
    child_list, child_schema$key_fields,
    lapply(child_schema$key_fields, function(k) new_item[[k]]),
    child_schema
  )
  if (!is.null(dup) && dup$action == "replace") {
    model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]] <-
      child_list[-dup$idx]
  }

  if (is.function(callbacks$pre_add)) {
    model <- callbacks$pre_add(model, new_item,
                               list(parent_idx = parent_idx, parent_key = parent_key))
  }

  model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]] <- c(
    model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]],
    list(new_item)
  )

  if (is.function(callbacks$post_add)) {
    model <- callbacks$post_add(model, new_item,
                                list(parent_idx = parent_idx, parent_key = parent_key))
  }

  model
}

entity_edit_nested <- function(model, parent_schema, parent_idx,
                               child_schema, child_idx, updates,
                               callbacks = list()) {
  child_list <- model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]]
  if (child_idx == 0L || child_idx > length(child_list)) {
    stop(sprintf('%s not found in %s.',
                 child_schema$entity_name, parent_schema$entity_name), call. = FALSE)
  }

  match_info <- list(
    parent_idx = parent_idx, child_idx = child_idx,
    name = child_list[[child_idx]][[child_schema$name_field]]
  )

  if (is.function(callbacks$pre_edit)) {
    model <- callbacks$pre_edit(model, match_info, updates)
  }

  for (field_name in names(updates)) {
    model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]][[child_idx]][[field_name]] <- updates[[field_name]]
  }

  if (is.function(callbacks$post_edit)) {
    model <- callbacks$post_edit(model, match_info, updates)
  }

  model
}

entity_remove_nested <- function(model, parent_schema, parent_idx,
                                 child_schema, child_idx, callbacks = list(),
                                 flags = list()) {
  child_list <- model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]]
  if (child_idx == 0L || child_idx > length(child_list)) {
    stop(sprintf('%s not found in %s.',
                 child_schema$entity_name, parent_schema$entity_name), call. = FALSE)
  }

  old_name <- child_list[[child_idx]][[child_schema$name_field]]
  match_info <- list(parent_idx = parent_idx, child_idx = child_idx, name = old_name)

  if (is.function(callbacks$pre_remove)) {
    model <- callbacks$pre_remove(model, match_info, flags)
  }

  model[[parent_schema$container]][[parent_idx]][[child_schema$child_container]] <-
    child_list[-child_idx]

  if (is.function(callbacks$post_remove)) {
    model <- callbacks$post_remove(model, old_name)
  }

  model
}

# --- Cascade Engine ----------------------------------------------------------

cascade_rename <- function(model, refs, old_value, new_value) {
  for (ref in refs) {
    if (ref$storage == "tibble") {
      tbl <- model[[ref$container]]
      if (!is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0) {
        for (fld in ref$fields) {
          mask <- tbl[[fld]] %in% old_value
          if (any(mask)) {
            model[[ref$container]][[fld]][mask] <- new_value
          }
        }
      }

    } else if (ref$storage == "list" && is.null(ref$child_list)) {
      container <- model[[ref$container]]
      if (length(container) > 0) {
        if (!is.null(ref$accessor)) {
          for (i in seq_along(container)) {
            model[[ref$container]][[i]] <- ref$mutator(container[[i]], old_value, new_value)
          }
        } else {
          for (i in seq_along(container)) {
            for (fld in ref$fields) {
              if (identical(container[[i]][[fld]], old_value)) {
                model[[ref$container]][[i]][[fld]] <- new_value
              }
            }
          }
        }
      }

    } else if (ref$storage == "list" && !is.null(ref$child_list)) {
      container <- model[[ref$container]]
      if (length(container) > 0) {
        for (i in seq_along(container)) {
          children <- container[[i]][[ref$child_list]]
          if (length(children) > 0) {
            for (j in seq_along(children)) {
              for (fld in ref$fields) {
                if (identical(children[[j]][[fld]], old_value)) {
                  model[[ref$container]][[i]][[ref$child_list]][[j]][[fld]] <- new_value
                }
              }
            }
          }
        }
      }
    }
  }
  model
}

cascade_collect_deps <- function(model, refs, value) {
  deps <- list()

  for (ref in refs) {
    dep_names <- character(0)

    if (ref$storage == "tibble") {
      tbl <- model[[ref$container]]
      if (!is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0) {
        mask <- rep(FALSE, nrow(tbl))
        for (fld in ref$fields) {
          mask <- mask | (tbl[[fld]] %in% value)
        }
        if (any(mask)) {
          if ("name" %in% names(tbl)) {
            dep_names <- unique(tbl$name[mask])
          } else {
            dep_names <- paste("row", which(mask))
          }
        }
      }

    } else if (ref$storage == "list" && is.null(ref$child_list)) {
      container <- model[[ref$container]]
      if (length(container) > 0) {
        if (!is.null(ref$dep_predicate)) {
          for (i in seq_along(container)) {
            if (ref$dep_predicate(container[[i]], value)) {
              dep_names <- c(dep_names, container[[i]]$name %||% paste0("item_", i))
            }
          }
        } else {
          for (i in seq_along(container)) {
            for (fld in ref$fields) {
              if (identical(container[[i]][[fld]], value)) {
                dep_names <- c(dep_names, container[[i]]$name %||% paste0("item_", i))
                break
              }
            }
          }
        }
      }

    } else if (ref$storage == "list" && !is.null(ref$child_list)) {
      container <- model[[ref$container]]
      if (length(container) > 0) {
        for (i in seq_along(container)) {
          children <- container[[i]][[ref$child_list]]
          if (length(children) > 0) {
            for (j in seq_along(children)) {
              for (fld in ref$fields) {
                if (identical(children[[j]][[fld]], value)) {
                  dep_names <- c(dep_names,
                                 children[[j]]$name %||% children[[j]]$title %||% paste0("item_", j))
                  break
                }
              }
            }
          }
        }
      }
    }

    if (length(dep_names) > 0) {
      deps[[ref$container]] <- dep_names
    }
  }

  Filter(function(x) length(x) > 0, deps)
}

cascade_remove_refs <- function(model, refs, value) {
  for (ref in refs) {
    if (ref$storage == "tibble") {
      tbl <- model[[ref$container]]
      if (!is.null(tbl) && is.data.frame(tbl) && nrow(tbl) > 0) {
        keep <- rep(TRUE, nrow(tbl))
        for (fld in ref$fields) {
          keep <- keep & !(tbl[[fld]] %in% value)
        }
        model[[ref$container]] <- tbl[keep, , drop = FALSE]
      }

    } else if (ref$storage == "list" && is.null(ref$child_list)) {
      container <- model[[ref$container]]
      if (length(container) > 0) {
        if (!is.null(ref$remove_predicate)) {
          keep <- vapply(container, function(item) !ref$remove_predicate(item, value),
                         logical(1))
        } else {
          keep <- vapply(container, function(item) {
            for (fld in ref$fields) {
              if (identical(item[[fld]], value)) return(FALSE)
            }
            TRUE
          }, logical(1))
        }
        model[[ref$container]] <- container[keep]
      }

    } else if (ref$storage == "list" && !is.null(ref$child_list)) {
      container <- model[[ref$container]]
      if (length(container) > 0) {
        for (i in seq_along(container)) {
          children <- container[[i]][[ref$child_list]]
          if (length(children) > 0) {
            keep <- vapply(children, function(child) {
              for (fld in ref$fields) {
                if (identical(child[[fld]], value)) return(FALSE)
              }
              TRUE
            }, logical(1))
            model[[ref$container]][[i]][[ref$child_list]] <- children[keep]
          }
        }
        if (!is.null(ref$min_children)) {
          keep_parents <- vapply(model[[ref$container]], function(item) {
            length(item[[ref$child_list]]) >= ref$min_children
          }, logical(1))
          model[[ref$container]] <- model[[ref$container]][keep_parents]
        }
      }
    }
  }
  model
}
