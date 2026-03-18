#' Diagnose a Variable
#'
#' Takes a `run_model()` results object and a variable name, and returns
#' rich diagnostic output including formatted text, flextables, and plots.
#' Intelligently handles strategy/group variation: if the variable takes the
#' same value across all segments, it is shown once. If it varies, separate
#' results are labeled by what actually differs.
#'
#' @param results A results object from [run_model()].
#' @param name Character. The name of the variable to diagnose.
#'
#' @return A list with fields: `name`, `display_name`, `formula_text`,
#'   `type`, `text` (single string), `table` (a flextable or NULL),
#'   and `plot` (a ggplot or NULL).
#'
#' @export
diagnose_variable <- function(results, name) {

  segments <- results$segments
  segments <- segments[segments$group != "_aggregated", ]

  if (nrow(segments) == 0) {
    stop("No non-aggregated segments found in results.", call. = FALSE)
  }

  if (!"eval_vars" %in% colnames(segments)) {
    stop("Results do not contain eval_vars. Re-run the model to include diagnostics.",
         call. = FALSE)
  }

  # Extract variable metadata
  var_meta <- NULL
  if (!is.null(results$metadata$variables) &&
      is.data.frame(results$metadata$variables) &&
      nrow(results$metadata$variables) > 0) {
    matches <- results$metadata$variables[results$metadata$variables$name == name, ]
    if (nrow(matches) > 0) {
      var_meta <- matches[1, ]
    }
  }

  display_name <- if (!is.null(var_meta) && "display_name" %in% names(var_meta)) {
    var_meta$display_name
  } else {
    NULL
  }

  # Extract evaluated values and formula text from each segment
  seg_data <- lapply(seq_len(nrow(segments)), function(i) {
    seg <- segments[i, ]
    ns <- seg$eval_vars[[1]]
    value <- extract_var_from_ns(ns, name)
    is_df_var <- name %in% colnames(ns$df)

    # For df vars, extract time-aware data
    var_df <- NULL
    if (is_df_var) {
      var_df <- data.frame(
        cycle = ns$df$cycle,
        state_cycle = ns$df$state_cycle,
        value = ns$df[[name]]
      )
    }

    # Get formula text from uneval_vars
    formula_text <- NULL
    if ("uneval_vars" %in% colnames(segments)) {
      uv <- seg$uneval_vars[[1]]
      if (!is.null(uv) && is.data.frame(uv) && "name" %in% colnames(uv)) {
        idx <- which(uv$name == name)
        if (length(idx) > 0) {
          f <- uv$formula[[idx[1]]]
          if (!is.null(f) && !is.null(f$text)) {
            formula_text <- f$text
          }
        }
      }
    }

    list(
      strategy = seg$strategy,
      group = seg$group,
      value = value,
      formula_text = formula_text,
      is_df_var = is_df_var,
      var_df = var_df
    )
  })

  # Get formula text (should be same across segments)
  formula_text <- NULL
  for (sd in seg_data) {
    if (!is.null(sd$formula_text)) {
      formula_text <- sd$formula_text
      break
    }
  }

  # Detect variation
  strategies <- unique(sapply(seg_data, `[[`, "strategy"))
  groups <- unique(sapply(seg_data, `[[`, "group"))

  varies_by_strategy <- FALSE
  varies_by_group <- FALSE

  if (length(strategies) > 1) {
    varies_by_strategy <- detect_variation_across(seg_data, "strategy", "group")
  }
  if (length(groups) > 1) {
    varies_by_group <- detect_variation_across(seg_data, "group", "strategy")
  }

  # Build rich output
  rich <- build_diagnostic_results(
    seg_data, varies_by_strategy, varies_by_group, name
  )

  list(
    name = name,
    display_name = display_name,
    formula_text = formula_text,
    type = rich$type,
    text = rich$text,
    table = rich$table,
    plot = rich$plot
  )
}

#' Diagnose All Variables
#'
#' Convenience function that calls [diagnose_variable()] for every variable
#' in the model and returns a named list of results.
#'
#' @param results A results object from [run_model()].
#'
#' @return A named list where each element is the output of
#'   `diagnose_variable()` for one variable.
#'
#' @export
diagnose_all_variables <- function(results) {
  if (is.null(results$metadata$variables) ||
      !is.data.frame(results$metadata$variables) ||
      nrow(results$metadata$variables) == 0) {
    return(list())
  }
  var_names <- unique(results$metadata$variables$name)
  res <- lapply(var_names, function(nm) {
    tryCatch(
      diagnose_variable(results, nm),
      error = function(e) {
        list(
          name = nm,
          error = conditionMessage(e)
        )
      }
    )
  })
  names(res) <- var_names
  res
}


# --- Internal helpers ---

#' Extract a variable value from a namespace
#' @keywords internal
extract_var_from_ns <- function(ns, name) {
  if (name %in% colnames(ns$df)) {
    return(ns$df[[name]])
  }
  if (exists(name, envir = ns$env, inherits = FALSE)) {
    return(get(name, envir = ns$env))
  }
  NULL
}

#' Compare two evaluated variable values
#' @keywords internal
values_are_equal <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)

  if (is.numeric(a) && is.numeric(b)) {
    res <- all.equal(a, b, tolerance = .Machine$double.eps^0.5)
    return(isTRUE(res))
  }

  identical(a, b)
}

#' Detect whether a variable varies across levels of a dimension
#'
#' @param seg_data List of segment data (each with strategy, group, value).
#' @param vary_dim The dimension to check for variation ("strategy" or "group").
#' @param hold_dim The other dimension to hold constant.
#' @keywords internal
detect_variation_across <- function(seg_data, vary_dim, hold_dim) {
  hold_levels <- unique(sapply(seg_data, `[[`, hold_dim))

  for (hl in hold_levels) {
    subset <- seg_data[sapply(seg_data, `[[`, hold_dim) == hl]
    if (length(subset) < 2) next

    ref_value <- subset[[1]]$value
    for (j in seq_along(subset)[-1]) {
      if (!values_are_equal(ref_value, subset[[j]]$value)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Build diagnostic results with rich text, tables, and plots
#'
#' @param seg_data List of per-segment data.
#' @param varies_by_strategy Logical.
#' @param varies_by_group Logical.
#' @param name Variable name.
#' @return List with `type`, `text`, `table`, `plot`.
#' @keywords internal
build_diagnostic_results <- function(seg_data, varies_by_strategy,
                                     varies_by_group, name) {
  # Get display segments and labels
  disp <- .get_display_segments(seg_data, varies_by_strategy, varies_by_group)
  segs <- disp$segments
  labels <- disp$labels

  # Detect type from first segment
  var_type <- .detect_var_type(segs[[1]]$value, segs[[1]]$is_df_var)

  rich <- switch(var_type,
    scalar = .build_scalar_output(segs, labels, name,
                                    varies_by_strategy, varies_by_group),
    vector = .build_vector_output(segs, labels, name),
    surv_dist = .build_surv_dist_output(segs, labels, name,
                                        varies_by_strategy, varies_by_group),
    eval_decision_tree = .build_decision_tree_output(segs, labels, name),
    data.frame = .build_data_frame_output(segs, labels, name),
    med_regimen = .build_med_regimen_output(segs, labels, name),
    med_regimen_combo = .build_med_regimen_combo_output(segs, labels, name),
    .build_object_output(segs, labels, name)
  )

  rich$type <- var_type
  rich
}


# =============================================================================
# Type detection
# =============================================================================

.detect_var_type <- function(value, is_df_var) {
  if (is_df_var) {
    if (length(unique(value)) == 1) return("scalar")
    return("vector")
  }
  if (inherits(value, "surv_dist")) return("surv_dist")
  if (inherits(value, "eval_decision_tree")) return("eval_decision_tree")
  if (inherits(value, "med_regimen_combo")) return("med_regimen_combo")
  if (inherits(value, "med_regimen")) return("med_regimen")
  if (is.data.frame(value)) return("data.frame")
  if (is.numeric(value) && !is.list(value) && length(value) == 1) return("scalar")
  "object"
}


# =============================================================================
# Segment deduplication and labeling
# =============================================================================

.get_display_segments <- function(seg_data, varies_by_strategy, varies_by_group) {
  if (!varies_by_strategy && !varies_by_group) {
    return(list(segments = seg_data[1], labels = NULL))
  }

  if (varies_by_strategy && !varies_by_group) {
    strategies <- unique(sapply(seg_data, `[[`, "strategy"))
    segs <- lapply(strategies, function(s) {
      seg_data[sapply(seg_data, `[[`, "strategy") == s][[1]]
    })
    return(list(segments = segs, labels = strategies))
  }

  if (!varies_by_strategy && varies_by_group) {
    groups <- unique(sapply(seg_data, `[[`, "group"))
    segs <- lapply(groups, function(g) {
      seg_data[sapply(seg_data, `[[`, "group") == g][[1]]
    })
    return(list(segments = segs, labels = groups))
  }

  # Both vary
  labels <- sapply(seg_data, function(seg) {
    paste(seg$strategy, seg$group, sep = ", ")
  })
  list(segments = seg_data, labels = labels)
}


# =============================================================================
# Type-specific output builders
# =============================================================================

# --- Scalar ---

.build_scalar_output <- function(segs, labels, name,
                                  varies_by_strategy = FALSE,
                                  varies_by_group = FALSE) {
  varies <- !is.null(labels)

  if (!varies) {
    value <- segs[[1]]$value
    if (segs[[1]]$is_df_var) value <- unique(value)
    formatted <- oq_format(value)

    text <- formatted
    table <- NULL
    plot <- NULL
  } else {
    values <- sapply(segs, function(seg) {
      v <- seg$value
      if (seg$is_df_var) v <- unique(v)
      v
    })
    formatted <- oq_format(values)

    # Build table with separate columns for varying dimensions
    table_df <- data.frame(stringsAsFactors = FALSE)
    if (varies_by_strategy && varies_by_group) {
      parsed <- strsplit(labels, ", ", fixed = TRUE)
      table_df <- data.frame(
        Strategy = sapply(parsed, `[`, 1),
        Group = sapply(parsed, `[`, 2),
        Value = formatted,
        stringsAsFactors = FALSE
      )
    } else if (varies_by_strategy) {
      table_df <- data.frame(
        Strategy = labels,
        Value = formatted,
        stringsAsFactors = FALSE
      )
    } else if (varies_by_group) {
      table_df <- data.frame(
        Group = labels,
        Value = formatted,
        stringsAsFactors = FALSE
      )
    }

    text <- ""
    table <- flextable::autofit(flextable::flextable(table_df))
    plot <- NULL
  }

  list(text = text, table = table, plot = plot)
}


# --- Vector (df-based, varies by time) ---

.build_vector_output <- function(segs, labels, name) {
  var_df <- segs[[1]]$var_df

  # Detect which time dimensions matter
  values_by_cycle <- tapply(var_df$value, var_df$state_cycle,
                            function(v) length(unique(v)) > 1)
  varies_by_cycle <- any(values_by_cycle, na.rm = TRUE)

  values_by_sc <- tapply(var_df$value, var_df$cycle,
                         function(v) length(unique(v)) > 1)
  varies_by_state_cycle <- any(values_by_sc, na.rm = TRUE)

  if (varies_by_cycle && varies_by_state_cycle) {
    .build_vector_heatmap_output(segs, labels, name)
  } else if (varies_by_cycle) {
    .build_vector_line_output(segs, labels, name,
                              x_col = "cycle", x_label = "Cycle")
  } else if (varies_by_state_cycle) {
    .build_vector_line_output(segs, labels, name,
                              x_col = "state_cycle", x_label = "State Cycle")
  } else {
    # Constant across time dimensions — treat as scalar
    .build_scalar_output(segs, labels, name)
  }
}

.build_vector_line_output <- function(segs, labels, name, x_col, x_label) {
  varies <- !is.null(labels)

  # Filter to relevant rows and drop the irrelevant time column
  other_col <- if (x_col == "cycle") "state_cycle" else "cycle"

  .filter_var_df <- function(df) {
    df[df[[other_col]] == df[[other_col]][1], c(x_col, "value")]
  }

  if (!varies) {
    var_df <- segs[[1]]$var_df
    filtered <- .filter_var_df(var_df)

    text <- paste(filtered$value, collapse = ", ")

    # Table
    table_data <- filtered
    colnames(table_data) <- c(x_label, name)
    table <- flextable::autofit(flextable::flextable(table_data))

    # Plot
    plot <- ggplot(filtered, aes(x = .data[[x_col]], y = .data$value)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = pretty_breaks(n = 5)) +
      theme_bw() +
      labs(x = x_label, y = name)

  } else {
    combined <- do.call(rbind, lapply(seq_along(segs), function(i) {
      df <- .filter_var_df(segs[[i]]$var_df)
      df$segment <- labels[i]
      df
    }))

    text <- paste(sapply(seq_along(segs), function(i) {
      vals <- .filter_var_df(segs[[i]]$var_df)$value
      paste0(labels[i], ": ", paste(vals, collapse = ", "))
    }), collapse = "\n")

    # Table
    table_data <- combined[, c("segment", x_col, "value")]
    colnames(table_data) <- c("Segment", x_label, name)
    table <- flextable::autofit(flextable::flextable(table_data))

    # Plot
    plot <- ggplot(combined, aes(x = .data[[x_col]], y = .data$value,
                               color = .data$segment,
                               group = .data$segment)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = pretty_breaks(n = 5)) +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(x = x_label, y = name, color = "Segment")
  }

  list(text = text, table = table, plot = plot)
}

.build_vector_heatmap_output <- function(segs, labels, name) {
  varies <- !is.null(labels)

  if (!varies) {
    var_df <- segs[[1]]$var_df

    text <- paste(var_df$value, collapse = ", ")

    table_data <- var_df
    colnames(table_data) <- c("Cycle", "State Cycle", name)
    table <- flextable::autofit(flextable::flextable(table_data))

    plot <- ggplot(var_df, aes(x = .data$cycle, y = .data$state_cycle,
                             fill = .data$value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_viridis_c() +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(x = "Cycle", y = "State Cycle", fill = name)

  } else {
    combined <- do.call(rbind, lapply(seq_along(segs), function(i) {
      df <- segs[[i]]$var_df
      df$segment <- labels[i]
      df
    }))

    text <- paste(sapply(seq_along(segs), function(i) {
      vals <- segs[[i]]$var_df$value
      paste0(labels[i], ": ", paste(vals, collapse = ", "))
    }), collapse = "\n")

    table_data <- combined[, c("segment", "cycle", "state_cycle", "value")]
    colnames(table_data) <- c("Segment", "Cycle", "State Cycle", name)
    table <- flextable::autofit(flextable::flextable(table_data))

    plot <- ggplot(combined, aes(x = .data$cycle, y = .data$state_cycle,
                               fill = .data$value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_viridis_c() +
      facet_wrap(~ segment) +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      labs(x = "Cycle", y = "State Cycle", fill = name)
  }

  list(text = text, table = table, plot = plot)
}


# --- Survival distribution ---

.build_surv_dist_output <- function(segs, labels, name,
                                     varies_by_strategy = FALSE,
                                     varies_by_group = FALSE) {
  varies <- !is.null(labels)

  # Two-pass time generation: coarse scan to find meaningful range,
  # then fine resolution within that range
  .gen_surv_data <- function(x, fine_max_time = NULL) {
    if (is.null(fine_max_time)) {
      # Pass 1: coarse scan over 100 years
      coarse_max <- 365.25 * 100
      coarse_times <- seq(0, coarse_max, length.out = 200)
      coarse_probs <- tryCatch(surv_prob(x, coarse_times),
                               error = function(e) NULL)
      if (is.null(coarse_probs)) return(NULL)

      meaningful <- which(coarse_probs > 1e-6)
      if (length(meaningful) == 0) return(NULL)
      trimmed_max <- coarse_times[min(max(meaningful) + 1, length(coarse_times))]
      fine_max_time <- trimmed_max
    }

    # Pass 2: fine resolution within meaningful range
    times <- seq(0, fine_max_time, length.out = 200)
    probs <- tryCatch(surv_prob(x, times), error = function(e) NULL)
    if (is.null(probs)) return(NULL)
    data.frame(time = times, survival = probs)
  }

  # --- Post-hoc deduplication for surv_dist ---
  # surv_dist objects are never structurally identical even when they represent

  # the same distribution. Compare capture.output text and survival data instead.
  if (varies) {
    # Generate text representations for all segments
    seg_texts <- sapply(segs, function(seg) {
      paste(capture.output(seg$value), collapse = "\n")
    })

    # Find global max time for consistent comparison
    coarse_max <- 365.25 * 100
    coarse_times <- seq(0, coarse_max, length.out = 200)
    global_max_time <- 0
    for (i in seq_along(segs)) {
      coarse_probs <- tryCatch(surv_prob(segs[[i]]$value, coarse_times),
                               error = function(e) NULL)
      if (!is.null(coarse_probs)) {
        meaningful <- which(coarse_probs > 1e-6)
        if (length(meaningful) > 0) {
          seg_max <- coarse_times[min(max(meaningful) + 1, length(coarse_times))]
          global_max_time <- max(global_max_time, seg_max)
        }
      }
    }
    if (global_max_time == 0) global_max_time <- coarse_max

    # Generate survival data for all segments
    seg_surv_data <- lapply(segs, function(seg) {
      .gen_surv_data(seg$value, fine_max_time = global_max_time)
    })

    # Check if all segments are actually identical
    all_identical <- TRUE
    ref_text <- seg_texts[1]
    ref_data <- seg_surv_data[[1]]
    for (i in seq_along(segs)[-1]) {
      text_match <- identical(ref_text, seg_texts[i])
      data_match <- if (!is.null(ref_data) && !is.null(seg_surv_data[[i]])) {
        isTRUE(all.equal(ref_data$survival, seg_surv_data[[i]]$survival,
                         tolerance = .Machine$double.eps^0.5))
      } else {
        is.null(ref_data) && is.null(seg_surv_data[[i]])
      }
      if (!text_match || !data_match) {
        all_identical <- FALSE
        break
      }
    }

    # If all identical, collapse to non-varies path
    if (all_identical) {
      varies <- FALSE
      labels <- NULL
      segs <- segs[1]
    } else {
      # Deduplicate to unique segments only
      keep <- c(TRUE)
      for (i in seq_along(segs)[-1]) {
        is_dup <- FALSE
        for (j in which(keep)) {
          text_match <- identical(seg_texts[j], seg_texts[i])
          data_match <- if (!is.null(seg_surv_data[[j]]) &&
                            !is.null(seg_surv_data[[i]])) {
            isTRUE(all.equal(seg_surv_data[[j]]$survival,
                             seg_surv_data[[i]]$survival,
                             tolerance = .Machine$double.eps^0.5))
          } else {
            is.null(seg_surv_data[[j]]) && is.null(seg_surv_data[[i]])
          }
          if (text_match && data_match) {
            is_dup <- TRUE
            break
          }
        }
        keep <- c(keep, !is_dup)
      }

      if (sum(keep) == 1) {
        varies <- FALSE
        labels <- NULL
        segs <- segs[keep]
      } else {
        segs <- segs[keep]
        labels <- labels[keep]
        seg_surv_data <- seg_surv_data[keep]

        # Re-derive variation flags after dedup
        parsed <- strsplit(labels, ", ", fixed = TRUE)
        remaining_strategies <- sapply(parsed, `[`, 1)
        remaining_groups <- sapply(parsed, function(p) {
          if (length(p) > 1) p[2] else NA
        })
        varies_by_strategy <- length(unique(remaining_strategies)) > 1
        varies_by_group <- length(unique(stats::na.omit(remaining_groups))) > 1

        # Simplify labels to only include varying dimensions
        if (varies_by_strategy && !varies_by_group) {
          labels <- remaining_strategies
        } else if (!varies_by_strategy && varies_by_group) {
          labels <- stats::na.omit(remaining_groups)
        }
        # If both still vary, labels stay as "strategy, group" — no change needed
      }
    }
  }

  if (!varies) {
    x <- segs[[1]]$value
    text <- paste(capture.output(x), collapse = "\n")

    prob_data <- .gen_surv_data(x)
    table <- NULL
    plot <- NULL

    if (!is.null(prob_data)) {
      ft <- flextable::flextable(prob_data)
      ft <- flextable::colformat_double(ft, j = "time", digits = 1)
      ft <- flextable::colformat_double(ft, j = "survival", digits = 4)
      ft <- flextable::autofit(ft)
      table <- ft

      max_time <- max(prob_data$time)
      plot <- tryCatch(plot(x, max_time = max_time),
                       error = function(e) NULL)
    }

  } else {
    text <- paste(sapply(seq_along(segs), function(i) {
      paste0(labels[i], ":\n",
             paste(capture.output(segs[[i]]$value), collapse = "\n"))
    }), collapse = "\n\n")

    # Use pre-computed global_max_time and seg_surv_data if available,
    # otherwise recompute
    if (!exists("global_max_time", inherits = FALSE) ||
        !exists("seg_surv_data", inherits = FALSE)) {
      coarse_max <- 365.25 * 100
      coarse_times <- seq(0, coarse_max, length.out = 200)
      global_max_time <- 0
      for (i in seq_along(segs)) {
        coarse_probs <- tryCatch(surv_prob(segs[[i]]$value, coarse_times),
                                 error = function(e) NULL)
        if (!is.null(coarse_probs)) {
          meaningful <- which(coarse_probs > 1e-6)
          if (length(meaningful) > 0) {
            seg_max <- coarse_times[min(max(meaningful) + 1, length(coarse_times))]
            global_max_time <- max(global_max_time, seg_max)
          }
        }
      }
      if (global_max_time == 0) global_max_time <- coarse_max
      seg_surv_data <- lapply(segs, function(seg) {
        .gen_surv_data(seg$value, fine_max_time = global_max_time)
      })
    }

    # Build wide-format table from survival data
    table <- NULL
    plot <- NULL

    # Collect non-null data
    valid_idx <- which(!sapply(seg_surv_data, is.null))
    if (length(valid_idx) > 0) {
      # All share the same time grid, so use first as base
      wide_df <- data.frame(Time = seg_surv_data[[valid_idx[1]]]$time)
      for (i in valid_idx) {
        wide_df[[labels[i]]] <- seg_surv_data[[i]]$survival
      }

      ft <- flextable::flextable(wide_df)
      ft <- flextable::colformat_double(ft, j = "Time", digits = 1)
      surv_cols <- setdiff(colnames(wide_df), "Time")
      for (sc in surv_cols) {
        ft <- flextable::colformat_double(ft, j = sc, digits = 4)
      }

      # Hierarchical headers when both strategy and group vary
      if (varies_by_strategy && varies_by_group) {
        # Labels are "strategy, group" — parse into components
        parsed <- strsplit(labels[valid_idx], ", ", fixed = TRUE)
        strategies <- sapply(parsed, `[`, 1)
        groups <- sapply(parsed, `[`, 2)

        # Build spanning header: strategy row above group row
        # Current column names are the full labels; rename to group names
        group_names <- groups
        colnames(wide_df) <- c("Time", group_names)
        ft <- flextable::flextable(wide_df)
        ft <- flextable::colformat_double(ft, j = "Time", digits = 1)
        for (gn in group_names) {
          ft <- flextable::colformat_double(ft, j = gn, digits = 4)
        }

        # Build strategy spanning header
        unique_strategies <- unique(strategies)
        header_values <- c("Time")
        header_colwidths <- c(1L)
        for (us in unique_strategies) {
          n_groups <- sum(strategies == us)
          header_values <- c(header_values, us)
          header_colwidths <- c(header_colwidths, as.integer(n_groups))
        }
        ft <- flextable::add_header_row(ft,
          values = header_values,
          colwidths = header_colwidths,
          top = TRUE
        )
        ft <- flextable::merge_v(ft, j = "Time", part = "header")
        ft <- flextable::align(ft, i = 1, align = "center", part = "header")
      }

      ft <- flextable::autofit(ft)
      table <- ft

      # Build combined long-format data for plot
      all_data <- do.call(rbind, lapply(valid_idx, function(i) {
        d <- seg_surv_data[[i]]
        d$segment <- labels[i]
        d
      }))

      max_time <- max(all_data$time)
      plot <- ggplot(all_data, aes(x = .data$time, y = .data$survival,
                                 color = .data$segment)) +
        geom_line(linewidth = 1) +
        coord_cartesian(xlim = c(0, max_time), ylim = c(0, 1)) +
        labs(x = "Time", y = "Survival", color = "Segment") +
        theme_bw() +
        theme(legend.position = "bottom")
    }
  }

  list(text = text, table = table, plot = plot)
}


# --- Decision tree ---

.build_decision_tree_output <- function(segs, labels, name) {
  varies <- !is.null(labels)

  .get_terminal_df <- function(x) {
    data.frame(
      Node = sapply(x$terminal_nodes, `[[`, "node"),
      Probability = sapply(x$terminal_nodes, function(n) n$prob[1]),
      Tags = sapply(x$terminal_nodes, function(n) paste(n$tags, collapse = ", ")),
      stringsAsFactors = FALSE
    )
  }

  if (!varies) {
    x <- segs[[1]]$value
    text <- paste(capture.output(x), collapse = "\n")

    term_df <- .get_terminal_df(x)
    display_df <- term_df
    display_df$Probability <- paste0(oq_format(display_df$Probability * 100), "%")
    table <- flextable::autofit(flextable::flextable(display_df))

    plot <- tryCatch(.plot_eval_decision_tree(x),
                     error = function(e) NULL)

  } else {
    text <- paste(sapply(seq_along(segs), function(i) {
      paste0(labels[i], ":\n",
             paste(capture.output(segs[[i]]$value), collapse = "\n"))
    }), collapse = "\n\n")

    combined_term <- do.call(rbind, lapply(seq_along(segs), function(i) {
      df <- .get_terminal_df(segs[[i]]$value)
      df$Segment <- labels[i]
      df
    }))
    combined_term$Probability <- paste0(
      oq_format(combined_term$Probability * 100), "%"
    )
    table <- flextable::autofit(flextable::flextable(combined_term))

    # One plot per segment, combined if patchwork available
    tree_plots <- lapply(seq_along(segs), function(i) {
      tryCatch(
        .plot_eval_decision_tree(segs[[i]]$value) + labs(title = labels[i]),
        error = function(e) NULL
      )
    })
    tree_plots <- tree_plots[!sapply(tree_plots, is.null)]

    if (length(tree_plots) > 0) {
      if (requireNamespace("patchwork", quietly = TRUE)) {
        plot <- patchwork::wrap_plots(tree_plots, ncol = 1)
      } else {
        plot <- tree_plots[[1]]
      }
    } else {
      plot <- NULL
    }
  }

  list(text = text, table = table, plot = plot)
}


# --- data.frame ---

.build_data_frame_output <- function(segs, labels, name) {
  varies <- !is.null(labels)

  .safe_print_df <- function(x) {
    tryCatch(
      paste(capture.output(print(x, n = 20)), collapse = "\n"),
      error = function(e) paste(capture.output(x), collapse = "\n")
    )
  }

  if (!varies) {
    x <- segs[[1]]$value
    text <- .safe_print_df(x)
    table <- flextable::autofit(flextable::flextable(x))
    plot <- NULL

  } else {
    text <- paste(sapply(seq_along(segs), function(i) {
      paste0(labels[i], ":\n", .safe_print_df(segs[[i]]$value))
    }), collapse = "\n\n")

    combined_df <- do.call(rbind, lapply(seq_along(segs), function(i) {
      df <- segs[[i]]$value
      df$Segment <- labels[i]
      df
    }))
    table <- flextable::autofit(flextable::flextable(combined_df))
    plot <- NULL
  }

  list(text = text, table = table, plot = plot)
}


# --- med_regimen ---

.build_med_regimen_output <- function(segs, labels, name) {
  varies <- !is.null(labels)

  .regimen_text <- function(x) {
    if (exists("summarise_regimen", mode = "function")) {
      tryCatch(
        paste(capture.output(summarise_regimen(x)), collapse = "\n"),
        error = function(e) paste(capture.output(x), collapse = "\n")
      )
    } else {
      paste(capture.output(x), collapse = "\n")
    }
  }

  .regimen_table <- function(x) {
    if (exists("detail_schedule", mode = "function") &&
        exists("as_flextable", mode = "function")) {
      ft <- tryCatch(as_flextable(detail_schedule(x)),
                     error = function(e) NULL)
      if (!is.null(ft)) return(ft)
    }
    if (!is.null(x$route) && x$route %in% c("iv", "sc", "im") &&
        exists("diagnose_dosing", mode = "function") &&
        exists("as_flextable", mode = "function")) {
      ft <- tryCatch(as_flextable(diagnose_dosing(x)),
                     error = function(e) NULL)
      if (!is.null(ft)) return(ft)
    }
    NULL
  }

  if (!varies) {
    x <- segs[[1]]$value
    text <- .regimen_text(x)
    table <- .regimen_table(x)
    if (is.null(table)) {
      return(.build_object_output(segs, labels, name))
    }
    plot <- NULL

  } else {
    text <- paste(sapply(seq_along(segs), function(i) {
      paste0(labels[i], ":\n", .regimen_text(segs[[i]]$value))
    }), collapse = "\n\n")

    seg_fts <- lapply(seq_along(segs), function(i) {
      .regimen_table(segs[[i]]$value)
    })
    seg_fts <- seg_fts[!sapply(seg_fts, is.null)]
    if (length(seg_fts) == 0) {
      return(.build_object_output(segs, labels, name))
    }
    table <- seg_fts[[1]]
    plot <- NULL
  }

  list(text = text, table = table, plot = plot)
}


# --- med_regimen_combo ---

.build_med_regimen_combo_output <- function(segs, labels, name) {
  varies <- !is.null(labels)

  .combo_text <- function(x) {
    parts <- paste(capture.output(x), collapse = "\n")
    if (exists("summarise_regimen", mode = "function") &&
        !is.null(x$regimens)) {
      for (reg in x$regimens) {
        parts <- paste0(parts, "\n\n",
          tryCatch(
            paste(capture.output(summarise_regimen(reg)), collapse = "\n"),
            error = function(e) ""
          )
        )
      }
    }
    parts
  }

  .combo_table <- function(x) {
    if (is.null(x$regimens) ||
        !exists("detail_schedule", mode = "function") ||
        !exists("as_flextable", mode = "function")) {
      return(NULL)
    }
    for (reg in x$regimens) {
      ft <- tryCatch(as_flextable(detail_schedule(reg)),
                     error = function(e) NULL)
      if (!is.null(ft)) return(ft)

      if (!is.null(reg$route) && reg$route %in% c("iv", "sc", "im") &&
          exists("diagnose_dosing", mode = "function")) {
        ft <- tryCatch(as_flextable(diagnose_dosing(reg)),
                       error = function(e) NULL)
        if (!is.null(ft)) return(ft)
      }
    }
    NULL
  }

  if (!varies) {
    x <- segs[[1]]$value
    text <- .combo_text(x)
    table <- .combo_table(x)
    if (is.null(table)) {
      return(.build_object_output(segs, labels, name))
    }
    plot <- NULL

  } else {
    text <- paste(sapply(seq_along(segs), function(i) {
      paste0(labels[i], ":\n", .combo_text(segs[[i]]$value))
    }), collapse = "\n\n")

    seg_fts <- lapply(seq_along(segs), function(i) {
      .combo_table(segs[[i]]$value)
    })
    seg_fts <- seg_fts[!sapply(seg_fts, is.null)]
    if (length(seg_fts) == 0) {
      return(.build_object_output(segs, labels, name))
    }
    table <- seg_fts[[1]]
    plot <- NULL
  }

  list(text = text, table = table, plot = plot)
}


# --- Generic object ---

.build_object_output <- function(segs, labels, name) {
  varies <- !is.null(labels)

  if (!varies) {
    x <- segs[[1]]$value
    print_text <- paste(capture.output(x), collapse = "\n")
    text <- print_text

    display_text <- print_text
    if (nchar(display_text) > 2000) {
      display_text <- paste0(substr(display_text, 1, 2000), "\n... (truncated)")
    }
    df <- data.frame(
      Field = c("Class", "Output"),
      Value = c(paste(class(x), collapse = ", "), display_text),
      stringsAsFactors = FALSE
    )
    ft <- flextable::flextable(df)
    ft <- flextable::font(ft, j = "Value", fontname = "Courier New")
    ft <- flextable::autofit(ft)
    table <- ft
    plot <- NULL

  } else {
    text <- paste(sapply(seq_along(segs), function(i) {
      paste0(labels[i], ":\n",
             paste(capture.output(segs[[i]]$value), collapse = "\n"))
    }), collapse = "\n\n")

    rows <- lapply(seq_along(segs), function(i) {
      x <- segs[[i]]$value
      pt <- paste(capture.output(x), collapse = "\n")
      if (nchar(pt) > 2000) {
        pt <- paste0(substr(pt, 1, 2000), "\n... (truncated)")
      }
      data.frame(
        Segment = labels[i],
        Class = paste(class(x), collapse = ", "),
        Output = pt,
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
    ft <- flextable::flextable(df)
    ft <- flextable::font(ft, j = "Output", fontname = "Courier New")
    ft <- flextable::autofit(ft)
    table <- ft
    plot <- NULL
  }

  list(text = text, table = table, plot = plot)
}
