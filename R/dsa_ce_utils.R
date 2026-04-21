#' Internal CE DSA Utilities
#'
#' Shared helpers for DSA cost-effectiveness plots and tables.
NULL

#' Classify Base Case ICER
#'
#' @param icer_value Numeric ICER value
#' @return Character classification
#' @keywords internal
classify_base_case <- function(icer_value) {
  if (length(icer_value) == 0) {
    return("reference")
  }
  if (is.nan(icer_value)) {
    return("identical")
  }
  if (is.na(icer_value)) {
    return("reference")
  }
  if (is.infinite(icer_value) && icer_value > 0) {
    return("dominated")
  }
  if (icer_value == 0) {
    return("dominant")
  }
  if (icer_value > 0) {
    return("normal")
  }
  if (icer_value < 0) {
    return("flipped")
  }
  "reference"
}

#' Detect Variation Error State
#'
#' @param base_class Character base case classification
#' @param variation_icer Numeric ICER for the variation
#' @return List with type, show_bar, bar_type, label components
#' @keywords internal
detect_variation_error <- function(base_class, variation_icer) {
  if (base_class == "identical") {
    return(list(type = "full_chart_error", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
  }

  if (is.nan(variation_icer)) {
    return(list(type = "identical", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
  }

  if (base_class == "flipped") {
    if (variation_icer == 0) {
      return(list(type = NULL, show_bar = TRUE, bar_type = "arrow", label = "Dominated"))
    }
    if (is.infinite(variation_icer) && variation_icer > 0) {
      return(list(type = NULL, show_bar = TRUE, bar_type = "to_zero", label = "Dominant"))
    }
    if (is.finite(variation_icer) && variation_icer > 0) {
      return(list(type = "direction_change", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
    }
    return(list(type = NULL, show_bar = TRUE, bar_type = "standard", label = NA_character_))
  }

  if (is.finite(variation_icer) && variation_icer < 0) {
    return(list(type = "direction_change", show_bar = FALSE, bar_type = NA_character_, label = NA_character_))
  }

  if (is.infinite(variation_icer) && variation_icer > 0) {
    if (base_class == "dominated") {
      return(list(type = NULL, show_bar = FALSE, bar_type = NA_character_, label = "Dominated"))
    }
    return(list(type = NULL, show_bar = TRUE, bar_type = "arrow", label = "Dominated"))
  }

  if (variation_icer == 0) {
    if (base_class == "dominant") {
      return(list(type = NULL, show_bar = FALSE, bar_type = NA_character_, label = "Dominant"))
    }
    return(list(type = NULL, show_bar = TRUE, bar_type = "to_zero", label = "Dominant"))
  }

  list(type = NULL, show_bar = TRUE, bar_type = "standard", label = NA_character_)
}

#' Generate Error Message for DSA CE Variation
#'
#' @param error_type Character error type
#' @param variation Character "Low" or "High"
#' @param icer_value Numeric ICER value
#' @param intervention_name Character intervention display name
#' @param comparator_name Character comparator display name
#' @param locale Optional locale
#' @return Character error message
#' @keywords internal
generate_ce_error_message <- function(error_type, variation, icer_value,
                                      intervention_name, comparator_name,
                                      locale = NULL) {
  if (is.null(error_type) || is.na(error_type)) {
    return(NA_character_)
  }

  if (error_type == "direction_change") {
    if (icer_value < 0) {
      sprintf(
        "%s value of parameter changes the directionality of ICER and cannot be displayed. ICER of %s reflects comparison of %s vs. %s",
        variation,
        oq_format(abs(icer_value), locale = locale, currency = TRUE),
        comparator_name,
        intervention_name
      )
    } else {
      sprintf(
        "%s value of parameter changes the directionality of ICER and cannot be displayed. ICER of %s reflects comparison of %s vs. %s",
        variation,
        oq_format(abs(icer_value), locale = locale, currency = TRUE),
        intervention_name,
        comparator_name
      )
    }
  } else if (error_type == "identical") {
    sprintf(
      "%s value of parameter results in identical outcomes and costs for %s and %s, resulting in undefined ICER",
      variation,
      intervention_name,
      comparator_name
    )
  } else {
    NA_character_
  }
}

#' Generate Footnote for Flipped Base Case
#'
#' @param asterisk Character asterisk string
#' @param comparator_name Character comparator display name
#' @param intervention_name Character intervention display name
#' @return Character footnote text
#' @keywords internal
generate_flipped_base_footnote <- function(asterisk, comparator_name, intervention_name) {
  sprintf(
    "%s %s is more costly and more effective than %s in base case, plot depicts comparison of %s vs. %s.",
    asterisk,
    comparator_name,
    intervention_name,
    comparator_name,
    intervention_name
  )
}

#' Generate Footnote for Flipped Bar
#'
#' @param asterisk Character asterisk string
#' @param comparator_name Character comparator display name
#' @param intervention_name Character intervention display name
#' @return Character footnote text
#' @keywords internal
generate_flipped_bar_footnote <- function(asterisk, comparator_name, intervention_name) {
  sprintf(
    "%s ICER reflects comparison of %s vs. %s.",
    asterisk,
    comparator_name,
    intervention_name
  )
}

#' Generate Footnote for Direction Change
#'
#' @param asterisk Character asterisk string
#' @param more_costly_name Character display name of more costly/effective strategy
#' @param less_costly_name Character display name of other strategy
#' @return Character footnote text
#' @keywords internal
generate_direction_change_footnote <- function(asterisk, more_costly_name, less_costly_name) {
  sprintf(
    "%s ICER reflects comparison of %s vs. %s and cannot be displayed on same axis as base case reflecting comparison of %s vs. %s.",
    asterisk,
    more_costly_name,
    less_costly_name,
    less_costly_name,
    more_costly_name
  )
}

#' Generate Footnote for Identical Outcomes
#'
#' @param asterisk Character asterisk string
#' @param intervention_name Character intervention display name
#' @param comparator_name Character comparator display name
#' @return Character footnote text
#' @keywords internal
generate_identical_footnote <- function(asterisk, intervention_name, comparator_name) {
  sprintf(
    "%s ICER is undefined and cannot be displayed due to %s and %s having identical outcomes and costs.",
    asterisk,
    intervention_name,
    comparator_name
  )
}

#' Format ICER Value for Display
#'
#' @param icer_value Numeric ICER value
#' @param asterisk Character asterisk string to append
#' @param decimals Number of decimal places
#' @param locale Optional locale
#' @param abbreviate_val Logical. Use abbreviated values?
#' @return Formatted string
#' @keywords internal
format_icer_label <- function(icer_value, asterisk = "", decimals = 0,
                              locale = NULL, abbreviate_val = FALSE) {
  if (is.nan(icer_value)) {
    return(paste0("Equivalent", asterisk))
  }
  if (is.na(icer_value)) {
    return("")
  }
  if (is.infinite(icer_value) && icer_value > 0) {
    return(paste0("Dominated", asterisk))
  }
  if (icer_value == 0) {
    return(paste0("Dominant", asterisk))
  }

  formatted <- oq_format_icer(icer_value, decimals = decimals, locale = locale,
                              abbreviate = abbreviate_val)
  paste0(formatted, asterisk)
}

#' Check if ICER is Flipped
#'
#' Canonical scalar helper for DSA CE outputs. Returns TRUE only for
#' finite negative scalar ICERs.
#'
#' @param icer_value Numeric ICER value
#' @return Logical TRUE if scalar, finite, and negative
#' @keywords internal
is_flipped_icer <- function(icer_value) {
  if (length(icer_value) != 1) {
    return(FALSE)
  }
  !is.na(icer_value) && is.finite(icer_value) && icer_value < 0
}

#' Calculate Displayable Range for Parameter Ordering
#'
#' @param low_icer Numeric low ICER value
#' @param base_icer Numeric base ICER value
#' @param high_icer Numeric high ICER value
#' @param low_show_bar Logical whether low bar is shown
#' @param high_show_bar Logical whether high bar is shown
#' @param base_class Character base case classification
#' @return Numeric displayable range
#' @keywords internal
calculate_displayable_range <- function(low_icer, base_icer, high_icer,
                                        low_show_bar, high_show_bar,
                                        base_class) {
  if (base_class == "identical") {
    return(NA_real_)
  }

  displayable_values <- numeric(0)

  if (base_class == "dominated") {
    displayable_values <- c(displayable_values, 1e12)
  } else if (base_class == "dominant") {
    displayable_values <- c(displayable_values, 0)
  } else if (base_class %in% c("normal", "flipped")) {
    displayable_values <- c(displayable_values, abs(base_icer))
  }

  if (low_show_bar && !is.na(low_icer) && !is.nan(low_icer)) {
    if (is.infinite(low_icer)) {
      displayable_values <- c(displayable_values, 1e12)
    } else if (low_icer == 0) {
      displayable_values <- c(displayable_values, 0)
    } else {
      displayable_values <- c(displayable_values, abs(low_icer))
    }
  }

  if (high_show_bar && !is.na(high_icer) && !is.nan(high_icer)) {
    if (is.infinite(high_icer)) {
      displayable_values <- c(displayable_values, 1e12)
    } else if (high_icer == 0) {
      displayable_values <- c(displayable_values, 0)
    } else {
      displayable_values <- c(displayable_values, abs(high_icer))
    }
  }

  if (length(displayable_values) == 0) {
    return(0)
  }

  range_val <- max(displayable_values) - min(displayable_values)
  if (range_val == 0) return(1)
  range_val
}

#' Assign Asterisks Per Facet for DSA CE Plot
#'
#' @param tornado_long Long-format tornado data with y_base calculated
#' @param facet_metadata Facet metadata with base_footnote_type
#' @param y_spacing Y spacing used for the plot
#' @return List with updated tornado_long, facet_metadata, and per-facet footnotes
#' @keywords internal
assign_facet_asterisks <- function(tornado_long, facet_metadata, y_spacing) {
  asterisk_symbols <- c("*", "**", "***")
  facet_metadata$base_asterisk <- ""

  facet_keys <- facet_metadata %>%
    distinct(.data$strategy, .data$group)

  all_facet_footnotes <- list()

  for (i in seq_len(nrow(facet_keys))) {
    strat <- facet_keys$strategy[i]
    grp <- facet_keys$group[i]

    facet_meta <- facet_metadata %>%
      filter(.data$strategy == strat, .data$group == grp)

    facet_tornado <- tornado_long %>%
      filter(.data$strategy == strat, .data$group == grp)

    footnote_types_seen <- character(0)
    footnote_map <- list()

    get_asterisk <- function(footnote_type) {
      if (is.na(footnote_type)) return("")
      if (footnote_type %in% footnote_types_seen) {
        return(footnote_map[[footnote_type]])
      }
      idx <- length(footnote_types_seen) + 1
      if (idx > length(asterisk_symbols)) {
        asterisk <- paste(rep("*", idx), collapse = "")
      } else {
        asterisk <- asterisk_symbols[idx]
      }
      footnote_types_seen <<- c(footnote_types_seen, footnote_type)
      footnote_map[[footnote_type]] <<- asterisk
      asterisk
    }

    base_footnote_type <- facet_meta$base_footnote_type[1]
    base_ast <- get_asterisk(base_footnote_type)

    facet_metadata$base_asterisk[
      facet_metadata$strategy == strat & facet_metadata$group == grp
    ] <- base_ast

    if (nrow(facet_tornado) > 0) {
      facet_tornado_sorted <- facet_tornado %>%
        arrange(.data$y_base, .data$variation)

      for (j in seq_len(nrow(facet_tornado_sorted))) {
        row_idx <- which(
          tornado_long$strategy == strat &
          tornado_long$group == grp &
          tornado_long$parameter == facet_tornado_sorted$parameter[j] &
          tornado_long$variation == facet_tornado_sorted$variation[j]
        )

        if (length(row_idx) > 0) {
          footnote_type <- tornado_long$footnote_type[row_idx[1]]
          asterisk <- get_asterisk(footnote_type)
          tornado_long$asterisk[row_idx] <- asterisk
        }
      }
    }

    facet_footnotes <- character(0)
    intervention_name <- facet_meta$intervention_name[1]
    comparator_name <- facet_meta$comparator_name[1]
    is_flipped <- facet_meta$is_flipped_facet[1]

    for (ft in footnote_types_seen) {
      ast <- footnote_map[[ft]]
      if (ft == "flipped_base") {
        facet_footnotes <- c(facet_footnotes,
                             generate_flipped_base_footnote(ast, comparator_name, intervention_name))
      } else if (ft == "flipped_bar") {
        facet_footnotes <- c(facet_footnotes,
                             generate_flipped_bar_footnote(ast, comparator_name, intervention_name))
      } else if (ft == "direction_change") {
        if (is_flipped) {
          facet_footnotes <- c(facet_footnotes,
                               generate_direction_change_footnote(ast, intervention_name, comparator_name))
        } else {
          facet_footnotes <- c(facet_footnotes,
                               generate_direction_change_footnote(ast, comparator_name, intervention_name))
        }
      } else if (ft == "identical") {
        facet_footnotes <- c(facet_footnotes,
                             generate_identical_footnote(ast, intervention_name, comparator_name))
      }
    }

    all_facet_footnotes[[paste(strat, grp, sep = "|||")]] <- facet_footnotes
  }

  if (!"base_asterisk" %in% names(facet_metadata)) {
    facet_metadata$base_asterisk <- ""
  }

  facet_metadata <- facet_metadata %>%
    mutate(base_label = paste0(.data$base_label_raw, .data$base_asterisk))

  list(
    tornado_long = tornado_long,
    facet_metadata = facet_metadata,
    facet_footnotes = all_facet_footnotes
  )
}
