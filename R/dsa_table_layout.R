#' Build DSA Triplet Table Spec
#'
#' Internal helper to create a low/base/high table spec for DSA outputs.
#'
#' @param data Data frame with parameter_display_name, strategy, group, low, base, high
#' @param groups Requested group selection
#' @param metadata Results metadata for ordering
#' @param font_size Font size for rendering
#' @param footnotes Optional character vector of footnotes
#' @return Table spec for render_table()
#' @keywords internal
build_dsa_triplet_table_spec <- function(data,
                                         groups = "overall",
                                         metadata = NULL,
                                         font_size = 11,
                                         footnotes = character()) {
  strategies_display <- unique(data$strategy)
  groups_display <- unique(data$group)
  groups_display <- get_group_order(groups_display, metadata)

  n_strategies <- length(strategies_display)
  n_groups <- length(groups_display)
  mode <- if (n_groups > 1 || is.null(groups)) "multi_group" else "single_group"

  build_headers <- function(include_group_borders = FALSE) {
    headers <- list()

    row1 <- list()
    row1[[1]] <- list(span = 1, text = "", borders = c(1, 0, if (include_group_borders) 0 else 1, 0))
    for (strat in strategies_display) {
      row1[[length(row1) + 1]] <- list(span = 3, text = strat, borders = c(1, 0, 1, 0))
    }
    headers[[1]] <- row1

    row2 <- list()
    row2[[1]] <- list(span = 1, text = " ", borders = c(0, 0, 1, 0))
    for (strat in strategies_display) {
      row2[[length(row2) + 1]] <- list(span = 1, text = "Low", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "Base", borders = c(0, 0, 1, 0))
      row2[[length(row2) + 1]] <- list(span = 1, text = "High", borders = c(0, 0, 1, 0))
    }
    headers[[2]] <- row2

    headers
  }

  reorder_columns <- function(result_data) {
    col_order <- c("parameter_display_name")
    for (strat in strategies_display) {
      col_order <- c(col_order, paste0(strat, "_low"), paste0(strat, "_base"), paste0(strat, "_high"))
    }
    result_data[, col_order]
  }

  if (mode == "single_group") {
    result_data <- data %>%
      arrange(.data$parameter_display_name) %>%
      select("parameter_display_name", "strategy", "low", "base", "high") %>%
      pivot_wider(
        names_from = "strategy",
        values_from = c("low", "base", "high"),
        names_glue = "{strategy}_{.value}"
      )

    result_data <- reorder_columns(result_data)
    names(result_data)[1] <- " "

    special_rows <- list()
    headers <- build_headers(include_group_borders = FALSE)
  } else {
    result_data <- tibble()
    group_header_rows <- integer()
    indented_rows <- integer()
    current_row <- 0

    for (grp in groups_display) {
      current_row <- current_row + 1
      group_header_rows <- c(group_header_rows, current_row)

      group_row <- tibble(parameter_display_name = grp)
      for (strat in strategies_display) {
        group_row[[paste0(strat, "_low")]] <- ""
        group_row[[paste0(strat, "_base")]] <- ""
        group_row[[paste0(strat, "_high")]] <- ""
      }
      result_data <- bind_rows(result_data, group_row)

      grp_data <- data %>%
        filter(.data$group == grp) %>%
        arrange(.data$parameter_display_name) %>%
        select("parameter_display_name", "strategy", "low", "base", "high") %>%
        pivot_wider(
          names_from = "strategy",
          values_from = c("low", "base", "high"),
          names_glue = "{strategy}_{.value}"
        )

      grp_data <- reorder_columns(grp_data)
      n_param_rows <- nrow(grp_data)
      indented_rows <- c(indented_rows, seq(current_row + 1, current_row + n_param_rows))
      result_data <- bind_rows(result_data, grp_data)
      current_row <- current_row + n_param_rows
    }

    names(result_data)[1] <- " "
    headers <- build_headers(include_group_borders = TRUE)
    group_boundary_rows <- if (length(group_header_rows) > 1) group_header_rows[-1] else integer()
    special_rows <- list(
      group_header_rows = group_header_rows,
      group_boundary_rows = group_boundary_rows,
      indented_rows = indented_rows
    )
  }

  spec <- create_simple_table_spec(
    headers = headers,
    data = result_data,
    column_alignments = c("left", rep(c("right", "right", "right"), n_strategies)),
    column_widths = rep(NA, ncol(result_data)),
    special_rows = special_rows,
    font_size = font_size,
    font_family = "Helvetica"
  )

  spec$footnotes <- footnotes

  spec
}
