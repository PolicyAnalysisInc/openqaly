#' Plot a Decision Tree
#'
#' Creates a health economics-standard visualization of a decision tree,
#' with horizontal left-to-right layout and standard node shapes:
#' circle for chance nodes and triangle for terminal nodes.
#'
#' @param model An \code{oq_model} or \code{oq_model_builder} object
#'   containing a \code{trees} component.
#' @param tree_name Character string specifying which tree to plot. If
#'   \code{NULL}, uses the tree referenced by \code{model$decision_tree$tree_name},
#'   or the first tree found.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2/ggraph object displaying the tree structure.
#'
#' @export
plot_decision_tree <- function(model, tree_name = NULL, ...) {

  # Validate model input

  if (!inherits(model, c("oq_model", "oq_model_builder"))) {
    stop("'model' must be an oq_model or oq_model_builder object.")
  }

  if (is.null(model$trees) || !is.data.frame(model$trees) || nrow(model$trees) == 0) {
    stop("Model does not contain any decision trees.")
  }

  # Resolve tree name
  if (is.null(tree_name)) {
    if (!is.null(model$decision_tree$tree_name)) {
      tree_name <- model$decision_tree$tree_name
    } else {
      tree_name <- model$trees$name[1]
    }
  }

  available_trees <- unique(model$trees$name)
  if (!tree_name %in% available_trees) {
    stop(
      "Tree '", tree_name, "' not found. Available trees: ",
      paste0("'", available_trees, "'", collapse = ", ")
    )
  }

  # Filter to selected tree
  tree_df <- model$trees[model$trees$name == tree_name, ]

  # Classify node types: non-leaf = chance, leaf = terminal
  # The decision between strategies is at the strategy level, not within the tree.
  all_nodes <- tree_df$node
  parent_nodes <- unique(tree_df$parent[!is.na(tree_df$parent)])
  leaf_nodes <- setdiff(all_nodes, parent_nodes)

  node_type <- ifelse(all_nodes %in% leaf_nodes, "terminal", "chance")

  nodes_df <- data.frame(
    node = all_nodes,
    node_type = node_type,
    stringsAsFactors = FALSE
  )

  # Build edges (only for non-root nodes)
  non_root <- tree_df[!is.na(tree_df$parent), ]
  edges_df <- data.frame(
    from = match(non_root$parent, all_nodes),
    to = match(non_root$node, all_nodes),
    formula = non_root$formula,
    stringsAsFactors = FALSE
  )

  the_graph <- tbl_graph(nodes = nodes_df, edges = edges_df)

  # Shape mapping: 21=filled circle, 24=filled triangle
  shape_map <- c(chance = 21, terminal = 24)

  ggraph(the_graph, layout = "tree") +
    coord_flip() +
    scale_y_reverse(expand = expansion(mult = 0.15)) +
    geom_edge_link(
      aes(label = .data$formula),
      angle_calc = "along",
      label_dodge = unit(2.5, "mm"),
      label_size = 2.5,
      end_cap = circle(3, "mm"),
      start_cap = circle(3, "mm"),
      colour = "grey40"
    ) +
    geom_node_point(
      aes(shape = .data$node_type, fill = .data$node_type),
      size = 5, colour = "black"
    ) +
    scale_shape_manual(values = shape_map) +
    scale_fill_discrete() +
    geom_node_text(aes(label = .data$node), nudge_y = 0.084, size = 3, hjust = 0) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom"
    ) +
    scale_x_continuous(expand = expansion(mult = 0.15)) +
    labs(shape = "Node Type", fill = "Node Type")
}


# =============================================================================
# Decision Tree Sankey Plot
# =============================================================================

#' Plot Decision Tree as Sankey Diagram
#'
#' Visualizes evaluated decision trees from model run results as a Sankey
#' diagram, showing computed probabilities flowing from root to terminal nodes.
#' Each flow's width is proportional to probability. Flows are colored by the
#' first branch (root's direct children). When multiple strategies or groups
#' are present, the plot is automatically faceted.
#'
#' @param results Output from \code{run_model()}.
#' @param tree_name Character string specifying which tree to plot. If
#'   \code{NULL}, auto-detects from evaluated variables.
#' @param strategies Character vector of strategy names to include (NULL for all).
#' @param groups Character vector of group names to include (NULL for all).
#' @param show_labels Logical. If TRUE (default), show node names and
#'   probability labels.
#' @param alpha Numeric. Transparency of flow ribbons (0 to 1). Default 0.6.
#' @param use_display_names Logical. If TRUE (default), use display names for
#'   strategies and groups in facet labels.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2 object.
#'
#' @export
plot_decision_tree_sankey <- function(
  results,
  tree_name = NULL,
  strategies = NULL,
  groups = NULL,
  show_labels = TRUE,
  alpha = 0.6,
  use_display_names = TRUE,
  ...
) {

  # Validate input
  if (is.null(results$segments)) {
    stop("'results' must be output from run_model() containing segments.",
         call. = FALSE)
  }

  segments <- results$segments

  if (!"eval_vars" %in% colnames(segments)) {
    stop("plot_decision_tree_sankey() requires base-case results from run_model(). ",
         "PSA or DSA results are not supported.", call. = FALSE)
  }

  # Filter by strategy
  if (!is.null(strategies)) {
    segments <- segments[segments$strategy %in% strategies, ]
  }

  # Filter by group
  if (!is.null(groups)) {
    segments <- segments[segments$group %in% groups, ]
  }

  if (nrow(segments) == 0) {
    stop("No segments remaining after filtering.", call. = FALSE)
  }

  name_field <- field_from_display_names(use_display_names)
  node_width <- 0.04

  all_nodes <- list()
  all_flows <- list()
  resolved_tree_name <- tree_name

  for (i in seq_len(nrow(segments))) {
    seg <- segments[i, ]

    # Extract eval_decision_tree
    et <- extract_eval_tree_sankey(seg, resolved_tree_name)
    eval_tree <- et$tree
    resolved_tree_name <- et$name

    # Get tree df for this specific tree
    tree_df <- eval_tree$df
    tree_df <- tree_df[tree_df$name == resolved_tree_name, ]
    tree_df$parent <- ifelse(is.na(tree_df$parent), "", tree_df$parent)

    cond_prob <- eval_tree$cond_prob

    # Build layout
    layout <- build_sankey_layout(tree_df, cond_prob)
    layout$strategy <- seg$strategy
    layout$group <- seg$group

    # Build flows
    flows <- build_sankey_flows(layout, node_width)
    if (nrow(flows) > 0) {
      flows$strategy <- seg$strategy
      flows$group <- seg$group
    }

    all_nodes[[i]] <- layout
    all_flows[[i]] <- flows
  }

  nodes_df <- do.call(rbind, all_nodes)
  flows_df <- do.call(rbind, all_flows)

  # Map display names
  if (!is.null(results$metadata)) {
    if (!is.null(results$metadata$strategies) && name_field != "name") {
      nodes_df$strategy <- map_names(
        nodes_df$strategy, results$metadata$strategies, name_field
      )
      if (nrow(flows_df) > 0) {
        flows_df$strategy <- map_names(
          flows_df$strategy, results$metadata$strategies, name_field
        )
      }
    }
    if (!is.null(results$metadata$groups) && name_field != "name") {
      nodes_df$group <- map_names(
        nodes_df$group, results$metadata$groups, name_field
      )
      if (nrow(flows_df) > 0) {
        flows_df$group <- map_names(
          flows_df$group, results$metadata$groups, name_field
        )
      }
    }
  }

  # Make flow_id unique per panel
  if (nrow(flows_df) > 0) {
    flows_df$flow_id <- paste0(
      flows_df$strategy, ":", flows_df$group, ":", flows_df$flow_id
    )
  }

  # Pre-compute rect coordinates
  nodes_df$rect_xmin <- nodes_df$depth - node_width
  nodes_df$rect_xmax <- nodes_df$depth + node_width

  # Determine faceting
  n_strategies <- length(unique(nodes_df$strategy))
  n_groups <- length(unique(nodes_df$group))

  if (n_strategies > 1 && n_groups > 1) {
    facet_component <- facet_grid(
      rows = vars(.data$group), cols = vars(.data$strategy)
    )
  } else if (n_strategies > 1) {
    facet_component <- facet_wrap(vars(.data$strategy))
  } else if (n_groups > 1) {
    facet_component <- facet_wrap(vars(.data$group))
  } else {
    facet_component <- NULL
  }

  # Build plot
  p <- ggplot()

  # Flow ribbons
  if (nrow(flows_df) > 0) {
    p <- p + geom_polygon(
      data = flows_df,
      aes(
        x = .data$x, y = .data$y,
        group = .data$flow_id,
        fill = .data$first_branch
      ),
      alpha = alpha
    )
  }

  # Node blocks
  p <- p + geom_rect(
    data = nodes_df,
    aes(
      xmin = .data$rect_xmin, xmax = .data$rect_xmax,
      ymin = .data$y_min, ymax = .data$y_max
    ),
    fill = "grey30",
    color = "white",
    linewidth = 0.3
  )

  # Labels
  if (show_labels) {
    # Terminal node labels (right side, with joint probability)
    terminal_df <- nodes_df[nodes_df$is_terminal, ]
    if (nrow(terminal_df) > 0) {
      terminal_df$label_x <- terminal_df$depth + node_width + 0.06
      terminal_df$label_y <- (terminal_df$y_min + terminal_df$y_max) / 2
      terminal_df$label_text <- paste0(
        terminal_df$node,
        " (", sprintf("%.1f%%", terminal_df$joint_prob * 100), ")"
      )
      p <- p + geom_text(
        data = terminal_df,
        aes(
          x = .data$label_x, y = .data$label_y,
          label = .data$label_text
        ),
        size = 3, hjust = 0, vjust = 0.5
      )
    }

    # Non-terminal node labels (left side)
    nonterminal_df <- nodes_df[!nodes_df$is_terminal, ]
    if (nrow(nonterminal_df) > 0) {
      nonterminal_df$label_x <- nonterminal_df$depth - node_width - 0.06
      nonterminal_df$label_y <- (nonterminal_df$y_min + nonterminal_df$y_max) / 2
      p <- p + geom_text(
        data = nonterminal_df,
        aes(
          x = .data$label_x, y = .data$label_y,
          label = .data$node
        ),
        size = 3, hjust = 1, vjust = 0.5
      )
    }

    # Conditional probability labels on flows
    non_root_df <- nodes_df[nodes_df$parent != "", ]
    if (nrow(non_root_df) > 0) {
      non_root_df$flow_label_x <- non_root_df$depth - 0.5
      non_root_df$flow_label_y <- (non_root_df$y_min + non_root_df$y_max) / 2
      non_root_df$flow_label <- sprintf("%.0f%%", non_root_df$cond_prob_val * 100)
      p <- p + geom_text(
        data = non_root_df,
        aes(
          x = .data$flow_label_x, y = .data$flow_label_y,
          label = .data$flow_label
        ),
        size = 2.5, color = "grey40", fontface = "italic"
      )
    }
  }

  # Color scale
  if (nrow(flows_df) > 0) {
    n_branches <- length(unique(flows_df$first_branch))
    if (n_branches <= 8) {
      p <- p + scale_fill_brewer(palette = "Set2", guide = "none")
    } else {
      p <- p + scale_fill_hue(guide = "none")
    }
  }

  # Faceting
  if (!is.null(facet_component)) {
    p <- p + facet_component
  }

  # Theme
  p <- p +
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    scale_y_continuous(expand = expansion(mult = 0.02)) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank()
    )

  p
}


# =============================================================================
# Internal helpers for Sankey plot
# =============================================================================

#' Extract eval_decision_tree from a segment row
#'
#' Scans the segment's eval_vars environment for objects of class
#' \code{eval_decision_tree} and returns the requested one.
#'
#' @param segment_row A single-row data frame from results$segments
#' @param tree_name Name of the tree to extract (NULL for auto-detect)
#' @return A list with \code{tree} (the eval_decision_tree) and \code{name}
#' @keywords internal
extract_eval_tree_sankey <- function(segment_row, tree_name = NULL) {
  ev <- segment_row$eval_vars[[1]]
  if (is.null(ev)) {
    stop("eval_vars not found in results. ",
         "plot_decision_tree_sankey() requires base-case results from run_model(). ",
         "PSA or DSA results are not supported.", call. = FALSE)
  }

  # Scan environment for eval_decision_tree objects
  env_names <- ls(ev$env)
  tree_objects <- list()
  for (nm in env_names) {
    obj <- get(nm, envir = ev$env)
    if (inherits(obj, "eval_decision_tree")) {
      tree_objects[[nm]] <- obj
    }
  }

  if (length(tree_objects) == 0) {
    stop("No evaluated decision trees found in results. ",
         "Ensure the model contains decision trees.", call. = FALSE)
  }

  if (is.null(tree_name)) {
    tree_name <- names(tree_objects)[1]
  }

  if (!tree_name %in% names(tree_objects)) {
    stop("Tree '", tree_name, "' not found. Available trees: ",
         paste0("'", names(tree_objects), "'", collapse = ", "),
         call. = FALSE)
  }

  list(tree = tree_objects[[tree_name]], name = tree_name)
}


#' Build Sankey layout from tree structure
#'
#' Recursively positions nodes for a Sankey diagram. X-axis = tree depth,
#' Y-axis = probability mass in [0, 1]. Children are stacked within their
#' parent's y-range proportionally to conditional probability.
#'
#' @param tree_df Data frame for one tree with columns: node, parent, formula.
#'   Root has parent = "".
#' @param cond_prob Data frame of conditional probabilities from eval_decision_tree.
#' @return Data frame with columns: node, depth, y_min, y_max, joint_prob,
#'   cond_prob_val, is_terminal, parent
#' @keywords internal
build_sankey_layout <- function(tree_df, cond_prob) {
  root_name <- tree_df$node[tree_df$parent == ""][1]

  # Nodes that have children
  parent_node_names <- unique(tree_df$parent[tree_df$parent != ""])

  nodes_list <- list()

  compute_joint_prob <- function(node_name) {
    parent <- tree_df$parent[tree_df$node == node_name][1]
    cp <- cond_prob[[node_name]][1]
    if (parent == "") return(cp)
    cp * compute_joint_prob(parent)
  }

  position_subtree <- function(node_name, y_min, y_max, depth) {
    joint_prob <- compute_joint_prob(node_name)
    cp_val <- cond_prob[[node_name]][1]
    parent <- tree_df$parent[tree_df$node == node_name][1]
    is_terminal <- !(node_name %in% parent_node_names)

    nodes_list[[length(nodes_list) + 1]] <<- data.frame(
      node = node_name,
      depth = depth,
      y_min = y_min,
      y_max = y_max,
      joint_prob = joint_prob,
      cond_prob_val = cp_val,
      is_terminal = is_terminal,
      parent = parent,
      stringsAsFactors = FALSE
    )

    # Recurse into children
    children <- tree_df$node[tree_df$parent == node_name]
    if (length(children) == 0) return()

    child_cps <- vapply(children, function(c) cond_prob[[c]][1], numeric(1))
    total_cp <- sum(child_cps)

    gap_size <- 0.005
    total_gap <- gap_size * max(length(children) - 1, 0)
    available <- (y_max - y_min) - total_gap

    cursor <- y_min
    for (j in seq_along(children)) {
      height <- (child_cps[j] / total_cp) * available
      position_subtree(children[j], cursor, cursor + height, depth + 1)
      cursor <- cursor + height + gap_size
    }
  }

  position_subtree(root_name, 0, 1, 0)
  do.call(rbind, nodes_list)
}


#' Build Sankey flow polygons from node layout
#'
#' For each parent-child edge, generates a sigmoid polygon connecting the
#' parent's sub-range to the child's range. Each flow is tagged with its
#' "first branch" (root's direct child ancestor) for coloring.
#'
#' @param nodes_df Data frame from build_sankey_layout
#' @param node_width Half-width of node blocks (default 0.04)
#' @return Data frame with columns: x, y, flow_id, first_branch
#' @keywords internal
build_sankey_flows <- function(nodes_df, node_width = 0.04) {
  non_root <- nodes_df[nodes_df$parent != "", ]

  if (nrow(non_root) == 0) {
    return(data.frame(
      x = numeric(0), y = numeric(0),
      flow_id = character(0), first_branch = character(0),
      stringsAsFactors = FALSE
    ))
  }

  get_first_branch <- function(node_name) {
    nd <- nodes_df[nodes_df$node == node_name, ]
    if (nd$depth[1] == 1) return(node_name)
    get_first_branch(nd$parent[1])
  }

  flows_list <- list()

  for (i in seq_len(nrow(non_root))) {
    child <- non_root[i, ]
    parent <- nodes_df[nodes_df$node == child$parent, ]

    first_branch <- get_first_branch(child$node)

    # Sigmoid from right edge of parent to left edge of child
    poly <- sigmoid_flow_polygon(
      x0 = parent$depth[1] + node_width,
      x1 = child$depth[1] - node_width,
      y_top_start = child$y_max,
      y_bot_start = child$y_min,
      y_top_end = child$y_max,
      y_bot_end = child$y_min,
      n_points = 50
    )

    poly$flow_id <- paste0(child$parent, "->", child$node)
    poly$first_branch <- first_branch

    flows_list[[i]] <- poly
  }

  do.call(rbind, flows_list)
}


#' Generate sigmoid flow polygon coordinates
#'
#' Creates a closed polygon for a smooth S-curve flow ribbon between two
#' x-positions. The top and bottom edges each follow a logistic sigmoid
#' interpolation between start and end y-coordinates.
#'
#' @param x0 Start x-coordinate
#' @param x1 End x-coordinate
#' @param y_top_start Top y at start
#' @param y_bot_start Bottom y at start
#' @param y_top_end Top y at end
#' @param y_bot_end Bottom y at end
#' @param n_points Number of points per edge (default 50)
#' @return Data frame with x and y columns forming a closed polygon
#' @keywords internal
sigmoid_flow_polygon <- function(x0, x1, y_top_start, y_bot_start,
                                  y_top_end, y_bot_end, n_points = 50) {
  t <- seq(0, 1, length.out = n_points)
  s <- 1 / (1 + exp(-12 * (t - 0.5)))
  s <- (s - s[1]) / (s[n_points] - s[1])  # normalize to exactly [0, 1]

  x <- x0 + (x1 - x0) * t
  y_top <- y_top_start + (y_top_end - y_top_start) * s
  y_bot <- y_bot_start + (y_bot_end - y_bot_start) * s

  # Polygon: top edge left-to-right, bottom edge right-to-left
  data.frame(
    x = c(x, rev(x)),
    y = c(y_top, rev(y_bot)),
    stringsAsFactors = FALSE
  )
}
