#' Plot a Decision Tree
#'
#' Creates a health economics-standard visualization of a decision tree,
#' with horizontal left-to-right layout and standard node shapes:
#' circle for chance nodes and triangle for terminal nodes.
#'
#' When called with a model or builder object, edge labels show formula text.
#' When called with results from \code{run_model()}, edge labels show
#' evaluated conditional probabilities as percentages.
#'
#' @param x An \code{oq_model}, \code{oq_model_builder}, or results object
#'   from \code{run_model()}.
#' @param tree_name Character string specifying which tree to plot. If
#'   \code{NULL}, auto-detects from the model or results.
#' @param strategy Character string specifying which strategy to plot.
#'   Required when \code{x} is a results object.
#' @param group Character string specifying which group to plot. If \code{NULL}
#'   (default), uses the first group. Only used when \code{x} is a results object.
#' @param probability_decimals Fixed decimal places for probability labels, or NULL for auto-precision
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2/ggraph object displaying the tree structure.
#'
#' @export
plot_decision_tree <- function(x, tree_name = NULL, strategy = NULL,
                               group = NULL, probability_decimals = NULL,
                               ...) {

  # Get locale for formatting (results path has metadata, model path uses US default)
  locale <- if (!is.null(x$metadata)) {
    get_results_locale(x)
  } else {
    get_locale("US")
  }

  # Detect input type and extract tree_df + edge labels
  if (inherits(x, c("oq_model", "oq_model_builder"))) {
    # Model/builder path: formula labels
    tree_info <- .extract_model_tree(x, tree_name)
    tree_df <- tree_info$tree_df
    edge_labels <- tree_info$edge_labels
    root_label <- tree_info$root_label
    joint_probs <- tree_info$joint_probs
  } else if (!is.null(x$segments) &&
             "eval_vars" %in% colnames(x$segments)) {
    # Results path: probability labels
    tree_info <- .extract_results_tree(x, tree_name, strategy, group,
                                        probability_decimals = probability_decimals,
                                        locale = locale)
    tree_df <- tree_info$tree_df
    edge_labels <- tree_info$edge_labels
    root_label <- tree_info$root_label
    joint_probs <- tree_info$joint_probs
  } else if (!is.null(x$segments)) {
    stop("Results do not contain eval_vars. ",
         "plot_decision_tree() on results requires base-case output from run_model().",
         call. = FALSE)
  } else {
    stop("'x' must be an oq_model, oq_model_builder, or run_model() results object.",
         call. = FALSE)
  }

  .render_tree_plot(tree_df, edge_labels, root_label, joint_probs,
                    probability_decimals, locale)
}


# =============================================================================
# Internal helpers for plot_decision_tree
# =============================================================================

#' Extract tree data from a model/builder object
#' @param model An oq_model or oq_model_builder
#' @param tree_name Tree name or NULL for auto-detect
#' @return List with tree_df, edge_labels, root_label
#' @keywords internal
.extract_model_tree <- function(model, tree_name) {
  if (is.null(model$trees) || !is.data.frame(model$trees) ||
      nrow(model$trees) == 0) {
    stop("Model does not contain any decision trees.", call. = FALSE)
  }

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
      paste0("'", available_trees, "'", collapse = ", "),
      call. = FALSE
    )
  }

  tree_df <- model$trees[model$trees$name == tree_name, ]

  # Detect multiple root-level nodes and synthesize an implicit root
  root_mask <- is.na(tree_df$parent) | tree_df$parent == ""
  if (sum(root_mask) > 1) {
    synthetic_root <- tree_df[1, , drop = FALSE]
    synthetic_root$node <- tree_name
    synthetic_root$parent <- NA_character_
    synthetic_root$formula <- "1"
    tree_df$parent[root_mask] <- tree_name
    tree_df <- rbind(synthetic_root, tree_df)
    rownames(tree_df) <- NULL
  }

  non_root <- tree_df[!is.na(tree_df$parent) & tree_df$parent != "", ]

  list(
    tree_df = tree_df,
    edge_labels = non_root$formula,
    root_label = NULL,
    joint_probs = NULL
  )
}

#' Extract tree data from run_model() results
#' @param results Output from run_model()
#' @param tree_name Tree name or NULL for auto-detect
#' @param strategy Strategy name (required)
#' @param group Group name or NULL for first group
#' @return List with tree_df, edge_labels, root_label
#' @keywords internal
.extract_results_tree <- function(results, tree_name, strategy, group,
                                   probability_decimals = NULL, locale = NULL) {
  segments <- results$segments

  if (is.null(strategy)) {
    stop("'strategy' is required when plotting results. Available strategies: ",
         paste0("'", unique(segments$strategy), "'", collapse = ", "),
         call. = FALSE)
  }

  segments <- segments[segments$strategy == strategy, ]
  if (nrow(segments) == 0) {
    stop("Strategy '", strategy, "' not found in results.", call. = FALSE)
  }

  if (is.null(group)) {
    segments <- segments[1, , drop = FALSE]
  } else {
    segments <- segments[segments$group == group, ]
    if (nrow(segments) == 0) {
      stop("Group '", group, "' not found for strategy '", strategy, "'.",
           call. = FALSE)
    }
  }

  seg <- segments[1, ]

  # Extract eval_decision_tree from eval_vars environment
  ev <- seg$eval_vars[[1]]
  if (is.null(ev)) {
    stop("eval_vars not found in results. ",
         "plot_decision_tree() on results requires base-case output from run_model().",
         call. = FALSE)
  }

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

  eval_tree <- tree_objects[[tree_name]]
  tree_df <- eval_tree$df
  tree_df <- tree_df[tree_df$name == tree_name, ]
  tree_df$parent <- ifelse(is.na(tree_df$parent), "", tree_df$parent)

  cond_prob <- eval_tree$cond_prob

  # Detect multiple root-level nodes and synthesize an implicit root
  root_mask <- tree_df$parent == ""
  if (sum(root_mask) > 1) {
    synthetic_name <- tree_name
    synthetic_root <- tree_df[1, , drop = FALSE]
    synthetic_root$node <- synthetic_name
    synthetic_root$parent <- ""
    tree_df$parent[root_mask] <- synthetic_name
    tree_df <- rbind(synthetic_root, tree_df)
    rownames(tree_df) <- NULL
    # Mark root with parent="" so it is recognized as the single root
    tree_df$parent[tree_df$node == synthetic_name] <- ""
    cond_prob[[synthetic_name]] <- 1
  }

  non_root <- tree_df[tree_df$parent != "", ]

  prob_values <- vapply(non_root$node, function(n) {
    cond_prob[[n]][1] * 100
  }, numeric(1), USE.NAMES = FALSE)
  if (is.null(locale)) locale <- get_locale("US")
  edge_labels <- paste0(oq_format(prob_values, decimals = probability_decimals, locale = locale), "%")

  # Compute joint (unconditional) probabilities for all nodes
  compute_joint_prob <- function(node_name) {
    cp <- cond_prob[[node_name]][1]
    parent <- tree_df$parent[tree_df$node == node_name][1]
    if (parent == "") return(cp)
    cp * compute_joint_prob(parent)
  }
  joint_probs <- vapply(tree_df$node, compute_joint_prob, numeric(1),
                        USE.NAMES = TRUE)

  # Root label: strategy, plus group if multiple groups
  n_groups <- length(unique(results$segments$group))
  root_label <- strategy
  if (n_groups > 1) {
    root_label <- paste0(strategy, " (", seg$group, ")")
  }

  list(
    tree_df = tree_df,
    edge_labels = edge_labels,
    root_label = root_label,
    joint_probs = joint_probs
  )
}

#' Render a tree plot from prepared data
#' @param tree_df Data frame with node/parent columns
#' @param edge_labels Character vector of edge labels
#' @param root_label Optional label for root node
#' @param joint_probs Named numeric vector of joint probabilities
#' @param probability_decimals Fixed decimal places or NULL for auto
#' @param locale Locale for formatting
#' @return A ggplot2/ggraph object
#' @keywords internal
.render_tree_plot <- function(tree_df, edge_labels, root_label = NULL,
                               joint_probs = NULL, probability_decimals = NULL,
                               locale = NULL) {
  all_nodes <- tree_df$node
  parent_nodes <- unique(tree_df$parent[!is.na(tree_df$parent) &
                                          tree_df$parent != ""])
  leaf_nodes <- setdiff(all_nodes, parent_nodes)

  node_type <- ifelse(all_nodes %in% leaf_nodes, "terminal", "chance")

  node_labels <- all_nodes
  if (!is.null(root_label)) {
    node_labels[1] <- root_label
  }

  if (!is.null(joint_probs)) {
    if (is.null(locale)) locale <- get_locale("US")
    is_terminal <- node_type == "terminal"
    prob_pct <- joint_probs[all_nodes[is_terminal]] * 100
    formatted_pct <- oq_format(prob_pct, decimals = probability_decimals,
                                locale = locale)
    node_labels[is_terminal] <- paste0(
      node_labels[is_terminal], " (", formatted_pct, "%)"
    )
  }

  nodes_df <- data.frame(
    node = node_labels,
    node_type = node_type,
    stringsAsFactors = FALSE
  )

  non_root <- tree_df[!is.na(tree_df$parent) & tree_df$parent != "", ]
  edges_df <- data.frame(
    from = match(non_root$parent, all_nodes),
    to = match(non_root$node, all_nodes),
    label = edge_labels,
    stringsAsFactors = FALSE
  )

  the_graph <- tbl_graph(nodes = nodes_df, edges = edges_df)

  layout <- create_layout(the_graph, layout = "tree")
  edge_label_df <- data.frame(
    x = layout$x[edges_df$from] +
      (layout$x[edges_df$to] - layout$x[edges_df$from]) * 0.33,
    y = layout$y[edges_df$from] +
      (layout$y[edges_df$to] - layout$y[edges_df$from]) * 0.33,
    label = edge_labels,
    stringsAsFactors = FALSE
  )

  shape_map <- c(chance = 21, terminal = 24)

  is_non_term <- layout$node_type != "terminal"
  is_term <- layout$node_type == "terminal"
  non_term_df <- data.frame(
    x = layout$x[is_non_term], y = layout$y[is_non_term],
    label = layout$node[is_non_term], stringsAsFactors = FALSE
  )
  term_df <- data.frame(
    x = layout$x[is_term], y = layout$y[is_term],
    label = layout$node[is_term], stringsAsFactors = FALSE
  )

  ggraph(layout) +
    coord_flip(clip = "off") +
    scale_y_reverse(expand = expansion(mult = c(0.35, 0.25))) +
    geom_edge_link() +
    geom_node_point(
      aes(shape = .data$node_type, fill = .data$node_type),
      size = 5, colour = "black"
    ) +
    scale_shape_manual(values = shape_map) +
    scale_fill_discrete() +
    geom_label(
      data = non_term_df,
      aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3, label.padding = unit(1.5, "pt"),
      fill = "white", linewidth = 0,
      nudge_y = -0.08, hjust = 1, vjust = 0.15
    ) +
    geom_label(
      data = term_df,
      aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3, label.padding = unit(1.5, "pt"),
      fill = "white", linewidth = 0,
      nudge_y = 0.08, hjust = 0, vjust = 0.15
    ) +
    geom_label(
      data = edge_label_df,
      aes(x = .data$x, y = .data$y, label = .data$label),
      size = 3, label.padding = unit(1.5, "pt"),
      fill = "white", linewidth = 0,
      hjust = 0.5, vjust = 0.5
    ) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom"
    ) +
    scale_x_continuous(expand = expansion(mult = 0.05)) +
    labs(shape = "Node Type", fill = "Node Type")
}

#' Plot an evaluated decision tree object
#' @param x An eval_decision_tree object
#' @param locale Locale for formatting, or NULL for US default
#' @return A ggplot2/ggraph object
#' @keywords internal
.plot_eval_decision_tree <- function(x, locale = NULL) {
  if (is.null(locale)) locale <- get_locale("US")

  tree_df <- x$df[x$df$name == x$df$name[1], ]
  tree_df$parent <- ifelse(is.na(tree_df$parent), "", tree_df$parent)

  # Handle synthetic root for multiple root-level nodes
  root_mask <- tree_df$parent == ""
  if (sum(root_mask) > 1) {
    tree_name <- tree_df$name[1]
    synthetic_root <- tree_df[1, , drop = FALSE]
    synthetic_root$node <- tree_name
    synthetic_root$parent <- ""
    tree_df$parent[root_mask] <- tree_name
    tree_df <- rbind(synthetic_root, tree_df)
    rownames(tree_df) <- NULL
    tree_df$parent[tree_df$node == tree_name] <- ""
    x$cond_prob[[tree_name]] <- 1
  }

  # Edge labels from conditional probabilities
  non_root <- tree_df[tree_df$parent != "", ]
  prob_values <- vapply(non_root$node, function(n) {
    x$cond_prob[[n]][1] * 100
  }, numeric(1), USE.NAMES = FALSE)
  edge_labels <- paste0(oq_format(prob_values, locale = locale), "%")

  # Joint probabilities from terminal nodes
  joint_probs <- setNames(
    sapply(x$terminal_nodes, function(n) n$prob[1]),
    sapply(x$terminal_nodes, function(n) n$node)
  )

  .render_tree_plot(tree_df, edge_labels, root_label = NULL,
                     joint_probs = joint_probs, locale = locale)
}
