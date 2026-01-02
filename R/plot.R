#' Plot a Decision Tree
#'
#' Creates a visualization of a decision tree structure using ggraph,
#' displaying nodes and edges with formula labels.
#'
#' @param x A decision tree object containing a `df` component with columns
#'   `node`, `parent`, and `formula`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot2/ggraph object displaying the tree structure.
#'
#' @export
plot_decision_tree <- function(x, ...) {
  nodes <- rbind(data.frame(node = 'root'), x$df %>% select("node"))
  edges <- x$df %>% select("parent", "node", "formula") %>% mutate(parent = ifelse(is.na(.data$parent), 'root', .data$parent))
  the_graph <- tbl_graph(nodes, edges)
  ggraph(the_graph, layout = 'tree') +
    geom_node_point() +
    geom_edge_link(aes(label = .data$formula), angle_calc = 'along', label_dodge = unit(2.5, 'mm')) +
    geom_node_label(aes(label = .data$node)) +
    theme_void() +
    scale_x_continuous(expand = expansion(mult = 0.2))
}
