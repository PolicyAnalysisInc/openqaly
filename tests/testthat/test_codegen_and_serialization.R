# Generated R code, exports and serialisation helpers.

test_that("apply_discounting is not exported", {
  expect_false("apply_discounting" %in% getNamespaceExports("openqaly"))
})

# Decision-tree model built with the builder API (3 nodes).
make_decision_tree_model <- function() {
  define_model("decision_tree") |>
    add_variable("drug_a_cost", 10000) |>
    add_strategy("treatment") |>
    add_tree_node("treatment", "root", parent = NA, formula = 1) |>
    add_tree_node("treatment", "drug_a", parent = "root", formula = 0.6) |>
    add_tree_node("treatment", "drug_b", parent = "root", formula = "C") |>
    set_decision_tree("treatment", duration = 0, duration_unit = "days") |>
    add_value("drug_cost", type = "cost", state = "decision_tree",
              formula = p(drug_a, treatment) * drug_a_cost + p(drug_b, treatment) * 5000) |>
    add_summary("total_cost", type = "cost", values = "drug_cost")
}
test_that("model_to_r_code emits one add_tree_node() per node and rebuilds the same tree", {
  m <- make_decision_tree_model()
  code <- model_to_r_code(m)

  node_lines <- grep("add_tree_node(", code, fixed = TRUE, value = TRUE)
  expect_length(node_lines, nrow(m$trees))

  # Intentional eval(): runs the package's own generated R code from a fixture
  # model built in this test. No external or user input is evaluated.
  rebuilt_env <- new.env(parent = globalenv())
  eval(parse(text = code), envir = rebuilt_env)
  cols <- c("name", "node", "parent", "formula")
  expect_equal(as.data.frame(rebuilt_env$model$trees[, cols]),
               as.data.frame(m$trees[, cols]))
})

test_that("DSA codegen keeps range_label", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("dead", initial_prob = 0) |>
    add_strategy("treatment") |>
    add_variable("p", 0.1) |>
    add_transition("healthy", "dead", "p") |>
    add_transition("healthy", "healthy", "1 - p") |>
    add_transition("dead", "dead", "1") |>
    add_dsa_variable("p", low = 0.05, high = 0.2, range_label = "lo to hi")
  expect_match(paste(model_to_r_code(m), collapse = "\n"), "range_label", fixed = TRUE)
})

test_that("convert_oq_formulas keeps a data.frame as a data.frame", {
  expect_s3_class(convert_oq_formulas(data.frame(a = 1:2)), "data.frame")
})
