context("edit_value and remove_value")

# Helper to build a minimal model with values and summaries
make_test_model <- function() {
  define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_value("cost", 100, state = "healthy", type = "cost", display_name = "Cost", description = "Cost desc") |>
    add_value("cost", 200, state = "sick", type = "cost", display_name = "Cost", description = "Cost desc") |>
    add_value("qaly", 1, state = "healthy", type = "outcome", display_name = "QALY", description = "QALY desc") |>
    add_value("qaly", 0.5, state = "sick", type = "outcome", display_name = "QALY", description = "QALY desc") |>
    add_summary("total_cost", "cost", type = "cost") |>
    add_summary("total_qaly", "qaly", type = "outcome")
}

# =============================================================================
# edit_value — basic field updates
# =============================================================================

test_that("edit_value updates formula", {
  m <- make_test_model()
  m2 <- edit_value(m, "cost", state = "healthy", formula = 999)
  expect_equal(m2$values$formula[m2$values$name == "cost" & m2$values$state == "healthy"], "999")
})

test_that("edit_value updates formula with NSE expression", {
  m <- make_test_model()
  m2 <- edit_value(m, "cost", state = "healthy", formula = base + extra)
  expect_equal(m2$values$formula[m2$values$name == "cost" & m2$values$state == "healthy"], "base + extra")
})

test_that("edit_value updates type", {
  m <- make_test_model()
  m2 <- edit_value(m, "cost", state = "healthy", type = "outcome")
  expect_equal(m2$values$type[m2$values$name == "cost" & m2$values$state == "healthy"], "outcome")
})

test_that("edit_value rejects invalid type", {
  m <- make_test_model()
  expect_error(edit_value(m, "cost", state = "healthy", type = "invalid"),
               "must be 'outcome' or 'cost'")
})

test_that("edit_value updates display_name and description for single-row value", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_value("single", 42, state = "healthy", type = "cost")
  m2 <- edit_value(m, "single", state = "healthy", display_name = "New Name", description = "New desc")
  expect_equal(m2$values$display_name[m2$values$name == "single"], "New Name")
  expect_equal(m2$values$description[m2$values$name == "single"], "New desc")
})

test_that("edit_value errors on inconsistent display_name", {
  m <- make_test_model()
  # Changing only one row's display_name should fail validation
  expect_error(
    edit_value(m, "cost", state = "healthy", display_name = "Different"),
    "display_name|description"
  )
})

test_that("edit_value errors when row not found", {
  m <- make_test_model()
  expect_error(edit_value(m, "nonexistent", formula = 1), "No value found")
})

# =============================================================================
# edit_value — single-row rename (split)
# =============================================================================

test_that("edit_value renames single row to new name", {
  m <- make_test_model()
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "drug_cost")
  # The renamed row should exist

  expect_true(any(m2$values$name == "drug_cost" & m2$values$state == "healthy"))
  # The other cost row should still be "cost"
  expect_true(any(m2$values$name == "cost" & m2$values$state == "sick"))
})

test_that("edit_value single rename: display_name independent for split", {
  m <- make_test_model()
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "drug_cost",
                   display_name = "Drug Cost", description = "Drug cost desc")
  expect_equal(m2$values$display_name[m2$values$name == "drug_cost"], "Drug Cost")
  # Original cost rows unchanged
  expect_equal(unique(m2$values$display_name[m2$values$name == "cost"]), "Cost")
})

# =============================================================================
# edit_value — bulk rename (rename_all = TRUE)
# =============================================================================

test_that("edit_value bulk rename updates all rows and summaries", {
  m <- make_test_model()
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "total_drug_cost", rename_all = TRUE)
  # All rows should be renamed
  expect_false(any(m2$values$name == "cost"))
  expect_equal(sum(m2$values$name == "total_drug_cost"), 2)
  # Summaries should reference new name
  cost_summary <- m2$summaries[m2$summaries$name == "total_cost", ]
  tokens <- trimws(strsplit(cost_summary$values, ",")[[1]])
  expect_true("total_drug_cost" %in% tokens)
  expect_false("cost" %in% tokens)
})

# =============================================================================
# edit_value — error_on_name_sharing
# =============================================================================

test_that("edit_value error_on_name_sharing throws when name has multiple rows", {
  m <- make_test_model()
  err <- tryCatch(
    edit_value(m, "cost", state = "healthy", new_name = "drug_cost", error_on_name_sharing = TRUE),
    value_name_shared = function(e) e
  )
  expect_s3_class(err, "value_name_shared")
  expect_equal(err$shared_count, 2)
  expect_equal(nrow(err$states), 2)
})

test_that("edit_value error_on_name_sharing does NOT error for single row value", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_value("single_val", 42, state = "healthy", type = "cost")
  # Only one row — should not error
  m2 <- edit_value(m, "single_val", state = "healthy", new_name = "renamed_val", error_on_name_sharing = TRUE)
  expect_true(any(m2$values$name == "renamed_val"))
})

# =============================================================================
# edit_value — merge into existing name
# =============================================================================

test_that("edit_value merge auto-adopts target display_name/description", {
  m <- make_test_model()
  # Add a new value with different display_name
  m <- add_value(m, "admin_cost", 50, state = "dead", type = "cost",
                 display_name = "Admin Cost", description = "Admin desc")
  # Move cost/healthy into admin_cost — should auto-adopt "Admin Cost"
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "admin_cost")
  moved_row <- m2$values[m2$values$name == "admin_cost" & m2$values$state == "healthy", ]
  expect_equal(moved_row$display_name, "Admin Cost")
  expect_equal(moved_row$description, "Admin desc")
})

test_that("edit_value error_on_field_changes throws when fields differ", {
  m <- make_test_model()
  m <- add_value(m, "admin_cost", 50, state = "dead", type = "cost",
                 display_name = "Admin Cost", description = "Admin desc")
  err <- tryCatch(
    edit_value(m, "cost", state = "healthy", new_name = "admin_cost", error_on_field_changes = TRUE),
    value_field_changes = function(e) e
  )
  expect_s3_class(err, "value_field_changes")
  expect_true(!is.null(err$field_changes))
  expect_equal(err$field_changes$display_name$old, "Cost")
  expect_equal(err$field_changes$display_name$new, "Admin Cost")
})

test_that("edit_value merge with explicit display_name that matches target succeeds", {
  m <- make_test_model()
  m <- add_value(m, "admin_cost", 50, state = "dead", type = "cost",
                 display_name = "Admin Cost", description = "Admin desc")
  # Move and provide explicit display_name that matches target — should succeed
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "admin_cost",
                   display_name = "Admin Cost", description = "Admin desc")
  moved_row <- m2$values[m2$values$name == "admin_cost" & m2$values$state == "healthy", ]
  expect_equal(moved_row$display_name, "Admin Cost")
})

test_that("edit_value merge with inconsistent explicit display_name errors", {
  m <- make_test_model()
  m <- add_value(m, "admin_cost", 50, state = "dead", type = "cost",
                 display_name = "Admin Cost", description = "Admin desc")
  # Move with display_name that differs from target — should error on validation
  expect_error(
    edit_value(m, "cost", state = "healthy", new_name = "admin_cost",
               display_name = "Custom Name", description = "Custom desc"),
    "display_name must be consistent"
  )
})

# =============================================================================
# edit_value — orphan handling
# =============================================================================

test_that("edit_value updates summaries when last row moved from old name", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_value("solo_val", 42, state = "healthy", type = "cost") |>
    add_value("other_val", 24, state = "healthy", type = "cost") |>
    add_summary("s1", "solo_val, other_val", type = "cost")
  m2 <- edit_value(m, "solo_val", state = "healthy", new_name = "new_val")
  tokens <- trimws(strsplit(m2$summaries$values[1], ",")[[1]])
  expect_true("new_val" %in% tokens)
  expect_false("solo_val" %in% tokens)
})

test_that("edit_value does NOT update summaries when other rows still have old name", {
  m <- make_test_model()
  # cost has 2 rows. Move only one.
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "drug_cost")
  # cost still exists (sick row), so summaries should still reference "cost"
  cost_summary <- m2$summaries[m2$summaries$name == "total_cost", ]
  tokens <- trimws(strsplit(cost_summary$values, ",")[[1]])
  expect_true("cost" %in% tokens)
})

# =============================================================================
# edit_value — name collision
# =============================================================================

test_that("edit_value errors on table name collision", {
  m <- make_test_model()
  m$tables <- list(my_table = data.frame(x = 1))
  expect_error(edit_value(m, "cost", state = "healthy", new_name = "my_table"),
               "Name collision")
})

# =============================================================================
# edit_value — row-order preservation
# =============================================================================

test_that("edit_value preserves row order after formula edit", {
  m <- make_test_model()
  original_order <- paste(m$values$name, m$values$state)
  m2 <- edit_value(m, "cost", state = "healthy", formula = 999)
  expect_equal(paste(m2$values$name, m2$values$state), original_order)
})

test_that("edit_value preserves row order after type edit", {
  m <- make_test_model()
  original_order <- paste(m$values$name, m$values$state)
  m2 <- edit_value(m, "qaly", state = "healthy", type = "cost")
  expect_equal(paste(m2$values$name, m2$values$state), original_order)
})

test_that("edit_value preserves row order after display_name and description edit", {
  m <- define_model("markov") |>
    add_state("healthy", initial_prob = 1) |>
    add_state("sick", initial_prob = 0) |>
    add_state("dead", initial_prob = 0) |>
    add_value("cost", 100, state = "healthy", type = "cost") |>
    add_value("qaly", 1, state = "healthy", type = "outcome") |>
    add_value("util", 0.5, state = "sick", type = "outcome")
  original_order <- paste(m$values$name, m$values$state)
  m2 <- edit_value(m, "cost", state = "healthy", display_name = "New Cost", description = "New desc")
  expect_equal(paste(m2$values$name, m2$values$state), original_order)
})

test_that("edit_value preserves row order after single-row rename", {
  m <- make_test_model()
  original_states <- m$values$state
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "drug_cost")
  expect_equal(m2$values$state, original_states)
})

test_that("edit_value preserves row order after bulk rename (rename_all=TRUE)", {
  m <- make_test_model()
  original_states <- m$values$state
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "drug_cost", rename_all = TRUE)
  expect_equal(m2$values$state, original_states)
})

test_that("edit_value preserves row order after merge rename", {
  m <- make_test_model()
  m <- add_value(m, "admin_cost", 50, state = "dead", type = "cost",
                 display_name = "Admin Cost", description = "Admin desc")
  original_states <- m$values$state
  m2 <- edit_value(m, "cost", state = "healthy", new_name = "admin_cost")
  expect_equal(m2$values$state, original_states)
})

# =============================================================================
# remove_value — basic
# =============================================================================

test_that("remove_value removes all rows by name", {
  m <- make_test_model()
  m2 <- remove_value(m, "cost")
  expect_false(any(m2$values$name == "cost"))
  # qaly rows should remain
  expect_equal(sum(m2$values$name == "qaly"), 2)
})

test_that("remove_value removes specific row", {
  m <- make_test_model()
  m2 <- remove_value(m, "cost", state = "healthy")
  expect_false(any(m2$values$name == "cost" & m2$values$state == "healthy"))
  # sick row should remain
  expect_true(any(m2$values$name == "cost" & m2$values$state == "sick"))
})

test_that("remove_value errors when not found", {
  m <- make_test_model()
  expect_error(remove_value(m, "nonexistent"), "No value found")
})

# =============================================================================
# remove_value — error_on_dependencies
# =============================================================================

test_that("remove_value error_on_dependencies throws for summary deps", {
  m <- make_test_model()
  err <- tryCatch(
    remove_value(m, "cost", error_on_dependencies = TRUE),
    value_has_dependencies = function(e) e
  )
  expect_s3_class(err, "value_has_dependencies")
  expect_true("total_cost" %in% err$dependencies$summaries)
})

test_that("remove_value error_on_dependencies does NOT error for partial removal", {
  m <- make_test_model()
  # Removing just one row of "cost" — name still exists, so no dep error
  m2 <- remove_value(m, "cost", state = "healthy", error_on_dependencies = TRUE)
  expect_true(any(m2$values$name == "cost"))
})

# =============================================================================
# remove_value — cascade removal
# =============================================================================

test_that("remove_value cascade strips value from summary CSV", {
  m <- make_test_model()
  m2 <- remove_value(m, "cost")
  cost_summary <- m2$summaries[m2$summaries$name == "total_cost", ]
  # Summary row should still exist but value stripped
  expect_equal(nrow(cost_summary), 1)
  tokens <- trimws(strsplit(cost_summary$values, ",")[[1]])
  expect_false("cost" %in% tokens)
})

test_that("remove_value partial removal leaves summaries intact", {
  m <- make_test_model()
  m2 <- remove_value(m, "cost", state = "healthy")
  cost_summary <- m2$summaries[m2$summaries$name == "total_cost", ]
  tokens <- trimws(strsplit(cost_summary$values, ",")[[1]])
  # cost still has the sick row, so summary should still reference it
  expect_true("cost" %in% tokens)
})

# =============================================================================
# update_summary_value_refs helper
# =============================================================================

test_that("update_summary_value_refs replaces old name with new", {
  summaries <- openqaly:::fast_tibble(
    name = "s1",
    values = "val_a, val_b, val_c",
    display_name = "s1",
    description = "s1",
    type = "cost",
    wtp = NA_real_
  )
  result <- openqaly:::update_summary_value_refs(summaries, "val_b", "val_new")
  tokens <- trimws(strsplit(result$values, ",")[[1]])
  expect_true("val_new" %in% tokens)
  expect_false("val_b" %in% tokens)
})

test_that("update_summary_value_refs removes name when new_name is NULL", {
  summaries <- openqaly:::fast_tibble(
    name = "s1",
    values = "val_a, val_b",
    display_name = "s1",
    description = "s1",
    type = "cost",
    wtp = NA_real_
  )
  result <- openqaly:::update_summary_value_refs(summaries, "val_a", NULL)
  tokens <- trimws(strsplit(result$values, ",")[[1]])
  expect_false("val_a" %in% tokens)
  expect_true("val_b" %in% tokens)
})

test_that("update_summary_value_refs handles NULL summaries", {
  expect_null(openqaly:::update_summary_value_refs(NULL, "x", "y"))
})
