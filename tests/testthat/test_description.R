# Package metadata.

test_that("Imports declares stats and utils", {
  imports <- utils::packageDescription("openqaly")$Imports
  expect_match(imports, "\\bstats\\b")
  expect_match(imports, "\\butils\\b")
})
