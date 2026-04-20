context("Model Lockfile / Environment")

# Sample lockfile as an R list (mimics parsed renv.lock)
sample_lockfile <- list(
  R = list(
    Version = "4.4.2",
    Repositories = list(
      list(Name = "CRAN", URL = "https://cloud.r-project.org")
    )
  ),
  Packages = list(
    jsonlite = list(
      Package = "jsonlite",
      Version = "1.8.9",
      Source = "Repository",
      Repository = "CRAN",
      Hash = "abcdef1234567890"
    ),
    rlang = list(
      Package = "rlang",
      Version = "1.1.0",
      Source = "Repository",
      Repository = "CRAN",
      Hash = "1234567890abcdef"
    )
  )
)

sample_lockfile_json <- jsonlite::toJSON(sample_lockfile, auto_unbox = TRUE, pretty = TRUE)

# ===========================================================================
# Builder functions
# ===========================================================================

test_that("set_lockfile stores lockfile from list", {
  model <- define_model("markov") |>
    set_lockfile(sample_lockfile)

  expect_true(!is.null(model$lockfile))
  expect_equal(model$lockfile$R$Version, "4.4.2")
  expect_equal(model$lockfile$Packages$jsonlite$Version, "1.8.9")
})

test_that("set_lockfile stores lockfile from JSON string", {
  model <- define_model("markov") |>
    set_lockfile(as.character(sample_lockfile_json))

  expect_true(!is.null(model$lockfile))
  expect_equal(model$lockfile$R$Version, "4.4.2")
})

test_that("set_lockfile validates JSON", {
  model <- define_model("markov")
  expect_error(set_lockfile(model, "not valid json"), "valid JSON")
})

test_that("set_lockfile requires R and Packages fields", {
  model <- define_model("markov")
  expect_error(
    set_lockfile(model, list(foo = "bar")),
    "R.*Packages"
  )
})

test_that("remove_lockfile clears the field", {
  model <- define_model("markov") |>
    set_lockfile(sample_lockfile) |>
    remove_lockfile()

  expect_null(model$lockfile)
})

test_that("snapshot_lockfile captures current environment", {
  model <- define_model("markov") |>
    snapshot_lockfile()

  expect_true(!is.null(model$lockfile))
  expect_true(!is.null(model$lockfile$R))
  expect_true(!is.null(model$lockfile$Packages))
  expect_equal(
    model$lockfile$R$Version,
    paste0(R.version$major, ".", R.version$minor)
  )
  # Should include at least some common packages
  expect_true("jsonlite" %in% names(model$lockfile$Packages))
  expect_true("rlang" %in% names(model$lockfile$Packages))
})

test_that("capture_lockfile errors on missing file", {
  model <- define_model("markov")
  expect_error(
    capture_lockfile(model, lockfile_path = "nonexistent_renv.lock"),
    "not found"
  )
})

# ===========================================================================
# JSON round-trip
# ===========================================================================

test_that("lockfile survives JSON round-trip", {
  withr::local_options(openqaly.env_check = "none")

  model <- define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    set_lockfile(sample_lockfile)

  json_str <- write_model_json(model)

  # Verify the raw JSON contains renv fields at top level
  raw <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  expect_true(!is.null(raw$R))
  expect_true(!is.null(raw$Packages))
  expect_equal(raw$R$Version, "4.4.2")
  expect_equal(raw$Packages$jsonlite$Version, "1.8.9")

  # Verify model fields are also present
  expect_true(!is.null(raw$settings))

  # Read back and verify lockfile extracted
  model2 <- read_model_json(text = json_str)
  expect_true(!is.null(model2$lockfile))
  expect_equal(model2$lockfile$R$Version, "4.4.2")
  expect_equal(model2$lockfile$Packages$rlang$Version, "1.1.0")
})

test_that("model without lockfile round-trips with NULL lockfile", {
  withr::local_options(openqaly.env_check = "none")

  model <- define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    )

  json_str <- write_model_json(model)
  model2 <- read_model_json(text = json_str)
  expect_null(model2$lockfile)
})

# ===========================================================================
# check_model_environment
# ===========================================================================

test_that("check_model_environment returns NULL for no lockfile", {
  model <- define_model("markov")
  result <- check_model_environment(model, action = "silent")
  expect_null(result)
})

test_that("check_model_environment detects version mismatches", {
  lockfile <- list(
    R = list(Version = "4.4.2", Repositories = list()),
    Packages = list(
      jsonlite = list(
        Package = "jsonlite",
        Version = "0.0.1",
        Source = "Repository"
      )
    )
  )

  model <- define_model("markov") |> set_lockfile(lockfile)
  result <- check_model_environment(model, action = "silent")
  expect_true(nrow(result) > 0)
  expect_true("jsonlite" %in% result$package)
  expect_equal(result$status[result$package == "jsonlite"], "mismatch")
})

test_that("check_model_environment detects missing packages", {
  lockfile <- list(
    R = list(Version = "4.4.2", Repositories = list()),
    Packages = list(
      totallyFakePackage999 = list(
        Package = "totallyFakePackage999",
        Version = "1.0.0",
        Source = "Repository"
      )
    )
  )

  model <- define_model("markov") |> set_lockfile(lockfile)
  result <- check_model_environment(model, action = "silent")
  expect_equal(nrow(result), 1)
  expect_equal(result$status[1], "missing")
})

test_that("check_model_environment warns by default", {
  lockfile <- list(
    R = list(Version = "4.4.2", Repositories = list()),
    Packages = list(
      totallyFakePackage999 = list(
        Package = "totallyFakePackage999",
        Version = "1.0.0"
      )
    )
  )

  model <- define_model("markov") |> set_lockfile(lockfile)
  expect_warning(
    check_model_environment(model, action = "warn"),
    "environment check"
  )
})

test_that("check_model_environment errors when action = error", {
  lockfile <- list(
    R = list(Version = "4.4.2", Repositories = list()),
    Packages = list(
      totallyFakePackage999 = list(
        Package = "totallyFakePackage999",
        Version = "1.0.0"
      )
    )
  )

  model <- define_model("markov") |> set_lockfile(lockfile)
  expect_error(
    check_model_environment(model, action = "error"),
    "environment check"
  )
})

# ===========================================================================
# Option control
# ===========================================================================

test_that("openqaly.env_check = none suppresses checks", {
  withr::local_options(openqaly.env_check = "none")

  lockfile <- list(
    R = list(Version = "0.0.1", Repositories = list()),
    Packages = list(
      totallyFakePackage999 = list(
        Package = "totallyFakePackage999",
        Version = "1.0.0"
      )
    )
  )

  model <- define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    set_lockfile(lockfile)

  expect_no_warning(normalize_and_validate_model(model))
})

# ===========================================================================
# restore_model_environment
# ===========================================================================

test_that("restore_model_environment errors when no lockfile", {
  skip_if_not_installed("renv")

  model <- define_model("markov")
  expect_error(
    restore_model_environment(model),
    "no embedded lockfile"
  )
})

test_that("restore_model_environment errors when renv not installed", {
  model <- define_model("markov") |> set_lockfile(sample_lockfile)

  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "renv") FALSE else base::requireNamespace(pkg, ...),
    .package = "openqaly"
  )

  expect_error(
    restore_model_environment(model),
    "renv.*required"
  )
})

# ===========================================================================
# renv compatibility of output JSON
# ===========================================================================

test_that("model JSON with lockfile is valid renv lockfile structure", {
  withr::local_options(openqaly.env_check = "none")

  model <- define_model("markov") |>
    set_settings(
      n_cycles = 10,
      cycle_length = 1,
      cycle_length_unit = "years"
    ) |>
    set_lockfile(sample_lockfile)

  json_str <- write_model_json(model)
  raw <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Verify renv-required structure

  expect_true("R" %in% names(raw))
  expect_true("Packages" %in% names(raw))
  expect_true("Version" %in% names(raw$R))
  expect_true("Repositories" %in% names(raw$R))
  expect_true("jsonlite" %in% names(raw$Packages))
  expect_true("Version" %in% names(raw$Packages$jsonlite))
})
