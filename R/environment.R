requireNamespace <- function(package, ...) base::requireNamespace(package, ...)

#' Check Model Environment Against Embedded Lockfile
#'
#' Compares the packages recorded in a model's embedded renv lockfile against
#' the currently installed package versions. Reports mismatches, missing
#' packages, and R version differences.
#'
#' @param model An openqaly model object with a \code{lockfile} field.
#' @param action One of \code{"warn"}, \code{"error"}, or \code{"silent"}.
#'   Controls how mismatches are reported.
#'
#' @return Invisibly returns a data.frame of mismatches with columns:
#'   \code{package}, \code{required}, \code{installed}, \code{status}.
#'
#' @export
check_model_environment <- function(model, action = c("warn", "error", "silent")) {
  action <- match.arg(action)
  lockfile <- model$lockfile
  if (is.null(lockfile)) return(invisible(NULL))

  issues <- character()

  # Check R version
  if (!is.null(lockfile$R$Version)) {
    current_r <- paste0(R.version$major, ".", R.version$minor)
    if (utils::compareVersion(current_r, lockfile$R$Version) != 0) {
      issues <- c(issues, sprintf(
        "R version mismatch: model requires %s, current is %s",
        lockfile$R$Version, current_r
      ))
    }
  }

  # Check package versions
  mismatches <- data.frame(
    package = character(0),
    required = character(0),
    installed = character(0),
    status = character(0),
    stringsAsFactors = FALSE
  )

  packages <- lockfile$Packages
  for (pkg_name in names(packages)) {
    required_ver <- packages[[pkg_name]]$Version
    if (is.null(required_ver)) next

    installed_ver <- tryCatch(
      as.character(utils::packageVersion(pkg_name)),
      error = function(e) NA_character_
    )

    if (is.na(installed_ver)) {
      mismatches <- rbind(mismatches, data.frame(
        package = pkg_name, required = required_ver,
        installed = "NOT INSTALLED", status = "missing",
        stringsAsFactors = FALSE
      ))
    } else if (utils::compareVersion(installed_ver, required_ver) != 0) {
      mismatches <- rbind(mismatches, data.frame(
        package = pkg_name, required = required_ver,
        installed = installed_ver, status = "mismatch",
        stringsAsFactors = FALSE
      ))
    }
  }

  # Build message
  if (nrow(mismatches) > 0) {
    pkg_lines <- sprintf(
      "  %s: need %s, have %s",
      mismatches$package, mismatches$required, mismatches$installed
    )
    issues <- c(issues, "Package version mismatches:", pkg_lines)
  }

  if (length(issues) > 0) {
    msg <- paste(
      "Model environment check:",
      paste(issues, collapse = "\n"),
      sep = "\n"
    )
    if (action == "error") {
      stop(msg, call. = FALSE)
    } else if (action == "warn") {
      warning(msg, call. = FALSE)
    }
  }

  invisible(mismatches)
}

#' Restore Model Environment from Embedded Lockfile
#'
#' Extracts the embedded renv lockfile from a model and calls
#' \code{renv::restore()} to install the recorded package versions.
#'
#' @param model An openqaly model object with a \code{lockfile} field.
#' @param prompt Logical. Whether to prompt before installing. Defaults to
#'   \code{interactive()}.
#' @param library Character. Library path to install packages into. Defaults
#'   to the first element of \code{.libPaths()}.
#' @param ... Additional arguments passed to \code{renv::restore()}.
#'
#' @export
restore_model_environment <- function(model, prompt = interactive(),
                                      library = .libPaths()[1], ...) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    stop(
      "Package 'renv' is required. Install with install.packages('renv').",
      call. = FALSE
    )
  }

  lockfile <- model$lockfile
  if (is.null(lockfile)) {
    stop("Model has no embedded lockfile.", call. = FALSE)
  }

  # Write just the renv portion to a temp file
  lockfile_path <- tempfile(fileext = ".json")
  on.exit(unlink(lockfile_path), add = TRUE)

  lockfile_json <- jsonlite::toJSON(lockfile, auto_unbox = TRUE, pretty = TRUE)
  writeLines(as.character(lockfile_json), lockfile_path)

  renv::restore(lockfile = lockfile_path, library = library, prompt = prompt, ...)
}
