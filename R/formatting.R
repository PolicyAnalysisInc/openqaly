#' @title Locale-Aware Number Formatting
#' @name formatting
#' @description Central formatting engine for locale-aware number and currency
#'   formatting. Provides auto-precision, abbreviation, and ICER formatting.
#' @keywords internal
NULL

# Environment to cache loaded locales
.formatting_env <- new.env(parent = emptyenv())

# Map from country codes to locale file names
.locale_map <- c(
  US = "en-US", GB = "en-GB", CA = "en-CA", IN = "en-IN",
  DE = "de-DE", CH = "de-CH", FR = "fr-FR", IT = "it-IT",
  ES = "es-ES", MX = "es-MX", NL = "nl-NL", NO = "nb-NO",
  SE = "sv-SE", DK = "da-DK", FI = "fi-FI", JP = "ja-JP",
  CN = "zh-CN", KR = "ko-KR", BR = "pt-BR", PL = "pl-PL",
  CZ = "cs-CZ", HU = "hu-HU", RU = "ru-RU", UA = "uk-UA",
  IL = "he-IL"
)

.compact_suffix_fallback <- list(
  `1000` = "K",
  `1000000` = "M",
  `1000000000` = "B",
  `1000000000000` = "T"
)

#' Get Locale Data
#'
#' Reads locale data from bundled d3-format JSON files. Results are cached
#' for performance.
#'
#' @param country Two-letter country code (default: "US")
#' @return A list with elements: decimal, thousands, grouping, currency
#' @export
get_locale <- function(country = "US") {
  locale_id <- resolve_locale_id(country)

  # Check cache
  cache_key <- paste0("locale_", locale_id)
  if (exists(cache_key, envir = .formatting_env)) {
    return(get(cache_key, envir = .formatting_env))
  }

  locale_path <- system.file("locales", paste0(locale_id, ".json"),
                             package = "openqaly")
  if (locale_path == "") {
    # Fallback to en-US
    locale_path <- system.file("locales", "en-US.json", package = "openqaly")
  }

  locale <- jsonlite::fromJSON(locale_path, simplifyVector = TRUE)
  if (is.null(locale$compact_suffixes)) {
    locale$compact_suffixes <- .compact_suffix_fallback
  }

  # Cache and return
  assign(cache_key, locale, envir = .formatting_env)
  locale
}

#' Get Locale from Results Object
#'
#' Extracts country from results metadata and returns the corresponding locale.
#'
#' @param results A results object with metadata$settings$country
#' @return A locale list (see \code{\link{get_locale}})
#' @export
get_results_locale <- function(results) {
  number_locale <- get_results_number_locale(results)
  currency_locale <- get_results_currency_locale(results)
  number_locale$currency <- currency_locale$currency
  number_locale
}

#' @rdname get_results_locale
#' @export
get_results_number_locale <- function(results) {
  settings <- results$metadata$settings
  number_country <- settings$number_country
  country <- results$metadata$settings$country
  if (is.null(number_country) || is.na(number_country) || number_country == "") {
    number_country <- country
  }
  if (is.null(number_country) || is.na(number_country) || number_country == "") {
    number_country <- "US"
  }
  get_locale(number_country)
}

#' @rdname get_results_locale
#' @export
get_results_currency_locale <- function(results) {
  country <- results$metadata$settings$country
  if (is.null(country) || is.na(country) || country == "") {
    country <- "US"
  }
  get_locale(country)
}

#' Auto-Precision Algorithm
#'
#' Determines appropriate number of decimal places based on value magnitude.
#' Uses adaptive fixed-point precision: \code{digits = max(0, base_precision - oom)}
#' where oom is the order of magnitude.
#'
#' @param values Numeric vector of values
#' @param base_precision Base precision level (default: 2)
#' @param exact If TRUE, use significant figures instead of adaptive precision
#' @param require_unique If TRUE, increase precision until all non-identical
#'   values format differently
#' @param abbreviate If TRUE, values may be scaled by K/M/B/T
#' @param locale Locale list used for abbreviation scaling
#' @return Integer vector of decimal places (one per value, or scalar if
#'   require_unique is TRUE)
#' @export
auto_precision <- function(values, base_precision = 2, exact = FALSE,
                           require_unique = FALSE, abbreviate = FALSE,
                           locale = NULL) {
  # Remove NA/NaN/Inf for calculation
  finite_vals <- values[is.finite(values)]
  if (length(finite_vals) == 0) return(rep(base_precision, length(values)))

  if (abbreviate) {
    # Scale values to their abbreviated form
    finite_vals <- abbreviate_scale(finite_vals, locale = locale)$scaled
  }

  if (require_unique) {
    # Start from exact inherent precision if exact mode, otherwise from 0
    start_decimals <- if (exact) {
      compute_precision_single(finite_vals, base_precision, exact = TRUE)
    } else {
      0
    }
    # Increase precision until all distinct values format differently
    n_unique <- length(unique(round(finite_vals, 10)))
    decimals <- start_decimals
    max_decimals <- 10
    while (decimals <= max_decimals) {
      formatted <- format_plain(finite_vals, decimals)
      if (length(unique(formatted)) >= n_unique) break
      decimals <- decimals + 1
    }
    return(decimals)
  }

  # Per-value precision
  sapply(values, function(v) {
    if (!is.finite(v)) return(base_precision)
    val <- if (abbreviate) abbreviate_scale(v, locale = locale)$scaled else v
    compute_precision_single(val, base_precision, exact)
  })
}

# Internal: compute precision for a single value or vector (returns scalar)
compute_precision_single <- function(vals, base_precision, exact) {
  vals <- vals[is.finite(vals) & vals != 0]
  if (length(vals) == 0) return(base_precision)

  if (exact) {
    # Compute inherent decimal places needed to represent each value exactly
    # Use tolerance-based comparison to handle floating point imprecision
    # (e.g. 0.1 is not exactly representable in binary)
    # Cap at base_precision
    tol <- sqrt(.Machine$double.eps)
    inherent <- max(sapply(vals, function(v) {
      d <- 0
      while (d < base_precision && abs(v - round(v, d)) > tol) {
        d <- d + 1
      }
      d
    }))
    return(inherent)
  }

  # Adaptive fixed-point: digits = max(0, base_precision - oom)
  abs_vals <- abs(vals)
  oom <- floor(log10(abs_vals))
  digits <- pmax(0, base_precision - oom)
  max(digits)
}

# Internal: format numbers without locale (for uniqueness checking)
format_plain <- function(values, decimals) {
  formatC(values, format = "f", digits = decimals)
}

#' Abbreviation Scale Helper
#'
#' Scales numeric values by K/M/B/T suffixes.
#'
#' @param values Numeric vector
#' @param locale Locale list
#' @param currency If TRUE, use currency compact data when available
#' @return List with \code{scaled} values and \code{suffix} character vector
#' @keywords internal
abbreviate_scale <- function(values, locale = NULL, currency = FALSE) {
  if (is.null(locale)) {
    locale <- get_locale("US")
  }

  abs_vals <- abs(values)
  suffix <- character(length(values))
  scaled <- values

  spec <- get_compact_suffixes(locale, currency = currency)
  finite <- which(!is.na(abs_vals))

  for (i in finite) {
    compact_rule <- select_compact_rule(abs_vals[i], spec)
    if (!is.null(compact_rule)) {
      scaled[i] <- values[i] / compact_rule$divisor
      suffix[i] <- compact_rule$suffix
    }
  }

  list(scaled = scaled, suffix = suffix)
}

#' Format Numbers with Locale
#'
#' Primary formatting function. Formats numbers using locale-specific decimal
#' and thousands separators, with optional currency symbols and abbreviation.
#'
#' @param x Numeric vector to format
#' @param decimals Fixed number of decimals, or NULL for auto-precision
#' @param locale Locale list from \code{\link{get_locale}}, or NULL for US
#' @param currency If TRUE, add currency prefix/suffix from locale
#' @param currency_locale Locale list for currency symbol/placement. Defaults
#'   to \code{locale}.
#' @param abbreviate If TRUE, use K/M/B/T suffixes
#' @param base_precision Base precision for auto mode (default: 2)
#' @param exact If TRUE, use significant figures
#' @param require_unique If TRUE, ensure distinct values format uniquely
#' @return Character vector of formatted values
#' @export
oq_format <- function(x, decimals = NULL, locale = NULL, currency = FALSE,
                       currency_locale = locale,
                       abbreviate = FALSE, base_precision = 2,
                       exact = FALSE, require_unique = FALSE) {
  if (is.null(locale)) locale <- get_locale("US")
  if (is.null(currency_locale)) currency_locale <- locale

  dec_mark <- locale$decimal
  thou_mark <- locale$thousands
  currency_sym <- currency_locale$currency

  # Handle abbreviation scaling
  if (abbreviate) {
    abbr <- abbreviate_scale(x, locale = locale, currency = currency)
    display_vals <- abbr$scaled
    suffixes <- abbr$suffix
  } else {
    display_vals <- x
    suffixes <- rep("", length(x))
  }

  # Determine precision
  if (is.null(decimals)) {
    if (require_unique) {
      # Single precision for all values
      dec <- auto_precision(x, base_precision = base_precision, exact = exact,
                            require_unique = TRUE, abbreviate = abbreviate,
                            locale = locale)
      decs <- rep(dec, length(x))
    } else {
      decs <- auto_precision(x, base_precision = base_precision, exact = exact,
                             require_unique = FALSE, abbreviate = abbreviate,
                             locale = locale)
    }
  } else {
    decs <- rep(decimals, length(x))
  }

  # Format each value
  result <- vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.nan(v)) return("NaN")
    if (is.na(v)) return(NA_character_)
    if (is.infinite(v)) {
      return(if (v > 0) "\u221E" else "-\u221E")
    }

    dv <- display_vals[i]
    d <- decs[i]

    # Format number
    formatted <- format_number_locale(abs(dv), d, dec_mark, thou_mark,
                                      locale$grouping)

    # Add suffix
    formatted <- paste0(formatted, suffixes[i])

    # Add currency
    if (currency) {
      formatted <- paste0(currency_sym[1], formatted, currency_sym[2])
    }

    # Add sign for negatives (after currency so sign wraps the whole thing)
    if (dv < 0) {
      formatted <- paste0("-", formatted)
    }

    formatted
  }, character(1))

  result
}

#' Create ggplot2 Label Function
#'
#' Returns a closure suitable for use with \code{scale_*_continuous(labels = ...)}.
#' Uses \code{require_unique = TRUE} so axis labels are always distinguishable.
#'
#' @inheritParams oq_format
#' @return A function that takes a numeric vector and returns formatted strings
#' @export
oq_label_fn <- function(decimals = NULL, locale = NULL, currency = FALSE,
                         currency_locale = locale, abbreviate = FALSE,
                         base_precision = 2) {
  force(decimals)
  force(locale)
  force(currency)
  force(currency_locale)
  force(abbreviate)
  force(base_precision)
  function(x) {
    oq_format(x, decimals = decimals, locale = locale, currency = currency,
              currency_locale = currency_locale,
              abbreviate = abbreviate, base_precision = base_precision,
              require_unique = TRUE)
  }
}

# Format a known numeric vector with uniqueness-safe auto precision.
oq_unique_labels <- function(x, decimals = NULL, locale = NULL, currency = FALSE,
                             currency_locale = locale, abbreviate = FALSE,
                             base_precision = 2) {
  oq_format(x, decimals = decimals, locale = locale, currency = currency,
            currency_locale = currency_locale, abbreviate = abbreviate,
            base_precision = base_precision, require_unique = TRUE)
}

# Create a uniqueness-safe percentage label function for ggplot2 scales.
oq_percent_label_fn <- function(decimals = NULL, locale = NULL,
                                base_precision = 2, scale = 100,
                                suffix = "%") {
  force(decimals)
  force(locale)
  force(base_precision)
  force(scale)
  force(suffix)
  function(x) {
    vals <- x * scale
    formatted <- oq_unique_labels(vals, decimals = decimals, locale = locale,
                                  base_precision = base_precision)
    ifelse(is.na(formatted), NA_character_, paste0(formatted, suffix))
  }
}

#' Format ICER Values
#'
#' Formats incremental cost-effectiveness ratio values with special handling
#' for non-finite cases:
#' \itemize{
#'   \item NaN -> "Equivalent"
#'   \item Inf -> "Dominated"
#'   \item 0 -> "Dominant"
#'   \item Negative -> absolute value with "*" suffix
#' }
#'
#' @param x Numeric vector of ICER values
#' @param decimals Fixed decimals or NULL for auto
#' @param locale Locale list or NULL for US
#' @param currency_locale Locale list for currency symbol/placement
#' @param abbreviate If TRUE, use K/M/B/T suffixes
#' @param base_precision Base precision for auto mode (default: 2)
#' @return Character vector of formatted ICER values
#' @export
oq_format_icer <- function(x, decimals = NULL, locale = NULL,
                            currency_locale = locale, abbreviate = FALSE,
                            base_precision = 2) {
  if (is.null(locale)) locale <- get_locale("US")
  if (is.null(currency_locale)) currency_locale <- locale

  result <- character(length(x))

  # Handle special cases first
  result[is.na(x) & !is.nan(x)] <- ""
  result[is.nan(x)] <- "Equivalent"
  result[is.infinite(x) & x > 0] <- "Dominated"
  result[is.infinite(x) & x < 0] <- "Dominant"
  result[!is.na(x) & !is.nan(x) & !is.infinite(x) & x == 0] <- "Dominant"

  # Positive ICERs
  pos <- !is.na(x) & !is.nan(x) & !is.infinite(x) & x > 0
  if (any(pos)) {
    result[pos] <- oq_format(x[pos], decimals = decimals, locale = locale,
                             currency_locale = currency_locale,
                             abbreviate = abbreviate, base_precision = base_precision,
                             currency = TRUE)
  }

  # Negative ICERs (SW quadrant) — show absolute value with asterisk
  neg <- !is.na(x) & !is.nan(x) & !is.infinite(x) & x < 0
  if (any(neg)) {
    formatted <- oq_format(abs(x[neg]), decimals = decimals, locale = locale,
                           currency_locale = currency_locale,
                           abbreviate = abbreviate, base_precision = base_precision,
                           currency = TRUE)
    result[neg] <- paste0(formatted, "*")
  }

  result
}

resolve_locale_id <- function(value) {
  if (is.null(value) || is.na(value) || value == "") {
    return("en-US")
  }

  if (grepl("^[A-Za-z]{2}-[A-Za-z]{2}$", value)) {
    return(value)
  }

  locale_id <- unname(.locale_map[toupper(value)])
  if (!length(locale_id) || is.na(locale_id)) {
    return("en-US")
  }

  locale_id
}

get_compact_suffixes <- function(locale, currency = FALSE) {
  field_name <- if (currency && !is.null(locale$compact_currency_suffixes)) {
    "compact_currency_suffixes"
  } else {
    "compact_suffixes"
  }

  suffixes <- locale[[field_name]]
  if (is.null(suffixes)) {
    suffixes <- .compact_suffix_fallback
  }

  suffixes
}

select_compact_rule <- function(value, suffixes) {
  thresholds <- suppressWarnings(as.numeric(names(suffixes)))
  if (!length(thresholds) || all(is.na(thresholds)) || !is.finite(value)) {
    return(NULL)
  }

  valid <- !is.na(thresholds) & thresholds <= value
  if (!any(valid)) {
    return(NULL)
  }

  divisor <- max(thresholds[valid])
  suffix_idx <- match(divisor, thresholds)
  suffix <- unname(unlist(suffixes[suffix_idx], use.names = FALSE))[1]
  if (is.na(suffix) || !nzchar(suffix)) {
    return(NULL)
  }

  list(divisor = divisor, suffix = suffix)
}

format_number_locale <- function(value, decimals, decimal_mark, thousands_mark,
                                 grouping) {
  raw <- formatC(value, format = "f", digits = decimals, big.mark = "",
                 decimal.mark = ".")
  parts <- strsplit(raw, ".", fixed = TRUE)[[1]]
  integer_part <- parts[1]
  fractional_part <- if (length(parts) > 1) parts[2] else NULL

  grouped_integer <- apply_grouping(integer_part, grouping, thousands_mark)
  if (!is.null(fractional_part) && nzchar(fractional_part)) {
    paste0(grouped_integer, decimal_mark, fractional_part)
  } else {
    grouped_integer
  }
}

apply_grouping <- function(integer_part, grouping, thousands_mark) {
  if (is.null(grouping) || !length(grouping) || nchar(integer_part) <= grouping[[1]]) {
    return(integer_part)
  }

  digits <- strsplit(integer_part, "", fixed = TRUE)[[1]]
  groups <- character(0)
  idx <- length(digits)
  group_idx <- 1

  while (idx > 0) {
    group_size <- grouping[[min(group_idx, length(grouping))]]
    start <- max(1, idx - group_size + 1)
    groups <- c(paste0(digits[start:idx], collapse = ""), groups)
    idx <- start - 1
    group_idx <- group_idx + 1
  }

  paste(groups, collapse = thousands_mark)
}
