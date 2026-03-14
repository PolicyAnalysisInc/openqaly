test_that("get_locale returns US locale by default", {
  locale <- get_locale()
  expect_equal(locale$decimal, ".")
  expect_equal(locale$thousands, ",")
  expect_equal(locale$currency, c("$", ""))
})

test_that("get_locale returns correct locale for known countries", {
  de <- get_locale("DE")
  expect_equal(de$decimal, ",")
  expect_equal(de$thousands, ".")

  jp <- get_locale("JP")
  expect_equal(jp$decimal, ".")
})

test_that("get_locale falls back to US for unknown countries", {
  locale <- get_locale("ZZ")
  expect_equal(locale$decimal, ".")
  expect_equal(locale$thousands, ",")
})

test_that("get_results_locale extracts locale from results", {
  results <- list(metadata = list(settings = list(country = "DE")))
  locale <- get_results_locale(results)
  expect_equal(locale$decimal, ",")
})

test_that("get_results_locale prefers number_country over country", {
  results <- list(metadata = list(settings = list(country = "US", number_country = "FR")))
  locale <- get_results_locale(results)
  expect_equal(locale$decimal, ",")
  expect_equal(locale$currency, c("$", ""))

  currency_locale <- get_results_currency_locale(results)
  expect_equal(currency_locale$currency, c("$", ""))
})

test_that("get_results_locale defaults to US when country missing", {
  results <- list(metadata = list(settings = list()))
  locale <- get_results_locale(results)
  expect_equal(locale$decimal, ".")
})

test_that("auto_precision adapts to magnitude", {
  # Large values should get fewer decimals
  prec <- auto_precision(c(12345), base_precision = 2)
  expect_equal(prec, 0)

  # Small values should get more decimals
  prec <- auto_precision(c(0.005), base_precision = 2)
  expect_equal(prec, 5)

  # Medium values
  prec <- auto_precision(c(5.5), base_precision = 2)
  expect_equal(prec, 2)
})

test_that("auto_precision require_unique increases precision", {
  # These values differ in the 3rd decimal
  vals <- c(1.001, 1.002)
  prec <- auto_precision(vals, base_precision = 2, require_unique = TRUE)
  expect_gte(prec, 3)
})

test_that("auto_precision handles edge cases", {
  expect_equal(length(auto_precision(c(NA, NaN, Inf))), 3)
  expect_equal(auto_precision(numeric(0)), numeric(0))
})

test_that("auto_precision with different base_precision", {
  prec4 <- auto_precision(c(100), base_precision = 4)
  prec2 <- auto_precision(c(100), base_precision = 2)
  expect_gt(prec4, prec2)
})

test_that("oq_format formats with US locale", {
  formatted <- oq_format(1234567.89, decimals = 2)
  expect_equal(formatted, "1,234,567.89")
})

test_that("oq_format formats with DE locale", {
  locale <- get_locale("DE")
  formatted <- oq_format(1234567.89, decimals = 2, locale = locale)
  expect_equal(formatted, "1.234.567,89")
})

test_that("oq_format formats with JP locale", {
  locale <- get_locale("JP")
  formatted <- oq_format(1234567, decimals = 0, locale = locale)
  expect_equal(formatted, "1,234,567")
})

test_that("oq_format handles currency", {
  formatted <- oq_format(1234.56, decimals = 2, currency = TRUE)
  expect_equal(formatted, "$1,234.56")

  locale <- get_locale("DE")
  formatted <- oq_format(1234.56, decimals = 2, locale = locale, currency = TRUE)
  # DE has currency suffix
  expect_true(grepl("\u20AC", formatted))
})

test_that("oq_format can split numeric locale from currency locale", {
  formatted <- oq_format(
    12345.67,
    decimals = 2,
    locale = get_locale("FR"),
    currency = TRUE,
    currency_locale = get_locale("US")
  )

  expect_equal(gsub("\u00A0", " ", formatted), "$12 345,67")
})

test_that("oq_format places negative sign outside currency", {
  formatted <- oq_format(
    c(-2703, 55006, -8619),
    decimals = 0,
    locale = get_locale("US"),
    currency = TRUE
  )
  expect_equal(formatted, c("-$2,703", "$55,006", "-$8,619"))

  locale <- get_locale("DE")
  formatted <- oq_format(-2703, decimals = 0, locale = locale, currency = TRUE)
  expect_equal(gsub("\u00A0", " ", formatted), "-2.703 \u20AC")
})

test_that("oq_format handles abbreviation", {
  formatted <- oq_format(1500000, decimals = 1, abbreviate = TRUE)
  expect_equal(formatted, "1.5M")

  formatted <- oq_format(2500, decimals = 1, abbreviate = TRUE)
  expect_equal(formatted, "2.5K")
})

test_that("oq_format uses locale-specific abbreviation schemes when available", {
  formatted <- oq_format(250000, decimals = 1, locale = get_locale("IN"), abbreviate = TRUE)
  expect_equal(formatted, "2.5L")

  formatted <- oq_format(25000000, decimals = 1, locale = get_locale("IN"), abbreviate = TRUE)
  expect_equal(formatted, "2.5Cr")
})

test_that("oq_format handles NA/NaN/Inf", {
  expect_true(is.na(oq_format(NA_real_)))
  expect_equal(oq_format(NaN), "NaN")
  expect_equal(oq_format(Inf), "\u221E")
  expect_equal(oq_format(-Inf), "-\u221E")
})

test_that("oq_format auto-precision adapts to values", {
  # Large values - fewer decimals
  formatted <- oq_format(12345)
  expect_false(grepl("\\.", formatted))

  # Small values - more decimals
  formatted <- oq_format(0.00123)
  expect_true(nchar(sub(".*\\.", "", formatted)) >= 4)
})

test_that("oq_format negative values", {
  formatted <- oq_format(-1234.5, decimals = 1)
  expect_equal(formatted, "-1,234.5")
})

test_that("oq_label_fn returns a function", {
  fn <- oq_label_fn()
  expect_true(is.function(fn))
})

test_that("oq_label_fn produces unique labels", {
  fn <- oq_label_fn()
  vals <- c(1.001, 1.002, 1.003)
  labels <- fn(vals)
  expect_equal(length(unique(labels)), 3)
})

test_that("oq_label_fn with currency", {
  fn <- oq_label_fn(currency = TRUE)
  labels <- fn(c(100, 200))
  expect_true(all(grepl("\\$", labels)))
})

test_that("oq_unique_labels produces unique labels", {
  labels <- oq_unique_labels(c(0.775, 0.8, 0.825))
  expect_equal(length(unique(labels)), 3)
})

test_that("oq_percent_label_fn produces unique percent labels", {
  fn <- oq_percent_label_fn()
  labels <- fn(c(0.1001, 0.1002, 0.1003))
  expect_equal(length(unique(labels)), 3)
  expect_true(all(grepl("%$", labels)))
})

test_that("oq_format_icer handles NaN as Equivalent", {
  expect_equal(oq_format_icer(NaN), "Equivalent")
})

test_that("oq_format_icer handles Inf as Dominated", {
  expect_equal(oq_format_icer(Inf), "Dominated")
})

test_that("oq_format_icer handles -Inf as Dominant", {
  expect_equal(oq_format_icer(-Inf), "Dominant")
})

test_that("oq_format_icer handles 0 as Dominant", {
  expect_equal(oq_format_icer(0), "Dominant")
})

test_that("oq_format_icer handles NA as empty", {
  expect_equal(oq_format_icer(NA_real_), "")
})

test_that("oq_format_icer formats positive values", {
  formatted <- oq_format_icer(50000, decimals = 0)
  expect_equal(formatted, "$50,000")
})

test_that("oq_format_icer formats negative values with asterisk", {
  formatted <- oq_format_icer(-50000, decimals = 0)
  expect_equal(formatted, "$50,000*")
})

test_that("oq_format_icer works with locale", {
  locale <- get_locale("DE")
  formatted <- oq_format_icer(50000, decimals = 0, locale = locale)
  expect_equal(formatted, "50.000\u00A0€")
})

test_that("abbreviate_scale categorizes correctly", {
  result <- abbreviate_scale(c(500, 5000, 5000000, 5000000000, 5000000000000))
  expect_equal(result$suffix, c("", "K", "M", "B", "T"))
  expect_equal(result$scaled, c(500, 5, 5, 5, 5))
})

test_that("FR locale uses non-breaking space for thousands", {
  locale <- get_locale("FR")
  expect_equal(locale$thousands, "\u00A0")
  formatted <- oq_format(1234567, decimals = 0, locale = locale)
  expect_true(grepl("\u00A0", formatted))
})

test_that("IN locale uses Indian grouping", {
  locale <- get_locale("IN")
  formatted <- oq_format(12345678, decimals = 0, locale = locale)
  expect_equal(formatted, "1,23,45,678")
})
