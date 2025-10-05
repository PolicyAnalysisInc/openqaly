# Test negative zero behavior in R
cat("Testing negative zero behavior:\n\n")

# Test 1: Simple rounding
x <- -0.00001
rounded <- round(x, 4)
cat("Original:", x, "\n")
cat("Rounded to 4 decimals:", rounded, "\n")
cat("Sign of rounded:", sign(rounded), "\n")
cat("sprintf output:", sprintf("%.4f", rounded), "\n\n")

# Test 2: Multiple values
values <- c(-0.00001, -0.00005, -0.000001, 0.00001, 0.00005)
cat("Testing multiple values:\n")
for (val in values) {
  r <- round(val, 4)
  cat(sprintf("  %.6f -> round(x,4) = %.4f, sprintf = %s\n",
              val, r, sprintf("%.4f", r)))
}
cat("\n")

# Test 3: Check flextable behavior
if (requireNamespace("flextable", quietly = TRUE)) {
  cat("Testing flextable::colformat_double:\n")
  df <- data.frame(
    label = c("neg_small", "pos_small", "neg_tiny", "pos_tiny"),
    value = c(-0.00001, 0.00001, -0.00009, 0.00009)
  )

  cat("\nOriginal data frame:\n")
  print(df)

  # Test what colformat_double actually does
  cat("\nChecking what happens with sprintf formatting:\n")
  for (i in 1:nrow(df)) {
    cat(sprintf("  %s: %.10f -> sprintf(\"%.4f\") = %s\n",
                df$label[i], df$value[i], sprintf("%.4f", df$value[i])))
  }
} else {
  cat("flextable not installed, skipping flextable test\n")
}
