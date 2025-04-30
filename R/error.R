define_error <- function(x) {
  define_object(
    message = modify_error_msg(as.character(x)),
    class = 'heRo_error'
  )
}

modify_error_msg <- function(x) {
  x <- gsub("Error in eval(x$expr, data, x$env): ", "", x, fixed = T)
  x <- gsub("Error: ", "", x, fixed = T)
  if (grepl('object.*not found', x)) {
    name <- strsplit(x, "'")[[1]][2]
    x <- glue('Variable "{name}" not found.')
  }
  x
}

# Determine whether a given object is an error
#' @export
is_hero_error <- function(x) {
  'heRo_error' %in% class(x)
}

#' @export
print.heRo_error <- function(x, ...) {
  print(glue("Error: {x$message}"))
}
