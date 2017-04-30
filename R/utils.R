get_formatC_width <- function(vec) {
  floor(log10(length(unique(vec)))) + 1
}

add_class <- function(obj, class_char) {
  class(obj) <- c(class_char, class(obj))

  obj
}
