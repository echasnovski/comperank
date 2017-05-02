get_formatC_width <- function(vec) {
  floor(log10(length(unique(vec)))) + 1
}

add_class <- function(obj, class_char) {
  class(obj) <- c(class_char, class(obj))

  obj
}

assert_used_names <- function(used, original) {
  if (any(used != original)) {
    unmatched <- used != original
    used_names_message <- paste0(used[unmatched], " -> ", original[unmatched],
                                 collapse = "\n")
    message("Some matched names are not perfectly matched:\n",
            used_names_message, "\n")
  }

  invisible(TRUE)
}
