# General -----------------------------------------------------------------
#' Skip some action
#'
#' @param x Input object
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return An exact copy of \code{x}.
#'
#' @examples identical(skip_action(1), 1)
#'
#' @export
skip_action <- function(x, ...) {
  x
}

# Competition results -----------------------------------------------------
get_formatC_width <- function(vec) {
  floor(log10(length(unique(vec)))) + 1
}

add_class <- function(obj, class_char) {
  class(obj) <- c(class_char, class(obj))

  obj
}

assert_used_names <- function(info, prefix = "") {
# info is a data.frame that should consist from two columns:
  # target - names of used columns;
  # original - names of original columns.
  absent_original <- is.na(info$original)

  if (any(absent_original)) {
    message(
      prefix,
      sprintf(
        "Next columns are not found. Creating with NAs.\n  %s",
        paste0(info$target[absent_original], collapse = ", ")
      ), "\n"
    )
  }

  target <- info$target[!absent_original]
  original <- info$original[!absent_original]
  if (any(!absent_original) && any(target != original)) {
    unmatched <- target != original
    used_names_message <-
      paste0(original[unmatched], " -> ", target[unmatched], collapse = "\n  ")
    message(prefix,
            "Some matched names are not perfectly matched:\n  ",
            used_names_message, "\n")
  }

  invisible(TRUE)
}

renamecreate_columns <- function(df, info, fill = NA_integer_) {
# info is a data.frame that should consist from two columns:
  # target - names of target columns (which will be repaired into);
  # original - names of original columns (which will be repaired from).
    # If original is NA then new column with corresponded target name is
    #created with values from 'fill'.
  res <- df
  absent_original <- is.na(info$original)
  if (any(absent_original)) {
    res[, info$target[absent_original]] <- rep(list(rep(fill, nrow(df))))
  }
  if (any(!absent_original)) {
    colnames(res)[match(info$original[!absent_original], colnames(res))] <-
      info$target[!absent_original]
  }

  res
}

reduce_full_join <- function(x, by) {
  if (length(x) == 1) {
    return(x[[1]])
  }

  reduce_f <- function(x, y) {
    full_join(x = x, y = y, by = by)
  }

  Reduce(f = reduce_f, x = x[-1], init = x[[1]])
}
