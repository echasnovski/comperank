# General -----------------------------------------------------------------
#' Skip some action
#'
#' Function for skipping action.
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
  reduce_f <- function(x, y) {
    full_join(x = x, y = y, by = by)
  }

  Reduce(f = reduce_f, x = x)
}

# Rating methods ----------------------------------------------------------
assert_used_objects <- function(used, original, prefix = "",
                                object_name, data_name) {
  not_present <- setdiff(used, original)
  if (length(not_present) > 0) {
    message(prefix, "These ", object_name,
            " are absent in ", data_name, ":\n  ",
            paste0(not_present, collapse = ", "), "\n")
  }

  invisible(TRUE)
}


#' Convert between ratings as named vector and tibble
#'
#' Functions to convert between different forms of ratings.
#'
#' @param rating_vec Named vector of ratings.
#' @param rating_tbl Tibble with columns 'player' and 'rating'.
#'
#' @return \code{to_rating_tbl} returns a \code{\link[=tbl_df]{tibble}} with
#' two columns: 'player' and 'rating'.
#'
#' \code{to_rating_vec} returns named vector of ratings.
#'
#' @examples
#' rating_vec <- c("pl1" = 1, "pl2" = 10, "pl3" = 100)
#' rating_tbl <- to_rating_tbl(rating_vec)
#' rating_tbl
#' to_rating_vec(rating_tbl)
#'
#' @name convert-rating
NULL

#' @rdname convert-rating
#' @export
to_rating_tbl <- function(rating_vec) {
  tibble(
    player = names(rating_vec),
    rating = rating_vec
  )
}

#' @rdname convert-rating
#' @export
to_rating_vec <- function(rating_tbl) {
  res <- rating_tbl$rating
  names(res) <- as.character(rating_tbl$player)

  res
}

#' Compute Perron-Frobenius vector
#'
#' Function to compute Perron-Frobenius vector.
#'
#' @param mat Square matrix
#'
#' @return Perron-Frobenius vector which values sum to 1
#'
#' @keywords internal
get_pf_vec <- function(mat) {
  mat_eigen_data <- eigen(mat)

  max_eigenvalue_ind <- which.max(abs(mat_eigen_data$value))
  fp_val <- mat_eigen_data$value[max_eigenvalue_ind]

  fp_vec <- mat_eigen_data$vectors[, max_eigenvalue_ind]
  fp_vec <- fp_vec * sign(Re(fp_vec[1]))

  is_value_real <- isTRUE(all.equal(Im(fp_val), 0))
  is_value_pos <- Re(fp_val) > 0
  is_all_real <- isTRUE(all.equal(Im(fp_vec), rep(0, length(fp_vec))))
  is_all_pos <- all(Re(fp_vec) > 0)

  if (!(is_value_real && is_value_pos && is_all_real && is_all_pos)) {
    stop("Something is wrong with application of Perron-Frobenius theorem.")
  }
  fp_vec <- Re(fp_vec)

  fp_vec / sum(fp_vec)
}
