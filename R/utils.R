# General -----------------------------------------------------------------
add_class <- function(obj, class_char) {
  class(obj) <- c(class_char, class(obj))

  obj
}

to_list <- function(x) {
  if (!is.list(x)) {
    x <- list(x)
  }

  x
}

is_function_list <- function(x) {
  all(sapply(x, rlang::is_function))
}

to_function_list <- function(x, var_name = "input") {
  x <- to_list(x)

  if (!is_function_list(x)) {
    stop("Object '", var_name, "' should be function or list of functions.")
  }

  x
}

#' Rank vector after rounding
#'
#' Function for ranking vector after rounding.
#'
#' @param x A numeric, complex, character or logical vector.
#' @param type Type of ranking: `"desc"` or `"asc"` (see Details).
#' @param na.last For controlling the treatment of `NA`s. If `TRUE`, missing
#'   values in the data are put last; if `FALSE`, they are put first; if `NA`,
#'   they are removed; if `"keep"` they are kept with rank `NA`.
#' @param ties A character string specifying how ties are treated (see Details).
#'   Can be abbreviated.
#' @param round_digits Value of `digits` for [round()].
#'
#' @details This is basically a wrapper around [rank()] in which `x` is
#' pre-modified by rounding to specific number of digits `round_digits`.
#'
#' `type` can have two values: `"desc"` for ranking in descending order (rank 1
#' is given to the biggest value in `x`) and `"asc"` (rank 1 is given to the
#' smallest value in `x`). Any other value will cause error.
#'
#' @return A numeric vector of the same length as `x` with names copied from `x`
#' (unless `na.last = NA`, when missing values are removed). The vector is of
#' integer type unless `x` is a long vector or `ties = "average"` when it is of
#' double type (whether or not there are any ties).
#'
#' @examples
#' round_rank(10:1, type = "desc")
#' round_rank(10:1, type = "asc")
#'
#' set.seed(334)
#' x <- 10^(-10) * runif(10)
#' round_rank(x)
#'
#' @export
round_rank <- function(x, type = "desc", na.last = TRUE,
                       ties = c("average", "first", "last",
                                "random", "max", "min"),
                       round_digits = 7) {
  x <- round(x, digits = round_digits)

  if (type == "desc") {
    return(rank(-xtfrm(x), na.last = na.last, ties.method = ties))
  } else if (type == "asc") {
    return(rank(x, na.last = na.last, ties.method = ties))
  } else {
    stop("Wrong value of argument 'type'.")
  }
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

assert_pairgames <- function(cr_data, input_name = "cr_data") {
  if (!is_pairgames(cr_data)) {
    stop(
      input_name, " is not pairgames. ",
      "Convert manually or with comperes::to_pairgames.",
      call. = FALSE
    )
  }

  cr_data
}

assert_h2h_fun <- function(...) {
  if (rlang::dots_n(...) < 1) {
    stop("You should supply Head-to-Head expression.", call. = FALSE)
  }

  invisible(TRUE)
}

unique_levels <- function(x, na.last = TRUE) {
  if (is.null(levels(x))) {
    sort(unique(x), na.last = na.last)
  } else {
    levs <- levels(x)
    factor(levs, levels = levs, ordered = is.ordered(x), exclude = NULL)
  }
}

enframe_vec <- function(x, ref = NULL, name = "name", value = "value") {
  res_tbl <- tibble::enframe(x, name = name, value = value)

  if (is.null(ref)) {
    res_tbl
  } else {
    res <- tibble::tibble(ref = ref) %>%
      mutate(ref_chr = as.character(ref)) %>%
      left_join(y = res_tbl, by = c(ref_chr = name)) %>%
      select(-.data$ref_chr)
    colnames(res)[1] <- name

    res
  }
}

add_ranking <- function(df, rating_name, ranking_name,
                        keep_rating = FALSE,
                        type = "desc",
                        ties = c("average", "first", "last",
                                 "random", "max", "min"),
                        round_digits = 7) {
  df[[ranking_name]] <- round_rank(
    df[[rating_name]], type = type,
    ties = ties, round_digits = round_digits
  )

  if (!isTRUE(keep_rating)) {
    df <- df[-match(rating_name, colnames(df))]
  }

  df
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
