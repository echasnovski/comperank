#' Keener method
#'
#' Functions to compute rating and ranking using Keener method.
#'
#' @inheritParams rate_massey
#' @param ... Head-to-Head expression (see [h2h_mat()][comperes::h2h_mat()]).
#' @param fill A single value to use instead of NA for missing pairs.
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param skew_fun Skew function.
#' @param normalize_fun Normalization function.
#' @param eps Coefficient for forcing irreducibility.
#' @inheritParams rank_massey
#' @param x Argument for `skew_keener()`.
#' @param mat Argument for `normalize_keener()`.
#'
#' @details Keener rating method is based on Head-to-Head matrix of the
#' competition results. Therefore it can be used for competitions with
#' variable number of players. Its algorithm is as follows:
#'
#' 1. Compute Head-to-Head matrix of competition results based on Head-to-Head
#' expression supplied in `...` (see [h2h_mat()][comperes::h2h_mat()]).
#' Head-to-Head values are computed based only on the games between players of
#' interest (see Players). Ensure that there are no `NA`s by using `fill`
#' argument. If `force_nonneg_h2h` is `TRUE` then the minimum value is
#' subtracted (in case some Head-to-Head value is strictly negative). __Note__
#' that Keener method is designed for Head-to-Head matrix with the following
#' property: the more value in row __i__ and column __j__ the better player from
#' row __i__ performed than player from column __j__. Supply Head-to-Head
#' expression appropriately.
#'
#' 1. Update raw Head-to-Head values (denoted as S) with the pair-normalization:
#' a_{ij} = (S_ij + 1) / (S_ij + S_ji + 2). This step should make comparing
#' different players more reasonable.
#'
#' 1. Skew Head-to-Head values with applying `skew_fun` to them. `skew_fun`
#' should take numeric vector as only argument. It should return skewed vector.
#' The default skew function is `skew_keener()`. This step should make abnormal
#' results not very abnormal. To omit this step supply `skew_fun = NULL`.
#'
#' 1. Normalize Head-to-Head values with `normalize_fun` using `cr_data`.
#' `normalize_fun` should take Head-to-Head matrix as the first argument and
#' `cr_data` as second. It should return normalized matrix. The default
#' normalization is `normalize_keener()` which divides Head-to-Head value of
#' 'player1'-'player2' matchup divided by the number of games played by
#' 'player1'. This step should take into account possibly not equal number of
#' games played by players. To omit this step supply `normalize_keener = NULL`.
#'
#' 1. Add small value to Head-to-Head matrix to ensure its irreducibility. If
#' all values are strictly positive then this step is omitted. In other case
#' small value is computed as the smallest non-zero Head-to-Head value
#' multiplied by `eps`. This step is done to ensure applicability of
#' Perron-Frobenius theorem.
#' 1. Compute Perron-Frobenius vector of the resultant matrix, i.e. the strictly
#' positive real eigenvector (which values sum to 1) for eigenvalue (which is
#' real) of the maximum absolute value. This vector is Keener rating vector.
#'
#' @inheritSection massey Players
#'
#' @return `rate_keener()` returns a named vector of the Keener rating. The
#' sum of all ratings should be equal to 1.
#'
#' `rank_keener()` returns a named vector of [ranking][rating-ranking] using
#' [round_rank()].
#'
#' @references James P. Keener (1993) *The Perron-Frobenius theorem and the
#'   ranking of football teams*. SIAM Review, 35(1):80–93, 1993.
#'
#' @examples
#' # Use transpose = TRUE is correct in this case
#' rate_keener(ncaa2005, sum(score1))
#' rank_keener(ncaa2005, sum(score1))
#'
#' # Impact of skewing
#' rate_keener(ncaa2005, sum(score1), skew_fun = NULL)
#'
#' # Impact of normalization.
#' rate_keener(ncaa2005[-(1:2), ], sum(score1))
#' rate_keener(ncaa2005[-(1:2), ], sum(score1), normalize_fun = NULL)
#'
#' @name keener
NULL

#' @rdname keener
#' @export
rate_keener <- function(cr_data, ..., fill = 0,
                        force_nonneg_h2h = TRUE,
                        skew_fun = skew_keener,
                        normalize_fun = normalize_keener,
                        eps = 0.001) {
  # Compute symmetrical Head-to-Head matrix
  mat <- cr_data %>%
    as_longcr(repair = TRUE) %>%
    h2h_mat(..., fill = fill) %>%
    force_nonneg(force = force_nonneg_h2h)
  mat <- (mat + 1) / (mat + t(mat) + 2)

  # Skew
  if (!identical(skew_fun, NULL)) {
    mat[, ] <- skew_fun(mat[, ])
  }

  # Normalize
  if (!identical(normalize_fun, NULL)) {
    mat <- normalize_fun(mat, cr_data)
  }

  # Force irreducibility
  is_min_h2h_zero <- isTRUE(all.equal(min(mat), 0))
  min_non_zero_h2h <- min(mat[mat > 0])
  mat <- mat + eps * as.integer(is_min_h2h_zero) * min_non_zero_h2h

  # Compute Perron-Frobenius vector
  res <- get_pf_vec(mat)
  names(res) <- rownames(mat)

  res
}

#' @rdname keener
#' @export
rank_keener <- function(cr_data, ..., fill = 0,
                        force_nonneg_h2h = TRUE,
                        skew_fun = skew_keener,
                        normalize_fun = normalize_keener,
                        eps = 0.001,
                        ties = c("average", "first", "last",
                                 "random", "max", "min"),
                        round_digits = 7) {
  round_rank(
    rate_keener(
      cr_data = cr_data, ..., fill = fill,
      force_nonneg_h2h = force_nonneg_h2h,
      skew_fun = skew_fun,
      normalize_fun = normalize_fun,
      eps = eps
    ),
    type = "desc", ties = ties, round_digits = round_digits
  )
}

force_nonneg <- function(x, force = TRUE) {
  if (force) {
    x <- x - min(min(x, na.rm = TRUE), 0)
  }

  x
}

#' @rdname keener
#' @export
skew_keener <- function(x) {
  (1 + sign(x - 0.5) * sqrt(abs(2*x - 1))) / 2
}

#' @rdname keener
#' @export
normalize_keener <- function(mat, cr_data) {
  player_num_games <- cr_data %>%
    as_longcr(repair = TRUE) %>%
    h2h_mat(!!h2h_funs[["num"]], fill = 0) %>%
    diag()

  mat / player_num_games
}
