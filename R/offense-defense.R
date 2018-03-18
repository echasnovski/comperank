#' Offense-Defense method
#'
#' Functions to compute rating and ranking using Offense-Defense method.
#'
#' @inheritParams rate_massey
#' @param ... Head-to-Head expression (see [h2h_mat()][comperes::h2h_mat()]).
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param eps Coefficient for total support.
#' @param tol Tolerance value for iterative algorithm.
#' @param max_iterations Maximum number of iterations for iterative algorithm.
#' @inheritParams rank_massey
#'
#' @details Offense-Defense (OD) rating is designed for games in which player's
#' goal is to make higher score than opponent(s). To describe competition
#' results Head-to-Head matrix is used (which is computed with `...` via
#' [h2h_mat()][comperes::h2h_mat()]). Element in row __i__ and column __j__
#' should represent the score player from column __j__ makes in games with
#' player from row __i__. For pairs of players without common games Head-to-Head
#' value is computed to 0 (not `NA`). __Note__ that values should be
#' non-negative and non-NA. This can be ensured with setting `force_nonneg_h2h`
#' to `TRUE`.
#'
#' For player which can make _high_ score (even against the player with strong
#' defense) it is said that he/she has __strong offense__ which results into
#' _high_ offensive rating. For player which can force their opponents into
#' making _low_ score (even if they have strong offense) it is said that he/she
#' has __strong defense__ which results into _low_ defensive rating.
#'
#' Offensive and defensive ratings describe different skills of players. In
#' order to fully rate players, OD ratings are computed: offensive ratings
#' divided be defensive. The more OD rating the better the player.
#'
#' Algorithm for OD ratings is as follows:
#'
#' 1. Compute Head-to-Head matrix using `...`.
#'
#' 1. Add small value to Head-to-Head matrix to ensure convergence of the
#' iterative algorithm in the next step. If all values are strictly positive
#' then this step is omitted. In other case small value is computed as the
#' smallest non-zero Head-to-Head value multiplied by `eps`.
#'
#' 1. Perform iterative fixed point search with the following recurrent formula:
#' \code{def_{k+1} = A \%*\% inv(t(A) \%*\% inv(def_{k}))} where `def_{k}`
#' is a vector of defensive ratings at iteration `k`, `A` is a perturbed
#' Head-to-Head matrix, `inv(x) = 1 / x`. Iterative search stops if at least one
#' of two conditions is met:
#'     - `sum(abs(def_{k+1} / def_{k} - 1)) < tol`.
#'     - Number of iterations exceeds maximum number of iterations
#'     `max_iterations`.
#'
#' 1. Compute offensive ratings: \code{off = t(A) \%*\% inv(def)}.
#'
#' 1. Compute OD ratings: `OD = off / def`.
#'
#' Ratings are computed based only on games between players of interest (see
#' Players). However, be careful with OD ratings for players with no games:
#' their will have weak offense (because they "scored" 0 in all games) but
#' strong defense (because all their opponents also "scored" 0 in all common
#' games). Therefor accounting for missing players might be not a very good
#' idea.
#'
#' @inheritSection massey Players
#'
#' @return `rate_od()` returns a matrix with rows named by players and the
#' following three columns:
#'
#' - __off__ - offensive rating of player.
#' - __def__ - defensive rating of player.
#' - __od__ - Offense-Defense rating of player.
#'
#' `rank_od()` returns a matrix of the same structure as `rate_od()` but
#' with [ranking][rating-ranking] using [round_rank()].
#'
#' @references Amy N. Langville, Carl D. Meyer (2012) *Who’s #1?: The
#'   science of rating and ranking*.
#'
#' Philip A. Knight (2008) *The Sinkhorn-Knopp algorithm:
#' Convergence and applications.*. SIAM Journal of Matrix Analysis,
#' 30(1):261–275, 2008 (For stopping rule of iterative algorithm).
#'
#' @examples
#' rate_od(ncaa2005, mean(score2))
#' rank_od(ncaa2005, mean(score2))
#'
#' # Account for self play
#' rate_od(ncaa2005, ifelse(player1[1] == player2[1], 0, mean(score2)))
#'
#' @name offense-defense
NULL

#' @rdname offense-defense
#' @export
rate_od <- function(cr_data, ..., force_nonneg_h2h = TRUE,
                    eps = 1e-3, tol = 1e-4, max_iterations = 100) {
  mat <- cr_data %>%
    as_longcr(repair = TRUE) %>%
    h2h_mat(..., fill = 0) %>%
    force_nonneg(force = force_nonneg_h2h)

  # Force total support
  is_min_h2h_zero <- isTRUE(all.equal(min(mat), 0))
  min_non_zero_h2h <- min(mat[mat > 0])
  mat <- mat + eps * as.integer(is_min_h2h_zero) * min_non_zero_h2h

  # OD iterations
  prev <- rep(1, nrow(mat))
  cur <- od_def_iteration(mat, prev)
  for (i in 2:max(2, max_iterations)) {
    if (od_stop_stat(prev, cur) < tol) {
      break
    }
    prev <- cur
    cur <- od_def_iteration(mat, prev)
  }

  def_ratings <- cur
  off_ratings <- t(mat) %*% (1 / def_ratings)
  od_ratings <- off_ratings / def_ratings

  res <- cbind(off_ratings, def_ratings, od_ratings)
  colnames(res) <- c("off", "def", "od")

  res
}

#' @rdname offense-defense
#' @export
rank_od <- function(cr_data, ..., force_nonneg_h2h = TRUE,
                    eps = 1e-3, tol = 1e-4, max_iterations = 100,
                    ties = c("average", "first", "last",
                             "random", "max", "min"),
                    round_digits = 7) {
  res <- rate_od(
    cr_data = cr_data, ..., force_nonneg_h2h = force_nonneg_h2h,
    eps = eps, tol = tol, max_iterations = max_iterations
  )

  res[, "off"] <- round_rank(res[, "off", drop = TRUE], type = "desc",
                             ties = ties, round_digits = round_digits)
  res[, "def"] <- round_rank(res[, "def", drop = TRUE], type = "asc",
                             ties = ties, round_digits = round_digits)
  res[, "od"] <- round_rank(res[, "od", drop = TRUE], type = "desc",
                            ties = ties, round_digits = round_digits)

  res
}

od_def_iteration <- function(mat, vec) {
  mat %*% (1 / (t(mat) %*% (1 / vec)))
}

od_stop_stat <- function(prev, cur) {
  sum(abs(cur / prev - 1))
}
