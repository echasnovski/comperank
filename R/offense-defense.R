#' Offense-Defense method
#'
#' Functions to compute [rating][rating-ranking] and [ranking][rating-ranking]
#' using Offense-Defense method.
#'
#' @inheritParams rate_massey
#' @param ... Head-to-Head expression (see [h2h_mat()][comperes::h2h_mat()]).
#' @param force_nonneg_h2h Whether to force nonnegative values in Head-to-Head
#'   matrix.
#' @param eps Coefficient for total support.
#' @param tol Tolerance value for iterative algorithm.
#' @param max_iterations Maximum number of iterations for iterative algorithm.
#' @param keep_rating Whether to keep rating columns in ranking output.
#' @inheritParams rank_massey
#'
#' @details Offense-Defense (OD) rating is designed for games in which player's
#' goal is to make higher score than opponent(s). To describe competition
#' results Head-to-Head matrix is computed using `...` (see
#' [h2h_mat()][comperes::h2h_mat()] for technical details and section __Design
#' of Head-to-Head values__ for design details). For pairs of players without
#' common games Head-to-Head value is computed to 0 (not `NA`). __Note__ that
#' values should be non-negative and non-NA. This can be ensured with setting
#' `force_nonneg_h2h` to `TRUE`.
#'
#' For player which can achieve _high_ Head-to-Head value (even against the
#' player with strong defense) it is said that he/she has __strong offense__
#' which results into _high_ offensive rating. For player which can force their
#' opponents into achieving _low_ Head-to-Head value (even if they have strong
#' offense) it is said that he/she has __strong defense__ which results into
#' _low_ defensive rating.
#'
#' Offensive and defensive ratings describe different skills of players. In
#' order to fully rate players, OD ratings are computed: offensive ratings
#' divided by defensive. The more OD rating the better player performance.
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
#' \code{def_{k+1} = t(A) \%*\% inv(A \%*\% inv(def_{k}))} where `def_{k}`
#' is a vector of defensive ratings at iteration `k`, `A` is a perturbed
#' Head-to-Head matrix, `inv(x) = 1 / x`. Iterative search stops if at least one
#' of two conditions is met:
#'     - `sum(abs(def_{k+1} / def_{k} - 1)) < tol`.
#'     - Number of iterations exceeds maximum number of iterations
#'     `max_iterations`.
#'
#' 1. Compute offensive ratings: \code{off = A \%*\% inv(def)}.
#'
#' 1. Compute OD ratings: `od = off / def`.
#'
#' Ratings are computed based only on games between players of interest (see
#' Players). However, be careful with OD ratings for players with no games:
#' they will have weak offense (because they "scored" 0 in all games) but
#' strong defense (because all their opponents also "scored" 0 in all common
#' games). Therefore accounting for missing players might be not a very good
#' idea.
#'
#' @inheritSection keener Design of Head-to-Head values
#'
#' @inheritSection massey Players
#'
#' @return `rate_od()` returns a [tibble][tibble::tibble] with the following
#' columns:
#' - __player__ - player identifier.
#' - __rating_off__ - offensive [rating][rating-ranking] of player. __Bigger
#' value indicates better player performance__.
#' - __rating_def__ - defensive rating of player. __Smaller value indicates
#' better player performance__.
#' - __rating_od__ - Offense-Defense rating of player. __Bigger value indicates
#' better player performance__.
#'
#' `rank_od()` returns a `tibble` of the similar structure as `rate_od()`:
#' - __player__ - player identifier.
#' - __rating_off__, __rating_def__, __rating_od__ - ratings (if
#' `keep_rating = TRUE`).
#' - __ranking_off__, __ranking_def__, __ranking_od__ -
#' [rankings][rating-ranking] computed with [round_rank()].
#'
#' @references Amy N. Langville, Carl D. Meyer (2012) *Who’s #1?: The
#'   science of rating and ranking*.
#'
#' Philip A. Knight (2008) *The Sinkhorn-Knopp algorithm:
#' Convergence and applications.*. SIAM Journal of Matrix Analysis,
#' 30(1):261–275, 2008 (For stopping rule of iterative algorithm).
#'
#' @examples
#' rate_od(ncaa2005, mean(score1))
#'
#' rank_od(ncaa2005, mean(score1))
#'
#' rank_od(ncaa2005, mean(score1), keep_rating = TRUE)
#'
#' # Account for self play
#' rate_od(ncaa2005, if(player1[1] == player2[1]) 0 else mean(score1))
#'
#' @name offense-defense
NULL

#' @rdname offense-defense
#' @export
rate_od <- function(cr_data, ..., force_nonneg_h2h = TRUE,
                    eps = 1e-3, tol = 1e-4, max_iterations = 100) {
  assert_h2h_fun(...)

  cr <- as_longcr(cr_data, repair = TRUE)
  mat <- cr %>%
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
  off_ratings <- mat %*% (1 / def_ratings)
  od_ratings <- off_ratings / def_ratings

  enframe_vec(
    off_ratings[, 1], unique_levels(cr$player),
    "player", "rating_off"
  ) %>%
    mutate(rating_def = def_ratings[, 1], rating_od = od_ratings[, 1])
}

#' @rdname offense-defense
#' @export
rank_od <- function(cr_data, ..., force_nonneg_h2h = TRUE,
                    eps = 1e-3, tol = 1e-4, max_iterations = 100,
                    keep_rating = FALSE,
                    ties = c("average", "first", "last",
                             "random", "max", "min"),
                    round_digits = 7) {
  rate_od(
    cr_data = cr_data, ..., force_nonneg_h2h = force_nonneg_h2h,
    eps = eps, tol = tol, max_iterations = max_iterations
  ) %>%
    add_ranking(
      "rating_off", "ranking_off",
      keep_rating = keep_rating, type = "desc",
      ties = ties, round_digits = round_digits
    ) %>%
    add_ranking(
      "rating_def", "ranking_def",
      keep_rating = keep_rating, type = "asc",
      ties = ties, round_digits = round_digits
    ) %>%
    add_ranking(
      "rating_od", "ranking_od",
      keep_rating = keep_rating, type = "desc",
      ties = ties, round_digits = round_digits
    )
}

od_def_iteration <- function(mat, vec) {
  t(mat) %*% (1 / (mat %*% (1 / vec)))
}

od_stop_stat <- function(prev, cur) {
  sum(abs(cur / prev - 1))
}
