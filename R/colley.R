#' Colley method
#'
#' Functions to compute rating and ranking using Colley method.
#'
#' @inheritParams rate_massey
#' @inheritParams rank_massey
#'
#' @details This rating method was initially designed for games between two
#' players. That is why competition results are transformed into pairwise games
#' `cr_pair` via [to_pairgames()][comperes::to_pairgames()]. Data from `cr_pair`
#' is used in rating computation.
#'
#' It is assumed that score is numeric and higher values are better for the
#' player.
#'
#' Computation is done based only on the games between players of interest (see
#' Players). __Note__ that it isn't necessary for all players of interest to be
#' present in `cr_pair` but it might be a good idea in order to obtain plausible
#' outcome rating.
#'
#' The outline of the Colley method is as follows:
#'
#' 1. Compute Colley matrix: diagonal elements are equal to number of games
#' played by certain player _plus 2_, off-diagonal are equal to minus number of
#' common games played. This matrix will be the matrix of system of linear
#' equations (SLE).
#'
#' 1. Compute right-hand side of SLE as 1 + 0.5*("number of player's wins" -
#' "number of player's losses").
#'
#' 1. Solve the SLE. The solution is the Colley rating.
#'
#' @inheritSection massey Players
#'
#' @return `rate_colley()` returns a named vector of the Colley rating. The mean
#' rating should be 0.5.
#'
#' `rank_colley()` returns a named vector of [ranking][rating-ranking] using
#' [round_rank()].
#'
#' @references Wesley N. Colley (2002) *Colley’s Bias Free College Football
#'   Ranking Method: The Colley Matrix Explained*. Available online at
#'   <http://www.colleyrankings.com>
#'
#' @examples rate_colley(ncaa2005)
#' rank_colley(ncaa2005)
#'
#' @name colley
NULL

#' @rdname colley
#' @export
rate_colley <- function(cr_data) {
  cr <- as_longcr(cr_data, repair = TRUE)
  if (!is_pairgames(cr)) {
    cr <- to_pairgames(cr)
  }

  # Compute Colley ratings
  colley_mat <- - h2h_mat(cr, !!h2h_funs[["num"]], fill = 0)
  diag(colley_mat) <- 0
  diag(colley_mat) <- 2 - rowSums(colley_mat)

  win_mat <- h2h_mat(cr, !!h2h_funs[["num_wins"]], fill = 0)
  right_hand <- 1 + 0.5*(rowSums(win_mat) - colSums(win_mat))

  solve(colley_mat, right_hand)
}

#' @rdname colley
#' @export
rank_colley <- function(cr_data,
                        ties = c("average", "first", "last",
                                 "random", "max", "min"),
                        round_digits = 7) {
  round_rank(
    rate_colley(cr_data = cr_data),
    type = "desc", ties = ties, round_digits = round_digits
  )
}
