#' Massey method
#'
#' Functions to compute rating and ranking using Massey method.
#'
#' @param cr_data Competition results in format ready for
#'   [as_longcr()][comperes::as_longcr()].
#' @param ties Value for `ties` in [round_rank()].
#' @param round_digits Value for `round_digits` in [round_rank()].
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
#' Players). __Note__ that all those players should be present in `cr_pair`
#' because otherwise there will be an error during solving linear system
#' described below. Message is given if there are players absent in `cr_pair`.
#'
#' The outline of Massey rating method is as follows:
#'
#' 1. Compute Massey matrix: diagonal elements are equal to number of games
#' played by certain player, off-diagonal are equal to minus number of common
#' games played. This matrix will be the matrix of system of linear equations
#' (SLE).
#'
#' 1. Compute right-hand side of SLE as cumulative score differences of players,
#' i.e. sum of all scores _for_ the player minus sum of all scores _against_ the
#' player.
#'
#' 1. Make adjustment for solvability of SLE. Modify the last row of Massey
#' matrix so that all its cells are equal to 1. Also change the last cell in
#' right-hand side to 0. This adjustment ensures that sum of all outcome ratings
#' will be 0.
#'
#' 1. Solve the SLE. The solution is the Massey rating.
#'
#' @section Players:
#'
#' `comperank` offers a possibility to handle certain set of players. It is done
#' by having `player` column (in [longcr][comperes::as_longcr] format) as factor
#' with levels specifying all players of interest. In case of factor the result
#' is returned only for players from its levels. Otherwise - for all present
#' players.
#'
#' @return `rate_massey()` returns a named vector of the Massey rating. The
#' sum of all ratings should be equal to 0.
#'
#' `rank_massey()` returns a named vector of [ranking][rating-ranking] using
#' [round_rank()].
#'
#' @references Kenneth Massey (1997) *Statistical models applied to the
#'   rating of sports teams*. Bachelor’s thesis, Bluefield College.
#'
#' @examples
#' rate_massey(ncaa2005)
#' rank_massey(ncaa2005)
#'
#' @name massey
NULL

#' @rdname massey
#' @export
rate_massey <- function(cr_data) {
  cr <- as_longcr(cr_data, repair = TRUE)
  if (!is_pairgames(cr)) {
    cr <- as_longcr(to_pairgames(cr))
  }

  # Assert used players
  players <- levels2(cr$player)
  original_players <- unique(cr$player)
  assert_used_objects(used = players, original = original_players,
                      prefix = "rate_massey: ", object_name = "players",
                      data_name = "competition results")

  # Compute Massey ratings
  massey_mat <- - h2h_mat(cr, !!h2h_funs[["num"]], fill = 0)
  diag(massey_mat) <- 0
  diag(massey_mat) <- - rowSums(massey_mat)

  sum_score_mat <- h2h_mat(cr, !!h2h_funs[["sum_score"]], fill = 0)
  diag(sum_score_mat) <- 0

  score_for <- rowSums(sum_score_mat)
  score_against <- colSums(sum_score_mat)
  score_diff <- score_for - score_against

  massey_mat_mod <- massey_mat
  massey_mat_mod[nrow(massey_mat_mod), ] <- 1
  score_diff_mod <- score_diff
  score_diff_mod[length(score_diff_mod)] <- 0

  solve(massey_mat_mod, score_diff_mod)
}

#' @rdname massey
#' @export
rank_massey <- function(cr_data,
                        ties = c("average", "first", "last",
                                 "random", "max", "min"),
                        round_digits = 7) {
  round_rank(
    rate_massey(cr_data = cr_data),
    type = "desc", ties = ties, round_digits = round_digits
  )
}
