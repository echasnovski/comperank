#' Massey rating
#'
#' Function to compute rating using Massey method.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param players Vector of players for which rating is computed.
#'
#' @details This rating method was initially designed for games between two
#'   players. That is why competition results are transformed into pairwise
#'   games \code{cr_pair} via \link{to_pairgames}. Data from \code{cr_pair} is
#'   used in rating computation.
#'
#'   It is assumed that score is numeric and higher values are better for
#'   the player.
#'
#'   Computation is done based only on the games between players in argument
#'   \code{players}. If \code{NULL} then all players present in \code{cr_pair}
#'   are used. \bold{Note} that all \code{players} should be present in
#'   \code{cr_pair} because otherwise there will be an error during solving
#'   linear system described below. Message is given if there are players absent
#'   in \code{cr_pair}.
#'
#'   The outline of Massey rating method is as follows:
#'   \enumerate{
#'     \item Compute Massey matrix: diagonal elements are equal to number of
#'       games played by certain player, off-diagonal are equal to minus number
#'       of common games played. This matrix will be the matrix of system of
#'       linear equations (SLE);
#'     \item Compute right-hand side of SLE as cumulative score differences of
#'       players, i.e. sum of all scores \emph{for} the player minus sum of all
#'       scores \emph{against} the player;
#'     \item Make adjustment for solvability of SLE. Modify the last row of
#'       Massey matrix so that all its cells are equal to 1. Also change the
#'       last cell in right-hand side to 0. This adjustment ensures that sum of
#'       all outcome ratings will be 0;
#'     \item Solve the SLE. The solution is the Massey rating.
#'   }
#'
#' @return Named vector of the Massey rating. The sum of all ratings should be
#'   equal to 0.
#'
#' @references Kenneth Massey (1997) \emph{Statistical models applied to the
#'   rating of sports teams}. Bachelor’s thesis, Bluefield College.
#'
#' @examples rate_massey(ncaa2005)
#' rate_massey(ncaa2005, players = c("UNC", "Duke", "Miami", "UVA", "VT"))
#' rate_massey(ncaa2005, players = c("UNC", "Miami", "UVA", "VT"))
#'
#' @export
rate_massey <- function(cr_data, players = NULL) {
  cr <- to_longcr(cr_data, repair = TRUE) %>%
    to_pairgames()

  # Assert used players
  original_players <- get_cr_players(cr, players = NULL)
  assert_used_objects(used = players, original = original_players,
                      prefix = "rate_massey: ", object_name = "players",
                      data_name = "competition results")

  # Compute Massey ratings
  massey_mat <- - get_h2h(cr_data = cr, h2h_fun = h2h_num,
                          players = players, absent_players = skip_action,
                          absent_h2h = fill_h2h, fill = 0)
  diag(massey_mat) <- 0
  diag(massey_mat) <- - rowSums(massey_mat)

  sum_score_mat <- get_h2h(cr_data = cr, h2h_fun = h2h_sum_score,
                           players = players, absent_players = skip_action,
                           absent_h2h = fill_h2h, fill = 0)
  diag(sum_score_mat) <- 0

  score_for <- colSums(sum_score_mat)
  score_against <- rowSums(sum_score_mat)
  score_diff <- score_for - score_against

  massey_mat_mod <- massey_mat
  massey_mat_mod[nrow(massey_mat_mod), ] <- 1
  score_diff_mod <- score_diff
  score_diff_mod[length(score_diff_mod)] <- 0

  solve(massey_mat_mod, score_diff_mod)
}
