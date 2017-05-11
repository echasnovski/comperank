#' Massey rating
#'
#' Function to compute rating using Massey method.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param players Vector of players for which rating is computed.
#'
#' @details Computation is done based only on the matchups between players in
#'   argument \code{players}. If \code{NULL} then all players present in
#'   \code{cr_data} are used.
#'
#'   The outline of Massey rating method is as follows:
#'   \enumerate{
#'     \item Compute Massey matrix: diagonal elements are equal to number of
#'       games played by certain player, off-diagonal are equal to minus number
#'       of common games played. In other words it is
#'       \link[=head-to-head]{Head-to-Head matrix} using \code{h2h_num} as
#'       \code{\link[=head-to-head-functions]{h2h_fun}} with off-diagonal
#'       elements switched sign. This matrix will be the matrix of system of
#'       linear equations (SLE);
#'     \item Compute right-hand side of SLE as cumulative score differences of
#'       players, i.e. sum of all scores \emph{for} the player minus sum of all
#'       scores \emph{against} a player;
#'     \item Make adjustment for solvability of SLE. Modify the last row of
#'       Massey matrix so that all its cells are equal to 1. Also change the
#'       last cell in right-hand side to 0. This adjustment ensures that sum of
#'       all outcome ratings will be 0;
#'     \item Solve the SLE. The solution is the Massey rating.
#'   }
#'
#' @return Named vector of the Massey rating.
#'
#' @examples rate_massey(ncaa2005)
#' rate_massey(ncaa2005, players = c("UNC", "Duke", "Miami", "UVA", "VT"))
#' rate_massey(ncaa2005, players = c("UNC", "Miami", "UVA", "VT"))
#'
#' @export
rate_massey <- function(cr_data, players = NULL) {
  cr <- to_longcr(cr_data, repair = TRUE)

  massey_mat <- - get_h2h(cr_data = cr, h2h_fun = h2h_num,
                          players = players, absent_players = skip_action,
                          absent_h2h = fill_h2h, fill = 0)
  diag(massey_mat) <- - diag(massey_mat)

  sum_score_mat <- get_h2h(cr_data = cr, h2h_fun = h2h_sum_score,
                           players = players, absent_players = skip_action,
                           absent_h2h = fill_h2h, fill = 0)
  score_for <- diag(sum_score_mat)
  score_against <- rowSums(sum_score_mat) - score_for
  score_diff <- score_for - score_against

  massey_mat_mod <- massey_mat
  massey_mat_mod[nrow(massey_mat_mod), ] <- 1
  score_diff_mod <- score_diff
  score_diff_mod[length(score_diff_mod)] <- 0

  solve(massey_mat_mod, score_diff_mod)
}
