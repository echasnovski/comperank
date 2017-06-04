#' Colley rating
#'
#' Function to compute rating using Colley method.
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
#'   Computation is done based only on the matchups between players in
#'   argument \code{players}. If \code{NULL} then all players present in
#'   \code{cr_data} are used. \bold{Note} that it isn't necessary for all
#'   entries in \code{players} to be present in \code{cr_pair} but it might be
#'   a good idea in order to obtain plausible outcome rating.
#'
#'   The outline of the Colley method is as follows:
#'   \enumerate{
#'     \item Compute Colley matrix: diagonal elements are equal to number of
#'       games played by certain player \emph{plus 2}, off-diagonal are equal to
#'       minus number of common games played. This matrix will be the matrix of
#'       system of linear equations (SLE);
#'     \item Compute right-hand side of SLE as 1 + 0.5*("number of player's
#'       wins" - "number of player's losses");
#'     \item Solve the SLE. The solution is the Colley rating.
#'   }
#'
#' @return Named vector of the Colley rating. The mean rating should be 0.5.
#'
#' @references Wesley N. Colley (2002) \emph{Colley’s Bias Free College Football
#'   Ranking Method: The Colley Matrix Explained}. Available online at
#'   \url{http://www.colleyrankings.com}
#'
#' @examples rate_colley(ncaa2005)
#' rate_colley(ncaa2005, players = c("UNC", "Duke", "Miami", "UVA", "VT"))
#' rate_colley(ncaa2005, players = c("UNC", "Miami", "UVA", "VT"))
#'
#' @export
rate_colley <- function(cr_data, players = NULL) {
  cr <- to_longcr(cr_data, repair = TRUE)
  if (!is_pairgames(cr)) {
    cr <- to_pairgames(cr)
  }

  # Compute Colley ratings
  colley_mat <- - get_h2h(cr, h2h_num, players = players,
                          absent_players = skip_action,
                          absent_h2h = fill_h2h, fill = 0)
  diag(colley_mat) <- 0
  diag(colley_mat) <- 2 - rowSums(colley_mat)

  win_mat <- get_h2h(cr, h2h_num_wins, players = players,
                     absent_players = skip_action,
                     absent_h2h = fill_h2h, fill = 0)
  right_hand <- 1 + 0.5*(colSums(win_mat) - rowSums(win_mat))

  solve(colley_mat, right_hand)
}
