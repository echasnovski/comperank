#' Compute Head-to-Head matrix
#'
#' This page describes methods of computing Head-to-Head matrices.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param h2h_fun Head-to-Head function (see Details).
#' @param players Vector of players for which Head-to-Head is computed.
#' @param absent_players Function which performs actions on Head-to-Head matrix
#'   dealing with players which absent in \code{cr_data}.
#' @param absent_h2h Function which performs actions on Head-to-Head matrix
#'   dealing with absent Head-to-Head records for some pairs of players.
#' @param transpose Whether to transpose Head-to-Head matrix.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details Head-to-Head value is a measure of a quality of direct
#' confrontation between two players. It is assumed that this value can be
#' computed based only on the players' scores in their common games. If it is
#' not true for some case then competition results should be changed by
#' transformation or addition of more information (in form of extra columns or
#' extra field in \code{score} column(s) making list-column(s)).
#'
#' \code{get_h2h} performs computation of Head-to-Head matrix: square matrix
#' with number of rows (and columns) equal to number of players for which it is
#' computed. Head-to-Head values are computed based only on
#' \link[=head-to-head-helpers]{matchups} (pairs of players from one game)
#' between players from argument \code{players}. \bold{Note} to be careful with
#' Head-to-Head values of players with themselves: it can be inaccurate if
#' \code{players} is not \code{NULL} because it will be based on possibly
#' undesirable data.
#'
#' The following algorithm is used:
#' \enumerate{
#'   \item Compute for every present in \code{cr_data} matchup
#'     between players from \code{players} its Head-to-Head value via
#'     \code{h2h_fun} based on these players' scores in common games.
#'     \code{h2h_fun} should accept two arguments:
#'     \itemize{
#'       \item A tibble of \link[=head-to-head-helpers]{matchups} data.
#'         Structure is like \code{\link[=results-widecr]{widecr}} with
#'         two players, i.e. with columns \code{game}, \code{player1},
#'         \code{score1}, \code{player2}, \code{score2} (in input of
#'         \code{h2h_fun} columns \code{player1} and \code{player2} will
#'         contain constant values);
#'       \item Argument \code{...} for easier use of \code{h2h}.
#'     }
#'     Also \code{h2h_fun} should return a single value.
#'
#'     \bold{Note} that order of the players in matchups matters. So, for
#'     example, matchup "player1"-"player2" is considered different from
#'     "player2"-"player1" in order to except not symmetrical
#'     Head-to-Head values.
#'
#'     For absent in \code{cr_data} matchups \code{NA_real_}s are produced;
#'
#'   \item Perform actions via \code{absent_players}. It should, based on
#'     Head-to-Head matrix, do something with data of players that have not
#'     enough games played. For no actions use \code{\link{skip_action}}.
#'     For other options see \link{head-to-head-helpers};
#'   \item Perform actions via \code{absent_h2h}. It should do something with
#'     those entries of Head-to-Head matrix which are \code{NA}. For no actions
#'     use \code{\link{skip_action}}. For other options see
#'     \link{head-to-head-helpers};
#'   \item If \code{transpose} is \code{TRUE} do transposition of Head-to-Head
#'     matrix. This option is added to minimize the need in almost duplicated
#'     \code{h2h_fun}s.
#' }
#'
#' If argument \code{players} is \code{NULL} then Head-to-Head matrix is
#' computed for all present in \code{cr_data} players. \bold{Note} that
#' \code{players} can contain values that are not present in \code{cr_data}: in
#' this case rows and columns in Head-to-Head matrix for these values will
#' contain only \code{NA}s (before applying \code{absent_players} function).
#'
#' @return An object of class \code{h2h} which is a square matrix of
#'   Head-to-Head values. Rows correspond to \code{player1} and columns to
#'   \code{player2} (as in input for \code{h2h_fun}). Row and column
#'   names are made with \code{as.character()} on players used for computation.
#'
#' @examples
#' set.seed(1002)
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 3),
#'   player = rep(1:5, times = 3),
#'   score = rnorm(15)
#' )
#'
#' # Compute Head-to-Head matrix with mean score difference.
#' mean_score_diff <- function(matchup_data, ...) {
#'   mean(matchup_data$score2 - matchup_data$score1)
#' }
#' get_h2h(
#'   cr_data = cr_data, h2h_fun = mean_score_diff,
#'   players = NULL, absent_players = players_drop,
#'   absent_h2h = fill_h2h
#' )
#'
#' @seealso \link{head-to-head-helpers} Head-to-Head helpers.
#' @seealso \link{head-to-head-functions} Specific functions for computing
#'   Head-to-Head matrices.
#' @name head-to-head
NULL

#' @rdname head-to-head
#' @export
get_h2h <- function(cr_data, h2h_fun, players = NULL,
                    absent_players = players_drop, absent_h2h = fill_h2h,
                    transpose = FALSE,
                    ...) {
  cr <- to_longcr(cr_data, ...)
  players <- get_cr_players(cr_data = cr, players = players, ...)

  h2h_long <- get_cr_matchups(cr_data = cr) %>%
    filter_(.dots = list(
      ~ player1 %in% players, ~ player2 %in% players
    )) %>%
    group_by_("player1", "player2") %>%
    do_(~ data.frame(h2hVal = h2h_fun(., ...))) %>%
    ungroup() %>%
    mutate_(.dots = list(
      player1 = ~ factor(player1, levels = players),
      player2 = ~ factor(player2, levels = players)
    )) %>%
    complete_(cols = c("player1", "player2")) %>%
    mutate_(.dots = list(
      player1 = ~ as.character(player1),
      player2 = ~ as.character(player2)
    ))

  players <- as.character(players)
  res <- matrix(NA_real_, nrow = length(players), ncol = length(players),
                dimnames = list(players, players))
  res[as.matrix(h2h_long[, c("player1", "player2")])] <- h2h_long$h2hVal

  res <- res %>%
    absent_players(...) %>%
    absent_h2h(...)

  class(res) <- c("h2h", "matrix")

  if (transpose) {
    t(res)
  } else {
    res
  }
}
