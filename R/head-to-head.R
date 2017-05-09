#' Compute Head-to-Head matrix
#'
#' This page describes methods of computing Head-to-Head matrices.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param h2h_fun Head-to-Head function (see Details).
#' @param score_game Function for scoring game (see
#'   \link[=game-scores]{Computing game scores})
#' @param players Vector of players for which Head-to-Head is computed.
#' @param absent_players Function which performs actions on Head-to-Head matrix
#'   dealing with players which absent in \code{cr_data}.
#' @param absent_h2h Function which performs actions on Head-to-Head matrix
#'   dealing with absent Head-to-Head records for some pairs of players.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details \code{h2h} performs computation of Head-to-Head matrix:
#' square matrix with number of rows (and columns) equal to number of players
#' for which it is computed. Head-to-Head values are computed with these steps:
#' \enumerate{
#'   \item Compute for every game its scores via \code{score_game}. For details
#'     see \link[=game-scores]{Computing game scores};
#'   \item Compute for every present in \code{cr_data} matchup (pair of players)
#'     its Head-to-Head value via \code{h2h_fun} based on these players' scores
#'     in common games and these games' scores. It should accept two arguments:
#'     \itemize{
#'       \item A tibble of \link[=head-to-head-helpers]{matchups} data with
#'         extra columns for game scores. Structure is like
#'         \code{\link[=results-widecr]{widecr}} with two players, i.e. with
#'         columns \code{game}, \code{player1}, \code{score1}, \code{player2},
#'         \code{score2} and other named columns corresponded to game scores of
#'         used games (in input of \code{h2h_fun} columns \code{player1} and
#'         \code{player2} will contain constant values);
#'       \item Argument \code{...} for easier use of \code{h2h}.
#'     }
#'     Also \code{h2h_fun} should return a single numeric value.
#'
#'     \bold{Note} that order of the players in matchups matters. So, for
#'     example, matchup "player1"-"player2" is considered different from
#'     "player2"-"player1" in order to except more advanced, not simmetrical
#'     Head-to-Head values.
#'
#'     For matchups absent in \code{cr_data} \code{NA_real_}s are produced;
#'   \item Perform actions via \code{absent_players} on those players' data
#'     which are absent in \code{cr_data}. For no actions use
#'     \code{\link{skip_action}};
#'   \item Perform actions via \code{absent_h2h} on those entries of
#'     Head-to-Head matrix which matchups didn't occure in \code{cr_data}.
#'     For no actions use \code{\link{skip_action}}.
#' }
#'
#' If \code{score_game} is \code{NULL} then no game scoring is performed.
#'
#' If \code{players} is \code{NULL} then Head-to-Head matrix is computed for
#' all present in \code{cr_data} players. \bold{Note} that argument
#' \code{players} can contain values that are not present in \code{cr_data}:
#' in this case row and column in Head-to-Head matrix for these values will
#' contain only \code{NA}s.
#'
#' @return An object of class "h2h" which is a square matrix of Head-to-Head
#'   values. Rows correspond to \code{player1} and columns to \code{player2}
#'   fields of input data for \code{h2h_fun}. Row and column names are made with
#'   \code{as.character(players)}.
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
#' h2h(
#'   cr_data = cr_data, h2h_fun = mean_score_diff,
#'   score_game = NULL,
#'   players = NULL, absent_players = players_drop,
#'   absent_h2h = fill_h2h
#' )
#'
#' # Compute Head-to-Head matrix with difference in distance
#'  #to the mean score of the game.
#' score_game_mean <- function(cr_data) {
#'   c(meanScore = mean(cr_data$score))
#' }
#' mean_diff_dist_to_meanScore <- function(matchup_data, ...) {
#'   dist1 <- abs(matchup_data$score1 - matchup_data$meanScore)
#'   dist2 <- abs(matchup_data$score2 - matchup_data$meanScore)
#'   # The more the result the better player2 is against player1
#'   mean(dist1 - dist2)
#' }
#' h2h(
#'   cr_data = cr_data, h2h_fun = mean_diff_dist_to_meanScore,
#'   score_game = score_game_mean,
#'   players = NULL, absent_players = players_drop,
#'   absent_h2h = fill_h2h
#' )
#'
#' @seealso \link{head-to-head-helpers} Head-to-Head helpers.
#' @seealso \link{head-to-head-functions} Specific functions for computing.
#' @name head-to-head
NULL

#' @rdname head-to-head
#' @export
h2h <- function(cr_data, h2h_fun, score_game = NULL,
                players = NULL, absent_players = players_drop,
                absent_h2h = fill_h2h, ...) {
  cr <- to_longcr(cr_data, ...)
  players <- get_players(cr_data = cr, players = players, ...)

  game_scores <- get_game_scores(cr_data = cr, score_game = score_game)

  h2h_long <- get_matchups(cr_data = cr) %>%
    left_join(y = game_scores, by = "game") %>%
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

  res
}
