# Head-to-Head abstract -------------------------------------------
#' Compute Head-to-Head matrix
#'
#' This page describes methods of computing Head-to-Head matrices.
#'
#' @param cr_data Competition results.
#' @param h2h_fun Head-to-Head function (see Details).
#' @param aggr_fun Aggregation function of Head-to-Head values (see Details).
#' @param score_game Function for scoring game (see
#'   \link[=game-score]{Computing game scores})
#' @param players Vector of players for which Head-to-Head is computed.
#' @param absent_players Function which performs actions on Head-to-Head matrix
#'   dealing with players which absent in \code{cr_data}.
#' @param absent_h2h Function which performs actions on Head-to-Head matrix
#'   dealing with absent Head-to-Head records for some pairs of players.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details \code{h2h_abstract} performs computation of Head-to-Head matrix:
#' square matrix with number of rows (and columns) equal to number of players
#' for which it is computed. Head-to-Head values are computed with these steps:
#' \enumerate{
#'   \item Compute for every game its scores via \code{score_game}. For details
#'     see \link[=game-score]{Computing game scores};
#'   \item Compute for every matchup (pair of players) inside a particular game
#'     its Head-to-Head value via \code{h2h_fun} based on these players scores
#'     and game scores. It should accept arguments \code{score1} (for score of
#'     player1), \code{score2} (for score of player2) and all game scores
#'     produced by \code{score_game}. \bold{Note} that, for example, matchup
#'     "player1"-"player2" is considered different from "player2"-"player1" in
#'     order to except more advanced, not simmetrical Head-to-Head values;
#'   \item Aggregate for every possible matchup of \code{players} all its
#'     present Head-to-Head values via \code{aggr_fun}. If some matchup didn't
#'     occure in \code{cr_data} then \code{NA} is produced;
#'   \item Perform actions via \code{absent_players} those players' data
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
#'   values.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 3),
#'   player = rep(1:5, times = 3),
#'   score = 31:45
#' )
#' # Compute Head-to-Head matrix with mean score difference.
#' score_diff <- function(score1, score2) {
#'   score2 - score1
#' }
#' h2h_abstract(
#'   cr_data = cr_data, h2h_fun = score_diff, aggr_fun = mean,
#'   score_game = NULL, players = NULL, absent_players = players_drop,
#'   absent_h2h = h2h_fill
#' )
#'
#' @seealso \link[=head-to-head-helpers]{Head-to-Head helpers}.
#' @name head-to-head
NULL

#' @rdname head-to-head
#' @export
h2h_abstract <- function(cr_data, h2h_fun,
                         aggr_fun = mean, score_game = NULL,
                         players = NULL, absent_players = players_drop,
                         absent_h2h = h2h_fill, ...) {
  cr <- to_longcr(cr_data, ...)
  players <- get_players(cr_data = cr, players = players, ...)

  game_scores <- get_game_scores(cr_data = cr, score_game = score_game)
  game_scores_names <- setdiff(colnames(game_scores), "game")

  matchups <- get_matchups(cr_data = cr) %>%
    left_join(y = game_scores, by = "game")

  matchups$h2hVal <- do.call(
    mapply,
    c(as.list(matchups[, c("score1", "score2", game_scores_names)]),
      list(FUN = h2h_fun, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    )
  )

  h2h_long <- matchups %>%
    group_by_("player1", "player2") %>%
    summarise_(.dots = list(
      aggrh2hVal = ~ aggr_fun(h2hVal)
    )) %>%
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
  res[as.matrix(h2h_long[, c("player1", "player2")])] <- h2h_long$aggrh2hVal

  res <- res %>%
    absent_players(...) %>%
    absent_h2h(...)

  class(res) <- c("h2h", "matrix")

  res
}

# Head-to-Head helpers ----------------------------------------------------
#' Helpers for computing Head-to-Head matrices
#'
#' @param h2h_mat Head-to-Head matrix.
#' @param fill Value to fill with.
#' @param x Value to print.
#' @param cr_data Competition results.
#' @param players Vector of players for which Head-to-Head is computed.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return \code{players_drop} removes those rows and column of \code{h2h_mat}
#' which are filled with \code{NA}s (and therefore the respective players
#' are absent in original competition results).
#'
#' \code{h2h_fill} replaces \code{NA}s in \code{h2h_mat} with \code{fill}.
#'
#' \code{get_players} in case of argument \code{players} is \code{NULL} returns
#' vector of players present in \code{cr_data}. If not \code{NULL} then returns
#' its argument \code{players}.
#'
#' \code{get_matchups} returns a \code{\link[=tbl_df]{tibble}} of all matchups
#' present in \code{cr_data}. It has following columns:
#' \describe{
#'   \item{game}{Game identifier of matchup}
#'   \item{player1}{Identifier of first player in matchup}
#'   \item{score1}{Score of the first player in matchup}
#'   \item{player2}{Identifier of second player in matchup}
#'   \item{score2}{Score of the second player in matchup}
#' }
#' \bold{Note} that matchups are not symmetrical: matchup "player1"-"player2"
#' is considered different from "player2"-"player1" in order to except more
#' advanced, not simmetrical Head-to-Head values.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 3),
#'   player = rep(1:5, times = 3),
#'   score = 31:45
#' )
#' get_players(cr_data, players = NULL)
#' get_matchups(cr_data)
#'
#' @seealso \link[=head-to-head]{Head-to-Head computing}.
#' @name head-to-head-helpers
NULL

#' @rdname head-to-head-helpers
#' @export
players_drop <- function(h2h_mat, ...) {
  is_present_h2h <- !is.na(h2h_mat)
  present_inds <- (colSums(is_present_h2h) > 0) &
    (rowSums(is_present_h2h) > 0)

  h2h_mat[present_inds, present_inds, drop = FALSE]
}

#' @rdname head-to-head-helpers
#' @export
h2h_fill <- function(h2h_mat, fill = NA_real_, ...) {
  h2h_mat[which(is.na(h2h_mat), arr.ind = TRUE)] <- fill

  h2h_mat
}

#' @rdname head-to-head-helpers
#' @export
print.h2h <- function(x, ...) {
  class(x) <- "matrix"

  print(x)
}

#' @rdname head-to-head-helpers
#' @export
get_players <- function(cr_data, players = NULL, ...) {
  if (is.null(players)) {
    players <- unique(to_longcr(cr_data, repair = TRUE)$player)
  }

  players
}

#' @rdname head-to-head-helpers
#' @export
get_matchups <- function(cr_data) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE) %>%
    select_("game", "player", "score")

  left_join(x = cr, y = cr, by = "game", suffix = c("1", "2"))
}


# Head-to-Head functions --------------------------------------------------
score_diff <- function(score1, score2) {
  score2 - score1
}
