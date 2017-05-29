#' Helpers for computing Head-to-Head matrices
#'
#' Helpers for computing Head-to-Head matrices.
#'
#' @param h2h_mat Head-to-Head matrix.
#' @param fill Value to fill with.
#' @param x Value to print.
#' @param cr_data Competition results.
#' @param players Vector of players for which Head-to-Head is computed.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @return \code{players_drop} removes from \code{h2h_mat} rows and columns of
#' the players for which either row or column contain only \code{NA} (in
#' most cases it means these players are absent in original competition
#' results).
#'
#' \code{fill_h2h} replaces \code{NA}s in \code{h2h_mat} with \code{fill}.
#'
#' \code{get_cr_players} in case of argument \code{players} is \code{NULL}
#' returns vector of players present in \code{cr_data}. If not \code{NULL} then
#' returns its argument \code{players}.
#'
#' \code{get_cr_matchups} returns a \code{\link[=tbl_df]{tibble}} of all
#' matchups (pairs of players from one game) present in \code{cr_data}. It has
#' following columns:
#' \describe{
#'   \item{game}{Game identifier of matchup}
#'   \item{player1}{Identifier of first player in matchup}
#'   \item{score1}{Score of the first player in matchup}
#'   \item{player2}{Identifier of second player in matchup}
#'   \item{score2}{Score of the second player in matchup}
#' }
#' \bold{Note} that matchups are not symmetrical: matchup "player1"-"player2"
#' is considered different from "player2"-"player1" in order to except more
#' advanced, not symmetrical Head-to-Head values.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 3),
#'   player = rep(1:5, times = 3),
#'   score = 31:45
#' )
#' get_cr_players(cr_data, players = NULL)
#' get_cr_matchups(cr_data)
#'
#' @seealso \link{head-to-head} Abstract computing of Head-to-Head matrices.
#' @seealso \link{head-to-head-functions} Specific functions for computing
#'   Head-to-Head matrices.
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
fill_h2h <- function(h2h_mat, fill = NA_real_, ...) {
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
get_cr_players <- function(cr_data, players = NULL, ...) {
  if (is.null(players)) {
    players <- sort(unique(to_longcr(cr_data, repair = TRUE)$player))
  }

  players
}

#' @rdname head-to-head-helpers
#' @export
get_cr_matchups <- function(cr_data) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE) %>%
    select(.data$game, .data$player, .data$score)

  left_join(x = cr, y = cr, by = "game", suffix = c("1", "2"))
}

