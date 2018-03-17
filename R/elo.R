#' Elo method
#'
#' Functions to compute rating and ranking using Elo method.
#'
#' @param cr_data Competition results of \link{pairgames} in format ready for
#'   \code{\link[=results-longcr]{as_longcr}}.
#' @param K K-factor for Elo formula.
#' @param ksi Normalization coefficient for Elo formula.
#' @param players Vector of players for which rating is computed.
#' @param initial_ratings Initial ratings (see
#'   \link[=iterative]{Iterative ratings}).
#' @param rating1 Rating of player1 before the game.
#' @param score1 Score of player1 in the game.
#' @param rating2 Rating of player2 before the game.
#' @param score2 Score of player2 in the game.
#' @param ties Value for \code{ties} in \code{\link{round_rank}}.
#' @param round_digits Value for \code{round_digits} in
#'   \code{\link{round_rank}}.
#'
#' @details \code{rate_elo} and \code{add_elo_ratings} are wrappers around
#'   \code{\link{rate_iterative}} and \code{\link{add_iterative_ratings}}
#'   correspondingly. Rate function is based on Elo algorithm of updating
#'   ratings:
#'   \itemize{
#'     \item Probability of player1 (with rating r1) winning against player2
#'       (with rating r2) is computed based on rating difference and sigmoid
#'       function: \code{P = 1 / (1 + 10^( (r2 - r1) / ksi ) )}. \code{ksi}
#'       defines the spread of ratings;
#'     \item Result of the game from player1 perspective is computed based on
#'       rule: \code{S = 1} (if \code{score1} > \code{score2}), \code{S = 0.5}
#'       (if \code{score1} == \code{score2}) and \code{S = 0} (if \code{score1}
#'       < \code{score2});
#'     \item Rating delta is computed: \code{d = K * (S - P)}. The more the
#'       \code{K} the more the delta (with other being equal);
#'     \item New ratings are computed: \code{r1_new = r1 + d},
#'       \code{r2_new = r2 - d}.
#'   }
#'
#' \code{elo} function implements this algorithm. It is vectorized over all its
#'   arguments with standard R recycling functionality. \bold{Note} that not
#'   this function is used in \code{rate_elo} and \code{add_elo_ratings} because
#'   of its not appropriate output format, but rather its non-vectorized
#'   reimplementation is.
#'
#' @return \code{rate_elo} returns a named vector of Elo rating by the end of
#'   competition results.
#'
#'   \code{rank_elo} returns a named vector of \link[=rating-ranking]{ranking}
#'   using \code{\link{round_rank}}.
#'
#'   \code{add_elo_ratings} returns a
#'   \code{\link[=results-widecr]{widecr}} form of \code{cr_data} with four
#'   rating columns added:
#'   \itemize{
#'     \item rating1Before - Rating of player1 before the game;
#'     \item rating2Before - Rating of player2 before the game;
#'     \item rating1After - Rating of player1 after the game;
#'     \item rating2After - Rating of player2 after the game.
#'   }
#'
#'   \code{elo} always returns a matrix with two columns containing ratings
#'   after the game. Rows represent games, columns - players.
#'
#' @references \href{https://en.wikipedia.org/wiki/Elo_rating_system}{Wikipedia
#'   page} for Elo rating system.
#'
#' @examples
#' # Elo ratings
#' rate_elo(ncaa2005)
#' rank_elo(ncaa2005)
#' add_elo_ratings(ncaa2005, initial_ratings = 100)
#'
#' # Elo function
#' elo((0:12)*100, 1, 0, 0)
#' elo((0:12)*100, 1, 0, 0, K = 10)
#' elo((0:12)*10, 1, 0, 0, ksi = 40)
#'
#' @name elo
NULL

#' @rdname elo
#' @export
rate_elo <- function(cr_data, K = 30, ksi = 400,
                     players = NULL, initial_ratings = 0) {
  rate_iterative(
    cr_data = cr_data,
    rate_fun = elo_fgen(K = K, ksi = ksi),
    players = players,
    initial_ratings = initial_ratings
  )
}

#' @rdname elo
#' @export
rank_elo <- function(cr_data, K = 30, ksi = 400,
                     players = NULL, initial_ratings = 0,
                     ties = c("average", "first", "last",
                              "random", "max", "min"),
                     round_digits = 7) {
  round_rank(
    rate_elo(cr_data = cr_data, K = K, ksi = ksi, players = players,
             initial_ratings = initial_ratings),
    type = "desc", ties = ties, round_digits = round_digits
  )
}

#' @rdname elo
#' @export
add_elo_ratings <- function(cr_data, K = 30, ksi = 400,
                            players = NULL, initial_ratings = 0) {
  add_iterative_ratings(
    cr_data = cr_data,
    rate_fun = elo_fgen(K = K, ksi = ksi),
    players = players,
    initial_ratings = initial_ratings
  )
}

#' @rdname elo
#' @export
elo <- function(rating1, score1, rating2, score2, K = 30, ksi = 400) {
  prob_win1 <- 1 / (1 + 10^((rating2 - rating1) / ksi))
  game_res1 <- as.integer(score1 > score2) +
    0.5 * dplyr::near(score1, score2)
  rating_delta <- K * (game_res1 - prob_win1)

  cbind(rating1 + rating_delta, rating2 - rating_delta)
}

elo_fgen <- function(K = 30, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    elo_rate_fun(
      rating1 = rating1, score1 = score1,
      rating2 = rating2, score2 = score2,
      K = K, ksi = ksi
    )
  }
}

elo_rate_fun <- function(rating1, score1, rating2, score2,
                         K = 30, ksi = 400) {
  prob_win1 <- 1 / (1 + 10^((rating2 - rating1) / ksi))
  game_res1 <- as.integer(score1 > score2) +
    0.5 * dplyr::near(score1, score2)
  rating_delta <- K * (game_res1 - prob_win1)

  c(rating1, rating2) + rating_delta * c(1, -1)
}
