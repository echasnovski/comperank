#' Elo method
#'
#' Functions to compute [rating][rating-ranking] and [ranking][rating-ranking]
#' using Elo method.
#'
#' @inheritParams rate_massey
#' @param K K-factor for Elo formula.
#' @param ksi Normalization coefficient for Elo formula.
#' @param initial_ratings Initial ratings (see [Iterative ratings][iterative]).
#' @param rating1 Rating of player1 before the game.
#' @param score1 Score of player1 in the game.
#' @param rating2 Rating of player2 before the game.
#' @param score2 Score of player2 in the game.
#' @inheritParams rank_massey
#'
#' @details `rate_elo()` and `add_elo_ratings()` are wrappers for
#' [rate_iterative()] and [add_iterative_ratings()] correspondingly. Rate
#' function is based on Elo algorithm of updating ratings:
#'
#' 1. Probability of player1 (with rating r1) winning against player2 (with
#' rating r2) is computed based on rating difference and sigmoid function:
#' `P = 1 / (1 + 10^( (r2 - r1) / ksi ) )`. `ksi` defines the spread of ratings.
#'
#' 1. Result of the game from player1 perspective is computed based on rule:
#' `S = 1` (if `score1` > `score2`), `S = 0.5` (if `score1` == `score2`) and
#' `S = 0` (if `score1` < `score2`).
#'
#' 1. Rating delta is computed: `d = K * (S - P)`. The more the `K` the more the
#' delta (with other being equal).
#'
#' 1. New ratings are computed: `r1_new = r1 + d`, `r2_new = r2 - d`.
#'
#' `elo()` function implements this algorithm. It is vectorized over all its
#' arguments with standard R recycling functionality. __Note__ that not this
#' function is used in `rate_elo()` and `add_elo_ratings()` because of its not
#' appropriate output format, but rather its non-vectorized reimplementation is.
#'
#' Ratings are computed based only on games between players of interest (see
#' Players) and `NA` values.
#'
#' @inheritSection massey Players
#'
#' @return `rate_elo()` returns a [tibble][tibble::tibble] with columns
#' `player` (player identifier) and `rating_elo` (Elo
#' [ratings][rating-ranking], based on row order, by the end of competition
#' results). __Bigger value indicates better player performance__.
#'
#' `rank_elo()` returns a `tibble` with columns `player`, `rating_elo` (if
#' `keep_rating = TRUE`) and `ranking_elo` (Elo [ranking][rating-ranking]
#' computed with [round_rank()]).
#'
#' `add_elo_ratings()` returns a [widecr][comperes::widecr()] form of `cr_data`
#' with four rating columns added:
#' - __rating1Before__ - Rating of player1 before the game.
#' - __rating2Before__ - Rating of player2 before the game.
#' - __rating1After__ - Rating of player1 after the game.
#' - __rating2After__ - Rating of player2 after the game.
#'
#' `elo()` always returns a matrix with two columns containing ratings after the
#' game. Rows represent games, columns - players.
#'
#' @references \href{https://en.wikipedia.org/wiki/Elo_rating_system}{Wikipedia
#'   page} for Elo rating system.
#'
#' @examples
#' # Elo ratings
#' rate_elo(ncaa2005)
#'
#' rank_elo(ncaa2005)
#'
#' rank_elo(ncaa2005, keep_rating = TRUE)
#'
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
rate_elo <- function(cr_data, K = 30, ksi = 400, initial_ratings = 0) {
  rate_iterative(
    cr_data = cr_data,
    rate_fun = elo_fgen(K = K, ksi = ksi),
    initial_ratings = initial_ratings
  ) %>%
    rename(rating_elo = !!rlang::sym("rating_iterative"))
}

#' @rdname elo
#' @export
rank_elo <- function(cr_data, K = 30, ksi = 400, initial_ratings = 0,
                     keep_rating = FALSE,
                     ties = c("average", "first", "last",
                              "random", "max", "min"),
                     round_digits = 7) {
  add_ranking(
    rate_elo(
      cr_data = cr_data, K = K, ksi = ksi,
      initial_ratings = initial_ratings
    ),
    "rating_elo", "ranking_elo",
    keep_rating = keep_rating, type = "desc",
    ties = ties, round_digits = round_digits
  )
}

#' @rdname elo
#' @export
add_elo_ratings <- function(cr_data, K = 30, ksi = 400, initial_ratings = 0) {
  add_iterative_ratings(
    cr_data = cr_data,
    rate_fun = elo_fgen(K = K, ksi = ksi),
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
