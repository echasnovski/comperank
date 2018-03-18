#' Iterative rating method
#'
#' Functions to compute iterative numeric ratings, i.e. which are recomputed
#' after every game, and corresponding rankings.
#'
#' @inheritParams rate_massey
#' @param rate_fun Rating function (see Details).
#' @param initial_ratings Initial ratings (see Details).
#' @param type Value for `type` in [round_rank()].
#' @inheritParams rank_massey
#'
#' @details Iterative ratings of group of players are recomputed after every
#' game based on players' game scores and their ratings just before the game.
#' Theoretically this kind of ratings can be non-numeric and be computed on
#' competition results with variable number of players but they rarely do. This
#' package provides functions for computing iterative __numeric__ ratings for
#' [pairgames][comperes::to_pairgames] (competition results with games only
#' between two players). Error is thrown if `cr_data` is not pairgames.
#'
#' Games in [widecr][comperes::widecr()] form are arranged in increasing order
#' of values in column `game` (if it is present) and processed from first to
#' last row.
#'
#' `NA` values in column `player` are allowed. These players are treated as
#' 'ghosts': players of the same rating as opponent before the game. 'Ghosts'
#' are not actual players so they don't appear in the output of
#' `rate_iterative()`. For games between two 'ghosts' ratings before and after
#' the game are set to 0.
#'
#' The core of the rating system is `rate_fun`. It should take the
#' following arguments:
#' - __rating1__ - Rating of player1 before the game.
#' - __score1__ - Score of player1 in the game.
#' - __rating2__ - Rating of player2 before the game.
#' - __score2__ - Score of player2 in the game.
#'
#' `rate_fun` should return a numeric vector of length 2: first element being a
#' rating of player1 after the game, second - of player2.
#'
#' Ratings are computed based only on games between players of interest (see
#' Players) and `NA` values.
#'
#' Initial ratings should be defined with argument `initial_ratings`. It
#' can be:
#' - A single numeric value. In this case initial ratings for all players are
#' set to this value.
#' - A named vector of ratings. All non-`NA` players, for which rating is
#' computed, should be present in its names.
#'
#' @inheritSection massey Players
#'
#' @return `rate_iterative()` returns a named vector of iterative ratings by
#' the end of competition results (based on row order).
#'
#' `rank_iterative()` returns a named vector of [ranking][rating-ranking] using
#' [round_rank()] based on specified `type`.
#'
#' `add_iterative_ratings()` returns a [widecr][comperes::widecr()] form of
#' `cr_data` with four rating columns added:
#' - __rating1Before__ - Rating of player1 before the game.
#' - __rating2Before__ - Rating of player2 before the game.
#' - __rating1After__ - Rating of player1 after the game.
#' - __rating2After__ - Rating of player2 after the game.
#'
#' @examples
#' test_rate_fun <- function(rating1, score1, rating2, score2) {
#'   c(rating1, rating2) + ((score1 >= score2) * 2 - 1) * c(1, -1)
#' }
#' set.seed(1002)
#' cr_data <- data.frame(
#'   game = rep(1:10, each = 2),
#'   player = rep(1:5, times = 4),
#'   score = runif(20)
#' )
#' cr_data$player[c(6, 8)] <- NA
#'
#' # Different settings of add_iterative_ratings
#' add_iterative_ratings(cr_data, test_rate_fun)
#'
#' add_iterative_ratings(cr_data, test_rate_fun, initial_ratings = 10)
#'
#' add_iterative_ratings(
#'   cr_data, test_rate_fun,
#'   initial_ratings = c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5)
#' )
#'
#' # Ratings and ranking at the end of competition results.
#' rate_iterative(cr_data, test_rate_fun)
#'
#' rank_iterative(cr_data, test_rate_fun, type = "desc")
#'
#' @name iterative
NULL

#' @rdname iterative
#' @export
rate_iterative <- function(cr_data, rate_fun, initial_ratings = 0) {
  cr_with_ratings <- add_iterative_ratings(
    cr_data = cr_data, rate_fun = rate_fun,
    initial_ratings = initial_ratings
  )

  # Add initial ratings to result in case player didn't play any game
  used_players <- levels2(as_longcr(cr_data)$player)
  ref_players <- used_players[!is.na(used_players)]
  init_ratings <- tibble(
    player = ref_players,
    rating = get_cr_initial_ratings(ref_players, initial_ratings)
  )

  res <- bind_rows(init_ratings, get_ratings_after(cr_with_ratings)) %>%
    filter(.data$player %in% ref_players) %>%
    group_by(.data$player) %>%
    slice(n()) %>%
    ungroup() %>%
    to_rating_vec()

  res[ref_players]
}

#' @rdname iterative
#' @export
rank_iterative <- function(cr_data, rate_fun,
                           initial_ratings = 0,
                           type = "desc",
                           ties = c("average", "first", "last",
                                    "random", "max", "min"),
                           round_digits = 7) {
  round_rank(
    rate_iterative(
      cr_data = cr_data, rate_fun = rate_fun,
      initial_ratings = initial_ratings
    ),
    type = type, ties = ties, round_digits = round_digits
  )
}

#' @rdname iterative
#' @export
add_iterative_ratings <-
  function(cr_data, rate_fun, initial_ratings = 0) {
  if (!is_pairgames(cr_data)) {
    stop("cr_data is not pairgames.")
  }

  used_players <- levels2(as_longcr(cr_data)$player)
  ref_players <- used_players[!is.na(used_players)]

  cr <- cr_data %>%
    as_longcr(repair = TRUE) %>%
    as_widecr(repair = FALSE) %>%
    filter(
      (.data$player1 %in% used_players) | is.na(.data$player1),
      (.data$player2 %in% used_players) | is.na(.data$player2)
    ) %>%
    mutate(
      .player1_id = to_players_id(.data$player1, ref_players),
      .player2_id = to_players_id(.data$player2, ref_players)
    )

  init_ratings <- get_cr_initial_ratings(
    players = ref_players,
    initial_ratings
  )

  ratings <- compute_iterative_ratings(
    rate_fun = rate_fun,
    player1_id = cr[[".player1_id"]], score1 = cr$score1,
    player2_id = cr[[".player2_id"]], score2 = cr$score2,
    initial_ratings = init_ratings
  ) %>%
    as.data.frame()

  cr %>%
    bind_cols(y = ratings) %>%
    select(-.data[[".player1_id"]], -.data[[".player2_id"]]) %>%
    as_widecr(repair = FALSE)
}

to_players_id <- function(players, ref_players) {
  res <- players %>%
    factor(levels = ref_players) %>%
    as.integer()

  if_else(is.na(res), 0L, res)
}

get_cr_initial_ratings <- function(players, initial_ratings = 0) {
  if (!is.numeric(initial_ratings)) {
    stop("initial_ratings should be a numeric vector.")
  }

  if (length(initial_ratings) == 1) {
    return(rep(initial_ratings, length(players)))
  }

  players_chr <- as.character(players)
  players_chr <- players_chr[!is.na(players_chr)]
  absent_players <- setdiff(players_chr, names(initial_ratings))
  if (length(absent_players) > 0) {
    stop("There are missing ratings for some players:\n  ",
         paste0(absent_players, collapse = ", "))
  }

  res <- initial_ratings[players_chr]
  names(res) <- NULL

  res
}

get_ratings_after <- function(cr_with_ratings) {
  ratings_wide <- cr_with_ratings %>%
    mutate(..game = 1:n())

  bind_rows(
    ratings_wide %>%
      transmute(
        game = .data[["..game"]],
        player = as.character(.data$player1),
        rating = .data$rating1After
      ),
    ratings_wide %>%
      transmute(
        game = .data[["..game"]],
        player = as.character(.data$player2),
        rating = .data$rating2After
      )
  ) %>%
    arrange(.data$game) %>%
    select(-.data$game)
}
