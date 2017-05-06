#' Compute game scores
#'
#' This page describes functionality for computing game scores, i.e. some
#' measurements (of arbitrary nature) of the game itself.
#'
#' @param cr_data Competition results.
#' @param score_game Function to compute game scores (see Details)
#'
#' @details \code{score_game} is a function that takes results of a
#' game in \code{\link[=results-longcr]{longcr}} form and produces named
#' vector of game scores.
#'
#' @return If \code{score_game} is \code{NULL} then \code{get_game_scores}
#' returns \code{\link[=tbl_df]{tibble}} with one column \code{game}
#' which has all games in \code{cr_data}. If not \code{NULL} then there will be
#' extra columns for every game score that \code{score_game} produces
#' (names are preserved).
#'
#' \code{score_game_mean_sd} returns a named vector: "meanScore" - mean score
#' of the game; "sdScore" - standard deviation of scores in the game.
#'
#' @examples
#' cr_data <- data.frame(
#'   player = rep(1:5, times = 2),
#'   game = rep(1:5, each = 2),
#'   score = 31:40
#' )
#'
#' get_game_scores(cr_data = cr_data, score_game = score_game_mean_sd)
#'
#' @name game-score
NULL

#' @rdname game-score
#' @export
get_game_scores <- function(cr_data, score_game = NULL) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE) %>%
    select_("game", "player", "score")

  if (is.null(score_game)) {
    distinct_(cr, "game")
  } else {
    cr %>%
      group_by_("game") %>%
      do_({
        ~ tbl_df(as.list(score_game(.)))
      }) %>%
      ungroup()
  }
}

#' @rdname game-score
#' @export
score_game_mean_sd <- function(cr_data) {
  c(meanScore = mean(cr_data$score), sdScore = sd(cr_data$score))
}
