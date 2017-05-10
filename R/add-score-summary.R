#' Add score summary to competition results
#'
#' This page describes function for adding score summary to competition results
#' as columns.
#'
#' @param cr_data Competition results.
#' @param item Character vector of columns to group by.
#' @param score_fun Function to compute score summary (see Details).
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details \code{cr_data} should be in format suitable for
#'   \code{\link[=results-longcr]{to_longcr}}.
#'
#'   If "game" is present in \code{item} but not in column names of
#'   \code{cr_data} (which can occur in case of
#'   \code{\link[=results-widecr]{widecr}} format) then every row is treated as
#'   separate game results. In all other cases every entry of \code{item} should
#'   be present in column names of \code{cr_data}.
#'
#'   As in \link{score-summary} one can control the names of the summaries by
#'   adding prefix stored in \code{prefix} as extra argument.
#'
#' @return \code{add_score_summary} returns \code{cr_data} with added
#'   (by left joining) score summaries.
#'
#'   \code{add_game_summary} and \code{add_player_summary} are wrappers around
#'   \code{add_score_summary} with \code{item} equals to "game" and "player"
#'   accordingly.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:20, each = 2),
#'   player = rep(1:10, times = 4),
#'   score = 31:70,
#'   season = rep(1:2, each = 20)
#' )
#'
#' # Computing summaries.
#' add_game_summary(cr_data = cr_data, score_fun = score_min_max)
#' add_player_summary(cr_data = cr_data, score_fun = score_min_max)
#' add_score_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   score_fun = score_min_max
#' )
#' add_score_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   score_fun = score_min_max, prefix = "seasonPlayer_"
#' )
#'
#' # Using without column "game"
#' cr_data <- to_widecr(data.frame(
#'   player1 = 1:10, score1 = 10:19,
#'   player2 = c(2:10, 1), score2 = 11:20
#' ))
#' add_game_summary(cr_data = cr_data, score_fun = score_min_max)
#'
#' @name add-score-summary
NULL

#' @rdname add-score-summary
#' @export
add_score_summary <- function(cr_data, item, score_fun = NULL, ...) {
  if (is.null(score_fun)) {
    return(cr_data)
  }

  need_repair_game <- ("game" %in% item) && !("game" %in% colnames(cr_data))
  if (need_repair_game) {
    cr_data$game <- 1:nrow(cr_data)
  }

  score_summary <-
    get_score_summary(cr_data = to_longcr(cr_data),
                      item = item, score_fun = score_fun,
                      ...)

  res <- left_join(x = cr_data, y = score_summary, by = item)

  if (need_repair_game) {
    res <- res %>%
      select_(.dots = list(~ -game))
  }

  res
}

#' @rdname add-score-summary
#' @export
add_game_summary <- function(cr_data, score_fun = NULL, ...) {
  add_score_summary(cr_data = cr_data, item = "game", score_fun = score_fun,
                    ...)
}

#' @rdname add-score-summary
#' @export
add_player_summary <- function(cr_data, score_fun = NULL, ...) {
  add_score_summary(cr_data = cr_data, item = "player", score_fun = score_fun,
                    ...)
}
