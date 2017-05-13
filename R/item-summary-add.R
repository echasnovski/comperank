#' Add item summary to competition results
#'
#' This page describes functions for adding item summary as columns to
#' competition results.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param item Character vector of columns to group by.
#' @param summary_fun Function to compute item summary (see Details).
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details If "game" is present in \code{item} but not in column names of
#'   \code{cr_data} (which can occur in case of
#'   \code{\link[=results-widecr]{widecr}} format) then every row is treated as
#'   separate game results. In all other cases every entry of \code{item} should
#'   be present in column names of \code{cr_data}.
#'
#'   As in \link{item-summary} one can control the names of the summaries by
#'   adding prefix stored in \code{prefix} as extra argument.
#'
#' @return \code{add_item_summary} returns \code{cr_data} with added
#'   (by left joining) item summaries.
#'
#'   \code{add_game_summary} and \code{add_player_summary} are wrappers around
#'   \code{add_item_summary} with \code{item} equals to "game" and "player"
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
#' add_game_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#' add_player_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#' add_item_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   summary_fun = summary_min_max_score
#' )
#' add_item_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   summary_fun = summary_min_max_score, prefix = "seasonPlayer_"
#' )
#'
#' # Using without column "game"
#' cr_data <- to_widecr(data.frame(
#'   player1 = 1:10, score1 = 10:19,
#'   player2 = c(2:10, 1), score2 = 11:20
#' ))
#' add_game_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#'
#' @seealso \link{item-summary} for description of computing item summary.
#' @seealso \link{item-summary-functions} for implemented \code{summary_fun}s.
#'
#' @name item-summary-add
NULL

#' @rdname item-summary-add
#' @export
add_item_summary <- function(cr_data, item, summary_fun = NULL, ...) {
  if (is.null(summary_fun)) {
    return(cr_data)
  }

  need_repair_game <- ("game" %in% item) && !("game" %in% colnames(cr_data))
  if (need_repair_game) {
    cr_data$game <- 1:nrow(cr_data)
  }

  item_summary <-
    get_item_summary(cr_data = to_longcr(cr_data),
                     item = item, summary_fun = summary_fun,
                     ...)

  res <- left_join(x = cr_data, y = item_summary, by = item)

  if (need_repair_game) {
    res <- res %>%
      select_(.dots = list(~ -game))
  }

  res
}

#' @rdname item-summary-add
#' @export
add_game_summary <- function(cr_data, summary_fun = NULL, ...) {
  add_item_summary(cr_data = cr_data, item = "game",
                   summary_fun = summary_fun,
                   ...)
}

#' @rdname item-summary-add
#' @export
add_player_summary <- function(cr_data, summary_fun = NULL, ...) {
  add_item_summary(cr_data = cr_data, item = "player",
                   summary_fun = summary_fun,
                   ...)
}
