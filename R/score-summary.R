#' Compute score summary
#'
#' This page describes functionality for computing score summary, i.e. some
#' summary measurements (of arbitrary nature) of items present in competition
#' results.
#'
#' @param cr_data Competition results.
#' @param item Character vector of columns to group by.
#' @param score_fun Function to compute score summary (see Details).
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details Argument \code{item} defines on which columns grouping is made for
#'   computing score summary. Basically \code{get_score_summary} performes a
#'   grouped apply of \code{score_fun} to \code{cr_data}.
#'
#'   \code{score_fun} is a function that takes competition results of a
#'   particular item(s) (game, player, etc.) convertable to
#'   \code{\link[=results-longcr]{longcr}} form via
#'   \code{\link[=results-longcr]{to_longcr}} and produces named vector of score
#'   summary. Also it should take \code{prefix} and \code{...} as argument for
#'   easier use. See \link{score-summary-functions} for more details.
#'
#'   One can control the names of the summaries by adding prefix stored in
#'   \code{prefix} as extra argument.
#'
#' @return If \code{score_fun} is \code{NULL} then \code{get_score_summary}
#'   returns a \code{\link[=tbl_df]{tibble}} with columns named as stored in
#'   argument \code{item} and which has all unique values of particular item(s)
#'   in \code{cr_data}. If not \code{NULL} then there will be extra columns for
#'   every summary value that \code{score_fun} produces (names are preserved).
#'
#'   \code{get_game_summary} and \code{get_player_summary} are wrappers around
#'   \code{get_score_summary} with \code{item} equals to "game" and "player"
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
#' get_game_summary(cr_data = cr_data, score_fun = score_min_max)
#' get_player_summary(cr_data = cr_data, score_fun = score_min_max)
#' get_score_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   score_fun = score_min_max
#' )
#'
#' # Varying prefix of the summary columns.
#' get_score_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   score_fun = score_mean_sd, prefix = "seasonPlayer_"
#' )
#'
#' @name score-summary
NULL

#' @rdname score-summary
#' @export
get_score_summary <- function(cr_data, item, score_fun = NULL, ...) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE)

  if (is.null(score_fun)) {
    res <- distinct_(cr, .dots = as.list(item))
  } else {
    res <- cr %>%
      group_by_(.dots = as.list(item)) %>%
      do_({
        ~ tbl_df(as.list(score_fun(., ...)))
      }) %>%
      ungroup()
  }

  class(res) <- class(tbl_df(data.frame()))

  res
}

#' @rdname score-summary
#' @export
get_game_summary <- function(cr_data, score_fun = NULL, ...) {
  get_score_summary(cr_data = cr_data,
                    item = "game",
                    score_fun = score_fun,
                    ...)
}

#' @rdname score-summary
#' @export
get_player_summary <- function(cr_data, score_fun = NULL, ...) {
  get_score_summary(cr_data = cr_data,
                    item = "player",
                    score_fun = score_fun,
                    ...)
}
