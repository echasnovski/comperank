#' Compute item summary
#'
#' This page describes functionality for computing item summary, i.e. some
#' summary measurements (of arbitrary nature) of item (one or more columns)
#' present in competition results.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#' @param item Character vector of columns to group by.
#' @param summary_fun Function to compute item summary (see Details).
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details Argument \code{item} defines on which columns grouping is made for
#'   computing item summary. Basically \code{get_item_summary} applies
#'   \code{summary_fun} to groups of \code{cr_data} defined by \code{item}.
#'
#'   \code{summary_fun} is a function that takes competition results of a
#'   particular item (game, player, their combination, etc.) and produces named
#'   vector of item summary. Also it should take \code{prefix} and \code{...} as
#'   argument for easier use. See \link{item-summary-functions} for more
#'   details.
#'
#'   One can control the names of the summaries by adding prefix stored in
#'   \code{prefix} as extra argument.
#'
#' @return If \code{summary_fun} is \code{NULL} then \code{get_item_summary}
#'   returns a \code{\link[dplyr]{tibble}} with columns named as stored in
#'   argument \code{item} and which has all unique values of particular item
#'   in \code{cr_data}. If not \code{NULL} then there will be extra columns for
#'   every summary value that \code{summary_fun} produces.
#'
#'   \code{get_game_summary} and \code{get_player_summary} are wrappers around
#'   \code{get_item_summary} with \code{item} equals to "game" and "player"
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
#' get_game_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#' get_player_summary(cr_data = cr_data, summary_fun = summary_min_max_score)
#' get_item_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   summary_fun = summary_min_max_score
#' )
#'
#' # Varying prefix of the summary columns.
#' get_item_summary(
#'   cr_data = cr_data, item = c("season", "player"),
#'   summary_fun = summary_mean_sd_score, prefix = "seasonPlayer_"
#' )
#'
#' @seealso \link{item-summary-functions} for implemented \code{summary_fun}s.
#' @seealso \link{item-summary-add} for adding item summary to competition
#'   results.
#'
#' @name item-summary
NULL

#' @rdname item-summary
#' @export
get_item_summary <- function(cr_data, item, summary_fun = NULL, ...) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE)

  if (is.null(summary_fun)) {
    res <- distinct(cr, rlang::UQS(rlang::syms(item)))
  } else {
    res <- cr %>%
      group_by(rlang::UQS(rlang::syms(item))) %>%
      do(dplyr::as_tibble(as.list(summary_fun(.data, ...)))) %>%
      ungroup()
  }

  class(res) <- class(dplyr::tibble())

  res
}

#' @rdname item-summary
#' @export
get_game_summary <- function(cr_data, summary_fun = NULL, ...) {
  get_item_summary(cr_data = cr_data,
                   item = "game",
                   summary_fun = summary_fun,
                   ...)
}

#' @rdname item-summary
#' @export
get_player_summary <- function(cr_data, summary_fun = NULL, ...) {
  get_item_summary(cr_data = cr_data,
                   item = "player",
                   summary_fun = summary_fun,
                   ...)
}
