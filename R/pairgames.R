#' Convert competition results into pairgames
#'
#' Function to convert competition results into games between two players.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#'
#' @details \code{to_pairgames} is a function that converts competition results
#'   into pairwise games: it drops games with one player and for every game with
#'   3 and more players this function transforms it into set of separate games
#'   between unordered pairs of players. In other words the result is a set of
#'   unordered \link[=head-to-head-helpers]{matchups} (as different games)
#'   between different players.
#'
#'   \bold{Note} that order in which players are assigned to \code{player1} or
#'   \code{player2} column in general shouldn't agree with any order in
#'   \code{cr_data}.
#'
#'   \bold{Note} that any column except \code{game}, \code{player} and
#'   \code{score} will be dropped after convertion to
#'   \code{\link[=results-longcr]{longcr}}.
#'
#'   \bold{Note} that in order for this function to work column \code{player}
#'   after convertion to \code{\link[=results-longcr]{longcr}} should be
#'   comparable, i.e. function '<' can be used properly on its values (which is
#'   true in most cases).
#'
#' @return A competition results of pairwised games as
#'   \link[=results-widecr]{widecr} object with two players.
#'
#' @examples
#' cr_data <- data.frame(
#'   game = c(rep(1:5, each = 3), 6),
#'   player = c(rep(1:5, times = 3), 1),
#'   score = 101:116,
#'   extraCol = -(1:16)
#' )
#'
#' to_pairgames(cr_data)
#' @aliases pairgames
#' @export
to_pairgames <- function(cr_data) {
  cr_data %>%
    get_cr_matchups() %>%
    group_by_("game") %>%
    filter_(.dots = list(
      ~ player1 < player2
    )) %>%
    ungroup() %>%
    mutate_(.dots = list(
      game = ~ 1:n()
    )) %>%
    to_widecr(repair = FALSE)
}
