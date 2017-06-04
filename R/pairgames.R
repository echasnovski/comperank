#' Сompetition results with games between two players
#'
#' Functions for competition results with games between two players.
#'
#' @param cr_data Competition results in format ready for
#'   \code{\link[=results-longcr]{to_longcr}}.
#'
#' @details Pairgames is a term for competition results with games between two
#'   players.
#'
#'   \code{to_pairgames} is a function that converts competition results
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
#'   \code{score} will be dropped after conversion to
#'   \code{\link[=results-longcr]{longcr}}.
#'
#'   \bold{Note} that \code{NA} and \code{NaN} in \code{players} are allowed.
#'   They are treated as different players.
#'
#'   \bold{Note} that \code{to_pairgames} is rather compute-intensive and can
#'   take much time for competition results with many games.
#'
#' @return \code{to_pairgames} returns a competition results of pairwised games
#'   as \link[=results-widecr]{widecr} object with two players.
#'
#'   \code{is_pairgames} returns a boolean value of whether \code{cr_data}
#'   contains only games between two players.
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
#'
#' # Missing values
#' cr_data_na <- data.frame(
#'   game = rep(1L, 3),
#'   player = c(1, NA, NA),
#'   score = 1:3
#' )
#' to_pairgames(cr_data_na)
#'
#' # Checks
#' is_pairgames(cr_data)
#' is_pairgames(to_pairgames(cr_data))
#'
#' @name pairgames
NULL

#' @rdname pairgames
#' @export
to_pairgames <- function(cr_data) {
  cr <- cr_data %>%
    to_longcr(repair = TRUE)

  multiple_players_games <- cr %>%
    count(.data$game) %>%
    filter(.data$n > 1)

  # In raw pairgames game identifier is formed from 'game' and '..subGame'
  raw_pairgames <- cr %>%
    semi_join(y = multiple_players_games, by = "game") %>%
    group_by(.data$game) %>%
    do({
      cr_pairs <- utils::combn(nrow(.data), 2)

      .data %>%
        slice(c(cr_pairs)) %>%
        mutate(..subGame = rep(1:ncol(cr_pairs), each = 2))
    }) %>%
    ungroup()

  # Compute new game identifiers
  pairgames_ids <- raw_pairgames %>%
    distinct(.data$game, .data[["..subGame"]]) %>%
    mutate(..pairgameId = 1:n())

  raw_pairgames %>%
    left_join(y = pairgames_ids, by = c("game", "..subGame")) %>%
    select(-.data$game, -.data[["..subGame"]]) %>%
    select(game = .data$..pairgameId, everything()) %>%
    to_longcr(repair = FALSE) %>%
    to_widecr(repair = FALSE)
}


#' @rdname pairgames
#' @export
is_pairgames <- function(cr_data) {
  cr_data %>%
    to_longcr(repair = TRUE) %>%
    count(.data$game) %>%
    summarise(isAllTwo = all(.data$n == 2)) %>%
    "[["("isAllTwo")
}
