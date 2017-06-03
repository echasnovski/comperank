#' Wide format of competition results
#'
#' Functions for converting data of competition results to wide format.
#'
#' @param cr_data Data of competition results (convertable to tabular).
#' @param repair Whether to repair input.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @section Wide format of competition results:
#' It is assumed that competition consists from multiple games (matches,
#' comparisons, etc.). One game can consist only from \bold{constant} number
#' of players. Inside a game all players are treated equally.
#' In every game every player has some score: the value of arbitrary nature
#' that fully characterizes player's performance in particular game (in most
#' cases it is some numeric value).
#'
#' \code{widecr} inherits from \code{\link[=tbl_df]{tibble}}. Data should be
#' organized in pairs of columns "player"-"score". Identifier of a pair should
#' go after respective keyword and consist only from digits. For example:
#' player1, score1, player2, score2. Order doesn't matter.
#' Extra columns are allowed.
#'
#' To account for R standard string ordering, identifiers of pairs should be
#' formatted with possible leading zeros. For example: player01, score01, ...,
#' player10, score10.
#'
#' Column \code{game} for game identifier is optional. If present it will
#' be used in conversion to \code{longcr} format via \code{\link{to_longcr}}.
#'
#' @details \code{to_widecr} is S3 method for converting data to \code{widecr}.
#' When using default method if \code{repair = TRUE} it tries to fix possible
#' problems with the following actions:
#' \itemize{
#'   \item Detect columns with names containing "player" or "score".
#'     All other columns are treated as "extra";
#'   \item Extract first occurrence of "player" or "score" from names of
#'     detected columns. Everything after extracted word is treated as
#'     identifier of "player"-"score" pair;
#'   \item Convert these identifiers to numeric form with
#'     \code{as.integer(as.factor(...))};
#'   \item Convert identifiers once again to character form with possible
#'     leading zeros (to account for R standard string ordering);
#'   \item Spread pairs to appropriate columns adding columns with
#'     \code{NA_integer_} if they were not present in original data;
#'   \item \bold{Note} that if there is column \code{game} it is placed as
#'     first column.
#' }
#' Note that the order (and numeration) of pairs can change.
#'
#' If \code{repair} is \code{FALSE} it converts \code{cr_data} to
#' \code{\link[=tbl_df]{tibble}} and adds \code{widecr} class to it.
#'
#' When applying \code{to_widecr} to \code{longcr} object, conversion is made:
#' \itemize{
#'   \item All columns except "game", "player" and "score" are dropped;
#'   \item Conversion from long to wide format is made. The number of
#'     "player"-"score" pairs is taken as the maximum number of players in game.
#'     If not all games are played between the same number of players then there
#'     will be \code{NA}'s in some pairs.
#'     Column \code{game} is preserved in output and is used for arranging in
#'     increasing order.
#' }
#'
#' For appropriate \code{widecr} objects \code{to_widecr} returns its input.
#'
#' @return \code{is_widecr} returns TRUE if its argument is appropriate object
#'   of class \code{widecr}.
#'
#' \code{to_widecr} returns an object of class \code{widecr}.
#'
#' @examples
#' cr_data <- data.frame(
#'   playerA = 1:10,
#'   playerB = 2:11,
#'   scoreC = 11:20,
#'   scoreB = 12:21,
#'   scoreA = 13:22,
#'   otherColumn =  101:110
#' )
#' cr_data_wide <- to_widecr(cr_data, repair = TRUE)
#' is_widecr(cr_data_wide)
#'
#' @name results-widecr
#' @seealso \link[=results-longcr]{longcr} for long format.
NULL

#' @rdname results-widecr
#' @export
is_widecr <- function(cr_data) {
  if (!(inherits(x = cr_data, what = "tbl_df"))) {
    return(FALSE)
  }
  names_cr <- tolower(colnames(cr_data))
  names_df <- data.frame(
    name = names_cr[grepl("player|score", x = names_cr)],
    stringsAsFactors = FALSE
  )

  if (nrow(names_df) == 0) {
    return(FALSE)
  }

  names_df <- names_df %>%
    tidyr::extract_(
      col = "name", into = c("group", "id"),
      regex = ".*(player|score)([0-9]+)",
      remove = TRUE
    ) %>%
    mutate(
      group = factor(.data$group, levels = c("player", "score")),
      id = factor(.data$id),
      name = interaction(.data$group, .data$id, sep = "")
    )

  (class(cr_data)[1] == "widecr") &&
    setequal(
      unique(as.character(names_df$name)),
      levels(names_df$name)
    )
}


#' @rdname results-widecr
#' @export
to_widecr <- function(cr_data, repair = TRUE, ...) {
  UseMethod("to_widecr")
}

#' @export
to_widecr.default <- function(cr_data, repair = TRUE, ...) {
  res <- dplyr::tbl_df(cr_data)
  if (repair) {
    res <- repair_widecr(res, ...)
  }
  res <- add_class(res, "widecr")

  res
}

#' @export
to_widecr.longcr <- function(cr_data, repair = TRUE, ...) {
  if (!is_longcr(cr_data)) {
    stop("Input is not appropriate object of class longcr.")
  }

  res <- cr_data %>%
    select(.data$game, .data$player, .data$score) %>%
    group_by(.data$game) %>%
    mutate(..in_game_id = seq_len(n())) %>%
    ungroup() %>%
    mutate(
      ..in_game_id = formatC(.data[["..in_game_id"]],
                           width = get_formatC_width(.data[["..in_game_id"]]),
                           format = "d", flag = "0")
    )
  res <- split(res, res[["..in_game_id"]]) %>%
    lapply(function(game_data) {
      pair_id <- game_data[["..in_game_id"]][1]
      player_name <- paste0("player", pair_id)
      score_name <- paste0("score", pair_id)

      game_data %>%
        rename(
          rlang::UQ(player_name) := .data$player,
          rlang::UQ(score_name) := .data$score
        ) %>%
        select(-.data[["..in_game_id"]])
    }) %>%
    reduce_full_join(by = "game") %>%
    arrange(.data$game) %>%
    select(.data$game, everything())

  if (repair) {
    res <- repair_widecr(res)
  }
  class(res) <- c("widecr", "tbl_df", "tbl", "data.frame")

  res
}

#' @export
to_widecr.widecr <- function(cr_data, repair = TRUE, ...) {
  if (!is_widecr(cr_data)) {
    stop("Input is not appropriate object of class widecr.")
  }

  cr_data
}

repair_widecr <- function(cr_data, ...) {
  repair_info <-
    data.frame(
      original = colnames(cr_data),
      stringsAsFactors = FALSE
    ) %>%
    tidyr::extract_(
      col = "original", into = c("group", "pair"),
      regex = ".*(player|score)(.*)", remove = FALSE
    ) %>%
    filter(.data$group %in% c("player", "score"))

  if (nrow(repair_info) == 0) {
    warning("Neither 'player' nor 'score' columns are detected.")
    return(cr_data)
  }

  repair_info <- repair_info %>%
    mutate(pair = as.integer(factor(.data$pair))) %>%
    tidyr::complete_(cols = c("group", "pair")) %>%
    mutate(pair = formatC(.data$pair,
                          width = get_formatC_width(.data$pair),
                          format = "d", flag = "0")) %>%
    arrange(.data$pair, .data$group) %>%
    tidyr::unite_(col = "target", from = c("group", "pair"), sep = "")

  res <- renamecreate_columns(cr_data, repair_info, fill = NA_integer_)

  if ("game" %in% colnames(res)) {
    res <- res %>%
      select(.data$game,
             rlang::UQS(rlang::syms(repair_info$target)),
             everything())
  } else {
    res <- res %>%
      select(rlang::UQS(rlang::syms(repair_info$target)),
             everything())
  }

  res
}
