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
#' that сharacterizes player's performance (in most cases it is some numeric
#' value).
#'
#' \code{widecr} inherits from \code{\link[=tbl_df]{tibble}}. Data should be
#' orginized in pairs of columns "player"-"score". For example: player1, score1,
#' player2, score2. Order doesn't matter. Extra columns are allowed.
#'
#' To account for R standard string ordering, identifiers of pairs should be
#' formatted with possible leading zeros. For example: player01, score01, ...,
#' player10, score10.
#'
#' Column \code{game} for game identifier is optional. If present it will
#' be used in convertion to \code{longcr} format via \code{\link{to_longcr}}.
#'
#' @details \code{to_widecr} is S3 method for converting data to \code{widecr}.
#' When using default method if \code{repair = TRUE} it tries to fix possible
#' problems with the following actions:
#' \itemize{
#'   \item Detect columns with names containing "player" or "score".
#'     All other columns are treated as "extra";
#'   \item Extract first occurence of "player" or "score" from names of detected
#'     columns. Everything after extracted word is treated as identifier of
#'     "player"-"score" pair;
#'   \item Convert these identifiers to numeric form with
#'     \code{as.integer(as.factor(...))};
#'   \item Convert identifiers once again to character form with possible
#'     leading zeros (to account for R standard string ordering);
#'   \item Spread pairs to appropriate columns adding columns with NA if
#'     they were not present in original data.
#' }
#' Note that the order (and numeration) of pairs can change.
#'
#' If \code{repair} is \code{FALSE} it converts \code{cr_data} to
#' \code{\link[=tbl_df]{tibble}} and adds \code{widecr} class to it.
#'
#' When applying \code{to_widecr} to \code{longcr} object, convertion is made:
#' \itemize{
#'   \item All columns except "game", "player" and "score" are dropped;
#'   \item Convertion from long to wide format is made. The number of
#'     "player"-"score" pairs is taken as the maximum number of players in game.
#'     If not all games are played between the same number of players then there
#'     will be \code{NA}'s in some pairs.
#'     Column \code{game} is preserved in output.
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
    name = names_cr[grepl("player|score", x = names_cr)]
  ) %>%
    extract_(col = "name", into = c("group", "id"),
             regex = ".*(player|score)(.*)",
             remove = TRUE) %>%
    mutate_(.dots = list(
      group = ~ factor(group, levels = c("player", "score")),
      id = ~ factor(id),
      name = ~ interaction(group, id, sep = "")
    ))

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
    select_("game", "player", "score") %>%
    group_by_("game") %>%
    mutate_(.dots = list(
      to_widecr_id = ~ 1:n()
    )) %>%
    ungroup() %>%
    gather_(key_col = "to_widecr_group", value_col = "to_widecr_value",
            gather_cols = c("player", "score")) %>%
    mutate_(.dots = list(
      to_widecr_id = ~ formatC(to_widecr_id,
                               width = get_formatC_width(to_widecr_id),
                               format = "d", flag = "0"),
      to_widecr_id = ~ factor(to_widecr_id),
      to_widecr_group = ~ factor(to_widecr_group,
                                 levels = c("player", "score")),
      to_widecr_name = ~ interaction(to_widecr_group,
                                     to_widecr_id,
                                     sep = "")
    )) %>%
    select_(.dots = list(quote(-to_widecr_id),
                         quote(-to_widecr_group))) %>%
    spread_(key_col = "to_widecr_name", value_col = "to_widecr_value",
            convert = TRUE, drop = FALSE)

  res <- res[, c(setdiff(colnames(res), "game"), "game")]

  class(res) <- c("widecr", class(cr_data)[-1])

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
  # Repairing column names and order
  is_wide_cr_name <- grepl(pattern = "player|score",
                           x = tolower(colnames(cr_data)))
  extra_data <- cr_data[, !is_wide_cr_name, drop = FALSE]
  res <- cr_data[, is_wide_cr_name, drop = FALSE]

  colnames(res) <- tolower(colnames(res))

  res <- res %>%
    mutate_(.dots = list(
      repair_widecr_game = ~ 1:n()
    )) %>%
    gather_(key_col = "repair_widecr_name",
            value_col = "repair_widecr_value",
            gather_cols = colnames(res)) %>%
    extract_(col = "repair_widecr_name",
             into = c("repair_widecr_group", "repair_widecr_id"),
             regex = ".*(player|score)(.*)",
             remove = TRUE) %>%
    filter_(.dots = list(
      ~ repair_widecr_group %in% c("player", "score")
    )) %>%
    mutate_(.dots = list(
      repair_widecr_group = ~ factor(repair_widecr_group,
                                     levels = c("player", "score")),
      repair_widecr_id = ~ as.integer(factor(repair_widecr_id)),
      repair_widecr_id = ~ formatC(repair_widecr_id,
                                   width = get_formatC_width(repair_widecr_id),
                                   format = "d", flag = "0"),
      repair_widecr_name = ~ interaction(repair_widecr_group,
                                         repair_widecr_id,
                                         sep = "")
    )) %>%
    select_("repair_widecr_game", "repair_widecr_name",
            "repair_widecr_value") %>%
    spread_(key_col = "repair_widecr_name", value_col = "repair_widecr_value",
            convert = TRUE, drop = FALSE) %>%
    select_(.dots = list(quote(-repair_widecr_game)))

  bind_cols(res, extra_data)
}
