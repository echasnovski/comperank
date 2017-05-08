#' Long format of competition results
#'
#' Functions for converting data of competition results to long format.
#'
#' @param cr_data Data of competition results (convertable to tabular).
#' @param repair Whether to repair input.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @section Long format of competition results:
#' It is assumed that competition consists from multiple games (matches,
#' comparisons, etc.). One game can consist from \bold{variable} number of
#' players. Inside a game all players are treated equally.
#' In every game every player has some score: the value of arbitrary nature
#' that сharacterizes player's performance (in most cases it is some numeric
#' value).
#'
#' \code{longcr} inherits from \code{\link[=tbl_df]{tibble}}. Data should have
#'   at least three columns with the following names:
#'   \itemize{
#'     \item "game" - game identifier;
#'     \item "player" - player identifier;
#'     \item "score" - score of particular player in particular game.
#'   }
#'   Extra columns are allowed. \bold{Note} that if object is converted to
#'   \code{widecr} they will be dropped. So it is better to store extra
#'   information about "game"-"player" pair as list-column "score" which will
#'   stay untouched.
#'
#' @details \code{to_longcr} is S3 method for converting data to \code{longcr}.
#' When using default method if \code{repair} is \code{TRUE} it also tries
#' to fix possible problems with the following actions:
#' \itemize{
#'   \item Detect first columns with names containing "game", "player" or
#'     "score" (ignoring case). If there are many matching names for one output
#'     name then the first one is used. In case of imperfect match, message
#'     is given;
#'   \item If some legitimate names aren't detected respective columns are
#'     created and filled with \code{NA_integer_}. Also a message is given.
#'   \item If in one game some player listed more than once the first record
#'     is taken;
#'   \item Return the tibble with at least 3 appropriate columns and column
#'     names.
#' }
#' If \code{repair} is \code{FALSE} it converts \code{cr_data} to
#' \code{\link[=tbl_df]{tibble}} and adds \code{longcr} class to it.
#'
#' When applying \code{to_longcr} to \code{widecr} object, convertion is made:
#' \itemize{
#'   \item If there is column \code{game} then it is used as game identifier.
#'     Else treat every row as separate game data;
#'   \item Every "player"-"score" pair for every game is converted to separate
#'     row with adding the appropriate extra columns.
#' }
#'
#' For appropriate \code{longcr} objects \code{to_longcr} returns its input.
#'
#' @return \code{is_longcr} returns TRUE if its argument is appropriate object
#'   of class \code{longcr}.
#'
#' \code{to_longcr} returns an object of class \code{longcr}.
#'
#' @examples # Repairing example
#' cr_data <- data.frame(
#'   playerscoregame_ID = rep(1:5, times = 2),
#'   gameId = rep(1:5, each = 2),
#'   scoreS = 31:40,
#'   scoreSS = 41:50
#' )
#' cr_data_long <- to_longcr(cr_data, repair = TRUE)
#' is_longcr(cr_data_long)
#'
#' @name results-longcr
#' @seealso \link[=results-widecr]{widecr} for wide format.
NULL

#' @rdname results-longcr
#' @export
is_longcr <- function(cr_data) {
  (class(cr_data)[1] == "longcr") &&
    (inherits(x = cr_data, what = "tbl_df")) &&
    (length(setdiff(c("game", "player", "score"), colnames(cr_data))) == 0)
}

#' @rdname results-longcr
#' @export
to_longcr <- function(cr_data, repair = TRUE, ...) {
  UseMethod("to_longcr")
}

#' @export
to_longcr.default <- function(cr_data, repair = TRUE, ...) {
  res <- dplyr::tbl_df(cr_data)
  if (repair) {
    res <- repair_longcr(res, ...)
  }
  res <- add_class(res, "longcr")

  res
}

#' @export
to_longcr.widecr <- function(cr_data, repair = TRUE, ...) {
  if (!is_widecr(cr_data)) {
    stop("Input is not appropriate object of class widecr.")
  }

  if (!("game" %in% colnames(cr_data))) {
    cr_data <- cr_data %>%
      mutate_(.dots = list(
        game = ~ 1:n()
      ))
    cr_data <- add_class(cr_data, "widecr")
  }

  matched_names <- colnames(cr_data)[grepl(pattern = "player|score",
                                           x = colnames(cr_data))]

  res <- cr_data %>%
    gather_(key_col = "to_longcr_widecr_name",
            value_col = "to_longcr_widecr_value",
            gather_cols = matched_names) %>%
    extract_(col = "to_longcr_widecr_name",
             into = c("to_longcr_widecr_group", "to_longcr_widecr_id"),
             regex = ".*(player|score)(.*)",
             remove = TRUE, convert = FALSE) %>%
    group_by_("game", "to_longcr_widecr_id") %>%
    spread_(key_col = "to_longcr_widecr_group",
            value_col = "to_longcr_widecr_value",
            convert = TRUE) %>%
    ungroup() %>%
    select_(.dots = list(quote(-to_longcr_widecr_id))) %>%
    select_(.dots = list(
      "game", "player", "score", ~ everything()
    ))

  class(res) <- c("longcr", class(cr_data)[-1])

  res
}

#' @export
to_longcr.longcr <- function(cr_data, repair = TRUE, ...) {
  if (!is_longcr(cr_data)) {
    stop("Input is not appropriate object of class longcr.")
  }

  cr_data
}

repair_longcr <- function(cr_data, ...) {
  longcr_colnames <- c("game", "player", "score")
  longcr_pattern <- paste0(longcr_colnames, collapse = "|")

  names_cr <- tolower(colnames(cr_data))
  matched_which <- which(grepl(pattern = longcr_pattern, x = names_cr))
  names_cr_extracted <-
    regexpr(
      pattern = longcr_pattern,
      text = names_cr[matched_which]
    ) %>%
    regmatches(x = names_cr[matched_which])

  matched_inds <- match(x = longcr_colnames, table = names_cr_extracted)
  repair_info <-
    data.frame(
      target = longcr_colnames,
      original = names(cr_data)[matched_which[matched_inds]],
      stringsAsFactors = FALSE
    )

  assert_used_names(repair_info, prefix = "to_longcr: ")

  res <- renamecreate_columns(cr_data, repair_info, fill = NA_integer_) %>%
    select_(.dots = list(
      "game", "player", "score", ~ everything()
    ))

  not_dupl_records <- (!duplicated(res[, c("game", "player")])) |
    is.na(res$game) | is.na(res$player)

  res[not_dupl_records, ]
}
