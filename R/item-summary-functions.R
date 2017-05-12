#' Specific functions for computing item summary
#'
#' This page describes specific functions for computing item summary.
#'
#' @param cr_data Competition results.
#' @param prefix Possible prefix to add to item summary's names.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details \code{cr_data} should be convertable to
#'   \code{\link[=results-longcr]{longcr}} form via
#'   \code{\link[=results-longcr]{to_longcr}}
#'
#'   Implementations of \code{summary_fun} should be able to take
#'   \code{prefix} and \code{...} as parameters.
#'
#' @return This is a list of currently implemented \code{summary_fun}s:
#' \describe{
#'   \item{summary_mean_sd_score}{Returns a named vector (before adding prefix):
#'     "meanScore" - mean score; "sdScore" - standard deviation of scores;}
#'   \item{summary_min_max_score}{Returns a named vector (before adding prefix):
#'     "minScore" - minimum score; "maxScore" - maximum score;}
#'   \item{summary_sum_score}{Returns a named vector (before adding prefix):
#'     "sumScore" - sum of all present scores.}
#' }
#'
#' @examples
#' cr_data <- data.frame(
#'   game = rep(1:20, each = 2),
#'   player = rep(1:10, times = 4),
#'   score = 31:70,
#'   season = rep(1:2, each = 20)
#' )
#'
#' # Computing item summary
#' summary_mean_sd_score(cr_data)
#' summary_min_max_score(cr_data)
#' summary_min_max_score(cr_data, prefix = "total_")
#'
#' @seealso \link{item-summary} for description of computing item summary.
#' @seealso \link{item-summary-add} for adding item summary to competition
#'  results.
#'
#' @name item-summary-functions
NULL

#' @rdname item-summary-functions
#' @export
summary_mean_sd_score <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- c(mean(cr$score), sd(cr$score))
  names(res) <- paste0(prefix, c("meanScore", "sdScore"))

  res
}

#' @rdname item-summary-functions
#' @export
summary_min_max_score <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- c(min(cr$score), max(cr$score))
  names(res) <- paste0(prefix, c("minScore", "maxScore"))

  res
}

#' @rdname item-summary-functions
#' @export
summary_sum_score <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- sum(cr$score)
  names(res) <- paste0(prefix, c("sumScore"))

  res
}
