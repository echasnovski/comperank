#' Specific functions for computing score summary
#'
#' This page describes specific functions for computing score summary.
#'
#' @param cr_data Competition results.
#' @param prefix Possible prefix to add to score summary's names.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details \code{cr_data} should be convertable to
#'   \code{\link[=results-longcr]{longcr}} form via
#'   \code{\link[=results-longcr]{to_longcr}}
#'
#'   Implementations of \code{score_fun} should be able to take
#'   \code{prefix} and \code{...} as parameters.
#'
#' @return This is a list of currently implemented \code{score_fun}s:
#' \describe{
#'   \item{score_mean_sd}{Returns a named vector (before adding prefix):
#'     "meanScore" - mean score; "sdScore" - standard deviation of scores;}
#'   \item{score_min_max}{Returns a named vector (before adding prefix):
#'     "minScore" - minimum score; "maxScore" - maximum score;}
#'   \item{score_sum}{Returns a named vector (before adding prefix):
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
#' # Computing score summary
#' score_mean_sd(cr_data)
#' score_min_max(cr_data)
#' score_min_max(cr_data, prefix = "total_")
#'
#' @name score-summary-functions
NULL

#' @rdname score-summary-functions
#' @export
score_mean_sd <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- c(mean(cr$score), sd(cr$score))
  names(res) <- paste0(prefix, c("meanScore", "sdScore"))

  res
}

#' @rdname score-summary-functions
#' @export
score_min_max <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- c(min(cr$score), max(cr$score))
  names(res) <- paste0(prefix, c("minScore", "maxScore"))

  res
}

#' @rdname score-summary-functions
#' @export
score_sum <- function(cr_data, prefix = "", ...) {
  cr <- to_longcr(cr_data, repair = TRUE)
  res <- sum(cr$score)
  names(res) <- paste0(prefix, c("sumScore"))

  res
}
