#' Specific functions for computing Head-to-Head matrices
#'
#' Specific functions for computing Head-to-Head matrices.
#'
#' @param matchup_data Data of matchups described in \link{head-to-head} as
#'   input for specific \code{h2h_fun}.
#' @param half_for_draw Whether to count half points for draws in
#'   \code{h2h_num_wins}.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details This is a list of currently implemented \code{h2h_fun}s:
#' \describe{
#'   \item{h2h_mean_score_diff}{Computes mean score difference of \code{player2}
#'     compared to \code{player1};}
#'   \item{h2h_mean_score}{Computes mean score of \code{player2};}
#'   \item{h2h_sum_score_diff}{Computes sum of score differences of
#'     \code{player2} compared to \code{player1};}
#'   \item{h2h_sum_score}{Computes sum of scores of \code{player2};}
#'   \item{h2h_num_wins}{Computes number of matchups \code{player2} scored
#'     \bold{more} than \code{player1}. If \code{half_for_draw} is \code{TRUE}
#'     then it also adds half number of matchups where they had draw.
#'     \bold{Note} that it excludes matchups of the players with themselves.}
#' }
#'
#' @return All \code{h2h_*} are implementations of \code{h2h_fun} and return a
#'   single Head-to-Head value.
#'
#' @examples # Initializing data
#' cr_data <- data.frame(
#'   game = rep(1:5, each = 2),
#'   player = rep(1:5, times = 2),
#'   score = c(31:39, 39)
#' )
#' matchup_data <- get_matchups(cr_data)
#'
#' # Computing Head-to-Head matrices
#' get_h2h(cr_data, h2h_num_wins)
#' get_h2h(cr_data, h2h_num_wins, half_for_draw = TRUE)
#' get_h2h(cr_data, h2h_num_wins, half_for_draw = TRUE, fill = 0)
#'
#' # Computing Head-to-Head values based on matchups
#' h2h_mean_score_diff(matchup_data[2, ])
#' h2h_mean_score_diff(matchup_data[19, ])
#'
#' @seealso \link{head-to-head} Abstract computing of Head-to-Head matrices.
#' @seealso \link{head-to-head-helpers} Head-to-Head helpers.
#' @name head-to-head-functions
NULL

#' @rdname head-to-head-functions
#' @export
h2h_mean_score_diff <- function(matchup_data, ...) {
  mean(matchup_data$score2 - matchup_data$score1)
}

#' @rdname head-to-head-functions
#' @export
h2h_mean_score <- function(matchup_data, ...) {
  mean(matchup_data$score2)
}

#' @rdname head-to-head-functions
#' @export
h2h_sum_score_diff <- function(matchup_data, ...) {
  sum(matchup_data$score2 - matchup_data$score1)
}

#' @rdname head-to-head-functions
#' @export
h2h_sum_score <- function(matchup_data, ...) {
  sum(matchup_data$score2)
}

#' @rdname head-to-head-functions
#' @export
h2h_num_wins <- function(matchup_data, half_for_draw = FALSE, ...) {
  not_identity <- matchup_data$player1 != matchup_data$player2
  score1 <- matchup_data$score1[not_identity]
  score2 <- matchup_data$score2[not_identity]
  sum(score2 > score1) + half_for_draw * 0.5 * sum(score2 == score1)
}
